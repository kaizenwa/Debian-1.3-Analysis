/* winsup file handling

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
*/

/* FIXME: Supporting all kinds of file types could make this file grow without
   bound.  Perhaps move things like socket support to their own files.  */

#include <sys/stat.h>
#include <sys/param.h>
#include <sys/termios.h>
#include <fcntl.h>
#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <unistd.h>
#include "winsup.h"

static const int CHUNK_SIZE=1024; /* Used for crlf conversions */

/* Record the file name.
   We currently don't allocate space for long file names (see fhandler.h),
   so only record the last part if P is too long.
   Filenames are used mostly for debugging messages, and it's hoped that
   in cases where the name is really required, the filename wouldn't ever
   be too long (e.g. devices or some such).  Again, see fhandler.h.

   execable_p checks the suffix of the file for
   .exe, etc. so we need to save something.

   This function may stay, but it is intended that it be rewritten when
   file handle allocation is rewritten.  */

void
fhandler_base::set_name (const char *p)
{
  int len = strlen (p);
  int maxlen = sizeof (name) - 1;

  if (len <= maxlen)
    strcpy (name, p);
  else
    {
      strcpy (name, "...");
      strncpy (name + 3, p + len - (maxlen - 3), maxlen - 3);
      name[maxlen] = 0;
    }
}

/* Normal file i/o handlers.  */

/* Cover function to ReadFile to achieve (as much as possible) Posix style
   semantics and use of errno.  */
int
fhandler_base::raw_read (void *ptr, size_t ulen)
{
  DWORD bytes_read;
  int len = ulen;

  if (!ReadFile (handle, ptr, len, &bytes_read, 0))
    {
      int errcode;

      /* Some errors are not really errors.  Detect such cases here.  */

      errcode = GetLastError ();
      switch (errcode)
	{
	case ERROR_BROKEN_PIPE:
	  /* This is really EOF.  */
	  bytes_read = 0;
	  break;
	case ERROR_MORE_DATA:
	  /* `bytes_read' is supposedly valid.  */
	  break;
	default:
	  syscall_printf ("ReadFile %s failed\n", name);
	  set_errno (EACCES);
	  return -1;
	  break;
	}
    }

  return bytes_read;
}

int
fhandler_tty::raw_read (void *ptr, size_t ulen)
{
  if (vtime || vmin)
    {
      if (vmin == 0)
	ulen = 1;
      else if (vmin < ulen)
	ulen = vmin;
      syscall_printf ("timeout len %d\n", ulen);
    }
  return fhandler_base::raw_read (ptr, ulen);
}

/* Cover function to WriteFile to provide Posix interface and semantics
   (as much as possible).  */
int
fhandler_base::raw_write (const void *ptr, size_t len)
{
  DWORD bytes_written;

  if (!WriteFile (handle, ptr, len, &bytes_written, 0))
    {
      __seterrno ();
      return -1;
    }
  return bytes_written;
}

/* Open system call handler for non device files.  */
/* FIXME: Do we need a `mode' argument?  */
fhandler * 
fhandler_base::open (const char *path, int flags)
{
  fhandler * res;
  HANDLE x;
  int file_attributes;
  int shared;
  SECURITY_ATTRIBUTES sa;
  int creation_distribution;

  syscall_printf ("fhandler_base::open (%s, 0x%x)\n", path, flags);

  /* FIXME: To add symlink support, insert a call to symlink_follow here.  */

  path_conv realpath (path);
  if (realpath.error)
    {
      set_errno (realpath.error);
      syscall_printf ("NULL = fhandler_base::open (%s, 0x%x)\n", path, flags);
      return NULL;
    }

  if ((flags & (O_RDONLY | O_WRONLY | O_RDWR)) == O_RDONLY)
    {
      access = GENERIC_READ;
    }
  else if ((flags & (O_RDONLY | O_WRONLY | O_RDWR)) == O_WRONLY)
    {
      access = GENERIC_WRITE;
    }
  else
    {
      access = GENERIC_READ | GENERIC_WRITE;
    }

  if (flags & O_CREAT)
    creation_distribution = CREATE_ALWAYS;
  else
    creation_distribution = OPEN_EXISTING;

  if (flags & O_APPEND) 
    {
      creation_distribution = OPEN_ALWAYS;
      append_p = 1;
    }

  shared = FILE_SHARE_READ | FILE_SHARE_WRITE;
  sa.nLength = sizeof (sa);
  sa.lpSecurityDescriptor = 0;

  sa.bInheritHandle = TRUE;
  file_attributes = FILE_ATTRIBUTE_NORMAL;

  x = CreateFileA (realpath.get_win32 (), access, shared,
		   &sa, creation_distribution,
		   file_attributes,
		   0);

  syscall_printf ("%d = CreateFileA (%s, 0x%x, 0x%x, 0x%x, 0x%x, 0x%x, 0)\n",
		  x,
		  realpath.get_win32 (), access, shared,
		  &sa, creation_distribution,
		  file_attributes);
	
  if (x == INVALID_HANDLE_VALUE)
    {
      if (GetLastError () == ERROR_INVALID_HANDLE)
	set_errno (ENOENT);
      else
	__seterrno ();
      syscall_printf ("NULL = fhandler_base::open (%s, 0x%x)\n", path, flags);
      return NULL;
    }

  set_name (path);
  namehash = hash_path_name (path);
  handle = x;
  readahead_valid = 0;
  close_exec_p = 0;
  rpos = 0;
  had_eof = 0;
  rsize = -1;

  if (realpath.binary_p || (flags & O_BINARY))
    {
      set_r_binary (1);
      set_w_binary (1);
    }
  else if (flags & O_TEXT)
    {
      set_r_binary (0);
      set_w_binary (0);
    }
  else if (u->fmode_ptr && *(u->fmode_ptr) & O_BINARY)
    {
      set_r_binary (1);
      set_w_binary (1);
      syscall_printf ("filemode defaulting to binary.\n");
    }
  else 
    {
      set_r_binary (0);
      set_w_binary (0);
      syscall_printf ("filemode defaulting to text.\n");
    }

  if (flags & O_APPEND)
    SetFilePointer (handle, 0, 0, FILE_END);
  else
    SetFilePointer (handle, 0, 0, FILE_BEGIN);

  res = this;
  syscall_printf ("%p = fhandler_base::open (%s, 0x%x)\n", res, path, flags);
  return res;
}

/* states:
   open buffer in binary mode?  Just do the read.  

   open buffer in text mode?  Scan buffer for control zs and handle
   the first one found.  Then scan buffer, converting every \r\n into
   an \n.  If last char is an \r, look ahead one more char, if \n then
   modify \r, if not, remember char.  
*/
int
fhandler_base::read (void *ptr, size_t ulen)
{
  int len = ulen;
  char *ctrlzpos;
  int i;

  if (len == 0)
    return 0;

  if (get_r_binary ())
    {
      int l = raw_read (ptr, len);
      if (l <= 0)
	return l;
      rpos += l;
      return l;
    }

  if (readahead_valid)
    {
      /* We read too much last time. */
      readahead_valid = 0;
      ((char *)ptr)[0] = readahead;
      int res = read ( (char *)ptr + 1, len - 1);
      rpos++;
      if (res < 0)
	{
	  return 1;
	}
      else
	{
	  return res + 1;
	}
    }
  
  int l = raw_read (ptr, len);
  if (l <= 0)
    return l;

  /* If the first character is a control-z we're at virtual EOF.  */
  if ( ((char *)ptr)[0] == 0x1a )
    {
      return 0;
    }

  /* Scan buffer for a control-z and shorten the buffer to that length */

  ctrlzpos = (char *)memchr ((char *)ptr, 0x1a, l);
  if (ctrlzpos)
    {	
      lseek ( (ctrlzpos - ((char *)ptr + l)), SEEK_CUR);
      l = ctrlzpos - (char *)ptr;
    }

  /* Scan buffer and turn \r\n into \n */
  register char *src= (char *)ptr;
  register char *dst = (char *)ptr;
  register char *end = src + l - 1;

  /* Read up to the last but one char - the last char needs special handling */
  while (src < end)
    {
      if (src[0] == '\r' && (src[1] == '\n' || src[1] == '\r'))
	{
	  /* Ignore this. */
	  src++;
	}
      else
	{
	  *dst++ = *src++;
	}
    }

  /* if last char is a '\r' then read one more to see if we should
     translate this one too */
  if (*src == '\r')
    {
      int len;
      char c;
      len = raw_read (&c, 1);
      if (len > 0)
	{
	  if (c == '\n')
	    {
	      *dst++ = '\n';
	    }
	  else
	    {
	      readahead_valid = 1;
	      readahead = c;
	    }
	}
    }
  else
    {
      *dst++ = *src;
    }

  l = dst - (char *)ptr;

  rpos += l;

  if (u->strace_mask & (_STRACE_DEBUG | _STRACE_ALL))
    {
      char buf[16 * 6 + 1];
      char *p = buf;

      for (int i = 0; i < l && i < 16; ++i)
	{
	  unsigned char c = ((unsigned char *) ptr)[i];
	  /* >= 33 so space prints in hex */
	  __small_sprintf (p, c >= 33 && c <= 127 ? " %c" : " 0x%x", c);
	  p += strlen (p);
	}
      debug_printf ("read %d bytes (%s%s)\n", l, buf, l > 16 ? " ..." : "");
    }

  return l;
}

int
fhandler_base::write (const void *ptr, size_t len)
{
  int res;

  if (append_p)
    SetFilePointer (handle, 0, 0, FILE_END);

  if (get_w_binary ())
    {
      res = raw_write (ptr, len);
    }
  else
    {
      /* Keep track of previous \rs, we don't want to turn existing
	 \r\n's into \r\n\n's */
      register int pr = 0;

      /* Copy things in chunks */
      char buf[CHUNK_SIZE];

      for (unsigned int i  = 0; i < len; i += sizeof (buf)/2 )
	{
	  register const char *src = (char *)ptr + i;
	  int todo = MIN(len - i, sizeof (buf) /2);
	  register const char *end = src + todo;
	  register char *dst = buf;
	  while (src < end)
	    {
	      if (*src == '\n' && !pr)
		{
		  /* Emit a cr lf here */
		  *dst ++ = '\r';
		  *dst ++ = '\n';
		}
	      else if (*src == '\r')
		{
		  *dst ++ = '\r';
		  pr = 1;
		}
	      else 
		{
		  *dst ++ = *src;
		  pr = 0;
		}
	      src++;
	    }
	  int want = dst-buf;
	  if (raw_write (buf, want) != want)
	    {
	      /* Tricky... Didn't write everything we wanted.. How can
		 we work out exactly which chars were sent ?  We don't...
		 This will only happen in pretty nasty circumstances. */
	      rpos += i;
	      return i;
	    }
	}
      /* Done everything, update by the chars that the user sent */
      rpos += len;
      /* Length of file has changed */
      rsize = -1;
      res = len;
      debug_printf ("after write, name %s, rpos %d\n", name, rpos);
    }
  return res;
}

off_t
fhandler_base::lseek (off_t offset, int whence)
{
  off_t res;

  /* Seeks on text files is tough, we rewind and read till we get to the
     right place.  */

  readahead_valid = 0;

  debug_printf ("lseek (%s, %d, %d)\n", name, offset, whence);

#if 0	/* lseek has no business messing about with text-mode stuff */

  if (!get_r_binary ())
    {
      int newplace;

      if (whence == 0)
	{
	  newplace = offset;
	}
      else if (whence ==1)
	{
	  newplace = rpos +  offset;
	}
      else 
	{
	  /* Seek from the end of a file.. */
	  if (rsize == -1)
	    {
	      /* Find the size of the file by reading till the end */
	      
	      char b[CHUNK_SIZE];
	      while (read (b, sizeof (b)) > 0)
		;
	      rsize = rpos;
	    }
	  newplace = rsize + offset;
	}

      if (rpos > newplace)
	{
	  SetFilePointer (handle, 0, 0, 0);
	  rpos = 0;
	}

      /* You can never shrink something more than 50% by turning CRLF into LF,
	 so we binary chop looking for the right place */

      while (rpos < newplace)
	{
	  char b[CHUNK_SIZE];
	  size_t span = (newplace - rpos) / 2;
	  if (span == 0)
	    span = 1;
	  if (span > sizeof (b))
	    span = sizeof (b);

	  debug_printf ("lseek (%s, %d, %d) span %d, rpos %d newplace %d\n",
		       name, offset, whence,span,rpos, newplace);
	  read (b, span);
	}

      debug_printf ("Returning %d\n", newplace);
      return newplace;
    }
#endif	/* end of deleted code dealing with text mode */

  DWORD win32_whence = whence == SEEK_SET ? FILE_BEGIN
                       : (whence == SEEK_CUR ? FILE_CURRENT : FILE_END);

  res = SetFilePointer (handle, offset, 0, win32_whence);

  if (res == -1)
    {
      __seterrno ();
    }
  return res;
}

int
fhandler_base::close ()
{
  int res = -1;

  syscall_printf ("fhandler_base::close (handle %d)\n",handle);

  /* int type = GetFileType (handle);*/
  int type = 0;

  /* Can't really close these things, but pretend we did. */
  if (type == FILE_TYPE_CHAR && 0)
    res = 0;
  else 
    {
      if (!CloseHandle (handle))
	{
	  paranoid_printf ("CloseHandle (%d <%s>) failed\n", handle, name);

	  __seterrno ();
	}
      else
	{
	  if (type == FILE_TYPE_DISK)
	    process_deletion_queue ();
	  res = 0;
	  handle = (HANDLE)-99;
	}
    }
  return res;
}

int
fhandler_base::ioctl (int cmd, void *buf)
{
  if (cmd == FIONBIO)
    {
      syscall_printf ("fhandler.cc: ioctl (FIONBIO,%x)\n",buf);
    }
  else
    {
      syscall_printf ("fhandler.cc: ioctl (%x,%x)\n", cmd, buf);
    }

  switch (cmd)
    {
    case TIOCGWINSZ:
      {
	CONSOLE_SCREEN_BUFFER_INFO info;
	int st;

	if (handle == GetStdHandle (STD_INPUT_HANDLE))
	  {
	    st = GetConsoleScreenBufferInfo (GetStdHandle (STD_OUTPUT_HANDLE),
					     &info);
	  }
        else
	  {
	    st = GetConsoleScreenBufferInfo (handle, &info);
	  }

	if (st)
	  {
	    /* *not* the buffer size, the actual screen size... */
	    /* based on Left Top Right Bottom of srWindow */
	    ((struct winsize *) buf)->ws_row = 
	        1 + info.srWindow.Bottom - info.srWindow.Top;
	    ((struct winsize *) buf)->ws_col = 
		1 + info.srWindow.Right - info.srWindow.Left;
	    syscall_printf ("fhandler.cc: WINSZ: dw (y=%d,x=%d) buf (row=%d,col=%d)\n",
			   info.dwSize.Y,info.dwSize.X,
			   ((struct winsize *) buf)->ws_row,
			   ((struct winsize *) buf)->ws_col);
	    return 0;
	  }
	else
	  {
	    syscall_printf ("fhandler.cc: WINSZ failed\n");
	    __seterrno ();
	    return -1;
	  }
	return 0;
      }
    case TIOCSWINSZ:
      return 0;
    default:
      {
        set_errno (EINVAL);
        return -1;
      }
    }
  return 0; /* should never get here */
}

/* Utility for fhandler_base::fstat.
   This is only intended to be called for disk files.  */
/* FIXME: Try to reorganize fstat so this can be moved to
   fhandler_disk_file.  */

int
fhandler_base::get_execable ()
{
  /* Has value been cached yet?  */
  if (execable_p != -1)
    return execable_p;

  /* Nope, so compute it.  */
  int len = strlen (name);
  const char *ch = name + (len > 4 ? len - 4 : len);
  if (strcasecmp (".bat", ch) == 0
      || strcasecmp (".exe", ch) == 0
      || strcasecmp (".com", ch) == 0)
    execable_p = 1;
  else
    {
      /* ??? This isn't bullet proof (and I hate non-bullet proof solutions).
	 The litmus test is whether any errors are harmless.  */
      char buf[2];
      DWORD done;
      if (ReadFile (handle, buf, 2, &done, 0) != 0
	  && done > 0)
	{
	  if (done == 2 && buf[0] == '#' && buf[1] == '!')
	    execable_p = 1;
	  /* Back up the file pointer.  */
	  SetFilePointer (handle, -done, 0, FILE_CURRENT);
	}
    }

  if (execable_p == -1)
    execable_p = 0;

  return execable_p;
}

int
fhandler_base::fstat (struct stat *buf)
{
  int res;
  BY_HANDLE_FILE_INFORMATION local;

  memset (buf, 0, sizeof (*buf));

  res = GetFileInformationByHandle (handle, &local);
  debug_printf ("%d = gfi h %s %d\n", res, name, handle);
  if (res == 0)
    {
      /* GetFileInformationByHandle will fail if it's given stdin/out/err 
	 or a pipe*/
      if (1)
	{
	  /* We expect these to fail! */
	  buf->st_mode |= S_IFCHR;
	  buf->st_blksize = S_BLKSIZE;
	  buf->st_ino = namehash;
	  syscall_printf ("0 = fstat (, 0x%x)\n",  buf);
	  return 0;
	}
      else
	{
	  __seterrno ();
	  syscall_printf ("-1 = fstat (, 0x%x)\n",  buf);
	  return -1;
	}
    }

  buf->st_atime   = to_time_t (&local.ftLastAccessTime);
  buf->st_mtime   = to_time_t (&local.ftLastWriteTime);
  buf->st_ctime   = to_time_t (&local.ftCreationTime);
  buf->st_nlink   = local.nNumberOfLinks;
  buf->st_dev     = local.dwVolumeSerialNumber;
  buf->st_size    = local.nFileSizeLow;
  buf->st_ino     = local.nFileIndexLow ^ namehash;
  buf->st_blksize = S_BLKSIZE;
  buf->st_blocks  = (buf->st_size + S_BLKSIZE-1) / S_BLKSIZE;
  buf->st_uid     = getuid ();
  buf->st_gid     = getgid ();
  buf->st_mode = 0;
  buf->st_mode |= STD_RBITS;
  
  if (! (local.dwFileAttributes & FILE_ATTRIBUTE_READONLY))
    buf->st_mode |= STD_WBITS;	
  /* | S_IWGRP | S_IWOTH; we don't give write to group etc */

  if (symlink_p)
    buf->st_mode |= S_IFLNK;
  else
    switch (GetFileType (handle))
      {
      case FILE_TYPE_CHAR:
      case FILE_TYPE_UNKNOWN:
	buf->st_mode |= S_IFCHR;
	break;
      case FILE_TYPE_DISK:
	buf->st_mode |= S_IFREG;
	if (get_execable ())
	  buf->st_mode |= STD_XBITS;
	break;
      case FILE_TYPE_PIPE:
	buf->st_mode |= S_IFSOCK;
	break;
      }

  syscall_printf ("0 = fstat (, 0x%x) st_atime=%x st_size=%d, st_mode=0x%x, st_ino=%d, sizeof=%d\n",
		 buf, buf->st_atime, buf->st_size, buf->st_mode, 
		 (int) buf->st_ino, sizeof (*buf));

  return 0;
}

void
fhandler_base::init (HANDLE f, int bin, int a, const char *n)
{
  set_handle (f);
  set_r_binary (bin);
  set_w_binary (bin);
  access = a;
  set_name (n);
  debug_printf ("created new fhandler for <%s> with handle %d\n", n, f);
}

void
fhandler::init (HANDLE, int bin, int, const char *)
{
  set_r_binary (bin);
  set_w_binary (bin);
}

void
fhandler_base::dump ()
{
  paranoid_printf ( "FHANDLER BASE\n");
}

void
fhandler_base::set_handle (HANDLE x)
{
  debug_printf ("set handle to %d\n", x);
  handle = x;
}

int
fhandler::has_handle_p ()
{
  return 0;
}

int
fhandler_base::has_handle_p ()
{
  return 1;
}

fhandler *
fhandler_console::open (const char *, int flags)
{
  HANDLE copy;
  if ((flags & O_RDONLY) == O_RDONLY)
    copy = GetStdHandle (STD_INPUT_HANDLE);
  else if ((flags & O_WRONLY) == O_WRONLY)
    copy = GetStdHandle (STD_OUTPUT_HANDLE);
  else
    copy = GetStdHandle (STD_OUTPUT_HANDLE);

  set_handle (copy);
  return this;
}

int
fhandler_tty::close ()
{
  /* FIXME: Sometimes we use get_handle (), sometimes we reference `handle'
     directly.  Need to pick one.  */
  paranoid_printf ("fhandler_tty::close (handle %d)\n", get_handle ());
  return 0;
}

void
fhandler_tty::dump ()
{
  paranoid_printf ("FHANDLER TTY\n");
}

void
fhandler::dump ()
{
  paranoid_printf ("FHANDLER BARE!!\n");
}

int
fhandler::read (void *, size_t)
{
  MARK();
  return 0;
}

HANDLE
fhandler::get_handle ()
{
  MARK();
  return 0;
}

fhandler *
fhandler::open (const char *, int)
{
  MARK();
  return 0;
}

int
fhandler::fstat (struct stat *)
{
  MARK();
  return 0;
}

int
fhandler::ioctl (int, void *)
{
  MARK();
  return 0;
}

const char *
fhandler::ttyname ()
{
  MARK();
  return 0;
}

int
fhandler::write (const void *, size_t len)
{
  MARK();
  return len;
}

off_t
fhandler::lseek (off_t , int)
{
  MARK();
  return 0;
}

int
fhandler::close ()
{
  MARK();
  return 0;
} 

/**********************************************************************/
/* /dev/null */

fhandler *
fhandler_dev_null::open (const char *, int)
{
  return this;
}

void
fhandler_dev_null::dump ()
{
  paranoid_printf ("FHANDLER DEV/NULL\n");
}

int
fhandler_dev_null::close ()
{
  return 0;
}

int
fhandler_dev_null::fstat (struct stat *buf) 
{
  memset (buf, 0, sizeof (*buf));
  buf->st_blksize = S_BLKSIZE;
  buf->st_dev = 1234;
  buf->st_ino = 4567;

  return 0;
}

int
fhandler_dev_null::ioctl (int, void *)
{
  return -1;
}

int
fhandler_dev_null::read (void *, size_t)
{
  return 0;
}

int
fhandler_dev_null::write (const void *, size_t x)
{
  syscall_printf ("/dev/null write %d\n",x);
  return x;
}

long
fhandler_dev_null::lseek (long, int)
{
  return 0;
}

HANDLE
fhandler_dev_null::get_handle () 
{
  return INVALID_HANDLE_VALUE;
}

void
hinfo::clearout ()
{
  h = 0;
}

int
fhandler_make_pipe (int fildes[2])
{
  int  fdr, fdw;
  pinfo *p = this_procinfo ();
  fdr = p->hmap.find_unused_handle (0);
  if (fdr < 0) 
    set_errno (ENMFILE);
  else 
    {
      fdw = p->hmap.find_unused_handle (fdr+1);
      if (fdw < 0)
	set_errno ( ENMFILE);
      else
	{
	  SECURITY_ATTRIBUTES sa;

	  sa.nLength = sizeof (sa);
	  sa.lpSecurityDescriptor = 0;

	  /* When we fork we duplicate all the file handles to be inherited,
	     therefore all fds must be created as non-inheritable if we're the
	     parent.  We still set the close-on-exec flag to "no" though. */
	  /* FIXME: This comment is out of date.  Gee, what a surprise.  */

	  sa.bInheritHandle = 1;
	  HANDLE r, w;
	  if (CreatePipe (&r, &w, &sa, 0))
	    {
	      u->self->hmap.build_fhandler ("piper", fdr);
	      u->self->hmap.build_fhandler ("pipew", fdw);
	      
	      ((fhandler_base *) p->hmap[fdr].h)->init (r, 1, GENERIC_READ, "piper");
	      ((fhandler_base *) p->hmap[fdw].h)->init (w, 1, GENERIC_WRITE, "pipew");

	      fildes[0] = fdr;
	      fildes[1] = fdw;
	      
	      debug_printf ("0 = pipe (%d) (%d:%d, %d:%d)\n",
			    fildes, 
			    fdr, 
			    p->hmap[fdr].h->get_handle (),
			    fdw,
			    p->hmap[fdw].h->get_handle ());

	      return 0;
	    }
	  else
	    {
	      __seterrno ();
	    }
	}
    }
  
  syscall_printf ("-1 = pipe (0x%x)\n", fildes);
  return -1;
}

void
fhandler::dup (fhandler *child)
{
  /* This should never happen... */
  child->close_exec_p = 0;
}

void
fhandler_base::dup (fhandler *generic_child)
{
  fhandler_base *child = (fhandler_base *)(generic_child);

  debug_printf ("in fhandler_base dup\n");

  child->close_exec_p = 0;

  const HANDLE proc = GetCurrentProcess ();
  HANDLE nh;
  if (!DuplicateHandle (proc, handle, proc, &nh, 0, 1, DUPLICATE_SAME_ACCESS))
    {
      small_printf ("COPY FOR DUP FAILED, handle in %x %x!!\n", handle,GetLastError ());
    }

  child->handle = nh;
}

fhandler::fhandler ()
{
  w_binary = 0;
  r_binary = 0;

  close_exec_p = 0;
}

fhandler_base::fhandler_base ()
{
  handle = 0; 
  access = 0; 
  readahead_valid = 0; 
  readahead = 0; 
  append_p = 0;
  rpos = 0;
  rsize = 0;
  had_eof= 0;
  symlink_p = 0;
  namehash = 0;
  /* Mark that we don't know whether the file is execable or not.  */
  execable_p = -1;
}

fhandler_disk_file::fhandler_disk_file ()
{
}

extern "C" 
{
  FILE *popen (const char *, const char *)
    {
      small_printf ("POPEN IS DOOMED\n");
      return 0;
    }
  int pclose (FILE *)
    {
      small_printf ("PCLOSE IS DOOMED\n");
      return 0;
    }
};

/**********************************************************************/

fhandler_tty::fhandler_tty (void)
{
  vmin = 0;
  vtime = 0;
}

const char *
fhandler_tty::ttyname ()
{
  return get_name ();
}

int
fhandler_tty::fstat (struct stat *buf)
{
  memset (buf, 0, sizeof (*buf));
  buf->st_mode |= S_IFCHR;
  buf->st_blksize = S_BLKSIZE;
  buf->st_uid     = getuid ();
  buf->st_gid     = getgid ();
  buf->st_mode |= STD_RBITS | STD_WBITS;
  buf->st_ino = namehash;
syscall_printf ("fhandler_tty:fstat (%x)->mode %x\n", buf, buf->st_mode);
  return 0;
}

int
fhandler_base::tcflush (int queue)
{
  if (queue & (TCOFLUSH | TCIOFLUSH))
    {
      PurgeComm (handle, PURGE_TXABORT | PURGE_TXCLEAR);
    }

  if (queue & (TCIFLUSH | TCIOFLUSH))
    {
      /* Input flushing by polling until nothing turns up
	 (we stop after 1000 chars anyway) */
      COMMTIMEOUTS old;
      COMMTIMEOUTS tmp;
      char b;
      DWORD more = 1;
      int max = 1000;
      PurgeComm (handle, PURGE_RXABORT | PURGE_RXCLEAR);
      GetCommTimeouts (handle, &old);
      memset (&tmp, 0, sizeof (tmp));
      tmp.ReadTotalTimeoutConstant = 100;
      SetCommTimeouts (handle, &tmp);
      while (max > 0 && more)
	{
	  ReadFile (handle, &b, 1,  &more,  0);
	  if (more) 
	    {
	      termios_printf ("dropping %d\n", b);
	    }
	  max--;
	}
      SetCommTimeouts (handle, &old);
    }
  return 0;
}

int
fhandler_tty::tcsetattr (int, const struct termios *t)
{
  int newrate;
  int newsize;

  COMMTIMEOUTS to;
  DCB state;

  switch (t->c_ospeed)
    {
    case B110:
      newrate = CBR_110;
      break;
    case B300:
      newrate = CBR_300;
      break;
    case B600:
      newrate = CBR_600;
      break;
    case B1200:
      newrate = CBR_1200;
      break;
    case B2400:
      newrate = CBR_2400;
      break;
    case B4800:
      newrate = CBR_4800;
      break;
    case B9600:
      newrate = CBR_9600;
      break;
    case B19200:
      newrate = CBR_19200;
      break;
    case B38400:
      newrate = CBR_38400;
      break;
    default:
      termios_printf ("t->c_ospeed was %d\n", t->c_ospeed);
      set_errno ( EINVAL);
      return -1;
    }

  switch (t->c_cflag & CSIZE)
    {
    case CS5:
      newsize = 5;
      break;
    case CS6:
      newsize = 6;
      break;
    case CS7:
      newsize = 7;
      break;
    case CS8:
      newsize = 8;
      break;
    default:
      newsize = 8;
    }

  GetCommState (get_handle (), &state);
#if 0
  ds ("First in tcsetattr", &state);
#endif
  state.BaudRate = newrate;
  state.ByteSize = newsize;
  state.fBinary = 1;
  state.fParity = 0;
  state.fOutxCtsFlow = 0;	/*!!*/
  state.fOutxDsrFlow = 0;	/*!!*/
  state.fDsrSensitivity = 0;	/*!!*/

  if (t->c_cflag & PARENB)
    state.Parity = (t->c_cflag & PARODD) ? ODDPARITY:EVENPARITY;
  else
    state.Parity = NOPARITY;
#if 0
  ds ("Before SetCommState", &state);  
#endif
  SetCommState (get_handle (), &state);

  set_r_binary ((t->c_iflag & IGNCR) ? 0 : 1);
  set_w_binary ((t->c_oflag & ONLCR) ? 0 : 1);

  vtime = t->c_cc[VTIME];
  vmin = t->c_cc[VMIN];

  memset (&to, 0, sizeof (to));

  to.ReadTotalTimeoutConstant = vtime * 100;

  int  res =  SetCommTimeouts (get_handle (), &to);
  if (!res)
    {
      small_printf ("CommTimeout failed\n");
      __seterrno ();
      return -1;
    }
  //  tdump (fd);
  return 0;
}

int
fhandler::tcflush (int)
{
  return -1;
}

int
fhandler::tcsetattr (int, termios const *)
{
  return -1;
}

int
fhandler::tcgetattr (termios *)
{
  return -1;
}

int
fhandler_tty::tcgetattr (struct termios *t)
{
  DCB state;
  int thisspeed;
  int thissize;

  GetCommState (get_handle (), &state);
#if 0
  ds ("In tcgetattr", &state);
#endif
  switch (state.BaudRate)
    {
    case CBR_110:
      thisspeed = B110;
      break;
    case CBR_300:
      thisspeed = B300;
      break;
    case CBR_600:
      thisspeed = B600;
      break;
    case CBR_1200:
      thisspeed = B1200;
      break;
    case CBR_2400:
      thisspeed = B2400;
      break;
    case CBR_4800:
      thisspeed = B4800;
      break;
    case CBR_9600:
      thisspeed = B9600;
      break;
    case CBR_19200:
      thisspeed = B19200;
      break;
    case CBR_38400:
      thisspeed = B38400;
      break;
    default:
      thisspeed = B9600;
      set_errno ( EINVAL);
    }

  switch (state.ByteSize)
    {
    case 5:
      thissize = CS5;
      break;
    case 6:
      thissize = CS6;
      break;
    case 7:
      thissize = CS7;
      break;
    default:
    case 8:
      thissize = CS8;
      break;
    }

  memset (t, 0, sizeof (*t));

  t->c_ospeed = t->c_ispeed = thisspeed;
  t->c_cflag |= thissize;
#if 0 /* IGNCR doesn't work yet */
  if (!get_r_binary ())
    t->c_iflag |= IGNCR;
#endif
  if (!get_w_binary ())
    t->c_oflag |= ONLCR;

  t->c_cc[VTIME] =vtime;
  t->c_cc[VMIN] = vmin;

  //  tdump (fd);
  return 0;
}

/**********************************************************************/
/* Console output stuff */

int fhandler_console_out::tcflush (int)
{
  return 0;
}

int fhandler_console_in::tcflush (int queue)
{
  int res = 0;
  if (queue == TCIFLUSH
      || queue == TCIOFLUSH)
    {
      if (!FlushConsoleInputBuffer (get_handle ()))
	{
	  __seterrno ();
	  res = -1;
	}
    }
  return res;
}

int fhandler_console_out::tcsetattr (int, termios const *t)
{
  /* Ignore the optional_actions stuff, since all output is emitted
     instantly */

  int flags = 0;

  /* Enable/disable LF -> CRLF conversions */
  set_w_binary ( (t->c_oflag & ONLCR) ? 0 : 1);   

  /* All the output bits we can ignore */

  flags |= ENABLE_PROCESSED_OUTPUT | ENABLE_WRAP_AT_EOL_OUTPUT;

  int  res = SetConsoleMode (get_handle (), flags) ? 0 : -1;
  syscall_printf ("%d = tcsetattr (console_out) (,%x) (ENABLE FLAGS %x) (c_lflag %x)\n", res, t, flags, t->c_lflag);
  return res;  
}

int
fhandler_console_in::tcsetattr (int, termios const *t)
{
  /* Ignore the optional_actions stuff, since all output is emitted
     instantly */

  int flags = 0;

  /* Enable/disable LF -> CRLF conversions */
  set_w_binary ( (t->c_oflag & ONLCR) ? 0: 1);   
  set_r_binary ( (t->c_iflag & IGNCR) ? 0 : 1);

  /* There's some disparity between what we need and what's
     available.  We've got ECHO and ICANON, they've
     got ENABLE_ECHO_INPUT and ENABLE_LINE_INPUT. */
     
  iflag = t->c_iflag;
  lflag = t->c_lflag;

  if (t->c_lflag & ECHO)
    {
      flags |= ENABLE_ECHO_INPUT;
    }
  if (t->c_lflag & ICANON)
    {
      flags |= ENABLE_LINE_INPUT;
    }

  if (flags & ENABLE_ECHO_INPUT
      && !(flags & ENABLE_LINE_INPUT))
    {
      /* This is illegal, so turn off the echo here, and fake it
	 when we read the characters */

      flags &= ~ENABLE_ECHO_INPUT;
    }

  if (t->c_lflag & ISIG)
    {
      flags |= ENABLE_PROCESSED_INPUT;
    }
  /* What about ENABLE_WINDOW_INPUT
     and ENABLE_MOUSE_INPUT   ? */

  int res = SetConsoleMode (get_handle (), flags) ? 0 : -1;
  if (res < 0)
    __seterrno ();
  syscall_printf ("%d = tcsetattr (console_in) (,%x) (ENABLE FLAGS %x) (c_lflag %x iflag %x)\n", res, t, flags, t->c_lflag, t->c_iflag);

  return res;  
}

int
fhandler_console_in::tcgetattr (termios *t)
{
  memset (t, 0, sizeof (*t));
  
  t->c_ospeed = t->c_ispeed = B38400;
  t->c_cflag |= CS8;
  t->c_iflag |= iflag;
  t->c_lflag |= lflag;
  if (!get_w_binary ())
    t->c_oflag |= ONLCR;

  syscall_printf ("o = tcgetattr (console_in) (%x) (t->lflag %x)\n", t, t->c_lflag);
  return 0;
}

int
fhandler_console_out::tcgetattr (termios  *t)
{
  int res;
  memset (t, 0, sizeof (*t));
  
  t->c_ospeed = t->c_ispeed = B38400;
  t->c_cflag |= CS8;

#if 0 /* IGNCR doesn't work yet */
  if (!get_r_binary ())
    t->c_iflag |= IGNCR;
#endif
  if (!get_w_binary ())
    t->c_oflag |= ONLCR;

  DWORD flags;

  if (!GetConsoleMode (get_handle (), &flags))
    {
      __seterrno ();
      res = -1;
    }
  else
    {
      if (flags & ENABLE_ECHO_INPUT)
	t->c_lflag |= ECHO;

      if (flags & ENABLE_LINE_INPUT)
	t->c_lflag |= ICANON;

      if (flags & ENABLE_PROCESSED_INPUT)
	t->c_iflag |= ISIG;

      /* What about ENABLE_WINDOW_INPUT
	 and ENABLE_MOUSE_INPUT   ? */

      /* All the output bits we can ignore */
      res = 0;
    }
  syscall_printf ("%d = tcgetattr (console_out) (%x) (win32 ENABLE_ %x) (t->lflag %x)\n", res, t, flags, t->c_lflag);
  return res;  
}

fhandler_console_out::fhandler_console_out () 
{
  state = normal;
}

void
fhandler_console_out::clear_screen ()
{
  CONSOLE_SCREEN_BUFFER_INFO info;
  COORD tlc = {0,0};
  DWORD done;
  GetConsoleScreenBufferInfo (get_handle (), &info);
  FillConsoleOutputCharacterA (get_handle (), ' ',
			       info.dwSize.X * info.dwSize.Y,
			       tlc,
			       &done);
}

void
fhandler_console_out::clear_to_eol ()
{
  CONSOLE_SCREEN_BUFFER_INFO info;
  DWORD done;
  GetConsoleScreenBufferInfo (get_handle (), &info);
  FillConsoleOutputCharacterA (get_handle (), ' ',
			       info.dwSize.X - info.dwCursorPosition.X,
			       info.dwCursorPosition,
			       &done);
}

void
fhandler_console_out::cursor_set (int x, int y)
{
  CONSOLE_SCREEN_BUFFER_INFO info;
  GetConsoleScreenBufferInfo (get_handle (), &info);

  COORD pos;

  if (y > info.dwSize.Y) 
    y = info.dwSize.Y-1;
  else if (y < 0)
    y = 0;

  if (x > info.dwSize.X) 
    x = info.dwSize.X-1;
  else   if (x < 0)
    x = 0;

  pos.X = x;
  pos.Y = y;
  SetConsoleCursorPosition (get_handle (), pos);
}

void
fhandler_console_out::cursor_rel (int x, int y)
{
  CONSOLE_SCREEN_BUFFER_INFO info;
  GetConsoleScreenBufferInfo (get_handle (), &info);

  x += info.dwCursorPosition.X;
  y += info.dwCursorPosition.Y;
  cursor_set (x,y);
}

#define BAK 1
#define ESC 2
#define NOR 0
#define IGN 4
#define ERR 5
#define DWN 6
#define BEL 7
#define TAB 8 /* We let the console deal with these */
#define CR 13
#define LF 10

static const char base_chars[256] =
{
/*00 01 02 03 04 05 06 07 */ IGN, ERR, ERR, ERR, ERR, ERR, ERR, BEL, 
/*08 09 0A 0B 0C 0D 0E 0F */ BAK, NOR, DWN, ERR, ERR, CR,  ERR, IGN, 
/*10 11 12 13 14 15 16 17 */ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, 
/*18 19 1A 1B 1C 1D 1E 1F */ ERR, ERR, ERR, ESC, ERR, ERR, ERR, ERR, 
/*   !  "  #  $  %  &  '  */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*(  )  *  +  ,  -  .  /  */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*0  1  2  3  4  5  6  7  */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*8  9  :  ;  <  =  >  ?  */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*@  A  B  C  D  E  F  G  */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*H  I  J  K  L  M  N  O  */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*P  Q  R  S  T  U  V  W  */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*X  Y  Z  [  \  ]  ^  _  */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*`  a  b  c  d  e  f  g  */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*h  i  j  k  l  m  n  o  */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*p  q  r  s  t  u  v  w  */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*x  y  z  {  |  }  ~  7F */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*80 81 82 83 84 85 86 87 */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*88 89 8A 8B 8C 8D 8E 8F */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*90 91 92 93 94 95 96 97 */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*98 99 9A 9B 9C 9D 9E 9F */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*A0 A1 A2 A3 A4 A5 A6 A7 */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*A8 A9 AA AB AC AD AE AF */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*B0 B1 B2 B3 B4 B5 B6 B7 */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*B8 B9 BA BB BC BD BE BF */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*C0 C1 C2 C3 C4 C5 C6 C7 */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*C8 C9 CA CB CC CD CE CF */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*D0 D1 D2 D3 D4 D5 D6 D7 */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*D8 D9 DA DB DC DD DE DF */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*E0 E1 E2 E3 E4 E5 E6 E7 */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*E8 E9 EA EB EC ED EE EF */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*F0 F1 F2 F3 F4 F5 F6 F7 */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR, 
/*F8 F9 FA FB FC FD FE FF */ NOR, NOR, NOR, NOR, NOR, NOR, NOR, NOR };

/*#define syscall_printf small_printf*/

void
fhandler_console_out::char_command (char c)
{
  if (arg1 == 0)
    arg1 = 1;
  if (arg2 == 0)
    arg2 = 1;
  switch (c)
    {
    case 'm':
      break;
    case 'h':
    case 'l':
      /* Ignore */
      break;
    case 'J':
      switch (arg1)
	{
	case 2:
	default:
	  clear_screen ();
	  cursor_set (0,0);
	  break;
	}
      break; 

    case 'A':  cursor_rel (0, -arg1);   break;
    case 'B':  cursor_rel (0, arg1);    break;
    case 'C':  cursor_rel (arg1, 0);     break;
    case 'D':  cursor_rel (-arg1,0);    break;
    case 'K':
      clear_to_eol ();
      break;
    case 'H':
    case 'f':
      cursor_set (arg2 -1, arg1 -1);
      break;
    default:
      small_printf ("Bad escape %d, %d %d (%c)\n", arg1, arg2, c,c);
      sleep (1);
      break;
    }
}

const unsigned char *
fhandler_console_out::write_normal (const unsigned char *src,
						const unsigned char *end)
{
  /* Scan forward to see what a char which needs special treatment */
  DWORD done;
  const unsigned char *found = src;
  while (found < end)
    {
      if (base_chars[*found] != NOR)
	break;
      found++;
    }
  /* Print all the base ones out */
  if (found != src) 
    {
      if (! WriteFile (get_handle (), src,  found - src, &done, 0))
	{
	  __seterrno ();
	  return 0;
	}
      src += done;
    }
  if (src < end) 
    {
      switch (base_chars[*src]) 
	{
	case BEL:
	  Beep (412, 100);
	  break;
	case ESC:
	  state = gotesc;
	  break;
	case DWN:
	  if (get_w_binary ())
	    cursor_rel (0, 1);
	  else
	    WriteFile (get_handle (), "\r\n", 2, &done, 0);
	  break;
	case BAK:
	  cursor_rel (-1, 0);
	  break;
	case IGN:
	  cursor_rel (0, 1);
	  break;
	case CR:
	  WriteFile (get_handle (), "\r", 1, &done, 0);
	  break;
	case ERR:
	  small_printf ("Got %d\n", *src);
	  break;
	}
      src ++;
    }
  return src;
}

int
fhandler_console_out::write ( const void *vsrc, size_t len)
{
  /* Run and check for ansi sequences */
  unsigned const  char *src = (unsigned char *)vsrc;
  unsigned  const char *end = src + len;

  while (src < end)
    {
      //      small_printf ("at %d state is %d\n", *src, state);
      switch (state)
	{
	case normal:
	  src = write_normal (src, end);
	  break;
	case gotesc:
	  if (*src == '[')
	    {
	      state = gotsquare;
	      arg1 = 0;
	      arg2 = 0;
	    }
	  else
	    {
	      state = normal;
	    }
	  src++;
	  break;
	case gotarg1:
	  if (isdigit (*src))
	    {
	      arg1 = arg1 * 10 + *src - '0';
	      src ++;
	    }
	  else if (*src == ';')
	    {
	      src++;
	      state = gotarg2;
	    }
	  else
	    {
	      state = gotcommand;
	    }
	  break;
	case gotarg2:
	  if (isdigit (*src))
	    {
	      arg2 = arg2 * 10 + *src - '0';
	      src ++;
	    }
	  else
	    {
	      state = gotcommand;
	    }
	  break;
	case gotcommand:
	  char_command (*src++);
	  state= normal;
	  break;
	case gotsquare:
	  if (*src == ';') 
	    {
	      state = gotarg2;
	      src++;
	    }
	  else if (isalpha (*src))
	    {
	      state = gotcommand;
	    }
	  else if (!isalpha (*src) && !isdigit (*src))
	    {
	      /* ignore any extra chars between [ and the first arg or command */
	      src++;
	    }
	  else 
	    state = gotarg1;
	  break;
	}
    }
  syscall_printf ("%d = write_console (,..%d)\n", len, len);
  
  return len;
}

static void
dbg_input_event (INPUT_RECORD*input_rec)
{
  select_printf ("polled: PeekConsoleInput, type 0x%x key: dn %d, st 0x%x\n",
	      input_rec->EventType, 
	      input_rec->Event.KeyEvent.bKeyDown,
	      input_rec->Event.KeyEvent.dwControlKeyState);
  select_printf ("polled: ... rpt %d vkc 0x%x vsc 0x%x ascii 0x%x\n",
	      input_rec->Event.KeyEvent.wRepeatCount,
	      input_rec->Event.KeyEvent.wVirtualKeyCode,
	      input_rec->Event.KeyEvent.wVirtualScanCode,
	      input_rec->Event.KeyEvent.AsciiChar);
}

static int
FakeReadFile (HANDLE hndl, void* pv, size_t lenin, int* done, 
			 OVERLAPPED *ov)
{
  DWORD flags;
  int res;
  int need_chars = 1;
  int copied_chars = 0;
  char *buf;
  res = GetConsoleMode (hndl, &flags);

  /* if things are special, just do what we used to */
  if ((!res) 
      || (flags & ENABLE_LINE_INPUT) 
      || (hndl != GetStdHandle (STD_INPUT_HANDLE))
      || (ov != 0))
        {
          return ReadFile (hndl, pv, lenin, done, ov);
        }

  /* otherwise, do something that works */
  int num_events = 0, ne2, st;

  st = GetNumberOfConsoleInputEvents (hndl, &num_events);
  if (!st) {
    /* it failed, we're confused */
    return 0;			/* seems to be failure */
  }
  if (num_events == 0) {
    select_printf ("fhandler_console_in::FakeReadFile: gnocie found no events\n");
    /* so are we blocking or what? FIONBIO isn't implemented... */
    /* either spin here and read at least one thing, return none... */
    /* readfile blocks already, so we probably can't do worse... */
    need_chars = 1;
  } 

  INPUT_RECORD input_rec;
  
  buf = (char*)pv;
  while (need_chars || num_events) {
    st = ReadConsoleInput (hndl, &input_rec, 1, &ne2);
    if (!st) {
      /* it failed, we're confused */
      return 0;			/* seems to be failure */
    }
    /* doc says it will return at least one event... */
    num_events--;
    /* check if we're just disposing of this one */
    if (input_rec.Event.KeyEvent.AsciiChar == 0)
      continue;
    if (input_rec.Event.KeyEvent.bKeyDown == 0)
      continue;
    /* keep it */
    buf[copied_chars++] = input_rec.Event.KeyEvent.AsciiChar;
    if (copied_chars >= lenin) {
      /* we got all we could handle */
      num_events = 0;
    }
    need_chars = 0;    
  }
  *done = copied_chars;
  return 1;			/* success == true */
}

int
fhandler_console_in::read (void *pv, size_t lenin)
{
  char *buf = (char *)pv;
  int res;
  DWORD done;
  
  DWORD flags;
  res = GetConsoleMode (get_handle (), &flags);
  syscall_printf ("fhandler_console_in::read (flags (%d)->%d/0x%x)\n", 
		    get_handle (), res, flags);

  int num_events = 0, ne2;
  if (GetNumberOfConsoleInputEvents (GetStdHandle (STD_INPUT_HANDLE), 
			   &num_events))
    {
      if (num_events == 0)
	{
	  select_printf ("fhandler_console_in::read: gnocie found no events\n");
	}
      else
	{
	  INPUT_RECORD input_rec[128];
	  select_printf ("fhandler_console_in::read: getnumberofconsoleinputevents->%d\n", num_events);

	  if (PeekConsoleInput (GetStdHandle (STD_INPUT_HANDLE), 
				      input_rec, 128, &ne2))
	    {
	      int i;
	      select_printf ("fhandler_console_in::read PeekConsoleInput got %d records\n", ne2);
	      for (i = 0; i < ne2; i++)
		  {
		    dbg_input_event (input_rec+i);
		  }	
	    }
	}
    }

  syscall_printf ("read console, r_binary %d\n", get_r_binary ());
  syscall_printf ("args %x %x %x %x %d\n",
		 get_handle (), pv, lenin, &done, 0);
  syscall_printf ("Console handle is %d\n", GetStdHandle (STD_INPUT_HANDLE));

  if (GetStdHandle (STD_INPUT_HANDLE) != get_handle ())
    {
      syscall_printf ("Guessed wrong console direction\n");
      set_handle (GetStdHandle (STD_INPUT_HANDLE));
    }

  if (!FakeReadFile (get_handle (),
		 pv, lenin, &done, 0))
    {
      syscall_printf ("rf fail\n");
      __seterrno ();
      res = -1;
    }
  else 
    {
      int len = done;
      DWORD flags;

      GetConsoleMode (get_handle (), &flags);
      syscall_printf ("readfile returned - len %d (flags 0x%x)\n", len, flags);
      if (! (flags &ENABLE_LINE_INPUT))
	{
	  /* There are nulls in the stream with this ??!! */
	  char *dst = (char *)pv;
	  
	  char *src = (char *)pv;
	  for (int j = 0; j < len; j++)
	    {
	      char c = *src++;
	      if (c)
		*dst++ = c;
	    }
	  len = (char *)dst - (char *)pv;
	}
      
      syscall_printf ("rf fail 1\n");
      /* Do magic transforms */
      
      for (int j = 0; j < len; j++)
	{
	  syscall_printf ("in %d %d\n", j, buf[j]);
	}

      syscall_printf ("rf fail 2\n");

      /* The console will always return CRLF for LF, and we don't want
	 the CR.  !!This should be done as a ReadConsoleInput loop one day */
      syscall_printf ("rf fail 4\n");

      /* if (igncr_enabled ()) -- WRONG CASE... */
      if (flags & ENABLE_LINE_INPUT)
	{
	  char *src = buf;
	  char *dst = buf;
          syscall_printf ("rf fail 5 %d\n", len);

	  for (int i =0; i < len ; i++)
	    {
	      /* only make CRNL -> NL, don't just nuke CR! */
	      if (*src != '\r')
		*dst++ = *src;
	      else if ((i < len) && (src[1] != '\n'))
		*dst++ = *src;
	      src++;
	    }
	  len = dst - buf;
          syscall_printf ("rf fail len set to  %d\n", len);
	}
      else if (iflag & ICRNL)
	{
	  /* Turn CR into NL */
	  for (int i= 0; i < len; i++)
	    {
	      if (buf[i] == '\r')
		buf[i] = '\n';
	    }
	}

      syscall_printf ("rf fail 3\n");
      if (iflag & INLCR)
	{
	  /* Turn NL into CR */
	  for (int i= 0; i < len; i++)
	    {
	      if (buf[i] == '\n')
		buf[i] = '\r';
	    }
	}
      
      syscall_printf ("rf fail 5\n");
      if (iflag & IUCLC)
	{
	  /* Uppercase to lowercase */
	  for (int i = 0; i <len; i++)
	    {
	      if (isupper (buf[i]))
		buf[i] = tolower (buf[i]);
	    }
	}
      res = len;
    }

  if (res > 0) 
    {
      for (int j = 0; j < res; j++)
	syscall_printf ("read %d %c\n", j, buf[j]);  
      
      if (lflag  & ECHO
	  && !(lflag &  ICANON))
	{
	  WriteFile (get_handle (), pv, res, &done, 0);
	}
    }
  
  syscall_printf ("%d = read (console) (%x, %d);\n", res, pv, lenin);
  return res;
}

int
fhandler_console_out::read (void *pv, size_t len)
{
  small_printf ("!! reading through an output console !\n");
  return fhandler_base::read (pv, len);
}

HANDLE
fhandler_base::get_handle ()
{
  return handle;
}

int
fhandler::get_access ()
{
  return 0;
}

void
fhandler::set_access (int)
{
  return;
}

int
fhandler_base::get_access ()
{
  return access;
}

void
fhandler_base::set_access (int x)
{
  access = x;
}

void
fhandler::set_r_binary (int x)
{
  r_binary = x;
}

void
fhandler::set_w_binary (int x)
{
  w_binary = x;
}

int
fhandler::get_r_binary ()
{
  return r_binary;
}

int
fhandler::get_w_binary () { return w_binary; }

int
fhandler::is_tty () { return 0;}

int
fhandler_tty::is_tty () { return 1;}

void
fhandler_console_in::init (HANDLE f, int bin, int a, const char *n)
{
  termios t;
  fhandler_tty::init (f,bin,a,n);
  
  /* IGNCR doesn't work yet */
  t.c_iflag = ICRNL /* | IGNCR */;
  t.c_lflag = ECHO | ICANON | ISIG;
  tcsetattr (0, &t);
}

int
fhandler_console_in::igncr_enabled ()
{
  return iflag & IGNCR;
}

fhandler *
fhandler_dev_floppy::open (const char *path, int flags)
{
  /* Always open a floppy existings */
  return fhandler_base::open (path, flags & ~O_CREAT);
}

fhandler *
fhandler_dev_tape::open (const char *path, int flags)
{
  /* Always open a tape existings */
  return  fhandler_base::open (path, flags & ~O_CREAT);
}

void
fhandler_socket::setup (unsigned int s)
{
  socket = s;
  debug_printf ("socket id %d\n", s);
}

int
fhandler_socket::get_socket ()
{
  return socket;
}

fhandler_socket *
fhandler::is_socket ()
{
  return 0;
}

fhandler_socket *
fhandler_socket::is_socket () 
{
  return this;
}

const char *
fhandler::get_name ()
{
  return "root";
}

const int
fhandler::always_ready ()
{
  return 1;
}

const int
fhandler_console_in::always_ready ()
{
  return 0;
}
