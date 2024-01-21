/* syscalls for WIN32.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <sys/cygwin.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <pwd.h>
#include <unistd.h>

#include "winsup.h"

/* Table to map Windows error codes to Errno values.  */
/* ??? Doing things this way is a little slow.  It's trivial to change this
   into a big case statement if necessary so it's left as is for now.  */

static const struct
  {
    int w;		 /* windows version of error */
    const char *s;	 /* text of windows version */
    int e;		 /* errno version of error */
  }
errmap[] =
{
  /* ??? Some of these choices are arbitrary.  */
  { ERROR_INVALID_FUNCTION, "ERROR_INVALID_FUNCTION", EBADRQC  },
  { ERROR_FILE_NOT_FOUND, "ERROR_FILE_NOT_FOUND", ENOENT  },
  { ERROR_PATH_NOT_FOUND, "ERROR_PATH_NOT_FOUND", ENOENT  },
  { ERROR_TOO_MANY_OPEN_FILES, "ERROR_TOO_MANY_OPEN_FILES", EMFILE  },
  { ERROR_ACCESS_DENIED, "ERROR_ACCESS_DENIED", EACCES  },
  { ERROR_INVALID_HANDLE, "ERROR_INVALID_HANDLE", EBADF  },
  { ERROR_NOT_ENOUGH_MEMORY, "ERROR_NOT_ENOUGH_MEMORY", ENOMEM  },
  { ERROR_INVALID_DATA, "ERROR_INVALID_DATA", EINVAL  },
  { ERROR_OUTOFMEMORY, "ERROR_OUTOFMEMORY", ENOMEM  },
  { ERROR_INVALID_DRIVE, "ERROR_INVALID_DRIVE", ENODEV  },
  { ERROR_NOT_SAME_DEVICE, "ERROR_NOT_SAME_DEVICE", EXDEV  },
  { ERROR_NO_MORE_FILES, "ERROR_NO_MORE_FILES", ENMFILE  },
  { ERROR_WRITE_PROTECT, "ERROR_WRITE_PROTECT", EROFS  },
  { ERROR_BAD_UNIT, "ERROR_BAD_UNIT", ENODEV  },
  { ERROR_SHARING_VIOLATION, "ERROR_SHARING_VIOLATION", EACCES  },
  { ERROR_LOCK_VIOLATION, "ERROR_LOCK_VIOLATION", EACCES  },
  { ERROR_SHARING_BUFFER_EXCEEDED, "ERROR_SHARING_BUFFER_EXCEEDED", ENOLCK  },
  { ERROR_HANDLE_EOF, "ERROR_HANDLE_EOF", ENODATA  },
  { ERROR_HANDLE_DISK_FULL, "ERROR_HANDLE_DISK_FULL", ENOSPC  },
  { ERROR_NOT_SUPPORTED, "ERROR_NOT_SUPPORTED", ENOSYS  },
  { ERROR_REM_NOT_LIST, "ERROR_REM_NOT_LIST", ENONET  },
  { ERROR_DUP_NAME, "ERROR_DUP_NAME", ENOTUNIQ  },
  { ERROR_BAD_NETPATH, "ERROR_BAD_NETPATH", ENXIO  },
  { ERROR_FILE_EXISTS, "ERROR_FILE_EXISTS", EEXIST  },
  { ERROR_CANNOT_MAKE, "ERROR_CANNOT_MAKE", EPERM  },
  { ERROR_INVALID_PARAMETER, "ERROR_INVALID_PARAMETER", EINVAL  },
  { ERROR_NO_PROC_SLOTS, "ERROR_NO_PROC_SLOTS", EAGAIN  },
  { ERROR_BROKEN_PIPE, "ERROR_BROKEN_PIPE", EPIPE  },
  { ERROR_OPEN_FAILED, "ERROR_OPEN_FAILED", EIO  },
  { ERROR_NO_MORE_SEARCH_HANDLES, "ERROR_NO_MORE_SEARCH_HANDLES", ENFILE  },
  { ERROR_CALL_NOT_IMPLEMENTED, "ERROR_CALL_NOT_IMPLEMENTED", ENOSYS  },
  { ERROR_INVALID_NAME, "ERROR_INVALID_NAME", ENOENT  },
  { ERROR_WAIT_NO_CHILDREN, "ERROR_WAIT_NO_CHILDREN", ECHILD  },
  { ERROR_CHILD_NOT_COMPLETE, "ERROR_CHILD_NOT_COMPLETE", EBUSY  },
  { ERROR_DIR_NOT_EMPTY, "ERROR_DIR_NOT_EMPTY", ENOTEMPTY  },
  { ERROR_SIGNAL_REFUSED, "ERROR_SIGNAL_REFUSED", EIO  },
  { ERROR_BAD_PATHNAME, "ERROR_BAD_PATHNAME", EINVAL  },
  { ERROR_SIGNAL_PENDING, "ERROR_SIGNAL_PENDING", EBUSY  },
  { ERROR_MAX_THRDS_REACHED, "ERROR_MAX_THRDS_REACHED", EAGAIN  },
  { ERROR_BUSY, "ERROR_BUSY", EBUSY  },
  { ERROR_ALREADY_EXISTS, "ERROR_ALREADY_EXISTS", EEXIST  },
  { ERROR_NO_SIGNAL_SENT, "ERROR_NO_SIGNAL_SENT", EIO  },
  { ERROR_FILENAME_EXCED_RANGE, "ERROR_FILENAME_EXCED_RANGE", EINVAL  },
  { ERROR_META_EXPANSION_TOO_LONG, "ERROR_META_EXPANSION_TOO_LONG", EINVAL  },
  { ERROR_INVALID_SIGNAL_NUMBER, "ERROR_INVALID_SIGNAL_NUMBER", EINVAL  },
  { ERROR_THREAD_1_INACTIVE, "ERROR_THREAD_1_INACTIVE", EINVAL  },
  { ERROR_BAD_PIPE, "ERROR_BAD_PIPE", EINVAL  },
  { ERROR_PIPE_BUSY, "ERROR_PIPE_BUSY", EBUSY  },
  { ERROR_NO_DATA, "ERROR_NO_DATA", ENODATA  },
  { ERROR_PIPE_NOT_CONNECTED, "ERROR_PIPE_NOT_CONNECTED", ECOMM  },
  { ERROR_MORE_DATA, "ERROR_MORE_DATA", EAGAIN  },
  { ERROR_DIRECTORY, "ERROR_DIRECTORY", EISDIR  },
  { ERROR_PIPE_CONNECTED, "ERROR_PIPE_CONNECTED", EBUSY  },
  { ERROR_PIPE_LISTENING, "ERROR_PIPE_LISTENING", ECOMM  },
  { ERROR_NO_TOKEN, "ERROR_NO_TOKEN", EINVAL  },
  { ERROR_PROCESS_ABORTED, "ERROR_PROCESS_ABORTED", EFAULT  },
  { ERROR_BAD_DEVICE, "ERROR_BAD_DEVICE", ENODEV  },
  { ERROR_BAD_USERNAME, "ERROR_BAD_USERNAME", EINVAL  },
  { ERROR_NOT_CONNECTED, "ERROR_NOT_CONNECTED", ENOLINK  },
  { ERROR_OPEN_FILES, "ERROR_OPEN_FILES", EAGAIN  },
  { ERROR_ACTIVE_CONNECTIONS, "ERROR_ACTIVE_CONNECTIONS", EAGAIN  },
  { ERROR_DEVICE_IN_USE, "ERROR_DEVICE_IN_USE", EAGAIN  },
};

/* Set `errno' based on GetLastError ().  */
void
seterrno (const char *file, int line)
{
  int i;
  int why = GetLastError ();

  why &= 0xff;

  for (i = 0; errmap[i].w != 0; ++i)
    if (why == errmap[i].w)
      break;

  if (errmap[i].w != 0)
    {
      syscall_printf ("%s:%d seterrno: %d (%s) -> %d\n", 
		      file, line,why, errmap[i].s, errmap[i].e);
      set_errno (errmap[i].e);
    }
  else
    {
      syscall_printf ("%s:%d seterrno: unknown error %d!!\n",file,line, why);
      set_errno (EPERM);
    }
}

/* Queue a file for deletion when the next close () is done.  */
struct file_queue
{
  struct file_queue *next;
  /* The name is recorded in malloc space.  */
  char *name;
  /* Not currently used.  */
  int fd;
};

void
process_deletion_queue (void)
{
  s->delqueue.process_queue ();
}

/* Close all files and process any queued deletions.
   Lots of unix style applications will open a tmp file, unlink it,
   but never call close.  This function is called by _exit to
   ensure we don't leave any such files lying around.  */
void
close_all_files (void)
{
  int i;
  in ("close_all_files");
  MARK();

  for (i = 0; i < NOFILE; ++i)
    if (this_procinfo ()->hmap[i].is_open ()) 
      _close (i);
  MARK();
  process_deletion_queue ();
  MARK();
  out ("close_all_files");
}

int
_unlink (const char *ourname)
{
  int res;

  path_conv dos_name (ourname);
  
  syscall_printf ("_unlink (%s)\n", dos_name.get_win32 ());

  if (!DeleteFileA (dos_name.get_win32 ()))
    {
      /* If we get ERROR_SHARING_VIOLATION, the file may still be open -
	 Windows doesn't support deleting a file while it's open.  */

      res = GetLastError ();
      if (res == ERROR_SHARING_VIOLATION
	  || res == ERROR_ACCESS_DENIED)
	{
	  s->delqueue.queue_file (dos_name.get_win32 ());
	  res = 0;
	}
      else
	{
	  __seterrno ();
	  res = -1;
	}
    }
  else
    res = 0;

  syscall_printf ("%d = unlink (%s)\n", res, ourname);
  return res;
}

pid_t 
_getpid ()
{
  return u->self->get_pid ();
}

pid_t
getppid () 
{
  return u->self->ppid;
}

pid_t 
setsid (void)
{
  /* FIXME: for now */
  return _getpid ();
}

int
_read (int fd, void *ptr, size_t len)
{
  int res = -1;
  in ("_read");

  if (NOT_OPEN_FD (fd))
    {
      set_errno (EBADF);
    }
  else 
    {
      /* Could block, so let user know we at least got here.  */
      syscall_printf ("read (%d, 0x%x, %d)\n", fd, ptr, len);
      res = u->self->hmap[fd].h->read (ptr, len);
      syscall_printf ("%d = read (%d, 0x%x, %d)\n", res, fd, ptr, len);
    }
  out ("_read");
  return res;
}

int
_write (int fd, const void *ptr, size_t len)
{
  int res = -1;
  in ("_write");
  if (NOT_OPEN_FD(fd))
    {
      set_errno (EBADF);
      goto done;
    }

  /* Could block, so let user know we at least got here.  */
  if (fd == 1 || fd == 2)
    {
      paranoid_printf ("write (%d, 0x%x, %d)\n", fd, ptr, len);
    }
  else
    {
      syscall_printf  ("write (%d, 0x%x, %d)\n", fd, ptr, len);
    }

  res = u->self->hmap[fd].h->write (ptr, len);

done:
  if (fd == 1 || fd == 2) 
    {
      paranoid_printf ("%d = write (%d, 0x%x, %d)\n", res, fd, ptr, len);
    }
  else 
    {
      syscall_printf ("%d = write (%d, 0x%x, %d)\n", res, fd, ptr, len);
    }
  out ("_write");
  return res;
}

int
_open (const char *unix_path, int flags, ...)
{
  int fd;
  int res = -1;
  in ("_open");
  pinfo *p = u->self;

  syscall_printf ("open (%s, 0x%x)\n", unix_path, flags);

  fd = p->hmap.find_unused_handle (0);
  
  if (fd < 0)
    {
      set_errno (ENMFILE);
      res = -1;
      goto done;
    }

  p->hmap.build_fhandler (unix_path, fd);
  
  if ( ! p->hmap[fd].h->open (unix_path, flags)) 
    {
      p->hmap.release (fd);
    }
  else
    res = fd;
  
done:
  syscall_printf ("%d = open (%s, 0x%x)\n", res, unix_path, flags);
  out ("_open");
  return res;
}

off_t
_lseek (int fd, off_t pos, int dir)
{
  off_t res;
  in ("_lseek");
  if (NOT_OPEN_FD(fd))
    {
      set_errno (EBADF);
      res = -1;
    }
  else
    {
      res = u->self->hmap[fd].h->lseek (pos, dir);
    }
  syscall_printf ("%d = lseek (%d, %d, %d)\n", res, fd, pos, dir);
out ("_lseek");
  return res;
}

int
_close (int fd)
{
  int res;
in ("_close");  
  syscall_printf ("close (%d)\n", fd);
  
  if (NOT_OPEN_FD(fd))
    {
      debug_printf ("handle %d not open\n", fd);
      set_errno (EBADF);
      res = -1;
    }
  else
    {
      res = u->self->hmap[fd].h->close ();
      u->self->hmap.release (fd);
    }
  syscall_printf ("%d = close (%d)\n", res, fd);
  process_deletion_queue ();  
out ("_close");
  return res;
}

int
isatty (int fd)
{
  int res;

  if (NOT_OPEN_FD (fd))
    {
      syscall_printf ("-1 = isatty (%d)\n", fd);
      set_errno (EBADF);
      return -1;
    }
  
  res = u->self->hmap[fd].h->is_tty ();
  syscall_printf ("%d = isatty (%d)\n", res, fd);
  return res;
}

/* FIXME: This function needs some thought.  */

int
_link (const char *a, const char *b)
{
  in ("_link");
  int res = -1;
  path_conv real_a (a);
  path_conv real_b (b);
  
  /* do this with a copy */
  if (CopyFileA (real_a.get_win32 (), real_b.get_win32 (), 1))
    res = 0;
  else
    __seterrno ();
  
  syscall_printf ("%d = link (%s, %s)\n", res, a, b);
  out ("_link");
  return res;
}

int
mkdir (const char *dir, mode_t mode)
{
  in ("mkdir");
  int res = -1;

  path_conv real_dir (dir);
  
  if (CreateDirectoryA (real_dir.get_win32 (), 0))
    res = 0;
  else
    __seterrno ();
  
  syscall_printf ("%d = mkdir (%s, %d)\n", res, dir, mode);
  out ("mkdir");
  return res;
}

int
rmdir (const char *dir)
{
  int res = -1;
  in ("rmdir");
  path_conv real_dir (dir);
  
  if (RemoveDirectoryA (real_dir.get_win32 ()))
    res = 0;
  else
    __seterrno ();
  
  syscall_printf ("%d = rmdir (%s)\n", res, dir);
  out ("rmdir");
  return res;
}

int
chown (const char * name, uid_t , gid_t )
{
  syscall_printf ("0 = chown (%s,...);\n", name);
  return 0;
  /* this works better if it always suceeds */
}

int
chdir (const char *dir)
{
  path_conv path (dir);
  const char *native_dir = path.get_win32 ();

  int res = SetCurrentDirectoryA (native_dir);
  if (!res)
    __seterrno ();
  syscall_printf ("%d = chdir (%s) (dos %s)\n", res ? 0 : -1, dir, native_dir);
  return res ? 0 : -1;
}

mode_t 
umask (mode_t )
{
  return 0;
}

int
chmod (const char *path, mode_t mode)
{
  path_conv dos_path (path);
  int res = -1;
  int now;

  if ((now = GetFileAttributesA (dos_path.get_win32 ())) == -1)
    {
      __seterrno ();
    }
  else
    {
      /* if the mode we want has any write bits set, we can't
	 be read only. */
      if (mode & STD_WBITS)
	{
	  now &= ~FILE_ATTRIBUTE_READONLY;
	}
      else
	{
	  now |= FILE_ATTRIBUTE_READONLY;
	}

      if (SetFileAttributesA (dos_path.get_win32 (), now))
	res = 0;
      else
	__seterrno ();
    }

  syscall_printf ("%d = chmod (%s, 0x%x)\n", res, path, mode);
  return res;
}

int
_fstat (int fd, struct stat *buf)
{
  int r;

  if (NOT_OPEN_FD (fd))
    {
      syscall_printf ("-1 = fstat (%d, 0x%x)\n", fd, buf);
      set_errno ( EBADF);
      r = -1;
    }
  else
    {
      r =  u->self->hmap[fd].h->fstat (buf);
      syscall_printf ("%d = fstat (%d, %x)\n", r,fd,buf);
    }

  return r;
}

/* Some programs rely on st_dev/st_ino being unique for each file.
   Hash the path name and hope for the best.
   FIXME: Not bullet proof.  */

unsigned long
hash_path_name (const char *name)
{
  unsigned long hash = 0;
  
  while (*name != '\0') 
    {
      hash += *name + (*name <<17);
      hash ^= hash >> 2;
      name++;
    }
  return hash;
}

static int
num_entries (const char *dos_name)
{
  WIN32_FIND_DATAA buf;
  HANDLE handle;
  char buf1[MAX_PATH];
  int count = 0;
  strcpy (buf1, dos_name);
  int len = strlen (buf1);
  if (len == 0 || buf1[len - 1] == '/')
    strcat (buf1, "*");
  else
    strcat (buf1, "/*");

  handle = FindFirstFileA (buf1, &buf);

  if (handle < 0)
    return 0;
  count ++;
  while (FindNextFileA (handle, &buf))
    {
      count ++;
    }
  FindClose (handle);
  return count;
}

extern "C"
int
windows_95() 
{
    DWORD version = GetVersion ();
    if ((version & 0x8000000) && (version & 0xff) > 3)
	return 1;
    return 0;
}

int _stat_worker (const char *name, struct stat *buf, int nofollow)
{
  int res = -1;
  int atts;
  char tmp[MAXPATHLEN];
  const  char *dos_name;
  int need_exe = 0;
  path_conv real_path (name);

  debug_printf ("_stat (%s, %x)\n", name, buf);
  
  memset (buf, 0, sizeof (struct stat));

  dos_name = real_path.get_win32 ();
  atts = GetFileAttributesA (dos_name);

  if (atts == -1)
    {
      /* If we can't find the name, try again with a .exe suffix.
	 you never know.. */
      strcpy (tmp, dos_name);
      strcat (tmp, ".exe");
      need_exe = 1;
      atts = GetFileAttributesA (tmp);
      debug_printf ("%d = gfa (%s);\n", atts, tmp);
      if (atts != -1)
	dos_name = tmp;
      else
	{
	  set_errno (ENOENT);
	  goto done;
	}
    }

  if (atts & FILE_ATTRIBUTE_DIRECTORY)
    {
      /* hmm, the number of links to a directory includes the 
	 number of entries in the directory, since all the things
	 in the directory point to it */
      buf->st_nlink += 2 + num_entries (dos_name);
      buf->st_dev = 42;
      buf->st_ino = hash_path_name (real_path.get_win32 ());
      buf->st_mode = S_IFDIR | STD_RBITS | STD_WBITS | STD_XBITS;
      buf->st_uid = getuid ();
      buf->st_gid = getgid ();
      res = 0;
    }
  else
    {
      if (need_exe)
	{
	  strcpy (tmp, name);
	  strcat (tmp, ".exe");
	  name = tmp;
	}
	 
      int h = _open (name, O_RDONLY | O_BINARY | nofollow);
      if (h < 0)
	res = -1;
      else
	{
	  res = _fstat (h, buf);
	  _close (h);
	}
    }
  
 done:
  syscall_printf ("%d = stat (%s, 0x%x)\n", res, name, buf);
  return res;
}

int
_stat (const char *name, struct stat *buf)
{
  int res;
  in ("stat");
  res =  _stat_worker (name, buf, 0);
  out ("stat");
  return res;
}

int
lstat (const char *name, struct stat *buf)
{
  int res;
  in ("lstat");
  res =  _stat_worker (name, buf, O_NOSYMLINK);
  out ("lstat");
  return res;
}

int
access (const char *fn, int flags)
{
  struct stat s;
  
  if (stat (fn, &s))
    return -1;
  if (s.st_mode & S_IFDIR)
    return 0;
  if (flags & W_OK)
    {
      if (s.st_mode & S_IWRITE)
	return 0;

      /* can't write to the file */
      set_errno(EACCES);
      return -1;
    }
  return 0;
}

int
_rename (const char *oldpath, const char *newpath)
{
  in ("_rename");
  path_conv real_old (oldpath);
  path_conv real_new (newpath);
  int res;
  int oldatts = GetFileAttributesA (real_old.get_win32 ());
  int newatts = GetFileAttributesA (real_new.get_win32 ());

  if (oldatts == -1) /* file to move doesn't exist */
    {
       syscall_printf ("rename: file to move doesn't exist\n");
       return (-1);
    }

  if (newatts != -1 && newatts & FILE_ATTRIBUTE_READONLY)
    {
      /* Destination file exists and is read only, change that or else
	 the rename wont work. */
      SetFileAttributesA (real_new.get_win32 (), newatts & ~ FILE_ATTRIBUTE_READONLY);
    }

  /* First make sure we have the permissions */
  if (!MoveFileEx (real_old.get_win32 (), real_new.get_win32 (), MOVEFILE_REPLACE_EXISTING))
    {
      res = -1;

      /* !!! fixme, check for windows version before trying this.. */
      if (GetLastError () == ERROR_CALL_NOT_IMPLEMENTED)
	{
	  /* How sad, we must be on win95, try it the stupid way */
	  syscall_printf ("rename: try win95 hack\n");
	  for (;;)
 	    {
	      if (MoveFile (real_old.get_win32 (), real_new.get_win32 ()))
 		{
		  res = 0;
		  break;
		}
	      
	      if (GetLastError () != ERROR_ALREADY_EXISTS)
 		break;
	      
	      syscall_printf ("rename: %s already_exists\n", real_new.get_win32 ());
 	      
	      if (!DeleteFileA (real_new.get_win32 ()) &&
		  GetLastError () != ERROR_FILE_NOT_FOUND)
 	        break;
	    }
	}
      if (res)
	__seterrno ();
    }

  res = 0;

  if (res == 0)
    {
      /* make the new file have the permissions of the old one */
      SetFileAttributesA (real_new.get_win32 (), oldatts);
    }
  syscall_printf ("%d = rename (%s, %s)\n",res, real_old.get_win32 (), real_new.get_win32 ());
  out ("_rename");
  return res;
}

int
system (const char *cmdstring)
{
  pid_t pid;
  int res;

  if (cmdstring == (const char *)NULL)
        return 1;
   
  if ((pid = fork()) < 0)
        return -1;

  if (pid == 0)
    {
      execlp("sh", "-c", cmdstring, (char *)NULL);
      exit(127);
    }
  else
    {
      while (waitpid(pid, &res, 0) < 0)
	{	
	  if (get_errno() != EINTR)
	    return -1;
        }
    }

  return res;
}

int
getdtablesize ()
{
  return NOFILE;
}

int
gethostname (char *name, size_t len)
{
  DWORD local_len = len;

  if (!GetComputerNameA (name, &local_len))
    {
      __seterrno ();
      return -1;
    }

  return 0;
}

size_t
getpagesize ()
{
  return sysconf (_SC_PAGESIZE);
}

long int
pathconf (char *file, int v) 
{
  switch (v)
    {
    case    _PC_PATH_MAX:
      return PATH_MAX - strlen (file);
      break;
    default:
      set_errno (EINVAL);
      return -1;
    }
}

char
*ttyname (int fd)
{
  if (NOT_OPEN_FD (fd))
    {
      set_errno (EBADF);
      return 0;
    }
  return (char *)(u->self->hmap[fd].h->ttyname ());
}

int
_setmode (int fd, int mode)
{
  if (NOT_OPEN_FD(fd))
    {
      set_errno (EBADF);
      return -1;
    }
  if (mode != O_BINARY  && mode != O_TEXT)
    {
      set_errno (EINVAL);
      return -1;
    }
  fhandler *p = u->self->hmap[fd].h;
  if (mode & O_BINARY) 
    {
      p->set_w_binary (1);
      p->set_r_binary (1);
    }
  else 
    {
      p->set_w_binary (0);
      p->set_r_binary (0);

    }
  return 0;
}

int
setmode (int fd, int mode)
{
  return _setmode (fd, mode);
}

int
ftruncate (int fd, size_t length) 
{
  int res = -1;
  in ("ftruncate");
  if (NOT_OPEN_FD(fd))
    {
      set_errno ( EBADF);
    }
  else
    {
      HANDLE h =u->self->hmap[fd].h->get_handle ();

      if (h)
	{
	  u->self->hmap[fd].h->lseek (length, 0);
	  if (!SetEndOfFile (h))
	    {
	      __seterrno ();
	    }
	  else
	    res = 0;
	}
    }
  syscall_printf ("%d = ftruncate (%d, %d); \n", fd, length);

  out ("ftruncate");
  return res;
}

int
cygwin_set_attributes (int what, int val)
{
#if 0
  switch (what)
    {

    case CYGWIN_FMODE_ALL_BINARY:
      {
	reg_session reg;
	s->master_fmode_binary = val;
	reg.fillone_int (&(s->master_fmode_binary), "fmode=binary", val);
      }
    }
#endif
  
  return 0;
}
