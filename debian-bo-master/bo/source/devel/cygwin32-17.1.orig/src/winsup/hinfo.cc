/* File descriptor support.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
  */

#include "winsup.h"

/* Initialize the file descriptor/handle mapping table.
   We only initialize the parent table here.  The child table is
   initialized at each fork () call.  */

void hmap_init ()
{
  MARK();
  pinfo *p = this_procinfo ();
  MARK();

  /* Set these before trying to output anything from strace.
     Also, always set them even if we're to pick up our parent's fds
     in case they're missed.  */

  if (p->ppid == p->pid) 
    {
      MARK();

      p->hmap.init_std_file_from_handle (0,
					GetStdHandle (STD_INPUT_HANDLE),
					0,
					GENERIC_READ,
					"{stdin}");

      /* STD_ERROR_HANDLE has been observed to be the same as
	 STD_OUTPUT_HANDLE.  We need separate handles (e.g. using pipes
	 to pass data from child to parent).  */
      HANDLE out = GetStdHandle (STD_OUTPUT_HANDLE);
      HANDLE err = GetStdHandle (STD_ERROR_HANDLE);
      const HANDLE proc = GetCurrentProcess ();
      if (out == err)
	{
	  /* Since this code is not invoked for fork'd tasks, we don't have
	     to worry about the close-on-exec flag here.  */
	  if (! DuplicateHandle (proc, out, proc, &err, 0,
				 1, DUPLICATE_SAME_ACCESS))
	    {
	      /* If that fails, do this as a fall back.  */
	      err = out;
	      small_printf ("Couldn't make stderr distinct from stdout!\n");
	    }
	}

      p->hmap.init_std_file_from_handle (1, out, 0, GENERIC_WRITE, "{stdout}");

      p->hmap.init_std_file_from_handle (2, err, 0, GENERIC_WRITE, "{stderr}");
    }

  if (0)
    {
      /* See if we can consoleify it */
      CONSOLE_SCREEN_BUFFER_INFO buf;
      if (GetConsoleScreenBufferInfo (GetStdHandle (STD_OUTPUT_HANDLE), 
				  &buf))
	{
	 
	  small_printf ("Handle for output is a console\n");
	}
    }

}




int hinfo_vec::not_open (int fd)
{
  int res = fd < 0 || fd > NOFILE || vec[fd].h == 0;
  return res;
}


hinfo & hinfo_vec::operator [] (int arg)
{
  return vec[arg];
}


int hinfo_vec::find_unused_handle (int start)
{
  int i;
  for (i = start; i < NOFILE; i++)
    if (vec[i].h == 0)
      return i;
  return -1;
}



void hinfo_vec::release (int fd)
{
  vec[fd].h = 0;
}

void hinfo_vec::clearout ()
{
  for (int i = 0; i < NOFILE; i++)
    {
      release (i);
    }
}


void 
hinfo_vec::init_std_file_from_handle (int fd, HANDLE handle, int bin, int access, const char *name)
{
  /* Check to see if we're being redirected - if not then
     we open then as consoles */
#if 0
  if (s->master_fmode_binary) 
    {
      bin = 1;
    }
#endif


  if (fd == 0 || fd == 1 || fd == 2)
    {
      /* See if we can consoleify it  - if it is a console,
       don't open it in binary.  That will screw up our crlfs*/
      CONSOLE_SCREEN_BUFFER_INFO buf;
      if (GetConsoleScreenBufferInfo (handle, &buf))
	{
	  bin = 0;
	  name = "/dev/conout";
	}
      if (FlushConsoleInputBuffer (handle))
	{
	  bin = 0;
	  name = "/dev/conin";
	}
    }
  MARK();

  fhandler *res = build_fhandler (name, fd);

  res->init (handle, bin, access, name);

  paranoid_printf ("init_std_file_from %d\n", handle);
}



fhandler * hinfo_vec::build_fhandler (const char *name, int fd)
{
  if (strcmp (name,"/dev/conin") == 0)
    {
      vec[fd].h = new (&vec[fd].item) fhandler_console_in;
    }
  else  if (strcmp (name,"/dev/conout") == 0)
    {
      vec[fd].h = new (&vec[fd].item) fhandler_console_out;
    }
  else if (strcmp (name,"/dev/tty") == 0)
    {
      vec[fd].h = new (&vec[fd].item) fhandler_console_in;
    }
  else if (strcmp (name,"/dev/null") == 0)
    {
      vec[fd].h = new (&vec[fd].item) fhandler_dev_null;
    }
  else if (strcmp (name,"/dev/null") == 0)
    {
      vec[fd].h = new (&vec[fd].item) fhandler_dev_null;
    }
  else if (strcmp (name,"/dev/fd0") == 0
	   || strcmp (name,"/dev/fd1") == 0)
    {
      vec[fd].h = new (&vec[fd].item) fhandler_dev_floppy;
    }
  else if (strcmp (name,"/dev/st0") == 0
	   || strcmp (name,"/dev/st1") == 0)
    {
      vec[fd].h = new (&vec[fd].item) fhandler_dev_tape;
    }
  else if (strncmp (name,"com1",4)  == 0
	   || strncmp (name,"com2",4)  == 0)
    {
      vec[fd].h = new (&vec[fd].item) fhandler_tty;
    }
  else 
    {
      vec[fd].h = new (&vec[fd].item) fhandler_disk_file;
    }
  return vec[fd].h;
}

/* We're about to turn this process into another one.  */

void hinfo_vec::dup_for_exec ()
{
  for (int i = 0; i < NOFILE; i++) 
    {
      if (vec[i].is_open ())
	{
	  if (vec[i].h->close_exec_p)
	    _close (i);
	}
    }
}


/* We're copying the state from this process to another one,
   Duplicate all the handles we have open into the space
   of the new child.

   The vec is that of the parents's, and we're passed the child's vec. */

void hinfo_vec::dup_for_fork (hinfo_vec *child)
{
  /* Copy the hmap info from the parent. */
  debug_printf ("dup_for_fork\n");

  for (int i = 0; i < NOFILE; i++) 
    {
      /* Do a bitwise copy */
      if (vec[i].h) 
	{
	  child->vec[i].h = &child->vec[i].item;
	  memcpy (child->vec[i].h, vec[i].h,  sizeof (vec[i].item));
	}
      else 
	{
	  child->vec[i].h = 0;
	}
    }
}


int hinfo_vec::dup2 (int oldfd, int newfd)
{
  int res = 1;

  debug_printf ("dup2 (%d, %d);\n", oldfd, newfd);
  
  if (not_open (oldfd) || newfd < 0 || newfd >= NOFILE)
    {
      set_errno ( EBADFD);
      goto done;
    }
  
  if (newfd == oldfd)
    {
      res = newfd;
      goto done;
    }
  
  if (vec[newfd].h)
    {
      _close (newfd);
    }
  
  vec[newfd].h = &vec[newfd].item;
  vec[newfd].item = vec[oldfd].item;
  vec[oldfd].h->dup (vec[newfd].h);
  
  res = newfd;

 done:
  if (res >= 0)
    {
      syscall_printf ("%d = dup2 (%d, %d) (%d:%d)\n",
		     res, oldfd, newfd, res,
		     vec[res].h->get_handle ());
    }
  else
    {
      syscall_printf ("%d = dup2 (%d, %d)\n", res, oldfd, newfd);
    }

  MARK();
  return res;
}



void * fhandler::operator new (size_t size, void *p)
{
  if (size > sizeof (fhandler_union)) 
    {
      small_printf ("fhandler_union too small!\n");
    }
  return p;
}
