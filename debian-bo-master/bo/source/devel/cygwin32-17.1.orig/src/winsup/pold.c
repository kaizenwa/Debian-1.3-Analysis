/* pipe for WIN32.

   Written by Doug Evans and Steve Chamberlain of Cygnus Support.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */



#include <unistd.h>
#include "fcntl.h"
#include "stdarg.h"
#include "syscalls.h"


int
pipe (int fildes[2])
{
  HANDLE r, w;
  int  fdr, fdw;

  fdr = __find_unused_handle ();
  if (fdr < 0) 
    set_errno ( ENMFILE);
  else 
    {
      __this_procinfo->hmap[fdr].open_p = 1;
      fdw = __find_unused_handle ();
      __this_procinfo->hmap[fdr].open_p = 0;
      if (fdw < 0)
	set_errno ( ENMFILE);
      else
	{
	  SECURITY_ATTRIBUTES sa;

	  sa.nLength = sizeof (sa);
	  sa.lpSecurityDescriptor = 0;
	  /* When we fork we duplicate all the file handles to be inherited,
	     therefore all fds must be created as non-inheritable if we're the
	     parent.  We still set the close-on-exec flag to "no" though.
	     If we're the child, fds must be marked as inheritable.  */
	  sa.bInheritHandle = CHILD_P;

	  if (CreatePipe (&r, &w, &sa, 0))
	    {
	      __init_hinfo (__this_procinfo->hmap + fdr);
	      __init_hinfo (__this_procinfo->hmap + fdw);
	      __this_procinfo->hmap[fdr].handle = r;
	      __this_procinfo->hmap[fdw].handle = w;
	      __this_procinfo->hmap[fdr].close_exec_p = 0;
	      __this_procinfo->hmap[fdw].close_exec_p = 0;
	      __this_procinfo->hmap[fdr].open_p = 1;
	      __this_procinfo->hmap[fdw].open_p = 1;
	      __this_procinfo->hmap[fdr].r_binary = 1;
	      __this_procinfo->hmap[fdr].w_binary = 1;
	      __this_procinfo->hmap[fdw].r_binary = 1;
	      __this_procinfo->hmap[fdw].w_binary = 1;
	      __this_procinfo->hmap[fdr].readahead_valid = 0;
	      __this_procinfo->hmap[fdw].readahead_valid = 0;
	      __this_procinfo->hmap[fdr].access = GENERIC_READ;
	      __this_procinfo->hmap[fdw].access = GENERIC_WRITE;
	      fildes[0] = fdr;
	      fildes[1] = fdw;

	      syscall_printf ("0 = pipe (0x%x) (%d:0x%x, %d:0x%x)\n",
			      fildes, fdr, __this_procinfo->hmap[fdr].handle,
			      fdw, __this_procinfo->hmap[fdw].handle);
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

int
dup (int fd)
{
  HANDLE new;
  int res = -1;

  if (NOT_OPEN_FD (fd))
    {
      set_errno ( EBADFD);
    }
  else {
    res = __find_unused_handle ();

    if (res < 0)
      {
	set_errno ( EMFILE);
      }
    else 
      {
	/* Note: Since vfork dups all the fds that will be inherited, all fds
	   must be created as non-inheritable if we're the parent.  If we're
	   the child they must be marked as interitable.  */
	if (DuplicateHandle (GetCurrentProcess (),
			     __this_procinfo->hmap[fd].handle,
			     GetCurrentProcess (),
			     &new,
			     0, 
			     CHILD_P,
			     DUPLICATE_SAME_ACCESS))
	  {
	    __this_procinfo->hmap[res] = __this_procinfo->hmap[fd];
	    __this_procinfo->hmap[res].handle = new;
	  }
	else
	  {
	    __seterrno ();
	  }
      }
  }

  if (res >= 0)
    _STRACE (_STRACE_SYSCALL, ("%d = dup (%d) (%d:0x%x)\n",
			       res, fd, res, __this_procinfo->hmap[res].handle));
  else
    _STRACE (_STRACE_SYSCALL, ("%d = dup (%d)\n", res, fd));
  return res;
}

int
dup2 (int fd, int fd2)
{
  int res;
  HANDLE new;

  if (NOT_OPEN_FD (fd)
      || fd2 < 0 || fd2 >= NOFILE)
    {
      set_errno ( EBADFD);
      res = -1;
      goto done;
    }

  if (fd == fd2)
    {
      res = fd2;
      goto done;
    }

  /* Note: Since vfork dups all the fds that will be inherited, all fds
     must be created as non-inheritable if we're the parent.  If we're
     the child they must be marked as interitable.  */

  if (DuplicateHandle (GetCurrentProcess (), __this_procinfo->hmap[fd].handle,
		       GetCurrentProcess (), &new,
		       0, CHILD_P, DUPLICATE_SAME_ACCESS))
    {
      close (fd2);
      __this_procinfo->hmap[fd2] = __this_procinfo->hmap[fd];
      __this_procinfo->hmap[fd2].handle = new;
      res = fd2;
    }
  else
    {
      __seterrno ();
      res = -1;
    }

done:
  if (res >= 0)
    _STRACE (_STRACE_SYSCALL, ("%d = dup2 (%d, %d) (%d:0x%x)\n",
			       res, fd, fd2, res, __this_procinfo->hmap[res].handle));
  else
    _STRACE (_STRACE_SYSCALL, ("%d = dup2 (%d, %d)\n", res, fd, fd2));
  return res;
}

int
_fcntl (int fd, int cmd,...)
{
  int arg = 0, res;
  va_list args;

  if (NOT_OPEN_FD (fd))
    {
      set_errno ( EBADFD);
      res = -1;
      goto done;
    }

  switch (cmd)
    {
    case F_DUPFD:
    case F_GETFD:
      break;
    case F_SETFD:
      va_start (args, cmd);
      arg = va_arg (args, int);
      va_end (args);
      if (arg == FD_CLOEXEC)
	{
	  
#if 0 /* Don't delete this.  Keep it around for reference.  */
	  HANDLE new;
	  if (CHILD_P ())
	    {
	      if (!DuplicateHandle (GetCurrentProcess (), __this_procinfo->hmap[fd].handle,
				    GetCurrentProcess (), &new,
				    0, 0,
				    (DUPLICATE_CLOSE_SOURCE
				     | DUPLICATE_SAME_ACCESS)))
		{
		  /* Ugh.  We've lost the original handle.  */
		  _STRACE (0, ("DuplicateHandle for FD_CLOEXEC failed!\n"));
		  __seterrno ();
		  res = -1;
		  goto done;
		}
	      __this_procinfo->hmap[fd].handle = new;
	    }
#endif
	  __this_procinfo->hmap[fd].close_exec_p = 1;
	  res = 0;
	  goto done;
	}
      break;

    case F_GETFL:
      return O_RDWR;


    case F_SETFL:
      {
	va_start (args, cmd);
	arg = va_arg (args, int);
	va_end (args);
	syscall_printf ("fcntl (%d, F_SETFL, %d);\n", arg); 
	return 0;
      }
    case F_GETLK:
    case F_SETLK:
    case F_SETLKW:
      break;
    default:
      set_errno ( EINVAL);
      res = -1;
      goto done;
    }

  set_errno ( ENOSYS);
  res = -1;

done:
  _STRACE (_STRACE_SYSCALL, ("%d = fcntl (%d, %d, %d)\n", res, fd, cmd, arg));
  return res;
}
