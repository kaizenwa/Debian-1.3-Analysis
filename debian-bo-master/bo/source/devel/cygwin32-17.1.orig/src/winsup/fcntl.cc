/* fcntl syscall.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
*/

#include <fcntl.h>
#include <stdarg.h>
#include "winsup.h"

int _fcntl (int fd, int cmd,...)
{
  va_list args;
  int arg = 0;
  int res;

  if (NOT_OPEN_FD (fd))
    {
      set_errno (EBADFD);
      res = -1;
      goto done;
    }

  switch (cmd)
    {
    case F_DUPFD:
      va_start (args, cmd);
      arg = va_arg (args,int);
      va_end (args);
      return dup2 (fd, u->self->hmap.find_unused_handle (arg));

    case F_GETFD:
      return this_procinfo ()->hmap[fd].h->close_exec_p ? FD_CLOEXEC : 0;
      break;

    case F_SETFD:
      va_start (args, cmd);
      arg = va_arg (args, int);
      va_end (args);
      if (arg == FD_CLOEXEC)
	{
/* Don't delete this.  Keep it around for reference.  */
#if 0
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
	  this_procinfo ()->hmap[fd].h->close_exec_p = 1;

	  res = 0;
	  goto done;
	}
      break;

    case F_GETFL:
       res = 0;
       if (this_procinfo ()->hmap[fd].h->get_access () & GENERIC_READ)
 	res |= O_RDONLY;
       if (this_procinfo ()->hmap[fd].h->get_access () & GENERIC_WRITE)
 	res |= O_WRONLY;
       if (this_procinfo ()->hmap[fd].h->get_access () & GENERIC_ALL)
 	res |= O_RDWR;
       goto done;

    case F_SETFL:
      {
	int temp = 0;
	va_start (args, cmd);
	arg = va_arg (args, int);
	va_end (args);

 	if (arg & O_RDONLY)
 	  temp |= GENERIC_READ;
 	if (arg & O_WRONLY)
 	  temp |= GENERIC_WRITE;

	syscall_printf ("fcntl (%d, F_SETFL, %d);\n", arg);  

 	this_procinfo ()->hmap[fd].h->set_access (temp);
	res =  0;
	goto done;
      }

    case F_GETLK:
    case F_SETLK:
    case F_SETLKW:
      break;
    default:
      set_errno (EINVAL);
      res = -1;
      goto done;
    }

  set_errno (ENOSYS);
  res = -1;

 done:
  syscall_printf ("%d = fcntl (%d, %d, %d)\n", res, fd, cmd, arg);
  return res;
}
