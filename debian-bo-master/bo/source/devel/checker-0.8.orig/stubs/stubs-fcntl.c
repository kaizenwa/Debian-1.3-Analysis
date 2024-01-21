/* Checker stubs for functions defined in fcntl.h
   Copyright 1995, 1996 Tristan Gingold
		  Written December 1995 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

 The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
#include "available-stubs.h"

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#include <errno.h>
#include "checker_api.h"

#if 0
#define HAVE_creat
#define HAVE_open
#define HAVE_lockf
#endif

/* compiled from: . */
#ifdef HAVE_creat
int
chkr$creat (const char *name, mode_t mode)
{
  int res;
  
  stubs_chkr_check_str (name, CHKR_RO, "name");
  res = creat (name, mode);
  if (res != -1)
    fd_returned_by_system (res);
  return res;
}
#endif /* HAVE_creat */

#ifdef HAVE_open
int
chkr$open (const char *name, int flags, int mode)
{
  int res;
  
  stubs_chkr_check_str (name, CHKR_RO, "name");
  res = open (name, flags, mode);
  if (res != -1)
    fd_returned_by_system (res);
  return res;
}
#endif /* HAVE_open */

#ifdef HAVE_fcntl
/* **** DO NOT DEFINE ARGX_ON_STACK at this time.  */
/* The third argument is optional.  We can check its presence only if it is
   passed with the stack (and not with a register).  */
#ifdef ARG3_ON_STACK
#define CHECK_ARG3 stubs_chkr_check_addr ((PTR) &arg, CHKR_RO, sizeof (int), "arg")
#else
#define CHECK_ARG3
#endif

/* The cases with `??' means that I am not sure them.  */
int
chkr$fcntl (int fd, int cmd, int arg)
{
  int res;

#if 0	/* Do not check FD and CMD.  This is done by the prototypes.  */
  /* Check the presence of FD and CMD (if possible).  */
#ifdef ARG1_ON_STACK
  stubs_chkr_check_addr ((PTR) &fd, CHKR_RO, sizeof (int), "fd");
#endif
#ifdef ARG2_ON_STACK
  stubs_chkr_check_addr ((PTR) &cmd, CHKR_RO, sizeof (int), "cmd");
#endif
#endif

  /* Be sure the user can use FD.  */
  if (! fd_used_by_prog (fd))
    {
      errno = EBADF;
      return -1;
    }
  
  switch (cmd)
    {
#ifdef F_DUPFD
    case F_DUPFD:		/* arg is used as a value */
      CHECK_ARG3;
      res = fcntl (fd, cmd, arg);
      /* Register this new FD. */
      fd_duped(res, fd);
      return res;
#endif

#ifdef F_GETFD /* F_GETFD, F_SETFD */
    case F_GETFD:
      return fcntl (fd, cmd);
    case F_SETFD:
      CHECK_ARG3;
      return fcntl (fd, cmd, arg);
#endif

#ifdef F_GETFL /* F_SETFL */
    case F_GETFL:
      return fcntl (fd, cmd);
    case F_SETFL:		/* arg is used as a value */
      CHECK_ARG3;
      return fcntl (fd, cmd, arg);
#endif

#ifdef F_GETLK /* F_SETLK, F_SETLKW */
    case F_GETLK:
      CHECK_ARG3;
      stubs_chkr_check_addr ((PTR) arg, sizeof (struct flock), CHKR_WO, "arg");
      return fcntl (fd, cmd, arg);
    case F_SETLK:
    case F_SETLKW:	/* Wait */
      CHECK_ARG3;
      stubs_chkr_check_addr ((PTR) arg, offsetof (struct flock, l_pid), CHKR_RO, "arg");
      return fcntl (fd, cmd, arg);
#endif

#ifdef F_SETOWN /* F_GETOWN */
    case F_SETOWN:		/* arg is used as a value */
      CHECK_ARG3;
      return fcntl (fd, cmd, arg);
    case F_GETOWN:
      return fcntl (fd, cmd);
#endif

#ifdef F_FREESP
    case F_FREESP:
      CHECK_ARG3;
      stubs_chkr_check_addr ((PTR) arg + offsetof (struct flock, l_whence),
		       sizeof (((struct flock *)arg)->l_whence), CHKR_RO, "arg->l_whence");
      stubs_chkr_check_addr ((PTR) arg + offsetof (struct flock, l_start),
		       sizeof (((struct flock *)arg)->l_start), CHKR_RO, "arg->l_start");
      stubs_chkr_check_addr ((PTR) arg + offsetof (struct flock, l_len),
		       sizeof (((struct flock *)arg)->l_len), CHKR_RO, "arg->l_len");
      return fcntl (fd, cmd, arg);
#endif

#ifdef F_RSETLK  /* svr4 */
    case F_RGETLK:
      CHECK_ARG3;
      stubs_chkr_check_addr ((PTR) arg, sizeof (struct flock), CHKR_WO, "arg");
      return fcntl (fd, cmd, arg);
    case F_RSETLK:
    case F_RSETLKW:	/* Wait */
      CHECK_ARG3;
      stubs_chkr_check_addr ((PTR) arg, offsetof(struct flock,l_pid), CHKR_RO, "arg->l_pid");
      return fcntl (fd, cmd, arg);
#endif

#ifdef F_ISSTREAM /* svr4 ?? */
    case F_ISSTREAM:
      return fcntl (fd, cmd);
#endif

#ifdef F_PRIV /* F_NPRIV svr4 ?? */
    case F_PRIV:
    case F_NPRIV:
      return fcntl (fd, cmd);
#endif

#ifdef F_BLOCKS /* F_BLKSIZE svr4 ?? */
    case F_BLOCKS:
    case F_BLKSIZE:
      return fcntl (fd, cmd);
#endif

/****** These are unsupported because I don't have any doc about them *******/
#ifdef F_QUOTACTL
    case F_QUOTACTL:
#endif
    default:
      chkr_printf ("Sorry, this fcntl (0x%x) is not implemented.\n", cmd);
      chkr_printf ("Please send a description of it to the author of Checker.\n");
      return 1;
    }
}
#endif /* HAVE_fcntl */

#endif /* HAVE_FCNTL_H */
