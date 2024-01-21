/* open.c: stubs for open.
   Copyright 1995 Tristan Gingold

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

/* This is a stub for 'open': the third argument is checked only if it is
   used.  Thus, bad warnings are avoided.  */

#include <fcntl.h> 
#include "checker.h"
#include <stdarg.h>

#undef open

int
open (const char *file, int oflag, ...)
{
  int res;

#ifdef CHECK_INCOMING_ARGS  
  chkr_check_addr ((PTR) &file, sizeof (char*), CHKR_RO);
  chkr_check_addr ((PTR) &oflag, sizeof (int*), CHKR_RO);
#endif
  chkr_check_str ((PTR) file, CHKR_RO);
  
  if (oflag & O_CREAT)
    {
      int *mode = &oflag + 1;		/* not really portable */
#ifdef CHECK_INCOMING_ARGS
      chkr_check_addr ((PTR) mode, sizeof (int), CHKR_RO);
#endif
      res = chkr_open (file, oflag, *mode);
      errno = chkr_errno;
      return res;
    }

  res = chkr_open (file, oflag);
  if (res != -1)
    fd_returned_by_system (res);
    
  errno = chkr_errno;
  return res;
}

#undef fcntl

int check_fcntl (int fd, int cmd, int arg);

/* This is a stub for fcntl.  */
int
fcntl (int filedes, int cmd, ...)
{
  int res;
  int *arg = &cmd + 1;
  
  chkr_check_addr ((PTR) &filedes, sizeof (int), CHKR_RO);
  chkr_check_addr ((PTR) &cmd, sizeof (int), CHKR_RO);
  
  switch(cmd)
    {
      case F_GETFD:
      case F_GETFL:
      case F_GETOWN:
        /* The argument is not used. */
        break;
      default:
        /* By default, the argument is used. */
        chkr_check_addr ((PTR) arg, sizeof (int), CHKR_RO);
    }
  check_fcntl (filedes, cmd, *arg);
  res = chkr_fcntl (filedes, cmd, *arg);
  errno = chkr_errno;
  return res;
}
