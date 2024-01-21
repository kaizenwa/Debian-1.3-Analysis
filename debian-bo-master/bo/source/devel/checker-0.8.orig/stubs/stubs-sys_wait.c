/* Checker stubs for functions defined in sys/wait.h
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

#ifdef HAVE_SYS_WAIT_H
#include <sys/types.h>
#include <sys/wait.h>
#include "checker_api.h"

#undef HAVE_wait3
#undef HAVE_wait4

#if 0
#define HAVE_wait
#define HAVE_waitpid
#endif

/* compiled from: . */
#ifdef HAVE_wait
/* From `/usr/include/sys/wait.h:96'.  */
pid_t
chkr$wait (int *status)
{
  if (status)
    stubs_chkr_check_addr (status, sizeof (int), CHKR_WO, "status");
#if USE_BI_JUMP
  __builtin_jump (wait);
#else
  return wait (status);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wait */

#ifdef HAVE_waitpid
/* From `/usr/include/sys/wait.h:119'.  */
pid_t
chkr$waitpid (pid_t pid, int *status, int opt)
{
  stubs_chkr_check_addr (status, sizeof (int), CHKR_WO, "status");
#if USE_BI_JUMP
  __builtin_jump (waitpid);
#else
  return waitpid (pid, status, opt);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_waitpid */

#ifdef HAVE_wait3
/* From `/usr/include/sys/wait.h:133'.  */
pid_t
chkr$wait3 (int *status, int arg1, struct rusage * arg2)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg2, sizeof (struct rusage), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (wait3);
#else
  {
    __pid_t res;
    res = wait3 (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wait3 */

#ifdef HAVE_wait4
/* From `/usr/include/sys/wait.h:139'.  */
pid_t
chkr$wait4 (pid_t arg0, int *status, int arg2, struct rusage * arg3)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg3, sizeof (struct rusage), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (wait4);
#else
  {
    __pid_t res;
    res = wait4 (arg0, arg1, arg2, arg3);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wait4 */

#endif /* HAVE_SYS_WAIT_H */
