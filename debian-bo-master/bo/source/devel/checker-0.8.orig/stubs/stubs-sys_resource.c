/* Checker stubs for functions defined in sys/resource.h
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

#ifdef HAVE_SYS_RESOURCE_H
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <sys/resource.h>
#include "checker_api.h"

void stubs_chkr_set_right_timeval (struct timeval *tv, int right);

#ifdef HAVE_getrusage
static void
stubs_chkr_set_right_struct_rusage (struct rusage *usage, int right)
{
  stubs_chkr_set_right_timeval (&usage->ru_utime, right);
  stubs_chkr_set_right_timeval (&usage->ru_stime, right);
  stubs_chkr_set_right (&usage->ru_maxrss, sizeof (usage->ru_maxrss), right);
  stubs_chkr_set_right (&usage->ru_ixrss, sizeof (usage->ru_ixrss), right);
  stubs_chkr_set_right (&usage->ru_idrss, sizeof (usage->ru_idrss), right);
  stubs_chkr_set_right (&usage->ru_isrss, sizeof (usage->ru_isrss), right);
  stubs_chkr_set_right (&usage->ru_minflt, sizeof (usage->ru_minflt), right);
  stubs_chkr_set_right (&usage->ru_majflt, sizeof (usage->ru_majflt), right);
  stubs_chkr_set_right (&usage->ru_nswap, sizeof (usage->ru_nswap), right);
  stubs_chkr_set_right (&usage->ru_inblock, sizeof (usage->ru_inblock), right);
  stubs_chkr_set_right (&usage->ru_oublock, sizeof (usage->ru_oublock), right);
  stubs_chkr_set_right (&usage->ru_msgsnd, sizeof (usage->ru_msgsnd), right);
  stubs_chkr_set_right (&usage->ru_msgrcv, sizeof (usage->ru_msgrcv), right);
  stubs_chkr_set_right (&usage->ru_nsignals, sizeof (usage->ru_nsignals), right);
  stubs_chkr_set_right (&usage->ru_nvcsw, sizeof (usage->ru_nvcsw), right);
  stubs_chkr_set_right (&usage->ru_nivcsw, sizeof (usage->ru_nivcsw), right);
}
#endif

/* compiled from: . */
#ifdef HAVE_getrlimit
/* From `/usr/include/sys/resource.h:16'.  */
int
chkr$getrlimit (int resource, struct rlimit *rlim)
{
  int res;
  stubs_chkr_check_addr (rlim, sizeof (struct rlimit), CHKR_MW, "rlim");
  res = getrlimit (resource, rlim);
  if (res != -1)
    stubs_chkr_set_right (rlim, sizeof (struct rlimit), CHKR_RW);
  return res;
}
#endif /* HAVE_getrlimit */

#ifdef HAVE_setrlimit
/* From `/usr/include/sys/resource.h:18'.  */
int
chkr$setrlimit (int resource, const struct rlimit *rlim)
{
  stubs_chkr_check_addr (rlim, sizeof (struct rlimit), CHKR_RO, "rlim");
#if USE_BI_JUMP
  __builtin_jump (setrlimit);
#else
  return setrlimit (resource, rlim);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setrlimit */

#ifdef HAVE_getpriority
/* From `/usr/include/sys/resource.h:20'.  */
int
chkr$getpriority (int which, int who)
{
#if USE_BI_JUMP
  __builtin_jump (getpriority);
#else
  return getpriority (which, who);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getpriority */

#ifdef HAVE_setpriority
/* From `/usr/include/sys/resource.h:22'.  */
int
chkr$setpriority (int which, int who, int prio)
{
#if USE_BI_JUMP
  __builtin_jump (setpriority);
#else
  return setpriority (which, who, prio);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setpriority */

#ifdef HAVE_getrusage
/* From `/usr/include/sys/resource.h:25'.  */
int
chkr$getrusage (int who, struct rusage *usage)
{
  int res;
  
  stubs_chkr_check_addr (usage, sizeof (struct rusage), CHKR_MW, "usage");
  res = getrusage (who, usage);
  if (res == 0)
    stubs_chkr_set_right_struct_rusage (usage, CHKR_RW);
  return res;
}
#endif /* HAVE_getrusage */

#endif /* HAVE_SYS_RESOURCE_H */
