/* sysconf and friends 

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <unistd.h>
#include <windows.h>
#include "winsup.h"

/* See Posix 1003.1 for more information.
   "The implementation shall support all of the variables listed in Table 4-2
   and may support others."
*/

long int
sysconf (int in)
{
  switch (in)
    {
    case _SC_OPEN_MAX:
      return NOFILE;
    case _SC_PAGESIZE:
      {
	SYSTEM_INFO b;
	GetSystemInfo (&b);
	return b.dwPageSize;
      }
    case _SC_CLK_TCK:
      return CLOCKS_PER_SEC;
    case _SC_JOB_CONTROL:
      return 0; /* FIXME: -1?  */
    case _SC_ARG_MAX:
      return 4096; /* FIXME */
    case _SC_CHILD_MAX:
      return 8; /* FIXME */
    case _SC_NGROUPS_MAX:
    case _SC_SAVED_IDS:
    case _SC_VERSION: /* FIXME: Need to support this.  */
#if 0 /* FIXME: Need to define.  */
    case _SC_TZNAME_MAX:
    case _SC_STREAM_MAX:
#endif
      return -1;
    }

  set_errno (EINVAL);
  return -1;
}
