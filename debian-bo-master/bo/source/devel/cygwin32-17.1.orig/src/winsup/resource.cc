/* resource for WIN32.

   Written by Steve Chamberlain and Doug Evans of Cygnus Support.
   sac@cygnus.com, dje@cygnus.com

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */


/* getrusage () and friends */

#include <windows.h>
#include <sys/resource.h>
#include "winsup.h"


int
getrusage (int intwho, struct rusage *rusage)
{
  FILETIME creation;
  FILETIME exit;
  FILETIME kernel;
  FILETIME user;

  HANDLE who;

  if (intwho == 0)
    who = GetCurrentProcess ();
  else
    who = procinfo (intwho)->hProcess; /* !!! not valid - the hProcess is closed.. */


  GetProcessTimes (who, &creation, &exit, &kernel, &user);
  totimeval (&rusage->ru_stime, &kernel,0);
  totimeval (&rusage->ru_utime, &user , 0);

  syscall_printf ("0 = getrusage (%d, 0x%x)\n", intwho, rusage);
  return 0;
}


