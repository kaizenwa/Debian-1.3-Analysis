/* ps for win32.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <stdio.h>
#include "winsup.h"

int
main (int argc, char *argv[])
{
  shared_info *s = cygwin32_getshared();
  char *title = "%8s %8s %10s %5s  %s\n";
  char *format = "%8d %8d %10d %5d  %s\n";

  printf (title, "PID", "PPID", "WIN32-PID", "UID", "COMMAND");

  for (int i = 0; i < s->p.size(); i++)
    {
      pinfo *p = &s->p.vec[i];

      if (! p->inuse_p)
	continue;

      printf (format, p->pid, p->ppid, p->dwProcessId, p->uid, p->progname);
    }

  return 0;
}
