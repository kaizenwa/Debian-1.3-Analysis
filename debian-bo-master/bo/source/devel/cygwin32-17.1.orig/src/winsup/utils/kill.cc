/* kill for win32.

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

static void usage ();

int
main (int ac, char **av)
{
  int i;

  if (ac == 1)
    usage ();

  /* FIXME: We currently don't handle picking the signal.  */
  if (av[1][0] == '-')
    usage ();

  for (i = 1; i < ac; ++i)
    {
      int pid = atoi (av[i]);
      printf ("Kill %d\n", pid);
      shared_info *s = cygwin32_getshared ();
      pinfo *p = s->p[pid];
      if (!p)
	{
	  fprintf (stderr, "Can't find process %d\n", pid);
	  exit (1);
	}
      else
	{
	  printf ("killing %d (win32 pid %d)\n", pid, p->dwProcessId);
	  HANDLE h = OpenProcess (PROCESS_TERMINATE, FALSE, p->dwProcessId);
	  if (!h)
	    {
	      fprintf (stderr, "Error opening %d (win32 %d)\n", pid, p->dwProcessId);
	      exit (1);
	    }
	  else
	    {
	      /* FIXME: The reason we're not using kill() is ???  */
	      if (TerminateProcess (h, 0))
		printf ("OK\n");
	      else
		{
		  fprintf (stderr, "TerminateProcess failed, win32 error %d\n",
			   GetLastError ());
		  exit (1);
		}
	      CloseHandle (h);
	    }
	}
    }
  return 0;
}

static void
usage ()
{
  fprintf (stderr, "Usage: kill pid1 [pid2 ...]\n");
  exit (1);
}
