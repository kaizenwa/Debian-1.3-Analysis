/* General system debugging tool.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/* This program is intended to be a general tool for debugging cygwin.
   Possibilities include
   - dumping various internal data structures
   - poking various values into system tables
   - turning on strace'ing for arbitrary tasks
   */

#include <string.h>
#include <stdio.h>
#include <getopt.h>
#include <windows.h>
#include "winsup.h"

static char *prog_name;

static void
usage (FILE *stream, int status)
{
  fprintf (stream, "\
Usage: %s \\\n\
       [-s|--strace pid mask]\\\n\
       [-H|--help] [-V|--version]\n\
",
	   prog_name);
  exit (status);
}

static struct option long_options[] =
{
  { "version", no_argument, NULL, 'V' },
  { "help", no_argument, NULL, 'H' },
  { "strace", required_argument, NULL, 's' },
  { 0, no_argument, 0, 0 }
};

struct strace_args
{
  int pid;
  int mask;
};

/* Turn on strace'ing for the indicated pid.  */

static void
set_strace (strace_args *args)
{
  shared_info *s = cygwin32_getshared ();

  pinfo *p = s->p[args->pid];
  if (!p)
    {
      fprintf (stderr, "%s: process %d not found\n", prog_name, args->pid);
      exit (1);
    }

  HANDLE h = OpenProcess (PROCESS_VM_OPERATION | PROCESS_VM_WRITE,
			  FALSE, p->dwProcessId);
  if (! h)
    {
      fprintf (stderr, "%s: unable to access process %d, win32 process id %d, win32 error %d\n",
	       prog_name, args->pid, p->dwProcessId, GetLastError ());
      exit (1);
    }

  int mask = args->mask;
#ifdef DEBUG
  printf ("%s: writing 0x%x to %p\n", prog_name, mask, p->strace_mask_ptr);
#endif
  if (! WriteProcessMemory (h, p->strace_mask_ptr, &mask, sizeof (mask), NULL))
    {
      fprintf (stderr, "%s: unable to write process %d memory, handle 0x%x, win32 error %d\n",
	       prog_name, args->pid, h, GetLastError ());
      exit (1);
    }
  CloseHandle (h);
}

int
main (int argc, char *argv[])
{
  int c;
  int seen_flag_p = 0;
  int show_version_p = 0;
  int set_strace_p = 0;
  strace_args strace_args;

  prog_name = strrchr (argv[0], '/');
  if (prog_name == NULL)
    prog_name = strrchr (argv[0], '\\');
  if (prog_name == NULL)
    prog_name = argv[0];

  while ((c = getopt_long (argc, argv, "HVs", long_options, (int *) 0))
	 != EOF)
    {
      seen_flag_p = 1;

      switch (c)
	{
	case 'H':
	  usage (stdout, 0);
	  break;
	case 'V':
	  show_version_p = 1;
	  break;
	case 's':
	  if (optind + 1 > argc)
	    usage (stderr, 1);
	  strace_args.pid = atoi (optarg);
	  strace_args.mask = atoi (argv[optind++]);
	  set_strace_p = 1;
	  break;
	default:
	  usage (stderr, 1);
	  break;
	}
    }

  if (show_version_p)
    printf ("CYGWIN version ???\n");

  if (!seen_flag_p || optind != argc)
    usage (stderr, 1);

  if (set_strace_p)
    set_strace (&strace_args);

  return 0;
}
