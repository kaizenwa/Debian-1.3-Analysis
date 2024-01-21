/*	Copyright (C) 1996 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>

#include "getopt.h"
#include "libguile.h"



/* Debugger interface */
#define GDB_TYPE SCM

#include "gdb_interface.h"

GDB_INTERFACE;


/* The main program.  */

/* The name this program was invoked by.  */
char *program_name = NULL;

char usage[] = "Usage: %s [-hev] [--help] [--emacs] [--version] [-s FILE ARGS]\n";

/* Table of long options.  */
struct option long_opts[] =
  {
    {"version", 0, 0, 'v'},
    {"emacs", 0, 0, 'e'},
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
  };

void
initplugs ()
{
#include "initplugs.c"
}

static void
guile_main (void *closure, int argc, char **argv)
{
  char *boot_cmd = (char *) closure;
  char *script = 0;
  int optchar;
  int show_version = 0;
  int show_help = 0;
  int use_emacs_interface = 0;
  int opt_error = 0;

  initplugs ();

  program_name = strrchr (argv[0], '/');
  if (! program_name)
    program_name = argv[0];
  else
    program_name++;

  while (1)
    {
      optchar = getopt_long (argc, argv, "+hevs:", long_opts, (int *) 0);
      if (optchar == EOF)
	break;

      if (optchar == 's')
	{
	  script = optarg;
	  break;
	}

      switch (optchar)
	{
	case 'h':
	  show_help = 1;
	  break;
	case 'e':
	  use_emacs_interface = 1;
	  break;
	case 'v':
	  show_version = 1;
	  break;
	default:
	  opt_error = 1;
	  break;
	}
    }

  if (opt_error)
    {
      /* Print error message and exit.  */
      fprintf (stderr, usage, program_name, program_name);
      exit (1);
    }

  if (show_version)
    {
      /* Print version number.  */
      fprintf (stdout, "Guile %s\n", GUILE_VERSION);
      fprintf (stdout, "Copyright (c) 1995, 1996 Free Software Foundation\n");
      fprintf (stdout,
"Guile may be distributed under the terms of the GNU General Public Licence;\n\
certain other uses are permitted as well.  For details, see the `COPYING',\n\
which is included in the Guile distribution.\n\
There is no warranty, to the extent permitted by law.\n");
    }

  if (show_help)
    {
      /* Print help info and exit.  */
      fputs ("This is Guile, a Scheme interpreter.\n", stdout);
      fprintf (stdout, usage, program_name, program_name);
      fputs ("  -h, --help          Print a summary of the options\n", stdout);
      fputs ("  -e, --emacs         Running under Emacs\n", stdout);
      fputs ("  -v, --version       Print the version number\n", stdout);
      fputs ("  -s FILE ARGS        Run scheme code in FILE; remaining ARGS passed to script\n", stdout);
      fputs ("                      must be final option\n", stdout);
      fputs ("Send bug reports to bug-guile@prep.ai.mit.edu.\n", stdout);
    }

  if (show_version || show_help)
    exit (0);

  {
    /* We want a path only containing directories from
       SCHEME_LOAD_PATH, SCM_SITE_DIR and SCM_LIBRARY_DIR when
       searching for the site init file, so we do this before
       evaluating the boot_cmd.  */
    SCM pathname = scm_sys_search_load_path (scm_makfrom0str ("init.scm"));
    
    scm_eval_0str (boot_cmd);
    
    if (SCM_NFALSEP (pathname))
      scm_primitive_load (pathname, SCM_UNDEFINED, SCM_UNDEFINED);
  }
  
  if (script)
    {
      scm_set_program_arguments (argc - optind, argv + optind, script);
      scm_primitive_load (scm_makfrom0str (script),
			  SCM_UNDEFINED, SCM_UNDEFINED);
    }
  else 
    {
      /* Load user's init file... */
      char *home = getenv ("HOME");
      if (home)
      {
	int len = strlen (home);
	char *namebuf = scm_must_malloc (len + sizeof ("/.guile") + 1,
					 "guile_main");
	struct stat mode;
	
	strcpy (namebuf, home);
	if (len >= 1 && namebuf[len - 1] == '/')
	  --len;
	strcpy (&namebuf[len], "/.guile");

	if (stat (namebuf, &mode) >= 0 && !(mode.st_mode & S_IFDIR))
	  scm_primitive_load (scm_makfrom0str (namebuf),
			      SCM_UNDEFINED, SCM_UNDEFINED);
	scm_must_free (namebuf);
      }
      
      scm_set_program_arguments (argc - optind, argv + optind, argv[0]);
      scm_eval_0str ("(top-repl)");
    }
}


int
main (argc, argv)
     int argc;
     char **argv;
{
  scm_boot_guile (argc, argv, guile_main, GUILE_BOOT_CMD);
}
