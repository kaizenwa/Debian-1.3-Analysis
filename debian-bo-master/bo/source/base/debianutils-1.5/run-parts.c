/* run-parts: run a bunch of scripts in a directory
 *
 * Debian Linux run-parts program, version 0.5
 * Copyright (C) 1996 Jeff Noxon <jeff@router.patch.net>,
 *
 * This is free software; see the GNU General Public License version 2
 * or later for copying conditions.  There is NO warranty.
 *
 * Based on run-parts.pl version 0.2, Copyright (C) 1994 Ian Jackson.
 *
 * Revision History
 *
 * 01/07/96 - JEN - v0.3 - First version; released with miscutils-1.3-7
 *
 * 04/11/96 - GM  - v0.4 - Made it skip over directories (fixes Bug#2244)
 *                         Better error checking
 *
 * 06/01/96 - AND - v0.5 - Added --verbose option to print the name of each
 *                         script before executing it
 *
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <getopt.h>
#include <string.h>
#include <errno.h>

int test_mode = 0;
int verbose_mode = 0;
int exitstatus = 0;

void error (char *format,...)
{
  va_list ap;
  
  fprintf (stderr, "run-parts: ");

  va_start (ap, format);
  vfprintf (stderr, format, ap);
  va_end (ap);

  fprintf (stderr, "\n");
}

void version (void)
{
  fprintf (stderr,
    "Debian Linux run-parts program, version 0.5.\n"
    "Distributed with Debian's base system.  Copyright (C) 1996 Jeff Noxon.\n"
    "This is free software; see the GNU General Public License version 2\n"
    "or later for copying conditions.  There is NO warranty.\n");
  exit (0);
}

void usage (void)
{
  fprintf (stderr,
    "usage: run-parts [--test|--help|--version|--verbose] [--umask=umask] "
	   "directory\n");
}

/* The octal conversion in libc is not foolproof; it will take the 8 and 9
 * digits under some circumstances.  We'll just have to live with it.
 */
void set_umask (void)
{
  int mask, result;

  result = sscanf (optarg, "%o", &mask);
  if ((result != 1) || (mask > 07777) || (mask < 0)) {
    error ("bad umask value");
    usage ();
    exit (1);
  }

  umask (mask);
}

/* True or false? Is this a valid filename (upper/lower alpha, digits,
 * underscores, and hyphens only?)
 */
int valid_name (char *filename)
{
  while (*filename) {
    if (!(((*filename >= 'a') && (*filename <= 'z')) ||
	  ((*filename >= 'A') && (*filename <= 'Z')) ||
	  ((*filename >= '0') && (*filename <= '9')) ||
	  (*filename == '_') ||
	  (*filename == '-')))
      return 0;
    ++filename;
  }
  
  return 1;
}

/* Execute a file */
void run_part (char *progname)
{
  int result;
  int pid;
  if ((pid=fork()) < 0) {
    error ("failed to fork: %s", strerror (errno));
    exit (1);
  }
  else if (!pid) {
    execl (progname, progname, 0);
    error ("failed to exec %s: %s", progname, strerror (errno));
    exit (1);
  }
  else
    wait (&result);
  
  if (WIFEXITED (result) && WEXITSTATUS(result)) {
    error ("%s exited with return code %d", progname, WEXITSTATUS(result));
    exitstatus = 1;
  }
  else if (WIFSIGNALED (result)) {
    error ("%s exited because of uncaught signal %d", progname,
	   WTERMSIG(result));
    exitstatus = 1;
  }
}

/* Find the parts to run & call run_part() */
void run_parts (char *dirname)
{
  struct dirent **namelist;
  char filename[PATH_MAX];
  int entries, i, result;
  struct stat st;

  /* scandir() isn't POSIX, but it makes things easy. */
  entries = scandir (dirname, &namelist, 0, alphasort);
  if (entries < 0)
    error ("failed to open directory %s: %s", dirname, strerror (errno));

  for (i = 0; i < entries; i++) {
    if (valid_name (namelist[i]->d_name)) {
      strcpy (filename, dirname);
      strcat (filename, "/");
      strcat (filename, namelist[i]->d_name);

      result = stat (filename, &st);
      if (result < 0) {
	error ("failed to stat component %s: %s", filename,
	       strerror (errno));
	exit (1);
      }
      if (S_ISREG(st.st_mode) && !access (filename, X_OK)) {
	if (test_mode)
	  printf ("run-parts would run %s\n", filename);
	else {
	  if (verbose_mode)
	    fprintf(stderr, "run-parts: about to run %s...\n", filename);
	  run_part (filename);
	}
      }
      else if (!S_ISDIR(st.st_mode)) {
	printf ("run-parts: component %s is not an executable plain file\n",
		filename);
	exitstatus = 1;
      }

    }
    free (namelist[i]);
  }
  free (namelist);
}

/* Process options */
int main (int argc, char *argv[])
{
  umask (022);
  
  while (1) {
    int c;
    int option_index = 0;

    static struct option long_options[] = {
      {"test", 0, 0, 0},
      {"help", 0, 0, 0},
      {"version", 0, 0, 0},
      {"umask", 1, 0, 0},
      {"verbose", 0, 0, 0},
      {0, 0, 0, 0}
    };

    opterr = 0;
    c = getopt_long (argc, argv, "", long_options, &option_index);
    
    if (c == -1)
      break;
    
    if (c == 0) {
      switch (option_index) {
      case 0:
	test_mode = 1;
	break;
      case 2:
	version ();
      case 3:
	set_umask ();
	break;
      case 4:
	verbose_mode = 1;
	break;
      case 1:
      default:
	usage ();
	exit (0);
      }
    }
  }
  
  /* We require exactly one argument: the directory name */
  if (optind != (argc - 1)) {
    usage ();
    exit (1);
  }

  run_parts (argv[optind]);

  return exitstatus;
}
