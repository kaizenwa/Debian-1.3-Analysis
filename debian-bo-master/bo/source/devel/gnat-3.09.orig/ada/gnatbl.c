/****************************************************************************/
/*                                                                          */
/*                           GNAT COMPILER TOOLS                            */
/*                                                                          */
/*                               G N A T B L                                */
/*                                                                          */
/*                          C Implementation File                           */
/*                                                                          */
/*                             $Revision: 1.53 $                            */
/*                                                                          */
/*    Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.   */
/*                                                                          */
/* GNAT is free software;  you can  redistribute it  and/or modify it under */
/* terms of the  GNU General Public License as published  by the Free Soft- */
/* ware  Foundation;  either version 2,  or (at your option) any later ver- */
/* sion.  GNAT is distributed in the hope that it will be useful, but WITH- */
/* OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License */
/* for  more details.  You should have  received  a copy of the GNU General */
/* Public License  distributed with GNAT;  see file COPYING.  If not, write */
/* to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, */
/* MA 02111-1307, USA.                                                      */
/*                                                                          */
/* GNAT was originally developed  by the GNAT team at  New York University. */
/* It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). */
/*                                                                          */
/****************************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "config.h"  /* this may define MSDOS */
#include <stdlib.h>
#if defined (__EMX__) || defined (MSDOS)
#include <process.h>
#endif
#include <string.h>
#include "a-adaint.h"

/* These can be set by command line arguments */
char *binder_path = 0;
char *linker_path = 0;
char *exec_file_name = 0;
#define BIND_ARG_MAX 512
char *bind_args[BIND_ARG_MAX];
int  bind_arg_index = -1;
#ifdef MSDOS
char *coff2exe_path = 0;
char *coff2exe_args[] = {(char *) 0, (char *) 0};
char *del_command = 0;
#endif
int  verbose      = 0;
int  o_present    = 0;
int  g_present    = 0;

int  link_arg_max = -1;
char **link_args = (char **) 0;
int  link_arg_index = -1;

char *gcc_B_arg = 0;
#ifndef DIR_SEPARATOR
#if defined (__EMX__)
#define DIR_SEPARATOR '\\'
#else
#define DIR_SEPARATOR '/'
#endif
#endif

extern char *rindex ();

static void proc_bind_file (char *aliname);
static int linkonly = 0;

static void
addarg (char *str)
{
  int i;

  if (++link_arg_index >= link_arg_max) {
    char **new_link_args
      = (char **) calloc (link_arg_max + 1000, sizeof (char *));
    for (i = 0; i <= link_arg_max; i++)
      new_link_args [i] = link_args [i];
    if (link_args) free (link_args);
    link_arg_max += 1000;
    link_args = new_link_args;
  }

  link_args [link_arg_index] = str;
}


static void
process_args (int *p_argc, char *argv [])
{
  int i, j;

  for (i = 1; i < *p_argc; i++) {

    /* -I is passed on to gnatbind */
    if (! strncmp( argv[i], "-I", 2))
    {
      bind_arg_index += 1;
      if (bind_arg_index >= BIND_ARG_MAX)
      {
         fprintf (stderr, "Too many arguments to gnatbind\n");
         exit (-1);
      }
      bind_args[bind_arg_index] = argv[i];
    }
    /* -B is passed on to gcc */
    if (! strncmp (argv [i], "-B", 2)) {
       gcc_B_arg = argv[i];
    }
    /* -v turns on verbose option here and is passed on to gcc */

    if (! strcmp (argv [i], "-v"))
      verbose = 1;

    if (! strcmp (argv [i], "-o"))
    {
      o_present = 1;
      exec_file_name = argv [i + 1];
    }

    if (! strcmp (argv [i], "-g"))
      g_present = 1;

    if (! strcmp (argv [i], "-gnatbind")) {

      /* Explicit naming of binder.  Grab the value then remove the
	 two arguments from the argument list. */

      if ( i + 1 >= *p_argc ) {
	fprintf (stderr, "Missing argument for -gnatbind\n");
	exit (1);
      }

      binder_path = locate_exec (argv [i + 1], ".");
      if (!binder_path) {
        fprintf (stderr, "Could not locate binder: %s\n", argv [i + 1]);
        exit (1);
      }
      for (j = i + 2; j < *p_argc; j++)
        argv [j - 2] = argv [j];
      (*p_argc) -= 2;
      i--;

    }

    else if (! strcmp (argv [i], "-linkonly")) {

      /* Don't call the binder. Set the flag and then remove the
	 argument from the argument list. */

      linkonly = 1;
      for (j = i + 1; j < *p_argc; j++)
	argv [j - 1] = argv [j];
      (*p_argc) -= 1;
      i--;
    }

    else if (! strcmp (argv [i], "-gnatlink")) {

      /* Explicit naming of binder.  Grab the value then remove the
	 two arguments from the argument list. */

      if ( i + 1 >= *p_argc ) {
	fprintf (stderr, "Missing argument for -gnatlink\n");
	exit (1);
      }
      linker_path = locate_exec (argv [i + 1], ".");
      if (!linker_path) {
	fprintf (stderr, "Could not locate linker: %s\n", argv [i + 1]);
	exit (1);
      }
      for (j = i + 2; j < *p_argc; j++)
	argv [j - 2] = argv [j];
      (*p_argc) -= 2;
      i--;
    }
  }
}

main (int argc, char *argv [])
{
  int i,j;
  int done_an_ali = 0;
  int file_name_index;
  int retcode;
  char *pathval = getenv ("PATH");
  char *spawn_args [5];
  int  spawn_index = 0;
  char *bind_src_file_name;
  char *bind_obj_file_name;
  int   bind_file_name_len;

#if defined (__EMX__) || defined(MSDOS)
  char *tmppathval = malloc (strlen (pathval) + 3);
  strcpy (tmppathval, ".;");
  pathval = strcat (tmppathval, pathval);
#endif

  process_args (&argc , argv);

  if (argc == 1) {
    fprintf (stdout,
    "Usage: %s 'name'.ali\n", argv[0]);
    fprintf (stdout,
    "             [-o exec_name]        -- by default it is 'name'\n");
    fprintf (stdout,
    "             [-v]                  -- verbose mode\n");
    fprintf (stdout,
    "             [-linkonly]           -- doesn't call the binder\n");
    fprintf (stdout,
    "             [-gnatbind name]      -- full name for gnatbind\n");
    fprintf (stdout,
    "             [-gnatlink name]      -- full name for the linker (gcc)\n");
    fprintf (stdout,
    "             [list of objects]     -- non Ada binaries\n");
    fprintf (stdout,
    "             [linker options]      -- other options for the linker\n");
    exit (1);
  }
  if (!binder_path && !linkonly)
    binder_path = locate_exec ("gnatbind", pathval);
  if (!binder_path && !linkonly) {
    fprintf (stderr, "Couldn't locate gnatbind\n");
    exit (1);
  }
  if (!linker_path)
    linker_path = locate_exec ("gcc", pathval);
    if (!linker_path) {
      fprintf (stderr, "Couldn't locate gcc\n");
      exit (1);
    }
#ifdef MSDOS
  coff2exe_path = locate_regular_file ("coff2exe.bat", pathval);
  if (!coff2exe_path) {
    fprintf (stderr, "Couldn't locate %s\n", "coff2exe.bat");
    exit (1);
  } else coff2exe_args[0] = coff2exe_path;
#endif
  addarg (linker_path);

  for (i = 1; i < argc; i++) {
    int arg_len = strlen (argv [i]);
    if (arg_len > 4 && ! strcmp (&argv [i][arg_len - 4], ".ali")) {

      if (done_an_ali) {
	fprintf (stderr, "Sorry - cannot handle more than one ALI file\n");
	exit (1);
      }
      done_an_ali = 1;

      if (is_regular_file (argv [i])) {
	int pid;

	if (!linkonly) {
	  /* Run gnatbind */
          spawn_index = 0;
	  spawn_args [spawn_index++] = binder_path;
	  spawn_args [spawn_index++] = argv[i];
	  for (j = 0 ; j <= bind_arg_index ; j++ )
	    spawn_args [spawn_index++] = bind_args [j];
 	  spawn_args [spawn_index] = 0;

          if (verbose) {
            int i;
            for (i = 0; i < 2; i++)
              printf ("%s ", spawn_args [i]);
            putchar ('\n');
          }

	  retcode = portable_spawn (spawn_args);
	  if (retcode != 0) exit (retcode);
	}

	for (file_name_index = arg_len - 4; ; file_name_index--)
	  if (file_name_index == 0
              /* DJGPP could have either or both */
	      || argv [i][file_name_index - 1] == '/'
	      || argv [i][file_name_index - 1] == DIR_SEPARATOR)
	    break;
	bind_file_name_len = strlen (&(argv [i][file_name_index]));
	bind_src_file_name = malloc (bind_file_name_len + 1);
	strcpy  (bind_src_file_name, "b_");
	strncat (bind_src_file_name,
		 &(argv [i][file_name_index]), bind_file_name_len - 3);
	strcat  (bind_src_file_name, "c");

	if (! o_present) {
#if defined (__EMX__)
	  exec_file_name = malloc (bind_file_name_len);
	  strncpy (exec_file_name,
		   &(argv [i][file_name_index]), bind_file_name_len - 3);
	  strcat (exec_file_name, "exe");
#else
	  exec_file_name = malloc (bind_file_name_len - 3);
	  strncpy (exec_file_name,
		   &(argv [i][file_name_index]), bind_file_name_len - 4);
          exec_file_name [bind_file_name_len - 4] = '\0';
#endif
	}

	/* Compile binder output */
	/* ??? should linker and compiler be separate */
        spawn_index = 0;
	spawn_args [spawn_index++] = linker_path;
	spawn_args [spawn_index++] = "-c";
        if (g_present) spawn_args [spawn_index++] = "-g";
        if (gcc_B_arg) spawn_args [spawn_index++] = gcc_B_arg;
	spawn_args [spawn_index++] = bind_src_file_name;
	spawn_args [spawn_index] = 0;

        if (verbose) {
           int i;
           for (i = 0; i < spawn_index; i++)
             printf ("%s ", spawn_args [i]);
           putchar ('\n');
        }

	retcode = portable_spawn (spawn_args);
	if (retcode != 0) exit (retcode);

	bind_obj_file_name = xstrdup (bind_src_file_name);
	bind_obj_file_name [strlen (bind_obj_file_name) - 1] = 'o';
	addarg (bind_obj_file_name);

	proc_bind_file (bind_src_file_name);
      }
      else {
	addarg (argv [i]);
      }
    }
#ifdef MSDOS
    else if (!strcmp (argv [i], "-o")) {

      addarg (argv [i]);
      if (i < argc) i++;
      {
        char* ptr = strstr (argv[i], ".exe");

        arg_len = strlen (argv [i]);
	coff2exe_args[1] = malloc (arg_len + 1);
	strcpy (coff2exe_args[1], argv[i]);
        if (ptr != NULL && strlen (ptr) == 4) {
          coff2exe_args[1][arg_len-4] = 0;
        }
        addarg (coff2exe_args[1]);
      }
    }
#endif
    else {
      addarg (argv [i]);
    }
  }

  if (! done_an_ali) {
    fprintf (stderr, "No \".ali\" file specified\n");
    exit (1);
  }

  if (! o_present) {
    addarg ("-o");
    addarg (exec_file_name);
#ifdef MSDOS
    coff2exe_args[1] = exec_file_name;
#endif
  }

  addarg ("-lgnat");
  addarg (NULL);

  if (verbose) {
    int i;
    for (i = 0; i < link_arg_index; i++)
      printf ("%s ", link_args [i]);
    putchar ('\n');
  }

  retcode = portable_spawn (link_args);
  if (retcode != 0) exit (retcode);

  /* Only keep the binder generated files if the debugging option was
   * specified when the gnatbl was invoked since this implies that the user
   * might want to break within the binder generated file.
   */
  if (!g_present) {
    unlink (bind_src_file_name);
    unlink (bind_obj_file_name);
  }

#ifdef MSDOS
  retcode = portable_spawn (coff2exe_args);
  if (retcode != 0) exit (retcode);
  if (!g_present)
    {
      del_command = malloc (strlen (coff2exe_args[1]) + 5);
      sprintf (del_command, "del %s", coff2exe_args[1]);
      retcode = system (del_command);
    }
#endif

  exit(0);
}

static void
proc_bind_file (char *bind_file_name)
{
  char *ptr, *tptr;
  char buf [1000];
  FILE *bind_file;
  int aliname_len = strlen (bind_file_name);
  char *start_string = "/* BEGIN Object";
  int  start_string_len = strlen (start_string);
  char *end_string   = "   END Object";
  int  end_string_len = strlen (end_string);
  char *argfile_name, *rf;
  FILE *argfile;

  if ((bind_file = fopen (bind_file_name, "r")) == NULL) {
    fprintf (stderr, "Failed to open binder output\n");
    exit (1);
  }

  for (;;) {
    if (fgets (buf, 1000, bind_file) == NULL) {
      fprintf (stderr, "Error reading binder output\n");
      exit (1);
    }
    if (!strncmp (buf, start_string, start_string_len))
      break;
  }

  for (;;) {
    if (fgets (buf, 1000, bind_file) == NULL) {
      fprintf (stderr, "Error reading binder output\n");
      exit (1);
    }
    if (!strncmp (buf, end_string, end_string_len))
      break;

    for (ptr = buf; *ptr && *ptr != '\n'; ptr++)
      ;
    *ptr = '\0';

    addarg (xstrdup (buf));

  }

  fclose (bind_file);
}
