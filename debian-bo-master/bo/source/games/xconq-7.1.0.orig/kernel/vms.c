/* VMS stuff.
   Copyright (C) 1992, 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"

#include <signal.h>

/* #include <sys/time.h> */

#ifndef XCONQLIB
#define XCONQLIB "[-.lib]"
#endif

char *
default_library_filename()
{
    return XCONQLIB;
}

char *
news_filename()
{
    make_pathname(xconqlib, NEWSFILE, "", spbuf);
    return spbuf;
}

char *
save_filename()
{
    return "save.xconq";
}

char *
checkpoint_filename()
{
    return "check.xconq";
}

char *
error_save_filename()
{
	return "errorsave.xconq";
}

char *
statistics_filename()
{
	return "stats.xconq";
}

/* Attempt to open a library file. */

FILE *
open_module_library_file(module)
Module *module;
{
    FILE *fp = NULL;

    /* Don't try to do on anon modules? */
    if (module->name == NULL)
      return NULL;
    /* Generate library pathname. */
    make_pathname(xconqlib, module->name, "g", spbuf);
    /* Now try to open the file. */
    if ((fp = fopen(spbuf, "r")) != NULL) {
	/* Remember the filename where we found it. */
	module->filename = copy_string(spbuf);
    }
    return fp;
}

FILE *
open_module_explicit_file(module)
Module *module;
{
    if (module->filename == NULL)
      return NULL;
    return (fopen(module->filename, "r"));
}

FILE *
open_library_file(filename)
char *filename;
{
    FILE *fp = NULL;

    /* Generate library pathname. */
    make_pathname(xconqlib, filename, NULL, spbuf);
    /* Now try to open the file. */
    if ((fp = fopen(spbuf, "r")) != NULL) {
    }
    return fp;
}

make_pathname(path, name, extn, pathbuf)
char *path, *name, *extn, *pathbuf;
{
    strcpy(pathbuf, "");
    if (!empty_string(path)) {
	strcat(pathbuf, path);
    }
    strcat(pathbuf, name);
    /* Don't add a second identical extension, but do add if extension
       is different (in case we want "foo.12" -> "foo.12.g" for instance) */
    if (strrchr(name, '.')
	&& extn
	&& strcmp(strrchr(name, '.')+1, extn) == 0)
      return;
    if (!empty_string(extn)) {
	strcat(pathbuf, ".");
	strcat(pathbuf, extn);
    }
}

/* Remove a saved game from the system. */

remove_saved_game()
{
}


init_sighandlers()
{
#if 0
    signal(SIGINT, stop_handler);
    if (0 /* don't accidently quit */ && !Debug) {
	signal(SIGINT, SIG_IGN);
    } else {
	signal(SIGINT, SIG_DFL);
/*	signal(SIGINT, crash_handler);  */
    }
    signal(1, crash_handler);
    signal(SIGHUP, hup_handler);
    signal(SIGBUS, crash_handler);
    signal(SIGSEGV, crash_handler);
    signal(SIGFPE, crash_handler);
    signal(SIGILL, crash_handler);
    signal(SIGSYS, crash_handler);
    signal(SIGINT, crash_handler);
    signal(SIGQUIT, crash_handler);
    signal(SIGTERM, crash_handler);
#endif
}

int n_seconds_elapsed(n)
int n;
{
    return FALSE;
}
