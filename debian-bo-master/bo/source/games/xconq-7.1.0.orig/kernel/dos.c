/* MSDOS-specific code for Xconq.
   Copyright (C) 1994 Ed Boston.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "config.h"
#include "misc.h"
#include "dir.h"
#include "lisp.h"
#include "module.h"
#include "system.h"

#include <signal.h>
#include <time.h>
#include <sys/types.h>

typedef void (*fptr)();

#ifndef XCONQLIB
#define XCONQLIB "..\\lib"
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
saved_game_filename()
{
	return "save.xcq";
}

char *
checkpoint_filename()
{
	return "checkpnt.xcq";
}

char *
error_save_filename()
{
	return "errorsav.xcq";
}

char *
statistics_filename()
{
	return "stats.xcq";
}

/* Attempt to open a library file. */

FILE *
open_library_file(module)
Module *module;
{
	FILE *fp = NULL;

	/* Don't try to do on anon modules? */
	if (module->name == NULL) return NULL;
	/* Generate library pathname. */
	make_pathname(xconqlib, module->name, "g", spbuf);
	/* Now try to open the file. */
	if ((fp = fopen(spbuf, "r")) != NULL) {
		/* Remember the filename where we found it. */
		module->filename = copy_string(spbuf);
	} else {
		/* Extra hack to find experimental games. */
		make_pathname("../lib2", module->name, "g", spbuf);
		if ((fp = fopen(spbuf, "r")) != NULL) {
			/* Remember the filename where we found it. */
			module->filename = copy_string(spbuf);
		}
	}
	return fp;
}

FILE *
open_explicit_file(module)
Module *module;
{
	 if (module->filename == NULL) return NULL;
	 return (fopen(module->filename, "r"));
}

FILE *
open_library_file(filename)
char *filename;
{
	FILE *fp = NULL;

	/* Don't try to do on anon modules? */
	/* Generate library pathname. */
	make_pathname(xconqlib, filename, NULL, spbuf);
	/* Now try to open the file. */
	if ((fp = fopen(spbuf, "r")) != NULL) {
	} else {
	}
	return fp;
}

void
make_pathname(path, name, extn, pathbuf)
char *path, *name, *extn, *pathbuf;
{
	sprintf(pathbuf, "");
	if (!empty_string(path)) {
		sprintf(pathbuf+strlen(pathbuf), "%s/", path);
	}
	sprintf(pathbuf+strlen(pathbuf), "%s", name);
	/* Don't add a second identical extension, but do add if extension
		is different (in case we want "foo.12" -> "foo.12.g" for instance) */
	if (strrchr(name, '.') && strcmp((char *) strrchr(name, '.')+1, extn) == 0)
		return;
	if (!empty_string(extn)) {
		sprintf(pathbuf+strlen(pathbuf), ".%s", extn);
	}
}

/* Remove a saved game from the system. */

remove_saved_game()
{
	unlink(saved_game_filename());
}

/* Default behavior on explicit kill. */

void
stop_handler(x)
int x;
{
	close_displays();
	exit(1);
}

/* This routine attempts to save the state before dying. */

void
crash_handler(sig)
int sig;
{
	static int already_been_here = FALSE;

	if (!already_been_here) {
		already_been_here = TRUE;
		close_displays();
		printf("Fatal error encountered. Signal %d\n", sig);
		write_entire_game_state("xconq.ack");
	}
	abort();
}

void
init_signal_handlers(void)
{
	signal(SIGINT, stop_handler);
	signal(SIGSEGV, crash_handler);
	signal(SIGFPE, crash_handler);
	signal(SIGILL, crash_handler);
	signal(SIGINT, crash_handler);
	signal(SIGTERM, crash_handler);
}


time_t reallasttime = (time_t) 0;
time_t realcurtime;

int
n_seconds_elapsed(n)
int n;
{
	time(&realcurtime);
	if (realcurtime > reallasttime + (n - 1)) {
		reallasttime = realcurtime;
		return TRUE;
	} else {
		return FALSE;
	}
}

int
n_ms_elapsed(n)
int n;
{
	return TRUE;
}

void
record_ms()
{
}
