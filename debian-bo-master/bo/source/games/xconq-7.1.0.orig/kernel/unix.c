/* Unix-specific code for Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* Unix interface stuff.  Do NOT attempt to use this file in a non-Unix
   system, not even an ANSI one! */

#include "config.h"
#include "misc.h"
#include "dir.h"
#include "lisp.h"
#include "module.h"
#include "system.h"

extern void close_displays PARAMS ((void));

#include <signal.h>
#include <unistd.h>
#include <sys/time.h>

#ifndef XCONQLIB
#define XCONQLIB "../lib"
#endif

char *
default_library_filename()
{
    return XCONQLIB;
}

char *
news_filename()
{
    /* (should search in library list) */
    make_pathname(xconq_libs->path, NEWSFILE, "", spbuf);
    return spbuf;
}

char *
saved_game_filename()
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
    return "ack!.xconq";
}

char *
statistics_filename()
{
    return STATSFILE;
}

/* Attempt to open a library file. */

FILE *
open_module_library_file(module)
Module *module;
{
    LibraryPath *p;
    FILE *fp;

    /* Don't try to do on anon modules? */
    if (module->name == NULL)
      return NULL;
    for_all_library_paths(p) {
	/* Generate library pathname. */
	make_pathname(p->path, module->name, "g", spbuf);
	/* Now try to open the file. */
	fp = fopen(spbuf, "r");
	if (fp != NULL) {
	    /* Remember the filename where we found it. */
	    module->filename = copy_string(spbuf);
	    return fp;
	}
    }
    return NULL;
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
    char fullnamebuf[1024];
    LibraryPath *p;
    FILE *fp = NULL;

    fp = fopen(filename, "r");
    if (fp != NULL) {
	return fp;
    }
    for_all_library_paths(p) {
	/* Generate library pathname. */
	make_pathname(p->path, filename, NULL, fullnamebuf);
	fp = fopen(fullnamebuf, "r");
	if (fp != NULL) {
	    return fp;
	}
    }
    return NULL;
}

FILE *
open_scorefile_for_reading(name)
char *name;
{
    FILE *fp;
	
    fp = fopen(name, "r");
    return fp;
}

FILE *
open_scorefile_for_writing(name)
char *name;
{
    FILE *fp;
	
    fp = fopen(name, "a");
    return fp;
}

void
make_pathname(path, name, extn, pathbuf)
char *path, *name, *extn, *pathbuf;
{
    strcpy(pathbuf, "");
    if (!empty_string(path)) {
	strcat(pathbuf, path);
	strcat(pathbuf, "/");
    }
    strcat(pathbuf, name);
    /* Don't add a second identical extension, but do add if extension
       is different (in case we want "foo.12" -> "foo.12.g" for instance) */
    if (strrchr(name, '.')
	&& extn
	&& strcmp((char *) strrchr(name, '.') + 1, extn) == 0)
      return;
    if (!empty_string(extn)) {
	strcat(pathbuf, ".");
	strcat(pathbuf, extn);
    }
}

/* Remove a saved game from the system. */

void
remove_saved_game()
{
    unlink(saved_game_filename());
}

/* Default behavior on explicit kill. */

void
stop_handler(sig, code, scp, addr)
int sig, code;
struct sigcontext *scp;
char *addr;     
{
    close_displays();
    exit(1);
}

/* This routine attempts to save the state before dying. */

void
crash_handler(sig, code, scp, addr)
int sig, code;
struct sigcontext *scp;
char *addr;     
{
    static int already_been_here = FALSE;

    if (!already_been_here) {
	already_been_here = TRUE;
	close_displays();  
	printf("Fatal error encountered. Signal %d code %d\n", sig, code);
	write_entire_game_state("ack!.xconq");
    }
    abort();
}

/* Accidental disconnection saves state. */

void
hup_handler(sig, code, scp, addr)
int sig, code;
struct sigcontext *scp;
char *addr;     
{
    static int already_been_here = FALSE;

    if (!already_been_here) {
	already_been_here = TRUE;
	close_displays();
	printf("Somebody was disconnected, saving the game.\n");
	write_entire_game_state("ack!.xconq");
    }
    abort();
}

void
init_signal_handlers()
{
    signal(SIGINT, stop_handler);
    if (0 /* don't accidently quit */ && !Debug) {
	signal(SIGINT, SIG_IGN);
    } else {
	signal(SIGINT, SIG_DFL);
/*	signal(SIGINT, crash_handler);  */
    }
    signal(SIGHUP, hup_handler);
    signal(SIGSEGV, crash_handler);
    signal(SIGFPE, crash_handler);
    signal(SIGILL, crash_handler);
    signal(SIGINT, crash_handler);
    signal(SIGQUIT, crash_handler);
    signal(SIGTERM, crash_handler);
    /* The following signals may not be available everywhere. */
#ifdef SIGBUS
    signal(SIGBUS, crash_handler);
#endif /* SIGBUS */
#ifdef SIGSYS
    signal(SIGSYS, crash_handler);
#endif /* SIGSYS */
}

struct timeval reallasttime = { 0, 0 };

struct timeval realcurtime;

int
n_seconds_elapsed(n)
int n;
{
    gettimeofday(&realcurtime, NULL);
    if (realcurtime.tv_sec > (reallasttime.tv_sec + (n - 1))) {
	reallasttime = realcurtime;
	return TRUE;
    } else {
	return FALSE;
    }
}

struct timeval reallastmstime = { 0, 0 };

int
n_ms_elapsed(n)
int n;
{
    int interval;
    struct timeval tmprealtime;

    gettimeofday(&tmprealtime, NULL);
    interval =
      (tmprealtime.tv_sec - reallastmstime.tv_sec) * 1000
	+ (tmprealtime.tv_usec - reallastmstime.tv_usec) / 1000;
    return (interval > n);
}

void
record_ms()
{
    gettimeofday(&reallastmstime, NULL);
}

