/* Mac-specific code for Xconq.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

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
void mac_abort PARAMS ((void));
extern void add_library_path PARAMS ((char *path));

#ifdef THINK_C
#include <MacHeaders>
#endif /* THINK_C */

#ifdef MPW
#include <Types.h>
#include <Resources.h>
#include <Events.h>  /* for TickCount */
#include <TextUtils.h>  /* for GetIndString */
#endif /* MPW */

#include <signal.h>

/* We need this in order to find string resources that have filenames
   in them. */
#include "macdefs.h"

/* #include <sys/time.h> */

#ifndef c2p
#define c2p(STR,PBUF) \
  strcpy(((char *) PBUF) + 1, STR);  \
  PBUF[0] = strlen(STR);
#endif

#ifndef p2c
#define p2c(PSTR,BUF)  \
  strncpy(BUF, ((char *) (PSTR) + 1), PSTR[0]);  \
  BUF[PSTR[0]] = '\0';
#endif

/* The HFS volume that the program started with. */

short initialvrefnum;

static char *news_fname;
static char *save_fname;
static char *checkpoint_fname;
static char *error_save_fname;
static char *statistics_fname;

#ifndef XCONQLIB
#define XCONQLIB ":lib"
#endif

char *
default_library_filename()
{
    return XCONQLIB;
}

char *
news_filename()
{
    Str255 tmpstr;
    char tmpbuf[255];
    
    if (news_fname == NULL) {
	GetIndString(tmpstr, sFilenames, siNews);
	p2c(tmpstr, tmpbuf);
	if (!empty_string(tmpbuf))
	  news_fname = copy_string(tmpbuf);
	else
	  news_fname = "news.txt";
    }
    return news_fname;
}

char *
saved_game_filename()
{
    Str255 tmpstr;
    char tmpbuf[255];
    
    if (save_fname == NULL) {
	GetIndString(tmpstr, sFilenames, siSavedGame);
	p2c(tmpstr, tmpbuf);
	if (!empty_string(tmpbuf))
	  save_fname = copy_string(tmpbuf);
	else
	  save_fname = "Saved Game";
    }
    return save_fname;
}

char *
checkpoint_filename()
{
    Str255 tmpstr;
    char tmpbuf[255];

    if (checkpoint_fname == NULL) {
	GetIndString(tmpstr, sFilenames, siCheckpoint);
	p2c(tmpstr, tmpbuf);
	if (!empty_string(tmpbuf))
	  checkpoint_fname = copy_string(tmpbuf);
	else
	  checkpoint_fname = "Checkpoint";
    }
    return checkpoint_fname;
}

char *
error_save_filename()
{
    Str255 tmpstr;
    char tmpbuf[255];

    if (error_save_fname == NULL) {
	GetIndString(tmpstr, sFilenames, siErrorSave);
	p2c(tmpstr, tmpbuf);
	if (!empty_string(tmpbuf))
	  error_save_fname = copy_string(tmpbuf);
	else
	  error_save_fname = "Error Save";
    }
    return error_save_fname;
}

char *
statistics_filename()
{
    Str255 tmpstr;
    char tmpbuf[255];

    if (statistics_fname == NULL) {
	GetIndString(tmpstr, sFilenames, siStatistics);
	p2c(tmpstr, tmpbuf);
	if (!empty_string(tmpbuf))
	  statistics_fname = copy_string(tmpbuf);
	else
	  statistics_fname = "Statistics";
    }
    return statistics_fname;
}

/* Attempt to open a library file. */

FILE *
open_module_library_file(Module *module)
{
    short curvrefnum;
    char fullnamebuf[255];
    LibraryPath *p;
    FILE *fp;
    
    /* Can't open anonymous library modules. */
    if (module->name == NULL)
      return NULL;
    /* Generate library pathname. */
    for_all_library_paths(p) {
	make_pathname(p->path, module->name, "g", fullnamebuf);
	/* Now try to open the file. */
	fp = fopen(fullnamebuf, "r");
	if (fp != NULL) {
	    /* Remember the filename where we found it. */
	    module->filename = copy_string(fullnamebuf);
	    return fp;
	}
	/* Try the same name in a different directory. */
	GetVol(NULL, &curvrefnum);
	SetVol(NULL, initialvrefnum);
	fp = fopen(fullnamebuf, "r");
	SetVol(NULL, curvrefnum);
	if (fp != NULL) {
	    /* Remember the filename (what about volume?) where we found it. */
	    module->filename = copy_string(fullnamebuf);
	    return fp;
	}
    }
    return NULL;
}

FILE *
open_module_explicit_file(Module *module)
{
    short curvrefnum;
    char fullnamebuf[255];
    LibraryPath *p;
    FILE *fp = NULL;
    
    if (module->filename == NULL) {
	/* Try guessing a filename, since none supplied. */
	if (module->name != NULL) {
	    for_all_library_paths(p) {
		make_pathname(p->path, module->name, "g", fullnamebuf);
		/* Now try to open the file. */
		fp = fopen(fullnamebuf, "r");
		if (fp != NULL)
		  return fp;
		GetVol(NULL, &curvrefnum);
		SetVol(NULL, initialvrefnum);
		fp = fopen(fullnamebuf, "r");
		SetVol(NULL, curvrefnum);
		if (fp != NULL)
		  return fp;
	    }
	}
    } else {
	/* Try some other random ideas. */
	sprintf(fullnamebuf, "%s", module->filename);
	fp = fopen(module->filename, "r");
	if (fp != NULL) {
	    add_library_path("");
	    return fp;
	}
	sprintf(fullnamebuf, ":%s", module->filename);
	fp = fopen(fullnamebuf, "r");
	if (fp != NULL)
	  return fp;
	sprintf(fullnamebuf, "%s%s", ":lib:", module->filename);
	fp = fopen(fullnamebuf, "r");
	if (fp != NULL)
	  return fp;
	/* Try opening a library module under where the program started. */
	GetVol(NULL, &curvrefnum);
	SetVol(NULL, initialvrefnum);
	fp = fopen(fullnamebuf, "r");
	SetVol(NULL, curvrefnum);
    }
    return fp;
}

FILE *
open_library_file(char *filename)
{
    short curvrefnum;
    char fullnamebuf[255];
    LibraryPath *p;
    FILE *fp;
	
    /* Now try to open the file. */
    fp = fopen(filename, "r");
    if (fp != NULL)
      return fp;
    /* Generate library pathname. */
    for_all_library_paths(p) {
	make_pathname(p->path, filename, NULL, fullnamebuf);
	/* Now try to open the file. */
	fp = fopen(fullnamebuf, "r");
	if (fp != NULL)
	  return fp;
	/* Change to volume where the program started. */
	GetVol(NULL, &curvrefnum);
	SetVol(NULL, initialvrefnum);
	fp = fopen(fullnamebuf, "r");
	SetVol(NULL, curvrefnum);
	if (fp != NULL)
	  return fp;
    }
    return NULL;
}

FILE *
open_scorefile_for_reading(char *name)
{
    short curvrefnum;
    FILE *fp;
	
    GetVol(NULL, &curvrefnum);
    SetVol(NULL, initialvrefnum);
    /* Now try to open the file. */
    fp = fopen(name, "r");
    SetVol(NULL, curvrefnum);
    return fp;
}

FILE *
open_scorefile_for_writing(char *name)
{
    short curvrefnum;
    FILE *fp;
	
    GetVol(NULL, &curvrefnum);
    SetVol(NULL, initialvrefnum);
    /* Now try to open the file. */
    fp = fopen(name, "a");
    SetVol(NULL, curvrefnum);
    return fp;
}

void
make_pathname(char *path, char *name, char *extn, char *pathbuf)
{
    strcpy(pathbuf, "");
    if (!empty_string(path)) {
	strcat(pathbuf, path);
	strcat(pathbuf, ":");
    }
    strcat(pathbuf, name);
    /* Don't add a second identical extension, but do add if extension
       is different (in case we want "foo.12" -> "foo.12.g" for instance) */
    if (strrchr(name, '.')
	&& extn
	&& strcmp(strrchr(name, '.') + 1, extn) == 0)
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
}

void
init_signal_handlers()
{
}

int last_ticks = 0;

int
n_seconds_elapsed(n)
int n;
{
    int ticks = TickCount();

    if (((ticks - last_ticks) / 60) > n) {
	last_ticks = ticks;
    	return TRUE;
    } else {
	return FALSE;
    }
}

int last_ticks_for_ms = 0;

int
n_ms_elapsed(n)
int n;
{
    return (((TickCount() - last_ticks_for_ms) * 16) > n);
}

void
record_ms()
{
    last_ticks_for_ms = TickCount();
}

/* Instead of coredumping, which is not a normal Mac facility, we
   drop into Macsbug.  If we then "g" from Macsbug, the program will
   exit cleanly. */

void
mac_abort ()
{
    /* Make sure no output still buffered up, then zap into MacsBug. */
#if 0 /* how to know if stdio in use? */
    fflush(stdout);
    fflush(stderr);
    printf("## Abort! ##\n");
#endif
#ifdef MPW_SADE
    SysError(8005);
#else 
    Debugger();
#endif
    /* "g" in MacsBug will then cause a regular error exit. */
    exit(1);
}

