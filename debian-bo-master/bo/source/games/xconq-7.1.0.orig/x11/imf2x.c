/* Convert the Xconq image format to X11 bitmaps and pixmaps.
   Copyright (C) 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "config.h"
#include "misc.h"
#include "lisp.h"
#include "imf.h"
#include "xutil.h"

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>

extern int numimages;

extern ImageFamily **images;

extern char *outdirname;

char imdirname[1000];

int showprogress = 0;

void usage PARAMS ((void));

int
main(argc, argv)
int argc;
char *argv[];
{
    int i, rslt;
    char *arg;
    FILE *ofp;

    init_lisp();

    if (argc == 1) usage();
    for (i = 1; i < argc; ++i) {
	arg = argv[i];
	if (strcmp(arg, "-o") == 0) {
	    if (i + 1 < argc) {
		outdirname = argv[i + 1];
		/* Blast the arg because we'll be scanning the args again
		   and we want to ignore it then. */
		argv[i] = NULL;
		argv[i + 1] = NULL;
		++i;
	    } else {
		init_error("No output directory following -o");
	    }
	} else if (strcmp(arg, "-p") == 0) {
	    showprogress = 1;
	    argv[i] = NULL;
	} else if (strcmp(arg, "--help") == 0) {
	    usage();
	    argv[i] = NULL;
	}
    }
    /* We prefer a -o spec in order to do output, as a safety precaution;
       one can always give "." as an argument so as to dump the bitmaps
       into the current directory. */
    if (outdirname == NULL)
      init_warning("No output directory specified");
    for (i = 1; i < argc; ++i) {
	if (argv[i] != NULL) {
	    /* Interpret the arg as an imf file, open and read it. */
	    rslt = load_imf_file(argv[i], NULL);
	    if (!rslt)
	      run_warning("Couldn't open \"%s\", ignoring", argv[i]);
	}
    }
    if (outdirname != NULL) {
	/* Write the image directory file. */
	sprintf(imdirname, "%s/%s", outdirname, "imf.dir");
	ofp = fopen(imdirname, "w");
    } else {
	ofp = NULL;
    }
    for (i = 0; i < numimages; ++i) {
	printf("/* %s imf */\n", images[i]->name);
	if (ofp != NULL)
	  fprintf(ofp, "%s\n", images[i]->name);
	write_x11_bitmaps(images[i], (ofp != NULL));
    }
    if (ofp != NULL) {
	fclose(ofp);
    }
    return 0;
}

void
usage()
{
    fprintf(stderr,
	    "usage: imf2x [ files ... ] -o outdir [ files ... ] [ -p ]\n");
    exit(1);
}
