/* This program converts a collection of X bitmaps into imf format.
   Copyright (C) 1992, 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "config.h"
#include "misc.h"
#include "lisp.h"
#include "imf.h"
#include "xutil.h"

#include <string.h>

extern int numimages;

extern ImageFamily **images;

char tmpbuf[500];

int
main(argc, argv)
int argc;
char *argv[];
{
    int i;

    for (i = 1; i < argc; ++i) {
	if (!read_any_file(argv[i], NULL)) {
	    fprintf(stderr, "Couldn't read \"%s\"\n", argv[i]);
	}
    }
    sort_all_images();
    /* Now write out all the images that were read. */
    for (i = 0; i < numimages; ++i) {
	reverse_rawdata(images[i]);
	/* Note that the generic image data slots were filled in upon
	   bitmap reading, so the images are ready to write out. */
	fprintf(stderr, "; %s\n", images[i]->name);
	write_imf(stdout, images[i]);
    }
    return 0;
}
