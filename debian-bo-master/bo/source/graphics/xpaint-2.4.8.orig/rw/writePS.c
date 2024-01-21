/* +-------------------------------------------------------------------+ */
/* | Copyright 1993, David Koblas (koblas@netcom.com)		       | */
/* |								       | */
/* | Permission to use, copy, modify, and to distribute this software  | */
/* | and its documentation for any purpose is hereby granted without   | */
/* | fee, provided that the above copyright notice appear in all       | */
/* | copies and that both that copyright notice and this permission    | */
/* | notice appear in supporting documentation.	 There is no	       | */
/* | representations about the suitability of this software for	       | */
/* | any purpose.  this software is provided "as is" without express   | */
/* | or implied warranty.					       | */
/* |								       | */
/* +-------------------------------------------------------------------+ */

/* $Id: writePS.c,v 1.3 1996/04/19 09:34:34 torsten Exp $ */

#include <stdio.h>
#include "image.h"
#ifdef sco
#include <time.h>
#else
#include <sys/time.h>
#endif
#include <string.h>

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

static void writePreview(FILE * fd, Image * image)
{
}

#if 0
static void writeCmap(FILE * fd, Image * image)
{
    int runlen, maxCount;
    int col, used;
    int i, x, y;
    unsigned char *ucp;
    char buf[280];
    short map[256];
    int isRLE;

    /*
    **	Remap the colors to the number used
     */
    for (i = 0; i < 256; i++)
	map[i] = -1;
    for (ucp = image->data, y = 0; y < image->width * image->height; y++, ucp++)
	map[*ucp] = 1;
    for (used = i = 0; i < 256; i++)
	if (map[i] != -1)
	    map[i] = used++;

    /*
    **	If compression will help
     */
    fprintf(fd, "/rgbmap %d string def\n", used * 3);
    fputs("% RLE drawing routine\n\
/buffer 2 string def							\n\
/rgb (000) def								\n\
/drawcolormappedimage {							\n\
  /buffer  1   string def						\n\
  /rgbval  3   string def						\n\
  /npixels 384 string def	% 128 * 3				\n\
  { currentfile buffer readhexstring pop  % read run information	\n\
    /len exch 0 get store						\n\
    len 128 ge								\n\
    { 0 1 len 128 sub	 % this is a run of raw data			\n\
      { currentfile buffer readhexstring pop pop			\n\
	/rgbval rgbmap buffer 0 get 3 mul 3 getinterval store		\n\
	npixels exch 3 mul rgbval putinterval				\n\
      } for								\n\
      npixels 0 len 127 sub 3 mul getinterval				\n\
    }									\n\
    { % else it's run data						\n\
      currentfile buffer readhexstring pop pop				\n\
      /rgbval rgbmap buffer 0 get 3 mul 3 getinterval store		\n\
      0 1 len { npixels exch 3 mul rgval putinterval } for		\n\
      npixels 0 len 1 add 3 mul getinterval				\n\
    } ifelse								\n\
  }									\n\
  false 3 colorimage							\n\
} bind def\n", fd);

    fprintf(fd, "%%%% get the rgb map\n");
    fprintf(fd, "currentfile rgbmap readhexstring\n");

    ucp = image->cmapData;
    for (col = i = 0; i < image->cmapSize; i++, ucp += 3) {
	if (map[i] == -1)
	    continue;

	fprintf(fd, "%02.2x%02.2x%02.2x ", ucp[0], ucp[1], ucp[2]);
	if (++col == 10) {
	    putc('\n', fd);
	    col = 0;
	}
    }
    if (col != 0)
	putc('\n', fd);
    fprintf(fd, "pop pop\n\n");

    fprintf(fd, "%d %d 8\n", image->width, image->height);
    fprintf(fd, "[ %d 0 0 -%d 0 %d ]\n",
	    image->width, image->height, image->height);
    fprintf(fd, "drawcolormappedimage\n");

    ucp = image->data;
    maxCount = 0x7f;

#define PUT(fd, val) do {			\
	fprintf(fd, "%02.2x", val);		\
	if (++col == 35)			\
		{ col = 0; putc('\n', fd); }	\
	} while (0)

#define	WRITE(fd, flg, buf, len) do { int _i;			\
	PUT(fd, (len) + (flg ? 0 : 128));			\
	if (flg)						\
		PUT(fd, map[buf[0]]);				\
	else							\
		for (_i = 0; _i < (len); _i++)			\
			PUT(fd, map[buf[_i]]);			\
	putc(' ', fd);						\
	} while (0)

    col = 0;
    for (y = 0; y < image->height; y++) {
	int prev;

	runlen = 0;
	for (x = 0; x < image->width; x++, ucp++) {
	    int v = *ucp;

	    if (runlen == 0)
		isRLE = TRUE;
	    else if (runlen == maxCount) {
		WRITE(fd, isRLE, buf, runlen);
		runlen = 0;
		isRLE = TRUE;
	    } else if (isRLE && v != prev) {
		if (runlen == 1)
		    isRLE = FALSE;
		else {
		    WRITE(fd, isRLE, buf, runlen);
		    runlen = 0;
		}
	    } else if (!isRLE && v == prev) {
		WRITE(fd, isRLE, buf, runlen);
		buf[0] = prev;
		runlen = 1;
		isRLE = TRUE;
	    }
	    buf[runlen++] = v;
	    prev = v;
	}

	if (runlen != 0)
	    WRITE(fd, isRLE, buf, runlen);
	col = 0;
	putc('\n', fd);
    }
    putc('\n', fd);
#undef WRITE
#undef PUT
}
#endif

static void writeRGB(FILE * fd, Image * image)
{
    unsigned char *ucp;
    int x, y;
    int col = 0;

    fprintf(fd, "/line %d string def\n", image->width * 3);
    fprintf(fd, "%d %d 8\n", image->width, image->height);
    fprintf(fd, "[ %d 0 0 -%d 0 %d ]\n",
	    image->width, image->height, image->height);
    fprintf(fd, "{currentfile line readhexstring pop}\n");
    fprintf(fd, "false 3 colorimage\n");

    for (y = 0; y < image->height; y++)
	for (x = 0; x < image->width; x++) {
	    ucp = ImagePixel(image, x, y);
	    fprintf(fd, "%02x%02x%02x", ucp[0], ucp[1], ucp[2]);
	    if (++col == 12) {
		putc('\n', fd);
		col = 0;
	    }
	}
    if (col != 0)
	putc('\n', fd);
}

int WritePS(char *file, Image * image)
{
    FILE *fd;
    char *cp, buf[256];
    time_t t;

    if ((fd = fopen(file, "w")) == NULL)
	return 1;

    if ((cp = strrchr(file, '/')) == NULL)
	cp = file;
    else
	cp++;
    strcpy(buf, cp);
    if ((cp = strrchr(buf, '.')) != NULL)
	*cp = '\0';

    time(&t);
    fprintf(fd, "%%!PS-Adobe-2.0 EPSF-2.0\n");
    fprintf(fd, "%%%%Creator: XPaint\n%%%%Title: %s\n%%%%Pages: 1\n", buf);
    fprintf(fd, "%%%%BoundingBox: %d %d %d %d\n", 0, 0,
	    image->width, image->height);
    fprintf(fd, "%%%%CreationDate: %s", ctime(&t));
    fprintf(fd, "%%%%EndComments\n");

    /*
    **	Write a preview image
     */

    writePreview(fd, image);

    fprintf(fd, "%%%%EndProlog\n%%%%Page: 1 1\n");
    fprintf(fd, "\n\ngsave\n\n");

    /*
    **	Write the actual image
     */

    fprintf(fd, "/inch {72 mul} def\n");
    fprintf(fd, "%d %d scale\n", image->width, image->height);

#if 0
    if (image->cmapSize == 0 || image->cmapSize > 256)
	writeRGB(fd, image);
    else
	writeCmap(fd, image);
#else
    writeRGB(fd, image);
#endif

    fprintf(fd, "%%\n\ngrestore\nshowpage\n%%%%Trailer\n");

    fclose(fd);

    return 0;
}
