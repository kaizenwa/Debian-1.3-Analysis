
/* 
 *     XaoS, a fast portable realtime fractal zoomer 
 *                  Copyright (C) 1996,1997 by
 *
 *      Jan Hubicka          (hubicka@paru.cas.cz)
 *      Thomas Marsh         (tmarsh@austin.ibm.com)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
#ifdef _plan9_
#include <u.h>
#include <libc.h>
#include <stdio.h>
#include "config.h"
#else
#include <stdio.h>
#ifdef __EMX__
#include <sys/types.h>
#endif
#ifndef _MAC
#include <sys/stat.h>
#endif
#include <unistd.h>
#include <stdlib.h>
#endif
#include "zoom.h"
#include "gif.h"

#ifdef _MAC
#include "stdioMac.h"
#endif

static int nimage = 0;
static zoom_context *context;



/*****************************************************************************
 *
 * GIFENCODE.C    - GIF Image compression interface
 *
 * GIFEncode( FName, GHeight, GWidth, GInterlace, Background, 
 *	      BitsPerPixel, Red, Green, Blue, GetPixel )
 *
 *****************************************************************************/


extern void compress(int, FILE *, int (*)(int, int));
static void Putword(int, FILE *);

/*
 * Pointer to function returning an int
 */

typedef int (*ifunptr) (int, int);

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

int GIFNextPixel(ifunptr getpixel);

static int Width, Height;
static int curx, cury;
static long CountDown;
static int Pass = 0;
static int Interlace;

/*
 * Bump the 'curx' and 'cury' to point to the next pixel
 */
static void BumpPixel(void)
{
    /*
     * Bump the current X position
     */
    curx++;

    /*
     * If we are at the end of a scan line, set curx back to the beginning
     * If we are interlaced, bump the cury to the appropriate spot,
     * otherwise, just increment it.
     */
    if (curx == Width) {
	curx = 0;

	if (!Interlace)
	    cury++;
	else {
	    switch (Pass) {

	    case 0:
		cury += 8;
		if (cury >= Height) {
		    Pass++;
		    cury = 4;
		}
		break;

	    case 1:
		cury += 8;
		if (cury >= Height) {
		    Pass++;
		    cury = 2;
		}
		break;

	    case 2:
		cury += 4;
		if (cury >= Height) {
		    Pass++;
		    cury = 1;
		}
		break;

	    case 3:
		cury += 2;
		break;
	    }
	}
    }
}

/*
 * Return the next pixel from the image
 */
int GIFNextPixel(getpixel)
ifunptr getpixel;
{
    int r;

    if (CountDown == 0)
	return EOF;

    CountDown--;

    r = (*getpixel) (curx, cury);

    BumpPixel();

    return r;
}

static void GIFEncode(char *FName, int GWidth, int GHeight, int GInterlace, int Background, int BitsPerPixel, char *Red, char *Green, char *Blue, ifunptr GetPixel)
{
    FILE *fp;
    int B;
    int RWidth, RHeight;
    int LeftOfs, TopOfs;
    int Resolution;
    int ColorMapSize;
    int InitCodeSize;
    int i;

    Interlace = GInterlace;

    ColorMapSize = 1 << BitsPerPixel;

    RWidth = Width = GWidth;
    RHeight = Height = GHeight;
    LeftOfs = TopOfs = 0;

    Resolution = BitsPerPixel;

    /*
     * Calculate number of bits we are expecting
     */
    CountDown = (long) Width *(long) Height;

    /*
     * Indicate which pass we are on (if interlace)
     */
    Pass = 0;

    /*
     * The initial code size
     */
    if (BitsPerPixel <= 1)
	InitCodeSize = 2;
    else
	InitCodeSize = BitsPerPixel;

    /*
     * Set up the current x and y position
     */
    curx = cury = 0;

    /*
     * Open the GIF file for binary write
     */
#ifndef _MAC
    fp = fopen(FName, "wb");
#else
    fp = fopenMac(FName, "wb", 'XAOS', 'TEXT');
#endif
    if (fp == (FILE *) 0) {
	printf("error: could not open output file\n");
	exit(1);
    }
    /*
     * Write the Magic header
     */
#ifndef _MAC
    fwrite("GIF87a", 1, 6, fp);
#else
    fwriteMac("GIF87a", 1, 6, fp);
#endif

    /*
     * Write out the screen width and height
     */
    Putword(RWidth, fp);
    Putword(RHeight, fp);

    /*
     * Indicate that there is a global colour map
     */
    B = 0x80;			/* Yes, there is a color map */

    /*
     * OR in the resolution
     */
    B |= (Resolution - 1) << 5;

    /*
     * OR in the Bits per Pixel
     */
    B |= (BitsPerPixel - 1);

    /*
     * Write it out
     */
#ifndef _MAC
    fputc(B, fp);
#else
    fputcMac(B, fp);
#endif

    /*
     * Write out the Background colour
     */
#ifndef _MAC
    fputc(Background, fp);
#else
    fputcMac(Background, fp);
#endif

    /*
     * Byte of 0's (future expansion)
     */
#ifndef _MAC
    fputc(0, fp);
#else
    fputcMac(0, fp);
#endif

    /*
     * Write out the Global Colour Map
     */
    for (i = 0; i < ColorMapSize; i++) {
#ifndef _MAC
	fputc(Red[i], fp);
	fputc(Green[i], fp);
	fputc(Blue[i], fp);
#else
	fputcMac(Red[i], fp);
	fputcMac(Green[i], fp);
	fputcMac(Blue[i], fp);
#endif
    }

    /*
     * Write an Image separator
     */
#ifndef _MAC
    fputc(',', fp);
#else
    fputcMac(',', fp);
#endif

    /*
     * Write the Image header
     */

    Putword(LeftOfs, fp);
    Putword(TopOfs, fp);
    Putword(Width, fp);
    Putword(Height, fp);

    /*
     * Write out whether or not the image is interlaced
     */
    if (Interlace)
#ifndef _MAC
	fputc(0x40, fp);
#else
	fputcMac(0x40, fp);
#endif
    else
#ifndef _MAC
	fputc(0x00, fp);
#else
	fputcMac(0x00, fp);
#endif

    /*
     * Write out the initial code size
     */
#ifndef _MAC
    fputc(InitCodeSize, fp);
#else
    fputcMac(InitCodeSize, fp);
#endif

    /*
     * Go and actually compress the data
     */
    compress(InitCodeSize + 1, fp, GetPixel);

    /*
     * Write out a Zero-length packet (to end the series)
     */
#ifndef _MAC
    fputc(0, fp);
#else
    fputcMac(0, fp);
#endif

    /*
     * Write the GIF file terminator
     */
#ifndef _MAC
    fputc(';', fp);
#else
    fputcMac(';', fp);
#endif

    /*
     * And close the file
     */
#ifndef _MAC
    fclose(fp);
#else
    fcloseMac(fp);
#endif

}

/*
 * Write out a word to the GIF file
 */
static void Putword(w, fp)
int w;
FILE *fp;
{
#ifndef _MAC
    fputc(w & 0xff, fp);
    fputc((w / 256) & 0xff, fp);
#else
    fputcMac(w & 0xff, fp);
    fputcMac((w / 256) & 0xff, fp);
#endif
}

/*********************************************/

static int calc(i, j)
int i;
int j;

{
    if (context->stereogram)
	return ((unsigned char) *(context->svbuff + i + context->scanline * j));
    return ((unsigned char) *(context->vbuff + i + context->scanline * j));
}

char *writegif(zoom_context * c)
{

    static char name[256];
#ifdef _MAC
    SFReply myReply;
    Point pt =
    {100, 100};
    SFTypeList myTypes =
    {'TEXT', 'TEXT', 'TEXT', 'TEXT'};
#endif

#ifndef _MAC			/* not currently supported */

#ifdef _plan9_
    char edir[DIRLEN];
#else
    struct stat sb;
#endif
#endif				/* _MAC */
    context = c;
#ifndef _MAC			/* not currently supported */
    do {
	sprintf(name, "fract%i.gif", nimage++);
#ifndef _plan9_
    } while (stat(name, &sb) != -1);
#else
    } while (stat(name, edir) != -1);
#endif
#else				/* _MAC */

    /* Mac version of getting a filename to write to.
       Here we bring up a standard putfile dialog and
       convert the FSspec to a pathname. */
    SFPutFile(pt, "\pSave GIF image as:", "\pUntitled fractal.gif", nil, &myReply);
    if (!myReply.good) {
	sprintf(name, "not");	/* HACK: caller displays 'file not saved' */
	return name;
    }
    FilespecFromSFReply(&myReply, (unsigned char *) name);
    p2cstr((unsigned char *) name);

#endif				/* _MAC */
    GIFEncode(name, c->width, c->height, 1, 0, 8, (char *) c->cmap[0], (char *) c->cmap[1], (char *) c->cmap[2], calc);
    return name;
}
