/*	
 *   xtel - Emulateur MINITEL sous X11
 *
 *   Copyright (C) 1991-1996  Lectra Systemes & Pierre Ficheux
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
static char rcsid[] = "$Id: imprime.c,v 1.4 1996/10/02 23:45:32 pierre Exp $";

#include "xtel.h"

/*
 * Impression de page (merci le M.I.T !!!)
 */
 
/* Copyright 1987 Massachusetts Institute of Technology */

/*
 * xwd.c MIT Project Athena, X Window system window raster image dumper.
 *
 * This program will dump a raster image of the contents of a window into a 
 * file for output on graphics printers or for other uses.
 *
 *  Author:	Tony Della Fera, DEC
 *		17-Jun-85
 * 
 *  Modification history:
 *
 *  11/14/86 Bill Wyatt, Smithsonian Astrophysical Observatory
 *    - Removed Z format option, changing it to an XY option. Monochrome 
 *      windows will always dump in XY format. Color windows will dump
 *      in Z format by default, but can be dumped in XY format with the
 *      -xy option.
 *
 *  11/18/86 Bill Wyatt
 *    - VERSION 6 is same as version 5 for monchrome. For colors, the 
 *      appropriate number of Color structs are dumped after the header,
 *      which has the number of colors (=0 for monochrome) in place of the
 *      V5 padding at the end. Up to 16-bit displays are supported. I
 *      don't yet know how 24- to 32-bit displays will be handled under
 *      the Version 11 protocol.
 *
 *  6/15/87 David Krikorian, MIT Project Athena
 *    - VERSION 7 runs under the X Version 11 servers, while the previous
 *      versions of xwd were are for X Version 10.  This version is based
 *      on xwd version 6, and should eventually have the same color
 *      abilities. (Xwd V7 has yet to be tested on a color machine, so
 *      all color-related code is commented out until color support
 *      becomes practical.)
 */

#ifndef lint
static char *rcsid_xwd_c = "$XConsortium: xwd.c,v 1.51 89/12/10 16:49:07 rws Exp $";
#endif

/*%
 *%    This is the format for commenting out color-related code until
 *%  color can be supported.
%*/

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xos.h>
#include <X11/Xmu/WinUtil.h>

#define FEEP_VOLUME 0

/* Include routines to do parsing */
/*#include "dsimple.h"*/

static Display *display;                                 /* The current display */
static int screen_num;                                   /* The current screen */

/* Setable Options */

static int format = ZPixmap;
static Bool nobdrs = False;
static Bool standard_out = True;
static Bool debug = False;
static long add_pixel_value = 0;

extern int (*_XErrorFunction)();
extern int _XDefaultError();

/*
 * outl: a debugging routine.  Flushes stdout then prints a message on stderr
 *       and flushes stderr.  Used to print messages when past certain points
 *       in code so we can tell where we are.  Outl may be invoked like
 *       printf with up to 7 arguments.
 */
static int outl(msg, arg0,arg1,arg2,arg3,arg4,arg5,arg6)
     char *msg;
     char *arg0, *arg1, *arg2, *arg3, *arg4, *arg5, *arg6;
{
	fflush(stdout);
	fprintf(stderr, msg, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
	fprintf(stderr, "\n");
	fflush(stderr);
}


/*
 * Standard fatal error routine - call like printf but maximum of 7 arguments.
 * Does not require display or screen defined.
 */
static void Fatal_Error(msg, arg0,arg1,arg2,arg3,arg4,arg5,arg6)
char *msg;
char *arg0, *arg1, *arg2, *arg3, *arg4, *arg5, *arg6;
{
	fflush(stdout);
	fflush(stderr);
	fprintf(stderr, msg, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
	fprintf(stderr, "\n");
	exit (1);
}


/*
 * Window_Dump: dump a window to a file which must already be open for
 *              writting.
 */

char *calloc();

#include "X11/XWDFile.h"

static int Window_Dump(window, out)
     Window window;
     FILE *out;
{
    unsigned long swaptest = 1;
    XColor *colors;
    unsigned buffer_size;
    int win_name_size;
    int header_size;
    int ncolors, i;
    char *win_name;
    Bool got_win_name;
    XWindowAttributes win_info;
    XImage *image;
    int absx, absy, x, y;
    unsigned width, height;
    int dwidth, dheight;
    int bw;
    Window dummywin;
    XWDFileHeader header;

    
    /*
     * Inform the user not to alter the screen.
     */
    XBell(display, 50);

    /*
     * Get the parameters of the window being dumped.
     */
    if (debug) outl("xwd: Getting target window information.\n");
    if(!XGetWindowAttributes(display, window, &win_info)) 
      Fatal_Error("Can't get target window attributes.");

    /* handle any frame window */
    if (!XTranslateCoordinates (display, window, RootWindow (display, screen_num), 0, 0,
				&absx, &absy, &dummywin)) {
	fprintf (stderr, 
		 "unable to translate window coordinates (%d,%d)\n",
		 absx, absy);
	exit (1);
    }
    win_info.x = absx;
    win_info.y = absy;
    width = win_info.width;
    height = win_info.height;
    bw = 0;

    if (!nobdrs) {
	absx -= win_info.border_width;
	absy -= win_info.border_width;
	bw = win_info.border_width;
	width += (2 * bw);
	height += (2 * bw);
    }
    dwidth = DisplayWidth (display, screen_num);
    dheight = DisplayHeight (display, screen_num);


    /* clip to window */
    if (absx < 0) width += absx, absx = 0;
    if (absy < 0) height += absy, absy = 0;
    if (absx + width > dwidth) width = dwidth - absx;
    if (absy + height > dheight) height = dheight - absy;

    XFetchName(display, window, &win_name);
    if (!win_name || !win_name[0]) {
	win_name = "xwdump";
	got_win_name = False;
    } else {
	got_win_name = True;
    }

    /* sizeof(char) is included for the null string terminator. */
    win_name_size = strlen(win_name) + sizeof(char);

    /*
     * Snarf the pixmap with XGetImage.
     */

    x = absx - win_info.x;
    y = absy - win_info.y;
    image = XGetImage (display, window, x, y, width, height, AllPlanes, format);
    if (!image) {
	fprintf (stderr, "unable to get image at %dx%d+%d+%d\n",
		 width, height, x, y);
	exit (1);
    }

    if (add_pixel_value != 0) XAddPixel (image, add_pixel_value);

    /*
     * Determine the pixmap size.
     */
    buffer_size = Image_Size(image);

    if (debug) outl("xwd: Getting Colors.\n");

    ncolors = Get_XColors(&win_info, &colors);

    /*
     * Inform the user that the image has been retrieved.
     */
    XBell(display, FEEP_VOLUME);
    XBell(display, FEEP_VOLUME);
    XFlush(display);

    /*
     * Calculate header size.
     */
    if (debug) outl("xwd: Calculating header size.\n");
    header_size = sizeof(header) + win_name_size;

    /*
     * Write out header information.
     */
    if (debug) outl("xwd: Constructing and dumping file header.\n");
    header.header_size = (CARD32) header_size;
    header.file_version = (CARD32) XWD_FILE_VERSION;
    header.pixmap_format = (CARD32) format;
    header.pixmap_depth = (CARD32) image->depth;
    header.pixmap_width = (CARD32) image->width;
    header.pixmap_height = (CARD32) image->height;
    header.xoffset = (CARD32) image->xoffset;
    header.byte_order = (CARD32) image->byte_order;
    header.bitmap_unit = (CARD32) image->bitmap_unit;
    header.bitmap_bit_order = (CARD32) image->bitmap_bit_order;
    header.bitmap_pad = (CARD32) image->bitmap_pad;
    header.bits_per_pixel = (CARD32) image->bits_per_pixel;
    header.bytes_per_line = (CARD32) image->bytes_per_line;
    header.visual_class = (CARD32) win_info.visual->class;
    header.red_mask = (CARD32) win_info.visual->red_mask;
    header.green_mask = (CARD32) win_info.visual->green_mask;
    header.blue_mask = (CARD32) win_info.visual->blue_mask;
    header.bits_per_rgb = (CARD32) win_info.visual->bits_per_rgb;
    header.colormap_entries = (CARD32) win_info.visual->map_entries;
    header.ncolors = ncolors;
    header.window_width = (CARD32) win_info.width;
    header.window_height = (CARD32) win_info.height;
    header.window_x = absx;
    header.window_y = absy;
    header.window_bdrwidth = (CARD32) win_info.border_width;

    if (*(char *) &swaptest) {
	_swaplong((char *) &header, sizeof(header));
	for (i = 0; i < ncolors; i++) {
	    _swaplong((char *) &colors[i].pixel, sizeof(long));
	    _swapshort((char *) &colors[i].red, 3 * sizeof(short));
	}
    }

    (void) fwrite((char *)&header, sizeof(header), 1, out);
    (void) fwrite(win_name, win_name_size, 1, out);

    /*
     * Write out the color maps, if any
     */

    if (debug) outl("xwd: Dumping %d colors.\n", ncolors);
    (void) fwrite((char *) colors, sizeof(XColor), ncolors, out);

    /*
     * Write out the buffer.
     */
    if (debug) outl("xwd: Dumping pixmap.  bufsize=%d\n",buffer_size);

    /*
     *    This copying of the bit stream (data) to a file is to be replaced
     *  by an Xlib call which hasn't been written yet.  It is not clear
     *  what other functions of xwd will be taken over by this (as yet)
     *  non-existant X function.
     */
    (void) fwrite(image->data, (int) buffer_size, 1, out);

    /*
     * free the color buffer.
     */

    if(debug && ncolors > 0) outl("xwd: Freeing colors.\n");
    if(ncolors > 0) free(colors);

    /*
     * Free window name string.
     */
    if (debug) outl("xwd: Freeing window name string.\n");
    if (got_win_name) XFree(win_name);

    /*
     * Free image
     */
    XDestroyImage(image);
}



/*
 * Determine the pixmap size.
 */

int Image_Size(image)
     XImage *image;
{
    if (format != ZPixmap)
      return(image->bytes_per_line * image->height * image->depth);

    return(image->bytes_per_line * image->height);
}

#define lowbit(x) ((x) & (~(x) + 1))

/*
 * Get the XColors of all pixels in image - returns # of colors
 */
int Get_XColors(win_info, colors)
     XWindowAttributes *win_info;
     XColor **colors;
{
    int i, ncolors;

    if (!win_info->colormap)
	return(0);

    if (win_info->visual->class == TrueColor)
	return(0);    /* colormap is not needed */

    ncolors = win_info->visual->map_entries;
    if (!(*colors = (XColor *) malloc (sizeof(XColor) * ncolors)))
      Fatal_Error("Out of memory!");

    if (win_info->visual->class == DirectColor) {
	Pixel red, green, blue, red1, green1, blue1;

	red = green = blue = 0;
	red1 = lowbit(win_info->visual->red_mask);
	green1 = lowbit(win_info->visual->green_mask);
	blue1 = lowbit(win_info->visual->blue_mask);
	for (i=0; i<ncolors; i++) {
	  (*colors)[i].pixel = red|green|blue;
	  (*colors)[i].pad = 0;
	  red += red1;
	  if (red > win_info->visual->red_mask)
	    red = 0;
	  green += green1;
	  if (green > win_info->visual->green_mask)
	    green = 0;
	  blue += blue1;
	  if (blue > win_info->visual->blue_mask)
	    blue = 0;
	}
    } else {
	for (i=0; i<ncolors; i++) {
	  (*colors)[i].pixel = i;
	  (*colors)[i].pad = 0;
	}
    }

    XQueryColors(display, win_info->colormap, *colors, ncolors);
    
    return(ncolors);
}

int _swapshort (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;

    while (bp < ep) {
	c = *bp;
	*bp = *(bp + 1);
	bp++;
	*bp++ = c;
    }
}

int _swaplong (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;
    register char *sp;

    while (bp < ep) {
	sp = bp + 3;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	sp = bp + 1;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	bp += 2;
    }
}

/*
 * Imprime la page courante
 */
static void imprime_page_courante (mode)
int mode;
{
    FILE *fp;
    char cmd[256], n[256];
    XWindowAttributes attr;

    sprintf (n, "/tmp/xtel%d", getpid());
    if ((fp = fopen (n, "w")) == NULL) {
	perror (n);
	exit (1);
    }

    if (mode == VIDEOTEX) {
	display = XtDisplay (ecran_minitel);
	screen_num = DefaultScreen (display);

	XGetWindowAttributes(display, DefaultRootWindow(display), &attr);
    
	if (attr.backing_store == NotUseful) {
	    XEvent ev;

	    XWindowEvent (display, XtWindow(ecran_minitel), ExposureMask, &ev);
	    XCopyArea (display, videotexPixmapEcranSauve(ecran_minitel), XtWindow(ecran_minitel), DefaultGC(display, screen_num), ev.xexpose.x, ev.xexpose.y, ev.xexpose.width, ev.xexpose.height, ev.xexpose.x, ev.xexpose.y);
	}

	Window_Dump (XtWindow(ecran_minitel), fp);
	sprintf (cmd, rsc_xtel.commandeImpression, n);
    }
    else { /* ASCII */
	videotexConversionAscii (ecran_minitel, fp);
	sprintf (cmd, rsc_xtel.commandeImpressionAscii, n);
    }

    fclose (fp);

    system (cmd);

    unlink (n);
}

void imprime_page_courante_ascii (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    imprime_page_courante (ASCII);
}

void imprime_page_courante_videotex (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    imprime_page_courante (VIDEOTEX);
}
