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
#include "aconfig.h"
#ifdef X11_DRIVER
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <X11/Xlib.h>
#include "xlib.h"
#ifdef MITSHM
#include <sys/ipc.h>
#include <sys/shm.h>
#include <X11/extensions/XShm.h>
#endif
#include "zoom.h"
#include "ui.h"
#include "palette.h"
#include "gif.h"
xlibparam xparams;
static int allocated;

struct ui_driver x11_driver;
static xdisplay *d;
static char *size;
static int Xsync;
static int busy;
static int Completion;
#ifdef AMIGA
#define XFlush(x) while(0)
#endif

static void x11_rotate(int direction)
{
    zoom_context *c = ui_getcontext();
    xrotate_palette(d, direction, c->cmap, c->num_colors);
#ifdef MITSHM
    if (d->SharedMemFlag && d->truecolor)
	busy++;
#endif
    XFlush(d->display);

}
static int x11_set_color(int r, int g, int b, int init)
{
    if (init)
	xfree_colors(d);
    return (xalloc_color(d, r * 256, g * 256, b * 256, init));
}

static void x11_print(int x, int y, char *text)
{
    xmoveto(d, x, y + x11_driver.textheight);
    xouttext(d, text);
    /*XSync(d->display, 0); */
    XFlush(d->display);

}

static void x11_display(void)
{
#ifdef MITSHM
    if (d->SharedMemFlag)
	busy++;
#endif
    if (Xsync)
	XSync(d->display, 0);
    draw_screen(d);
    XFlush(d->display);
}

static void x11_flip_buffers(void)
{
    xflip_buffers(d);
}
void x11_free_buffers(char *b1, char *b2)
{
    if (allocated) {
	XSync(d->display, 0);
	allocated = 0;
	free_image(d);
    }
}
int x11_alloc_buffers(char **b1, char **b2)
{
    if (!allocated) {
	allocated = 1;
	if (!alloc_image(d)) {
	    return (0);
	}
	xflip_buffers(d);
    }
    *b1 = d->vbuff;
    *b2 = d->back;
    return (d->linewidth);
}
static void x11_getsize(int *w, int *h)
{
    xupdate_size(d);
    *w = d->width;
    *h = d->height;
}

static void x11_processevents(int wait, int *mx, int *my, int *mb, int *k)
{
    static mousex = 0, mousey = 0;
    static int iflag = 0;
    static unsigned int mousebuttons = 0;
    static int resized;
    XEvent ev;

    if (Xsync)
	XSync(d->display, 0);
    if (busy >= 2 || wait || XPending(d->display)) {
	do {
	    XNextEvent(d->display, &ev);
	    switch (ev.type) {
	    case ButtonRelease:
		mousex = ev.xbutton.x;
		mousey = ev.xbutton.y;
		switch (ev.xbutton.button) {
		case 1:
		    mousebuttons &= ~BUTTON1;
		    break;
		case 2:
		    mousebuttons &= ~BUTTON2;
		    break;
		case 3:
		    mousebuttons &= ~BUTTON3;
		    break;
		}
		break;
	    case ButtonPress:
		mousex = ev.xbutton.x;
		mousey = ev.xbutton.y;
		if (mousex < 0 || mousey < 0 || mousex > d->width || mousey > d->height)
		    return;
		switch (ev.xbutton.button) {
		case 1:
		    mousebuttons |= BUTTON1;
		    break;
		case 2:
		    mousebuttons |= BUTTON2;
		    break;
		case 3:
		    mousebuttons |= BUTTON3;
		    break;
		}
		break;
	    case MotionNotify:
		mousex = ev.xmotion.x;
		mousey = ev.xmotion.y;
		mousebuttons = ev.xmotion.state & (BUTTON1 | BUTTON2 | BUTTON3);
		break;
	    case Expose:
		if (resized)
		    break;
#ifdef MITSHM
		if (d->SharedMemFlag)
		    busy++;
#endif
		XSync(d->display, 0);
		draw_screen(d);
		ui_tbreak();
		break;
	    case ConfigureNotify:{
		    int oldw = d->width, oldh = d->height;
		    XSync(d->display, 0);
		    xupdate_size(d);
		    if (d->width != oldw || d->height != oldh) {
			resized = 2;
			ui_resize();
			resized = 0;
		    }
		}
		break;
	    case KeyRelease:{
		    KeySym ksym;
		    switch (ksym = XLookupKeysym(&ev.xkey, 0)) {
		    case XK_Left:
			iflag &= ~1;
			break;
		    case XK_Right:
			iflag &= ~2;
			break;
		    case XK_Up:
			iflag &= ~4;
			break;
		    case XK_Down:
			iflag &= ~8;
			break;
		    }
		}
		break;
	    case KeyPress:{
		    KeySym ksym;
		    switch (ksym = XLookupKeysym(&ev.xkey, 0)) {
		    case XK_Left:
			iflag |= 1;
			break;
		    case XK_Right:
			iflag |= 2;
			break;
		    case XK_Up:
			iflag |= 4;
			break;
		    case XK_Down:
			iflag |= 8;
			break;
		    default:
			{
			    char *name;
			    name = XKeysymToString(ksym);
#ifdef DEBUG
			    printf("%s\n", name);
#endif
			    if (ksym == XK_division)
				name = "/";
			    if (ksym == XK_question)
				name = "?";
			    if (ksym == XK_space)
				name = " ";
			    if (ksym == XK_Escape)
				name = "q";
			    if (strlen(name) == 1) {
				if (ui_key(*name) == 2)
				    return;
			    }
			}
		    }
		}
		break;
	    default:
#ifdef MITSHM
		if (ev.xany.type == Completion)
		    busy--;
#endif
		break;
	    }
	} while (busy >= 2 || XEventsQueued(d->display, QueuedAlready));
    }
    *mx = mousex;
    *my = mousey;
    *mb = mousebuttons;
    *k = iflag;
}
int x11_init(void)
{
    if (size != NULL) {
	int x, y;
	sscanf(size, "%ix%i", &x, &y);
	if (x < 0)
	    x = XSIZE;
	if (y < 0)
	    y = YSIZE;
	d = xalloc_display("XaoS", x, y, &xparams);
    } else
	d = xalloc_display("XaoS", XSIZE, YSIZE, &xparams);
    if (d == NULL)
	return 0;
    if (d->truecolor || d->privatecolormap)
	x11_driver.flags &= ~RANDOM_PALETTE_SIZE;
    x11_driver.maxwidth = XDisplayWidth(d->display, d->screen);
    x11_driver.maxheight = XDisplayHeight(d->display, d->screen);
    x11_driver.width = ((float) XDisplayWidthMM(d->display, d->screen)) / x11_driver.maxwidth / 10.0;
    x11_driver.height = ((float) XDisplayHeightMM(d->display, d->screen)) / x11_driver.maxheight / 10.0;
    x11_driver.textheight = xsetfont(d, "fixed");
#ifdef MITSHM
    Completion = XShmGetEventBase(d->display) + ShmCompletion;
#endif
    if (d->privatecolormap) {
	x11_driver.flags |= PALETTE_ROTATION | ROTATE_INSIDE_CALCULATION;
    }
    if (d->truecolor) {
	x11_driver.flags |= PALETTE_ROTATION | PALETTE_REDISPLAYS;
    }
    return (1);
}
void x11_uninitialise(void)
{
    xfree_colors(d);
    xfree_display(d);
}
void x11_getmouse(int *x, int *y, int *b)
{
    int rootx, rooty;
    Window rootreturn, childreturn;
    XQueryPointer(d->display, d->window,
		  &rootreturn, &childreturn,
		  &rootx, &rooty, x,
		  y, (unsigned int *) b);
}
static char *helptext[] =
{
    "X11 DRIVER VERSION 2.1                 ",
    "======================                 ",
    "This was the first driver done for XaoS",
    "so it is fully featured but have       ",
    "following bugs/limitations:            ",
    " o supports only 8bpp pseudocolor,     ",
    "   15,16,24 and 32bpp truecolor        ",
    "   modes.                              ",
    " o Palette rotating does not work for  ",
    "   8bpp pseudocolor w/o private palette",
    "   use -private to enable it.          ",
    "   Also on truecolor modes uses        ",
    "   lots of cpu                         ",
    " o does not support use of private     ",
    "    colormap, shared colormap is       ",
    "    sometimes full and XaoS looks ugly ",
    " o someone wants help?                 ",
    "                                       ",
    "And have following features:           ",
    " o runtime resizing of window          ",
    " o uses mitshm extension               ",
    "                                       ",
    "                                       ",
    "   X11 driver was done by Jan Hubicka  ",
    "           and Thomas Marsh            ",
    "              (C) 1997                 ",
};
#define UGLYTEXTSIZE (sizeof(helptext)/sizeof(char *))
static struct params params[] =
{
    {"-size", P_STRING, &size, "Select size of window (WIDTHxHEIGHT)"},
    {"-sync", P_SWITCH, &Xsync, "Generate sync signals before looking for events. This helps on old and buggy HP-UX X servers"},
    {"-private", P_SWITCH, &xparams.privatecolormap, "Use private colormap on pseudocolor display"},
    {"-usedefault", P_SWITCH, &xparams.usedefault, "Use default visual in case autodetection causes troubles"},
  {"-nomitshm", P_SWITCH, &xparams.nomitshm, "Disable MITSHM extension"},
    {NULL, 0, NULL, NULL}
};

struct ui_driver x11_driver =
{
    "X11",
    x11_init,
    x11_getsize,
    x11_processevents,
    x11_getmouse,
    x11_uninitialise,
    x11_set_color,
    x11_print,
    x11_display,
    x11_alloc_buffers,
    x11_free_buffers,
    x11_flip_buffers,
    x11_rotate,
    256 - 16,
    8,
    helptext,
    UGLYTEXTSIZE,
    params,
    RANDOM_PALETTE_SIZE | UPDATE_AFTER_PALETTE | RESOLUTION | PIXELSIZE /*| UPDATE_AFTER_RESIZE */ ,
    0.0, 0.0,
    0, 0,
};

#endif
