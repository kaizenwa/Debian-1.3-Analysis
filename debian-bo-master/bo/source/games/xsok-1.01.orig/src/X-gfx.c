/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.00 -- module X-gfx.c				     */
/*									     */
/*	Drawing routines for the X window system.			     */
/*	Written by Michael Bischoff (mbi@mo.math.nat.tu-bs.de)		     */
/*	November-1994							     */
/*	see COPYRIGHT.xsok for Copyright details			     */
/*									     */
/*									     */
/*****************************************************************************/
#include "X-sok.h"

/*#include <X11/X.h>*/
#include <xpm.h>

#ifndef DELAY
#define DELAY 50 /* time to sleep (in ms) between auto-moves & replay */
#endif

Display *dpy;
Window table = 0;
struct graphic graphic;
int gamegraphic = 1;

static Pixmap floor, objd, objclip;
static GC gc;
static int resize_pending = 0;

#ifndef HAVE_USLEEP
#include <sys/time.h>
/* usleep emulation code taken from xboing-2.2 by Justin C. Kibell */

static int usleep(unsigned long usec) {
#ifdef SYSV
#ifdef __clipper__
    struct timeval tv;
    tv.tv_sec=((usec)/1000);
    tv.tv_usec=(((usec)%1000)*1000);
    select(1,NULL,NULL,NULL,&tv);
#else
    poll((struct poll *) 0, (size_t) 0, usec / 1000);   /* ms resolution */
#endif
#else
    struct timeval timeout;
    timeout.tv_usec = usec % (unsigned long) 1000000;
    timeout.tv_sec = usec / (unsigned long) 1000000;
    select(0, (void *) 0, (void *) 0, (void *) 0, &timeout);
#endif
    return 0;
}
#endif


void sync_and_wait(void) {
    XSync(dpy, 0);
    usleep(DELAY*1000);
}

void NewLevel(int levelnr) {
    if (levelnr < 1)
	levelnr = 1;
    if (levelnr > maxlevel)
	levelnr = maxlevel;
    game.level = levelnr;
    ParseMapFile();
    SetTitle();
    if (table) {
	init_layout();
	AskWidgetForResize(graphic.width, graphic.height);
	cmd_Resize();
	if (gamegraphic)
	    refresh_screen();
	resize_pending = 1;
    }
}

void init_layout(void) {
    graphic.width = game.numcols * DX;
    graphic.height = game.numrows * DY;
}

void init_gfx(const char *xpmdir) {
    int screen, retcode;
    char s[MAXXSOKDIRLEN+14];
    screen = DefaultScreen(dpy);
    gc = XDefaultGC(dpy, screen);
    sprintf(s, "%s/floor.xpm", xpmdir);
    if ((retcode = XpmReadFileToPixmap(dpy, RootWindow(dpy, screen),
				       s,  &floor, 0, NULL)) == XpmSuccess) {
	sprintf(s, "%s/objects.xpm", xpmdir);
	retcode = XpmReadFileToPixmap(dpy, RootWindow(dpy, screen), s, &objd,
			    &objclip, NULL);
    }
    switch (retcode) {
    case XpmSuccess:
	return;		/* no error */
    case XpmColorFailed:
    case XpmColorError:
	fatal("Not enough colors for %s", s);
    case XpmOpenFailed:
	fatal("Cannot open %s", s);
    case XpmFileInvalid:
	fatal("Invalid File: %s", s);
    case XpmNoMemory:
	fatal("Out of memory reading %s", s);
    default:
	fatal("Unknown error (code %d) reading %s", s);
    }
}

#define ODRAW(c) {   \
    XSetClipOrigin(dpy, gc, x*DX - ((c)&3)*DX, y*DY - ((c)>>2)*DY);\
    XCopyArea(dpy, objd, table, gc, DX*((c)&3), DY*((c)>>2), DX, DY, (x)*DX, (y)*DY); }

static void do_redraw(int x, int y) {
    if (x >= game.numcols || y >= game.numrows || map[y][x]->pic == 16)
	return; /* x = y = 0; */
#ifndef SIMPLE_WALLS
    if (!map[y][x]->pic) {
	int b;
	b = 0;
	if (!map[y][x-1]->pic)
	    b += 1;
	if (!map[y+1][x]->pic)
	    b += 2;
	if (!map[y][x+1]->pic)
	    b += 4;
	if (!map[y-1][x]->pic)
	    b += 8;
	XCopyArea(dpy, floor, table, gc,
		  DX * (b & 7), DY * (b / 8), DX, DY, (x)*DX, (y)*DY);
    } else
#endif
    XCopyArea(dpy, floor, table, gc, DX * (map[y][x]->pic&7),
	      DY * (map[y][x]->pic >> 3), DX, DY, (x)*DX, (y)*DY);
    if (obj[y][x]) {
	int c;
	XSetClipMask(dpy, gc, objclip);
	c = obj[y][x]->pic;
	ODRAW(c);
	XSetClipMask(dpy, gc, None);
    }
}

static void dotPaint(int minx, int miny, int maxx, int maxy) {
    int x, y;
    if (minx > maxx || miny > maxy) {
	int h;
	h = minx; minx = maxx; maxx = h;
	h = miny; miny = maxy; maxy = h;
    }
    x = minx-1;
    do {
	++x;
	for (y = miny; y <= maxy; ++y) {
	    do_redraw(x, y);
	}
    } while (x != maxx);
}

/* doPaint: just request it */
void doPaint(int minx, int miny, int maxx, int maxy) {
#if 0
    XClearArea(dpy, table, minx*DX, miny*DY, (maxx+1)*DX-1, (maxy+1)*DY-1, True);
#else
    dotPaint(minx, miny, maxx, maxy);
#endif
}

void redraw_table(XExposeEvent *xev) {
    int minx, miny, maxx, maxy;
    if (resize_pending) {
	if (xev->count)
	    return;
	/* after a resize, do a complete redraw */
	XClearArea(dpy, table, 0, 0, 0, 0, False);
	dotPaint(1, 1, game.numcols-2, game.numrows-2);
	resize_pending = 0;
    } else {
	minx = xev->x / DX;
	miny = xev->y / DY;
	maxx = (xev->x + xev->width - 1) / DX;
	maxy = (xev->y + xev->height - 1) / DY;
	dotPaint(minx, miny, maxx, maxy);
    }
}
