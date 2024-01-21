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
#include <config.h>
#ifdef PLAN9_DRIVER
#include <u.h>
#include <libc.h>
#include <stdio.h>
#include <libg.h>
#include "zoom.h"
#include "ui.h"
#include "palette.h"
#include "gif.h"

Bitmap *mybitmap;
char *buffers[2];
Rectangle rect, rect1;
int width, height;
int current = 0;
int nopalette;

RGB cmap[256];
int ncolors = 255;
static int mousex, mousey, mousebuttons;
static int lasttime = 0;

static int plan9_msec(void)
{				/*this function was sent by Nigel Roles */
    static int fd = -1;
    char buf[20];		/* ish */
    if (fd < 0)
	fd = open("/dev/msec", OREAD);
    else
	seek(fd, 0, 0);
    read(fd, buf, sizeof(buf));
    return atoi(buf);
}
int lookup_timer(void)
{				/* printf("%i %i\n",lasttime,plan9_msec()); */
    return ((plan9_msec() - lasttime) * 1000);
}
int get_timer()
{
    int time1;
    time1 = lookup_timer();
    lasttime = plan9_msec();
    return (time1);
}

static int plan9_set_color(int r, int g, int b, int init)
{
    RGB c =
    {r * 256 * 256 * 256, g * 256 * 256 * 256, b * 256 * 256 * 256};
    if (nopalette) {
	return (rgbpix(&screen, c));
    } else {
	ncolors--;
	if (init)
	    ncolors = 254;
	if (ncolors < 5)
	    return -1;
	cmap[255].red = 0;
	cmap[255].green = 0;
	cmap[255].blue = 0;
	cmap[0].red = 0xffffffffUL;
	cmap[0].green = 0xffffffffUL;
	cmap[0].blue = 0xffffffffUL;

	cmap[ncolors].red = (unsigned long) r *256UL * 256UL * 256UL;
	cmap[ncolors].green = (unsigned long) g *256UL * 256UL * 256UL;
	cmap[ncolors].blue = (unsigned long) b *256UL * 256UL * 256UL;
	wrcolmap(&screen, cmap);
    }
    return (ncolors);
}

static void plan9_rotate(int direction)
{
    int i;
    zoom_context *c = ui_getcontext();
    for (i = 1; i < c->num_colors; i++) {
	cmap[c->colors[i]].red = ((unsigned long) c->cmap[0][c->colors[i]]) * 256UL * 256UL * 256UL;
	cmap[c->colors[i]].green = ((unsigned long) c->cmap[1][c->colors[i]]) * 256UL * 256UL * 256UL;
	cmap[c->colors[i]].blue = ((unsigned long) c->cmap[2][c->colors[i]]) * 256UL * 256UL * 256UL;
    }
    wrcolmap(&screen, cmap);
}
static void plan9_print(int x, int y, char *text)
{
    Point p =
    {rect.min.x + x, rect.min.y + y};
    string(&screen, p, font, text, S);
    bflush();

}
static void plan9_getmouse(int *x, int *y, int *buttons)
{
    *x = mousex;
    *y = mousey;
    *buttons = mousebuttons;

}

static void plan9_display(void)
{
    wrbitmap(mybitmap, 0, height, (unsigned char *) buffers[current]);
    bitblt(&screen, rect.min, mybitmap, rect1, S);
    bflush();
}

static void plan9_flip_buffers(void)
{
    current ^= 1;
/*
   xflip_buffers(d);
 */
}

void ereshaped(Rectangle rect1)
{
    ui_resize();
}
static void plan9_processevent(int wait, int *mx, int *my, int *b, int *k)
{
    static int keys;
    Event E;
    while (wait || ecanread(Emouse | Ekeyboard)) {
	wait = 0;
	switch (event(&E)) {
	case Emouse:
	    mousex = E.mouse.xy.x - rect.min.x;
	    mousey = E.mouse.xy.y - rect.min.y;
	    mousebuttons = 0;
	    if (E.mouse.buttons & 1)
		mousebuttons = BUTTON1;
	    if (E.mouse.buttons & 2)
		mousebuttons |= BUTTON2;
	    if (E.mouse.buttons & 4)
		mousebuttons |= BUTTON3;
	    break;
	case Ekeyboard:{
		if (E.kbdc == '[')
		    keys ^= 1;
		if (E.kbdc == ']')
		    keys ^= 2;
		if (E.kbdc == '\'')
		    keys ^= 4;
		if (E.kbdc == ';')
		    keys ^= 8;
		ui_key(tolower(E.kbdc));
	    }
	    break;
	}

    }

    *mx = mousex;
    *my = mousey;
    *b = mousebuttons;
    *k = keys;
}
static void plan9_getsize(int *w, int *h)
{
    bscreenrect(&rect);
    width = rect.max.x - rect.min.x;
    height = rect.max.y - rect.min.y;
    rect1.min.x = 0;
    rect1.min.y = 0;
    rect1.max.x = width;
    rect1.max.y = height;
    *w = width;
    *h = height;
}
static int plan9_allocbuffers(char **b1, char **b2)
{
    mybitmap = balloc(rect1, 3);
    current = 0;
    *b1 = buffers[0] = (char *) malloc(width * (height + 1));
    *b2 = buffers[1] = (char *) malloc(width * (height + 1));
    return (width);
}
static void plan9_freebuffers(char *b1, char *b2)
{
    free(buffers[0]);
    free(buffers[1]);
    bfree(mybitmap);
}
struct ui_driver plan9_driver;

static int plan9_init(void)
{
    binit(NULL, NULL, "XaoS");
    einit(Ekeyboard | Emouse);
    if (nopalette)
	plan9_driver.flags |= UPDATE_AFTER_PALETTE;
    else
	plan9_driver.flags |= PALETTE_ROTATION | ROTATE_INSIDE_CALCULATION;
    return 1;
    /*ui_init(context, display, set_color, 1, myprint, font->height); */
}
static void plan9_uninit()
{
    bexit();
}

static char *helptext[] =
{
    "PLAN9 DRIVER VERSION 2.2               ",
    "========================               ",
    "  This driver is bit uncomplette.      ",
    "  please read README.plan9 and help me ",
    "  complete it if you want.             ",
    "                                       ",
    "  Additional keys:                     ",
    "  '[' and '[' changing iterations      ",
    "  ';' amd ''' changing speed           ",
    "                                       ",
    "  For stereogram generator you need    ",
    "  specify pixel size(using -pixelwidth ",
    "  -pixelheight) because -screenwidth   ",
    "  and screenheight does not work.      ",
    "                                       ",
    "  Palette handling:                    ",
    "  By default XaoS changes colormap.    ",
    "  This will break all colorfull        ",
    "  programs like mortha (is there any   ",
    "  other?) There is easy trick:         ",
    "  use -nopalette switch. At the other  ",
    "  hand this makes XaoS look ugly       ",
    "  and palette rotation does not work.  ",
    "  Nothing is perfect...                ",
    "                                       ",
    "  PLAN9 driver was done by Jan Hubicka ",
    "              (C) 1997                 ",
};
#define UGLYTEXTSIZE (sizeof(helptext)/sizeof(char *))
static struct params params[] =
{
    {"-nopalette", P_SWITCH, &nopalette, "Disable palette allocating. Use ugly looking rgbpixel instead"},
    {NULL, 0, NULL, NULL}
};

struct ui_driver plan9_driver =
{
    "plan9",
    plan9_init,
    plan9_getsize,
    plan9_processevent,
    plan9_getmouse,
    plan9_uninit,
    plan9_set_color,
    plan9_print,
    plan9_display,
    plan9_allocbuffers,
    plan9_freebuffers,
    plan9_flip_buffers,
    plan9_rotate,
    256,
    14,
    helptext,
    UGLYTEXTSIZE,
    params,
    RESOLUTION,
    0.0, 0.0,
    800, 600,
};

#endif
