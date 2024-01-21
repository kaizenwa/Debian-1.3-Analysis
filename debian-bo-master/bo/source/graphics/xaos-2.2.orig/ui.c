/* 
 *     XaoS, a fast portable realtime fractal zoomer 
 *                  Copyright (C) 1996 by
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
#include <stdio.h>
#include <libc.h>
#else
#include "aconfig.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#endif
#include "config.h"
#ifndef _plan9_
#include <assert.h>
#endif
#include "zoom.h"
#include "help.h"
#include "gif.h"
#include "autopilot.h"
#include "complex.h"
#include "ui.h"
#include "palette.h"
#include "plane.h"
#include "ui.h"
#include "param.h"
#include "version.h"
#include "timers.h"


#define VBUFF (context->stereogram?context->svbuff:context->vbuff)
#define BACK (context->stereogram?context->sback:context->back)
#define SIZE 1024
#define add_buffer(char) if(((end+1)%SIZE)!=begin) {keybuffer[end]=char;end=(end+1)%SIZE;}
#define INTERRUPTIBLE 0
#define ANIMATION 1
#define NEW_IMAGE 2
#define UNINTERRUPTIBLE 3
#define xstoc(x)        (((number_t) (context->s.nc + (x) * ((context->s.mc - context->s.nc) \
                                / (number_t) context->width))))
#define ystoc(y)        (((number_t) (context->s.ni + (y) * ((context->s.mi - context->s.ni) \
                                / (number_t) context->height))))

#define textwidth driver->textheight
#define FRAMETIME (1000000/FRAMERATE)

#define FORMULA 0
#define HELP 1
#define DRIVER 2
#define MENU 3


extern struct ui_driver *drivers[];
extern int ndrivers;

static tl_timer *maintimer, *rotatetimer;
static void ui_do_fractal(int);
static int displayed;
static void ui_savefile(void);
static int lastjx, lastjy;
static void ui_mandelbrot(int, int);
static void ui_incoloringmode(void);
static int ui_autopilot(void);
static void ui_help(int);
static int ui_mouse(int, int, int, int);
static int keybuffer[SIZE];
static int rotate, direction = 1, rotated, stopped = 0, rotationspeed = ROTATIONSPEED;
static int begin = 0, end = 0;
static int incalculation, resized;
static int callresize = 0;
char *interruptkeys = "z123456789rmfqe";
static int defformula[10];
static int defautopilot = 0;
static int defiters = DEFAULT_MAX_ITER;
static int defincoloring = 1;
static int defoutcoloring = 1;
static int defplane = 1;
static float defspeed = 1;
static char *defdriver = NULL;
static int todriver = 0;
static int defguessing = 3;
static int deflist;
static int printconfig;
static int fastmode = 2;
static float defscreenwidth = 0.0, defscreenheight = 0.0, defpixelwidth = 0.0,
 defpixelheight = 0.0;
const struct params global_params[] =
{
    {"", P_HELP, NULL, "Screen size options: \n\n  Knowedge of exact screen size makes random dot stereogram look better. \n  Also is used for choosing correct view area."},
    {"-screenwidth", P_FLOAT, &defscreenwidth, "exact size of screen in centimeters"},
    {"-screenheight", P_FLOAT, &defscreenheight, "exact size of screen in centimeters"},
    {"", P_HELP, NULL, "  Use this option in case you use some kind of virtual screen\n  or something similiar that confuses previous options"},
    {"-pixelwidth", P_FLOAT, &defpixelwidth, "exact size of one pixel in centimeters"},
    {"-pixelheight", P_FLOAT, &defpixelheight, "exact size of one pixel in centimeters"},
    {"", P_HELP, NULL, "Fractal formulas:"},
    {"-mandel", P_SWITCH, defformula, "Start with Mandelbrot set fractal(default)"},
    {"-mandel3", P_SWITCH, defformula + 1, "Start with Mandelbrot set power 3 fractal"},
    {"-mandel4", P_SWITCH, defformula + 2, "Start with Mandelbrot set power 4 fractal"},
    {"-mandel5", P_SWITCH, defformula + 3, "Start with Mandelbrot set power 5 fractal"},
    {"-mandel6", P_SWITCH, defformula + 4, "Start with Mandelbrot set power 6 fractal"},
    {"-octo", P_SWITCH, defformula + 5, "Start with Octal fractal"},
    {"-newton", P_SWITCH, defformula + 6, "Start with Newton fractal"},
  {"-barnsley", P_SWITCH, defformula + 7, "Start with Barnsley fractal"},
    {"-phoenix", P_SWITCH, defformula + 8, "Start with Phoenix fractal"},
    {"-magnet", P_SWITCH, defformula + 9, "Start with Magnet fractal"},
    {"", P_HELP, NULL, "Fractal oprions:"},
    {"-iter", P_NUMBER, &defiters, "Default number of iterations"},
    {"-incoloring", P_SWITCH, &defincoloring, "Use incoloring"},
    {"-outcoloring", P_NUMBER, &defoutcoloring, "Default outcoloringcoloring scheme"},
    {"-plane", P_NUMBER, &defplane, "Default plane"},
    {"-guessing", P_NUMBER, &defguessing, "Default solid guessing range"},
    {"-fastdrawing", P_NUMBER, &fastmode, "Default fast drawing mode"},
    {"", P_HELP, NULL, "Parameters for ugly interface:"},
    {"-autopilot", P_SWITCH, &defautopilot, "Enable autopilot"},
    {"-speed", P_FLOAT, &defspeed, "Zooming speed (default is 1)"},
    {"-driver", P_STRING, &defdriver, "Select driver"},
    {"-list", P_SWITCH, &deflist, "List available drivers. Than exit"},
    {"-config", P_SWITCH, &printconfig, "Print configuration. Than exit"},
    {NULL, 0, NULL, NULL}
};

static int interruptiblemode;
static int autopilot = 0;
static double maxstep = MAXSTEP, speedup = STEP;
static zoom_context *context;
static double mul;
static int drawingtime, tbreak = 0;
static struct ui_driver *driver;
static int ministatus = 0;
static char statustext[256];

static void rotate_continue(void);
static int numbertype = FORMULA;
static int helppage;
static int juliamode;
static void ui_display(void)
{
    displayed = 1;
    driver->display();
    if (numbertype != FORMULA)
	rotate_continue();
    numbertype = FORMULA;
    if (ministatus)
	driver->print(0, 0, statustext);
}

extern int dynsize;
static void ui_outofmem(void)
{
    printf("\n\nXaoS is out of memory.\n"
	   "The memory space dynamically allocated by XaoS is approx:\n\n"
	   " 2*(width+16)*height+%i*width+%i*height+%i*(bigger of width or height)+%i bytes\n\n"
	   "Plus memory additionaly allocated by drivers. For example X11 driver in\n"
	   "truecolor or hicolor modes allocated again (width+16)*height*bytes per pixel.\n"
	   "Curses driver uses 9*64KB table and mac driver allocates 40Kb for saving.\n"
	   "So take this calculation as \"lower bound\"\n",
	   sizeof(*context->xpos) + sizeof(realloc_t),
	   sizeof(*context->xpos) + sizeof(realloc_t),
	   9 * dynsize + sizeof(int) * 2
	   ,sizeof(realloc_t) * 2 + sizeof(*context));
}

static int waitcount;
static int waitcount1;
static int starttime;
static int endtime;
static int maxtime = 1000000 / 10;
#define WAITTIME  200000
#define WAITTIME1 2000000
#define CHECKPROCESSEVENTS(b,k) assert(!((k)&~15)&&!((b)&~(BUTTON1|BUTTON2|BUTTON3)))
static void ui_waitfunc(void)
{
    char str[80];
    int l, x, y, b, k;
    tl_process_group(syncgroup);
    l = tl_lookup_timer(maintimer);
    if (interruptiblemode) {
	if (context->incalculation && !starttime)
	    starttime = l;
	else if (starttime && !context->incalculation && !endtime)
	    endtime = l;
	if (maxtime && l > maxtime && context->readyforinterrupt)
	    context->interrupt = 1, endtime = l;
    }
    if ((l) > (waitcount + 1) * WAITTIME) {
	if (!interruptiblemode && l > (waitcount1 + 1) * WAITTIME1) {
	    ui_display();
	    waitcount1++;
	}
	sprintf(str, "%s %3.2f%%        ", context->pass, (float) (context->max ? context->pos * 100.0 / context->max : 100.0));
	driver->processevents(0, &x, &y, &b, &k);
	CHECKPROCESSEVENTS(b, k);
	if (k & 3)
	    context->interrupt = 1;
	if (!interruptiblemode)
	    driver->print(0, context->height - textwidth, str);
	waitcount++;
    }
}
static float get_pixelwidth(int width)
{
    if (defscreenwidth > 0.0 && driver->flags & RESOLUTION)
	return (defscreenwidth * width / driver->maxwidth);
    if (defscreenwidth > 0.0)
	return (defscreenwidth);
    if (defpixelwidth > 0.0)
	return (defpixelwidth * width);
    if (driver->flags & PIXELSIZE)
	return (driver->width * width);
    if (driver->flags & SCREENSIZE)
	return (driver->width);
    if (driver->flags & RESOLUTION)
	return (29.0 / driver->maxwidth * width);
    return (29.0);
}
static float get_pixelheight(int height)
{
    if (defscreenheight > 0.0 && driver->flags & RESOLUTION)
	return (defscreenheight * height / driver->maxheight);
    if (defscreenheight > 0.0)
	return (defscreenheight);
    if (defpixelheight > 0.0)
	return (defpixelheight * height);
    if (driver->flags & PIXELSIZE)
	return (driver->height * height);
    if (driver->flags & SCREENSIZE)
	return (driver->height);
    if (driver->flags & RESOLUTION)
	return (21.0 / driver->maxheight * height);
    return (21.5);
}
static void rotate_off(void)
{
    if (rotate) {
	tl_free_timer(rotatetimer);
	rotate = 0;
    }
}
static void rotate_stop(void)
{
    if (rotate && !stopped) {
	tl_remove_timer(rotatetimer);
	stopped = 1;
    }
}
static void rotate_continue(void)
{
    if (rotate && stopped) {
	stopped = 0;
	if (driver->flags & ASYNC_PALETTE) {
	    tl_add_timer(asyncgroup, rotatetimer);
	} else
	    tl_add_timer(syncgroup, rotatetimer);
    }
}
static void rotatehandler(int n)
{
    int direct;
    direct = direction * n;
    if (direction > 0)
	direction %= context->num_colors - 1;
    else
	direction = -((-direction) % (context->num_colors - 1));
    if (!direction)
	return;
    rotated = 1;
    rotate_palette(context, direction * n);
    driver->rotate_palette(direction * n);
    if (ministatus && (driver->flags & PALETTE_REDISPLAYS)) {
	driver->print(0, 0, statustext);
    }
}
static void rotate_update(void)
{
    if (rotationspeed < 0)
	direction = -1;
    else
	direction = 1;
    tl_set_interval(rotatetimer, 1000000 / rotationspeed * direction);
}
static void rotate_on(void)
{
    if (driver->rotate_palette != NULL && (driver->flags & PALETTE_ROTATION)) {
	rotate = 1;
	rotatetimer = tl_create_timer();
	rotate_update();
	tl_set_multihandler(rotatetimer, rotatehandler);
	if (driver->flags & ASYNC_PALETTE) {
	    tl_add_timer(asyncgroup, rotatetimer);
	} else
	    tl_add_timer(syncgroup, rotatetimer);
    } else {
	driver->print(0, 0, "Rotating of palette not supported by this configuration");
    }
}
static void updatestatus(void)
{
    static float lastspeed = -1;
    double times = (context->currentformula->v.mc - context->currentformula->v.nc) / (context->s.mc - context->s.nc),
     speed;
    rotated = 0;
    tl_process_group(syncgroup);
    if (!rotated || (!(driver->flags & PALETTE_REDISPLAYS)))
	ui_display();
    tl_process_group(syncgroup);
    drawingtime = tl_lookup_timer(maintimer);
    tl_reset_timer(maintimer);
    speed = drawingtime ? 1000000.0 / drawingtime : 0.0;
    if (lastspeed == -1 || speed / lastspeed < 0.5 || speed / lastspeed > 1.5)
	lastspeed = speed;
    speed = lastspeed = (lastspeed * 10 + speed) / 11;
    sprintf(statustext, "%s %.2f times %2.2f frames/sec %c %i %i %i %i            ", times < 1 ? "unzoomed" : "zoomed", times < 1 ? 1.0 / times : times, speed, autopilot ? 'A' : ' ', context->coloringmode + 1, context->incoloringmode + 1, context->plane + 1, context->maxiter);
    if (drawingtime > 200000)
	drawingtime = 200000;
    mul = (double) drawingtime / FRAMETIME;
    numbertype = FORMULA;
    if (ministatus)
	driver->print(0, 0, statustext);
    STAT(printf("framerate:%f %i\n", 1000000.0 / drawingtime, drawingtime);
	)
	incalculation = 0;
}
static void ui_do_julia(int xx, int yy)
{
    int oldw = context->width, oldh = context->height;
    float size;

    unsigned char *vold = context->vbuff;
    tl_process_group(syncgroup);
    if (tbreak)
	tl_reset_timer(maintimer);
    incalculation = 1;
    lastjx = xx;
    lastjy = yy;
    context->pre = xstoc(xx), context->pim = ystoc(yy);
    recalculate(context->plane, &context->pre, &context->pim);
    if (context->windowwidth > context->windowheight)
	size = context->windowheight / 2;
    else
	size = context->windowwidth / 2;
    context->width = context->width * size / context->windowwidth;
    context->height = context->height * size / context->windowheight;
    if (context->stereogram)
	context->vbuff += (oldw - context->width) / 2;
    do_julia(context,
	     context->pre,
	     context->pim);
    context->width = oldw;
    context->height = oldh;
    context->vbuff = vold;
    if (context->stereogram)
	do_stereogram(context);
    updatestatus();
}
void juliamode_on(int x, int y)
{
    juliamode = 1;
    memcpy(context->back, context->vbuff, context->scanline * context->height);
    ui_do_julia(x, y);
}
void juliamode_off(void)
{
    juliamode = 0;
    memcpy(context->vbuff, context->back, context->scanline * context->height);
    ui_display();
}


static void ui_do_fractal(int mode)
{
    int i, y, time, time1;
    if (mode < fastmode + 1)
	interruptiblemode = 1;
    else
	interruptiblemode = 0;
    if (juliamode)
	juliamode_off();
    if (tbreak)
	tl_reset_timer(maintimer);
    for (i = begin; i != end; i = (i + 1) % SIZE) {
	for (y = 0; y < strlen(interruptkeys); y++)
	    if (interruptkeys[y] == tolower(keybuffer[i]))
		return;
    }
    incalculation = 1;
    starttime = 0;
    endtime = 0;
    set_view(context, &context->s);
    waitcount = tl_lookup_timer(maintimer) / WAITTIME + 2;
    waitcount1 = tl_lookup_timer(maintimer) / WAITTIME1 + 1;
    if (!(driver->flags & ROTATE_INSIDE_CALCULATION))
	rotate_stop();
    do_fractal(context, interruptiblemode);
    if (!(driver->flags & ROTATE_INSIDE_CALCULATION))
	rotate_continue();
    if (interruptiblemode) {
	tl_update_time();
	time1 = time = tl_lookup_timer(maintimer);
	time -= endtime - starttime;
	maxtime = time * 5;
	if (maxtime > 1000000 / 10)
	    maxtime = time * 3;
	if (maxtime < 1000000 / 30)
	    maxtime = 1000000 / 30;
	/*printf("Future framerate:%i %i %i\n",1000000/maxtime,starttime,endtime); */
	maxtime -= time1 - endtime;
    }
    /*s = context->s; */
    updatestatus();
}
static double step = 0;
static int menumax;
static void (*menucallback) (int);
static void ui_menu(char **text, char *label, int n, void (*callback) (int))
{
    int i;
    char s[50];
    if (driver->flags & PALETTE_REDISPLAYS)
	rotate_stop();
    numbertype = MENU;
    menumax = n;
    menucallback = callback;
    sprintf(s, "%-35s", label);
    driver->print(0, 0, s);
    for (i = 0; i < n; i++) {
	sprintf(s, " %i - %-30s", i + 1, text[i]);
	driver->print(0, (i + 1) * textwidth, s);
    }
}
static void ui_menupress(int number)
{
    if (number > menumax)
	return;
    displayed = 0;
    menucallback(number);
    if (!displayed)
	ui_display();
}

static void ui_fastmode(int mode)
{
    fastmode = mode;
}
static void ui_fastmode_menu(void)
{
    char *fastmodes[] =
    {
	"fast drawing disabled",
	"use only during animation",
	"use also for new images",
    };
    ui_menu(fastmodes, "Plase select drawing mode", 3, ui_fastmode);
}
static void ui_guessing(int mode)
{
    context->range = mode + 1;
}
static void ui_guessing_menu(void)
{
    char *guessmodes[] =
    {
	"solid guessing disabled",
	"guess maximally 2x2 rectangles",
	"guess maximally 3x3 rectangles",
	"guess maximally 4x4 rectangles",
	"guess maximally 5x5 rectangles",
	"guess maximally 6x6 rectangles",
	"guess maximally 7x7 rectangles",
	"guess maximally 8x8 rectangles",
    };
    ui_menu(guessmodes, "Plase select solid guessing mode", 8, ui_guessing);
}

static void ui_outcoloringmode(int mode)
{
    context->coloringmode = mode;
    context->coloringmode %= OUTCOLORING;
    ui_message();
    init_tables(context);
    ui_do_fractal(NEW_IMAGE);
}
static void ui_outcoloringmode_menu(void)
{
    ui_menu(outcolorname, "Please select outcoloring mode", OUTCOLORING, ui_outcoloringmode);
}
static void ui_incoloringmode(void)
{
    context->incoloringmode++;
    context->incoloringmode %= INCOLORING;
    ui_message();
    init_tables(context);
    ui_do_fractal(NEW_IMAGE);
}


static void ui_savefile(void)
{
    char *s;
    char str[256];

    if (!context->dirty) {
	driver->print(0, 0, "Recalculating image...");
	init_tables(context);
    }
    ui_do_fractal(UNINTERRUPTIBLE);
    rotate_stop();
    driver->print(0, 0, "Writing gif image..  ");
    s = writegif(context);
    rotate_continue();
    sprintf(str, "File %s saved", s);
    driver->print(0, 0, str);
}


void ui_tbreak(void)
{
    tbreak = 2;
}

static void ui_plane(int mode)
{
    /*recalculate(context, &context->s.mc, &context->s.mi);
       recalculate(context, &context->s.nc, &context->s.ni); */
    context->plane = mode;
    context->plane %= PLANES;
    /*recalculateback(context, &context->s.mc, &context->s.mi);
       recalculateback(context, &context->s.nc, &context->s.ni); */
    init_tables(context);
    /*context->s = s; */
    ui_message();
    ui_do_fractal(NEW_IMAGE);
    ui_tbreak();
}
static void ui_driver_menu(void)
{
    int i;
    char s[80];
    if (driver->flags & PALETTE_REDISPLAYS)
	rotate_stop();
    if (ndrivers != 1) {
	driver->print(0, 0, "Please select driver:                        ");
	driver->print(0, textwidth, "Warning..Xaos Can crash during this operation");
	driver->print(0, 2 * textwidth, "Palette will be set to default               ");
	driver->print(0, 3 * textwidth, " 1 - abort action                            ");
	for (i = 0; i < ndrivers; i++) {
	    sprintf(s, " %i - %-40s", i + 2, drivers[i]->name);
	    driver->print(0, (i + 4) * textwidth, s);
	}
	numbertype = DRIVER;
    } else
	driver->print(0, 0, "Only one driver available-function disabled");
}
static void ui_plane_menu(void)
{
    ui_menu(planename, "Please select display plane:", PLANES, ui_plane);
}

static void ui_mandelbrot(int mousex, int mousey)
{
    context->mandelbrot ^= 1;
    context->pre = xstoc(mousex), context->pim = ystoc(mousey);
    recalculate(context->plane, &context->pre, &context->pim);
    init_tables(context);
    ui_message();
    ui_do_fractal(NEW_IMAGE);
    ui_tbreak();
}

static int ui_autopilot(void)
{
    clean_autopilot();
    return (autopilot ^= 1);
}


static void ui_help(int number)
{
    int nlines = context->height / textwidth - 7;
    int i, page;
    int npages = (UGLYTEXTSIZE + driver->helpsize + nlines - 1) / (nlines ? nlines : 1);
    char str[80];
    if (nlines <= 0)
	return;
    if (!number)
	helppage--;
    if (number == 1)
	helppage++;
    if (helppage < 0)
	helppage = 0;
    if (helppage >= npages)
	helppage = npages - 1;
    page = helppage;
    if (driver->flags & PALETTE_REDISPLAYS)
	rotate_stop();
    sprintf(str, "------> Ugly help <-----> %2i/%2i <------", page + 1, npages);
    driver->print(0, textwidth, "                                       ");
    driver->print(0, 0, str);
    for (i = 0; i < nlines; i++) {
	if (page * nlines + i < UGLYTEXTSIZE)
	    driver->print(0, (i + 2) * textwidth, helptext[page * nlines + i]);
	else {
	    if (page * nlines + i < UGLYTEXTSIZE + driver->helpsize)
		driver->print(0, (i + 2) * textwidth, driver->help[page * nlines + i - UGLYTEXTSIZE]);
	    else
		driver->print(0, (i + 2) * textwidth, "~                                      ");
	}
    }
    driver->print(0, (nlines + 2) * textwidth, "                                       ");
    driver->print(0, (nlines + 3) * textwidth, "Hyperuglytext browser (tm) version 1.0 ");
    driver->print(0, (nlines + 4) * textwidth, "Press '1' for previous and '2' for next");
    driver->print(0, (nlines + 5) * textwidth, "             space for exit            ");
    numbertype = HELP;
}


void ui_status(void)
{
    char str[6000];

    sprintf(str, "Fractal name:%s", context->currentformula->name[!context->mandelbrot]);
    driver->print(0, textwidth, str);
    sprintf(str, "Fractal type:%s", context->mandelbrot ? "Mandelbrot" : "Julia");
    driver->print(0, 2 * textwidth, str);
    sprintf(str, "View:[%1.12f,%1.12f]", (double) context->s.nc, (double) context->s.ni);
    driver->print(0, 3 * textwidth, str);
    sprintf(str, "size:[%1.12f,%1.12f]", (double) (context->s.mc - context->s.nc), (double) (context->s.mi - context->s.ni));
    driver->print(0, 4 * textwidth, str);
    sprintf(str, "Screen size:%4i:%-4i", context->width, context->height);
    driver->print(0, 5 * textwidth, str);
    sprintf(str, "Iterations:%4i   Palette size:%i", context->maxiter, context->num_colors);
    driver->print(0, 6 * textwidth, str);
    sprintf(str, "Autopilot:%-4s    Plane:%s", autopilot ? "On" : "Off", planename[context->plane]);
    driver->print(0, 7 * textwidth, str);
    sprintf(str, "zoomspeed:%f", (float) maxstep * 1000);
    driver->print(0, 8 * textwidth, str);
    sprintf(str, "incoloring:%s outcoloring:%s", incolorname[context->incoloringmode], outcolorname[context->coloringmode]);
    driver->print(0, 9 * textwidth, str);
    if (context->mandelbrot)
	strcpy(str, "Parameter:none");
    else
	sprintf(str, "Parameter:[%f,%f]", (float) context->pre, (float) context->pim);
    driver->print(0, 10 * textwidth, str);
}

void ui_message(void)
{
    char s[80];

    sprintf(s, "Please wait while calculating %s", context->currentformula->name[!context->mandelbrot]);
    driver->print(0, 0, s);
    ui_status();
}

static void ui_changed(void)
{
    ui_tbreak();
    ui_message();
}

static int ui_mouse(int mousex, int mousey, int mousebuttons, int iterchange)
{
    int inmovement = 0, slowdown = 1;
    static double autopilot_counter = 0;
    static number_t oldx = 0, oldy = 0;
    static int dirty = 0;
    static int iterstep = 1;
    static int pressed;
    char str[80];


    CHECKPROCESSEVENTS(mousebuttons, iterchange);
    if (tbreak)
	mul = 1.0;
    if (juliamode) {
	if (mousebuttons & BUTTON1)
	    ui_do_julia(mousex, mousey);
	return (inmovement);
    }
    if (!rotate) {
	if (iterchange == 2 && tl_lookup_timer(maintimer) > FRAMETIME) {
	    tl_reset_timer(maintimer);
	    context->maxiter += iterstep;
	    if (!(context->maxiter % 10))
		iterstep = 10;
	    dirty = 1;
	    sprintf(str, "Iterations:%i ", context->maxiter);
	    driver->print(0, 0, str);
	    init_tables(context);
	    return 1;
	}
	if (iterchange == 1 && tl_lookup_timer(maintimer) > FRAMETIME) {
	    tl_reset_timer(maintimer);
	    context->maxiter -= iterstep;
	    if (!(context->maxiter % 10))
		iterstep = 10;
	    dirty = 1;
	    if (context->maxiter < 0) {
		context->maxiter = 0;
		return autopilot;
	    } else {
		sprintf(str, "Iterations:%i ", context->maxiter);
		driver->print(0, 0, str);
	    }
	    init_tables(context);
	    return 1;
	}
	if (!iterchange || iterchange == 3)
	    iterstep = 1;
	if (iterchange & 3)
	    return 1;
    }
    if (dirty)
	ui_message(), ui_do_fractal(NEW_IMAGE), ui_tbreak(), dirty = 0;
    if (autopilot && numbertype == FORMULA) {
	static int mousex1, mousey1, mousebuttons1;
	autopilot_counter += mul;
	while (autopilot_counter > 1) {
	    do_autopilot(context, &mousex1, &mousey1, &mousebuttons1, ui_changed);
	    CHECKPROCESSEVENTS(mousebuttons1, 0);
	    autopilot_counter--;
	}
	mousex = mousex1, mousey = mousey1, mousebuttons = mousebuttons1;
    }
    if (tbreak)
	mul = 1;
    if (mul == 0)
	mul = 0.001;
    if (mousebuttons > 0) {
	switch (mousebuttons) {
	case BUTTON1:		/* button 1 */
	    step -= speedup * 2 * mul, slowdown = 0;
	    inmovement = 1;
	    break;
	case BUTTON3:		/* button 3 */
	    step += speedup * 2 * mul, slowdown = 0;
	    inmovement = 1;
	    break;
	case BUTTON2:		/* button 2 */
	    {
		number_t x = xstoc(mousex), y = ystoc(mousey);
		if (pressed && (oldx != x || oldy != y)) {
		    context->s.nc -= x - oldx;
		    context->s.mc -= x - oldx;
		    context->s.ni -= y - oldy;
		    context->s.mi -= y - oldy;
		    if (!step) {
			ui_do_fractal(ANIMATION), ui_tbreak();
		    }
		}
		pressed = 1;
		oldx = xstoc(mousex), oldy = ystoc(mousey);
	    }
	    break;
	default:
	    break;
	}
    }
    if (!(mousebuttons & BUTTON2))
	pressed = 0;
    if (slowdown) {
	if (step > 0) {
	    if (step < speedup * mul)
		step = 0;
	    else
		step -= speedup * mul;
	} else if (step < 0) {
	    if (step > -speedup * mul)
		step = 0;
	    else
		step += speedup * mul;
	}
    }
    if (step > maxstep)
	step = maxstep;
    else if (step < -maxstep)
	step = -maxstep;
    if (step) {
	number_t x = xstoc(mousex);
	number_t y = ystoc(mousey);
	number_t mmul = pow((double) (1 + step), (double) mul);
	context->s.mc = x + (context->s.mc - x) * (mmul);
	context->s.nc = x + (context->s.nc - x) * (mmul);
	context->s.mi = y + (context->s.mi - y) * (mmul);
	context->s.ni = y + (context->s.ni - y) * (mmul);
	inmovement = 1;
	ui_do_fractal(ANIMATION);
	if (tbreak)
	    tbreak--;
    }
    if (!inmovement)
	ui_tbreak();
    if (rotate) {
	if (iterchange & 1 && (tl_lookup_timer(maintimer) > FRAMETIME || mousebuttons)) {
	    rotationspeed--;
	    if (!rotationspeed)
		rotationspeed = -1;
	    if (!inmovement)
		tl_reset_timer(maintimer);
	    sprintf(str, "rotationspeed:%i ", rotationspeed);
	    rotate_update();
	    driver->print(0, 0, str);
	}
	if (iterchange & 2 && (tl_lookup_timer(maintimer) > FRAMETIME || mousebuttons)) {
	    rotationspeed++;
	    if (!rotationspeed)
		rotationspeed = 1;
	    if (!inmovement)
		tl_reset_timer(maintimer);
	    sprintf(str, "rotationspeed:%i ", rotationspeed);
	    rotate_update();
	    driver->print(0, 0, str);
	}
    }
    if (iterchange & 4 && (tl_lookup_timer(maintimer) > FRAMETIME || mousebuttons)) {
	double mul1 = (double) tl_lookup_timer(maintimer) / FRAMETIME;
	double su = 1 + (SPEEDUP - 1) * mul1;
	if (su > 2 * SPEEDUP)
	    su = SPEEDUP;
	if (!inmovement)
	    tl_reset_timer(maintimer);
	else
	    su = 1 + (SPEEDUP - 1) * mul;
	speedup *= su, maxstep *= su;
	sprintf(str, "speed:%2.2f ", (double) speedup * (1 / STEP));
	driver->print(0, 0, str);
    }
    if (iterchange & 8 && (tl_lookup_timer(maintimer) > FRAMETIME || mousebuttons)) {
	double mul1 = (double) tl_lookup_timer(maintimer) / FRAMETIME;
	double su = 1 + (SPEEDUP - 1) * mul1;
	if (su > 2 * SPEEDUP)
	    su = SPEEDUP;
	if (!inmovement)
	    tl_reset_timer(maintimer);
	else
	    su = 1 + (SPEEDUP - 1) * mul;
	speedup /= su, maxstep /= su;
	sprintf(str, "speed:%2.2f ", (double) speedup * (1 / STEP));
	driver->print(0, 0, str);
    }
    if (iterchange & 15)
	inmovement = 1;
    return (inmovement || (autopilot && numbertype == FORMULA) || inmovement);
}
void ui_call_resize(void)
{
    callresize = 1;
}
void ui_resize(void)
{
    int w, h, scanline;
    char *b1, *b2;
    if (incalculation) {
	resized = 1;
	context->interrupt = 1;
	return;
    }
    ui_tbreak();
    rotate_stop();
    driver->getsize(&w, &h);
    assert(w > 0 && w < 65000 && h > 0 & h < 65000);
    if (w != context->width || h != context->height || (driver->flags & UPDATE_AFTER_RESIZE)) {
	driver->free_buffers((char *) VBUFF, (char *) BACK);
	if (!(scanline = driver->alloc_buffers(&b1, &b2))) {
	    driver->uninit();
	    printf("Can not allocate buffers\n");
	    ui_outofmem();
	    exit(-1);
	}
	assert(scanline >= w);
	if (!resize_to(context, w, h, scanline, b1, b2, get_pixelwidth(w), get_pixelheight(h))) {
	    driver->uninit();
	    printf("Can not allocate tables\n");
	    ui_outofmem();
	    exit(-1);
	}
	ui_message();
	tl_process_group(syncgroup);
	tl_reset_timer(maintimer);
	ui_do_fractal(NEW_IMAGE);
	ui_tbreak();
    }
    ui_display();
    rotate_continue();
}
static void ui_driver(int d)
{
    int w, h, scanline;
    char *b1, *b2;
    struct ui_driver *driver1;
    if (d < 0)
	d = 0;
    if (d >= ndrivers)
	d = ndrivers - 1;
    driver1 = driver;
    rotate_off();
    driver->free_buffers((char *) VBUFF, (char *) BACK);
    driver->uninit();
    driver = drivers[d];
    if (!driver->init()) {
	driver = driver1;
	printf("Can not initialize new driver\n");
	if (!driver->init()) {
	    printf("Can not return back to previous driver\n");
	    exit(-1);
	}
    }
    context->switch_function = driver->flip_buffers;
    context->fullscreen = (driver->flags & FULLSCREEN) != 0;
    driver->getsize(&w, &h);
    assert(w > 0 && w < 65000 && h > 0 & h < 65000);

    if (!(scanline = driver->alloc_buffers(&b1, &b2))) {
	driver->uninit();
	printf("Can not allocate buffers\n");
	ui_outofmem();
	exit(-1);
    }
    assert(scanline >= w);
    init_tables(context);
    resize_to(context, w, h, scanline, b1, b2, get_pixelwidth(w), get_pixelheight(h));
    rotate_off();
    mkdefaultpalette(context, driver->set_color, (driver->flags & RANDOM_PALETTE_SIZE) != 0);
    ui_message();
    ui_do_fractal(NEW_IMAGE);
    ui_tbreak();
    ui_display();
}
static void processbuffer(void)
{
    int k;
    for (; begin != end % SIZE;) {
	if (resized)
	    resized = 0, ui_resize();
	k = keybuffer[begin];
	begin = (begin + 1) % SIZE;
	ui_key(k);
    }
}

int ui_key(int key)
{
    int sym;
    int lasttype = numbertype;
#ifdef DEBUG
    printf("key:%c\n", key);
#endif
    if (incalculation) {
	int i;
	if (tolower(key) == ' ')
	    ui_display();
	else if (tolower(key) == 'z')
	    context->interrupt = 1;
	else {
	    add_buffer(key);
	    for (i = 0; i < strlen(interruptkeys); i++) {
		if (tolower(key) == interruptkeys[i])
		    context->interrupt = 1;
	    }
	}
	return 1;
    }
    switch (sym = tolower(key)) {
    case 'o':
	if (rotate)
	    rotate_off();
	else {
	    if (!context->stereogram)
		rotate_on();
	}
	break;
    case 'j':
	if (juliamode)
	    juliamode_off();
	else {
	    if (context->currentformula->juliamode != -1 && context->mandelbrot) {
		int mousex, mousey, b;
		driver->getmouse(&mousex, &mousey, &b);
		juliamode_on(mousex, mousey);
	    } else {
		driver->print(0, 0, "Julia mode not available for this fractal. Sorry");
	    }
	}
	break;
    case 'v':
	ui_driver_menu();
	break;
    case '=':
	callresize = 1;
	break;
    case 'm':
	{
	    int mousex, mousey, b;
	    if (juliamode)
		ui_mandelbrot(lastjx, lastjy);
	    else {
		driver->getmouse(&mousex, &mousey, &b);
		ui_mandelbrot(mousex, mousey);
	    }
	}
	break;
    case 'i':
	ui_plane_menu();
	break;
    case 'f':
	ui_incoloringmode();
	break;
    case 'c':
	ui_outcoloringmode_menu();
	break;
    case 'h':
	ui_help(5);
	break;
    case 's':
	ui_savefile();
	break;
    case 't':
	ui_fastmode_menu();
	break;
    case 'g':
	ui_guessing_menu();
	break;
    case '?':
    case '/':
	ui_status();
	break;
    case 'r':
	ui_message();
	init_tables(context);
	ui_tbreak();
	ui_do_fractal(NEW_IMAGE);
	break;
    case 'p':
	{
	    int recalculate;
	    if (context->stereogram)
		break;
	    rotate_stop();
	    recalculate = mkpalette(context, driver->set_color, (driver->flags & RANDOM_PALETTE_SIZE) != 0);
	    rotate_continue();
	    if (recalculate) {
		ui_message();
		init_tables(context);
		ui_tbreak();
		ui_do_fractal(NEW_IMAGE);
	    } else if (driver->flags & UPDATE_AFTER_PALETTE) {
		ui_tbreak();
		ui_display();
	    }
	}
	break;
    case 'd':
	{
	    int recalculate;
	    if (context->stereogram)
		break;
	    rotate_stop();
	    recalculate = mkdefaultpalette(context, driver->set_color, (driver->flags & RANDOM_PALETTE_SIZE) != 0);
	    rotate_continue();
	    if (recalculate) {
		ui_message();
		init_tables(context);
		ui_tbreak();
		ui_do_fractal(NEW_IMAGE);
	    } else if (driver->flags & UPDATE_AFTER_PALETTE) {
		ui_tbreak();
		ui_display();
	    }
	}
	break;
    case ' ':
	ui_display();
	break;
    case 'l':
	ministatus ^= 1;
	ui_display();
	break;
    case 'e':
	if (!context->stereogram) {
	    enable_stereogram(context);
	    rotate_off();
	    mkstereogrampalette(context, driver->set_color);
	    rotate_continue();
	    ui_do_fractal(NEW_IMAGE);
	    ui_tbreak();
	} else {
	    int recalculate;
	    disable_stereogram(context);
	    rotate_stop();
	    recalculate = mkdefaultpalette(context, driver->set_color, (driver->flags & RANDOM_PALETTE_SIZE) != 0);
	    rotate_continue();
	    if (recalculate)
		ui_do_fractal(NEW_IMAGE);
	    else
		ui_display();
	    ui_tbreak();
	}
	break;
    case 'a':
	ui_autopilot();
	return (1);
	break;
    case 'q':
	rotate_off();
	driver->free_buffers((char *) VBUFF, (char *) BACK);
	driver->uninit();
	printf("Thank you for using XaoS\n");
	exit(0);
	break;
    default:
	{
	    int number;
	    if (sym >= '0' && sym <= '9') {
		number = sym - '1';
		if (number < 0)
		    number = 9;
		switch (numbertype) {
		case FORMULA:
		    set_formula(context, number);
		    ui_message();
		    ui_do_fractal(NEW_IMAGE);
		    ui_tbreak();
		    break;
		case HELP:
		    ui_help(number);
		    break;
		case MENU:
		    ui_menupress(number);
		    break;
		case DRIVER:
		    if (number)
			todriver = number;
		    else
			ui_display();
		    return 2;
		    break;
		}
	    }
	}
	break;
    }
    if (numbertype != FORMULA && lasttype == FORMULA)
	step = 0;
    processbuffer();
    return 0;
}
#ifdef _EFENCE_
extern int EF_ALIGNMENT;
extern int EF_PROTECT_BELOW;
extern int EF_PROTECT_FREE;
#endif
static void main_loop(void)
{
    int inmovement = 1;
    int x, y, b, k;
    int time;
    while (1) {
	processbuffer();
	if (context->uncomplette && numbertype == FORMULA && !juliamode) {
	    inmovement = 1;
	    if (!displayed)
		ui_do_fractal(ANIMATION);
	}
	if ((time = tl_process_group(syncgroup)) != -1) {
	    if (!inmovement) {
		if (time > 1000000 / 50)
		    time = 1000000 / 50;
		if (!inmovement && !autopilot)
		    tl_sleep(time);
	    }
	    inmovement = 1;
	}
	driver->processevents(!(inmovement || autopilot), &x, &y, &b, &k);
	displayed = 0;
	inmovement = ui_mouse(x, y, b, k);
	if (todriver)
	    ui_driver(todriver - 1), todriver = 0;
	if (callresize)
	    ui_resize(), callresize = 0;
    }
}

int main(int argc, char **argv)
{
    int width, height, scanline;
    int i, formula = 0;
    char *buffer1, *buffer2;
    params_parser(argc, argv);
#ifdef _EFENCE_
    EF_ALIGNMENT = 0;
    EF_PROTECT_BELOW = 1;
    /* EF_PROTECT_FREE=1; */
#endif
    maintimer = tl_create_timer();
#ifdef DEBUG
    printf("Initialising driver\n");
#endif
#ifndef _plan9_
    signal(SIGFPE, SIG_IGN);
#endif
    if (printconfig) {
#define tostring(s) #s
	printf("XaoS configuration\n"
	       "Version:   %s\n"
	       "Type size: %i\n"
	       "Maxiter:   %i\n"
	       "MaxStep:   %f\n"
	       "integer size: %i\n"

#ifndef _plan9_
#ifdef HAVE_ALLOCA
	       "using alloca\n"
#endif
#ifdef HAVE_LONG_DOUBLE
	       "using long double\n"
#endif
#ifdef USE_NCURSES
	       "using ncurses\n"
#endif
#ifdef const
	       "const disabled\n"
#endif
#ifdef inline
	       "inline disabled\n"
#endif
#ifdef HAVE_GETTIMEOFDAY
	       "using gettimeofday\n"
#endif
#ifdef HAVE_FTIME
	       "using ftime\n"
#endif
#ifdef MITSHM
	       "using mitshm\n"
#endif
#ifdef HAVE_MOUSEMASK
	       "using ncurses mouse\n"
#endif
#ifdef DEBUG
	       "debug enabled\n"
#endif
#ifdef NDEBUG
	       "assertions disabled\n"
#endif
#ifdef STATISTICS
#endif
	       "statistics enabled\n"
#endif
	       ,XaoS_VERSION, sizeof(FPOINT_TYPE), DEFAULT_MAX_ITER, MAXSTEP, sizeof(int));
    }
    if (deflist || printconfig) {
	printf("Available drivers:\n");
	for (i = 0; i < ndrivers; i++) {
	    printf("   %s\n", drivers[i]->name);
	}
	exit(0);
    }
    if (defdriver != NULL) {
	for (i = 0; i < ndrivers; i++) {
	    int y;
	    for (y = 0; tolower(drivers[i]->name[y]) == tolower(defdriver[y]) && drivers[i]->name[y] != 0; y++);
	    if (drivers[i]->name[y] == 0) {
		driver = drivers[i];
		if (driver->init())
		    break;
		else {
		    printf("Can not initialize %s driver\n", defdriver);
		    exit(-1);
		}
	    }
	}
	if (i == ndrivers) {
	    printf("Unknown driver %s\n", defdriver);
	    exit(-1);
	}
    } else {
	for (i = 0; i < ndrivers; i++) {
	    driver = drivers[i];
	    if (driver->init())
		break;
	}
	if (i == ndrivers) {
	    printf("Can not initialize driver\n");
	    exit(-1);
	}
    }
#ifdef DEBUG
    printf("Getting size\n");
#endif
    driver->getsize(&width, &height);
    if (!(scanline = driver->alloc_buffers(&buffer1, &buffer2))) {
	driver->uninit();
	printf("Can not alocate buffers\n");
	exit(-1);
    }
    for (i = 0; i < nformulas; i++)
	if (defformula[i])
	    formula = i;
    context = make_context(width, height, scanline, formula, (driver->flags & FULLSCREEN) != 0, driver->flip_buffers, ui_waitfunc, buffer1, buffer2, get_pixelwidth(width), get_pixelheight(height));
    if (!context) {
	driver->uninit();
	printf("Can not allocate context\n");
	ui_outofmem();
	exit(-1);
    }
    set_formula(context, formula);
    srand(time(NULL));
    mkdefaultpalette(context, driver->set_color, (driver->flags & RANDOM_PALETTE_SIZE) != 0);
    tbreak = 2;
    context->maxiter = defiters;
    fastmode--;
    context->range = defguessing;
    context->incoloringmode = abs(defincoloring - 1) % INCOLORING;
    context->coloringmode = abs(defoutcoloring - 1) % OUTCOLORING;
    context->plane = abs(defplane - 1) % PLANES;
    ui_message();
    ui_do_fractal(NEW_IMAGE);
    if (defautopilot)
	ui_autopilot();
    if (defspeed <= 0 || defspeed >= 100)
	defspeed = 1.0;
    speedup = STEP * defspeed;
    tl_process_group(syncgroup);
    tl_reset_timer(maintimer);
    main_loop();
    driver->free_buffers((char *) VBUFF, (char *) BACK);
    driver->uninit();
    printf("Thank you for choosing XaoS!\n");
    return (0);
}
zoom_context *
 ui_getcontext()
{
    return (context);
}
