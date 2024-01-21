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
#ifdef SVGA_DRIVER
#include <stdio.h>
#include <malloc.h>
#include <vga.h>
#include <vgagl.h>
#include <vgamouse.h>
#include <vgakeyboard.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include "keytables.h"
#include "zoom.h"
#include "gif.h"
#include "ui.h"
#include "palette.h"

struct ui_driver svga_driver;
static char keys[128];
static int initialised;
static int mode = -1;
static vga_modeinfo *info;
static width = 640, height = 480;
static int svga_currentbuff = 0;
static int ncolors = 0;
static int defmode = -1;
static GraphicsContext screen;
static GraphicsContext buffers[2];
void *font = NULL;

static void svga_print(int x, int y, char *text)
{
    gl_setcontext(&screen);
    gl_enableclipping();
    if (y > height - svga_driver.textheight)
	return;
    gl_write(x, y / svga_driver.textheight * 8, text);
}

static void draw_mouse(int x, int y, int clear)
{
    static int oldx, oldy;

    if (x < 3)
	x = 3;
    if (y < 3)
	y = 3;
    if (x > width - 5)
	x = width - 5;
    if (y > height - 5)
	y = height - 5;
    if ((oldx != x || oldy != y) && clear) {
	gl_setcontext(&buffers[svga_currentbuff]);
	gl_enableclipping();
	if (mode == G320x240x256
	    || mode == G320x400x256
	    || mode == G360x480x256)
	    gl_copyscreen(&screen);
	else
	    gl_copyboxtocontext(oldx - 3, oldy - 3, 8, 8, &screen, oldx - 3, oldy - 3);
    }
    vga_setcolor(255);
    vga_drawline(x - 3, y - 3, x + 3, y + 3);
    vga_drawline(x - 3, y + 3, x + 3, y - 3);
    vga_setcolor(0);
    vga_drawline(x + 1 - 3, y - 3, x + 1 + 3, y + 3);
    vga_drawline(x + 1 - 3, y + 3, x + 1 + 3, y - 3);
    oldx = x;
    oldy = y;
}

static void svga_display()
{
    gl_setcontext(&buffers[svga_currentbuff]);
#if 0
    vga_waitretrace();
#endif
    gl_copyscreen(&screen);
    draw_mouse(mouse_getx(), mouse_gety(), 0);
}

static int svga_set_color(int r, int g, int b, int init)
{
    if (init)
	ncolors = 1;
    if (ncolors == 255)
	return (-1);
    gl_setpalettecolor(ncolors, r / 4, g / 4, b / 4);
    return (ncolors++);
}
static void svga_rotate(int direction)
{
    int i;
    zoom_context *c;
    c = ui_getcontext();
    vga_waitretrace();
    /*vga_screenoff(); */
    for (i = 1; i <= c->num_colors; i++) {
	vga_setpalette(i,
		       c->cmap[0][i] >> 2,
		       c->cmap[1][i] >> 2,
		       c->cmap[2][i] >> 2);
    }
    /*vga_screenon(); */
#if 0
    *(co++) = c->cmap[0][i] / 4;
    *(co++) = c->cmap[1][i] / 4;
    *(co++) = c->cmap[2][i] / 4;
#endif
}

static void fix_palette()
{
    int i;
    zoom_context *context = ui_getcontext();

    for (i = 255; i >= 0; i--)
	gl_setpalettecolor(context->colors[i], context->cmap[0][i] / 4, context->cmap[1][i] / 4, context->cmap[2][i] / 4);
    gl_setpalettecolor(255, 63, 63, 63);
}

static void svga_flip_buffers()
{
    svga_currentbuff ^= 1;
}

static void handler(int scancode, int press)
{
    keys[scancode] = press;
    if (press)
	ui_key(plain_table[scancode]);
}
static void init_mode()
{
    int i, high;

    while (mode == -1) {
	printf("Choose one of the following video modes: \n");
	high = 0;
	for (i = 1; i <= GLASTMODE; i++)
	    if (vga_hasmode(i)) {
		char expl[100];
		char *cols = NULL;
		*expl = '\0';
		info = vga_getmodeinfo(i);
		if (info->colors != 256)
		    continue;
		if (i == G320x200x256)
		    strcpy(expl, "packed-pixel");
		else if (i == G320x240x256
			 || i == G320x400x256
			 || i == G360x480x256)
		    strcpy(expl, "Mode X");
		else
		    strcpy(expl,
			   "packed-pixel, banked");
		if (info->flags & IS_INTERLACED) {
		    if (*expl != '\0')
			strcat(expl, ", ");
		    strcat(expl, "interlaced");
		}
		if (info->flags & IS_DYNAMICMODE) {
		    if (*expl != '\0')
			strcat(expl, ", ");
		    strcat(expl, "dynamically loaded");
		}
		high = i;
		if (defmode == -1) {
		    printf("%5d: %dx%d, ",
			   i, info->width, info->height);
		    if (cols == NULL)
			printf("%d", info->colors);
		    else
			printf("%s", cols);
		    printf(" colors ");
		    if (*expl != '\0')
			printf("(%s)", expl);
		    printf("\n");
		}
	    }
	if (defmode == -1) {
	    printf("Enter mode number (1-%d): ", high);
	    fflush(stdout);
	    scanf("%d", &mode);
	    getchar();
	    printf("\n");
	} else
	    mode = defmode, defmode = -1;
	if (mode < 1 || mode > GLASTMODE) {
	    printf("Error: Mode number out of range \n");
	    mode = -1;
	    continue;
	}
	if (!vga_hasmode(mode)) {
	    printf("Error: Video mode not supported by driver\n");
	    mode = -1;
	    continue;
	}
	info = vga_getmodeinfo(mode);
	if (info->colors != 256) {
	    printf("Error: Only 256 color modes supported\n");
	    mode = -1;
	    continue;
	}
    }
    width = info->width;
    height = info->height;
    vga_setmode(mode);
    if (mode == G320x240x256
	|| mode == G320x400x256
	|| mode == G360x480x256)
	svga_driver.textheight = 8 * 4;
    else
	svga_driver.textheight = 8;
    gl_setcontextvga(mode);
    gl_getcontext(&screen);
    keyboard_init();
    keyboard_translatekeys(0);
#ifdef VGA_KEYEVENT
    keyboard_seteventhandler(handler);
#endif
    gl_setpalettecolor(255, 63, 63, 63);
    gl_setpalettecolor(0, 0, 0, 0);
    if (font == NULL) {
	font = (char *) malloc(256 * 8 * 8);
	gl_expandfont(8, 8, 255, gl_font8x8, font);
	gl_setfont(8, 8, font);
    }
    draw_mouse(mouse_getx(), mouse_gety(), 0);
}

static void svga_uninitialise()
{
    keyboard_close();
    vga_setmode(TEXT);

}



static int svga_init()
{
    int fd;
    struct stat chkbuf;
    if ((fd = open("/dev/console", O_RDONLY)) < 0) {
	return (0);
    }
    close(fd);
    fstat(2, &chkbuf);
    if (chkbuf.st_rdev >> 8 != 4 || (chkbuf.st_rdev & 0xff) >= 64) {
	return (0);
    }
    if ((fd = open("/dev/mem", O_RDWR)) < 0) {
	return (0);
    }
    close(fd);
    if (!initialised) {
	vga_setmousesupport(1);
	vga_init();
	initialised = 1;
    }
    return (1);
}
static void svga_get_size(int *x, int *y)
{
    int m = 0;
    if (mode != -1) {
	m = 1;
	svga_uninitialise();
	mode = -1;
    }
    init_mode();
    *x = width;
    *y = height;
    if (m)
	fix_palette();
}
static void svga_getmouse(int *x, int *y, int *buttons)
{
    mouse_update();
    *x = mouse_getx();
    *y = mouse_gety();
    *buttons = 0;
    if (mouse_getbutton() == MOUSE_LEFTBUTTON)
	*buttons |= BUTTON1;
    if (mouse_getbutton() == MOUSE_RIGHTBUTTON)
	*buttons |= BUTTON3;
    if (mouse_getbutton() & MOUSE_MIDDLEBUTTON || mouse_getbutton() == (MOUSE_LEFTBUTTON | MOUSE_RIGHTBUTTON))
	*buttons |= BUTTON2;
    draw_mouse(mouse_getx(), mouse_gety(), 1);
}
static void svga_processevents(int wait, int *x, int *y, int *buttons, int *k)
{
    if (wait) {
#ifdef VGA_KEYEVENT
	struct timeval timeout;
	fd_set inputs;
	FD_ZERO(&inputs);
	timeout.tv_sec = 999999;
	timeout.tv_usec = 999999;
	vga_waitevent(VGA_KEYEVENT | VGA_MOUSEEVENT, &inputs, NULL, NULL, &timeout);
#else
#warning Your SVGALIB is old. Please update to latest one to get better XaoS
	mouse_update();
	keyboard_update();
#endif
    } else {
	mouse_update();
	keyboard_update();
    }
#ifndef VGA_KEYEVENT
    {
	int i;
	for (i = 0; i < 128; i++)
	    if (keys[i] && !keyboard_keypressed(i))
		handler(i, 0);
	    else {
		if (!keys[i] && keyboard_keypressed(i))
		    handler(i, 1);
	    }
    }
#endif
    *x = mouse_getx();
    *y = mouse_gety();
    *buttons = 0;
    if (mouse_getbutton() == MOUSE_LEFTBUTTON)
	*buttons |= BUTTON1;
    if (mouse_getbutton() == MOUSE_RIGHTBUTTON)
	*buttons |= BUTTON3;
    if (mouse_getbutton() & MOUSE_MIDDLEBUTTON || mouse_getbutton() == (MOUSE_LEFTBUTTON | MOUSE_RIGHTBUTTON))
	*buttons |= BUTTON2;
    *k = keys[SCANCODE_CURSORBLOCKLEFT] +
	2 * keys[SCANCODE_CURSORBLOCKRIGHT] +
	4 * keys[SCANCODE_CURSORBLOCKUP] +
	8 * keys[SCANCODE_CURSORBLOCKDOWN];
    draw_mouse(mouse_getx(), mouse_gety(), 1);
}
static int svga_alloc_buffers(char **buffer1, char **buffer2)
{
    svga_currentbuff = 0;
    gl_setcontextvgavirtual(mode);
    gl_getcontext(&buffers[0]);
    gl_setcontextvgavirtual(mode);
    gl_getcontext(&buffers[1]);
    *buffer1 = buffers[0].vbuf;
    *buffer2 = buffers[1].vbuf;
    return (width);
}
static void svga_free_buffers(char *buffer1, char *buffer2)
{
    gl_freecontext(&buffers[0]);
    gl_freecontext(&buffers[1]);
}
static char *helptext[] =
{
    "SVGALIB DRIVER VERSION 2.1             ",
    "==========================             ",
    " I really like this driver, because    ",
    " I much prefer full screen zooming     ",
    " instead of small 320x320 window in X11",
    " It was one of first drivers for XaoS  ",
    " and is fully featured.                ",
    " The following problems can ocour:     ",
    " o Mouse doesn't work                  ",
    " o Screen is blank in higher resolution",
    "    Both this problems are probably    ",
    "    caused by wrong configuration of   ",
    "    SVGAlib. Please configure it in    ",
    "    etc/vga/libvga.cong or             ",
    "    /usr/local/lib/libvga.conf         ",
    "    Also gpm can cause problems.       ",
    "    Try to kill it before starting XaoS",
    " o It crashes my computer              ",
    " o It doesn't start                    ",
    "    Thats can not be true. You can see ",
    "    this text!                         ",
    " o Text on screen is STRANGE           ",
    "    You probably don't have this       ",
    "    problem, because you are reading   ",
    "    this text.                         ",
    "    This problem is caused by vgagl    ",
    "    limitiation in modes-X. It doesn't ",
    "    work. Try to ignore it and don't   ",
    "    read messages.                     ",
    " o When I switch console I can't switch",
    "    back.                              ",
    "    This is another typical SVGAlib bug",
    "    Try to hodl F key longer than alt. ",
    "    It helps on my computer.           ",
    "    On older svgalibs there was famous ",
    "    'enter bug' that caused crash after",
    "    pressing enter. Try to update to   ",
    "    newer release.                     ",
    "                                       ",
    " SVGAlib driver was done by Jan Hubicka",
    "              (C) 1997                 ",
};
#define UGLYTEXTSIZE (sizeof(helptext)/sizeof(char *))
static struct params params[] =
{
    {"-mode", P_NUMBER, &defmode, "Select graphics mode(same number as in interactive menu)"},
    {NULL, 0, NULL, NULL}
};

struct ui_driver svga_driver =
{
    "SVGAlib",
    svga_init,
    svga_get_size,
    svga_processevents,
    svga_getmouse,
    svga_uninitialise,
    svga_set_color,
    svga_print,
    svga_display,
    svga_alloc_buffers,
    svga_free_buffers,
    svga_flip_buffers,
    svga_rotate,
    256,
    8,
    helptext,
    UGLYTEXTSIZE,
    params,
    FULLSCREEN | UPDATE_AFTER_RESIZE | PALETTE_ROTATION | ROTATE_INSIDE_CALCULATION,
    0.0, 0.0,
    0, 0,
};
#endif
