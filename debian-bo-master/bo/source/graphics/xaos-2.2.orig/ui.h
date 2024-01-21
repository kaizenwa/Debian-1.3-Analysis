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
#ifndef UI_H
#define UI_H
#include "zoom.h"
#include "param.h"

#define BUTTON1 256
#define BUTTON2 512
#define BUTTON3 1024

#ifdef _UNDEFINED_
void ui_do_fractal(void);
void ui_savefile(void);
void ui_coloringmode(int);
void ui_mandelbrot(int, int);
void ui_incoloringmode(void);
int ui_autopilot(void);
void ui_help(int);
int ui_mouse(int, int, int, int);
int ui_inverse(int);
#endif
void ui_message(void);
void ui_status(void);
void ui_tbreak(void);
void ui_resize(void);
void ui_call_resize(void);
int ui_key(int);
zoom_context *ui_getcontext(void);

struct ui_driver {
    char *name;
    int (*init) (void);		/*initializing function. recturns 0 if fail */
    void (*getsize) (int *, int *);	/*get current size..in fullscreen versions
					   i.e svga and dos asks user for it */
    void (*processevents) (int, int *, int *, int *, int *);
    /*processevents..calls ui_resize,ui_key
       laso returns possitions of mouse..
       waits for event if first parameter is
       1 */
    void (*getmouse) (int *, int *, int *);
    /*returns current mouse possitions */
    void (*uninit) (void);
    /*called before exit */
    int (*set_color) (int, int, int, int);
    /*sets palette color and returns number */
    void (*print) (int, int, char *);	/*prints text */
    void (*display) (void);	/*displays bitmap */
    int (*alloc_buffers) (char **buffer1, char **buffer2);	/*makes buffers */
    void (*free_buffers) (char *buffer1, char *buffer2);	/*frees buffers */
    void (*flip_buffers) (void);	/*prints text */
    void (*rotate_palette) (int);	/*rotates palette for n steps */
    int maxsize;		/*maximal size of palette */
    int textheight;		/*width of text */
    char **help;
    int helpsize;
    struct params *params;
    int flags;
    float width, height;
    int maxwidth, maxheight;
};

#define RANDOM_PALETTE_SIZE 1
#define FULLSCREEN 2
#define UPDATE_AFTER_PALETTE 4
#define UPDATE_AFTER_RESIZE 8
#define PALETTE_ROTATION 16
#define ASYNC_PALETTE 32
#define ROTATE_INSIDE_CALCULATION 64
#define PALETTE_REDISPLAYS 128
#define SCREENSIZE 256
#define PIXELSIZE 512
#define RESOLUTION 1024

#endif				/* UI_H */
