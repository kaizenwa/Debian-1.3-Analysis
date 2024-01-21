/* wintype.h - xtron v1.1 header for wintype.c
 *
 *   Copyright (C) 1995 Rhett D. Jacobs <rhett@hotel.canberra.edu.au>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 1, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Last Modified: 16/4/95
 */

#ifndef _WINTYPE_H
#define _WINTYPE_H

#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>    
#include <X11/Xutil.h>    
#include <X11/keysym.h>

#include "xpm2pixmap.h"

extern Display *display;
extern int screen;
extern Window main_window;
extern GC gc;
extern unsigned long foreground;
extern unsigned long background;

void win_setup(void);
void win_shutdown(void);
GC win_getGC(void);
unsigned long ColourSet(char *name);
Window win_open(int x, int y, int width, int height, int border_width,
		Window parent, int istoplevel, int argc, char *argv[],
		char *win_name);

#endif
