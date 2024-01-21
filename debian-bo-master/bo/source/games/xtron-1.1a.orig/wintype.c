/* wintype.c - xtron v1.1 X windows handler
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

#include"wintype.h"

void win_setup(void)
{
  /* set the display name from  the environment variable DISPLAY */    
  display = XOpenDisplay (NULL);    
  if (display == NULL) {
    fprintf (stderr,"Unable to open display %s\n",    
	     XDisplayName (NULL));    
    exit (1);    
  } 
  screen = DefaultScreen (display);
  foreground = ColourSet("black");
  background = ColourSet("grey75");
}


unsigned long ColourSet(char *name)
{
  int DefaultDepth;
  Colormap DefaultCMap;
  XColor ExactDef;
  Visual *DefaultVisual;

  DefaultDepth = DefaultDepth(display, screen);
  DefaultVisual = DefaultVisual(display, screen);
  DefaultCMap = DefaultColormap(display, screen);

  if (!XParseColor(display, DefaultCMap, name, &ExactDef)) {
    fprintf(stderr,"error: colour not in colour database..\n");
    exit(0);
  }
  if (!XAllocColor(display, DefaultCMap, &ExactDef)) {
    fprintf(stderr,"error: can't allocate colour..\n");
    exit(0);
  }
  return(ExactDef.pixel);
}


void win_shutdown(void)
{
  XFreeGC(display, gc);
  XCloseDisplay (display);    
  exit (0);    
}


Window win_open(int x, int y, int width, int height, int border_width,
		Window parent, int istoplevel, int argc, char *argv[],
		char *win_name)
{
  Window new_window;
  XSizeHints size_hints;

  /* create window */
  new_window = XCreateSimpleWindow(display, parent, x, y, width, height,
				   border_width, foreground, background);

  /* set size hints for window manager */
   if (istoplevel) {
     size_hints.flags = PMinSize | PMaxSize | PSize | PResizeInc;
     size_hints.min_width = width;
     size_hints.max_width = width;
     size_hints.min_height = height;
     size_hints.max_height = height;
     size_hints.width_inc=1;
     size_hints.height_inc=1;
    
     XSelectInput(display, new_window,
		  ButtonPressMask | KeyPressMask | ExposureMask |
		  VisibilityChangeMask | FocusChangeMask);
     
     /* state which hints are included */
     XSetStandardProperties (display, new_window, win_name, 
			     "xtron", ReadXPM("Xtron_icon.xpm"),
			     argv, argc, &size_hints);
   }
  /* return the window ID */
  return (new_window);
}


GC win_getGC(void)
{
  XGCValues gcValues;

  gc = XCreateGC (display, main_window,
		  (unsigned long) 0, &gcValues);
  
  XSetBackground (display, gc, background);
  XSetForeground (display, gc, foreground);

  return (gc);
}
