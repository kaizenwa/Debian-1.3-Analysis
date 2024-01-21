/* xpm2pixmap.c - xtron v1.1 routine to convert from xpm to pixmap
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

#include "xpm2pixmap.h"

extern Window main_window;
extern Display *display;
extern int screen;

/* ReadXPM is copyright (c) 1994 Alex Brown All Rights Reserved. Used by permission. */
Pixmap ReadXPM(char *filename)
{
  Window root;
  Pixmap rootXpm;
  char real_filename[MAX_PATH_LEN];
  XWindowAttributes root_attr;
  XpmAttributes xpm_attributes;
  int val;
  char resfilename[MAX_PATH_LEN];

  PixmapsDIR(resfilename);

  strcpy(real_filename, resfilename);
  strcat(real_filename, filename);

  root = RootWindow(display, screen);
  XGetWindowAttributes(display,root,&root_attr); 
  xpm_attributes.colormap = root_attr.colormap;
  xpm_attributes.valuemask = XpmSize | XpmReturnPixels|XpmColormap;

  if((val = XpmReadFileToPixmap(display, root, real_filename,
                                &rootXpm, NULL,
			        &xpm_attributes))!= XpmSuccess) {
    if(val == XpmOpenFailed)	
      fprintf(stderr, "Couldn't open pixmap file %s\n", real_filename);
    else if(val == XpmColorFailed)
      fprintf(stderr, "Couldn't allocated required colors\n");
    else if(val == XpmFileInvalid)
      fprintf(stderr, "Invalid Format for an Xpm File\n");
    else if(val == XpmColorError)
      fprintf(stderr, "Invalid Color specified in Xpm FIle\n");
    else if(val == XpmNoMemory)
      fprintf(stderr, "Insufficient Memory\n");
    return -1;
  }
  return(rootXpm);
}
