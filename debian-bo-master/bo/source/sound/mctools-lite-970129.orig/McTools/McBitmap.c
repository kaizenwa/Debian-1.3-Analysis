/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; see the file COPYING.LIB.  If
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
*/



#include "McApp.h"
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/xpm.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/mman.h>


#include "McBitmap.h"

McBitmap *McCreateIndexedBitmap(McWindow *mcw, int x, int y,
				char *bits, int w, int h, int wpi) {
  McBitmap *bitmap = (McBitmap *) calloc(sizeof(McBitmap), 1);
  bitmap->x      = x;
  bitmap->y      = y;
  bitmap->width  = w;
  bitmap->height = h;
  bitmap->wpi    = wpi;
  bitmap->pixmap = XCreateBitmapFromData(mcw->app->display, mcw->clientwin,
					 bits, w, h);
  bitmap->depth  = 1;
  bitmap->next   = NULL;
  return bitmap;
}

McBitmap *McCreateIndexedPixmap(McWindow *mcw, int x, int y,
				char *bits, int wpi) {
  McBitmap *bitmap = (McBitmap *) calloc(sizeof(McBitmap), 1);
  bitmap->x      = x;
  bitmap->y      = y;
  bitmap->bits   = bits;
  bitmap->flags  = MCBM_XPM;
  bitmap->wpi    = wpi;
  McCreateBitmapFromData(mcw, bitmap);
  return bitmap;
}

McBitmap *McCreateIndexedPixmapFromFile(McWindow *mcw, int x, int y,
					char *path, int wpi) {
  McBitmap *bitmap;
  XpmAttributes attr;
  XImage *image=NULL, *shape=NULL;

  attr.valuemask=XpmExactColors | XpmCloseness;
  attr.exactColors = 0;
  attr.closeness = 65535;
  if (XpmReadFileToImage(mcw->app->display, path, &image, &shape, &attr))
    return NULL;

  bitmap = (McBitmap *) calloc(sizeof(McBitmap), 1);
  bitmap->x      = x;
  bitmap->y      = y;
  bitmap->flags  = MCBM_XPM;
  bitmap->wpi    = wpi;

  if (image) {
    bitmap->pixmap=McCreatePixmapFromImage(mcw, image);
    bitmap->width=image->width;
    bitmap->height=image->height;
    bitmap->depth=image->depth;
    XDestroyImage(image);
  }

  if (shape) {
    bitmap->shape=McCreatePixmapFromImage(mcw, shape);
    XDestroyImage(shape);
  }

  return bitmap;
}

McBitmap *McCreateBitmapFromData(McWindow *mcw, McBitmap *bitmap) {
  if (bitmap->flags&MCBM_XPM) {
    XpmAttributes attr;
    XImage *image=NULL, *shape=NULL;

    attr.valuemask=XpmExactColors | XpmCloseness;
    attr.exactColors = 0;
    attr.closeness = 65535;

    XpmCreateImageFromData(mcw->app->display, (char **)(bitmap->bits),
			   &image, &shape, &attr);

    if (image) {
      bitmap->pixmap=McCreatePixmapFromImage(mcw, image);
      bitmap->width=image->width;
      bitmap->height=image->height;
      bitmap->depth=image->depth;
      XDestroyImage(image);
    }

    if (shape) {
      bitmap->shape=McCreatePixmapFromImage(mcw, shape);
      XDestroyImage(shape);
    }
  } else {
    bitmap->pixmap = XCreateBitmapFromData(mcw->app->display,
					   mcw->clientwin,
					   (char *)(bitmap->bits), 
					   bitmap->width, bitmap->height);
  }

  return bitmap;
}

void McFreeBitmap(McApp *app, McBitmap *bitmap) {
  if (bitmap->pixmap)
    XFreePixmap(app->display, bitmap->pixmap);
  if ((!(bitmap->flags & MCBM_STATICBITS)) && (bitmap->bits))
    free(bitmap->bits);
  if (!(bitmap->flags & MCBM_STATIC))
    free(bitmap);
}

void McPutBitmap(McWindow *mcw, Window win,
		 const McBitmap *bitmap, GC gc, int x, int y) {
  if (!win) win=mcw->clientwin;
  while (bitmap) {
    if (bitmap->depth == 1) {
      if (bitmap->pixmap)
	XCopyPlane(mcw->app->display, bitmap->pixmap, win, gc, 0, 0,
		   bitmap->width, bitmap->height, bitmap->x+x, bitmap->y+y, 1);
    } else {
      if (bitmap->shape) {
	XSetClipMask(mcw->app->display, gc, bitmap->shape);
	XSetClipOrigin(mcw->app->display, gc, bitmap->x+x, bitmap->y+y);
      }
      if (bitmap->pixmap)
	XCopyArea(mcw->app->display, bitmap->pixmap, win, gc, 0, 0,
		  bitmap->width, bitmap->height, bitmap->x+x, bitmap->y+y);
      if (bitmap->shape)
	XSetClipMask(mcw->app->display, gc, None);
    }
    bitmap=bitmap->next;
  }
}

/*
 * A bitmap might be divided into single images, each having the
 * height of the complete bitmap and the width as specified in
 * McBitmap.wpi
 */
void McPutIndexedBitmap(McWindow *mcw, Window win,
			const McBitmap *bitmap,
			GC gc, int x, int y, int indx) {
  if (!win) win=mcw->clientwin;
  while (bitmap) {
    int ofs=bitmap->wpi*indx;
    if (bitmap->depth == 1) {
      if (bitmap->pixmap)
	XCopyPlane(mcw->app->display, bitmap->pixmap, win, gc,
		   ofs, 0,
		   bitmap->wpi, bitmap->height,
		   bitmap->x+x, bitmap->y+y, 1);
    } else {
      if (bitmap->shape) {
	XSetClipMask(mcw->app->display, gc, bitmap->shape);
	XSetClipOrigin(mcw->app->display, gc,
		       bitmap->x+x-ofs, bitmap->y+y);
      }
      if (bitmap->pixmap)
	XCopyArea(mcw->app->display, bitmap->pixmap, win, gc,
		  ofs, 0,
		  bitmap->wpi, bitmap->height, bitmap->x+x, bitmap->y+y);
      if (bitmap->shape)
	XSetClipMask(mcw->app->display, gc, None);
    }
    bitmap=bitmap->next;
  }
}

/*****************************************************************************/

Pixmap McCreatePixmapFromImage(McWindow *mcw, XImage *ximage) {
  Pixmap pixmap;
  GC gc;
  Display *display=mcw->app->display;

  pixmap = XCreatePixmap(display, mcw->clientwin, ximage->width,
			 ximage->height, ximage->depth);
  gc = XCreateGC(display, pixmap, 0, NULL);

  XPutImage(display, pixmap, gc, ximage, 0, 0, 0, 0,
	    ximage->width, ximage->height);
  XFreeGC(display, gc);

  return pixmap;
}
