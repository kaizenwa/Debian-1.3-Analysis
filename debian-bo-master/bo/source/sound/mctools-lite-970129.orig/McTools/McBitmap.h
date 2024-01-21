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

#ifndef _McBitmap_h_
#define _McBitmap_h_

typedef struct McBitmap {
  struct McBitmap *next;
  Pixmap pixmap;
  Pixmap shape;
  void *bits;
  int width,height;
  int x,y;
  int wpi; /* Width per Image */
  unsigned short depth, flags;
} McBitmap;

#define McCreateBitmap(a,b,c,d,e,f) McCreateIndexedBitmap(a,b,c,d,e,f,-1)
extern McBitmap *McCreateIndexedBitmap(McWindow *mcw, int x, int y, char *bits,
				       int w, int h, int wpi);

#define McCreatePixmap(a,b,c,d) McCreateIndexedPixmap(a,b,c,d,-1)
extern McBitmap *McCreateIndexedPixmap(McWindow *mcw, int x, int y,
				       char *bits, int wpi);

#define McCreatePixmapFromFile(a,b,c,d) \
				McCreateIndexedPixmapFromFile(a,b,c,d,-1)
McBitmap *McCreateIndexedPixmapFromFile(McWindow *mcw, int x, int y,
					char *path, int wpi);

extern McBitmap *McCreateBitmapFromData(McWindow *mcw, McBitmap *bitmap);

extern Pixmap McCreatePixmapFromImage(McWindow *mcw, XImage *ximage);

extern void McFreeBitmap(McApp *, McBitmap *bitmap);
extern void McPutBitmap(McWindow *mcw, Window win,
			const McBitmap *bitmap, GC gc, int x, int y);
void McPutIndexedBitmap(McWindow *mcw, Window win,
			const McBitmap *bitmap, GC gc, int x, int y,
			int /*index*/);



#define MCBM_STATIC     1
#define MCBM_STATICBITS 2
#define MCBM_XPM        4

#define MCBITMAP(b,w,h,x,y) { NULL, 0, 0, b, w, h, x, y, -1, 1,\
			      MCBM_STATIC | MCBM_STATICBITS }
#define MCPIXMAP(b,x,y)     { NULL, 0, 0, b, 0, 0, x, y, -1, 0,\
			      MCBM_STATIC | MCBM_STATICBITS | MCBM_XPM }

#define MCIDXBITMAP(b,w,h,x,y,wpi) { NULL, 0, 0, b, w, h, x, y, wpi, 1,\
  				     MCBM_STATIC | MCBM_STATICBITS }
#define MCIDXPIXMAP(b,x,y,wpi)     { NULL, 0, 0, b, 0, 0, x, y, wpi, 0,\
				     MCBM_STATIC | MCBM_STATICBITS | MCBM_XPM }

#endif /* _McBitmap_h_ */

