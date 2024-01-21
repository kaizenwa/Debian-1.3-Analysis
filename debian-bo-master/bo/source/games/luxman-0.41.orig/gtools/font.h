/*
   font.h

   This file is part of libgtools.
   
   Copyright (C) 1994,1995 Frank McIngvale (frankm@nuance.com)
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifndef _font_h_
#define _font_h_

/*
 * Format of font file:
 *
 * 	text comment		Terminated with 0x00
 * 	char #0
 * 	char #1
 * 	.
 * 	.
 * 	char #255
 *
 * Each char #i is:
 * 				   
 * 	width (int)      Bitmap width
 * 	height (int)     Bitmap height
 * 	data             width*height bytes, row major
 */

#include <gtools/bitmap.h>

struct Font {
  Bitmap *map[255];		/* One bitmap for each char */

  /* This constructor inits map to NULL */
  Font();

  /* This constructor inits from a file */
  Font( char *filename );
  
  /* The destructor deletes all bitmaps */
  ~Font();

  /* Substitute color "nc" for color "oc" */
  void subst( int nc, int oc );
};

/* Note!! All maps must be non-NULL! */
int font_write( FILE *, Font * );

/* Allocs new bitmaps */
Font *font_read( FILE * );

#endif	/* _font_h_ */
