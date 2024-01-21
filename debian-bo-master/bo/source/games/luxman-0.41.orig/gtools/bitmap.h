/*
   bitmap.h

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

#ifndef _bitmap_h_
#define _bitmap_h_

#include <stdio.h>

struct Bitmap {
  int w, h;				/* Width, height in pixels */
  unsigned char *map;	/* Bitmap - row major, 1 byte per pixel */

  int *rofs;			/* Array of row offsets (into map) */

  /* Info for optimization in transparency routine */

  int *row_start;	/* Ofs to 1st non-transparent bit in row */
  int *row_len;		/* Len of non-transparent section */
  int *optimize;	/* For each row, this is 1 if the row has
					 * only 1 non-transparent section.
					 */

  void optimize_map();

#ifdef DEBUG
  int verify_magic();
#endif
  
  /*
   * All constructors alloc new memory for the bitmap.
   * It is assumed that a bitmap will remain the same size
   * throughout its lifetime.
   *
   * The first constructor zeros memory.
   * The second constructor copies the passed bitmap.
   * The third constructor fills the new bitmap with the given color.
   * The fourth constructor inits from a bitmap file.
   * The fifth constructor copies the passed bitmap.
   */
  Bitmap( int W, int H );
  Bitmap( int W, int H, unsigned char *img );
  Bitmap( int W, int H, int c );
  Bitmap( char *filename, char *prefix = NULL );
  Bitmap( Bitmap *src );
  
  /* The destructor frees the bitmap */
  virtual ~Bitmap();

  /* Substitute color "nc" for color "oc" */
  void subst( unsigned char nc, unsigned char oc );

  void subst2( unsigned char nc1, unsigned char oc1,
			  unsigned char nc2, unsigned char oc2 );

  /* "Special effects" */

  /*
   * void copy( Bitmap *src, unsigned char pad )
   *
   * This copies the map `src' into the bitmap with the following
   * constraints:
   *
   * (a) If src has FEWER rows/cols than this bitmap, extra rows/cols
   *     are added to the bottom/right of the bitmap. The extra rows
   *     are filled with the pad character `pad'.
   * (b) If src has MORE rows/cols than this bitmap, rows/cols on
   *     the bottom/right are not copied.
   * 	
   */
  void copy( Bitmap *map, unsigned char pad );
  
  /* Write map to a file */
  int write( char *filename );

 protected:
  void init( int W, int H );
  void init_opt_info();
  void optimize_row( int row );
};

/*
 * void put_bitmap( int x, int y, Bitmap *map )
 *
 * Put bitmap to screen.
 */
void put_bitmap( int x, int y, Bitmap *map );

/*
 * void put_bitmap_t( int x, int y, Bitmap *map )
 *
 * Put bitmap to screen with transparency checking.
 * The color value 0xff is "transparent".
 */
void put_bitmap_t( int x, int y, Bitmap *map );

/*
 * void put_bitmap_xor_t( int x, int y, Bitmap *map )
 *
 * Puts bitmap to screen with XOR'ing and transparency checking.
 */
void put_bitmap_xor_t( int x, int y, Bitmap *map );

/* Uses map->w and map->h; assumes map->map allocated */
void get_bitmap( Bitmap *map, int x, int y );

#endif /* _bitmap_h_ */
