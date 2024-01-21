/*
   mapsize.cc

   Utility to resize libgtools bitmaps.
   
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

/*
 * mapsize.cc
 *
 * Usage:
 * 	mapsize file nc nr
 *
 * Changes the size of the bitmap in file `file' to `nr' rows and
 * `nc' columns.
 *
 * Columns are added/deleted to/from the right side.
 * Rows are added/deleted to/from the bottom.
 */
#include <string.h>
#include <stdlib.h>
#include <gtools/gtools.h>
#include <stdarg.h>

char *gb_top_path = ".:/usr/games/lib/luxman";
char *gb_image_subdir = "";
char *gb_font_subdir = "";

void do_fatal_error( char *file, int line, char *fmt, ... )
{
  va_list ap;

  va_start( ap, fmt );
  vprintf( fmt, ap );
  exit(1);
}

#define fatal( fmt, arg... ) do_fatal_error( 0, 0, fmt, ## arg )

main( int argc, char *argv[] )
{
  Bitmap *map1, *map2;
  int w, h;
  
  if ( argc < 4 )
	{
	  printf("Usage: mapsize filename width height\n");
	  return -1;
	}

  map1 = new Bitmap( argv[1] );
  
  w = atoi( argv[2] );
  h = atoi( argv[3] );

  map2 = new Bitmap( w, h );
  
  map2->copy( map1, 0xff );
  
  map2->write( argv[1] );

  delete map1;
  delete map2;
}
