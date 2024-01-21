/*
   grx2map.cc

   Utility to convert LIBGRX font files to libgtools font files.
   
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

#include <Grx/grx.h>
#include <vga.h>
#include <gtools/bitmap.h>
#include <gtools/font.h>
#include <gtools/gr.h>
#include <stdarg.h>

char *gb_library_path = ".";

void do_fatal_error( char *file, int line, char *fmt, ... )
{
  va_list ap;

  va_start( ap, fmt );
  vprintf( fmt, ap );
  exit(1);
}

#define fatal( fmt, arg... ) do_fatal_error( 0, 0, fmt, ## arg )

Bitmap* grab_char( int c, GrFont *font )
{
  int w, h;
  GrTextOption txo;
  Bitmap *map;
  
  txo.txo_font = font;
  txo.txo_xmag = txo.txo_ymag = 1;
  txo.txo_fgcolor.v = 15;
  txo.txo_bgcolor.v = 0;
  txo.txo_direct = 0;
  txo.txo_xalign = 0;
  txo.txo_yalign = 0;
  txo.txo_chrtype = 0;
  
  w = GrCharWidth( c, &txo );
  h = GrCharHeight( c, &txo );

  map = new Bitmap( w, h );

  GrDrawChar( c, 0, 0, &txo );

  get_bitmap( map, 0, 0 );

  /* Replace 0 with 255 */
  map->subst( 255, 0 );

  GrFilledBox( 0,0,map->w, map->h, 0 );
  
  return map;
}

Font* grab_charset( char *fontname )
{
  GrFont *font;
  Font *rfont;
  int i;
  
  font = GrLoadFont( fontname );

  if ( !font )
	{
	  printf("Error: Unable to load font `%s'\n", fontname );
	  return NULL;
	}

  rfont = new Font();

  for( i=0; i<255; ++i )
	rfont->map[i] = grab_char( i, font );

  GrUnloadFont( font );
  
  return rfont;
}

main( int argc, char *argv[] )
{
  FILE *fp;
  Font *font;

  vga_init();
  
  if ( argc < 3 )
	{
	  printf("Usage: grx2map input_file output_file\n");
	  return -1;
	}
  
  GrSetMode( GR_320_200_graphics, 320, 200, 256 );

  font = grab_charset( argv[1] );

  if ( !font )
	return -1;
  
  fp = fopen( argv[2], "w" );
  font_write( fp, font );
  fclose( fp );

  delete font;

  fp = fopen( argv[2], "r" );
  font = font_read( fp );
  fclose( fp );

  font->subst( 14, 15 );
  
  gr_textxy( "Hello world!", 100,50, font );

  vga_getch();
  GrSetMode( GR_default_graphics );

  delete font;
}
