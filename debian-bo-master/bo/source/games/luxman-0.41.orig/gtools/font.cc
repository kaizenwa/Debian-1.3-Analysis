/*
   font.cc

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

#include <string.h>
#include <stdlib.h>
#include <gtools/font.h>
#include <gtools/util.h>

extern void do_fatal_error( char *file, int line, char *fmt, ... );
#define fatal( fmt, arg... ) do_fatal_error( __FILE__, __LINE__, fmt, ## arg )

extern char *gb_top_path;
extern char *gb_font_subdir;

Font::Font()
{
  int i;

  for ( i=0; i<255; ++i )
	map[i] = NULL;
}

Font::Font( char *filename )
{
  char *newname, *s;
  zFile *zfp;
  int i;
  int w, h;

  s = (char*)alloca( strlen( filename ) + strlen( gb_font_subdir ) + 1 );
  if ( !s )
	fatal( "Out of memory\n" );

  strcpy( s, gb_font_subdir );
  strcat( s, filename );
  
  if ( find_file( &newname, s, gb_top_path, "gz" ) != 1 )
	fatal( "Error: Can't find font file `%s'\n"
		  "Search path `%s'\n", s, gb_top_path );
  
  zfp = zfopen( newname, "r" );
  if ( !zfp )
	fatal("Error: Can't open font file `%s'", newname );

  free( newname );
  
  /* Skip comment */
  while( fgetc( zfp->fp ) != 0x00 );

  /* Read all 255 bitmaps into font */
  for( i=0; i<255; ++i )
	{
	  /* Read width and height */
	  fread( &w, sizeof( int ), 1, zfp->fp );
	  fread( &h, sizeof( int ), 1, zfp->fp );

	  /* Create bitmap */
	  map[i] = new Bitmap( w, h );

	  fread( map[i]->map, sizeof( char ), w*h, zfp->fp );
	}

  zfclose( zfp );
}

Font::~Font()
{
  int i;

  for( i=0; i<255; ++i )
	{
	  if ( map[i] )
		delete map[i];
	}
}

int font_write( FILE *fp, Font *font )
{
  int i;
  
  fputc( 0x00, fp );		/* Terminate comment */

  for( i=0; i<255; ++i )
	{
	  fwrite( &(font->map[i]->w), sizeof( int ), 1, fp );
	  fwrite( &(font->map[i]->h), sizeof( int ), 1, fp );
	  fwrite( font->map[i]->map, sizeof( char ),
			 font->map[i]->w * font->map[i]->h, fp );
	}

  return 0;
}

Font *font_read( FILE *fp )
{
  int i;
  Font *font;
  int w, h;
  
  /* Skip comment */
  while( fgetc( fp ) != 0x00 );

  /* Create new font */
  font = new Font();

  /* Read all 255 bitmaps into font */
  for( i=0; i<255; ++i )
	{
	  /* Read width and height */
	  fread( &w, sizeof( int ), 1, fp );
	  fread( &h, sizeof( int ), 1, fp );

	  /* Create bitmap */
	  font->map[i] = new Bitmap( w, h );

	  fread( font->map[i]->map, sizeof( char ), w*h, fp );
	}

  return font;
}	 

void Font::subst( int nc, int oc )
{
  int i;

  for( i=0; i<255; ++i )
	map[i]->subst( nc, oc );
}

