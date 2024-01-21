/*
   bitmap.cc

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

#include <assert.h>
#include <malloc.h>
#include <string.h>
#include <stdlib.h>
#include <vga.h>

#include <gtools/bitmap.h>
#include <gtools/util.h>
#include <gtools/gr.h>
#include <gtools/fmalloc.h>

extern void do_fatal_error( char *file, int line, char *fmt, ... );

#define fatal( fmt, arg... ) do_fatal_error( __FILE__, __LINE__, fmt, ## arg )

/* Where to search for bitmaps */
extern char *gb_top_path;
extern char *gb_image_subdir;

#define ERROR_NOMEM() fatal( "Out of memory (%s,%d)\n", __FILE__, __LINE__ )

void Bitmap::init( int W, int H )
{
  int i;
  
  w = W;
  h = H;

  /*
   * We sometimes use 0x0 maps temporarily --
   * Watch for this and alloc 1 byte instead of
   * 0 since it may be bad (?) on some systems to
   * alloc 0 bytes.
   */
  if ( !W || !H )
	map = (unsigned char*)Malloc( 1 );
  else
	map = (unsigned char *)Malloc( W*H );
  
  if ( map == NULL )
	ERROR_NOMEM();

  /* Alloc and calculate row offsets */
  if ( h )
	rofs = (int*)Malloc( h*sizeof( int ) );
  else
	rofs = (int*)Malloc( 1 );

  if ( rofs == NULL )
	ERROR_NOMEM();

  for( i=0; i<h; ++i )
	rofs[i] = i*w;

  init_opt_info();
}

void Bitmap::optimize_map()
{
  int i;

  for( i=0; i<h; ++i )
	optimize_row( i );
}

/*
 * Determines if row `row' can be optimized and sets
 * optimize[row], row_start[row] and row[len] row.
 */
void Bitmap::optimize_row( int row )
{
  unsigned char *m;
  int i;
  int len, start;
  
  m = map + rofs[row];
  i = w;

  /* Skip leading transparency */
  for( ; *m == 0xff && i > 0; --i, ++m );
  
  if ( !i )		/* Entire row transparent */
	{		
	  optimize[row] = 1;
	  row_start[row] = 0;
	  row_len[row] = 0;
	  return;
	}

  start = w - i;
  len = 0;

  /* Skip opaque section */
  for( ; *m != 0xff && i > 0; --i, ++m, ++len );

  if ( !i )		/* 1 trans section, 1 opaque section */
	{
	  optimize[row] = 1;
	  row_start[row] = start;
	  row_len[row] = len;
	  return;
	}

  /* Determine if there is another opaque section */
  for( ; *m == 0xff && i > 0; --i, ++m );

  if ( i )		/* There is another opaque section -- can't optimize */
	{
	  optimize[row] = 0;
	  row_start[row] = 0;
	  row_len[row] = 0;
	  return;
	}
  else		/* No more opaque sections -- Can optimize */
	{
	  optimize[row] = 1;
	  row_start[row] = start;
	  row_len[row] = len;
	  return;
	}
}  

void Bitmap::init_opt_info()
{
  if ( !h )
	{
	  optimize = (int*)Malloc( 1 );
	  row_start = (int*)Malloc( 1 );
	  row_len = (int*)Malloc( 1 );
	}
  else
	{
	  optimize = (int*)Malloc( h*sizeof(int) );
	  row_start = (int*)Malloc( h*sizeof(int) );
	  row_len = (int*)Malloc( h*sizeof(int) );
	}

  if ( optimize == NULL || row_start == NULL || row_len == NULL )
	ERROR_NOMEM();
}

Bitmap::Bitmap( int W, int H )
{
  int i;
  
  init( W,H );

  for( i=0; i<h; ++i )
	optimize[i] = 0;
}

Bitmap::Bitmap( int W, int H, unsigned char *img )
{
  init( W,H );
  memcpy( map, img, W*H );
  optimize_map();
}

Bitmap::Bitmap( Bitmap *bmp )
{
  init( bmp->w, bmp->h );
  memcpy( map, bmp->map, w*h );
  optimize_map();
}

Bitmap::Bitmap( int W, int H, int c )
{
  init( W,H );
  memset( map, c, W*H );
  optimize_map();
}

Bitmap::Bitmap( char *filename, char *prefix )
{
  char *newname, *s, *pfx;
  zFile *zfp;

  if ( prefix == NULL )
	pfx = gb_image_subdir;
  else
	pfx = prefix;
  
  s = (char*)alloca( strlen( filename ) + strlen( pfx ) + 1 );
  if ( !s )
	fatal( "Out of memory\n" );

  strcpy( s, pfx );
  strcat( s, filename );
  
  if ( find_file( &newname, s, gb_top_path, "gz" ) != 1 )
	fatal( "Can't find bitmap file `%s'.\n"
		  "Search path `%s'\n", s, gb_top_path );
  
  zfp = zfopen( newname, "r" );

  if ( !zfp )
	fatal( "Can't open bitmap file `%s'", newname );

  free( newname );
  
  /* Skip comment */
  while( fgetc( zfp->fp ) != 0 );

  /* Read width and height from file */
  fread( &w, sizeof(int), 1, zfp->fp );
  fread( &h, sizeof(int), 1, zfp->fp );

  init( w, h );

  /* Read bitmap data */
  fread( map, sizeof(char), w*h, zfp->fp );
  zfclose( zfp );
  optimize_map();
}

#ifdef DEBUG
int Bitmap::verify_magic()
{
  if ( Mcheck( map ) != 0 || Mcheck( rofs ) != 0 ||
	  Mcheck( optimize ) || Mcheck( row_start ) != 0 ||
	  Mcheck( row_len ) != 0 )
	return -1;
  else
	return 0;
}
#endif

Bitmap::~Bitmap()
{
  Free( map );
  Free( rofs );
  Free( optimize );
  Free( row_start );
  Free( row_len );
}

void Bitmap::subst( unsigned char nc, unsigned char oc )
{
  int n, i;
  
  n = w*h;

  for( i=0; i<n; ++i )
	{
	  if ( map[i] == oc )
		map[i] = nc;
	}
}

void Bitmap::subst2( unsigned char nc1, unsigned char oc1,
					unsigned char nc2, unsigned char oc2 )
{
  int n, i;
  
  n = w*h;

  for( i=0; i<n; ++i )
	{
	  if ( map[i] == oc1 )
		map[i] = nc1;
	  else if ( map[i] == oc2 )
		map[i] = nc2;
	}
}

/*
 * Bitmap file format:
 *
 * 	text comment	(variable - terminated with 0x00)
 * 	width	(int)
 * 	height	(int)
 * 	data	(width*height bytes)
 */

int Bitmap::write( char *filename )
{
  FILE *fp;

  fp = fopen( filename, "w" );
  if ( !fp )
	return -1;

  fprintf( fp, "Bitmap file\nWidth: %d, Height: %d\n", w, h );
  fputc( 0x00, fp );
  
  fwrite( &w, sizeof(int), 1, fp );
  fwrite( &h, sizeof(int), 1, fp );
  fwrite( map, sizeof(char), w*h, fp );

  fclose( fp );
  
  return 0;
}

void put_bitmap( int x, int y, Bitmap *map )
{
  unsigned char *mem, *img;
  int i;
  int w, h;

  img = map->map;
  w = map->w;
  h = map->h;
  
  /* For each row... */
  for( i=0; i<h; ++i )
	{
	  /* Point to first column */
	  mem = graph_buffer + screen_row_ofs[y+i] + x;

	  memcpy( mem, img, w );
	  img += w;
	}
}

void put_bitmap_t( int x, int y, Bitmap *map )
{
  int i, j;
  unsigned char *d, *s, *mem;
  int *opt;

  opt = map->optimize;
  s = map->map;
  mem = graph_buffer + screen_row_ofs[y];
  
  /* For each row... */
  for( i=0; i<map->h; ++i, mem += 320 )
	{
	  /* Can we optimize this row? */
	  if ( opt[i] )
		{
		  memcpy( mem + map->row_start[i] + x,
				 s + map->row_start[i], map->row_len[i] );
		  s += map->w;
		}
	  else	/* no */
		{
		  d = mem + x;
		  
		  for( j=0; j<map->w; ++j, ++d, ++s )
			{
			  if ( *s != 0xff )
				*d = *s;
			}
		}
	}			  
}

void put_bitmap_xor_t( int x, int y, Bitmap *map )
{
  unsigned char *mem, *img;
  int i, j;
  int w, h;

  w = map->w;
  h = map->h;
  img = map->map;
  
  /* For each row... */
  for( i=0; i<h; ++i )
	{
	  /* Point to first column */
	  mem = graph_buffer + screen_row_ofs[y+i] + x;

	  /* For each col... */
	  for( j=0; j<w; ++j, ++img )
		{
		  if ( *img != 0xff )
			mem[j] ^= *img;
		}
	}
}

void get_bitmap( Bitmap *map, int x, int y )
{
  unsigned char *mem, *img;
  int i;
  int w, h;
  
  img = map->map;
  w = map->w;
  h = map->h;
  
  /* For each row... */
  for( i=0; i<h; ++i )
	{
	  /* Point to first column of src */
	  mem = graph_buffer + screen_row_ofs[y+i] + x;
	  
	  memcpy( img, mem, w );
	  img += w;
	}
}

#ifndef min
#define min( a,b ) ((a<b)?a:b)
#endif

void Bitmap::copy( Bitmap *src, unsigned char pad )
{
  int r, c;
  int i, j;
  unsigned char *s, *d;
  
  /* How many rows can we copy? */
  r = min( h, src->h );

  /* How many columns can we copy? */
  c = min( w, src->w );

  /* For each copyable row... */
  for( i=0; i<r; ++i )
	{
	  s = src->map + src->rofs[i];
	  d = map + rofs[i];
	  
	  /* For each copyable column... */
	  for( j=0; j<c; ++j )
		*(d++) = *(s++);

	  /* Pad remainder with `pad' */
	  for( ; j<w; ++j )
		*(d++) = pad;
	}

  /* Pad remainder with 0xff */
  for( ; i<h; ++i )
	memset( map + rofs[i], pad, w );
}
