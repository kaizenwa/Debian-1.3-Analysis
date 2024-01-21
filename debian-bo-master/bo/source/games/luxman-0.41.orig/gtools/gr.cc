/*
   gr.cc

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

#include <stdlib.h>
#define _gr_cc_		/* So that we get the defn of screen_row_ofs */
#include <gtools/gr.h>
#include <gtools/screen.h>

extern void do_fatal_error( char *file, int line, char *fmt, ... );
#define fatal( fmt, arg... ) do_fatal_error( __FILE__, __LINE__, fmt, ## arg )

unsigned char *graph_buffer = graph_mem;
int gr_no_updates = 0;

static int gr_initted = 0;

void gr_update()
{
  if ( graph_mem != graph_buffer && gr_no_updates == 0 )
	memcpy( graph_mem, graph_buffer, 320*200 );
}
		   
int gr_init( int buffer )
{
  int i;

//  vga_init();
  
  if ( buffer )
	{
	  graph_buffer = (unsigned char*)malloc( 320*200 );
	  if ( !graph_buffer )
		{
		  printf("Error: Can't alloc graph_buffer\n");
		  exit(1);
		}
	  i = vga_setmode( G320x200x256 );
	}
  else
	{
	  i = vga_setmode( G320x200x256 );
	  graph_buffer = graph_mem;
	}

  gr_initted = 1;
  return i;
}

int gr_close()
{
  if ( !gr_initted )
	return 0;
  
  if ( graph_buffer != graph_mem )
	free( graph_buffer );

  gr_initted = 0;
  return vga_setmode( TEXT );
}

void gr_hline( int x1, int y, int x2, int c )
{
  unsigned char *m;

#ifdef DEBUG
  if ( x1 > x2 )
	fatal( "Bad arg" );
#endif
  
  m = graph_buffer + screen_row_ofs[y] + x1;  
  memset( m, c, x2-x1+1 );
}

void gr_vline( int x, int y1, int y2, int c )
{
  int i;
  unsigned char *m;
  
#ifdef DEBUG
  if ( y1 > y2 )
	fatal( "Bad arg" );
#endif
  
  for( i=y1; i<=y2; ++i )
	{
	  m = graph_buffer + screen_row_ofs[i] + x;
	  *m = c;
	}
}

void gr_box( int x1, int y1, int x2, int y2, int c )
{
  gr_hline( x1, y1, x2, c );
  gr_hline( x1, y2, x2, c );
  gr_vline( x1, y1, y2, c );
  gr_vline( x2, y1, y2, c );
}

void gr_fillbox( int x1, int y1, int x2, int y2, int c )
{
  int i;
  int w;
  unsigned char *m;

#ifdef DEBUG
  if ( x1 > x2 || y1 > y2 )
	fatal( "Bad arg" );
#endif
  
  w = x2 - x1 + 1;
  
  /* For each row... */
  for( i=y1; i<=y2; ++i )
	{
	  /* Point to 1st col */
	  m = graph_buffer + screen_row_ofs[i] + x1;
	  
	  memset( m, c, w );
	}
}

void gr_frame( int x1, int y1, int x2, int y2, int w, int h,
			  int normal, int bright, int dark, int inside )
{
  gr_fillbox( x1, y1, x2, y2, inside );		/* interior */
  
  gr_fillbox( x1, y1, x2, y1+h-1, normal );	/* top */
  gr_fillbox( x1, y1, x1+w-1, y2, normal );	/* left */
  gr_fillbox( x2-w+1, y1, x2, y2, normal );	/* right */
  gr_fillbox( x1, y2-h+1, x2, y2, normal );	/* bottom */

  /* highlights */
  gr_hline( x1, y1, x2, bright );		/* top outside */
  gr_vline( x1, y1, y2, bright );		/* left outside */
  gr_hline( x1, y2, x2, dark );			/* bottom outside */
  gr_vline( x2, y1, y2, dark );			/* right outside */

  gr_hline( x1+w-1, y1+h-1, x2-w+1, dark );		/* top inside */
  gr_vline( x1+w-1, y1+h-1, y2-h+1, dark );		/* left inside */
  gr_hline( x1+w-1, y2-h+1, x2-w+1, bright );  	/* bottom inside */
  gr_vline( x2-w+1, y1+h-1, y2-h+1, bright );	/* right inside */
}

void gr_textxy( char *str, int x, int y, Font *font )
{
  int i, len, c;
  Bitmap *map;
  
  len = strlen( str );

  for( i=0; i<len; ++i )
	{
	  c = (int)(*((unsigned char*)str++));
	  map = font->map[c];
	  put_bitmap_t( x, y, map );
	  x += map->w;
	}
}

void gr_textxy_c( char *str, int x, int y, Font *font )
{
  int w, h;

  w = gr_textw( str, font );
  h = gr_texth( str, font );
  
  gr_textxy( str, x-w/2, y-h/2, font );
}

void gr_textxy_u( char *str, int x, int y, Font *font )
{
  int w, h;

  w = gr_textw( str, font );
  h = gr_texth( str, font );
  
  gr_textxy( str, x, y, font );

  gr_hline( x, y+h+1, x+w-1, WHITE );
}

void gr_textxy_cu( char *str, int x, int y, Font *font )
{
  int w, h;

  w = gr_textw( str, font );
  h = gr_texth( str, font );
  
  gr_textxy( str, x-w/2, y-h/2, font );

  gr_hline( x-w/2, y+h/2+1, x+w/2-1, WHITE );
}

int gr_putc( char c, int x, int y, Font *font )
{
  int i;
  Bitmap *map;
  int w;
  
  i = c;
  map = font->map[i];
  w = map->w;
  put_bitmap_t( x, y, map );
  return w;
}

int gr_textw( char *str, Font *font )
{
  int i, width, len, c;
  Bitmap *map;

  width = 0;
  
  len = strlen( str );

  for( i=0; i<len; ++i )
	{
	  c = (int)(*((unsigned char*)str++));
	  map = font->map[c];
	  width += map->w;
	}

  return width;
}

int gr_texth( char *str, Font *font )
{
  int i, height, len, c;
  Bitmap *map;

  height = 0;
  
  len = strlen( str );

  for( i=0; i<len; ++i )
	{
	  c = (int)(*((unsigned char*)str++));
	  map = font->map[c];
	  if ( map->h > height )
		height = map->h;
	}

  return height;
}

