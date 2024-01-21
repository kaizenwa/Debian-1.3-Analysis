/*
   input.cc

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

#include "input.h"
#include <rawkey/rawkey.h>
#include "bitmap.h"
#include <unistd.h>

/* Scancodes */
static int sc_alpha[52];		/* lower then upper */
static int sc_numeric[10];		/* 0-9 */

static void draw_string( char *str, int x, int y, Font *font, int bg )
{
  static int last_w=0, last_h=0;

  if ( last_w )
	gr_fillbox( x, y, x+last_w-1, y+last_h-1, bg );

  last_w = gr_textw( str, font );
  last_h = gr_texth( str, font );

  gr_textxy( str, x, y, font );
}

static void insert_char( char *dest, int *num, int maxlen, int c, int sc,
				 int x, int y, Font *font, int bg )
{
  while( is_key_pressed( sc ) )
	scan_keyboard();
		  
  if ( *num < maxlen-2 )
	{
	  dest[(*num)++] = c;
	  dest[*num] = '_';
	  dest[(*num)+1] = 0;
	  draw_string( dest, x, y, font, bg );
	  gr_update();
	}  
}

int input_line( char *dest, int maxlen, int x, int y, Font *font,
			   int bg_color )
{
  int num;		/* Num chars in dest (not counting _) */
  int i;
  int done;
  int c;
  int sc_space;
  
  /* Setup scancode translations */
  for( i=0; i<26; ++i )
	{
	  sc_alpha[i] = scancode_trans( 'a' + i );
	  sc_alpha[i+26] = scancode_trans( 'A' + i );
	}
  for( i=0; i<10; ++i )
	sc_numeric[i] = scancode_trans( '0' + i );

  sc_space = scancode_trans( ' ' );
  
  num = 0;
  done = 0;
  dest[0] = '_';
  dest[1] = 0;

  draw_string( dest, x, y, font, bg_color );
  gr_update();
  
  while( !done )
	{
	  scan_keyboard();

	  if ( is_key_pressed( ENTER_KEY ) )
		{
		  done = 1;
		  while( is_key_pressed( ENTER_KEY ) )
			scan_keyboard();
		  continue;
		}

	  if ( is_key_pressed( BACKSPACE ) )
		{
		  if ( num > 0 )
			{
			  usleep( 50000 );
			  --num;
			  dest[num] = '_';
			  dest[num+1] = 0;
			  draw_string( dest, x, y, font, bg_color );
			  gr_update();
			}
		  continue;
		}

	  /* See if alphabetic key pressed */
	  for( i=0; i<26; ++i )
		{
		  if ( is_key_pressed( sc_alpha[i] ) )
			break;
		}

	  if ( i != 26 )
		{
		  if ( is_key_pressed( LEFT_SHIFT ) ||
			  is_key_pressed( RIGHT_SHIFT ) )
			  c = 'A' + i;
		  else
			c = 'a' + i;

		  insert_char( dest, &num, maxlen, c, sc_alpha[i], x, y, font,
					  bg_color );
		  continue;
		}

	  /* See if numeric key pressed */
	  for( i=0; i<10; ++i )
		{
		  if ( is_key_pressed( sc_numeric[i] ) )
			break;
		}

	  if ( i != 10 )
		{
		  c = '0' + i;

		  insert_char( dest, &num, maxlen, c, sc_numeric[i], x, y, font,
					  bg_color );
		  continue;
		}

	  /* Spacebar? */
	  if ( is_key_pressed( sc_space ) )
		{
		  insert_char( dest, &num, maxlen, ' ', sc_space, x, y, font,
					  bg_color );
		}
	}

  dest[num] = 0;
  return 0;
}
