/*
   maped.cc

   Utility to edit libgtools bitmaps.
   
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

#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <vga.h>
#include <vgamouse.h>
#include <string.h>
#include <gtools/gtools.h>

#ifndef DEPEND		/* This next file isn't created until compile time */
#include "maped_maps.c"
#endif

char *gb_top_path = ".:/usr/games/lib/luxman";
char *gb_font_subdir = "";
char *gb_image_subdir = "";

char *Filename;
Bitmap *bitmap;
Mouse *mouse;

#define CMD_SAVE		0
#define CMD_QUIT		1
#define CMD_LEFT		2
#define CMD_RIGHT		3
#define CMD_UP			4
#define CMD_DOWN		5
#define CMD_UP_DOWN		6
#define CMD_LEFT_RIGHT	7

int Got_Quit = 0;

ColorButton *clr_btn[17];
CommandButton *cmd_btn[8];

BitmapGrid *grid;

#include <stdarg.h>

void do_fatal_error( char *file, int line, char *fmt, ... )
{
  va_list ap;

  va_start( ap, fmt );
  vprintf( fmt, ap );
  exit(1);
}

#define fatal( fmt, arg... ) do_fatal_error( 0, 0, fmt, ## arg )

void set_color_cb( void *obj, int c )
{
  grid->set_draw_color( c );
}

void cmd_callback( void *obj, int c )
{
  switch( c )
	{
	case CMD_UP_DOWN:
	  grid->flip_ud();
	  return;
	  
	case CMD_LEFT_RIGHT:
	  grid->flip_lr();
	  return;
	  
	case CMD_UP:
	  grid->shift_up();
	  return;

	case CMD_DOWN:
	  grid->shift_down();
	  return;

	case CMD_RIGHT:
	  grid->shift_right();
	  return;

	case CMD_LEFT:
	  grid->shift_left();
	  return;

	case CMD_SAVE:
	  bitmap->write( Filename );
	  return;

	case CMD_QUIT:
	  Got_Quit = 1;
	  return;
	}
}

void init_btns( int x1, int y1, int x2, int y2 )
{
  int cx, cy;
  int i;
  
  cx = (x2-x1+1) / 8;
  cy = (y2-y1+1) / 2;

  for( i=0; i<8; ++i )
	{
	  clr_btn[i] = new ColorButton( ExtRect( x1 + i*cx, y1, cx, cy ),
								   set_color_cb, i );
	  clr_btn[i+8] = new ColorButton( ExtRect( x1 + i*cx, y1 + cy, cx, cy ),
									 set_color_cb, i+8 );
	}

  clr_btn[16] = new ColorButton( ExtRect( 318-cx, y1-cy-2, cx, cy ),
								set_color_cb, 255 );

  cx = 60;
  cy = 20;

  cmd_btn[CMD_SAVE] = new CommandButton( ExtRect( 318-cx, y1-3*cy-2,
												 cx, cy ),
										"Save", new Font( "test.font" ),
										cmd_callback, CMD_SAVE );
												 
  cmd_btn[CMD_QUIT] = new CommandButton( ExtRect( 318-cx, y1-2*cy-2,
												 cx, cy ),
										"Quit", new Font( "test.font" ),
										cmd_callback, CMD_QUIT );
  cmd_btn[CMD_RIGHT] = new CommandButton( 318-cx, y1-4*cy-2,
										new Bitmap( Bitmap_rarrow_width,
												   Bitmap_rarrow_height,
												   Bitmap_rarrow_data ),
										 cmd_callback, CMD_RIGHT );
  cmd_btn[CMD_LEFT] = new CommandButton( 318-cx, y1-5*cy-2,
										new Bitmap( Bitmap_larrow_width,
												   Bitmap_larrow_height,
												   Bitmap_larrow_data ),
										cmd_callback, CMD_LEFT );
  cmd_btn[CMD_UP] = new CommandButton( 318-cx/2+2, y1-5*cy-2,
										new Bitmap( Bitmap_uarrow_width,
												   Bitmap_uarrow_height,
												   Bitmap_uarrow_data ),
									  cmd_callback, CMD_UP );
  cmd_btn[CMD_DOWN] = new CommandButton( 318-cx/2+2, y1-4*cy-2,
										new Bitmap( Bitmap_darrow_width,
												   Bitmap_darrow_height,
												   Bitmap_darrow_data ),
										cmd_callback, CMD_DOWN );
  cmd_btn[CMD_UP_DOWN] = new CommandButton( 318-cx, y1-6*cy-2,
										new Bitmap( Bitmap_ud_arrow_width,
												   Bitmap_ud_arrow_height,
												   Bitmap_ud_arrow_data ),
										   cmd_callback, CMD_UP_DOWN );
  cmd_btn[CMD_LEFT_RIGHT] = new CommandButton( 318-cx/2, y1-6*cy-2,
										new Bitmap( Bitmap_lr_arrow_width,
												   Bitmap_lr_arrow_height,
												   Bitmap_lr_arrow_data ),
											  cmd_callback, CMD_LEFT_RIGHT );
}

/*
 * Usage:
 *
 * 	Editing existing bitmap "file.bmap":
 * 		t file.bmap
 *
 * 	Creating new bitmap:
 * 		t file.bmap w h
 *
 * 	Where "w" and "h" are numeric values for width and height.
 */
main( int argc, char *argv[] )
{
  int w,h,i,x,y;
  Font *font;
  char buf[200];

  vga_init();
  
  if ( argc < 2 )
	{
	  printf("Usage: maped filename\n");
	  return -1;
	}

  Filename = strdup( argv[1] );
  
  if ( access( Filename, 0 ) == 0 )
	bitmap = new Bitmap( Filename );
  else
	{
	  if ( argc < 4 )
		{
		  printf("Usage: maped filename width height [fill]\n");
		  return -1;
		}

	  w = atoi( argv[2] );
	  h = atoi( argv[3] );

	  if ( argc == 5 )
		{
		  i = atoi( argv[4] );
		  bitmap = new Bitmap( w, h, i );
		}
	  else
		bitmap = new Bitmap( w,h,0xff );
	}

  for( i=0; i<bitmap->h; ++i )
	bitmap->optimize[i] = 0;
  
  gr_init( 0 );
  vga_setpalette( 255, 31,15,15 );

// The following line causes a seg fault  
//  mouse = new Mouse;
// (Oh, but only when the bitmap being loaded is smaller
// than 6x6, of course...)

  grid = new BitmapGrid( Rect( 0,0,255,150 ), bitmap );

  // .. must move it to here... lovely
  mouse = new Mouse;

  init_btns( 0,160,300,190 );
  
  font = new Font( "small.font" );

  sprintf( buf, "Width: %d, Height: %d", bitmap->w, bitmap->h );
  gr_textxy( buf, 0, 160-gr_texth(buf,font)-1, font );

  gr_update();
  
  while( !Got_Quit )
	{
	  mouse->update();
	  if ( mouse->btn() != 0 )
		{
		  mouse->hide();

		  x = mouse->x();
		  y = mouse->y();
		  
		  grid->handle_mouse( x,y );
		  for( i=0; i<17; ++i )
			clr_btn[i]->handle_mouse(x,y);

		  for( i=0; i<8; ++i )
			cmd_btn[i]->handle_mouse(x,y);

		  gr_fillbox( 318-bitmap->w, 1, 318, 1+bitmap->h, BLACK );
		  put_bitmap_t( 318-bitmap->w, 1, bitmap );
		  gr_box( 317-bitmap->w, 0, 318, bitmap->h+1, WHITE );

		  gr_update();
		  
		  mouse->show();

		  while( mouse->btn() )
			mouse->update();
		}
	}

  delete mouse;
  gr_close();
}
  
  
