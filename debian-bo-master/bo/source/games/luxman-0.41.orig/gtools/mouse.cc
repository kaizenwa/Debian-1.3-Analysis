/*
   mouse.cc

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

#include <vga.h>
#include <vgamouse.h>
#include <gtools/mouse.h>
#include <gtools/gr.h>

/* Width and height of cursor bitmap */
#define CRS_W	8
#define CRS_H	8

/* X and Y offsets to cursor "hotspot" */
#define HSO_X	3
#define HSO_Y	3

static unsigned char cursor_data[] = {
  0xff, 0xff, 0xff, 0x0f, 0x0f, 0xff, 0xff, 0xff, 
  0xff, 0xff, 0xff, 0x0f, 0x0f, 0xff, 0xff, 0xff, 
  0xff, 0xff, 0xff, 0x0f, 0x0f, 0xff, 0xff, 0xff, 
  0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 
  0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 
  0xff, 0xff, 0xff, 0x0f, 0x0f, 0xff, 0xff, 0xff, 
  0xff, 0xff, 0xff, 0x0f, 0x0f, 0xff, 0xff, 0xff, 
  0xff, 0xff, 0xff, 0x0f, 0x0f, 0xff, 0xff, 0xff };

  
Mouse::Mouse()
{
  if ( mouse_init( "/dev/mouse", vga_getmousetype(), 150 ) != 0 )
	{
	  valid = 0;
	  return;
	}

  valid = 1;
  
  mouse_setxrange( 0,319-CRS_W );
  mouse_setyrange( 0,199-CRS_H );
  mouse_setwrap( MOUSE_NOWRAP );
  
  save = new Bitmap( CRS_W, CRS_H );
  cursor = new Bitmap( CRS_W, CRS_H, cursor_data );
  
  last_x = last_y = -1;
  drawn = 0;
}

Mouse::~Mouse()
{
  if ( valid )
	mouse_close();
  
  delete save;
  delete cursor;
}

int Mouse::ok()
{
  return valid;
}

int Mouse::hide()
{
  if ( !valid )
	return -1;

  erase();

  return 0;
}

int Mouse::show()
{
  if ( !valid )
	return -1;
  
  last_x = last_y = -1;
  return update();
}

int Mouse::erase()
{
  if ( !valid )
	return -1;
  
  if ( drawn )
	{
	  put_bitmap( last_x, last_y, save );
	  gr_update();
	  drawn = 0;
	}

  return 0;
}

int Mouse::update()
{
  int x, y;

  if ( !valid )
	return -1;
  
  mouse_update();
  x = mouse_getx();
  y = mouse_gety();

  if ( x != last_x || y != last_y )
	{
	  /* Erase mouse cursor */
	  erase();
	  
	  /* Save area we are about to overwrite */
	  get_bitmap( save, x, y );
	  
	  /* Draw new cursor */
	  put_bitmap_xor_t( x, y, cursor );
	  gr_update();
	  
	  drawn = 1;

	  last_x = x;
	  last_y = y;
	}

  return 0;
}

int Mouse::x()
{
  if ( !valid )
	return 0;
  
  return mouse_getx()+HSO_X;
}

int Mouse::y()
{
  if ( !valid )
	return 0;
  return mouse_gety()+HSO_Y;
}

int Mouse::btn()
{
  if ( !valid )
	return 0;
  
  return mouse_getbutton();
}
