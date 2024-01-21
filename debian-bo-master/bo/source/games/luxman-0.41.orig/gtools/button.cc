/*
   button.cc

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

#include <gtools/button.h>
#include <gtools/gr.h>

CommandButton::CommandButton( ExtRect &r, void (*cb)(void*,int), int Cmd,
							 void *Obj )
{
  callback = cb;
  rect = r;
  obj = Obj;
  cmd = Cmd;
  map = NULL;
  font = NULL;
  
  gr_box( rect.a.x, rect.a.y, rect.b.x, rect.b.y, WHITE );
}

CommandButton::CommandButton( int x, int y, Bitmap *Map,
							 void (*cb)(void*,int), int Cmd, void *Obj )
{
  callback = cb;

  map = Map;
  font = NULL;
  
  rect.a.x = x;
  rect.a.y = y;
  rect.b.x = x + map->w + 1;
  rect.b.y = y + map->h + 1;
  
  obj = Obj;
  cmd = Cmd;

  put_bitmap_t( rect.a.x + 1, rect.a.y + 1, map );
  gr_box( rect.a.x, rect.a.y, rect.b.x, rect.b.y, WHITE );
}

CommandButton::CommandButton( ExtRect &r, char *str, Font *FONT, 
							 void (*cb)(void*,int), int Cmd, void *Obj )
{
  int w, h;
  
  callback = cb;
  cmd = Cmd;
  obj = Obj;
  rect = r;
  map = NULL;
  font = FONT;

  w = gr_textw( str, font );
  h = gr_texth( str, font );

  gr_textxy( str, rect.a.x + rect.w()/2 - w/2,
			rect.a.y + rect.h()/2 - h/2, font );

  gr_box( rect.a.x, rect.a.y, rect.b.x, rect.b.y, WHITE );
}

CommandButton::~CommandButton()
{
  if ( map )
	delete map;
  if ( font )
	delete font;
}

int CommandButton::handle_mouse( int x, int y )
{
  if ( x >= rect.a.x && x <= rect.b.x &&
	  y >= rect.a.y && y <= rect.b.y && callback )
	(callback)(obj,cmd);
  return 0;
}

ColorButton::ColorButton( ExtRect &r, void (*cb)(void*,int), int fill_color,
						 void *Obj )
{
  callback = cb;
  rect = r;
  clr = fill_color;
  obj = Obj;
  
  gr_fillbox( rect.a.x,rect.a.y,rect.b.x,rect.b.y,clr );
  gr_box( rect.a.x,rect.a.y,rect.b.x,rect.b.y,WHITE );
}

ColorButton::~ColorButton()
{}

int ColorButton::handle_mouse( int x, int y )
{
  if ( x >= rect.a.x && x <= rect.b.x &&
	  y >= rect.a.y && y <= rect.b.y && callback )
	(callback)( obj,clr );

  return 0;
}
