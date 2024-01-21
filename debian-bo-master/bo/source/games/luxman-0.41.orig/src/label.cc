/*
   label.cc

   This file is part of LuxMan.
   
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

#include "label.h"

Label::Label( int tx, int ty, Font *FONT, char *text, int lifespan,
			 Bitmap *tile ) : Runnable( lifespan )
{
  int w, h;

  font = FONT;
  str = text;
  
  w = gr_textw( str, font );
  h = gr_texth( str, font );

  x = tx * tile->w + tile->w/2 - w/2;
  if ( x < 0 )
	x = 0;
  else if ( x + w > 319 )
	x = 318 - w;
  
  y = ty * tile->h + tile->h/2 - h/2;
  if ( y < 0 )
	y = 0;
  else if ( y + h > 199 )
	y = 198 - h;
  
  save = new Bitmap( w, h );
  map = new Bitmap( w, h );
  
  /* Clear area to draw string */
  get_bitmap( save, 0, 0 );
  gr_fillbox( 0,0,w-1,h-1,0xff );
  gr_textxy( str, 0, 0, font );
  get_bitmap( map, 0, 0 );
  put_bitmap( 0,0, save );
}

Label::~Label()
{
  delete save;
  delete map;
}

void Label::draw()
{
  get_bitmap( save, x, y );
  put_bitmap_t( x, y, map );
}

void Label::erase()
{
  put_bitmap( x, y, save );
}

  
