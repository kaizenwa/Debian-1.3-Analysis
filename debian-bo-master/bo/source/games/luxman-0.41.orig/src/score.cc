/*
   score.cc

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

#include "score.h"
#include "error.h"

Score::Score( int X, int Y, Font *FONT, int Score )
{
  char buf[3];
  int i;
  int mw, mh;

  x = X;
  y = Y;
  
  font = FONT;
  score = Score;

  mw = 0;
  mh = 0;
  buf[1] = 0;
  
  for( i=0; i<9; ++i )
	{
	  buf[0] = '0' + i;

	  if ( gr_textw( buf, font ) > mw )
		mw = gr_textw( buf, font );
	  
	  if ( gr_texth( buf, font ) > mh )
		mh = gr_texth( buf, font );
	}

  w = 6*mw;
  h = mh + 2;

  /* Just calculate this once */
  w_1up = gr_textw( "1UP:", font );
}

Score::~Score()
{}

void Score::add( int val )
{
  score += val;
  refresh();
}

void Score::reset()
{
  score = 0;
  refresh();
}

int Score::get()
{
  return score;
}

void Score::refresh()
{
  char buf[10];

  sprintf( buf, "%d", score );
  gr_fillbox( x, y, x+w-1, y+h-1, BLACK );
  gr_textxy( buf, x, y+1, font );

  gr_textxy( "1UP:", x-w_1up, y+1, font );  
}
  
