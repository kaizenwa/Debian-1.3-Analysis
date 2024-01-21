/*
   banner.cc

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

#include <unistd.h>
#include "banner.h"
#include "error.h"
#include "globals.h"
#include <rawkey/rawkey.h>
#include "run.h"
#include <signal.h>

#define BORDER_W	5
#define BORDER_H	5

Bitmap *display_banner( char *line1, char *line2, Font *font )
{
  int w, w1, w2;
  int h, h1, h2;
  Bitmap *save;
  
  w1 = gr_textw( line1, font );
  w2 = gr_textw( line2, font );

  w = (w1>w2) ? w1 : w2;
  w = w + gr_textw( "XXXXXX", font ) + BORDER_W * 2;

  h1 = gr_texth( line1,font );
  h2 = gr_texth( line2,font );
  
  h = (h1+h2) * 2 + BORDER_H*2;

  save = new Bitmap( w, h );

  get_bitmap( save, 320/2 - w/2, 200/2 - h/2 );
  
  gr_frame( 320/2 - w/2, 200/2 - h/2, 320/2 + w/2 - 1, 200/2 + h/2 - 1,
		   BORDER_W, BORDER_H, LIGHTGRAY, WHITE, DARKGRAY, LIGHTGRAY );

  font->subst( BLACK, WHITE );
  gr_textxy( line1, 1 + 320/2 - w1/2,
			1 + 200/2 - h/2 + h1/2 + BORDER_H, font );
  gr_textxy( line2, 1 + 320/2 - w2/2,
			1 + 200/2 - h/2 + h1*2 + h2/2 + BORDER_H, font );

  font->subst( WHITE, BLACK );
  gr_textxy( line1, 320/2 - w1/2, 200/2 - h/2 + h1/2 + BORDER_H, font );
  gr_textxy( line2, 320/2 - w2/2, 200/2 - h/2 + h1*2 + h2/2 + BORDER_H, font );

  gr_update();

  return save;
}

