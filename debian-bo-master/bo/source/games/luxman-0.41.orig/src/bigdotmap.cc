/*
   bigdotmap.cc

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

#include "bigdotmap.h"
#include "error.h"

#define FLASH_PERIOD	15

BigDotMap::BigDotMap( Maze *maze, Bitmap *tile, int c )
{
  int i, j;
  unsigned char *m;
  BigDot *dot;

  count = 0;
  state = 1;
  drawn = 0;
  
  dotmap = new Bitmap( "bigdot.map" );
  dotmap->subst( c, YELLOW );
  
  list = new VLList();

  xpos = (int*)Malloc( maze->w * sizeof(int) );
  ypos = (int*)Malloc( maze->h * sizeof(int) );

  if ( !xpos || !ypos )
	fatal( "Out of memory" );
  
  for( i=0; i<maze->w; ++i )
	xpos[i] = i*tile->w;

  for( i=0; i<maze->h; ++i )
	ypos[i] = i*tile->h;

  /* Find all bigdots */
  m = maze->map;
  
  for( i=0; i<maze->h; ++i )
	{
	  for( j=0; j<maze->w; ++j )
		{
		  if ( *m == MAZE_BIGDOT )
			{
			  dot = new BigDot;
			  dot->tx = j;
			  dot->ty = i;
			  dot->save = new Bitmap( tile->w, tile->h );

			  list->add_tail( dot );
			}
		  ++m;
		}
	}
}

BigDotMap::~BigDotMap()
{
  BigDot *dot;

#ifdef DEBUG
  printf("In ~BigDotMap\n");
#endif
  
  while( list->num_items() > 0 )
	{
	  dot = (BigDot*)(list->pop_head());
	  delete dot->save;
	  delete dot;
	}

  delete list;
  delete dotmap;
  
  if ( Free( xpos ) != 0 || Free( ypos ) != 0 )
	fatal( "Mem clobbered" );
  
#ifdef DEBUG
  printf("End ~BigDotMap\n");
#endif  
}

int BigDotMap::draw_dot( BigDot *dot )
{
  get_bitmap( dot->save, xpos[dot->tx], ypos[dot->ty] );
  put_bitmap_t( xpos[dot->tx], ypos[dot->ty], dotmap );
  return 0;
}

int BigDotMap::erase_dot( BigDot *dot )
{
  put_bitmap( xpos[dot->tx], ypos[dot->ty], dot->save );
  return 0;
}

int BigDotMap::num_left()
{
  return list->num_items();
}

void BigDotMap::draw()
{
  void *v;

  ++count;
  if ( count == FLASH_PERIOD )
	{
	  state = (state) ? 0 : 1;
	  count = 0;
	}

  if ( !state )
	return;
  
  for( v=list->go_head(); v; v=list->go_next() )
	draw_dot( (BigDot*)v );

  drawn = 1;
}

void BigDotMap::erase()
{
  void *v;

  if ( !drawn )
	return;
  
  for( v=list->go_tail(); v; v=list->go_prev() )
	erase_dot( (BigDot*)v );

  drawn = 0;
}

int BigDotMap::check_collide( int wx, int wy )
{
  void *v;
  BigDot *dot;
  
  for( v=list->go_head(); v; v=list->go_next() )
	{
	  dot = (BigDot*)v;

	  if ( dot->tx == wx && dot->ty == wy )
		{
		  list->pop_cur_n();
		  delete dot->save;
		  delete dot;
		  return 1;
		}
	}

  return 0;
}
	  
