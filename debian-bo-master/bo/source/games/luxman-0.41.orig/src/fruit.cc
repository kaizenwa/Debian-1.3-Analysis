/*
   fruit.cc

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

#include "fruit.h"
#include "error.h"
#include "colors.h"
#include "label.h"

Fruit::Fruit( char *fruit, Maze *maze, Bitmap *tile, int quota,
			 int frames_off, int frames_on )
{
  unsigned char *m;
  int i, j;
  int lx, ly;
  
  map = new Bitmap( fruit );
  save = new Bitmap( map->w, map->h );

  num_left = quota;
  count_on = frames_on;
  count_off = frames_off;
  
  m = maze->map;

  tx = ty = -1;
  lx = ly = -1;
  
  for( i=0; i<maze->h; ++i )
	{
	  for( j=0; j<maze->w; ++j )
		{
		  if ( *m == MAZE_FRUIT )
			{
			  tx = j;
			  ty = i;
			}
		  else if ( *m == MAZE_LUX )
			{
			  lx = j;
			  ly = i;
			}
		  ++m;
		}
	}

  if ( tx == -1 )
	{
     if ( lx == -1 )
		fatal( "No fruit position" );
	  else
		{
		  /* Default to luxman position */
		  tx = lx;
		  ty = ly;
		}
	}
  
  x = tx * tile->w + tile->w/2 - map->w/2;
  y = ty * tile->h + tile->h/2 - map->h/2;

  visible = 0;
  drawn = 0;
  
  count = 0;
  next_count = count_off;
}

Fruit::~Fruit()
{
  delete save;
  delete map;
}

void Fruit::draw()
{
  if ( !visible )
	return;

  get_bitmap( save, x, y );
  put_bitmap_t( x, y, map );
  drawn = 1;
}

void Fruit::erase()
{
  if ( !drawn )
	return;

  put_bitmap( x, y, save );
  drawn = 0;
}
  
int Fruit::update( int lx, int ly )
{
  if ( num_left <= 0 )
	return 0;
  
  ++count;

  /* Did fruit get eaten? */
  if ( visible && lx == tx && ly == ty )
	{
	  /* Yes -- disappear */
	  visible = 0;
	  count = 0;
	  next_count = count_off;
	  --num_left;
	  return 1;
	}
  else if ( count >= next_count )
	{
	  if ( !visible )		/* Time to show fruit? */
		{
		  visible = 1;
		  count = 0;
		  next_count = count_on;
		}
	  else				/* Time to hide fruit */
		{
		  visible = 0;
		  next_count = count_off;
		  count = 0;
		  --num_left;
		}
	}

  return 0;
}
