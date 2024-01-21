/*
   dotmap.cc

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
#include <stdlib.h>
#include "dotmap.h"
#include <gtools/gr.h>
#include <gtools/fmalloc.h>
#include "colors.h"
#include "error.h"
#include "globals.h"

DotMap::DotMap( Maze *MAZE, Bitmap *Tile, int W, int H, int c )
{
  int i, j;
  unsigned char *s, *d;
  
  maze = MAZE;
  tile = Tile;
  clr = c;
  
  xlen = W-1;
  ylen = H-1;

  /* Calc dot offsets to center dot in tile */
  ofx = (tile->w - W/2) / 2;
  ofy = (tile->h - H/2) / 2;

  /* Setup tile origins */
  xpos = (int*)Malloc( maze->w * sizeof(int) );
  ypos = (int*)Malloc( maze->h * sizeof(int) );

  if ( !xpos || !ypos )
	fatal( "Out of memory" );
  
  for( i=0; i<maze->w; ++i )
	xpos[i] = i*tile->w;

  for( i=0; i<maze->h; ++i )
	ypos[i] = i*tile->h;
  
  /* Alloc dotmap */
  map = new Bitmap( maze->w, maze->h );

  if ( !map )
	fatal( "Out of memory" );
  
  /* Go through maze and fill in dot locations */

  s = MAZE->map;
  d = map->map;

  num_dots = 0;
  
  /* For each row... */
  for( i=0; i<maze->h; ++i )
	{
	  for( j=0; j<maze->w; ++j, ++s, ++d )
		{
		  if ( *s != MAZE_NODOT && *s != MAZE_LUX &&
			  *s != MAZE_TILE && *s != MAZE_BIGDOT && *s != MAZE_HOME )
			{
			  *d = 1;
			  ++num_dots;
			}
		  else
			*d = 0;
		}
	}
}

DotMap::~DotMap()
{
#ifdef DEBUG
  if ( map->verify_magic() != 0 )
	fatal( "bitmap clobbered" );
#endif
  
  delete map;

  if ( Free( xpos ) != 0 || Free( ypos ) != 0 )
	fatal( "Mem clobbered" );

#ifdef DEBUG  
  printf("DotMap magic OK\n");
#endif  
}

int DotMap::draw_dot( int x, int y, int c )
{
  gr_fillbox( x + ofx, y + ofy, x + ofx + xlen, y + ofy + ylen, c );
  return 0;
}

void DotMap::draw()
{
  int i, j;
  int x, y;
  unsigned char *smap, *sdot;
  Bitmap *home;

  home = new Bitmap( "home.map" );
  
  smap = maze->map;
  sdot = map->map;

  gr_fillbox( 0,0,maze->w*tile->w,maze->h*tile->h, gb_bg_color );

  for( i=0, y=0; i<maze->h; ++i, y += tile->h )
	{
	  for( j=0, x=0; j<maze->w; ++j, x += tile->w, ++sdot, ++smap )
		{
		  if ( *smap == MAZE_TILE )
			put_bitmap( x, y, tile );
		  else if ( *smap == MAZE_HOME )
			put_bitmap_t( x, y, home );
		  else if ( *sdot == 1 )
			draw_dot( x, y, clr );
		}
	}

  gr_box( 0,0,maze->w*tile->w,maze->h*tile->h,WHITE );

  delete home;
}

int DotMap::update( int tx, int ty )
{
  unsigned char *m;

#ifdef DEBUG
  if ( tx < 0 || tx > (map->w-1) ||
	  ty < 0 || ty > (map->h-1) )
	fatal( "Bad coord" );
#endif
  
  /* See if there is a dot in this tile */
  m = map->map + map->rofs[ty] + tx;

  if ( *m == 0 )		/* No dot */
	return 0;

  *m = 0;
  draw_dot( xpos[tx], ypos[ty], gb_bg_color );
  --num_dots;

  return 1;
}

int DotMap::num_left()
{
  return num_dots;
}
