/*
   attack.cc

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

#include "attack.h"
#include "movement.h"
#include "error.h"

int find_path_to( int tx, int ty, int from_x, int from_y, Maze *maze )
{
  int x, y;
  int inc, dir;
  int p[4], i;
  
#ifdef DEBUG
  if ( tx < 0 || tx > (maze->w-1) || ty < 0 || ty > (maze->h-1) ||
	  from_x < 0 || from_x > (maze->w-1) || from_y < 0 ||
	  from_y > (maze->h-1) )
	fatal( "Bad args" );
#endif
  
  if ( from_x == tx && from_y == ty )	/* Already there? */
	{
	  /* Just find some direction we can move */
	  find_all_moves( p, tx, ty, maze );
	  for( i=0; i<4; ++i )
		{
		  if ( p[i] == 1 )
			return i;
		}
	}
  
  /* Are we looking for a horizontal path? */
  if ( from_y == ty )
	{
	  /* Yes -- decide left or right */
	  if ( tx > from_x )
		{
		  inc = 1;
		  dir = LUX_RIGHT;
		}
	  else
		{
		  inc = -1;
		  dir = LUX_LEFT;
		}

	  /* Do search */
	  x = from_x;
	  while( x != tx && can_move_dir( x, from_y, dir, maze ) )
		x += inc;

	  if ( x != tx )
		return -1;		/* Path blocked */
	  else
		return dir;		/* Path OK */
	}
  
  /* Looking for vertical path? */
  else if ( from_x == tx )
	{
	  /* Yes -- decide up or down */
	  if ( ty > from_y )
		{
		  inc = 1;
		  dir = LUX_DOWN;
		}
	  else
		{
		  inc = -1;
		  dir = LUX_UP;
		}

	  /* Do search */
	  y = from_y;
	  while( y != ty && can_move_dir( from_x, y, dir, maze ) )
		y += inc;

	  if ( y != ty )
		return -1;		/* Path blocked */
	  else
		return dir;		/* Path OK */
	}

  /* Else, didn't match X or Y -- no straight path */
  else
	return -1;
}

int find_dist_dir( int tx, int ty, int dir, Maze *maze )
{
  int r, c;
  int inc;
  
  if ( dir == LUX_LEFT || dir == LUX_RIGHT )
	{
	  inc = (dir == LUX_LEFT) ? -1 : 1;
	  c = tx;

	  while( can_move_dir( c, ty, dir, maze ) )
		c += inc;

	  /* Could use abs(), but perhaps this is faster */
	  return (dir == LUX_LEFT) ? (tx - c) : (c - tx);
	}
  else
	{
	  inc = (dir == LUX_UP) ? -1 : 1;
	  r = ty;

	  while( can_move_dir( tx, r, dir, maze ) )
		r += inc;

	  return (dir == LUX_UP) ? (ty - r) : (r - ty);
	}
}
