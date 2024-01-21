/*
   search.cc

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

#include "search.h"
#include "colors.h"
#include "error.h"
#include "attack.h"

#define HIGH_COUNT	(2<<24)

/*
 * Returns `count' if path found, -1 if not.
 * Assumes that `map' is zero wherever we CAN move,
 * and non-zero where we cannot move.
 */
static
int do_search_step( int tx, int ty, int from_x, int from_y,
				   int max_depth, int count, AgeMap *map,
				   Maze *maze, int prefer )
{
  int r;
  int dir;
  int i;
  
  /* First, see if search terminates with this tile */
  
  if ( !max_depth )
	return -1;			/* Search failed */

  if ( tx == from_x && ty == from_y )
	{
	  map->set( tx, ty, count );
	  return count;		/* Search succeeded */
	}

  /* Else, try to branch off in other directions */

  /* Set marker so that we do not try to come back to this square */
  map->set( from_x, from_y, count );
  
  /* If we can see target, branch off to it */
  dir = find_path_to( tx, ty, from_x, from_y, maze );

  r = -1;
  
  if ( dir != -1 )		/* See target? */
	{
	  switch( dir )		/* Yes -- branch off */
		{
		case LUX_LEFT:
		  r = do_search_step( tx, ty, from_x-1, from_y, max_depth-1,
							 count+1, map, maze, prefer );
		  break;

		case LUX_RIGHT:
		  r = do_search_step( tx, ty, from_x+1, from_y, max_depth-1,
							 count+1, map, maze, prefer );
		  break;

		case LUX_UP:
		  r = do_search_step( tx, ty, from_x, from_y-1, max_depth-1,
							 count+1, map, maze, prefer );
		  break;

		case LUX_DOWN:
		  r = do_search_step( tx, ty, from_x, from_y+1, max_depth-1,
							 count+1, map, maze, prefer );
		  break;

		default:
		  fatal( "Can't get here!" );
		}

	  if ( r != -1 )		
		return r;		/* Found a path! */
	}

  dir = prefer;
  
  /* Can't see target -- pick a direction -- favor `prefer' */
  for( i=0; i<4; ++i )
	{
	  switch( dir )
		{
		case LUX_LEFT:
		  if ( from_x > 0 && map->get( from_x-1, from_y ) == 0 )
			{
			  r = do_search_step( tx, ty, from_x-1, from_y, max_depth-1,
								 count+1, map, maze, prefer );

			  if ( r != -1 )
				return r;		/* Found a path! */
			}
		  break;

		case LUX_RIGHT:
		  if ( from_x < (map->W()-1) && map->get( from_x+1, from_y ) == 0 )
			{
			  r = do_search_step( tx, ty, from_x+1, from_y, max_depth-1,
								 count + 1, map, maze, prefer );

			  if ( r != -1 )
				return r;
			}
		  break;

		case LUX_UP:
		  if ( from_y > 0 && map->get( from_x, from_y-1 ) == 0 )
			{
			  r = do_search_step( tx, ty, from_x, from_y-1, max_depth-1,
								 count+1, map, maze, prefer );

			  if ( r != -1 )
				return r;
			}
		  break;

		case LUX_DOWN:
		  if ( from_y < (map->H()-1) && map->get( from_x, from_y+1 ) == 0 )
			{
			  r = do_search_step( tx, ty, from_x, from_y+1, max_depth - 1,
								 count+1, map, maze, prefer );

			  if ( r != -1 )
				return r;
			}
		  break;
		}

	  if ( ++dir > 3 )
		dir = 0;
	}
  
  /* All searches failed - Remove marker and exit */
  map->set( from_x, from_y, 0 );

  return -1;
}

AgeMap *do_search_for( int tx, int ty, int from_x, int from_y,
					  Maze *maze, int max_depth, int *max_count,
					  int prefer )
{
  AgeMap *map;
  int i, j;
  unsigned char *m;

#ifdef DEBUG
  if ( tx < 0 || tx > (maze->w-1) || ty < 0 || ty > (maze->h-1) ||
	  from_x < 0 || from_x > (maze->w-1) ||
	  from_y < 0 || from_y > (maze->h-1) )	/* Whew! */
	fatal( "bad arg" );
#endif
  
  map = new AgeMap( maze->w, maze->h );
  if ( !map )
	fatal( "Out of memory" );

  m = maze->map;
  
  /* Set all obstructions to HIGH_COUNT so we don't go there */
  for( i=0; i<maze->h; ++i )
	{
	  for( j=0; j<maze->w; ++j )
		{
		  if ( *m == MAZE_TILE )
			map->set( j, i, HIGH_COUNT );
		  ++m;
		}
	}

  /* Do search */
  i = do_search_step( tx, ty, from_x, from_y, max_depth, 1, map, maze,
					 prefer );

  if ( i == -1 )
	{
	  delete map;
	  return NULL;
	}
  else
	{
	  if ( max_count )
		*max_count = i;
	  return map;
	}
}

