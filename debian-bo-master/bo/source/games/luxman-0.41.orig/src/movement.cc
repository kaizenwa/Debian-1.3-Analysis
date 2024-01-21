/*
   movement.cc

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

#include "movement.h"
#include "error.h"

#ifdef DEBUG
static int check( int tx, int ty, Maze *map )
{
  if ( tx < 0 || tx > (map->w-1) || ty < 0 || ty > (map->h-1) )
	return -1;
  else
	return 0;
}
#endif

int can_move_up( int tx, int ty, Maze *map )
{
#ifdef DEBUG
  if ( check( tx, ty, map ) != 0 )
	fatal( "Bad arg" );
#endif  
  return (ty != 0) && (*(map->map + map->rofs[ty-1] + tx) != MAZE_TILE);
}

int can_move_down( int tx, int ty, Maze *map )
{
#ifdef DEBUG
  if ( check( tx, ty, map ) != 0 )
	fatal( "Bad arg" );
#endif  
  return (ty != (map->h-1)) &&
	(*(map->map + map->rofs[ty+1] + tx) != MAZE_TILE);
}

int can_move_left( int tx, int ty, Maze *map )
{
#ifdef DEBUG
  if ( check( tx, ty, map ) != 0 )
	fatal( "Bad arg" );
#endif  
  return (tx != 0) && (*(map->map + map->rofs[ty] + tx - 1) != MAZE_TILE);
}

int can_move_right( int tx, int ty, Maze *map )
{
#ifdef DEBUG
  if ( check( tx, ty, map ) != 0 )
	fatal( "Bad arg" );
#endif  
  return (tx != (map->w-1)) &&
	(*(map->map + map->rofs[ty] + tx + 1) != MAZE_TILE);
}

struct cookie {
  int (*can_fn)( int,int,Maze* );
};

static cookie bear[4] = {
  { can_move_right },
  { can_move_left },
  { can_move_up },
  { can_move_down } };

int can_move_dir( int tx, int ty, int dir, Maze *map )
{
#ifdef DEBUG
  if ( check( tx, ty, map ) != 0 )
	fatal( "bad arg" );

  if ( dir < 0 || dir > 3 )
	fatal( "bad arg");
#endif
  
  return (bear[dir].can_fn)( tx, ty, map );
}

void find_all_moves( int *moves, int tx, int ty, Maze *map )
{
  moves[LUX_RIGHT] = can_move_right( tx, ty, map );
  moves[LUX_LEFT] = can_move_left( tx, ty, map );
  moves[LUX_UP] = can_move_up( tx, ty, map );
  moves[LUX_DOWN] = can_move_down( tx, ty, map );
}



