/*
   movement.h

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

#ifndef _movement_h_
#define _movement_h_

#include <gtools/gtools.h>
#include "maze.h"
#include "colors.h"

/* Don't change order of these */
#define LUX_RIGHT	0
#define LUX_LEFT	1
#define LUX_UP		2
#define LUX_DOWN	3

/*
 * The can_move... functions assume that you are currently
 * in tile (tx,ty) (0-based) and are wanting to move in
 * the desired direction.
 *
 * Returns 1 if move OK, 0 if not (tile in the way or
 * edge of map reached).
 *
 * NOTE!! (tx,ty) must specify a location inside the map.
 * For efficiency, this is NOT checked!
 *
 * Assumes that color `MAZE_TILE' specifies a tile location.
 */
int can_move_up( int tx, int ty, Maze *map );
int can_move_down( int tx, int ty, Maze *map );
int can_move_left( int tx, int ty, Maze *map );
int can_move_right( int tx, int ty, Maze *map );

/*
 * int can_move_dir( int tx, int ty, int dir, Maze *map )
 *
 * Checks direction specified by `dir', where `dir' is one
 * of the LUX_.. constants.
 */
int can_move_dir( int tx, int ty, int dir, Maze *map );
				 
/*
 * int find_all_moves( int *moves, int tx, int ty, Maze *map )
 *
 * `moves' must be at least 4 elements long. On exit it will be
 * set as follows:
 *
 * moves[LUX_RIGHT] will be 1 iff movement to right legal
 * moves[LUX_LEFT]   "         "          "   left   "
 *
 * etc.
 */
void find_all_moves( int *moves, int tx, int ty, Maze *map );

#endif	/* _movement_h_ */


