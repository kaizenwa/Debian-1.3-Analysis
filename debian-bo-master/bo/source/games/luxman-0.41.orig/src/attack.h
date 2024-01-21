/*
   attack.h

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

#ifndef _attack_h_
#define _attack_h_

#include "movement.h"

/*
 * int find_path_to( int tx, int ty, int from_x, int from_y, Maze *maze )
 *
 * Determines if there is a direct (straight) path from (TX,TY) to
 * (FROM_X,FROM_Y).
 *
 * If there is a path, one of {LUX_LEFT, LUX_RIGHT, LUX_UP, LUX_DOWN}
 * is returned, indicating where the path is.
 *
 * If there is no path, -1 is returned.
 */
int find_path_to( int tx, int ty, int from_x, int from_y, Maze *maze );

/*
 * int find_dist_dir( int tx, int ty, int dir, Maze *maze )
 *
 * Find distance from (TX,TY) you can move in direction `dir' without
 * hitting a tile.
 *
 * Returns distance.
 */
int find_dist_dir( int tx, int ty, int dir, Maze *maze );

#endif
