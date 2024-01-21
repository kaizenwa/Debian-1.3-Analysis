/*
   search.h

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

#ifndef _search_h_
#define _search_h_

/*
 * AgeMap* do_search_for( int tx, int ty, int from_x, int from_y,
 * 		Maze *maze, int max_depth, int *max_count, int prefer=0 )
 *
 * Find a path through `maze' from point (from_x,from_y) to
 * point (tx,ty) in at most `max_depth' moves, and record the
 * information in an AgeMap structure and return it.
 *
 * If a path is found, `max_count' is set to the number of steps
 * required to reach (tx,ty) and the path is recorded in `map'
 * such that (from_x,from_y) has a count of 1 and (tx,ty) has
 * a value of `max_count'.
 *
 * If no path found (or `max_depth' exceeded), NULL is returned.
 *
 * Note 1: `maze' should contain MAZE_TILE where a path is not allowed.
 *
 * Note 2: You should delete the retuned AgeMap when you are finished
 *         with it.
 *
 * Note 3: The path, if found, is not likely to be optimal. The
 *         `max_depth' parameter should (hopefully) screen out overly
 *         inefficient paths, though.
 *         
 *
 */
#include "agemap.h"
#include <gtools/bitmap.h>
#include "maze.h"

AgeMap* do_search_for( int tx, int ty, int from_x, int from_y,
					  Maze *maze, int max_depth, int *max_count,
					  int prefer = 0 );

#endif
