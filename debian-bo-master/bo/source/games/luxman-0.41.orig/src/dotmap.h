/*
   dotmap.h

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

#ifndef _dotmap_h_
#define _dotmap_h_

#include <gtools/bitmap.h>
#include "maze.h"

class DotMap {

 public:
  /*
   * DotMap( Maze *maze, Bitmap *tile, int w, int h, int c )
   *
   * maze	Bitmap describing the maze
   * tile	Bitmap used for tile
   * w	Dot width
   * h	Dot height
   * c  Color for dots
   */
  DotMap( Maze *MAZE, Bitmap *Tile, int W, int H, int c );
  ~DotMap();

  /* Draw entire map */
  void draw();
  
  /*
   * Update dotmap - (tx,ty) is luxman location.
   * Returns 1 if luxman `ate' a dot, 0 if not.
   */
  int update( int tx, int ty );

  int num_left();		/* Number of dots left */
  
 protected:
  int num_dots;		/* Number of dots left in maze */
  Bitmap *map;		/* Dot map */
  Bitmap *tile;
  Maze *maze;		/* Maze */
  int ofx, ofy;		/* Dot X and Y offsets from upper left */
  int xlen, ylen;	/* Dot X and Y size */
  int clr;			/* Dot color */
  int *xpos, *ypos;	/* X and Y tile origins */
  
  int draw_dot( int x, int y, int c );
};

#endif
