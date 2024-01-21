/*
   fruit.h

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
#ifndef _fruit_h_
#define _fruit_h_

#include <gtools/bitmap.h>
#include "maze.h"

class Fruit {

 public:
  Fruit( char *fruit, Maze *maze, Bitmap *tile, int quota,
		int frames_off, int frames_on );
  ~Fruit();

  void draw();
  void erase();

  /* Returns 1 if collision with (lx,ly), 0 if not */
  int update( int lx, int ly );

 protected:
  int visible;
  int drawn;
  int count_on, count_off;		/* # frames to be on/off */
  int count;
  int next_count;
  int num_left;		/* Number of fruit left to be displayed */
  int x, y;
  Bitmap *map;
  Bitmap *save;
  int tx, ty;
};

#endif
