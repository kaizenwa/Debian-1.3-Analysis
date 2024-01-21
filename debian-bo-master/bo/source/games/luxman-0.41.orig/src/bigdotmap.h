/*
   bigdotmap.h

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

#ifndef _bigdotmap_h_
#define _bigdotmap_h_

#include <gtools/gtools.h>
#include <lib/vllist.h>
#include "maze.h"
#include "colors.h"

struct BigDot {
  int tx, ty;
  Bitmap *save;
};

class BigDotMap {

 public:
  BigDotMap( Maze *maze, Bitmap *tile, int c );
  ~BigDotMap();
  
  void draw();
  void erase();

  /* Returns 1 if collision, 0 if not */
  int check_collide( int wx, int wy );

  int num_left();
  
 protected:
  VLList *list;
  
  Bitmap *dotmap;		/* Bitmap for bigdot */

  int *xpos, *ypos;		/* Tile origins */

  int count;			/* Counter for flashing */
  int state;			/* 1 - on, 0 - off */
  int drawn;
  
  int draw_dot( BigDot* );
  int erase_dot( BigDot* );
};

#endif
