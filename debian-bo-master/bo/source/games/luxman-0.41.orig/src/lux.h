/*
   lux.h

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

#ifndef _lux_h_
#define _lux_h_

#include <gtools/bitmap.h>
#include "movement.h"
#include "maze.h"

class LuxMan {
 public:
  LuxMan( int spos_color, Maze *MAZE, int CX, int CY,
		 int body_clr=YELLOW, int glasses_clr=YELLOW );
  ~LuxMan();

  void move();
  void erase();
  void do_draw();
  
  int TX();		/* Returns tile location */
  int TY();

  int X();		/* Returns screen coordinates */
  int Y();		

 protected:
  /*
   * Maps:
   * 0-3	Right
   * 4-7	Left
   * 8-11	Up
   * 12-15	Down
   */
  Bitmap *map[16];

  Bitmap *save;

  Maze *maze;

  int tx, ty;
  int x, y;
  int cx, cy;
  int ofx, ofy;

  int movedir;
  int imgofs;		/* Which anim frame (0-3) */
  int cycledir;		/* 1 - cycling up, 0 cycling down */
  int imgbase;		/* usually same as movedir, unless movedir=-1 */
  int visible;

  int pending_key;
  
  void update_movedir();	
  void do_movement();

#ifdef DEBUG
  void verify_magic();
#endif  
};

#endif
