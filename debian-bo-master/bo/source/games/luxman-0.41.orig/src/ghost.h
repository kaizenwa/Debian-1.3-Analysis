/*
   ghost.h

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
#ifndef _ghost_h_
#define _ghost_h_

#include <gtools/bitmap.h>
#include "movement.h"
#include "agemap.h"
#include "maze.h"

/* Ghost states */
#define GHOST_NORMAL	0
#define GHOST_ENERGIZED	1
#define GHOST_FLASHING	2
#define GHOST_EYES		3
#define GHOST_WAIT		4		/* Waiting to be regenerated.
								   Not allowed to set this directly. */
								
class Ghost {

 public:
  /*
   * Constructor args:
   *
   * 	color	Color for ghost
   * 	Maze	Bitmap for maze -- NOT copied or deleted
   * 	CX	Pixels per tile in X direction
   * 	CY	Pixels per tile in Y direction
   *
   * Note: Maze must have exactly 1 location of color `color'. This
   *       determines the ghost's starting position.	
   */
  Ghost( unsigned char color, Maze *MAZE, int CX, int CY,
		int number );
  ~Ghost();

  void move( int to_x, int to_y );		/* Do movement and draw --
									   (TX,TY) is target location */
  
  void erase();		/* Erase */
  void do_draw();
  
  int TX();		  	/* Return X and Y tile locations (0-based) */
  int TY();

  int X();			/* Returns screen coords */
  int Y();

  /* Note! Make sure ghost is erased before calling set_state */
  void set_state( int );
  int get_state();
  
 protected:
  int state;		/* One of GHOST_... constants */
  
  /*
   * map[LUX_RIGHT]	Looking right
   * map[LUX_LEFT]	Looking left
   * map[LUX_UP]	Looking up
   * map[LUX_DOWN]	Looking down
   * map[4]  		Energized (blue)
   * map[5]		    Energized (white)
   * map[6+LUX_RIGHT]	Eyes right
   * map[6+LUX_LEFT]		Eyes left
   * map[6+LUX_UP]		Eyes up
   * map[6+LUX_DOWN]		Eyes down
   * map[10+LUX_RIGHT]	Right - anim 2
   * map[10+LUX_LEFT]	Left - anim 2
   * map[10+LUX_UP]		Up - anim 2
   * map[10+LUX_DOWN]	Down - anim 2
   * map[14]			Energized (blue) 2
   * map[15]			Energized (white) 2
   */
  
  Bitmap *map[16];

  AgeMap *agemap;
  
  Bitmap *save;		/* Save area we overwrite */

  Maze *maze;		/* The maze */

  int ghost_number;		/* Ghost number */

  int anim_count;		/* For toggling between the two sets of images */
  int anim_state;
  
  int tx, ty;		/* Tile location */
  int x, y;			/* Upper left corner location on screen */

  int vx, vy;		/* X and Y velocity */

  int new_vx, new_vy;	/* `Pending' velocity changes */
  
  int home_x, home_y;		/* `home' location */
  
  int cx, cy;		/* Pixels per tile in X and Y directions */
  
  int ofx, ofy;		/* Current X and Y offsets relative to tile origin */
					   
  int movedir;		/* One of LUX_.. constants */

  int visible;		/* 1 if currently drawn */

  int clr;			/* color */

  int cur_age;

  int term_count;	/* For state GHOST_EYES - terminal count.
					   Also used for state GHOST_WAIT as frames
					   until regeneration. */
  
  AgeMap *home_path;	/* For state GHOST_EYES - path to home */

  int flash_count;	/* For state GHOST_FLASHING */
  int flash_state;	/* 0 - normal, 1 - flashed */
  
  /* Helper functions */
  void do_movement();
  void update_movedir0( int to_x, int to_y ); /* Called when ofx == ofx == 0 */
  void update_movedir1( int to_x, int to_y ); /* Called when ofx == ofx == 0 */
  void update_movedir2( int to_x, int to_y ); /* Called when ofx == ofx == 0 */
  void update_movedir3( int to_x, int to_y ); /* "" */
  
#ifdef DEBUG
  void verify_magic( int pt );
  void update_magic_sum();
  void print_map();
#endif  
};

#endif

