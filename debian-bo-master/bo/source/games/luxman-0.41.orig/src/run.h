/*
   run.h

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

#ifndef _run_h_
#define _run_h_

#include <gtools/gtools.h>
#include "dotmap.h"
#include "bigdotmap.h"
#include "score.h"

struct RunState {
  
  Maze *maze;
  Bitmap *tile;
  DotMap *dotmap;
  BigDotMap *bigdotmap;
  int search_depth;		/* Ghost search depth */
  int regen_wait;	   	/* Ghost regeneration delay (frames) */
  int frames;
  int seconds;
  char *fruitname;		/* Bitmap name */
  int fruitval;			/* Point value */
  int fruit_off;	   	/* # frames fruit off */
  int fruit_on;			/* # frames fruit on */
  int max_fruit;		/* Fruit `quota' */
  Score *score;			/* Scoreboard */

  int ghost_etime;		/* Energize time, in frames */
  int ghost_ftime;		/* Flash time, in frames */

  int next_free;		/* Score to get next `free' man */
  int lives;
  int level;

  int skill;			/* 0 - no l.o.s., no seek
						   1 - l.o.s., no seek
						   2 - l.o.s., seek, momentum
						   3 - l.o.s., seek, no momentum */
  int play_intro;		/* 1 if intro music should be played */

  int lux_body;		  	/* LuxMan body color */
  int lux_glasses;	   	/* LuxMan glasses color */
};

/*
 * Runs screen. Updates runstate->frames and runstate->lives.
 *
 * Returns 1 if screen completed, 0 if not.
 */
int run_screen( RunState *state );

void shell_out( RunState *state );

#endif
