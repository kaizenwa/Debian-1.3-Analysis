/*
   lf.h

   This file is part of LuxMan.
   
   Copyright (C) 1995 Frank McIngvale (frankm@nuance.com)
   
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

#ifndef _lf_h_
#define _lf_h_

#define MAX_LINE_LEN		200

struct Level {
  char *mazename;  				/* Name of maze bitmap file */
  char *fruitname;				/* Name of fruit bitmap file */
  char *tile;				   	/* Name of tile bitmap file */
  int depth;					/* Max search depth (ghosts) */
  int regen_wait;				/* # frames ghost regeneration delay */
  int bg;						/* Background color */
  int dot_clr;	   				/* Dot color */
  int bigdot_clr;				/* Bigdot color */
  int fruitval;					/* Fruit point value */
  int fruit_off;				/* # frames fruit off */
  int fruit_on;					/* # frames fruit on */
  int max_fruit;				/* Fruit quota */
  int en_frames;				/* # frames to stay blue */
  int fl_frames;				/* # frames to flash */
  int lux_body;					/* LuxMan body color */
  int lux_glasses;				/* LuxMan glasses color */

  Level();
  ~Level();
};

/*
   Returns:
		0	Level read OK
		-1	Error while reading level
		-2	No more levels (EOF found)
*/   
int lf_read_level( Level *lev, FILE *fp );

#endif
