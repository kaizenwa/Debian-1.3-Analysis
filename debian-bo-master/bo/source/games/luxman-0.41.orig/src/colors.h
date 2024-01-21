/*
   colors.h

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

#ifndef _colors_h_
#define _colors_h_

#include <gtools/screen.h>

/* Colors used in maze bitmap */

#define MAZE_GHOST1	LIGHTBLUE		/* Each of the 4 ghosts */
#define MAZE_GHOST2	RED
#define MAZE_GHOST3	MAGENTA
#define MAZE_GHOST4	GREEN
#define MAZE_LUX	CYAN			/* luxman location */
#define MAZE_TILE	WHITE	   		/* a tile location */
#define MAZE_NODOT	0xff			/* no dot here */
#define MAZE_HOME	DARKGRAY		/* ghost `home' square */
#define MAZE_BIGDOT	YELLOW
#define MAZE_FRUIT	LIGHTRED		/* `Fruit' location */

/* Color of ghost in bitmap files `ghost_x.map' */
#define CLR_GHOST	LIGHTBLUE

#endif

