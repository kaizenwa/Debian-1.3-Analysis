/* Definitions for directions of the compass.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* The terrain model is based on hexes arranged in horizontal rows.  This
   means that although east and west remain intact, the concepts of north
   and south have vanished. */

/* Unfortunately, not all hex-dependent definitions are here.  Pathfinding
   code has some knowledge of hexes also, as does map generation and
   probably parts of the machine player code. */

#define NUMDIRS 6

/* Symbols naming the directions. */

#define NORTHEAST 0
#define EAST  1
#define SOUTHEAST 2
#define SOUTHWEST 3
#define WEST  4
#define NORTHWEST 5

/* String names for the directions. */
		 
#define DIRNAMES { "NE", "E", "SE", "SW", "W", "NW" }

/* Conversions from directions to x,y deltas. */

#define DIRX { 0, 1,  1,  0, -1, -1 }
#define DIRY { 1, 0, -1, -1,  0,  1 }

/* Iteration over the different directions. */

#define for_all_directions(dir)  for ((dir) = 0; (dir) < NUMDIRS; ++(dir))

#define for_all_directions_randomly(dir,tmp)  \
  for ((tmp) = 0, (dir) = xrandom(NUMDIRS); (tmp) < NUMDIRS; ++(tmp), (dir) = ((dir) + 1) % NUMDIRS)

/* Formulas for relative directions. */

#define left_dir(d) (((d) + 5) % NUMDIRS)

#define right_dir(d) (((d) + 1) % NUMDIRS)

#define opposite_dir(d) (((d) + 3) % NUMDIRS)

#define angle_with(d1, d2)  \
  min((d1 - d2 + NUMDIRS) % NUMDIRS, (d2 - d1 + NUMDIRS) % NUMDIRS)

/* To generate a random direction. */

#define random_dir() (xrandom(NUMDIRS))

extern char *dirnames[];

extern int dirx[], diry[];
