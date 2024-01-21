/*
 * Program XBLAST V2.1.8 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * September 8th 1996
 * started August 1993
 *
 * File: include.h
 * constants and macros for all files.
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as by published
 * by the Free Software Foundation; either version 2; or (at your option)
 * any later version
 *
 * This program is distributed in the hope that it will entertaining,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILTY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Publis License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */

 /* base dimensions */
#define BASE_X 8
#define BASE_Y 6

  /* constants */

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#define MAX_PLAYER 6
#define MAX_DISPLAY 6

#define STEP_HORI BASE_X
#define STEP_VERT BASE_Y

#define DEFAULT_DELAY 5

#define BLOCK_WIDTH  (8*BASE_X) 
#define BLOCK_HEIGHT (8*BASE_Y)

#define STAT_WIDTH  (6*BASE_X) 
#define STAT_HEIGHT (8*BASE_Y)

#define MAZE_W 15
#define MAZE_H 13

#define STAT_W 20
#define STAT_H 2

#define ILL_X (4*BASE_X)
#define ILL_Y (5*BASE_Y)

#define CHARW 3
#define CHARH 5


#define MAX_BLOCK 11


#define ILLTIME 256
#define BOMB_PROB 32

#define MAX_JUNKIE_TIME (384 + (rand() & 31))

#define MAX_EXTRA   2

  /* SpriteTypen */

#define STPlayer 0
#define STBomb   1

#if 0
/* walk directions macros */
#define TURN_CLOCKWISE(a)     ( ((a<1) || (a>4)) ? (a) : (((a)+2) % 4 + 1) )
#define TURN_ANTICLOCKWISE(a) ( ((a<1) || (a>4)) ? (a) : (((a)+0) % 4 + 1) )
#define TURN_OPPOSITE(a)      ( ((a<1) || (a>4)) ? (a) : (((a)+1) % 4 + 1) )
#define TURN_RANDOM           ( rand()%4 + 1 )
#endif

/* other Makros */

#ifdef MAX
#undef MAX
#endif
#define MAX(a,b) ( (a)>=(b) ? (a) : (b) )

#ifdef MIN
#undef MIN
#endif
#define MIN(a,b) ( (a)<=(b) ? (a) : (b) )

#ifdef ABS
#undef ABS
#endif
#define ABS(a)   ( (a)>=0 ? (a) : (-(a)) )

/*
 * end file include.h
 */












