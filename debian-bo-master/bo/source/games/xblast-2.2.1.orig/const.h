/*
 * Program XBLAST V2.1.8 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * September 24th 1996
 * started August 1993
 *
 *
 * File: const.h
 * constants and types for main.c, maze.c, graphics.c .
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

#define MAX_VICTORIES 9

#define FADE_STEP 16

#define DEAD_TIME 8

#define TELEPORT_TIME 20 /* Define > 1 */

#define TIME_STEP 48 /* 48 */
#define STUN_TIME 16
#define GAME_TIME (60*TIME_STEP + DEAD_TIME)
#define PAUSE_DELAY (TIME_STEP/2)

#define NEW_INVINCIBLE 64
#define EXTRA_INVINCIBLE 160
#define EXTRA_INVISIBLE 256
#define MAX_LIVES 3
#define MAX_RANGE 10

#define BIG_WIDTH 112
#define BIG_HEIGHT 144

#define SPRITE_WIDTH   (6*BASE_X)
#define SPRITE_HEIGHT  (69*BASE_Y/6)
#define SPRITE_X_OFF   (BASE_X)
#define SPRITE_Y_OFF   (5*BASE_Y/2)

#define WINNER_WIDTH   (8*BASE_X)
#define WINNER_HEIGHT  (69*BASE_Y/6)
#define WINNER_Y_OFF   (5*BASE_Y/2)
#define WINNER_X_OFF   0

#define LED_WIDTH      (2*BASE_X)
#define LED_HEIGHT     (8*BASE_Y/3)

#define PIXW   (MAZE_W * BLOCK_WIDTH)
#define PIXH   (MAZE_H * BLOCK_HEIGHT)
#define SCOREH (STAT_HEIGHT+LED_HEIGHT)

#define BOMB_VX        (2*BASE_X)
#define BOMB_VY        (2*BASE_Y)

#define BOMB_STUN_X    (4*BASE_X)
#define BOMB_STUN_Y    (4*BASE_Y)

/*
 * abort mode 
 */
#define ABORT_NONE   0
#define ABORT_TRUE   1
#define ABORT_CANCEL 2
/*
 * constants for getting single left, and right player of display
 */ 
#define SINGLE_PLAYER  MAX_PLAYER
#define RIGHT_PLAYER  (MAX_PLAYER+1)
#define LEFT_PLAYER   (MAX_PLAYER+2)

/*
 * team partner macro for two on two team mode
 */
#if 0
#define PARTNER(p) ( ((p)/2)*2 + 1 - ((p)%2) )
#define DOUBLE(p) ( ((p)+(num_player/2)) % num_player )
#endif

/*
 * end of file const.h
 */






