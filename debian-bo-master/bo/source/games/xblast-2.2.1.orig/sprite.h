/*
 * Programm XBLAST V1.2.5 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 30th 1996
 * started August 1993
 *
 * File: sprite.c
 * bitmap data of player sprites
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

#ifndef _SPRITE_H
#define _SPRITE_H

/*
 * constants for sprites 
 */
#define SPRITE_DEPTH 4

#define MAX_ANIME 17
#define WINNER_ANIME 15
#define BIG_ANIME 16



#ifndef _SPRITE_C
extern BitmapStruct sprite_mask[MAX_ANIME];
extern BitmapStruct sprite_bits[MAX_PLAYER][MAX_ANIME];
extern BitmapStruct sprite_addon[MAX_ANIME][SPRITE_DEPTH];
#else

#endif

#endif
/*
 * end of fiile sprite.h
 */
