/*
 * Programm XBLAST V1.2.5 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 31st 1996
 * started August 1993
 *
 * File: score.c
 * include file to score.c
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

#ifndef _SCORE_H
#define _SCORE_H

  /* Scorebitmaps */

#define SBVoid      0
#define SBTextLeft  1
#define SBTextMid   2
#define SBTextRight 3
#define SBDead      4
#define SBSick      (4+MAX_PLAYER)
#define SBPlayer    (4+2*MAX_PLAYER)

#define MAX_SCORE_TILES (4 + 3*MAX_PLAYER)
#define MAX_SCORE_ADDON 3

#define SCORE_DEPTH 3

#ifndef _SCORE_C
extern BitmapStruct score_led[2];
extern BitmapStruct score_led_addon[2];
extern BitmapStruct score_tile[MAX_SCORE_TILES];
extern BitmapStruct score_tile_addon[MAX_SCORE_ADDON][SCORE_DEPTH];
#endif

#endif
/*
 * end of file score.h
 */

