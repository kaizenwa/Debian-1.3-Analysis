/*
 * Program XBLAST V2.1.8 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * September 24th 1996
 * started August 1993
 *
 * File: maze.h
 *       include file for maze.c
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

#ifndef _MAZE_H
#define _MAZE_H

/*
 * globals
 */
#ifndef _MAZE_C
extern int levelMax;
extern int scoreBoard;
extern int winningTheGame;
#endif

/*
 * prototypes
 */

#ifdef _MAZE_C
#define _EXTERN
#else
#define _EXTERN extern
#endif

#ifdef __STDC__
_EXTERN int load_all_levels (void);
_EXTERN void load_score_board (int game_mode, int num_victories);
_EXTERN void load_maze (int level, XBConfig *config);
_EXTERN void show_levels (void);
_EXTERN void show_levels_tcl (void);
_EXTERN BMLevelData * get_current_level (void);
_EXTERN char *get_level_name (int level);
_EXTERN char *get_level_res_name (int level);
_EXTERN char *get_level_author (int level);
_EXTERN char *get_level_tip (int level);
_EXTERN int get_game_mode (int level);
#else
_EXTERN int load_all_levels ();
_EXTERN void load_score_board ();
_EXTERN void load_maze ();
_EXTERN void show_levels ();
_EXTERN void show_levels_tcl ();
_EXTERN BMLevelData *get_current_level ();
_EXTERN char *get_level_name ();
_EXTERN char *get_level_res_name ();
_EXTERN char *get_level_author ();
_EXTERN char *get_level_tip ();
_EXTERN int get_game_mode ();
#endif

#undef _EXTERN

#endif
/*
 * end of file maze.h
 */
