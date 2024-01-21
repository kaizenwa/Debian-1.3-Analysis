/*
 * Program XBLAST V2.2.1 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 26th 1997
 * started August 1993
 *
 * File: main.c 
 *
 * include file for main.h
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as published
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

#ifndef _MAIN_H
#define _MAIN_H

/*
 * global variables
 */
#ifndef _MAIN_C
extern int game_time;
extern int FrameTime;
#endif

/*
 * global prototypes
 */
#ifdef _MAIN_C
#define _EXTERN 
#else
#define _EXTERN extern
#endif

#ifdef __STDC__
_EXTERN void setup_funcs (BMFuncData *cur_level);
_EXTERN void setup_players(unsigned long g_mode, BMPlayerData *level);
_EXTERN void drop_bomb (BMPlayer *ps, int type);
_EXTERN void kill_player_at (int x, int y);
_EXTERN int kill_other_players (int team);
_EXTERN int check_b_near (int x, int y);
_EXTERN void exit_prg (int status);
#else
_EXTERN void setup_funcs ();
_EXTERN void setup_players();
_EXTERN void drop_bomb ();
_EXTERN void kill_player_at ();
_EXTERN int kill_other_players ();
_EXTERN int check_b_near ();
_EXTERN void exit_prg ();
#endif

#undef _EXTERN

#endif
/*
 * end of main.h
 */
