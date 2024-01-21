/*
 * Programm XBLAST V2.1.7 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * September 1st 1996
 * started August 1993
 *
 * File: intro.h
 * include file for intro.c
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

#ifndef _INTRO_H
#define _INTRO_H

/*
 * constants for intro
 */
#define CHAR_ANIME 7

/*
 * number of points for the intro x
 */
#define SIZE_OF_X 16
/*
 * length of intro
 */
#define INTRO_LENGTH (5*CHAR_ANIME)

/*
 * protoptypes
 */

#ifdef _INTRO_C
#define _EXTERN 
#else
#define _EXTERN extern
#endif

#ifdef __STDC__
_EXTERN void do_intro (BMPlayer *ps, PlayerStrings *player_stat, 
		       XBConfig *config);
_EXTERN void level_start (int num_disp);
_EXTERN void level_intro (int lvl, BMPlayer *player_stat, XBConfig *config);
_EXTERN void welcome (int n_player, PlayerStrings *st);
_EXTERN void status_board (int last_player, int num_victories,
			   BMPlayer *player_stat, PlayerStrings *p_string,
			   XBConfig *config);
_EXTERN void winning_the_game (int last_player, BMPlayer *player_stat, 
			       PlayerStrings *p_string, XBSettings *setup,
			       XBConfig *config);
#else
_EXTERN void do_intro ();
_EXTERN void level_start ();
_EXTERN void level_intro ();
_EXTERN void welcome ();
_EXTERN void status_board ();
_EXTERN void winning_the_game ();
#endif

#undef _EXTERN
#endif
/*
 * end of file intro.h
 */
