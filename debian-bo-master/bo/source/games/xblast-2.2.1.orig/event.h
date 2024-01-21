/*
 * Programm XBLAST V2.1.7 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * September 1st 1996
 * started August 1993
 *
 * File: event.h
 * header file for event.c
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

#ifndef _EVENT_H
#define _EVENT_H

/*
 * global varaiables
 */

#ifndef _EVENT_C
extern DispPlayer disp_player[2*MAX_PLAYER];
extern int forkMode;
#endif

/*
 * globals
 */
#ifndef _EVENT_C
#ifdef __STDC__
extern void (*quit_function)(void);
#else
extern void (*quit_function)();
#endif
#endif


/*
 * prototypes
 */

#ifdef _EVENT_C
#define _EXTERN
#else
#define _EXTERN extern
#endif

#ifdef __STDC__
_EXTERN void init_timer (void);
_EXTERN void set_game_keys (int disp);
_EXTERN void set_event_communication (int type, int p1, int p2);
_EXTERN void game_event (int num_disp);
_EXTERN void intro_event (int num_disp);
_EXTERN void wait2_event (int num_disp);
_EXTERN int game_eval_keys(int n_player, BMPlayer *player_stat, 
			   PlayerStrings *p_string, PFV key_func,
			   int* pause_mode);
_EXTERN int wait_eval_keys (int n_player);
_EXTERN void clear_keys(int n_player);
_EXTERN void set_players_for_display (int disp, int player1, int player2);
_EXTERN void fade_in (int num_disp);
_EXTERN void fade_out (int num_disp);
_EXTERN void circle_in (int num_disp);
_EXTERN void wait2_two_text (int num_disp, int n_player, BMPlayer *player_stat,
			     char *text1, char *text2);
#else
_EXTERN void init_timer ();
_EXTERN void set_game_keys ();
_EXTERN void set_event_communication ();
_EXTERN void game_event ();
_EXTERN void intro_event ();
_EXTERN void wait2_event ();
_EXTERN int game_eval_keys();
_EXTERN int wait_eval_keys ();
_EXTERN void clear_keys();
_EXTERN void set_players_for_displays ();
_EXTERN void fade_in ();
_EXTERN void fade_out ();
_EXTERN void circle_in ();
_EXTERN void wait2_two_text ();
#endif

#undef _EXTERN

#endif
/*
 * end of file event.h
 */
