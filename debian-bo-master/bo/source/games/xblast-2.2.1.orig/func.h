/*
 * Programm XBLAST V2.2.1 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 26th, 1997
 * started August 1993
 *
 * File: func.h
 * special functions for levels 
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as by published
 * by the Free Software Foundation; either version 2; or (at your option)
 * any later version
 *
 * This program is distributed in the hope that it will be entertaining,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILTY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Publis License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef _FUNC_H
#define _FUNC_H

#ifdef _FUNC_C
#define _EXTERN 
#else
#define _EXTERN extern
#endif

/* 
 * special init functions 
 */
#ifdef __STDC__     
_EXTERN void special_init_void (BMPlayer *player_stat);
_EXTERN void special_init_special_bombs_30 (BMPlayer *player_stat);
_EXTERN void special_init_nasty_walls (BMPlayer *player_stat);
_EXTERN void special_init_nasty_walls_2 (BMPlayer *player_stat);
#else
_EXTERN void special_init_void ();
_EXTERN void special_init_special_bombs_30 ();
_EXTERN void special_init_nasty_walls ();
_EXTERN void special_init_nasty_walls_2 ();
#endif

/* 
 * special game_fucntions 
 */
#ifdef __STDC__
_EXTERN void special_game_void (void);
_EXTERN void special_game_nasty_walls (void);
_EXTERN void special_game_haunt (void);
_EXTERN void special_game_haunt_fast (void);
#else
_EXTERN void special_game_void ();
_EXTERN void special_game_nasty_walls ();
_EXTERN void special_game_haunt ();
_EXTERN void special_game_haunt_fast ();
#endif

/* 
 * special extra_functions 
 */
#ifdef __STDC__
_EXTERN void special_extra_void (BMPlayer *ps);
_EXTERN void special_extra_invincible (BMPlayer *ps);
_EXTERN void special_extra_kick (BMPlayer *ps);
_EXTERN void special_extra_teleport (BMPlayer *ps);
_EXTERN void special_extra_RC (BMPlayer *ps);
_EXTERN void special_extra_ignite_all (BMPlayer *ps);
_EXTERN void special_extra_air (BMPlayer *ps);
_EXTERN void special_extra_special_bomb (BMPlayer *ps);
_EXTERN void special_extra_junkie (BMPlayer *ps);
#else
_EXTERN void special_extra_void ();
_EXTERN void special_extra_invincible ();
_EXTERN void special_extra_kick ();
_EXTERN void special_extra_teleport ();
_EXTERN void special_extra_RC ();
_EXTERN void special_extra_ignite_all ();
_EXTERN void special_extra_air ();
_EXTERN void special_extra_special_bomb ();
_EXTERN void special_extra_junkie ();
#endif

/* 
 * special key_functions 
 */
#ifdef __STDC__
_EXTERN void special_key_void (BMPlayer *ps);
_EXTERN void special_key_RC (BMPlayer *ps);
_EXTERN void special_key_teleport (BMPlayer *ps);
_EXTERN void special_key_air (BMPlayer *ps);
_EXTERN void special_key_special_bomb (BMPlayer *ps);
#else
_EXTERN void special_key_void ();
_EXTERN void special_key_RC ();
_EXTERN void special_key_teleport ();
_EXTERN void special_key_air ();
_EXTERN void special_key_special_bomb ();
#endif

#undef _EXTERN

#endif
/*
 * end of file func.h
 */
