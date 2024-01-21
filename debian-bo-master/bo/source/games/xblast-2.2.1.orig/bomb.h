/*
 * Program XBLAST V2.1.8 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * September 8th 1996
 * started August 1993
 *
 * File: bomb.h
 * include file for bomb.c
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

#ifndef _BOMB_H
#define _BOMB_H

/*
 * prototypes
 */

#ifdef _MAZE_C
#define _EXTERN
#else
#define _EXTERN extern
#endif

#ifdef __STDC__
_EXTERN void setup_bombs (BMBombData *cur_level);
_EXTERN void bomb_click_none (Explosion *bomb);
_EXTERN void bomb_click_initial (Explosion *bomb);
_EXTERN void bomb_click_thru (Explosion *bomb);
_EXTERN void bomb_click_snooker (Explosion *bomb);
_EXTERN void bomb_click_contact (Explosion *bomb);
_EXTERN void bomb_click_clockwise (Explosion *bomb);
_EXTERN void bomb_click_anticlockwise (Explosion *bomb);
_EXTERN void bomb_click_randomdir (Explosion *bomb);
_EXTERN void bomb_click_rebound (Explosion *bomb);
_EXTERN void do_bombs (void);
_EXTERN int ignite_players_bombs (BMPlayer *ps);
_EXTERN int ignite_all_bombs (void);
_EXTERN void ignite_bombs (void);
_EXTERN int do_explosions (void);
_EXTERN int new_player_bomb (BMPlayer *ps, BMBombType type);
_EXTERN int new_evil_bomb (int x, int y);
_EXTERN int new_nasty_bomb (int x, int y, int range, BMDirection dir);
_EXTERN void stun_players (BMPlayer *ps, int num_player);
_EXTERN int check_bomb (int x, int y);
_EXTERN int number_of_explosions (void);
_EXTERN void delete_bomb_at (int x, int y);
_EXTERN void move_bomb (int x, int y, int dir);
_EXTERN int check_distrib_expl (short dist_extra[MAZE_W][MAZE_H], int free_blocks);
_EXTERN void haunt_kick (int prob);
_EXTERN void do_air (BMPlayer *ps);
#else
_EXTERN void setup_bombs ();
_EXTERN void bomb_click_none ();
_EXTERN void bomb_click_initial ();
_EXTERN void bomb_click_thru ();
_EXTERN void bomb_click_snooker ();
_EXTERN void bomb_click_contact ();
_EXTERN void bomb_click_clockwise ();
_EXTERN void bomb_click_anticlockwise ();
_EXTERN void bomb_click_randomdir ();
_EXTERN void bomb_click_rebound ();
_EXTERN void do_bombs ();
_EXTERN int ignite_players_bombs ();
_EXTERN int ignite_all_bombs ();
_EXTERN void ignite_bombs ();
_EXTERN int do_explosions ();
_EXTERN int new_player_bomb ();
_EXTERN int new_evil_bomb ();
_EXTERN int new_nasty_bomb ();
_EXTERN void stun_players ();
_EXTERN int check_bomb ();
_EXTERN int number_of_explosions ();
_EXTERN void delete_bomb_at ();
_EXTERN void move_bomb ();
_EXTERN int check_distrib_expl ();
_EXTERN void haunt_kick ();
_EXTERN void do_air ();
#endif

#undef _EXTERN

/*
 * bomb click flags 
 */
#define BC_None          0
#define BC_Rebound       1
#define BC_Contact       2
#define BC_Clockwise     3
#define BC_Anticlockwise 4
#define BC_Random        5
#define BC_Snooker       6

#define WC_None          0
#define WC_Rebound       1
#define WC_Contact       2
#define WC_Clockwise     3
#define WC_Anticlockwise 4
#define WC_Random        5

#define PC_StunStop      0
#define PC_StunThruInit  1
#define PC_StunThru      2
#define PC_Contact       3
#define PC_Rebound       4

/* fuse times */

#define BOMB_TIME 64
#define SHORT_FUSE 32
#define LONG_FUSE 128


/* Bomb types */

#define BMTdefault -1
#define BMTspecial -2
#define BMTevil    -3

#define BMTnormal 0
#define BMTnapalm 1
#define BMTblastnow 2
#define BMTclose 3
#define BMTfirecracker 4
#define BMTfirecracker2 5
#define BMTconstruction 6
#define BMTthreebombs 7
#define BMTgrenade 8
#define BMTtrianglebombs 9
#define BMTdestruction 10
#define BMTfungus 11
#define BMTrenovation 12
#define BMTpyro 13
#define BMTpyro2 14
#define BMTrandom 15

  /* Fuse lengths */

#define FUSEshort 0
#define FUSEnormal 1
#define FUSElong 2

#define NUM_FUSES 3

#endif
/*
 * end of file bomb.h
 */






























