/*
 * Programm XBLAST V2.2.1 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 26th, 1997
 * started August 1993
 *
 * File: data.h
 * include file for data.c
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as by published
 * by the Free Software Foundation; either version 3; or (at your option)
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

#ifndef _DATA_H
#define _DATA_H



#include "include.h"

#include "mytypes.h"

/*
 * global variables
 */
#ifndef _DATA_C
extern BMLevelData *internal_maze_data[];
extern BMLevelData *score_board_data[];
extern BMLevelData *winning_data[];
#endif

/* 
 * game mode flags for levels 
 */
#define GM_Random     (1<<0)
#define GM_2_Player   (1<<1)
#define GM_3_Player   (1<<2)
#define GM_4_Player   (1<<3)
#define GM_5_Player   (1<<4)
#define GM_6_Player   (1<<5)
#define GM_NoGrid     (1<<6) 
#define GM_Single     (1<<7)
#define GM_Team       (1<<8)
#define GM_Double     (1<<9)

#define GM_234_Player   (GM_2_Player|GM_3_Player|GM_4_Player)
#define GM_2345_Player  (GM_234_Player|GM_5_Player)
#define GM_23456_Player (GM_2345_Player|GM_6_Player)

#define GM_All        (GM_Single|GM_Team|GM_Double)

#define NUM_GM        9

/*
 * init flags for extras
 */
#define IF_None     0
#define IF_Kick     (1<<0)
#define IF_RC       (1<<1)
#define IF_Teleport (1<<2)
#define IF_Airpump  (1<<3)

#define NUM_IF 4

  /* extras */

#define EXNone 0

/*
 * scramble macro
 */
#define SCRAMBLE_VOID {GAME_TIME+1, 0, NULL}

/*
 * macros for extras
 */
#define EXTRA_BOMB        { BLBomb,         "Black", "Red",         "White" }
#define EXTRA_RANGE       { BLRange,        "Black", "Yellow",      "White" }
#define EXTRA_TRAP        { BLTrap,         "Black", "Cyan",        "White" }
#define EXTRA_KICK        { BLKick,         "Black", "Blue",        "White" }
#define EXTRA_INVINC      { BLInvincible,   "Black", "Gold",        "White" }
#define EXTRA_BUTTON      { BLIgnite,       "Black", "DeepPink",    "White" }
#define EXTRA_CONSTR      { BLConstruction, "Black", "Firebrick1",  "White" }
#define EXTRA_RC          { BLRemoteControl,"Black", "SpringGreen", "White" }
#define EXTRA_BEAM        { BLQ3aBeam,      "Black", "Orchid",      "White" }
#define EXTRA_AIRPUMP     { BLAirPump,      "Black", "SkyBlue",     "White" }
#define EXTRA_NAPALM      { BLNapalm,       "Black", "OrangeRed",   "White" }
#define EXTRA_FIRECRACKER { BLFirecracker,  "Black", "Orange",      "White" }
#define EXTRA_SYRINGE     { BLSyringe,      "Black", "YellowGreen", "White" }

#endif
/*
 * end of file data.h
 */



