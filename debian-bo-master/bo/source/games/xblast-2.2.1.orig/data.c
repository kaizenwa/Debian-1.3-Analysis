/*
 * Program XBLAST V2.1.9 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * September 25th 1996
 * started August 1993
 *
 * File: data.c
 * level data 
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
 * Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#define _DATA_C

#include <stdio.h>
#include <stdlib.h>

#include "include.h"
#include "mytypes.h"
#include "const.h"
#include "block.h"
#include "func.h"
#include "data.h"
#include "shrink.h"
#include "bomb.h"
#include "main.h"

#define _ BTFree
#define B BTBlock
#define R BTBlockRise
#define X BTExtra
#define b BTBomb
#define r BTRange
#define s BTSick
#define q BTSpecial
#define v BTVoid
#define e BTEvil 
#define V -1

/*
 * default void for scrambling blocks
 */

#include "level/Seek_N_Destroy.h"
#include "level/Treasure_Hunt.h"
#include "level/Shrinking_World.h"
#include "level/XBlast_2000.h"
#include "level/Full_Power.h"
#include "level/Hexagon_Excitation.h"
#include "level/Running_Man.h"
#include "level/Legoland.h"
#include "level/Paradise_City.h"
#include "level/Halloween.h"
#include "level/Gravitation.h"
#include "level/Duel_In_The_Sun.h"
#include "level/Beamania.h"

#include "level/ChainReaction.h"
#include "level/Nowhere2run.h"
#include "level/Haunted_House.h"
#include "level/Four_Corners.h"
#include "level/Cave_In.h"
#include "level/Mind_Games.h"

#include "level/Suicide.h"
#include "level/Indiana_Jones.h"
#include "level/Hallways.h"

#include "level/Losange_Over_excitation.h"

#include "level/Mr_Beam.h"

#include "level/Shrinking_Arena.h"
  
#include "level/Gold_Rush.h"
#include "level/Survivor.h"
#include "level/Inv_X_Ible.h"
#include "level/Tic_Tac_Toe.h"
#include "level/Je_M_Appelle_Rico.h"
#include "level/Toutencarton.h"
#include "level/Born_To_Be_Kill.h"
  
#include "level/Peppercorn.h"
#include "level/Closet_Psycho.h"
#include "level/LaBoom.h"
#include "level/TwoBeTwo.h"
#include "level/Footwork.h"
#include "level/Hall_of_Snooker.h"
#include "level/Contact_Sports.h"
#include "level/Popcorn.h"
#include "level/NapalmJustice.h"
#include "level/FantasyLand.h"
#include "level/Survival.h"
#include "level/FryingPan.h"
#include "level/BricksFries.h"
#include "level/FireSurprise.h"
#include "level/Spindizzy.h"
#include "level/NothingShort.h"
#include "level/YourAge.h"
#include "level/SpaceHead.h"
#include "level/BeAJunkie.h"
#include "level/HotStuff.h"
#include "level/SkyShow.h"

#include "level/Bouncy.h"
#include "level/WatchBanana.h"

#include "level/Fungus_Fun.h"
#include "level/Open_Warfare.h"
#include "level/Get_Your_Kit_Out.h"
#include "level/Moonwalking.h"
#include "level/Pot_Luck.h"

/* No new levels beyond this point */



BMLevelData *internal_maze_data[] = {
  &Seek_N_Destroy,
  &Treasure_Hunt,
  &Shrinking_World,
  &XBlast_2000,
  &Full_Power,
  &Hexagon_Excitation,
  &Running_Man,
  &Legoland,
  &Paradise_City,
  &Halloween,
  &Gravitation,
  &Duel_In_The_Sun,
  &BlastFree,
  
  /* contributed levels*/

  &ChainReaction,
  &Nowhere2run,
  &Haunted_House,
  &Four_Corners,
  &Cave_In,
  &Mind_Games,

  &Suicide,
  &Indiana_Jones,
  &Hallways,

  &Losange_Over_excitation,

  &Mr_Beam,

  &Shrinking_Arena,
  
  &Gold_Rush,
  &Survivor,
  &Inv_X_Ible,
  &Tic_Tac_Toe,
  &Je_M_Appelle_Rico,
  &Toutencarton,
  &Born_To_Be_Kill,
  
  &Peppercorn,
  &Closet_Psycho,
  &LaBoom,
  &TwoBeTwo,
  &Footwork,
  &Hall_of_Snooker,
  &Contact_Sports,
  &Popcorn,
  &NapalmJustice,
  &FantasyLand,
  &Survival,
  &FryingPan,
  &BricksFries,
  &FireSurprise,
  &Spindizzy,
  &NothingShort,
  &YourAge,
  &SpaceHead,
  &BeAJunkie,
  &HotStuff,
  &SkyShow,

  &BouncyBouncy,
  &WatchBanana,

  &Fungus_Fun,
  &Open_Warfare,
  &Get_Your_Kit_Out,
  &Moonwalking,
  &Pot_Luck,

  /* No new levels beyond this point */
  NULL,
};

#include "level/ScoreBoard2.h"
#include "level/ScoreBoard3.h"
#include "level/ScoreBoard4.h"
#include "level/ScoreBoard5.h"
#include "level/ScoreBoard6.h"
#include "level/ScoreBoard2x2.h"
#include "level/ScoreBoard3x2.h"

BMLevelData *score_board_data[] = {
  &ScoreBoard2,
  &ScoreBoard3,
  &ScoreBoard4,
  &ScoreBoard5,
  &ScoreBoard6,
  &ScoreBoard2x2,
  &ScoreBoard3x2,
  NULL,
};

#include "level/Congratulations2.h"
#include "level/Congratulations3.h"
#include "level/Congratulations4.h"
#include "level/Congratulations5.h"
#include "level/Congratulations6.h"
#include "level/Congratulations2x2.h"
#include "level/Congratulations3x2.h"
#include "level/CongratulationsD2.h"
#include "level/CongratulationsD3.h"
#include "level/CongratulationsD4.h"


BMLevelData *winning_data[] = {
  &Congratulations2,
  &Congratulations3,
  &Congratulations4,
  &Congratulations5,
  &Congratulations6,
  &Congratulations2x2,
  &Congratulations3x2,
  &CongratulationsD2,
  &CongratulationsD3,
  &CongratulationsD4,
  NULL,
};


/* This is important */

#undef _ 
#undef B 
#undef X 
#undef b 
#undef r 
#undef s 
#undef q 

/*
 * end of file data.c
 */

