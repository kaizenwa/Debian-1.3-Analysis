/*
 * Programm XBLAST V2.1.9 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * October 13th 1996
 * started August 1993
 *
 * File: score.c
 * bitmap data of status panel
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

#define _SCORE_C

#include "include.h"
#include "mytypes.h"
#include "score.h"

#include "bitmap/score/led_off.xbm"
#include "bitmap/score/led_on.xbm"

BitmapStruct score_led[2] = {
  { led_off_width, led_off_height, led_off_bits },
  { led_on_width, led_on_height, led_on_bits },
};

#include "bitmap/color/score/led_off_A.xbm"
#include "bitmap/color/score/led_on_A.xbm"

BitmapStruct score_led_addon[2] = {
  { led_off_A_width, led_off_A_height, led_off_A_bits },
  { led_on_A_width, led_on_A_height, led_on_A_bits },
};

#include "bitmap/score/tile_void.xbm"
#include "bitmap/score/text_left.xbm"
#include "bitmap/score/text_middle.xbm"
#include "bitmap/score/text_right.xbm"
#include "bitmap/score/player_1_dead.xbm"
#include "bitmap/score/player_2_dead.xbm"
#include "bitmap/score/player_3_dead.xbm"
#include "bitmap/score/player_4_dead.xbm"
#include "bitmap/score/player_5_dead.xbm"
#include "bitmap/score/player_6_dead.xbm"
#include "bitmap/score/player_1_sick.xbm"
#include "bitmap/score/player_2_sick.xbm"
#include "bitmap/score/player_3_sick.xbm"
#include "bitmap/score/player_4_sick.xbm"
#include "bitmap/score/player_5_sick.xbm"
#include "bitmap/score/player_6_sick.xbm"
#include "bitmap/score/player_1.xbm"
#include "bitmap/score/player_2.xbm"
#include "bitmap/score/player_3.xbm"
#include "bitmap/score/player_4.xbm"
#include "bitmap/score/player_5.xbm"
#include "bitmap/score/player_6.xbm"

BitmapStruct score_tile[MAX_SCORE_TILES] = {
  { tile_void_width, tile_void_height, tile_void_bits },
  { text_left_width, text_left_height, text_left_bits },
  { text_middle_width, text_middle_height, text_middle_bits },
  { text_right_width, text_right_height, text_right_bits },
  { player_1_dead_width, player_1_dead_height, player_1_dead_bits },
  { player_2_dead_width, player_2_dead_height, player_2_dead_bits },
  { player_3_dead_width, player_3_dead_height, player_3_dead_bits },
  { player_4_dead_width, player_4_dead_height, player_4_dead_bits },
  { player_5_dead_width, player_5_dead_height, player_5_dead_bits },
  { player_6_dead_width, player_6_dead_height, player_6_dead_bits },
  { player_1_sick_width, player_1_sick_height, player_1_sick_bits },
  { player_2_sick_width, player_2_sick_height, player_2_sick_bits },
  { player_3_sick_width, player_3_sick_height, player_3_sick_bits },
  { player_4_sick_width, player_4_sick_height, player_4_sick_bits },
  { player_5_sick_width, player_5_sick_height, player_5_sick_bits },
  { player_6_sick_width, player_6_sick_height, player_6_sick_bits },
  { player_1_width, player_1_height, player_1_bits },
  { player_2_width, player_2_height, player_2_bits },
  { player_3_width, player_3_height, player_3_bits },
  { player_4_width, player_4_height, player_4_bits },
  { player_5_width, player_5_height, player_5_bits },
  { player_6_width, player_6_height, player_6_bits },
};


#include "bitmap/color/score/player_dead_W.xbm"
#include "bitmap/color/score/player_dead_R.xbm"
#include "bitmap/color/score/player_dead_G.xbm"
#include "bitmap/color/score/player_sick_W.xbm"
#include "bitmap/color/score/player_sick_R.xbm"
#include "bitmap/color/score/player_sick_G.xbm"
#include "bitmap/color/score/player_W.xbm"
#include "bitmap/color/score/player_R.xbm"
#include "bitmap/color/score/player_G.xbm"

BitmapStruct score_tile_addon[MAX_SCORE_ADDON][SCORE_DEPTH] = {
  {
    { player_dead_W_width, player_dead_W_height, player_dead_W_bits },
    { player_dead_R_width, player_dead_R_height, player_dead_R_bits },
    { player_dead_G_width, player_dead_G_height, player_dead_G_bits },
  },
  {
    { player_sick_W_width, player_sick_W_height, player_sick_W_bits },
    { player_sick_R_width, player_sick_R_height, player_sick_R_bits },
    { player_sick_G_width, player_sick_G_height, player_sick_G_bits },
  },
  {
    { player_W_width, player_W_height, player_W_bits },
    { player_R_width, player_R_height, player_R_bits },
    { player_G_width, player_G_height, player_G_bits },
  },
};

/*
 * end of file score.c
 */
