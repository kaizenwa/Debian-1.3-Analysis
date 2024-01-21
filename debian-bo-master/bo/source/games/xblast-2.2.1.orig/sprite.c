/*
 * Programm XBLAST V2.1.8 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * September 18th 1996
 * started August 1993
 *
 * File: sprite.c
 * bitmap data of player sprites
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

#define _SPRITE_C

#include "include.h"
#include "mytypes.h"

#include "sprite.h"

/*
 * bitmap data for player 
 */

#include "bitmap/sprite/player_mask_1.xbm"
#include "bitmap/sprite/player_mask_2.xbm"
#include "bitmap/sprite/player_mask_3.xbm"
#include "bitmap/sprite/player_mask_4.xbm"
#include "bitmap/sprite/player_mask_5.xbm"
#include "bitmap/sprite/player_mask_6.xbm"
#include "bitmap/sprite/player_mask_7.xbm"
#include "bitmap/sprite/player_mask_8.xbm"
#include "bitmap/sprite/player_mask_9.xbm"
#include "bitmap/sprite/player_mask_10.xbm"
#include "bitmap/sprite/player_mask_11.xbm"
#include "bitmap/sprite/player_mask_12.xbm"
#include "bitmap/sprite/player_mask_13.xbm"
#include "bitmap/sprite/player_mask_14.xbm"
#include "bitmap/sprite/player_mask_15.xbm"
#include "bitmap/sprite/winner_mask.xbm"
#include "bitmap/sprite/big_mask.xbm"

BitmapStruct sprite_mask[MAX_ANIME] =
{
  { player_mask_1_width, player_mask_1_height, player_mask_1_bits },
  { player_mask_2_width, player_mask_2_height, player_mask_2_bits },
  { player_mask_3_width, player_mask_3_height, player_mask_3_bits },
  { player_mask_4_width, player_mask_4_height, player_mask_4_bits },
  { player_mask_5_width, player_mask_5_height, player_mask_5_bits },
  { player_mask_6_width, player_mask_6_height, player_mask_6_bits },
  { player_mask_7_width, player_mask_7_height, player_mask_7_bits },
  { player_mask_8_width, player_mask_8_height, player_mask_8_bits },
  { player_mask_9_width, player_mask_9_height, player_mask_9_bits },
  { player_mask_10_width, player_mask_10_height, player_mask_10_bits },
  { player_mask_11_width, player_mask_11_height, player_mask_11_bits },
  { player_mask_12_width, player_mask_12_height, player_mask_12_bits },
  { player_mask_13_width, player_mask_13_height, player_mask_13_bits },
  { player_mask_14_width, player_mask_14_height, player_mask_14_bits },
  { player_mask_15_width, player_mask_15_height, player_mask_15_bits },
  { winner_mask_width, winner_mask_height, winner_mask_bits },
  { big_mask_width, big_mask_height, big_mask_bits },
};

#include "bitmap/sprite/player_1_1.xbm"
#include "bitmap/sprite/player_1_2.xbm"
#include "bitmap/sprite/player_1_3.xbm"
#include "bitmap/sprite/player_1_4.xbm"
#include "bitmap/sprite/player_1_5.xbm"
#include "bitmap/sprite/player_1_6.xbm"
#include "bitmap/sprite/player_1_7.xbm"
#include "bitmap/sprite/player_1_8.xbm"
#include "bitmap/sprite/player_1_9.xbm"
#include "bitmap/sprite/player_1_10.xbm"
#include "bitmap/sprite/player_1_11.xbm"
#include "bitmap/sprite/player_1_12.xbm"
#include "bitmap/sprite/player_1_13.xbm"
#include "bitmap/sprite/player_1_14.xbm"
#include "bitmap/sprite/player_1_15.xbm"
#include "bitmap/sprite/winner_1.xbm"
#include "bitmap/sprite/big_1.xbm"

#include "bitmap/sprite/player_2_1.xbm"
#include "bitmap/sprite/player_2_2.xbm"
#include "bitmap/sprite/player_2_3.xbm"
#include "bitmap/sprite/player_2_4.xbm"
#include "bitmap/sprite/player_2_5.xbm"
#include "bitmap/sprite/player_2_6.xbm"
#include "bitmap/sprite/player_2_7.xbm"
#include "bitmap/sprite/player_2_8.xbm"
#include "bitmap/sprite/player_2_9.xbm"
#include "bitmap/sprite/player_2_10.xbm"
#include "bitmap/sprite/player_2_11.xbm"
#include "bitmap/sprite/player_2_12.xbm"
#include "bitmap/sprite/player_2_13.xbm"
#include "bitmap/sprite/player_2_14.xbm"
#include "bitmap/sprite/player_2_15.xbm"
#include "bitmap/sprite/winner_2.xbm"
#include "bitmap/sprite/big_2.xbm"

#include "bitmap/sprite/player_3_1.xbm"
#include "bitmap/sprite/player_3_2.xbm"
#include "bitmap/sprite/player_3_3.xbm"
#include "bitmap/sprite/player_3_4.xbm"
#include "bitmap/sprite/player_3_5.xbm"
#include "bitmap/sprite/player_3_6.xbm"
#include "bitmap/sprite/player_3_7.xbm"
#include "bitmap/sprite/player_3_8.xbm"
#include "bitmap/sprite/player_3_9.xbm"
#include "bitmap/sprite/player_3_10.xbm"
#include "bitmap/sprite/player_3_11.xbm"
#include "bitmap/sprite/player_3_12.xbm"
#include "bitmap/sprite/player_3_13.xbm"
#include "bitmap/sprite/player_3_14.xbm"
#include "bitmap/sprite/player_3_15.xbm"
#include "bitmap/sprite/winner_3.xbm"
#include "bitmap/sprite/big_3.xbm"

#include "bitmap/sprite/player_4_1.xbm"
#include "bitmap/sprite/player_4_2.xbm"
#include "bitmap/sprite/player_4_3.xbm"
#include "bitmap/sprite/player_4_4.xbm"
#include "bitmap/sprite/player_4_5.xbm"
#include "bitmap/sprite/player_4_6.xbm"
#include "bitmap/sprite/player_4_7.xbm"
#include "bitmap/sprite/player_4_8.xbm"
#include "bitmap/sprite/player_4_9.xbm"
#include "bitmap/sprite/player_4_10.xbm"
#include "bitmap/sprite/player_4_11.xbm"
#include "bitmap/sprite/player_4_12.xbm"
#include "bitmap/sprite/player_4_13.xbm"
#include "bitmap/sprite/player_4_14.xbm"
#include "bitmap/sprite/player_4_15.xbm"
#include "bitmap/sprite/winner_4.xbm"
#include "bitmap/sprite/big_4.xbm"

#include "bitmap/sprite/player_5_1.xbm"
#include "bitmap/sprite/player_5_2.xbm"
#include "bitmap/sprite/player_5_3.xbm"
#include "bitmap/sprite/player_5_4.xbm"
#include "bitmap/sprite/player_5_5.xbm"
#include "bitmap/sprite/player_5_6.xbm"
#include "bitmap/sprite/player_5_7.xbm"
#include "bitmap/sprite/player_5_8.xbm"
#include "bitmap/sprite/player_5_9.xbm"
#include "bitmap/sprite/player_5_10.xbm"
#include "bitmap/sprite/player_5_11.xbm"
#include "bitmap/sprite/player_5_12.xbm"
#include "bitmap/sprite/player_5_13.xbm"
#include "bitmap/sprite/player_5_14.xbm"
#include "bitmap/sprite/player_5_15.xbm"
#include "bitmap/sprite/winner_5.xbm"
#include "bitmap/sprite/big_5.xbm"


#include "bitmap/sprite/player_6_1.xbm"
#include "bitmap/sprite/player_6_2.xbm"
#include "bitmap/sprite/player_6_3.xbm"
#include "bitmap/sprite/player_6_4.xbm"
#include "bitmap/sprite/player_6_5.xbm"
#include "bitmap/sprite/player_6_6.xbm"
#include "bitmap/sprite/player_6_7.xbm"
#include "bitmap/sprite/player_6_8.xbm"
#include "bitmap/sprite/player_6_9.xbm"
#include "bitmap/sprite/player_6_10.xbm"
#include "bitmap/sprite/player_6_11.xbm"
#include "bitmap/sprite/player_6_12.xbm"
#include "bitmap/sprite/player_6_13.xbm"
#include "bitmap/sprite/player_6_14.xbm"
#include "bitmap/sprite/player_6_15.xbm"
#include "bitmap/sprite/winner_6.xbm"
#include "bitmap/sprite/big_6.xbm"


BitmapStruct sprite_bits[MAX_PLAYER][MAX_ANIME] =
{
  {
    { player_1_1_width, player_1_1_height, player_1_1_bits },
    { player_1_2_width, player_1_2_height, player_1_2_bits },
    { player_1_3_width, player_1_3_height, player_1_3_bits },
    { player_1_4_width, player_1_4_height, player_1_4_bits },
    { player_1_5_width, player_1_5_height, player_1_5_bits },
    { player_1_6_width, player_1_6_height, player_1_6_bits },
    { player_1_7_width, player_1_7_height, player_1_7_bits },
    { player_1_8_width, player_1_8_height, player_1_8_bits },
    { player_1_9_width, player_1_9_height, player_1_9_bits },
    { player_1_10_width, player_1_10_height, player_1_10_bits },
    { player_1_11_width, player_1_11_height, player_1_11_bits },
    { player_1_12_width, player_1_12_height, player_1_12_bits },
    { player_1_13_width, player_1_13_height, player_1_13_bits },
    { player_1_14_width, player_1_14_height, player_1_14_bits },
    { player_1_15_width, player_1_15_height, player_1_15_bits },
    { winner_1_width, winner_1_height, winner_1_bits },
    { big_1_width, big_1_height, big_1_bits },
  },
  {
    { player_2_1_width, player_2_1_height, player_2_1_bits },
    { player_2_2_width, player_2_2_height, player_2_2_bits },
    { player_2_3_width, player_2_3_height, player_2_3_bits },
    { player_2_4_width, player_2_4_height, player_2_4_bits },
    { player_2_5_width, player_2_5_height, player_2_5_bits },
    { player_2_6_width, player_2_6_height, player_2_6_bits },
    { player_2_7_width, player_2_7_height, player_2_7_bits },
    { player_2_8_width, player_2_8_height, player_2_8_bits },
    { player_2_9_width, player_2_9_height, player_2_9_bits },
    { player_2_10_width, player_2_10_height, player_2_10_bits },
    { player_2_11_width, player_2_11_height, player_2_11_bits },
    { player_2_12_width, player_2_12_height, player_2_12_bits },
    { player_2_13_width, player_2_13_height, player_2_13_bits },
    { player_2_14_width, player_2_14_height, player_2_14_bits },
    { player_2_15_width, player_2_15_height, player_2_15_bits },
    { winner_2_width, winner_2_height, winner_2_bits },
    { big_2_width, big_2_height, big_2_bits },
  },
  {
    { player_3_1_width, player_3_1_height, player_3_1_bits },
    { player_3_2_width, player_3_2_height, player_3_2_bits },
    { player_3_3_width, player_3_3_height, player_3_3_bits },
    { player_3_4_width, player_3_4_height, player_3_4_bits },
    { player_3_5_width, player_3_5_height, player_3_5_bits },
    { player_3_6_width, player_3_6_height, player_3_6_bits },
    { player_3_7_width, player_3_7_height, player_3_7_bits },
    { player_3_8_width, player_3_8_height, player_3_8_bits },
    { player_3_9_width, player_3_9_height, player_3_9_bits },
    { player_3_10_width, player_3_10_height, player_3_10_bits },
    { player_3_11_width, player_3_11_height, player_3_11_bits },
    { player_3_12_width, player_3_12_height, player_3_12_bits },
    { player_3_13_width, player_3_13_height, player_3_13_bits },
    { player_3_14_width, player_3_14_height, player_3_14_bits },
    { player_3_15_width, player_3_15_height, player_3_15_bits },
    { winner_3_width, winner_3_height, winner_3_bits },
    { big_3_width, big_3_height, big_3_bits },
  },
  {
    { player_4_1_width, player_4_1_height, player_4_1_bits },
    { player_4_2_width, player_4_2_height, player_4_2_bits },
    { player_4_3_width, player_4_3_height, player_4_3_bits },
    { player_4_4_width, player_4_4_height, player_4_4_bits },
    { player_4_5_width, player_4_5_height, player_4_5_bits },
    { player_4_6_width, player_4_6_height, player_4_6_bits },
    { player_4_7_width, player_4_7_height, player_4_7_bits },
    { player_4_8_width, player_4_8_height, player_4_8_bits },
    { player_4_9_width, player_4_9_height, player_4_9_bits },
    { player_4_10_width, player_4_10_height, player_4_10_bits },
    { player_4_11_width, player_4_11_height, player_4_11_bits },
    { player_4_12_width, player_4_12_height, player_4_12_bits },
    { player_4_13_width, player_4_13_height, player_4_13_bits },
    { player_4_14_width, player_4_14_height, player_4_14_bits },
    { player_4_15_width, player_4_15_height, player_4_15_bits },
    { winner_4_width, winner_4_height, winner_4_bits },
    { big_4_width, big_4_height, big_4_bits },
  },
  {
    { player_5_1_width, player_5_1_height, player_5_1_bits },
    { player_5_2_width, player_5_2_height, player_5_2_bits },
    { player_5_3_width, player_5_3_height, player_5_3_bits },
    { player_5_4_width, player_5_4_height, player_5_4_bits },
    { player_5_5_width, player_5_5_height, player_5_5_bits },
    { player_5_6_width, player_5_6_height, player_5_6_bits },
    { player_5_7_width, player_5_7_height, player_5_7_bits },
    { player_5_8_width, player_5_8_height, player_5_8_bits },
    { player_5_9_width, player_5_9_height, player_5_9_bits },
    { player_5_10_width, player_5_10_height, player_5_10_bits },
    { player_5_11_width, player_5_11_height, player_5_11_bits },
    { player_5_12_width, player_5_12_height, player_5_12_bits },
    { player_5_13_width, player_5_13_height, player_5_13_bits },
    { player_5_14_width, player_5_14_height, player_5_14_bits },
    { player_5_15_width, player_5_15_height, player_5_15_bits },
    { winner_5_width, winner_5_height, winner_5_bits },
    { big_5_width, big_5_height, big_5_bits },
  },
  {
    { player_6_1_width, player_6_1_height, player_6_1_bits },
    { player_6_2_width, player_6_2_height, player_6_2_bits },
    { player_6_3_width, player_6_3_height, player_6_3_bits },
    { player_6_4_width, player_6_4_height, player_6_4_bits },
    { player_6_5_width, player_6_5_height, player_6_5_bits },
    { player_6_6_width, player_6_6_height, player_6_6_bits },
    { player_6_7_width, player_6_7_height, player_6_7_bits },
    { player_6_8_width, player_6_8_height, player_6_8_bits },
    { player_6_9_width, player_6_9_height, player_6_9_bits },
    { player_6_10_width, player_6_10_height, player_6_10_bits },
    { player_6_11_width, player_6_11_height, player_6_11_bits },
    { player_6_12_width, player_6_12_height, player_6_12_bits },
    { player_6_13_width, player_6_13_height, player_6_13_bits },
    { player_6_14_width, player_6_14_height, player_6_14_bits },
    { player_6_15_width, player_6_15_height, player_6_15_bits },
    { winner_6_width, winner_6_height, winner_6_bits },
    { big_6_width, big_6_height, big_6_bits },
  },
};

/*
 * add on bitmaps for sprites
 */
#include "bitmap/color/sprite/player_1_W.xbm"
#include "bitmap/color/sprite/player_1_R.xbm"
#include "bitmap/color/sprite/player_1_G.xbm"
#include "bitmap/color/sprite/player_1_B.xbm"
#include "bitmap/color/sprite/player_2_W.xbm"
#include "bitmap/color/sprite/player_2_R.xbm"
#include "bitmap/color/sprite/player_2_G.xbm"
#include "bitmap/color/sprite/player_2_B.xbm"
#include "bitmap/color/sprite/player_3_W.xbm"
#include "bitmap/color/sprite/player_3_R.xbm"
#include "bitmap/color/sprite/player_3_G.xbm"
#include "bitmap/color/sprite/player_3_B.xbm"
#include "bitmap/color/sprite/player_4_W.xbm"
#include "bitmap/color/sprite/player_4_R.xbm"
#include "bitmap/color/sprite/player_4_G.xbm"
#include "bitmap/color/sprite/player_4_B.xbm"
#include "bitmap/color/sprite/player_5_W.xbm"
#include "bitmap/color/sprite/player_5_R.xbm"
#include "bitmap/color/sprite/player_5_G.xbm"
#include "bitmap/color/sprite/player_5_B.xbm"
#include "bitmap/color/sprite/player_6_W.xbm"
#include "bitmap/color/sprite/player_6_R.xbm"
#include "bitmap/color/sprite/player_6_G.xbm"
#include "bitmap/color/sprite/player_6_B.xbm"
#include "bitmap/color/sprite/player_7_W.xbm"
#include "bitmap/color/sprite/player_7_R.xbm"
#include "bitmap/color/sprite/player_7_G.xbm"
#include "bitmap/color/sprite/player_7_B.xbm"
#include "bitmap/color/sprite/player_8_W.xbm"
#include "bitmap/color/sprite/player_8_R.xbm"
#include "bitmap/color/sprite/player_8_G.xbm"
#include "bitmap/color/sprite/player_8_B.xbm"
#include "bitmap/color/sprite/player_9_W.xbm"
#include "bitmap/color/sprite/player_9_R.xbm"
#include "bitmap/color/sprite/player_9_G.xbm"
#include "bitmap/color/sprite/player_9_B.xbm"
#include "bitmap/color/sprite/player_10_W.xbm"
#include "bitmap/color/sprite/player_10_R.xbm"
#include "bitmap/color/sprite/player_10_G.xbm"
#include "bitmap/color/sprite/player_10_B.xbm"
#include "bitmap/color/sprite/player_11_W.xbm"
#include "bitmap/color/sprite/player_11_R.xbm"
#include "bitmap/color/sprite/player_11_G.xbm"
#include "bitmap/color/sprite/player_11_B.xbm"
#include "bitmap/color/sprite/player_12_W.xbm"
#include "bitmap/color/sprite/player_12_R.xbm"
#include "bitmap/color/sprite/player_12_G.xbm"
#include "bitmap/color/sprite/player_12_B.xbm"
#include "bitmap/color/sprite/player_14_W.xbm"
#include "bitmap/color/sprite/player_14_R.xbm"
#include "bitmap/color/sprite/player_14_G.xbm"
#include "bitmap/color/sprite/player_14_B.xbm"
#include "bitmap/color/sprite/player_15_W.xbm"
#include "bitmap/color/sprite/player_15_R.xbm"
#include "bitmap/color/sprite/player_15_G.xbm"
#include "bitmap/color/sprite/player_15_B.xbm"
#include "bitmap/color/sprite/winner_W.xbm"
#include "bitmap/color/sprite/winner_R.xbm"
#include "bitmap/color/sprite/winner_G.xbm"
#include "bitmap/color/sprite/winner_B.xbm"
#include "bitmap/color/sprite/big_W.xbm"
#include "bitmap/color/sprite/big_R.xbm"
#include "bitmap/color/sprite/big_G.xbm"
#include "bitmap/color/sprite/big_B.xbm"

BitmapStruct sprite_addon [MAX_ANIME][SPRITE_DEPTH] = {
  { 
    { player_1_R_width, player_1_R_height, player_1_R_bits },
    { player_1_G_width, player_1_G_height, player_1_G_bits },
    { player_1_B_width, player_1_B_height, player_1_B_bits },
    { player_1_W_width, player_1_W_height, player_1_W_bits },
  },
  { 
    { player_2_R_width, player_2_R_height, player_2_R_bits },
    { player_2_G_width, player_2_G_height, player_2_G_bits },
    { player_2_B_width, player_2_B_height, player_2_B_bits },
    { player_2_W_width, player_2_W_height, player_2_W_bits },
  },
  { 
    { player_3_R_width, player_3_R_height, player_3_R_bits },
    { player_3_G_width, player_3_G_height, player_3_G_bits },
    { player_3_B_width, player_3_B_height, player_3_B_bits },
    { player_3_W_width, player_3_W_height, player_3_W_bits },
  },
  { 
    { player_4_R_width, player_4_R_height, player_4_R_bits },
    { player_4_G_width, player_4_G_height, player_4_G_bits },
    { player_4_B_width, player_4_B_height, player_4_B_bits },
    { player_4_W_width, player_4_W_height, player_4_W_bits },
  },
  { 
    { player_5_R_width, player_5_R_height, player_5_R_bits },
    { player_5_G_width, player_5_G_height, player_5_G_bits },
    { player_5_B_width, player_5_B_height, player_5_B_bits },
    { player_5_W_width, player_5_W_height, player_5_W_bits },
  },
  { 
    { player_6_R_width, player_6_R_height, player_6_R_bits },
    { player_6_G_width, player_6_G_height, player_6_G_bits },
    { player_6_B_width, player_6_B_height, player_6_B_bits },
    { player_6_W_width, player_6_W_height, player_6_W_bits },
  },
  { 
    { player_7_R_width, player_7_R_height, player_7_R_bits },
    { player_7_G_width, player_7_G_height, player_7_G_bits },
    { player_7_B_width, player_7_B_height, player_7_B_bits },
    { player_7_W_width, player_7_W_height, player_7_W_bits },
  },
  { 
    { player_8_R_width, player_8_R_height, player_8_R_bits },
    { player_8_G_width, player_8_G_height, player_8_G_bits },
    { player_8_B_width, player_8_B_height, player_8_B_bits },
    { player_8_W_width, player_8_W_height, player_8_W_bits },
  },
  { 
    { player_9_R_width, player_9_R_height, player_9_R_bits },
    { player_9_G_width, player_9_G_height, player_9_G_bits },
    { player_9_B_width, player_9_B_height, player_9_B_bits },
    { player_9_W_width, player_9_W_height, player_9_W_bits },
  },
  { 
    { player_10_R_width, player_10_R_height, player_10_R_bits },
    { player_10_G_width, player_10_G_height, player_10_G_bits },
    { player_10_B_width, player_10_B_height, player_10_B_bits },
    { player_10_W_width, player_10_W_height, player_10_W_bits },
  },
  { 
    { player_11_R_width, player_11_R_height, player_11_R_bits },
    { player_11_G_width, player_11_G_height, player_11_G_bits },
    { player_11_B_width, player_11_B_height, player_11_B_bits },
    { player_11_W_width, player_11_W_height, player_11_W_bits },
  },
  { 
    { player_12_R_width, player_12_R_height, player_12_R_bits },
    { player_12_G_width, player_12_G_height, player_12_G_bits },
    { player_12_B_width, player_12_B_height, player_12_B_bits },
    { player_12_W_width, player_12_W_height, player_12_W_bits },
  },
  { 
    { 0, 0, (unsigned char *)0 },
    { 0, 0, (unsigned char *)0 },
    { 0, 0, (unsigned char *)0 },
    { 0, 0, (unsigned char *)0 },
  },
  { 
    { player_14_R_width, player_14_R_height, player_14_R_bits },
    { player_14_G_width, player_14_G_height, player_14_G_bits },
    { player_14_B_width, player_14_B_height, player_14_B_bits },
    { player_14_W_width, player_14_W_height, player_14_W_bits },
  },
  { 
    { player_15_R_width, player_15_R_height, player_15_R_bits },
    { player_15_G_width, player_15_G_height, player_15_G_bits },
    { player_15_B_width, player_15_B_height, player_15_B_bits },
    { player_15_W_width, player_15_W_height, player_15_W_bits },
  },
  { 
    { winner_R_width, winner_R_height, winner_R_bits },
    { winner_G_width, winner_G_height, winner_G_bits },
    { winner_B_width, winner_B_height, winner_B_bits },
    { winner_W_width, winner_W_height, winner_W_bits },
  },
  { 
    { big_R_width, big_R_height, big_R_bits },
    { big_G_width, big_G_height, big_G_bits },
    { big_B_width, big_B_height, big_B_bits },
    { big_W_width, big_W_height, big_W_bits },
  },
};

/*
 * end of sprite.c
 */
