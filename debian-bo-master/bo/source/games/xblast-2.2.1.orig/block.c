/*
 * Programm XBLAST V1.2.14 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * May 9th 1996
 * started August 1993
 *
 * File: block.c
 * bitmap data of blocks
 *
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

#define _BLOCK_C

#include "include.h"
#include "mytypes.h"
#include "block.h"

/* background bitmaps */
#include "bitmap/misc/title.xbm"
#include "bitmap/misc/text_bg.xbm"

BitmapStruct title_bitmap = { title_width, title_height, title_bits };
BitmapStruct text_bg_bitmap = { text_bg_width, text_bg_height, text_bg_bits };

/* block bitmaps fuer bakcground */

#include "bitmap/block/karo_light.xbm"
#include "bitmap/block/karo_dark.xbm"
#include "bitmap/block/pyramid.xbm"
#include "bitmap/block/wall.xbm"
#include "bitmap/block/extra.xbm"
#include "bitmap/block/range.xbm"
#include "bitmap/block/bomb.xbm"
#include "bitmap/block/chest.xbm"
#include "bitmap/block/trap.xbm"
#include "bitmap/block/hex.xbm"
#include "bitmap/block/hex_extra.xbm"
#include "bitmap/block/hex_wall.xbm"
#include "bitmap/block/lego_floor.xbm"
#include "bitmap/block/lego_white.xbm"
#include "bitmap/block/lego_black.xbm"
#include "bitmap/block/score_left_up.xbm"
#include "bitmap/block/score_left_down.xbm"
#include "bitmap/block/score_mid_up.xbm"
#include "bitmap/block/score_mid_down.xbm"
#include "bitmap/block/score_right_up.xbm"
#include "bitmap/block/score_right_down.xbm"
#include "bitmap/block/score_floor.xbm"
#include "bitmap/block/iron_floor.xbm"
#include "bitmap/block/dark_block.xbm"
#include "bitmap/block/dark_house.xbm"
#include "bitmap/block/light_house.xbm"
#include "bitmap/block/city_free.xbm"
#include "bitmap/block/control_num.xbm"
#include "bitmap/block/control_alpha.xbm"
#include "bitmap/block/display.xbm"
#include "bitmap/block/sphere_light.xbm"
#include "bitmap/block/sphere_dark.xbm"
#include "bitmap/block/sphere_half.xbm"
#include "bitmap/block/invincible.xbm"
#include "bitmap/block/temple.xbm"
#include "bitmap/block/remote_control.xbm"
#include "bitmap/block/book_shelf.xbm"
#include "bitmap/block/chess_floor.xbm"
#include "bitmap/block/chess_sphere.xbm"
#include "bitmap/block/pumpkin.xbm"
#include "bitmap/block/r_i_p.xbm"
#include "bitmap/block/dark_way.xbm"
#include "bitmap/block/karo_light_S.xbm"
#include "bitmap/block/extra_O.xbm"
#include "bitmap/block/karo_dark_S.xbm"
#include "bitmap/block/chest_O.xbm"
#include "bitmap/block/iron_floor_S.xbm"
#include "bitmap/block/brick_O.xbm"
#include "bitmap/block/sphere_half_S.xbm"
#include "bitmap/block/sphere_light_O.xbm"
#include "bitmap/block/hex_extra_O.xbm"
#include "bitmap/block/chess_floor_S.xbm"
#include "bitmap/block/chess_sphere_O.xbm"
#include "bitmap/block/lego_floor_S.xbm"
#include "bitmap/block/lego_black_o.xbm"
#include "bitmap/block/city_free_S.xbm"
#include "bitmap/block/light_house_O.xbm"
#include "bitmap/block/dark_way_S.xbm"
#include "bitmap/block/pumpkin_O.xbm"
#include "bitmap/block/kick_bomb.xbm"
#include "bitmap/block/pyramid_R.xbm"
#include "bitmap/block/wall_R.xbm"
#include "bitmap/block/dark_block_R.xbm"
#include "bitmap/block/r_i_p_R.xbm"
#include "bitmap/block/dark_house_R.xbm"
#include "bitmap/block/ignite.xbm"
#include "bitmap/block/box.xbm"
#include "bitmap/block/button_floor.xbm"
#include "bitmap/block/dark_button.xbm"
#include "bitmap/block/teleport.xbm"
#include "bitmap/block/mr_beam_free.xbm"
#include "bitmap/block/mr_beam_bear.xbm"
#include "bitmap/block/mr_beam_bear_O.xbm"
#include "bitmap/block/mr_beam_tv.xbm"
#include "bitmap/block/q3a_beam.xbm"
#include "bitmap/block/air_pump.xbm"
#include "bitmap/block/napalm.xbm"
#include "bitmap/block/rock_floor.xbm"
#include "bitmap/block/rock_floor_S.xbm"
#include "bitmap/block/pit.xbm"
#include "bitmap/block/weight.xbm"
#include "bitmap/block/pit_R.xbm"
#include "bitmap/block/weight_R.xbm"
#include "bitmap/block/ghost.xbm"
#include "bitmap/block/ghost_sq_R.xbm"
#include "bitmap/block/ghost_sq.xbm"
#include "bitmap/block/ghost_ci_R.xbm"
#include "bitmap/block/ghost_ci.xbm"
#include "bitmap/block/firecracker.xbm"
#include "bitmap/block/construction.xbm"
#include "bitmap/block/score_step.xbm"
#include "bitmap/block/score_drop.xbm"
#include "bitmap/block/syringe.xbm"
/* add bitmap files here */

BitmapStruct block_tile[MAX_BLOCK_TILES] =
{
  { karo_light_width, karo_light_height, karo_light_bits },
  { karo_dark_width, karo_dark_height, karo_dark_bits },
  { pyramid_width, pyramid_height, pyramid_bits },
  { wall_width, wall_height, wall_bits },
  { extra_width, extra_height, extra_bits },
  { range_width, range_height, range_bits },
  { bomb_width, bomb_height, bomb_bits },
  { chest_width, chest_height, chest_bits },
  { trap_width, trap_height, trap_bits },
  { hex_width, hex_height, hex_bits },
  { hex_extra_width, hex_extra_height, hex_extra_bits },
  { hex_wall_width, hex_wall_height, hex_wall_bits },
  { lego_floor_width, lego_floor_height, lego_floor_bits },
  { lego_white_width, lego_white_height, lego_white_bits },
  { lego_black_width, lego_black_height, lego_black_bits },
  { score_left_up_width, score_left_up_height, score_left_up_bits },
  { score_left_down_width, score_left_down_height, score_left_down_bits },
  { score_mid_up_width, score_mid_up_height, score_mid_up_bits },
  { score_mid_down_width, score_mid_down_height, score_mid_down_bits },
  { score_right_up_width, score_right_up_height, score_right_up_bits },
  { score_right_down_width, score_right_down_height, score_right_down_bits },
  { score_floor_width, score_floor_height, score_floor_bits },
  { iron_floor_width, iron_floor_height, iron_floor_bits },
  { dark_block_width, dark_block_height, dark_block_bits },
  { dark_house_width, dark_house_height, dark_house_bits },
  { light_house_width, light_house_height, light_house_bits },
  { city_free_width, city_free_height, city_free_bits },
  { control_num_width, control_num_height, control_num_bits },
  { control_alpha_width, control_alpha_height, control_alpha_bits },
  { display_width, display_height, display_bits },
  { sphere_light_width, sphere_light_height, sphere_light_bits },
  { sphere_dark_width, sphere_dark_height, sphere_dark_bits },
  { sphere_half_width, sphere_half_height, sphere_half_bits },
  { invincible_width, invincible_height, invincible_bits },
  { temple_width, temple_height, temple_bits },
  { remote_control_width, remote_control_height, remote_control_bits },
  { book_shelf_width, book_shelf_height, book_shelf_bits },
  { chess_floor_width, chess_floor_height, chess_floor_bits },
  { chess_sphere_width, chess_sphere_height, chess_sphere_bits },
  { pumpkin_width, pumpkin_height, pumpkin_bits },
  { r_i_p_width, r_i_p_height, r_i_p_bits },
  { dark_way_width, dark_way_height, dark_way_bits },
  { karo_light_S_width, karo_light_S_height, karo_light_S_bits },
  { extra_O_width, extra_O_height, extra_O_bits },
  { karo_dark_S_width, karo_dark_S_height, karo_dark_S_bits },
  { chest_O_width, chest_O_height, chest_O_bits },
  { iron_floor_S_width, iron_floor_S_height, iron_floor_S_bits },
  { brick_O_width, brick_O_height, brick_O_bits },
  { sphere_half_S_width, sphere_half_S_height, sphere_half_S_bits },
  { sphere_light_O_width, sphere_light_O_height, sphere_light_O_bits },
  { hex_extra_O_width, hex_extra_O_height, hex_extra_O_bits },
  { chess_floor_S_width, chess_floor_S_height, chess_floor_S_bits },
  { chess_sphere_O_width, chess_sphere_O_height, chess_sphere_O_bits },
  { lego_floor_S_width, lego_floor_S_height, lego_floor_S_bits },
  { lego_black_o_width, lego_black_o_height, lego_black_o_bits },
  { city_free_S_width, city_free_S_height, city_free_S_bits },
  { light_house_O_width, light_house_O_height, light_house_O_bits },
  { dark_way_S_width, dark_way_S_height, dark_way_S_bits },
  { pumpkin_O_width, pumpkin_O_height, pumpkin_O_bits },
  { kick_bomb_width, kick_bomb_height, kick_bomb_bits },
  { pyramid_R_width, pyramid_R_height, pyramid_R_bits },
  { wall_R_width, wall_R_height, wall_R_bits },
  { dark_block_R_width, dark_block_R_height, dark_block_R_bits },
  { r_i_p_R_width, r_i_p_R_height, r_i_p_R_bits },
  { dark_house_R_width, dark_house_R_height, dark_house_R_bits },
  { ignite_width, ignite_height, ignite_bits },
  { box_width, box_height, box_bits },
  { button_floor_width, button_floor_height, button_floor_bits },
  { dark_button_width, dark_button_height, dark_button_bits },
  { teleport_width, teleport_height, teleport_bits },
  { mr_beam_free_width, mr_beam_free_height, mr_beam_free_bits },
  { mr_beam_bear_width, mr_beam_bear_height, mr_beam_bear_bits },
  { mr_beam_bear_O_width, mr_beam_bear_O_height, mr_beam_bear_O_bits },
  { mr_beam_tv_width, mr_beam_tv_height, mr_beam_tv_bits },
  { q3a_beam_width, q3a_beam_height, q3a_beam_bits },
  { air_pump_width, air_pump_height, air_pump_bits },
  { napalm_width, napalm_height, napalm_bits },
  { rock_floor_width, rock_floor_height, rock_floor_bits },
  { rock_floor_S_width, rock_floor_S_height, rock_floor_S_bits },
  { pit_width, pit_height, pit_bits },
  { weight_width, weight_height, weight_bits },
  { pit_R_width, pit_R_height, pit_R_bits },
  { weight_R_width, weight_R_height, weight_R_bits },
  { ghost_width, ghost_height, ghost_bits },
  { ghost_sq_R_width, ghost_sq_R_height, ghost_sq_R_bits },
  { ghost_sq_width, ghost_sq_height, ghost_sq_bits },
  { ghost_ci_R_width, ghost_ci_R_height, ghost_ci_R_bits },
  { ghost_ci_width, ghost_ci_height, ghost_ci_bits },
  { firecracker_width, firecracker_height, firecracker_bits },
  { construction_width, construction_height, construction_bits },
  { score_step_width, score_step_height, score_step_bits },
  { score_drop_width, score_drop_height, score_drop_bits },
  { syringe_width, syringe_height, syringe_bits },
  /* add bitmaps here */
};


#include "bitmap/color/block/karo_light_A.xbm"
#include "bitmap/color/block/karo_dark_A.xbm"
#include "bitmap/color/block/extra_A.xbm"
#include "bitmap/color/block/range_A.xbm"
#include "bitmap/color/block/bomb_A.xbm"
#include "bitmap/color/block/chest_A.xbm"
#include "bitmap/color/block/trap_A.xbm"
#include "bitmap/color/block/hex_A.xbm"
#include "bitmap/color/block/hex_extra_A.xbm"
#include "bitmap/color/block/hex_wall_A.xbm"
#include "bitmap/color/block/lego_floor_A.xbm"
#include "bitmap/color/block/lego_black_A.xbm"
#include "bitmap/color/block/score_left_up_A.xbm"
#include "bitmap/color/block/score_left_down_A.xbm"
#include "bitmap/color/block/score_mid_up_A.xbm"
#include "bitmap/color/block/score_mid_down_A.xbm"
#include "bitmap/color/block/iron_floor_A.xbm"
#include "bitmap/color/block/dark_block_A.xbm"
#include "bitmap/color/block/dark_house_A.xbm"
#include "bitmap/color/block/light_house_A.xbm"
#include "bitmap/color/block/city_free_A.xbm"
#include "bitmap/color/block/sphere_half_A.xbm"
#include "bitmap/color/block/invincible_A.xbm"
#include "bitmap/color/block/temple_A.xbm"
#include "bitmap/color/block/remote_control_A.xbm"
#include "bitmap/color/block/book_shelf_A.xbm"
#include "bitmap/color/block/chess_sphere_A.xbm"
#include "bitmap/color/block/pumpkin_A.xbm"
#include "bitmap/color/block/r_i_p_A.xbm"
#include "bitmap/color/block/dark_way_A.xbm"
#include "bitmap/color/block/karo_light_S_A.xbm"
#include "bitmap/color/block/extra_O_A.xbm"
#include "bitmap/color/block/karo_dark_S_A.xbm"
#include "bitmap/color/block/chest_O_A.xbm"
#include "bitmap/color/block/iron_floor_S_A.xbm"
#include "bitmap/color/block/sphere_half_S_A.xbm"
#include "bitmap/color/block/hex_extra_O_A.xbm"
#include "bitmap/color/block/chess_sphere_O_A.xbm"
#include "bitmap/color/block/lego_floor_S_A.xbm"
#include "bitmap/color/block/city_free_S_A.xbm"
#include "bitmap/color/block/light_house_O_A.xbm"
#include "bitmap/color/block/dark_way_S_A.xbm"
#include "bitmap/color/block/pumpkin_O_A.xbm"
#include "bitmap/color/block/kick_bomb_A.xbm"
#include "bitmap/color/block/pyramid_R_A.xbm"
#include "bitmap/color/block/wall_R_A.xbm"
#include "bitmap/color/block/dark_block_R_A.xbm"
#include "bitmap/color/block/r_i_p_R_A.xbm"
#include "bitmap/color/block/dark_house_R_A.xbm"
#include "bitmap/color/block/ignite_A.xbm"
#include "bitmap/color/block/box_A.xbm"
#include "bitmap/color/block/mr_beam_bear_A.xbm"
#include "bitmap/color/block/mr_beam_bear_O_A.xbm"
#include "bitmap/color/block/mr_beam_tv_A.xbm"
#include "bitmap/color/block/q3a_beam_A.xbm"
#include "bitmap/color/block/air_pump_A.xbm"
#include "bitmap/color/block/napalm_A.xbm"
#include "bitmap/color/block/weight_A.xbm"
#include "bitmap/color/block/ghost_sq_R_A.xbm"
#include "bitmap/color/block/ghost_sq_A.xbm"
#include "bitmap/color/block/ghost_ci_R_A.xbm"
#include "bitmap/color/block/ghost_ci_A.xbm"
#include "bitmap/color/block/firecracker_A.xbm"
#include "bitmap/color/block/construction_A.xbm"
#include "bitmap/color/block/score_step_A.xbm"
#include "bitmap/color/block/score_drop_A.xbm"
#include "bitmap/color/block/syringe_A.xbm"
/* add addon files here */

BitmapStruct block_addon[MAX_BLOCK_TILES] =
{
  { karo_light_A_width, karo_light_A_height, karo_light_A_bits },
  { karo_dark_A_width, karo_dark_A_height, karo_dark_A_bits },
  {0, 0, (unsigned char *)0}, /*  pyramid_bits, */
  {0, 0, (unsigned char *)0}, /*  wall_bits, */
  { extra_A_width, extra_A_height, extra_A_bits },
  { range_A_width, range_A_height, range_A_bits },
  { bomb_A_width, bomb_A_height, bomb_A_bits },
  { chest_A_width, chest_A_height, chest_A_bits },
  { trap_A_width, trap_A_height, trap_A_bits },
  { hex_A_width, hex_A_height, hex_A_bits },
  { hex_extra_A_width, hex_extra_A_height, hex_extra_A_bits },
  { hex_wall_A_width, hex_wall_A_height, hex_wall_A_bits },
  { lego_floor_A_width, lego_floor_A_height, lego_floor_A_bits },
  {0, 0, (unsigned char *)0}, /*  lego_white_bits, */
  { lego_black_A_width, lego_black_A_height, lego_black_A_bits },
  { score_left_up_A_width, score_left_up_A_height, score_left_up_A_bits },
  { score_left_down_A_width,score_left_down_A_height, score_left_down_A_bits },
  { score_mid_up_A_width, score_mid_up_A_height, score_mid_up_A_bits },
  { score_mid_down_A_width, score_mid_down_A_height, score_mid_down_A_bits },
  {0, 0, (unsigned char *)0}, /*  score_right_up_bits, */
  {0, 0, (unsigned char *)0}, /*  score_right_down_bits, */
  {0, 0, (unsigned char *)0}, /*  score_floor_bits, */
  { iron_floor_A_width, iron_floor_A_height, iron_floor_A_bits },
  { dark_block_A_width, dark_block_A_height, dark_block_A_bits },
  { dark_house_A_width, dark_house_A_height, dark_house_A_bits },
  { light_house_A_width, light_house_A_height, light_house_A_bits },
  { city_free_A_width, city_free_A_height, city_free_A_bits },
  {0, 0, (unsigned char *)0}, /*  control_num_bits, */
  {0, 0, (unsigned char *)0}, /*  control_alpha_bits, */
  {0, 0, (unsigned char *)0}, /*  display_bits, */
  {0, 0, (unsigned char *)0}, /*  sphere_light_bits, */
  {0, 0, (unsigned char *)0}, /*  sphere_dark_bits, */
  { sphere_half_A_width, sphere_half_A_height, sphere_half_A_bits },
  { invincible_A_width, invincible_A_height, invincible_A_bits },
  { temple_A_width, temple_A_height, temple_A_bits },
  { remote_control_A_width, remote_control_A_height, remote_control_A_bits },
  { book_shelf_A_width, book_shelf_A_height, book_shelf_A_bits },
  {0, 0, (unsigned char *)0}, /*  chess_floor_bits, */
  { chess_sphere_A_width, chess_sphere_A_height, chess_sphere_A_bits },
  { pumpkin_A_width, pumpkin_A_height, pumpkin_A_bits },
  { r_i_p_A_width, r_i_p_A_height, r_i_p_A_bits },
  { dark_way_A_width, dark_way_A_height, dark_way_A_bits },
  { karo_light_S_A_width, karo_light_S_A_height, karo_light_S_A_bits },
  { extra_O_A_width, extra_O_A_height, extra_O_A_bits },
  { karo_dark_S_A_width, karo_dark_S_A_height, karo_dark_S_A_bits },
  { chest_O_A_width, chest_O_A_height, chest_O_A_bits },
  { iron_floor_S_A_width, iron_floor_S_A_height, iron_floor_S_A_bits },
  {0, 0, (unsigned char *)0}, /*  brick_O_bits, */
  { sphere_half_S_A_width, sphere_half_S_A_height, sphere_half_S_A_bits },
  {0, 0, (unsigned char *)0}, /*  sphere_light_O_bits, */
  { hex_extra_O_A_width, hex_extra_O_A_height, hex_extra_O_A_bits },
  {0, 0, (unsigned char *)0}, /*  chess_floor_S_bits, */
  { chess_sphere_O_A_width, chess_sphere_O_A_height, chess_sphere_O_A_bits },
  { lego_floor_S_A_width, lego_floor_S_A_height, lego_floor_S_A_bits },
  {0, 0, (unsigned char *)0}, /*  lego_black_o_bits, */
  { city_free_S_A_width, city_free_S_A_height, city_free_S_A_bits },
  { light_house_O_A_width, light_house_O_A_height, light_house_O_A_bits },
  { dark_way_S_A_width, dark_way_S_A_height, dark_way_S_A_bits },
  { pumpkin_O_A_width, pumpkin_O_A_height, pumpkin_O_A_bits },
  { kick_bomb_A_width, kick_bomb_A_height, kick_bomb_A_bits },
  { pyramid_R_A_width, pyramid_R_A_height, pyramid_R_A_bits },
  { wall_R_A_width, wall_R_A_height, wall_R_A_bits },
  { dark_block_R_A_width, dark_block_R_A_height, dark_block_R_A_bits },
  { r_i_p_R_A_width, r_i_p_R_A_height, r_i_p_R_A_bits },
  { dark_house_R_A_width, dark_house_R_A_height, dark_house_R_A_bits },
  { ignite_A_width, ignite_A_height, ignite_A_bits },
  { box_A_width, box_A_height, box_A_bits },
  {0, 0, (unsigned char *)0}, /*  button_floor_bits, */
  {0, 0, (unsigned char *)0}, /*  dark_button_bits, */
  {0, 0, (unsigned char *)0}, /*  teleport_bits, */
  {0, 0, (unsigned char *)0}, /*  mr_beam_free_bits, */
  { mr_beam_bear_A_width, mr_beam_bear_A_height, mr_beam_bear_A_bits },
  { mr_beam_bear_O_A_width, mr_beam_bear_O_A_height, mr_beam_bear_O_A_bits },
  { mr_beam_tv_A_width, mr_beam_tv_A_height, mr_beam_tv_A_bits },
  { q3a_beam_A_width, q3a_beam_A_height, q3a_beam_A_bits },
  { air_pump_A_width, air_pump_A_height, air_pump_A_bits },
  { napalm_A_width, napalm_A_height, napalm_A_bits }, 
  {0, 0, (unsigned char *)0}, /* rock_floor_bits */
  {0, 0, (unsigned char *)0}, /* rock_floor_S_bits, */
  {0, 0, (unsigned char *)0}, /* pit_bits, */
  { weight_A_width, weight_A_height, weight_A_bits },
  {0, 0, (unsigned char *)0}, /* pit_R_bits, */
  {0, 0, (unsigned char *)0}, /* weight_R_bits, */
  {0, 0, (unsigned char *)0}, /* ghost_bits, */
  { ghost_sq_R_A_width, ghost_sq_R_A_height, ghost_sq_R_A_bits },
  { ghost_sq_A_width, ghost_sq_A_height, ghost_sq_A_bits },
  { ghost_ci_R_A_width, ghost_ci_R_A_height, ghost_ci_R_A_bits },
  { ghost_ci_A_width, ghost_ci_A_height, ghost_ci_A_bits },
  { firecracker_A_width, firecracker_A_height, firecracker_A_bits },
  { construction_A_width, construction_A_height, construction_A_bits },
  { score_step_A_width, score_step_A_height, score_step_A_bits },
  { score_drop_A_width, score_drop_A_height, score_drop_A_bits },
  { syringe_A_width, syringe_A_height, syringe_A_bits },
  /* add addons here */
};


/*
 * end of file block.c
 */







