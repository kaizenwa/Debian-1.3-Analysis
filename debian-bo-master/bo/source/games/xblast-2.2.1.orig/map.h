/*
 * Program XBLAST V2.2.1 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 26th, 1997
 * started August 1993
 *
 * File: map.h
 *       include file for map.c
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

#ifndef _MAP_H
#define _MAP_H

/*
 * some constants
 */

/*
 * prototypes
 */

#ifdef _MAP_C
#define _EXTERN
#else
#define _EXTERN extern
#endif

#ifdef __STDC__
_EXTERN void setup_graphics (int num_disp, BMGraphicsData *cur_level);
_EXTERN void setup_map (BMMapData *cur_level);
_EXTERN void unload_blocks (int num_disp);
_EXTERN void draw_maze (int num_disp);
_EXTERN void set_redraw_rectangles (void);
_EXTERN void mark_maze (int x1, int y1, int x2, int y2);
_EXTERN void mark_maze_tile (int x, int y);
_EXTERN void mark_maze_rect (int x, int y, int w, int h);
_EXTERN void clear_redraw_map (void);
_EXTERN void update_maze (int num_disp);
_EXTERN void update_expl (int player);
_EXTERN int check_maze (int x, int y);
_EXTERN int check_maze_free (int x, int y);
_EXTERN int check_maze_open (int x, int y);
_EXTERN int check_maze_wall (int x, int y);
_EXTERN int check_maze_solid (int x, int y);
_EXTERN int check_maze_extra (int x, int y);
_EXTERN void set_block_extra (int x, int y, int value);
_EXTERN void set_maze_block (int x, int y, int block);
_EXTERN int check_explosion (int x, int y);
_EXTERN void set_expl_block (int x, int y, int value);
_EXTERN int get_extra (int invincible, int x, int y);
_EXTERN int distrib_special (void);
_EXTERN void distribute_extras (int bombs, int range, int extras, int specials);
_EXTERN void blast_extra_block (int x, int y);
_EXTERN void copy_expl_block(int x, int y, int block[CHARH][CHARW]);
_EXTERN void draw_sprites (int disp);
_EXTERN void add_trophy_to_sprite_list (int x, int y);
_EXTERN void add_bomb_to_sprite_list (int x, int y, int an, int mode);
_EXTERN void add_player_to_sprite_list (int pl, int x, int y, int an, int m);
_EXTERN void clear_sprite_list (void);
_EXTERN void sort_sprite_list (void);
#else
_EXTERN void setup_graphics ();
_EXTERN void setup_map ();
_EXTERN void unload_blocks ();
_EXTERN void draw_maze ();
_EXTERN void set_redraw_rectangles ();
_EXTERN void mark_maze ();
_EXTERN void mark_maze_tile ();
_EXTERN void mark_maze_rect ();
_EXTERN void clear_redraw_map ();
_EXTERN void update_maze ();
_EXTERN void update_expl ();
_EXTERN int check_maze ();
_EXTERN int check_maze_free ();
_EXTERN int check_maze_open ();
_EXTERN int check_maze_wall ();
_EXTERN int check_maze_solid ();
_EXTERN int check_maze_extra ();
_EXTERN void set_block_extra ();
_EXTERN void set_maze_block ();
_EXTERN int check_explosion ();
_EXTERN void set_expl_block ();
_EXTERN int get_extra ();
_EXTERN int distrib_special ();
_EXTERN void distribute_extras (); 
_EXTERN void blast_extra_block ();
_EXTERN void copy_expl_block();
_EXTERN void draw_sprites ();
_EXTERN void add_trophy_to_sprite_list ();
_EXTERN void add_bomb_to_sprite_list ();
_EXTERN void add_player_to_sprite_list ();
_EXTERN void clear_sprite_list ();
_EXTERN void sort_sprite_list ();
#endif

#undef _EXTERN

#endif
/*
 * end of file map.h
 */
