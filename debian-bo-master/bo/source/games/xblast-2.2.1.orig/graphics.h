/*
 * Programm XBLAST V2.2.1 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 26th, 1997
 * started August 1993
 *
 * File: graphics
 * graphics using X11 standard
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

#ifndef _GRAPHICS_H
#define _GRAPHICS_H

/*
 * font constants
 */
#define LARGE_FONT 0
#define MEDIUM_FONT 1
#define SMALL_FONT 2
#define NUM_FONTS 3
/*
 * font flags
 */
#define FF_Large       0x00
#define FF_Medium      0x01
#define FF_Small       0x02
#define FF_Black       0x00
#define FF_White       0x04
#define FF_Boxed       0x08
#define FF_Transparent 0x10
/*
 * font flag mask
 */
#define FM_Size        0x03
#define FM_Color       0x04
#define FM_Boxed       0x08
#define FM_Transparent 0x10

/*
 * extern variables
 */

#ifndef _GRAPHICS_C
extern colorMode[MAX_PLAYER];
#endif

/*
 * prototypes
 */

#ifdef _GRAPHICS_C
#define _EXTERN 
#else
#define _EXTERN extern
#endif

#ifdef __STDC__
_EXTERN void init_display (int disp, char *display);
_EXTERN void finish_display (int disp);
_EXTERN void init_graphics (int disp, char*win_title);
_EXTERN void link_keysyms (int disp, int nelem, KeyPressDefine *keydef);
_EXTERN void check_event (int disp, void (*expose_func)());
_EXTERN void init_block (int disp, int in_data, int in_pix, char *fg_name, 
			 char *bg_name, char *add_name);
_EXTERN void init_explosion_blocks (int disp);
_EXTERN void free_block (int disp, int in_pix);
_EXTERN void free_explosion_blocks (int disp);
_EXTERN void draw_block (int x, int y, int block);
_EXTERN void draw_block_at (int disp, int x, int y, int block);
_EXTERN void draw_explosion (int x, int y, int block);
_EXTERN void flush_blocks (int disp, int flag);
_EXTERN void draw_explosion_sprite (int disp, int x, int y, int block);
_EXTERN void clear_window (int disp);
_EXTERN void set_fade_max (int max);
_EXTERN void init_fade (int step);
_EXTERN void fade_out_window (int disp);
_EXTERN void fade_in_window (int disp);
_EXTERN void clear_pixmap (int disp);
_EXTERN void add_maze_rectangle (int x, int y);
_EXTERN void add_stat_rectangle (int x, int y);
_EXTERN void flush_score_board (int disp);
_EXTERN void flush_pixmap (int disp, int num_disp, int flag);
_EXTERN int win_is_mapped (int disp);
_EXTERN void draw_bomb_sprite (int disp, Sprite *spl);
_EXTERN void draw_player_sprite (int disp, Sprite *spl);
_EXTERN void draw_time_led (int disp, int x, int block);
_EXTERN void draw_score_board (int disp, int num_player);
_EXTERN void draw_score_block (int disp, int x, int block);
_EXTERN void draw_score_block_half (int disp, int x, int block, int left);
_EXTERN void draw_polygon (int disp, int x, int y, int w, int h,
			   BMPoint *points, int npoints, int black_white);
_EXTERN void draw_textbox (int disp, char *text, int flag, BMRectangle *rect);
_EXTERN void draw_winner (int disp, int player, int x, int y);
_EXTERN void draw_circle_from_pixmap (int disp, int x, int y, int r);
_EXTERN void do_bell (int disp);
_EXTERN void no_bell (int disp);
#else
_EXTERN void init_display ();
_EXTERN void finish_display ();
_EXTERN void init_graphics ();
_EXTERN void link_keysyms ();
_EXTERN void check_event ();
_EXTERN void init_block ();
_EXTERN void init_explosion_blocks ();
_EXTERN void free_block ();
_EXTERN void free_explosion_blocks ();
_EXTERN void draw_block ();
_EXTERN void draw_explosion ();
_EXTERN void flush_blocks ();
_EXTERN void draw_explosion_sprite ();
_EXTERN void clear_window ();
_EXTERN void set_fade_max ();
_EXTERN void init_fade ();
_EXTERN void fade_out_window ();
_EXTERN void fade_in_window ();
_EXTERN void clear_pixmap ();
_EXTERN void add_stat_rectangle ();
_EXTERN void add_maze_rectangle ();
_EXTERN void flush_score_board ();
_EXTERN void flush_pixmap ();
_EXTERN int win_is_mapped ();
_EXTERN void draw_bomb_sprite ();
_EXTERN void draw_player_sprite ();
_EXTERN void draw_time_led ();
_EXTERN void draw_score_board ();
_EXTERN void draw_score_block ();
_EXTERN void draw_score_block_half ();
_EXTERN void draw_polygon ();
_EXTERN void draw_textbox ();
_EXTERN void draw_circle_from_pixmap ();
_EXTERN void do_bell ();
_EXTERN void no_bell ();
#endif

#undef _EXTERN

#endif
/*
 * end of file graphics.h
 */
