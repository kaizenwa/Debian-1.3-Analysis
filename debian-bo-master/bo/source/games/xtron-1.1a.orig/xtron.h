/* tron.h - xtron v1.1 header for xtron.c
 *
 *   Copyright (C) 1995 Rhett D. Jacobs <rhett@hotel.canberra.edu.au>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 1, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Last Modified: 16/4/95
 */

#ifndef _XTRON_H
#define _XTRON_H

#define WIN_WIDTH 384
#define WIN_HEIGHT 512
#define DIMS (WIN_WIDTH/8)
#define MAXVERT (WIN_WIDTH/8)
#define MINVERT 0
#define MAXHORZ (WIN_WIDTH/8)
#define MINHORZ 0

enum directions {left, right, up, down};
enum play_types {human, computer};

struct Player {
  short int co_ords[2];
  short int plr_num;  
  int score;
  int alive;
  enum directions plr_dir;
  enum play_types plr_type; 
} p[2];

struct Board {
  short int contents[200][200];
} b;
 
void plr_setup(void);
int plr_checkmove(int p_num, int new_val, int axis_type, enum directions dir);
void plr_turn(int p_num, enum directions dir);
void brd_setup(void);
int brd_newcontents(int x, int y, int what);

#endif
