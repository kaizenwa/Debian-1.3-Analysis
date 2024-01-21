/*
   globals.cc

   This file is part of LuxMan.
   
   Copyright (C) 1994,1995 Frank McIngvale (frankm@nuance.com)
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/
#include <argv/argv.h>
#include <unistd.h>
#include <gtools/screen.h>

char *gb_version_string = "0.41";

int gb_bg_color = BLACK;

char gb_test_mode = ARGV_FALSE;

int gb_frames_per_second = 28;

int gb_usleep_time = 0;		/* Run `calibrate_usleep' to set */

int gb_ghost_search_depth = 25;

int gb_ghost_regen_wait = 0;

int gb_start_level = 1;

int gb_skill_level = 2;

char *gb_root_path = ".:/usr/games/lib/luxman";
char *gb_top_path;
char *gb_maze_subdir = "mazes/";
char *gb_image_subdir = "maps/";
char *gb_sound_subdir = "11k/";
char *gb_font_subdir = "fonts/";

char gb_use_8k_sound = ARGV_FALSE;

char gb_dont_use_sound = ARGV_FALSE;

pid_t gb_pid_sndserver = -1;

