/*
   globals.h

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

#ifndef _globals_h_
#define _globals_h_

#include <argv/argv.h>
#include <unistd.h>

extern char *gb_version_string;

/* Background color */
extern int gb_bg_color;

/* Set to ARGV_TRUE to disable collision detection */
extern char gb_test_mode;

/* Set to desired FPS */
extern int gb_frames_per_second;

/* Number of microseconds of overhead in usleep */
extern int gb_usleep_time;

/* Search depth for eyes */
extern int gb_ghost_search_depth;

/* # of frames to wait before ghost regenerates (after eyes get home) */
extern int gb_ghost_regen_wait;

/* Level to start on */
extern int gb_start_level;

/* Skill level - 0 (easy) -- 3 (hard) */
extern int gb_skill_level;

/*--- Various search paths ---*/
extern char *gb_root_path;

extern char *gb_top_path;  		
extern char *gb_maze_subdir;	/* These are 1 level below top_path. */
extern char *gb_image_subdir;	/* e.g. /mazes, /maps, etc. */			
extern char *gb_sound_subdir;
extern char *gb_font_subdir;

/* Set to ARGV_TRUE to use 8k instead of 11k sound */
extern char gb_use_8k_sound;

/* Set to ARGV_TRUE to disable sound */
extern char gb_dont_use_sound;

/* Process ID of soundserver */
extern pid_t gb_pid_sndserver;

#endif



