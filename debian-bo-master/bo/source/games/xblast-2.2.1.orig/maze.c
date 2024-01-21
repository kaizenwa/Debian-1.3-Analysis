/*
 * Program XBLAST V2.1.9 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * September 25th 1996
 * started August 1993
 *
 * File: maze.c
 * level management
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



#include <stdio.h>
#include <stdlib.h>

#include "include.h"
#include "mytypes.h"
#include "const.h"
#include "func.h"
#include "data.h"
#include "maze.h"
#include "map.h"
#include "bomb.h"
#include "main.h"
#include "expl.h"
#include "intro.h"
#include "shrink.h"
#include "info.h"


/*
 * global variables
 */
int levelMax = -1;
int scoreBoard = -1;
int winningTheGame = -1;

/*
 * local variables
 */
static BMLevelData *cur_level;
static BMLevel **maze_data;

/*
 * public function load_all_levels
 */
#ifdef __STDC__
int
load_all_levels (void) 
#else
int 
load_all_levels ()
#endif
{
  int l, total_level;
  BMLevelData **ptr;

  /* simple version */
  levelMax = 0;

  /* get number of internal levels */
  for (; internal_maze_data[levelMax]!=NULL; levelMax++);

  /* check if any levels are loaded */
  if (levelMax == 0) {
    fprintf(stderr, "No levels were loaded.\n");
    return 1;
  }
  
    /* get total number including scoreboards */
  total_level  = levelMax + 2;
  
  /* alloc array */
  if (NULL == (maze_data = (BMLevel **) 
	       calloc(total_level, sizeof(BMLevelData *))) ) {
    fprintf(stderr, "level data alloc failed\n");
    return 1;
  }

  /* setup new array */
  l=0;
  
  /* "load" internal levels */
  for (ptr = internal_maze_data; *ptr != NULL; ptr ++) {
    maze_data[l] = &((*ptr)->level);
    l++;
  }

  scoreBoard     = levelMax;
  winningTheGame = levelMax+1;
  /* copy scroeboard */
  maze_data[l++] = NULL;
  maze_data[l++] = NULL;

  return 0;
}

/*
 * public function
 */
#ifdef _STDC__
void
load_score_board (int game_mode,
		  int num_victories)
#else
void
load_score_board (game_mode, num_victories) 
     int game_mode;
     int num_victories;
#endif
{
  int x,y;
  int i;
  
  /* find correct scoreboard */
  i=0;
  while (game_mode != (game_mode & score_board_data[i]->level.game_mode)) {
    i ++;
    if (score_board_data[i] == NULL) {
      fprintf(stderr, "Failed to load scoreboard\n");
      exit_prg(2);
    }
  }
  maze_data[scoreBoard] = &(score_board_data[i]->level);
  
  /* find correct scoreboard */
  i=0;
  while (game_mode != (game_mode & winning_data[i]->level.game_mode)) {
    i ++;
    if (winning_data[i] == NULL) {
      fprintf(stderr, "Failed to load end screen\n");
      exit_prg(2);
    }
  }
  maze_data[winningTheGame] = &(winning_data[i]->level);
  
  for (x = num_victories + 6; x < MAZE_W; x ++) {
    for (y = 0; y < MAZE_H; y++) {
      if (7 != ((BMLevelData *)maze_data[scoreBoard]->data)->map.maze[x][y]) {
	((BMLevelData *)maze_data[scoreBoard]->data)->map.maze[x][y] = 6;
      }
    }
  }
}



/* public function load_maze */

#ifdef __STDC__
void 
load_maze (int level,   
	   XBConfig *config)
#else
void 
load_maze (level, config)
     int level;
     XBConfig *config;
#endif
{
  cur_level = maze_data[level]->data;
  /* reset level info data */
  reset_info();

  /* setup shrink and scrambles */
  setup_shrink(&(cur_level->shrink));

  /* setup special functions */
  setup_funcs(&(cur_level->func));

  /* setup players */
  setup_players(cur_level->level.game_mode, &(cur_level->player));

  /* setup game map */
  setup_map(&(cur_level->map));

  /* setup bombs and explosions */
  setup_bombs(&(cur_level->bomb));

  /* setup graphics */
  setup_graphics (config->num_disp, &(cur_level->graphics));
}


/* 
 * public function show_levels 
 */
#ifdef __STDC__
void 
show_levels (void)
#else
void 
show_levels ()
#endif
{
  int i;

  printf("%3s  %-36s %-30s\n","Lvl","Level Name","Author");  
  for (i=0; i < levelMax; i++) {
    printf("%3d  %-36s %-30s\n",i,
           maze_data[i]->name, maze_data[i]->author);
  }
}


/* 
 * public function show_levels_tcl
 * 
 * with tcl list output including resources
 */
#ifdef __STDC__
void 
show_levels_tcl (void)
#else
void 
show_levels_tcl ()
#endif
{
  int i;

  for (i=0; i < levelMax; i++) {
    printf("{%d} {%s} {%s}\n", i, maze_data[i]->name, maze_data[i]->resource);
  }
}


#if 0
/* public function get_current_level */
#ifdef __STDC__
BMLevelData *
get_current_level (void)
#else
BMLevelData *
get_current_level ()
#endif
{
  return cur_level;
}
#endif

/* public function get_level_name */
#ifdef __STDC__
char *
get_level_name (int level)
#else
char *
get_level_name(level)
     int level;
#endif
{
  return maze_data[level]->name;
}


#ifdef __STDC__
char *
get_level_res_name (int level)
#else
char *
get_level_res_name(level)
     int level;
#endif
{
  return maze_data[level]->resource;
}


#ifdef __STDC__
char *
get_level_author (int level)
#else
char *
get_level_author (level)
     int level;
#endif
{
  static char result[256];

  sprintf(result, "created by %s", maze_data[level]->author);
  return result;
}



#ifdef __STDC__
char *
get_level_tip (int level)
#else
char *
get_level_tip (level)
     int level;
#endif
{
  return maze_data[level]->DSCtips;
}



/* public function get_game_mode */
#ifdef __STDC__
int 
get_game_mode (int level)
#else
int 
get_game_mode (level)
     int level;
#endif
{
  return(maze_data[level]->game_mode);
}


/*
 * end of file maze.c
 */
