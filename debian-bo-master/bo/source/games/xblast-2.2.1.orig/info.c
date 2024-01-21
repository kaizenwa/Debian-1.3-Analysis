/*
 * Program XBLAST V2.1.8 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * September 8th 1996
 * started August 1993
 *
 * File: info.c
 * level info
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as by published
 * by the Free Software Foundation; either version 2; or (at your option)
 * any later version
 *
 * This program is distributed in the hope that it will be entertaining,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILTY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Publis License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#define _INFO_C

#include <stdio.h>
#include <stdlib.h>

#include "include.h"
#include "mytypes.h"
#include "data.h"
#include "func.h"
#include "shrink.h"
#include "info.h"
#include "bomb.h"
#include "main.h"


/*
 * local function alloc_info
 */
#ifdef __STDC__
static char **
alloc_info (void)
#else
static char **
alloc_info ()
#endif
{
  char **ptr;
  int i;

  if (NULL == (ptr = (char **) calloc(MAX_INFO,sizeof(char *) ) ) ) {
    return NULL;
  }
  for (i=0; i<MAX_INFO; i++) {
    if (NULL == (ptr[i] = calloc(INFO_LENGTH,sizeof(char) ) ) ) {
      while (i > 0) {
	free (ptr[--i]);
      }
      free(ptr);
      return NULL;
    }    
  } 
  return ptr;
}



/*
 * Shrink Info
 */
static FuncInfo shrink_info[] = {
  { shrink_void,           NULL },
  { shrink_spiral,         "Spiral shrinking at half time" },
  { shrink_speed_spiral,   "Fast spiral shrinking at half time" },
  { shrink_spiral_plus,    "Spiral shrinking at half time" },
  { shrink_spiral_3,       "3 level spiral shrinking at half time" },
  { shrink_early_spiral,   "Spiral shrinking just before half time" },
  { shrink_compound,       "Continuous compound shrinking" },
  { shrink_compound_f,     "Continuous compound shrinking" },
  { shrink_compound_solid, "Continuous compound shrinking" },
  { shrink_compound_extra, "Compound shrinking with blastables" },
  { shrink_savage_compound,"Double continous compound shrinking" },
  { shrink_down,           "Continuous downward shrinking" },
  { shrink_down_f,         "Continuous downward shrinking" },
  { shrink_quad,           "Quad shrinking at half time" },
  { shrink_constrict_wave, "3 level wave shrink at half time" },
  { shrink_lazy_compound_f,"Lazy compound shrinking" },
  { shrink_compound_2_f,   "2 level compound shrinking" },
  { NULL, NULL },
};

/*
 * game function info
 */
static FuncInfo game_info[] = {
  { special_game_void,        NULL },
  { special_game_nasty_walls, "The Walls launch bombs" },
  { special_game_haunt,       "All bombs are haunted" },
  { special_game_haunt_fast,  "All bombs are haunted (and dangerous)" },
  { NULL, NULL },
};


/*
 * Extra info
 */
static FuncInfo special_extra_info[] = {
  { special_extra_void,         NULL  },
  { special_extra_invincible,   "Invincibility as an extra" },
  { special_extra_kick,         "Kick bomb as an extra" },
  { special_extra_teleport,     "Teleporter as an extra" },
  { special_extra_RC,           "Remote control as an extra" },
  { special_extra_ignite_all,   "Button as an extra" },
  { special_extra_air,          "Airpump as an extra" },
  { special_extra_junkie,       "Junkie virus as an extra" },
  { special_extra_special_bomb, NULL },
  { NULL, NULL },
};

/*
 * bomb extra info
 */
static char *bomb_name[] = {
  NULL,
  "Napalm bomb",
  NULL,
  NULL,
  "Firecracker",
  NULL,
  "Construction bomb",
  "Three bomb",
  "Grenade",
  "Triangle bomb",
  "Destruction bomb",
  "Fungus bomb",
  "Renovation bomb",
  "Pyro bomb",
  NULL,
  "Random bomb",
};

/* 
 * fuse time info
 */
static char *fuse_info[NUM_FUSES] = {
  "All bombs are short fused",
  NULL,
  "All bombs are long fused",
};


/*
 * info on bomb directions
 */
static char *bomb_dir_info[] = {
  NULL,
  "Bombs are going up",
  "Bombs are going left",
  "Bombs are falling down",
  "Bombs are going right",
  NULL,
};

/*
 * info on bomb clicks
 */
static FuncInfo bomb_click_info[] = {
  { bomb_click_rebound,       "Bombs rebound from others" },
  { bomb_click_contact,       "Bombs explode on contact with others" },
  { bomb_click_clockwise,     "Bombs turn clockwise on hitting others" },
  { bomb_click_anticlockwise, "Bombs turn anticlockwise on hitting others" },
  { bomb_click_randomdir,     "Bombs bounce off randomly from others" },
  { bomb_click_snooker,       "Bombs are snooker bombs" },
  { NULL, NULL },
};

/*
 * info on wall clicks
 */
static FuncInfo wall_click_info[] = {
  { bomb_click_rebound,       "Bombs rebound off walls" },
  { bomb_click_contact,       "Bombs explode on contact with walls" },
  { bomb_click_clockwise,     "Bombs turn clockwise on hitting walls" },
  { bomb_click_anticlockwise, "Bombs turn anticlockwise on hitting walls" },
  { bomb_click_randomdir,     "Bombs bounce off randomly of walls" },
  { NULL, NULL },
};

/*
 * info on player click
 */
static FuncInfo player_click_info[] = {
  { bomb_click_thru,    "Bombs stun players running through" },
  { bomb_click_contact, "Bombs explode on contact with players" },
  { bomb_click_rebound, "Bombs rebound off players" },
  { NULL, NULL },
};



#ifdef __STDC__
static int
set_func_info (PFV func, 
	       FuncInfo *info,
	       char **line)
#else
static int
set_func_info (func, info, line)
     PFV func;
     FuncInfo *info;
     char **line;
#endif
{
  int i;

  for (i=0; info[i].func != NULL; i ++) {
    if (info[i].func == func) {
      if (info[i].msg != NULL) {
	strcpy(*line,info[i].msg);
	return 1;
      } else {
	return 0;
      }
    }
  }
  return 0;
}


#ifdef __STDC__
static int
set_value_info (int value,
		char **info,
		char **line)
#else
static int
set_value_info (value, info, line)
     int value;
     char **info;
     char **line;
#endif
{
  if (info[value] == NULL) {
    return 0;
  } else {
    strcpy(*line,info[value]);
    return 1;
  }
}


/*
 * extra info strings
 */

static char *prob_info[] = {
  NULL,
  "%s are scarce",
  "%s are rare",
  "%s are uncommon",
  "%s are common",
  "%s are plentilful"
};

/* special extras */

/*
 * local function set_extra_info
 */
#ifdef __STDC__
static int
set_extra_info (int val, char *name, char **line)
#else
static int
set_extra_info (val, name, line)
     int val; 
     char *name; 
     char **line;
#endif
{
  int rate;

  /* first get rating */
  if (val <= 0) {
    rate = XR_None;
  } else if (val <=4) {
    rate = XR_Scarce;
  } else if (val <=8) {
    rate = XR_Rare;
  } else if (val <=16) {
    rate = XR_Uncommon;
  } else if (val <=32) {
    rate = XR_Common;
  } else {
    rate= XR_Plentiful;
  }

  if (NULL != prob_info[rate]) {
    sprintf(*line, prob_info[rate], name);
    return 1;
  }
  return 0;
}


/*
 * illness info strings
 */
static char *sick_info[] = {
  NULL,
  "Permanent random bombing",
  "Permanent slowdown",
  "Permanent running",
  "Permanent mini bombs",
  "No bombs while healthy",
  "Permanent invisibility",
  "Permanent malfunctions",
  "Permanent reverse controls",
};



static int level_count;
static int extra_count;
static int player_count;
static char **level_info = NULL;
static char **extra_info = NULL;
static char **player_info = NULL;

static int special_bomb_flag;
static int special_30_flag;

/*
 * public function: reset_info
 *
 * reset info strings
 */
#ifdef __STDC__
void
reset_info (void)
#else
void
reset_info ()
#endif
{
  int i;

  /* alloc info structures if needed */
  if ( ( (NULL == extra_info) && (NULL == (extra_info = alloc_info()) ) ) 
      || ( (NULL == level_info) && (NULL == (level_info = alloc_info()) ) ) 
      || ( (NULL == player_info) && (NULL == (player_info = alloc_info()) ) ) ) {
    fprintf(stderr, "Failed to alloc info structure\n");
    exit_prg(1);
  }

  for (i=0; i<MAX_INFO; i++) {
    level_info[i][0] = '\0';
    extra_info[i][0] = '\0';
    player_info[i][0] = '\0';
  }
  level_count = 0;
  extra_count = 0;
  player_count = 0;
  /* misc flags */
  special_bomb_flag = FALSE;
  special_30_flag = FALSE;
}

/*
 * public function: set_info_shrink
 *
 * set info strings from shrink data 
 */
#ifdef __STDC__
void
set_info_shrink (BMShrinkData *data)
#else
void
set_info_shrink (data)
     BMShrinkData *data;
#endif
{
  /* look for shrink function */
  if (set_func_info(data->shrink_func, shrink_info, level_info + level_count) ) {
    if ( ++level_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }

  return;
 MessagesFinished:
  fprintf(stderr, "Too many messages\n");
  return ;
}

/*
 * public function: set_info_func
 *
 * set info strings from func data 
 */
#ifdef __STDC__
void
set_info_func (BMFuncData *data)
#else
void
set_info_func (data)
     BMFuncData *data;
#endif
{
  /* look for extra function */
  if (set_func_info(data->extra_func, special_extra_info, 
		    level_info + level_count ) ) {
    if ( ++level_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }
  /* look for special bombs */
  if (data->extra_func == special_extra_special_bomb) {
    special_bomb_flag = TRUE;
  }
  /* look for game functions */
  if (set_func_info(data->game_func, game_info, level_info + level_count) ) {
    if ( ++level_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }
  /* look up 30 special func */
  if (data->init_func == special_init_special_bombs_30) {
    special_30_flag = TRUE;
  }
  
  return;
 MessagesFinished:
  fprintf(stderr, "Too many messages\n");
  return ;
}

/*
 * public function: set_info_player
 *
 * set info strings from player data 
 */
#ifdef __STDC__
void
set_info_player (BMPlayerData *data)
#else
void
set_info_player (data)
     BMPlayerData *data;
#endif
{
  /* get number of bombs */
  switch(data->bombs) {
  case 0:
    sprintf(player_info[player_count], "No bombs");
    break;
  case 1:
    sprintf(player_info[player_count], "1 bomb");
    break;
  default:
    sprintf(player_info[player_count], "%d bombs", data->bombs);
    break;
  }
  if ( ++player_count >= MAX_INFO) {
    goto MessagesFinished;
  }

  /* get range of bombs */
  switch (data->range) {
  case 0:
    sprintf(player_info[player_count], "No initial Range");
    break;
  case 1:
    sprintf(player_info[player_count], "Initial mini bombs");
    break;
  default:
    sprintf(player_info[player_count], "Initial range %d", data->range);
    break;
  }
  if ( ++player_count >= MAX_INFO) {
    goto MessagesFinished;
  }

  /* check inital illness */
  if (set_value_info(data->init_health, sick_info, 
		     player_info + player_count) ) {
    if ( ++player_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }

  /* look for default extras */
  if (IF_Kick & data->init_flags) {
    sprintf(player_info[player_count], "Kick extra as default");
    if ( ++player_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }
  if (IF_RC & data->init_flags) {
    sprintf(player_info[player_count], "Remote control as default");
    if ( ++player_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }
  if (IF_Teleport & data->init_flags) {
    sprintf(player_info[player_count], "Teleport as default");
    if ( ++player_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }

  return;
 MessagesFinished:
  fprintf(stderr, "Too many messages\n");
  return ;
}

/*
 * public function: set_info_map
 *
 * set info strings from map data 
 */
#ifdef __STDC__
void
set_info_map (BMMapData *data)
#else
void
set_info_map (data)
     BMMapData *data;
#endif
{
  /* look for bombs */
  if (set_extra_info(data->prob.bomb, "Bomb extras" , 
		     extra_info + extra_count ) ) {
    if ( ++extra_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }
  /* look for ranges */
  if (set_extra_info(data->prob.range - data->prob.bomb, "Range extras", 
		     extra_info + extra_count ) ) {
    if ( ++extra_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }
  /* look for traps */
  if (set_extra_info(data->prob.ill - data->prob.range, "Infections", 
		     extra_info + extra_count ) ) {
    if ( ++extra_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }
  /* look for special bombs */
  if (set_extra_info(data->prob.invinc - data->prob.ill, "Special extras", 
		     extra_info + extra_count ) ) {
    if ( ++extra_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }
  /* look for evil bombs */
  if (set_extra_info(data->prob.evil - data->prob.invinc, "Hidden Bombs", 
		     extra_info + extra_count ) ) {
    if ( ++extra_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }

  return;
 MessagesFinished:
  fprintf(stderr, "Too many messages\n");
  return ;
}

/*
 * public function: set_info_bomb
 *
 * set info strings from bomb data 
 */
#ifdef __STDC__
void
set_info_bombs (BMBombData *data)
#else
void
set_info_bombs (data)
     BMBombData *data;
#endif
{
  /* special bombs */
  if (special_bomb_flag) {
    if (NULL != bomb_name[data->buttonBMT])  {
      sprintf(level_info[level_count], "%s as an extra", 
	      bomb_name[data->buttonBMT]);
      if ( ++level_count >= MAX_INFO) {
	goto MessagesFinished;
      }
    }
  }
  /* look up fuse times */
  if (set_value_info(data->fuse_time, fuse_info, level_info + level_count) ) {
    if ( level_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }
  /* look inital bomb  direction */
  if (set_value_info(data->bomb_dir, bomb_dir_info, 
		     level_info + level_count) ) {
    if ( level_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }
  /* look for bomb_click */
  if (set_func_info(data->bomb_click, bomb_click_info, 
		    level_info + level_count) ) {
    if ( level_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }
  /* look for wall_click */
  if (set_func_info(data->wall_click, wall_click_info, 
		    level_info + level_count) ) {
    if ( level_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }
  /* look for player_click */
  if (set_func_info(data->player_click, player_click_info, 
		    level_info + level_count)) {
    if ( level_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }
  /* special bombs as default */
  if (NULL != bomb_name[data->defaultBMT])  {
    sprintf(player_info[player_count], "%ss as default", 
	    bomb_name[data->defaultBMT]);
    if ( ++player_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }
  /* special bombs 30 active */
  if (special_30_flag) {
    sprintf(player_info[player_count],"30 %ss",bomb_name[data->buttonBMT]);
    if ( ++player_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }

  return;
 MessagesFinished:
  fprintf(stderr, "Too many messages\n");
  return ;
}

/*
 * public function: set_info_graphics
 *
 * set info strings from graphics data 
 */
#ifdef __STDC__
void
set_info_graphics (BMGraphicsData *data)
#else
void
set_info_graphics (data)
     BMGraphicsData *data;
#endif
{
  /* look for invisible walls etc */
  if (data->block[BTBlock].id == data->block[BTFree].id) {
    sprintf(level_info[level_count],"Walls are invisble");
    if ( ++level_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }

  /* look for identical extras etc */
  if ( (data->block[BTBomb].id == data->block[BTRange].id) ||
       (data->block[BTBomb].id == data->block[BTSick].id) ||
       (data->block[BTBomb].id == data->block[BTSpecial].id) ) {
    sprintf(level_info[level_count],"Extras all look the same");
    if ( ++level_count >= MAX_INFO) {
      goto MessagesFinished;
    }
  }

  return;
 MessagesFinished:
  fprintf(stderr, "Too many messages\n");
  return ;
}

/*
 * public function get_info
 *
 * retriev info for intro screen
 */
#ifdef __STDC__
void
get_info (char ***extra, 
	  char ***level,
	  char ***player)
#else
void
get_info (extra, level, player)
     char ***extra;
     char ***level;
     char ***player;
#endif
{
  *extra = extra_info;
  *level = level_info;
  *player = player_info;
}
/*
 * end of file info.c
 */


