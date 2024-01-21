/*
 * Program XBLAST V2.3.2 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 19th 1997
 * started August 1993
 *
 * File: main.c 
 * gameplay and main programm
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licence as published
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

#define _MAIN_C

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>
#include "include.h"
#include "mytypes.h"
#include "const.h"
#include "block.h"
#include "patchlev.h"
#include "intro.h"
#include "sprite.h"
#include "score.h"
#include "graphics.h"
#include "func.h"
#include "maze.h"
#include "map.h"
#include "bomb.h"
#include "data.h"
#include "event.h"
#include "shrink.h"
#include "status.h"
#include "main.h"
#include "setup.h"
#include "pipe.h"
#include "info.h"

#if defined(XBLAST_SOUND)
#include "sound.h"

static int play_music = 0;
static int act_score = SND_SNG1;
static int score_playing;

#endif

/* 
 * some constants 
 */

#define BOMB_STEP 2
#define TIME_STEP 48 /* 48 */
#define GAME_TIME (60*TIME_STEP + DEAD_TIME)
#define MIN_RANGE 2
#define EXTRA_RETRY 4

/* 
 * local function prototypes 
 */
/* 
 * global variables 
 */

/* Player Stats */
static BMPlayer player_stat[2*MAX_PLAYER];
static PlayerStrings p_string[2*MAX_PLAYER];
static XBConfig Config;
static XBSettings Setup;
static int num_player;
static int game_mode;

#ifdef DEBUG
static XBDebug Debug;
#endif

/* special functions */
static void (*special_init_function)();
static void (*special_game_function)();
static void (*special_extra_function)();
static void (*special_key_function)();


/*
 * local variables
 */

static int num_disp;
static int pause_status = -1;
static int active_player;
static int last_team;
static int level;
static int num_victories = 0;
static int min_range, min_bombs;
static BMHealth revive_health;
static void (*sound_function)();


/*
 * global variables 
 */
int game_time;
int FrameTime;


/*
 * public function exit_prg
 */
#ifdef __STDC__
void
exit_prg (int status)
#else
void
exit_prg (status)
     int status;
#endif
{
  int disp;

  /* close connection to X-Server */
  fprintf(stderr, "Disconnecting from X-server.\n");
  for (disp = 0; disp<num_disp; disp++) {
    finish_display(disp);
  }
#if defined(XBLAST_SOUND)
  /* close connection to sound server */
  if (Config.com_mode != CM_Child) {
    fprintf(stderr, "Shutting down sound server.\n");
    stop_sound_server();
  };
#endif
  fprintf(stderr, "Quit!\n");
  exit(status);
}

/* 
 * local function bm_fatal 
 */
#ifdef __STDC__
static void 
bm_fatal (char *text,
	  char *prog)
#else
static void 
bm_fatal (text,prog)
     char *text,*prog;
#endif
{
  fprintf(stderr,"ERROR : %s.\n",text);
  fprintf(stderr,"Type %s -? for help\n",prog); 
  exit_prg(2);
}


/*
 * public function setup_special_funcs
 */
#ifdef __STDC__
void
setup_funcs (BMFuncData *cur_level)
#else
void
setup_funcs (cur_level)
     BMFuncData *cur_level;
#endif
{
  /* set info */
  set_info_func(cur_level);
  /* set funcs */
  special_init_function = cur_level->init_func;
  special_game_function = cur_level->game_func;
  special_extra_function = cur_level->extra_func;
  special_key_function = cur_level->key_func;
}


static int sposswap[MAX_PLAYER];
static int maxShuffle;

/* starting position offsets for double mode */
static BMPosition delta_pos[MAX_PM][4][2] = {
  /* same position */
  {
    { {0, 0}, {0, 0}, },
    { {0, 0}, {0, 0}, },
    { {0, 0}, {0, 0}, },
    { {0, 0}, {0, 0}, },
  },
  /* polar position */
  {
    { {BLOCK_HEIGHT, 0}, {0, BLOCK_WIDTH}, },
    { {BLOCK_HEIGHT, 0}, {0, -BLOCK_WIDTH}, },
    { {-BLOCK_HEIGHT, 0}, {0, -BLOCK_WIDTH}, },
    { {-BLOCK_HEIGHT, 0}, {0, BLOCK_WIDTH}, },
  },
  /* right position */
  {
    { {0, 0}, {0, BLOCK_WIDTH} },
    { {0, 0}, {0, BLOCK_WIDTH} },
    { {0, 0}, {0, BLOCK_WIDTH} },
    { {0, 0}, {0, BLOCK_WIDTH} },
  },
  /* inner position */
  {
    { {0, 0}, {BLOCK_HEIGHT, BLOCK_WIDTH} },
    { {0, 0}, {BLOCK_HEIGHT, -BLOCK_WIDTH} },
    { {0, 0}, {-BLOCK_HEIGHT, -BLOCK_WIDTH} },
    { {0, 0}, {-BLOCK_HEIGHT, BLOCK_WIDTH} },
  },
  /* left & right position */
  {
    { {0, -BLOCK_WIDTH}, {0, BLOCK_WIDTH} },
    { {0, -BLOCK_WIDTH}, {0, BLOCK_WIDTH} },
    { {0, -BLOCK_WIDTH}, {0, BLOCK_WIDTH} },
    { {0, -BLOCK_WIDTH}, {0, BLOCK_WIDTH} },
  },
  /*  position below */
  {
    { {0, 0}, {BLOCK_HEIGHT, 0}, },
    { {0, 0}, {BLOCK_HEIGHT, 0}, },
    { {0, 0}, {BLOCK_HEIGHT, 0}, },
    { {0, 0}, {BLOCK_HEIGHT, 0}, },
  },
  /* horizontal positioning */
  {
    { {0, 0}, {0, BLOCK_WIDTH} },
    { {0, 0}, {0, -BLOCK_WIDTH} },
    { {0, 0}, {0, -BLOCK_WIDTH} },
    { {0, 0}, {0, BLOCK_WIDTH} },
  },
  /* vertical positioning */
  {
    { {0,0}, {BLOCK_HEIGHT, 0} },
    { {0,0}, {BLOCK_HEIGHT, 0} },
    { {0,0}, {-BLOCK_HEIGHT, 0} },
    { {0,0}, {-BLOCK_HEIGHT, 0} },
  },
  /* circle positioning */
  {
    { {-BLOCK_HEIGHT, 0 }, {BLOCK_HEIGHT, 0} },
    { {0, -BLOCK_WIDTH}, {0, BLOCK_WIDTH} },
    { {BLOCK_HEIGHT, 0 }, {-BLOCK_HEIGHT, 0} },
    { {0, BLOCK_WIDTH}, {0, -BLOCK_WIDTH} },
  },
};

#ifdef __STDC__
static void 
init_shuffle_startpos (int npos)
#else
static void 
init_shuffle_startpos (npos)
     int npos;
#endif
{
  int i,x;
  int j;
  int a;

  maxShuffle = npos;

  for(i=0; i<npos; i++) {
    sposswap[i] = i;
  }
  for(x=0; x<(npos-1); x++) {
    for(i=0; i<npos; i++) {
      j = random_number(npos);
      a           = sposswap[i];
      sposswap[i] = sposswap[j];
      sposswap[j] = a;
    }
  }
}

/*
 * public function setup_players
 */
#ifdef __STDC__
void
setup_players (unsigned long g_mode, 
	       BMPlayerData *data)
#else
void
setup_players (g_mode, data)
     unsigned long g_mode;
     BMPlayerData *data;
#endif
{
  BMPlayer *ps;
  int player;
  static int tmpx[MAX_PLAYER];
  static int tmpy[MAX_PLAYER];

  /* set player info */
  set_info_player(data);

  min_range = (int) data->range;
  min_bombs = (int) data->bombs;

  revive_health = data->init_health;

  /* Shuffle start positions (if desired) */
  
  /* Must load for all players as we scramble the x/y positions */
  for (player = 0; player < MAX_PLAYER; player ++) {
    ps = player_stat + player;

    if ( GM_NoGrid & g_mode) {
      ps->y = data->position[player].y;
      ps->x = data->position[player].x;
    } else {
      ps->y = (data->position[player].y-1) * BLOCK_HEIGHT;
      ps->x =  data->position[player].x * BLOCK_WIDTH;
    }
  }
  if ( (Setup.random_spos) && ( g_mode & GM_Random) 
      && !(game_mode & GM_Double) ) {
    int i;
    
    for(i=0;i<maxShuffle;i++) {
      tmpx[i] = player_stat[i].x;
      tmpy[i] = player_stat[i].y;
    }
    for(i=0;i<maxShuffle;i++) {
      player_stat[i].x = tmpx[sposswap[i]];
      player_stat[i].y = tmpy[sposswap[i]];
    }
  }

  /* setup other player attribtes */
  for (player = 0; player < num_player; player ++) {
    ps = player_stat + player;

    if (game_mode & GM_Double) {
      /* set new player positions here */
      if (player >= (num_player/2)) {
	ps->y = player_stat[player-(num_player/2)].y 
	  - data->pm_radius*delta_pos[data->pos_mod][player-(num_player/2)][0].y
	  + data->pm_radius*delta_pos[data->pos_mod][player-(num_player/2)][1].y;
	ps->x = player_stat[player-(num_player/2)].x
	  - data->pm_radius*delta_pos[data->pos_mod][player-(num_player/2)][0].x
	  + data->pm_radius*delta_pos[data->pos_mod][player-(num_player/2)][1].x;
      } else {
	ps->y += data->pm_radius*delta_pos[data->pos_mod][player][0].y;
	ps->x += data->pm_radius*delta_pos[data->pos_mod][player][0].x;
      }
    } 

    ps->anime = 0;
    ps->invincible = NEW_INVINCIBLE;
    ps->illness = revive_health;
    ps->health = revive_health;
    ps->illtime = 0;
    ps->junkie = 0;
    ps->dying = 0;
    ps->stunned = 0;
    ps->lives = Setup.max_lives;
    ps->range = data->range;
    ps->bombs = data->bombs;
    ps->extra_flags = data->init_flags;
    ps->special_bombs = 0;
    ps->cloaking = 0;
    if (IF_RC & ps->extra_flags) {
      ps->remote_control = 1;
    } else {
      ps->remote_control = 0;
    }
    if (IF_Teleport & ps->extra_flags) {
      ps->teleport = 1;
    } else {
      ps->teleport = 0;
    }      
    if (IF_Kick & ps->extra_flags) {
      ps->kick = 1;
    } else {
      ps->kick = 0;
    }
    if (IF_Airpump & ps->extra_flags) {
      ps->air_button = TRUE;
    } else {
      ps->air_button = FALSE;
    }
    ps->num_extras = 0;
    ps->abort = FALSE;
    ps->d_ist = GoStop;
    ps->d_soll= GoStop;
    ps->d_look= GoDown;
  }
}


/*
 * local function init_players
 */
#ifdef __STDC__
static void
init_players (void)
#else
static void
init_players ()
#endif
{
  BMPlayer *ps;
  int player;

  for (player =0; player < Config.num_player; player++) {
    ps = player_stat + player;
    ps->victories = 0;
    ps->id = player;

    /* set partner and sprite if in team mode */
    switch (Config.team_mode) {
    case TM_Single:
      ps->sprite = player;
      ps->team = ps->id;
      break;

    case TM_Team:
      ps->sprite = player;
      if (player % 2) {
	ps->team = ps->id-1;;
      } else {
	ps->team = ps->id;
      }
      break;

    case TM_Double:
      if (player < Config.num_player/2) {
	ps->team = ps->id;
	ps->sprite = player;
      } else {
	ps->team = ps->id - Config.num_player/2;
	ps->sprite = player - (Config.num_player/2);
      }
    }
#ifdef DEBUG
    fprintf(stderr, "XBlast[%d]: Player(%d) @ Display(%d)\n",
	    (int) getpid(), ps->id, ps->disp);
#endif
  }
}


/* 
 * local function randomize_levels 
 */
#ifdef __STDC__
static void 
randomize_levels (int *feld)
#else
static void 
randomize_levels (feld)
     int *feld;
#endif
{
  int i, j, k;
  int hilf;
  int old_end;
  
  old_end = feld[levelMax-1];

  for (k=0; k<4; k++) {
    for (i=0; i < levelMax; i++) {
      j = random_number(levelMax);
      hilf = feld[i];
      feld[i] = feld[j];
      feld[j] = hilf;
    }
  }

  if (old_end == feld[0]) {
    j = 1 + random_number(levelMax-1);
    hilf = feld[0];
    feld[0] = feld[j];
    feld[j] = hilf;
  }
}



/* 
 * global function drop_bomb 
 */
#ifdef __STDC__
void 
drop_bomb (BMPlayer *ps,
	   int type)
#else
void 
drop_bomb(ps, type)
     BMPlayer *ps;
     int type;
#endif
{
  if (ps->bombs !=0 && ps->illness!=IllEmpty
      && (type == BMTdefault || ps->special_bombs > 0)) {
    if (ps->lives >0) {
      if (new_player_bomb(ps, type)) {
#if defined(XBLAST_SOUND)
	play_sound(SND_DROP, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
	ps->bombs --;
      }
      if (type != BMTdefault) {
	ps->special_bombs --;
      }
    }
  }
}



/* 
 * local fucntion walk_stop 
 */
#ifdef __STDC__
static int 
walk_stop (BMPlayer *ps,
	   int flag,
	   int mazex, 
	   int mazey)
#else
static int 
walk_stop (ps, flag, mazex, mazey)
     BMPlayer *ps;
     int flag;
     int mazex, mazey;
#endif
{
  if (ps->illness != IllReverse) {
    switch(ps->d_look) {
    case GoDown:
      ps->anime = 0;
      break;
    case GoUp:
      ps->anime = 3;
      break;
    case GoLeft:
      ps->anime = 9;
      break;
    case GoRight:
      ps->anime = 6;
      break;
    default:
      break;
    }
  }else {
    switch(ps->d_look) {
    case GoDown:
      ps->anime = 3;
      break;
    case GoUp:
      ps->anime = 0;
      break;
    case GoLeft:
      ps->anime = 6;
      break;
    case GoRight:
      ps->anime = 9;
      break;
    default:
      break;
    }
  }
  return FALSE;
}



/* 
 * local function walk_up 
 */
#ifdef __STDC__
static int 
walk_up (BMPlayer *ps,
	 int flag,
	 int mazex, 
	 int mazey)
#else
static int 
walk_up (ps, flag, mazex, mazey)
     BMPlayer *ps;
     int flag;
     int mazex, mazey;
#endif
{
  int look_to_wall = FALSE;

  if ( !(flag && check_maze(mazex, mazey-1))) {
    ps->y -= STEP_VERT;
    mazey = ps->y/BLOCK_HEIGHT+1;
  } else {
    ps->d_ist = GoStop;
    look_to_wall = TRUE;
  }
  
  if (ps->illness != IllReverse) {
    switch((ps->y/STEP_VERT) % 4) {
    case 0:
    case 2:
      ps->anime = 3;
      break;
    case 1:
      ps->anime = 4;
      break;
    case 3:
      ps->anime = 5;
      break;
    }
  } else {
    switch((ps->y/STEP_VERT) % 4) {
    case 0:
    case 2:
      ps->anime = 0;
      break;
    case 1:
      ps->anime = 1;
      break;
    case 3:
      ps->anime = 2;
      break;
    }
  }

  if ( check_bomb(mazex, mazey) 
      && ( (ps->y % BLOCK_HEIGHT) == (STEP_VERT*BOMB_STEP) ) ) {
    if (ps->kick) {
#if defined(XBLAST_SOUND)
      play_sound(SND_KICK, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
      move_bomb(mazex, mazey, GoUp);
      ps->d_soll = GoStop;
    }
    ps->y += STEP_VERT;
  }

  return look_to_wall;
}



/* 
 * local function walk_left 
 */
#ifdef __STDC__
static int 
walk_left (BMPlayer *ps,
	   int flag,
	   int mazex, 
	   int mazey)
#else
static int 
walk_left (ps, flag, mazex, mazey)
     BMPlayer *ps;
     int flag;
     int mazex, mazey;
#endif
{
  int look_to_wall = FALSE;

  if ( !(flag && check_maze(mazex -1, mazey) )) {
    ps->x -= STEP_HORI;
    mazex = ps->x/BLOCK_WIDTH;
  } else {
    ps->d_ist = GoStop;
    look_to_wall = TRUE;
  }

  if (ps->illness != IllReverse) { 
    switch(ps->x % (STEP_HORI*4)) {
    case 0:
    case (STEP_HORI*2):
      ps->anime = 9;
      break;
    case STEP_HORI:
      ps->anime = 10;
      break;
    case (STEP_HORI*3):
      ps->anime = 11;
      break;
    }
  } else {
    switch(ps->x % (STEP_HORI*4)) {
    case 0:
    case (STEP_HORI*2):
      ps->anime = 6;
      break;
    case STEP_HORI:
      ps->anime = 7;
      break;
    case (STEP_HORI*3):
      ps->anime = 8;
      break;
    }
  }

  /* do kick and walk */
  if ( check_bomb(mazex, mazey) 
      && ((ps->x % BLOCK_WIDTH) == (STEP_HORI*BOMB_STEP) ) ) {
    if (ps->kick) {
#if defined(XBLAST_SOUND)
      play_sound(SND_KICK, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
      move_bomb(mazex, mazey, GoLeft);
      ps->d_soll = GoStop;
    }
    ps->x += STEP_HORI;
  }

  return look_to_wall;
}



/* 
 * local function walk_down 
 */
#ifdef __STDC__
static int 
walk_down (BMPlayer *ps,
	   int flag,
	   int mazex, 
	   int mazey)
#else
static int 
walk_down (ps, flag, mazex, mazey)
     BMPlayer *ps;
     int flag;
     int mazex, mazey;
#endif
{
  int look_to_wall = FALSE;

  if ( !(flag && check_maze(mazex, mazey+1) )) {
    ps->y += STEP_VERT;
    mazey = ps->y/BLOCK_HEIGHT + 1;
  } else {
    look_to_wall = TRUE;
      ps->d_ist = GoStop;
  }
  
  if (ps->illness == IllReverse) {
    switch(ps->y % (STEP_VERT*4))  {
    case 0:
    case (2*STEP_VERT):
      ps->anime = 3;
      break;
    case STEP_VERT:
      ps->anime = 4;
      break;
    case (STEP_VERT*3):
      ps->anime = 5;
      break;
    }
  } else {
    switch(ps->y % (STEP_VERT*4)) {
    case 0:
    case (STEP_VERT*2):
      ps->anime = 0;
      break;
    case STEP_VERT:
      ps->anime = 1;
      break;
    case (STEP_VERT*3):
      ps->anime = 2;
      break;
    }
  }

  if ( check_bomb(mazex, mazey+1)
      && ( (ps->y % BLOCK_HEIGHT) == (BLOCK_HEIGHT - STEP_VERT*BOMB_STEP) )
      ) {
    if (ps->kick) {
#if defined(XBLAST_SOUND)
      play_sound(SND_KICK, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
      move_bomb(mazex, mazey+1,	GoDown);
      ps->d_soll = GoStop;
    }
    ps->y -= STEP_VERT;
  }

  return(look_to_wall);
}



/* 
 * local function walk_right 
 */
#ifdef __STDC__
static int 
walk_right (BMPlayer *ps,
	    int flag,
	    int mazex, 
	    int mazey)
#else
static int 
walk_right (ps, flag, mazex, mazey)
     BMPlayer *ps;
     int flag;
     int mazex, mazey;
#endif
{
  int look_to_wall = FALSE;

  if ( !(flag && check_maze(mazex +1, mazey))) {
    ps->x += STEP_HORI;
    mazex = ps->x/BLOCK_WIDTH;
  } else {
    look_to_wall = TRUE;
    ps->d_ist = GoStop;
  }
  
  if (ps->illness == IllReverse) { 
    switch(ps->x % (STEP_HORI*4)) {
    case 0:
    case (STEP_HORI*2):
      ps->anime = 9;
      break;
    case STEP_HORI:
      ps->anime = 10;
      break;
    case (STEP_HORI*3):
      ps->anime = 11;
      break;
    }
  } else {
    switch(ps->x % (STEP_HORI*4)) {
    case 0:
    case (STEP_HORI*2):
      ps->anime = 6;
      break;
    case STEP_HORI:
      ps->anime = 7;
      break;
    case (STEP_HORI*3):
      ps->anime = 8;
      break;
    }
  }

  /* do kick */
  if ( check_bomb(mazex+1, mazey)
      && ( (ps->x % BLOCK_WIDTH) == (BLOCK_WIDTH - STEP_HORI*BOMB_STEP) ) ) {
    if (ps->kick) {
#if defined(XBLAST_SOUND)
      play_sound(SND_KICK, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
      move_bomb(mazex+1, mazey, GoRight);
      ps->d_soll = GoStop;
    }
    ps->x -= STEP_HORI;
  }
  

  return look_to_wall;
}


/*
 * local function teleport_player
 */
#ifdef __STDC__
static int
teleport_player (BMPlayer *ps,
		 int mazex,
		 int mazey) 
#else
static int
teleport_player (ps, mazex, mazey) 
     BMPlayer *ps;
     int mazex, mazey;
#endif
{
  /* teleport dri@eup.siemens-albis.ch */
  /* 'No spare space' bug killed by Garth Denley */
  /* Horizontal/Vertical teleport re-enabled */
  int new_mazex, new_mazey;
  int tele_tries;
  int tele_success;
  
  tele_tries = 25;
  tele_success = FALSE;
  
  do {
    new_mazex = random_number(MAZE_W);
    new_mazey = random_number(MAZE_H);
    tele_tries--;
    if ((!check_maze(new_mazex,new_mazey))
	&& ((mazex != new_mazex) || (mazey != new_mazey))) {
      tele_success = TRUE;
    }
  } while ((!tele_success) && (tele_tries>0) );
  
  if (tele_success) {
#if defined(XBLAST_SOUND)
	play_sound(SND_TELE1, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
    mazex = new_mazex;
    mazey = new_mazey;
    ps->x = BLOCK_WIDTH * mazex;
    ps->y = BLOCK_HEIGHT * (mazey - 1);
    ps->d_soll = GoStop;
    ps->d_look = GoDown;
#if defined(XBLAST_SOUND)
	play_sound(SND_TELE2, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
  }

  return tele_success;
}


/*
 * array of walk functions
 */

static PFI walk_dir[MAX_DIR] = {
  walk_stop,
  walk_up,
  walk_left,
  walk_down,
  walk_right,
  walk_stop,
};

/* 
 * local function do_walk 
 */
#ifdef __STDC__
static void 
do_walk (BMPlayer *ps)
#else
static void 
do_walk (ps)
     BMPlayer *ps;
#endif
{
  int flag;
  int look_to_wall = FALSE;
  int mazex,mazey;
  int i;
  int xalt,yalt;
  int spm_mode;

  xalt = ps->x;
  yalt = ps->y;

  if ( !( (ps->illness == IllSlow) && (game_time & 0x01) ) ) {
    for (i=0; i<=(ps->illness == IllRun); i++) {
      flag = FALSE;
      
      mazex = ps->x / BLOCK_WIDTH;
      mazey = ps->y / BLOCK_HEIGHT + 1;
      
      if ( ( (ps->x % BLOCK_WIDTH) == 0)
	  && ((ps->y % BLOCK_HEIGHT) == 0) ) {
	flag = TRUE;
	
	if (ps->teleport == TELEPORT_TIME) {
	  if (teleport_player(ps, mazex, mazey)) {
	    ps->teleport--;
	  }
	}

	ps->d_ist = ps->d_soll;
	if (ps->d_ist != GoStop)
	  ps->d_look = ps->d_ist;
      }
      
      /* random teleporting */
      if ( (ps->illness == IllTeleport) && (0 == random_number(32) ) ) {
	mark_maze_rect (ps->x + SPRITE_X_OFF, ps->y + SPRITE_Y_OFF,
			SPRITE_WIDTH, SPRITE_HEIGHT);
	teleport_player(ps, mazex, mazey);
	ps->d_ist = GoStop;
	ps->d_soll = GoStop;
	mark_maze_rect (ps->x + SPRITE_X_OFF, ps->y + SPRITE_Y_OFF,
			SPRITE_WIDTH, SPRITE_HEIGHT);
      }
	
      
      look_to_wall = (*walk_dir[ps->d_ist])(ps, flag, mazex, mazey);
 
      /* insert get _extra here */
      if ( (ps->x % BLOCK_WIDTH == 0) && (ps->y % BLOCK_HEIGHT == 0) ) {
	switch(get_extra(ps->invincible, ps->x / BLOCK_WIDTH ,
			 ps->y / BLOCK_HEIGHT + 1 ))
	  {
	  case BTBomb:
#if defined(XBLAST_SOUND)
            play_sound(SND_NEWBOMB, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
	    ps->bombs ++;
	    break;
	  case BTRange:
#if defined(XBLAST_SOUND)
            play_sound(SND_MOREFIRE, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
	    if (ps->range < MAX_RANGE)
	      ps->range ++;
	    break;
	  case BTSick:
	    ps->illtime = ILLTIME;
	    ps->illness = random_number(MAX_ILL)+1;

#if defined(XBLAST_SOUND)
	    if (ps->illness == IllInvisible) {
	      play_sound(SND_INVIS, ps->x / (PIXW / MAX_SOUND_POSITION));
	    } else {
	      play_sound(SND_BAD, ps->x / (PIXW / MAX_SOUND_POSITION));
	    } 
#endif
	    if (ps->illness == IllReverse) {
	      switch (ps->d_ist) {
	      case GoDown:
		ps->d_ist = GoUp;
		break;
	      case GoUp:
		ps->d_ist = GoDown;
		break;
	      case GoLeft:
		ps->d_ist = GoRight;
		break;
	      case GoRight:
		ps->d_ist = GoLeft;
		break;
	      default:
		break;
	      }
	    }
	    break;

	  case BTSpecial:
	    ps->num_extras++;
	    (*special_extra_function)(ps);
	    break;
	  }
      }
    }
  }

  /* ckeck if player really has to be redrawn */
  if (look_to_wall || (ps->teleport > 1) 
      || (ps->d_ist != GoStop) || (ps->invincible > 0)
      || (ps->dying > 0) || (ps->stunned >0) || (ps->cloaking < 0) ) {
    mark_maze_rect (xalt + SPRITE_X_OFF, yalt + SPRITE_Y_OFF,
		    SPRITE_WIDTH, SPRITE_HEIGHT);
    mark_maze_rect (ps->x + SPRITE_X_OFF, ps->y + SPRITE_Y_OFF,
		    SPRITE_WIDTH, SPRITE_HEIGHT);
  }

  /* draw player if not totally invisible */
  if (ps->illness != IllInvisible) {
    /* set default mode */
    spm_mode = SPM_ALL_DISPLAYS;
    /* first check for cloak */
    if (ps->cloaking < 0) {
      ps->cloaking ++;
      if (ps->cloaking & 0x01) {
	spm_mode = ps->disp;
      } else {
	spm_mode = SPM_NO_DISPLAY;
      }
    }
    /* blinking if invincible */
    if(ps->invincible > 0) {
      ps->invincible --;
      if (ps->invincible & 0x01) {
	spm_mode |= SPM_MASK_ONLY;
      }
      /* or slower blinking if arrived from teleport */
    } else if (ps->teleport > 1) {
      ps->teleport --;
      if ( (ps->teleport>>1) & 0x01) {
	spm_mode |= SPM_MASK_ONLY;
      }
    }
    /* now draw sprite */
    add_player_to_sprite_list (ps->sprite, ps->x, ps->y, ps->anime, spm_mode);
  }

  /* is player still sick? */
  if (ps->illness != ps->health) {
    /* decrement illness timer */
    if ( (ps->illtime --) == 0) {
      /* heal if time is over */
      ps->illness = ps->health;
    }
  }

  /* drop random bombs if needed */
  if ( (ps->x % BLOCK_WIDTH == 0) && (ps->y % BLOCK_HEIGHT == 0) ) {
    if (ps->illness == IllBomb) {
      if ( random_number(4) != 0) {
	drop_bomb(ps, BMTdefault);
      }
    }
  }
}

#define JUNKIE_ILL_TIME (ILLTIME)
#define JUNKIE_STUN_TIME 12
#define JUNKIE_TIME_1 360
#define JUNKIE_TIME_2 210
#define JUNKIE_TIME_3 60   /* Speed */

/*
 * local function do_junkie
 */
#ifdef __STDC__
static void
do_junkie (void)
#else
static void
do_junkie ()
#endif
{
  BMPlayer *ps1;

  /* Junkie countdown */
  for (ps1 = player_stat; ps1 < player_stat + num_player; ps1++) {
    if ((ps1->lives) && (ps1->junkie)) {
      /* Junkie sickness */
      switch (--(ps1->junkie)) {
      case JUNKIE_TIME_1:
      case JUNKIE_TIME_2:
        /* Give a random illness */
        ps1->illtime = JUNKIE_ILL_TIME;
	ps1->illness = random_number(MAX_ILL)+1;
	break;
	
      case JUNKIE_TIME_3:
        /* Stun player and give speed */
        ps1->stunned += JUNKIE_STUN_TIME;
        ps1->illtime = JUNKIE_ILL_TIME;
        ps1->illness = IllRun;
	break;

      case 0:
        /* Too long! Take a hit. */
        ps1->dying = DEAD_TIME;
        ps1->junkie = MAX_JUNKIE_TIME;
	break;
      }
    }
  }
}


/*
 * local function infect_other_players
 */
#ifdef __STDC__
static void 
infect_other_players (void)
#else
static void 
infect_other_players ()
#endif
{
  BMPlayer *ps1,*ps2;

  for (ps1 = player_stat; ps1 < player_stat+num_player; ps1 ++) {
    for (ps2= ps1+1; ps2 < player_stat+num_player; ps2 ++) {
      if ( (ABS(ps1->x - ps2->x) < ILL_X)
	  && (ABS(ps1->y - ps2->y) < ILL_Y)) {
	/* infection with "normal" viruses */
	if (ps1->illness != ps2->illness) {
	  if ( (! ps2->invincible) &&  (ps1->illtime > ps2->illtime) ) {
	    ps2->illness = ps1->illness;
	    ps2->illtime = ILLTIME;
	  } else if ( (! ps1->invincible) &&  (ps2->illtime > ps1->illtime) ) {
	    ps1->illness = ps2->illness;
	    ps1->illtime = ILLTIME;
	  }
	}
	/* infection with junkie virus */
	if ( ( (ps2->junkie) && (!ps1->invincible) ) || (ps1->junkie) ) {
	  ps1->junkie = MAX_JUNKIE_TIME;
	}
	if ( ( (ps1->junkie) && (!ps2->invincible) ) || (ps2->junkie) ) {
	  ps2->junkie = MAX_JUNKIE_TIME;
	} 
      }
    }
  }
}


/*
 * local functzion have_a_gloat
 */
#ifdef __STDC__
static void 
have_a_gloat (int player)
#else
static void 
have_a_gloat (player)
     int player;
#endif
{
  int g,gloatpl,gloatpltt;
  
  gloatpl = -1;
  for(g=0; g<6; g++) {
    gloatpltt = random_number(num_player);
    if ((gloatpltt != player) && (player_stat[gloatpltt].lives > 0)) {
      gloatpl = gloatpltt;
      break;
    }
  }      
  if(gloatpl>-1) {
    set_message(p_string[gloatpl].gloat, FALSE);
  }
}


/*
 * local function kill_player_at
 */
#ifdef __STDC__
void
kill_player_at (int x, int y)
#else
void
kill_player_at (x,y)
     int x, y;
#endif
{
  BMPlayer *ps;
  int player;

  for (player = 0; player < num_player; player ++) {
    ps = player_stat + player;
    if ( ps->lives > 0 ) {
      if (    (ps->x < (x+1)*BLOCK_WIDTH)
           && (ps->x > (x-1)*BLOCK_WIDTH)
           && (ps->y < (y)  *BLOCK_HEIGHT)
           && (ps->y > (y-2)*BLOCK_HEIGHT) ) {
        ps->lives = 1;
        ps->dying = DEAD_TIME;
      }
    }
  }
}


/*
 * public function kill_other_players
 */
#ifdef __STDC__
int
kill_other_players (int team)
#else
int
kill_other_players (team)
  int team;
#endif
{
  int count = 0;
  int player;

  for (player = 0; player <num_player; player ++) {
    if ( (player_stat[player].team != team) 
	 && (player_stat[player].lives > 0) ) {
      player_stat[player].dying= DEAD_TIME;
      count ++;
    }
  }

  return count;
}


/*
 * local function revive_player 
 */
#ifdef __STDC__
static void 
revive_player (BMPlayer *ps)
#else
static void 
revive_player (ps)
     BMPlayer *ps;
#endif 
{
  BMPlayer *ptr;
  PlayerStrings *st;
  int i, team_alive;
  
  st = p_string + ps->id;

  ps->lives --;

  if (ps->lives == 0) {
#if defined(XBLAST_SOUND)
    play_sound(SND_DEAD, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
    team_alive = FALSE;
    for (i = 0, ptr = player_stat; i<num_player; i++, ptr++) {
      if (ptr->team == ps->team) {
	team_alive |= (ptr->lives != 0);
      }
    }
    if (!team_alive) {
      active_player --;
    }
    
    distribute_extras(ps->bombs - min_bombs, 
		      ps->range - min_range,
		      ps->num_extras,
		      ps->special_bombs);
    set_message (st->loselevel, FALSE);
  }  else {
#if defined(XBLAST_SOUND)
    play_sound(SND_OUCH, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
    distribute_extras(0, 0,
		      ps->num_extras,
		      ps->special_bombs);
    set_message (st->loselife, FALSE);
  }

  have_a_gloat(ps->id);

  /* reset values */
  ps->invincible = NEW_INVINCIBLE;
  ps->dying = 0;
  ps->stunned = 0;
  ps->illness = revive_health;
  ps->health = revive_health;
  ps->illtime = 0;
  ps->teleport = 0;
  ps->cloaking = 0;

  /* Note that junkie ISN'T reset (not a bug) */

  /* very important */
  if (ps->remote_control > 0) {
    ignite_players_bombs(ps);
  }

  ps->remote_control = 0;
  ps->kick = 0;
  ps->air_button = FALSE;

  /* If special bombs are distributed, then zero the count */
  if (distrib_special()) {
    ps->special_bombs = 0;
  }

  /* Reset extra pickup count */
  ps->num_extras = 0;

  /* reset inital extras */
  if (IF_RC & ps->extra_flags) {
    ps->remote_control = 1;
  }
  if (IF_Teleport & ps->extra_flags) {
    ps->teleport = 1;
  }
  if (IF_Kick & ps->extra_flags) {
    ps->kick = 1;
  }
  if (IF_Airpump & ps->extra_flags) {
    ps->air_button = 1;
  }
  mark_maze_rect (ps->x + SPRITE_X_OFF, ps->y + SPRITE_Y_OFF,
		  SPRITE_WIDTH, SPRITE_HEIGHT);
}



/*
 * local function do_stunned
 */
#ifdef __STDC__
static void 
do_stunned (BMPlayer *ps)
#else
static void 
do_stunned (ps)
     BMPlayer *ps;
#endif
{
  switch( (ps->d_look + ps->stunned)%4 ) {
  case GoDown:
    ps->anime = 0;
    break;
  case GoUp:
    ps->anime = 3;
    break;
  case GoLeft:
    ps->anime = 9;
    break;
  case GoRight:
    ps->anime = 6;
    break;
  }
  
  add_player_to_sprite_list (ps->sprite, ps->x, ps->y, ps-> anime, 
			     SPM_ALL_DISPLAYS);
  
  mark_maze_rect (ps->x + SPRITE_X_OFF, ps->y + SPRITE_Y_OFF,
		  SPRITE_WIDTH, SPRITE_HEIGHT);
  ps->stunned -- ;
}



/*
 * local function do_die
 */
#ifdef __STDC__
static void 
do_die (BMPlayer *ps)
#else
static void 
do_die (ps)
     BMPlayer *ps;
#endif
{
  if (ps->lives > 1) {
    add_player_to_sprite_list (ps->sprite, ps->x,ps->y, 13, SPM_ALL_DISPLAYS);
  } else {
    add_player_to_sprite_list (ps->sprite, ps->x,ps->y, 12, SPM_ALL_DISPLAYS);
  }
  if (ps->dying == DEAD_TIME) {
    mark_maze_rect (ps->x + SPRITE_X_OFF, ps->y + SPRITE_Y_OFF,
		    SPRITE_WIDTH, SPRITE_HEIGHT);
  }
  ps->dying -- ;
}


/*
 * local function mark_overlapping_players
 *  not very elegant ...
 */
#ifdef __STDC__
static void
mark_overlapping_players (void)
#else
static void
mark_overlapping_players ()
#endif
{
  int p1, p2;
  BMPlayer *ps1, *ps2;

  for (p1 = 0, ps1 = player_stat; p1 < (num_player-1); p1++, ps1++) {
    if ( (ps1->lives) && (0 == (ps1->x % BLOCK_WIDTH)) 
	&& (0 == (ps1->y % BLOCK_HEIGHT)) ) {
      for (p2 = p1+1, ps2 = ps1+1; p2 < num_player; p2++, ps2++) {
	if ( (ps2->lives) && (0 == (ps2->x % BLOCK_WIDTH)) 
	    && (0 == (ps2->y % BLOCK_HEIGHT))) {
	  if ( (ps1->x == ps2->x) && (ps1->y == ps2->y)) {
	    mark_maze (ps1->x/BLOCK_WIDTH, ps1->y/BLOCK_HEIGHT,
		       ps1->x/BLOCK_WIDTH, 1+ps1->y/BLOCK_HEIGHT);
	  }
	}
      }
    }
  }
}



/* 
 * after one game 
 */
#ifdef __STDC__
static void 
level_end (void)
#else
static void 
level_end ()
#endif
{
  static char *msg_oot  = "Out Of Time";
  static char *msg_draw = "Draw Game";
  char *Message;
  BMPlayer *ps;
  int active_teams = FALSE;
  int player;

#if 0
  playerList = get_player_list();
#endif

  /* draw winning players (if any) */
  clear_sprite_list();
  for (ps = player_stat; ps < player_stat + num_player; ps++) {
    if (ps->dying) {
      ps->lives --;
    }
    if (ps->lives) {
      active_teams = TRUE;
      add_player_to_sprite_list(ps->sprite, ps->x, ps->y, WINNER_ANIME, 
				SPM_ALL_DISPLAYS );
      mark_maze_rect (ps->x + WINNER_X_OFF, ps->y + WINNER_Y_OFF,
		      WINNER_WIDTH, WINNER_HEIGHT);
    }
  }
  sort_sprite_list();
  
  /* determine game result */
  if (game_time >= (GAME_TIME-1)) {
    /* This game was out of time */
    last_team = 2*MAX_PLAYER;
    Message = msg_oot;
    /* print statistics */
    if (Setup.print_stat) {
      printf("LevelStat {%s} {%s} {", get_level_name(level), "Out Of Time");
      for (player =0; player < num_player; player ++) {
	printf(" {%s} ",p_string[player].name);
      }
      printf("}\n");
    }
  } else {
    /* either draw or win game */
    if (active_teams) {
      /* one player/team has won */
      last_team = 2*MAX_PLAYER;
      for (ps = player_stat; ps < player_stat + num_player; ps++) {
	if (ps->lives) {
	  last_team = ps->team;
	}
      }
      Message = p_string[last_team].winlevel;
      /* print statistics */
      if (Setup.print_stat)  {
	printf("LevelStat {%s} {%s} {", get_level_name(level), 
	       p_string[last_team].name);
	for (player =0; player < num_player; player ++) {
	  printf(" {%s} ",p_string[player].name);
	}
	printf("}\n");
      }
    } else {
      /* draw game */
      last_team = 2*MAX_PLAYER;
      Message = msg_draw;
      /* print statistics */
      if (Setup.print_stat) {
	printf("LevelStat {%s} {%s} {", get_level_name(level), "Draw Game");
	for (player =0; player < num_player; player ++) {
	  printf(" {%s} ",p_string[player].name);
	}
	printf("}\n");
      }
    }
  }
  
  /* add victories for player and team mates */
  for (ps = player_stat; ps < player_stat + num_player; ps++) {
    if (ps->team == last_team) {
      ps->victories ++;
      num_victories = MAX(num_victories, ps->victories);
    }
  }
    
  wait2_two_text(num_disp, num_player, player_stat, Message,"Press Space");
  
  fade_out(num_disp);
}


#ifdef __STDC__
int 
check_b_near (int x,
	      int y)
#else
int 
check_b_near (x,y)
     int x,y;
#endif
{
  int player;

  for (player = 0; player < num_player; player ++) {
    if ((ABS(x*BLOCK_WIDTH - player_stat[player].x) < BLOCK_WIDTH)
	&& (ABS(y*BLOCK_HEIGHT-BLOCK_HEIGHT - player_stat[player].y)  
	    < BLOCK_HEIGHT)) {
      return 0;
    }
  }
  return 1;
}


/*
 * local function config_normal_game
 *
 * setup controls for normal (not forked) game mode
 */
#ifdef __STDC__
static void 
config_normal_game (void)
#else
static void 
config_normal_game ()
#endif
{
  int disp, np, p1, p2;
#ifndef DEBUG
  int p3;
#endif

  /* no IPC or TCP/IP communication */
  Config.com_mode = CM_None;
  set_event_communication(CM_None, 0, 0);
  set_setup_communication(CM_None);

  /* set player display link to "none" */
  for (p1 = 0; p1 <num_player; p1++) {
    player_stat[p1].disp = SPM_NO_DISPLAY;
  }
  if (game_mode & GM_Double) {
    for (p1 = 0; p1 <num_player; p1++) {
      player_stat[p1 + num_player].disp = SPM_NO_DISPLAY;
    }
  }
  
  for (disp=0; disp < num_disp; disp ++) {
    /* find first player for display */
    for (p1 = 0; (p1<num_player) && (Config.pl_at_disp[p1] != disp) ; p1++);
    if (p1 == num_player) {
      fprintf(stderr, "Oops no player for display %d\n",disp);
      exit_prg(1);
    }
    /* set 2nd player for double mode */
    if (game_mode & GM_Double) {
      p2 = p1 + num_player;
      np = 2;
    }
    /* look for optional second one */
    for (p2 = p1+1; (p2<num_player) && (Config.pl_at_disp[p2] != disp) ; p2++);
    if (p2 < num_player) {
      /* only one double team per display */
      if (game_mode & GM_Double) {
#ifdef DEBUG
	p2 = p1 + num_player;
#else
	fprintf(stderr, "Error: 2 Teams for Display %s\n",
		Config.display[disp]);
	exit_prg(1);
#endif
      } 
      np = 2;
#ifndef DEBUG
      /* look for forbidden third one */
      for (p3 = p2+1; (p3<num_player) && (Config.pl_at_disp[p3]!=disp) ; p3++);
      if (p3 < num_player) {
	fprintf(stderr, "Error: 3 players for Display %s\n",
		Config.display[disp]);
	exit_prg(1);
      }
#endif
    } else {
      np = 1;
      p2 = p1;
    }

    /* set player(s) at display */
    Config.disp_player[disp].num = np;
    Config.disp_player[disp].p1  = p1;
    Config.disp_player[disp].p2  = p2;
    set_players_for_display(disp, p1, p2);
    player_stat[p1].disp = SPM_AT_DISPLAY(disp);
    player_stat[p2].disp = SPM_AT_DISPLAY(disp);
  }  
}


/*
 * local function config_forked_game
 *
 * setup controls for forked game mode
 */
#ifdef __STDC__
static void 
config_forked_game (void)
#else
static void 
config_forked_game ()
#endif
{
  int disp, np, p1, p2, flag;
#ifndef DEBUG
  int p3;
#endif

  /* set player display link to "none" */
  for (p1 = 0; p1 <num_player; p1++) {
    player_stat[p1].disp = SPM_NO_DISPLAY;
  }
  if (game_mode & GM_Double) {
    for (p1 = 0; p1 <num_player; p1++) {
      player_stat[p1 + num_player].disp = SPM_NO_DISPLAY;
    }
  }
  
  /* set communication to normal in the case we only have one display */
  set_event_communication(CM_None, 0, 0);
  set_setup_communication(CM_None);
  
  /* fork mode */
  for (disp=0; disp < num_disp; disp ++) {
    /* find first player for display */
    for (p1 = 0; (p1<num_player) && (Config.pl_at_disp[p1] != disp) ; p1++);
    if (p1 == num_player) {
      fprintf(stderr, "Oops no player for display %d\n",disp);
      exit_prg(1);
    }
    /* set 2nd player for double mode */
    if (game_mode & GM_Double) {
      p2 = p1 + num_player;
      np = 2;
    }
    /* look for optional second one */
    for (p2 = p1+1; (p2<num_player) && (Config.pl_at_disp[p2] != disp) ; p2++);
    /* fork if we are not in the first display */
    if (disp == 0) {
      flag = 0;
    } else {
      flag = create_child();
    }
    
    /* check number of players */
    if (p2 < num_player) {
      /* only one double team per display */
      if (game_mode & GM_Double) {
#ifdef DEBUG
	p2 = p1 + num_player;
#else
	fprintf(stderr, "Error: 2 Teams for Display %s\n",
		Config.display[disp]);
	exit_prg(1);
#endif
      }
      /* two player or more */
      np = 2;
      /* look for forbidden third one */
#ifndef DEBUG
      for (p3=p2+1; (p3<num_player)&&(Config.pl_at_disp[p3]!=disp) ; p3++);
      if (p3 < num_player) {
	fprintf(stderr, "Error: 3 players for Display %s\n",
		Config.display[disp]);
	exit_prg(1);
      }
#endif
    } else {
      /* one player */
      np = 1;
      p2 = p1;
    }

    if (!flag) {
      Config.disp_player[0].num = np;
      Config.disp_player[0].p1  = p1;
      Config.disp_player[0].p2  = p2;
      set_players_for_display(0, p1, p2);
      if (disp) {
	/* child */
	player_stat[p1].disp = SPM_AT_DISPLAY(0);
	player_stat[p2].disp = SPM_AT_DISPLAY(0);
	Config.com_mode = CM_Child;
	set_event_communication(CM_Child, p1, p2);
	set_setup_communication(CM_Child);
	Config.display[0]=Config.display[disp];
	Config.default_disp = -1;
	goto LeaveForkedConfig;
      }
    } else {
      /* parent */
      Config.com_mode = CM_Parent;
      set_event_communication(CM_Parent, p1, p2);
      set_setup_communication(CM_Parent);
    }
  }
  player_stat[Config.disp_player[0].p1].disp = SPM_AT_DISPLAY(0);
  player_stat[Config.disp_player[0].p2].disp = SPM_AT_DISPLAY(0);
  
 LeaveForkedConfig:
  /* only display per process, even for parent */
  Config.num_disp = num_disp = 1;
  return;
}

/*
 * local function eval_config
 */
#ifdef __STDC__
static void 
eval_config (char *prog_name)
#else
static void 
eval_config (prog_name)
     char *prog_name;
#endif
{
  
  /* set values derived from setup */
  num_player = Config.num_player;
  if (num_player <= 0) {
    bm_fatal("Number of players not defined",prog_name);
  }
  
  /* check number of players */

  num_disp = Config.num_disp;
  if (num_disp <= 0) {
    bm_fatal("Number of displays not defined",prog_name);
  }


  /* set game mode flag */
  switch (num_player) {
  case 2:
    game_mode = GM_2_Player;
    break;
  case 3:
    game_mode = GM_3_Player;
    break;
  case 4:
    game_mode = GM_4_Player;
    break;
  case 5:
    game_mode = GM_5_Player;
    break;
  case 6:
    game_mode = GM_6_Player;
    break;
  default:
    bm_fatal("Unsupported number of players", prog_name);
    break;
  }
  
  /* check number of players */
  switch (Config.team_mode) {
  case TM_Single:
    game_mode |= GM_Single;
    break;

  case TM_Team:
    if (! (game_mode & (GM_6_Player|GM_4_Player) ) ) {
      bm_fatal("Team mode only works with 4 or 6 players",prog_name);
    }
    game_mode |= GM_Team;
    break;

  case TM_Double:
    if (! (game_mode & (GM_234_Player) ) ) {
      bm_fatal("Double mode only works with 2, 3 or 4 teams",prog_name);
    }
    game_mode |= GM_Double;
    break;
  }


  /* set control, not very elegant, but it works */
  if (!Config.fork) {
    config_normal_game();
  } else {
    config_forked_game();
  }

  /* reset num_player for double mode */
  if (game_mode & GM_Double) {
    num_player *= 2;
    Config.num_player *= 2;
  }

#ifdef DEBUG
  {
    int cdd;

    cdd=Config.default_disp;
    fprintf(stderr, "Default Display is %d: %s\n", cdd,
	    (cdd>=0)?(Config.display[cdd]):("none"));
  }
#endif
}



/*
 * local function eval_setup
 */
#ifdef __STDC__
static void 
eval_setup (char *prog_name)
#else
static void 
eval_setup (prog_name)
     char *prog_name;
#endif
{
  int i, n_level;

  FrameTime = Setup.frame_time;
  if (Setup.sound_flag) {
    sound_function = do_bell;
  } else {
    sound_function = no_bell;
  }
  
  /* correct use_level field */
  n_level = 0;
  for (i=0; i<levelMax; i++) {
    if ( game_mode != (game_mode & get_game_mode(i))) {
      /* mode not supported by level */
      if (Setup.use_level[i]) {
	fprintf(stderr, "%s: Level \"%s\" not supported in this game mode.\n",
		prog_name, get_level_name(i) );
      }
      Setup.use_level[i]=FALSE;
    }
    if (Setup.use_level[i]) {
      n_level ++;
    }
  }
  if (n_level == 0) {
    fprintf (stderr, "%s: No Level left to play in this game mode.\n", 
	     prog_name);
    exit_prg(1);
  }
  /* set print_stat off for childs */
  if (Config.com_mode == CM_Child) {
    Setup.print_stat = FALSE;
  }
}


#ifdef XBLAST_SOUND
/*
 * local function load_level_sounds
 */
#ifdef __STDC__
static void
load_level_sounds (void)
#else
static void
load_level_sounds ()
#endif
{
  /* load samples which are frequently used 
     now: load all samples (for testing)  */
  load_sound(SND_BAD);
  load_sound(SND_DROP);
  load_sound(SND_NEWBOMB);
  load_sound(SND_NEWKICK);
  load_sound(SND_NEWPUMP);
  load_sound(SND_NEWRC);
  load_sound(SND_MOREFIRE);
  load_sound(SND_DEAD);
  load_sound(SND_EXPL);
  load_sound(SND_KICK);
  load_sound(SND_PUMP);
  load_sound(SND_OUCH);
  load_sound(SND_BUTT);
  load_sound(SND_SHOOT);
  load_sound(SND_INVIS);
  load_sound(SND_INVINC);
  load_sound(SND_NEWTELE);
  load_sound(SND_TELE);
  load_sound(SND_INJ);
  load_sound(SND_MINIBOMB);
  load_sound(SND_HAUNT);
  load_sound(SND_SPIRAL);
  load_sound(SND_SPBOMB);
  load_sound(SND_SLIDE);
  load_sound(SND_STUN);
  load_sound(SND_WARN);
  load_sound(SND_COMPOUND);
  /* these sample are moved up from the level end (ov) */
  load_sound(SND_APPL);
  load_sound(SND_WON);
  if (play_music) {
    load_sound(act_score);
  }
}
#endif



/* main program what else */
#ifdef __STDC__
int 
main (int argc,
      char *argv[])
#else
int 
main (argc, argv)
     int argc;
     char *argv[];
#endif
{
  int p, player, disp;
  int level_index;
  int *level_field;
  int setup_flag;
#ifdef DEBUG
  int ft_count;
  double ft_time;
  struct timeval ft_start_time, ft_end_time;
  struct timezone ft_tz;
#endif

  printf("%s\n",c_string);
  printf("Report any bugs to: vogel@ikp.uni-koeln.de\n");

  /* Initialize Random */
#ifndef DEBUG
  seed_random((unsigned)getpid());
#else
  seed_random(0);
#endif

  /* load levels now */
  load_all_levels();

  /* choose setup method */
  if (1 != argc) {
    parse_commandline(argc, argv);
    setup_flag = FALSE;
  } else {
    setup_flag = ! interactive_config(argv[0]);
  }

  /* get config from database */
  config_from_database(&Config);

  /* evaluate config */
  eval_config(argv[0]);

#ifdef DEBUG
  debug_from_database(&Debug);
#endif

  /* init_players */
  init_players();

#if defined(XBLAST_SOUND)
  /* Initialize sound server */
  if (Config.com_mode != CM_Child) {
    (void) init_sound(sound_server_from_database());
  }
#endif

  /* Initialize Displays and X Databases */
  for (disp=0; disp < num_disp; disp++) {
    init_display(disp, Config.display[disp]);
  }

  /* merge databases  */
  merge_default_databases(&Config);

  /* alloc use level field */
  if (NULL == (Setup.use_level = (char *) malloc(levelMax*sizeof(char))) ) {
    fprintf(stderr, "Failed to alloc use level array\n");
    exit_prg(1);
  }
  switch(Config.com_mode) {
  case CM_None:
    if (setup_flag) {
      interactive_setup(num_player);
    }       
    setup_from_database(&Setup);
    break;

  case CM_Parent:
    if (setup_flag) {
      /* run interactive setup and send data to child processes */
      interactive_setup(num_player);
      send_setup(num_player);
    }
    setup_from_database(&Setup);
    break;

  case CM_Child:
    if (setup_flag) {
      /* receive setup from parent, when parent uses interactive setup */
      receive_setup(num_player);
    } 
    /* get it from internal database*/
    setup_from_database(&Setup);
    break;
  }

  /* evaluate setup */
  eval_setup(argv[0]);

  /* get names and messages */
  player_strings_from_database(p_string, &Config);

  /* now save setup if needed */
  save_setup(&Config, &Setup, p_string);

  /* load status board and end screen */
  load_score_board(game_mode, Setup.max_victories);

  /* Initialize Graphics */
  for (disp=0; disp < num_disp; disp++) {
    char win_title[1024];

    if (Config.disp_player[disp].num == 1) {
      sprintf(win_title, win_string1, p_string[Config.disp_player[disp].p1].name);
    } else {
      sprintf(win_title, win_string2, p_string[Config.disp_player[disp].p1].name,
	      p_string[Config.disp_player[disp].p2].name);
    }
    init_graphics(disp, win_title);
  }

  /* now erase al databases */
  delete_databases(num_disp);

  /* setup status_bar */
  init_status_bar(&Config, player_stat, get_level_name(level),
		  FALSE);

#ifndef DEBUG
  /* Title Screen */
  do_intro(player_stat, p_string, &Config);
#else
  /* Title Screen */
  if (! Debug.no_title) {
    do_intro(player_stat, p_string, &Config);
  }
#endif

  /* the game */
  if (NULL == (level_field = (int *)malloc(levelMax*sizeof(int))) ) {
    fprintf(stderr, "Failed to alloc level field\n");
    exit_prg(1);
  }
  for (level_index = 0; level_index < levelMax; level_index ++) {
    level_field[level_index] = level_index;
  }

  if (Setup.random_mode) {
    level_index = 0;
    randomize_levels(level_field);
  } else {
    level_index = Setup.start_level;
  }


#if defined(XBLAST_SOUND)
  load_level_sounds();
#endif

  while( num_victories < Setup.max_victories ) {
    do {
      level = level_field[level_index];
	
      if ( (++ level_index) >= levelMax ) {
	level_index = 0;
	if (Setup.random_mode) {
	  randomize_levels(level_field);
	}
      }
    } while (!Setup.use_level[level]);
    
    /* Set up scrambled start positions */
    if ( (Config.num_player <= 4) || (game_mode & GM_Double)){
      init_shuffle_startpos(4);
    } else {
      init_shuffle_startpos(MAX_PLAYER);
    }

    /* load maze from data */
    load_maze(level, &Config);

    /* Introduce level */
    clear_sprite_list();
    level_intro(level, player_stat, &Config);

    /* setup number of active players or teams */
    switch (Config.team_mode) {
    case TM_Single:
      active_player = num_player;
      break;
    case TM_Team:
    case TM_Double:
      active_player = num_player / 2;
      break;
    }

    /* exec special_init_functions */
    (*special_init_function)(player_stat);
    
    /* draw maze in pixmap */
    reset_status_bar(player_stat, get_level_name(level), TRUE);
    draw_maze(num_disp);

    /* mark player positions */
    for (player = 0; player < num_player; player ++) {
      mark_maze_rect (player_stat[player].x + SPRITE_X_OFF, 
		      player_stat[player].y + SPRITE_Y_OFF,
		      SPRITE_WIDTH, SPRITE_HEIGHT);
    }

    level_start(num_disp);

    /* set up welcome messages */
    welcome(num_player, p_string);

    for (disp = 0; disp < num_disp; disp ++) {
      flush_pixmap(disp, num_disp, FALSE);
    }

    game_time = 0;

    init_timer();
    clear_keys(num_player);
    
#if defined(XBLAST_SOUND)
    if (play_music) {
      play_sound(act_score, SOUND_MIDDLE_POSITION);
      score_playing = act_score;
      act_score++;
      if (act_score > SND_SNG6) {
	act_score = SND_SNG1;
      }
    }
#endif

#ifdef DEBUG
    gettimeofday (&ft_start_time, &ft_tz);
    ft_count = 0;
#endif

    do {
#ifdef DEBUG
      ft_count ++;
#endif
      /* increase game time counter, when not in PAUSE mode */
      if (pause_status < 0) {
	if (pause_status == -1) {
	  game_time ++;
	} else {
	  pause_status ++;
	  if (-1 == pause_status) {
	    for (disp=0; disp <num_disp; disp ++) {
	      do_bell(disp);
	    }
	    reset_message();
	  }
	}
      }
      /* kill players if we are near the end of game */
      if (game_time == (GAME_TIME - DEAD_TIME + 1)) {
	for (player = 0; player < num_player; player ++) {
	  if (player_stat[player].lives>0) {
	    player_stat[player].lives = 1;
	    player_stat[player].dying = DEAD_TIME;
	  }
	}
      }

      /* check if we are not in PAUSE mode */
      if (pause_status == -1) {
	/* clear list of all sprites */
	clear_sprite_list();
	
	/* check player status */
	for (p = 0; p <num_player; p ++) {
	  /* to permute player when drawing and stunning */
	  /* quick and dirty but hopefully it solves some problems */
	  player = (p + game_time) % num_player;
	  if (player_stat[player].lives != 0) {
	    
	    switch(player_stat[player].dying) {
	    case 0:
	      /* player is alive and ... */
	      if (player_stat[player].stunned == 0) {
		/* ... walks around */
		do_walk(player_stat + player);
	      } else {
		/* ... and stunned */
		do_stunned(player_stat + player);
	      }
	      break;
	      
	    case 1:
	      /* try to revive player */
	      revive_player(player_stat + player);
	      break;
	      
	    default:
	      /* player is dying */
	      do_die(player_stat + player);
	      break;
	    }
	  }
	}
	
	/* check if players stand on the same position */
	mark_overlapping_players();

	/* Shrink functions */
	do_shrink(game_time);

	/* Scramble blocks */
	do_scramble2(game_time);
	
	/* Game functions */
	(*special_game_function)();
	
	/* check bombs */
	do_bombs();
	
	/* sort spites by y-position */
	sort_sprite_list();
	
	/* check explosions */
	if(do_explosions()) {
	  for (disp = 0; disp < num_disp; disp ++) {
	    (*sound_function)(disp);
	  }
	}
	
	/* check if bombs are ignited by other bombs */
	ignite_bombs();
	
	/* stun players hit by other bombs */
	stun_players(player_stat, num_player);
	
	/* check if and Virus or Junkie infects another player */
	infect_other_players();
	
	/* do junkie countdown  */
	do_junkie();

	for (player = 0; player < num_player; player ++) {
	  if ((player_stat[player].lives !=0  )
	      && (player_stat[player].invincible==0)
	      && check_explosion
	      ( (player_stat[player].x + (BLOCK_WIDTH>>1))/BLOCK_WIDTH,
	       (player_stat[player].y + (BLOCK_HEIGHT>>1))/BLOCK_HEIGHT
	       +1 )) {
	    player_stat[player].dying = DEAD_TIME;
	  }
	}
      }
      /* update status bar */
      update_status_bar (player_stat, game_time, 
			 (Config.team_mode == TM_Double) );

      game_event(num_disp);
      if (game_eval_keys(num_player, player_stat, p_string, 
			 special_key_function, &pause_status)) {
	if (pause_status < 0) {
	  for (disp=0; disp <num_disp; disp ++) {
	    do_bell(disp);
	  }
	  set_message("Continue", TRUE);
	} else {
	  set_message(p_string[pause_status].pause, TRUE);
	}
      }

    } while( (game_time<= GAME_TIME)
	    && ( active_player > 1 || number_of_explosions()!=0 ) 
	    && (active_player > 0) );

#ifdef DEBUG
    gettimeofday (&ft_end_time, &ft_tz);
    ft_time = (double)(ft_end_time.tv_sec - ft_start_time.tv_sec) 
      + (double)(ft_end_time.tv_usec - ft_start_time.tv_usec) / 1E6;
    fprintf(stderr, "Frame rate: %6.3f fps \"%s\"\n",
	    ft_count / ft_time, get_level_name(level));
#endif

    
#if defined(XBLAST_SOUND)
    if (play_music)
      {
	stop_sound(score_playing);
	unload_sound(score_playing);
      }
    play_sound(SND_WON, SOUND_MIDDLE_POSITION);
#endif

    level_end();
    unload_blocks(num_disp);
    
    clear_sprite_list();

#if defined(XBLAST_SOUND)
    play_sound(SND_APPL, SOUND_MIDDLE_POSITION);
#endif

    status_board(last_team, num_victories, player_stat, p_string, &Config);
    unload_blocks(num_disp);
    
  }
  
#ifdef DEBUG
  fprintf(stderr, "Game finished\n [%d/%d]\n",num_victories, Setup.max_victories);
#endif

#if 0
  if (num_victories == Setup.max_victories) {
  }
#endif
#if defined(XBLAST_SOUND)
  stop_sound(STOP_ALL_SOUNDS);
  load_sound(SND_FINALE);
  play_sound(SND_FINALE, SOUND_MIDDLE_POSITION);
#endif
  winning_the_game(last_team, player_stat, p_string, &Setup, &Config);
  
#if defined(XBLAST_SOUND)
  stop_sound_server();
#endif
  printf("Ready.\n");
  return 0;
}

/*
 * end of file main.c
 */
