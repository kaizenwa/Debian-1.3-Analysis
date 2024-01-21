/*
 * Program XBLAST V2.1.8 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * September 24th 1996
 * started August 1993
 *
 * File: event.c
 * event handlig routines
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

#define _EVENT_C

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>

#include "const.h"
#include "include.h"
#include "mytypes.h"
#include "graphics.h"
#include "map.h"
#include "status.h"
#include "main.h"
#include "func.h"
#include "event.h"
#include "pipe.h"
#include "setup.h"
#include "bomb.h"

#if defined(XBLAST_SOUND)
#include "sound.h"
#endif

/*
 * local variables for delay etc 
 */
static int last_delay = 0;
static int skip_frame = 0;
static max_skip_frame = 0;
static int SkipTime =0;
static struct timeval tv1,tv2,tvgame;
static struct timezone tzp;

/*
 * local variables for player_key etc
 */
static PlayerAction player_action[2*MAX_PLAYER];
DispPlayer disp_player[MAX_DISPLAY];

/*
 * local function pointers for client-server io
 */

#ifdef __STDC__
static void (*keys_to_server)(void);
static void (*keys_to_clients)(void);
static int  (*wait_function)(int _t);
void (*quit_function)(void);
#else
static void (*keys_to_server)();
static void (*keys_to_clients)();
static int  (*wait_function)();
void (*quit_function)();
#endif



/*
 * public function init_delta_time
 */
#ifdef __STDC__
void 
init_timer (void)
#else
void 
init_timer ()
#endif
{
  /* init timer constants */
  last_delay     = 0;
  skip_frame     = 0;
  max_skip_frame = 1;
  SkipTime       = -FrameTime/2;
     
  /* Now including a bugfix for Solaris */
  /* send in by ianj@melko.co.uk */
  do {
    gettimeofday(&tv1,&tzp);
  } while( tv1.tv_usec >999999);
  
  tvgame = tv1;
}



/*
 * public function get_delta_time
 */
#ifdef __STDC__
static int 
get_timer (void)
#else
static int 
get_timer ()
#endif
{
  int rueck;

  do {
    gettimeofday(&tv2,&tzp);
  } while( tv2.tv_usec > 999999);
  rueck = ( 1000000*(tv2.tv_sec-tv1.tv_sec)+(tv2.tv_usec-tv1.tv_usec));
  tv1 = tv2;

  return rueck;
}



/*
 * local  function wait_micsec
 */
#ifdef __STDC__
static int 
wait_usec (int i)
#else
static int 
wait_usec (i)
     int i;
#endif
{
  static struct timeval tv;

  if (i>0) {
    tv.tv_sec = 0;
    tv.tv_usec = i;
    select(0,NULL,NULL,NULL,&tv);
  }

  return i;
}

/*
 * local function wait_not
 */
#ifdef __STDC__
static int 
wait_not (int i)
#else
static int 
wait_not (i)
     int i;
#endif
{
  return i;
}

/*
 * local function flush_all_displays
 */
#ifdef __STDC__
static void
flush_all_displays (int num_disp)
#else
static void
flush_all_displays (num_disp)
     int num_disp;
#endif
{
  int disp;
  
  update_maze(num_disp);
  for (disp=0; disp<num_disp; disp ++) {
    draw_sprites(disp);
    flush_pixmap(disp, num_disp, 1);
  }
}



/*
 * local function flush_all_displays
 */
#ifdef __STDC__
static void
intro_flush_all (int num_disp)
#else
static void
intro_flush_all (num_disp)
     int num_disp;
#endif
{
  int disp;
  
  for (disp=0; disp<num_disp; disp ++) {
    flush_blocks(disp, (disp == (num_disp-1)));
    draw_sprites(disp);
    update_expl(disp);
    flush_pixmap(disp, num_disp, 1);
  }
}




/* 
 * local function game_expose 
 */
#ifdef __STDC__
static void 
game_expose (int disp, 
	     XExposeEvent *xev) 
#else
static void
game_expose (disp, xev)
     int disp;
     XExposeEvent *xev;
#endif
{
  flush_pixmap(disp, disp, 0);
}



/*
 * local quit functions
 */
#ifdef __STDC__
static void 
quit_simple (void)
#else
static void 
quit_simple ()
#endif
{
  fprintf(stderr, "Quit!\n");
#if defined(XBLAST_SOUND)
  stop_sound_server();
#endif
  exit_prg(-1);
}

#ifdef __STDC__
static void 
quit_none (void)
#else
static void 
quit_none ()
#endif
{
}



#define NUM_KEYS_SINGLE  (9+2)
#define NUM_KEYS_DOUBLE (18+4)

/*
 * public function set_game_keys
 */
#ifdef __STDC__
void
set_game_keys (int disp)
#else
void
set_game_keys (disp)
     int disp;
#endif
{
  int offset;
  static KeyPressDefine keydef[NUM_KEYS_DOUBLE];
  
#ifdef DEBUG
  fprintf(stderr,"set_game_keys %d\n",disp);
#endif

  if (1 == disp_player[disp].num) {
    offset = keys_from_database (disp, SINGLE_PLAYER, keydef,
				 &(player_action[disp_player[disp].p1]) );
  } else {
    offset = keys_from_database (disp, RIGHT_PLAYER, keydef,
				 &(player_action[disp_player[disp].p1]) );
#ifdef DEBUG
  fprintf(stderr,"offset %d\n",offset);
#endif
    offset += keys_from_database (disp, LEFT_PLAYER, keydef + offset,
				  &(player_action[disp_player[disp].p2]) );
  }
  /* create table */
#ifdef DEBUG
  fprintf(stderr,"offset %d\n",offset);
#endif

  link_keysyms (disp, offset, keydef);
}
	       

/*
 * public function set_communication
 */
#ifdef __STDC__
void
set_event_communication (int type, 
		   int p1, 
		   int p2)
#else
void
set_event_communication (type, p1, p2)
     int type;
     int p1, p2;
#endif
{
  switch (type) {
  case CM_None:
    keys_to_server  = no_keys_to_server;
    keys_to_clients = no_keys_to_clients;
    wait_function   = wait_usec;
    quit_function   = quit_simple;
    break;

  case CM_Parent:
    keys_to_server   = get_keys_from_children;
    keys_to_clients  = send_keys_to_children;
    wait_function    = wait_usec;
    quit_function    = quit_simple;
    parent_link_keys(-1, player_action, p1, p2);
    break;

  case CM_Child:
    keys_to_server   = send_keys_to_parent;
    keys_to_clients  = get_keys_from_parent;
    wait_function    = wait_not;
    quit_function    = quit_none;
    child_link_keys(player_action, p1, p2);
    break;
  }
}

/* 
 * local function get_all_keys
 */ 
#ifdef __STDC__
static void 
get_all_keys (int num_disp) 
#else
static void 
get_all_keys (num_disp)
     int num_disp;
#endif
{
  int disp;

  /* get your own events */
  for (disp = 0; disp < num_disp; disp ++) {
    check_event(disp, game_expose);
  }
  (*keys_to_server)();
  (*keys_to_clients)();
}

/*
 * public function game_event 
 */
#ifdef __STDC__
void
game_event (int num_disp) 
#else
void
game_event (num_disp)
     int num_disp;
#endif
{
  /* check if frame must be skipped */
  last_delay=(*wait_function)(FrameTime - get_timer() + last_delay);

  /* don't skip frame */
  set_redraw_rectangles();
  /* redraw all display */
  flush_all_displays(num_disp);
  /* check all events for all players */
  get_all_keys(num_disp);
  clear_redraw_map();

  if ( (last_delay < SkipTime) ) {
    last_delay = SkipTime;
  }
}  


/*
 * public function intro_event 
 */
#ifdef __STDC__
void
intro_event (int num_disp)
#else
void
intro_event (num_disp)
     int num_disp;
#endif
{
  /* delay to get correct frame rate */
  last_delay=(*wait_function)(FrameTime - get_timer() + last_delay);

  set_redraw_rectangles();
  /* redraw all display */
  intro_flush_all(num_disp);
  /* check all events for all players */
  get_all_keys(num_disp);
  clear_redraw_map();
}  


/*
 * public function game_event 
 */

#ifdef __STDC__
void
wait2_event (int num_disp)
#else
void
wait2_event (num_disp)
     int num_disp;
#endif
{
  /* delay to get correct frame rate */
  last_delay=(*wait_function)(FrameTime - get_timer() + last_delay);

  clear_redraw_map();
  flush_all_displays(num_disp);
  get_all_keys(num_disp);
}  


/*
 * public function set_players_for_display 
 */
#ifdef __STDC__
void
set_players_for_display (int disp, 
			 int player1, 
			 int player2)
#else
void
set_players_for_display (disp, player1, player2)
     int disp;
     int player1;
     int player2;
#endif
{
#ifdef DEBUG
  fprintf(stderr, "set player (%d,%d) for display %d\n",player1,player2,disp); 
#endif
  disp_player[disp].num = (player1 == player2) ? 1 : 2;
  disp_player[disp].p1 = player1;
  disp_player[disp].p2 = player2;
}



/*
 * public function clear_keys
 */
#ifdef __STDC__
void
clear_keys (int num_player) 
#else
void
clear_keys (num_player) 
     int num_player;
#endif
{
  int player;

  for (player=0; player<num_player; player++) {
    player_action[player].player  = player;
    player_action[player].dir     = GoDefault;
    player_action[player].bomb    = FALSE;
    player_action[player].special = FALSE;
    player_action[player].pause   = FALSE;
    player_action[player].abort   = ABORT_NONE;
  }
}



/*
 * public function game_eval_keys
 */
#ifdef __STDC__
int
game_eval_keys (int num_player, 
		BMPlayer *player_stat,
		PlayerStrings *p_string,
		PFV key_func,
		int *pause_mode) 
#else
int
game_eval_keys (num_player, player_stat, p_string, key_func, pause_mode) 
     int num_player;
     BMPlayer *player_stat;
     PlayerStrings *p_string;
     PFV key_func;
     int *pause_mode;
#endif
{
  int player;
  BMPlayer *ps;
  PlayerAction *pa;
  int result = FALSE;
  int do_abort;

  for (player=0; player<num_player; player++) {
    ps = player_stat + player;
    pa = player_action + player;

    /* toggle pause mode */
    if (pa->pause) {
      if (*pause_mode == player) {
	/* pause mode off */
	*pause_mode = -PAUSE_DELAY;
      } else if (*pause_mode < 0) {
	/* pause_mode on */
	*pause_mode = player;
      }
      result = TRUE;
    }

    /* drop bomb if needed */
    if (pa->bomb) {
      drop_bomb(ps, BMTdefault);
    }
    /* execute special key function */
    if (pa->special) {
      (*key_func)(ps);
    }
    /* try to abort game if needed */
    if (pa->abort == ABORT_CANCEL) {
      if ((ps->lives >0) && (ps->abort)) {
	set_message (p_string[player].abortcancel, FALSE);
      }
      ps->abort=FALSE;
    }
    if (pa->abort == ABORT_TRUE) {
      if (ps->lives>0) {
	ps->abort = TRUE;
	set_message (p_string[player].abort, FALSE);
	do_abort = TRUE;
	for (player=0;player<num_player;player++) {
	  if (player_stat[player].lives >0) {
	    do_abort &= player_stat[player].abort;
	  }
	}
	if (do_abort) {
	  /* Only if everyone agrees */

	  set_message("* * Level Aborted * *", TRUE);
	  for (player=0;player<num_player;player++) {
	    if (player_stat[player].lives > 0) {
	      player_stat[player].lives = 1;
	      player_stat[player].dying = DEAD_TIME;
	    }
	  }
	}
      }
    }

    /* get direction */
    if (pa->dir != GoDefault) {
      /* reverse direction  if player is ill */
      if (ps->illness == IllReverse) {
	switch (pa->dir) {
	case GoUp:
	  pa->dir = GoDown;
	  break;
	case GoDown:
	  pa->dir = GoUp;
	  break;
	case GoLeft:
	  pa->dir = GoRight;
	  break;
	case GoRight:
	  pa->dir = GoLeft;
	  break;
	}
      }
      /* set new course */
      ps->d_soll = pa->dir;
      /* look if player reverts course */
      switch (ps->d_ist) {
      case GoUp:
	if (GoDown == ps->d_soll) {
	  ps->d_ist = GoDown;
	}
	break;
      case GoDown:
	if (GoUp == ps->d_soll) {
	  ps->d_ist = GoUp;
	}
	break;
      case GoRight:
	if (GoLeft == ps->d_soll) {
	  ps->d_ist = GoLeft;
	}
	break;
      case GoLeft:
	if (GoRight == ps->d_soll) {
	  ps->d_ist = GoRight;
	}
	break;
      default:
	break;
      }
    }
    
    /* clear action struct */
    pa->dir     = GoDefault;
    pa->bomb    = FALSE;
    pa->special = FALSE;
    pa->pause   = FALSE;
    pa->abort   = ABORT_NONE;
  }

  return result;
}


#ifdef __STDC__
int
wait_eval_keys (int num_player)
#else
int
wait_eval_keys (num_player)
     int num_player;
#endif
{
  int player;
  PlayerAction *pa;
  int result = 0;

  for (player=0; player<num_player; player ++) {
    pa = player_action + player;

    /* bit 0 shifted by player number for resulr mask */
    result |= ((pa->bomb) << player);

    /* clear action struct */
    pa->dir     = GoDefault;
    pa->bomb    = FALSE;
    pa->special = FALSE;
    pa->pause   = FALSE;
  }

  return result;
}



/* 
 * public function fade_in 
 */
#ifdef __STDC__
void 
fade_in (int num_disp)
#else
void 
fade_in (num_disp)
     int num_disp;
#endif
{
  int disp;
  int count;
  count = FADE_STEP;

  init_timer();
  last_delay = 0;

  while (count > 1) {
    init_fade(count);
    last_delay=(*wait_function)(FrameTime - get_timer() + last_delay);
    for (disp=0; disp < num_disp; disp++) {
      fade_in_window(disp);
    }
    get_all_keys(num_disp);
    count /= 2;
  }
}


/* 
 * public function fade_in 
 */
#ifdef __STDC__
void 
fade_out (int num_disp)
#else
void 
fade_out (num_disp)
     int num_disp;
#endif
{
  int disp;
  int count;
  count = FADE_STEP;

  init_timer();
  last_delay = 0;

  while (count > 1) {
    init_fade(count);
    last_delay=(*wait_function)(FrameTime - get_timer() + last_delay);
    for (disp=0; disp < num_disp; disp++) {
      fade_out_window(disp);
    }
    get_all_keys(num_disp);
    count /= 2;
  }
}



/*
 * public function circle_in
 */
#ifdef __STDC__
void
circle_in (int num_disp)
#else
void
circle_in (num_disp)
     int num_disp;
#endif
{
  int disp, radius;

  init_timer();
  
  for (radius = BLOCK_WIDTH/4; radius < 9*BLOCK_WIDTH; radius+=BLOCK_WIDTH/4) {
    last_delay=(*wait_function)(FrameTime - get_timer() + last_delay);
    for (disp=0; disp<num_disp; disp ++) {
      draw_circle_from_pixmap(disp, PIXW/2, PIXH/2, radius);
    }
    get_all_keys(num_disp);
  }
}



/* 
 * public function wait_two_text 
 */
#ifdef __STDC__
void 
wait2_two_text (int num_disp,
		int num_player,
		BMPlayer *player_stat,
		char *text1, 
		char *text2)
#else
void 
wait2_two_text (num_disp, num_player, player_stat, text1, text2)
     int num_disp;
     int num_player;
     BMPlayer *player_stat;
     char *text1, *text2;
#endif
{
  int count;

  set_message (text1,TRUE);
  
  init_timer();
  count = 0;
  do {
    count ++;
    if (count & 0x1f) {
      mark_maze(4, MAZE_H, 10, MAZE_H);
      if (count & 0x20) {
	set_message(text2,TRUE);
      } else {
	set_message(text1,TRUE);
      }
    }
    /* update status bar */
    update_status_bar (player_stat, game_time, FALSE);
    game_event(num_disp);
  } while (!wait_eval_keys(num_player));
  reset_message();
}


/*
 * end of file event.c
 */

