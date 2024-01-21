/*
 * Programm XBLAST V2.2 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * December 11th 1996
 * started August 1993
 *
 * File: status.c 
 * status board handling
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

#include <stdio.h>

#define _STATUS_C

#include "include.h"
#include "mytypes.h"
#include "const.h"
#include "graphics.h"
#include "map.h"
#include "score.h"



#define TEXT_ATTR_NORMAL (FF_Medium|FF_White|FF_Boxed)
#define TEXT_ATTR_NUMBER (FF_Large|FF_White|FF_Boxed)

#define MESSAGE_TIME TIME_STEP

static int textAttr;

/* status board text box */
static BMRectangle *status_box;
static BMRectangle status_box_normal = {
  77*STAT_WIDTH/12, 79*STAT_HEIGHT/6,
  87*STAT_WIDTH/12,  2*STAT_HEIGHT/3,
};

/* status board number boxes */
static BMRectangle status_number_box [STAT_W] = {
  {   1*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  {  13*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  {  25*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  {  37*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  {  49*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  {  61*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  {  73*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  {  85*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  {  97*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  { 109*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  { 121*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  { 133*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  { 145*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  { 157*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  { 169*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  { 181*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  { 193*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  { 205*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  { 217*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
  { 229*STAT_WIDTH/12, 79*STAT_HEIGHT/6, 8*STAT_WIDTH/12-1, 4*STAT_HEIGHT/6 },
};

#define MAX_MSG 8

/*
 * local variables
 */
static int num_disp;
static int num_player;

static int game_next, time_led;
static int face_pos[2*MAX_PLAYER];
static int num_pos[2*MAX_PLAYER];
static int face_used[2*MAX_PLAYER];
static int num_used[2*MAX_PLAYER];

static int text_left;
static int text_right;

static char *default_message;
static msg_restore;

/* variables for message queue */
static int num_msg = 0;
static int first_msg;
static char *msg_queue[MAX_MSG];
static int msg_delay[MAX_MSG+1] = {
  -1, 50, 40, 20, 12, 7, 7, 7, 7
};

/*
 * local function
 */
#ifdef __STDC__
static void
set_score_block (int x,
		 int block)
#else
static void
set_score_block (x, block)
     int x;
     int block;
#endif
{
  int disp;
  for (disp = 0; disp < num_disp; disp ++) {
    draw_score_block(disp, x, block);
  }
  mark_maze_tile(x, MAZE_H);
}

/*
 * local function
 */
#ifdef __STDC__
static void
set_score_block_half (int x,
		      int block,
		      int left)
#else
static void
set_score_block_half (x, block, left)
     int x;
     int block;
     int left;
#endif
{
  int disp;
  for (disp = 0; disp < num_disp; disp ++) {
    draw_score_block_half(disp, x, block, left);
  }
  mark_maze_tile(x, MAZE_H);
}

/*
 * local function
 */
#ifdef __STDC__
static void
set_time_led (int x,
	      int val)
#else
static void
set_time_led (x, val)
     int x;
     int val;
#endif
{
  int disp;
  for (disp = 0; disp < num_disp; disp ++) {
    draw_time_led(disp, x, val);
  }
  mark_maze_tile(x/3, MAZE_H+1);
}

/*
 * local function set status text 
 */
#ifdef __STDC__
void 
set_status_text (char *msg)
#else
static void
set_status_text (msg)
     char *msg;
#endif
{
  int disp;

  for (disp=0; disp<num_disp; disp++) {
    draw_textbox(disp, msg, textAttr, status_box);
  }
  mark_maze(text_left, MAZE_H, text_right, MAZE_H);
}

/*
 * local function set_status_number
 */
#ifdef __STDC__
void
set_status_number (int pos,
		   int value)
#else
void
set_status_number (pos, value)
     int pos;
     int value;
#endif
{
  int disp;
  char num_string[2];;

  num_string[0] = '0' + value;
  num_string[1] = '\0';
  for (disp=0; disp<num_disp; disp++) {
    draw_textbox(disp, num_string, TEXT_ATTR_NUMBER, status_number_box + pos);
  }
  mark_maze_tile(pos, MAZE_H);
}


/*
 * public function 
 */
#ifdef __STDC__
void
init_status_bar (XBConfig *config,
		 BMPlayer *ps,
		 char *msg,
		 int flag) 
#else 
void
init_status_bar (config, ps, msg, flag)
     XBConfig *config;
     BMPlayer *ps;
     char *msg;
     int flag;
#endif
{
  int p;

  /* set player and display number */
  num_disp = config->num_disp;
  num_player = config->num_player;

  switch (config->team_mode) {

    /* normal (no teams) mode */
  case TM_Single:
    switch (num_player) {
      /* setup six player if needed */
    case 6:
      face_pos[5] = 18;
      num_pos[5]  = 19;

      /* setup five player if needed */
    case 5:
      face_pos[4] = 16;
      num_pos[4]  = 17;
      
      /* setup player four if needed */
    case 4:
      face_pos[3] = 14;
      num_pos[3]  = 15;
      
      /* setup player three if needed */
    case 3:
      face_pos[2] = 4;
      num_pos[2]  = 5;
      
      /* setup player two if needed */
    case 2:
      face_pos[1] = 2;
      num_pos[1]  = 3;
      
      /* setup player two if needed */
    case 1:
      face_pos[0] = 0;
      num_pos[0]  = 1;
      break;
    }
    break;

    /*  team modes */
  case TM_Team:
    switch(num_player) {
    case 4:
      face_pos[0] = 1;
      num_pos[0]  = 2;
      face_pos[1] = 3;
      num_pos[1]  = 4;

      face_pos[2] = 15;
      num_pos[2]  = 16;
      face_pos[3] = 17;
      num_pos[3]  = 18;
      break;

    case 6:
      face_pos[0] = 0;
      num_pos[0]  = 1;
      face_pos[1] = 2;
      num_pos[1]  = 3;

      face_pos[2] = 4;
      num_pos[2]  = 5;
      face_pos[3] = 14;
      num_pos[3]  = 15;

      face_pos[4] = 16;
      num_pos[4]  = 17;
      face_pos[5] = 18;
      num_pos[5]  = 19;
      break;
    }
    break;

    /* double mode */
  case TM_Double:
    switch (num_player) {
      /* setup player four if needed */
    case 8:
      face_pos[0] = 0;
      num_pos[0]  = 1;
      face_pos[4] = 0;
      num_pos[4]  = 2;

      face_pos[1] = 3;
      num_pos[1]  = 4;
      face_pos[5] = 3;
      num_pos[5]  = 5;

      face_pos[2] = 14;
      num_pos[2]  = 15;
      face_pos[6] = 14;
      num_pos[6]  = 16;

      face_pos[3] = 17;
      num_pos[3]  = 18;
      face_pos[7] = 17;
      num_pos[7]  = 19;
      break;
      
      /* setup player three if needed */
    case 6:
      face_pos[0] = 0;
      num_pos[0]  = 1;
      face_pos[3] = 0;
      num_pos[3]  = 2;

      face_pos[1] = 3;
      num_pos[1]  = 4;
      face_pos[4] = 3;
      num_pos[4]  = 5;

      face_pos[2] = 14;
      num_pos[2]  = 15;
      face_pos[5] = 14;
      num_pos[5]  = 16;
      break;
      
      /* setup player two if needed */
    case 4:
      face_pos[0] = 0;
      num_pos[0]  = 1;
      face_pos[2] = 0;
      num_pos[2]  = 2;

      face_pos[1] = 3;
      num_pos[1]  = 4;
      face_pos[3] = 3;
      num_pos[3]  = 5;
      break;
    }
    break;
  }

  /* clear all */
  for (p=0; p<STAT_W; p++) {
    set_score_block(p, SBVoid);
  }

  /* player boxes */
  for (p=0; p<num_player; p++) {
    set_score_block(face_pos[p], SBPlayer+ps[p].sprite);
    face_used[p] = SBPlayer;
#if 1
      if ( (config->team_mode == TM_Double) && (p < num_player/2) ) {
	set_score_block(num_pos[p], SBTextMid);
      } else {
	set_score_block(num_pos[p], SBTextRight);
      }
#endif
    if (flag) {
#if 0
      set_score_block(num_pos[p], SBLives+ps[p].lives);
#endif
      num_used[p] = ps[p].lives;
    } else {
#if 0
      set_score_block(num_pos[p], SBLives+ps[p].victories);
#endif
      num_used[p] = ps[p].victories;
    }
    set_status_number(num_pos[p], num_used[p]);
  }
  
  /* text box */
  text_left = 6;
  text_right = 13;
  set_score_block(text_left, SBTextLeft);
  set_score_block(text_right, SBTextRight);
  for (p=text_left+1; p<text_right; p++) {
    set_score_block(p, SBTextMid);
  }
  /* setup text box */
  status_box = &status_box_normal;
  textAttr = TEXT_ATTR_NORMAL;

  default_message = msg;
  num_msg = 0;
  set_status_text(msg);

  /* set leds */
  for (p=0; p<(4*MAZE_W); p++) {
    set_time_led(p, 1);
  }
  /* set timers for leds */
  game_next = TIME_STEP;
  time_led = MAZE_W*4;

}



/* 
 * public function 
 */
#ifdef __STDC__
void
reset_status_bar (BMPlayer *ps,
		  char *msg, 
		  int flag)
#else
void
reset_status_bar (ps, msg, flag)
     BMPlayer *ps;
     char *msg;
     int flag;
#endif
{
  int p;

  /* player boxes */
  for (p=0; p<num_player; p++) {
    set_score_block(face_pos[p], SBPlayer+ps[p].sprite);
    face_used[p] = SBPlayer;
    if (flag) {
#if 0
      set_score_block(num_pos[p], SBLives+ps[p].lives);
#endif
      num_used[p] = ps[p].lives;
    } else {
#if 0
      set_score_block(num_pos[p], SBLives+ps[p].victories);
#endif
      num_used[p] = ps[p].victories;
    }
    set_status_number(num_pos[p], num_used[p]);
  }
  
  default_message = msg;
  num_msg = 0;
  set_status_text(msg);

  /* set leds */
  for (p=0; p<(4*MAZE_W); p++) {
    set_time_led(p, 1);
  }
  /* set timers for leds */
  game_next = TIME_STEP;
  time_led = MAZE_W*4;

  /* marl all filed to be redrawed */
  mark_maze(0,MAZE_H, MAZE_W, MAZE_H+1);
}



#ifdef __STDC__
void
update_status_bar (BMPlayer *ps,
		   int game_time,
		   int double_flag)
#else
void
update_status_bar (ps, game_time, double_flag)
     BMPlayer *ps;
     int game_time;
     int double_flag;
#endif
{
  int p;

  for (p=0; p<num_player; p++) {
    /* if in normal mode check if player is ill or dead */
    if (face_used[p] == SBPlayer) {
      if (ps[p].dying) {
	if (double_flag) {
	  set_score_block_half(face_pos[p], SBDead+ps[p].sprite, 
			       ps[p].team == ps[p].id);
	} else {
	  set_score_block(face_pos[p], SBDead+ps[p].sprite);
	}
	face_used[p] = SBDead;
      } else if (ps[p].illness != ps[p].health) {
	if (double_flag) {
	  set_score_block_half(face_pos[p], SBSick+ps[p].sprite, 
			       ps[p].team == ps[p].id);
	} else {
	  set_score_block(face_pos[p], SBSick+ps[p].sprite);
	}
	face_used[p] = SBSick;
      }
    } else if (face_used[p] == SBSick) {
      if (ps[p].illness == ps[p].health) {
	if (double_flag) {
	  set_score_block_half(face_pos[p], SBPlayer+ps[p].sprite, 
			       ps[p].team == ps[p].id);
	} else {
	  set_score_block(face_pos[p], SBPlayer+ps[p].sprite);
	}
	face_used[p] = SBPlayer;
      }
    } else if (face_used[p] == SBDead) {
      if ( (!ps[p].dying) && (ps[p].lives != 0) ) {
	if (double_flag) {
	  set_score_block_half(face_pos[p], SBPlayer+ps[p].sprite, 
			       ps[p].team == ps[p].id);
	} else {
	  set_score_block(face_pos[p], SBPlayer+ps[p].sprite);
	}
	face_used[p] = SBPlayer;
      }
    }
    /* check player lives */
    if (num_used[p] != ps[p].lives) {
#if 0
      set_score_block(num_pos[p], SBLives + ps[p].lives);
#endif
      num_used[p] = ps[p].lives;
      set_status_number(num_pos[p], num_used[p]);
    }
  }
  /* check leds */
  if (game_time > game_next) {
    game_next += TIME_STEP;
    time_led --;
    set_time_led(time_led, 0);
  }
  /* check message */
  /* if any message is in the queue */
  if (num_msg) {
    msg_restore ++;
    /* check if message is outdated */
    if (msg_restore >= msg_delay[num_msg]) {
      /* return to default message if last queued one */
      if (--num_msg) {
	/* set next message */
	first_msg = (first_msg+1) % MAX_MSG;
	set_status_text(msg_queue[first_msg]);
	msg_restore = 0;
      } else {
	/* return to default message if last queued one */
	set_status_text(default_message);
      }
    }
  }
}


/*
 * public function set_message
 */
#ifdef __STDC__
void
set_message (char *msg,
	     int perm)
#else
void
set_message (msg, perm)
	     char *msg;
	     int perm;
#endif
{
  int m;

  if (NULL != msg) {
    if (perm) {
      set_status_text(msg);
      num_msg = 0;
    } else {
      /* if no other message exists set to start of queue */
      if (0 == num_msg) {
	m = first_msg = 0;
	msg_restore = 0;
	set_status_text(msg);
      } else {
	m = (first_msg + num_msg) % MAX_MSG;
      }
      msg_queue[m] = msg;
      num_msg ++;
      /* check for queue overflow */
      if (num_msg > MAX_MSG) {
	/* remove first message */
	num_msg --;
	first_msg = (first_msg+1) % MAX_MSG;
      }
    }
  }
}

/*
 * public function set_message
 */
#ifdef __STDC__
void
reset_message (void)
#else
void
reset_message ()
#endif
{
  if (num_msg > 0) {
    set_status_text(msg_queue[first_msg]);

  } else {
    set_status_text(default_message);
  }
}

/*
 * end of file status.c
 */




