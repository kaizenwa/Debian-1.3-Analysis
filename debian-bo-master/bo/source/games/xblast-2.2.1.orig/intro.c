/*
 * Programm XBLAST V2.2.1 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 26th, 1997
 * started August 1993
 *
 * File: intro.c
 * intros etc for xblast
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

#define _INTRO_C

#include <stdio.h>
#include <stdlib.h>


#include "const.h"
#include "include.h"
#include "mytypes.h"
#include "patchlev.h"

#include "block.h"
#include "sprite.h"
#include "graphics.h"
#include "data.h"
#include "info.h"
#include "maze.h"
#include "map.h"
#include "main.h"
#include "event.h"
#include "status.h"
#include "intro.h"
#include "introdat.h"
#include "pipe.h"

#if defined(XBLAST_SOUND)
#include "sound.h"
#endif

#define SCORE_TIME 16

#define PARTNER(p) ( ((p)/2)*2 + 1 - ((p)%2) )

/*
 * local function draw_intro_player
 */
#ifdef __STDC__
static void
draw_intro_player (int player, 
		   char *name,
		   XBConfig *config)
#else
static void
draw_intro_player (player, name, config)
     int player;
     char *name;
     XBConfig *config;
#endif
{
  int disp;
  static char disp_name[]="X\0";

  disp_name[0]='1'+config->pl_at_disp[player];
  add_player_to_sprite_list(player, intro_player_pos[player].x, 
			    intro_player_pos[player].y, 0, SPM_ALL_DISPLAYS);
  if (config->team_mode == TM_Double) {
    add_player_to_sprite_list(player, intro_player_pos[player].x, 
			      intro_player_pos[player].y - BLOCK_HEIGHT, 
			      0, SPM_ALL_DISPLAYS);
  }
  /* Floor */
  draw_block(control_pos[player][0].x, control_pos[player][0].y, 0);
  draw_block(control_pos[player][0].x, control_pos[player][0].y-1, 0);
  for (disp = 0; disp < config->num_disp; disp ++) {
    draw_textbox(disp, name, FF_White|FF_Boxed|FF_Large, 
		 intro_player_box+player);
    if (config->team_mode != TM_Double) {
      /* alpha control */
      if (player == disp_player[disp].p2) {
	draw_block_at(disp,control_pos[player][2].x,control_pos[player][2].y, 2);
      }
      /* num control */
      if (player == disp_player[disp].p1) {
	draw_block_at(disp,control_pos[player][3].x,control_pos[player][3].y, 1);
      }
    } else {
      /* both controls for double mode */
      if (player == disp_player[disp].p1) {
	draw_block_at(disp,control_pos[player][2].x,control_pos[player][2].y, 2);
	draw_block_at(disp,control_pos[player][3].x,control_pos[player][3].y, 1);
      }
    }
    /* Display */
    draw_block_at(disp, control_pos[player][1].x, control_pos[player][1].y, 3);
    draw_textbox(disp, disp_name, FF_Large | FF_Black, disp_box+player);
  }

  switch(player) {
  case 0:
    mark_maze(5,0,5,1);
    mark_maze(1,0,3,1);
    break;
    
  case 1:
    mark_maze(9,0,9,1);
    mark_maze(11,0,13,1);
    break;
    
  case 2:
    mark_maze(9,5,9,6);
    mark_maze(11,5,13,6);
    break;
    
  case 3:
    mark_maze(5,5,5,6);
    mark_maze(1,5,4,6);
    break;
  }
}

		   

/*
 * public function: do_intro 
 *   intro screen of xblast
 */
#ifdef __STDC__
void 
do_intro (BMPlayer *ps,
	  PlayerStrings *p_string, 
	  XBConfig *config)
#else
void 
do_intro (ps, p_string, config)
     BMPlayer *ps;
     PlayerStrings *p_string;
     XBConfig *config;
#endif
{
  int disp;
  int count;
  int player;
  int i, flags;
  double pfactor;

#if defined (XBLAST_SOUND)
  load_sound(SND_WHIRL);
#endif


  for (disp = 0; disp < config->num_disp; disp ++) {
    init_block(disp, BLIronFloor, BTFree, "Black","Gray75","MidnightBlue");
    init_block(disp, BLControlNum, 1, NULL, NULL, NULL);
    init_block(disp, BLControlAlpha, 2, NULL, NULL, NULL);
    init_block(disp, BLDisplay, 3, NULL, NULL, NULL);

    clear_pixmap(disp);

    if (colorMode[disp]) {
      flags = FF_Large | FF_Black;
    } else {
      flags = FF_Large | FF_White;
    }

    draw_textbox(disp, "On a Workstation", flags, intro_box+0);
    draw_textbox(disp, "not far away", flags, intro_box+1);
    draw_textbox(disp, "Press SPACE to begin", FF_Large | FF_White | FF_Boxed,
		 intro_box+2);
  }
  
#if defined(XBLAST_SOUND)
  play_sound(SND_WHIRL, SOUND_MIDDLE_POSITION);
#endif

  set_fade_max(PIXH+SCOREH);
  fade_in(config->num_disp);
  
  for (disp = 0; disp < config->num_disp; disp ++) {
    flush_pixmap(disp, config->num_disp, FALSE);
  }

  /* wait for space bar */
  init_timer();
  clear_keys(config->num_player);
  do {
    wait2_event(config->num_disp);
  } while(!wait_eval_keys(config->num_player));


  for (disp = 0; disp < config->num_disp; disp ++) {
    clear_pixmap(disp);
  }

#if defined (XBLAST_SOUND)
  stop_sound(STOP_ALL_SOUNDS);
  unload_sound(SND_WHIRL);
  load_sound(SND_INTRO);
  load_sound(SND_EXPL);
#endif

  /* update status bar */
  init_status_bar(config, ps, "Press Space", FALSE);

  fade_in(config->num_disp);

  for (disp = 0; disp < config->num_disp; disp ++) {
    flush_pixmap(disp, config->num_disp, FALSE);
  }

  /* main intro screen */
  init_timer();
  clear_sprite_list();
  for (count = 0; count < INTRO_LENGTH; count ++) {
#if defined(XBLAST_SOUND)
    if (! (count % CHAR_ANIME) ) {
      play_sound(SND_EXPL, (count/CHAR_ANIME)*3+1);
    }
#endif
    if (count < CHAR_ANIME) {
      copy_expl_block( 0, 8, Block_B[count]);
    } else if (count < (CHAR_ANIME*2)) {
      copy_expl_block( 3, 8, Block_L[count-CHAR_ANIME]);
    } else if (count < (CHAR_ANIME*3)) {
      copy_expl_block( 6, 8, Block_A[count-CHAR_ANIME*2]);
    } else if (count < (CHAR_ANIME*4)) {
      copy_expl_block( 9, 8, Block_S[count-CHAR_ANIME*3]);
    } else {
      copy_expl_block(12, 8, Block_T[count-CHAR_ANIME*4]);
    }

    /* draw growing poly */
    pfactor = (double)(count+1)/INTRO_LENGTH;
    for (disp=0; disp < config->num_disp; disp ++) {
      draw_polygon(disp, 
		   (int)(0.5+(7.5-2.5*pfactor)*BLOCK_WIDTH), 
		   (int)(0.5+(10.0-9.0*pfactor)*BLOCK_HEIGHT), 
		   (int)(0.5+pfactor*5.0*BLOCK_WIDTH), 
		   (int)(0.5+pfactor*6.0*BLOCK_HEIGHT), 
		   pointx, SIZE_OF_X, TRUE);
    }
    mark_maze(5,1,9,10);


    if (count==(INTRO_LENGTH-1)) {
      for (disp = 0; disp < config->num_disp; disp ++) {
	for (i=0; i<NUM_XC; i++) {
	  draw_textbox(disp, xc_string[i], xc_flags[i], &(xc_box[i]) );
	}
      }
      mark_maze(4, 9, 10, 11 ); 
      mark_maze(0,0,14,12);
      /* draw players */
      if (config->team_mode != TM_Double) {
	for (player = 0; player < config->num_player; player ++) {
	  draw_intro_player(player, p_string[player].tag, config);
	}
      } else {
	for (player = 0; player < config->num_player/2; player ++) {
	  draw_intro_player(player, p_string[player].tag, config);
	}
      }
      /* draw team tags if needed */
      if (config->team_mode == TM_Team) {
	for (i = 0; i < (config->num_player / 2); i ++) {
	  for (disp = 0; disp < config->num_disp; disp ++) {
	    draw_textbox(disp, NULL, FF_Medium|FF_White|FF_Boxed,
			 team_box2 + i );
	    draw_textbox(disp, team_string[i], FF_Medium|FF_Black|FF_Boxed,
			 team_box + i );
	  }
	}
      }
    }
    for (disp = 0; disp < config->num_disp; disp ++) {
      intro_event(config->num_disp);
    } 
  }

    /* nachher hier die fanfare */
#if defined(XBLAST_SOUND)
  play_sound(SND_INTRO, SOUND_MIDDLE_POSITION);
#endif

  init_timer();
  clear_keys(config->num_player);
  do {
    wait2_event(config->num_disp);
  } while(!wait_eval_keys(config->num_player));

  fade_out(config->num_disp);
  set_fade_max(PIXH);
  
  for (disp = 0; disp < config->num_disp; disp ++) {
    free_block(disp, BTFree);
  }
#if defined(XBLAST_SOUND)
  stop_sound(STOP_ALL_SOUNDS);
  unload_sound(SND_INTRO);
  unload_sound(SND_EXPL);
#endif
}


/* 
 * public function level_start  
 */
#ifdef __STDC__
void 
level_start (int num_disp)
#else
void 
level_start(num_disp)
     int num_disp;
#endif
{
  int disp;

  for (disp =0; disp < num_disp; disp ++) {
    flush_score_board(disp);
  }
  fade_out(num_disp);
}



/*
 * public function level_intro
 * Level Introduction (Garth Denley)
 * modified by Oliver Vogel
 */
#ifdef __STDC__
void 
level_intro (int lvl, 
	     BMPlayer *player_stat,
	     XBConfig *config)
#else
void 
level_intro (lvl, player_stat, config)
     int lvl;
     BMPlayer *player_stat;
     XBConfig *config;
#endif
{
  int player;
  int disp;
  int i, count;

  char **extra_info;
  char **level_info;
  char **player_info;

  reset_status_bar(player_stat, "Press Space", FALSE);
  draw_maze(config->num_disp);
  
  /* draw player positions */
  for (player = 0; player < config->num_player; player ++) {
    player_stat[player].anime = 0;
    add_player_to_sprite_list(player_stat[player].sprite,
			      player_stat[player].x, player_stat[player].y,
			      player_stat[player].anime, SPM_ALL_DISPLAYS);
  }

  sort_sprite_list();

  for (disp = 0; disp < config->num_disp; disp ++) {
    draw_sprites(disp);
  }

  clear_sprite_list();

  /* Display level info */
  get_info(&extra_info, &level_info, &player_info);

  for (disp = 0; disp < config->num_disp; disp ++) {
    /* draw level title */
    draw_textbox(disp, NULL, FF_White | FF_Boxed , title_box+0);
    draw_textbox(disp, get_level_name(lvl), FF_White |FF_Large, title_box+1);
    draw_textbox(disp, get_level_author(lvl), FF_White |FF_Small, title_box+2);

    /* draw player info */
    draw_textbox(disp, NULL, FF_White | FF_Boxed | FF_Transparent,
		 &player_info_frame);
    draw_textbox(disp, "Player Info", FF_Black | FF_Boxed | FF_Medium,
		 player_info_box+0);
    for (i=0; (i<MAX_INFO) && (player_info[i][0] != '\0'); i++) {
      draw_textbox(disp, player_info[i], FF_White | FF_Boxed | FF_Small, 
		   player_info_box+i+1);
    }
    
    /* draw level info */
    draw_textbox(disp, NULL, FF_White | FF_Boxed | FF_Transparent,
		 &level_info_frame);
    draw_textbox(disp, "Level Info", FF_Black | FF_Boxed | FF_Medium,
		 level_info_box+0);
    for (i=0; (i<MAX_INFO) && (level_info[i][0] != '\0'); i++) {
      draw_textbox(disp, level_info[i], FF_White | FF_Boxed | FF_Small, 
		   level_info_box+i+1);
    }
    
    /* draw extra info */
    draw_textbox(disp, NULL, FF_White | FF_Boxed | FF_Transparent,
		 &extra_info_frame);
    draw_textbox(disp, "Extra Info", FF_Black | FF_Boxed | FF_Medium,
		 extra_info_box+0);
    for (i=0; (i<MAX_INFO) && (extra_info[i][0] != '\0'); i++) {
      draw_textbox(disp, extra_info[i], FF_White | FF_Boxed | FF_Small, 
		   extra_info_box+i+1);
    }

    /* draw general tip */
    draw_textbox(disp, get_level_tip(lvl), FF_White | FF_Boxed | FF_Large,
		 &level_tip_box);
  }

  level_start (config->num_disp);

  for (disp = 0; disp < config->num_disp; disp ++) {
    flush_pixmap(disp, config->num_disp, FALSE);
  }

  init_timer();
  clear_keys(config->num_player);
  for (count = 0; count < SCORE_TIME ; count ++) {
    wait2_event(config->num_disp);
  }
  do {
    wait2_event(config->num_disp);
  } while(!wait_eval_keys(config->num_player));

  fade_out(config->num_disp);
}


/* 
 * public function welcome
 */
#ifdef __STDC__
void 
welcome (int num_player, PlayerStrings *st)
#else
void 
welcome (num_player, st)
     int num_player;
     PlayerStrings *st;
#endif
{
  char *tmpl;
  int j;
  int p1,p2;
  char *welcomes[2*MAX_PLAYER];

  for(p1=0; p1<num_player; p1++) {
    welcomes[p1] = st[p1].welcome;
  }
  for(j=0; j<3; j++) {
    for(p1=0; p1<num_player; p1++) {
      p2 = random_number(num_player);
      tmpl = welcomes[p1];
      welcomes[p1] = welcomes[p2];
      welcomes[p2] = tmpl;
    }
  }
  for(p1=0; p1<num_player; p1++) {
    set_message(welcomes[p1], FALSE);
  }
}



/* 
 * public function status_board 
 */
#ifdef __STDC__
void 
status_board (int last_team,
	      int num_victories,
	      BMPlayer *player_stat,
	      PlayerStrings *p_string,
	      XBConfig *config)
#else
void 
status_board (last_team, num_victories, player_stat, p_string, config)
     int last_team;
     int num_victories;
     BMPlayer *player_stat;
     PlayerStrings *p_string;
     XBConfig *config;
#endif
{
  int player,disp , i;
  int count;
  int box_set = 0;
  int flag = 0;

  /* load score board maze */
  load_maze(scoreBoard, config);

  /* draw maze in pixmap */
  reset_status_bar(player_stat, get_level_name(scoreBoard), FALSE);
  draw_maze(config->num_disp);

  /* draw audience */
  if ( (config->num_player != 6) || (config->team_mode != TM_Single) ) {
    for (i = 0; i < (3 * PIXW-128); i += 128) {
      add_player_to_sprite_list (random_number(MAX_PLAYER), i/3, 36, 0, 
				 SPM_ALL_DISPLAYS);
    }
    for (i = 64; i < (3 * PIXW-128); i += 128) {
      add_player_to_sprite_list (random_number(MAX_PLAYER), i/3, -12, 0, 
				 SPM_ALL_DISPLAYS);
    }
  }
  for (i = 0; i < (3 * PIXW-128); i += 128) {
    add_player_to_sprite_list (random_number(MAX_PLAYER), i/3, -60, 0, 
			       SPM_ALL_DISPLAYS);
  }

  /* draw player positions */
  switch (config->team_mode) {
  case TM_Single:
    box_set = config->num_player-2;
    flag = FF_Medium |FF_Boxed |FF_White;
    break;

  case TM_Team:
    box_set = (config->num_player-4)/2+5;
    flag = FF_Small |FF_Boxed |FF_White;
    break;

  case TM_Double:
    box_set = (config->num_player/2)-2;
    flag = FF_Medium |FF_Boxed |FF_White;
    break;
  }

  for (player = 0; player < config->num_player; player ++) {
    /* draw player names */
    if ( (config->team_mode != TM_Double) || (player < config->num_player/2) ) {
      for (disp = 0; disp < config->num_disp; disp ++) {
	draw_textbox(disp, p_string[player].name, flag, 
		     score_player_box[box_set]+player);
      }
    }
    /* set player animes */
    if (player_stat[player].team == last_team) {
      player_stat[player].anime = WINNER_ANIME;
    } else if (player_stat[player].victories == num_victories) {
      player_stat[player].anime = 0;
    } else {
      player_stat[player].anime = 14;
    }
    
    add_player_to_sprite_list(player_stat[player].sprite,
			      player_stat[player].x, player_stat[player].y,
			      player_stat[player].anime, SPM_ALL_DISPLAYS);
    for (i = 0; i < player_stat[player].victories; i ++) {
      add_trophy_to_sprite_list(i, player_stat[player].y+BLOCK_HEIGHT);
    }
  }

  for (disp = 0; disp < config->num_disp; disp ++) {
    draw_sprites(disp);
  }
  clear_sprite_list();

  level_start(config->num_disp);

  for (disp = 0; disp < config->num_disp; disp ++) {
    flush_pixmap(disp, config->num_disp, FALSE);
  }

  init_timer();
  clear_keys(config->num_player);
  for (count = 0; count < SCORE_TIME ; count ++) {
    wait2_event(config->num_disp);
  }
  do {
    wait2_event(config->num_disp);
  } while(!wait_eval_keys(config->num_player));

  fade_out(config->num_disp);
}




/* local function winning the game */
#ifdef __STDC__
void 
winning_the_game (int last_team,
		  BMPlayer *player_stat,
		  PlayerStrings *p_string,
		  XBSettings *setup,
		  XBConfig *config)
#else
void 
winning_the_game (last_team, player_stat, p_string, setup, config)
     int last_team;
     BMPlayer *player_stat;
     PlayerStrings *p_string;
     XBSettings *setup;
     XBConfig *config;
#endif

{
  static char Message[80];
  int disp,player,pos;
  int i;
  BMPlayer *ps, *last_ps;

  last_ps = player_stat + last_team;

  for (disp = 0; disp < config->num_disp; disp++) {
    clear_window(disp);
  }

  /* load score board maze */
  load_maze(winningTheGame, config);

  /* draw maze in pixmap */
  game_time = 0;
  reset_status_bar(player_stat, get_level_name(winningTheGame), FALSE);
  draw_maze(config->num_disp);

  /* draw audience */
  for (i = 0; i < (3 * PIXW-128); i += 128) {
    player = random_number(MAX_PLAYER);
    if (player < config->num_player) {
      if (player_stat[player].team == last_team) {
	add_player_to_sprite_list(player, i/3, 32, 15 , SPM_ALL_DISPLAYS);
      } else {
	add_player_to_sprite_list(player, i/3, 32, 14 , SPM_ALL_DISPLAYS);
      }
    } else {
      add_player_to_sprite_list(player, i/3, 32,  0 , SPM_ALL_DISPLAYS);
    }
  }

  for (i = 64; i < (3 * PIXW-128); i += 128) {
    player = random_number(MAX_PLAYER);
    if (player < config->num_player) {
      if (player_stat[player].team == last_team) {
	add_player_to_sprite_list(player, i/3, -16, 15 , SPM_ALL_DISPLAYS);
      } else {
	add_player_to_sprite_list(player, i/3, -16, 14 , SPM_ALL_DISPLAYS);
      }
    } else {
      add_player_to_sprite_list(player, i/3, -16,  0 , SPM_ALL_DISPLAYS);
    }
  }
  for (i = 0; i < (3 * PIXW-128); i += 128) {
    player = random_number(MAX_PLAYER);
    if (player < config->num_player) {
      if (player_stat[player].team == last_team) {
	add_player_to_sprite_list(player, i/3, -64, 15 , SPM_ALL_DISPLAYS);
      } else {
	add_player_to_sprite_list(player, i/3, -64, 14 , SPM_ALL_DISPLAYS);
      }
    } else {
      add_player_to_sprite_list(player, i/3, -64, 0 , SPM_ALL_DISPLAYS);
    }
  }

  /* draw player positions */
  pos = 0;
  for (player = 0; player < config->num_player; player ++) {
    if ( (config->team_mode == TM_Double) && (player == config->num_player/2) ) {
      pos ++;
    }
    if (player_stat[player].victories != last_ps->victories) {
      player_stat[player].anime = player_stat[player].victories ? 14 : 13;
      add_player_to_sprite_list(player_stat[player].sprite,
				player_stat[pos].x, player_stat[pos].y,
				player_stat[player].anime, 
				SPM_ALL_DISPLAYS);
      pos ++;
    }
  }
  
  switch (config->team_mode) {
  case TM_Single:
    for (disp = 0; disp < config->num_disp; disp ++) {
      draw_winner(disp, last_ps->sprite,
		  (PIXW-BIG_WIDTH)/2, (PIXH-BIG_HEIGHT)/2-24 ); 
      draw_textbox(disp, p_string[last_team].name,
		   FF_White | FF_Medium | FF_Boxed, &winner_box);
    }
    break;

  case TM_Team:
  case TM_Double:
      for (disp = 0; disp < config->num_disp; disp ++) {
	int xpos = (PIXW-BIG_WIDTH)/2 - 3*BLOCK_WIDTH/2;
	for (i=0, ps = player_stat; i<config->num_player; i++, ps++) {
	  if (ps->team == last_team) {
	    draw_winner(disp, ps->sprite, xpos, (PIXH-BIG_HEIGHT)/2-24 ); 
	    xpos += 3*BLOCK_WIDTH;
	  }
	}
	draw_textbox(disp, p_string[last_team].name,
		     FF_White | FF_Medium | FF_Boxed, &two_winner_box);
    }
    break;

  }
  
  for (disp = 0; disp < config->num_disp; disp ++) {
    draw_sprites(disp);
  }
  clear_sprite_list();

  if (setup->print_stat) {
    printf("GameStat {%s} {",p_string[last_team].name);
    for (player =0; player < config->num_player; player ++) {
      printf(" {%s} ",p_string[player].name);
    }
    printf("}\n");
  }

  circle_in(config->num_disp);

  for (disp = 0; disp < config->num_disp; disp ++) {
    flush_pixmap(disp, config->num_disp, FALSE);
  }
  
  sprintf(Message, p_string[last_team].wingame,  p_string[last_team].name);
  wait2_two_text(config->num_disp, config->num_player, player_stat, Message,
		 "Press Space");
  
  fade_out(config->num_disp);
}



/*
 * end of file intro.c
 */
