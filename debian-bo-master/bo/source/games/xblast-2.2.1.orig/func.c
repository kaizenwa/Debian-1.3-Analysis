/*
 * Program XBLAST V2.2.1 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 26th 1997
 * started August 1993
 *
 * File: func.c
 * special functions for levels 
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


#include <stdlib.h>

#define _FUNC_C

#include "const.h"
#include "include.h"
#include "mytypes.h"
#include "score.h"
#include "graphics.h"
#include "maze.h"
#include "bomb.h"
#include "main.h"
#include "func.h"
#include "data.h"
#include "pipe.h"

#if defined(XBLAST_SOUND)
#include "sound.h"
#endif

/* 
 * void functions 
 */
#ifdef __STDC__
void 
special_init_void (BMPlayer *player_stat)
#else
void 
special_init_void (player_stat)
     BMPlayer *player_stat;
#endif
{
}

#ifdef __STDC__
void 
special_game_void (void)
#else
void 
special_game_void ()
#endif
{
}

#ifdef __STDC__
void 
special_extra_void (BMPlayer *ps)
#else
void 
special_extra_void (ps)
     BMPlayer *ps;
#endif
{
}

#ifdef __STDC__
void 
special_key_void (BMPlayer *ps)
#else
void 
special_key_void (ps)
     BMPlayer *ps;
#endif
{
}


/* 
 * Invincible 
 */
#ifdef __STDC__
void 
special_extra_invincible (BMPlayer *ps)
#else
void 
special_extra_invincible (ps)
     BMPlayer *ps;
#endif
{
#if defined(XBLAST_SOUND)
  play_sound(SND_INVINC, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
  ps->illtime = 0;
  ps->illness = ps->health;
  ps->invincible += EXTRA_INVINCIBLE;
}



/* 
 * Kicking 
 */
#ifdef __STDC__
void 
special_extra_kick (BMPlayer *ps)
#else
void 
special_extra_kick (ps)
     BMPlayer *ps;
#endif
{
#if defined(XBLAST_SOUND)
  play_sound(SND_NEWKICK, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
  ps->kick = TRUE;
}



/* 
 * Remote Control 
 */
#ifdef __STDC__
void 
special_extra_RC (BMPlayer *ps)
#else
void 
special_extra_RC(ps)
     BMPlayer *ps;
#endif
{
#if defined(XBLAST_SOUND)
  play_sound(SND_NEWRC, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
  ps->remote_control = TRUE;
}

#ifdef __STDC__
void 
special_key_RC (BMPlayer *ps)
#else
void 
special_key_RC (ps)
     BMPlayer *ps;
#endif
{
  if (ps->remote_control > 0) {
#if defined(XBLAST_SOUND)
    play_sound(SND_SHOOT, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
    ignite_players_bombs(ps);
  }
}


/* 
 * Teleport 
 */
#ifdef __STDC__
void 
special_extra_teleport (BMPlayer *ps)
#else
void 
special_extra_teleport (ps)
     BMPlayer *ps;
#endif
{
#if defined(XBLAST_SOUND)
  play_sound(SND_NEWTELE, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
  if (ps->teleport == 0) {
    ps->teleport = 1;
  }
}

#ifdef __STDC__
void 
special_key_teleport (BMPlayer *ps)
#else
void 
special_key_teleport (ps)
     BMPlayer *ps;
#endif
{
  if (ps->teleport == 1) {
    ps->teleport = TELEPORT_TIME;
  }
}


/* 
 * Extra Ignite All 
 */
#ifdef __STDC__
void 
special_extra_ignite_all (BMPlayer *ps)
#else
void 
special_extra_ignite_all (ps)
     BMPlayer *ps;
#endif
{
#if defined(XBLAST_SOUND)
  play_sound(SND_BUTT, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
  ignite_all_bombs();
}


/* 
 * Special bombs 
 */
#ifdef __STDC__
void 
special_init_special_bombs_30 (BMPlayer *player_stat)
#else
void 
special_init_special_bombs_30 (player_stat)
     BMPlayer *player_stat;
#endif
{
  int player;
  
  for (player = 0; player < 2*MAX_PLAYER; player ++) {
    player_stat[player].special_bombs = 30;
  }
}

#ifdef __STDC__
void 
special_key_special_bomb (BMPlayer *ps)
#else
void 
special_key_special_bomb (ps)
     BMPlayer *ps;
#endif
{
  drop_bomb(ps, BMTspecial);
}

#ifdef __STDC__
void 
special_extra_special_bomb (BMPlayer *ps)
#else
void 
special_extra_special_bomb (ps)
     BMPlayer *ps;
#endif
{
#if defined(XBLAST_SOUND)
  play_sound(SND_SPBOMB, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
  ps->special_bombs += 3;
}

/*
 * Junkie (Garth again)
 */

#ifdef __STDC__
void 
special_extra_junkie (BMPlayer *ps)
#else
void
special_extra_junkie(ps)
     BMPlayer *ps;
#endif
{
#if defined(XBLAST_SOUND)
  play_sound(SND_INJ, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
  ps->junkie = MAX_JUNKIE_TIME;
}

/* 
 * Air Pump (Garth Denley) 
 */
#ifdef __STDC__
void 
special_extra_air (BMPlayer *ps)
#else
void 
special_extra_air(ps)
     BMPlayer *ps;
#endif
{
#if defined(XBLAST_SOUND)
  play_sound(SND_NEWPUMP, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
  ps->air_button = TRUE;
}

#ifdef __STDC__
void 
special_key_air (BMPlayer *ps)
#else
void 
special_key_air (ps)
     BMPlayer *ps;
#endif
{
  if ((ps->air_button > 0) && (ps->lives > 0)) {
#if defined(XBLAST_SOUND)
    play_sound(SND_PUMP, ps->x / (PIXW / MAX_SOUND_POSITION));
#endif
    do_air(ps);
  }
}



/* 
 * Haunting 
 */
#ifdef __STDC__
void 
special_game_haunt (void)
#else
void 
special_game_haunt ()
#endif
{
  haunt_kick(35);
}

#ifdef __STDC__
void 
special_game_haunt_fast (void)
#else
void 
special_game_haunt_fast ()
#endif
{
  haunt_kick(10);
}



/* 
 * Nasty Walls (Garth Denley) 
 */

static int next_nasty;
static int gentle_nasty;
static int range_nasty;

#define NASTY_INC GAME_TIME/12

#ifdef __STDC__
void
init_wall_launch_generic (int gentle,
			  int range)
#else
void
init_wall_launch_generic (gentle,range)
     int gentle,range;
#endif
{
  next_nasty = NASTY_INC;
  gentle_nasty = gentle * GAME_TIME;
  range_nasty = range;
}

#ifdef __STDC__
void
special_init_nasty_walls (BMPlayer *player_stat)
#else
void
special_init_nasty_walls (player_stat)
     BMPlayer *player_stat;
#endif
{
  init_wall_launch_generic(1,1);
}

#ifdef __STDC__
void
special_init_nasty_walls_2 (BMPlayer *player_stat)
#else
void
special_init_nasty_walls_2 (player_stat)
     BMPlayer *player_stat;
#endif
{
  init_wall_launch_generic(4,3);
}

#ifdef __STDC__
void
special_game_nasty_walls (void)
#else
void
special_game_nasty_walls ()
#endif
{
  int x,y;
  int dir = GoStop;

  if (game_time >= next_nasty) {
    if (game_time >= next_nasty + NASTY_INC) {
      next_nasty += NASTY_INC*2;
    }
    if (random_number(gentle_nasty) < game_time) {
      x = random_number(MAZE_W-2) + 1;
      y = random_number(MAZE_H-2) + 1;
      
      switch (random_number(4)) {
      case 0:
	dir=GoUp;
	y=MAZE_H-2;
	break;
      case 1:
	dir=GoDown;
	y=1;
	break;
      case 2:
	dir=GoLeft;
	x=MAZE_W-2;
	break;
      case 3:
	dir=GoRight;
	x=1;
	break;
      }
      new_nasty_bomb(x, y, range_nasty, dir);
    }
  }
}

/*
 * end of file func.c
 */

