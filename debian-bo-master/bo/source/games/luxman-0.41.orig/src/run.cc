/*
   run.cc

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

#include <sys/time.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <rawkey/rawkey.h>
#include <stdlib.h>
#include <time.h>
#include <gtools/gtools.h>
#include <signal.h>
#include "run.h"
#include "ghost.h"
#include "lux.h"
#include "globals.h"
#include "runlist.h"
#include "label.h"
#include "fruit.h"
#include "banner.h"
#include "error.h"
#include "sound.h"
#include "isleep.h"

#define STATE_NORMAL	0
#define STATE_ENERG		1
#define STATE_FLASH		2

static int check_collide( LuxMan *man, Ghost *ghost, Bitmap *tile )
{
  if ( gb_test_mode == ARGV_TRUE )
	return 0;

  switch( ghost->get_state() )
	{
	case GHOST_NORMAL:
	  if ( (man->X() == ghost->X()) && (abs(man->Y()-ghost->Y()) < tile->h/2) )
		return 1;

	  if ( (man->Y() == ghost->Y()) && (abs(man->X()-ghost->X()) < tile->w/2) )
		return 1;

	  return 0;

	case GHOST_ENERGIZED:
	case GHOST_FLASHING:
	  
	  if ( abs( man->Y() - ghost->Y() ) < tile->h/2 &&
		  abs( man->X() - ghost->X() ) < tile->w/2 )
		return 1;
	  else
		return 0;

	case GHOST_EYES:
	  return 0;
	}
  
  return 0;
}

static int check_quit( int q, int c, int y, int n, Font *font )
{
  Bitmap *bmp;
  int sc = -1, r = 0;
  
  /* Check if `q' pressed (prompt for quit) */
  if ( is_key_pressed( q ) ||
	  (is_key_pressed( LEFT_CTRL ) && is_key_pressed( c )) )
	{
	  snd_sleep();
	  
	  bmp = display_banner( "Really quit?", "y/n", font );
	  
	  do {
		scan_keyboard();

		if ( is_key_pressed( y ) )
		  {
			sc = y;
			r = 1;
		  }
		else if ( is_key_pressed( n ) )
		  {
			sc = n;
			r = 0;
		  }

	  } while( sc == -1 );

	  while( is_key_pressed( sc ) )
		scan_keyboard();
		  
	  put_bitmap( 320/2 - bmp->w/2, 200/2 - bmp->h/2, bmp );
	  delete bmp;
	  gr_update();

	  snd_wakeup();
	  
	  return r;
	}
  else
	return 0;
}

static int draw_status( Bitmap *lux, int lives )
{
  int i;
  static int last_lives=0;

  if ( last_lives )
	gr_fillbox( 0, 198-lux->h, last_lives*(lux->w+4)-1, 199, BLACK );

  /* Draw max 5 men -- interlocked with draw_level_display in map.cc */
  if ( lives > 5 )
	lives = 5;

  last_lives = lives;

  for( i=0; i<lives; ++i )
	put_bitmap( (lux->w+4) * i, 198-lux->h, lux );
  
  return 0;
}

/* Scores for eating ghosts */
static char *scores[] = { "200", "400", "800", "1600" };

int run_screen( RunState *runinfo )
{
  int run_state = STATE_NORMAL;
  Ghost *ghost[4];
  LuxMan *man;
  RunList *runlist;
  Label *label;
  Fruit *fruit;
  int caught = 0;
  int count=0, next_count=0;
  int num_eaten = 0;
  int i;
  int sc_q=0, sc_n=0, sc_p, sc_y, sc_c;
  timeval tv1, tv2;
  long frame_time, tdiff, uwait;
  Font *font;
  char fruit_val_string[100];
  Bitmap *lux_bitmap;
  time_t t1;
  int snd_loop;		/* Currently looping sound */
  int do_eyes;
  int pending_ghost_eat;
  Bitmap *bmp;
  
  gb_skill_level = runinfo->skill;
  gb_ghost_search_depth = runinfo->search_depth;
  gb_ghost_regen_wait = runinfo->regen_wait;
  
  sc_p = scancode_trans( 'p' );
  sc_q = scancode_trans( 'q' );
  sc_n = scancode_trans( 'n' );
  sc_y = scancode_trans( 'y' );
  sc_c = scancode_trans( 'c' );
  
  runlist = new RunList();

  lux_bitmap = new Bitmap( "lux_l1.map" );
  
  font = new Font( "small.font" );
  
  /* Init ghosts */
  ghost[0] = new Ghost( MAZE_GHOST1, runinfo->maze, runinfo->tile->w,
					   runinfo->tile->h, 0 );
  ghost[1] = new Ghost( MAZE_GHOST2, runinfo->maze, runinfo->tile->w,
					   runinfo->tile->h, 1 );
  ghost[2] = new Ghost( MAZE_GHOST3, runinfo->maze, runinfo->tile->w,
					   runinfo->tile->h, 2 );
  ghost[3] = new Ghost( MAZE_GHOST4, runinfo->maze, runinfo->tile->w,
					   runinfo->tile->h, 3 );

  /* Init LuxMan */
  man = new LuxMan( MAZE_LUX, runinfo->maze, runinfo->tile->w,
				   runinfo->tile->h, runinfo->lux_body,
				   runinfo->lux_glasses );

  runinfo->dotmap->draw();

  draw_status( lux_bitmap, runinfo->lives );
  
  runinfo->score->add( 0 );

  while( is_key_pressed( CURSOR_RIGHT ) ||
		is_key_pressed( CURSOR_LEFT ) ||
		is_key_pressed( CURSOR_UP ) ||
		is_key_pressed( CURSOR_DOWN ) )
	scan_keyboard();

  if ( runinfo->play_intro )
	{
	  /* Watch for getting stuck here */
	  alarm( 20 );
	  
	  snd_clear_all( 0 );
	  snd_play_sample( SND_BEGIN, CHAN1, 1 );

	  alarm( 0 );
	}

  for( i=0; i<4; ++i )
	ghost[i]->do_draw();
  
  man->do_draw();
  
  bmp = display_banner( "Player One", "Ready!", font );

  if ( runinfo->play_intro )
	{
	  /* Make sure we don't get stuck here */
	  alarm( 20 );
	  
	  while( snd_query_complete( CHAN1 ) != 1 )
		scan_keyboard();	/* Allow VT switch */
	  
	  snd_sync();
	  runinfo->play_intro = 0;

	  if ( gb_dont_use_sound == ARGV_TRUE )
		isleep( 2 );
	  
	  alarm( 0 );
	}
  else
	isleep( 2 );
  
  put_bitmap( 320/2 - bmp->w/2, 200/2 - bmp->h/2, bmp );
  delete bmp;
  gr_update();
  
  man->erase();

  for( i=3; i>=0; --i )
	ghost[i]->erase();
  
  fruit = new Fruit( runinfo->fruitname, runinfo->maze, runinfo->tile,
					runinfo->max_fruit, runinfo->fruit_off,
					runinfo->fruit_on );
  sprintf( fruit_val_string, "%d", runinfo->fruitval );

  /* Calc microseconds per frame */
  frame_time = 1000000 / gb_frames_per_second;

  uwait = frame_time - gb_usleep_time;

  t1 = time( NULL );

  alarm( 20 );
  
  snd_loop = SND_SIREN;
  snd_start_loop( snd_loop, CHAN1 );
  pending_ghost_eat = 0;
  
  do
	{
	  gettimeofday( &tv1, NULL );

	  /* Use alarm to catch program getting `stuck' */
	  alarm( 20 );
	  scan_keyboard();

	  if ( runinfo->score->get() >= runinfo->next_free )
		{
		  runinfo->next_free += 10000;
		  draw_status( lux_bitmap, ++runinfo->lives );
		  runinfo->score->add( 0 );		/* May have been overwritten */
		}
	  
	  /* Do drawing */
	  if ( runinfo->dotmap->update( man->TX(), man->TY() ) == 1 )
		{
		  runinfo->score->add( 10 );
		  snd_play_sample( SND_DOT, CHAN2, 0 );
		}
	  
	  runinfo->bigdotmap->draw();
	  fruit->draw();
	  runlist->draw();
	  man->move();

	  for( i=0; i<4; ++i )
		ghost[i]->move( man->TX(), man->TY() );

	  /* Update screen */
	  gr_update();
	  ++(runinfo->frames);

	  if ( pending_ghost_eat )
		{
		  if ( gb_dont_use_sound == ARGV_TRUE )
			usleep( 250000 );
		  else
			{
			  snd_clear_all( 0 );
			  snd_play_sample( SND_EATGHOST, CHAN1, 1 );
			  while( snd_query_complete( CHAN1 ) != 1 );

			  snd_loop = SND_EYES;
			  snd_start_loop( snd_loop, CHAN1 );
			}
		  
		  pending_ghost_eat = 0;
		}

	  /* Watch for `q' being pressed */
	  if ( check_quit( sc_q, sc_c, sc_y, sc_n, font ) == 1 )
		{
		  caught = 1;
		  runinfo->lives = 0;
		  continue;
		}
	  
	  /* Erase everything */
	  for( i=3; i >= 0; --i )
		ghost[i]->erase();

	  man->erase();
	  runlist->erase();
	  fruit->erase();
	  runinfo->bigdotmap->erase();

	  if( fruit->update( man->TX(), man->TY() ) )
		{
		  snd_play_sample( SND_FRUIT, CHAN2, 1 );
		  label = new Label( man->TX(), man->TY(), font,
							fruit_val_string, 50, runinfo->tile );
		  runlist->add( label );
		  runinfo->score->add( runinfo->fruitval );
		}
	  
	  /* See if LuxMan got a power pill */
	  if ( runinfo->bigdotmap->check_collide( man->TX(), man->TY() ) )
		{
		  /* Yes */
		  for( i=0; i<4; ++i )
			{
			  if ( ghost[i]->get_state() != GHOST_EYES )
				ghost[i]->set_state( GHOST_ENERGIZED );
			}

		  snd_loop = SND_ENERGIZED;
		  snd_start_loop( snd_loop, CHAN1 );
		  
		  run_state = STATE_ENERG;
		  next_count = runinfo->ghost_etime;
		  count = 0;
		  num_eaten = 0;

		  runinfo->score->add( 50 );
		}
	  
	  /* Check collisions with ghosts */
	  for( i=0; i<4; ++i )
		{
		  if( check_collide( man, ghost[i], runinfo->tile ) == 1 )
			{
			  if ( ghost[i]->get_state() == GHOST_NORMAL )
				{
				  caught = 1;
				  break;
				}
			  else if ( ghost[i]->get_state() == GHOST_ENERGIZED ||
					   ghost[i]->get_state() == GHOST_FLASHING )
				{
				  ghost[i]->set_state( GHOST_EYES );
				  label = new Label( man->TX(), man->TY(), font,
									scores[num_eaten], 50, runinfo->tile );
				  
				  runinfo->score->add( atoi( scores[num_eaten++] ) );
				  
				  runlist->add( label );

				  pending_ghost_eat = 1;
				}
			}
		}

	  if ( caught )
		break;
	  
	  scan_keyboard();
		
	  /* Check state changes */
	  switch( run_state )
		{
		case STATE_NORMAL:	/* Nothing (again! so there!) ;) */
		  break;

		case STATE_ENERG:
		  do_eyes = 0;
		  
		  for( i=0; i<4; ++i )
			{
			  if( ghost[i]->get_state() == GHOST_EYES )
				{
				  do_eyes = 1;
				  break;
				}
			}

		  if ( !do_eyes )
			{
			  if ( snd_loop != SND_ENERGIZED )
				{
				  snd_loop = SND_ENERGIZED;
				  snd_start_loop( snd_loop, CHAN1 );
				}
			}
		  
		  if ( ++count >= next_count )	/* Time to start flashing? */
			{
			  /* Yes */
			  run_state = STATE_FLASH;
			  next_count = runinfo->ghost_ftime;
			  count = 0;

			  for( i=0; i<4; ++i )
				{
				  if ( ghost[i]->get_state() == GHOST_ENERGIZED )
					ghost[i]->set_state( GHOST_FLASHING );
				}
			}
		  break;

		case STATE_FLASH:
		  do_eyes = 0;
		  
		  for( i=0; i<4; ++i )
			{
			  if( ghost[i]->get_state() == GHOST_EYES )
				{
				  do_eyes = 1;
				  break;
				}
			}

		  if ( !do_eyes )
			{
			  if ( snd_loop != SND_ENERGIZED )
				{
				  snd_loop = SND_ENERGIZED;
				  snd_start_loop( snd_loop, CHAN1 );
				}
			}

		  if ( ++count >= next_count )	/* Time to quit flashing? */
			{
			  /* Yes */
			  run_state = STATE_NORMAL;
			  snd_start_loop( SND_SIREN, CHAN1 );
			  
			  for( i=0; i<4; ++i )
				{
				  if ( ghost[i]->get_state() == GHOST_FLASHING )
					ghost[i]->set_state( GHOST_NORMAL );
				}
			}
		  break;
		}

	  gettimeofday( &tv2, NULL );

	  /* How many microseconds did that take? */
	  tdiff = tv2.tv_usec - tv1.tv_usec;

	  if ( tdiff < 0 )
		tdiff += 1000000;

	  /* How many more microseconds should we wait? */
	  tdiff = uwait - tdiff;

	  if ( tdiff > 0 )
		{
		  int r = tdiff % 10000;
		  tdiff -= r;
		  if ( r >= 5000 )
			tdiff += 10000;
		  usleep( tdiff );
		}
	  
	} while( !caught &&
			(runinfo->dotmap->num_left() || runinfo->bigdotmap->num_left())
			&& !is_key_pressed( sc_n ) );

  alarm( 30 );
  
  snd_clear_all( 1 );
  
  runinfo->seconds += time( NULL ) - t1;
  
  for( i=0; i<4; ++i )
	delete ghost[i];

  delete fruit;
  delete man;
  delete font;
  delete runlist;
  delete lux_bitmap;
  
  if ( caught )
	{
	  --runinfo->lives;
	  return 0;
	}
  else
	{
	  ++runinfo->level;
	  return 1;
	}

  alarm( 0 );
}
