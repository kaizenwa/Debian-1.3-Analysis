/*
   play.cc

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

#include <unistd.h>
#include <sys/types.h>
#include <time.h>
#include <vga.h>
#include <rawkey/rawkey.h>
#include <stdlib.h>
#include <gtools/bitmap.h>
#include <gtools/gr.h>
#include <argv/argv.h>
#include <lib/vllist.h>
#include "dotmap.h"
#include "sigs.h"
#include "bigdotmap.h"
#include "run.h"
#include "globals.h"
#include "score.h"
#include "banner.h"
#include "sound.h"
#include "error.h"
#include "highscore.h"
#include "calib.h"
#include "lf.h"
#include "isleep.h"

VLList *levels=NULL;	/* Level *'s */

#define LEVEL( n ) ((Level*)(levels->at(n)))

static void delete_levels( void )
{
  Level *l;

  for( l=(Level*)levels->go_head(); l; l = (Level*)levels->go_next() )
	delete l;

  delete levels;
  
  levels = NULL;
}

/* Make sure SOMETHING given in level file for non-optional items */
static int brief_check_level( Level *lvl )
{
  if ( !lvl->mazename )
	return -1;

  if ( !lvl->fruitname )
	return -2;

  if ( !lvl->tile )
	return -3;

  if ( lvl->depth == -1 )
	return -4;
  
  return 0;
}

static int load_levels( char *filename )
{
  FILE *fp;
  char *name;
  int i;
  Level *lvl;
  
  if ( find_file( &name, filename, gb_top_path, NULL ) != 1 )
	fatal( "Can't find levels definition file `%s'.\n"
		  "Search path `%s'\n", filename, gb_top_path );
  
  fp = fopen( name, "r" );

  if ( !fp )
	fatal( "Error opening levels file `%s'\n", name );

  free( name );
  
  if ( levels )
	delete_levels();

  levels = new VLList();
  
  do {
	lvl = new Level;
					   
	if ( lf_read_level( lvl, fp ) != 0 )
	  {
		fclose( fp );
		return 0;
	  }

	if ( (i = brief_check_level( lvl )) != 0 )
	  {
		if ( i == -1 )
		  fatal( "Error: No mazename given for level %d\n",
				levels->num_items()+1 );
		if ( i == -2 )
		  fatal( "Error: No fruitname given for level %d\n", 
				levels->num_items()+1 );
		if ( i == -3 )
		  fatal( "Error: No tilename given for level %d\n",
				levels->num_items()+1 );
		if ( i == -4 )
		  fatal( "Error: No search depth given for level %d\n",
				levels->num_items()+1 );
	  }

	levels->add_tail( lvl );
	
  } while( 1 );
}

static void draw_level_display( int level, int wrap )
{
  Bitmap *fruit;
  int i;
  int w, h;
  
  fruit = new Bitmap( LEVEL(0)->fruitname );
  w = fruit->w;
  h = fruit->h;
  delete fruit;

  gr_fillbox( 5*(w+4), 198-h, 10*(w+4), 199, BLACK );
  
  for( i=0; i<5; ++i ) 
	{
	  fruit = new Bitmap( LEVEL(level-1)->fruitname );
	  put_bitmap_t( (5+i)*(fruit->w+4), 198-fruit->h, fruit );
	  delete fruit;

	  if ( level == 1 )
		{
		  if ( wrap )
			{
			  level = levels->num_items();
			  wrap = 0;	
			}
		  else
			return;
		}
	  else
		--level;
	}
}

int run_game( char *level_file, int *level, int skill )
{
  RunState *runstate;
  Font *small_font;
  float fps;
  Bitmap *lux;
  int r;
  int cur_level;
  int wrap=0;
  int score;
  Maze *maze;
  Bitmap *tile, *bmp;
  int time_div = 1;
  
  load_levels( level_file );

  if ( gb_start_level < 1 || gb_start_level > levels->num_items() )
	fatal( "Bad start level: %d\nValid range: [%d,%d]", gb_start_level,
		  1, levels->num_items() );
	  
  cur_level = gb_start_level;
  
  runstate = new RunState;

  lux = new Bitmap( "lux_l1.map" );

  small_font = new Font( "small.font" );

  runstate->lives = 2;
  runstate->next_free = 10000;

  runstate->skill = skill;

  maze = new Maze( LEVEL(0)->mazename );
  tile = new Bitmap( LEVEL(0)->tile );
  
  runstate->score = new Score( 319 - gr_textw( "00000000", small_font ),
					   maze->h*tile->h + 2, small_font );
  
  delete maze;
  delete tile;
  
  runstate->frames = 0;
  runstate->seconds = 0;
  runstate->level = 1;
  runstate->play_intro = 1;

  r = 1;
  
  do {
	
	if ( r == 1 )
	  {
		runstate->maze = new Maze( LEVEL(cur_level-1)->mazename );
		runstate->tile = new Bitmap( LEVEL(cur_level-1)->tile );
		runstate->dotmap = new DotMap( runstate->maze, runstate->tile, 2, 2,
									  LEVEL(cur_level-1)->dot_clr );
		runstate->bigdotmap = new BigDotMap( runstate->maze, runstate->tile,
											LEVEL(cur_level-1)->bigdot_clr );
		gb_bg_color = LEVEL(cur_level-1)->bg;
	  }

	runstate->search_depth = LEVEL(cur_level-1)->depth;
	runstate->regen_wait = LEVEL(cur_level-1)->regen_wait / time_div;
	runstate->fruitname = LEVEL(cur_level-1)->fruitname;
	runstate->fruitval = LEVEL(cur_level-1)->fruitval;
	runstate->fruit_off = LEVEL(cur_level-1)->fruit_off;
	runstate->fruit_on = LEVEL(cur_level-1)->fruit_on;
	runstate->max_fruit = LEVEL(cur_level-1)->max_fruit;

	runstate->ghost_etime = LEVEL(cur_level-1)->en_frames / time_div;
	runstate->ghost_ftime = LEVEL(cur_level-1)->fl_frames / time_div;

	runstate->lux_body = LEVEL(cur_level-1)->lux_body;
	runstate->lux_glasses = LEVEL(cur_level-1)->lux_glasses;
	
	draw_level_display( cur_level, wrap );
	
	r = run_screen( runstate );

	if ( r == 1 )
	  {
		if ( cur_level < levels->num_items() )	
		  ++cur_level;
		else
		  {
			/* Wrap around to first level */
			cur_level = 1;
			wrap = 1;

			/* Bump skill level */
			if ( runstate->skill < 3 )
			  ++runstate->skill;
			else
			  time_div *= 2;	/* Already at max skill -- decrease
								   energized time, etc. */
		  }

		delete runstate->maze;
		delete runstate->dotmap;
		delete runstate->bigdotmap;
		delete runstate->tile;
	  }

  } while( runstate->lives >= 0 && cur_level <= levels->num_items() );

  alarm( 0 );
  
  bmp = display_banner( "Player One", "Game Over", small_font );

  isleep( 2 );	

  put_bitmap( 320/2 - bmp->w/2, 200/2 - bmp->h/2, bmp );
  delete bmp;
  gr_update();
  
  score = runstate->score->get();
  *level = runstate->level;
  
  if ( runstate->seconds > 0 )
	fps = ((float)runstate->frames)/((float)runstate->seconds);
  else
	fps = 0;
  
  alarm( 10 );

  delete lux;
  delete small_font;
  delete runstate->maze;
  delete runstate->tile;
  delete runstate->dotmap;
  delete runstate->bigdotmap;
  delete runstate->score;
  delete runstate;

  delete_levels();
  
  printf("%f fps\n", fps );

  alarm( 0 );
  
  return score;
}
