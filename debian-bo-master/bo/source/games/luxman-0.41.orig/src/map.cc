/*
   map.cc

   This file is part of LuxMan.
   
   Copyright (C) 1994,1995 Frank McIngvale (frankm@nuance.com)
   Portions Copyright (C) 1994 Bruce Tenison (rbtenis@ibm.net)

   Thanks to Michael Weller (eowmob@exp-math.uni-essen.de) for
   improvements.
   
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

#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <sys/mount.h>
#include <rawkey/rawkey.h>
#include <gtools/gtools.h>
#include <argv/argv.h>
#include "sigs.h"
#include "run.h"
#include "globals.h"
#include "score.h"
#include "sound.h"
#include "highscore.h"
#include "calib.h"
#include "play.h"
#include "error.h"
#include "vts.h"
#include "sf.h"

char cmd_help = ARGV_FALSE;
static char *scen_file = NULL;

argv_t args[] = {
#ifdef DEBUG  
  { 't', "test", ARGV_BOOL, &gb_test_mode, "test", "enable test mode" },
#endif  
  { 'l', "level", ARGV_INT, &gb_start_level, "level", "start level" },
  { 'f', "file", ARGV_CHARP, &scen_file, "file", "scenario file" },
  { 'r', "fps", ARGV_INT, &gb_frames_per_second, "fps", "set frames-per-second" },
  { 'h', "help", ARGV_BOOL, &cmd_help, "help", "show this help screen" },
  { '8', "8k", ARGV_BOOL, &gb_use_8k_sound, "8k sound", "use 8kHz sound (11kHz default)" },
  { 'n', "nosnd", ARGV_BOOL, &gb_dont_use_sound, "no sound", "don't use sound" },
  { ARGV_LAST }
};

static long vt_alarm_rem = 0;

void vt_switch_away( void )
{
  /* Cancel pending alarm before leaving */
  vt_alarm_rem = alarm( 0 );
  
  snd_sleep();
  vga_setmode( TEXT );
}

void vt_switch_back( void )
{
  vga_setmode( G320x200x256 );
  set_sig_handlers();
  snd_wakeup();
  gr_update();

  /* Restore alarm (if any) we cancelled */
  alarm( vt_alarm_rem );
}

int init_sound( int fd_cmd, int fd_ret )
{
  if ( snd_open_server( SND_LAST + 1, CHAN_LAST + 1, fd_cmd, fd_ret ) != 0 )
	{
	  gb_dont_use_sound = ARGV_TRUE;
	  return -1;
	}
  
  load_sounds();

  return 0;
}

/*
   items		-- strings to put in menu
   num			-- number of strings in `items'
   y			-- top of menu (screen coordinate)
   default_item	-- default item [0..(num-1)]
   centered		-- 1 if items should be centered
   
   Returns [0..(num-1)] on success, -1 on error
*/   
int do_lux_menu( char **items, int num, int y, int default_item,
				int centered )
{
  Bitmap *lux, *lux_b;
  Font *font, *font_b;
  int w, h, i, j;
  int pos;
  int x, sc;
  int sc_p;

  sc_p = scancode_trans( 'p' );

  if ( default_item < 0 || default_item >= num )
	{
	  fatal( "Bad arg (%d)", default_item );
	  return -1;	/* make gcc shut up */
	}
  else
	pos = default_item;
  
  lux = new Bitmap( "lux_r1.map" );
  lux_b = new Bitmap( "lux_r1.map" );
  lux_b->subst( BLACK, YELLOW );
  
  h = lux->h * 3/2;
  
  font = new Font( "small.font" );

  font_b = new Font( "small.font" );
  font_b->subst( BLACK, WHITE );
  
  /* Get max string width */
  w = 0;
  for( i=0; i<num; ++i )
	{
	  if ( (j=gr_textw( items[i], font )) > w )
		w = j;
	}

  w = w * 3/2;
  
  /* Center strings on screen */
  j = w + lux->w * 3/2;
  x = 320/2 - j/2;

  gr_frame( x - 5, y - 5, x + lux->w*3/2 + w + 5,
		   y + h*num + 5, 5, 5, LIGHTGRAY, WHITE, DARKGRAY, LIGHTGRAY );
		 
  /* Draw strings */
  for( i=0; i<num; ++i )
	{
	  if ( !centered )
		{
		  gr_textxy( items[i], 1 + x + lux->w * 3/2,
					1 + y + (i*h) + h/2 - gr_texth( items[i], font )/2,
					font_b );
		  gr_textxy( items[i], x + lux->w * 3/2,
					y + (i*h) + h/2 - gr_texth( items[i], font )/2,
					font_b );
		}
	  else
		{
		  gr_textxy( items[i], 1 + x + lux->w * 3/2 + w/2 -
					gr_textw( items[i],font )/2,
					1 + y + (i*h) + h/2 - gr_texth( items[i], font )/2,
					font_b );
		  gr_textxy( items[i], x + lux->w * 3/2 + w/2 -
					gr_textw( items[i],font )/2,
					y + (i*h) + h/2 - gr_texth( items[i], font )/2, font );
		}
	}	
  
  /* Draw pointer */
  put_bitmap_t( 1 + x + lux->w/4, 1 + y + (pos*h) + h/2 - lux->h/2, lux_b );
  put_bitmap_t( x + lux->w/4, y + (pos*h) + h/2 - lux->h/2, lux );
  gr_update();

  while( 1 )
	{
	  scan_keyboard();

	  if ( is_key_pressed( ENTER_KEY ) )
		{
		  while( is_key_pressed( ENTER_KEY ) )
			scan_keyboard();
		  break;
		}
	  
	  if ( is_key_pressed( CURSOR_UP ) || is_key_pressed( CURSOR_DOWN ) )
		/* Erase current pointer */
		gr_fillbox( x, y + (pos*h) + h/2 - lux->h/2,
				   x + lux->w + 1, y + (pos)*h + h/2 + lux->h/2,
				   LIGHTGRAY );
	  else
		continue;

	  if ( is_key_pressed( CURSOR_UP ) )
		{
		  if ( pos == 0 )
			pos = num-1;
		  else
			--pos;

		  sc = CURSOR_UP;
		}
	  else
		{
		  if ( pos == num-1 )
			pos = 0;
		  else
			++pos;

		  sc = CURSOR_DOWN;
		}

	  put_bitmap_t( 1 + x + lux->w/4, 1 + y + (pos*h) + h/2 - lux->h/2,
				   lux_b );
	  put_bitmap_t( x + lux->w/4, y + (pos*h) + h/2 - lux->h/2, lux );
	  gr_update();

	  while( is_key_pressed( sc ) )
		scan_keyboard();
	}

  delete lux;
  delete lux_b;
  delete font;
  delete font_b;
  return pos;
}

int ask_scenario( Scenarios *sns )
{
  int r, i;
  char *banner = "Select Scenario";
  Font *font;
  char **items;
  char *s;
  
  font = new Font( "small.font" );
  
  gr_fillbox( 0,0,319,198, BLACK );
  gr_textxy( banner, 320/2 - gr_textw(banner,font)/2, 20, font );

  items = (char**)alloca( (sns->names->num_items()+1) * sizeof( char* ) );
  if ( !items )
	fatal( "Out of memory." );

  for ( i=0, s = (char*)sns->names->go_head(); s;
	   s = (char*)sns->names->go_next(), ++i )
	items[i] = s;

  items[sns->names->num_items()] = "Back to Main";
  
  r = do_lux_menu( items, sns->names->num_items()+1, 50, 0, 1 );

  delete font;

  return (r != sns->names->num_items()) ? r : -1;
}

int do_main_menu( int default_item )
{
  Font *big, *small;
  char *banner = "LuxMan";
  char *banner2 = "A maze game for Linux";
  char *banner3 = "(c) 1995 Frank McIngvale (frankm@nuance.com)";
 
  char *items[] = { "Start new game", "View highscores",
					"About LuxMan...", "Quit" };
  int r;
  
  big = new Font( "test.font" );
  small = new Font( "small.font" );
  
  gr_fillbox( 0,0,319,198,BLACK );

  gr_textxy( banner, 320/2 - gr_textw(banner,big)/2, 10, big );
  gr_textxy( banner2, 320/2 - gr_textw(banner2,small)/2, 35, small );
  gr_textxy( banner3, 320/2 - gr_textw(banner3,small)/2, 180, small );
  
  r = do_lux_menu( items, 4, 65, default_item, 1 );

  delete small;
  delete big;

  return r;
}

/* Returns 0 (easy) through 3 (hard), or -1 if user
   chooses `Back to Main' */
int ask_skill_level()
{
  char *labels[] = { "Hard", "Medium", "Easy", "Simple", "Back to Main" };
  char *banner = "Select Skill Level";
  int r;
  Font *font;

  font = new Font( "small.font" );
  
  gr_fillbox( 0,0,319,198, BLACK );
  gr_textxy( banner, 320/2 - gr_textw(banner,font)/2, 20, font );

  r = do_lux_menu( labels, 5, 50, 1, 1 );

  delete font;

  if ( r == 4 )
	return -1;
  else
	return (3-r);
}

void do_credits_screen()
{
  Font *small, *big;
  int x1=80, x2, y = 50;
  int spc=30;
  char buf[200];

  big = new Font( "test.font" );
  small = new Font( "small.font" );

  x2 = x1 + gr_textw( "Graphics: ", small );
  
  gr_fillbox( 0,0,319,199,BLACK );

  sprintf( buf, "LuxMan v%s", gb_version_string );
  
  gr_textxy_cu( buf, 320/2, 15, big );
  
  gr_textxy_u( "Graphics", x1, y, small );
  gr_textxy( "Frank McIngvale", x2, y, small );

  gr_textxy_u( "Sound", x1, y+spc, small );
  gr_textxy( "Bruce Tenison", x2, y+spc, small );
  gr_textxy( "Frank McIngvale", x2, y+2*spc, small );
  
  gr_textxy_u( "Sampling", x1, y+3*spc, small );
  gr_textxy( "David Clem", x2, y+3*spc, small );
  gr_textxy( "Frank McIngvale", x2, y+4*spc, small );

  gr_update();

  /* Wait for any keypress */
  do {
	scan_keyboard();
  } while( !is_any_key_pressed() );

  /* Wait for key release */
  do {
	scan_keyboard();
  } while( is_any_key_pressed() );
}

main( int argc, char *argv[] )
{
  int score, level, skill, rate;
  int r, index=-1;
  int ret_pipe[2], cmd_pipe[2]; 
  Scenarios *sns;
  char *name;
  FILE *fp;
  int n;
  
  vt_watch_switch();
  
  printf("LuxMan v%s, Copyright (c) 1995 Frank McIngvale\n", gb_version_string );
  printf("LuxMan comes with ABSOLUTELY NO WARRANTY; see COPYING for details.\n\n");

  /* Init VGA and give up root privs */
  vga_init();

  argv_process( args, argc, argv );

  if ( cmd_help == ARGV_TRUE )
	{
	  argv_usage( args, ARGV_USAGE_LONG );
	  vt_unwatch_switch();
	  return 0;
	}

  if ( gb_use_8k_sound == ARGV_TRUE )
	{
	  printf("\n"
			 "*************************************************************\n"
			 "  NOTE! The `-8' option will likely be removed in a future\n"
			 "  version of LuxMan. If you find this option useful, please\n"
			 "  e-mail me (frankm@nuance.com) and tell me.\n"
			 "*************************************************************\n"
			 );
	  printf("\nPausing.");
	  fflush( stdout );
	  for( r=0; r<10; ++r )
		{
		  sleep(1);
		  printf(".");
		  fflush( stdout );
		}
	  printf("\n");
	}
  
  if ( gb_test_mode == ARGV_TRUE )
	printf("Test mode -- Skipping calibration\n");
  else
	{
	  printf("\nThe frame rate is now set to %d frames per second.\n",
			 gb_frames_per_second );
	  printf("If the game seems too fast, too slow, or too jerky,\n");
	  printf("you can adjust this value the `-r' option.\n\n");
	  printf("Calibrating delay...");
	  fflush( stdout );
	  calibrate_usleep( 3 );
	}

  vt_unwatch_switch();
  
  /* Init two pipes to talk to snd-server */
  if ( pipe( ret_pipe ) || pipe( cmd_pipe ) )
 	{
 	  perror("Error: Can't make pipes");
 	  exit(1);
 	}
  
  gb_pid_sndserver = fork();

  if ( gb_pid_sndserver < 0 )
	{
	  printf("Error: Can't fork\n");
	  exit(1);
	}
  
  if ( gb_pid_sndserver == 0 )	/* Child */
	{
 	  char argument[40]; /* Well this is not really fool proof but
							should be really enough for two small ints */
 
 	  /* Child, put fd's of our pipe ends in a string to pass
		 through the exec */
 	  sprintf( argument, "%d %d", cmd_pipe[0] /* reading end */,
 					 ret_pipe[1] /* writing end */ );
 
 	  /* close unneeded ends */
 	  close( cmd_pipe[1] );
 	  close( ret_pipe[0] );
 
 	  execlp( "luxman-snd", "luxman-snd", argument, NULL );
	  
	  printf("Error: Unable to exec luxman-snd (sound server)\n");
	  exit(0);
	}

  /* (Parent) */
  
  /* close unneeded ends */
  close( cmd_pipe[0] );
  close( ret_pipe[1] );
	
  /* Load scenarios file */
  if ( scen_file == NULL )
	scen_file = "scenarios";

  if ( find_file( &name, scen_file, gb_root_path, NULL ) != 1 )
	{
	  printf("Error: Can't find scenarios file `%s'\n", scen_file );
	  return 1;
	}
  
  fp = fopen( name, "r" );
  if ( !fp )
	{
	  printf("Error: Can't open scenarios file `%s'\n", name );
	  return 1;
	}

  sns = new Scenarios( fp );

  if ( sns->error == -2 )
	{
	  printf("Error in scenario file `%s'.\n", name );
	  return 1;
	}
  else if ( sns->error == -1 )
	{
	  printf("Error: Out of memory reading scenario file.\n" );
	  return 1;
	}

  free( name );

  gb_top_path = (char*)(sns->dirs->at(0));
  
  gr_init( 1 );
  set_sig_handlers();
  rawmode_init();

  set_switch_functions( vt_switch_away, vt_switch_back );
  allow_switch( 1 );

#ifdef DEBUG
  printf("[Debugging version]\n");
#endif

  init_sound( cmd_pipe[1], ret_pipe[0] );
  r = 0;
  
  do {
	r = do_main_menu( r );

	if ( r == 0 )
	  {
		n = ask_scenario( sns );

		if ( n == -1 )		/* Back to main */
		  continue;
		
		skill = ask_skill_level();
		if ( skill == -1 )	/* Back to main */
		  continue;

		if ( gb_use_8k_sound == ARGV_TRUE )
		  rate = 8000;
		else
		  rate = 11025;
		
		if ( gb_dont_use_sound != ARGV_TRUE )
		  {
		    if ( snd_open_sound( rate ) != 0 )
		      gb_dont_use_sound = ARGV_TRUE;
		  }

		gb_top_path = (char*)(sns->dirs->at(n));
		
		score = run_game( (char*)(sns->files->at(n)), &level, skill );
		
		snd_close_sound();

		index = update_highscores( score, level, skill );
		display_highscores( index );
	  }
	else if ( r == 1 )
	  display_highscores( index );
	else if ( r == 2 )
	  do_credits_screen();

  } while( r != 3 );

  delete sns;
  
  allow_switch( 0 );
  rawmode_exit();
  gr_close();

  snd_close_server();

  kill( gb_pid_sndserver, SIGTERM );
}



