/*
   highscore.cc

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

#include "error.h"
#include <rawkey/rawkey.h>
#include <gtools/gtools.h>
#include <unistd.h>
#include <limits.h>
#include "run.h"
#include "globals.h"

#define NUM_HIGHSCORES	10

#define HS_FILE "~/.luxman.scores"

#define NAMELEN		20

/* Entry in hiscore file */
extern "C" {
  typedef struct {
	char name[NAMELEN+1];
	int score;
	int level;
	int skill;
  } Entry;
}

static void write_hs_file( Entry *e )
{
  FILE *fp;
  int i;
  char name[PATH_MAX+NAME_MAX+1];

  if ( !resolve_tilde( name, HS_FILE ) )
	fatal( "Can't get home directory" );
		  
  fp = fopen( name, "w" );
  if ( !fp )
	fatal( "Can't open highscore file!");
  
  for( i=0; i<NUM_HIGHSCORES; ++i )
	fwrite( e, sizeof( Entry ), NUM_HIGHSCORES, fp );

  fclose( fp );
}
	 
static Entry *read_hs_file()
{
  Entry *e;
  int i;
  FILE *fp;
  char name[PATH_MAX+NAME_MAX+1];
  
  e = (Entry*)Malloc( NUM_HIGHSCORES * sizeof( Entry ) );

  if ( !resolve_tilde( name, HS_FILE ) )
	fatal( "Can't get home directory" );
	  
  if ( access( name, 0 ) != 0 )
	{
	  /* New file */
	  for( i=0; i<NUM_HIGHSCORES; ++i )
		{
		  memset( e[i].name, '-', NAMELEN );
		  e[i].name[NAMELEN] = 0;
		  e[i].score = 0;
		  e[i].level = 0;
		  e[i].skill = 0;
		}
	}
  else
	{
	  fp = fopen( name, "r" );
	  fread( e, sizeof( Entry ), NUM_HIGHSCORES, fp );
	  fclose( fp );
	}
  
  return e;
}

int update_highscores( int score, int level, int skill )
{
  Entry *e;
  int i, add_index;
  Font *font;
  char buf[NAMELEN+1];

  font = new Font( "small.font" );
  
  e = read_hs_file();
  i = 0;

  while( i < NUM_HIGHSCORES )
	{
	  if ( score > e[i].score ||
		  (score == e[i].score && skill > e[i].skill) )
		break;

	  ++i;
	}

  if ( i >= NUM_HIGHSCORES )
	{
	  Free( e );
	  return -1;
	}

  /* Want to add at i since score > score[i] && < score[i-1] */
  add_index = i;
  
  for( i=NUM_HIGHSCORES-2; i >= add_index; --i )
	{
	  strcpy( e[i+1].name, e[i].name );
	  e[i+1].score = e[i].score;
	  e[i+1].level = e[i].level;
	  e[i+1].skill = e[i].skill;
	}

  gr_fillbox( 0,0,319,198,BLACK );

  gr_textxy( "You made the Top 10!", 10, 25, font );

  gr_textxy( "Enter your name below", 10, 40, font );

  gr_textxy( "Name: ", 10, 55, font );
  
  input_line( buf, NAMELEN, 10 + gr_textw( "Name: ",font ), 55, font, BLACK );

  strcpy( e[add_index].name, buf );
  e[add_index].score = score;
  e[add_index].level = level;
  e[add_index].skill = skill;

  write_hs_file( e );
  Free( e );
  delete font;

  gr_fillbox( 0,0,319,198,BLACK );

  return add_index;
}

void display_highscores( int highlight )
{
  Font *font, *font_y;
  Entry *e;
  int i;
  char buf[200];
  char *banner = "High Score List";
  char *skill[] = { "Simple", "Easy", "Med", "Hard" };
  int w;
  
  gr_fillbox( 0,0,319,198,BLACK );
  font = new Font( "small.font" );
  font_y = new Font( "small.font" );
  font_y->subst( YELLOW, WHITE );
  
  e = read_hs_file();
  
  w = gr_textw( banner, font );
  gr_textxy( banner, 320/2-w/2, 10, font );

  gr_textxy( "Name", 70, 25, font );
  gr_textxy( "Score", 160, 25, font );
  gr_textxy( "Level", 200, 25, font );
  gr_textxy( "Skill", 240, 25, font );
  
  for( i=0; i<NUM_HIGHSCORES; ++i )
	{
	  sprintf( buf, "%*s   %7d   %2d    %5s", NAMELEN, e[i].name, e[i].score,
			  e[i].level, skill[e[i].skill] );
	  if ( i == highlight )
		gr_textxy( buf, 10, (i+3)*15, font_y );
	  else
		gr_textxy( buf, 10, (i+3)*15, font );	  
	}

  gr_update();

  /* Wait for any keypress */
  do {
	scan_keyboard();
  } while( !is_any_key_pressed() );

  /* Wait for release */
  do {
	scan_keyboard();
  } while( is_any_key_pressed() );
  
  Free( e );
  delete font;
  
  gr_fillbox( 0,0,319,198,BLACK );
}








