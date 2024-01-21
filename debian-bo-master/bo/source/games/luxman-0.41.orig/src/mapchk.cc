/*
   mapchk.cc

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

#include <gtools/gtools.h>
#include <argv/argv.h>
#include <time.h>
#include <stdlib.h>
#include <unistd.h>
#include "agemap.h"
#include "colors.h"
#include "search.h"
#include "globals.h"

char *filename=NULL;
char do_help = ARGV_FALSE;

argv_t args[] = {
  { 'd', "depth", ARGV_INT, &gb_ghost_search_depth, "depth",
	"set max search depth" },
  { ARGV_MAND, NULL, ARGV_CHARP, &filename, "filename", "maze file" },
  { 'h', "help", ARGV_BOOL, &do_help, "help", "show this screen" },
  { ARGV_LAST }
};

/* Returns:
   0 - Exactly one found
   -1 - More than one
   -2 - None
   */
int find_color( Bitmap *maze, unsigned char c, int *tx, int *ty )
{
  int i, j;
  unsigned char *m;
  int found=0;
  
  m = maze->map;

  for( i=0; i<maze->h; ++i )
	{
	  for( j=0; j<maze->w; ++j )
		{
		  if ( *m == c )
			{
			  if ( found )
				return -1;		/* More than one */
								 
			  *tx = j;
			  *ty = i;
			  found = 1;
			}
		  ++m;
		}
	}

  if ( !found )
	return -2;

  return 0;
}

void find_error( char *what, int i )
{
  if ( i == -1 )
	printf("Error: More than one %s found.\n", what );
  else
	printf("Error: No %s found.\n", what );

  exit(1);
}

main( int argc, char *argv[] )
{
  Bitmap *maze;
  unsigned char *m;
  int i, j;
  AgeMap *map;
  int tx, ty;
  int c;
  time_t t1, t2, tlong=0;
  int lx=-1, ly=-1;
  
  argv_process( args, argc, argv );

  if ( do_help == ARGV_TRUE )
	{
	  argv_usage( args, ARGV_USAGE_LONG );
	  return 0;
	}

  if ( access( filename, 0 ) != 0 )
	{
	  printf("Can't find file `%s'\n", filename );
	  return 1;
	}
  
  maze = new Bitmap( filename );

  m = maze->map;
  
  i = find_color( maze, MAZE_HOME, &tx, &ty );

  if ( i != 0 )
	find_error( "ghost home", i );
  
  printf("(%d,%d)\n", tx, ty );
  printf("Depth: %d\n", gb_ghost_search_depth );
  
  for( i=0; i<maze->h; ++i )
	{
	  printf("Checking row %d... ", i );
	  for( j=0; j<maze->w; ++j )
		{
		  if ( *m != MAZE_TILE )
			{
			  t1 = time(NULL);
			  map = do_search_for( tx, ty, j, i, maze, gb_ghost_search_depth,
								  &c );
			  
			  if ( !map )
				{
				  printf("\nSearch failed at tile (%d,%d)\n", j, i );
				  return 1;
				}
			  t2 = time(NULL);
			  if ( t2-t1 >= tlong )
				{
				  tlong = t2-t1;
				  lx = j;
				  ly = i;
				}
			  
			  delete map;
			}
		  ++m;
		}
	  printf("OK\n");
	}

  printf("Longest: %d sec (%d,%d)\n", (int)tlong, lx, ly );

  /* Check other items */
  printf("Checking other items...\n");
  
  i = find_color( maze, MAZE_GHOST1, &tx, &ty );

  if ( i != 0 )
	find_error( "blue ghost location", i );

  i = find_color( maze, MAZE_GHOST2, &tx, &ty );

  if ( i != 0 )
	find_error( "red ghost location", i );

  i = find_color( maze, MAZE_GHOST3, &tx, &ty );

  if ( i != 0 )
	find_error( "purple ghost location", i );

  i = find_color( maze, MAZE_GHOST4, &tx, &ty );

  if ( i != 0 )
	find_error( "green ghost location", i );  

  i = find_color( maze, MAZE_LUX, &tx, &ty );

  if ( i != 0 )
	find_error( "luxman location", i );  

  i = find_color( maze, MAZE_FRUIT, &tx, &ty );

  if ( i == -1 )
	{
	  printf("Error: More than one fruit location found.\n");
	  return 1;
	}
  else if ( i == - 2 )
	printf("Note: No fruit location -- will use luxman location\n");

  printf("Check finished!\n");

  delete maze;
  
  return 0;
}

