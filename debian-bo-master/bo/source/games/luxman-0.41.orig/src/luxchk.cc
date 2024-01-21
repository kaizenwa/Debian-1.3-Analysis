/*
   luxchk.cc

   This file is part of LuxMan.
   
   Copyright (C) 1995 Frank McIngvale (frankm@nuance.com)
   
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
#include <unistd.h>
#include <gtools/gtools.h>
#include <argv/argv.h>
#include <time.h>
#include <stdlib.h>
#include <stdarg.h>
#include "agemap.h"
#include "colors.h"
#include "search.h"
#include "globals.h"
#include "lf.h"
#include "maze.h"

char be_verbose = ARGV_FALSE;
char *lvl_file = NULL;
int check_lvl = -1;

argv_t args[] = {
  { 'v', "verbose", ARGV_BOOL, &be_verbose, "verbose", "verbose messages" },
  { ARGV_MAND, NULL, ARGV_CHARP, &lvl_file, "file", "level file" },
  { 'l', "level", ARGV_INT, &check_lvl, "level", "check one level" },
  { ARGV_LAST }
};

void vv( char *fmt, ... )
{
  va_list list;

  if ( be_verbose == ARGV_FALSE )
	return;
  
  va_start( list, fmt );
  vprintf( fmt, list );
}

/* Returns:
   1  - Exactly one found
   2  - More than one
   0  - None
   */
int find_color( Maze *maze, unsigned char c, int *tx, int *ty )
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
				return 2;		/* More than one */
								 
			  *tx = j;
			  *ty = i;
			  found = 1;
			}
		  ++m;
		}
	}

  if ( !found )
	return 0;

  return 1;
}

void find_error( char *what, int i )
{
  if ( i == -1 )
	printf("Error: More than one %s found.\n", what );
  else
	printf("Error: No %s found.\n", what );

  exit(1);
}

/*
   Returns 0 if level OK, < 0 if not
*/   
int check_level( Level *lvl )
{
  Maze *maze;
  unsigned char *m;
  int i, j;
  AgeMap *map;
  int tx, ty;
  int c;
  long u_long=0, u_diff;
  int lx=-1, ly=-1;
  char *name;
  struct timeval t1, t2;
  float rtot;
  
  /* ---- Check required level items ---- */
  printf("Required items: Level - ");
  fflush( stdout );
  vv("\n");
  
  if ( !lvl->fruitname )
	{
	  printf("\nNo fruit name given.\n");
	  return -1;
	}

  if ( find_file( &name, lvl->fruitname, "./maps", NULL ) != 1 )  
	{
	  printf( "\nCan't find fruit bitmap file `%s'\n", lvl->fruitname );
	  return -1;
	}

  vv( "Fruit bitmap:%s:\n", name );
  
  free( name );
  
  if ( !lvl->tile )
	{
	  printf("\nNo tile name given.\n");
	  return -1;
	}

  if ( find_file( &name, lvl->tile, "./maps", NULL ) != 1 )
	{
	  printf( "\nCan't find tile bitmap file `%s'\n", lvl->tile );
	  return -1;
	}

  vv( "Tile bitmap:%s:\n", name );
  
  free( name );
  
  if ( lvl->depth == -1 )
	{
	  printf("\nNo search depth given.\n");
	  return -1;
	}

  vv( "Search depth: %d\n", lvl->depth );
  
  if ( !lvl->mazename )
	{
	  printf("\nNo maze name given.\n");
	  return -1;
	}
  
  if ( find_file( &name, lvl->mazename, "./mazes", NULL ) != 1 )
	{
	  printf( "\nCan't find maze file `%s'\n", lvl->mazename );
	  return -1;
	}

  vv( "Maze name:%s:\n", name );
  
  /* ---- Check maze file ---- */
  
  maze = new Maze( name );
  free( name );
  
  m = maze->map;

  /* Check for necessary items */
  printf("OK, Maze - ");
  fflush( stdout );
  vv("\n");
  
  if( find_color( maze, MAZE_GHOST1, &tx, &ty ) == 0 )
	{
	  printf( "\nNo blue ghost location.\n" );
	  return -1;
	}

  if( find_color( maze, MAZE_GHOST2, &tx, &ty ) == 0 )
	{
	  printf( "\nNo red ghost location.\n" );
	  return -1;
	}

  if( find_color( maze, MAZE_GHOST3, &tx, &ty ) == 0 )
	{
	  printf( "\nNo purple ghost location.\n" );
	  return -1;
	}

  if( find_color( maze, MAZE_GHOST4, &tx, &ty ) == 0 )
	{
	  printf( "\nNo green ghost location.\n" );
	  return -1;
	}

  vv( "Found all ghosts.\n" );
  
  if( find_color( maze, MAZE_LUX, &tx, &ty ) == 0 )
	{
	  printf( "\nNo luxman location.\n" );
	  return -1;
	}

  vv( "Found LuxMan.\n" );
  
  i = find_color( maze, MAZE_FRUIT, &tx, &ty );

  if ( i == 2 )
	{
	  printf("\nError: More than one fruit location found.\n");
	  return -1;
	}
  else if ( i == 0 )
	printf("(fruit==lux)");

  vv( "Fruit location OK\n" );
  
  if ( find_color( maze, MAZE_HOME, &tx, &ty ) == 0 )
	{
	  printf( "No ghost home.\n" );
	  return -1;
	}

  vv( "Found ghost home.\n" );
  printf(" OK\n");
  
  /* Check that search depth is sufficient */
  printf("Checking search depth...");

  rtot = 0.0;
  
  for( i=0; i<maze->h; ++i )
	{
	  printf(".");
	  fflush( stdout );
	  
	  for( j=0; j<maze->w; ++j )
		{
		  if ( *m != MAZE_TILE )
			{
			  gettimeofday( &t1, NULL );
			  
			  map = do_search_for( tx, ty, j, i, maze, lvl->depth,
								  &c );
			  
			  if ( !map )
				{
				  printf("\nSearch failed at tile (%d,%d)\n", j, i );
				  return -1;
				}
			  
			  gettimeofday( &t2, NULL );

			  if ( t2.tv_usec < t1.tv_usec )
				{
				  u_diff = (t2.tv_sec-t1.tv_sec-1)*1000000 +
					(t2.tv_usec - t1.tv_usec + 1000000);
				}
			  else
				{
				  u_diff = (t2.tv_sec-t1.tv_sec)*1000000 +
					(t2.tv_usec - t1.tv_usec);
				}

			  rtot += ((float)u_diff) / 1000000.0;
				
			  if ( u_diff >= u_long )
				{
				  u_long = u_diff;
				  lx = j;
				  ly = i;
				}

			  delete map;
			}
		  ++m;
		}
	}

  printf(" OK!\n");
  printf("Longest: %f seconds; tile: (%d,%d)", ((float)u_long)/1000000.0,
		 lx, ly );
  printf("; Average: %f seconds.\n",
		 ((float)rtot) / ((float)(maze->h*maze->w)) );

  delete maze;
  
  return 0;
}

main( int argc, char *argv[] )
{
  FILE *fp;
  char *name;
  int i;
  Level *lvl;
  int nchecked;
  int nread;
  
  printf("LuxCheck v%s, Copyright (c) 1995 Frank McIngvale\n", gb_version_string );
  printf("LuxCheck comes with ABSOLUTELY NO WARRANTY; see COPYING for details.\n\n");

  if ( argc < 2 )
	{
	  argv_usage( args, ARGV_USAGE_LONG );
	  return 0;
	}
  
  argv_process( args, argc, argv );

  gb_top_path = ".";
  gb_maze_subdir = "";
  gb_image_subdir = "";
  
  if ( find_file( &name, lvl_file, gb_top_path, NULL ) == 0 )
	{
	  printf("Error: Can't find level file `%s'\n", lvl_file );
	  return 1;
	}

  vv( "Reading level file `%s'\n", name );

  fp = fopen( name, "r" );
  if ( !fp )
	{
	  printf("Error opening level file `%s'\n", name );
	  return 1;
	}

  nread = 0;
  nchecked = 0;
  
  do {
	lvl = new Level();
	
	i = lf_read_level( lvl, fp );

	if ( i == 0 )
	  {
		++nread;
		
		if ( !(check_lvl > 0 && check_lvl != nread ) )
		{
		  printf("Checking level %d...\n", nread );
		
		  i = check_level( lvl );

		  if ( i != 0 )
			{
			  printf("\n **** Error while checking level #%d ****\n", nread );
			  return 1;
			}

		  ++nchecked;
		}
	  }

	delete lvl;
	
  } while( i == 0 );

  fclose( fp );

  /* Figure out why we stopped */
  
  if ( i == -2 )	/* EOF */
	{
	  /* Do this the `pretty' way */
	  if ( nread == 1 )
		printf("1 level found, " );
	  else
		printf("%d levels found, ", nread );

	  if ( nchecked == 1 )
		printf("1 level checked.\n" );
	  else
		printf("%d levels checked.\n", nchecked );
	}
  else		/* Error */
	printf(" **** Error: End-of-file in level %d **** \n", nread+1 );
}
