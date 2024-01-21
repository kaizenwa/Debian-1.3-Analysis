/*
   ghost.cc

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
#include <stdlib.h>
#include <math.h>
#include "ghost.h"
#include "movement.h"
#include "error.h"
#include "colors.h"
#include "attack.h"
#include "search.h"
#include "globals.h"

#define VX	2
#define VY	2
#define OLD_AGE	(0x76543210)	  

#define FLASH_PERIOD	10	/* frames */
#define ANIM_PERIOD		3	/* frames */

Ghost::Ghost( unsigned char color, Maze *MAZE, int CX, int CY, int number )
{
  int i, j;
  unsigned char *m;

  ghost_number = number;
  
  new_vx = vx = VX;
  new_vy = vy = VY;
 
  state = GHOST_NORMAL;
  home_path = NULL;

  flash_state = 0;
  anim_count = 0;
  anim_state = 0;
  
  /* Load ghosts */
  map[LUX_LEFT] = new Bitmap( "ghost_l.map" );
  map[LUX_RIGHT] = new Bitmap( "ghost_r.map" );
  map[LUX_UP] = new Bitmap( "ghost_u.map" );
  map[LUX_DOWN] = new Bitmap( "ghost_d.map" );

  map[10+LUX_LEFT] = new Bitmap( "ghost_l2.map" );
  map[10+LUX_RIGHT] = new Bitmap( "ghost_r2.map" );
  map[10+LUX_UP] = new Bitmap( "ghost_u2.map" );
  map[10+LUX_DOWN] = new Bitmap( "ghost_d2.map" );  

  /* Replace CLR_GHOST with `color' */
  for( i=0; i<4; ++i )
	{
	  map[i]->subst( color, CLR_GHOST );
	  map[10+i]->subst( color, CLR_GHOST );
	}

  map[4] = new Bitmap( "ghost_z.map" );		/* Energized */
  map[5] = new Bitmap( "ghost_z.map" );
  map[5]->subst( WHITE, BLUE );				/* Flashing */

  map[14] = new Bitmap( "ghost_z2.map" );		/* Energized */
  map[15] = new Bitmap( "ghost_z2.map" );
  map[15]->subst( WHITE, BLUE );				/* Flashing */  
  
  map[6+LUX_LEFT] = new Bitmap( "eyes_l.map" );
  map[6+LUX_RIGHT] = new Bitmap( "eyes_r.map" );
  map[6+LUX_UP] = new Bitmap( "eyes_u.map" );
  map[6+LUX_DOWN] = new Bitmap( "eyes_d.map" );

  /* Setup area to save what we overwrite */
  save = new Bitmap( map[0]->w, map[0]->h );

  maze = MAZE;

  /* Search for `color' in maze to determine starting position */
  tx = ty = -1;
  home_x = home_y = -1;
  
  m = maze->map;
  
  for( i=0; i<maze->h; ++i )
	{
	  for( j=0; j<maze->w; ++j, ++m )
		{
		  if ( *m == color )
			{
			  tx = j;
			  ty = i;
			}
		  else if ( *m == MAZE_HOME )
			{
			  home_x = j;
			  home_y = i;
			}
		}
	}

  if ( tx == -1 )
	{
	  fatal("No starting position found for ghost [color %d]",
			 color );
	}

  if ( home_x == -1 )
	fatal( "No home position!" );
  
  cx = CX;
  cy = CY;

  /* Start ghost on tile boundary */
  x = tx * cx;
  y = ty * cy;

  ofx = ofy = 0;

  /* Pick a starting direction */
  if ( can_move_up( tx, ty, maze ) )
	movedir = LUX_UP;
  else if ( can_move_down( tx, ty, maze ) )
	movedir = LUX_DOWN;
  else if ( can_move_left( tx, ty, maze ) )
	movedir = LUX_LEFT;
  else if ( can_move_right( tx, ty, maze ) )
	movedir = LUX_RIGHT;
  else
	fatal("Ghost [color %d] is trapped!", color );
  
  visible = 0;

  clr = color;

  /*
   * Setup age map --
   * Set ages of all tiles to a large number so that we will
   * never move there.
   */
  agemap = new AgeMap( maze->w, maze->h );

  m = maze->map;

  for( i=0; i<maze->h; ++i )
	{
	  for( j=0; j<maze->w; ++j )
		{
		  if ( *m == MAZE_TILE )
			agemap->set( j, i, OLD_AGE );
		  ++m;
		}
	}
  
  cur_age = 1;
}

Ghost::~Ghost()
{
  int i;

#ifdef DEBUG
  verify_magic( __LINE__ );
  printf("Ghost magic OK\n");
#endif
  
  for( i=0; i<16; ++i )
	delete map[i];
  
  delete save;
  
  delete agemap;

  if ( home_path )
	delete home_path;
}

/* Draws ghost at (x,y) */
void Ghost::do_draw()
{
  /* Save underlying area */
  get_bitmap( save, x, y );
  
  if ( ++anim_count >= ANIM_PERIOD )
	{
	  anim_state = (anim_state == 0) ? 1 : 0;
	  anim_count = 0;
	}

  /* Write ghost image */
  switch( state )
	{
	case GHOST_NORMAL:
	  if ( anim_state )
		put_bitmap_t( x, y, map[10+movedir] );
	  else
		put_bitmap_t( x, y, map[movedir] );

	  break;

	case GHOST_ENERGIZED:
	  if ( anim_state )
		put_bitmap_t( x, y, map[14] );
	  else
		put_bitmap_t( x, y, map[4] );

	  break;

	case GHOST_FLASHING:
	  if ( ++flash_count >= FLASH_PERIOD )
		{
		  flash_state = (flash_state) ? 0:1;
		  flash_count = 0;
		}

	  if ( anim_state )
		put_bitmap_t( x, y, map[14+flash_state] );
	  else
		put_bitmap_t( x, y, map[4+flash_state] );
	  
	  break;
				   
	case GHOST_EYES:
	  put_bitmap_t( x, y, map[6+movedir] );
	  break;

	case GHOST_WAIT:
	  if ( term_count == 0 )
		{
		  state = GHOST_NORMAL;
		  new_vx = new_vy = 2;
		}
	  --term_count;
	  put_bitmap_t( x, y, map[6+movedir] );
	  break;
	}
  
  visible = 1;
}

void Ghost::erase()
{
  if ( visible )
	{
	  put_bitmap( x, y, save );
	  visible = 0;
	}
}

void Ghost::move( int to_x, int to_y )
{
  if ( !ofx && !ofy )
	{
	  switch( state )
		{
		case GHOST_NORMAL:
		  update_movedir0( to_x, to_y );
		  break;
		  
		case GHOST_ENERGIZED:
		case GHOST_FLASHING:
		  update_movedir0( to_x, to_y );
		  break;

		case GHOST_EYES:
		  update_movedir2( to_x, to_y );
		  break;

		case GHOST_WAIT:
		  update_movedir3( to_x, to_y );
		  break;

		default:
		  fatal( "bad ghost state" );
		}
	}

  do_movement();

  do_draw();
}

#ifdef DEBUG
void Ghost::verify_magic( int line )
{
  int i;

  agemap->verify_magic();

  for( i=0; i<16; ++i )
	{
	  if ( map[i]->verify_magic() != 0 )
		fatal( "clobbered ghost map (%d)", line );
	}

  if ( save->verify_magic() != 0 )
	fatal( "clobbered savemap");

}
#endif

#ifdef DEBUG
void Ghost::print_map()
{
  int i, j, k;
  
  printf("In (%d,%d)\n", tx, ty );

  for( i=0; i<maze->h; ++i )
	{
	  for( j=0; j<maze->w; ++j )
		{
		  k = agemap->get( j, i );
		  if ( k == OLD_AGE )
			printf("XX ");
		  else
			printf("%02d ", k );
		}
	  printf("\n");
	}
}
#endif

void Ghost::update_movedir0( int to_x, int to_y )
{
  int i, young;
  int count[4], prefer[4];

  int d=-1;		/* Make gcc shut up */

  /* Do line-of-sight checking for skill levels > 0 */

  /* Can we see the target? */
  if ( gb_skill_level > 0 &&
	  (d = find_path_to( to_x, to_y, tx, ty, maze )) >= 0 )
	{
	  /* Yes -- move that direction and clear count map */
	  agemap->clear( OLD_AGE );

#ifdef DEBUG
	  if ( can_move_dir( tx, ty, d, maze ) != 1 )
		fatal( "failure in find_path_to!");
#endif

	  if ( state == GHOST_NORMAL )
		movedir = d;		/* Move towards target */
	  else
		{
		  /* Try to move away from target */
		  movedir = -1;
		  
		  for( i=0; i<4; ++i )
			{
			  if ( i != d && can_move_dir( tx, ty, i, maze ) )
				{
				  movedir = i;
				  break;
				}
			}

		  if ( movedir == -1 )
			movedir = d;		/* No  choice */
		}
	  return;
	}
  
  /* Get path ages */
  
  /* Left */
  count[LUX_LEFT] = (tx > 0) ? (agemap->get( tx-1, ty )) : (OLD_AGE);

  /* Right */
  count[LUX_RIGHT] = (tx < (maze->w-1)) ? (agemap->get( tx+1, ty )) : (OLD_AGE);

  /* Up */
  count[LUX_UP] = (ty > 0) ? (agemap->get( tx, ty-1 )) : (OLD_AGE);

  /* Down */
  count[LUX_DOWN] = (ty < (maze->h-1)) ? (agemap->get( tx, ty+1 )) : (OLD_AGE);

  /* Find youngest */
  young = OLD_AGE;
  
  for( i=0; i<4; ++i )
	{
	  if ( count[i] <= young )
		young = count[i];
	}

  for( i=0; i<4; ++i)
	{
	  if ( count[i] > young )
		count[i] = OLD_AGE;
	}
  
  /* Update age map */
  ++cur_age;
  
  agemap->set( tx, ty, cur_age );

#ifdef DEBUG  
  if ( agemap->get( tx, ty ) != cur_age )
	fatal( "AgeMap write failed (??????)" );
#endif  

  /*
   * Now the only ones that aren't OLD_AGE are the also
   * the youngest.
   */

  /* Make `preferred' map */
  for( i=0; i<4; ++i )
	prefer[i] = 0;

  if ( count[LUX_LEFT] == young )
	{
	  if ( state == GHOST_NORMAL )
		{
		  if ( to_x <= tx )
			prefer[LUX_LEFT] = 1;
		}
	  else
		{
		  if ( to_x >= tx )
			prefer[LUX_LEFT] = 1;
		}
	}

  if ( count[LUX_RIGHT] == young )
	{
	  if ( state == GHOST_NORMAL )
		{
		  if ( to_x >= tx )
			prefer[LUX_RIGHT] = 1;
		}
	  else
		{
		  if ( to_x <= tx )
			prefer[LUX_RIGHT] = 1;
		}
	}

  if ( count[LUX_UP] == young )
	{
	  if ( state == GHOST_NORMAL )
		{
		  if ( to_y <= ty )
			prefer[LUX_UP] = 1;
		}
	  else
		{
		  if ( to_y >= ty )
			prefer[LUX_UP] = 1;
		}
	}

  if ( count[LUX_DOWN] == young )
	{
	  if ( state == GHOST_NORMAL )
		{
		  if ( to_y >= ty )
			prefer[LUX_DOWN] = 1;
		}
	  else
		{
		  if ( to_y <= ty )
			prefer[LUX_DOWN] = 1;
		}
	}      

  /* Disable seeking for skill levels < 2 */
  if ( gb_skill_level < 2 )
	memset( prefer, 0, 4*sizeof(int) );
  
  /* See if we prefer a direction */
  if ( prefer[0] == 0 && prefer[1] == 0 && prefer[2] == 0 &&
	  prefer[3] == 0 )
	{
	  /* Try current direction first */
	  if ( count[movedir] == young )
		return;
  
	  /* Try first youngest */
	  for( i=0; i<4; ++i )
		{
		  if ( count[i] == young )
			break;
		}

	  if ( i < 0 || i >= 4 || young == OLD_AGE)
		{
#ifdef DEBUG	  
		  print_map();
#endif	  

		  fatal( "No youngest tile found (?????)" );
		}

#ifdef DEBUG
	  if ( can_move_dir( tx, ty, i, maze ) != 1 )
		fatal( "Bad movedir" );
#endif
	  
	  movedir = i;
	  return;
	}
  else	/* There is a preferred direction */
	{
	  /* Use momentum for skill levels < 3 */
	  if ( (gb_skill_level < 3) && (prefer[movedir] == 1) )
		return;
	  else
		{
		  for( i=ghost_number; i<4; ++i )
			{
			  if ( prefer[i] != 0 )
				{
				  movedir = i;
				  return;
				}
			}
		  for( i=0; i<ghost_number; ++i )
			{
			  if ( prefer[i] != 0 )
				{
				  movedir = i;
				  return;
				}
			}
		   
		  fatal( "No preferred???" );
		}
	}
}

void Ghost::update_movedir2( int to_x, int to_y )
{
  int c;
  
#ifdef DEBUG
  if ( !home_path )
	fatal( "home path NULL" );
#endif
  
  c = home_path->get( tx, ty );

  if ( c == term_count )	/* Are we home? */
	{
	  state = GHOST_WAIT;
	  term_count = gb_ghost_regen_wait;
	  delete home_path;
	  home_path = NULL;
	  new_vx = new_vy = 1;
	  agemap->clear( OLD_AGE );
	  update_movedir3( to_x, to_y );
	  return;
	}

  /* Find next */
  if ( tx > 0 && home_path->get( tx-1, ty ) == c+1 )
	{
	  movedir = LUX_LEFT;
	  return;
	}

  if ( tx < (maze->w-1) && home_path->get( tx+1, ty ) == c+1 )
	{
	  movedir = LUX_RIGHT;
	  return;
	}

  if ( ty > 0 && home_path->get( tx, ty-1 ) == c+1 )
	{
	  movedir = LUX_UP;
	  return;
	}

  if ( ty < (maze->h-1) && home_path->get( tx, ty+1 ) == c+1 )
	{
	  movedir = LUX_DOWN;
	  return;
	}

  fatal( "path broken??" );
}

void Ghost::update_movedir3( int tx, int ty )
{
  switch( movedir )
	{
	case LUX_LEFT:
	  movedir = LUX_RIGHT;
	  break;

	case LUX_RIGHT:
	  movedir = LUX_LEFT;
	  break;

	case LUX_UP:
	  movedir = LUX_DOWN;
	  break;

	case LUX_DOWN:
	  movedir = LUX_UP;
	  break;

	default:
	  fatal( "Bad movedir [%d]", movedir );
	}
}

void Ghost::do_movement()
{
#ifdef DEBUG
  verify_magic( __LINE__ );
#endif

  /* See if we can update to `pending' velocity */
  if ( new_vx != vx )
	{
	  switch( new_vx )
		{
		case 1:
		  vx = new_vx;
		  break;
		  
		case 2:
		  if ( (ofx & 0x01) == 0 )
			vx = new_vx;
		  break;

		case 4:
		  if ( (ofx & 0x03) == 0 )
			vx = new_vx;
		  break;
		}
	}

  if ( new_vy != vy )
	{
	  switch( new_vy )
		{
		case 1:
		  vy = new_vy;
		  break;

		case 2:
		  if ( (ofy & 0x01) == 0 )
			vy = new_vy;
		  break;
		}
	}
		  
  /* Do movement */
  switch( movedir )
	{
	case LUX_RIGHT:
	  ofx += vx;
	  x += vx;

	  /* If we are halfway into next tile, update tx */
	  if ( ofx > (cx / 2) )
		{
		  tx += 1;
		  ofx = -cx + ofx;
		}
	  break;

	case LUX_LEFT:
	  ofx -= vx;
	  x -= vx;

	  if ( -ofx > (cx/2) )
		{
		  tx -= 1;
		  ofx = cx + ofx;
		}
	  break;

	case LUX_UP:
	  ofy -= vy;
	  y -= vy;

	  if ( -ofy > (cy/2) )
		{
		  ty -= 1;
		  ofy = cy + ofy;
		}
	  break;

	case LUX_DOWN:
	  ofy += vy;
	  y += vy;
	  
	  if ( ofy > (cx/2) )
		{
		  ty += 1;
		  ofy = -cy + ofy;
		}
	  break;

	default:
	  fatal( "Bad movedir [%d] (This shouldn't happen!!)",
			movedir );
	}
}

int Ghost::TX()
{
  return tx;
}

int Ghost::TY()
{
  return ty;
}

int Ghost::X()
{
  return x;
}

int Ghost::Y()
{
  return y;
}

int Ghost::get_state()
{
  /* Backwards compatibility with routines not expecting GHOST_WAIT */
  if ( state == GHOST_WAIT )
	return GHOST_EYES;
  else
	return state;
}

void Ghost::set_state( int i )
{
#ifdef DEBUG
  if ( i < 0 || i > 3 )		/* Don't allow setting state GHOST_WAIT */
	fatal( "Bad arg" );
#endif

  if ( i == state )
	return;
  
  state = i;

  if ( home_path )
	{
	  delete home_path;
	  home_path = NULL;
	}
  
  switch( state )
	{
	case GHOST_NORMAL:
	  new_vx = 2;
	  new_vy = 2;
	  break;

	case GHOST_ENERGIZED:
	case GHOST_FLASHING:
	  new_vx = 1;
	  new_vy = 1;
	  flash_count = 0;
	  flash_state = 0;
	  break;

	case GHOST_EYES:
	  new_vx = 4;
	  new_vy = 2;

	  home_path = do_search_for( home_x, home_y, tx, ty, maze,
							  gb_ghost_search_depth, &term_count,
								ghost_number );
	  if ( !home_path )
		fatal( "No path found!\n"
			  "**** DID YOU FORGET yTO RUN LuxCheck??? ****\n" );
	  break;
	}
}

	
