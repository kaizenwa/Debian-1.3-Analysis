/*
   lux.cc

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
#include <rawkey/rawkey.h>
#include "ghost.h"
#include "movement.h"
#include "error.h"
#include "lux.h"
#include "globals.h"
#include <argv/argv.h>

#define VX	2
#define VY	2

LuxMan::LuxMan( int spos_color, Maze *MAZE, int CX, int CY,
			   int body_clr, int glasses_clr )
{
  int i, j;
  char buf[200];
  static char *dirs = "rlud";
  unsigned char *m;
  
  /* Load bitmaps */

  /* Each direction */
  for( i=0; i<4; ++i )
	{
	  /* Each anim frame */
	  for( j=0; j<4; ++j )
		{
		  sprintf( buf, "lux_%c%1d.map", dirs[i], j+1 );
		  map[i*4 + j] = new Bitmap( buf );
		  map[i*4 + j]->subst2( body_clr, YELLOW, glasses_clr, BLACK );
		}
	}

  save = new Bitmap( map[0]->w, map[0]->h );
  maze = MAZE;
  tx = ty = -1;

  m = maze->map;

  for( i=0; i<maze->h; ++i )
	{
	  for( j=0; j<maze->w; ++j, ++m )
		{
		  if ( *m == spos_color )
			{
			  tx = j;
			  ty = i;
			  break;
			}
		}

	  if ( tx != -1 )
		break;
	}
  
  if( tx == -1 )
	{
	  fatal( "No starting tile for luxman");
	}

  cx = CX;
  cy = CY;

  x = tx*cx;
  y = ty*cy;

  ofx = ofy = 0;

  /* Pick a starting direction */
  if ( can_move_left( tx, ty, maze ) )
	movedir = LUX_LEFT;
  else if ( can_move_right( tx, ty, maze ) )
	movedir = LUX_RIGHT;
  else if ( can_move_up( tx, ty, maze ) )
	movedir = LUX_UP;
  else if ( can_move_down( tx, ty, maze ) )
	movedir = LUX_DOWN;
  else
	fatal( "Error: luxman is trapped!" );

  imgbase = movedir;
  
  visible = 0;

  imgofs = 0;

  pending_key = -1;
}

#ifdef DEBUG
void LuxMan::verify_magic()
{
  int i;

  for ( i=0; i<16; ++i )
	{
	  if ( map[i]->verify_magic() != 0 )
		fatal( "clobbered image in luxman" );
	}

  if ( save->verify_magic() != 0 )
	fatal( "clobbered saveimage in luxman" );
}
#endif
  
LuxMan::~LuxMan()
{
  int i;

#ifdef DEBUG
  verify_magic();
  printf("LuxMan magic OK\n");
#endif
  
  for( i=0; i<16; ++i )
	delete map[i];

  delete save;
}

void LuxMan::move()
{
  update_movedir();
  
  do_movement();
  do_draw();
}

void LuxMan::do_draw()
{
#ifdef DEBUG
  verify_magic();
#endif
  
  get_bitmap( save, x, y );
  
  put_bitmap_t( x, y, map[imgbase*4 + imgofs] );
  
  visible = 1;
}

void LuxMan::erase()
{
#ifdef DEBUG
  verify_magic();
#endif
  
  if ( visible )
	{
	  put_bitmap( x, y, save );
	  visible = 0;
	}
}

void LuxMan::update_movedir()
{
#ifdef DEBUG
  verify_magic();
#endif

  /* See if one of arrow keys pressed */
  scan_keyboard();

  if ( is_key_pressed( CURSOR_RIGHT ) )
	pending_key = CURSOR_RIGHT;
  else if ( is_key_pressed( CURSOR_LEFT ) )
	pending_key = CURSOR_LEFT;
  else if ( is_key_pressed( CURSOR_UP ) )
	pending_key = CURSOR_UP;
  else if ( is_key_pressed( CURSOR_DOWN ) )
	pending_key = CURSOR_DOWN;

  if ( ofx )
	{
       /* Not on boundary in X direction so we can't move up or down */
	  if ( pending_key == CURSOR_UP || pending_key == CURSOR_DOWN )
		pending_key = -1;
	}

  if ( ofy )
	{
	  /* Not on boundary in Y direction so we can't move left or right */
	  if ( pending_key == CURSOR_LEFT || pending_key == CURSOR_RIGHT )
		pending_key = -1;
	}

  /* If we got an arrow key, update movedir */
  if ( pending_key != -1 )
	{
	  switch( pending_key )
		{
		case CURSOR_LEFT:
		  if ( ofx || (!ofx && can_move_dir( tx, ty, LUX_LEFT, maze ) ) )
			{
			  movedir = LUX_LEFT;
			  pending_key = -1;
			  return;
			}
		  break;

		case CURSOR_RIGHT:
		  if ( ofx || (!ofx && can_move_dir( tx, ty, LUX_RIGHT, maze )) )
			{
			  movedir = LUX_RIGHT;
			  pending_key = -1;
			  return;
			}
		  break;

		case CURSOR_DOWN:
		  if ( ofy || (!ofy && can_move_dir( tx, ty, LUX_DOWN, maze )) )
			{
			  movedir = LUX_DOWN;
			  pending_key = -1;
			  return;
			}
		  break;

		case CURSOR_UP:
		  if ( ofy || (!ofy && can_move_dir( tx, ty, LUX_UP, maze )) )
			{
			  movedir = LUX_UP;
			  pending_key = -1;
			  return;
			}
		  break;
		}
	}

  /* If we made it here, we didn't change directions based on keystroke,
	 so make sure we can keep going in same direction */
  if ( !ofx && !ofy && movedir != -1 )
	{
	  if ( can_move_dir( tx, ty, movedir, maze ) != 1 )
		movedir = -1;
	}
}

void LuxMan::do_movement()
{
  if ( movedir == -1 )
	return;

  imgbase = movedir;

  switch( movedir )
	{
	case LUX_RIGHT:
	  ofx += VX;
	  x += VX;

	  if ( ofx > (cx / 2) )
		{
		  tx += 1;
		  ofx = -cx + ofx;
		}
	  break;

	case LUX_LEFT:
	  ofx -= VX;
	  x -= VX;

	  if ( -ofx > (cx/2) )
		{
		  tx -= 1;
		  ofx = cx + ofx;
		}
	  break;

	case LUX_UP:
	  ofy -= VY;
	  y -= VY;

	  if ( -ofy > (cy/2) )
		{
		  ty -= 1;
		  ofy = cy + ofy;
		}
	  break;

	case LUX_DOWN:
	  ofy += VY;
	  y += VY;
	  
	  if ( ofy > (cx/2) )
		{
		  ty += 1;
		  ofy = -cy + ofy;
		}
	  break;

	default:
	  fatal( "Bad movedir (this shouldn't happen!) [%d]\n",
			movedir );
	  break;
	}

  /* Cycle animation */
  if ( cycledir )
	{
	  if( imgofs == 3 )
		{
		  cycledir = 0;
		  imgofs = 2;
		}
	  else
		++imgofs;
	}
  else
	{
	  if ( imgofs == 0 )
		{
		  imgofs = 1;
		  cycledir = 1;
		}
	  else
		--imgofs;
	}
}

int LuxMan::TX()
{
  return tx;
}

int LuxMan::TY()
{
  return ty;
}

int LuxMan::X()
{
  return x;
}

int LuxMan::Y()
{
  return y;
}

