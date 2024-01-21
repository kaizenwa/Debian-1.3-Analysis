/*
   agemap.cc

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
#include <string.h>
#include <stdio.h>
#include "agemap.h"
#include "error.h"
#include <gtools/fmalloc.h>

AgeMap::AgeMap( int W, int H )
{
  int i;
  
  w = W;
  h = H;

  map_bytes = W * H * sizeof( int );

  map = (int*)Malloc( map_bytes );

  if ( !map )
	fatal( "Out of memory" );

  memset( map, 0, map_bytes );
  
  /* Calc row offsets */
  rofs = (int*)Malloc( h * sizeof( int ) );

  if ( !rofs )
	fatal( "Out of memory" );

  for( i=0; i<h; ++i )
	rofs[i] = i*W;
}

#ifdef DEBUG
void AgeMap::verify_magic()
{
  if ( Mcheck( map ) != 0 )
	fatal( "AgeMap clobbered" );

  if ( Mcheck( rofs ) != 0 )
	fatal( "rofs clobbered" );
}
#endif

AgeMap::~AgeMap()
{
#ifdef DEBUG  
  verify_magic();
  printf("AgeMap magic OK\n");
#endif

  Free( map );
  Free( rofs );
}

int AgeMap::set( int x, int y, int val )
{
#ifdef DEBUG
  verify_magic();
  
  if ( x < 0 || x >= w )
	fatal( "Bad x-coord - [%d]\n", x );
  if ( y < 0 || y >= h )
	fatal( "Bad y-coord - [%d]\n", y );
#endif

  *(map + rofs[y] + x) = val;
  return 0;
}

int AgeMap::get( int x, int y )
{
#ifdef DEBUG
  verify_magic();
  
  if ( x < 0 || x >= w )
	fatal( "Bad x-coord - [%d]\n", x );
  if ( y < 0 || y >= h )
	fatal( "Bad y-coord - [%d]\n", y );
#endif

  return *(map + rofs[y] + x);
}

int AgeMap::W()
{
  return w;
}

int AgeMap::H()
{
  return h;
}

void AgeMap::clear( int except )
{
  int i, j;
  int *m;

#ifdef DEBUG
  verify_magic();
#endif
  
  m = map;
  
  for( i=0; i<h; ++i )
	{
	  for( j=0; j<w; ++j )
		{
		  if ( *m != except )
			*m = 0;
		  ++m;
		}
	}
}
