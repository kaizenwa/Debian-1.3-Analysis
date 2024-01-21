/*
   pool.cc

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

#include <stdio.h>
#include <stdlib.h>
#include "pool.h"

void do_fatal_error( char *file, int line, char *fmt, ... );
#define fatal( fmt, arg... ) do_fatal_error( __FILE__, __LINE__, fmt, ## arg )

Pool::Pool( char *pname, int num,
		   void* (*alloc_f)(void), void (*free_f)(void*) )
{
  int i;

  /* Copy pointers */
  name = pname;
  alloc_fn = alloc_f;
  free_fn = free_f;

  /* Alloc pool pointers */
  pool = (void**)calloc( num, sizeof( void* ) );
  
  if ( !pool )
	fatal("Pool `%s': init error", name );

  /* Fill pool with objects */
  for( i=0; i<num; ++i )
	{
	  pool[i] = (alloc_fn)();
	  if ( !pool[i] )
		fatal("Pool `%s': init error", name );
	}

  min_index = max_index = next = num-1;
}

Pool::~Pool()
{
  int i;

#ifdef DEBUG  
  printf("Pool `%s': deleted\n", name );
  printf("-- Min index: %d/%d\n", min_index, max_index );
#endif
  
  if ( next != max_index )
	{
	  printf("-- WARNING!! Not all objects deleted (missing %d)\n",
			 max_index - next );
	}

  /* Free all objects we are holding */
  for( i=0; i<=next; ++i )
	  (free_fn)( pool[i] );

  free( pool );
}

void *Pool::palloc( void )
{
  void *obj;
  
  if ( next < 0 )
 	fatal( "Pool `%s' exhausted!!", name );

  obj = pool[next--];

  if ( next < min_index )
	min_index = next;

  return obj;
}

void Pool::pfree( void *obj )
{
  if ( next > max_index )
	fatal("Pool `%s': too many freed", name );

  pool[++next] = obj;
}

  

