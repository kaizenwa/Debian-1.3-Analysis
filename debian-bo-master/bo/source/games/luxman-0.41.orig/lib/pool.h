/*
   pool.h
   
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

#ifndef _pool_h_
#define _pool_h_

class Pool {
  
 public:
  Pool( char *pname, int num, void* (*alloc_f)(void), void (*free_f)(void*) );
  ~Pool();

  void *palloc( void );		/* Get an object from pool */
  void pfree( void* );		/* Return object to pool */

 protected:
  void **pool;
  int next, max_index;
  int min_index;		/* Stats */
  char *name;			/*       */
  
  void* (*alloc_fn)();		/* How to alloc 1 object */
  void (*free_fn)( void* );	/* How to free 1 object */
};

#endif	/* _pool_h_ */
  
