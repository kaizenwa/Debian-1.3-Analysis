/* ########################################################################

				 hash.h

   File: hash.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/hash.h
   Description: 
   Created: Tue Feb 21 12:54:49 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:54:49 MET 1995
   Last maintained by: Bruno Pages

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Bruno Pages

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

   ######################################################################## */



#ifndef _hash_h
#define _hash_h

#include "fctdecl.h"
#include "word.h"

typedef struct _HashItem {
  Object _key;
  Object _data;
  struct _HashItem * _next;
} HashItem;

extern FCT(HashItem *, HashItem__HashItem,
	   (Object key, Object data, HashItem * next));

#define HashItem__Key(x)	(x)->_key
#define HashItem__Data(x)	(x)->_data
    
#define HashItem__Delete	free

typedef struct {
  HashItem ** _table;
  unsigned _n_entries;
  unsigned _size;
  FCT (unsigned int, (*_hfct), (Object));
  FCT (int, (*_cfct), (Object, Object));
} HashTable;

extern FCT(void, HashTable_Iter,(HashTable *, HashItem **, HashItem ***) );
     
extern FCT( HashTable *, HashTable__HashTable,(unsigned size, int str)   );
extern FCT( Object, HashTable__Search, (HashTable *, Object)		 );
extern FCT( void, HashTable__Add, (HashTable *, Object, Object)		 );
extern FCT( Object, HashTable__Remove, (HashTable *, Object)		 );
extern FCT( void, HashTable__Empty, (HashTable *)			 );

#endif
