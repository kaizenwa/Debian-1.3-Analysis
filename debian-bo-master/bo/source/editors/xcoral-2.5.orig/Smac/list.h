/* ########################################################################

				 list.h

   File: list.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/list.h
   Description: 
   Created: Tue Feb 21 12:56:39 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:56:40 MET 1995
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



#ifndef _list_h
#define _list_h

#include "fctdecl.h"
#include "Instruction.h"

typedef struct _List {
  FCT(Instruction *, (*fct), (struct _List *));
  void * info;
  struct _List * previous;
  struct _List * next;
} List;

typedef FCT(Instruction *, (*InstrFct), (List *));
     
FCT(	List *, List__List,(InstrFct fct, void * info)		);
FCT(	List *, List__AddLast,(List * list, List * newcell)		);
FCT(	List *, List__InsertBefore,(List * list, List * newcell)	);
FCT(	List *, List__InsertAfter,(List * list, List * newcell)	);
FCT(	List *, List__PopLast,(List * list, List ** memolast)		);
FCT(	List *, List__Remove,(List * list, List * last)		);
     
#define List__Delete 		free
     
#define AddLast(x, y)		x = List__AddLast(x, y)
#define InsertAfter(x, y)	x = List__InsertAfter(x, y)
#define PopLast(x, y)		x = List__PopLast(x, (List **) &y)


extern FCT(int, list_length,(List *)	);

#define MAKE(x)			(((x)->fct)(x))
     
extern List * Memo;

#endif
