/*
   runlist.cc

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

#include "runlist.h"

RunList::RunList()
{
  list = new VLList();
}

RunList::~RunList()
{
  Runnable *o;

  while( list->num_items() )
	{
	  o = ((Runnable*)list->pop_head());
	  delete o;
	}
}

int RunList::add( Runnable *obj )
{
  list->add_tail( obj );
  return 0;
}

void RunList::draw()
{
  void *v;
  
  for( v=list->go_head(); v; v = list->go_next() )
	((Runnable*)v)->draw();
}

void RunList::erase()
{
  void *v;
  Runnable *obj;
  
  for( v=list->go_tail(); v; v = list->go_prev() )
	((Runnable*)v)->erase();

  v = list->go_head();

  while( v )
	{
	  obj = (Runnable*)v;

	  if ( !(obj->frames_left) )
		{
		  list->pop_cur_n();
		  delete obj;
		  v = list->go_head();
		}
	  else
		v = list->go_next();
	}

  for( v=list->go_head(); v; v = list->go_next() )
	{
	  obj = ((Runnable*)v);
	  --(obj->frames_left);
	}
}
