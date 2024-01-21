/* ########################################################################

				return.c

   File: return.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/return.c
   Description: 
   Created: Tue Feb 21 12:59:56 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:59:57 MET 1995
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



#include "Return.h"
#include "list.h"
#include "decl.h"
#include "comma.h"

/*
  fct = make_return
  info = List * val, peut etre no_comma_expression
*/

Instruction * make_return(l)
     List * l;
{
  List * v = (List *) l->info;

  free(l);

  if (v->fct == no_comma_expression)
    /* return; */
    return (Instruction *) Return__Return(0);
  else
    /* return val; */
    return (Instruction *) Return__Return(MAKE(v));
}
