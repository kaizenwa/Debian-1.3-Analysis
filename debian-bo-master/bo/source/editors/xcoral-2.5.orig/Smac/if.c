/* ########################################################################

				  if.c

   File: if.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/if.c
   Description: 
   Created: Tue Feb 21 12:55:48 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:55:49 MET 1995
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



#include "If.h"
#include "list.h"

/* fct = make_if
   info = List ** [test, iftrue, ifalse / 0] */

If * make_if(l)
     List * l;
{
  List ** m = (List **) l->info;
  If * result;

  result = If__If(MAKE(m[0]), MAKE(m[1]), (m[2]) ? MAKE(m[2]) : 0);
		  
  free(l);
  free(m);
  
  return result;
}


/* fct = make_arith_if
   info = List ** [test iftrue ifalse] */

ArithIf * make_arith_if(l)
     List * l;
{
  List ** m = (List **) l->info;
  ArithIf * result;

  result = ArithIf__ArithIf(MAKE(m[0]), MAKE(m[1]), MAKE(m[2]));
		  
  free(l);
  free(m);
  
  return result;
}
