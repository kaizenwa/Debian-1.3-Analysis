/* ########################################################################

				 for.c

   File: for.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/for.c
   Description: 
   Created: Tue Feb 21 12:53:43 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:53:43 MET 1995
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



#include "For.h"
#include "list.h"
#include "control.h"
#include "comma.h"

/*
  fct = make_for
  info = [init, test, modif, body]
*/

For * make_for(l)
     List * l;
{
  List ** m = (List **) l->info;
  Instruction * init = (m[0]->fct == no_comma_expression) ? 0 : MAKE(m[0]);
  Instruction * test = (m[1]->fct == no_comma_expression) ? 0 : MAKE(m[1]);
  Instruction * modif = (m[2]->fct == no_comma_expression) ? 0 : MAKE(m[2]);
  int prev_in_iter = in_iteration;
  int prev_in_switch = in_switch;
  Instruction * body;
  For * result;

  in_iteration = 1;
  in_switch = 0;
  
  body = (m[3]->fct == no_comma_expression) ? 0 : MAKE(m[3]);
  /* ne pas mettre le MAKE dans For__For car il peut modifier in_iteration
     et l'ordre d'execution des arguments est quelconque */
  result = For__For(init, test, modif, body, in_iteration != 1);
  
  in_iteration = prev_in_iter;
  in_switch = prev_in_switch;
  
  free(l);
  free(m);

  return result;
}
