/* ########################################################################

				while.c

   File: while.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/while.c
   Description: 
   Created: Tue Feb 21 13:02:55 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 13:02:55 MET 1995
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



#include "While.h"
#include "list.h"
#include "control.h"
#include "comma.h"

/*
  fct = make_while
  info = [test, body]
*/

While * make_while(l)
     List * l;
{
  List ** m = (List **) l->info;
  Instruction * test = MAKE(m[0]);
  int prev_in_iter = in_iteration;
  int prev_in_switch = in_switch;
  Instruction * body;
  While * result;

  in_iteration = 1;
  in_switch = 0;
                  
  body = (m[1]->fct == (InstrFct) no_comma_expression) ? 0 : MAKE(m[1]);
  /* ne pas mettre le MAKE dans While__While car il peut modifier in_iteration
     et l'ordre d'execution des arguments est quelconque */
  result = While__While(test, body, in_iteration != 1);
  
  in_iteration = prev_in_iter;
  in_switch = prev_in_switch;
  
  free(l);
  free(m);

  return result;
}

/*
  fct = make_while
  info = [body, test]
*/

While * make_do_while(l)
     List * l;
{
  List ** m = (List **) l->info;
  int prev_in_iter = in_iteration;
  int prev_in_switch = in_switch;
  Instruction * test = MAKE(m[1]);
  Instruction * body;
  While * result;

  in_iteration = 1;
  in_switch = 0;
  
  body = (m[0]) ? MAKE(m[0]) : 0;
  /* ne pas mettre le MAKE dans DoWhile__DoWhile car il peut modifier
     in_iteration et l'ordre d'execution des arguments est quelconque */
  result = DoWhile__DoWhile(test, body, in_iteration != 1);
  
  in_iteration = prev_in_iter;
  in_switch = prev_in_switch;
                  
  free(l);
  free(m);
  
  return result;
}

