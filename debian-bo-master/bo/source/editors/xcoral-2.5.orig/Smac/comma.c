/* ########################################################################

				comma.c

   File: comma.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/comma.c
   Description: 
   Created: Tue Feb 21 12:50:58 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:50:59 MET 1995
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



#include "Comma.h"
#include "list.h"

/*
  fct = no_comma_expression
  info = 0
  */

Instruction * no_comma_expression(l)
   List * l;
{
  free(l);

  return (Instruction *) CommaExpression__CommaExpression(0, 0, 0);
}


/*
  fct = make_comma
  info = (char **) [nbre_elt, elt1, ... eltn] avec elti : List *, nbre_elt > 0
*/

Instruction * make_comma(l)
     List * l;
{
  int ndisc = (int) ((char **) l->info)[0] - 1;
  Instruction ** discarded =
    (Instruction **) Malloc(ndisc * sizeof(Instruction *));
  List ** p = ((List **) l->info) + 1;
  Instruction * result;
  int index;

  for (index = 0; index != ndisc; index += 1)
    discarded[index] = MAKE(p[index]);

  result = (Instruction *)
    CommaExpression__CommaExpression(discarded, MAKE(p[index]), ndisc);
  
  free(l->info);
  free(l);

  return result;
}
