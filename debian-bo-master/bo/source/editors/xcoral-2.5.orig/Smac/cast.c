/* ########################################################################

				 cast.c

   File: cast.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/cast.c
   Description: 
   Created: Tue Feb 21 12:50:29 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:50:30 MET 1995
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



#include "Cast.h"
#include "list.h"
#include "Function.h"
#include "Var.h"

/*
   fct = make_cast
   info = List ** [Type * type, List * expr]
   */

Instruction * make_cast(l)
     List * l;
{
  Type * desiredtype = ((Type **) l->info)[0];
  Instruction * expr = MAKE(((List**) l->info)[1]);
  ConvFct convert = ExprType__Convertible(expr, desiredtype);

  free(l);
  
  if (convert != AlreadyConvertible)
    if (convert ||
	IsaFunction(expr) || IsaGlobalVar(expr) || IsaLocalVar(expr))
      return (Instruction *) Cast__Cast(expr, desiredtype, convert);
    else
      /* on fait comme si .. on peut faire un effet de bord sur l'objet */
      GetExprType(expr) = desiredtype;

  return expr;
}
