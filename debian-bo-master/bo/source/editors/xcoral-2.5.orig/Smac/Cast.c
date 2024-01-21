/* ########################################################################

				 Cast.c

   File: Cast.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Cast.c
   Description: 
   Created: Tue Feb 21 10:49:27 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:49:28 MET 1995
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

/* Cast

   Utilise lorsqu'il y a une fonction de conversion
   */

static inherit_instruction_function_vector
  cast_inherit_instruction_function_vector;

Cast * Cast__Cast(expr, desiredtype, convert)
     Instruction * expr;
     Type * desiredtype;
     ConvFct convert;
{
  Cast * this = (Cast *) Malloc(sizeof(Cast));

  Instruction_Constructor
    (cast_inherit_instruction_function_vector, this, desiredtype);

  this->_expr = expr;
  this->_convert = (convert == AlreadyConvertible) ? 0 : convert;

  return this;
}

static Object Cast__Eval(this)     
     Cast * this;
{
  return  (this->_convert)
    ? (this->_convert)(Eval(this->_expr))
    : /* il s'agit d'un cast sur une fonction ou variable pour lesquels
	 il ne faut pas faire d'effet de bord sur le type memorise (voir
	 cast.c) on rend simplement la valeur */
      Eval(this->_expr);
}



void Init_Cast()
{
  cast_inherit_instruction_function_vector._Eval = (EvalFct) Cast__Eval;
}
