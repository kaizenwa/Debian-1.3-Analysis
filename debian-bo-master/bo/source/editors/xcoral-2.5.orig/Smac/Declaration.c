/* ########################################################################

			     Declaration.c

   File: Declaration.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Declaration.c
   Description: 
   Created: Tue Feb 21 10:53:38 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:53:39 MET 1995
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



#include "Declaration.h"
#include "stack.h"

static inherit_instruction_function_vector
  declaration_inherit_instruction_function_vector;

Declaration * Declaration__Declaration(initval, convert, sizeinword)
     Instruction * initval;
     FCT( Object,(*convert),(Object));
     int sizeinword;
{
  Declaration * this = (Declaration *) Malloc(sizeof(Declaration));

  Instruction_Constructor
    (declaration_inherit_instruction_function_vector, this, Type_Void);

  this->_init_value = initval;
  this->_convert = convert;
  this->_sizeinword = sizeinword;

  return this;
}

static Object Declaration__Eval(this)
     Declaration * this;
{
  if (this->_sizeinword)
    reserve_struct_loc(this->_sizeinword);
  else
    push_loc((this->_init_value)
	     ? (this->_convert)
	       ? (this->_convert)(Eval(this->_init_value))
	       : Eval(this->_init_value)
	     : 0);

  return 0;
}

void Init_Declaration()
{
  declaration_inherit_instruction_function_vector._Eval =
    (EvalFct) Declaration__Eval;
}
