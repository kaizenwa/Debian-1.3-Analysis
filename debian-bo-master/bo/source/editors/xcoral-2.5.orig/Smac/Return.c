/* ########################################################################

				Return.c

   File: Return.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Return.c
   Description: 
   Created: Tue Feb 21 12:45:06 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:45:07 MET 1995
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
#include "error.h"
#include "function.h"
#include "control.h"

static inherit_instruction_function_vector
  return_inherit_instruction_function_vector;

Return * Return__Return(value)
     Instruction * value;
{
  Return * this = (Return *) Malloc(sizeof(Return));
  Type * type = (value) ? GetExprType(value) : Type_Void;
  ConvFct convert;

  if (! Function_Name)
    Error("return at top level");
  if (! value)
    this->_convert = 0;
  else if (! (convert = ExprType__Convertible(value, Return_Value_Type)))
    Error(  (type == Type_Void)
	  ? "the function return value"
	  : (Return_Value_Type == Type_Void)
	  ? "the function return no value"
	  : "incompatible return type");
  else if (convert != AlreadyConvertible)
    this->_convert = convert;
  else
    this->_convert = 0;
      
  Instruction_Constructor
    (return_inherit_instruction_function_vector, this, type);

  this->_value = value;

  return this;
}

static Object Return__Eval(this)
     Return * this;
{
  if (this->_value)
    if (this->_convert)
      current_result = (this->_convert)(Eval(this->_value));
    else
      current_result = Eval(this->_value);

  longjmp(current_env, JMP_RETURN);
}

void Init_Return()
{
  return_inherit_instruction_function_vector._Eval = (EvalFct) Return__Eval;
}
