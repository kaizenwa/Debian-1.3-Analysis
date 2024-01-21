/* ########################################################################

				  If.c

   File: If.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/If.c
   Description: 
   Created: Tue Feb 21 10:56:52 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:56:52 MET 1995
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
#include "stack.h"

/* if else */

static inherit_instruction_function_vector
  if_inherit_instruction_function_vector;

If * If__If(test, iftrue, iffalse)
     Instruction * test;
     Instruction * iftrue;
     Instruction * iffalse;
{
  If * this = (If *) Malloc(sizeof(If));

  Instruction_Constructor
    (if_inherit_instruction_function_vector, this, Type_Void);
       
  if (GetExprType(test) == Type_Void)
    Error("if (void) ..\n");
  else
    this->_test = test;
  
  this->_iftrue = iftrue;
  this->_else = iffalse;

  return this;
}


static Object If__Eval(this)
     If * this;
{
  if (Eval(this->_test)) {
    if (this->_iftrue)
      Eval(this->_iftrue);
  }
  else
    if (this->_else)
      Eval(this->_else);

  return 0;
}


/* ? : */

static inherit_instruction_function_vector
  arithif_inherit_instruction_function_vector;

ArithIf * ArithIf__ArithIf(test, iftrue, iffalse)
     Instruction * test;
     Instruction * iftrue;
     Instruction * iffalse;
{
  ArithIf * this = (ArithIf *) Malloc(sizeof(ArithIf));
  Type * truetype = GetExprType(iftrue);
  Type * falsetype = GetExprType(iffalse);
  ConvFct convert;
  Type * type;

  if (GetExprType(test) == Type_Void)
    Error("(void) ? .. : ..\n");
  else
    this->_test = test;

  this->_iftrue = iftrue;
  this->_else = iffalse;

  if (! (convert  = ExprType__Convertible(iftrue, falsetype)))
    if (! (convert  = ExprType__Convertible(iffalse, truetype)))
      error_incompatible_types(truetype, falsetype, "incompatible types in ?:");
    else
      type = truetype;
  else
    type = falsetype;

  if (convert != AlreadyConvertible) {
    /* il ne peut s'agir que de int et char, va vers int */
    if (truetype == Type_Char) {
      this->_converttrue = (FCT(Object, (*), (Object))) char2int;
      this->_convertfalse = 0;
    }
    else {
      this->_converttrue = 0;
      this->_convertfalse = (FCT(Object, (*), (Object))) char2int;
    }
    type = Type_Int;
  }
  else
    this->_converttrue = this->_convertfalse = 0;
      
  Instruction_Constructor
    (arithif_inherit_instruction_function_vector, this, type);
       
  return this;
}

static Object ArithIf__Eval(this)
     ArithIf * this;
{
  /* c'est le moment d'utiliser ? et : !! */
  return (Eval(this->_test))
    ? (this->_converttrue)
      ? (this->_converttrue)(Eval(this->_iftrue))
      : Eval(this->_iftrue)
    : (this->_convertfalse)
      ? (this->_convertfalse)(Eval(this->_else))
      : Eval(this->_else);
}

/**/

void Init_If()
{
  if_inherit_instruction_function_vector._Eval = (EvalFct) If__Eval;
  arithif_inherit_instruction_function_vector._Eval = (EvalFct) ArithIf__Eval;
}
