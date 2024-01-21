/* ########################################################################

				Comma.c

   File: Comma.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Comma.c
   Description: 
   Created: Tue Feb 21 10:49:52 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:49:52 MET 1995
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
#include "stack.h"

static inherit_instruction_function_vector
  commaexpression_inherit_instruction_function_vector;

CommaExpression * CommaExpression__CommaExpression(discarded, last, ndisc)
     Instruction ** discarded;
     Instruction * last;
     int ndisc;
{
  CommaExpression * this = (CommaExpression *) Malloc(sizeof(CommaExpression));
  
  Instruction_Constructor
    (commaexpression_inherit_instruction_function_vector,
     this, (last) ? GetExprType(last) : Type_Void);
       
  this->_discarded = discarded;
  this->_last = last;
  this->_n_discarded = ndisc;

  return this;
}

static Object CommaExpression__Eval(this)
     CommaExpression * this;
{
  if (this->_last) {
    Instruction ** p = this->_discarded;
    Instruction ** l = p + this->_n_discarded;
#ifdef object_sur_plus_d_un_mot
    Object * stackpointer = Stack_Pointer;
#endif
    
    while (p != l) {
      Eval(*p);
      p += 1;
#ifdef object_sur_plus_d_un_mot
      Stack_Pointer = stackpointer;
#endif
    }
    
    return Eval(this->_last);
  }
  else
    return 0;
}

void Init_CommaExpression()
{
  commaexpression_inherit_instruction_function_vector._Eval =
    (EvalFct) CommaExpression__Eval;
}
