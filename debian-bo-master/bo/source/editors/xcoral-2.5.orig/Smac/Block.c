/* ########################################################################

				Block.c

   File: Block.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Block.c
   Description: 
   Created: Tue Feb 21 10:48:14 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:48:15 MET 1995
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



#include <stdio.h>

#include "Block.h"
#include "Type.h"
#include "stack.h"

static inherit_instruction_function_vector
  block_inherit_instruction_function_vector;

Block * Block__Block(nexpr, instrs)
     int nexpr;
     Instruction ** instrs;
{
  Block * this = (Block *) Malloc(sizeof(Block));

  Instruction_Constructor
    (block_inherit_instruction_function_vector, this, Type_Void);
  
  this->_nexpr = nexpr;
  this->_instrs = instrs;

  return this;
}

static Object Block__Eval(this)
     Block * this;
{
  Instruction ** pi = this->_instrs;
  Instruction ** pli = pi + this->_nexpr;
  Object * stackpointer = Stack_Pointer;

  {
#ifdef object_sur_plus_d_un_mot
    Object * stackp = Stack_Pointer;
#endif
    
    while (pi != pli) {
      Eval(*pi);
      pi += 1;
#ifdef object_sur_plus_d_un_mot
      Stack_Pointer = stackp;
#endif
    }
  }

  Stack_Pointer = stackpointer;
    
  return 0;
}

void Init_Block()
{
  block_inherit_instruction_function_vector._Eval = (EvalFct) Block__Eval;
}
