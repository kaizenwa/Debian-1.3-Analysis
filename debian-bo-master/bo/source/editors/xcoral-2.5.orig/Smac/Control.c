/* ########################################################################

			       Control.c

   File: Control.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Control.c
   Description: 
   Created: Tue Feb 21 10:53:05 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:53:05 MET 1995
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



#include "Control.h"

static inherit_instruction_function_vector
  control_inherit_instruction_function_vector;

Control * Control__Control(type)
     JMP_TYPE type;
{
  Control * this = (Control *) Malloc(sizeof(Control));

  Instruction_Constructor
    (control_inherit_instruction_function_vector, this, Type_Void);

  this->_type = type;

  return this;
}

static Object Control__Eval(this)
     Control * this;
{
  longjmp(current_env, this->_type);
}

void Init_Control()
{
  control_inherit_instruction_function_vector._Eval = (EvalFct) Control__Eval;
}
