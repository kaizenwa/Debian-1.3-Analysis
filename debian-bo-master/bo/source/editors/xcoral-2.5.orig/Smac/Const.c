/* ########################################################################

				Const.c

   File: Const.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Const.c
   Description: 
   Created: Tue Feb 21 10:52:26 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:52:27 MET 1995
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

#include "Const.h"
#include "Type.h"


/* Int */

static inherit_const_function_vector int_inherit_const_function_vector;

Int * Int__Int(value)
     int value;
{
  Int * this = (Int *) Malloc(sizeof(Int));

  Const_Constructor(int_inherit_const_function_vector, this, Type_Int);
  
  this->_value = value;

  return this;
}
  
static Object Int__Eval(this)
     Int * this;
{
  return this->_value;
}

void Init_Int()
{
  int_inherit_const_function_vector._Eval = (EvalFct) Int__Eval;
}


/* Char */

static inherit_const_function_vector char_inherit_const_function_vector;

Char * Char__Char(value)
     int value;
{
  Char * this = (Char *) Malloc(sizeof(Char));

  Const_Constructor(char_inherit_const_function_vector, this, Type_Char);
  
  this->_value = (char) value;

  return this;
}
  
static Object Char__Eval(this)
     Char * this;
{
  return this->_value;
}

void Init_Char()
{
  char_inherit_const_function_vector._Eval = (EvalFct) Char__Eval;
}


/* String */

static inherit_const_function_vector string_inherit_const_function_vector;

String * String__String(value)
     char * value;
{
  String * this = (String *) Malloc(sizeof(String));

  Const_Constructor(string_inherit_const_function_vector, this, Type_String);
  
  this->_value = value;

  return this;
}
  
static Object String__Eval(this)
     String * this;
{
  return (Object) this->_value;
}

void Init_String()
{
  string_inherit_const_function_vector._Eval = (EvalFct) String__Eval;
}
  
