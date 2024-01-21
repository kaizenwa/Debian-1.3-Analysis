/* ########################################################################

				Const.h

   File: Const.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Const.h
   Description: 
   Created: Tue Feb 21 10:52:48 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:52:49 MET 1995
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



#ifndef _Const_h
#define _Const_h

#include "Instruction.h"

/* Const : type pure non terminal */

#define Inherit_Const Inherit_Instruction		/* rien de neuf */

#define inherit_const_function_vector	inherit_instruction_function_vector
  
typedef struct _Const {
  Inherit_Instruction;
} Const;
  

/* Int : type terminal */

typedef struct {
  Inherit_Const;

  int _value;
} Int;

extern FCT( Int *, Int__Int,(int) );
     
void Init_Int();


/* Char : type terminal */

typedef struct {
  Inherit_Const;

  char _value;
} Char;

extern FCT( Char *, Char__Char,(int) );
     
void Init_Char();

     
/* Char * : type terminal */

typedef struct {
  Inherit_Const;

  char * _value;
} String;

extern FCT( String *, String__String,(char *) );
     
void Init_String();

#endif

