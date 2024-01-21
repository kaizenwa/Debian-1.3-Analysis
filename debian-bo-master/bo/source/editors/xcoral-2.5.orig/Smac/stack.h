/* ########################################################################

				stack.h

   File: stack.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/stack.h
   Description: 
   Created: Tue Feb 21 13:01:36 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 13:01:36 MET 1995
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



#ifndef _stack_h
#define _stack_h

#include "fctdecl.h"
#include "Object.h"
#include "error.h"

typedef struct {
  Object * Pointer;
  Object * End;

  Object * _memory;
} Stack;


FCT (Stack *, Stack__Stack,(unsigned)		);
FCT (Object, Stack__Push,(Stack *, Object)	);
FCT (void, Stack__ReserveStruct,(Stack *, int)	);
FCT (void, Stack_Empty,(Stack *)		);

#define Stack__Top(s)	(s->Pointer[-1])

     
/* La pile des variables locales */

extern Stack * Var_Stack;
extern Object * Frame_Pointer;

#define Stack_Pointer Var_Stack->Pointer
#define End_Stack Var_Stack->End
#define push_loc(x) Stack__Push(Var_Stack, x)
#define reserve_struct_loc(x) Stack__ReserveStruct(Var_Stack, x)

extern unsigned Stack_Size;

extern void Init_Stack();
extern void Reinit_Stack();
     
#endif
