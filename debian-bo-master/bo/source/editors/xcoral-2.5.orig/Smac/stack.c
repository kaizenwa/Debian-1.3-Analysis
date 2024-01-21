/* ########################################################################

				stack.c

   File: stack.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/stack.c
   Description: 
   Created: Tue Feb 21 13:01:27 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 13:01:28 MET 1995
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



#include "stack.h"
#include "mem.h"

Stack * Stack__Stack(size)
     unsigned size;
{
  Stack * this = (Stack *) Malloc(sizeof(Stack));
  
  this->Pointer = this->_memory =
      (Object *) RTCMalloc(size * sizeof(Object));
  this->End = this->_memory + size;

#if RUNTIMECHECK
  forbit_RTCfree(this->Pointer);
#endif

  return this;
}


Object Stack__Push(this, obj)
     Stack * this;
     Object obj;
{
  if (this->Pointer >= this->End)
    Error_Full_Stack();

  *this->Pointer++ = obj;

  return obj;
}

/* Pour les vecteur locaux, empile l'adresse du mot suivant en pile
   puis reserve de la place, ainsi l'evaluation d'une variable locale
   de type tableaux est indentique a l'evaluation d'une variable locale
   `normale' */

void Stack__ReserveStruct(this, size)
     Stack * this;
     int size;
{
  Object * pt = this->Pointer;
  
  if ((this->Pointer = pt + size + 1) >= this->End)
    Error_Full_Stack();
  *pt = (Object) (pt + 1);
}


/* vide la pile */

void Stack_Empty(this)
     Stack * this;
{
  this->Pointer = this->_memory;
}


/* La pile des variables locales */

unsigned Stack_Size = 1024;

Stack * Var_Stack;
Object * Frame_Pointer;


void Init_Stack()
{
  Var_Stack = Stack__Stack(Stack_Size);
}

void Reinit_Stack()
{
  Stack_Empty(Var_Stack);
}
