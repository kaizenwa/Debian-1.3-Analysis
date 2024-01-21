/* ########################################################################

				 For.c

   File: For.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/For.c
   Description: 
   Created: Tue Feb 21 10:55:07 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:55:08 MET 1995
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



#include "For.h"
#include "stack.h"
#include "control.h"

/* FOR */

static inherit_instruction_function_vector
  for_inherit_instruction_function_vector;

For * For__For(init, test, modif, body, hasbreakcont)
     Instruction * init;
     Instruction * test;
     Instruction * modif;
     Instruction * body;
     int hasbreakcont;
{
  For * this = (For *) Malloc(sizeof(For));

  Instruction_Constructor
    (for_inherit_instruction_function_vector, this, Type_Void);

  this->_init = init;
  if (test && (GetExprType(test) == Type_Void))
    Error("for (..; void; ..) ..\n");
  else
    this->_test = test;
  this->_body = body;
  this->_modif = modif;
  this->_hasbreakcont = hasbreakcont;

  return this;
}


static Object For__Eval(this)
     For * this;
{
  Instruction * test = this->_test;
  Instruction * modif = this->_modif;
  Instruction * body = this->_body;

  if (this->_init)
    Eval(this->_init);
  
  if (body)
    if (this->_hasbreakcont) {
      Object * stackpointer = Stack_Pointer;
      jmp_buf save_env;
      
      COPY_ENV(save_env, current_env);
      while ((! test) || Eval(test)) {
	switch (setjmp(current_env)) {
	case NO_JMP :
	  Eval(body);
	  break;
	case JMP_RETURN:
	  longjmp(save_env, JMP_RETURN);
	case JMP_CONTINUE:
	  Stack_Pointer = stackpointer;
	  goto etiq_cont;
	case JMP_BREAK:
	  Stack_Pointer = stackpointer;
	  goto etiq_break;
	case JMP_ERROR :
	  longjmp(save_env, JMP_ERROR);
	}
      etiq_cont: ;
	COPY_ENV(current_env, save_env);	/* pour modif test */
	if (modif) Eval(modif);
      }
    etiq_break:
      COPY_ENV(current_env, save_env);
    }
    else if (test)
      if (modif)
	for (; Eval(test); Eval(modif)) Eval(body);
      else
	for (; Eval(test);) Eval(body);
    else if (modif)
      for (;; Eval(modif)) Eval(body);
    else
      for (;;) Eval(body);
  
  else if (! test)
    Error("for without test and body, I think it is too dangerous !");
  else if (modif)
    while(Eval(test)) Eval(modif);
  else
    while(Eval(test));

  return 0;
}

void Init_For()
{
  for_inherit_instruction_function_vector._Eval = (EvalFct) For__Eval;
}
