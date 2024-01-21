/* ########################################################################

				While.c

   File: While.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/While.c
   Description: 
   Created: Tue Feb 21 12:47:04 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:47:05 MET 1995
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



#include "While.h"
#include "stack.h"
#include "control.h"

/* WHILE */

static inherit_instruction_function_vector
  while_inherit_instruction_function_vector;

While * While__While(test, body, hasbreakcont)
     Instruction * test;
     Instruction * body;
     int hasbreakcont;
{
  While * this = (While *) Malloc(sizeof(While));

  Instruction_Constructor
    (while_inherit_instruction_function_vector, this, Type_Void);
       
  if (GetExprType(test) == Type_Void)
    Error("while (void) ..\n");
  else
    this->_test = test;
  
  this->_body = body;
  this->_hasbreakcont = hasbreakcont;

  return this;
}


static Object While__Eval(this)
     While * this;
{
  Instruction * test = this->_test;
  Instruction * body = this->_body;
  
  if (body)
    if (this->_hasbreakcont) {
      Object * stackpointer = Stack_Pointer;
      jmp_buf save_env;
      
      COPY_ENV(save_env, current_env);
      while(Eval(test)) {
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
	COPY_ENV(current_env, save_env);	/* pour le test */
      }
    etiq_break:
      COPY_ENV(current_env, save_env);
    }
    else
      while(Eval(test)) Eval(body);
  else
    while(Eval(test)) ;

  return 0;
}


/* DO WHILE */

static inherit_instruction_function_vector
  dowhile_inherit_instruction_function_vector;

While * DoWhile__DoWhile(test, body, hasbreakcont)
     Instruction * test;
     Instruction * body;
     int hasbreakcont;
{
  While * this = (While *) Malloc(sizeof(While));

  Instruction_Constructor
    (dowhile_inherit_instruction_function_vector, this, Type_Void);
       
  if (GetExprType(test) == Type_Void)
    Error("do .. while(void)\n");
  else
    this->_test = test;
  
  this->_body = body;
  this->_hasbreakcont = hasbreakcont;

  return this;
}


static Object DoWhile__Eval(this)
     While * this;
{
  Instruction * test = this->_test;
  Instruction * body = this->_body;
  
  if (body)
    if (this->_hasbreakcont) {
      Object * stackpointer = Stack_Pointer;
      jmp_buf save_env;
      
      COPY_ENV(save_env, current_env);
      do {
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
      etiq_cont: 
	COPY_ENV(current_env, save_env);	/* pour le test */
      } while (test);
    etiq_break:
      COPY_ENV(current_env, save_env);
    }
    else
      do Eval(body); while(Eval(test));
  else
    while(Eval(test)) ;

  return 0;
}


void Init_While()
{
  while_inherit_instruction_function_vector._Eval = (EvalFct) While__Eval;
  dowhile_inherit_instruction_function_vector._Eval = (EvalFct) DoWhile__Eval;
}
