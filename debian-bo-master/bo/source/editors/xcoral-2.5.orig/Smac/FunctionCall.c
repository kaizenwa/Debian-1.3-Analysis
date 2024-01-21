/* ########################################################################

			     FunctionCall.c

   File: FunctionCall.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/FunctionCall.c
   Description: 
   Created: Tue Feb 21 10:55:59 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:56:00 MET 1995
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
#include <string.h>

#include "FunctionCall.h"
#include "funcall.h"
#include "stack.h"
#include "control.h"

/***********************************

  L'appel d'une fonction utilisateur

  **********************************/

static inherit_instruction_function_vector
  functioncall_inherit_instruction_function_vector;

FunctionCall * FunctionCall__FunctionCall(func, args, narg)
     Function * func;
     Instruction ** args;
     int narg;
{
  FunctionCall * this = (FunctionCall *) Malloc(sizeof(FunctionCall));
  
  Instruction_Constructor
    (functioncall_inherit_instruction_function_vector, this,
     Function__ValType(func));
       
  this->_function = func;
  this->_narg = narg;
  this->_args = args;

  return this;
}

static Object funcall_user_function(currentarg, narg, function)
     Instruction ** currentarg;
     int narg;
     Function * function;
{
  /* Pourrait optimiser en verifiant une fois pour toute s'il y a
     assez de place en pile (les tests sont fait par les push) */
  
  Object * previousframe = Frame_Pointer;

  {
    Instruction ** postarg = currentarg + narg;
    Object * masques = function->_masks;
    Object * nextframe = Stack_Pointer;
    
    while (currentarg != postarg) {
      /* Le resultat tient dans un mot */
      push_loc(Eval(*currentarg) & *masques++);
      currentarg += 1;
    }

    Frame_Pointer = nextframe;
  }

  {
    memo_functions_called previous_function_called;
    jmp_buf save_env;

    previous_function_called.previous = Last_Function_Called;
    previous_function_called.last_function_called = (Object) function;
    Last_Function_Called = &previous_function_called;
      
    COPY_ENV(save_env, current_env);
    
    switch (setjmp(current_env)) {
    case NO_JMP :
      Eval(Function__UserDef(function));
      break;
    case JMP_ERROR :
      longjmp(save_env, JMP_ERROR);
    }

    Last_Function_Called = previous_function_called.previous;
    COPY_ENV(current_env, save_env);
  }
  
  Stack_Pointer = Frame_Pointer;
  Frame_Pointer = previousframe;

  return current_result;
}


static Object FunctionCall__Eval(this)
     FunctionCall * this;
{
  return funcall_user_function(this->_args, this->_narg, this->_function);
}


#ifdef XCORAL

/* Appel d'une fonction utilisateur ne pouvant prendre que des int ou char
   en arg, a seulement verifier que fct est bien une fonc utilisateur */

void call_user_function_from_xcoral(function, narg, args)
     Function * function;
     int narg;
     int * args;
{
  if (Function__NbreArg(function) != narg)
    sprintf(err_msg, "%d arguments for %s, %d required",
	    narg, Function__Name(function), Function__NbreArg(function));
  else {
    /* Pourrait optimiser en verifiant une fois pour toute s'il y a
       assez de place en pile (les tests sont fait par les push) */
    
    Object * previousframe = Frame_Pointer;
    
    {
      Object * nextframe = Stack_Pointer;
      
      while (narg--) push_loc(*args++);
      
      Frame_Pointer = nextframe;
    }
    
    {
      memo_functions_called previous_function_called;
      jmp_buf save_env;
      
      previous_function_called.previous = Last_Function_Called;
      previous_function_called.last_function_called = (Object) function;
      Last_Function_Called = &previous_function_called;
      
      COPY_ENV(save_env, current_env);
      
      switch (setjmp(current_env)) {
      case NO_JMP :
	Eval(Function__UserDef(function));
	break;
      case JMP_ERROR :
	longjmp(save_env, JMP_ERROR);
      }
      
      Last_Function_Called = previous_function_called.previous;
      COPY_ENV(current_env, save_env);
    }
    
    Stack_Pointer = Frame_Pointer;
    Frame_Pointer = previousframe;
  }
}

#endif

/***********************************

  L'appel des fonctions pre-definies

  **********************************/

static inherit_instruction_function_vector
  builtinfunctioncall_inherit_instruction_function_vector;

FunctionCall *
  BuiltinFunctionCall__BuiltinFunctionCall(func, args, narg, type)
     Function * func;
     Instruction ** args;
     int narg;
     Type * type;
{
  FunctionCall * this = FunctionCall__FunctionCall(func, args, narg);

  /* Change le vecteur des fonctions `virtuelles' */
  
  Instruction_Constructor
    (builtinfunctioncall_inherit_instruction_function_vector, this, type);

  return this;  
}


static Object BuiltinFunctionCall__Eval(this)
     FunctionCall * this;
{
  /* Chaque fonction builtin se charge de l'evaluation de ses arguments */

  return (*(Function__BuiltinDef(this->_function)))(this->_args);
}


#ifdef XCORAL

/* Une fonction d'Xcoral appelee a cause d'une liaison de clef, il ne doit
   pas y avoir d'argument fournis, elle ne doit pas en attendre.
*/

#include "Const.h"

void call_builtin_function_of_xcoral(function, narg)
     Function * function;
     int narg;
{
  if (narg || Function__NbreArg(function)) {
    sprintf(err_msg, "illegal fonction call\n");
    return;
  }
      
  Function_Name = 0;
  Last_Function_Called = 0;
      
  (*(Function__BuiltinDef(function)))((Instruction **) 0);
}

#endif

/********************************

  L'appel d'une fonction calculee

  *******************************/

static inherit_instruction_function_vector
  calculatefunctioncall_inherit_instruction_function_vector;

CalculateFunctionCall *
  CalculateFunctionCall__CalculateFunctionCall(func, args, narg)
     Instruction * func;
     Instruction ** args;
     int narg;
{
  CalculateFunctionCall * this =
    (CalculateFunctionCall *) Malloc(sizeof(CalculateFunctionCall));
  TypeFunction * ftype = Type__Function(GetExprType(func));

  Instruction_Constructor
    (calculatefunctioncall_inherit_instruction_function_vector, this,
     TypeFunction__ValType(ftype));

  this->_function = func;
  this->_narg = narg;
  this->_args = args;
  
  return this;  
}


static Object CalculateFunctionCall__Eval(this)
     CalculateFunctionCall * this;
{
  Function * fct = (Function *) Eval(this->_function);

#ifdef RUNTIMECHECK
  if (! IsaPointerToFunction(fct))
    Error("computed function is not a function");
#endif
  
  if (Function__IsBuiltin(fct)) {

    /* Fonction pre-definie */
    
#ifdef RUNTIMECHECK
    if (bad_number_of_args(fct, this->_narg)) {
      sprintf(err_msg, "%d arguments for %s, %d required",
	      this->_narg, Function__Name(fct), Function__NbreArg(fct));
      Error(err_msg);
    }
#endif
    
    /* Chaque fonction builtin se charge de l'evaluation de ses arguments */

    return (*(Function__BuiltinDef(fct)))(this->_args);
  }
  else {
    
    /* Fonction utilisateur */

#ifdef RUNTIMECHECK
    if (Function__NbreArg(fct) != this->_narg) {
      sprintf(err_msg, "%d arguments for %s, %d required",
	      this->_narg, Function__Name(fct), Function__NbreArg(fct));
      Error(err_msg);
    }
#endif

    return funcall_user_function(this->_args, this->_narg, fct);
  }
}


#if defined(RUNTIMECHECK) || defined(XCORAL)

/* Pseudo argument place en dernier pour sortir en erreur s'il est evalue,
   utilise pour les fonctions n'ayant pas un nombre fixe d'argument */

static inherit_instruction_function_vector
  notenougharg_inherit_instruction_function_vector;

NotEnoughArg * NotEnoughArg__NotEnoughArg(func, narg)
     Function * func;
     int narg;
{
  NotEnoughArg * this = (NotEnoughArg *) Malloc(sizeof(NotEnoughArg));

  Instruction_Constructor
    (notenougharg_inherit_instruction_function_vector, this, Type_Void);
       
  this->_function = func;
  this->_narg = narg;

  return this;  
}


static Object NotEnoughArg__Eval(this)
     NotEnoughArg * this;
{
  sprintf(err_msg, "not enough number of argument for %s (%d given)",
	  Function__Name(this->_function), this->_narg);
  Error(err_msg);

  return 0;						/* pour le compilo */
}

int IsaNotEnoughArg(i)
     Instruction * i;
{
  return (i->instruction_functions ==
          &notenougharg_inherit_instruction_function_vector);
}

#endif
  
/***/

void Init_FunctionCall()
{
  functioncall_inherit_instruction_function_vector._Eval =
    (EvalFct) FunctionCall__Eval;
  builtinfunctioncall_inherit_instruction_function_vector._Eval =
    (EvalFct) BuiltinFunctionCall__Eval;
  calculatefunctioncall_inherit_instruction_function_vector._Eval =
    (EvalFct) CalculateFunctionCall__Eval;
#ifdef RUNTIMECHECK
  notenougharg_inherit_instruction_function_vector._Eval =
    (EvalFct) NotEnoughArg__Eval;
#endif
}
