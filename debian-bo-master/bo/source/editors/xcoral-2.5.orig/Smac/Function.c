/* ########################################################################

			       Function.c

   File: Function.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Function.c
   Description: 
   Created: Tue Feb 21 10:55:35 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:55:36 MET 1995
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

#include "Function.h"
#include "error.h"

static inherit_instruction_function_vector
  function_inherit_instruction_function_vector;

Function * Function__Function(nom, valtype, narg, argtypes, def)
     char * nom;
     Type * valtype;
     int narg;
     Type ** argtypes;
     Object def;
{
  Function * this = (Function *) Malloc(sizeof(Function));
  TypeFunction * typefunction = (TypeFunction *) Malloc(sizeof(TypeFunction));
  Type * thetype = Type__Type(typefunction, T_Function, sizeof(void **));
  
  Const_Constructor
    (function_inherit_instruction_function_vector, this, thetype);

  this->_name = nom;
  this->_valtype = typefunction->_valtype = valtype;
  this->_nbre_arg = typefunction->_nbre_arg = narg;
  this->_argtypes = typefunction->_argtypes = argtypes;

  if (def) {
    this->_def._builtin_def = (FCT(Object, (*), (Instruction **))) def;
    this->_masks = 0;
  }
  else {
    /* Fonction utilisateur */
    Object * masques = (narg)
      ? (Object *) Malloc(narg * sizeof(Object))
      : (Object *) Malloc(1);
    int index;
    
    this->_def._user_def =
      (Instruction *) UndefinedFunctionCall__UndefinedFunctionCall(this);
    this->_masks = masques;
    for (index = 0; index != narg; index += 1)
      masques[index] = Type__Mask(argtypes[index]);    
  }
  
  return this;
}

static Object Function__Eval(this)
     Function * this;
{
  return (Object) this;
}

void Function__UpdateDef(this, true_def)
     Function * this;
     Instruction *true_def;
{
  free(this->_def._user_def);
  this->_def._user_def = true_def;
}


/* Verifie que la fonction n'est pas en cours d'execution */

void Function__VerifyNotActive(this)
    Function * this;
{
  memo_functions_called * fc;

  for (fc = Last_Function_Called; fc; fc = fc->previous)
    if (fc->last_function_called == (Object) this)
      Error("you cannot redefine or undefine an active function");
}


void Init_Function()
{
  function_inherit_instruction_function_vector._Eval =
    (EvalFct) Function__Eval;
}

/* Execution du corps d'une fonction declaree mais pas definie */

static inherit_instruction_function_vector
  undefinedfunctioncall_inherit_instruction_function_vector;

UndefinedFunctionCall *
  UndefinedFunctionCall__UndefinedFunctionCall(func)
    Function * func;
{
  UndefinedFunctionCall * this =
    (UndefinedFunctionCall *) Malloc(sizeof(UndefinedFunctionCall));
  
  Const_Constructor
    (undefinedfunctioncall_inherit_instruction_function_vector,
     this, Function__ValType(func));

  this->_func = func;

  return this;
}

static Object UndefinedFunctionCall__Eval(this)
     UndefinedFunctionCall * this;
{
  sprintf(err_msg, "function %s not yet defined\n",
	  Function__Name(this->_func));
  Error(err_msg);

  return 0;			/* pour le compilo */
}

int Function__IsUndefined(f)
     Function * f;
{
 return (f->_def._user_def->instruction_functions ==
	 &undefinedfunctioncall_inherit_instruction_function_vector);
}

void Init_UndefinedFunctionCall()
{
  undefinedfunctioncall_inherit_instruction_function_vector._Eval =
    (EvalFct) UndefinedFunctionCall__Eval;
}


int IsaFunction(i)
     Instruction * i;
{
  return ((i->instruction_functions ==
	   &function_inherit_instruction_function_vector) ||
	  (i->instruction_functions ==
	   &undefinedfunctioncall_inherit_instruction_function_vector));
}


/* Pour des verifications a l'execution */

int IsaPointerToFunction(f)
     void * f;
{
  return(
#ifdef RUNTIMECHECK
	 (((char *) f < Address_Min) ||
	  ((char *) f > Address_Pointer_Max)) 		&&
#endif
#if CHECK_POINTER_POINTER
	 (! ((Object) f & CHECK_POINTER_POINTER)) 	&&
#endif
	 IsaFunction((Instruction *) f)
	 );
}
