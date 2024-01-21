/* ########################################################################

				 Var.c

   File: Var.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Var.c
   Description: 
   Created: Tue Feb 21 12:46:39 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:46:40 MET 1995
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

#include "Var.h"
#include "stack.h"

#ifdef RUNTIMECHECK
#include "mem.h"
#endif

/* Les variables locales */

inherit_instruction_function_vector
  local_var_inherit_instruction_function_vector;

LocalVar * LocalVar__LocalVar(nom, position, t)
     char * nom;
     int position;
     Type * t;
{
  LocalVar * this = (LocalVar *) Malloc(sizeof(LocalVar));
  
  Instruction_Constructor
    (local_var_inherit_instruction_function_vector, this, t);
  
  this->_name = nom;
  this->_position = position;

  return this;
}

Object LocalVar__Eval(this)
     LocalVar * this;
{
  return Frame_Pointer[this->_position];
}


/* affectation d'une variable locale */

inherit_instruction_function_vector
  local_var_assignment_inherit_instruction_function_vector;

LocalVarAssignment * LocalVarAssignment__LocalVarAssignment(ident, value, cast)
     LocalVar * ident;
     Instruction * value;
     Type * cast;
{
  LocalVarAssignment * this =
    (LocalVarAssignment *) Malloc(sizeof(LocalVarAssignment));

  if (! cast) cast = GetExprType(ident);
  
  Instruction_Constructor
    (local_var_assignment_inherit_instruction_function_vector, this, cast);
  
  this->_position = ident->_position;
  this->_value = value;
  if ((this->_convert =
       (FCT( Object,(*),(Object))) ExprType__Convertible(value, cast))
      == (FCT( Object,(*),(Object))) Type__Convertible)
    this->_convert = 0;
  else if (! this->_convert)
    error_incompatible_types(cast, GetExprType(value), "incompatible type in assignment");

  return this;
}

Object LocalVarAssignment__Eval(this)
     LocalVarAssignment * this;
{
  return Frame_Pointer[this->_position] =
    	 (this->_convert) ? (this->_convert) (Eval(this->_value))
			  : Eval(this->_value);
}

/* post incr/decr d'une variable locale */

inherit_instruction_function_vector
  local_var_postincrdecr_inherit_instruction_function_vector;

LocalVarPostIncrDecr * LocalVarPostIncrDecr__LocalVarPostIncrDecr
  							(ident, value, cast)
     LocalVar * ident;
     Instruction * value;
     Type * cast;
{
  LocalVarPostIncrDecr * this =
    (LocalVarPostIncrDecr *) Malloc(sizeof(LocalVarPostIncrDecr));

  if (! cast) cast = GetExprType(ident);
  
  Instruction_Constructor
    (local_var_postincrdecr_inherit_instruction_function_vector, this, cast);
  
  this->_position = ident->_position;
  this->_value = value;
  if ((this->_convert =
       (FCT( Object,(*),(Object))) ExprType__Convertible(value, cast))
      == (FCT( Object,(*),(Object))) Type__Convertible)
    this->_convert = 0;
  else if (! this->_convert)
    error_incompatible_types(cast, GetExprType(value), "incompatible type in assignment");

  return this;
}

Object LocalVarPostIncrDecr__Eval(this)
     LocalVarPostIncrDecr * this;
{
  Object value;

  if (this->_convert) {
    value = (this->_convert)(Frame_Pointer[this->_position]);
    Frame_Pointer[this->_position] = (this->_convert) (Eval(this->_value));
  }
  else {
    value = Frame_Pointer[this->_position];
    Frame_Pointer[this->_position] = Eval(this->_value);
  }

  return value;
}


/* Reference a une variable locale (&) */

int correction_adresses(type)
     Type * type;
{
  /* Les caracteres et int sont memorises et accedes dans un champ
     declare et vu comme un Objet afin d'acceler les affectations et
     acces. Mais en procedant ainsi la reference a la variable ne
     peut etre l'adresse de ce champs que lorsqu'on est en format
     type intel, c'est a dire lorsque l'octet de poid faible est
     place a l'adresse du mot. Cette fonction a pour role de calculer
     le declage a apporter a partir de l'adresse. Le travail est
     cependant realise par word qui defini les constantes utilisee */

  if (type == Type_Char)
    return CHAR_CORRECTION;

  if (type == Type_Int)
    return INT_CORRECTION;

  else
    return 0;
}


inherit_instruction_function_vector
  local_var_ref_inherit_instruction_function_vector;

LocalVarRef * LocalVarRef__LocalVarRef(var)
     LocalVar * var;
{
  LocalVarRef * this = (LocalVarRef *) Malloc(sizeof(LocalVarRef));
  Type * type = GetExprType(var);
  
  Instruction_Constructor
    (local_var_ref_inherit_instruction_function_vector, this,
     Type__Type(type, T_Pointer, sizeof(void *)));
  
  this->_position =
/*    var->_position * Type__Sizeof(type) + correction_adresses(type); */
    var->_position * sizeof(Object) + correction_adresses(type);
  return this;
}

Object LocalVarRef__Eval(this)
     LocalVarRef * this;
{
  return ((Object) Frame_Pointer) + this->_position;
}




int IsaLocalVar(i)
     Instruction * i;
{
  return (i->instruction_functions ==
	  &local_var_inherit_instruction_function_vector);
}


void Init_LocalVar()
{
  local_var_inherit_instruction_function_vector._Eval =
      (EvalFct) LocalVar__Eval;

  local_var_assignment_inherit_instruction_function_vector._Eval =
    (EvalFct) LocalVarAssignment__Eval;
  
  local_var_ref_inherit_instruction_function_vector._Eval =
      (EvalFct) LocalVarRef__Eval;
  
  local_var_postincrdecr_inherit_instruction_function_vector._Eval =
    (EvalFct) LocalVarPostIncrDecr__Eval;
}


/* Les variables globales */

inherit_instruction_function_vector
  global_var_inherit_instruction_function_vector;

GlobalVar * GlobalVar__GlobalVar(nom, type, value)
     char * nom;
     Type * type;
     Object value;
{
  GlobalVar * this = (GlobalVar *) Malloc(sizeof(GlobalVar));
  
  Instruction_Constructor
    (global_var_inherit_instruction_function_vector, this, type);
  
  this->_name = nom;
#ifdef RUNTIMECHECK
  {
    Object * pvalue = (Object *) RTCMalloc(sizeof(Object));

    forbit_RTCfree(pvalue);
    *pvalue = value;
    this->_value = (Object) pvalue;
  }
#else
  this->_value = value;
#endif

  return this;
}

Object GlobalVar__Eval(this)
     GlobalVar * this;
{
#ifdef RUNTIMECHECK
  return *((Object *) this->_value);
#else
  return this->_value;
#endif
}

int IsaGlobalVar(i)
     Instruction * i;
{
  return (i->instruction_functions ==
	  &global_var_inherit_instruction_function_vector);
}


/* affectation d'une variable globale */

inherit_instruction_function_vector
  global_var_assignment_inherit_instruction_function_vector;

GlobalVarAssignment * GlobalVarAssignment__GlobalVarAssignment
  							(ident, value, cast)
     GlobalVar * ident;
     Instruction * value;
     Type * cast;
{
  GlobalVarAssignment * this =
    (GlobalVarAssignment *) Malloc(sizeof(GlobalVarAssignment));

  if (! cast) cast = GetExprType(ident);
  
  Instruction_Constructor
    (global_var_assignment_inherit_instruction_function_vector, this, cast);
  
#ifdef RUNTIMECHECK
  this->_pvalue = ((Object *) ident->_value);
#else
  this->_pvalue = &ident->_value;
#endif
  this->_value = value;
  if ((this->_convert =
       (FCT( Object,(*),(Object))) ExprType__Convertible(value, cast))
      == (FCT( Object,(*),(Object))) Type__Convertible)
    this->_convert = 0;
  else if (! this->_convert)
    error_incompatible_types(cast, GetExprType(value), "incompatible type in assignment");

  return this;
}

Object GlobalVarAssignment__Eval(this)
     GlobalVarAssignment * this;
{
  return *(this->_pvalue) =
    (this->_convert) ? (this->_convert) (Eval(this->_value))
		     : Eval(this->_value);
}

/* Reference a une variable globale */

inherit_instruction_function_vector
  global_var_ref_inherit_instruction_function_vector;

GlobalVarRef * GlobalVarRef__GlobalVarRef(var)
     GlobalVar * var;
{
  GlobalVarRef * this = (GlobalVarRef *) Malloc(sizeof(GlobalVarRef));
  Type * type = GetExprType(var);
  
  Instruction_Constructor
    (global_var_ref_inherit_instruction_function_vector, this,
     Type__Type(type, T_Pointer, sizeof(void *)));
  
  this->_value =
#ifdef RUNTIMECHECK
    var->_value + correction_adresses(type);
#else
    ((Object) &var->_value) + correction_adresses(type);
#endif

  return this;
}

Object GlobalVarRef__Eval(this)
     GlobalVarRef * this;
{
  return this->_value;
}

/* post incr decr d'une variable globale */

inherit_instruction_function_vector
  global_var_postincrdecr_inherit_instruction_function_vector;

GlobalVarPostIncrDecr * GlobalVarPostIncrDecr__GlobalVarPostIncrDecr
  							(ident, value, cast)
     GlobalVar * ident;
     Instruction * value;
     Type * cast;
{
  GlobalVarPostIncrDecr * this =
    (GlobalVarPostIncrDecr *) Malloc(sizeof(GlobalVarPostIncrDecr));

  if (! cast) cast = GetExprType(ident);
  
  Instruction_Constructor
    (global_var_postincrdecr_inherit_instruction_function_vector, this, cast);
  
#ifdef RUNTIMECHECK
  this->_pvalue = (Object *) ident->_value;
#else
  this->_pvalue = &ident->_value;
#endif
  this->_value = value;
  if ((this->_convert =
       (FCT( Object,(*),(Object))) ExprType__Convertible(value, cast))
      == (FCT( Object,(*),(Object))) Type__Convertible)
    this->_convert = 0;
  else if (! this->_convert)
    error_incompatible_types(cast, GetExprType(value), "incompatible type in assignment");

  return this;
}

Object GlobalVarPostIncrDecr__Eval(this)
     GlobalVarPostIncrDecr * this;
{
  Object value;

  if (this->_convert) {
    value = (this->_convert) (*(this->_pvalue));
    *(this->_pvalue) = (this->_convert) (Eval(this->_value));
  }
  else {
    value = *(this->_pvalue);
    *(this->_pvalue) = Eval(this->_value);
  }

  return value;
}


void Init_GlobalVar()
{
  global_var_inherit_instruction_function_vector._Eval =
    (EvalFct) GlobalVar__Eval;

  global_var_assignment_inherit_instruction_function_vector._Eval =
    (EvalFct) GlobalVarAssignment__Eval;

  global_var_ref_inherit_instruction_function_vector._Eval =
    (EvalFct) GlobalVarRef__Eval;

  global_var_postincrdecr_inherit_instruction_function_vector._Eval =
    (EvalFct) GlobalVarPostIncrDecr__Eval;
}

