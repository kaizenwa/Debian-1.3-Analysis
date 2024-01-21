/* ########################################################################

			       RefDeref.c

   File: RefDeref.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/RefDeref.c
   Description: 
   Created: Tue Feb 21 10:58:27 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:58:27 MET 1995
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



#include "RefDeref.h"

#ifdef RUNTIMECHECK

#include <stdio.h>
#include "error.h"
#include "mem.h"

#endif

/***********
  
 Indirection

 **********/

  
/* Les fonctions d'indirection en fonction de leur type */

Object char_indirection(p)
     char * p;
{
#ifdef RUNTIMECHECK
  if ((p < Address_Min) || (p > Address_Char_Max))
    Error("char access through pointer out of your memory area");
#endif

  return *p;
}

Object int_indirection(p)
     int * p;
{
#ifdef RUNTIMECHECK
  if (((char *) p < Address_Min) || ((char *) p > Address_Int_Max))
    Error("int access through pointer out of your memory area");

#if CHECK_INT_POINTER
  if ((Object) p & CHECK_INT_POINTER)
    Error("illegal pointer alignment for int access");
#endif
#endif

  return *p;
}

Object pointer_indirection(p)
     Object * p;
{
#ifdef RUNTIMECHECK
  if (((char *) p < Address_Min) || ((char *) p > Address_Pointer_Max))
    Error("pointer access through pointer out of your memory area");

#if CHECK_POINTER_POINTER
  if ((Object) p & CHECK_POINTER_POINTER)
    Error("illegal pointer alignment for pointer access");
#endif
#endif

  return *p;
}

Object sub_array_indirection(p)
     char * p;
{
  return (Object) (p);
}


static inherit_instruction_function_vector
  indirection_inherit_instruction_function_vector;

Indirection * Indirection__Indirection(pointer, type)
     Instruction * pointer;
     Type * type;
{
  Indirection * this = (Indirection *) Malloc(sizeof(Indirection));
      
  Instruction_Constructor
    (indirection_inherit_instruction_function_vector, this, type);

  this->_pointer = pointer;
  this->_deref = (Type__IsaArray(type))
		 ? (FCT ( Object, (*), (Object))) sub_array_indirection
	       : (type == Type_Char)
		 ? (FCT ( Object, (*), (Object))) char_indirection
	       : (type == Type_Int)
    		 ? (FCT ( Object, (*), (Object))) int_indirection
		 : (FCT ( Object, (*), (Object))) pointer_indirection;
  return this;
}

static Object Indirection__Eval(this)
     Indirection * this;
{
  return (this->_deref)(Eval(this->_pointer));
}


/**********************
  
  Affectation indirecte

 *********************/


/* Les fonctions d'affectation en fonction de leur type */

Object char_indirection_assignment(p, value)
     char * p;
     Object value;
{
  return *p = value;
}

Object int_indirection_assignment(p, value)
     int * p;
     Object value;
{
  return *p = value;
}

Object pointer_indirection_assignment(p, value)
     Object * p;
     Object value;
{
  return *p = value;
}

#ifdef RUNTIMECHECK

/* ces fonctions ne sont pas appelees lors des modifications
   car le test d'adresse est realise lors de l'acces */

Object check_char_indirection_assignment(p, value)
     char * p;
     Object value;
{
  if ((p < Address_Min) || (p > Address_Char_Max))
    Error("char assignment through pointer out of your memory area");

  return *p = value;
}

Object check_int_indirection_assignment(p, value)
     int * p;
     Object value;
{
  if (((char *) p < Address_Min) || ((char *) p > Address_Int_Max))
    Error("int assignment through pointer out of your memory area");

#if CHECK_INT_POINTER
  if ((Object) p & CHECK_INT_POINTER)
    Error("illegal pointer alignment for int assignment");
#endif

  return *p = value;
}

Object check_pointer_indirection_assignment(p, value)
     Object * p;
     Object value;
{
  if (((char *) p < Address_Min) || ((char *) p > Address_Pointer_Max))
    Error("pointer assignment through pointer out of your memory area");

#if CHECK_POINTER_POINTER
  if ((Object) p & CHECK_POINTER_POINTER)
    Error("illegal pointer alignment for pointer assignment");
#endif

  return *p = value;
}

#endif


static inherit_instruction_function_vector
  indirectionassignment_inherit_instruction_function_vector;

IndirectionAssignment *
IndirectionAssignment__IndirectionAssignment(pointer, value, type, convert)
     Instruction * pointer;
     Instruction * value;
     Type * type;
     FCT( Object,(*convert),(Object) );
{
  IndirectionAssignment * this =
    (IndirectionAssignment *) Malloc(sizeof(IndirectionAssignment));
      
  Instruction_Constructor
    (indirectionassignment_inherit_instruction_function_vector, this, type);

  this->_pointer = pointer;
  this->_value = value;
  this->_convert = convert;
  this->_refassign =
#ifdef RUNTIMECHECK
	(type == Type_Char)
      ? (FCT(Object,(*),(Object, Object))) check_char_indirection_assignment
      : (type == Type_Int)
      ? (FCT(Object,(*),(Object, Object))) check_int_indirection_assignment
      : (FCT(Object,(*),(Object, Object))) check_pointer_indirection_assignment;
#else
	(type == Type_Char)
      ? (FCT(Object,(*),(Object, Object))) char_indirection_assignment
      : (type == Type_Int)
      ? (FCT(Object,(*),(Object, Object))) int_indirection_assignment
      : (FCT(Object,(*),(Object, Object))) pointer_indirection_assignment;
#endif
  
  return this;
}

static Object IndirectionAssignment__Eval(this)
     IndirectionAssignment * this;
{
  if (this->_convert)
    return (this->_refassign)(Eval(this->_pointer),
			      (this->_convert)(Eval(this->_value)));
  else
    return (this->_refassign)(Eval(this->_pointer), Eval(this->_value));
}


/*********************
  
 Modification indirect

 ********************/

static inherit_instruction_function_vector
  indirectionmodify_inherit_instruction_function_vector;

IndirectionModify *
IndirectionModify__IndirectionModify(pointer, value, bfct, type, convert)
     Instruction * pointer;
     Instruction * value;
     FCT( Object,(*bfct),(Object, Object, int));
     Type * type;
     FCT( Object,(*convert),(Object));
{
  IndirectionModify * this =
    (IndirectionModify *) Malloc(sizeof(IndirectionModify));

  Instruction_Constructor
    (indirectionmodify_inherit_instruction_function_vector, this, type);

  this->_pointer = pointer;
  this->_value = value;
  this->_convert = convert;
  this->_bfct = bfct;
  
  if (type == Type_Char) {
    this->_refassign =
      (FCT(Object,(*),(Object, Object))) char_indirection_assignment;
    this->_deref = (FCT(Object,(*),(Object))) char_indirection;
  }
  else if (type == Type_Int) {
    this->_refassign =
      (FCT(Object,(*),(Object, Object))) int_indirection_assignment;
    this->_deref = (FCT(Object,(*),(Object))) int_indirection;
  }
  else {
    this->_refassign =
      (FCT(Object,(*),(Object, Object))) pointer_indirection_assignment;
    this->_deref = (FCT(Object,(*),(Object))) pointer_indirection;
  }
  this->_size = Type__Sizeof(type);

  return this;
}

static Object IndirectionModify__Eval(this)
     IndirectionModify * this;
{
  Object pointer = Eval(this->_pointer);
  Object value =
    this->_bfct((this->_deref)(pointer), Eval(this->_value), this->_size);
  
  if (this->_convert)
    return (this->_refassign)(pointer, (this->_convert)(value));
  else
    return (this->_refassign)(pointer, value);
}

/***********************
  
 Post incr decr indirect

 **********************/

static inherit_instruction_function_vector
  indirectionpostincrdecr_inherit_instruction_function_vector;

IndirectionPostIncrDecr *
IndirectionPostIncrDecr__IndirectionPostIncrDecr
  					(pointer, value, bfct, type, convert)
     Instruction * pointer;
     int value;
     FCT( Object,(*bfct),(Object, Object, int));
     Type * type;
     FCT( Object,(*convert),(Object));
{
  IndirectionPostIncrDecr * this =
    (IndirectionPostIncrDecr *) Malloc(sizeof(IndirectionPostIncrDecr));

  Instruction_Constructor
    (indirectionpostincrdecr_inherit_instruction_function_vector, this, type);

  this->_pointer = pointer;
  this->_value = value;
  this->_convert = convert;
  this->_bfct = bfct;
  
  if (type == Type_Char) {
    this->_refassign =
      (FCT(Object,(*),(Object, Object))) char_indirection_assignment;
    this->_deref = (FCT(Object,(*),(Object))) char_indirection;
  }
  else if (type == Type_Int) {
    this->_refassign =
      (FCT(Object,(*),(Object, Object))) int_indirection_assignment;
    this->_deref = (FCT(Object,(*),(Object))) int_indirection;
  }
  else {
    this->_refassign =
      (FCT(Object,(*),(Object, Object))) pointer_indirection_assignment;
    this->_deref = (FCT(Object,(*),(Object))) pointer_indirection;
  }
  this->_size = Type__Sizeof(type);

  return this;
}

static Object IndirectionPostIncrDecr__Eval(this)
     IndirectionPostIncrDecr * this;
{
  Object pointer = Eval(this->_pointer);
  Object newvalue =
    this->_bfct((this->_deref)(pointer), this->_value, this->_size);
  Object value;
  
  if (this->_convert) {
    value = (this->_convert)((this->_deref)(pointer));
    (this->_refassign)(pointer, (this->_convert)(newvalue));
  }
  else {
    value = (this->_deref)(pointer);
    (this->_refassign)(pointer, newvalue);
  }

  return value;
}

/**/


void Init_RefDeref()
{
  indirection_inherit_instruction_function_vector._Eval =
    (EvalFct) Indirection__Eval;
  indirectionassignment_inherit_instruction_function_vector._Eval =
    (EvalFct) IndirectionAssignment__Eval;
  indirectionmodify_inherit_instruction_function_vector._Eval =
    (EvalFct) IndirectionModify__Eval;
  indirectionpostincrdecr_inherit_instruction_function_vector._Eval =
    (EvalFct) IndirectionPostIncrDecr__Eval;
}
