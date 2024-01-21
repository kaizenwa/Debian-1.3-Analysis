/* ########################################################################

				Array.c

   File: Array.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Array.c
   Description: 
   Created: Tue Feb 21 10:43:22 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:43:23 MET 1995
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

#include "Array.h"
#include "list.h"

#ifdef RUNTIMECHECK

#include <stdio.h>
#include "error.h"
#include "mem.h"

#endif

/*********************
  
 Acces dans un tableau

 ********************/


/* Les fonctions d'acces aux elements en fonction de leur type */

Object array_char_access(p, index)
     char * p;
     int index;
{
#ifdef RUNTIMECHECK
  if (((p += index) < Address_Min) || (p > Address_Char_Max))
    Error("char array access out of memory");

  return *p;

#else

  return p[index];
  
#endif
  
}

Object array_int_access(p, index)
     int * p;
     int index;
{
#ifdef RUNTIMECHECK
  if (((char *) (p += index) < Address_Min) || ((char *) p > Address_Int_Max))
    Error("int array access out of memory");

#if CHECK_INT_POINTER
  if ((Object) p & CHECK_INT_POINTER)
    Error("illegal pointer alignment for int access");
#endif

  return *p;

#else

  return p[index];
  
#endif
}

Object array_pointer_access(p, index)
     Object * p;
     int index;
{
#ifdef RUNTIMECHECK
  if (((char *) (p += index) < Address_Min) ||
      ((char *) p > Address_Pointer_Max))
    Error("pointer array access out of memory");

#if CHECK_POINTER_POINTER
  if ((Object) p & CHECK_POINTER_POINTER)
    Error("illegal pointer alignment for pointer access");
#endif

  return *p;

#else

  return p[index];
  
#endif
}

Object sub_array_access(p, index, subsize)
     char * p;
     int index;
     int subsize;
{
  return (Object) (p + index * subsize);
}

/**/


static inherit_instruction_function_vector
  arrayaccess_inherit_instruction_function_vector;

ArrayAccess * ArrayAccess__ArrayAccess(array, index)
     Instruction * array;
     Instruction * index;
{
  ArrayAccess * this = (ArrayAccess *) Malloc(sizeof(ArrayAccess));
  Type * type = Type__Pointed_Type(GetExprType(array));
      
  Instruction_Constructor
    (arrayaccess_inherit_instruction_function_vector, this, type);

  this->_array = array;
  this->_index = index;
  if (Type__IsaArray(type)) {
    this->_subsize = Type__Sizeof(type);
    this->_access = (FCT( Object,(*),(Object, Object, int))) sub_array_access;
  }
  else {
    this->_subsize = 0;
    this->_access =
      (type == Type_Char)
	    ? (FCT( Object,(*),(Object, Object, int))) array_char_access
	  : (type == Type_Int)
	    ? (FCT( Object,(*),(Object, Object, int))) array_int_access
	    : (FCT( Object,(*),(Object, Object, int))) array_pointer_access;
  }
  
  return this;
}

static Object ArrayAccess__Eval(this)
     ArrayAccess * this;
{
  return (this->_access)
	  	(Eval(this->_array), Eval(this->_index), this->_subsize);
}


/****************************
  
 Affectation dans un tableau

 ***************************/


/* Les fonctions d'affectation des elements en fonction de leur type */

Object array_char_affect(p, index, value)
     char * p;
     int index;
     char value;
{
  return p[index] = value;
}

Object array_int_affect(p, index, value)
     int * p;
     int index;
     int value;
{
  return p[index] = value;
}

Object array_pointer_affect(p, index, value)
     Object * p;
     int index;
     Object value;
{
  return p[index] = value;
}

#ifdef RUNTIMECHECK

/* ces fonctions ne sont pas appelees lors des modifications
   car le test d'adresse est realise lors de l'acces */

Object check_array_char_affect(p, index, value)
     char * p;
     int index;
     char value;
{
  if (((p += index) < Address_Min) || (p > Address_Char_Max))
    Error("char assignment through pointer out of your memory area");

  return *p = value;
}

Object check_array_int_affect(p, index, value)
     int * p;
     int index;
     int value;
{
  if (((char *) (p += index) < Address_Min) || ((char *) p > Address_Int_Max))
    Error("int assignment through pointer out of your memory area");

#if CHECK_INT_POINTER
  if ((Object) p & CHECK_INT_POINTER)
    Error("illegal pointer alignment for int assignment");
#endif

  return *p = value;
}

Object check_array_pointer_affect(p, index, value)
     Object * p;
     int index;
     Object value;
{
  if (((char *) (p += index) < Address_Min) ||
      ((char *) p > Address_Pointer_Max))
    Error("pointer assignment through pointer out of your memory area");

#if CHECK_POINTER_POINTER
  if ((Object) p & CHECK_POINTER_POINTER)
    Error("illegal pointer alignment for pointer assignment");
#endif

  return *p = value;
}

#endif

static inherit_instruction_function_vector
  arrayassignment_inherit_instruction_function_vector;

ArrayAssignment * ArrayAssignment__ArrayAssignment(array, index, value,
						   type, convert)
     Instruction * array;
     Instruction * index;
     Instruction * value;
     Type * type;
     FCT( Object,(* convert),(Object));
{
  ArrayAssignment * this = (ArrayAssignment *) Malloc(sizeof(ArrayAssignment));

  Instruction_Constructor
    (arrayassignment_inherit_instruction_function_vector, this, type);

  this->_array = array;
  this->_index = index;
  this->_value = value;
  this->_affect =
#ifdef RUNTIMECHECK
	(type == Type_Char)
      ? (FCT(Object,(*),(Object, Object, Object))) check_array_char_affect
      : (type == Type_Int)
      ? (FCT(Object,(*),(Object, Object, Object))) check_array_int_affect
      : (FCT(Object,(*),(Object, Object, Object))) check_array_pointer_affect;
#else
	(type == Type_Char)
      ? (FCT(Object,(*),(Object, Object, Object))) array_char_affect
      : (type == Type_Int)
      ? (FCT(Object,(*),(Object, Object, Object))) array_int_affect
      : (FCT(Object,(*),(Object, Object, Object))) array_pointer_affect;
#endif
  this->_convert = convert;

  return this;
}

static Object ArrayAssignment__Eval(this)
     ArrayAssignment * this;
{
  if (this->_convert)
    return (this->_affect)(Eval(this->_array), Eval(this->_index),
			   (this->_convert)(Eval(this->_value)));
  else
    return (this->_affect)(Eval(this->_array), Eval(this->_index),
			   Eval(this->_value));
}


/****************************
  
 Modification dans un tableau

 ***************************/

static inherit_instruction_function_vector
  arraymodify_inherit_instruction_function_vector;

ArrayModify * ArrayModify__ArrayModify(array, index, value, bfct,
				       type, convert)
     Instruction * array;
     Instruction * index;
     Instruction * value;
     FCT( Object,(*bfct),(Object, Object, int));
     Type * type;
     FCT( Object,(*convert),(Object));
{
  ArrayModify * this = (ArrayModify *) Malloc(sizeof(ArrayModify));

  Instruction_Constructor
    (arraymodify_inherit_instruction_function_vector, this, type);

  this->_array = array;
  this->_index = index;
  this->_value = value;
  if (type == Type_Char) {
    this->_affect =
      (FCT(Object,(*),(Object, Object, Object))) array_char_affect;
    this->_access = (FCT(Object,(*),(Object, Object))) array_char_access;
  }
  else if (type == Type_Int) {
    this->_affect =
      (FCT(Object,(*),(Object, Object, Object))) array_int_affect;
    this->_access = (FCT(Object,(*),(Object, Object))) array_int_access;
  }
  else {
    this->_affect =
      (FCT(Object,(*),(Object, Object, Object))) array_pointer_affect;
    this->_access =
      (FCT(Object,(*),(Object, Object))) array_pointer_access;
  }
  this->_convert = convert;
  this->_bfct = bfct;
  this->_size = Type__Sizeof(type);

  return this;
}

static Object ArrayModify__Eval(this)
     ArrayModify * this;
{
  Object array = Eval(this->_array);
  Object index = Eval(this->_index);
  Object value = this->_bfct((this->_access)(array, index),
			     Eval(this->_value), this->_size);
  
  if (this->_convert)
    return (this->_affect)(array, index, (this->_convert)(value));
  else
    return (this->_affect)(array, index, value);
}


/**************************
  
 Reference a un element (&)

 *************************/

static inherit_instruction_function_vector
  arrayref_inherit_instruction_function_vector;

ArrayRef * ArrayRef__ArrayRef(array, index)
     Instruction * array;
     Instruction * index;
{
  ArrayRef * this = (ArrayRef *) Malloc(sizeof(ArrayRef));
  Type * elttype = Type__Pointed_Type(GetExprType(array));
      
  Instruction_Constructor
    (arrayref_inherit_instruction_function_vector, this,
     Type__Type(elttype, T_Pointer, sizeof(void *)));

  this->_array = array;
  this->_index = index;
  this->_subsize = Type__Sizeof(elttype);
  
  return this;
}

static Object ArrayRef__Eval(this)
     ArrayRef * this;
{
  return (Object) (Eval(this->_array) + Eval(this->_index) * this->_subsize);
}

/******************************
  
 Post incr decr dans un tableau

 *****************************/

static inherit_instruction_function_vector
  arraypostincrdecr_inherit_instruction_function_vector;

ArrayPostIncrDecr * ArrayPostIncrDecr__ArrayPostIncrDecr(array, index, value,
							 bfct, type, convert)
     Instruction * array;
     Instruction * index;
     int value;
     FCT( Object,(*bfct),(Object, Object, int));
     Type * type;
     FCT( Object,(*convert),(Object));
{
  ArrayPostIncrDecr * this =
    (ArrayPostIncrDecr *) Malloc(sizeof(ArrayPostIncrDecr));

  Instruction_Constructor
    (arraypostincrdecr_inherit_instruction_function_vector, this, type);

  this->_array = array;
  this->_index = index;
  this->_value = value;
  if (type == Type_Char) {
    this->_affect =
      (FCT(Object,(*),(Object, Object, Object))) array_char_affect;
    this->_access = (FCT(Object,(*),(Object, Object))) array_char_access;
  }
  else if (type == Type_Int) {
    this->_affect =
      (FCT(Object,(*),(Object, Object, Object))) array_int_affect;
    this->_access = (FCT(Object,(*),(Object, Object))) array_int_access;
  }
  else {
    this->_affect =
      (FCT(Object,(*),(Object, Object, Object))) array_pointer_affect;
    this->_access =
      (FCT(Object,(*),(Object, Object))) array_pointer_access;
  }
  this->_convert = convert;
  this->_bfct = bfct;
  this->_size = Type__Sizeof(type);

  return this;
}

static Object ArrayPostIncrDecr__Eval(this)
     ArrayPostIncrDecr * this;
{
  Object array = Eval(this->_array);
  Object index = Eval(this->_index);
  Object newvalue =
    this->_bfct((this->_access)(array, index), this->_value, this->_size);
  Object value;
  
  if (this->_convert) {
    value = (this->_convert)((this->_access)(array, index));
    (this->_affect)(array, index, (this->_convert)(newvalue));
  }
  else {
    value = (this->_access)(array, index);
    (this->_affect)(array, index, newvalue);
  }

  return value;
}


/**/


void Init_Array()
{
  arrayaccess_inherit_instruction_function_vector._Eval =
    (EvalFct) ArrayAccess__Eval;
  
  arrayassignment_inherit_instruction_function_vector._Eval =
    (EvalFct) ArrayAssignment__Eval;
  
  arraymodify_inherit_instruction_function_vector._Eval =
    (EvalFct) ArrayModify__Eval;
  
  arrayref_inherit_instruction_function_vector._Eval =
    (EvalFct) ArrayRef__Eval;
  
  arraypostincrdecr_inherit_instruction_function_vector._Eval =
    (EvalFct) ArrayPostIncrDecr__Eval;
}
