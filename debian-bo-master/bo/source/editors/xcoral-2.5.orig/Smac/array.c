/* ########################################################################

				array.c

   File: array.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/array.c
   Description: 
   Created: Tue Feb 21 12:47:30 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:47:30 MET 1995
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

#include "Array.h"
#include "Type.h"
#include "list.h"
#include "error.h"
#include "operator.h"

/* fct = make_array_access
   info = List ** [array, index]
   */

Instruction * make_array_access(l)
     List * l;
{
  List ** m = (List **) l->info;
  Instruction * a1 = MAKE(m[0]);
  Instruction * a2 = MAKE(m[1]);
  Type * t1 = GetExprType(a1);
  Type * t2 = GetExprType(a2);

  free(l);
  free(m);

  if (Type__IsaPointer(t1) && (! Type__IsaFunction(t1))) {
    if ((t2 == Type_Int) || (t2 == Type_Char))
      /* le prog est tab[index] */
      return (Instruction *) ArrayAccess__ArrayAccess(a1, a2);
  }
  else if (Type__IsaPointer(t2) && (! Type__IsaFunction(t2))) {
    if ((t1 == Type_Int) || (t1 == Type_Char))
      /* le prog est index[tab] */
      return (Instruction *) ArrayAccess__ArrayAccess(a2, a1);
  }

  sprintf(err_msg, "illegal array access");
  Error(err_msg);

  return 0;				/* pour le compilo */
}


/* Modification d'un element de tableau */

Instruction * array_assignment(place, lvalue, opername, cast)
     List * place;
     List * lvalue;
     char * opername;
     Type * cast;
{

  List ** m = (List **) place->info;
  Instruction * a1 = MAKE(m[0]);
  Instruction * a2 = MAKE(m[1]);
  Instruction * value = MAKE(lvalue);
  Type * t1 = GetExprType(a1);
  Type * t2 = GetExprType(a2);
  Type * type = GetExprType(value);
  FCT( Object,(*convert),(Object));
  
  free(place);
  free(m);

  if (Type__IsaFunction(t1) ||
      Type__IsaFunction(t2) ||
      (Type__IsaPointer(t1)
       ? (((t2 != Type_Int) && (t2 != Type_Char)) ||
	  (Type__IsaArray(Type__Pointed_Type(t1))))
       : ((! Type__IsaPointer(t2)) ||
	  (Type__IsaArray(Type__Pointed_Type(t2))) ||
	  ((t1 != Type_Int) && (t1 != Type_Char)))))
    Error("illegal array or index");

  if (Type__IsaPointer(t2)) {
    Instruction * ix = a1;
    
    a1 = a2;
    a2 = ix;
  }

  /* a1 est le tableau, a2 l'index */

  if (! cast) cast = Type__Pointed_Type(t1);

  if (Type__IsaArray(cast))
    Error("illegal array or index");
  
  if (*opername != '=') {
    Type * valtype;
    FCT( Object,(*bfct),(Object, Object, int));

    opername[strlen(opername) - 1] = 0;			/* enleve = */
    bfct = find_modifbinary_oper(opername, cast, type, &valtype);
    free(opername);

    if ((convert = (FCT(Object,(*),(Object))) Type__Convertible(valtype, cast))
	== (FCT( Object,(*),(Object))) Type__Convertible)
      convert = 0;
    else if ((! convert) &&
	     ((type != Type_Int) || (! IsaConst(value)) || Eval(value)))
      error_incompatible_types(cast, valtype,
			       "incompatible type in array assignment");
    
    return (Instruction *)
      ArrayModify__ArrayModify(a1, a2, value, bfct, cast, convert);
  }
  
  if ((convert =
       	(FCT( Object,(*),(Object))) ExprType__Convertible(value, cast))
      == (FCT( Object,(*),(Object))) Type__Convertible)
    convert = 0;
  else if (! convert)
    error_incompatible_types(cast, type,
			     "incompatible type in array assignment");
  
  free(opername);
  
  return (Instruction *)
    ArrayAssignment__ArrayAssignment(a1, a2, value, cast, convert);
}


/* Reference a un element (&) */

Instruction * array_reference(place)
     List * place;		/* make_array_access */
{
  List ** m = (List **) place->info;
  Instruction * a1 = MAKE(m[0]);
  Instruction * a2 = MAKE(m[1]);
  Type * t1 = GetExprType(a1);
  Type * t2 = GetExprType(a2);

  free(place);
  free(m);

  if (Type__IsaPointer(t1) && (! Type__IsaFunction(t1))) {
    if ((t2 == Type_Int) || (t2 == Type_Char))
      /* le prog est tab[index] */
      return (Instruction *) ArrayRef__ArrayRef(a1, a2);
  }
  else if (Type__IsaPointer(t2) && (! Type__IsaFunction(t2))) {
    if ((t1 == Type_Int) || (t1 == Type_Char))
      /* le prog est index[tab] */
      return (Instruction *) ArrayRef__ArrayRef(a2, a1);
  }

  sprintf(err_msg, "illegal array reference");
  Error(err_msg);

  return 0;				/* pour le compilo */
}


/* Post incr decr d'un element de tableau */

Instruction * array_post_incrdecr(place, value, cast)
     List * place;
     int    value;
     Type * cast;
{
  List ** m = (List **) place->info;
  Instruction * a1 = MAKE(m[0]);
  Instruction * a2 = MAKE(m[1]);
  Type * t1 = GetExprType(a1);
  Type * t2 = GetExprType(a2);
  Type * type;
  FCT( Object,(*convert),(Object));
  FCT( Object,(*bfct),(Object, Object, int));
  
  free(place);
  free(m);

  if (Type__IsaFunction(t1) ||
      Type__IsaFunction(t2) ||
      (Type__IsaPointer(t1)
       ? (((t2 != Type_Int) && (t2 != Type_Char)) ||
	  (Type__IsaArray(Type__Pointed_Type(t1))))
       : ((! Type__IsaPointer(t2)) ||
	  (Type__IsaArray(Type__Pointed_Type(t2))) ||
	  ((t1 != Type_Int) && (t1 != Type_Char)))))
    Error("illegal array or index");

  if (Type__IsaPointer(t2)) {
    Instruction * ix = a1;
    
    a1 = a2;
    a2 = ix;
  }

  /* a1 est le tableau, a2 l'index */

  type = Type__Pointed_Type(t1);
  if (! cast) cast = type;

  if (Type__IsaArray(cast))
    Error("illegal array or index");
  
  {
    Type * dummy;

    bfct = find_modifbinary_oper("+", cast, type, &dummy);
  }

  if ((convert = (FCT(Object,(*),(Object))) Type__Convertible(type, cast))
      == (FCT( Object,(*),(Object))) Type__Convertible)
    convert = 0;
  else if (! convert)
    error_incompatible_types(cast, type, "incompatible type in array assignment");
    
  return (Instruction *)
    ArrayPostIncrDecr__ArrayPostIncrDecr(a1, a2, value, bfct, cast, convert);
}


