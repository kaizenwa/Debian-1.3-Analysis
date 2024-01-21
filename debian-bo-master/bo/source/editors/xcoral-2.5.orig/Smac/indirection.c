/* ########################################################################

			     indirection.c

   File: indirection.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/indirection.c
   Description: 
   Created: Tue Feb 21 12:55:59 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:56:00 MET 1995
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



#include <string.h>

#include "indirection.h"
#include "list.h"
#include "operator.h"
#include "Const.h"
#include "error.h"

/*   ((cast) *place) opername value; */

Instruction * indirection_assignment(place, lvalue, opername, cast)
     List * place;
     List * lvalue;
     char * opername;
     Type * cast;
{
  Instruction * p = MAKE((List *) place->info);
  Instruction * value = MAKE(lvalue);
  Type * ptype = GetExprType(p);
  Type * type = GetExprType(value);
  FCT( Object,(*convert),(Object) );

  free(place);

  if (! Type__IsaPointer(ptype))
    Error("illegal indirection (not a pointer)");

  if (! cast) cast = Type__Pointed_Type(ptype);

  if (Type__IsaArray(cast))
    Error("illegal assignment (*&array = ...)");
  
  if (*opername != '=') {
    Type * valtype;
    FCT( Object,(*bfct),(Object, Object, int));

    opername[strlen(opername) - 1] = 0;			/* enleve = */
    bfct = find_modifbinary_oper(opername, cast, type, &valtype);
    free(opername);

    if ((convert =
	 (FCT(Object,(*),(Object))) Type__Convertible(valtype, cast))
	== (FCT( Object,(*),(Object))) Type__Convertible)
      convert = 0;
    else if (! convert)
      error_incompatible_types(cast, valtype, 
			       "incompatible type in assignment");
    
    return (Instruction *)
      IndirectionModify__IndirectionModify(p, value, bfct, cast, convert);
  }
  
  if ((convert =
       (FCT(Object,(*),(Object))) ExprType__Convertible(value, cast))
      == (FCT( Object,(*),(Object))) Type__Convertible)
    convert = 0;
  else if (! convert)
    error_incompatible_types(cast, GetExprType(value), 
			     "incompatible type in assignment");
  
  free(opername);
  
  return (Instruction *)
    IndirectionAssignment__IndirectionAssignment(p, value, cast, convert);
}


/*   ((cast) *place)++/--; */

Instruction * indirection_post_incrdecr(place, value, cast)
     List * place;
     int    value;
     Type * cast;
{
  Instruction * p = MAKE((List *) place->info);
  Type * ptype = GetExprType(p);
  Type * type = Type__Pointed_Type(ptype);
  FCT( Object,(*convert),(Object) );
  FCT( Object,(*bfct),(Object, Object, int));

  free(place);

  if (! Type__IsaPointer(ptype))
    Error("illegal indirection (not a pointer)");

  if (! cast) cast = type;

  if (Type__IsaArray(cast))
    Error("illegal assignment (*(&array)++/--)");
  
  {
    Type * dummy;
    
    bfct = find_modifbinary_oper("+", cast, Type_Int, &dummy);
  }

  if ((convert = (FCT(Object,(*),(Object))) Type__Convertible(type, cast))
      == (FCT( Object,(*),(Object))) Type__Convertible)
    convert = 0;
  else if (! convert)
    error_incompatible_types(cast, type, "incompatible type in assignment");
    
  return (Instruction *)
    IndirectionPostIncrDecr__IndirectionPostIncrDecr
      		(p, value, bfct, cast, convert);
}
