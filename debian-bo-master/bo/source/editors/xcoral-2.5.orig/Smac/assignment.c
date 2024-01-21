/* ########################################################################

			      assignment.c

   File: assignment.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/assignment.c
   Description: 
   Created: Tue Feb 21 12:50:06 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:50:07 MET 1995
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

#include "identifier.h"
#include "Instruction.h"
#include "list.h"
#include "error.h"
#include "stack.h"
#include "decl.h"
#include "array.h"
#include "indirection.h"
#include "Var.h"
#include "Function.h"


/*
  fct = make_assignment
  info = (List **) [place, make_identifier "oper", value]

  place peut etre
  	(fct) make_identifier
  	(fct) make_oper_asterisk
	(fct) make_array_access
*/

/* Modification d'une (supposee) variable */

Instruction * var_assignment(mkident, lvalue, opername, cast)
     List * mkident;
     List * lvalue;
     char * opername;
     Type * cast;
{
  Object ident = make_identifier(List__List(0, Strdup(mkident->info)));
  Instruction * value;

  if (Type__IsaArray(GetExprType((Instruction *) ident)) ||
      Type__IsaFunction(GetExprType((Instruction *) ident))) {
    sprintf(err_msg, "illegal assignment %s %s ..",
	    (char *) mkident->info, opername);
    Error(err_msg);
  }

  if (*opername != '=') {
    /* fabrique l'affectation */
    List ** m = (List **) Malloc(3 * sizeof(List *));

    opername[strlen(opername) - 1] = 0;		/* enleve = */
    ((char **) m)[0] = opername;
    if (cast) {
      void ** cm = (void **) Malloc(2 * sizeof(void *));
      
      cm[0] = (void *) cast;
      cm[1] = (void *) mkident;
      m[1] = List__List(make_cast, (void *) cm);
    }
    else
      m[1] = mkident;
    m[2] = lvalue;
    
    value = make_binary_oper(List__List(0, m));
  }
  else {
    free(mkident);
    value = MAKE(lvalue);
  }
  free(opername);
    
  if (IsaGlobalVar((Instruction *) ident))
    return (Instruction *)
      GlobalVarAssignment__GlobalVarAssignment
			((GlobalVar *) ident, value, cast);
  else
    return (Instruction *)
      LocalVarAssignment__LocalVarAssignment((LocalVar *) ident, value, cast);
}


Instruction * make_assignment(l)
     List * l;
{
  List ** m = (List **) l->info;
  List * place = m[0];
  char * opername = (char *) m[1]->info;
  List * value = m[2];
  Type * cast = 0;

  free(m);
  free(l);

  if (place->fct == make_cast) {
    /* Seul le cast englobant est utile lorsqu'on est a gauche de l'affect..
       A noter que cette utilisation du cast est interdite en ANSI C, pour
       le moment je le laisse par compatibilite avec C */
    cast = ((Type **) place->info)[0];
    do {
      List * pl = ((List**) place->info)[1];

      /* liberer le type = ((Type **) place->info)[0] */
      free(place);
      place = pl;
    } while (place->fct == make_cast);
  }
  
  if (place->fct == ((InstrFct) make_identifier))
    return var_assignment(place, value, opername, cast);

  if (place->fct == ((InstrFct) make_oper_asterisk))
    return indirection_assignment(place, value, opername, cast);

  if (place->fct == ((InstrFct) make_array_access))
    return array_assignment(place, value, opername, cast);

  Error("illegal assignment");

  return 0;					/* pour le compilo */
}


/* x-- x++

  fct = make_post_incrdecr
  info = (void **) [(List *) place, (int) -1/+1]

  place peut etre
  	(fct) make_identifier
  	(fct) make_oper_asterisk
	(fct) make_array_access
*/


/* Modification d'une (supposee) variable */

Instruction * var_post_incrdecr(mkident, ivalue, cast)
     List * mkident;
     int    ivalue;
     Type * cast;
{
  Object ident = make_identifier(List__List(0, Strdup(mkident->info)));
  List ** m = (List **) Malloc(3 * sizeof(List *));
  Instruction * value;

  if (Type__IsaArray(GetExprType((Instruction *) ident))) {
    sprintf(err_msg, "illegal assignment %s%s ..", (char *) mkident->info,
	    (*((char *)m[1]) == '-') ? "--" : "++");
    Error(err_msg);
  }

  /* fabrique l'affectation */

  ((char **) m)[0] = "+";
  if (cast) {
    void ** cm = (void **) Malloc(2 * sizeof(void *));

    cm[0] = (void *) cast;
    cm[1] = (void *) mkident;
    m[1] = List__List(make_cast, (void *) cm);
  }
  else
    m[1] = mkident;
  m[2] = List__List(make_integer, (void *) ivalue);
  value = make_binary_oper(List__List(0, m));
  
  if (IsaGlobalVar((Instruction *) ident))
    return (Instruction *)
      GlobalVarPostIncrDecr__GlobalVarPostIncrDecr
		((GlobalVar *) ident, value, cast);
  else
    return (Instruction *)
      LocalVarPostIncrDecr__LocalVarPostIncrDecr
		((LocalVar *) ident, value, cast);
}


Instruction * make_post_incrdecr(l)
     List * l;
{
  List ** m = (List **) l->info;
  List * place = m[0];
  int value = (int) m[1];
  Type * cast = 0;

  free(m);
  free(l);

  if (place->fct == make_cast) {
    /* Seul le cast englobant est utile lorsqu'on est a gauche de l'affect */
    cast = ((Type **) place->info)[0];
    do {
      List * pl = ((List**) place->info)[1];
      
      /* liberer le type = ((Type **) place->info)[0] */
      free(place);
      place = pl;
    } while (place->fct == make_cast);
  }
  
  if (place->fct == ((InstrFct) make_identifier))
    return var_post_incrdecr(place, value, cast);

  if (place->fct == ((InstrFct) make_oper_asterisk))
    return indirection_post_incrdecr(place, value, cast);

  if (place->fct == ((InstrFct) make_array_access))
    return array_post_incrdecr(place, value, cast);

  Error("illegal assignment");

  return 0;					/* pour le compilo */
}
