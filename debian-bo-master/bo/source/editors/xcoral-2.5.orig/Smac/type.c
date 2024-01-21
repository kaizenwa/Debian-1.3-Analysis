/* ########################################################################

				 type.c

   File: type.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/type.c
   Description: 
   Created: Tue Feb 21 13:02:16 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 13:02:17 MET 1995
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

#include "type.h"
#include "error.h"
#include "list.h"
#include "memo.h"
#include "decl.h"
#include "Const.h"

Type * make_typedef(type)
     Type * type;
{
  return (Type *) type;
}


/* Retourne le type et detruit l */

Object asterisk = (Object) "asterisk";

static FCT(Type *, pointeur_ou_tableau_de_fonction,(List *, List **));
static FCT(Type *, pointeur_ou_tableau_de_tableau,(List *, List **));

Type * abstract_type(l)
     List * l;
{
  Type * result;
  List * p;

  for (p = l; p && (p->fct != (InstrFct) preserve_list); p = p->next);

  if (p) {
    /* Description de type complexe :

       int (*)()     : & ( int .() )      : pt sur fonc
       int (*[1])()  : .[1] = &(int . ()) : vect de pt de fonc
       
       int (*)[1]    : & ( int . [1] )    : pt sur vect d'int
       int (*[1])[2] : .[1] = &(int .[2]) : vect de pt de vect d'int

       Note : les fonctions retournant un type complexe ont ete traitees par
              l'appelant, il n'y a donc pas ici de cas tel que  int (*f())()
    */

    if (((Object) ((List *) p->info)->fct) != asterisk)
      Internal_Error("I cannot understand the type description");
    
    if (p->next->fct == (InstrFct) function_parameter_type_list) {
      result = pointeur_ou_tableau_de_fonction(p, &l);
    }

    else if (p->next->fct == (InstrFct) array_dims) {
      result = pointeur_ou_tableau_de_tableau(p, &l);
    }
    
    else
      Internal_Error("I cannot understand the type description");
  }

  while(l) {
    List * suiv = l->next;
    if (l->fct == (InstrFct) make_typedef) {
      /* un type builtin */
      result = (Type *) l->info;
      free(l);
      l = suiv;
    }
    else if (((Object) l->fct) == asterisk) {
      /* * */
      result = Type__Type(result, T_Pointer, sizeof(void *));
      free(l);
      l = suiv;
    }
    else if (l->fct == (InstrFct) array_dims) {
      List * d = (List *) l->info;

      while(d) {
	List * prev = d->previous;

	if (! d->fct) {
	  Internal_Error("[] not yet implemented");
	  free(d);
	}
	else {
	  Instruction * i = MAKE(d);
	  int dim = Eval(i);

	  free(i);
	  if (dim <= 0) {
	    sprintf(err_msg, "%d illegal array dimension", dim);
	    Error(err_msg);
	  }
	  result = Type__Type(result, T_Array, dim * Type__Sizeof(result));
	}

	d = prev;
      }
      free(l);
      l = suiv;
    }
    else
      Error("bad type declaration");
  }

  return result;
}


static Type * pointeur_ou_tableau_de_fonction(p, l)
     List * p;
     List ** l;
{
  /*
       int (*)()     : & ( int .() )      : pt sur fonc
       int (*[1])()  : .[1] = &(int . ()) : vect de pt de fonc

       p pointe sur (*..), *l sur tout

       retourne le type fonction pointe, et change l en les []
  */
  
  TypeFunction * typefunction =
    (TypeFunction *) Malloc(sizeof(TypeFunction));
  List * params = (List *) p->next->info;
  Type ** pargtypes;
  Type * result;

  typefunction->_nbre_arg = list_length(params);
  free(p->next);
  pargtypes = (Type **) Malloc(typefunction->_nbre_arg * sizeof(Type *));
  typefunction->_argtypes = pargtypes;
  while (params) {
    /* ellipsis (...) actuellement ecarte dans lex */
    List * suite = params->next;
	
    *pargtypes++ = (Type *) ((Object) params->fct);
    if (params->info)				/* le nom de la var */
      free(params->info);
    free(params);
    params = suite;
  }
  result = Type__Type(typefunction, T_Function, sizeof(void **));
  p->previous->next = 0;
  typefunction->_valtype = abstract_type(*l);
  *l = ((List *) p->info)->next;		/* les [] s'ils existent */
  free(p);

  return result;
}

static Type * pointeur_ou_tableau_de_tableau(p, l)
     List * p;
     List ** l;
{
  /*
       int (*)[1]    : & ( int . [1] )    : pt sur vect d'int
       int (*[1])[2] : .[1] = &(int .[2]) : vect de pt de vect d'int

       p pointe sur (*..), *l sur tout

       retourne le type tableau pointe, et change l en les []
  */

  Type * result;
  
  p->previous->next = p->next;			/* retire (*..) */
  result = abstract_type(*l);

  *l = (List *) p->info;

  return result;
}


/* Retourne une chaine de caract correspondant a un
   type pour s'en servir de clef d'une hash table */

char * encode_type(t)
     Type * t;
{
  if (Type__IsBuiltin(t))
    return Type__Name(t);

  if ((Type__IsaFunction(t)) || (Type__Pointed_Type(t) != Type_Char))
    return "<void *>";

  return "<char *>";
}


/* Sizeof 

   fct = make_sizeof_type
   info = Type * type
   */

Instruction * make_sizeof_type(l)
     List * l;
{
  Instruction * result = (Instruction *)
    Int__Int(Type__Sizeof((Type *) l->info));

  /* liberer le type si pointeur .. */

  free(l);

  return result;
}
