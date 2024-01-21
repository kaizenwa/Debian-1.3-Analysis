/* ########################################################################

				 memo.c

   File: memo.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/memo.c
   Description: 
   Created: Tue Feb 21 12:57:26 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:57:27 MET 1995
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
#include <memory.h>

#include "memo.h"
#include "Instruction.h"
#include "decl.h"
#include "type.h"
#include "declaration.h"
#include "comma.h"
#include "switch.h"
#include "error.h"

/*********
  
 Les blocs
 
 *********/

/*
  fct = make_block / make_block_without_decl
  info = (List *) 1ere expression, les suivantes par next
*/


/* marque de debut de block */

Object begin_block = (Object) "begin_block";

FCT(    Instruction *, make_block,(List *));

/* Un bloc non vide */

void memo_block()
{
  do { Memo = Memo->previous; } while (((Object) Memo->info) != begin_block);

  Memo->fct = make_block;
  Memo->info = (void *) Memo->next;
  Memo->next->previous = 0;
  Memo->next = 0;
}


/* Un block completement vide (!) */

void memo_empty_block()
{
  Memo->fct = make_block;
  Memo->info = 0;
}


/**********

 Les commas

 *********/

/*
  fct = make_comma
  info = (char **) [nbre_elt, elt1, ... eltn] avec elti : List *, nbre_elt > 0
*/

void memo_comma()
{
  List * e1, * e2;
  char ** m;

  PopLast(Memo, e2);
  PopLast(Memo, e1);

  if (e1->fct == make_comma)
    if (e2->fct == make_comma) {
      /* ((c1, ..., cn), e2 = (sc1, ..,. scm)) -> (c1, ..., cn, sc1, ..,. scm)
	 ou (c1, c2, e2 = (sc1, ..,. scm)) -> (c1, c2, cn, sc1, ..,. scm) */
      int n1 = (int) *((char **) (e1->info));
      int n2 = (int) *((char **) (e2->info));

      m = (char **) Malloc((n1 + n2 + 1) * sizeof(void *));
      m[0] = (char *) (n1 + n2);
      memcpy((char *) &m[1], (char *) ((char **) (e1->info)) + 1,
	     n1 * sizeof(void *));
      memcpy((char *) &m[n1 + 1], (char *) ((char **) (e2->info)) + 1,
	     n2 * sizeof(void *));
    }
    else {
      /* ((c1, ..., cn), e2 = c) -> (c1, ..., cn, c)
	 ou (c1, c2, ..., e2 = cn) -> (c1, c2, ..., cn) ! */
      int n1 = (int) *((char **) (e1->info));

      m = (char **) Malloc((n1 + 2) * sizeof(void *));
      memcpy((char *) m, e1->info, (n1 + 1) * sizeof(void *));
      m[(int) (m[0] += 1)] = (char *) e2;
    }
  else
    if (e2->fct == make_comma) {
      /* (c1, e2 = (sc1, ..,. scn)) -> (c1, sc1, ..., scn) */
      int n2 = (int) *((char **) (e2->info));

      m = (char **) Malloc((n2 + 2) * sizeof(void *));
      m[0] = (char *) (n2 + 1);
      m[1] = (char *) e1;
      memcpy((char *) &m[2], (char *) ((char **) (e2->info)) + 1,
	     n2 * sizeof(void *));
    }
    else {
      /* (c1, e2 = c2) */
      m = (char **) Malloc(3 * sizeof(void *));
      m[0] = (char *) 2;
      m[1] = (char *) e1;
      m[2] = (char *) e2;
    }

  AddLast(Memo, List__List(make_comma, m));
}


/*********************
  
 Initialisation = {..}

 ********************/

/*
  fct = make_initializer / make_block_without_decl
  info = (List *) 1ere expression, les suivantes par next
*/

Object begin_initializer_list = (Object) "begin_initializer_list";

void memo_initializer_list()
{
  do { Memo = Memo->previous; }
  while (((Object) Memo->info) != begin_initializer_list);

  Memo->fct = make_initializer_list;
  Memo->info = (void *) Memo->next;
  Memo->next->previous = 0;
  Memo->next = 0;
}


/*****************
  
 Appel fonctionnel

 ****************/

Object first_arg = (Object) "first_arg";
Object one_arg = (Object) "one_arg";
Object some_args = (Object) "some_args";

/*
  fct = make_funcall
  info = (char **) [nbrearg, fct, arg1 ..] avec argi,fct : List *, nbrearg >= 0
*/

void memo_funcall_nargs()
{
  List ** m;

  if (((Object) Memo->info) == some_args) {
    int narg = 1;
    List * l = Memo->previous;
    List ** p;

    List__Delete(Memo);
    Memo = l;
    
    do {
      l = l->previous;
      narg += 1;
    } while (((Object) l->info) != first_arg);

    p = (m = (List **) Malloc((narg + 2) * sizeof(List *))) + narg + 1;

    do {
      PopLast(Memo, *p);
      p -= 1;
    } while (Memo != l);

    Memo = Memo->previous;	/* arg 1 */
    List__Delete(l);		/* first_arg */

    PopLast(Memo, *p);
    PopLast(Memo, m[1]);	/* la fonction */
    m[0] = (List *) narg;
  }
  else {
    /* un arg */
    List * l = Memo->previous;
    
    List__Delete(Memo);	/* one_arg */
    Memo = l;
    
    m = (List **) Malloc(3 * sizeof(List *));

    PopLast(Memo, m[2]);	/* l'arg */
    PopLast(Memo, m[1]);	/* la fonction */
    m[0] = (List *) 1;
  }

  AddLast(Memo, List__List(make_funcall, m));
}


/************

    Switch 

 ***********/


/* fct = make_switch
   info = List ** [clef, exprs]
   */

void memo_switch()
{
  List ** m = (List **) Malloc(2 * sizeof(List *));
  
  if (Memo->fct != make_block) {
    /* la forme est switch (..) [cas1: .. casn:] expr; */
    
    do { Memo = Memo->previous; } while (Memo->fct == make_case);

    PopLast(Memo, m[0]);
    if ((m[1] = m[0]->next) != 0) {
      m[1]->previous = 0;
      m[0]->next = 0;
    }
  }
  else  {
    /* La forme est switch (..) {..} */
    List * bloc;
    
    PopLast(Memo, bloc);
    PopLast(Memo, m[0]);
    m[1] = (List *) bloc->info;
    free(bloc);
  }

  AddLast(Memo, List__List((InstrFct) make_switch, m));
}


/************

 Declarations

 ***********/

/*
  Memorisation des () necessaires dans les indications de type

  fct = preserve_list
  info = List * 1ere expr, les suivantes par next
*/
   

Object begin_of_list = (Object) "begin_of_list";

void preserve_list() { /* sert juste de marque */ }

void forget_list()
{
  /* retire le dernier begin_of_list (non ferme) */

  List * l;

  for (l = Memo; ((Object) l->info) != begin_of_list; l = l->previous)
    ;

  Memo = List__Remove(l, Memo);
  List__Delete(l);
}

void forget_list_if_possible()
{
  /* Retire begin_of_list s'il n'y a qu'un element apres */

  if (((Object) Memo->previous->info) == begin_of_list) {
    List * l = Memo->previous;

    List__Remove(l, Memo);
    List__Delete(l);
  }
  else
    /* on a (*v ou (v[.] etc ... on doit memoriser le () */
    end_of_list();
}

void end_of_list()
{
  /* Memorise la liste */
  
  do { Memo = Memo->previous;} while (((Object) Memo->info) != begin_of_list);

  Memo->info = (void *) Memo->next;
  Memo->fct = (InstrFct) preserve_list;
  Memo->next->previous = 0;
  Memo->next = 0;
}

/**/

/* Regroupe les declarations des dimensions des tableaux

   fct = array_dims
   info = List * derniere dim, les precedantes par previous

   chaque dim est : fct = info = 0 si pas d'indication de dimension
                    fct, info : fabrication d'un int ou d'un char sinon
*/

void array_dims() { /* sert juste de marque */ }
void array_dim() { /* sert juste de marque */ }

void array_abstract_declarator()
{
  /* Est precede d'au moins une List * avec fct = array_dim, info = la dim */

  List * l;
  List * last = Memo;

  do {
    if (Memo->info) {
      /* [ dim ] */
      List * d = (List *) Memo->info;
      
      Memo->fct = d->fct;
      Memo->info = d->info;
      List__Delete(d);
    }
    else
      /* [ ] */
      Memo->fct = 0;

    Memo = Memo->previous;
  } while (Memo->fct == ((InstrFct) array_dim));

  l = List__List((InstrFct) array_dims, last);
  Memo->next->previous = 0;
  Memo->next = 0;

  AddLast(Memo, l);
}


/**/

/* Enleve dans Memo une indication de type et la retourne */

Type * the_abstract_type()
{
  List * ltype;
  
  while (Memo->fct != (InstrFct) make_typedef) Memo = Memo->previous;
  ltype = Memo;

  Memo = Memo->previous;
  if (Memo) Memo->next = 0;
  ltype->previous = 0;

  return abstract_type(ltype);
}


/* Enleve dans Memo une declaration de type de variable,
   retourne le type et memorise le nom de la variable via varname */

Type * the_typed_var(varname)
     char ** varname;
{
  List * ltype;
  
  while (Memo->fct != (InstrFct) make_typedef) {
    if (Memo->fct == (InstrFct) make_identifier) {
      List * var = Memo;
      *varname = (char *) var->info;

      /* Retire la var */
      Memo = Memo->previous;
      List__Remove(var, 0);
      free(var);
    }
    else {
      if (Memo->fct == (InstrFct) preserve_list) {
	/* L'identifier est dedans */
	List * l = (List *) Memo->info;
	
	while (l->fct != (InstrFct) make_identifier)
	  l = l->next;
	*varname = (char *) l->info;

	/* retire la var */
	if (l == (List *) Memo->info)
	  Memo->info = (void *) l->next;
	else
	  List__Remove(l, 0);
	free(l);
      }
      Memo = Memo->previous;
    }
  }

  /* Memo pointe sur le debut de l'indic de type */
  
  ltype = Memo;
  if ((Memo = Memo->previous) != 0) {
    Memo->next = 0;
    ltype->previous = 0;
  }

  return abstract_type(ltype);
}


/**/

Object begin_identifier_list = (Object) "begin_identifier_list";
Object begin_function_parameter_type_list =
  (Object) "begin_function_parameter_type_list";

char * ellipsis = "ellipsis";

/*
  Lien entre un nom de param et son type :

  fct = 0 lorsque le type n'est pas encore connu car vieille syntaxe ou ...
  	Type * le_type
  info = 0 lorsqu'il n'y a pas de variable associee (param ignore)
         char * le nom de la variable ou ellipsis
*/

/* Il n'y a qu'un type, pas de nom de var */

void ignored_param()
{
  Object vartype = (Object) the_abstract_type();
  
  /* ne PAS ecrire AddLast(Memo, List__List(the_abstract_type(), 0))
     car Memo est modifie par the_abstract_type */
  AddLast(Memo, List__List((InstrFct) vartype, 0));
}


/* le nom du param et son type */

void typed_param()
{
  char * varname;
  Object vartype = (Object) the_typed_var(&varname);

  AddLast(Memo, List__List((InstrFct) vartype, varname));
}


/*
  Liste des liens param-type d'une fonction a definir ou declaree :

  fct = end_function_parameter_type_list
  info = List * 1er param-type, les suivants par next
*/

void function_parameter_type_list() { /* juste une marque */ }

/* Nouvelle syntaxe, contient des ignored/typed_params */

void end_function_parameter_type_list()
{
  while (((Object) Memo->info) != begin_function_parameter_type_list)
    Memo = Memo->previous;

  Memo->info = (void *) Memo->next;
  Memo->fct = (InstrFct) function_parameter_type_list;
  Memo->next->previous = 0;
  Memo->next = 0;
}


/* les noms des params sans leurs types : vieille syntaxe */

void end_identifier_list()
{
  /* Transforme les identifiers en var-type : type = 0 car encore inconnu */
  while (((Object) Memo->info) != begin_identifier_list) {
    Memo->fct = 0;
    Memo = Memo->previous;
  }

  Memo->info = (void *) Memo->next;
  Memo->fct = (InstrFct) function_parameter_type_list;
  Memo->next->previous = 0;
  Memo->next = 0;
}


/**/

/* Reutilisation d'un type, ex) int i, j, ...;

   partial_declaration est appelee lors de la prise en compte de la ','
   la declaration qui precede n'est pas encore traitee. On va chercher
   l'indication de type (le premier typedef) afin de l'ajouter en fin
   de liste, de plus la declaration qui precede est traitee.
   L'ajout du type permet de traiter les partial_declaration suivants
   et le declaration final.
   A noter que declaration traite egalement la declaration qui precede.
   */

/*
  Chaque declaration a le format :

  fct = declaration
  info = (char **) [List * : var, List * : type, List * init]
*/

Object no_initializer = (Object) "no_initializer";

void partial_declaration()
{
  List * l = Memo;
  void * type;

  do { l = l->previous; } while (l->fct != (InstrFct) make_typedef);

  type = l->info;				/* il y aura un free */

  declaration();

  AddLast(Memo, List__List((InstrFct) make_typedef, type));
}

/* */

void declaration()
{
  void ** m = (void **) Malloc(3 * sizeof(void *));

  PopLast(Memo, m[2]);				/* initialisation */
  
  if (((Object) ((List *) m[2])->info) == no_initializer)
    m[2] = 0;

  if ((Memo->previous->fct == (InstrFct) preserve_list) &&
      ((List *) Memo->previous->info)->next &&
      ((List *) Memo->previous->info)->next->next &&
      (((List *) Memo->previous->info)->next->next->fct ==
       (InstrFct) function_parameter_type_list)) {
    
    /*
       Une declaration de fonction complexe de la forme
          <type> (*f(..))[..]..[..] : fct retournant un pt sur <type>
       ou <type> (*f(..))(..)       : fct ret. un pt sur fct ret. un <type>
     */
    
    if (m[2])
      Error("function initialized as a variable");
    m[0] = ((List*) Memo->previous->info)->next->info;		/* nom */
    m[1] = ((List*) Memo->previous->info)->next->next->info;	/* params */

    /* Liberation des listes inutile qui contenait le nom et les params */

    free(((List*) Memo->previous->info)->next->next);
    free(((List*) Memo->previous->info)->next);
    ((List*) Memo->previous->info)->next = 0;

    /* maintenant Memo pointe sur <type> (*)[..]..[..] ou <type> ()(..) */
    
    m[2] = (void *) the_abstract_type();		/* type retourne */
    
    AddLast(Memo, List__List(make_function_declaration, m));
  }
  
  else if ((Memo->fct == (InstrFct) function_parameter_type_list) &&
	   (Memo->previous->fct == (InstrFct) make_identifier)) {
    
    /*
       Une declaration de fonction simple
       info = les params
       previous->fct = make_identifier
       previous->previous ... le type retourne (oblig)
    */
    
    if (m[2])
      Error("function initialized as a variable");
    m[0] = Memo->previous->info;				/* nom */
    m[1] = Memo->info;					/* params */

    /* Liberation des listes inutile qui contenait le nom et les params */
    
    Memo = Memo->previous->previous;
    free(Memo->next->next);
    free(Memo->next);
    Memo->next = 0;
    m[2] = (void *) the_abstract_type();		/* type retourne */
    
    AddLast(Memo, List__List(make_function_declaration, m));
  }
  
  else {
    
    /* Une declaration de variable */
    
    m[1] = (void *) the_typed_var((char **) &m[0]);
    
    AddLast(Memo, List__List(make_declaration, m));
  }
}
