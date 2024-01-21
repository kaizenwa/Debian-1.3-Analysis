/* ########################################################################

			       function.c

   File: function.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/function.c
   Description: 
   Created: Tue Feb 21 12:54:15 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:54:16 MET 1995
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

#include "function.h"
#include "memo.h"
#include "Function.h"
#include "type.h"
#include "identifier.h"
#include "error.h"
#include "declaration.h"


/* Le type retourne par la fonction en cours de definition */

Type * Return_Value_Type = 0;


/* La hash table permettant de retrouver les variables locales */

HashTable * DynamicVarsHashTable;


/* La position des vars locales dans la pile */

int DynamicVar_FramePosition;

/***************************
  
  Declaration d'une fonction

  *************************/

void function_declaration(identifier, params, valtype)
     Identifier * identifier;
     List * params;
     Type * valtype;
{
  /* Verifie qu'il n'y a pas deja de definition associee */
  
  if (Identifier__GlobalVar(identifier)) {
    sprintf(err_msg, "%s already defined (global var)",
	    Identifier__Name(identifier));
    Error(err_msg);
  }

  if (Identifier__FunctionDef(identifier)) {

    /* Identifier est deja une fonction,
       verifie que les definitions concordent */

    Function * func = Identifier__FunctionDef(identifier);
    int paramrank;
    int nparams;
    
    if (! (Type__Equal(valtype, Function__ValType(func)))) {
      sprintf(err_msg, "%s already defined with different return type",
	      Identifier__Name(identifier));
      Error(err_msg);
    }

    nparams = Function__NbreArg(func);
    paramrank = 0;

    for (;;) {
      if (paramrank == nparams) {
	if (! params)
	  return;
      }
      else if (params &&
	       Type__Equal((Type *) ((Object) params->fct),
			   Function__ArgType(func, paramrank))) {
	paramrank += 1;
	params = params->next;
	continue;
      }
      {
	sprintf(err_msg, "%s already declared with different profile",
		Identifier__Name(identifier));
	Error(err_msg);
      }
    }
  }
  else {

    /* Premiere declaration : cree une fonction indefinie */

    int nparams = list_length(params);
    Type ** argtypes;
    Type ** pargtypes;

    pargtypes = argtypes = (Type **) Malloc(nparams * sizeof(Type *));

    for (; params; params = params->next)
      *pargtypes++ = (Type *) ((Object) params->fct);

    Identifier__FunctionDef(identifier) =
      Function__Function(Identifier__Name(identifier), valtype, nparams,
			 argtypes, 0);
  }
}


/***************************
  
  Definition d'une fonction

  *************************/

void function_definition(valtype, funcname, params, body)
     Type * valtype;
     char * funcname;
     List * params;
     List * body;
{
  Identifier * identifier;
  
  {
    List * d = params;
    
    params = (List *) params->info;
    free(d);				/* function_parameter_type_list */
  }

  /* L'Identifier */
  
  identifier = Get_Identifier(funcname);
  
  /* Declare la fonction si ce n'est deja fait */
    
  function_declaration(identifier, params, valtype);

  if (! Function__IsUndefined(Identifier__FunctionDef(identifier)))
    if (Function__IsBuiltin(Identifier__FunctionDef(identifier))) {
      sprintf(err_msg, "%s is builtin, you cannot redefine it", funcname);
      Error(err_msg);
    }
    else if (Debug_Mode) {
      /* Verifie que la fonction n'est pas en cours d'execution */
      Function__VerifyNotActive(Identifier__FunctionDef(identifier));

      if (Debug_Mode > 1) {
#ifdef XCORAL
	extern FCT(void, DisplayWMessage ,(char *, char *, int));
	
	DisplayWMessage("Warning : ", "", 1);
	DisplayWMessage(funcname, "", 0);
	DisplayWMessage(" is redefined\n", "Smac Redefinition [see debug_mode()]", 0);
#else
	fprintf(stderr, "Warning : %s is redefined [see debug_mode()]\n", funcname);
#endif
      }
      
      /* liberer la definition */
    }
    else {
      sprintf(err_msg, "%s already defined", funcname);
      Error(err_msg);
    }
  
  /* Les parametres deviennent des variables locales */
  
  DynamicVar_FramePosition = 0;

  while (params) {
    /*
      fct = Type * le_type
      info = 0 lorsqu'il n'y a pas de variable associee (param ignore)
             le nom de la variable ou ellipsis
    */
    if (params->info)
      HashTable__Add(DynamicVarsHashTable,
		     (Object) params->info,
		     (Object) LocalVar__LocalVar
		     		(params->info,
				 DynamicVar_FramePosition,
				 (Type *) ((Object) params->fct)));

    DynamicVar_FramePosition +=
      Type__SizeInWord((Type *) ((Object) params->fct));
    
    {
      List * d = params;
      
      params = params->next;
      free(d);
    }
  }

  /* Mise a jour de la definition */

  Return_Value_Type = valtype;
  Function_Name = funcname;
  
  Function__UpdateDef(Identifier__FunctionDef(identifier), MAKE(body));

  Return_Value_Type = 0;
  Function_Name = 0;

  /* on oublie toutes les variables locales */

  HashTable__Empty(DynamicVarsHashTable);
}


/**********************************
  Fonctions de fabrication a partir
  du contenu de la pile Memo
  ********************************/


/* Definition d'une fonction suivant la nouvelle syntaxe */

int make_typedef_function_def()
{
  /* Memo = 	son corps
     		ses parametres
		son identifier et son type (non traites)
		
        ou ex)	int (*f(<param de f>))(<params de la fonction retournee>) {..}
	  
	  	son corps
		les parametres de la fonction qu'elle retourne
		son nom et ses parametres
		le type (non traite) retourne par la fonction retournee

        ou ex)	int (*f(<param de f>))[..]..[..] {..}
	  
	  	son corps
		les dimensions du tableau qu'elle retourne
		son nom et ses parametres
		le type (non traite) retourne par le tableau
  */

  List * body;
  List * params;
  Type * valtype;
  char * funcname = 0;

  PopLast(Memo, body);

  if (Memo->previous->fct == (InstrFct) preserve_list)
    if (! (Memo->previous->info &&
	   ((List *) Memo->previous->info)->next &&
	   (params = ((List *) Memo->previous->info)->next->next)))
      Error("Bad function definition");
    else {
      params->previous->next = params->next;	/* theo 0 */
      params->previous = 0;
      params->next = 0;
    }
  else
    PopLast(Memo, params);

  valtype = the_typed_var(&funcname);
  if (! funcname)
    Error("Bad function definition");

  function_definition(valtype, funcname, params, body);

  return 0;
}


/* Definition d'une fonction suivant la nouvelle
   syntaxe retournant un int par defaut */

int make_function_def()
{
  /* Memo = 	son corps
     		ses parametres
		son identifier (non traite)
		
       ou ex)   (*f(<param de f>))(<params de la fonction retournee>) {..}
	  
	  	son corps
		les parametres de la fonction qu'elle retourne
		son nom et ses parametres
		
       ou ex)	(*f(<param de f>))[..]..[..] {..}
       
	  	son corps
		les dimensions du tableau qu'elle retourne
		son nom et ses parametres
  */

  if (! Memo->previous->previous)
    Error("Bad function definition");
  
  List__InsertBefore(Memo->previous->previous,
		       List__List((InstrFct) make_typedef, Type_Int));
  
  return make_typedef_function_def();
}


/* Definition d'une fonction suivant l'ancienne syntaxe
   avec ses parametres declares int par defaut */

int make_typedef_oldfunction_def_without_decllist()
{
  /* Memo = 	son corps
     		ses parametres (au moins 1)
		son identifier et son type (non traites)

       ou ex)   int (*f<vars de f>)(<params de la fonction retournee>) {..}

       		son corps
		les parametres de la fonction qu'elle retourne (nouv. syntax)
		son nom et ses vars (non typees)
		le type (non traite) retourne par la fonction retournee

        ou ex)	int (*f(<vars de f>))[..]..[..] {..}
	  
	  	son corps
		les dimensions du tableau qu'elle retourne
		son nom et ses vars (non typees)
		le type (non traite) retourne par le tableau
  */

  List * body;
  List * params;
  Type * valtype;
  char * funcname;

  PopLast(Memo, body);

  if (Memo->previous->fct == (InstrFct) preserve_list)
    if (! (Memo->previous->info &&
	   ((List *) Memo->previous->info)->next &&
	   (params = ((List *) Memo->previous->info)->next->next)))
      Error("Bad function definition");
    else {
      params->previous->next = params->next;    /* theo 0 */
      params->previous = 0;
      params->next = 0;
    }
  else
    PopLast(Memo, params);

  /* les params sont de type int par defaut : */
  {
    List * lparams = (List *) params->info;

    do {
      lparams->fct = (InstrFct) ((Object) Type_Int);
      lparams = lparams->next;
    } while (lparams);
  }

  valtype = the_typed_var(&funcname);

  function_definition(valtype, funcname, params, body);

  return 0;
}


/* Definition d'une fonction retournant int par defaut suivant
   l'ancienne syntaxe avec ses parametres declares int par defaut */

int make_oldfunction_def_without_decllist()
{
  /* Memo = 	son corps
     		ses parametres (au moins 1)
		son identifier (non traite)

       ou ex) (*f<vars de f>)(<params de la fonction retournee>) {..}

       		son corps
		les parametres de la fonction qu'elle retourne (nouv. syntax)
		son nom et ses vars (non typees)

        ou ex)	(*f(<vars de f>))[..]..[..] {..}
	  
	  	son corps
		les dimensions du tableau qu'elle retourne
		son nom et ses vars (non typees)
  */

  List__InsertBefore(Memo->previous->previous,
		       List__List((InstrFct) make_typedef, Type_Int));
  
  return make_typedef_oldfunction_def_without_decllist();
}


/* Definition d'une fonction suivant l'ancienne syntaxe */

int make_typedef_oldfunction_def()
{
  /* Memo = 	son corps
     		les declarations des params avec leurs types (traites)
     		ses parametres non types (au moins 1)
		son identifier et son type (non traite)

       ou ex) int (*f<vars de f>)(<params de la fonction retournee>)
       			<param1 de f>; .. {..}

       		son corps
     		les declarations des params avec leurs types (traites)
		les parametres de la fonction qu'elle retourne (nouv. syntax)
		son nom et ses vars (non typees)
		le type (non traite) retourne par la fonction retournee

        ou ex)	int (*f(<vars de f>))[..]..[..]
       			<param1 de f>; .. {..}
	  
	  	son corps
     		les declarations des params avec leurs types (traites)
		les dimensions du tableau qu'elle retourne
		son nom et ses vars (non typees)
		le type (non traite) retourne par le tableau

  Chaque declaration a le format :
		fct = declaration
		info = (char **) [char * : varname, Type * : type, List * 0]
  */

  List * body;
  List * params;
  Type * valtype;
  char * funcname;
  List * p;

  PopLast(Memo, body);

  /* Type les parametres */

  params = Memo;
  
  do { params = params->previous; }
  while (params->fct == make_declaration);
  
  if (params->previous->fct == (InstrFct) preserve_list)
    if (! (params->previous->info &&
	   ((List *) params->previous->info)->next &&
	   (params = ((List *) params->previous->info)->next->next)))
      Error("Bad function definition");
    else {
      params->previous->next = params->next;    /* theo 0 */
      params->previous = 0;
      params->next = 0;
    }

  /* Type les params a partir des types fournis */
  
  do {
    List * decl;

    PopLast(Memo, decl);
    for (p = (List *) params->info;
	 p && strcmp(p->info, ((char **) decl->info)[0]);
	 p = p->next)
      ;
    if (! p) {
      sprintf(err_msg, "missing declared argument %s",
	      ((char **) decl->info)[0]);
      Error(err_msg);
    }
    free(((char **) decl->info)[0]);
    if (p->fct) {
      sprintf(err_msg, "redeclaration of %s", (char *) p->info);
      Error(err_msg);
    }
    p->fct = (InstrFct) ((Object) ((char **) decl->info)[1]);
    free(decl->info);
    free(decl);
  } while (Memo->fct == make_declaration);

  /* Type int par defaut ceux qui ne sont pas encore ete */

  for (p = (List *) params->info; p; p = p->next) {
    List * pp;

    for (pp = p->next; pp; pp = pp->next)
      if (! strcmp(p->info, pp->info)) {
	sprintf(err_msg, "redeclaration of %s", (char *) p->info);
	Error(err_msg);
      }
    if (! p->fct)
      p->fct = (InstrFct) ((Object) Type_Int);
  }

  if (params == Memo)
    /* Type simple, les params sont encore empiles */
    PopLast(Memo, params);
  
  valtype = the_typed_var(&funcname);

  function_definition(valtype, funcname, params, body);

  return 0;
}


/* Definition d'une fonction retournant int par
   defaut suivant l'ancienne syntaxe */

int make_oldfunction_def()
{
  /* Memo = 	son corps
     		les declarations des params avec leurs types (traites)
     		ses parametres non types (au moins 1)
		son identifier (non traite)

       ou ex) (*f<vars de f>)(<params de la fonction retournee>)
       			<param1 de f>; .. {..}

       		son corps
     		les declarations des params avec leurs types (traites)
		les parametres de la fonction qu'elle retourne (nouv. syntax)
		son nom et ses vars (non typees)

        ou ex)	(*f(<vars de f>))[..]..[..]
       			<param1 de f>; .. {..}
	  
	  	son corps
     		les declarations des params avec leurs types (traites)
		les dimensions du tableau qu'elle retourne
		son nom et ses vars (non typees)

  Chaque declaration a le format :
		fct = declaration
		info = (char **) [List * : var, List * : type, List * init]
  */
  
  List__InsertBefore(Memo->previous->previous->previous,
		       List__List((InstrFct) make_typedef, Type_Int));
  
  return make_typedef_oldfunction_def();
}


/**/

void Init_function()
{
  DynamicVarsHashTable = HashTable__HashTable(10, 1);
}

