/* ########################################################################

			     declaration.c

   File: declaration.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/declaration.c
   Description: 
   Created: Tue Feb 21 12:52:35 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:52:35 MET 1995
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

#include "declaration.h"
#include "Identifier.h"
#include "function.h"
#include "error.h"
#include "Declaration.h"

/*
  fct = make_declaration
  info = [varname, type (traite), initial_value / 0]
  */

static Instruction * global_initialized_array(type, init, varname)
     Type * type;
     List * init;
     char * varname;
{
  if (init->fct != make_initializer_list) {
    sprintf(err_msg, "illegal array initializer for %s", varname);
    Error(err_msg);
  }

  /* initialisation par { .. } */
  Error("array initialization not yet implemented");
}

Instruction * make_declaration(l)
     List * l;
{
  char * varname = ((char **) l->info)[0];
  Type * type = ((Type **) l->info)[1];
  Instruction * initval = 0;
  ConvFct convert = 0;

  if (((List **) l->info)[2])
/*    if (Type__IsaArray(type) && (! Return_Value_Type)) */
    if (Type__IsaArray(type) && (! Function_Name))
      initval =
	global_initialized_array(type, ((List **) l->info)[2], varname);
    else {
      initval = MAKE(((List **) l->info)[2]);
      if (! (convert = ExprType__Convertible(initval, type))) {
	sprintf(err_msg, "incompatible initial value type for %s ",
		varname);
	Type__Print(type, err_msg);
	strcat(err_msg, " <> ");
	Type__Print(GetExprType(initval), err_msg);
	Error(err_msg);
      }
      else if (convert == AlreadyConvertible)
	convert = 0;
    }

/*  if (! Return_Value_Type) { */
    if (! Function_Name) {

    /*******************
      
     Declaration globale
     
     ******************/

    Identifier * ident = Get_Identifier(varname);
    Object initvalue = 0;
    int redefine = 0;
    
    free(l->info);
    free(l);

    if (Identifier__FunctionDef(ident)) {
      sprintf(err_msg, "%s is already a function", varname);
      Error(err_msg);
    }
    if (Identifier__GlobalVar(ident))
      if (Debug_Mode) {
	Type * oldtype = GetExprType(Identifier__GlobalVar(ident));

	if ((Type__Sizeof(type) != Type__Sizeof(oldtype)) ||
	    (! Type__Equal(type, oldtype))) {
	  sprintf(err_msg,
		  "%s is already a global var with different definition",
		  varname);
	  Error(err_msg);
	}
	else if (Debug_Mode > 1) {
#ifdef XCORAL
	  extern FCT(void, DisplayWMessage ,(char *, char *, int));
	
	  DisplayWMessage("Warning : ", "", 1);
	  DisplayWMessage(varname, "", 0);
	  DisplayWMessage(" is redefined\n", "Smac Redefinition [see debug_mode()]", 0);
#else
	  fprintf(stderr, "Warning : %s is redefined [see debug_mode()]\n", varname);
#endif
	}
	redefine = 1;
      }
      else {
	sprintf(err_msg, "%s is already a global var", varname);
	Error(err_msg);
      }

    if (initval) {

      /* La valeur initiale des var glob est evaluee immediatement */
      
      initvalue = Eval(initval);
      if (convert)
	initvalue = convert(initvalue);

/* brrr...      free(initval);	*/		/* c'est deja ca .. */

      if (redefine) {
	if (Type__IsaArray(type))
	  Internal_Error("Array init not yet implemented");
	else
	  GlobalVar__Value(Identifier__GlobalVar(ident)) = initvalue;

	/* Retourne 0 pour que le toplevel n'execute pas d'instruction */
    
	return 0;
      }
      
      if (Type__IsaArray(type))
	Internal_Error("Array init not yet implemented");
    }
    else if (Type__IsaArray(type))
      initvalue = (Object) RTCCalloc(Type__Sizeof(type), 1);
      
    Identifier__GlobalVar(ident) =
      GlobalVar__GlobalVar(varname, type, initvalue);

    /* Retourne 0 pour que le toplevel n'execute pas d'instruction */
    
    return 0;
  }
  
  {

    /******************
      
     Declaration locale
     
     *****************/

    /* l'ajout en hash table est fait au debut de la liste sans
       verifier si la clef est deja la, il y a donc bien masquage
       s'il y a deja une variable portant le meme nom */

    LocalVar * var =
      LocalVar__LocalVar(varname, DynamicVar_FramePosition, type);
	
    HashTable__Add(DynamicVarsHashTable, (Object) varname, (Object) var);

    if (Type__IsaArray(type)) {
      /* vecteur local, memorise en pile l'adresse en pile pour
         eviter un cas particulier dans l'evaluation des var loc */
      DynamicVar_FramePosition +=
	(((Object *) l->info)[1] = Type__SizeInWord(type) + 1);
      return (Instruction *)
	Declaration__Declaration(initval, convert, Type__SizeInWord(type));
    }
    else {
      DynamicVar_FramePosition += (((Object *) l->info)[1] = 1);
      return (Instruction *)
	Declaration__Declaration(initval, convert, 0);
    }

    /* l et l->info seront desalloues lors du (re)passage sur les
       declaration en fin du block courant afin d'annuler les
       ajouts dans DynamicVarsHashTable et DynamicVar_FramePosition
       faits pas */
  }
}


Instruction * make_initializer_list(ignore_l)
     List * ignore_l;
{
  Error("{..} illegal initializer");

  return 0;					/* pour le compilo */
}

  
/*
  fct = make_function_declaration
  info = [funcname, les params, type (traite)]
  */

Instruction * make_function_declaration(l)
     List * l;
{
  Object * m = (Object *) l->info;
  Identifier * identifier = Get_Identifier((char *) m[0]);

  function_declaration(identifier, (List *) m[1], (Type *) m[2]);

/*  if (Return_Value_Type) { */
  if (Function_Name) {
    /* Declaration locale
       place la valeur 0 en DynamicVar_FramePosition pour masque
       une eventuelle variable locale de meme nom, en effet les
       HashTable__Search retourneront 0 comme si la liaison n'
       existait pas. la pseudo var locale sera retiree en sortie
       de block par remove_varloc.*/
    HashTable__Add(DynamicVarsHashTable, m[0], 0);
    m[1] = 0;
  }
  else {
    free(l);
    free(m);
  }
  
  return 0;
}


void remove_varloc(l, ndecl)
     List * l;
     int ndecl;
{
  while (ndecl--) {
    char * varname = ((char **) l->info)[0];
    List * suiv = l->next;

    DynamicVar_FramePosition -= ((Object *) l->info)[1];
    HashTable__Remove(DynamicVarsHashTable, (Object) varname);
    
    free(l->info);
    free(l);
    l = suiv;
  }
}
