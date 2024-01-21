/* ########################################################################

			       operator.c

   File: operator.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/operator.c
   Description: 
   Created: Tue Feb 21 12:58:52 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:58:53 MET 1995
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



/* Les operateurs */

#include <stdio.h>
#include <string.h>

#include "operator.h"
#include "list.h"
#include "FunctionCall.h"
#include "Builtin.h"
#include "Const.h"
#include "Type.h"
#include "RefDeref.h"
#include "type.h"
#include "error.h"
#include "Cast.h"
#include "cast.h"
#include "const.h"
#include "identifier.h"
#include "array.h"
#include "Var.h"

/*******************
  
  Operateurs unaires

  *****************/

Instruction * var_reference(mkident)
     List * mkident;
{
  Instruction * ident = (Instruction *) MAKE(mkident);

  return
    (IsaConst(ident))
      ? ident
      : (IsaLocalVar(ident))
        ? (Instruction *) LocalVarRef__LocalVarRef((LocalVar *)ident)
        : (Instruction *) GlobalVarRef__GlobalVarRef((GlobalVar *)ident);
}

FCT ( Instruction *, make_oper_asterisk,(List *)	);

Instruction * make_oper_ampersand(l)
     List * l;
{
  List * place = (List *) l->info;

  free(l);

  if (place->fct == make_cast)
    Error("cast unacceptable operand for &");

  if (place->fct == make_string)
    return MAKE(place);
  
  if (place->fct == ((InstrFct) make_identifier))
    return var_reference(place);

  if (place->fct == ((InstrFct) make_oper_asterisk)) {
    /* &*X = X */
    Instruction * result = MAKE((List *) place->info);
    
    free(place);
    if (! Type__IsaPointer(GetExprType(result)))
      Error("illegal indirection (not a pointer)");
    
    return result;
  }

  if (place->fct == ((InstrFct) make_array_access))
    return array_reference(place);

  Error("illegal reference");

  return 0;						/* pour le compilo */
}


Instruction * make_oper_asterisk(l)
     List * l;
{
  Instruction * p = MAKE((List *) l->info);
  Type * type = GetExprType(p);

  free(l);

  if (! Type__IsaPointer(type))
    Error("illegal indirection (not a pointer)");

  if (Type__IsaFunction(type))
    return p;

  return (Instruction *)
    Indirection__Indirection(p, Type__Pointed_Type(type));
}


/* (+ x) = x */

Instruction * make_oper_mono_plus(l)
     List * l;
{
  Instruction * result = MAKE((List *) l->info);

  free(l);
  
  return result;
}


/* - */

Instruction * make_oper_mono_minus(l)
     List * l;
{
  Instruction * arg = MAKE((List *) l->info);
  Type * type = GetExprType(arg);

  free(l);
  
  if (IsaConst (arg)) {
    /* c'est ce qui se passe lorsqu'on ecrit un nombre negatif */
    if (type == Type_Int)
      ((Int *) arg)->_value = -(((Int *) arg)->_value);
    else if (type == Type_Char)
      ((Char *) arg)->_value = -(((Char *) arg)->_value);
    else
      Error("pointer nagation");

    return arg;
  }
  else {  
    Instruction ** m = (Instruction **) Malloc(sizeof(Instruction *));
    
    m[0] = arg;
    
    if ((type == Type_Int) || (type == Type_Char))
      return (Instruction *)
	BuiltinFunctionCall__BuiltinFunctionCall
	  	(Builtin_Mono_Minus, m, 1, type);
    
    Error((type == Type_Void) ? "void negation" : "pointer negation");

    return 0;			/* pour le compilo */
  }
}


/* ~ */

Instruction * make_oper_complement(l)
     List * l;
{
  Instruction * arg = MAKE((List *) l->info);
  Type * type = GetExprType(arg);
  
  free(l);

  if (IsaConst(arg)) {
    if (type == Type_Int)
      ((Int *) arg)->_value = ~(((Int *) arg)->_value);
    else if (type == Type_Char)
      ((Char *) arg)->_value = ~(((Char *) arg)->_value);
    else
      Error("pointer complement");

    return arg;
  }
  else {  
    Instruction ** m = (Instruction **) Malloc(sizeof(Instruction *));

    m[0] = arg;
    if ((type == Type_Int) || (type == Type_Char))
      return (Instruction *)
	BuiltinFunctionCall__BuiltinFunctionCall
		(Builtin_Complement, m, 1, type);

    Error((type == Type_Void)
	    	? "void complement"
	        : "pointer complement");
    
    return 0;					/* pour le compilo */
  }
}


Instruction * make_oper_not(l)
     List * l;
{
  Instruction * arg = MAKE((List *) l->info);
  
  free(l);

  if (IsaConst(arg)) {
    if (Eval(arg)) {
      if (! IsaFunction(arg)) free(arg);
      return (Instruction *) Int__Int(1);
    }
    if (! IsaFunction(arg)) free(arg);
    return (Instruction *) Int__Int(0);
  }
  else {
    Instruction ** m = (Instruction **) Malloc(sizeof(Instruction *));
  
    m[0] = arg;
    if (GetExprType(m[0]) != Type_Void)
      return (Instruction *)
	BuiltinFunctionCall__BuiltinFunctionCall
	    (Builtin_Not, m, 1, Type_Int);

    Error("! void");
    return 0;
  }
}


/********************
  
  Operateurs binaires

  *******************/

/*
   fct = make_binary_oper
   info = (void **) ["opername" arg1 arg2]
*/

Instruction * make_binary_oper(l)
     List * l;
{
  List ** exprs = (List **) l->info;
  char * opername = (char *) exprs[0];
  Instruction ** args = (Instruction **) Malloc(2 * sizeof(Instruction *));
  Function * bfct;
  Type * valtype = 0;
  Type * t1, * t2;
  
  args[0] = MAKE(exprs[1]);
  args[1] = MAKE(exprs[2]);
  t1 = GetExprType(args[0]);
  t2 = GetExprType(args[1]);

  free(l);
  free(exprs);

  bfct = find_binary_oper(opername, t1, t2, &valtype);

  if (IsaConst(args[0]) && IsaConst(args[1])) {
    Object val = (*(Function__BuiltinDef(bfct)))(args);
    Instruction * result = 0;
    
    if (valtype == Type_Int)
      result = (Instruction *) Int__Int(val);
    else if (valtype == Type_Char)
      result = (Instruction *) Char__Char(val);

    if (result) {
      if (! Type__IsaFunction(t1)) free(args[0]);
      if (! Type__IsaFunction(t2)) free(args[1]);
      free(args);
      
      return result;
    }
  }

  return (Instruction *)
    BuiltinFunctionCall__BuiltinFunctionCall(bfct, args, 2, valtype);
}


/* Retourne la Function correspondant a` l'operateur et modifie valtype*/

Function * find_binary_oper(opername, t1, t2, valtype)
     char * opername;
     Type * t1;
     Type * t2;
     Type ** valtype;
{
  Function * bfct;
  
  *valtype = 0;

  if (! strcmp(opername, "==")) {
    bfct = Builtin_egal;
    *valtype = Type_Int;
  }
  else if (! strcmp(opername, "!=")) {
    bfct = Builtin_negal;
    *valtype = Type_Int;
  }
  else if (! strcmp(opername, "&&")) {
    bfct = Builtin_cond_and;
    *valtype = Type_Int;
  }
  else if (! strcmp(opername, "||")) {
    bfct = Builtin_cond_or;
    *valtype = Type_Int;
  }
  else {
    char encodedname[24];
    
    sprintf(encodedname, "%s %s ", encode_type(t1), opername);
    strcat(encodedname, encode_type(t2));

    if ((bfct = (Function *)
	 HashTable__Search(binary_oper_hashtable, (Object) encodedname))
	!= 0)
      *valtype = Function__ValType(bfct);
    else if ((bfct =
	      (Function *) HashTable__Search(binary_special_oper_hashtable,
					     (Object) encodedname))
	     != 0)
      /* Il y a bien une fonction, mais le type retourne n'est pas fixe :
	 il est du type de celui des deux arguments qui est un pointeur,
	 a moins qu'il s'agisse de deux pointeurs auxquel cas il faut
	 verifier qu'ils soient de meme type */
      if (! Type__IsBuiltin(t1))
	if (! Type__IsBuiltin(t2))
	  if (! Type__Equal(t1, t2)) {
	    sprintf(err_msg, "%s arguments have different types", opername);
	    Error(err_msg);
	  }
	  else
	    *valtype = Function__ValType(bfct);
	else
	  *valtype = t1;
      else
	*valtype = t2;

    if (! *valtype)
      /* promotion de char en int */
      if (t1 == Type_Char)
	if (t2 == Type_Char)
	  return find_binary_oper(opername, Type_Int, Type_Int, valtype);
	else
	  return find_binary_oper(opername, Type_Int, t2, valtype);
      else if (t2 == Type_Char)
	return find_binary_oper(opername, t1, Type_Int, valtype);
      else {
	sprintf(err_msg, "bad argument in %s", encodedname);
	Error(err_msg);
      }
  }

  return bfct;
}

/* La meme chose, mais la fonction est un derive ou aucun
   argument n'est evalue, ceci pour les modifications */

FCT ( Object,(* find_modifbinary_oper(opername, t1, t2,	valtype)),
     	     (Object, Object, int))
     char * opername;
     Type * t1;
     Type * t2;
     Type ** valtype;
{
  return (FCT( Object,(*),(Object, Object, int)))
    HashTable__Search
	(binary_modifoper_hashtable,
	 (Object) find_binary_oper(opername, t1, t2, valtype));
}
