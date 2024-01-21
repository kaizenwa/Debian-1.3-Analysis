/* ########################################################################

			       funcall.c

   File: funcall.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/funcall.c
   Description: 
   Created: Tue Feb 21 12:53:55 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:53:56 MET 1995
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

#include "funcall.h"
#include "FunctionCall.h"
#include "Builtin.h"
#include "list.h"
#include "hash.h"
#include "error.h"

/*
  fct = make_funcall (fonction non calculee)
  info = (char **) [nbrearg, fct, arg1 ..] avec argi,fct : List *, nbrearg >= 0
*/

int bad_number_of_args(fct, nbrearg)
     Function * fct;
     int nbrearg;
{
  if (Function__NbreArg(fct) != nbrearg) {
    if (! Function__IsBuiltin(fct))
      return 1;
    else {
      int min = (int) HashTable__Search(builtin_fct_nbre_arg_min, (Object) fct);

      if ((! min) || (nbrearg < (min - 1)))
	return 1;
    }
  }
  
  return 0;
}

Instruction * make_funcall(l)
     List * l;
{
  List ** largs = (List **) l->info;
  int nbrearg = (int) *largs++;
  Instruction * result;

  /* Ici on est ANSI : si la fonction n'est pas calculee ou est une variable
     globale il y a un appel a make_identifier qui DOIT trouver l'identifier */

  Function * fct = (Function *) MAKE(*largs);

  largs += 1;

  if (IsaFunction((Instruction *) fct)) {
    
    /*******************************
      La fonction n'est pas calculee
      ******************************/
    
    if (bad_number_of_args(fct, nbrearg)) {
      sprintf(err_msg, "%d arguments for %s, %s%d required",
	      nbrearg, Function__Name(fct),
	      (HashTable__Search(builtin_fct_nbre_arg_min, (Object) fct))
	      ? "at least " : "",
	      Function__NbreArg(fct));
      Error(err_msg);
    }
    else {
      int nargtheo = Function__NbreArg(fct);
      int index;
      Instruction ** args;
      
#if defined(RUNTIMECHECK) || defined(XCORAL)
      if (HashTable__Search(builtin_fct_nbre_arg_min, (Object) fct)) {
	args = (Instruction **) Malloc((nbrearg + 1) * sizeof(Instruction *));
	args[nbrearg] =
	  (Instruction *) NotEnoughArg__NotEnoughArg(fct, nbrearg);
      }
      else
	args = (Instruction **) Malloc(nbrearg * sizeof(Instruction *));
#else
      args = (Instruction **) Malloc(nbrearg * sizeof(Instruction *));
#endif
    
      for (index = 0; index != nbrearg; index += 1) {
	args[index] = MAKE(*largs);
	largs += 1;
	if (nargtheo) {
	  nargtheo -= 1;
	  if (! ExprType__Convertible(args[index],
				      Function__ArgType(fct, index))) {
	    sprintf(err_msg, "incompatible type for %s argument %d, ",
		    Function__Name(fct), index + 1);
	    Type__Print(Function__ArgType(fct, index), err_msg);
	    strcat(err_msg, " <> ");
	    Type__Print(GetExprType(args[index]), err_msg);
	    Error(err_msg);
	  }
	}
      }

      if (Function__IsBuiltin(fct))
	result = (Instruction *)
	  BuiltinFunctionCall__BuiltinFunctionCall(fct, args, nbrearg,
						   Function__ValType(fct));
      else
	result = (Instruction *)
	    FunctionCall__FunctionCall(fct, args, nbrearg);
    }
  }
  else if (Type__IsaFunction(GetExprType((Instruction *) fct))) {
    TypeFunction * tfct = Type__Function(GetExprType((Instruction *) fct));
    
    /*************************
      La fonction est calculee
      ************************/

    if (TypeFunction__NbreArg(tfct) > nbrearg) {
      sprintf(err_msg, "%d arguments at least %d required",
	      nbrearg, TypeFunction__NbreArg(tfct));
      Error(err_msg);
    }
    else {
      int index;
      Instruction ** args =
	(Instruction **) Malloc(nbrearg * sizeof(Instruction *));
    
      for (index = 0; index != nbrearg; index += 1) {
	args[index] = MAKE(*largs);
	largs += 1;
	if (! ExprType__Convertible(args[index],
				    TypeFunction__ArgType(tfct, index))) {
	  sprintf(err_msg, "incompatible type argument %d, ", index + 1);
	  Type__Print(TypeFunction__ArgType(tfct, index), err_msg);
	  strcat(err_msg, " <> ");
	  Type__Print(GetExprType(args[index]), err_msg);
	  Error(err_msg);
	}
      }

      result = (Instruction *)
	CalculateFunctionCall__CalculateFunctionCall((Instruction *) fct,
						       args, nbrearg);
    }
  }
  else
    Error("illegal function call");
  
  free(l->info);
  free(l);
  
  return result;
}
