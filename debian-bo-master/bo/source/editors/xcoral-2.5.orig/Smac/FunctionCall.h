/* ########################################################################

			     FunctionCall.h

   File: FunctionCall.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/FunctionCall.h
   Description: 
   Created: Tue Feb 21 10:56:10 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:56:11 MET 1995
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



#ifndef _FunctionCall_h
#define _FunctionCall_h

#include "Instruction.h"
#include "Function.h"

/* Appel d'une fonction utilisateur */

typedef struct {
  Inherit_Instruction;

  Function * _function;
  Instruction ** _args;
  int _narg;
} FunctionCall;

extern FCT( FunctionCall *, FunctionCall__FunctionCall,
	   (Function *, Instruction **, int narg)	);


/* Appel d'une fonction builtin
   
   Utilise directement la meme structure
   au lieu de passer par un heritage */

extern FCT(FunctionCall *, BuiltinFunctionCall__BuiltinFunctionCall,
	   (Function *, Instruction **, int narg, Type *)	);

/* Appel d'une fonction calculee */

typedef struct {
  Inherit_Instruction;

  Instruction * _function;
  Instruction ** _args;
  int _narg;
} CalculateFunctionCall;

extern FCT( CalculateFunctionCall *,
	    CalculateFunctionCall__CalculateFunctionCall,
	   	(Instruction *, Instruction **, int narg)	);



#if defined(RUNTIMECHECK) || defined(XCORAL)

/* Pseudo argument place en dernier pour sortir en erreur s'il est evalue,
   utilise pour les fonctions n'ayant pas un nombre fixe d'argument */

typedef struct {
  Inherit_Instruction;

  Function * _function;
  int _narg;
} NotEnoughArg;

extern FCT( NotEnoughArg *, NotEnoughArg__NotEnoughArg,(Function *, int)  );
extern FCT( int, IsaNotEnoughArg,(Instruction *) );
     
#endif

#ifdef XCORAL
extern FCT( void, call_user_function_from_xcoral,(Function *, int, int *) );
extern FCT( void, call_builtin_function_of_xcoral,(Function *, int) );
#endif
     
void Init_FunctionCall();

#endif
