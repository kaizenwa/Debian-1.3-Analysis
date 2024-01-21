/* ########################################################################

			       Function.h

   File: Function.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Function.h
   Description: 
   Created: Tue Feb 21 10:55:45 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:55:46 MET 1995
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



#ifndef _Function_h
#define _Function_h

/* L'objet fonction */

#include "Instruction.h"

typedef struct {
  Inherit_Instruction;

  char * _name;
  int _nbre_arg;
  union {
    FCT( Object, (* _builtin_def), (Instruction **) );
    Instruction * _user_def;
  } _def;
  Type ** _argtypes;
  Type * _valtype;
  Object * _masks;
  int _profile_count;
} Function;

extern FCT (Function *, Function__Function,
	    (char * nom, Type * valtype, int narg,
	     Type ** argtypes, Object def));

#define Function__Name(f)		((f)->_name)
#define Function__ValType(f) 		((f)->_valtype)
#define Function__ArgType(f, rank) 	((f)->_argtypes[rank])
#define Function__NbreArg(f) 		((f)->_nbre_arg)
#define Function__IsBuiltin(f)		(! (f)->_masks)
#define Function__BuiltinDef(f)		((f)->_def._builtin_def)
#define Function__UserDef(f)		((f)->_def._user_def)

extern FCT(void, Function__UpdateDef,(Function * this, Instruction *));
extern FCT(void, Function__VerifyNotActive,(Function * this));

void Init_Function();

/**/
  
typedef struct {
  Inherit_Instruction;

  Function * _func;
} UndefinedFunctionCall;

extern FCT( UndefinedFunctionCall *,
	   UndefinedFunctionCall__UndefinedFunctionCall,
	   (Function * func));

extern FCT ( int, IsaFunction,(Instruction *)		);
extern FCT ( int, Function__IsUndefined,(Function *)	);
extern FCT ( int, IsaPointerToFunction,(void *)		);
     
void Init_UndefinedFunctionCall();
  
#endif
