/* ########################################################################

				 Var.h

   File: Var.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Var.h
   Description: 
   Created: Tue Feb 21 12:46:49 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:46:50 MET 1995
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



#ifndef _Var_h
#define _Var_h

#include "Instruction.h"


/* LocalVar : les variables locales */

typedef struct {
  Inherit_Instruction;
  
  char * _name;
  int _position;
} LocalVar;

extern FCT (LocalVar *, LocalVar__LocalVar,
	    (char * nom, int position, Type * t));

typedef struct {
  Inherit_Instruction;
  
  int _position;
  Instruction * _value;
  FCT( Object,(* _convert),(Object));
} LocalVarAssignment;

extern FCT (LocalVarAssignment *, LocalVarAssignment__LocalVarAssignment,
	    (LocalVar *, Instruction *, Type *)	);

typedef struct {
  Inherit_Instruction;
  
  int _position;
  Instruction * _value;
  FCT( Object,(* _convert),(Object));
} LocalVarPostIncrDecr;

extern FCT (LocalVarPostIncrDecr *, LocalVarPostIncrDecr__LocalVarPostIncrDecr,
	    (LocalVar *, Instruction *, Type *)	);

typedef struct {
  Inherit_Instruction;
  
  int _position;
} LocalVarRef;

extern FCT (LocalVarRef *, LocalVarRef__LocalVarRef, (LocalVar * var));

extern FCT (int, IsaLocalVar,(Instruction *)				);


void Init_LocalVar();

	    
/* GlobalVar : les variables globales */

typedef struct {
  Inherit_Instruction;

  char * _name;
  Object _value;
} GlobalVar;

extern FCT (GlobalVar *, GlobalVar__GlobalVar, (char *, Type *, Object)	);

typedef struct {
  Inherit_Instruction;
  
  Object * _pvalue;
  Instruction * _value;
  FCT( Object,(* _convert),(Object));
} GlobalVarAssignment;

extern FCT (GlobalVarAssignment *, GlobalVarAssignment__GlobalVarAssignment,
	    (GlobalVar *, Instruction *, Type *)	);

typedef struct {
  Inherit_Instruction;

  Object _value;
} GlobalVarRef;

extern FCT (GlobalVarRef *, GlobalVarRef__GlobalVarRef, (GlobalVar *)	);

typedef struct {
  Inherit_Instruction;
  
  Object * _pvalue;
  Instruction * _value;
  FCT( Object,(* _convert),(Object));
} GlobalVarPostIncrDecr;

extern FCT (GlobalVarPostIncrDecr *,
	    GlobalVarPostIncrDecr__GlobalVarPostIncrDecr,
	    (GlobalVar *, Instruction *, Type *)	);

extern FCT (int, IsaGlobalVar,(Instruction *)				);

void Init_GlobalVar();

#ifdef RUNTIMECHECK
#define GlobalVar__Value(gv)	*((Object *) (gv)->_value)
#else
#define GlobalVar__Value(gv)	(gv)->_value
#endif
    
#endif
