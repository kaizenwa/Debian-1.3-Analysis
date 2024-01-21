/* ########################################################################

			     Instruction.h

   File: Instruction.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Instruction.h
   Description: 
   Created: Tue Feb 21 10:58:02 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:58:03 MET 1995
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



/* Doit etre laisse au tout debut */

#include "Type.h"

#ifndef _Instruction_h
#define _Instruction_h

#include "fctdecl.h"
#include "Object.h"
#include "mem.h"

/* Les fonctions definies pour chaque sous type d'Instruction :
     
     Eval evalue l'expression et rend sa valeur

     IsaConst retourne 1 pour les constantes builtin, sinon 0
*/

typedef FCT( Object, (*EvalFct), (void *) );
     
typedef struct {
  EvalFct _Eval;
} inherit_instruction_function_vector;


/* A inclure dans les definitions des `sous types' pour heriter */

#define Inherit_Instruction						\
  inherit_instruction_function_vector * instruction_functions;		\
  int _isa_const;							\
  Type * _expr_type

  
/* Les appels des fonctions virtuelles */

#define Eval(i)		((*((i)->instruction_functions->_Eval))(i))

/* Les constructeurs herites */

#define Instruction_Constructor(v, i, t)	\
  i->instruction_functions = &v;		\
  i->_expr_type = t;				\
  i->_isa_const = 0

#define Const_Constructor(v, c, t)		\
  c->instruction_functions = &v;		\
  c->_expr_type = t;				\
  c->_isa_const = 1
 
/* Le type `pure' Instruction */
  
typedef struct _Instruction {
  Inherit_Instruction;
} Instruction;


#define GetExprType(i)	((i)->_expr_type)
#define IsaConst(i)	((i)->_isa_const)
  
#endif
