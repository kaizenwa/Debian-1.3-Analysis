/* ########################################################################

				 Type.h

   File: Type.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Type.h
   Description: 
   Created: Tue Feb 21 12:46:28 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:46:28 MET 1995
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



#ifndef _Type_h
#define _Type_h

/* Doit etre place avant les includes */

typedef enum { T_Builtin, T_Pointer, T_Array, T_Function } type_of_type;

typedef struct {
  void * _name_or_pointed;
  type_of_type _type;
  int _sizeof;
} Type;

typedef struct {
  int _nbre_arg;
  Type ** _argtypes;
  Type * _valtype;
} TypeFunction;

#include "fctdecl.h"
#include "hash.h"
#include "word.h"
#include "Instruction.h"

typedef FCT(Object, (*ConvFct), (Object)	);
     
extern FCT( Type *, Type__Type,(void * subinfo, type_of_type tot,
				int size_of));

extern FCT( int, Type__Equal,(Type *, Type *)		);
extern FCT( void, Type__Print,(Type *, char *)		);
extern FCT( Object, Type__Mask,(Type *)			);
extern FCT( ConvFct, Type__Convertible,(Type *, Type *)	);
extern FCT( ConvFct, ExprType__Convertible,(Instruction *, Type *)	);

#define AlreadyConvertible	((ConvFct) Type__Convertible)
     
extern FCT( char, int2char,(int)	);
extern FCT( int, char2int,(int)	);

#define Type__IsBuiltin(t)	((t)->_type == T_Builtin)
#define Type__IsaPointer(t)	(((t)->_type == T_Pointer) ||		\
       				 ((t)->_type == T_Array)   ||		\
       				 ((t)->_type == T_Function))
#define Type__IsaArray(t)	((t)->_type == T_Array)
#define Type__IsaFunction(t)	((t)->_type == T_Function)
     
#define Type__Sizeof(t)		((t)->_sizeof)
#define Type__Pointed_Type(t)	((Type *) (t)->_name_or_pointed)
#define Type__Name(t)		((char *) (t)->_name_or_pointed)
#define Type__Function(t)	((TypeFunction *) (t)->_name_or_pointed)

#define TypeFunction__ValType(tf) 	((tf)->_valtype)
#define TypeFunction__ArgType(tf, rank) ((tf)->_argtypes[rank])
#define TypeFunction__NbreArg(tf) 	((tf)->_nbre_arg)

#define Type__SizeInWord(t)	\
       ((Type__Sizeof(t) + sizeof(void *) - 1) / sizeof(void *))

/**/
     
extern Type * Type_Int;
extern Type * Type_Char;
extern Type * Type_Void;

extern Type * Type_String;
extern Type * Type_PInt;

extern FCT( Type *, find_type, (char * name)	);
extern FCT( void, error_incompatible_types,(Type * t1, Type * t2, char * msg)	);

void Init_type();
     
#endif
