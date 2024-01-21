/* ########################################################################

			      Identifier.h

   File: Identifier.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Identifier.h
   Description: 
   Created: Tue Feb 21 10:56:37 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:56:38 MET 1995
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



#ifndef _Identifier_h
#define _Identifier_h

#include "fctdecl.h"
#include "Function.h"
#include "Var.h"
#include "hash.h"

typedef struct {
  char * _name;
  Function * _functiondef;
  GlobalVar * _globalvar;
} Identifier;

#define Identifier__Name(i)		((i)->_name)
#define Identifier__FunctionDef(i)	((i)->_functiondef)
#define Identifier__GlobalVar(i)	((i)->_globalvar)

extern FCT( Identifier *, Identifier__Identifier,
	   	(char *, Function *, GlobalVar *));

extern FCT ( Identifier *, Get_Identifier,(char * name)	);
     
#define find_identifier(name) 	\
	((Identifier *) HashTable__Search(IdentifierHashTable, (Object) name))

extern HashTable * IdentifierHashTable;

extern void Init_Identifier();

#endif
