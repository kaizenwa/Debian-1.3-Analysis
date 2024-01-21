/* ########################################################################

			       Builtin.h

   File: Builtin.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Builtin.h
   Description: 
   Created: Tue Feb 21 10:49:05 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:49:05 MET 1995
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



#ifndef _Builtin_h
#define _Builtin_h

#include "Function.h"
#include "hash.h"

extern Function * Builtin_Not;
extern Function * Builtin_Mono_Minus;
extern Function * Builtin_Complement;

extern Function * Builtin_egal;
extern Function * Builtin_negal;

extern Function * Builtin_cond_and;
extern Function * Builtin_cond_or;

extern HashTable * binary_oper_hashtable;
extern HashTable * binary_unique_oper_hashtable;
extern HashTable * binary_special_oper_hashtable;

extern HashTable * builtin_fct_nbre_arg_min;

extern HashTable * binary_modifoper_hashtable;

extern FCT(int, check_strlen,(char *)	);
extern FCT(Object, new_builtin,(char *, Type *, int, Type **,
				FCT (Object, (*fct),(Instruction **))) );

extern void Init_Builtin();

#ifdef XCORAL
extern FCT(void, stop_profile,()	);
extern FCT(void, rerun_profile,()	);
#endif

#endif
