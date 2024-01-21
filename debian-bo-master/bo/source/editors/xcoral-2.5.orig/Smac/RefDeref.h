/* ########################################################################

			       RefDeref.h

   File: RefDeref.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/RefDeref.h
   Description: 
   Created: Tue Feb 21 10:58:39 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:58:39 MET 1995
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



#ifndef _RefDeref_h
#define _RefDeref_h

#include "Instruction.h"

typedef struct {
  Inherit_Instruction;

  Instruction * _pointer;
  FCT ( Object, (*_deref), (Object)	);
} Indirection;

extern FCT(Indirection  *,Indirection__Indirection,(Instruction *, Type *) );

     
typedef struct {
  Inherit_Instruction;

  Instruction * _pointer;
  Instruction * _value;
  FCT ( Object, (*_convert), (Object)	);
  FCT ( Object, (*_refassign), (Object, Object)	);
} IndirectionAssignment;

extern FCT(IndirectionAssignment  *,
	   IndirectionAssignment__IndirectionAssignment,
	   (Instruction *, Instruction *, Type *, FCT2(Object,(*),(Object) )));

typedef struct {
  Inherit_Instruction;

  Instruction * _pointer;
  Instruction * _value;
  FCT ( Object, (*_convert), (Object)	);
  FCT ( Object, (*_deref), (Object)	);
  FCT ( Object, (*_refassign), (Object, Object)	);
  FCT( Object,(* _bfct),(Object, Object, int)	);
  int _size;
} IndirectionModify;

extern FCT(IndirectionModify  *,
	   IndirectionModify__IndirectionModify,
	   (Instruction *, Instruction *,
	    FCT2( Object,(*),(Object, Object, int)),
	    Type *, FCT2(Object,(*),(Object) )));

typedef struct {
  Inherit_Instruction;

  Instruction * _pointer;
  int _value;
  FCT ( Object, (*_convert), (Object)	);
  FCT ( Object, (*_deref), (Object)	);
  FCT ( Object, (*_refassign), (Object, Object)	);
  FCT( Object,(* _bfct),(Object, Object, int)	);
  int _size;
} IndirectionPostIncrDecr;

extern FCT(IndirectionPostIncrDecr  *,
	   IndirectionPostIncrDecr__IndirectionPostIncrDecr,
	   (Instruction *, int, FCT2( Object,(*),(Object, Object, int)),
	    Type *, FCT2(Object,(*),(Object) )));

extern void Init_RefDeref();

#endif
