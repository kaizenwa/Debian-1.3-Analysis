/* ########################################################################

				Array.h

   File: Array.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Array.h
   Description: 
   Created: Tue Feb 21 10:46:41 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:46:42 MET 1995
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



#ifndef _ArrayAccess_h
#define _ArrayAccess_h

#include "Instruction.h"

typedef struct {
  Inherit_Instruction;

  Instruction * _array;
  Instruction * _index;
  int _subsize;
  FCT( Object,(* _access),(Object, Object, int)	);
} ArrayAccess;

extern FCT( ArrayAccess *,ArrayAccess__ArrayAccess,
	   			(Instruction *, Instruction *)	);

typedef struct {
  Inherit_Instruction;

  Instruction * _array;
  Instruction * _index;
  Instruction * _value;
  FCT( Object,(* _convert),(Object));
  FCT( Object,(* _affect),(Object, Object, Object)	);
} ArrayAssignment;

extern FCT( ArrayAssignment *,ArrayAssignment__ArrayAssignment,
	   	(Instruction *, Instruction *, Instruction *,
		 Type *, FCT2( Object,(*),(Object)))	);


typedef struct {
  Inherit_Instruction;

  Instruction * _array;
  Instruction * _index;
  Instruction * _value;
  FCT( Object,(* _convert),(Object)			);
  FCT( Object,(* _affect),(Object, Object, Object)	);
  FCT( Object,(* _access),(Object, Object)		);
  FCT( Object,(* _bfct),(Object, Object, int)		);
  int _size;
} ArrayModify;

extern FCT( ArrayModify *,ArrayModify__ArrayModify,
	   	(Instruction *, Instruction *, Instruction *,
		 FCT2( Object,(*),(Object, Object, int)),
		 Type *, FCT2( Object,(*),(Object)))	);


typedef struct {
  Inherit_Instruction;

  Instruction * _array;
  Instruction * _index;
  int _subsize;
} ArrayRef;

extern FCT( ArrayRef *,ArrayRef__ArrayRef,(Instruction *, Instruction *)   );

typedef struct {
  Inherit_Instruction;

  Instruction * _array;
  Instruction * _index;
  int _value;
  FCT( Object,(* _convert),(Object)			);
  FCT( Object,(* _affect),(Object, Object, Object)	);
  FCT( Object,(* _access),(Object, Object)		);
  FCT( Object,(* _bfct),(Object, Object, int)		);
  int _size;
} ArrayPostIncrDecr;

extern FCT( ArrayPostIncrDecr *,ArrayPostIncrDecr__ArrayPostIncrDecr,
	   	(Instruction *, Instruction *, int,
		 FCT2( Object,(*),(Object, Object, int)),
		 Type *, FCT2( Object,(*),(Object)))	);


/**/

extern void Init_Array();

#endif
