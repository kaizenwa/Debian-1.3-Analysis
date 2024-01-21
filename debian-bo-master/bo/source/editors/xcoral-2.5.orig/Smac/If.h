/* ########################################################################

				  If.h

   File: If.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/If.h
   Description: 
   Created: Tue Feb 21 10:57:39 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:57:40 MET 1995
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



#ifndef _If_h
#define _If_h

#include "Instruction.h"

typedef struct {
  Inherit_Instruction;

  Instruction * _test;
  Instruction * _iftrue;
  Instruction * _else;
} If;

extern FCT( If *, If__If,(Instruction * test, Instruction * iftrue,
			  Instruction * iffalse)	);

typedef struct {
  Inherit_Instruction;

  Instruction * _test;
  Instruction * _iftrue;
  Instruction * _else;
  FCT ( Object, (*_converttrue), (Object)	);
  FCT ( Object, (*_convertfalse), (Object)	);
} ArithIf;

extern FCT( ArithIf *, ArithIf__ArithIf,(Instruction * test,
					 Instruction * iftrue,
					 Instruction * iffalse)	);


extern void Init_If();

#endif
