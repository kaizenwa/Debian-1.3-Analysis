/* ########################################################################

			     Declaration.h

   File: Declaration.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Declaration.h
   Description: 
   Created: Tue Feb 21 10:54:28 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:54:28 MET 1995
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



#ifndef _Declaration_h
#define _Declaration_h

#include "Instruction.h"

typedef struct {
  Inherit_Instruction;

  Instruction * _init_value;
  FCT( Object,(* _convert),(Object));
  int _sizeinword;
} Declaration;

extern FCT( Declaration *, Declaration__Declaration,
	   (Instruction *, FCT2( Object,(*),(Object)), int)	);

extern void Init_Declaration();

#endif
