/* ########################################################################

				While.h

   File: While.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/While.h
   Description: 
   Created: Tue Feb 21 12:47:16 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:47:17 MET 1995
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



#ifndef _While_h
#define _While_h

#include "Instruction.h"

typedef struct {
  Inherit_Instruction;

  Instruction * _test;
  Instruction * _body;
  int _hasbreakcont;
} While;

extern FCT( While *, While__While,(Instruction *, Instruction *, int));

extern FCT( While *, DoWhile__DoWhile, (Instruction *, Instruction *, int));

extern void Init_While();

#endif
