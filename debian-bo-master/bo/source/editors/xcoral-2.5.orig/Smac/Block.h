/* ########################################################################

				Block.h

   File: Block.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Block.h
   Description: 
   Created: Tue Feb 21 10:48:26 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:48:26 MET 1995
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



#ifndef _Block_h
#define _Block_h

#include "Instruction.h"

typedef struct{
  Inherit_Instruction;

  int _nexpr;
  Instruction ** _instrs;
} Block;

extern FCT( Block *,  Block__Block,(int nexpr, Instruction ** instrs) );
     
extern void Init_Block();
     
#endif
