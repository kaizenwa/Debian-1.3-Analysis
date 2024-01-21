/* ########################################################################

				 Cast.h

   File: Cast.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Cast.h
   Description: 
   Created: Tue Feb 21 10:49:40 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:49:41 MET 1995
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



#ifndef _Cast_h
#define _Cast_h

#include "Instruction.h"
#include "Type.h"

typedef struct {
  Inherit_Instruction;

  Instruction * _expr;
  FCT( Object,(* _convert),(Object));
} Cast;

extern FCT( Cast *,Cast__Cast,(Instruction *, Type *, ConvFct)	);

extern FCT( void, Init_Cast,()			       		);
     
#endif
     
