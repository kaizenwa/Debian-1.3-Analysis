/* ########################################################################

				switch.h

   File: switch.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/switch.h
   Description: 
   Created: Tue Feb 21 13:02:03 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 13:02:04 MET 1995
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



#ifndef _switch_h
#define _switch_h

#include "Instruction.h"
#include "list.h"
#include "Switch.h"

extern FCT( Instruction *, make_case, (List *)	);
extern FCT( Switch *, make_switch, (List *)	);

#endif
