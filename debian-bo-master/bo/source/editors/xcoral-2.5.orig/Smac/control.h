/* ########################################################################

			       control.h

   File: control.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/control.h
   Description: 
   Created: Tue Feb 21 12:51:59 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:51:59 MET 1995
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



#ifndef _control_h
#define _control_h

#include <setjmp.h>
#include <memory.h>

#include "Object.h"
#include "list.h"
#include "fctdecl.h"
#include "error.h"

extern jmp_buf current_env;
extern Object current_result;

extern int in_iteration;
extern int in_switch;

extern FCT(Instruction *, make_control,(List *)	);
     
extern void Reinit_Control();

#endif
