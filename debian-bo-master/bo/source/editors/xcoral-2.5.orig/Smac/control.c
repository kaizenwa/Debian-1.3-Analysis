/* ########################################################################

			       control.c

   File: control.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/control.c
   Description: 
   Created: Tue Feb 21 12:51:39 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:51:39 MET 1995
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



#include <stdio.h>

#include "control.h"
#include "Control.h"
#include "error.h"
#include "list.h"

jmp_buf current_env;
Object current_result;

int in_iteration = 0;
int in_switch = 0;


Instruction * make_control(l)
     List * l;
{
  JMP_TYPE type = (l->info) ? JMP_CONTINUE : JMP_BREAK;

  free(l);
  
  if ((! in_iteration) &&
      ((! in_switch) || (type == JMP_CONTINUE))) {
    sprintf(err_msg, "illegal %s", (type == JMP_BREAK) ? "break" : "continue");
    Error(err_msg);
  }

  if ((! in_switch) || (type == JMP_CONTINUE))
    in_iteration += 1;
  
  return (Instruction *) Control__Control(type);
}

void Reinit_Control()
{
  in_iteration = 0;
  in_switch = 0;
}
