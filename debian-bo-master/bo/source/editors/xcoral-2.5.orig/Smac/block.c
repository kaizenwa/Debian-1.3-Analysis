/* ########################################################################

				block.c

   File: block.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/block.c
   Description: 
   Created: Tue Feb 21 12:50:17 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:50:17 MET 1995
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



#include "Instruction.h"
#include "Block.h"
#include "list.h"
#include "declaration.h"
#include "function.h"
#include "error.h"

/* fct = make_block
   info = (List *) 1ere expression, les suivantes par next */

Instruction * make_block(body)
     List * body;
{
  int length;

  {
    List * l = (List *) body->info;

    free(body);
    body = l;
  }
  
  length = list_length(body);

  if ((length == 1) &&
      (body->fct != (InstrFct) make_declaration) &&
      (body->fct != (InstrFct) make_function_declaration))
    /* Une seul instruction : pas besoin de block */
    return MAKE(body);

  {
    Instruction ** instrs;
    Instruction ** pinstrs;
    List * begin = body;
    int ndecl = 0;
    
    pinstrs = instrs = (Instruction **) Malloc(length * sizeof(Instruction *));
    
    while (body) {
      List * next = body->next;

      /* on compte les decl car les instructions qui ne
	 sont pas des declarations sont desallouees, il
	 n'est plus alors possible de se demander si ce
	 sont ou non des declaration et retrouver le nom
	 des vars a retirer */
      if (body->fct == (InstrFct) make_function_declaration)
/*	if (! Return_Value_Type) */
	if (! Function_Name)
	  Error("Declaration in eval form");
	else {
	  MAKE(body);
	  ndecl += 1;
	  length -= 1;		/* pas d'instruction associee */
	}
      else {
	if (body->fct == (InstrFct) make_declaration)
/* 	  if (! Return_Value_Type) */
	  if (! Function_Name)
	    Error("Declaration in eval form");
	  else
	    ndecl += 1;
	*pinstrs++ = MAKE(body);
      }
      
      body = next;
    }

    if (ndecl)
      remove_varloc(begin, ndecl);
    
    return (Instruction *) Block__Block(length, instrs);
  }
}
