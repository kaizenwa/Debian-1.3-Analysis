/* ########################################################################

				switch.c

   File: switch.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/switch.c
   Description: 
   Created: Tue Feb 21 13:01:46 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 13:01:46 MET 1995
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



#include "Switch.h"
#include "switch.h"
#include "error.h"
#include "control.h"

/* fct = make_switch
   info = List ** [clef, exprs]
   */

Switch * make_switch(l)
     List * l;
{
  List ** m = (List **) l->info;
  Instruction * clef = MAKE(m[0]);
  List * lexprs;
  int iexpr, icase;
  Instruction ** exprs;
  int * cases;
  int idefault = -1;
  int prev_in_switch = in_switch;

  in_switch = 1;

#ifndef XCORAL
  if ((GetExprType(clef) != Type_Int) &&
      (GetExprType(clef) != Type_Char))
    Warning("switch key is not an int or a char");
#endif

  /* Compte les etiquettes et les cases */
  
  for (lexprs = m[1], iexpr = 0, icase = 0; lexprs; lexprs = lexprs->next)
    if (lexprs->fct != make_case)
      iexpr += 1;
    else if (lexprs->info)
      /* pas default: */
      icase += 1;
    else if (idefault == -1)
      idefault = iexpr;
    else
      Error("more than one default: in switch");

  exprs = (Instruction **) Malloc(sizeof(Instruction *) * (iexpr + 1));
  cases = (int *) Malloc(sizeof(int) * icase * 2);

  /* Genere les instructions */
  
  for (lexprs = m[1], iexpr = 0, icase = 0; lexprs; ) {
    List * l = lexprs->next;
    
    if (lexprs->fct == make_case) {
      if (lexprs->info) {
	/* pas default: */
	Instruction * c =
	  MAKE((List *) lexprs->info);		/* constant : int ou char */
	int entry = Eval(c);
	int index;

	cases[icase++] = entry;			/* clef */
	cases[icase++] = iexpr;			/* indice dans exprs */

	for (index = 0; cases[index] != entry; index += 2);
	if (index != (icase - 2)) {
	  sprintf(err_msg, "duplicate case %d", entry);
	  Error(err_msg);
	}
	
	free(c);
	free(lexprs);
      }	  
    }
    else
      exprs[iexpr++] = MAKE(lexprs);
    
    lexprs = l;  
  }

  exprs[iexpr] = (Instruction *)
    make_control(List__List(0, 0));		/* un break final */

  in_switch = prev_in_switch;
  
  free(m);
  free(l);
  
  return Switch__Switch(clef, exprs, cases, icase, idefault);
}

/* la fonction ne peut etre appelee que si le case est invalide */

Instruction * make_case(l)
     List * l;
{
  sprintf(err_msg, "case %s out of switch", (char *) ((List *) l->info)->info);
  Error(err_msg);

  return 0;			/* pour le compilo */
}

