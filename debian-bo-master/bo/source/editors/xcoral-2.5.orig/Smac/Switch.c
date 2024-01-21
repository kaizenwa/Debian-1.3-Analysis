/* ########################################################################

				Switch.c

   File: Switch.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Switch.c
   Description: 
   Created: Tue Feb 21 12:45:29 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:45:30 MET 1995
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
#include "error.h"
#include "function.h"
#include "control.h"
#include "stack.h"

static inherit_instruction_function_vector
  switch_inherit_instruction_function_vector;

Switch * Switch__Switch(key, exprs, cases, ncases, defaultindice)
     Instruction * key;
     Instruction ** exprs;
     int * cases;
     int ncases;
     int defaultindice;
{
  Switch * this = (Switch *) Malloc(sizeof(Switch));

  Instruction_Constructor
    (switch_inherit_instruction_function_vector, this, Type_Void);

  this->_key = key;
  this->_exprs = exprs;
  this->_cases = cases;
  this->_ncases = ncases;
  this->_default_indice = defaultindice;

  return this;
}

static Object Switch__Eval(this)
     Switch * this;
{
  Instruction ** exprs = 0;

  /* Cherche l'entree correspondante
     Les clefs pourraient etre triees pour faire une recherche dico */
  {
    Object key = Eval(this->_key);
    int * cases = this->_cases;
    int ncases = this->_ncases;
    int icase;
    
    for (icase = 0; icase != ncases; icase += 2)
      if (key == cases[icase]) {
	/* C'est elle ! */
	exprs = &this->_exprs[cases[icase + 1]];
	break;
      }
  }

  if (! exprs)
    if (this->_default_indice != -1)
      exprs = &this->_exprs[this->_default_indice];
    else
      return 0;

  /* On execute les expressions a partir de la 1ere
     correspondante, il y a obligatoirement break final */
  
  {
    Object * stackpointer = Stack_Pointer;
    jmp_buf save_env;

    COPY_ENV(save_env, current_env);
    
    for(;;) {
      JMP_TYPE jmptype;
      
      switch (jmptype = (JMP_TYPE) setjmp(current_env)) {
      case NO_JMP :
	Eval(*exprs);
	exprs += 1;
	break;
      case JMP_BREAK:
	Stack_Pointer = stackpointer;
	goto etiq_break;
      default:
	longjmp(save_env, jmptype);
      }
    }
  etiq_break:
    COPY_ENV(current_env, save_env);
    return 0;
  }
}

void Init_Switch()
{
  switch_inherit_instruction_function_vector._Eval = (EvalFct) Switch__Eval;
}
