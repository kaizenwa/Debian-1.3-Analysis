/* ########################################################################

				 smac.c

   File: smac.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/smac.c
   Description: 
   Created: Tue Feb 21 13:00:09 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 13:00:10 MET 1995
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


#ifdef XCORAL
#include "smacXcoral.h"
#else
#include <stdio.h>
#endif
#include <stdlib.h>
  
#include "list.h"
#include "stack.h"
#include "control.h"
#include "Type.h"
#include "Var.h"
#include "Const.h"
#include "Block.h"
#include "Return.h"
#include "Comma.h"
#include "Function.h"
#include "FunctionCall.h"
#include "Identifier.h"
#include "function.h"
#include "Builtin.h"
#include "Declaration.h"
#include "If.h"
#include "Control.h"
#include "While.h"
#include "For.h"
#include "Cast.h"
#include "Array.h"
#include "RefDeref.h"
#include "Switch.h"

extern int yyparse();
int yyinflag = 0;

void load_file()
{
  extern void smac_read_new_file();
#ifndef mylex
  static int needed = 0;
#endif
  
  /* load_file peut etre appelee aussi bien pendant une execution que
     par Xcoral, il faut ignorer le signal pendant la lecture */
  ignore_ctrl_c();

#ifdef mylex
    smac_read_new_file();
#else
  /* pour flex, on le reinitialise ici et non dans yywrap pour que
     cela soit fait apres un ^c pendant une execution precedante */
  if (needed)
    smac_read_new_file();
  else
    needed = 1;
#endif
  
  while (! yyparse()) {
    Instruction * i;

    /* Il y a plusieurs formes a evaluer dans la pile dans le cas
       d'une declaration multiple 	type var1[=val1], ... varn[=valn]; */
    
    do {
      List * l;
      jmp_buf save_env;

      COPY_ENV(save_env, come_back);
      PopLast(Memo, l);
      switch (setjmp(come_back)) {
      case NO_JMP:
	i = (l->fct)(l);
	COPY_ENV(come_back, save_env);
	break;
      case JMP_ERROR:
	while (Memo) {
	  List * l = Memo->next;
	  
	  free(Memo);		/* c'est deja ca .. */
	  Memo = l;
	}
	COPY_ENV(come_back, save_env);
	accept_ctrl_c();
	longjmp(come_back, JMP_ERROR);
      }
    
      if (i) {
	accept_ctrl_c();
	Eval(i);
	ignore_ctrl_c();
      }
#ifndef XCORAL
      else {
	extern FILE * yyin;
	
	if (yyin == stdin)
	  putchar('\n');
      }
#endif
	
      Reinit_Control();
      Reinit_Error();
      Reinit_Stack();
    }
    while (Memo);
  }

  ignore_ctrl_c();
  
  if (Memo) {				/* ya eu erreur */
    while (Memo) {
      List * l = Memo->next;
      
      free(Memo);			/* c'est deja ca .. */
      Memo = l;
    }
  }
}


#ifdef RUNTIMECHECK
char * init_smac(stacksize, memorysize)
     int stacksize, memorysize;
{
  char * result;

  if ((result = getenv("SMAC_STACK_SIZE")) != 0) {
    int size = atoi(result);

    if (size > stacksize)
      stacksize = size;
  }
    
  Stack_Size = (stacksize / sizeof(Object)) * sizeof(Object);

  if ((result  = getenv("SMAC_MEMORY_SIZE")) != 0) {
    int size = atoi(result);

    if (size > memorysize)
      memorysize = size;
  }
    
  if ((result = init_rtcmalloc(memorysize)) != 0)
    return result;
  
#else
char * init_smac(stacksize)
     int stacksize;
{
  char * result;

  if ((result = getenv("SMAC_STACK_SIZE")) != 0) {
    int size = atoi(result);

    if (size > stacksize)
      stacksize = size;
  }
    
  Stack_Size = (stacksize / sizeof(Object)) * sizeof(Object);
#endif

#ifdef mylex
  {
    extern void mylex_init();
    
    mylex_init();
  }
#endif

  if (! setjmp(come_back)) {
    Init_Stack();
    Init_type();
    Init_Int();
    Init_Char();
    Init_String();
    Init_Block();
    Init_If();
    Init_LocalVar();
    Init_GlobalVar();
    Init_Return();
    Init_Control();
    Init_CommaExpression();
    Init_Function();
    Init_UndefinedFunctionCall();
    Init_FunctionCall();
    Init_Identifier();
    Init_function();
    Init_Builtin();
    Init_Declaration();
    Init_While();
    Init_For();
    Init_Cast();
    Init_Array();
    Init_RefDeref();
    Init_Switch();

#ifdef XCORAL
    Init_Smac_Xcoral();
#endif
  
    return 0;
  }
  else
    return err_msg;
}

