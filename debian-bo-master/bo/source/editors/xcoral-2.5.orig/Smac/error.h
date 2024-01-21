/* ########################################################################

				error.h

   File: error.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/error.h
   Description: 
   Created: Tue Feb 21 12:53:18 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:53:19 MET 1995
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



#ifndef _error_h
#define _error_h

#include <setjmp.h>
#include <memory.h>

#include "fctdecl.h"
#include "word.h"

extern char * Function_Name;
extern char err_msg[];
extern int Debug_Mode;

typedef struct _memo_functions_called {
  Object last_function_called;
  struct _memo_functions_called * previous;
} memo_functions_called;

extern memo_functions_called * Last_Function_Called;

extern jmp_buf come_back;

extern FCT( void, Error_Full_Stack,()		);
extern FCT( void, Internal_Error,(char *)	);
extern FCT( void, Fatal_Error,(char *)		);
extern FCT( void, Error,(char *)		);

extern FCT( void, printf_function_called,()	);
extern FCT( void, Reinit_Error,()		);

extern int Showed_Stack_Size;


#define COPY_ENV(dest, src) memcpy((char*) dest, (char*) src, sizeof(dest))

typedef enum { NO_JMP,
	       JMP_RETURN,
	       JMP_CONTINUE, JMP_BREAK,
	       JMP_ERROR}
	JMP_TYPE;

/* Le traitement du ctrl-c */

#include <signal.h>

extern int During_Evaluation;
extern FCT( void, accept_ctrl_c,()	);
extern FCT( void, error_ctrl_c,(int)	);
extern FCT( void, clear_ctrl_c,()	);

#define ignore_ctrl_c() 		During_Evaluation = 0


#endif
