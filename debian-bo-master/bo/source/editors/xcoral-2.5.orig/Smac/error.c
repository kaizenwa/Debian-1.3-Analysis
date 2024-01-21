/* ########################################################################

				error.c

   File: error.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/error.c
   Description: 
   Created: Tue Feb 21 12:53:07 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:53:08 MET 1995
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
#include <signal.h>

#include "error.h"
#include "FunctionCall.h"

/* Le nom de la fonction en cours de `digestion' */

char * Function_Name = 0;

/* L'adresse de la derniere structure de memorisation
   des fonctions dynamiquement appelees */

memo_functions_called * Last_Function_Called = 0;


/* une chaine pour memoriser les messages d'erreur */

#define MSG_SIZE 512
char err_msg[MSG_SIZE];


/* le contexte de retour */

jmp_buf come_back;


/* Le mode debug */

int Debug_Mode = 2;
int Showed_Stack_Size = 10;

/* Affiche les appels de fonction imbriques */

#ifdef XCORAL
  extern FCT(void, DisplayWMessage ,(char *, char *, int));
#endif

void printf_function_called()
{
  int n = Showed_Stack_Size;
  memo_functions_called * fc;

  if (! n)
    return;

#ifdef XCORAL
  DisplayWMessage("\nStack :\n\t", "", 0);
#else
  printf("Stack : ");
#endif

  for (fc = Last_Function_Called; (n-- > 0) && fc; fc = fc->previous) {
#ifdef XCORAL
    DisplayWMessage(Function__Name((Function *) fc->last_function_called),
		    "Smac error", 0);
    DisplayWMessage("\n\t", "", 0);
#else
    printf("%s < ", Function__Name((Function *) fc->last_function_called));
#endif
  }

#ifdef XCORAL
  DisplayWMessage((n > 0) ? "<toplevel>\n" : "...\n", "Smac error", 0);
#else
  printf((n > 0) ? "<toplevel>\n" : "...\n");
#endif
}

/* indique dans quel contexte on est */

static char * context()
{
  return (Function_Name)
	 ? Function_Name
	 : (Last_Function_Called)
	   ? Function__Name((Function *)
			      Last_Function_Called->last_function_called)
	   : "toplevel form";
}


/* Fonction de sortie en erreur */

void Error_Full_Stack()
{
#ifdef XCORAL
  strcpy(err_msg, "Error : stack is full\n");
  DisplayWMessage(err_msg, "Smac error", 1);
#else
  fprintf(stderr, "Error : stack is full\n");
  *err_msg = 0;
#endif
  printf_function_called();
  Reinit_Error();
  longjmp(come_back, JMP_ERROR);
}

static char mess[MSG_SIZE];

void Internal_Error(msg)
     char * msg;
{
#ifdef XCORAL
  strcpy(mess, msg);		/* au cas ou msg = err_msg */
  sprintf(err_msg, "Internal Error in %s : %s\n", context(), mess);
  DisplayWMessage(err_msg, "Smac internal error", 1);
#else
  fprintf(stderr, "Internal Error in %s : %s\n", context(), msg);
  *err_msg = 0;
#endif
  printf_function_called();
  Reinit_Error();
  longjmp(come_back, JMP_ERROR);
}

void Error(msg)
     char * msg;
{
#ifdef XCORAL
  strcpy(mess, msg);		/* au cas ou msg = err_msg */
  sprintf(err_msg, "Error in %s : %s\n", context(), mess);
  DisplayWMessage(err_msg, "Smac error", 1);
#else
  fprintf(stderr, "Error in %s : %s\n", context(), msg);
  *err_msg = 0;
#endif
  printf_function_called();
  Reinit_Error();
  longjmp(come_back, JMP_ERROR);
}

void Fatal_Error(msg)
     char * msg;
{
  fprintf(stderr, "Fatal Error in %s : %s\n", context(), msg);
  printf_function_called();
  /* long_jump .. */
  exit(1);
}

#ifndef XCORAL
void Warning(msg)
     char * msg;
{
  fprintf(stderr, "Warning in %s : %s\n", context(), msg);
  *err_msg = 0;
}
#endif



/* Le traitement du ctrl-c */

int During_Evaluation = 0;
int ya_eu_ctrl_c = 0;

void clear_ctrl_c()
{
  ya_eu_ctrl_c = 0;
}

void error_ctrl_c(ignore)
     int ignore;
{
  /* May be a warning (prototype is not always the same), not important */
  signal(SIGINT, error_ctrl_c);
  
  if (During_Evaluation) {
    During_Evaluation = 0;
    Error("Stop execution on CTRL-C");
  }
  else
    ya_eu_ctrl_c = 1;
}

void accept_ctrl_c()
{
  During_Evaluation = 1;

  if (ya_eu_ctrl_c) {
    ya_eu_ctrl_c = 0;
    error_ctrl_c(0);
  }
}


/**/

void Reinit_Error()
{
  Function_Name = 0;
  Last_Function_Called = 0;
}
