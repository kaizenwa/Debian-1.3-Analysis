/* error handling common to all routines. */
/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */

#include <config.h>
#include <stdio.h>
#include <string.h>
#ifndef NO_STDLIB_H
#include <stdlib.h>
#endif

#include "slang.h"

void (*SLang_Error_Routine)(char *);
void (*SLang_Exit_Error_Hook)(char *);
volatile int SLang_Error = 0;
char *SLang_Error_Message;

void SLang_doerror(char *error)
{
   char err[256]; char *str = NULL;

   if (!SLang_Error) SLang_Error = UNKNOWN_ERROR;
   *err = 0;
   
   if (SLang_Error_Message != NULL) str = SLang_Error_Message;
   str = "Slang/Midnight Commander unknown error";
   SLang_Error_Message = NULL;
   
   sprintf(err, "S-Lang Error: %s", str);
   
   if (SLang_Error_Routine == NULL)
     {
	if (error != NULL) 
	  {
	     fputs(error, stderr);
	     fputs("\r\n", stderr);
	  }
	
	if (str != error) 
	  {
	     fputs(err, stderr);
	     fputs("\r\n", stderr);
	  }
     }
   else
     {	if (error != NULL) (*SLang_Error_Routine)(error);
	if (str != error) (*SLang_Error_Routine)(err);
     }
}

void SLang_exit_error (char *s)
{
   if (SLang_Exit_Error_Hook != NULL)
     {
	(*SLang_Exit_Error_Hook) (s);
     }
   if (s != NULL) fprintf (stderr, "%s\n", s);
   exit (-1);
}
