/* error handling common to all routines. */
/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */


#include "config.h"

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "slang.h"
#include "_slang.h"

void (*SLang_Error_Routine)(char *);
void (*SLang_Exit_Error_Hook)(char *);
volatile int SLang_Error = 0;
char *SLang_Error_Message;
volatile int SLKeyBoard_Quit = 0;

static char *get_error_string (void)
{
   char *str;
   
   if (!SLang_Error) SLang_Error = UNKNOWN_ERROR;
   if (SLang_Error_Message != NULL) str = SLang_Error_Message;
   else switch(SLang_Error)
     {
      case SL_INVALID_PARM: str = "Invalid Parameter"; break;
      case SL_MALLOC_ERROR : str = "Malloc Error."; break;
      case INTERNAL_ERROR: str = "Internal error"; break;
      case STACK_OVERFLOW: str = "Stack Overflow"; break;
      case STACK_UNDERFLOW: str = "Stack Underflow"; break;
      case INTRINSIC_ERROR: str = "Intrinsic Error"; break;
      case USER_BREAK: str = "User Break!"; break;
      case UNDEFINED_NAME: str = "Undefined Name"; break;
      case SYNTAX_ERROR: str = "Syntax Error"; break;
      case DUPLICATE_DEFINITION: str = "Duplicate Definition"; break;
      case TYPE_MISMATCH: str = "Type Mismatch"; break;
      case READONLY_ERROR: str = "Variable is read only."; break;
      case DIVIDE_ERROR: str = "Divide by zero."; break;
      case SL_OBJ_NOPEN: str = "Object not opened"; break;
      case SL_OBJ_UNKNOWN: str = "Object unknown"; break;
	
      case UNKNOWN_ERROR:
      default: str = "Unknown Error.";
     }
   
   SLang_Error_Message = NULL;
   return str;
}

void SLang_doerror (char *error)
{
   char err [1024];
   char *str = NULL;

   *err = 0;
   
   str = get_error_string ();

   sprintf(err, "S-Lang Error: %s: %s", ((error == NULL) ? "" : error), str);
   
   if (SLang_Error_Routine == NULL)
     {
	fputs (err, stderr);
	fputs("\r\n", stderr);
     }
   else
     (*SLang_Error_Routine)(err);
}



void _SLdo_error (char *fmt, ...)
{
   va_list ap;
   char err [1024];
   
   va_start(ap, fmt);
   (void) vsprintf (err, fmt, ap);
   va_end(ap);
   SLang_doerror (err);
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
