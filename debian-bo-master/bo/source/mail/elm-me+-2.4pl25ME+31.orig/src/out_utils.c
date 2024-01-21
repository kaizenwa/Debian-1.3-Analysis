
static char rcsid[] = "@(#)$Id: out_utils.c,v 5.2 1993/02/03 19:06:31 syd Exp $";

/*******************************************************************************
 *  The Elm Mail System  -  $Revision: 5.2 $   $State: Exp $
 *
 *			Copyright (c) 1988-1992 USENET Community Trust
 *			Copyright (c) 1986,1987 Dave Taylor
 *****************************************************************************/

/** This file contains routines used for output in the ELM program.

**/

#include "headers.h"
#include "me.h"

static char err_buffer[SLEN];		/* store last error message */

static char central_message_buffer[SLEN];

show_last_error()
{
  /** rewrite last error message! **/
  if(RawState()) {
    error(err_buffer);
  }
}

clear_error()
{
  if(RawState()) {
    MoveCursor(elm_LINES,0);
    CleartoEOLN();
    err_buffer[0] = '\0';
  }
}

set_error(s)
char *s;
{
  strfcpy(err_buffer, s, sizeof err_buffer);
}

error(s)
char *s;
{
	/** outputs error 's' to screen at line 22, centered! **/

  dprint(1, (debugfile, "ERROR message: %s\n", 
	     s));

  if(!RawState())
    printf("%s\n", s);
  else {
    MoveCursor(elm_LINES,0);
    CleartoEOLN();
    PutLine0(elm_LINES,(elm_COLUMNS-strlen(s))/2,s);
    fflush(stdout);
  }
  strfcpy(err_buffer, s, sizeof err_buffer);	/* save it too! */
}

/*VARARGS1*/

error1(s, a)
char *s, *a;
{
  /** same as error, but with a 'printf' argument **/
  char buffer[SLEN];

  sprintf(buffer,s,a);
  error(buffer);
}

/*VARARGS1*/

error2(s, a1, a2)
char *s, *a1, *a2;
{
  /** same as error, but with two 'printf' arguments **/
  char buffer[SLEN];
  
  MCsprintf(buffer,s, a1, a2);
  error(buffer);
}

/*VARARGS1*/

error3(s, a1, a2, a3)
char *s, *a1, *a2, *a3;
{
  /** same as error, but with three 'printf' arguments **/
  char buffer[SLEN];

  MCsprintf(buffer,s, a1, a2, a3);
  error(buffer);
}

lower_prompt(s)
char *s;
{
  /** prompt user for input on LINES-1 line, left justified **/
  
  if(RawState()) {
    PutLine0(elm_LINES-1,0,s);
    CleartoEOLN();
  } else {
    fprintf(stderr,"%s\n",s);
  }
}

prompt(s)
char *s;
{
  /** prompt user for input on LINES-3 line, left justified **/

  if(RawState()) {
    PutLine0(elm_LINES-3,0,s);
    CleartoEOLN();
  } else {
    fprintf(stderr,"%s\n",s);
  }
}


set_central_message(string, arg)
char *string, *arg;
{
  /** set up the given message to be displayed in the center of
    the current window **/ 

  sprintf(central_message_buffer, string, arg);
    
}

display_central_message()
{
  /** display the message if set... **/

  if(RawState()) {
    if (central_message_buffer[0] != '\0') {
      ClearLine(elm_LINES-15);
      Centerline(elm_LINES-15, central_message_buffer);
      fflush(stdout);
    }
  }
}

clear_central_message()
{
  /** clear the central message buffer **/

  central_message_buffer[0] = '\0';
}

void sleep_message() {
  if (sleepmsg > 0)
    sleep(sleepmsg);
}
