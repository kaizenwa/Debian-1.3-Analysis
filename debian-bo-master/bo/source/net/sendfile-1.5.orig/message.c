/*
 * File:	message.c
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * Contrib.:	Martin Buck (Martin-2.Buck@student.uni-ulm.de)
 *
 * History:	11 Aug 95   Framstag	initial version
 *           	 1 Sep 95   Framstag	added global variable prg
 *           	21 Dec 95   Framstag	with or without unix error message
 *           	19 Feb 95   mbuck	alternative message format
 *           	12 Apr 95   Framstag	added call to cleanup routine
 *
 * VMS-like information, warning and error message routine.
 *
 * Copyright © 1995 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>

#include "message.h"
#include "config.h"


/*
 * message - print information, warning and error messages on stderr
 *           ( shameless plug from VMS :-) )
 *
 * INPUT:  cmd		- command or programm which sends message
 *         severity	- severity of message
 *         text		- text of message
 *
 * exit program on a fatal 'F' error
 */
void message(char *cmd, char severity, const char *text)
{ char *facility;	/* facility id */
  extern char *prg;	/* name of the game */

  /* no cmd? then use global variable prg */
  if (*cmd==0) cmd=prg;

  /* strip off path */
  facility=strrchr(cmd,'/');
  if (!facility)
    facility=cmd;
  else
    facility++;

  /* print the message */
  severity=toupper(severity);
#ifdef ALT_MESSAGES
  fprintf(stderr, "%s: %s: %s", facility,
	  (severity == 'F') ? "Fatal" :
	  ((severity == 'E') ? "Error" :
	   ((severity == 'W') ? "Warning" :
	    ((severity == 'I') ? "Info" : "Unknown"))), text);
#else
  fprintf(stderr,"%%%s-%c, %s",facility,severity,text);
#endif

  /* on error print the internal error message, too */
  if ((severity=='E' || severity=='F') && errno)
    fprintf(stderr," : %s",strerror(errno));
  fprintf(stderr,"\n");

  /* exit on a fatal error */
  if (toupper(severity)=='F')
  { cleanup();
    exit(1);
  }

}
