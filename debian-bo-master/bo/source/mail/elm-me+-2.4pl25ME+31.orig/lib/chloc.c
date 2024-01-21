
static char rcsid[] = "@(#)$Id: chloc.c,v 5.1 1992/10/03 22:41:36 syd Exp $";

/*******************************************************************************
 *  The Elm Mail System  -  $Revision: 5.1 $   $State: Exp $
 *
 *			Copyright (c) 1988-1992 USENET Community Trust
 *			Copyright (c) 1986,1987 Dave Taylor
 *******************************************************************************
 * Bug reports, patches, comments, suggestions should be sent to:
 *
 *	Syd Weinstein, Elm Coordinator
 *	elm@DSI.COM			dsinc!elm
 *
 *******************************************************************************
 * $Log: chloc.c,v $
 * Revision 5.1  1992/10/03  22:41:36  syd
 * Initial checkin as of 2.4 Release at PL0
 *
 *
 ******************************************************************************/

/** 

**/

#include "headers.h"

/* Is this really needed? This does same than strchr or index, except
 *  that return type is different.
 *
 *                          - K E H <hurtta@ozone.FMI.FI>
 */

/* Argument (second) can't be char because there is both prototype and
 * non-prototype declaration.
 */
int chloc(string, ch)
     char *string;
     int ch;
{
	/** returns the index of ch in string, or -1 if not in string **/
	char *s;

	for (s = string; *s; s++)
		if (*s == ch)
			return(s - string);
	return(-1);
}

int qchloc(string, ch)
     char *string;
     int ch;
{
	/* returns the index of ch in string, or -1 if not in string
         * skips over quoted portions of the string
	 */
	register char *s;
	register int l;

	for (s = string; *s; s++) {
		l = len_next_part(s);
		if (l > 1) { /* a quoted char/string can not be a match */
			s += l - 1;
			continue;
		}

		if (*s == ch)
			return(s - string);
	}

	return(-1);
}

