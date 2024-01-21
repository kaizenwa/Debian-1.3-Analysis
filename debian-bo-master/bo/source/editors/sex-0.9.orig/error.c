/*
 * File:	error.c
 * Purpose:	Implement error reporting functions.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: error.c,v 1.2 1996/11/19 05:17:38 liw Exp $"
 */


#include <stdarg.h>
#include <stdio.h>
#include "error.h"


/*
 * Variables:	error_list, error_count, error_start
 * Purpose:	List of error messages.
 * Note:	This shouldn't be dynamic, since that introduces more
 *		possibilities for errors.
 */

#define MAX_ERRORS	2
#define MAX_ERROR_LEN	200
static struct {
	char msg[MAX_ERROR_LEN];
	struct win *win;
} error_list[MAX_ERRORS];
static int error_count = 0;
static int error_start = 0;



/*
 * Function:	error
 * Purpose:	Add new error to queue of error messages.
 * Arguments:	win	the window for which the error occurred
 *			(NULL means relevant to all windows)
 *		other arguments are like in printf.
 * Return:	Nothing.
 * Note:	The output may not be more than MAX_ERROR_LEN-1 characters.
 */
void error(struct win *win, const char *fmt, ...) {
	va_list args;
	int i;
	static FILE *devnull = NULL;
	int n;

	if (error_count == MAX_ERRORS)
		error_start = (error_start + 1) % MAX_ERRORS;
	i = (error_start + error_count) % MAX_ERRORS;
	++error_count;
	
	if (devnull == NULL) {
		devnull = fopen("/dev/null", "w");
		if (devnull == NULL) {
			sprintf(error_list[i].msg, "couldn't open /dev/null");
			return;
		}
	}

	va_start(args, fmt);
	n = vfprintf(devnull, fmt, args);
	va_end(args);
	
	if (n >= MAX_ERROR_LEN)
		sprintf(error_list[i].msg, "error message too long");
	else {
		va_start(args, fmt);
		vsprintf(error_list[i].msg, fmt, args);
		error_list[i].win = win;
		va_end(args);
	}
}



/*
 * Function:	next_error
 * Purpose:	Return next error message and remove it from list.
 * Arguments:	win	the window for which messages should be reported
 * Return:	Pointer to string, NULL if none.
 * Note:	The string will be overwritten by a future call to error.
 */
char *next_error(struct win *win) {
	char *p;

	if (error_count == 0)
		return NULL;

	p = error_list[error_start].msg;
	error_start = (error_start + 1) % MAX_ERRORS;
	--error_count;
	return p;
}
