/*
 * File:	error.h
 * Purpose:	Declarations for error reporting functions.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: error.h,v 1.1 1996/01/05 13:16:05 liw Exp $"
 * Description:	This file declares the functions error and next_error,
 *		which are used by most parts of SeX to report errors.
 *		error adds an error message to a queue.  next_error
 *		returns and removes the first error from the queue.
 *		next_error is used by the window display logic to pop up
 *		an error window, if there have been any errors.
 */
 
 
#ifndef error_h
#define error_h

struct win;
void error(struct win *, const char *, ...);
char *next_error(struct win *);

#endif
