/*
 * File:	message.h
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	12 Aug 95   Framstag	initial version
 *
 * Header-file for the VMS-like message routine.
 *
 * Copyright © 1995 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


/* print information, warning and error messages on stderr */
void message(char *, char, const char *);

/* cleanup routine before exit */
void cleanup();
