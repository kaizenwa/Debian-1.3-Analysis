/*
 * dip		A program for handling dialup IP connecions.
 *		This module handles the TERM protocol.
 *
 * Version:	@(#)term.c	3.3.3	11/16/94
 *
 *		This program is free software; you can redistribute it
 *		and/or  modify it under  the terms of  the GNU General
 *		Public  License as  published  by  the  Free  Software
 *		Foundation;  either  version 2 of the License, or  (at
 *		your option) any later version.
 *
 *		Colten Edwards Nov 11 94:
 *		Try and add term support
 */

#include "dip.h"

#include <unistd.h>

extern char **the_envp;

static char *argv[] = { "term (under dip)", NULL };

void
do_termp(struct dip *dip)
{
#if DIP_TERM
 int stat;

 (void) strcpy((char *) dip->protocol, "TERM");

 if (opt_v == 1) {
   fprintf(stderr,"DIP: Executing %s\n", _PATH_BIN_TERM);
 }
 syslog(LOG_INFO, "DIP: Executing %s", _PATH_BIN_TERM);

 dup2(tty_askfd(),0);
 dup2(tty_askfd(),1);

 seteuid(getuid()); /* otherwise term will start in shared mode */
 setegid(getgid());

 stat=execve(_PATH_BIN_TERM, argv, the_envp);

 perror("DIP: do_term(term)");    /* point of no return */
 return;
#else
 fprintf(stderr, "DIP: TERM protocol not available yet.\n");
#endif
}
