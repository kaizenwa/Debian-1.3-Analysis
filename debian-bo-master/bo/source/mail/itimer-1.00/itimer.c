/*
 * Timer program for GNU Emacs timer implementation.
 * Copyright (C) 1988, 1991 Kyle E. Jones
 *
 * Verbatim copies of this file may be freely redistributed.
 *
 * Modified versions of this file may be redistributed provided that this
 * notice remains unchanged, the file contains prominent notice of
 * author and time of modifications, and redistribution of the file
 * is not further restricted in any way.
 *
 * This file is distributed `as is', without warranties of any kind.
 */

#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

#define boolean char
#define TRUE 1
#define FALSE 0

boolean signaled = FALSE;

void wakeup(int sig)
{
    (void) signal(SIGINT, wakeup);
    (void) signal(SIGALRM, wakeup);
    signaled = TRUE;
}

int main(void)
{
    unsigned sleeptime;
    long time(), lastwakeup, now;
    char timebuf[20];

    (void) signal(SIGINT, wakeup);
    (void) signal(SIGALRM, wakeup);

    (void) time(&lastwakeup);

    /*
     * 1. read the number of seconds to sleep from stdin.
     * 2. sleep until a SIGALRM or SIGINT arrives.
     * 3. report the number of seconds actually slept to stdout.
     * 4. repeat...
     */
    while (1) {
	/* read the number of seconds we should sleep */
	(void) fgets(timebuf, sizeof timebuf, stdin);
	sleeptime = atoi(timebuf);
	(void) alarm(sleeptime);
	/* sleep if no signal received since last wakeup */
	if (! signaled)
	  (void) pause();
	signaled = FALSE;
	/* report the number of seconds we actually slept */
	(void) time(&now);
	(void) sprintf(timebuf, "%ld", now - lastwakeup);
	(void) fputs(timebuf, stdout);
	(void) fflush(stdout);
	lastwakeup = now;
    }
}
