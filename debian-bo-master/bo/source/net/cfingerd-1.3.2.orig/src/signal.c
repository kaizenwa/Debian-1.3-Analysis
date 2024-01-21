/*
 * CFINGERD
 * Signal handler version 1.10
 *
 * Fixed a bit for POSIX compliance
 * Removed SIGUNUSED and SIGSTKFLT (since they're never used, anyway)
 * Removed SIGCHLD -- caused too many problems on other OSes
 * Removed SIGKILL -- you can't trap it
 * Changed signal installation to work better and be easier to read
 * Added prog_config strings here for signal names
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

#include "cfingerd.h"
#include "proto.h"

typedef struct {
    int value;
    char *sigtype;
    int sigvalue;
    int fatal;
} SLIST;

#define NUM_SIGS 23

/* Our trapped signals */
SLIST siglist[] = {
	{SIGHUP,	"SIGHUP",	S_SIGHUP,	FALSE},
	{SIGINT,	"SIGINT",	S_SIGINT,	TRUE},
	{SIGQUIT,	"SIGQUIT",	S_SIGQUIT,	TRUE},
	{SIGILL,	"SIGILL",	S_SIGILL,	TRUE},
	{SIGTRAP,	"SIGTRAP",	S_SIGTRAP,	FALSE},
	{SIGABRT,	"SIGABRT",	S_SIGABRT,	TRUE},
	{SIGFPE,	"SIGFPE",	S_SIGFPE,	TRUE},
	{SIGUSR1,	"SIGUSR1",	S_SIGUSR1,	FALSE},
	{SIGSEGV,	"SIGSEGV",	S_SIGSEGV,	TRUE},
	{SIGUSR2,	"SIGUSR2",	S_SIGUSR2,	FALSE},
	{SIGPIPE,	"SIGPIPE",	S_SIGPIPE,	FALSE},
	{SIGALRM,	"SIGALRM",	S_SIGALRM,	FALSE},
	{SIGTERM,	"SIGTERM",	S_SIGTERM,	TRUE},
	{SIGCONT,	"SIGCONT",	S_SIGCONT,	FALSE},
	{SIGTSTP,	"SIGTSTP",	S_SIGTSTP,	FALSE},
	{SIGTTIN,	"SIGTTIN",	S_SIGTTIN,	FALSE},
	{SIGTTOU,	"SIGTTOU",	S_SIGTTOU,	FALSE},
	{SIGIO,		"SIGIO",	S_SIGIO,	TRUE},
	{SIGXCPU,	"SIGXCPU",	S_SIGXCPU,	TRUE},
	{SIGXFSZ,	"SIGXFSZ",	S_SIGXFSZ,	TRUE},
	{SIGVTALRM,	"SIGVTALRM",	S_SIGVTALRM,	FALSE},
	{SIGPROF,	"SIGPROF",	S_SIGPROF,	FALSE},
	{SIGWINCH,	"SIGWINCH",	S_SIGWINCH,	FALSE}
};

/*
 * INT_HANDLER
 *
 * This is the main signal handler, catching any signals that are made, and
 * ending the program if the program needs to be stopped.
 */
void int_handler(int signo)
{
    int i;
    BOOL caught = FALSE;

    for (i = 0; i < NUM_SIGS; i++) {
	if (signo == siglist[i].value) {
	    printf("Signal \"%s\": %s signal\n",
		siglist[i].sigtype,
		prog_config.siglist[siglist[i].sigvalue]);
	    syslog(LOG_ERR, "\"%s\" caught", siglist[i].sigtype);

	    caught = TRUE;

	    if (siglist[i].fatal) {
		printf("\nPlease report this bug to khollis@bitgate.com!\n");
		log(LOG_ERROR, "Signal (fatal): ", siglist[i].sigtype);
	    } else {
		if (siglist[i].value == SIGALRM) {
		    printf("Ooh, you caught an alarm signal.  This may be a cause from either not\n");
		    printf("having GCC 2.5.8 used with compilation, or your script simply timed\n");
		    printf("out on you.  May want to check this.  :)\n\n");
		}

		log(LOG_WARN, "Signal: Alarm signal", " ");

		printf("Non-fatal; continuing.\n");
	    }

	    (void) fflush(stdout);

	    if (siglist[i].fatal)
		exit(PROGRAM_BUG);
	    else
		log(LOG_WARN, "Signal (non-fatal): ", siglist[i].sigtype);
	}
    }

    if (!caught) {
	printf("Whoops, the program crashed.  Could not catch the signal type.\n");
	printf("Please report this to khollis@bitgate.com.\n\n");
	syslog(LOG_ERR, "SIGUNKNOWN caught");
	(void) fflush(stdout);

	log(LOG_ERROR, "Signal: Unknown signal", " ");

	exit(PROGRAM_BUG);
    }
}

/*
 * START_HANDLER
 *
 * This simply starts the signal handler
 */
void start_handler(void)
{
    int sigs = 24, i = 0;
    int signals[] = {
	SIGHUP,  SIGINT,  SIGQUIT,   SIGILL,  SIGTRAP, SIGABRT,
	SIGFPE,  SIGUSR1, SIGSEGV,   SIGUSR2, SIGPIPE, SIGALRM,
	SIGTERM, SIGCONT, SIGTSTP,   SIGTTIN, SIGTTOU, SIGIO,
	SIGXCPU, SIGXFSZ, SIGVTALRM, SIGPROF, SIGWINCH };

    /* BSD/OS sends these... */
    signal(SIGCHLD, SIG_IGN);

    for (i = 0; i < sigs; i++)
	signal(signals[i], int_handler);
}
