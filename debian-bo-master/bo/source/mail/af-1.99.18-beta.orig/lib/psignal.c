/* Psignal.c - Emulate the POSIX signal API with BSD or SYSV signals.
   Copyright (C) 1995, 1996 Malc Arnold.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>
#include <signal.h>
#include "psignal.h"

/* Set up the number of signals we have */

#ifndef NSIG
#ifdef _NSIG
#define NSIG _NSIG
#else /* ! _NSIG */
#define NSIG (sizeof(int) * 8)
#endif /* ! _NSIG */
#endif /* ! NSIG */

/****************************************************************************/
/* RCS info. */

#ifndef lint
static char *RcsId = "$Id: psignal.c,v 1.3 1996/08/28 17:35:38 malc Exp $";
static char *PsigId = PSIGID;
#endif /* ! lint */

/****************************************************************************/
/*LINTLIBRARY*/
/****************************************************************************/
/* Global function declarations */

#ifdef HAVE_SIGSETMASK
extern int sigsetmask(), sigblock();
extern int sigpending();
extern RETSIGTYPE (*signal())();
#endif /* HAVE_SIGSETMASK */

/* Local function declcarations */

#ifdef HAVE_SIGSETMASK
static RETSIGTYPE mark_pending();
#endif /* HAVE_SIGSETMASK */
   
/****************************************************************************/
/* A variable to check if a signal was caught */

#ifdef HAVE_SIGSETMASK
static int sig_pending = 0;
#endif /* HAVE_SIGSETMASK */

/****************************************************************************/
int sigprocmask(action, mask, old_mask)
int action;
sigset_t *mask, *old_mask;
{
	/*
	 * Adjust the signal mask for the process according to mask,
	 * blocking or unblocking the signals in mask, or setting
	 * the processes' signal mask to mask.  If old_mask is not a
	 * NULL pointer, then the old signal mask is stored there.
	 * Of course, there's nothing we can do if the system we're
	 * running on only has USG signals, but for what I'm using
	 * this stuff for, I don't have to worry about that.
	 */

#ifdef HAVE_SIGSETMASK
	sigset_t orig_mask, new_mask;

	/* BSD signals use several routines to accomplish this */

	switch (action) {
	case SIG_BLOCK:
		/* Simply block the signals in the mask */

		orig_mask = sigblock(*mask);
		break;
	case SIG_UNBLOCK:
		/* A little more complex here */

		orig_mask = sigblock(0);
		new_mask = (orig_mask & ~(*mask));
		(void) sigsetmask(new_mask);
		break;
	case SIG_SETMASK:
		/* Pretty simple too */

		orig_mask = sigsetmask(mask);
		break;
	}

	/* Now we should set the old signal mask */

	if (old_mask != NULL) {
		*old_mask = orig_mask;
	}
	return(0);
#else /* ! HAVE_SIGSETMASK */
	/* We only have USG signals, so fail */

	return(-1);
#endif /* ! HAVE_SIGSETMASK */
}
/****************************************************************************/
int sigismember(sigset, signo)
sigset_t *sigset;
int signo;
{
	/* Return whether the named signal is a member of the signal set */

	return(((*sigset) & signo) != 0);
}
/****************************************************************************/
int sigemptyset(sigset)
sigset_t *sigset;
{
	/* Initialise sigset to the empty signal set */

	*sigset = 0;
	return(0);
}
/****************************************************************************/
int sigaddset(sigset, signo)
sigset_t *sigset;
int signo;
{
	/*
	 * Add signo to the signal set sigset.  This won't do anything
	 * unless we have BSD signals, but that's not a problem since
	 * USG signals can't be masked, and so signal sets are useless.
	 */

#ifdef HAVE_SIGSETMASK	
	*sigset = (*sigset | sigmask(signo));
#endif /* HAVE_SIGSETMASK */
	return(0);
}
/****************************************************************************/
int sigpending(sigset)
sigset_t *sigset;
{
	/* Return the set of pending signals if possible */

#ifdef HAVE_SIGSETMASK
	int signo, mask;
	RETSIGTYPE (*old_handler)();
#endif /* HAVE_SIGSETMASK */

	/* First we need to clear the signal set */

	(void) sigemptyset(sigset);

#ifdef HAVE_SIGSETMASK
	/* Extract the signal mask for the process */

	mask = sigblock(0);
	
	/* Loop over possible signals checking if they're masked */

	for (signo = 0; signo < NSIG; signo++) {
		/* Check this sigal if it's currently blocked */

		if (mask & sigmask(signo)) {
			/* Set the handler for this signal */

			old_handler = signal(signo, mark_pending);
			sig_pending = 0;

			/* And unblock the signal to check for the mask */

			(void) sigsetmask(mask & ~(sigmask(signo)));
			(void) sigsetmask(mask);

			/* Restore the original signal handler */

			(void) signal(signo, old_handler);

			/* If the signal was pending then add it to sigset */

			if (sig_pending) {
				(void) sigaddset(sigset, signo);
			}
		}
	}
#endif /* ! HAVE_SIGSETMASK */

	/* We've found the pending signals, now return success */

	return(0);
}
/****************************************************************************/
#ifdef HAVE_SIGSETMASK
/*ARGSUSED*/
static RETSIGTYPE mark_pending(st)
int st;
{
	/* Mark the signal as pending */

	sig_pending = 1;
}
#endif /* ! HAVE_SIGSETMASK */
/****************************************************************************/
