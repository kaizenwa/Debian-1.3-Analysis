/*
 * profil.c -- user space version of the profil(2) system call
 * Copyright (C) 1992, 1993 Rick Sladkey <jrs@world.std.com>
 *
 * This file is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 */

/*
 *     IMPORTANT:
 *
 *   This file should never be compiled with -p or -pg!
 *   It will need special consideration in the Makefile for libc_p.a
 *   to ensure that it is not compiled with any profiling flags.
 *
 *     Limitations:
 *
 * - Any program that tries to use SIGPROF itimers and profil at the
 *   same time won't work.
 *
 * - The user's program is free to trash our handler with a call to
 *   signal or sigaction.
 *
 * - Calling profil with an invalid buffer will cause a core dump instead
 *   of just disabling profiling.
 *
 * - The precision of the histogram is worse than with true kernel
 *   profiling.
 *
 */

#include <stdio.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

static struct sigaction old_sa;
static struct itimerval old_it;

static unsigned long prof_buf;
static int prof_bufsiz;
static int prof_offset;
static int prof_scale = 0;

/*
 * This depends a lot on the stack at the time the signal handler
 * is invoked.  For Linux-0.99.8 and before eip was the seventh
 * argument.  For Linux-0.99.9 and later it is the fourteenth.
 * I think.  For Linux-0.99.13 it is the sixteenth.  It's a
 * continuing mystery.
 */
  
static void
_profil_handler(int signr)
{
	unsigned long pc, spot;
	unsigned long eip = ((unsigned long *) &signr)[15];

#ifdef PROFIL_DEBUG
	printf("eip = %#x\n", eip);
#endif
	pc = eip - prof_offset;
	spot = (prof_scale*(pc >> 16)
		+ ((prof_scale*(pc & (0x10000 - 1))) >> 16)) & ~1;
	if (spot < prof_bufsiz)
		++*((unsigned short *) (spot + prof_buf));
}

int
profil(char *buf, int bufsiz, int offset, int scale)
{
	struct sigaction new_sa;
	struct itimerval new_it;
	int old_scale = prof_scale;

	if (!buf || bufsiz == 0 || scale < 2) {
		if (prof_scale) {
			setitimer(ITIMER_PROF, &old_it, &new_it);
			sigaction(SIGPROF, &old_sa, &new_sa);
		}
		prof_scale = 0;
	}
	else {
		prof_buf = (unsigned long) buf;
		prof_bufsiz = bufsiz;
		prof_offset = offset;
		prof_scale = scale;
		if (!old_scale) {
			prof_scale = scale;
			new_sa.sa_handler =
				(void (*)(int)) _profil_handler;
			new_sa.sa_mask = 0;
#ifdef SA_RESTART
			new_sa.sa_flags = SA_RESTART;
#else
			new_sa.sa_flags = 0;
#endif
			new_it.it_interval.tv_sec = 0;
			new_it.it_interval.tv_usec = 1;
			new_it.it_value.tv_sec = 0;
			new_it.it_value.tv_usec = 1;
			sigaction(SIGPROF, &new_sa, &old_sa);
			setitimer(ITIMER_PROF, &new_it, &old_it);
		}
	}
	return 0;
}
