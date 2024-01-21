/* signal.c -- A file which should cantain some *REAL* signals management.  */

/* Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

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

/* Written by Tudor Hulubei and Andrei Pitis.  */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#else /* !HAVE_STDLIB_H */
#include "ansi_stdlib.h"
#endif /* !HAVE_STDLIB_H */

#include <signal.h>

#include "signals.h"
#include "inputline.h"
#include "tty.h"
#include "misc.h"


extern pid_t pid;
extern char *screen;
extern input_line_t *input;


int suspend_allowed = OFF;
int signals_status;
int UserHeartAttack = 0;


#ifdef SIGSTOP

RETSIGTYPE
suspend(signum)
    int signum;
{
    signal(signum, suspend);

    if (suspend_allowed)
    {
	tty_set_mode(TTY_CANONIC);
	tty_defaults();
	tty_put_screen(screen);
	kill(pid, SIGSTOP);
    }
}

#endif


RETSIGTYPE
resume(signum)
    int signum;
{
    extern int current_mode;
    extern void refresh_after_suspend PROTO ((int));

    signal(signum, resume);

    refresh_after_suspend(current_mode);
}


RETSIGTYPE
user_panic(signum)
    int signum;
{
    signal(signum, user_panic);

    UserHeartAttack = 1;
}


void
signals_dfl()
{
    signal(SIGTERM, SIG_DFL);
    signal(SIGQUIT, SIG_DFL);
    signal(SIGINT , SIG_DFL);
}


void
signals(status)
    int status;
{
    signals_status = status;


    /* Don't use conditional expressions here, the Alpha OSF 4.0 C
       compiler will complain.  */
    if (status == ON)
    {
	signal(SIGTERM, fatal_signal);
	signal(SIGQUIT, fatal_signal);
	signal(SIGINT , user_panic);
    }
    else
    {
	signal(SIGTERM, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGINT , SIG_IGN);
    }
}


void
ignore_signals()
{
    signal(SIGILL,  SIG_IGN);
    signal(SIGTRAP, SIG_IGN);
    signal(SIGABRT, SIG_IGN);
    signal(SIGUSR1, SIG_IGN);
    signal(SIGUSR2, SIG_IGN);
    signal(SIGPIPE, SIG_IGN);
    signal(SIGFPE,  SIG_IGN);
}


void
restore_signals()
{
    signal(SIGILL,  SIG_DFL);
    signal(SIGTRAP, SIG_DFL);
    signal(SIGABRT, SIG_DFL);
    signal(SIGUSR1, SIG_DFL);
    signal(SIGUSR2, SIG_DFL);
    signal(SIGPIPE, SIG_DFL);
    signal(SIGFPE,  SIG_DFL);
}
