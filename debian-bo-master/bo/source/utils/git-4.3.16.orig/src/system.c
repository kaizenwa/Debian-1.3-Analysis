/* system.c -- The code needed in order to start child processes.  */

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

#include <sys/types.h>

#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include <errno.h>

/* Not all systems declare ERRNO in errno.h... and some systems #define it! */
#if !defined (errno)
extern int errno;
#endif /* !errno */

#include "xmalloc.h"
#include "xstring.h"
#include "xio.h"
#include "tty.h"
#include "signals.h"
#include "inputline.h"
#include "system.h"
#include "misc.h"


char *stdout_log_name = NULL;
char *stderr_log_name = NULL;

extern int signals_status;
extern char *screen;

/* This should be read from the configuration file here, in system.c.
   We will be able to do so when the hooks package will be ready.  */
extern char *TempDirectory;


extern char il_read_char PROTO ((char *, char *, int));


int
start(cmd, hide)
    char *cmd;
    int hide;
{
    int child_exit_code;
    FILE *stdout_log, *stderr_log;

    if (hide)
    {
	/* It is not a good idea to close the terminal descriptors.
	   We might loose.  Suppose git has been started as root + su
	   user.  git will run as user, but the tty will belong to
	   root.  git will be able to use it at the beginning, since
	   it inherits the descriptors (they are already opened), but
	   once the descriptors closed, it will not be able to open
	   them again.  Therefore we don't close the descriptors but
	   instead save them (using the dup() system call) and restore
	   them later.  */

	int old_stdout = dup(1);
	int old_stderr = dup(2);

	close(1);
	close(2);

	stdout_log = fopen(stdout_log_name, "w");
	stderr_log = fopen(stderr_log_name, "w");

	restore_signals();
	signals_dfl();
	child_exit_code = system(cmd);
	signals(signals_status);
	ignore_signals();

	fclose(stdout_log);
	fclose(stderr_log);

	dup(old_stdout);
	dup(old_stderr);

	close(old_stdout);
	close(old_stderr);
    }
    else
    {
	tty_set_mode(TTY_CANONIC);
	tty_defaults();
	tty_put_screen(screen);

	restore_signals();
	signals_dfl();
	child_exit_code = system(cmd);
	signals(signals_status);
	ignore_signals();

	xwrite(1, "\n\n", 2);
	tty_set_mode(TTY_NONCANONIC);
	tty_defaults();
    }

    return child_exit_code;
}


void
removelog()
{
    if (stdout_log_name)
	unlink(stdout_log_name);

    if (stderr_log_name)
	unlink(stderr_log_name);
}


void
display_errors(command)
    char *command;
{
    FILE *stderr_log = fopen(stderr_log_name, "r");

    if (stderr_log == NULL)
    {
	size_t buf_len = 32 + strlen(stderr_log_name);
	char *buf      = xmalloc(buf_len);

	sprintf(buf, "%s: cannot open log file %s", command, stderr_log_name);

	il_read_char(buf, NULL, IL_MOVE | IL_BEEP | IL_SAVE | IL_ERROR);
	xfree(buf);
    }
    else
    {
	char *buf = xmalloc(2048 + 1);

	while (fgets(buf, 2048 + 1, stderr_log))
	    if (il_read_char(buf, NULL, IL_MOVE | IL_ERROR) == 0)
		break;

	xfree(buf);
    }

    fclose(stderr_log);
}
