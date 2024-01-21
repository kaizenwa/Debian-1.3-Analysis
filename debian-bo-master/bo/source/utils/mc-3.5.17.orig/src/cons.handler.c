/* Client interface for General purpose Linux console save/restore server
   Copyright (C) 1994 Janne Kukonlehto <jtklehto@stekt.oulu.fi>
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* The cons saver can't have a pid of 1, used to prevent bunches of */
/*#ifdef linux */
#include <config.h>

int    cons_saver_pid = 1;

#if defined(linux) || defined(__linux__)

#include "tty.h"
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include "util.h"
#include "win.h"
#include "cons.saver.h"

signed char console_flag = 0;

static int pipefd1 [2] = {-1, -1}, pipefd2 [2] = {-1, -1};

void show_console_contents (int starty, unsigned char begin_line, unsigned char end_line)
{
    unsigned char message = 0;
    unsigned short bytes = 0;
    int i;

    standend ();
    /* Is tty console? */
    if (!console_flag)
	return;
    /* Paranoid: Is the cons.saver still running? */
    if (cons_saver_pid < 1 || kill (cons_saver_pid, SIGCONT)){
	cons_saver_pid = 0;
	console_flag = 0;
	return;
    }

    /* Send command to the console handler */
    message = CONSOLE_CONTENTS;
    write (pipefd1[1], &message, 1);
    /* Check for outdated cons.saver */
    read (pipefd2[0], &message, 1);
    if (message != CONSOLE_CONTENTS)
	return;

    /* Send the range of lines that we want */
    write (pipefd1[1], &begin_line, 1);
    write (pipefd1[1], &end_line, 1);
    /* Read the corresponding number of bytes */
    read (pipefd2[0], &bytes, 2);

    /* Read the bytes and output them */
    for (i = 0; i < bytes; i++){
	if ((i % COLS) == 0)
	    move (starty+(i/COLS), 0);
	read (pipefd2[0], &message, 1);
	addch (message);
    }

    /* Read the value of the console_flag */
    read (pipefd2[0], &message, 1);
}

void handle_console (unsigned char action)
{
    char *tty_name;
    int status;

    switch (action){
    case CONSOLE_INIT:
	/* Close old pipe ends in case it is the 2nd time we run cons.saver */
	close (pipefd1[1]);
	close (pipefd2[0]);
	/* Create two pipes for communication */
	pipe (pipefd1);
	pipe (pipefd2);
	/* Get the console saver running */
	cons_saver_pid = fork ();
	if (cons_saver_pid < 0){
	    /* Can't fork */
	    /* Delete pipes */
	    close (pipefd1[1]);
	    close (pipefd1[0]);
	    close (pipefd2[1]);
	    close (pipefd2[0]);
	    console_flag = 0;
	} else if (cons_saver_pid > 0){
	    /* Parent */
	    /* Close the extra pipe ends */
	    close (pipefd1[0]);
	    close (pipefd2[1]);
	    /* Was the child successful? */
	    read (pipefd2[0], &console_flag, 1);
	    if (!console_flag){
		close (pipefd1[1]);
		close (pipefd2[0]);
		waitpid (cons_saver_pid, &status, 0);
	    }
	} else {
	    /* Child */
	    /* Close the extra pipe ends */
	    close (pipefd1[1]);
	    close (pipefd2[0]);
	    tty_name = ttyname (0);
	    /* Bind the pipe 0 to the standard input */
	    close (0);
	    dup (pipefd1[0]);
	    close (pipefd1[0]);
	    /* Bind the pipe 1 to the standard output */
	    close (1);
	    dup (pipefd2[1]);
	    close (pipefd2[1]);
	    /* Bind standard error to /dev/null */
	    close (2);
	    open ("/dev/null", O_WRONLY);
	    /* Exec the console save/restore handler */
	    execl (LIBDIR "bin/cons.saver", "cons.saver", tty_name, NULL);
	    /* Exec failed */
	    console_flag = 0;
	    write (1, &console_flag, 1);
	    close (1);
	    close (0);
	    exit (3);
	} /* if (cons_saver_pid ...) */
	break;

    case CONSOLE_DONE:
    case CONSOLE_SAVE:
    case CONSOLE_RESTORE:
	/* Is tty console? */
	if (!console_flag)
	    return;
	/* Paranoid: Is the cons.saver still running? */
	if (cons_saver_pid < 1 || kill (cons_saver_pid, SIGCONT)){
	    cons_saver_pid = 0;
	    console_flag = 0;
	    return;
	}
	/* Send command to the console handler */
	write (pipefd1[1], &action, 1);
	if (action != CONSOLE_DONE){
	    /* Wait the console handler to do its job */
	    read (pipefd2[0], &console_flag, 1);
	}
	if (action == CONSOLE_DONE || !console_flag){
	    /* We are done -> Let's clean up */
	    close (pipefd1 [1]);
	    close (pipefd2 [0]);
	    waitpid (cons_saver_pid, &status, 0);
	    console_flag = 0;
	}
	break;
    default:
	/* Nothing */
    }
}

#endif /* #ifdef linux */

