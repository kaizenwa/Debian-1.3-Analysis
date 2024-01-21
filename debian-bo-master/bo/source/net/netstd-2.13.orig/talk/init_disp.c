/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
 * From: @(#)init_disp.c	5.4 (Berkeley) 6/1/90
 */
char id_rcsid[] = 
  "$Id: init_disp.c,v 1.8 1996/12/29 16:54:22 dholland Exp $";

/*
 * Initialization code for the display package,
 * as well as the signal handling routines.
 */

#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <termios.h>

#include "talk.h"

static void sig_sent(int);

/* 
 * Set up curses, catch the appropriate signals,
 * and build the various windows.
 */
void
init_display(void)
{
	struct sigaction sigac;

	LINES = COLS = 0;
	if (initscr() == NULL) {
		printf("initscr failed: TERM is unset or unknown terminal type.\n");
		exit(-1);
	}
	(void) sigaction(SIGTSTP, NULL, &sigac);
	sigaddset(&sigac.sa_mask, SIGALRM);
	(void) sigaction(SIGTSTP, &sigac, NULL);
	curses_initialized = 1;
	clear();
	refresh();
	noecho();
	cbreak();
	signal(SIGINT, sig_sent);
	signal(SIGPIPE, sig_sent);
	/* curses takes care of SIGTSTP (^Z) */
	my_win.x_nlines = LINES / 2;
	my_win.x_ncols = COLS;
	my_win.x_win = newwin(my_win.x_nlines, my_win.x_ncols, 0, 0);
	scrollok(my_win.x_win, FALSE);
	wclear(my_win.x_win);

	his_win.x_nlines = LINES / 2 - 1;
	his_win.x_ncols = COLS;
	his_win.x_win = newwin(his_win.x_nlines, his_win.x_ncols,
	    my_win.x_nlines+1, 0);
	scrollok(his_win.x_win, FALSE);
	wclear(his_win.x_win);

	line_win = newwin(1, COLS, my_win.x_nlines, 0);
	box(line_win, '-', '-');
	wrefresh(line_win);
	/* let them know we are working on it */
	current_state = "No connection yet";
}

/*
 * Trade edit characters with the other talk. By agreement
 * the first three characters each talk transmits after
 * connection are the three edit characters.
 */
void
set_edit_chars(void)
{
	char buf[3];
	int cc;
	struct termios tios;

	tcgetattr(0, &tios);
	my_win.cerase = tios.c_cc[VERASE];
	my_win.kill = tios.c_cc[VKILL];
	my_win.werase = tios.c_cc[VWERASE];

	if ((unsigned char)my_win.werase == 0xff) {
		my_win.werase = '\027';	 /* control W */
	}

	buf[0] = my_win.cerase;
	buf[1] = my_win.kill;
	buf[2] = my_win.werase;
	cc = write(sockt, buf, sizeof(buf));
	if (cc != sizeof(buf) )
		p_error("Lost the connection");
	cc = read(sockt, buf, sizeof(buf));
	if (cc != sizeof(buf) )
		p_error("Lost the connection");
	his_win.cerase = buf[0];
	his_win.kill = buf[1];
	his_win.werase = buf[2];
}

void
sig_sent(int signum)
{
	(void)signum;
	message("Connection closing. Exiting");
	quit();
}

/*
 * All done talking...hang up the phone and reset terminal thingy
 */
void
quit(void)
{
	if (curses_initialized) {
		wmove(his_win.x_win, his_win.x_nlines-1, 0);
		wclrtoeol(his_win.x_win);
		wrefresh(his_win.x_win);
		endwin();
	}
	if (invitation_waiting)
		send_delete();
	exit(0);
}
