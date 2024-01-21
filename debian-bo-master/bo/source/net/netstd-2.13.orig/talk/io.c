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
 * From: @(#)io.c	5.6 (Berkeley) 3/1/91
 */
char io_rcsid[] = 
  "$Id: io.c,v 1.9 1996/12/29 16:50:37 dholland Exp $";

/*
 * This file contains the I/O handling and the exchange of 
 * edit characters. This connection itself is established in
 * ctl.c
 */

#include <sys/time.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include "talk.h"

/*
 * The routine to do the actual talking
 */
void
talk(void)
{
	fd_set read_set;
	int nb;
	unsigned char buf[BUFSIZ];

	message("Connection established");
	beep();
	wrefresh(curscr);

	current_line = 0;

	/*
	 * Wait on both the other process and standard input
	 */
	for (;;) {
		FD_ZERO(&read_set);
		FD_SET(0, &read_set);
		FD_SET(sockt, &read_set);
		nb = select(sockt+1, &read_set, NULL, NULL, NULL);
		if (nb <= 0) {
			if (errno == EINTR) {
				continue;
			}
			/* panic, we don't know what happened */
			p_error("Unexpected error from select");
			quit();
		}
		if (FD_ISSET(sockt, &read_set)) { 
			/* There is data on sockt */
			nb = read(sockt, buf, sizeof(buf));
			if (nb <= 0) {
				message("Connection closed. Exiting");
				quit();
			}
			display(&his_win, buf, nb);
		}
		if (FD_ISSET(0, &read_set)) {
			buf[0] = getch();
			nb = 1;
			display(&my_win, buf, nb);
			/* might lose data here because sockt is nonblocking */
			write(sockt, buf, nb);
		}
	}
}

/*
 * p_error prints the system error message on the standard location
 * on the screen and then exits. (i.e. a curses version of perror)
 */
void
p_error(const char *string) 
{
	wmove(my_win.x_win, current_line%my_win.x_nlines, 0);
	wprintw(my_win.x_win, "[%s : %s (%d)]\n",
	    string, strerror(errno), errno);
	wrefresh(my_win.x_win);
	move(LINES-1, 0);
	refresh();
	quit();
}

/*
 * Display string in the standard location
 */
void
message(const char *string)
{

	wmove(my_win.x_win, current_line%my_win.x_nlines, 0);
	wprintw(my_win.x_win, "[%s]\n", string);
	wrefresh(my_win.x_win);
}
