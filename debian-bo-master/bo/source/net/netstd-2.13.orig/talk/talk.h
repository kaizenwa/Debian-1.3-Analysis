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
 *
 *	from: @(#)talk.h	5.7 (Berkeley) 3/1/91
 *	$Id: talk.h,v 1.8 1996/12/29 17:08:03 dholland Exp $
 */

#include <curses.h>
#include <sys/socket.h>

/* 
 * Linux libc doesn't have osockaddr, but gnu libc does.
 * This has to come before include of protocols/talkd.h.
 */
#ifndef GNU_LIBC
#define osockaddr sockaddr
#endif

#include <protocols/talkd.h>

extern int sockt;
extern int curses_initialized;
extern int invitation_waiting;

extern const char *current_state;
extern int current_line;

typedef struct xwin {
	WINDOW	*x_win;
	int	x_nlines;
	int	x_ncols;
	int	x_line;
	int	x_col;
	char	kill;
	char	cerase;
	char	werase;
} xwin_t;

extern	xwin_t my_win;
extern	xwin_t his_win;
extern	WINDOW *line_win;

void p_error(const char *string);
void quit(void);
void message(const char *mesg);
void get_names(int argc, char *argv[]);
void get_addrs(char *, char *);
void init_display(void);
void open_ctl(void);
void open_sockt(void);
void start_msgs(void);
int check_local(void);
void invite_remote(void);
void end_msgs(void);
void set_edit_chars(void);
void talk(void);
void send_delete(void);
void display(xwin_t *, unsigned char *, int);
struct in_addr;
void ctl_transact(struct in_addr, CTL_MSG, int, CTL_RESPONSE *);
