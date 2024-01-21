/*
 * screen.h: header for screen.c
 *
 * written by matthew green.
 *
 * copyright (c) 1993, 1994.
 *
 * see the copyright file, or type help ircii copyright
 *
 * @(#)$Id: screen.h,v 1.14 1995/09/03 13:45:23 mrg Exp $
 */

#ifndef __screen_h_
#define __screen_h_

#include "window.h"

#define WAIT_PROMPT_LINE        0x01
#define WAIT_PROMPT_KEY         0x02

/* Stuff for the screen/xterm junk */

#define ST_NOTHING      -1
#define ST_SCREEN       0
#define ST_XTERM        1

/* This is here because it happens in so many places */
#define curr_scr_win	current_screen->current_window

	void	clear_window _((Window *));
	void	recalculate_window_positions _((void));
	int	output_line _((char *, char **, int));
	void	recalculate_windows _((void));
	Window	*create_additional_screen _((void));
	void	scroll_window _((Window *));
	Window	*new_window _((void));
	void	update_all_windows _((void));
	void	add_wait_prompt _((char *, void (*) (char *, char *), char *, int));
	void	clear_all_windows _((int));
	void	cursor_in_display _((void));
	int	is_cursor_in_display _((Screen *));
	void	cursor_not_in_display _((void));
	void	set_current_screen _((Screen *));
	void	window_redirect _((char *, int));
	void	redraw_resized _((Window *, ShrinkInfo, int));
	void	close_all_screen _((void));
	void	scrollback_forwards _((char, char *));
	void	scrollback_backwards _((char, char *));
	void	scrollback_end _((char, char *));
	void	scrollback_start _((char, char *));
RETSIGTYPE	sig_refresh_screen _((int));
	int	check_screen_redirect _((char *));
	void	kill_screen _((Screen *));
	int	rite _((Window *, char *, int, int, int, int));
	ShrinkInfo	resize_display _((Window *));
	void	redraw_window _((Window *, int));
	void	redraw_all_windows _((void));
	void	add_to_screen _((char *));
	void	do_screens _((fd_set *));
	
extern	Window	*to_window;
extern	Screen	*current_screen;
extern	Screen	*main_screen;
extern	Screen	*last_input_screen;
extern	Screen	*screen_list;

#endif /* __screen_h_ */
