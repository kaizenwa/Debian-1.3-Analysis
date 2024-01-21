/*
 * window.h: header file for window.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: window.h,v 1.25 1995/09/03 13:45:28 mrg Exp $
 */

#ifndef __window_h_
#define __window_h_

/*
 * Define this if you want to play with the new window feature, 
 * CREATE, that allows you to start new iscreen or xterm windows
 * connected to the ircII client.
 */
#define	WINDOW_CREATE

#ifdef M_UNIX
#undef WINDOW_CREATE
#endif /* M_UNIX */

/*
 * Define this if you want ircII to scroll after printing a line,
 * like it used to (2.1.5 and back era), not before printing the
 * line.   Its a waste of a line to me, but what ever people want.
 * Thanks to Veggen for telling me what to do for this.
 */
#undef SCROLL_AFTER_DISPLAY

#include "hold.h"
#include "lastlog.h"
#include "edit.h"
#include "menu.h"

/* used by the update flag to determine what needs updating */
#define REDRAW_DISPLAY_FULL 1
#define REDRAW_DISPLAY_FAST 2
#define UPDATE_STATUS 4
#define REDRAW_STATUS 8

#define	LT_UNLOGGED	0
#define	LT_LOGHEAD	1
#define	LT_LOGTAIL	2

/* var_settings indexes */
#define OFF 0
#define ON 1
#define TOGGLE 2

	void	set_scroll_lines _((int));
	void	set_scroll _((int));
	void	reset_line_cnt _((int));
	void	set_continued_line _((char *));
	void	set_underline_video _((int));
	void	window_get_connected _((Window *, char *, int, int, char *));
	void	erase_display _((Window *));
	int	unhold_windows _((void));
	Window	*traverse_all_windows _((int *));
	void	add_to_invisible_list _((Window *));
	void	delete_window _((Window *));
	Window	*add_to_window_list _((Window *));
	void	erase_display _((Window *));
	void	set_scroll _((int));
	void	set_scroll_lines _((int));
	void	update_all_status _((void));
	void	set_query_nick _((char *));
	char	*query_nick _((void));
	void	update_window_status _((Window *, int));
	void	window _((char *, char *, char *));
	void	next_window _((unsigned char, char *));
	void	swap_last_window _((unsigned char, char *));
	void	swap_next_window _((unsigned char, char *));
	void	previous_window _((unsigned char, char *));
	void	swap_previous_window _((unsigned char, char *));
	void	back_window _((unsigned char, char *));
	void	window_kill_swap _((void));
	int	is_current_channel _((char *, int, int));
	void	redraw_all_status _((void));
	void	message_to _((u_int));
	void	message_from _((char *, int));
	void	unstop_all_windows _((unsigned char, char *));
	void	set_prompt_by_refnum _((u_int, char *));
	int	number_of_windows _((void));
	void	clear_window_by_refnum _((u_int));
	unsigned int	current_refnum _((void));
	Window	*get_window_by_refnum _((u_int));
	char	*get_target_by_refnum _((u_int));
	char	*get_prompt_by_refnum _((u_int));
	char	*set_channel_by_refnum _((unsigned int, char *));
	char	*get_channel_by_refnum _((u_int));
	void	set_window_server _((int, int, int));
	Window	*get_window_by_name _((char *));
	int	get_window_server _((unsigned int));
	int	message_from_level _((int));
	void	restore_message_from _((void));
	void	save_message_from _((void));
	void	window_check_servers _((void));
	void	set_current_window _((Window *));
	void	set_level_by_refnum _((u_int, int));
	int	is_bound _((char *, int));
	void	add_window_to_server_group _((Window *, char *));
	void	delete_window_from_server_group _((Window *, char *));
	void	window_restore_server _((int));

extern	Window	*to_window;
extern	Window	*invisible_list;
extern	int	underline;
extern	int	who_level;
extern	char	*who_from;
extern	int	in_window_command;
extern	unsigned int	window_display;

#define WINDOW_NOTIFY	((unsigned) 0x0001)
#define WINDOW_NOTIFIED	((unsigned) 0x0002)

#endif /* __window_h_ */
