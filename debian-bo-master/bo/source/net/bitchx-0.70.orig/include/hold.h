/*
 * hold.h: header for hold.c
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: hold.h,v 1.8 1995/08/31 03:51:29 scottr Exp $
 */

#ifndef __hold_h_
#define __hold_h_

	void	remove_from_hold_list _((Window *));
	void	add_to_hold_list _((Window *, char *, int));
	void	hold_mode _((Window *, int, int));
	int	hold_output _((Window *));
	char	*hold_queue _((Window *));
	void	reset_hold _((Window *));
	int	hold_queue_logged _((Window *));
	void	toggle_stop_screen _((char, char *));

#endif /* __hold_h_ */
