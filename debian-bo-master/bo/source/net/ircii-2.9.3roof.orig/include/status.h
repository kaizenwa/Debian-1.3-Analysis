/*
 * status.h: header for status.c
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: status.h,v 1.8 1995/09/03 13:45:24 mrg Exp $
 */

#ifndef __status_h_
#define __status_h_

	void	make_status _((Window *));
	void	set_alarm _((char *));
	char	*update_clock _((int));
	void	reset_clock _((char *));
	void	build_status _((char *));
	void	status_update _((int));

#define GET_TIME 1
#define RESET_TIME 2

#endif /* __status_h_ */
