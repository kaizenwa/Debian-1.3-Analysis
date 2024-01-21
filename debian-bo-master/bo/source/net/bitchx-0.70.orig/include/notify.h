/*
 * notify.h: header for notify.c
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: notify.h,v 1.12 1995/08/31 03:51:48 scottr Exp $
 */

#ifndef __notify_h_
#define __notify_h_

	void	show_notify_list _((int));
	void	notify _((char *, char *, char *));
	void	do_notify _((void));
	void	notify_mark _((char *, int, int));
	void	save_notify _((FILE *));
	void	set_notify_handler _((Window *, char *, int));
	void	make_notify_list _((int));
	int	hard_uh_notify _((int, char *, char *, char *));
extern	char	*get_notify_nicks _((int, int));
		
#endif /* __notify_h_ */
