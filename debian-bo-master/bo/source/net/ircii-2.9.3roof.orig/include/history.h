/*
 * history.h: header for history.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: history.h,v 1.9 1995/08/31 03:51:28 scottr Exp $
 */

#ifndef __history_h_
#define __history_h_

	void	set_history_size _((int));
	void	set_history_file _((char *));
	void	add_to_history _((char *));
	char	*get_from_history _((int));
	char	*do_history _((char *, char *));
	void	history _((char *, char *, char *));

/* used by get_from_history */
#define NEXT 0
#define PREV 1

#endif /* __history_h_ */
