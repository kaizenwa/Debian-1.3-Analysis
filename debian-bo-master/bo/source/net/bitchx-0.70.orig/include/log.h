/*
 * log.h: header for log.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: log.h,v 1.7 1995/08/31 03:51:42 scottr Exp $
 */

#ifndef __log_h_
#define __log_h_

	FILE	*do_log _((int, char *, FILE *));
	void	logger _((Window *, char *, int));
	void	set_log_file _((Window *, char *, int));
	void	add_to_log _((FILE *, char *));

#endif /* __log_h_ */
