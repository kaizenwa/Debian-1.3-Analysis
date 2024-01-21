/*
 * numbers.h: header for numbers.c
 *
 * written by michael sandrof
 *
 * copyright(c) 1990 
 *
 * see the copyright file, or do a help ircii copyright 
 *
 * @(#)$Id: numbers.h,v 1.8 1995/08/31 03:51:49 scottr Exp $
 */

#ifndef __numbers_h_
#define __numbers_h_

	char	*numeric_banner _((void));
	void	display_msg _((char *, char **));
	void	numbered_command _((char *, int, char **));

#endif /* __numbers_h_ */
