/*
 * parse.h
 *
 * written by matthew green
 * copyright (c) 1993
 *
 * @(#)$Id: parse.h,v 1.8 1995/10/12 15:19:54 mrg Exp $
 */

#ifndef __parse_h_
# define __parse_h_

	char	*PasteArgs _((char **, int));
	void	parse_server _((char *));
	void	irc2_parse_server _((char *));

extern	char	*FromUserHost;

extern	int	doing_privmsg;

#endif /* __parse_h_ */
