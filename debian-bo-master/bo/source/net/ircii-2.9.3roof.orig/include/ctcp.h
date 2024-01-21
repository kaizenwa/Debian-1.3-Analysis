/*
 * ctcp.h: header file for ctcp.c
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: ctcp.h,v 1.13 1995/09/06 22:05:02 scottr Exp $
 */

#ifndef __ctcp_h_
#define __ctcp_h_

#define CTCP_DELIM_CHAR '\001'
#define CTCP_DELIM_STR "\001"
#define CTCP_QUOTE_CHAR '\\'
#define CTCP_QUOTE_STR "\\"

#define CTCP_QUOTE_EM "\n\r\001\\"

#define CTCP_PRIVMSG 0
#define CTCP_NOTICE 1

#define	CTCP_SED 0
#define CTCP_VERSION 1
#define CTCP_CLIENTINFO 2
#define	CTCP_USERINFO 3
#define	CTCP_ERRMSG 4
#define	CTCP_FINGER 5
#define	CTCP_TIME 6
#define CTCP_ACTION 7
#define	CTCP_DCC_CHAT 8
#define	CTCP_UCT 9
#define CTCP_PING 10
#define CTCP_ECHO 11
#define	NUMBER_OF_CTCPS 12

extern	char	*ctcp_type[];
extern	int	sed;

	char	*do_ctcp _((char *, char *, char *));
	char	*ctcp_quote_it _((char *, int));
	char	*ctcp_unquote_it _((char *, int *));
	char	*do_notice_ctcp _((char *, char *, char *));
	int	in_ctcp _((void));
#ifdef HAVE_STDARG_H
	void    send_ctcp_reply _((char *, char *, char *, ...));
	void    send_ctcp _((char *, char *, char *, char *, ...));
#else
	void    send_ctcp_reply _(());
	void    send_ctcp _(());
#endif /* HAVE_STDARG_H */

#endif /* __ctcp_h_ */
