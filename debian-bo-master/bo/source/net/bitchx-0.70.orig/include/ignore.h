/*
 * ignore.h: header for ignore.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: ignore.h,v 1.9 1995/08/31 03:51:33 scottr Exp $
 */

#ifndef __ignore_h_
#define __ignore_h_

/* declared in ignore.c */
	int	is_ignored _((char *, int));
	int	ignore_combo _((int, int));
	int	check_ignore _((char *, char *, char *, int));
	void	ignore _((char *, char *, char *));
	void	ignore_nickname _((char *, int, int));

extern	int	ignore_usernames;
extern	char	*highlight_char;

/* Type of ignored nicks */
#define IGNORE_MSGS	0x0001
#define IGNORE_PUBLIC	0x0002
#define IGNORE_WALLS	0x0004
#define IGNORE_WALLOPS	0x0008
#define IGNORE_INVITES	0x0010
#define IGNORE_NOTICES	0x0020
#define IGNORE_NOTES	0x0040
#define IGNORE_CTCPS	0x0080
#define IGNORE_CRAP	0x0100
#define IGNORE_CDCC	0x0200
#define IGNORE_ALL (IGNORE_MSGS | IGNORE_PUBLIC | IGNORE_WALLS | \
	IGNORE_WALLOPS | IGNORE_INVITES | IGNORE_NOTICES | IGNORE_NOTES | \
	IGNORE_CTCPS | IGNORE_CRAP | IGNORE_CDCC)

#define IGNORED 1
#define DONT_IGNORE 2
#define HIGHLIGHTED -1

extern Ignore *ignored_nicks;

#endif /* __ignore_h_ */
