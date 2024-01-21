/*
 *  linux/ibcs/stream.h
 *
 *  Copyright (C) 1994  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: stream.h,v 1.4 1996/01/24 10:22:42 mike Exp $
 * $Source: /usr/CVS/ibcs/include/ibcs/stream.h,v $
 */

#ifndef _IBCS_STREAM_H_
#define _IBCS_STREAM_H_

#define MSG_HIPRI	1
#define RS_HIPRI	MSG_HIPRI
#define MSG_ANY		2
#define MSG_BAND	4

#define MORECTL		1
#define MOREDATA	2

struct strbuf {
	int	maxlen;		/* size of buffer */
	int	len;		/* number of bytes in buffer */
	char	*buf;		/* pointer to buffer */
};

/* Used for the I_PEEK STREAMS ioctl. */
struct strpeek {
	struct strbuf ctl;
	struct strbuf dat;
	long flags;
};

#endif
