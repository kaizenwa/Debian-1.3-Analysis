/* @(#) tcpsmtp.h,v 1.4 1992/07/11 11:51:49 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * tcpsmtp.h:
 *	Interface file for transport driver in tcpsmtp.c.
 */

#ifdef HAVE_BIND
#include "../bindlib.h"
#endif

/* structure for tcpsmtp driver's private data */
struct tcpsmtp_private {
    unsigned short_timeout;		/* timeout for short SMTP commands */
    unsigned long_timeout;		/* normal SMTP read timeout */
    char *service;			/* service port for SMTP */
#ifdef HAVE_BIND
    struct bindlib_private bindlib_attr;
#endif
};

/* transport flags private to tcpsmtp.c */
#define TCPSMTP_USE_BIND    0x00010000	/* look up next_host with bind */
