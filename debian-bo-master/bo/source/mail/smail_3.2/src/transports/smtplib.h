/*
#ident	"@(#)smail/src:RELEASE-3_2:smtplib.h,v 1.6 1996/02/28 14:27:46 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 by Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 *
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * smtplib.h:
 *	interface file for routines in smtplib.c.
 */

/* values returned by functions in smtplib.c */
#define SMTP_SUCCEED	0
#define SMTP_FAIL	(-1)
#define SMTP_AGAIN	(-2)
#define SMTP_EHLO_FAIL	(-3)

#define ESMTP_none		0x0000
#define ESMTP_basic		0x0001
#define ESMTP_8bitmime		0x0002
#define ESMTP_size		0x0004
#define ESMTP_pipelining	0x0008
#define ESMTP_verbose		0x0010
#define ESMTP_one		0x0020
#define ESMTP_queue		0x0040

/*
 * the following structure is passed around between SMTP functions and
 * should be initialized as necessary to describe the SMTP connection
 * characteristics.
 *
 * NOTE:  If "in" is set to NULL, then we will be producing batch SMTP.
 */
struct smtp {
    FILE *in;				/* input channel from remote server */
    FILE *out;				/* output channel to remote server */
    unsigned short_timeout;		/* timeout period for short commands */
    unsigned long_timeout;		/* normal SMTP read timeout period */
    char *nl;				/* line terminator string */
    struct transport *tp;		/* associated transport */
    int smtp_flags;			/* ESMTP extensions supported by remote */
    unsigned long max_size;		/* message size limit of remote */
};

/* functions defined in smtplib.c */
extern int smtp_startup();
extern int smtp_send();
extern void smtp_shutdown();
