/*
 *
 *	RADIUS Accounting -- Remote Authentication Dial In User Service
 *
 *
 *	Livingston Enterprises, Inc.
 *	6920 Koll Center Parkway
 *	Pleasanton, CA   94566
 *
 *	Copyright 1992 - 1994 Livingston Enterprises, Inc.
 *
 *	Permission to use, copy, modify, and distribute this software for any
 *	purpose and without fee is hereby granted, provided that this
 *	copyright and permission notice appear on all copies and supporting
 *	documentation, the name of Livingston Enterprises, Inc. not be used
 *	in advertising or publicity pertaining to distribution of the
 *	program without specific prior permission, and notice be given
 *	in supporting documentation that copying and distribution is by
 *	permission of Livingston Enterprises, Inc.
 *
 *	Livingston Enterprises, Inc. makes no representations about
 *	the suitability of this software for any purpose.  It is
 *	provided "as is" without express or implied warranty.
 *
 * [C] The Regents of the University of Michigan and Merit Network, Inc. 1992,
 * 1993, 1994, 1995, 1996 All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice and this permission notice appear in all
 * copies of the software and derivative works or modified versions thereof,
 * and that both the copyright notice and this permission and disclaimer
 * notice appear in supporting documentation.
 *
 * THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE REGENTS OF THE
 * UNIVERSITY OF MICHIGAN AND MERIT NETWORK, INC. DO NOT WARRANT THAT THE
 * FUNCTIONS CONTAINED IN THE SOFTWARE WILL MEET LICENSEE'S REQUIREMENTS OR
 * THAT OPERATION WILL BE UNINTERRUPTED OR ERROR FREE.  The Regents of the
 * University of Michigan and Merit Network, Inc. shall not be liable for any
 * special, indirect, incidental or consequential damages with respect to any
 * claim by Licensee or any third party arising from use of the software.
 *
 */

static char     sccsid[] =
	"@(#)rad.accounting.c	1.5  Copyright 1994 Livingston Enterprises Inc";

static char     rcsid[] = "$Id: rad.accounting.c,v 1.10 1996/04/04 18:00:31 web Exp $";

#include	<sys/types.h>
#include	<sys/param.h>
#include	<sys/socket.h>
#include	<sys/time.h>
#include	<sys/file.h>
#include	<sys/stat.h>
#include	<netinet/in.h>

#include	<stdio.h>
#include	<netdb.h>
#include	<time.h>
#include	<sys/wait.h>
#include	<syslog.h>

#include	"radius.h"

extern char     send_buffer[4096];
extern char     port_msg[128];
extern int      debug_flag;
extern u_short  inetd;
extern u_short  aport;
extern char    *radacct_dir;

static void     rad_acct_init PROTO((AATV *));
static int 	rad_acct_action PROTO((AUTH_REQ *, int, char *));

static AATV     acct_aatv =
		{
			"ACCT",
			-1,
			AA_SOCKET,
			rad_acct_init,
			NULL,
			rad_acct_action,
			rad_recv,
			NULL,
			-1
		};

AATVPTR         rad_acct_aatv = & acct_aatv;

/*************************************************************************
 *
 *	Function: rad_acct_action
 *
 *	Purpose: Process Accounting requests.
 *
 *************************************************************************/

static int
rad_acct_action (authreq, value, afpar)

AUTH_REQ       *authreq;
int             value;
char           *afpar;

{
	int             i;
	long            curtime;
	FILE           *outfd;
	VALUE_PAIR     *pair;
	char            buffer[MAXPATHLEN];
	char            clientname[AUTH_ID_LEN];
	static char    *func = "rad_acct_action";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	strcpy (clientname, ip_hostname (authreq->ipaddr));

	/*
	 * Create a directory for this client.
	 */
	sprintf (buffer, "%s/%s", radacct_dir, clientname);
	i = mkdir (buffer, 0755);

	/*
	 * Write Detail file.
	 */
	sprintf (buffer, "%s/%s/detail", radacct_dir, clientname);
	if ((outfd = fopen (buffer, "a")) == (FILE *) NULL)
	{
		logit (LOG_DAEMON, LOG_ERR,
			"%s: Couldn't open file %s/%s/detail",
			func, radacct_dir, clientname);
		return EV_ERROR;
	}

	/* Post a timestamp */
	curtime = time (0);
	fputs (ctime (&curtime), outfd);

	/* Write each attribute/value to the call detail file */
	pair = authreq->request;
	while (pair != (VALUE_PAIR *) NULL)
	{
		fputs ("\t", outfd);
		fprint_attr_val (outfd, pair);
		fputs ("\n", outfd);
		debug_pair (stdout, pair);
		pair = pair->next;
	}

	fputs ("\n", outfd);
	fclose (outfd);

	return EV_ACK;
} /* end of rad_acct_action () */

/*************************************************************************
 *
 *	Function: rad_acct_init
 *
 *	Purpose: Open a socket for sending to and receiving from NAS's.
 *		 Check if there's a socket on file descriptor zero.  If so,
 *		 check the socket port number.  If it is not ours, get and
 *		 bind the client request socket ourself.  Otherwise, use
 *		 this file descriptor for the client request socket.  If
 *		 file descriptor zero is not a socket, get and bind the
 *		 client request socket ourself.
 *
 *************************************************************************/

static void
rad_acct_init (aatv)

AATV           *aatv;

{
	struct sockaddr_in  lclsin;
	int             lcllen;
	int             acct_sockfd = 0;
	static char    *func = "rad_acct_init";

	dprintf(3, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if (aatv->sockfd == -1)
	{
		lcllen = sizeof (lclsin);
		memset ((char *) &lclsin, '\0', lcllen);
		if ((getsockname (acct_sockfd, (struct sockaddr *) &lclsin,
								&lcllen) < 0) ||
				lclsin.sin_port != htons(aport))
		{
			acct_sockfd = setupsock (&lclsin, aport);
		}
		else
		{
			inetd++;	/* indicate automatic invocation */
		}
		aatv->sockfd = acct_sockfd;
	}

	return;
} /* end of rad_acct_init () */

static int 	rad_acct_switch_action PROTO((AUTH_REQ *, int, char *));

static AATV     acct_switch_aatv =
		{
			"ACCT_SWITCH",
			-1,
			AA_DIRECT,
			NULL,
			NULL,
			rad_acct_switch_action,
			NULL,
			NULL,
			0
		};

AATVPTR         rad_acct_switch_aatv = & acct_switch_aatv;

/*************************************************************************
 *
 *	Function: rad_acct_switch_action
 *
 *	Purpose: Generate an event depending on the Accounting status type.
 *
 *************************************************************************/

static int
rad_acct_switch_action (authreq, value, afpar)

AUTH_REQ       *authreq;
int             value;
char           *afpar;

{
	VALUE_PAIR     *vp;
	static char    *func = "rad_acct_switch_action";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if ((vp = get_vp (authreq->request, PW_ACCT_STATUS_TYPE))
							== (VALUE_PAIR *) NULL)
	{
		logit (LOG_AUTH, LOG_ERR,
			"%s: missing accounting status type", func);
		return EV_NAK;
	}

	switch (vp->lvalue)
	{
	     case PW_STATUS_START:
		return EV_ACCT_START;
		break;

	     case PW_STATUS_STOP:
		return EV_ACCT_STOP;
		break;

	     case PW_STATUS_ALIVE:
		return EV_ACCT_ALIVE;
		break;

	     case PW_STATUS_MODEM_START:
		return EV_ACCT_MODEM_START;
		break;

	     case PW_STATUS_MODEM_STOP:
		return EV_ACCT_MODEM_STOP;
		break;

	     case PW_STATUS_CANCEL:
		return EV_ACCT_CANCEL;
		break;

	     default:
		return EV_NAK;
	}
} /* end of rad_acct_switch_action () */

static int 	accounting_action PROTO((AUTH_REQ *, int, char *));

static AATV     acct_accounting =
		{
			"ACCOUNTING",
			-1,
			AA_DIRECT,
			NULL,
			NULL,
			accounting_action,
			NULL,
			NULL,
			0
		};

AATVPTR         rad_accounting_aatv = & acct_accounting;

/*************************************************************************
 *
 *	Function: accounting_action
 *
 *	Purpose: Creates an Accounting request given Acct-Status-Type.
 *
 *************************************************************************/

static int
accounting_action (authreq, value, afpar)

AUTH_REQ       *authreq;
int             value;		/* holds Acct-Status-Type */
char           *afpar;		/* holds Acct-Session-Id */

{
	AUTH_REQ       *newreq;
	static char    *func = "accounting_action";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	newreq = build_acct_req (authreq, value, afpar, 0, (VALUE_PAIR *) NULL);

	start_fsm (newreq, EV_NEW_ACCT, (char *) NULL, (char *) NULL);

	return EV_ACK;
} /* end of accounting_action () */
