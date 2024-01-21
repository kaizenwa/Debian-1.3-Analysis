/*
 *
 *	RADIUS Accounting
 *	Remote Authentication Dial In User Service
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
 */

static char sccsid[] =
"@(#)acct.c	1.6  Copyright 1994 Livingston Enterprises Inc";

#include	<sys/types.h>
#include	<sys/socket.h>
#include	<sys/time.h>
#include	<sys/file.h>
#include	<netinet/in.h>

#include	<stdio.h>
#include	<netdb.h>
#include	<pwd.h>
#include	<time.h>
#include	<ctype.h>
#include	<unistd.h>
#include	<signal.h>
#include	<errno.h>
#include	<sys/wait.h>

#include	"radius.h"

extern char		recv_buffer[4096];
extern char		send_buffer[4096];
extern char		*progname;
extern int		debug_flag;
extern char		*radacct_dir;
extern char		*radius_dir;
extern UINT4		expiration_seconds;
extern UINT4		warning_seconds;
extern int		errno;

UINT4			calctime();

rad_accounting(authreq, activefd)
AUTH_REQ	*authreq;
int		activefd;
{
	FILE		*outfd;
	char		*ip_hostname();
	char		clientname[128];
	char		buffer[512];
	VALUE_PAIR	*pair;
	long		curtime;

	strcpy(clientname, ip_hostname(authreq->ipaddr));

	/*
	 * Create a directory for this client.
	 */
	sprintf(buffer, "%s/%s", radacct_dir, clientname);
	mkdir(buffer, 0755);

	/*
	 * Write Detail file.
	 */
	sprintf(buffer, "%s/%s/detail", radacct_dir, clientname);
	if((outfd = fopen(buffer, "a")) == (FILE *)NULL) {
		sprintf(buffer,
			"Acct: Couldn't open file %s/%s/detail\n",
			radacct_dir, clientname);
		log_err(buffer);
		/* don't respond if we can't save record */
	} else {

		/* Post a timestamp */
		curtime = time(0);
		fputs(ctime(&curtime), outfd);

		/* Write each attribute/value to the log file */
		pair = authreq->request;
		while(pair != (VALUE_PAIR *)NULL) {
			fputs("\t", outfd);
			fprint_attr_val(outfd, pair);
			fputs("\n", outfd);
		pair = pair->next;
		}
		fputs("\n", outfd);
		fclose(outfd);
		/* let NAS know it is OK to delete from buffer */
		send_acct_reply(authreq, (VALUE_PAIR *)NULL, 
				(char *)NULL,activefd);
	}

	pairfree(authreq->request);
	memset(authreq, 0, sizeof(AUTH_REQ));
	free(authreq);
	return;
}

/*************************************************************************
 *
 *	Function: send_acct_reply
 *
 *	Purpose: Reply to the request with an ACKNOWLEDGE.  Also attach
 *		 reply attribute value pairs and any user message provided.
 *
 *************************************************************************/

send_acct_reply(authreq, reply, msg, activefd)
AUTH_REQ	*authreq;
VALUE_PAIR	*reply;
char		*msg;
int		activefd;
{
	AUTH_HDR		*auth;
	u_short			total_length;
	struct	sockaddr_in	saremote;
	struct	sockaddr_in	*sin;
	u_char			*ptr;
	int			len;
	UINT4			lvalue;
	u_char			digest[16];
	int			secretlen;
	char			*ip_hostname();

	auth = (AUTH_HDR *)send_buffer;

	/* Build standard header */
	auth->code = PW_ACCOUNTING_RESPONSE;
	auth->id = authreq->id;
	memcpy(auth->vector, authreq->vector, AUTH_VECTOR_LEN);

	DEBUG("Sending Accounting Ack of id %d to %lx (%s)\n",
		authreq->id, authreq->ipaddr, ip_hostname(authreq->ipaddr));

	total_length = AUTH_HDR_LEN;

	/* Load up the configuration values for the user */
	ptr = auth->data;
	while(reply != (VALUE_PAIR *)NULL) {
		debug_pair(stdout, reply);
		*ptr++ = reply->attribute;

		switch(reply->type) {

		case PW_TYPE_STRING:
			len = strlen(reply->strvalue);
			*ptr++ = len + 2;
			strcpy(ptr, reply->strvalue);
			ptr += len;
			total_length += len + 2;
			break;
			
		case PW_TYPE_INTEGER:
		case PW_TYPE_IPADDR:
			*ptr++ = sizeof(UINT4) + 2;
			lvalue = htonl(reply->lvalue);
			memcpy(ptr, &lvalue, sizeof(UINT4));
			ptr += sizeof(UINT4);
			total_length += sizeof(UINT4) + 2;
			break;

		default:
			break;
		}

		reply = reply->next;
	}

	/* Append the user message */
	if(msg != (char *)NULL) {
		len = strlen(msg);
		if(len > 0 && len < AUTH_STRING_LEN) {
			*ptr++ = PW_PORT_MESSAGE;
			*ptr++ = len + 2;
			memcpy(ptr, msg, len);
			ptr += len;
			total_length += len + 2;
		}
	}

	auth->length = htons(total_length);

	/* Calculate the response digest */
	secretlen = strlen(authreq->secret);
	memcpy(send_buffer + total_length, authreq->secret, secretlen);
	md5_calc(digest, (char *)auth, total_length + secretlen);
	memcpy(auth->vector, digest, AUTH_VECTOR_LEN);
	memset(send_buffer + total_length, 0, secretlen);

	sin = (struct sockaddr_in *) &saremote;
        memset ((char *) sin, '\0', sizeof (saremote));
	sin->sin_family = AF_INET;
	sin->sin_addr.s_addr = htonl(authreq->ipaddr);
	sin->sin_port = htons(authreq->udp_port);

	/* Send it to the user */
	sendto(activefd, (char *)auth, (int)total_length, (int)0,
			(struct sockaddr *) sin, sizeof(struct sockaddr_in));
}
