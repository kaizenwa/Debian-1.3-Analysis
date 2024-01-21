/*
 * RADIUS -- Remote Authentication Dial In User Service
 *
 *
 * COPYRIGHT  (c)  1992, 1993, 1994, 1995, 1996
 * THE REGENTS OF THE UNIVERSITY OF MICHIGAN AND MERIT NETWORK, INCORPORATED
 * ALL RIGHTS RESERVED
 * 
 * PERMISSION IS GRANTED TO USE, COPY, CREATE DERIVATIVE WORKS AND REDISTRIBUTE
 * THIS SOFTWARE AND SUCH DERIVATIVE WORKS IN BINARY FORM ONLY FOR ANY PURPOSE,
 * SO LONG AS NO FEE IS CHARGED, AND SO LONG AS THE COPYRIGHT NOTICE ABOVE, THIS
 * GRANT OF PERMISSION, AND THE DISCLAIMER BELOW APPEAR IN ALL COPIES MADE; AND
 * SO LONG AS THE NAME OF THE UNIVERSITY OF MICHIGAN IS NOT USED IN ANY
 * ADVERTISING OR PUBLICITY PERTAINING TO THE USE OR DISTRIBUTION OF THIS
 * SOFTWARE WITHOUT SPECIFIC, WRITTEN PRIOR AUTHORIZATION.
 * 
 * THIS SOFTWARE IS PROVIDED AS IS, WITHOUT REPRESENTATION FROM THE UNIVERSITY
 * OF MICHIGAN AS TO ITS FITNESS FOR ANY PURPOSE, AND WITHOUT WARRANTY BY THE
 * UNIVERSITY OF MICHIGAN OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING
 * WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE.  THE REGENTS OF THE UNIVERSITY OF MICHIGAN SHALL NOT BE
 * LIABLE FOR ANY DAMAGES, INCLUDING SPECIAL, INDIRECT, INCIDENTAL, OR
 * CONSEQUENTIAL DAMAGES, WITH RESPECT TO ANY CLAIM ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OF THE SOFTWARE, EVEN IF IT HAS BEEN OR IS HEREAFTER
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 * 
 * For a License to distribute source code or to charge a fee for the program
 * or a product containing the program, contact MERIT at the University of
 * Michigan:
 * 
 * aaa-license@merit.edu
 * 
 * [This version puts NO LIMITS on the use.  It grants the right to create
 * DERIVATIVE WORKS.  The user may copy and distribute the code in the form
 * received AND DERIVATIVE WORKS, so long as no fee is charged.  If copies are
 * made, our copyright notice and the disclaimer must be included on them.  USE
 * THIS VERSION WITH CARE.  THIS VERSION VERY LIKELY WILL KILL ANY POTENTIAL
 * FOR LATER COMMERCIALIZATION OF THE SOFTWARE.]
 *
 *
 * Public entry points in this file:
 *
 */

static char     rcsid[] = "$Id: rad.tacacs.c,v 1.14 1996/05/22 19:58:19 web Exp $";

#include	<sys/types.h>
#include	<sys/param.h>
#include	<sys/socket.h>
#include	<sys/time.h>
#include	<sys/file.h>
#include	<sys/wait.h>
#include	<net/if.h>
#include	<netinet/in.h>

#include	<stdio.h>
#include	<netdb.h>
#include	<time.h>
#include	<errno.h>
#include	<signal.h>
#include	<memory.h>
#include	<syslog.h>

#include	"radius.h"

extern char      recv_buffer[4096];
extern char      send_buffer[4096];
extern char      port_msg[128];
extern char      default_tacacs_server[];
extern int       debug_flag;
extern int       ack_nak_flag;
extern char     *radius_dir;
extern AUTH_REQ *global_request_q;

#ifndef TACACS
AATVPTR          rad_tacs_aatv = NULL;
#else

#include	"tacacs.h"

static AUTH_REQ *tacs_recv PROTO((struct sockaddr_in *, UINT4, u_int, EV *));
static void      rad_tacs_init PROTO((AATV *));
static int       tacacs_pass PROTO((AUTH_REQ *, int, char *));
static int       tacacs_reply PROTO((AUTH_REQ *, u_int, UINT4));

static AATV      tacs_aatv =
{"TACACS", AA_TACACS, AA_SOCKET, rad_tacs_init, NULL, tacacs_pass, tacs_recv, NULL, -1};

AATVPTR          rad_tacs_aatv = & tacs_aatv;

/*************************************************************************
 *
 *	Function: rad_tacs_init
 *
 *	Purpose: Perform TACACS socket initialization.
 *
 *************************************************************************/

static void
rad_tacs_init (aatv)

AATV   *aatv;

{
	struct sockaddr_in lclsin;
	static char    *func = "rad_tacs_init";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if (aatv->sockfd == -1)
	{
		aatv->sockfd = setupsock (&lclsin, 0);
	}

	return;
} /* end of rad_tacs_init () */

/*************************************************************************
 *
 *	Function: tacacs_pass
 *
 *	Purpose: Check users password against TACACS authentication system.
 *		 Use RADIUS style encryption extension to TACACS.
 *
 *************************************************************************/

static int
tacacs_pass (authreq, value, realm)

AUTH_REQ       *authreq;
int             value;
char           *realm;

{
	VALUE_PAIR     *vp;
	struct sockaddr_in sin;
	struct servent *svp;
	u_short         svc_port;
	u_char          md5buf[AUTH_VECTOR_LEN + AUTH_PASS_LEN];
	UINT4           auth_ipaddr;
	int             total_length;
	char           *ptr;
	int             secretlen;
	int             i;
	char            id[AUTH_ID_LEN + 1];
	char            passwd[AUTH_PASS_LEN + 1];
	char           *secret;
	char           *pfx;
	xtacacstype    *TApkt;
	static char    *func = "tacacs_pass";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	/*
	 * If no server system DNS name is provided, a default is used.
	 * Set #define DEFAULT_TACACS_SERVER to the name of the default
	 * server system to use for TACACS authentication.
	 */
	if ((realm == NULL) || (realm[0] == '\0'))
	{
		realm = default_tacacs_server;
	}

	if ((vp = get_vp (authreq->cur_request, PW_USER_ID))
							== (VALUE_PAIR *) NULL)
	{
		logit (LOG_DAEMON, LOG_ALERT,
			"%s: Improper userid specification", func);
		reply_message (authreq, EC_INTERNAL, func);
		return EV_NAK;
	}
	strcpy (id, vp->strvalue);

	/* Find tacacs server in the database */
	if (find_client_by_name (&auth_ipaddr, realm, &secret, &pfx) != 0)
	{
		logit (LOG_DAEMON, LOG_ERR,
			"%s: server '%s' not in %s/%s",
			 func, realm, radius_dir, RADIUS_CLIENTS);
		return EV_NAK;
	}

	/*
	 * Decrypt password, if it's there.  Pass along PW_CHAP_PASSWORD, too.
	 * If the PW_USER_PASSWORD value pair is present, this routine will
	 * decrypt it in passwd.  The PW_CHAP_PASSWORD is passed along in case
	 * the TACACS server can do anything with it.
	 */

	svp = getservbyname ("tacacs", "udp");
	if (svp == (struct servent *) 0)     /* Use default configured value */
	{
		svc_port = DFLT_TACACS_UDP_PORT;
	}
	else
	{
		svc_port = svp->s_port;
	}
	TApkt = (xtacacstype *) send_buffer;
	TApkt->version = XTA_VERSION;		/* Extended tacacs */
	TApkt->type = XTA_LOGIN;		/* Login access type */
	TApkt->trans = authreq->reply_id;	/* Transaction number */
	TApkt->namelen = strlen (id);
	if ((vp = get_vp (authreq->request, PW_CHAP_PASSWORD)) == NULL)
	{
		TApkt->pwlen = AUTH_PASS_LEN + 1;
	}
	else
	{
		TApkt->pwlen = CHAP_VALUE_LENGTH + 1 + 1;
	}
	TApkt->response = 0;
	TApkt->reason = 0;
	TApkt->uuid = 0;
	TApkt->dhost = 0;
	TApkt->dport = 0;
	TApkt->lport = 0;
	TApkt->flags = 0;
	TApkt->accesslist = 0;
	ptr = send_buffer + XTACACSSIZE;
	strcpy (ptr, id);
	ptr += strlen (id);

	if (vp == NULL)		/* no PW_CHAP_PASSWORD value-pair */
	{
		memset (passwd, 0, sizeof (passwd));
		get_passwd (authreq, passwd, (char *) NULL, (char *) NULL);
		*ptr++ = 0;			/* Flag says not CHAP */
		/* Calculate the MD5 Digest */
		secretlen = strlen (secret);
		strncpy (md5buf, secret, secretlen);
		memcpy ((char *) md5buf + secretlen, (char *) authreq->vector,
			AUTH_VECTOR_LEN);
		md5_calc (ptr, md5buf, secretlen + AUTH_VECTOR_LEN);
		/* Xor the password into the MD5 digest */
		for (i = 0; i < AUTH_PASS_LEN; i++)
		{
			*ptr++ ^= passwd[i];
		}
		memset ((char *) md5buf, 0, sizeof (md5buf));
		/* Don't keep password around */
		memset (passwd, 0, sizeof (passwd));
	}
	else			/* CHAP - pass name and challenge response */
	{
		*ptr++ = 1;			/* Flag to indicate CHAP */
		memcpy (ptr, vp->strvalue, CHAP_VALUE_LENGTH + 1);
		ptr += CHAP_VALUE_LENGTH + 1;
	}
	memcpy (ptr, (char *) authreq->vector, AUTH_VECTOR_LEN);
	ptr += AUTH_VECTOR_LEN;
	total_length = ptr - send_buffer;

	dprintf(1, (LOG_AUTH, LOG_DEBUG,
		"Issuing TACACS_REQ of id %d from %lx (%s) to %s",
		authreq->reply_id, (UINT4) authreq->ipaddr,
		ip_hostname (authreq->ipaddr), realm));

	memset ((char *) &sin, '\0', sizeof (sin));
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = htonl(auth_ipaddr);
	sin.sin_port = htons(svc_port);

	sendto (tacs_aatv.sockfd, (char *) send_buffer, (int) total_length,
		 (int) 0, (struct sockaddr *) &sin, sizeof (sin));

	return EV_WAIT;		/* EV_WAIT says to expect reply later */
} /* end of tacacs_pass () */

/*************************************************************************
 *
 *	Function: tacacs_reply
 *
 *	Purpose: Validate and check reply from TACACS server.
 *
 *	Returns: EV_ACK if authentication was valid,
 *		 EV_NAK if authentication was rejected,
 *		 EV_ERROR if reply was invalid.
 *
 *************************************************************************/

static int
tacacs_reply (authreq, rcvlen, from_ipaddr)

AUTH_REQ       *authreq;
u_int           rcvlen;
UINT4           from_ipaddr;

{
	u_char          md5buf[AUTH_VECTOR_LEN];
	u_char          reply_digest[AUTH_VECTOR_LEN];
	char           *hostname;
	char           *secret;
	char           *pfx;
	char           *ptr;
	int             secretlen;
	xtacacstype    *TApkt;
	static char    *func = "tacacs_reply";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if (find_client (from_ipaddr, &hostname, &secret, &pfx) != 0)
	{
		logit (LOG_DAEMON, LOG_ERR,
			"%s: server \"%s\" not in %s/%s", func,
			ip_hostname (from_ipaddr), radius_dir, RADIUS_CLIENTS);
		return EV_ERROR;
	}

	TApkt = (xtacacstype *) recv_buffer;
	/* Set "ptr" to point to end of fixed part of TApkt. */
	/* The MD5 signature is supposed to be hidden there. */
	ptr = recv_buffer + XTACACSSIZE;
	memcpy ((char *) md5buf, ptr, AUTH_VECTOR_LEN);
	memcpy (ptr, (char *) authreq->vector, AUTH_VECTOR_LEN);
	secretlen = strlen (secret);
	memcpy (recv_buffer + rcvlen, secret, secretlen);
	md5_calc (reply_digest, recv_buffer, rcvlen + secretlen);
	memset (recv_buffer + rcvlen, 0, secretlen);
	if (memcmp ((char *) md5buf, (char *) reply_digest,
		    AUTH_VECTOR_LEN) != 0)
	{

		logit (LOG_DAEMON, LOG_INFO, "%s: Invalid reply digest from %s",
			func, ip_hostname (from_ipaddr));
		return EV_ERROR;
	}
	if ((TApkt->trans != authreq->reply_id) || (TApkt->type != XTA_ANSWER))
	{
		logit (LOG_DAEMON, LOG_INFO, "%s: Invalid reply from %s",
			func, ip_hostname (from_ipaddr));
		return EV_ERROR;
	}

	switch (TApkt->response)
	{
	    case XTA_A_ACCEPTED:
		return EV_ACK;

	    case XTA_A_REJECTED:
	    default:
		return EV_NAK;
	}
} /* end of tacacs_reply () */

/*************************************************************************
 *
 *	Function: tacs_recv
 *
 *	Purpose: Match TACACS reply with our client's request.
 *		 Looks for request on global_request_q.
 *		 Calls tacacs_reply () to validate and check the TACACS reply.
 *
 *************************************************************************/

static AUTH_REQ *
tacs_recv (sin, from_ipaddr, rcvlen, ev)

UINT4                   from_ipaddr;
struct sockaddr_in     *sin;
u_int                   rcvlen;
EV                     *ev;

{
	u_char          rcv_id;
	xtacacstype    *TApkt;
	AUTH_REQ       *authreq;
	EVENT_ENT      *event;
	EVENT_ENT     **prev_event;
	static char    *func = "tacs_recv";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	TApkt = (xtacacstype *) recv_buffer;
	rcv_id = TApkt->trans;

	for (authreq = global_request_q;
		authreq && authreq->reply_id != rcv_id;
		authreq = authreq->next)
	{
		;
	}

	if (authreq == (AUTH_REQ *) NULL)
	{
		logit (LOG_DAEMON, LOG_INFO,
			"%s: unexpected TACACS reply for request %u",
			func, rcv_id);
		return authreq;
	}

	dprintf(1, (LOG_AUTH, LOG_DEBUG,
		"Received reply to TACACS request %u", rcv_id));

	for (prev_event = &authreq->event_q ;
		event = *prev_event ;
		prev_event = &event->next)
	{
		if (event->sub_aatv == rad_tacs_aatv)
		{
			ev->state = event->state;
			ev->a.aatv = event->fsm_aatv;
			ev->isproxy = 0;
			ev->value = tacacs_reply (authreq, rcvlen, from_ipaddr);
			strcpy (ev->xstring, event->estring);

			/* unlink this EVENT_ENT and free the memory */
			*prev_event = event->next;
			free (event);
			break;
		}
	}

	if (event == (EVENT_ENT *) NULL)
	{
		logit (LOG_DAEMON, LOG_INFO,
			"%s: unexpected TACACS reply for request %u",
			func, rcv_id);
		return (AUTH_REQ *) NULL;
	}

	port_msg[0] = '\0';

	return authreq;
} /* end of tacs_recv () */

#endif	/* TACACS */
