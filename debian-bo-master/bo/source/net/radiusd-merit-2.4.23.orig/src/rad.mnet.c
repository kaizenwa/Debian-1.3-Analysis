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
 */

static char     rcsid[] =
		"$Id: rad.mnet.c,v 1.12 1996/06/11 16:31:42 web Exp $";

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

extern char       recv_buffer[4096];
extern char       send_buffer[4096];
extern char       port_msg[128];
extern int        debug_flag;
extern int        ack_nak_flag;

#ifndef MINOS
AATVPTR         rad_mnet_aatv = NULL;
#else

#include	"hash.h"
#include	"minosd.h"

char           *minostab = CONFIG_FILE;
char           *addrtab  = NULL;
int             ma_sockfd;

/* free_arcb calls this to free client request structure */

extern struct sockaddr_in fromsin;  /* remote socket info of client */

static struct sockaddr_in masin;    /* local MnetAuth socket info */

static void 	set_small_ttl PROTO((AUTH_REQ *));
void          (*free_arcb_req) () = set_small_ttl;

static void 	rad_mnet_init PROTO((AATV *));
static int	mnet_pass PROTO((AUTH_REQ *, int, char *));
static AUTH_REQ *mnet_recv PROTO((struct sockaddr_in *, UINT4, u_int, EV *));

extern int      mareq_timer PROTO((void));

static AATV     mnet_aatv =
{"MNET", AA_MNET, AA_SOCKET, rad_mnet_init, mareq_timer, mnet_pass, mnet_recv, NULL, -1};

AATVPTR		rad_mnet_aatv = & mnet_aatv;

/*************************************************************************
 *
 *	Function: rad_mnet_init
 *
 *	Purpose: Perform MNET authentication initialization.
 *
 *************************************************************************/

static void
rad_mnet_init (aatv)

AATV           *aatv;

{
	static char    *func = "rad_mnet_init";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if (aatv->sockfd == -1)
	{
		aatv->sockfd = ma_sockfd = setupsock (&masin, 0);
	}
	return;
} /* end of rad_mnet_init () */

/*************************************************************************
 *
 *	Function: set_small_ttl
 *
 *	Purpose: Kludge to force (almost) immediate timeout of this
 *		 request.  This lets Minos inform RADIUS to remove it.
 *
 *************************************************************************/

static void
set_small_ttl (authreq)

AUTH_REQ       *authreq;

{
	authreq->ttl = 1;
	return;
} /* end of set_small_ttl () */

/*************************************************************************
 *
 *	Function: mnet_pass
 *
 *	Purpose: Have MINOS authentication agent at the specified location
 *		 check the id and password.
 *
 *	Returns: EV_ACK if valid id and password,
 *		 EV_NAK if invalid,
 *		 EV_ERROR if timed out waiting for agent to respond.
 *
 *************************************************************************/

static int
mnet_pass (authreq, value, realm)

AUTH_REQ       *authreq;
int             value;
char           *realm;

{
	u_char          flags;	/* Authorization flags - use em someday */
	VALUE_PAIR     *vp;
	char           *prot;
	char            id[AUTH_ID_LEN + 1];
	char            passwd[AUTH_PASS_LEN + 1];
	char            terminfo[AUTH_ID_LEN];
	int             result;
	MINOS_AUTH_ARGS maa;
	static char    *func = "mnet_pass";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	/*
	 * First a trick to tell the server not to timeout this request.
	 * The MINOS routines do timeouts for us.
	 */
	authreq->ttl = 0;		/* Setting ttl to zero does the trick */

	/* Decrypt pw - NULL arg means we don't handle CHAP */
	if (get_passwd (authreq, passwd, (char *) NULL, (char *) NULL) != 0)
	{
		return EV_NAK;
	}

	/* Determine protocol type for Richard to log */
	if ((vp =
		get_vp (authreq->request, PW_FRAMED_PROTOCOL))
							== (VALUE_PAIR *) NULL)
	{
		prot = "dumb";
	}
	else
	{
		prot = dict_valget (vp->lvalue, vp->name)->name;
	}
	if ((vp = get_vp (authreq->request, PW_NAS_PORT)) != NULL)
	{
		sprintf (terminfo, "NAS = %s  port %u",
			 ip_hostname (authreq->ipaddr), vp->lvalue);
	}
	else
	{
		sprintf (terminfo, "NAS = %s  id = %u",
			 ip_hostname (authreq->ipaddr), authreq->id);
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

	dprintf(1, (LOG_AUTH, LOG_DEBUG,
		"%s: ID = %s Realm = %s Info = %s Prot = %s",
		func, id, realm, terminfo, prot));

	/* Make sure MINOS database is initialized and current */

	if (readtab () != 0)
	{
		return EV_NAK;
	}

	maa.hst_name = realm;
	maa.id = id;
	maa.password = passwd;
	maa.terminfo = terminfo;
	maa.servinfo = ":telnet";
	maa.destinfo = prot;
	maa.client_req = (char *) authreq;
	maa.req_len = 0;		/* Says "don't copy client request" */
	maa.from_sin = &fromsin;	/* Client socket info */
	maa.authport = masin.sin_port;	/* Source port num. of socket we'll
					 * use to send request to MnetAuthd */
	port_msg[0] = '\0';
	result = minos_auth (maa, port_msg, &flags);
	memset (passwd, 0, sizeof (passwd));
	if (result == 1) /* reply will arrive later */
	{
		port_msg[0] = '\0';
		return EV_WAIT;
	}
	/* error was detected */
	ack_nak_flag = 0;
	return EV_NAK;
} /* end of mnet_pass () */

/*************************************************************************
 *
 *	Function: mnet_recv
 *
 *	Purpose: Process replies received from MINOS authentication servers.
 *
 *************************************************************************/

static AUTH_REQ *
mnet_recv (fromsin, ipaddr, len, ev)

struct sockaddr_in *fromsin;
UINT4           ipaddr;
u_int           len;
EV             *ev;

{
	int              retval;
	AUTH_REQ        *authreq;
	AUTHREQ_CTL_BLK *arcb;
	AUTHREQ_CTL_BLK *find_mareq ();
	EVENT_ENT       *event;
	EVENT_ENT      **prev_event;
	u_char           flagbits;
	char            *func = "mnet_recv";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	/* Look for matching request */

	if ((arcb =
		find_mareq (recv_buffer, fromsin, sizeof (*fromsin))) == NULL)
	{
		return (AUTH_REQ *) NULL; /* Request not found ... Forget it */
	}

	port_msg[0] = '\0';
	retval = minos_auth_reply (recv_buffer, arcb->accthost, port_msg,
				   &flagbits);
	authreq = (AUTH_REQ *) arcb->request;
	arcb->request = NULL;	/* So free_arcb won't toss authreq */
	free_arcb (arcb);

	if (retval < 0) /* error was detected */
	{
		ack_nak_flag = 0;
	}
	else
	{
		port_msg[0] = '\0';	/* Nonsense was added */
	}

	ev->value = retval;

	for (prev_event = &authreq->event_q ;
		event = *prev_event ;
		prev_event = &event->next)
	{
		if (event->sub_aatv == rad_mnet_aatv)
		{
			ev->state = event->state;
			ev->a.aatv = event->fsm_aatv;
			ev->isproxy = 0;
			strcpy (ev->xstring, event->estring);

			/* unlink this EVENT_ENT and free the memory */
			*prev_event = event->next;
			free (event);
			break;
		}
	}

	if (event == NULL)
	{
		return (AUTH_REQ *) NULL; /* Old reply ... Forget it */
	}

	authreq->ttl = MAX_REQUEST_TIME; /* Have engine do timing again */
	return authreq;
} /* end of mnet_recv () */

#endif	/* MINOS */
