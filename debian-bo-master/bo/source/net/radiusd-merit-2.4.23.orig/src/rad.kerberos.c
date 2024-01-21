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

static char     rcsid[] = "$Id: rad.kerberos.c,v 1.9 1996/05/22 19:58:19 web Exp $";

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

#ifndef M_KERB
AATVPTR           rad_mkrb_aatv = NULL;
#endif /* M_KERB */

#ifndef A_KERB
AATVPTR           rad_akrb_aatv = NULL;
#endif /* A_KERB */

#if defined(M_KERB) || defined(A_KERB)

#include	<krb.h>

extern int      mit_passwd_to_key ();
extern int      afs_passwd_to_key ();

static int      krb_pass PROTO((AUTH_REQ *, int, char *));

#if defined(A_KERB)
static AATV     akrb_aatv =
 { "AKERB", AA_AKRB, AA_FORK, NULL, NULL, krb_pass, NULL, NULL, 0 };

AATVPTR         rad_akrb_aatv = & akrb_aatv;
#endif /* A_KERB */

#if defined(M_KERB)
static AATV     mkrb_aatv =
 { "MKERB", AA_MKRB, AA_FORK, NULL, NULL, krb_pass, NULL, NULL, 0 };

AATVPTR         rad_mkrb_aatv = & mkrb_aatv;
#endif /* M_KERB */

#ifndef KRB_ENVIRON
#define KRB_ENVIRON	"KRBTKFILE"
#endif

#ifndef KRB_TK_DIR
#define KRB_TK_DIR	"/tmp/tkt_"
#endif

/*************************************************************************
 *
 *	Function: krb_pass
 *
 *	Purpose: Gets Kerberos ticket from specified realm for userid.
 *
 *	Returns: EV_ACK if id/pw pair was valid,
 *		 EV_NAK if invalid, 
 *		 EV_ERROR if no response (timed out). 
 *
 *************************************************************************/

static int
krb_pass (authreq, value, realm)

AUTH_REQ       *authreq;
int             value;
char           *realm;

{
	VALUE_PAIR     *vp;
	char            tkfile[MAXPATHLEN];
	int             krbval;
	int             krbreturn;
	char            userid[AUTH_ID_LEN + 1];
	char            passwd[AUTH_PASS_LEN + 1];
	char            lrealm[REALM_SZ];
	static char    *func = "krb_pass";

	if ((vp = get_vp (authreq->cur_request, PW_USER_ID))
							== (VALUE_PAIR *) NULL)
	{
		logit (LOG_DAEMON, LOG_ALERT,
			"%s: Improper userid specification", func);
		reply_message (authreq, EC_INTERNAL, func);
		return EV_NAK;
	}
	strcpy (userid, vp->strvalue);

	dprintf(1, (LOG_AUTH, LOG_DEBUG, "%s: ID = %s  Realm = %s\n",
		func, userid, realm));

	if (!realm || !*realm)		/* if no realm given, use local realm */
	{
		if (krb_get_lrealm (lrealm, 1) != KSUCCESS)
		{
			(void) strncpy (lrealm, KRB_REALM, sizeof (lrealm));
		}
		realm = lrealm;
	}

	if (get_passwd (authreq, passwd, (char *) NULL, (char *) NULL) != 0)
	{
		return (-1);
	}

	/* Set up the ticket file environment variable */

	strncpy (tkfile, KRB_TK_DIR, sizeof (tkfile));
	(void) setenv (KRB_ENVIRON, tkfile, 1);
	krb_set_tkt_string (tkfile);

	krbval = INTK_BADPW;  /* Fail if type is bad somehow */

	/* get the ticket */

#if defined(M_KERB)
	if (strcmp (authreq->direct_aatv->id, "MKERB") == 0)
	{
		krbval = krb_get_in_tkt (userid, "", realm, "krbtgt", realm,
					DEFAULT_TKT_LIFE, mit_passwd_to_key,
					NULL, passwd);
	}
#endif	/* M_KERB */

#if defined(A_KERB)
	if (strcmp (authreq->direct_aatv->id, "AKERB") == 0)
	{
		krbval = krb_get_in_tkt (userid, "", realm, "krbtgt", realm,
					DEFAULT_TKT_LIFE, afs_passwd_to_key,
					NULL, passwd);
	}
#endif	/* A_KERB */

	switch (krbval)
	{
	    case INTK_OK:
		krbreturn = EV_ACK;
		break;
	    case INTK_BADPW:	/* Tell client to give up on bad pw */
	    case INTK_W_NOTALL: /* Also on no pw */
		krbreturn = EV_NAK;
		break;
	    default:
		krbreturn = EV_ERROR;
		logit (LOG_DAEMON, LOG_ERR, "%s: odd Kerberos error %d",
			func, krbval);
		break;
	}

	dest_tkt ();		/* destroy the ticket */
	memset (passwd, 0, sizeof (passwd));
	return (krbreturn);
} /* end of krb_pass() */

#endif	/* M_KERB || A_KERB */
