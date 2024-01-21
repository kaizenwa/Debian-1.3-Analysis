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

static char     rcsid[] = "$Id: rad.kchap.c,v 1.9 1996/05/22 19:58:19 web Exp $";

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

#include	"radius.h"

extern char     recv_buffer[4096];
extern char     send_buffer[4096];
extern char     port_msg[128];
extern int      debug_flag;

#ifndef KCHAP
AATVPTR         rad_kchp_aatv = NULL;
#else

static int      kchap_pass PROTO((AUTH_REQ *, int, char *));

static AATV	kchap_aatv =
{"KCHAP", AA_KCHAP, AA_DIRECT, NULL, NULL, kchap_pass, NULL, NULL, 0};

AATVPTR		rad_kchp_aatv = & kchap_aatv;

/*************************************************************************
 *
 *	Function: kchap_pass
 *
 *	Purpose: Check users password against MIT Kerberos server for
 *		 the specified realm.
 *
 *************************************************************************/

static int
kchap_pass (authreq, value, af_param)

AUTH_REQ       *authreq;
int             value;
char	       *af_param;

{
	char            key[42];
	int             id_to_key ();
	VALUE_PAIR     *vp;
	char            id[AUTH_ID_LEN + 1];
	static char    *func = "kchap_pass";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if ((vp = get_vp (authreq->cur_request, PW_USER_ID))
							== (VALUE_PAIR *) NULL)
	{
		logit (LOG_DAEMON, LOG_ALERT,
			"%s: Improper userid specification", func);
		reply_message (authreq, EC_INTERNAL, func);
		return EV_NAK;
	}
	strcpy (id, vp->strvalue);

	/* Get key (encrypted secret) for this id */

	if (id_to_key (id, key, sizeof (key)) != 0)
	{
		return EV_NAK;
	}

	/* Now use it for CHAP evaluation (NULL pw forces only CHAP check) */

	if (get_passwd (authreq, (char *) NULL, key, (char *) NULL) != 0)
	{
		memset (key, '\0', sizeof (key));
		return EV_NAK;
	}
	memset (key, '\0', sizeof (key));
	return EV_ACK;
} /* end of kchap_pass () */

#endif	/* KCHAP */
