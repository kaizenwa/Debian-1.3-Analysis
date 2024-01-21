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

static char     rcsid[] = "$Id: authenticate.c,v 1.39 1996/06/19 18:17:13 web Exp $";

#include	<sys/types.h>

#ifdef OSF
#include	<sys/security.h>
#include	<prot.h>
#endif /* OSF */

#ifdef SIA
#include	<sia.h>
#include	<siad.h>
#endif	/* SIA */

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

#ifndef NOSHADOW
#include	<shadow.h>
#endif	/* NOSHADOW */

#include	"radius.h"

extern char     recv_buffer[4096];
extern char     send_buffer[4096];
extern char     ourhostname[MAXHOSTNAMELEN];
extern char     default_radius_server[];
extern char     port_msg[128];
extern int      debug_flag;
extern int      ack_nak_flag;
extern AATV    *authtype_tv[];

static int      chk_pass PROTO((AUTH_REQ *, int, char *));

static AATV     realm_aatv =
{"REALM", AA_REALM, AA_DIRECT, NULL, NULL, chk_pass, NULL, NULL, 0};

AATVPTR         rad_realm_aatv = & realm_aatv;

/*************************************************************************
 *
 *	Function: chk_pass
 *
 *	Purpose: Check the users password against any server
 *
 *************************************************************************/

static int
chk_pass (authreq, value, af_param)

AUTH_REQ       *authreq;
int             value;
char           *af_param;

{
	int             authprot;
	int             type;
	VALUE_PAIR     *vp;
	char           *agent;
	char           *realm;
	char           *filter;
	char            user_realm[AUTH_ID_LEN + 1];
	static char    *func = "chk_pass";

	dprintf(1, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if (get_vp (authreq->request, PW_CHAP_PASSWORD))
	{
		authprot = PW_PROTTYPE_CHAP;
	}
	else
	{
		authprot = PW_PROTTYPE_PW;
	}

	if ((vp = parse_realm (authreq)) == (VALUE_PAIR *) NULL)
	{
		sprintf (port_msg, "Improper 'userid@realm' specification");
		ack_nak_flag = 0;
		return EV_NAK;
	}
	strcpy (user_realm, vp->strvalue);

	if (user_realm == (char *) NULL || user_realm[0] == '\0')
	{
		if (find_auth_type (NULL_REALM, authprot,
				    (char *) authreq->file_pfx, &type, &agent,
				    &realm, &filter) != 0)
		{
			sprintf (port_msg,
				"Improper 'userid@realm' specification");
			ack_nak_flag = 0;
			return EV_NAK;
		}
	}
	else			/* Check for protected keyword realm value */
	{
		if (strcasecmp (user_realm, DEFAULT_REALM) == 0 ||
			strcasecmp (user_realm, NULL_REALM) == 0)
		{
			sprintf (port_msg, "Invalid authentication realm: '%s'",
				user_realm);
			ack_nak_flag = 0;
			return EV_NAK;
		}

		/* Try to match realm name in AUTHFILE */
		if (find_auth_type (user_realm, authprot,
				    (char *) authreq->file_pfx, &type, &agent,
				    &realm, &filter) != 0)
		{
			/* If no match, try for DEFAULT entry */
			if (find_auth_type (DEFAULT_REALM, authprot,
					    (char *) authreq->file_pfx, &type,
					    &agent, &realm, &filter) != 0)
			{		/* No DEFAULT configured so give up */
				sprintf (port_msg,
					"Invalid authentication realm: '%s'",
					user_realm);
				ack_nak_flag = 0;
				return EV_NAK;
			}
		}
	}

	authreq->realm_filter = filter;

	if (type == AA_REALM) 	/* Watch for misconfiguration */
	{
		/* Can't have REALM auth type in authfile! */
		logit (LOG_AUTH, LOG_ERR, "%s: Invalid Type '%s' in authfile",
			 func, type);
		return EV_NAK;
	}

	return call_action (authtype_tv[type], authreq, 0, agent);
} /* end of chk_pass () */

static int      rad_authenticate PROTO((AUTH_REQ *, int, char *));

static AATV     authen_aatv =
{"AUTHENTICATE", -1, AA_DIRECT, NULL, NULL, rad_authenticate, NULL, NULL, 0};

AATVPTR		rad_authen_aatv = & authen_aatv;

/*************************************************************************
 *
 *	Function: rad_authenticate
 *
 *	Purpose: Process and reply to an authentication request
 *
 *************************************************************************/

static int
rad_authenticate (authreq, value, realm)

AUTH_REQ       *authreq;
int             value;
char           *realm;

{
	int          found_pw = 0;
	int          protocol;
	int          result;
	int          retval;
	VALUE_PAIR  *namepair;
	VALUE_PAIR  *protpair;
	VALUE_PAIR  *check_item;
	VALUE_PAIR  *auth_item;
	VALUE_PAIR  *user_reply;
	VALUE_PAIR **prev_ptr;
	DICT_ATTR   *attr;
	char        *server_name = "";
	char         pwmsg[128];
	char        *func = "rad_authenticate";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	/* Get the username from the request */
	if (((namepair =
		get_vp (authreq->request, PW_USER_NAME))
						== (VALUE_PAIR *) NULL) ||
		(strlen (namepair->strvalue) <= 0))
	{
		logit (LOG_AUTH, LOG_ERR, "%s: from %s - No User Name",
			 func, ip_hostname (authreq->ipaddr));
		return EV_NAK;
	}

	protpair = get_vp (authreq->request, PW_FRAMED_PROTOCOL);
	protocol = (protpair == (VALUE_PAIR *) NULL) ? 0 : protpair->lvalue;

	if (authreq->user_check == (VALUE_PAIR *) NULL)
	{
		/* Look in the database only if both lists are NULL. */
		if ((retval = user_find ((char *) authreq->file_pfx,
					namepair->strvalue, protocol,
					&authreq->user_check,
					&user_reply, 0)) < 0)
		{
			logit (LOG_AUTH, LOG_ERR,
				"%s: from %s - Problem in user_find",
			 	func, ip_hostname (authreq->ipaddr));
			return EV_NAK;
		}
		if (retval > 0)		/* If no match, use DEFAULT entry */
		{
			if (user_find ((char *) authreq->file_pfx, "DEFAULT",
					protocol, &authreq->user_check,
					&user_reply, 0) != 0)
			{
				logit (LOG_AUTH, LOG_ERR,
			      "%s: from %s - Invalid/missing 'DEFAULT' entry",
			   		func, ip_hostname (authreq->ipaddr));
				return EV_NAK;
			}
		}
		list_cat (&authreq->cur_request, user_reply);
	}

	/* Log what we're doing (for diagnostic purposes) */

	logit (LOG_AUTH, LOG_INFO, "%s: %u/%u '%s' at %s %s", func,
		authreq->id, authreq->reply_id, namepair->strvalue,
		ip_hostname (authreq->ipaddr),
		type_string (authreq, protpair));

	result = EV_ACK; /* assume good until proven otherwise */

	/* No null names allowed */

	if (*namepair->strvalue == '\0')
	{
		result = EV_NAK;
	}

	/*
	 *	See if this is strictly an authentication check.
	 *	Throw out all but pw and pw expiration items if it is.
	 */
	if ((auth_item =
		get_vp (authreq->request, PW_SERVICE_TYPE))
						!= (VALUE_PAIR *) NULL &&
		(auth_item->lvalue == PW_AUTHENTICATE_ONLY))
	{
		for (prev_ptr = &authreq->user_check, check_item = *prev_ptr;
			check_item != (VALUE_PAIR *) NULL;
			check_item = *prev_ptr)
		{
			switch (check_item->attribute)
			{
			    case CI_PROHIBIT:
				if (check_item->lvalue == PW_AUTH_ONLY)
				{
					result = EV_NAK;
				}
				/* Leave in user_check list */
				prev_ptr = &check_item->next;
				break;

			    case CI_ENCRYPTED_PASSWORD:
			    case CI_USER_PASSWORD:
			    case CI_AUTHENTICATION_TYPE:
				found_pw = 1;
				/***FALLTHROUGH***/

			    case CI_EXPIRATION:
			    case CI_SERVER_NAME:
				/* Leave in user_check list */
				prev_ptr = &check_item->next;
				break;

			    default:
				/* Remove all others from list */
				*prev_ptr = check_item->next;
				free (check_item);
			}
		}

		if (found_pw == 0) /* Authenticate-Only requires a password! */
		{
			result = EV_NAK;
		}
	}


	/* Check those check items */

	/* Initialize port message string */
	port_msg[0] = '\0';
	ack_nak_flag = 0;
	for (check_item = authreq->user_check;
		((result == EV_ACK) && (check_item != (VALUE_PAIR *) NULL));
		check_item = check_item->next)
	{
		switch (check_item->attribute)
		{
			/*
			 * Check expiration date if we are doing password
			 * aging.
			 */
		    case CI_EXPIRATION:

			/* Has this user's password expired */
			retval = pw_expired (check_item->lvalue);
			if (retval < 0)
			{
				strcpy (port_msg, "Password Has Expired\r\n");
				ack_nak_flag = 0;
				result = EV_NAK;
			}
			else
			{
				if (retval > 0)
				{
					sprintf (port_msg, /* old adv_msg */
					  "Password Will Expire in %d Days\r\n",
						retval);
					ack_nak_flag = 1;
				}
			}
			continue;

		    case CI_SERVER_NAME:
			server_name = check_item->strvalue;
			continue;

		    case CI_AUTHENTICATION_TYPE:
		    case CI_SIMULTANEOUS_USE:
		    case CI_COMMENT:
		    case CI_PROHIBIT:
		    case PW_SERVICE_CLASS:
			continue;

		    case CI_ENCRYPTED_PASSWORD:
			retval = get_passwd (authreq, pwmsg,
				    check_item->strvalue, check_item->strvalue);
			if (retval != 0)
			{
				result = EV_NAK;
			}
			memset ((char *) pwmsg, '\0', sizeof (pwmsg));
			continue;

		    case CI_USER_PASSWORD:

			/*
			 *	Pass password to get_passwd() so it will
			 *	do CHAP and PASSWORD check for us.
			 */
			retval = get_passwd (authreq, pwmsg,
					   check_item->strvalue, (char *) NULL);
			if (retval != 0)
			{
				result = EV_NAK;
			}
			/* Test Code for Challenge */
			if ((strcmp (pwmsg, "challenge") == 0) && (retval == 1))
			{
				memset ((char *) pwmsg, 0, sizeof (pwmsg));
				strcpy (port_msg,
			       "You want me to challenge you??\r\nOkay I will");
				if ((auth_item =
				    (VALUE_PAIR *) malloc (sizeof (VALUE_PAIR)))
							== (VALUE_PAIR *) NULL)
				{
					logit (LOG_DAEMON, LOG_ALERT,
						"%s: FATAL out of memory",
						func);
					abort ();
				}
				attr = dict_attrget (PW_STATE);
				strcpy (auth_item->name, attr->name);
				auth_item->attribute = attr->value;
				auth_item->type = attr->type;
				auth_item->next = (VALUE_PAIR *) NULL;
				strcpy (auth_item->strvalue, "1");
				insert_vp (&authreq->cur_request,
						(VALUE_PAIR *) NULL, auth_item);
				ack_nak_flag = 1;
				result = EV_ACC_CHAL;
			}
			memset ((char *) pwmsg, 0, sizeof (pwmsg));
			continue;
		}

		/*
		 * Process check items that didn't require special processing
		 * above.  Just look for the matching attribute in the
		 * request.
		 */

		if ((auth_item =
			get_vp (authreq->request, check_item->attribute))
							!= (VALUE_PAIR *) NULL)
		{
			switch (check_item->type)
			{
			    case PW_TYPE_STRING:
				if (strcmp (check_item->strvalue,
					    auth_item->strvalue) != 0)
				{
					result = EV_NAK;
				}
				break;

			    case PW_TYPE_IPADDR:
				if (*check_item->strvalue != '\0')
				{
					/* Resolve all DNS names */
					if (find_host_by_name (
						    &check_item->lvalue,
						    check_item->strvalue) == 1)
					{
					     /* Ignore if no DNS response yet */
						logit (LOG_AUTH, LOG_WARNING,
		     "%s: request ignored - unresolved host '%s' in check item",
							func,
							check_item->strvalue);
						result = EV_ABORT;
						break;
					}
				}

			    case PW_TYPE_INTEGER:
				if (check_item->lvalue != auth_item->lvalue)
				{
					result = EV_NAK;
				}
				break;

			    default:
				result = EV_NAK;
				break;
			}
		}
		else
		{
			result = EV_NAK;	/* check item not in request */
		}
	} /* end of for loop */

	if ((result == EV_ACK) &&
	     (check_item = get_vp (authreq->user_check, CI_AUTHENTICATION_TYPE))
							!= (VALUE_PAIR *) NULL)
	{
		if (authtype_tv[check_item->lvalue] == (AATV *) NULL)
		{
			logit (LOG_DAEMON, LOG_ERR,
			 "%s: FATAL: Unsupported authentication type %d for %s",
				func, check_item->lvalue, namepair->strvalue);
			logit (LOG_DAEMON, LOG_ERR,
				"%s: CHECK THE MAKEFILE BUILD!", func);
			result = EV_FATAL;
		}
		else
		{
			result = call_action (authtype_tv[check_item->lvalue],
						authreq, 0, server_name);
		}
	}
	return result;
} /* end of rad_authenticate () */

static void     rad_2rad_init PROTO((AATV *));
static int      radius_pass PROTO((AUTH_REQ *, int, char *));

static AATV     rad2rad_aatv =
{"RAD2RAD", AA_RAD, AA_SOCKET, rad_2rad_init, NULL, radius_pass, rad_2rad_recv, NULL, -1};

AATVPTR		rad_2rad_aatv = & rad2rad_aatv;

/*************************************************************************
 *
 *	Function: rad_2rad_init
 *
 *	Purpose: Perform RADIUS to RADIUS initialization.
 *
 *************************************************************************/

static void
rad_2rad_init (aatv)

AATV   *aatv;

{
	struct sockaddr_in lclsin;
	static char    *func = "rad_2rad_init";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if (aatv->sockfd == -1)
	{
		aatv->sockfd = setupsock (&lclsin, 0);
	}
	return;
} /* end of rad_2rad_init () */

/*************************************************************************
 *
 *	Function: radius_pass
 *
 *	Purpose: Have remote RADIUS system handle authentication for this
 *		 request (RADIUS to RADIUS request)
 *
 *	Returns: EV_ACK if valid userid and pw,
 *		 EV_NAK if invalid,
 *		 EV_WAIT if request issued.
 *
 *************************************************************************/

static int
radius_pass (authreq, value, realm)

AUTH_REQ       *authreq;
int             value;
char           *realm;

{
	VALUE_PAIR     *vp;
	char            id[AUTH_ID_LEN + 1];
	static char    *func = "radius_pass";

	if ((vp = get_vp (authreq->cur_request, PW_USER_ID))
							== (VALUE_PAIR *) NULL)
	{
		logit (LOG_DAEMON, LOG_ALERT,
			"%s: Improper userid specification", func);
		reply_message (authreq, EC_INTERNAL, func);
		return EV_NAK;
	}
	strcpy (id, vp->strvalue);

	/*
	 *	If no server system DNS name is provided, a default is used.
	 *	Set #define DEFAULT_RADIUS_SERVER (or set it in authfile)
	 *	to the name of the default server system to use for
	 *	RADIUS authentication.
	 */

	if ((realm == NULL) || (realm[0] == '\0'))
	{
		realm = default_radius_server;
	}

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: name = %s  realm = %s",
		func, id, realm));

	/* Check to see if we are the server for this realm */
	if (strcasecmp (ourhostname, realm) == 0)
	{
		dprintf(2, (LOG_AUTH, LOG_DEBUG,
			"%s: handle locally", func));
		/* Treat this one just like Unix-PW authentication */
		return call_action (authtype_tv[AA_UNIX], authreq, 0, realm);
	}

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: handle remotely", func));

	if (radius_send ((char *) authreq->fsm_aatv->id, PW_ACCESS_REQUEST,
			realm, authreq, rad2rad_aatv.sockfd) == -1)
	{
		return EV_NAK;
	}

	return EV_WAIT;	/* RC says to expect reply later */
} /* end of radius_pass () */

static int      unix_pass PROTO((AUTH_REQ *, int, char *));

static AATV     unix_aatv =
{ "UNIX-PW", AA_UNIX, AA_FORK, NULL, NULL, unix_pass, NULL, NULL, 0 };

AATVPTR         rad_unix_aatv = & unix_aatv;

/*************************************************************************
 *
 *	Function: unix_pass
 *
 *	Purpose: Check the users password against the standard UNIX
 *		 password table.
 *
 *************************************************************************/

static int
unix_pass (authreq, value, af_param)

AUTH_REQ       *authreq;
int             value;
char           *af_param;

{
	int             has_etc_shells;
	VALUE_PAIR     *vp;
	struct passwd  *pwd;
	char           *encpw;
	char           *encrypted_pass;
	char           *crypt ();
#ifdef	CHK_SHELLS
	char           *getusershell ();
#endif	/* CHK_SHELLS */
	char           *pshell;
	FILE           *fp;
	char            name[AUTH_ID_LEN + 1];
	char            passwd[AUTH_PASS_LEN + 1];
	char            buffer[128];
	static char    *func = "unix_pass";

#ifdef OSF
	struct pr_passwd *getprpwnam ();
	struct pr_passwd *osf_pw_info;
#endif /* OSF */

#ifdef SIA
	int           (*sia_collect) () = sia_collect_trm;
	char           *info[2];
	char           *progname = "radius";
	SIAENTITY      *ent = NULL;
#endif

#if !defined(NOSHADOW)

#if defined(M_UNIX)
	struct passwd  *spwd;
#else	/* M_UNIX */
	struct spwd    *spwd;
#endif	/* M_UNIX */

#endif	/* !NOSHADOW */

	if ((vp = get_vp (authreq->cur_request, PW_USER_ID))
							== (VALUE_PAIR *) NULL)
	{
		logit (LOG_DAEMON, LOG_ALERT,
			"%s: Improper userid specification", func);
		reply_message (authreq, EC_INTERNAL, func);
		return EV_NAK;
	}
	strcpy (name, vp->strvalue);

	dprintf(1, (LOG_AUTH, LOG_DEBUG, "%s: ID = '%s'", func, name));

	/* Decrypt pw - NULL 3rd arg. says we don't handle CHAP */
	if (get_passwd (authreq, passwd, (char *) NULL, (char *) NULL) != 0)
	{
		memset ((char *) passwd, '\0', sizeof (passwd));
		return EV_NAK;
	}
	/* Get encrypted password from UNIX password file */
	if ((pwd = getpwnam (name)) == NULL)
	{
		memset ((char *) passwd, '\0', sizeof (passwd));
		return EV_NAK;
	}
	encrypted_pass = pwd->pw_passwd;

#ifdef OSF
	/* Get encrypted password from security files */
	if ((osf_pw_info = getprpwnam (name)) == NULL)
	{
		memset ((char *) passwd, '\0', sizeof (passwd));
		return EV_NAK;
	}
	strcpy (pwd->pw_passwd, osf_pw_info->ufld.fd_encrypt);
	pwd->pw_uid = osf_pw_info->ufld.fd_uid;
#endif /* OSF */

#ifdef ULTRIX_ENHANCED		/* 4/3/94 jeff@oakland.edu */
	if (authenticate_user (pwd, passwd, "/dev/ttypXX") < 0)
	{
		memset ((char *) passwd, '\0', sizeof (passwd));
		return EV_NAK;
	}

#elif  SIA  /* DEC OSF/1 SIA , 6/19/95 minnebo@oakland.edu */

	info[0] = progname;
	info[1] = NULL;
	if (sia_ses_init (&ent, (1), info, NULL, pwd->pw_name, NULL, TRUE, NULL)
								!= SIASUCCESS )
	{
		return EV_NAK;
	}

	if (sia_ses_authent (sia_collect, passwd, ent) != SIASUCCESS)
	{
		(void) sia_ses_release (&ent);
		return EV_NAK;
	}

	if (sia_ses_release (&ent) != SIASUCCESS)
	{
		return EV_NAK;
	}

#else	/* ULTRIX_ENHANCED */

#if !defined(NOSHADOW) || (defined(__sun__) && defined(__svr4__))
	if ((strcmp (pwd->pw_passwd, "x") == 0) ||
		(strcmp (pwd->pw_passwd, "*") == 0))
	{
		if ((spwd = getspnam (name)) == NULL)
		{
			return EV_NAK;
		}

#if defined(M_UNIX)
	encrypted_pass = spwd->pw_passwd;
#else	/* M_UNIX */
	encrypted_pass = spwd->sp_pwdp;
#endif	/* M_UNIX */

	}
#endif  /* !NOSHADOW */

	/* Run encryption algorithm */
	encpw = crypt (passwd, encrypted_pass);

	memset ((char *) passwd, '\0', sizeof (passwd));

	/* Check it */
	if (strcmp (encpw, encrypted_pass))
	{
		return EV_NAK;
	}
#endif	/* ULTRIX_ENHANCED */

	/* Don't allow authentication with id "root" */
	if (pwd->pw_uid == 0)
	{
		logit (LOG_AUTH, LOG_WARNING,
			"%s: Attempt to authenticate using 'root'", func);
		return EV_NAK;	/* Don't do it */
	}

#ifdef	CHK_SHELLS
	/* Also make sure id uses a standard shell */
	if ((fp = fopen ("/etc/shells", "r")) == (FILE *) NULL)
	{
		has_etc_shells = 0;
	}
	else
	{
		has_etc_shells = 1;
	}
	while (has_etc_shells &&
		(fgets (buffer, sizeof (buffer), fp) != (char *) NULL))
	{
		if (strncmp (buffer, pwd->pw_shell, strlen (pwd->pw_shell)) == 0)
		{
			fclose (fp);
			return EV_ACK;
		}
	}
	if (has_etc_shells)
	{
		fclose (fp);
	}
#if !(defined _AIX || defined ultrix || defined SCO || defined __sgi)
	else
	{
		setusershell ();
		while ((pshell = getusershell ()) != (char *) NULL)
		{
			if (strncmp (pshell, pwd->pw_shell,
				     strlen (pwd->pw_shell)) == 0)
			{
				endusershell ();
				return EV_ACK;
			}
		}
		endusershell ();
	}
#endif	/* !(defined _AIX || defined ultrix || defined SCO) */
	logit (LOG_AUTH, LOG_WARNING,
		"%s: Attempt to authenticate with funny id '%s' - shell = %s",
		 func, name, pwd->pw_shell);
	return EV_NAK;
#else	/* CHK_SHELLS */
	return EV_ACK;
#endif	/* CHK_SHELLS */
} /* end of unix_pass () */
