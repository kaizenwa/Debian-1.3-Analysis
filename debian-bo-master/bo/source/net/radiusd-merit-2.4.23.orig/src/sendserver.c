/*
 *
 *	RADIUS   Remote Authentication Dial In User Service
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
 * check_radius_reply (only public for the MINOS/MNET daemon)
 * dir_init
 * send_server
 * random_vector
 *
 */

static char     rcsid[] = "$Id: sendserver.c,v 1.54 1996/05/22 19:58:19 web Exp $";

#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<arpa/inet.h>
#include	<sys/param.h>
#include	<sys/time.h>

#if !(defined(FD_SET) || defined(linux))
#include	<sys/select.h>
#endif	/* FD_SET */

#include	<errno.h>
#include	<netdb.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<syslog.h>
#include	<time.h>
#include	<unistd.h>

#include	"radius.h"

#if !defined(__FreeBSD__) && !defined(_BSDI_VERSION) && !defined(__NetBSD__)
extern char    *sys_errlist[];
#endif	/* __FreeBSD__ */

#ifndef DEFAULT_SERVER
#define DEFAULT_SERVER	"radius1.merit.edu"
#endif

#ifndef DEFAULT_DIR
#define DEFAULT_DIR	"../raddb"
#endif

#ifndef DEFAULT_DIR2
#define DEFAULT_DIR2	"/usr/private/etc/raddb"
#endif

extern char     ourhostname[MAXHOSTNAMELEN];
extern char    *progname;
extern int      debug_flag;
extern char    *radius_dir;

int             radsock = 0;	/* fd for radius socket, if non-blocking mode */
static int      find_server PROTO ((char *, int, UINT4 *, char *, char *));
void            random_vector PROTO ((u_char *));
int             check_radius_reply PROTO ((u_char *, char *, u_char *, u_int, char *));

/*************************************************************************
*
*	dir_init - if not set, initializes global var for RADIUS clients
*
**************************************************************************/

void
dir_init ()

{
	if (radius_dir == NULL || radius_dir[0] == '\0')
	{
		radius_dir = DEFAULT_DIR;
		if (access (radius_dir, X_OK) != 0)
		{
			radius_dir = DEFAULT_DIR2;
		}
	}
	return;
} /* end of dir_init () */

/*************************************************************************
 *
 *	Function: pack_list
 *
 *	Purpose: Packs an attribute value pair list into a buffer.
 *
 *	Returns: Number of octets packed.
 *
 *************************************************************************/

static int
pack_list (vp, buf)

VALUE_PAIR     *vp;
char           *buf;

{
	int             length;
	int             total_length = 0;
	UINT4           lvalue;

	while (vp != (VALUE_PAIR *) NULL)
	{
		debug_pair (stderr, vp);
		*buf++ = vp->attribute;

		switch (vp->type)
		{
		    case PW_TYPE_STRING:
			/* length = length = (vp->lvalue > 0) ?
					vp->lvalue : strlen (vp->strvalue); */
			length = strlen (vp->strvalue);
			*buf++ = length + 2;
			memcpy (buf, vp->strvalue, length);
			buf += length;
			total_length += length + 2;
			break;

		    case PW_TYPE_INTEGER:
		    case PW_TYPE_IPADDR:
			*buf++ = sizeof (UINT4) + 2;
			lvalue = htonl (vp->lvalue);
			memcpy (buf, (char *) &lvalue, sizeof (UINT4));
			buf += sizeof (UINT4);
			total_length += sizeof (UINT4) + 2;
			break;

		    default:
			break;
		}

		vp = vp->next;
	}
	return total_length;
} /* end of pack_list () */

/*************************************************************************
*
*	send_server - Sends request to specified RADIUS server and waits
*		      for response.  Request is retransmitted every
*		      "response_timeout" seconds a maximum of "retry_max"
*		      times.  Result is 0 if response was received, -1 if
*		      a problem occurred, or +1 on no-response condition.
*		      Returns request retransmit count in "retries" if
*		      server does respond.
*
*	Returns:	-1 ERROR_RC   -- on local error,
*			 0 OK_RC      -- on valid response from server,
*			 1 TIMEOUT_RC -- after retries * resp_timeout seconds,
*			-2 BADRESP_RC -- if response from server had errors.
*
**************************************************************************/

int 
send_server (data, retries, msg)

SEND_DATA      *data;		/* Data structure built by clients */
int            *retries;	/* Maximum num of times to retransmit request */
				/* Receives number of retries required, also */
char           *msg;		/* Receives error or advisory message */

{
	u_char          seq_nbr;    /* Sequence number to use in request  */
	int             fptype;     /* Framed proto, ustype == PW_FRAMED */
	int             i;
	int             length;
	int             result;
	int             retry_max;
	int             salen;
	int             secretlen;
	int             sockfd;
	int             timeout;    /* Number of secs. to wait for response */
	int             total_length;
	int             ustype;     /* User service type for this user */
	UINT4           auth_ipaddr;
	UINT4           lvalue;
	UINT4           myipaddr;
	UINT4           port_num;   /* Port number to use in request  */
	AUTH_HDR       *auth;
	VALUE_PAIR     *check;
	char           *passwd;		/* User password (unencrypted) */
	u_char         *ptr;
	VALUE_PAIR     *reply;
	char           *server_name;	/* Name of server to query */
	struct sockaddr_in *sin;
	struct servent *svp;
	struct timeval  authtime;
	fd_set          readfds;
	struct sockaddr salocal;
	struct sockaddr saremote;
	u_char          md5buf[256];
	u_char          passbuf[AUTH_PASS_LEN];
	u_char          send_buffer[1024];
	u_char          recv_buffer[1024];
	u_char          vector[AUTH_VECTOR_LEN];
	char            file[MAXPATHLEN];
	char            secret[MAX_SECRET_LENGTH + 1];

#ifdef KCHAP
	char            kchap_auth;
	u_char          buffer[AUTH_PASS_LEN + AUTH_VECTOR_LEN + 1];
	u_char          digest[AUTH_VECTOR_LEN];
	u_char          user_secret[CHAP_VALUE_LENGTH + 1];

#endif	/* KCHAP */

	/* Set up some defaults */
	dir_init ();

	server_name = data->server;
	if (server_name == (char *) NULL || server_name[0] == '\0')
	{
		server_name = DEFAULT_SERVER;
	}

	ustype = data->ustype;

#ifdef	KCHAP
	if (ustype == 255) /* KCHAP indicator */
	{
		kchap_auth = 1;
		ustype = PW_AUTHENTICATE_ONLY;
	}
	else
	{
		kchap_auth = 0;
	}
#endif	/* KCHAP */

	if (find_server (server_name, ustype, &auth_ipaddr, secret, msg) != 0)
	{
		return (ERROR_RC);
	}

	timeout = data->timeout;
	if (timeout == 0)
	{
		timeout++;
	}

	if (data->svc_port == 0)
	{
		if ((svp = getservbyname ("radius", "udp")) == NULL)
		{
			data->svc_port = PW_AUTH_UDP_PORT;
		}
		else
		{
			data->svc_port = ntohs (svp->s_port);
		}
	}

	if (!radsock)
	{
		sockfd = socket (AF_INET, SOCK_DGRAM, 0);
		if (sockfd < 0)
		{
			memset (secret, '\0', sizeof (secret));
			sprintf (msg, "socket: %s\n", sys_errlist[errno]);
			return (ERROR_RC);
		}

		length = sizeof (salocal);
		sin = (struct sockaddr_in *) & salocal;
		memset ((char *) sin, '\0', length);
		sin->sin_family = AF_INET;
		sin->sin_addr.s_addr = INADDR_ANY;
		sin->sin_port = htons (0);
		if (bind (sockfd, (struct sockaddr *) sin, length) < 0 ||
			   getsockname (sockfd, (struct sockaddr *) sin,
					&length) < 0)
		{
			close (sockfd);
			memset (secret, '\0', sizeof (secret));
			sprintf (msg, "bind: %s\n", sys_errlist[errno]);
			return (ERROR_RC);
		}
		retry_max = *retries;	/* Max. numbers to try for reply */
		*retries = 0;	/* Init retry cnt for blocking call */
	}
	else
	{
		sockfd = radsock;
		retry_max = 0;	/* No retries if non-blocking */
	}

	/* Build an authentication request */
	auth = (AUTH_HDR *) send_buffer;
	auth->code = data->code;
	random_vector (vector);
	seq_nbr = data->seq_nbr;
	auth->id = seq_nbr;
	memcpy ((char *) auth->vector, (char *) vector, AUTH_VECTOR_LEN);
	total_length = AUTH_HDR_LEN;
	ptr = auth->data;

	/* User Name */
	*ptr++ = PW_USER_NAME;
	length = strlen (data->user_name);
	if (length > AUTH_ID_LEN)
	{
		length = AUTH_ID_LEN;
	}
	*ptr++ = length + 2;
	memcpy ((char *) ptr, data->user_name, length);
	ptr += length;
	total_length += length + 2;

	passwd = data->password;

	if (auth->code != PW_ACCOUNTING_REQUEST)
	{
#ifdef  KCHAP
		if (kchap_auth)
		{
			/* User Password */
			*ptr++ = PW_CHAP_PASSWORD;
			*ptr++ = 1 + AUTH_VECTOR_LEN + 2;
			*ptr++ = seq_nbr;  /* Pass CHAP identifier to RADIUS */
			*buffer = seq_nbr;/* Put CHAP id in work area for md5 */
			afs_pwd_to_secret (passwd, user_secret);/* Get secret */
			user_secret[CHAP_VALUE_LENGTH] = '\0';
			memcpy ((char *) buffer + 1, (char *) user_secret,
				CHAP_VALUE_LENGTH);
			memcpy ((char *) buffer + 1 + CHAP_VALUE_LENGTH,
				(char *) vector, AUTH_VECTOR_LEN);
			md5_calc (digest, buffer,
			  	1 + CHAP_VALUE_LENGTH + AUTH_VECTOR_LEN);
			memcpy ((char *) ptr, (char *) digest, AUTH_VECTOR_LEN);
			ptr += AUTH_VECTOR_LEN;
			total_length += 1 + AUTH_VECTOR_LEN + 2;
		}
		else
		{
#endif	/* KCHAP */

			/* User Password */
			*ptr++ = PW_USER_PASSWORD;
			*ptr++ = AUTH_PASS_LEN + 2;

			/* Encrypt the Password */
			length = strlen (passwd);
			if (length > AUTH_PASS_LEN)
			{
				length = AUTH_PASS_LEN;
			}
			memset ((char *) passbuf, '\0', AUTH_PASS_LEN);
			memcpy ((char *) passbuf, passwd, length);

			/* Calculate the MD5 Digest */
			secretlen = strlen (secret);
			strcpy ((char *) md5buf, secret);
			memcpy ((char *) md5buf + secretlen,
				(char *) auth->vector, AUTH_VECTOR_LEN);
			md5_calc (ptr, md5buf, secretlen + AUTH_VECTOR_LEN);

			/* Xor the password into the MD5 digest */
			for (i = 0; i < AUTH_PASS_LEN; i++)
			{
				*ptr++ ^= passbuf[i];
			}
			total_length += AUTH_PASS_LEN + 2;

#ifdef	KCHAP
		}
#endif	/* KCHAP */
	}

	/* Service Type */
	*ptr++ = PW_SERVICE_TYPE;
	*ptr++ = 2 + sizeof (UINT4);
	lvalue = htonl (ustype);
	memcpy ((char *) ptr, (char *) &lvalue, sizeof (UINT4));
	ptr = ptr + sizeof (UINT4);
	total_length += sizeof (UINT4) + 2;

	fptype = data->fptype;
	if (fptype > 0)			/* if -t [slip | ppp] */
	{
		/* Framed Protocol Type */
		*ptr++ = PW_FRAMED_PROTOCOL;
		*ptr++ = 2 + sizeof (UINT4);
		lvalue = htonl (fptype);
		memcpy ((char *) ptr, (char *) &lvalue, sizeof (UINT4));
		ptr = ptr + sizeof (UINT4);
		total_length += sizeof (UINT4) + 2;
	}

	/* Client IP Address */
	*ptr++ = PW_NAS_IP_ADDRESS;
	*ptr++ = 2 + sizeof (UINT4);
	myipaddr = htonl(data->client_id);
	memcpy ((char *) ptr, (char *) &myipaddr, sizeof (UINT4));
	ptr = ptr + sizeof (UINT4);
	total_length += sizeof (UINT4) + 2;

	/* Client Port Number */
	*ptr++ = PW_NAS_PORT;
	*ptr++ = 2 + sizeof (UINT4);
	port_num = htonl((UINT4) data->port_num);
	memcpy ((char *) ptr, (char *) &port_num, sizeof (UINT4));
	ptr = ptr + sizeof (UINT4);
	total_length += sizeof (UINT4) + 2;

	if (data->user_file != (char *) NULL) /* add a/v pairs from user_file */
	{
		sprintf (file, "%s.", data->user_file);
		check = (VALUE_PAIR *) NULL;
		if ((user_find (file, data->group, 0, &check, &reply, 1)) == 0)
		{
			total_length += (length = pack_list (check, ptr));
			ptr += length;
			total_length += (length = pack_list (reply, ptr));
			ptr += length;
		}
	}

	if (data->send_pairs != (VALUE_PAIR *) NULL) /* add more a/v pairs */
	{
		total_length += (length = pack_list (data->send_pairs, ptr));
		ptr += length;
	}

	auth->length = htons (total_length);

	sin = (struct sockaddr_in *) & saremote;
	memset ((char *) sin, '\0', sizeof (saremote));
	sin->sin_family = AF_INET;
	sin->sin_addr.s_addr = htonl (auth_ipaddr);
	sin->sin_port = htons (data->svc_port);

	for (;;)
	{
		sendto (sockfd, (char *) auth, (int) total_length, (int) 0,
			(struct sockaddr *) sin, sizeof (struct sockaddr_in));

		if (radsock)
		{		/* If non-blocking */

			/*
			 * Return stuff to be saved for evaluation of reply
			 * when it comes in
			 */
			strcpy (msg, secret);
			memcpy (msg + strlen (msg) + 1, (char *) vector,
				AUTH_VECTOR_LEN);
			memset (secret, '\0', sizeof (secret));
			return 1;	/* Pos. return means no error */
		}
		authtime.tv_usec = 0L;
		authtime.tv_sec = (long) timeout;
		FD_ZERO (&readfds);
		FD_SET (sockfd, &readfds);
		if (select (sockfd + 1, &readfds, NULL, NULL, &authtime) < 0)
		{
			if (errno == EINTR)
				continue;
			sprintf (msg, "select: %s\n", sys_errlist[errno]);
			memset (secret, '\0', sizeof (secret));
			close (sockfd);
			return (ERROR_RC);
		}
		if (FD_ISSET (sockfd, &readfds))
			break;

		/*
		 * Timed out waiting for response.  Retry "retry_max" times
		 * before giving up.  If retry_max = 0, don't retry at all.
		 */
		if (++(*retries) >= retry_max)
		{
			if (debug_flag > 0)
			{
				fprintf (stderr, "\n");
			}
			sprintf (msg,
				"No reply from RADIUS server \"%s(%u)\"\n",
				 ip_hostname (auth_ipaddr), data->svc_port);
			close (sockfd);
			memset (secret, '\0', sizeof (secret));
			return (TIMEOUT_RC);
		}
		else
		{
			if (debug_flag > 0)
			{
				fprintf (stderr, ".");
			}
		}
	}
	salen = sizeof (saremote);
	length = recvfrom (sockfd, (char *) recv_buffer,
			   (int) sizeof (recv_buffer),
			   (int) 0, &saremote, &salen);

	if (length <= 0)
	{
		sprintf (msg, "recvfrom: %s\n", sys_errlist[errno]);
		close (sockfd);
		memset (secret, '\0', sizeof (secret));
		return (ERROR_RC);
	}
	result = check_radius_reply (recv_buffer, secret, vector,
		(u_int) seq_nbr, msg);
	close (sockfd);
	memset (secret, '\0', sizeof (secret));
	return (result);
} /* end of send_server () */

/*************************************************************************
*
*	check_radius_reply - Verify items in returned packet.
*
*	Returns:	OK_RC       -- upon success,
*			BADRESP_RC  -- if anything looks funny.
*
*	Public entry point necessary for MINOS/MNET daemon.
*
**************************************************************************/

int 
check_radius_reply (buffer, secret, vector, seq_nbr, msg)

u_char         *buffer;
char           *secret;
u_char          vector[];
u_int           seq_nbr;
char           *msg;

{
	u_char          len;
	int             result;
	int             secretlen;
	int             totallen;
	AUTH_HDR       *auth;
	u_char         *next;
	u_char         *ptr;
	VALUE_PAIR     *vp;
	u_char          calc_digest[AUTH_VECTOR_LEN];
	u_char          reply_digest[AUTH_VECTOR_LEN];

	auth = (AUTH_HDR *) buffer;
	totallen = ntohs (auth->length);

	/* Verify that id (seq. number) matches what we sent */
	if (auth->id != (u_char) seq_nbr)
	{
		sprintf (msg, "Received non-matching id in server response\n");
		return (BADRESP_RC);
	}

	/* Verify the reply digest */
	memcpy ((char *) reply_digest, (char *) auth->vector, AUTH_VECTOR_LEN);
	memcpy ((char *) auth->vector, (char *) vector, AUTH_VECTOR_LEN);
	secretlen = strlen (secret);
	memcpy ((char *) buffer + totallen, secret, secretlen);
	md5_calc (calc_digest, (char *) auth, totallen + secretlen);

	if (memcmp ((char *) reply_digest, (char *) calc_digest,
		    AUTH_VECTOR_LEN) != 0)
	{
		sprintf (msg, "Received invalid reply digest from server\n");
		return (BADRESP_RC);
	}

	if (debug_flag)
	{
		fprintf (stderr, "Received attribute/value pair(s):\n");
		vp = gen_valpairs (auth); /* just to print out in debug mode */
	}

	msg[0] = '\0';
	ptr = (u_char *) auth->data;
	totallen -= AUTH_HDR_LEN;
	while (totallen > 0)
	{
		len = ptr[1];
		totallen -= len;
		next = ptr + len;
		if (*ptr == '\0')
		{
			sprintf (msg, "Received bad attribute type from server\n");
			return (BADRESP_RC);
		}

		if (*ptr == PW_REPLY_MESSAGE)
		{
			ptr++;
			ptr++;
			strncat (msg, (char *) ptr, len - 2);
			strcat (msg, "\n");
		}
		ptr = next;
	}

	if ((auth->code == PW_ACCESS_ACCEPT) ||
		(auth->code == PW_PASSWORD_ACK) ||
		(auth->code == PW_ACCOUNTING_RESPONSE))
	{
		result = OK_RC;
	}
	else
	{
		result = BADRESP_RC;
	}

	return (result);
} /* end of check_radius_reply () */

/*************************************************************************
*
*	random_vector - Generates a random vector of AUTH_VECTOR_LEN octets.
*
*	Returns:	the vector (call by reference)
*
**************************************************************************/

void
random_vector (vector)

u_char         *vector;

{
	int             randno;
	int             i;

	srand (time (0));
	for (i = 0; i < AUTH_VECTOR_LEN;)
	{
		randno = rand ();
		memcpy ((char *) vector, (char *) &randno, sizeof (int));
		vector += sizeof (int);
		i += sizeof (int);
	}
	return;
} /* end of random_vector () */

/*************************************************************************
*
*	find_match - See if given IP address matches any address of hostname.
*
*	Returns:	 0 success
*			-1 failure
*
**************************************************************************/

static int 
find_match (ip_addr, hostname)

UINT4          *ip_addr;
char           *hostname;

{
	UINT4           addr;
	char          **paddr;
	struct hostent *hp;

	if (good_ipaddr (hostname) == 0)
	{
		if (*ip_addr == ntohl(inet_addr (hostname)))
		{
			return (0);
		}
	}
	else
	{
		if ((hp = gethostbyname (hostname)) == (struct hostent *) NULL)
		{
			return (-1);
		}
		if (hp->h_addr_list != (char **) NULL)
		{
			for (paddr = hp->h_addr_list; *paddr; paddr++)
			{
				addr = ** (UINT4 **) paddr;
				if (ntohl(addr) == *ip_addr)
				{
					return (0);
				}
			}
		}
	}
	return (-1);
} /* end of find_match */

/*************************************************************************
*
*	find_server - Look up the given server name in the clients file.
*
*	Returns:	 0 success
*			-1 failure
*
**************************************************************************/

static int 
find_server (server_name, ustype, ip_addr, secret, msg)

char           *server_name;
int             ustype;
UINT4          *ip_addr;
char           *secret;
char           *msg;

{
	static UINT4    myipaddr = 0;
	int             len;
	int             line_nbr = 0;
	int             result;
	FILE           *clientfd;
	char           *h;
	char           *s;
	char           *host2;
	char            buffer[128];
	char            fname[MAXPATHLEN];
	char            hostnm[AUTH_ID_LEN + 1];

	/* Get the IP address of the authentication server */
	if ((*ip_addr = get_ipaddr (server_name)) == (UINT4) 0)
	{
		sprintf (msg, "No such server: \"%s\"\n", server_name);
		return (-1);
	}
	/* Just use dummy secret for management polls */
	if (ustype == PW_ADMINISTRATIVE_USER) /* was old PW_MANAGEMENT_POLL */
	{
		strcpy (secret, MGMT_POLL_SECRET);
		return 0;
	}
	sprintf (fname, "%s/%s", radius_dir, RADIUS_CLIENTS);
	if ((clientfd = fopen (fname, "r")) == (FILE *) NULL)
	{
		sprintf (msg, "Couldn't open file \"%s\"\n", fname);
		return (-1);
	}
	if (!myipaddr)
	{
		if ((myipaddr = get_ipaddr (ourhostname)) == 0)
		{
			sprintf (msg, "Couldn't get our own ip address\n");
			fclose (clientfd);
			return (-1);
		}
	}

	result = 0;
	while (fgets (buffer, sizeof (buffer), clientfd) != (char *) NULL)
	{
		line_nbr++;

		if (*buffer == '#')
		{
			continue;
		}

		if ((h = strtok (buffer, " \t\n\r")) == NULL) /* 1st hostname */
		{
			continue;
		}

		memset (hostnm, '\0', AUTH_ID_LEN);
		len = strlen (h);
		if (len > AUTH_ID_LEN)
		{
			len = AUTH_ID_LEN;
		}
		strncpy (hostnm, h, len);
		hostnm[AUTH_ID_LEN] = '\0';

		if ((s = strtok (NULL, " \t\n\r")) == NULL) /* & secret field */
		{
			continue;
		}

		memset (secret, '\0', MAX_SECRET_LENGTH);
		len = strlen (s);
		if (len > MAX_SECRET_LENGTH)
		{
			len = MAX_SECRET_LENGTH;
		}
		strncpy (secret, s, len);
		secret[MAX_SECRET_LENGTH] = '\0';

		if (!strchr (hostnm, '/')) /* If single name form */
		{
			if (find_match (ip_addr, hostnm) == 0)
			{
				result++;
				break;
			}
		}
		else /* <name1>/<name2> "paired" form */
		{
			strtok (hostnm, "/"); /* replaces "/" with NULL char */
			host2 = strtok (NULL, " ");
			if (find_match (&myipaddr, hostnm) == 0)
			{	     /* If we're the 1st name, target is 2nd */
				if (find_match (ip_addr, host2) == 0)
				{
					result++;
					break;
				}
			}
			else	/* Check to see if we are the second name */
			{
				if (find_match (&myipaddr, host2) == 0)
				{ /* We are the 2nd name, target is 1st name */
					if (find_match (ip_addr, hostnm) == 0)
					{
						result++;
						break;
					}
				}
			}
		}
	}
	fclose (clientfd);
	if (result == 0)
	{
		memset (buffer, '\0', sizeof (buffer));
		memset (secret, '\0', sizeof (secret));
		sprintf (msg, "Couldn't find server in \"%s/%s\": \"%s\"\n",
			 radius_dir, RADIUS_CLIENTS, server_name);
		return (-1);
	}
	return 0;
} /* end of find_server () */
