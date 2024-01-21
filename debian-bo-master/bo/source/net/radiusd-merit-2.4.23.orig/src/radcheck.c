/*
 *
 *	RADIUS -- Remote Authentication Dial In User Service
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

static char     rcsid[] = "$Id: radcheck.c,v 1.45 1996/05/22 20:12:39 web Exp $";

#include	<sys/types.h>
#include	<sys/socket.h>
#include	<sys/param.h>
#include	<netinet/in.h>
#include	<sys/time.h>

#ifdef	SVR4
#include	<sys/systeminfo.h>
#endif	/* SVR4 */

#include	<netdb.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<time.h>

#include	"radius.h"

#ifndef RESPONSE_TIMEOUT
#define RESPONSE_TIMEOUT 3
#endif

#ifndef RETRY_MAX
#define RETRY_MAX	10
#endif

char            recv_buffer[4096];
char            send_buffer[4096];
char            ourhostname[MAXHOSTNAMELEN];
char           *progname;
char           *radius_dir;
int             debug_flag = 0;
int             dumpcore = 0;
int             file_logging = 2;   /* 0 => syslog, 1 => logfile, 2 => stderr */
int             zap_logfile = 0;
int             authfile_cnt = 0;
int             clients_cnt = 0;
int             users_cnt = 0;
time_t          birthdate;
AATVPTR		rad_authen_aatv = (AATV *) NULL;
AATVPTR         rad_ipc_aatv = (AATV *) NULL;
AATV           *authtype_tv[PW_AUTH_MAX + 1];
FILE           *ddt = NULL;
FILE           *msgfd = stderr;
extern void     dir_init ();

static void     radcheck_usage ();
static int      parse_hg ();
static char     hgserver[MAXHOSTNAMELEN];

int
main (argc, argv)

int             argc;
char           *argv[];

{
#ifdef	MERIT_TIMELEFT
	VALUE_PAIR     *vp;
	HG_ENTRY       *hg;
	int             timeleft = 0;
	int             num;
	char            p;
	char            m;
#endif	/* MERIT_TIMELEFT */
	int             result;
	int             max_retries;
	int             retries;
	int             new_old;
	int             zero = 0;
	UINT4           ip_addr;
	char            buf[256];
	char            msg[4096]; /* big enough to hold several messages */
	SEND_DATA       data;
	int             send_server ();

	progname = *argv;

	data.svc_port = 0;
	max_retries = RETRY_MAX;
	data.timeout = RESPONSE_TIMEOUT;
	new_old = 0; /* assume old style */
	radius_dir = "";
	data.user_file = (char *) NULL;

	while (--argc > 0 && *(*++argv) == '-')
	{
		/* switch on char. after "-" */
		switch (*(*argv + 1))
		{
		    case 'd':
			if (--argc == 0)
			{
				radcheck_usage ();
			}
			argv++;
			radius_dir = *argv;
			break;

		    case 'p':
			if (--argc == 0)
			{
				radcheck_usage ();
			}
			argv++;
			sscanf (*argv, "%u", &data.svc_port);
			break;

		    case 't':	/* Timeout value */
			if (--argc == 0)
			{
				radcheck_usage ();
			}
			argv++;
			sscanf (*argv, "%u", &data.timeout);
			break;

		    case 'r':	/* max. Retransmit count */
			if (--argc == 0)
			{
				radcheck_usage ();
			}
			argv++;
			sscanf (*argv, "%u", &max_retries);
			break;

		    case 'n':
			new_old = 1;
			break;

		    case 'x':
			debug_flag++;
			ddt = stderr;
			break;

		    case 'v':
			fprintf (stderr, "Version %s\n", RADIUS_VERSION);
			exit (0);
			break;
#ifdef	MERIT_TIMELEFT
		    case 'Z':
			timeleft++;
			break;
#endif	/* MERIT_TIMELEFT */

		    case '0':
			zero = 1;
			break;

		    default:
			radcheck_usage ();
		}
	}

	if (zero == 0)
	{
		printf ("Merit RADIUS 2.4.23C, licensed software\n");
		printf ("Copyright (c) 1992, 1993, 1994, 1995, 1996 by The\n");
		printf ("Regents of the University of Michigan and Merit Network, Inc.\n");
	}

#ifndef	MERIT_TIMELEFT
	if (argc == 1)
	{
		data.server = *argv;
	}
	else
	{
		radcheck_usage ();
	}
#else	/* MERIT_TIMELEFT */
	if (timeleft == 0) /* If no -Z option, at as normal radcheck client */
	{
		if (argc == 1)
		{
			data.server = *argv;
		}
		else
		{
			radcheck_usage ();
		}
	}
#endif	/* MERIT_TIMELEFT */

	dir_init ();

	if (dict_init () != 0)
	{
		exit (-1);
	}

	/*
	 *	Specify maximum number of times to issue request.
	 *	Number of retransmissions required shows up in retries.
	 */
	retries = max_retries;

	data.seq_nbr = 0;
	data.user_name = "TEST";
	data.password = "PW";
	data.fptype = 0;

#ifdef	SVR4
	if (sysinfo (SI_HOSTNAME, ourhostname, sizeof (ourhostname)) < 0)
	{
		perror ("SI_HOSTNAME");
		exit (-1);
	}
#else	/* Assume BSD */
	if (gethostname (ourhostname, sizeof (ourhostname)) < 0)
	{
		perror ("gethostname");
		exit (-1);
	}
#endif	/* SVR4 */

	if ((data.client_id = get_ipaddr (ourhostname)) == 0)
	{
		printf ("%s: Couldn't get our own IP address!\n", progname);
	}
	data.port_num = 1;

	data.ustype = PW_ADMINISTRATIVE_USER; /* was old PW_MANAGEMENT_POLL */
	if (new_old == 1) /* new style */
	{
		data.code = PW_STATUS_SERVER;
	}
	else /* old style */
	{
		data.code = PW_ACCESS_REQUEST;
	}

	data.send_pairs = (VALUE_PAIR *) NULL;

#ifdef	MERIT_TIMELEFT
	if (timeleft != 0)
	{
		vp = (VALUE_PAIR *) NULL;
		avpair_add (&vp, PW_PROXY_ACTION, "TIMELEFT", 0);
		strcpy (buf, getenv ("REMOTEHOST"));
		strtok (buf, "-");
		sscanf (buf, "%c%c%d", &p, &m, &num);
		sprintf (buf, "%c%c%d", p, m, num);
		ip_addr = get_ipaddr (buf);
		strcpy (buf, ip_hostname (ip_addr)); /* full NAS name */
		avpair_add (&vp, PW_NAS_IDENTIFIER, buf , 0);
		ip_addr = get_ipaddr (getenv ("REMOTEIP"));
		avpair_add (&vp, PW_FRAMED_IP_ADDRESS, &ip_addr, 0);
		data.send_pairs = vp;
		hgserver[0] = '\0';
		result = parse_hg (buf); /* gets HG server name in hgserver */
		if (result == 0)
		{
			printf ("Unable to determine time remaining.\n");
			sleep (30);
			exit (result);
		}
		data.server = hgserver;
	}
#endif	/* MERIT_TIMELEFT */

	msg[0] = '\0';
	result = send_server (&data, &retries, msg);

#ifdef	MERIT_TIMELEFT
	if (debug_flag != 0 && timeleft != 0)
	{
		printf ("host was '%s'\n", getenv ("REMOTEHOST"));
		printf ("IP address was '%s'\n", getenv ("REMOTEIP"));
		printf ("huntgroup server was '%s'\n", data.server);
	}

	if (timeleft != 0)
	{
		if (result == OK_RC)
		{
			if (msg[0] != '\0')
			{
				printf ("%s\n", msg);
				sleep (30);
			}
		}
		exit (result);
	}
#endif	/* MERIT_TIMELEFT */

	if (result == OK_RC)
	{
		if (msg[0] != '\0')
		{
			printf ("%s\n", msg);
		}
		printf ("\"%s(%u)\" is responding", data.server, data.svc_port);
		if (retries)
		{
			printf (" (%u retries)", retries);
		}
		putchar ('\n');
	}
	else
	{
		if (msg[0] == '\0')
		{
			sprintf (msg, "RC = %i - Strange response!", result);
		}
		printf ("%s(%u): %s\n", data.server, data.svc_port, msg);
	}
	exit (result);
} /* end of main () */

#ifdef	MERIT_TIMELEFT

/*************************************************************************
 *
 *	Function: parse_hg
 *
 *	Purpose: Locate the huntgroup server for this NAS
 *
 *	Returns: 1, if the matching HG server was placed in static hgserver,
 *		 or 0, if any errors.
 *
 *************************************************************************/

static int
parse_hg (given_nas)

char           *given_nas;

{
	int             result = 0;
	int             found = 0;
	char           *nas;
	char           *ptr;
	char           *s;
	FILE           *hgfd;
	char            buffer[256];
	char            fname[MAXPATHLEN];

	/*
	 * Open the huntgroups file
	 */
	sprintf (fname, "%s/%s", radius_dir, RADIUS_HUNTGROUPS);

	if ((hgfd = fopen (fname, "r")) == (FILE *) NULL)
	{
		return result;
	}

	while (fgets (buffer, sizeof (buffer), hgfd) != (char *) NULL)
	{
		if (*buffer == COMMENT || isspace (*buffer))
		{
			continue;
		}

		if ((ptr = strtok (buffer, " \t\n")) == NULL)
		{
			continue;
		}

		if (strcasecmp (ptr, "nas") == 0) 
		{
			if (found == 1) /* keep looking for server */
			{
				continue;
			}

			if ((nas = strtok (NULL, " \t")) == (char *) NULL ||
				strlen (nas) >= AUTH_ID_LEN)
			{
				continue;
			}
 			else /* see if this NAS == given NAS */
			{
				if (strcasecmp (nas, given_nas) == 0)
				{
					found = 1;
				}
			}
			continue;
		}
		else /* not nas, check for server */
		{
			if ((strcasecmp (ptr, "server") == 0) && (found == 1))
			{
				if (((s = strtok (NULL, " ,)\t\n")) != NULL) &&
					strlen (s) < MAXHOSTNAMELEN)
				{
					if (strcasecmp (s, "localserver") == 0)
					{
						s = ourhostname;
					}
					strncpy (hgserver, s, strlen (s));
					result = 1;
					break;
				}
			}
			found = 0; /* only grabs first server after NAS match */
			continue;
		}
		continue;

	} /* end of while () */

	fclose (hgfd);

	return result;
		
} /* end of parse_hg () */

#endif	/* MERIT_TIMELEFT */

static void
radcheck_usage ()

{
	printf ("Usage: %s [-d dir] [-p port] [-t timeout] [-r retries]\n",
		progname);
	printf ("\t[-x] [-n] [-v] servername\n");
	exit (-1);
} /* end of radcheck_usage () */
