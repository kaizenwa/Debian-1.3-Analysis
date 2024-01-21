/*
 *
 *	RADIUS -- Remote Authentication Dial In User Service
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

static char     rcsid[] = "$Id: radpwtst.c,v 1.38 1996/05/18 00:14:45 web Exp $";

#include	<sys/types.h>
#include	<sys/socket.h>
#include	<sys/param.h>
#include	<netinet/in.h>
#include	<sys/time.h>
#include	<sys/signal.h>

#ifdef	aix
#include	<sys/termio.h>
#else	/* aix */
#include	<sys/termios.h>
#endif	/* aix */

#ifdef	SVR4
#include	<sys/systeminfo.h>
#endif	/* SVR4 */

#include	<netdb.h>
#include	<pwd.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<time.h>
#include	<unistd.h>

#include	"radius.h"

#ifndef RESPONSE_TIMEOUT
#define RESPONSE_TIMEOUT 3
#endif

#ifndef MAX_RETRIES
#define MAX_RETRIES	10
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
extern void     dir_init();

static void     radpwtst_usage ();

typedef struct string_list_struct
{
	struct string_list_struct *next;
	char                      *str;
}string_list;

static char *
rad_getpass (prompt)

char           *prompt;

{
	int             ch;
	char           *p;
	FILE           *infp;
	FILE           *outfp;
	sigset_t        newset;
	sigset_t        oldset;

#ifdef	TCSAFLUSH
typedef	struct termios  TERM_STRUCT;
#define	GET_TTY(x,y)    tcgetattr(x,y)
#define	SET_TTY(x,y)    tcsetattr(x,TCSAFLUSH,y)
#else	/* TCSAFLUSH */
typedef	struct termio   TERM_STRUCT;
#define	GET_TTY(x,y)    ioctl(x,TCGETA,y)
#define	SET_TTY(x,y)    ioctl(x,TCSETAF,y)
#endif	/* TCSAFLUSH */

	TERM_STRUCT     term;
	TERM_STRUCT     termsave;
	static char     buf[AUTH_PASS_LEN + 1];

	infp = stdin;
	outfp = stderr;

	sigemptyset (&newset);  /* block SIGINT and SIGTSTP, save signal mask */
	sigaddset (&newset, SIGINT);
	sigaddset (&newset, SIGTSTP);
	sigprocmask (SIG_BLOCK, &newset, &oldset);

	GET_TTY(fileno(infp), &termsave);
	term = termsave;			/* copy the entire structure */
	term.c_lflag &= ~(ECHO | ECHOE | ECHOK | ECHONL); /* turn off echoing */
	SET_TTY(fileno(infp), &term);

	fputs (prompt, outfp);
	rewind (outfp);				/* implied flush */

	p = buf;
	while (((ch = getc (infp)) != EOF) && (ch != '\n'))
	{
		if (p < buf + AUTH_PASS_LEN)
		{
			*p++ = ch;
		}
	}
	*p = '\0';				/* null terminated password */

	write (fileno (outfp), "\n", 1);	/* echo one newline */

	SET_TTY(fileno(infp), &termsave);

	sigprocmask (SIG_SETMASK, &oldset, NULL);

	return (buf);
} /* end of rad_getpass () */

int
main (argc, argv)

int             argc;
char           *argv[];

{
	int             result;
	int             retries;
	int             new_old;
	int             zero = 0;
	char           *client_name = (char *) NULL;
	char           *clear_pw = (char *) NULL;
	string_list    *vplist = NULL;  /* For '-:' option(s) */
	string_list   **vpnext = &vplist; /* For '-:' option */
	char            msg[4096]; /* big enough to hold several messages */
	char            passwd[AUTH_PASS_LEN + 1];
	SEND_DATA       data;
	int             send_server ();

	progname = *argv;

	/* Set up some defaults */
	data.code = PW_ACCESS_REQUEST;
	data.svc_port = 0;      /* Default... figure it out later. */
	data.server = "";	/* SendServer picks server, if need be */
	radius_dir = "";	/* SendServer picks directory, if need be */
	data.timeout = RESPONSE_TIMEOUT;
	data.user_file = (char *) NULL;
	data.group = (char *) NULL;
	data.send_pairs = (VALUE_PAIR *) NULL;

	retries = MAX_RETRIES;	/* Try for response this many times */
	new_old = 0;		/* Assume old style */
	data.ustype = 0;
	data.fptype = 0;	/* by default */
	data.port_num = 1;	/* just default to port number one here */

	/* Check command args */
	while (--argc > 0 && *(*++argv) == '-')
	{
		/* switch on char. after "-" */
		switch (*(*argv + 1))
		{
		    case 'c':	/* "Packet Code" */
			if (--argc == 0)
			{
				fprintf (stderr, "%s: Missing packet code\n",
					progname);
				radpwtst_usage ();
			}
			argv++;
			data.code = atoi (*argv);
			break;

		    case 'd':	/* "Directory" */
			if (--argc == 0)
			{
				fprintf (stderr, "%s: Missing directory\n",
					progname);
				radpwtst_usage ();
			}
			argv++;
			radius_dir = *argv;	/* Use specified directory */
			break;

		    case 'f':	/* "Users" file */
			if (--argc == 0)
			{
				fprintf (stderr, "%s: Missing users file\n",
					progname);
				radpwtst_usage ();
			}
			argv++;
			data.user_file = *argv;
			break;

		    case 'g':	/* "Users" group */
			if (--argc == 0)
			{
				fprintf (stderr, "%s: Missing users group\n",
					progname);
				radpwtst_usage ();
			}
			argv++;
			data.group = *argv;
			break;

		    case 'h':	/* "Help" message */
			radpwtst_usage ();
			argv++;
			break;

		    case 'i':   /* Client-Id */
			if (--argc == 0)
			{
				fprintf (stderr, "%s: Missing client ID\n",
					progname);
				radpwtst_usage ();
			}
			argv++;
			client_name = *argv;
			break;

		    case 'l':	/* async-line */
			if (--argc == 0)
			{
				fprintf (stderr, "%s: Missing async-line\n",
					progname);
				radpwtst_usage ();
			}
			argv++;
			sscanf (*argv, "%u", &data.port_num);
			break;

		    case 'n':
			new_old = 1;
			break;

		    case 'p':	/* UDP-port */
			if (--argc == 0)
			{
				fprintf (stderr, "%s: Missing UDP port\n",
					progname);
				radpwtst_usage ();
			}
			argv++;
			sscanf (*argv, "%u", &data.svc_port);
			break;

		    case 'r':   /* Retries */
			if (--argc == 0)
			{
				fprintf (stderr, "%s: Missing retry limit\n",
					progname);
				radpwtst_usage ();
			}
			argv++;
			sscanf (*argv, "%u", &retries);
			break;

		    case 's':	/* "Server name" */
			if (--argc == 0)
			{
				fprintf (stderr, "%s: Missing server name\n",
					progname);
				radpwtst_usage ();
			}
			argv++;
			data.server = *argv;
			break;

		    case 't':   /* Timeout */
			if (--argc == 0)
			{
				fprintf (stderr, "%s: Missing timeout value\n",
					progname);
				radpwtst_usage ();
			}
			argv++;
			sscanf (*argv, "%u", &data.timeout);
			break;

		    case 'u':	/* "Service Type" */
			if (--argc == 0)
			{
				fprintf (stderr, "%s: Missing service type\n",
					progname);
				radpwtst_usage ();
			}
			argv++;
			if (strcasecmp (*argv, "auth") == 0)
			{
				if (new_old == 1) /* new style */
				{
					data.ustype = PW_AUTHENTICATE_ONLY;
				}
				else /* old style */
				{
					data.ustype = PW_OUTBOUND_USER;
				}
			}
			else if (strcasecmp (*argv, "kchap") == 0)
			{
				data.ustype = 255;	/* for testing kchap */
			}
			else if (strcasecmp (*argv, "dumb") == 0)
			{
				data.ustype = PW_LOGIN;
			}
			else if (strcasecmp (*argv, "slip") == 0)
			{
				data.ustype = PW_FRAMED;
				data.fptype = PW_SLIP;
			}
			else if (strcasecmp (*argv, "ppp") == 0)
			{
				data.ustype = PW_FRAMED;
				data.fptype = PW_PPP;
			}
			else if (strcasecmp (*argv, "dbdumb") == 0)
			{
				data.ustype = PW_CALLBACK_LOGIN;
			}
			else if (strcasecmp (*argv, "dbslip") == 0)
			{
				data.ustype = PW_CALLBACK_FRAMED;
				data.fptype = PW_SLIP;
			}
			else if (strcasecmp (*argv, "dbppp") == 0)
			{
				data.ustype = PW_CALLBACK_FRAMED;
				data.fptype = PW_PPP;
			}
			else if (strcasecmp (*argv, "outbound") == 0)
			{
				data.ustype = PW_OUTBOUND_USER;
			}
			else if (strcasecmp (*argv, "admin") == 0)
			{
				data.ustype = PW_ADMINISTRATIVE_USER;
			}
			else if (strcasecmp (*argv, "exec") == 0)
			{
				data.ustype = PW_SHELL_USER;
			}
			else if (strcasecmp (*argv, "dbadmin") == 0)
			{
				data.ustype = PW_CALLBACK_ADMIN_USER;
			}
			else
			{
				radpwtst_usage ();
			}
			break;

		    case 'w':	/* password */
			if (--argc == 0)
			{
				fprintf (stderr, "%s: Missing password\n",
					progname);
				radpwtst_usage ();
			}
			argv++;
			clear_pw = *argv;
			break;

		    case 'X':
			debug_flag++;
			ddt = stderr;
			msgfd = fdopen (dup (fileno (stderr)), "w");
			file_logging = 1;
			break;

		    case 'x':
			debug_flag++;
			ddt = stderr;
			break;

		    case 'v':
			fprintf (stderr, "Version %s\n", RADIUS_VERSION);
			break;

		    case ':':	/* Send arbirary a/v pairs this way... */
			*vpnext = (string_list *) malloc (sizeof (string_list));
			(*vpnext)->str = *argv + 2;
			(*vpnext)->next = (string_list *) NULL;
			vpnext = &((*vpnext)->next);
			break;

		    case '0':
			zero = 1;
			break;

		    default:
			fprintf (stderr, "%s: Invalid option, '%s'\n",
				progname, **argv);
			radpwtst_usage ();
		}
	}

	if (zero == 0)
	{
		printf ("Merit RADIUS 2.4.23C, licensed software\n");
		printf ("Copyright (c) 1992, 1993, 1994, 1995, 1996 by The\n");
		printf ("Regents of the University of Michigan and Merit Network, Inc.\n");
	}

	/* Plain authentication request ==> PW_AUTHENTICATE_ONLY */
	if (data.ustype == 0)
	{
		if (new_old == 1) /* new style */
		{
			data.ustype = PW_AUTHENTICATE_ONLY;
		}
		else /* old style */
		{
			data.ustype = PW_OUTBOUND_USER;
		}
	}

	/* Get the user name */
	if (argc == 1)
	{
		data.user_name = argv[0];
	}
	else
	{
		fprintf (stderr, "%s: Missing user name\n", progname);
		radpwtst_usage ();
	}

	dir_init ();

	if (dict_init () != 0)
	{
		fprintf (stderr, "%s: Missing dictionary, check -d option\n",
			progname);
		exit (-1);
	}

	/* Process saved a/v pairs. */
	for ( ; vplist ; vplist = vplist->next)
	{
		if (pair_parse (vplist->str, &data.send_pairs) != 0)
		{
			fprintf (stderr,
				"%s: Invalid attribute-value pair, '%s'\n",
				 progname, vplist->str);
			radpwtst_usage ();
		}
	}

	if (data.code == PW_ACCOUNTING_REQUEST)
	{
		data.password = "";
	}
	else /* get a password from somewhere */
	{
		if (clear_pw == (char *) NULL) /* Get the password */
		{
			if ((clear_pw = rad_getpass ("Password:"))
							== (char *) NULL)
			{
				exit (-1);
			}

			strncpy (passwd, clear_pw, sizeof (passwd));
			data.password = passwd;
		}
		else /* Use the password from the command line (-w) */
		{
			data.password = clear_pw;
		}
	}

	srand (time (0));	/* Use random sequence number in request */
	data.seq_nbr = (u_char) rand ();

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

	if (client_name == (char *) NULL)
	{
		if ((data.client_id = get_ipaddr (ourhostname)) == 0)
		{
			printf ("%s: Couldn't get own IP address!\n", progname);
			data.client_id = 0;
		}
	}
	else /* use name from -i command line option */
	{
		data.client_id = get_ipaddr (client_name);
	}

	if ((data.user_file != (char *) NULL) && (data.group == (char *) NULL))
	{
		data.group = "DEFAULT";
	}

	if ( data.svc_port == 0)
	{
		if (data.code == PW_ACCOUNTING_REQUEST)
		{
			data.svc_port = PW_ACCT_UDP_PORT;
		}
		else
		{
			data.svc_port = PW_AUTH_UDP_PORT;
		}
	}

	msg[0] = '\0';
	result = send_server (&data, &retries, msg);

	if (result == OK_RC)
	{
		printf ("\"%s\" authentication OK", data.user_name);
	}
	else
	{
		printf ("\"%s\" authentication failed", data.user_name);
		if (result != BADRESP_RC)
		{
			printf ("(RC=%i)", result);
		}
	}

	if (msg[0])
	{
		printf (": %s", msg);
	}
	putchar ('\n');
	exit (result);
} /* end of main () */

static void
radpwtst_usage ()

{
	printf ("Usage: %s [-c code] [-d directory] [-f file]\n", progname);
	printf ("\t[-g group] [-h] [-i client-id] [-l async port] [-n]\n");
	printf ("\t[-p UDP-port] [-r retries] [-s server] [-t timeout]\n");
	printf ("\t[-u type] [-v] [-w password] [-x] accessID\n");
	printf ("Codes: Access-Request = 1\n");
	printf ("       Accounting-Request = 4\n");
	printf ("       Password = 7\n");
	printf ("       Status-Server = 12\n");
	exit (-1);
} /* end of radpwtst_usage () */
