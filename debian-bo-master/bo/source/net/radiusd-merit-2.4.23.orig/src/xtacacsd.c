/* #define DEBUG1
   #define DEBUG */
/*
 * TACACS daemon suitable for using on Un*x systems.
 *
 * Janruary 1989, Greg Satz
 *
 * Copyright (c) 1989 by cisco Systems, Inc.
 * All rights reserved.
 *
 * VMS note: Stand-alone mode is the opposite from Unix systems.
 */

/* HUJI - list of machines */
#define	LOCAL_INTERNET_NAME	"MINNIE.ACS.WMICH.EDU"
char           *TrustedHosts[] = {"HUMUS.CS.HUJI.AC.IL", "SHAWARMA.CS.HUJI.AC.IL",
	"SHUM.CC.HUJI.AC.IL", "KINERET.HUJI.AC.IL",
	"BATATA.FH.HUJI.AC.IL", "PLUTO.HUJI.AC.IL",
"MARS.HUJI.AC.IL", "MERCURY.HUJI.AC.IL", "COMA.HUJI.AC.IL", ""};

#define	REMOTE_TACACS_TIMEOUT	5	/* Wait up to 5 seconds for reply
					 * from remote server */

#ifndef VMS
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/file.h>

#include <netinet/in.h>

#include <stdio.h>
#include <errno.h>
#include <pwd.h>
#include <netdb.h>
#include <sys/syslog.h>
#include <utmp.h>
#include <sys/time.h>
#include <sys/types.h>
#else				/* VMS */
#include "includes.h"		/* Will include all needed files */
#define index strchr
#define	errno	socket_errno	/* Multinet calls it Socket_Errno */
#define	read	socket_read
#define	write	socket_write
#define	close	socket_close
#define	perror	socket_perror
#endif				/* VMS */

#ifdef SYSV
#include <fcntl.h>
#define index strchr
#endif

/*
 * TACACS protocol defintions
 */
#define uchar unsigned char
#define ulong unsigned long
#include "tacacs.h"
#define oresponse namelen
#define oreason pwlen


#define	TIMEOUT		(5*60)

#define	TACACS_PORT	49
#define VECTOR_LEN	16	/* random vector length */
#define SECRET		"tempVAXtacacs"
#define SEC_IN_DAY      (24*60*60)	/* seconds in a day */
#define WARNING_PERIOD  14	/* days of expiration warning */

#ifdef VMS
#define	PASSWD_LENGTH	32	/* length of password for crypt */
#else				/* VMS */
#define	PASSWD_LENGTH	14	/* length of password for crypt */
#endif				/* VMS */

#define SOME_ARBITRARILY_LARGE_NUMBER 100

/* our own structure -- neat, eh? */

typedef struct newxtacacstype_
{
	xtacacstype     tp;
	char            data[BUFSIZ];
}               newxtacacstype;


int             debug;		/* debugging flag */
int             logging;	/* syslog logging flag */
int             stand;		/* running standalone or not */
char           *file;		/* validation filename */
char           *wtmpfile;	/* wtmp format filename */
FILE           *wtmpf;
unsigned long   querytime;	/* time query came in */

struct sockaddr_in from;
int             fromlen;
char            SenderHost[256];/* Who sent us the request - for logging */
newxtacacstype  buf;
char           *monthname[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
long            days_ere_month[] = {0, 31, 59, 90, 120, 151,
181, 212, 243, 273, 304, 334};


/*
 * main
 * We can be called from inetd or via the rc scripts directly
 * Parse arguments are act appropiately.
 */

main (argc, argv)
int             argc;
char          **argv;
{
	int             cc,
	                c,
	                on = 1,
	                s;
	struct servent *sp;
	tacacstype     *tp;
	extern char    *optarg;
	struct hostent *hp;

	debug = 0;		/* no debugging */

#ifdef VMS
	logging = 1;		/* Full logging */
	stand = 1;		/* Stand alone - we are run with RUN/DETACH */
#else				/* VMS */
	logging = 0;		/* No logging */
	stand = 0;		/* under inetd */
#endif

	file = NULL;		/* /etc/passwd */
	wtmpfile = NULL;
	wtmpf = NULL;

#ifdef LOG_LOCAL6
	openlog ("tacacsd", LOG_PID, LOG_LOCAL6);
#else
	openlog ("tacacsd", LOG_PID);
#endif

#ifdef VMS
	for (cc = 1; cc < argc; cc++)
		switch (argv[cc][1])
		{
#else				/* VMS */
	while ((c = getopt (argc, argv, "df:lsw:")) != EOF)
		switch (c)
		{
#endif				/* VMS */

		    case 'd':	/* debug */
			debug = 1;
			break;
		    case 'f':	/* file name */

#ifdef VMS
			file = argv[++cc];
#else				/* VMS */
			file = optarg;
#endif				/* VMS */

			break;
		    case 'l':	/* logging */
			logging = 1;
			break;
		    case 's':	/* stand-alone */

#ifdef VMS
			stand = 0;
#else				/* VMS */
			stand = 1;
#endif				/* VMS */

			break;
		    case 'w':

#ifdef VMS
			wtmpfile = argv[++cc];
#else				/* VMS */
			wtmpfile = optarg;
#endif				/* VMS */

			break;
		    default:
			fprintf (stderr, "%s: illegal switch\n", argv[0]);
			exit (1);
		}

	if (debug)
		syslog (LOG_DEBUG, "server starting\n");

	read_tacacs_users_file ();	/* HUJI - read the users database */

	if (stand)
	{

		/*
		 * Background ourselves and let go of controlling tty
		 */
		if (!debug)
		{

#ifndef VMS			/* On VMS it will run from the beginning in
				 * detach mode */
			if (fork ())
				exit (0);
			for (c = 0; c < getdtablesize (); c++)
				(void) close (c);
			(void) open ("/", O_RDONLY);
			(void) dup2 (0, 1);
			(void) dup2 (0, 2);

#ifndef SYSV
			c = open ("/dev/tty", O_RDWR);
			if (c >= 0)
			{
				ioctl (c, TIOCNOTTY, (char *) 0);
				(void) close (c);
			}
#endif

#ifdef LOG_LOCAL6
			openlog ("tacacsd", LOG_PID, LOG_LOCAL6);
#else
			openlog ("tacacsd", LOG_PID);
#endif

#endif				/* !VMS */
		}
	}

	if (stand)
	{

		/*
		 * Pick up a socket
		 */
		if ((s = socket (AF_INET, SOCK_DGRAM, 0)) < 0)
		{
			syslog (LOG_ERR, "socket: %m\n");
			exit (1);
		}

		/*
		 * Get port we need to pay attention to
		 */
		bzero ((caddr_t) & from, sizeof (from));

#ifdef sun
		from.sin_family = AF_INET;
		from.sin_addr.s_addr = INADDR_ANY;
#endif

		sp = getservbyname ("tacacs", "udp");
		if (sp == NULL)
			from.sin_port = ntohs (TACACS_PORT);
		else
			from.sin_port = ntohs (sp->s_port);

		if (bind (s, &from, sizeof (from)) < 0)
		{
			syslog (LOG_ERR, "bind: %m\n");
			exit (1);
		}
	}
	else
	{
		s = 0;

#ifndef VMS
		if (ioctl (s, FIONBIO, &on) < 0)
		{
			syslog (LOG_ERR, "ioctl(FIONBIO): %m\n");
			exit (1);
		}
#endif
	}

	/*
	 * For 4.3BSD machines, this routine sets the file the pw routines
	 * use to the given argument. We emulate it for others.
	 */

#ifdef VMS
	if (getenv ("TACACS_UAF"))
	{
		printf ("Using localy defined UAF file\n");
		init_tacacs_uaf ();
	}
	else
		printf ("Using the standard VMS SYSUAF file\n");
#else				/* VMS */
	if (file != NULL)
		setpwfile (file);
#endif				/* VMS */

	if (wtmpfile != NULL)
	{
		wtmpf = fopen (wtmpfile, "a+");
		if (!wtmpf)
			fprintf (stderr, "\nCan't open wtmp file \"%s\"", wtmpfile);
	}
	if (!stand)
		alarm (TIMEOUT);

again:
	fromlen = sizeof (from);
	c = recvfrom (s, &buf, sizeof (buf), 0, (caddr_t) & from, &fromlen);
	if (c <= 0)
	{
		if (errno == EINTR && stand)
			goto again;
		syslog (LOG_ERR, "recvfrom: %m\n");
		exit (1);
	}
/* Copy HP since next call to GetHostxxx will ruin it */

#ifdef DEBUG
	hp = gethostbyaddr (&from.sin_addr, sizeof (struct in_addr), AF_INET);
	fprintf (stderr, "main: received validation request from %s\n",
		 hp ? hp->h_name : (char *) inet_ntoa (from.sin_addr));
#endif

	if (logging)
	{
		hp = gethostbyaddr (&from.sin_addr, sizeof (struct in_addr), AF_INET);
		if (hp != NULL)
			strcpy (SenderHost, hp->h_name);
		else
			strcpy (SenderHost, (char *) inet_ntoa (from.sin_addr));
		syslog (LOG_INFO, "validation request from %s\n", SenderHost);
	}

	if (buf.tp.version == TA_VERSION)
		old_process (s, &from, &buf.tp);
	else if (buf.tp.version == XTA_VERSION)
		new_process (s, &from, &buf);
	else if (logging)
		syslog (LOG_INFO, "illegal version specified: %d\n", buf.tp.version);

	if (stand)
		goto again;

	exit (0);
}


#ifndef VMS
/*
 * pw_verify
 * verify the provided name/password.
 */
pw_verify (name, passwd, ppw)

#ifdef SYSV
char            name[SOME_ARBITRARILY_LARGE_NUMBER];
char            passwd[SOME_ARBITRARILY_LARGE_NUMBER];

#else
char           *name,
               *passwd;

#endif
struct passwd **ppw;
{
	struct passwd  *pw;

	setpwent ();
	if (file != NULL)
		setpwfile (file);
	pw = getpwnam (name);

#ifdef DEBUG
	if (pw)
		fprintf (stderr, "password: user %8.8s, password %13.13s\r\n",
			 pw->pw_name, pw->pw_passwd);
#endif


	/*
	 * Verify the entry.
	 */
	if (pw != NULL && *passwd != '\0' && *pw->pw_passwd != '\0')
	{

#ifdef SYSV
		strcpy (passwd, (char *) crypt (passwd, pw->pw_passwd));
#else
		passwd = (char *) crypt (passwd, pw->pw_passwd);
#endif

		*ppw = pw;
		if (strcmp (passwd, pw->pw_passwd) == 0)
			return (1);
	}
	*ppw = NULL;
	return (0);
}

#endif				/* VMS */


/*
 * process
 * Perform necessary stuff to do a query operation. Return ANSWER.
 */

old_process (s, client, tp)
int             s;
struct sockaddr_in *client;
tacacstype     *tp;
{

#ifdef SYSV
	char            name[SOME_ARBITRARILY_LARGE_NUMBER];
	char            passwd[SOME_ARBITRARILY_LARGE_NUMBER];

#else
	char           *name,
	               *passwd;

#endif

#ifdef VMS
	int             pw;

#else				/* VMS */
	struct passwd  *pw;

#endif
	int             expired;

	querytime = time (NULL);

#ifdef DEBUG
	fprintf (stderr, "process: starting\r\n");
	fprintf (stderr, "process: namelen %d, pwdlen %d\r\n",
		 tp->namelen, tp->pwlen);
#endif

#ifndef SYSV
	name = (char *) malloc (tp->namelen + 1);
	if (name == NULL)
		perror ("Malloc");

	passwd = (char *) malloc (tp->pwlen + 1);
	if (passwd == NULL)
		perror ("Malloc");

	if (name == NULL || passwd == NULL)
		return;
#endif				/* not SYSV */

	strncpy (name, (char *) (tp + 1), tp->namelen);
	name[tp->namelen] = '\0';
	strncpy (passwd, (char *) (tp + 1) + tp->namelen, tp->pwlen);
	if (tp->pwlen > PASSWD_LENGTH)
		tp->pwlen = PASSWD_LENGTH;
	passwd[tp->pwlen] = '\0';

#ifdef DEBUG
	fprintf (stderr, "packet: %s %s\r\n", (char *) (tp + 1),
		 (char *) (tp + 1) + tp->namelen);
	fprintf (stderr, "local:  %s %s\r\n", name, passwd);
#endif

	/*
	 * Assume failure
	 */
	tp->oresponse = TA_A_REJECTED;
	tp->oreason = TA_A_DENIED;

/* HUJI - check whether the user is in our local database */
	{
		char            phone[256],
		                flags;

		if (get_user_entry (name, passwd, &flags, phone))
		{		/* got it ok */
			if (flags & F_DIALBACK)
			{	/* Need a dialback */
				syslog (LOG_INFO, "Dialback requested by '%s' to phone %s\n",
					name, phone);
				tp->oreason = TA_A_EXPIRING;	/* Reject with expired
								 * message */
			}
			else
			{	/* OK - let him in */
				syslog (LOG_INFO, "User '%s' passed-in according to NUI file\n",
					name);
				tp->oresponse = TA_A_ACCEPTED;
				tp->oreason = TA_A_NONE;
			}
			goto o_ok;	/* jump over the regular
					 * authorization */
		}
	}

/* Remove @Host or %host form username if there is */
	{
		char           *p;

		if ((p = strchr (name, '@')) != NULL)
			*p = '\0';
		else if ((p = strchr (name, '%')) != NULL)
			*p = '\0';
	}
/* HUJI - end of modifications */

#ifdef VMS
	if ((verify_user (name, passwd, &pw) & 0x1) == 1)
	{			/* Verify returned ok */
		if (pw == USER_OK)
		{
			tp->oresponse = TA_A_ACCEPTED;
			tp->oreason = TA_A_NONE;
		}
		else if (pw == USER_EXPIRING)
		{
			tp->oresponse = TA_A_ACCEPTED;
			tp->oreason = TA_A_EXPIRING;
		}
		else if (pw == USER_EXPIRED)
		{
			tp->oreason = TA_A_EXPIRING;
		}
	}
#else				/* VMS */
	if (pw_verify (name, passwd, &pw))
	{
		tp->oresponse = TA_A_ACCEPTED;
		tp->oreason = TA_A_NONE;

		/*
		 * Now check the expiration time.
		 */

		expired = check_expiration (pw->pw_shell);
		if (expired == 2)
		{
			tp->oresponse = TA_A_DENIED;
			tp->oreason = TA_A_EXPIRING;
		}
		else if (expired == 1)
			tp->oreason = TA_A_EXPIRING;
	}
#endif				/* VMS */

o_ok:
	if (logging)
	{

#ifdef VMS
		syslog (LOG_INFO, "login query for %s@%s %s\n", name,
			(char *) inet_ntoa (from.sin_addr),
		  tp->oresponse == TA_A_ACCEPTED ? "accepted" : "rejected");
#else				/* VMS */
		if (pw != NULL)
			syslog (LOG_INFO, "login query for %s (%s) %s\n", name, pw->pw_gecos,
				tp->oresponse == TA_A_ACCEPTED ? "accepted" : "rejected");
		else
			syslog (LOG_INFO, "login query for %s %s\n", name,
				tp->oresponse == TA_A_ACCEPTED ? "accepted" : "rejected");
#endif				/* VMS */
	}

	tp->type = TA_ANSWER;
	if (sendto (s, buf, sizeof (tacacstype), 0, client,
		    sizeof (struct sockaddr_in)) != sizeof (tacacstype))
		syslog (LOG_ERR, "write: %m\n");

#ifndef SYSV
	free (name);
	free (passwd);
#endif

#ifdef DEBUG
	fprintf (stderr, "process: done\r\n");
#endif
}


/*
 * new_process
 * Perform necessary stuff to do a query operation. Return ANSWER.
 */

new_process (s, client, buf)
int             s;
struct sockaddr_in *client;
newxtacacstype *buf;
{

#ifdef DEBUG
	fprintf (stderr, "new_process: start\r\n");
#endif

	querytime = time (NULL);
	switch (buf->tp.type)
	{
	    case XTA_SLIPADDR:
	    case XTA_LOGIN:
		xlogin (s, client, buf);
		break;
	    case XTA_CONNECT:
		xconnect (s, client, buf->tp);
		break;
	    case XTA_ENABLE:
		xenable (s, client, buf->tp);
		break;
	    case XTA_LOGOUT:
		xlogout (s, client, buf->tp);
		break;
	    case XTA_RELOAD:
		xreload (s, client, buf->tp);
		break;
	    case XTA_SLIPON:
		xslipon (s, client, buf->tp);
		break;
	    case XTA_SLIPOFF:
		xslipoff (s, client, buf->tp);
		break;
	    default:
		if (logging)
			syslog (LOG_INFO, "illegal type specified: %d\n", buf->tp.type);
	}
}

#ifndef VMS
check_expiration (date)
char           *date;
{
	long            day,
	                month,
	                year,
	                leaps,
	                now,
	                expiration,
	                warning;
	char            monthstr[10];

	/*
	 * If no date or a shell, let it pass.  (Backward compatability.)
	 */
	if (!date || (strlen (date) == 0) || (*date == '/'))
		return (0);

	/*
	 * Parse date string.  Fail it upon error.
	 */
	if (sscanf (date, "%s %d %d", monthstr, &day, &year) != 3)
		return (2);

	/*
	 * Compute the expiration date in days.
	 */
	for (month = 0; month < 12; month++)
		if (strncmp (monthstr, monthname[month], 3) == 0)
			break;
	if (month > 11)
		return (2);
	leaps = (year - 1969) / 4 + (((year % 4) == 0) && (month > 2));
	expiration = (((year - 1970) * 365) + days_ere_month[month] + (day - 1) + leaps);
	warning = expiration - WARNING_PERIOD;

	/*
	 * Get the current time (to the day)
	 */
	now = querytime / SEC_IN_DAY;

	if (now > expiration)
		return (2);
	if (now > warning)
		return (1);
	return (0);
}

#endif				/* VMS */


#ifndef BSD43

#ifndef VMS
/*
 * setpwfile
 * Hack to get around the default for the pw routines using /etc/passwd
 */

#include <sys/stat.h>

setpwfile (file)
char           *file;
{
	FILE           *f;
	struct stat     pwbuf,
	                fbuf;
	int             i;
	char           *c;

	if (stat ("/etc/passwd", &pwbuf) < 0)
	{
		syslog (LOG_ERR, "stat: %m\n");
		exit (1);
	}

	setpwent ();		/* open /etc/passwd */

	/*
	 * This loop assumes that the stdio file buffers are contiguous which
	 * isn't true for 4.3, but then we won't be here.
	 */

	for (f = stderr + 1; f < stdin + getdtablesize (); f++)
		if (f->_flag & (_IOREAD | _IOWRT | _IORW) &&
				fstat (fileno (f), &fbuf) >= 0 &&
				pwbuf.st_dev == fbuf.st_dev &&
				pwbuf.st_ino == fbuf.st_ino)
		{
			freopen (file, "r", f);
			fprintf (stderr, "hit at %d\n", fileno (f));
			return;
		}

	syslog (LOG_ERR, "couldn't find /etc/passwd to replace\n");
	exit (1);
}

#endif				/* VMS */

#endif


#ifdef SYSV
getdtablesize ()
{
	return (_NFILE);
}

#endif


wtmp_entry (line, name, host)
char           *line,
               *name,
               *host;
{

#ifdef VMS
	return;			/* No WTMP file support yet */
#else				/* VMS */
	struct utmp     entry;

	if (wtmpf == NULL)
		return;

	bzero (&entry, sizeof entry);

	if (strlen (line) < sizeof entry.ut_line)
		strcpy (entry.ut_line, line);
	else
		bcopy (line, entry.ut_line, sizeof entry.ut_line);

	if (strlen (name) < sizeof entry.ut_name)
		strcpy (entry.ut_name, name);
	else
		bcopy (name, entry.ut_name, sizeof entry.ut_name);

	if (strlen (host) < sizeof entry.ut_host)
		strcpy (entry.ut_host, host);
	else
		bcopy (host, entry.ut_host, sizeof entry.ut_host);

	entry.ut_time = querytime;

	if (fwrite (&entry, sizeof entry, 1, wtmpf) != 1)
	{
		if (logging)
			syslog (LOG_ERR, "couldn't write syslog file\n");
	}
	else
		fflush (wtmpf);


#ifdef DEBUG1
	fprintf (stderr, "\nwtmp: %s, %s %s %d", line, name, host, querytime);
#endif

#endif				/* VMS */
}


xlogin (s, client, buf)
int             s;
struct sockaddr_in *client;
register newxtacacstype *buf;
{

#ifdef SYSV
	char            name[SOME_ARBITRARILY_LARGE_NUMBER];
	char            passwd[SOME_ARBITRARILY_LARGE_NUMBER];

#else
	char           *name,
	               *passwd,
	               *secret,
	               *filename;
	char           *vector,
	               *buffer_ptr;
	uchar           buffer[BUFSIZ];
	uchar           digest[32];

#endif

#ifdef VMS
	int             pw,
	                i;

#else				/* VMS */
	struct passwd  *pw;

#endif				/* VMS */
	int             expired;
	char            linename[20];

#ifdef DEBUG
	fprintf (stderr, "xlogin: starting\r\n");
	fprintf (stderr, "xlogin: namelen %d, pwdlen %d\r\n",
		 buf->tp.namelen, buf->tp.pwlen);
#endif

#ifndef SYSV
	name = (char *) malloc (buf->tp.namelen + 1);
	if (name == NULL)
		perror ("Malloc");

	passwd = (char *) malloc (buf->tp.pwlen + 1);
	if (passwd == NULL)
		perror ("Malloc");

	secret = (char *) malloc (40);
	if (secret == NULL)
		perror ("Malloc");
	if (name == NULL || passwd == NULL)
		return;
#endif				/* not SYSV */

/*    strncpy(name, ((char *)tp)+XTACACSSIZE, tp->namelen); */
	strncpy (name, buf->data, buf->tp.namelen);
/*   name[tp->namelen] = '\0'; */
	name[buf->tp.namelen] = '\0';
/*    strncpy(passwd, ((char *)tp)+XTACACSSIZE + tp->namelen + 1,
              tp->pwlen-1); */
	strncpy (passwd, &(buf->data[buf->tp.namelen + 1]), buf->tp.pwlen - 1);
/*
    if (tp->pwlen > PASSWD_LENGTH)
	tp->pwlen = PASSWD_LENGTH;
    passwd[tp->pwlen] = '\0';
*/
	if (buf->tp.pwlen > PASSWD_LENGTH)
		buf->tp.pwlen = PASSWD_LENGTH;
	passwd[buf->tp.pwlen] = '\0';

/* 16 uchar random vector to be appended to the secret */
/*
    vector = (char *) ((char *) tp + sizeof(*tp) +
		       (u_long) tp->namelen + (u_long) tp->pwlen);
*/
	vector = &(buf->data[buf->tp.namelen + buf->tp.pwlen]);

	if (get_secret (secret) < 0)
	{
		fprintf (stderr, "Access Denied");
		return;
	}

	strcpy (buffer, secret);
	buffer_ptr = (char *) (buffer + (u_long) strlen (secret));
	memcpy (buffer_ptr, vector, VECTOR_LEN);
	md5_calc (digest, buffer, strlen (secret) + VECTOR_LEN);

	for (i = 0; i < strlen (secret); i++)
		passwd[i] ^= digest[i];


#ifdef DEBUG
/*
    fprintf(stderr, "packet: %s %s\r\n", (char *)(tp + 1),
	    (char *)(tp + 1) + tp->namelen);
    fprintf(stderr, "local:  %s %s\r\n", name, passwd);
*/
#endif

#ifdef DEBUG1
	fprintf (stderr, "\nxlogin: user %s on tty%x, host %s", name, buf->tp.lport,
		 SenderHost);
#endif

	/*
	 * Assume failure
	 */
	buf->tp.response = TA_A_REJECTED;
	buf->tp.reason = TA_A_DENIED;

/* HUJI - check whether the user is in our local database */
	{
		char            phone[256],
		                flags;

		if (get_user_entry (name, passwd, &flags, phone))
		{		/* got it ok */
			if (flags & F_DIALBACK)
			{	/* Need a dialback */
				syslog (LOG_INFO, "Dialback requested by '%s' to phone %s\n",
					name, phone);
				buf->tp.reason = TA_A_EXPIRING;	/* Reject with expired
								 * message */
			}
			else
			{	/* OK - let him in */
				syslog (LOG_INFO, "User '%s' passed-in according to NUI file\n",
					name);
				buf->tp.response = TA_A_ACCEPTED;
				buf->tp.reason = TA_A_NONE;
			}
			goto n_ok;	/* jump over the regular
					 * authorization */
		}
	}

/* HUJI - If there is @ or % then remove it and check the address. If it is ours -
   continue as usual. If not, and if the other machine is listed in the ones
   we trust - ask it */
	{
		char           *p;

		p = strchr (name, '@');
		if (p == NULL)
			p = strchr (name, '%');
		if (p != NULL)
		{
			*p++ = '\0';	/* Remove the @ from the username */
			upcase_string (p);
			if (strcmp (p, LOCAL_INTERNET_NAME) != 0)
			{	/* Not us */
				verify_user_remote (p, name, passwd);
				goto RemoteVerified;
			}
		}
	}
/* HUJI - end of modifications */

#ifdef VMS
	if (check_username (name, filename) == 1)
	{

		if ((verify_user (name, passwd, &pw) & 0x1) == 1)
		{		/* Verify returned ok */
			if (pw == USER_OK)
			{
				buf->tp.response = TA_A_ACCEPTED;
				buf->tp.reason = TA_A_NONE;
			}
			else if (pw == USER_EXPIRING)
			{
				buf->tp.response = TA_A_ACCEPTED;
				buf->tp.reason = TA_A_EXPIRING;
			}
			else if (pw == USER_EXPIRED)
			{
				buf->tp.response = TA_A_DENIED;
				buf->tp.reason = TA_A_EXPIRING;
			}
		}
	}
#else				/* VMS */
	if (pw_verify (name, passwd, &pw))
	{
		buf->tp.response = XTA_A_ACCEPTED;
		buf->tp.reason = XTA_A_NONE;
		buf->tp.uuid = pw->pw_uid;
		buf->tp.accesslist = pw->pw_gid;
		buf->tp.flags = xta_getflags (pw->pw_gecos);

		/*
		 * Now check the expiration time.
		 */

		expired = check_expiration (pw->pw_shell);
		if (expired == 2)
		{
			buf->tp.response = TA_A_DENIED;
			buf->tp.reason = TA_A_EXPIRING;
		}
		else if (expired == 1)
			buf->tp.reason = TA_A_EXPIRING;
	}
#endif				/* VMS */

n_ok:
RemoteVerified:
	sprintf (linename, "TTY%x", buf->tp.lport);
	if (buf->tp.response == TA_A_ACCEPTED && buf->tp.type == XTA_LOGIN)
		wtmp_entry (linename, name, SenderHost);

	if (logging && buf->tp.type == XTA_LOGIN)
	{

#ifdef VMS
		syslog (LOG_INFO, "xlogin query from %s %s for %s %s\n",
			SenderHost,
			linename, name,
		buf->tp.response == TA_A_ACCEPTED ? "accepted" : "rejected");
#else				/* VMS */
		if (pw != NULL)
			syslog (LOG_INFO, "xlogin query from %s %s for %s (%s) %s\n",
				SenderHost,
				linename, name, pw->pw_gecos,
				buf->tp.response == TA_A_ACCEPTED ? "accepted" : "rejected");
		else
			syslog (LOG_INFO, "xlogin query from %s %s for %s %s\n",
				SenderHost,
				linename, name,
				buf->tp.response == TA_A_ACCEPTED ? "accepted" : "rejected");
#endif				/* VMS */
	}
	if (logging && buf->tp.type == XTA_SLIPADDR)
	{

#ifdef VMS
		syslog (LOG_INFO, "slipaddress from %s %s for %s %s\n",
			SenderHost,
			linename, name,
		buf->tp.response == TA_A_ACCEPTED ? "accepted" : "rejected");
#else				/* VMS */
		if (pw != NULL)
			syslog (LOG_INFO, "slipaddress from %s %s for %s (%s) %s\n",
				SenderHost,
				linename, name, pw->pw_gecos,
				buf->tp.response == TA_A_ACCEPTED ? "accepted" : "rejected");
		else
			syslog (LOG_INFO, "slipaddress from %s %s for %s %s\n",
				SenderHost,
				linename, name,
				buf->tp.response == TA_A_ACCEPTED ? "accepted" : "rejected");
#endif
	}

	buf->tp.type = TA_ANSWER;

	/* We want to copy the current data stored in tp to buffer */

	/*
	 * we want to copy the vector we received from the sender to the end
	 * of the packet we want to send to md5
	 */

	memcpy (buf->data, vector, VECTOR_LEN);

	/* we want to copy the secret after the vector */

	memcpy (&(buf->data[VECTOR_LEN]), secret, strlen (secret));

	/*
	 * send this entire new buf to md5_calc so we can get a digest to
	 * append at tp->access back to the sending program
	 */

	md5_calc (digest, buf, sizeof (xtacacstype) + VECTOR_LEN + strlen (secret));

	/* copy the digest after the accesslist */

	memcpy (buf->data, digest, VECTOR_LEN);

	if ((i = (sendto (s, buf, sizeof (xtacacstype) + VECTOR_LEN, 0, client,
	sizeof (struct sockaddr_in)))) != sizeof (xtacacstype) + VECTOR_LEN)
	{
		syslog (LOG_ERR, "write: %m\n");
		printf ("i=%d", i);
	}

#ifndef SYSV
	free (name);
	free (passwd);
#endif

#ifdef DEBUG
	fprintf (stderr, "xlogin: done\r\n");
#endif
}


xconnect (s, client, tp)
int             s;
struct sockaddr_in *client;
xtacacstype    *tp;
{
	struct hostent *hp1;
	char           *name = ((char *) tp) + XTACACSSIZE;

	name[tp->namelen] = 0;
	hp1 = gethostbyaddr (&tp->dhost, sizeof (struct in_addr), AF_INET);

#ifdef DEBUG1
	fprintf (stderr, "\nxconnect: user %.*s(%d) to %s:%d", tp->namelen,
		 ((char *) tp) + XTACACSSIZE, tp->uuid,
	     hp1 ? hp1->h_name : (char *) inet_ntoa (tp->dhost), tp->dport);
#endif


	if (logging)
		syslog (LOG_INFO, "xconnect from %s tty%x for %s (%d) to %s:%d\n",
			SenderHost,
			tp->lport, name, tp->uuid,
			hp1 ? hp1->h_name : (char *) inet_ntoa (tp->dhost), tp->dport);
	replyok (s, client, tp);
}


xenable (s, client, tp)
int             s;
struct sockaddr_in *client;
xtacacstype    *tp;
{

#ifdef SYSV
	char            name[SOME_ARBITRARILY_LARGE_NUMBER];
	char            passwd[SOME_ARBITRARILY_LARGE_NUMBER];

#else
	char           *name,
	               *passwd;

#endif
	struct passwd  *pw;
	int             expired;
	char            linename[20];

#ifndef SYSV
	name = (char *) malloc (tp->namelen + 1);
	passwd = (char *) malloc (tp->pwlen + 1);
	if (name == NULL || passwd == NULL)
		return;
#endif				/* not SYSV */

	sprintf (linename, "TTY%x", tp->lport);
	strncpy (name, (char *) (tp + 1), tp->namelen);
	name[tp->namelen] = '\0';
	strncpy (passwd, (char *) (tp + 1) + tp->namelen, tp->pwlen);
	if (tp->pwlen > PASSWD_LENGTH)
		tp->pwlen = PASSWD_LENGTH;
	passwd[tp->pwlen] = '\0';

#ifdef DEBUG1
	fprintf (stderr, "\nxenable: user %s on tty%x, host %s", name, tp->lport,
		 SenderHost);
#endif

	/*
	 * Assume failure
	 */
	tp->response = TA_A_REJECTED;
	tp->reason = TA_A_DENIED;

#ifdef VMS
	if ((verify_user ("$ENABLE$", passwd, &pw) & 0x1) == 1)
	{			/* Verify returned ok */
		if (pw == USER_OK)
		{
			tp->response = TA_A_ACCEPTED;
			tp->reason = TA_A_NONE;
		}
		else if (pw == USER_EXPIRING)
		{
			tp->response = TA_A_ACCEPTED;
			tp->reason = TA_A_EXPIRING;
		}
		else if (pw == USER_EXPIRED)
		{
			tp->reason = TA_A_EXPIRING;
		}
	}
#else				/* VMS */
	if (pw_verify ("$enable$", passwd, &pw))
	{
		tp->response = XTA_A_ACCEPTED;
		tp->reason = XTA_A_NONE;
		tp->uuid = pw->pw_uid;
		tp->accesslist = pw->pw_gid;
		tp->flags = xta_getflags (pw->pw_gecos);

		/*
		 * Now check the expiration time.
		 */

		expired = check_expiration (pw->pw_shell);
		if (expired == 2)
		{
			tp->response = TA_A_DENIED;
			tp->reason = TA_A_EXPIRING;
		}
		else if (expired == 1)
			tp->reason = TA_A_EXPIRING;
	}
#endif				/* VMS */

	sprintf (linename, "TTY%x", tp->lport);

	tp->type = TA_ANSWER;
	if (logging)
		syslog (LOG_INFO, "xenable from %s %s for %s %s\n",
			SenderHost,
			linename, name,
		   tp->response == TA_A_ACCEPTED ? "accepted" : "rejected");
	if (sendto (s, buf, XTACACSSIZE, 0, client,
		    sizeof (struct sockaddr_in)) != XTACACSSIZE)
		syslog (LOG_ERR, "write: %m\n");

#ifndef SYSV
	free (name);
	free (passwd);
#endif
}


xlogout (s, client, tp)
int             s;
struct sockaddr_in *client;
xtacacstype    *tp;
{
	char           *name = ((char *) tp) + XTACACSSIZE;
	char            linename[20];
	char           *reason;

	switch (tp->reason)
	{
	    case XTA_R_IDLE:
		reason = "Idle-timeout";
		break;
	    case XTA_R_DROP:
		reason = "Carrier-Drop";
		break;
	    case XTA_R_BAD:
		reason = "Bad-Passwords";
		break;
	    case XTA_R_QUIT:
		reason = "";
		break;
	    default:
		reason = "Unknown-reason";
		break;
	}

	name[tp->namelen] = 0;

#ifdef DEBUG1
	fprintf (stderr, "\nxlogout:  user %s(%d) line %x %s", name, tp->uuid,
		 tp->lport, reason);
#endif

	sprintf (linename, "TTY%x", tp->lport);
	if (logging)
		syslog (LOG_INFO, "xlogout from %s %s, user %s(%d) %s\n",
			SenderHost, linename,
			name, tp->uuid, reason);
	wtmp_entry (linename, "", SenderHost);
	replyok (s, client, tp);
}

xreload (s, client, tp)
int             s;
struct sockaddr_in *client;
xtacacstype    *tp;
{

#ifdef DEBUG1
	fprintf (stderr, "\nxreload: host %s", SenderHost);
#endif

	if (logging)
		syslog (LOG_INFO, "system reload from %s\n", SenderHost);
	wtmp_entry ("~", "", SenderHost);
	replyok (s, client, tp);
}

xslipon (s, client, tp)
int             s;
struct sockaddr_in *client;
xtacacstype    *tp;
{
	struct hostent *hp1;
	char            linename[20];
	char           *name = (char *) (tp + 1);

	name[tp->namelen] = 0;

	hp1 = gethostbyaddr (&tp->dhost, sizeof (struct in_addr), AF_INET);

#ifdef DEBUG1
	fprintf (stderr, "\nxslipon:  user %.*s(%d) line %x slip address %s",
	      tp->namelen, ((char *) tp) + XTACACSSIZE, tp->uuid, tp->lport,
		 hp1 ? hp1->h_name : (char *) inet_ntoa (tp->dhost));
#endif

	sprintf (linename, "SLIP%x", tp->lport);
	wtmp_entry (linename, name, hp1 ? hp1->h_name : (char *) inet_ntoa (tp->dhost));
	if (logging)
		syslog (LOG_INFO, "xslipon from %s %s for  user %s(%d) address %s\n",
			SenderHost, linename,
			name, tp->uuid,
			hp1 ? hp1->h_name : (char *) inet_ntoa (tp->dhost));


	replyok (s, client, tp);
}



xslipoff (s, client, tp)
int             s;
struct sockaddr_in *client;
xtacacstype    *tp;
{
	struct hostent *hp1;
	char            linename[20];
	char           *name;

	hp1 = gethostbyaddr (&tp->dhost, sizeof (struct in_addr), AF_INET);


#ifdef DEBUG1
	fprintf (stderr, "\nxslipoff:  user %.*s(%d) line %x slip address %s",
	      tp->namelen, ((char *) tp) + XTACACSSIZE, tp->uuid, tp->lport,
		 hp1 ? hp1->h_name : (char *) inet_ntoa (tp->dhost));
#endif

	sprintf (linename, "SLIP%x", tp->lport);
	wtmp_entry (linename, "", hp1 ? hp1->h_name : (char *) inet_ntoa (tp->dhost));
	name = (char *) (((char *) tp) + XTACACSSIZE);
	name[tp->namelen] = 0;
	if (logging)
		if (logging)
			syslog (LOG_INFO, "xslip off from %s %s for %s(%d) address %s\n",
				SenderHost, linename,
				name, tp->uuid,
			hp1 ? hp1->h_name : (char *) inet_ntoa (tp->dhost));

	replyok (s, client, tp);
}


xta_getflags (string)
char           *string;
{
	return (0);
}

/*
 * Send an "ok" reply to client (for things like reload, logout)
 */
replyok (s, client, tp)
int             s;
struct sockaddr_in *client;
xtacacstype    *tp;
{
	tp->response = XTA_A_ACCEPTED;
	tp->reason = XTA_A_NONE;
	tp->type = TA_ANSWER;
	if (sendto (s, buf, XTACACSSIZE, 0, client,
		    sizeof (struct sockaddr_in)) != XTACACSSIZE)
		syslog (LOG_ERR, "write: %m\n");
}


/* HUJI
 | If the user specified @some-host or %host verify that this is a trusted host and
 | send the query to it.
 |  Both the input and the output and in BUF. We assume that it was initialized
 | to an error reply, so in case of errors we do not touch it.
 */
verify_user_remote (host, name, passwd)
char           *host,		/* The host name to query */
               *name,		/* the username without @ */
               *passwd;		/* the password */
{
	int             i,
	                s,	/* Socket channel */
	                size;	/* Send size */
	xtacacstype    *tp;
	struct hostent *hp1;	/* For getting the host address */
	struct sockaddr_in RemoteSocket;
	fd_set          ReadFds;/* For select */
	struct timeval  timeout;
	int             TransactionID;	/* The ID the caller gave to this
					 * trnasaction */

	tp = (struct xtacacstype *) & (buf.tp);

/* Check whether the host is in the allowed list */
	size = strlen (host);
	for (i = 0; *TrustedHosts[i] != '\0'; i++)
		if (strncmp (host, TrustedHosts[i], size) == 0)	/* Match */
			break;

	if (*TrustedHosts[i] == '\0')	/* Not found */
		return;

/* OK - the host is trusted. Find its IP address. */
	if ((hp1 = gethostbyname (TrustedHosts[i])) == NULL)
		return;

/* Rewrite the username and password so the username now do not contain the @ */
	strcpy (((char *) tp) + XTACACSSIZE, name);
	tp->namelen = strlen (name);
	tp->pwlen = strlen (passwd);
	strcpy (((char *) tp) + XTACACSSIZE + tp->namelen, passwd);
	size = XTACACSSIZE + tp->namelen + tp->pwlen;	/* Total size */
	TransactionID = tp->trans;	/* Save it to compare later */

/* Create a local socket */
	if ((s = socket (AF_INET, SOCK_DGRAM, 0)) == -1)
	{
		perror ("Socket");
		return;
	}

/* Fill the Sockaddr parameters */
	bzero ((caddr_t) & RemoteSocket, sizeof (RemoteSocket));
	memcpy ((char *) &RemoteSocket.sin_addr.s_addr,
		(char *) hp1->h_addr, sizeof (RemoteSocket.sin_addr.s_addr));
	RemoteSocket.sin_family = hp1->h_addrtype;
	RemoteSocket.sin_port = htons (TACACS_PORT);

	if (sendto (s, buf, size, 0, &RemoteSocket, sizeof (RemoteSocket)) < size)
	{
		perror ("Sendto");
		close (s);
		return;
	}

/* Now, wait up to the sepcified time to the reply */
	timeout.tv_sec = REMOTE_TACACS_TIMEOUT;
	timeout.tv_usec = 0;
	FD_ZERO (&ReadFds);
	FD_SET (s, &ReadFds);
	if (select (getdtablesize (), &ReadFds, NULL, NULL, &timeout) <= 0)
	{
		/* Timeout or error */
		close (s);
		return;
	}

/* OK, now read */
	size = sizeof (RemoteSocket);
	if (recvfrom (s, buf, XTACACSSIZE, 0, &RemoteSocket, &size) < XTACACSSIZE)
	{
		perror ("Recvfrom");
		close (s);
		return;
	}

	close (s);

/* Check that the sender is indeed the one we asked and from the port we asked.
   If not - invalidate the response. If yes - return as-is */
	if ((memcmp ((char *) &RemoteSocket.sin_addr.s_addr,
	(char *) hp1->h_addr, sizeof (RemoteSocket.sin_addr.s_addr)) != 0) ||
			(RemoteSocket.sin_port != ntohs (TACACS_PORT)))
	{
		syslog (LOG_INFO, "Received reply from host %x instead of %x\n",
			RemoteSocket.sin_addr.s_addr, (int) hp1->h_addr);
		tp->response = TA_A_REJECTED;
		tp->reason = TA_A_DENIED;
	}
	if (tp->trans != TransactionID)
	{			/* received response for a different query */
		syslog (LOG_INFO, "Transaction ID different: %d != %d from host %x\n",
		    tp->trans, TransactionID, RemoteSocket.sin_addr.s_addr);
		tp->response = TA_A_REJECTED;
		tp->reason = TA_A_DENIED;
	}
}

#ifndef VMS


/* upcase_string
 *   Upcase a string in place.
 */

void 
upcase_string (s)
char           *s;
{
	while (*s)
	{
		*s = _toupper (*s);
		s++;
	}

}				/* upcase_string */

#endif				/* VMS */

/* Check_valid username
    User names are read from the given file.  The name
    of the file is hard coded for the time being but
    can be made to pass as an argument when the daemon is started

    Arguments: char *name,*filename
    Returns:
  	     1 -  if the user is not found in the file.
             0 -  if the user is found in the file or null username
	     -1   if some error occurs

*/
int 
check_username (name, filename)
char           *name;
char           *filename;
{
	FILE           *fp;
	int             done,
	                len;
	char            username[32];

	fp = fopen ("user_list.lis", "r");
	if (fp == NULL)
	{
		syslog (LOG_ERR, "can't open file user_list.lis\n");
		return -1;
	}
	done = 0;
	if (strlen (name) == 0)
	{
		syslog (LOG_INFO, "Null Username found. rejected\n");
		return 0;
	}
	my_lower (name);
	while (!done)
	{
		fscanf (fp, "%s", username);
		len = strlen (username);
		my_lower (username);
		if (strncmp (name, username, len) == 0)
		{
			syslog (LOG_INFO, "User %s found in file. rejecting\n", name);
			return 0;
		}
		if (feof (fp))
			done = 1;
		else
			fscanf (fp, "\n");
	}
	fclose (fp);
	return 1;
}
int 
get_secret (s)
char           *s;
{
	FILE           *fp;

	fp = fopen ("secfile.lis", "r");
	if (fp == NULL)
	{
		syslog (LOG_ERR, "can't open file secfile.lis");
		return -1;
	}
	fscanf (fp, "%s", s);
	fclose (fp);
	return 1;
}

my_lower (s)
char           *s;
{
	char           *p;

	p = s;
	while (*s != '\0')
	{
		*s = toascii (tolower (*s));
		s++;
	}
	s = p;
}
