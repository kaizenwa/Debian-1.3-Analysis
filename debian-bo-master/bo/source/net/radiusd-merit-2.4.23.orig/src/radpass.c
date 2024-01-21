/*
 *
 *	RADIUS
 *	Remote Authentication Dial In User Service
 *
 *
 *	Livingston Enterprises, Inc.
 *	6920 Koll Center Parkway
 *	Pleasanton, CA   94566
 *
 *	Copyright 1992 Livingston Enterprises, Inc.
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

static char     sccsid[] =
		"@(#)radpass.c	1.2 Copyright 1992 Livingston Enterprises Inc";

static char     rcsid[] = "$Id: radpass.c,v 1.16 1996/03/08 19:03:27 web Exp $";

#include	<sys/types.h>
#include	<sys/socket.h>
#include	<sys/param.h>
#include	<netinet/in.h>

#include	<stdio.h>
#include	<netdb.h>
#include	<time.h>

#include	"radius.h"

#define MAXPWNAM	8

u_char          recv_buffer[4096];
u_char          send_buffer[4096];
char            ourhostname[MAXHOSTNAMELEN];
u_char         *progname;
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

extern void     random_vector ();

static u_char   vector[AUTH_VECTOR_LEN];
static u_char   oldpass[AUTH_PASS_LEN];
static void     radpass_usage ();
static void     result_recv ();

int
main (argc, argv)

int             argc;
u_char         *argv[];

{
	int             salen;
	int             result;
	int             sockfd;
	struct sockaddr salocal;
	struct sockaddr saremote;
	struct sockaddr_in *sin;
	struct servent *svp;
	u_short         svc_port;
	AUTH_HDR       *auth;
	u_char         *username;
	u_char          newpass1[AUTH_PASS_LEN];
	u_char          newpass2[AUTH_PASS_LEN];
	u_char          passbuf[AUTH_PASS_LEN];
	u_char          md5buf[256];
	u_char         *oldvector;
	UINT4           auth_ipaddr;
	u_short         local_port;
	int             total_length;
	u_char         *ptr;
	char           *getpass ();
	int             length;
	int             secretlen;
	int             i;

	progname = argv[0];
	radius_dir = RADIUS_DIR;

	if (argc != 2)
	{
		radpass_usage ();
	}
	/* Get the user name */
	username = argv[1];

	svp = getservbyname ("radius", "udp");
	if (svp == (struct servent *) 0)
	{
		fprintf (stderr, "No such service: %s/%s\n", "radius", "udp");
		exit (-1);
	}
	svc_port = ntohs (svp->s_port);

	/* Get the IP address of the authentication server */
	if ((auth_ipaddr = get_ipaddr ("radius-server")) == (UINT4) 0)
	{
		fprintf (stderr, "Couldn't find host radius-server\n");
		exit (-1);
	}

	sockfd = socket (AF_INET, SOCK_DGRAM, 0);
	if (sockfd < 0)
	{
		(void) perror ("socket");
		exit (-1);
	}

	sin = (struct sockaddr_in *) & salocal;
	memset ((char *) sin, '\0', sizeof (salocal));
	sin->sin_family = AF_INET;
	sin->sin_addr.s_addr = INADDR_ANY;

	local_port = 1025;
	do
	{
		local_port++;
		sin->sin_port = htons ((u_short) local_port);
	} while ((bind (sockfd, (struct sockaddr *) sin,
			sizeof (struct sockaddr)) < 0) &&
		local_port < 64000);
	if (local_port >= 64000)
	{
		close (sockfd);
		(void) perror ("bind");
		exit (-1);
	}

	printf ("Changing Password for user %s\n", username);

	/* Get their old password */
	strcpy ((char *) oldpass, getpass ("Old Password:"));
	if (*oldpass == '\0')
	{
		exit (0);
	}

	/* Get their new password */
	strcpy ((char *) newpass1, getpass ("New Password:"));
	if (*newpass1 == '\0')
	{
		exit (0);
	}

	/* Get their new password again */
	strcpy ((char *) newpass2, getpass ("Re-type New Password:"));
	if (strcmp ((char *) newpass1, (char *) newpass2) != 0)
	{
		printf ("New Passwords didn't match\n");
		exit (-1);
	}

	/* Build a password change request */
	auth = (AUTH_HDR *) send_buffer;
	auth->code = PW_PASSWORD_REQUEST;
	auth->id = 0;
	random_vector (vector);
	memcpy ((char *) auth->vector, (char *) vector, AUTH_VECTOR_LEN);
	total_length = AUTH_HDR_LEN;
	ptr = auth->data;

	/* User Name */
	*ptr++ = PW_USER_NAME;
	length = strlen ((char *) username);
	if (length > MAXPWNAM)
	{
		length = MAXPWNAM;
	}
	*ptr++ = length + 2;
	memcpy ((char *) ptr, (char *) username, length);
	ptr += length;
	total_length += length + 2;

	/* New Password */
	*ptr++ = PW_USER_PASSWORD;
	*ptr++ = AUTH_PASS_LEN + 2;

	/* Encrypt the Password */
	length = strlen ((char *) newpass1);
	if (length > AUTH_PASS_LEN)
	{
		length = AUTH_PASS_LEN;
	}
	memset ((char *) passbuf, '\0', AUTH_PASS_LEN);
	memcpy ((char *) passbuf, (char *) newpass1, length);
	/* Calculate the MD5 Digest */
	secretlen = strlen ((char *) oldpass);
	strcpy ((char *) md5buf, (char *) oldpass);
	memcpy ((char *) md5buf + secretlen, (char *) auth->vector,
		AUTH_VECTOR_LEN);
	md5_calc (ptr, md5buf, secretlen + AUTH_VECTOR_LEN);
	oldvector = ptr;
	/* Xor the password into the MD5 digest */
	for (i = 0; i < AUTH_PASS_LEN; i++)
	{
		*ptr++ ^= passbuf[i];
	}
	total_length += AUTH_PASS_LEN + 2;

	/* Old Password */
	*ptr++ = PW_OLD_PASSWORD;
	*ptr++ = AUTH_PASS_LEN + 2;

	/* Encrypt the Password */
	length = strlen ((char *) oldpass);
	if (length > AUTH_PASS_LEN)
	{
		length = AUTH_PASS_LEN;
	}
	memset ((char *) passbuf, '\0', AUTH_PASS_LEN);
	memcpy ((char *) passbuf, (char *) oldpass, length);
	/* Calculate the MD5 Digest */
	secretlen = strlen ((char *) oldpass);
	strcpy ((char *) md5buf, (char *) oldpass);
	memcpy ((char *) md5buf + secretlen, (char *) oldvector,
		AUTH_VECTOR_LEN);
	md5_calc (ptr, md5buf, secretlen + AUTH_VECTOR_LEN);

	/* Xor the password into the MD5 digest */
	for (i = 0; i < AUTH_PASS_LEN; i++)
	{
		*ptr++ ^= passbuf[i];
	}
	total_length += AUTH_PASS_LEN + 2;

	auth->length = htonl (total_length);

	sin = (struct sockaddr_in *) & saremote;
	memset ((char *) sin, '\0', sizeof (saremote));
	sin->sin_family = AF_INET;
	sin->sin_addr.s_addr = htonl (auth_ipaddr);
	sin->sin_port = htons (svc_port);

	sendto (sockfd, (char *) auth, (int) total_length, (int) 0,
		(struct sockaddr *) sin, sizeof (struct sockaddr_in));

	salen = sizeof (saremote);
	result = recvfrom (sockfd, (char *) recv_buffer,
			   (int) sizeof (recv_buffer),
			   (int) 0, &saremote, &salen);

	if (result > 0)
	{
		result_recv (recv_buffer, result);
		exit (0);
	}
	(void) perror ("recv");
	close (sockfd);
	exit (0);
} /* end of main () */

static void
result_recv (buffer, length)

u_char         *buffer;
int             length;

{
	AUTH_HDR       *auth;
	int             totallen;
	u_char          reply_digest[AUTH_VECTOR_LEN];
	u_char          calc_digest[AUTH_VECTOR_LEN];
	int             secretlen;

	auth = (AUTH_HDR *) buffer;
	totallen = ntohs (auth->length);

	if (totallen != AUTH_HDR_LEN)
	{
		printf ("Received invalid reply length from server\n");
		exit (-1);
	}

	/* Verify the reply digest */
	memcpy ((char *) reply_digest, (char *) auth->vector, AUTH_VECTOR_LEN);
	memcpy ((char *) auth->vector, (char *) vector, AUTH_VECTOR_LEN);
	secretlen = strlen ((char *) oldpass);
	memcpy ((char *) buffer + AUTH_HDR_LEN, (char *) oldpass, secretlen);
	md5_calc (calc_digest, (char *) auth, AUTH_HDR_LEN);

	if (memcmp ((char *) reply_digest, (char *) calc_digest,
		    AUTH_VECTOR_LEN) != 0)
	{
		printf ("Warning: Received invalid reply digest from server\n");
	}

	if (auth->code == PW_PASSWORD_ACK)
	{
		printf ("Password successfully changed\n");
	}
	else
	{
		printf ("Request Denied\n");
	}
	return;
} /* end of result_recv () */

static void
radpass_usage ()
{
	printf ("Usage: %s username\n", progname);
	exit (-1);
} /* end of radpass_usage () */
