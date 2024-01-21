 /*
  * RADIUS Remote Authentication Dial In User Service
  *
  *
  * Livingston Enterprises, Inc. 6920 Koll Center Parkway Pleasanton, CA  94566
  *
  * Copyright 1992 Livingston Enterprises, Inc.
  *
  * Permission to use, copy, modify, and distribute this software for any
  * purpose and without fee is hereby granted, provided that this copyright
  * and permission notice appear on all copies and supporting documentation,
  * the name of Livingston Enterprises, Inc. not be used in advertising or
  * publicity pertaining to distribution of the program without specific
  * prior permission, and notice be given in supporting documentation that
  * copying and distribution is by permission of Livingston Enterprises, Inc.
  * 
  * Livingston Enterprises, Inc. makes no representations about the suitability
  * of this software for any purpose.  It is provided "as is" without express
  * or implied warranty.
  *
  * Public entry points in this file:
  *
  * pw_expired
  *
  */

static char     sccsid[] =
	       "@(#)passchange.c 1.5 Copyright 1992 Livingston Enterprises Inc";

static char     rcsid[] = "$Id: passchange.c,v 1.3 1995/07/12 14:12:55 web Exp $";

#include	<sys/types.h>

#include	<sys/param.h>
#include	<sys/socket.h>
#include	<sys/time.h>
#include	<sys/file.h>
#include	<net/if.h>
#include	<netinet/in.h>

#include	<stdio.h>
#include	<netdb.h>
#include	<time.h>
#include	<errno.h>
#include	<signal.h>
#include	<syslog.h>

#include	"radius.h"

extern char     port_msg[128];
extern int      ack_nak_flag;
extern char     send_buffer[4096];
extern UINT4    expiration_seconds;
extern UINT4    warning_seconds;
extern int      debug_flag;

static int      set_expiration PROTO((VALUE_PAIR *, UINT4));

static int 	rad_passchange PROTO((AUTH_REQ *, int, char *));

static AATV     passwd_aatv =
 {"PASSWD", -1, AA_DIRECT, NULL, NULL, rad_passchange, NULL, NULL, 0};

AATVPTR         rad_passwd_aatv = & passwd_aatv;

/*************************************************************************
 *
 *	Function: rad_passchange
 *
 *	Purpose: Change a users password
 *
 *************************************************************************/

static int
rad_passchange (authreq, value, afpar)

AUTH_REQ       *authreq;
int             value;
char           *afpar;

{
	int             result;
	VALUE_PAIR     *namepair;
	VALUE_PAIR     *check_item;
	VALUE_PAIR     *newpasspair;
	VALUE_PAIR     *oldpasspair;
	VALUE_PAIR     *curpass;
	VALUE_PAIR     *user_check;
	VALUE_PAIR     *user_reply;
	char            pw_digest[AUTH_VECTOR_LEN];
	char            string[64];
	char            passbuf[AUTH_PASS_LEN];
	int             i;
	int             secretlen;
	static char    *func = "rad_passchange";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	/* Get the username */
	namepair = authreq->request;

	while (namepair != (VALUE_PAIR *) NULL)
	{
		if (namepair->attribute == PW_USER_NAME)
		{
			break;
		}
		namepair = namepair->next;
	}
	if (namepair == (VALUE_PAIR *) NULL)
	{
		logit (LOG_DAEMON, LOG_ERR,
			"%s: from %s - No User name supplied",
			func, ip_hostname (authreq->ipaddr));
		return EV_NAK;
	}

	/*
	 * Look the user up in the database
	 */
	if (user_find (NULL, namepair->strvalue, 0, &user_check, &user_reply, 0)
									!= 0)
	{
		logit (LOG_DAEMON, LOG_ERR,
			"%s: from %s - Invalid User: %s", func,
			ip_hostname (authreq->ipaddr), namepair->strvalue);
		return EV_NAK;
	}

	/*
	 * Validate the user -
	 * 
	 * We have to unwrap this in a special way to decrypt the old and new
	 * passwords.  The MD5 calculation is based on the old password.  The
	 * vector is different.  The old password is encrypted using the
	 * encrypted new password as its vector.  The new password is
	 * encrypted using the random encryption vector in the request
	 * header.
	 */

	/* Extract the attr-value pairs for the old and new passwords */
	check_item = authreq->request;
	while (check_item != (VALUE_PAIR *) NULL)
	{
		if (check_item->attribute == CI_USER_PASSWORD)
		{
			newpasspair = check_item;
		}
		else if (check_item->attribute == PW_OLD_PASSWORD)
		{
			oldpasspair = check_item;
		}
		check_item = check_item->next;
	}

	/* Verify that both encrypted passwords were supplied */
	if (newpasspair == (VALUE_PAIR *) NULL ||
			oldpasspair == (VALUE_PAIR *) NULL)
	{
		/* Missing one of the passwords */
		logit (LOG_DAEMON, LOG_ERR,
			"%s: from %s - Missing Password: %s", func,
			ip_hostname (authreq->ipaddr), namepair->strvalue);
		list_free (user_check);
		list_free (user_reply);
		return EV_NAK;
	}

	/* Get the current password from the database */
	curpass = user_check;
	while (curpass != (VALUE_PAIR *) NULL)
	{
		if (curpass->attribute == CI_USER_PASSWORD)
		{
			break;
		}
		curpass = curpass->next;
	}
	if ((curpass == (VALUE_PAIR *) NULL) ||
		(curpass->strvalue == (char *) NULL))
	{
		/* Missing our local copy of the password */
		logit (LOG_DAEMON, LOG_ERR,
			"%s: from %s - Missing Local Password: %s", func,
			ip_hostname (authreq->ipaddr), namepair->strvalue);
		list_free (user_check);
		list_free (user_reply);
		return EV_NAK;
	}
	if (strcmp (curpass->strvalue, "UNIX"))
	{
		/* Can't change passwords that aren't in users file */
		logit (LOG_DAEMON, LOG_ERR,
			"%s: from %s: system password change not allowed: %s\n",
			func, ip_hostname (authreq->ipaddr),
			namepair->strvalue);
		list_free (user_check);
		list_free (user_reply);
		return EV_NAK;
	}

	/* Decrypt the old password */
	secretlen = strlen (curpass->strvalue);
	memcpy (string, curpass->strvalue, secretlen);
	memcpy (string + secretlen, newpasspair->strvalue, AUTH_VECTOR_LEN);
	md5_calc (pw_digest, string, AUTH_VECTOR_LEN + secretlen);
	memcpy ((char *) passbuf, oldpasspair->strvalue, AUTH_PASS_LEN);
	for (i = 0; i < AUTH_PASS_LEN; i++)
	{
		passbuf[i] ^= pw_digest[i];
	}

	/* Did they supply the correct password ??? */
	if (strncmp (passbuf, curpass->strvalue, AUTH_PASS_LEN) != 0)
	{
		logit (LOG_DAEMON, LOG_ERR,
			"%s: from %s - Incorrect Password: %s", func,
			ip_hostname (authreq->ipaddr), namepair->strvalue);
		list_free (user_check);
		list_free (user_reply);
		return EV_NAK;
	}

	/* Decrypt the new password */
	memcpy (string, curpass->strvalue, secretlen);
	memcpy (string + secretlen, (char *) authreq->vector, AUTH_VECTOR_LEN);
	md5_calc (pw_digest, string, AUTH_VECTOR_LEN + secretlen);
	memcpy ((char *) passbuf, newpasspair->strvalue, AUTH_PASS_LEN);
	for (i = 0; i < AUTH_PASS_LEN; i++)
	{
		passbuf[i] ^= pw_digest[i];
	}

	/* Update the users password */
	strncpy (curpass->strvalue, passbuf, AUTH_PASS_LEN);

	/* Add a new expiration date if we are aging passwords */
	if (expiration_seconds != (UINT4) 0)
	{
		set_expiration (user_check, expiration_seconds);
	}

	/* Update the database */
	if (user_update (namepair->strvalue, user_check, user_reply) != 0)
	{
		result = EV_NAK;
	}
	else
	{
		result = EV_ACK;
	}
	list_free (user_check);
	list_free (user_reply);
	return result;
} /* end of rad_passchange () */

/*************************************************************************
 *
 *	Function: set_expiration
 *
 *	Purpose: Set the new expiration time by updating or adding
 *		 the Expiration attribute-value pair.
 *
 *************************************************************************/

static int
set_expiration (user_check, expiration)

VALUE_PAIR     *user_check;
UINT4           expiration;

{
	VALUE_PAIR     *exppair;
	VALUE_PAIR     *prev;
	struct timeval  tp;
	struct timezone tzp;
	static char    *func = "set_expiration";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if (user_check == (VALUE_PAIR *) NULL)
	{
		return (-1);
	}

	/* Look for an existing expiration entry */
	exppair = user_check;
	prev = (VALUE_PAIR *) NULL;
	while (exppair != (VALUE_PAIR *) NULL)
	{
		if (exppair->attribute == CI_EXPIRATION)
		{
			break;
		}
		prev = exppair;
		exppair = exppair->next;
	}
	if (exppair == (VALUE_PAIR *) NULL)
	{
		/* Add a new attr-value pair */
		if ((exppair = (VALUE_PAIR *) malloc (sizeof (VALUE_PAIR))) ==
				(VALUE_PAIR *) NULL)
		{
			logit (LOG_DAEMON, LOG_ALERT,
				"%s: FATAL out of memory", func);
			abort ();
		}
		/* Initialize it */
		strcpy (exppair->name, "Expiration");
		exppair->attribute = CI_EXPIRATION;
		exppair->type = PW_TYPE_DATE;
		*exppair->strvalue = '\0';
		exppair->lvalue = (UINT4) 0;
		exppair->next = (VALUE_PAIR *) NULL;

		/* Attach it to the list. */
		prev->next = exppair;
	}

	/* calculate a new expiration */
	gettimeofday (&tp, &tzp);
	exppair->lvalue = tp.tv_sec + expiration;
	return (0);
} /* end of set_expiration () */

/*************************************************************************
 *
 *	Function: pw_expired
 *
 *	Purpose: Tests to see if the user's password has expired.
 *
 *	Return: Number of days before expiration if a warning is required
 *		otherwise zero for success and -1 for failure.
 *
 *************************************************************************/

int
pw_expired (exptime)

UINT4           exptime;

{
	struct timeval  tp;
	struct timezone tzp;
	UINT4           exp_remain;
	int             exp_remain_int;

	if (expiration_seconds == (UINT4) 0)
	{
		return -1;
	}

	gettimeofday (&tp, &tzp);
	if (tp.tv_sec > exptime)
	{
		return -1;
	}

	if (warning_seconds != (UINT4) 0)
	{
		if (tp.tv_sec > exptime - warning_seconds)
		{
			exp_remain = exptime - tp.tv_sec;
			exp_remain /= (UINT4) SECONDS_PER_DAY;
			exp_remain_int = exp_remain;
			return (exp_remain_int);
		}
	}
	return 0;
} /* end of pw_expired () */
