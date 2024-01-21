/*
 * Copyright 1994, John F. Haugh II
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by John F. Haugh, II
 *      and other contributors.
 * 4. Neither the name of John F. Haugh, II nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY JOHN HAUGH AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL JOHN HAUGH OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <config.h>

#include "rcsid.h"
RCSID("$Id: expiry.c,v 1.1.1.1 1996/08/10 07:59:52 marekm Exp $")

#include <sys/types.h>
#include <signal.h>
#include <stdio.h>
#include "prototypes.h"
#include "defines.h"
#include <pwd.h>

#ifndef	AGING
#if defined(HAVE_USERSEC_H) || defined(SHADOWPWD)
#define	AGING	1
#endif
#endif

#if !defined(SHADOWPWD) && !defined(AGING) /*{*/

/*
 * Not much to do here ...
 */

int
main ()
{
	exit (0);
}

#else	/*} AGING || SHADOWPWD {*/

/*
 * catch - signal catcher
 */

static RETSIGTYPE
catch(sig)
	int sig;
{
	exit (10);
}

/*
 * usage - print syntax message and exit
 */

static void
usage()
{
	fprintf (stderr, "Usage: expiry { -f | -c }\n");
	exit (10);
}

/* 
 * expiry - check and enforce password expiration policy
 *
 *	expiry checks (-c) the current password expiraction and
 *	forces (-f) changes when required.  It is callable as a
 *	normal user command.
 */

int
main(argc, argv)
	int argc;
	char **argv;
{
	struct	passwd	*pwd;
#ifdef	SHADOWPWD
	struct	spwd	*spwd;
#endif

	/* 
	 * Start by disabling all of the keyboard signals.
	 */

	signal (SIGHUP, catch);
	signal (SIGINT, catch);
	signal (SIGQUIT, catch);
#ifdef	SIGTSTP
	signal (SIGTSTP, catch);
#endif

	/*
	 * expiry takes one of two arguments.  The default action
	 * is to give the usage message.
	 */

	if (argc != 2 || (strcmp (argv[1], "-f")  && strcmp (argv[1], "-c")))
		usage ();

	/*
	 * Make sure I am root.  Can't open /etc/shadow without root
	 * authority.
	 */

	if (geteuid () != 0) {
		fprintf (stderr, "%s: WARNING!  Must be set-UID root!\n",
			argv[0]);
		exit (10);
	}

	/*
	 * Get user entries for /etc/passwd and /etc/shadow
	 */

	if (! (pwd = getpwuid(getuid ()))) {
		fprintf (stderr, "unknown user\n");
		exit(10);
	}
#ifdef	SHADOWPWD
	spwd = getspnam(pwd->pw_name);
#endif

	/*
	 * If checking accounts, use agecheck() function.
	 */

	if (strcmp (argv[1], "-c") == 0) {

		/*
		 * Print out number of days until expiration.
		 */

#ifdef	SHADOWPWD
		agecheck (pwd, spwd);
#else
		agecheck (pwd);
#endif

		/*
		 * Exit with status indicating state of account --
		 */

#ifdef	SHADOWPWD
		exit (isexpired (pwd, spwd));
#else
		exit (isexpired (pwd));
#endif
	}

	/*
	 * If forcing password change, use expire() function.
	 */

	if (strcmp (argv[1], "-f") == 0) {

		/*
		 * Just call expire().  It will force the change
		 * or give a message indicating what to do.  And
		 * it doesn't return at all unless the account
		 * is unexpired.
		 */

#ifdef	SHADOWPWD
		expire (pwd, spwd);
#else
		expire (pwd);
#endif
		exit (0);
	}

	/*
	 * Can't get here ...
	 */

	usage ();
	exit (1);
}
#endif	/*}*/
