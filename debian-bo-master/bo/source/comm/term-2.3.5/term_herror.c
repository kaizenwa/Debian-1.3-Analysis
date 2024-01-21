/* 
From metcalf@catfish.LCS.MIT.EDU  Mon Jun 20 10:11:21 1994
Received: from catfish.LCS.MIT.EDU (root@localhost)
	by bohr.physics.purdue.edu (8.6.8.1/3.0-LD) with ESMTP
	id KAA23719; Mon, 20 Jun 1994 10:11:20 -0500
Received: (from metcalf@localhost) by catfish.LCS.MIT.EDU (8.6.9/8.6.9) id LAA05423 for bcr@physics.purdue.edu; Mon, 20 Jun 1994 11:10:55 -0400
Date: Mon, 20 Jun 1994 11:10:55 -0400
From: Chris Metcalf <metcalf@catfish.LCS.MIT.EDU>
Message-Id: <199406201510.LAA05423@catfish.LCS.MIT.EDU>
To: bcr@physics.purdue.edu
Subject: private : figured out "Term: gethostbyname: Error 0" msg

The right thing to do is call herror() instead of perror() after a failed
get*by*() call.  Not all systems have it though (e.g. SunOS 4.x and 5.x
and NeXT 2.1 don't; Linux, Ultrix, and HCX/UX all do).  You might try
using the following definition and then using herror() instead.
*/

#define I_STRING
#include "includes.h"


char *term_strherror(int err)
{
	static char string[80];
	switch (err) {
	case 0:
		strcpy(string, "Hostname lookup failed; no error reported");
		break;
#ifdef HOST_NOT_FOUND
	case HOST_NOT_FOUND:
		strcpy(string, "Host not found");
		break;
#endif
#ifdef TRY_AGAIN
	case TRY_AGAIN:
		strcpy(string,
		    "Non-authoritative `host not found', or server failed");
		break;
#endif
#ifdef NO_RECOVERY
	case NO_RECOVERY:
		strcpy(string,
		    "Non-recoverable hostname lookup error");
		break;
#endif
#ifdef NO_DATA
	case NO_DATA:
		strcpy(string,
		    "Valid name, no data record of requested type");
		break;
#endif
	default:
		sprintf(string, "Hostname lookup error %d", err);
		break;
	}
	return string;
}

/*
 * Define herror() for systems known to need it.  Those include
 * SunOS 4.x, 5.x; NeXT 2.1.  Systems known not to need it include
 * Linux 1.x, Ultrix 4.4, and HCX/UX 5.1.
 *
 * Provision is made for pre-DNS systems as well.
 */

extern int h_errno;

void term_herror(char *msg)
{
	fprintf(stderr, "%s: %s\n", msg, term_strherror(h_errno));
}

