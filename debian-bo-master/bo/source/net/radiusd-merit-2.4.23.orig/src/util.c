/*
 *
 *	RADIUS   Remote Authentication Dial In User Service
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
 * [C] The Regents of the University of Michigan and Merit Network, Inc. 1992,
 * 1993, 1994, 1995, 1996 All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice and this permission notice appear in all
 * copies of the software and derivative works or modified versions thereof,
 * and that both the copyright notice and this permission and disclaimer
 * notice appear in supporting documentation.
 *
 * THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE REGENTS OF THE
 * UNIVERSITY OF MICHIGAN AND MERIT NETWORK, INC. DO NOT WARRANT THAT THE
 * FUNCTIONS CONTAINED IN THE SOFTWARE WILL MEET LICENSEE'S REQUIREMENTS OR
 * THAT OPERATION WILL BE UNINTERRUPTED OR ERROR FREE.  The Regents of the
 * University of Michigan and Merit Network, Inc. shall not be liable for any
 * special, indirect, incidental or consequential damages with respect to any
 * claim by Licensee or any third party arising from use of the software.
 *
 *	Public entry points in this file:
 *
 *	get_ipaddr
 *	good_ipaddr
 *	list_free
 *
 */

static char     sccsid[] =
		"@(#)util.c 1.1 Copyright 1992 Livingston Enterprises Inc";

static char     rcsid[] = "$Id: util.c,v 1.9 1996/05/21 15:16:01 web Exp $";

#include	<sys/types.h>
#include	<sys/socket.h>
#include	<sys/time.h>
#include	<netinet/in.h>
#include	<arpa/inet.h>

#include	<stdio.h>
#include	<netdb.h>
#include	<ctype.h>
#include	<time.h>

#include	"radius.h"

/*************************************************************************
 *
 *	Function: get_ipaddr
 *
 *	Purpose: Return an IP address in host long notation from a host
 *		 name or address in dot notation.
 *
 *************************************************************************/

UINT4
get_ipaddr (host)

char           *host;

{
	struct hostent *hp;

	if (good_ipaddr (host) == 0)
	{
		return ntohl(inet_addr (host));
	}
	else if ((hp = gethostbyname (host)) == (struct hostent *) NULL)
	{
		return ((UINT4) 0);
	}
	return ntohl((*(UINT4 *) hp->h_addr));
} /* end of get_ipaddr () */

/*************************************************************************
 *
 *	Function: good_ipaddr
 *
 *	Purpose: Check for valid IP address in standard dot notation.
 *
 *************************************************************************/

int
good_ipaddr (addr)

char           *addr;

{
	int             dot_count;
	int             digit_count;

	if (addr == (char *) NULL)
	{
		return (-1);
	}

	dot_count = 0;
	digit_count = 0;

	while (*addr != '\0' && *addr != ' ')
	{
		if (*addr == '.')
		{
			dot_count++;
			digit_count = 0;
		}
		else if (!isdigit (*addr))
		{
			dot_count = 5;
		}
		else
		{
			digit_count++;
			if (digit_count > 3)
			{
				dot_count = 5;
			}
		}
		addr++;
	}
	if (dot_count != 3)
	{
		return (-1);
	}
	else
	{
		return (0);
	}
} /* end of good_ipaddr () */

/*************************************************************************
 *
 *	Function: list_free
 *
 *	Purpose: Release the memory used by a list of a/v pairs.
 *
 *************************************************************************/

void
list_free (pair)

VALUE_PAIR     *pair;

{
	VALUE_PAIR     *next;

	while (pair != (VALUE_PAIR *) NULL)
	{
		next = pair->next;
		free (pair);
		pair = next;
	}
	return;
} /* end of list_free () */
