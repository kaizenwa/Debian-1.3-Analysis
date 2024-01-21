/*
** hosts.c                           /etc/hosts access functions
**
** This file is part of the NYS Library.
**
** The NYS Library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS Library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
** 
** You should have received a copy of the GNU Library General Public
** License along with the NYS Library; see the file COPYING.LIB.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
**
**
** Copyright (c) 1985, 1988 Regents of the University of California.
** All rights reserved.
** 
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions
** are met:
** 1. Redistributions of source code must retain the above copyright
**    notice, this list of conditions and the following disclaimer.
** 2. Redistributions in binary form must reproduce the above copyright
**    notice, this list of conditions and the following disclaimer in the
**    documentation and/or other materials provided with the distribution.
** 3. All advertising materials mentioning features or use of this software
**    must display the following acknowledgement:
** 	This product includes software developed by the University of
** 	California, Berkeley and its contributors.
** 4. Neither the name of the University nor the names of its contributors
**    may be used to endorse or promote products derived from this software
**    without specific prior written permission.
** 
** THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
** ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
** IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
** ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
** FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
** DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
** OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
** HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
** LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
** OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
** SUCH DAMAGE.
** -
** Portions Copyright (c) 1993 by Digital Equipment Corporation.
** 
** Permission to use, copy, modify, and distribute this software for any
** purpose with or without fee is hereby granted, provided that the above
** copyright notice and this permission notice appear in all copies, and that
** the name of Digital Equipment Corporation not be used in advertising or
** publicity pertaining to distribution of the document or software without
** specific, written prior permission.
** 
** THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
** WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES
** OF MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
** CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
** DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
** PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
** ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
** SOFTWARE.
** Author: Peter Eriksson <pen@signum.se>
*/

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <arpa/nameser.h>

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <netdb.h>
#include <resolv.h>

#define MAXALIASES 35
#define MAXADDRS   35

static char *h_addr_ptrs[MAXADDRS+1];

static struct hostent host;
static char *host_aliases[MAXALIASES];
static char hostbuf[BUFSIZ+1];
static  struct in_addr host_addr;
static FILE *hostf = NULL;
static int stayopen = 0;

void _sethostent(int f)
{
    if (hostf == NULL)
	hostf = fopen(_PATH_HOSTS, "r");
    else
	rewind(hostf);

    stayopen |= f;
}

void _endhostent(void)
{
    if (hostf && !stayopen)
    {
	fclose(hostf);
	hostf = NULL;
    }
}



struct hostent *_gethostent(void)
{
    char *p;
    register char *cp, **q;

    
    if (hostf == NULL)
	_sethostent(0);
    
    if (hostf == NULL) {
	h_errno = NETDB_INTERNAL;
	return (NULL);
    }

again:
	if (!(p = fgets(hostbuf, sizeof hostbuf, hostf))) {
		h_errno = HOST_NOT_FOUND;
		return (NULL);
	}
	if (*p == '#')
		goto again;
	if (!(cp = strpbrk(p, "#\n")))
		goto again;
	*cp = '\0';
	if (!(cp = strpbrk(p, " \t")))
		goto again;
	*cp++ = '\0';
	/* THIS STUFF IS INTERNET SPECIFIC */
	if (!inet_aton(p, &host_addr))
		goto again;
	h_addr_ptrs[0] = (char *)&host_addr;
	h_addr_ptrs[1] = NULL;
#if BSD >= 43 || defined(h_addr)	/* new-style hostent structure */
	host.h_addr_list = h_addr_ptrs;
#else
	host.h_addr = h_addr_ptrs[0];
#endif
	host.h_length = INT32SZ;
	host.h_addrtype = AF_INET;
	while (*cp == ' ' || *cp == '\t')
		cp++;
	host.h_name = cp;
	q = host.h_aliases = host_aliases;
	cp = strpbrk(cp, " \t");
	if (cp)
		*cp++ = '\0';
	while (cp && *cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &host_aliases[MAXALIASES - 1])
			*q++ = cp;
		cp = strpbrk(cp, " \t");
		if (cp)
			*cp++ = '\0';
	}
	*q = NULL;
	h_errno = NETDB_SUCCESS;
	return (&host);
}

struct hostent *_gethostbyname(const char *name)
{
    struct hostent *hp;
    register char *cp, **acp;

	/*
	 * if there aren't any dots, it could be a user-level alias.
	 * this is also done in res_query() since we are not the only
	 * function that looks up host names.
	 */
	if (!strchr(name, '.') && (cp = __hostalias(name)))
		name = cp;

	/*
	 * disallow names consisting only of digits/dots, unless
	 * they end in a dot.
	 */
	if (isdigit(name[0]))
		for (cp = name; ; ++cp) {
			if (!*cp) {
				if (*--cp == '.')
					break;
				/*
				 * All-numeric, no dot at the end.
				 * Fake up a hostent as if we'd actually
				 * done a lookup.
				 */
				if (!inet_aton(name, &host_addr)) {
					h_errno = HOST_NOT_FOUND;
					return (NULL);
				}
				host.h_name = (char *)name;
				host.h_aliases = host_aliases;
				host_aliases[0] = NULL;
				host.h_addrtype = AF_INET;
				host.h_length = INT32SZ;
				h_addr_ptrs[0] = (char *)&host_addr;
				h_addr_ptrs[1] = NULL;
#if BSD >= 43 || defined(h_addr)	/* new-style hostent structure */
				host.h_addr_list = h_addr_ptrs;
#else
				host.h_addr = h_addr_ptrs[0];
#endif
				h_errno = NETDB_SUCCESS;
				return (&host);
			}
			if (!isdigit(*cp) && *cp != '.') 
				break;
		}

    _sethostent(0);
    while ((hp = _gethostent()) != NULL)
    {
	if (strcasecmp(hp->h_name, name) == 0)
	    break;

	for (acp = hp->h_aliases; acp && *acp; acp++)
	    if (strcasecmp(*acp, name) == 0)
		goto End;
    }

  End:

    _endhostent();

    return hp;
}


struct hostent *_gethostbyaddr(const char *addr, int alen, int type)
{
    struct hostent *hp;

    
    _sethostent(0);

    while ((hp = _gethostent()) != NULL)
	if (hp->h_addrtype == type && !memcmp(hp->h_addr, addr, alen))
	    break;

    _endhostent();
    
    return hp;
}
