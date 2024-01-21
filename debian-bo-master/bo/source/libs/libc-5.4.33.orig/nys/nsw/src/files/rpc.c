/*
** rpc.c                           /etc/rpc access functions
**
** Copyright (C) 1993 Signum Support AB
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
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>


#define MAXALIASES 35

static FILE *rfp = NULL;
static struct rpcent rpcb;
static char *rpc_aliases[MAXALIASES];
static char rpcbuf[BUFSIZ+1];
static int stayopen = 0;


void _setrpcent(int f)
{
    if (rfp == NULL)
	rfp = fopen(_PATH_RPC, "r");
    else
	rewind(rfp);

    stayopen |= f;
}

void _endrpcent(void)
{
    if (rfp && !stayopen)
    {
	fclose(rfp);
	rfp = NULL;
    }
}


struct rpcent *_getrpcent(void)
{
    char *p;
    register char *cp, **q;

    
    if (rfp == NULL)
	_setrpcent(0);
    
    if (rfp == NULL)
	return NULL;

  Again:
    if ((p = fgets(rpcbuf, BUFSIZ, rfp)) == NULL)
	return (NULL);

    if (*p == '#')
	goto Again;
    
    cp = strpbrk(p, "#\n");
    if (cp == NULL)
	goto Again;
    *cp = '\0';
    
    cp = strpbrk(p, " \t");
    if (cp == NULL)
	goto Again;
    
    *cp++ = '\0';

    rpcb.r_name = p;
    
    while (*cp == ' ' || *cp == '\t')
	cp++;
    
    p = cp;
    
    cp = strpbrk(cp, " \t");
    if (cp != NULL) 
	*cp++ = '\0';

    rpcb.r_number = atoi(p);

    q = rpcb.r_aliases = rpc_aliases;
    
    while (cp && *cp)
    {
	if (*cp == ' ' || *cp == '\t')
	{
	    cp++;
	    continue;
	}
	
	if (q < &rpc_aliases[MAXALIASES - 1])
	    *q++ = cp;
	
	cp = strpbrk(cp, " \t");
	if (cp != NULL)
	    *cp++ = '\0';
    }
    
    *q = NULL;
    
    return (&rpcb);
}


struct rpcent *_getrpcbyname(const char *name)
{
    struct rpcent *rp;
    char **cp;
	

    _setrpcent(0);
    while ((rp = _getrpcent()) != NULL)
    {
	if (strcasecmp(rp->r_name, name) == 0)
	    break;

	for (cp = rp->r_aliases; cp && *cp; cp++)
	    if (strcasecmp(*cp, name) == 0)
		goto End;
    }

  End:

    _endrpcent();

    return rp;
}


struct rpcent *_getrpcbynumber(int number)
{
    struct rpcent *rp;

    
    _setrpcent(0);

    while ((rp = _getrpcent()) != NULL)
	if (rp->r_number == number)
	    break;

    _endrpcent();
    
    return rp;
}

