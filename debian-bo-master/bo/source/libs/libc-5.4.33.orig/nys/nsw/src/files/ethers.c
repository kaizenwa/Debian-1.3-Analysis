/*
** ethers.c                           /etc/ethers access functions
**
** Copyright (c) 1993 Signum Support AB, Sweden
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
** Author: Peter Eriksson <pen@signum.se>
*/

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "ethers.h"
#include "misc.h"

static FILE *efp = NULL;


void _setethent(void)
{
    if (efp)
	rewind(efp);
    else
	efp = fopen(_PATH_ETHERS, "r");
}

void _endethent(void)
{
    if (efp)
	fclose(efp);
    efp = NULL;
}

struct ether *_getethent(void)
{
    static struct ether eb;
    static char *buf = NULL;
    static int size = 0;

    
    if (efp == NULL)
	_setethent();
    
    if (efp == NULL)
	return NULL;
    
    while (_nsw_getline(&buf, &size, efp) >= 0)
    {
	if (ether_line(buf, &eb.addr, eb.name) == 0)
	{
	    return &eb;
	}
    }
    
    return NULL;
}

struct ether *_getethbyname(const char *name)
{
    struct ether *ep;
    

    _setethent();

    while ((ep = _getethent()) && strcmp(name, ep->name) != 0)
	;

    _endethent();

    if (ep == NULL)
	errno = 0;
    
    return ep;
}

struct ether *_getethbyaddr(const struct ether_addr *addr)
{
    struct ether *ep;
    

    _setethent();

    while ((ep = _getethent()) &&
	   memcmp(addr, &ep->addr, sizeof(struct ether_addr)) != 0)
	;

    _endethent();

    if (ep == NULL)
	errno = 0;
    
    return ep;
}


char *ether_ntoa(const struct ether_addr *addr)
{
    static char buf[18];

    sprintf(buf, "%02x:%02x:%02x:%02x:%02x:%02x",
	    addr->ether_addr_octet[0],
	    addr->ether_addr_octet[1],
	    addr->ether_addr_octet[2],
	    addr->ether_addr_octet[3],
	    addr->ether_addr_octet[4],
	    addr->ether_addr_octet[5]);

    return buf;
}


struct ether_addr * ether_aton(const char *addr)
{
    static struct ether_addr eb;
    int a0, a1, a2, a3, a4, a5;

    if (sscanf(addr, "%x:%x:%x:%x:%x:%x", &a0, &a1, &a2, &a3, &a4, &a5) != 6)
	return NULL;

    eb.ether_addr_octet[0] = a0;
    eb.ether_addr_octet[1] = a1;
    eb.ether_addr_octet[2] = a2;
    eb.ether_addr_octet[3] = a3;
    eb.ether_addr_octet[4] = a4;
    eb.ether_addr_octet[5] = a5;

    return &eb;
}


int ether_ntohost(char *hostname, const struct ether_addr *addr)
{
    struct ether *ep;

    ep = getethbyaddr(addr);
    if (ep == NULL)
	return -1;

    strcpy(hostname, ep->name);
    return 0;
}


int ether_hostton(const char *hostname, struct ether_addr *addr)
{
    struct ether *ep;

    ep = getethbyname(hostname);
    if (ep == NULL)
	return -1;

    *addr = ep->addr;
    return 0;
}


int ether_line(char *line, struct ether_addr *addr, char *hostname)
{
    char *cp;
    struct ether_addr *eap;


    cp = strchr(line, '#');
    if (cp)
	*cp = '\0';

    cp = strtok(line, " \t\n");
    if (cp == NULL)
	return -1;

    eap = ether_aton(cp);
    if (eap == NULL)
	return 0;

    *addr = *eap;
    
    cp = strtok(NULL, " \t\n");
    if (cp == NULL)
	return -1;

    strcpy(hostname, cp);

    return 0;
}

