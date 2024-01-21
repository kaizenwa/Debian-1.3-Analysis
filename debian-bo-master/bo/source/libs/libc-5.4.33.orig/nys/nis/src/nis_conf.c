/*
** nis_conf.c                Config file parsing routines for NIS+
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

#include "config.h"

#ifdef ENABLE_NISEMU


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <rpc/rpc.h>
#include "rpcsvc/nis.h"
#include "nis_conf.h"

struct nis_conf *_nis_config = NULL;


int nisconf_set(void)
{
    if (_nis_config != NULL)
	nisconf_end();

    _nis_config = nisconf_read(PATH_NISCONF);
    return (_nis_config == NULL) ? -1 : 0;
}


void nisconf_end(void)
{
    nisconf_free(_nis_config);
}


void nisconf_free(struct nis_conf *ncp)
{
    int i;
    
    if (ncp == NULL)
	return;

    if (ncp->domainname)
	free(ncp->domainname);
    
    for (i = 0; i < ncp->servers; i++)
    {
	free(ncp->server[i].address);
	if (ncp->server[i].domain)
	    free(ncp->server[i].domain);
    }

    free(ncp);
    ncp = NULL;
}


struct nis_conf *nisconf_read(char *path)
{
    struct nis_conf *ncp;
    FILE *fp;
    char buf[1024];
    char *cp, *tmp, *tmp2;

    
    ncp = malloc(sizeof(struct nis_conf));
    if (ncp == NULL)
	return NULL;

    ncp->domainname = NULL;
    ncp->servers = 0;
    ncp->cserver = 0;
    ncp->timeout = -1;
    ncp->retries = -1;

    fp = fopen(path, "r");
    if (fp == NULL)
    {
	nisconf_free(ncp);
	return NULL;
    }

    while ((cp = fgets(buf, sizeof(buf), fp)) != NULL)
    {
	tmp = strchr(cp, '#');
	if (tmp)
	    *tmp = '\0';
	
	while (isspace(*cp))
	    cp++;
	if (*cp == '\0')
	    continue;

	tmp = cp;
	while (!isspace(*cp))
	    cp++;
	if (*cp != '\0')
	    *cp++ = '\0';

	while (isspace(*cp))
	    cp++;

	if (*cp == '\0')
	    tmp2 = NULL;
	else
	{
	    tmp2 = cp;
	    while (*cp && !isspace(*cp))
		cp++;
	    if (*cp != '\0')
		*cp++ = '\0';
	}

	while (isspace(*cp))
	    cp++;
	
	if (strcmp(tmp, "domainname") == 0)
	{
	    ncp->domainname = tmp2 ? strdup(tmp2) : NULL;
	}
	else if (strcmp(tmp, "nisserver") == 0)
	{
	    if (tmp2 && ncp->servers < NISCONF_MAXSERVERS)
	    {
		ncp->server[ncp->servers].address = strdup(tmp2);
		
		if (*cp)
		    ncp->server[ncp->servers].domain = strdup(cp);
		else
		    ncp->server[ncp->servers].domain = NULL;
		
		ncp->server[ncp->servers].client = NULL;
		ncp->servers++;
	    }
	}
	else if (strcmp(tmp, "timeout") == 0)
	{
	    ncp->timeout = atoi(tmp2);
	}
	else if (strcmp(tmp, "retry") == 0)
	{
	    ncp->retries = atoi(tmp2);
	}

	/* Everything else is ignored */
    }

    fclose(fp);
    return ncp;
}



char *nisconf_domain2address(char *domain, int *idx)
{
    int i;

    
    if (idx == NULL)
    {
	idx = &i;
	i = 0;
    }
    
    while (*idx < _nis_config->servers)
    {
	/* A server that handles all domains */
	if (_nis_config->server[*idx].domain == NULL)
	    return _nis_config->server[(*idx)++].address;

	/* Else look for a specific server */
	else if (domain &&
		 strcmp(domain, _nis_config->server[*idx].domain) == 0)
	    
	    return _nis_config->server[(*idx)++].address;

	(*idx)++;
    }

    return NULL;
}


CLIENT *nisconf_domain2client(char *domain, int *idx, char **address,
			      int tcp_flag)
{
    int i;
    CLIENT *clh;

    
    if (_nis_config->servers <= 0)
	return NULL;

    if (idx == NULL)
    {
	idx = &i;
	i = 0;
    }

    while (*idx < _nis_config->servers)
    {
	if (_nis_config->server[*idx].domain == NULL ||
	    (domain &&
	     strcmp(domain, _nis_config->server[*idx].domain) == 0))
	{
	    if (tcp_flag)
	    {
		clh = clnt_create(_nis_config->server[*idx].address,
				  NIS_PROG, NIS_VERSION, "tcp");
		if (!clh)
		    goto next;
		
		clh->cl_auth = authunix_create_default();
		if (address)
		    *address = _nis_config->server[*idx].address;
		
		return clh;
	    }

	    if (_nis_config->server[*idx].client == NULL)
	    {
		clh = clnt_create(_nis_config->server[*idx].address,
				  NIS_PROG, NIS_VERSION, "udp");
		if (!clh)
		    goto next;
		
		clh->cl_auth = authunix_create_default();
		
		_nis_config->server[*idx].client = clh;
	    }

	    if (address)
		*address = _nis_config->server[*idx].address;

	    return _nis_config->server[(*idx)++].client;
	}
    
      next:
	(*idx)++;
    }
    
    return NULL;
}

#endif /* ENABLE_NISEMU */
