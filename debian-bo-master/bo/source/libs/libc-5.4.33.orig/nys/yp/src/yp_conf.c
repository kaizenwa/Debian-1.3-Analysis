/*
** yp_conf.c                Config file parsing routines for YP (NIS)
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

#ifdef ENABLE_YPEMU


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <rpc/rpc.h>
#include <rpcsvc/yp.h>
#include "yp_conf.h"

struct yp_conf *_yp_config = NULL;


int ypconf_set(void)
{
    if (_yp_config != NULL)
	ypconf_end();

    _yp_config = ypconf_read(PATH_YPCONF);
    return (_yp_config == NULL) ? -1 : 0;
}


void ypconf_end(void)
{
    ypconf_free(_yp_config);
}


void ypconf_free(struct yp_conf *ncp)
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
	if (ncp->server[i].client)
	    clnt_destroy(ncp->server[i].client);
    }

    free(ncp);
    ncp = NULL;
}


struct yp_conf *ypconf_read(char *path)
{
    struct yp_conf *ncp;
    FILE *fp;
    char buf[1024];
    char *cp, *tmp, *tmp2, *tmp3;

    
    ncp = malloc(sizeof(struct yp_conf));
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
	ypconf_free(ncp);
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

	/* Get first entry */
	tmp = cp;
	while (!isspace(*cp))
	    cp++;
	if (*cp != '\0')
	    *cp++ = '\0';

	while (isspace(*cp))
	    cp++;

	/* Get second entry */
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
	
	/* And the third */
	if (*cp == '\0')
	    tmp3 = NULL;
	else
	{
	    tmp3 = cp;
	    while (*cp && !isspace(*cp))
		cp++;
	    if (*cp != '\0')
		*cp++ = '\0';
	}

	if (strcmp(tmp, "domainname") == 0)
	{
	    ncp->domainname = tmp2 ? strdup(tmp2) : NULL;
	}
	else if (strcmp(tmp, "ypserver") == 0)
	{
	    if (tmp2 && ncp->servers < YPCONF_MAXSERVERS)
	    {
		ncp->server[ncp->servers].address = strdup(tmp2);
		
		if (tmp3)
		    ncp->server[ncp->servers].domain = strdup(tmp3);
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



char *ypconf_domain2address(const char *domain, int *idx)
{
    int i;

    
    if (idx == NULL)
    {
	idx = &i;
	i = 0;
    }
    
    while (*idx < _yp_config->servers)
    {
	/* A server that handles all domains */
	if (_yp_config->server[*idx].domain == NULL)
	    return _yp_config->server[(*idx)++].address;

	/* Else look for a specific server */
	else if (domain &&
		 strcmp(domain, _yp_config->server[*idx].domain) == 0)
	    
	    return _yp_config->server[(*idx)++].address;

	(*idx)++;
    }

    return NULL;
}


CLIENT *ypconf_domain2client(const char *domain,
			     int *idx,
			     char **address)
{
    int i;
    CLIENT *clh;

    
    if (idx == NULL)
    {
	idx = &i;
	i = 0;
    }

    while (*idx < _yp_config->servers)
    {
	if (_yp_config->server[*idx].domain == NULL ||
	    (domain &&
	     strcmp(domain, _yp_config->server[*idx].domain) == 0))
	{
	    if (_yp_config->server[*idx].client == NULL)
	    {
		clh = clnt_create(_yp_config->server[*idx].address,
				  YPPROG, YPVERS, "udp");
		if (!clh)
		    goto next;
		
		clh->cl_auth = authunix_create_default();
		
		_yp_config->server[*idx].client = clh;
	    }

	    if (address)
		*address = _yp_config->server[*idx].address;

	    return _yp_config->server[(*idx)++].client;
	}
    
      next:
	(*idx)++;
    }
    
    return NULL;
}

int ypconf_unbinddomain(char *domain)
{
    int i;

    
    for (i = 0; i < _yp_config->servers; i++)
    {
	if (NULL == _yp_config->server[i].domain ||
	    strcmp(_yp_config->server[i].domain, domain) == 0)
	{
	    if (_yp_config->server[i].client)
	    {
		auth_destroy(_yp_config->server[i].client->cl_auth);
		clnt_destroy(_yp_config->server[i].client);
		_yp_config->server[i].client = NULL;
	    }
	    return i;
	}
    }
    
    return -1;
}

#endif /* ENABLE_YPEMU */
