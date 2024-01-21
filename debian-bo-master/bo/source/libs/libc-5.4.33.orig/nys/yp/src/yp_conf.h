/*
** yp_conf.h                Config file parsing definitions for YP (NIS)
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

#ifndef __YP_CONF_H__
#define __YP_CONF_H__

#define YPCONF_MAXSERVERS 32
#ifndef PATH_YPCONF
#define PATH_YPCONF "/etc/yp.conf"
#endif


struct yp_conf
{
    char *domainname;
    int servers;
    int cserver;
    struct
    {
	char *address;
	char *domain;
	CLIENT *client;
    } server[YPCONF_MAXSERVERS];
    int timeout;
    int retries;
};

/* These functions shouldn't be externally visible */
#define ypconf_set __ypconf_set
#define ypconf_end __ypconf_end
#define ypconf_free __ypconf_free
#define ypconf_read __ypconf_read
#define ypconf_domain2address __ypconf_domain2address
#define ypconf_domain2client __ypconf_domain2client
#define ypconf_unbinddomain __ypconf_unbinddomain

extern struct yp_conf *_yp_config;

extern int ypconf_set(void);
extern void ypconf_end(void);
extern void ypconf_free(struct yp_conf *ncp);
extern struct yp_conf *ypconf_read(char *path);

extern char *ypconf_domain2address(const char *domain, int *idx);
extern CLIENT *ypconf_domain2client(const char *domain,
				    int *idx,
				    char **address);

extern int ypconf_unbinddomain(char *domain);

#endif
