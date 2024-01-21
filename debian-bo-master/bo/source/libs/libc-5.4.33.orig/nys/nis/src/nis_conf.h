/*
** nis_conf.h                Config file parsing definitions for NIS+
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

#ifndef __NIS_CONF_H__
#define __NIS_CONF_H__

#define NISCONF_MAXSERVERS 32
#define PATH_NISCONF "/etc/nis.conf"

struct nis_conf
{
    char *domainname;
    int servers;
    int cserver;
    struct
    {
	char *address;
	char *domain;
	CLIENT *client;
    } server[NISCONF_MAXSERVERS];
    int timeout;
    int retries;
};


#define nisconf_set __nisconf_set
#define nisconf_end __nisconf_end
#define nisconf_free __nisconf_free
#define nisconf_read __nisconf_read
#define nisconf_domain2address __nisconf_domain2address
#define nisconf_domain2client  __nisconf_domain2client

extern struct nis_conf *_nis_config;

extern int nisconf_set(void);
extern void nisconf_end(void);
extern void nisconf_free(struct nis_conf *ncp);
extern struct nis_conf *nisconf_read(char *path);

extern char *nisconf_domain2address(char *domain, int *idx);
extern CLIENT *nisconf_domain2client(char *domain,
				     int *idx, char **address,
				     int tcp_flag);

#endif
