/*
** netdb/networks.h    /etc/networks access functions and structures
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

#ifndef __NETDB_NETWORKS_H__
#define __NETDB_NETWORKS_H__

#define _PATH_NETWORKS "/etc/networks"

struct netent
{
    char *n_name;
    char **n_aliases;
    int n_addrtype;
    unsigned long n_net;
};

extern void _setnetent(int stayopen);
extern void _endnetent(void);
extern struct netent *_getnetent(void);
extern struct netent *_getnetbyname(const char *name);
extern struct netent *_getnetbyaddr(unsigned long net, int type);

extern void _yp_setnetent(int stayopen);
extern void _yp_endnetent(void);
extern struct netent *_yp_getnetent(void);
extern struct netent *_yp_getnetbyname(const char *name);
extern struct netent *_yp_getnetbyaddr(unsigned long net, int type);

extern void _compat_setnetent(int stayopen);
extern void _compat_endnetent(void);
extern struct netent *_compat_getnetent(void);
extern struct netent *_compat_getnetbyname(const char *name);
extern struct netent *_compat_getnetbyaddr(unsigned long net, int type);

extern void _nis_setnetent(int stayopen);
extern void _nis_endnetent(void);
extern struct netent *_nis_getnetent(void);
extern struct netent *_nis_getnetbyname(const char *name);
extern struct netent *_nis_getnetbyaddr(unsigned long net, int type);

extern void _dns_setnetent(int stayopen);
extern void _dns_endnetent(void);
extern struct netent *_dns_getnetent(void);
extern struct netent *_dns_getnetbyname(const char *name);
extern struct netent *_dns_getnetbyaddr(unsigned long net, int type);

extern void _dbm_setnetent(int stayopen);
extern void _dbm_endnetent(void);
extern struct netent *_dbm_getnetent(void);
extern struct netent *_dbm_getnetbyname(const char *name);
extern struct netent *_dbm_getnetbyaddr(unsigned long net, int type);

extern void setnetent(int stayopen);
extern void endnetent(void);
extern struct netent *getnetent(void);
extern struct netent *getnetbyname(const char *name);
extern struct netent *getnetbyaddr(unsigned long net, int type);

#endif
