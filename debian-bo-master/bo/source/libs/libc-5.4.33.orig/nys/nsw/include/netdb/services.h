/*
** netdb/services.h    /etc/services access functions and structures
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

#ifndef __NETDB_SERVICES_H__
#define __NETDB_SERVICES_H__

#define _PATH_SERVICES "/etc/services"

struct servent
{
    char *s_name;
    char **s_aliases;
    int s_port;
    char *s_proto;
};

extern void _setservent(int stayopen);
extern void _endservent(void);
extern struct servent *_getservent(void);
extern struct servent *_getservbyname(const char *name, const char *proto);
extern struct servent *_getservbyport(int port, const char *proto);

extern void _yp_setservent(int stayopen);
extern void _yp_endservent(void);
extern struct servent *_yp_getservent(void);
extern struct servent *_yp_getservbyname(const char *name, const char *proto);
extern struct servent *_yp_getservbyport(int port, const char *proto);

extern void _compat_setservent(int stayopen);
extern void _compat_endservent(void);
extern struct servent *_compat_getservent(void);
extern struct servent *_compat_getservbyname(const char *name, const char *proto);
extern struct servent *_compat_getservbyport(int port, const char *proto);

extern void _nis_setservent(int stayopen);
extern void _nis_endservent(void);
extern struct servent *_nis_getservent(void);
extern struct servent *_nis_getservbyname(const char *name, const char *proto);
extern struct servent *_nis_getservbyport(int port, const char *proto);

extern void _dns_setservent(int stayopen);
extern void _dns_endservent(void);
extern struct servent *_dns_getservent(void);
extern struct servent *_dns_getservbyname(const char *name, const char *proto);
extern struct servent *_dns_getservbyport(int port, const char *proto);

extern void _dbm_setservent(int stayopen);
extern void _dbm_endservent(void);
extern struct servent *_dbm_getservent(void);
extern struct servent *_dbm_getservbyname(const char *name, const char *proto);
extern struct servent *_dbm_getservbyport(int port, const char *proto);

extern void setservent(int stayopen);
extern void endservent(void);
extern struct servent *getservent(void);
extern struct servent *getservbyname(const char *name, const char *proto);
extern struct servent *getservbyport(int port, const char *proto);

#endif
