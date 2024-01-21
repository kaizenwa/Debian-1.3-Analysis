/*
** netdb/protocols.h     /etc/protocols access functions and structures
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

#ifndef __NETDB_PROTOCOLS_H__
#define __NETDB_PROTOCOLS_H__

#define _PATH_PROTOCOLS "/etc/protocols"

struct protoent
{
    char *p_name;
    char **p_aliases;
    int p_proto;
};


extern void _setprotoent(int stayopen);
extern void _endprotoent(void);
extern struct protoent *_getprotoent(void);
extern struct protoent *_getprotobyname(const char *name);
extern struct protoent *_getprotobynumber(int proto);

extern void _yp_setprotoent(int stayopen);
extern void _yp_endprotoent(void);
extern struct protoent *_yp_getprotoent(void);
extern struct protoent *_yp_getprotobyname(const char *name);
extern struct protoent *_yp_getprotobynumber(int proto);

extern void _compat_setprotoent(int stayopen);
extern void _compat_endprotoent(void);
extern struct protoent *_compat_getprotoent(void);
extern struct protoent *_compat_getprotobyname(const char *name);
extern struct protoent *_compat_getprotobynumber(int proto);

extern void _nis_setprotoent(int stayopen);
extern void _nis_endprotoent(void);
extern struct protoent *_nis_getprotoent(void);
extern struct protoent *_nis_getprotobyname(const char *name);
extern struct protoent *_nis_getprotobynumber(int proto);

extern void _dns_setprotoent(int stayopen);
extern void _dns_endprotoent(void);
extern struct protoent *_dns_getprotoent(void);
extern struct protoent *_dns_getprotobyname(const char *name);
extern struct protoent *_dns_getprotobynumber(int proto);

extern void _dbm_setprotoent(int stayopen);
extern void _dbm_endprotoent(void);
extern struct protoent *_dbm_getprotoent(void);
extern struct protoent *_dbm_getprotobyname(const char *name);
extern struct protoent *_dbm_getprotobynumber(int proto);

extern void setprotoent(int stayopen);
extern void endprotoent(void);
extern struct protoent *getprotoent(void);
extern struct protoent *getprotobyname(const char *name);
extern struct protoent *getprotobynumber(int proto);

#endif
