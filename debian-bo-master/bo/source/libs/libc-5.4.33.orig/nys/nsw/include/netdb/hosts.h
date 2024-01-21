/*
** netdb/hosts.h             /etc/hosts access functions and structures
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

#ifndef __NETDB_HOSTS_H__
#define __NETDB_HOSTS_H__

#define _PATH_HOSTS "/etc/hosts"

struct hostent {
    char *h_name;
    char **h_aliases;
    int h_addrtype;
    int h_length;
    char **h_addr_list;
};

/* Backwards compatiblity kludge */
#define h_addr h_addr_list[0]

#ifndef h_errno
extern h_errno;
#endif

/* DNS/Hesiod error codes left in 'extern int h_errno' */
#define	NETDB_INTERNAL -1 /* see errno */
#define	NETDB_SUCCESS   0 /* no problem */
#define	HOST_NOT_FOUND 1 /* Authoritative Answer Host not found */
#define	TRY_AGAIN      2 /* Non-Authoritive Host not found, or SERVERFAIL */
#define	NO_RECOVERY    3 /* Non recoverable errors, FORMERR, REFUSED, NOTIMP */
#define	NO_DATA	       4 /* Valid name, no data record of requested type */
#define	NO_ADDRESS     NO_DATA		/* no address, look for MX record */


extern void _sethostent(int stayopen);
extern void _endhostent(void);
extern struct hostent *_gethostent(void);
extern struct hostent *_gethostbyname(const char *name);
extern struct hostent *_gethostbyaddr(const char *addr, int len, int type);

extern void _yp_sethostent(int stayopen);
extern void _yp_endhostent(void);
extern struct hostent *_yp_gethostent(void);
extern struct hostent *_yp_gethostbyname(const char *name);
extern struct hostent *_yp_gethostbyaddr(const char *addr, int len, int type);

extern void _compat_sethostent(int stayopen);
extern void _compat_endhostent(void);
extern struct hostent *_compat_gethostent(void);
extern struct hostent *_compat_gethostbyname(const char *name);
extern struct hostent *_compat_gethostbyaddr(const char *addr, int len, int type);

extern void _nis_sethostent(int stayopen);
extern void _nis_endhostent(void);
extern struct hostent *_nis_gethostent(void);
extern struct hostent *_nis_gethostbyname(const char *name);
extern struct hostent *_nis_gethostbyaddr(const char *addr, int len, int type);

extern void _dns_sethostent(int stayopen);
extern void _dns_endhostent(void);
extern struct hostent *_dns_gethostent(void);
extern struct hostent *_dns_gethostbyname(const char *name);
extern struct hostent *_dns_gethostbyaddr(const char *addr, int len, int type);

extern void _dbm_sethostent(int stayopen);
extern void _dbm_endhostent(void);
extern struct hostent *_dbm_gethostent(void);
extern struct hostent *_dbm_gethostbyname(const char *name);
extern struct hostent *_dbm_gethostbyaddr(const char *addr, int len, int type);

extern void sethostent(int stayopen);
extern void endhostent(void);
extern struct hostent *gethostent(void);
extern struct hostent *gethostbyname(const char *name);
extern struct hostent *gethostbyaddr(const char *addr, int len, int type);

#endif
