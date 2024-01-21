/*
** ethers.h           Ethers "map" handling functions and structures
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

#ifndef __ETHERS_H__
#define __ETHERS_H__

#include <sys/types.h>

#define _PATH_ETHERS "/etc/ethers"


struct ether_addr
{
    unsigned char ether_addr_octet[6];
};


struct ether
{
    char name[128];
    struct ether_addr addr;
};

/* /etc/ethers file access functions */
extern void _setethent(void);
extern void _endethent(void);
extern struct ether *_getethent(void);
extern struct ether *_getethbyname(const char *name);
extern struct ether *_getethbyaddr(const struct ether_addr *addr);

/* YP access functions */
extern void _yp_setethent(void);
extern void _yp_endethent(void);
extern struct ether *_yp_getethent(void);
extern struct ether *_yp_getethbyname(const char *name);
extern struct ether *_yp_getethbyaddr(const struct ether_addr *addr);

/* COMPAT access functions */
extern void _compat_setethent(void);
extern void _compat_endethent(void);
extern struct ether *_compat_getethent(void);
extern struct ether *_compat_getethbyname(const char *name);
extern struct ether *_compat_getethbyaddr(const struct ether_addr *addr);

/* NIS+ access functions */
extern void _nis_setethent(void);
extern void _nis_endethent(void);
extern struct ether *_nis_getethent(void);
extern struct ether *_nis_getethbyname(const char *name);
extern struct ether *_nis_getethbyaddr(const struct ether_addr *addr);

/* DNS/Hesiod access functions */
extern void _dns_setethent(void);
extern void _dns_endethent(void);
extern struct ether *_dns_getethent(void);
extern struct ether *_dns_getethbyname(const char *name);
extern struct ether *_dns_getethbyaddr(const struct ether_addr *addr);

/* DBM access functions */
extern void _dbm_setethent(void);
extern void _dbm_endethent(void);
extern struct ether *_dbm_getethent(void);
extern struct ether *_dbm_getethbyname(const char *name);
extern struct ether *_dbm_getethbyaddr(const struct ether_addr *addr);

/* NameServiceSwitch indirect access functions */
extern void setethent(void);
extern void endethent(void);
extern struct ether *getethent(void);
extern struct ether *getethbyname(const char *name);
extern struct ether *getethbyaddr(const struct ether_addr *addr);

/* Client access functions */
extern char *              ether_ntoa(const struct ether_addr *addr);
extern struct ether_addr * ether_aton(const char *addr);

extern int ether_ntohost(char *hostname, const struct ether_addr *addr);
extern int ether_hostton(const char *hostname, struct ether_addr *addr);

extern int ether_line(char *line, struct ether_addr *addr, char *hostname);

#endif
