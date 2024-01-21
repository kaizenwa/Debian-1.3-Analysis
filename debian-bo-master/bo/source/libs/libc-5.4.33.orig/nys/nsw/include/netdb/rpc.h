/*
** netdb/rpc.h     /etc/rpc access functions and structures
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

#ifndef __NETDB_RPC_H__
#define __NETDB_RPC_H__

#define _PATH_RPC "/etc/rpc"

struct rpcent
{
    char *r_name;
    char **r_aliases;
    int r_number;
};

extern void _setrpcent(int stayopen);
extern void _endrpcent(void);
extern struct rpcent *_getrpcent(void);
extern struct rpcent *_getrpcbyname(const char *name);
extern struct rpcent *_getrpcbynumber(int number);

extern void _yp_setrpcent(int stayopen);
extern void _yp_endrpcent(void);
extern struct rpcent *_yp_getrpcent(void);
extern struct rpcent *_yp_getrpcbyname(const char *name);
extern struct rpcent *_yp_getrpcbynumber(int number);

extern void _compat_setrpcent(int stayopen);
extern void _compat_endrpcent(void);
extern struct rpcent *_compat_getrpcent(void);
extern struct rpcent *_compat_getrpcbyname(const char *name);
extern struct rpcent *_compat_getrpcbynumber(int number);

extern void _nis_setrpcent(int stayopen);
extern void _nis_endrpcent(void);
extern struct rpcent *_nis_getrpcent(void);
extern struct rpcent *_nis_getrpcbyname(const char *name);
extern struct rpcent *_nis_getrpcbynumber(int number);

extern void _dns_setrpcent(int stayopen);
extern void _dns_endrpcent(void);
extern struct rpcent *_dns_getrpcent(void);
extern struct rpcent *_dns_getrpcbyname(const char *name);
extern struct rpcent *_dns_getrpcbynumber(int number);

extern void _dbm_setrpcent(int stayopen);
extern void _dbm_endrpcent(void);
extern struct rpcent *_dbm_getrpcent(void);
extern struct rpcent *_dbm_getrpcbyname(const char *name);
extern struct rpcent *_dbm_getrpcbynumber(int number);

extern void setrpcent(int stayopen);
extern void endrpcent(void);
extern struct rpcent *getrpcent(void);
extern struct rpcent *getrpcbyname(const char *name);
extern struct rpcent *getrpcbynumber(int number);

#endif
