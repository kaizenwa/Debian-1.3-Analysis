/*
** netdb.h           Generic network "database" functions and structures
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

#ifndef __NETDB_H__
#define __NETDB_H__

#include <sys/types.h>

#define _PATH_RESCONF "/etc/resolv.conf"
#define _PATH_HEQUIV  "/etc/hosts.equiv"

#include <netdb/hosts.h>
#include <netdb/networks.h>
#include <netdb/protocols.h>
#include <netdb/services.h>
#include <netdb/rpc.h>

#if defined(_POSIX_THREAD_SAFE_FUNCTIONS) || defined(_REENTRANT)
int *__h_errno_location __P((void));
#define h_errno	(*__h_errno_location ())
#else
extern int h_errno;
#endif

#endif
