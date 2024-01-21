/*
** publickey.c                      YP publickey map access functions
**
** Copyright (C) 1993 Signum Support AB
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
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <rpc/rpc.h>
#include <rpc/key_prot.h>
#include <rpcsvc/ypclnt.h>

extern int xdecrypt(char *secret, char *passwd);

static const char *PKMAP = "publickey.byname";


int _yp_getpublickey (const char *netname, char *publickey)
{
	char *domain;
	char *keyval = NULL;
	int keylen,err;
	char *p;
	
	domain = strchr(netname, '@');
	if (!domain)
		return (0);
	domain++;
	err = yp_match(domain, PKMAP, netname, strlen(netname), &keyval, &keylen);
	if (err)
		return (0);
	p = strchr(keyval, ':');
	if (p == NULL) {
		free(keyval);
		return (0);
	}
	
	*p = '\0';
	(void) strcpy(publickey, keyval);
	free(keyval);
	return (1);
}

int _yp_getsecretkey (const char *netname, char *secretkey, char *passwd)
{
	char *domain;
	char *keyval = NULL;
	int keylen,err;
	char *p;

	domain = strchr(netname, '@');
	if (!domain)
		return (0);
	domain++;
	err = yp_match(domain, PKMAP, netname, strlen(netname), &keyval, &keylen);
	if (err)
		return (0);
	p = strchr(keyval, ':');
	if (p == NULL) {
		free(keyval);
		return (0);
	}

	p++;
	if (!xdecrypt(p, passwd)) {
		free(keyval);
		return (0);
	}
	if (bcmp(p, p + HEXKEYBYTES, KEYCHECKSUMSIZE) != 0) {
		secretkey = NULL;
		free(keyval);
		return(0);
	}
	p[HEXKEYBYTES] = 0;
	(void) strcpy(secretkey, p);
	free(keyval);
	return (1);
}
