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
#include <rpcsvc/nis.h>

extern int xdecrypt(char *secret, char *passwd);

static const char *PKTABLE = "cred.org_dir";
#define PKTABLE_LEN 12

int _nis_getpublickey (const char *netname, char *publickey)
{
	nis_result *res;
	char *domain;
	int len;
	char buf[NIS_MAXNAMELEN+1];
	char *p;

	publickey[0] = '\0';

	domain = strchr(netname, '@');
	if (!domain)
		return (0);
	domain++;

	if ((strlen(netname)+PKTABLE_LEN+strlen(domain)+32) > NIS_MAXNAMELEN)
		return (0);

	sprintf(buf, "[auth_name=%s,auth_type=DES],%s.%s",
		netname, PKTABLE, domain);
	if (buf[strlen(buf)-1] != '.')
		strcat(buf, ".");

	res = nis_list(buf, EXPAND_NAME,
			NULL, NULL);
	if (res == NULL)
	  return 0;

	switch (res->status) {
	case NIS_SUCCESS:
	case NIS_S_SUCCESS:
		break;
	case NIS_NOTFOUND:
	case NIS_PARTIAL:
	case NIS_NOSUCHNAME:
	case NIS_NOSUCHTABLE:
		nis_freeresult(res);
		return (0);
	case NIS_S_NOTFOUND:
	case NIS_TRYAGAIN:
		nis_freeresult(res);
		return (0);
	default:
		nis_freeresult(res);
		return (0);
	}

	len = ENTRY_LEN(res->objects.objects_val, 3);
	strncpy(publickey, ENTRY_VAL(res->objects.objects_val, 3), len);
	publickey[len] = 0;
	p = strchr(publickey, ':');
	if (p)
		*p = 0;
	nis_freeresult(res);
	return (1);
}

int _nis_getsecretkey (const char *netname, char *secretkey, char *passwd)
{
	nis_result *res;
	char *domain;
	int len;
	char buf[NIS_MAXNAMELEN+1];

	secretkey[0] = '\0';

	domain = strchr(netname, '@');
	if (!domain)
		return (0);
	domain++;

	if ((strlen(netname)+PKTABLE_LEN+strlen(domain)+32) > NIS_MAXNAMELEN)
		return (0);

	sprintf(buf, "[auth_name=%s,auth_type=DES],%s.%s",
		netname, PKTABLE, domain);
	if (buf[strlen(buf)-1] != '.')
		strcat(buf, ".");

	res = nis_list(buf, USE_DGRAM+NO_AUTHINFO+FOLLOW_LINKS+FOLLOW_PATH,
			NULL, NULL);
	if (res == NULL)
	  return 0;
	switch (res->status) {
	case NIS_SUCCESS:
	case NIS_S_SUCCESS:
		break;
	case NIS_NOTFOUND:
	case NIS_PARTIAL:
	case NIS_NOSUCHNAME:
	case NIS_NOSUCHTABLE:
		nis_freeresult(res);
		return (0);
	case NIS_S_NOTFOUND:
	case NIS_TRYAGAIN:
		nis_freeresult(res);
		return (0);
	default:
		nis_freeresult(res);
		return (0);
	}

	len = ENTRY_LEN(res->objects.objects_val, 4);
	strncpy(buf, ENTRY_VAL(res->objects.objects_val, 4), len);
	if (!xdecrypt(buf, passwd) !=0) {
		nis_freeresult(res);
		return (0);
	}
	if (bcmp(buf, &(buf[HEXKEYBYTES]), KEYCHECKSUMSIZE) != 0) {
		secretkey = NULL;
		nis_freeresult(res);
		return (0);
	}
	buf[HEXKEYBYTES] = 0;
	(void) strcpy(secretkey, buf);
	nis_freeresult(res);
	return (1);
}
