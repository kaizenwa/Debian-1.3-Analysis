/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

/*
 *	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
 */
static	char	RcsId[] = "Header: pass.c,v 1.2 93/11/11 16:22:22 mjr rel ";
#include	"firewall.h"
#include	"auth.h"

#ifdef	AUTHPROTO_PASSWORD

extern	char	*crypt();

passverify(user,pass,ap,rbuf)
char	*user;
char	*pass;
Auth	*ap;
char	*rbuf;
{
	if(ap->pw[0] == '\0') {
		strcpy(rbuf,"ok");
		return(0);
	}

	if(pass == (char *)0)
		goto reject;
	if(strlen(pass) > 7)
		pass[8] = '\0';

	if(!strcmp(crypt(pass,ap->pw),ap->pw)) {
		strcpy(rbuf,"ok");
		return(0);
	}
reject:
	strcpy(rbuf,"Permission Denied.");
	return(1);
}



static unsigned char itoa64[] =		/* 0 ... 63 => ascii - 64 */
	"./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

passset(user,pass,ap,rbuf)
char	*user;
char	*pass;
Auth	*ap;
char	*rbuf;
{
	if(pass == (char *)0)
		ap->pw[0] = '\0';
	else {
		long	t;
		char	salt[2];

		time(&t);
		salt[0] = itoa64[getpid() & 0x3f];
		salt[1] = itoa64[(int)t & 0x3f];
		if(strlen(pass) > 7)
			pass[8] = '\0';
		strncpy(ap->pw,crypt(pass,salt),AUTH_PWSIZ);
	}
	if(auth_dbputu(user,ap) == 0)
		sprintf(rbuf,"Password for %s changed.",user);
	else
		strcpy(rbuf,"Database error.");
	return(0);
}
#endif
