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
static	char	RcsId[] = "Header: skey.c,v 1.4 94/11/01 11:52:33 mjr rel ";
#include	<stdio.h>
#include	<sys/types.h>
#include	<time.h>

#include	"firewall.h"
#include	"auth.h"

#ifdef	AUTHPROTO_SKEY

#include	"skey.h"

static	struct	skey	kbuf;
static	int		challenged = 0;

s_keychallng(user,buf,bs)
char	*user;
char	*buf;
int	bs;
{
	strcpy(buf,"Skey Challenge: ");
	if(skeychallenge(&kbuf,user,&buf[16]) == 0) {
		challenged = 1;
		return(0);
	}
	sprintf(buf,"Skey not initialized for %s",user);
	return(1);
}



s_keyverify(user,pass,ap,rbuf)
char	*user;
char	*pass;
Auth	*ap;
char	*rbuf;
{
	strcpy(rbuf,"Permission Denied.");
	if(!challenged)
		return(1);
	challenged = 0;
	if(skeyverify(&kbuf,pass) == 0) {
		strcpy(rbuf,"ok");
		if(kbuf.n < 5)
			strcat(rbuf,"Warning -- Change password soon!");
		return(0);
	}
	return(1);
}



#define NAMELEN 2

/* this is a ripped-up version of skeyinit.c from the skey dist'n */
s_keyset(user,pass,ap,rbuf)
char	*user;
char	*pass;
Auth	*ap;
char	*rbuf;
{
	struct	skey	skey;
	struct	tm	*tm;
	int		rval;
	int		n;
	int		nn;
	char		seed[18];
	char		key[8];
	char		defaultseed[17];
	long		now;
	char		tbuf[27];
	char		hbuf[512];
	char		lastc;
	int		l;

	if(pass == (char *)0) {
		strcpy(rbuf,"NULL passwords not permitted for S/key.");
		return(1);
	}
	time(&now);
	tm = localtime(&now);
	strftime(tbuf, sizeof(tbuf), "%M%j", tm);
	gethostname(hbuf,sizeof(hbuf));
	defaultseed[0] = hbuf[0];
	defaultseed[1] = hbuf[1];
	strcpy(&defaultseed[NAMELEN],tbuf);

	rval = skeylookup(&skey,user);
	switch(rval) {
	case -1:
		strcpy(rbuf,"Database error.");
		return(1);
	case 0:
		l = strlen(skey.seed);
		if(l > 0) {
			lastc = skey.seed[l-1];
			if(isdigit(lastc) && lastc != '9') {
				strcpy(defaultseed, skey.seed);
				defaultseed[l-1] = lastc + 1;
			}
			if(isdigit(lastc) && lastc == '9' && l < 16) {
				strcpy(defaultseed, skey.seed);
				defaultseed[l-1] = '0';
				defaultseed[l] = '0';
				defaultseed[l+1] = '\0';
			}
		}
		break;
	}
	n = 664;
	strcpy(seed,defaultseed);
	/* Crunch seed and password into starting key */
	if(keycrunch(key,seed,pass) != 0) {
		strcpy(rbuf,"Database error.");
		return(1);
	}
	nn = n;
	while(nn-- != 0)
		f(key);
	time(&now);
	tm = localtime(&now);
	strftime(tbuf, sizeof(tbuf), " %b %d,%Y %T", tm);

	skey.val = (char *)skey.buf;
	btoa8(skey.val,key);
	fprintf(skey.keyfile,"%s %04d %-16s %s %-21s\n",user,n,seed,skey.val,tbuf);
	fclose(skey.keyfile);
	sprintf(rbuf,"ID %s s/key is %d %s",user,n,seed);
	return(0);
}
#endif
