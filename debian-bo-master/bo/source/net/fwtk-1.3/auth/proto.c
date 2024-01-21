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
static	char	RcsId[] = "Header: proto.c,v 1.2 94/02/11 11:04:23 mjr rel ";
#include	"firewall.h"
#include	"auth.h"


#ifdef	AUTHPROTO_PASSWORD
extern	int	passverify(), passset();
#endif

#ifdef	AUTHPROTO_SKEY
extern	int	s_keychallng(), s_keyverify(), s_keyset();
#endif

#ifdef	AUTHPROTO_SNK
extern	int	snkchallng(), snkverify(), snkset();
#endif

#ifdef	AUTHPROTO_SECURID
extern	int	secichallng(), seciverify(), seciset();
#endif

#ifdef  AUTHPROTO_ENIGMA
extern  int	enigchallng(), enigverify(), enigset();
#endif

AProto aprototab[] = {
	'0',	"none",		0,		0,		0,
#ifdef	AUTHPROTO_PASSWORD
	'p',	"password",	0,		passverify,	passset,
#endif
#ifdef	AUTHPROTO_SKEY
	's',	"Skey",		s_keychallng,	s_keyverify,	s_keyset,
#endif
#ifdef	AUTHPROTO_SNK
	'd',	"Snk",		snkchallng,	snkverify,	snkset,
#endif
#ifdef	AUTHPROTO_SECURID
	'S',	"Securid",	secichallng,	seciverify,	seciset,
#endif
#ifdef	AUTHPROTO_ENIGMA
	'e',	"Enigma",	enigchallng,	enigverify,	enigset,
#endif
	0,	0,		0,		0,		0
};



AProto	*
codetoproto(c)
char	c;
{
	AProto	*ap;
	for(ap = aprototab; ap->name != (char *)0; ap++)
		if(ap->typ == c)
			return(ap);
	return((AProto *)0);
}



AProto	*
nametoproto(c)
char	*c;
{
	AProto	*ap;
	int	l;

	l = strlen(c);
	for(ap = aprototab; ap->name != (char *)0; ap++)
		if(!strncasecmp(ap->name,c,l))
			return(ap);
	return((AProto *)0);
}


getprotolist(b,bs)
char	*b;
int	bs;
{
	int	l = 0;
	int	x;
	AProto	*ap;

	for(ap = aprototab; ap->name != (char *)0; ap++) {
		x = strlen(ap->name);
		if(x + l >= bs - 2) {
			b[l] = '\0';
			return;
		}
		if(l > 0)
			b[l++] = ' ';
		strcpy(&b[l],ap->name);
		l += x;
	}
	b[l] = '\0';
}
