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
static	char	RcsId[] = "Header: nama.c,v 1.4 94/11/01 11:55:28 mjr rel ";

#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<netdb.h>
#include	<syslog.h>
#include	<ctype.h>

extern	char	*inet_ntoa();
extern	long	inet_addr();


#include	"firewall.h"


/* ripped bloody and kicking from some code by Guido Van Rossum */
int
namatch(pattern,string)
char	*pattern;
char	*string;
{
	register char c;
	while(1)
		switch(c = *pattern++) {
		case '\0':
			return(*string == '\0');
		case '*':
			c = *pattern;
			while(c == '*')
				c = *++pattern;
			if(c == '\0')
				return(1);
			/* general case, use recursion */
			while(*string != '\0') {
				if(namatch(pattern,string))
					return(1);
				++string;
			}
			return(0);
		default:
			if(c != *string++)
				return(0);
			break;
		}
}





/*
all singing and dancing host name match code. this is more complex
than I'd like it to be but it's somewhat unavoidable. the model is
that we're given a pattern and either and address or a host name
to match it against. we need to look at the type of the pattern,
and depending on the pattern, we convert what we're given, then
match on that. generally, IP addresses are better to use, and
generally should be matched with addresses.
*/
hostmatch(pattern,name)
char	*pattern;
char	*name;
{
	struct	hostent		*hp;
	struct	sockaddr_in	sin;
	char			pat[512];
	char			nam[512];
	char			*p;
	int			x;
	int			y;

	/* copy into private area because we lowercase names */
	if((x = strlen(pattern)) >= sizeof(pat)) {
		syslog(LLEV,"fwtkcfgerr: pattern %s too long!",pattern);
		return(0);
	}
	strcpy(pat,pattern);
	for(y = 0; y < x; y++)
		if(isupper(pat[y]))
			pat[y] = tolower(pat[y]);


	if((x = strlen(name)) >= sizeof(nam)) {
		syslog(LLEV,"fwtkcfgerr: name %s too long!",name);
		return(0);
	}
	strcpy(nam,name);
	for(y = 0; y < x; y++)
		if(isupper(nam[y]))
			nam[y] = tolower(nam[y]);


	/* is the pattern numeric ? */
	p = pat;
	while(*p != '\0' && (*p == '.' || *p == '*' || isdigit(*p)))
		p++;


	/* match against a text name */
	if(*p != '\0') {
		long		f;
		char		*p = nam;
		struct in_addr	*hp_addr;
		int		eq = 0;

		while(*p != '\0' && (*p == '.' || isdigit(*p)))
			p++;

		/* if the name is also a text name, just match */
		if(*p != 0)
			return(namatch(pattern,nam));

		/* fooey, it's not, we need to reverse lookup */
		if((f = inet_addr(nam)) == (long) -1) {
			syslog(LLEV,"fwtkcfgerr: inet_addr, malformed address: %s",nam);
			return(0);
		}

		hp = gethostbyaddr(&f,sizeof(f),AF_INET);
		if(hp == (struct hostent *)0)
			return(namatch(pat,"unknown"));

		/* rewrite name */
		if((x = strlen(hp->h_name)) >= sizeof(nam)) {
			syslog(LLEV,"fwtksyserr: name %s too long!",hp->h_name);
			return(0);
		}
		strcpy(nam,hp->h_name);
		for(y = 0; y < x; y++)
			if(isupper(nam[y]))
				nam[y] = tolower(nam[y]);

		/* cross-check reverse lookup to try to detect DNS spoofs */
		hp = gethostbyname(nam);
		if(hp == (struct hostent *)0)
			return(namatch(pat,"unknown"));

		while((hp_addr = (struct in_addr *)*hp->h_addr_list++) != (struct in_addr *)0) {
			if(bcmp(hp_addr,&f,hp->h_length) == 0) {
				eq = 1;
				break;
			}
		}

		if(!eq) {
			syslog(LLEV,"securityalert: possible spoof %s/%s != %s name lookup mismatch",nam,inet_ntoa(&f),hp->h_name);
			return(namatch(pat,"unknown"));
		}

		/* name valid: match */
		return(namatch(pat,nam));
	}



	/* match against a numeric pattern */
	p = nam;
	while(*p != '\0' && (*p == '.' || isdigit(*p)))
		p++;

	/* all numeric match is easy ! */
	if(*p == '\0')
		return(namatch(pat,nam));

	/* get address and covert to numbers to match on */
	hp = gethostbyname(nam);

	/* unknown host can never match numeric spec */
	if(hp == (struct hostent *)0)
		return(0);

	/* match names */
	bcopy(hp->h_addr,&sin.sin_addr,hp->h_length);
	return(namatch(pat,inet_ntoa(sin.sin_addr)));
}





#ifdef	TESTNAMATCH
#include <stdio.h>

main()
{
	char	buf1[1024];
	char	buf2[1024];

	while(1) {
		fprintf(stderr,"pattern: ");
		if(gets(buf1) == (char *)0)
			exit(0);
		fprintf(stderr,"name: ");
		if(gets(buf2) == (char *)0)
			exit(0);
		if(hostmatch(buf1,buf2))
			printf("matches\n");
		else
			printf("no match\n");
	}
}
#endif
