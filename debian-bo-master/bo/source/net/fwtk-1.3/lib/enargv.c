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
#include	<ctype.h>

#include	"firewall.h"

static	char	RcsId[] = "Header: enargv.c,v 1.4 94/11/01 11:55:22 mjr rel ";

int
enargv(buf,av,avsiz,tobuf,bsiz)
char	*buf;
char	**av;
int	avsiz;
char	*tobuf;
int	bsiz;
{
	register	char	*ip = buf;
	register	char	*op = tobuf;
	register	char	*sp = 0;
	int		u_ac = 0;
	int		quot = 0;


	if(ip == (char *)0 || *ip == '\0') {
		av[0] = (char *)0;
		return(0);
	}
	while(isspace(*ip) || !isprint(*ip) || *ip == ',')
		ip++;

	while(*ip) {
		if(!isspace(*ip) && !isprint(*ip)) {
			ip++;
			continue;
		}

		if(!quot && (*ip == '\"' || *ip == '\'')) {
			quot = *ip++;

			if(sp == (char *)0)
				sp = op;
			if(bsiz - 1 < 0)
				return(0);
			*op = '\0';
			continue;
		}
		if(isspace(*ip) && !quot) {
			if(--bsiz < 0)
				return(0);
			*op++ = '\0';

			if(u_ac  + 1 >= avsiz)
				return(u_ac);

			av[u_ac++] = sp;
			sp = av[u_ac] = (char *)0;

			while(isspace(*ip))
				ip++;

			if(*ip == '\0')
				break;
			continue;
		}

		if(quot && *ip == quot) {
			quot = 0;
			ip++;
			continue;
		}

		if(*ip == '\\') {
			if(--bsiz < 0)
				return(0);
			switch(*++ip) {
			case	't':
				*op++ = '\t';
				break;

			case	'n':
				*op++ = '\n';
				break;

			default:
				*op++ = *ip;
			}
			ip++;
			continue;
		}

		if(sp == (char *)0)
			sp = op;
		if(--bsiz < 0)
			return(0);
		*op++ = *ip++;
	}

	if(sp != 0) {
		*op = '\0';

		if(u_ac  + 1 >= avsiz)
			return(u_ac);
		av[u_ac++] = sp;
		av[u_ac] = (char *)0;
	}
	return(u_ac);
}
