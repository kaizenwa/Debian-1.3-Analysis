/*
** ypcat.c		Display the contents of YP (NIS) maps
**
** Copyright (c) 1993 Signum Support AB
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

#include <stdio.h>
#include <rpcsvc/ypclnt.h>

extern void yp_prnick2real(FILE *fp);


int debug_flag = 0;
int nonick_flag = 0;
int empty_flag = 0;

extern char *yp_nick2real(char *mapname);



static void help(void)
{
    fprintf(stderr, "Usage:\n\typcat [-k] [-d domainname] [-t] mapname\n");
    fprintf(stderr, "\typcat -x\n");
}


int main(int argc, char *argv[])
{
    char *domain;
    char *mapname;
    char *result;
    int len;
    char *outkey;
    int keylen;
    int i;
    int code;

#ifdef KLUDGE
    (void) gethostbyname("lysator");
    (void) getprotobyname("ident", "tcp");
#endif
    domain = NULL;
    
    for (i = 1; i < argc && argv[i][0] == '-'; i++)
	switch (argv[i][1])
	{
	  case 'h':
	    help();
	    break;

	  case 'D':
	    debug_flag = 1;
	    break;
	    
	  case 'd':
	    if (i + 1 < argc)
		domain = argv[++i];
	    else
	    {
		fprintf(stderr, "%s: Missing domainname argument to '-d'.\n",
			argv[0]);
		exit(1);
	    }
	    break;

	  case 'k':
	    empty_flag = 1;
	    break;

	  case 't':
	    nonick_flag = 1;
	    break;

	  case 'x':
	    yp_prnick2real(stdout);
	    exit(0);

	  default:
	    fprintf(stderr, "%s: Unknown switch '%s'.\n",
		    argv[0], argv[i]);
	    exit(1);
	}

    if (i >= argc)
    {
	help();
	exit(1);
    }

    if (debug_flag)
	fprintf(stderr, "Before yp_get_default_domain(), domain = %s\n",
		domain);
    
    if (domain == NULL)
	if ((code = yp_get_default_domain(&domain)) != 0)
	{
	    fprintf(stderr, "%s: can't get default domainname. Reason: %s.\n",
		    argv[0], yperr_string(code));
	    exit(1);
	}

    if (debug_flag)
	fprintf(stderr, "After yp_get_default_domain(), domain = %s\n",
		domain);
    
    if (nonick_flag)
	mapname = argv[i];
    else
	mapname = yp_nick2real(argv[i]);

    if (debug_flag)
	fprintf(stderr, "After yp_nick2real(), mapname = %s\n", mapname);
    
    if ((code = yp_first(domain, mapname, &outkey, &keylen, &result, &len)))
    {
	fprintf(stderr, "%s: Can't get first entry in map '%s'. Reason: %s.\n",
		argv[0], mapname, yperr_string(code));
	exit(1);
    }

    if (debug_flag)
	fprintf(stderr, "After yp_first()...\n");
    
    do
    {
 	if (len != 0) {
 		if (result[len] == '\n') result[len] = '\0';
 	}
 	if (keylen != 0) {
 		if (outkey[keylen] == '\n') outkey[keylen] = '\0';
 	}
 
	if (debug_flag)
	{
	    printf("Key: len = %d, val = '%.*s'\n", keylen, keylen, outkey);
	    printf("Res: len = %d, val = '%.*s'\n", len, len, result);
	}

	if (empty_flag && len == 0)
	    printf("%.*s\n", keylen, outkey);
	else if (empty_flag)
 	    printf("%.*s %.*s\n", keylen, outkey, len, result);
	else
	    printf("%.*s\n", len, result);
    } while ((code = yp_next(domain, mapname,
			     outkey, keylen,
			     &outkey, &keylen,
			     &result, &len
			     )) == 0);
    if (code != YPERR_NOMORE)
    {
	fprintf(stderr, "%s: Map read aborted. Reason: %s (%d).\n",
		argv[0], yperr_string(code), code);
	exit(1);
    }

    exit(0);
}
