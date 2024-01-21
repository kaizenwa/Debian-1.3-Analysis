/*
** ypmatch.c		Show an entry in a NIS map
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
    fprintf(stderr,
	    "Usage:\n\typmatch [-k] [-d domainname] [-t] key mapname\n");
    fprintf(stderr, "\typmatch -x\n");
}


int main(int argc, char *argv[])
{
    char *domain;
    char *mapname;
    char *result;
    int len;
    int i;
    int code;
    
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

    if (i + 1 >= argc)
    {
	help();
	exit(1);
    }
    
    if (domain == NULL)
	if ((code = yp_get_default_domain(&domain)) != 0)
	{
	    fprintf(stderr, "%s: Can't get default domainname. Reason: %s.\n",
		    argv[0], yperr_string(code));
	    exit(1);
	}

    if (nonick_flag)
	mapname = argv[i+1];
    else
	mapname = yp_nick2real(argv[i+1]);
    


    code = yp_match(domain, mapname, argv[i], strlen(argv[i]),
		    &result, &len);

    if (code)
    {
	fprintf(stderr, "%s: Can't match key %s in map %s. Reason: %s.\n",
		argv[0], argv[i], mapname, yperr_string(code));
	exit(1);
    }

    printf("%.*s\n", len, result);
    exit(0);
}
