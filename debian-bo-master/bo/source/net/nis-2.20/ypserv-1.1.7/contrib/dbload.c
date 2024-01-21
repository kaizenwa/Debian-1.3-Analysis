/*
** dbload.c	            NYS YP Server database loader
**
** Copyright (c) 1993 Signum Support AB, Sweden
**
** This file is part of the NYS YP Server.
**
** The NYS YP Server is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS YP Server is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** General Public License for more details.
** 
** You should have received a copy of the GNU General Public
** License along with the NYS YP Server; see the file COPYING.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
**
** Author: Peter Eriksson <pen@signum.se>
*/

#include "system.h"

#include <stdio.h>
#include <fcntl.h>
#include <gdbm.h>
#include <ctype.h>
#include <string.h>


static void inject_data(GDBM_FILE dbp, char *key, char *val, int flag)
{
    datum kdat, vdat;
    
    kdat.dsize = strlen(key);
    kdat.dptr = key;

    vdat.dsize = strlen(val);
    vdat.dptr = val;
    
    if (gdbm_store(dbp, kdat, vdat, flag) < 0)
    {
	perror("dbload: dbm_store");
	gdbm_close(dbp);
	exit(1);
    }
}

static void inject_file(GDBM_FILE dbp, FILE *fp, int iflag, char *argv0)
{
    char buf[8192];
    char *cp;
    

    while ((cp = fgets(buf, sizeof(buf)-1, fp)) != NULL)
    {
	int len = strlen(cp);

	if (len > 1 && cp[len-1] == '\n')
	    cp[len-1] = '\0';
	
	while (*cp && *cp != '\t' && *cp != ' ')
	    ++cp;

	if (!*cp)
	    fprintf(stderr, "%s: warning: malformed input data (ignored)\n",
		    argv0);
	else
	{
	    *cp++ = '\0';

	    inject_data(dbp, buf, cp, iflag);
	}
    }
}


int main(int argc, char *argv[])
{
    GDBM_FILE dbp;
    char *fname;
    int i;
    
    int iflag = GDBM_REPLACE;
    FILE *fp = NULL;

    
    for (i = 1; i < argc && argv[i][0] == '-'; i++)
	switch (argv[i][1])
	{
	  case 'r':
	    iflag = GDBM_REPLACE;
	    break;

	  case 'i':
	    iflag = GDBM_INSERT;
	    break;

	  case 'f':
	    if (argv[i][2])
		fname = argv[i]+2;
	    else
		fname = argv[++i];
	    if (strcmp(fname, "-") == 0)
		fp = stdin;
	    else
	    {
		fp = fopen(fname, "r");
		if (fp == NULL)
		{
		    fprintf(stderr, "%s: ", argv[0]);
		    perror(fname);
		    exit(1);
		}
	    }
	    break;

	  default:
	    fprintf(stderr, "%s: illegal command line switch: %s\n",
		    argv[0], argv[i]);
	    exit(1);
	}

    if (i >= argc)
    {
	fprintf(stderr,
		"usage: %s [-f<inputfile>] <database> [<key> <value>]*\n",
		argv[0]);
	exit(1);
    }
    
    dbp = gdbm_open(argv[i++], 256, GDBM_WRCREAT, 0622, NULL);
    if (dbp == NULL)
    {
	perror("dbload: dbm_open");
	exit(1);
    }

    if (fp)
	inject_file(dbp, fp, iflag, argv[0]);
    
    while (i < argc)
    {
	if (i + 1 >= argc)
	{
	    fprintf(stderr, "%s: missing value in key-value pair\n",
		    argv[0]);
	    exit(1);
	}
	
	inject_data(dbp, argv[i], argv[i+1], iflag);
	i += 2;
    }
	    
    gdbm_close(dbp);
    exit(0);
}
