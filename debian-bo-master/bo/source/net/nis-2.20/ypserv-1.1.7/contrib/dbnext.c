/*
** dbnext.c	            NYS YP Server database debugging tool
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


int main(int argc, char *argv[])
{
    GDBM_FILE dbp;
    datum key, val;
    int i, verbose = 0;


    for (i = 1; i < argc && argv[i][0] == '-'; i++)
	switch (argv[i][1])
	{
	  case 'v':
	    verbose = 1;
	    break;
	}
    
    if (i+1 >= argc) {
      fprintf( stderr, "Usage: %s [-v] GDBM-File key\n", argv[0]);
      exit( 1);
    }
    dbp = gdbm_open(argv[i], 128, O_RDONLY, 0, NULL);
    if (dbp == NULL)
    {
	perror("dbnext: dbm_open");
	exit(1);
    }

    key.dsize = strlen(argv[i+1]);
    key.dptr = argv[i+1];
    
    key = gdbm_nextkey(dbp, key);
    if (key.dptr == NULL)
    {
	perror("dbnext: gdbm_nextkey()");
	exit(1);
    }
    
    val = gdbm_fetch(dbp, key);
    if (val.dptr == NULL)
    {
	perror("dbnext: gdbm_fetch()");
	exit(1);
    }
    
    if (verbose)
	printf("key = \"%.*s\", val = \"%.*s\"\n",
	       key.dsize, key.dptr,
	       val.dsize, val.dptr);
    else
	printf("%.*s\t%.*s\n",
	       key.dsize, key.dptr,
	       val.dsize, val.dptr);
    
    gdbm_close(dbp);
    exit(0);
}
