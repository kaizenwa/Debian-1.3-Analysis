/*
** dbcat.c	            NYS YP Server database viewer
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
    int i;
    int verbose = 0;

    
    for (i = 1; i < argc && argv[i][0] == '-'; i++)
	switch (argv[i][1])
	{
	  case 'v':
	    verbose = 1;
	    break;
	}
    if (i >= argc) {
      fprintf( stderr, "Usage: %s [-v] GDBM-File\n", argv[0]);
      exit( 1);
    }
    dbp = gdbm_open(argv[i], 128, O_RDONLY, 0, NULL);
    if (dbp == NULL)
    {
	perror("dbadd: dbm_open");
	exit(1);
    }

    for (key = gdbm_firstkey(dbp); key.dptr != NULL;
	 key = gdbm_nextkey(dbp, key))
    {
	val = gdbm_fetch(dbp, key);

	if (verbose)
	    printf("key = \"%.*s\", value = \"%.*s\"\n",
		   key.dsize, key.dptr,
		   val.dsize, val.dptr);
	else
	    printf("%.*s\t%.*s\n",
		   key.dsize, key.dptr,
		   val.dsize, val.dptr);
    }
    
    gdbm_close(dbp);
    exit(0);
}
