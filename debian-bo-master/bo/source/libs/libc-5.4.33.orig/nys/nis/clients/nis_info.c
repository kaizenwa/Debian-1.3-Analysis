/*
** nis_info.c
**
** Copyright (c) 1993 Signum Support AB, Sweden
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
#include <rpcsvc/nis.h>
#include "../src/nis_print.h"


int main(argc,argv)
    int argc;
    char *argv[];
{
    nis_result *nsres;

    
    if (argc < 2)
    {
	fprintf(stderr, "usage: %s nisobject\n", argv[0]);
	exit(1);
    }


    nsres = nis_lookup(argv[1], EXPAND_NAME);
    if (nsres == NULL)
    {
	perror("nis_lookup");
	exit(3);
    }

    if (nsres->status != 0 && nsres->status != 1)
        nis_perror(nsres->status, argv[1]);
    else
        nis_print_result(nsres);

    nis_freeresult(nsres);
    
    exit(0);
}
