/*
** nis_cat.c
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


void p_entry_col(entry_col *ecp)
{
    if (ecp->ec_value.ec_value_len == 0)
	return;
    
    printf("%.*s",
	   (int) ecp->ec_value.ec_value_len,
	   ecp->ec_value.ec_value_val);
}


void p_entry(entry_obj *eop, int sep)
{
    int i;
    
    for (i = 0; i < eop->en_cols.en_cols_len; i++)
    {
	p_entry_col(&eop->en_cols.en_cols_val[i]);
	if (i + 1 < eop->en_cols.en_cols_len)
	    putchar(sep);
    }

    putchar('\n');
}


int main(int argc, char *argv[])
{
    nis_result *res, *nres = NULL;
    int separator;
    char *name;
    int verbose = 0;

    
    if (argc < 2)
    {
	fprintf(stderr, "usage: %s [-v] nisobject\n", argv[0]);
	exit(1);
    }

    if (strcmp(argv[1], "-v") == 0)
    {
	if (argc < 3)
	{
	    fprintf(stderr, "usage: %s [-v] nisobject\n", argv[0]);
	    exit(1);
	}

	verbose = 1;
	name = argv[2];
    }
    else
	name = argv[1];
    
    res = nis_lookup(name, 0);
    if (res == NULL)
    {
	perror("nis_lookup");
	exit(3);
    }

    if (verbose)
    {
	fprintf(stderr, "Result of object lookup:\n");
	nis_fprint_result(res, 0, stderr);
    }
    
    separator = res->objects.objects_val[0].zo_data.objdata_u.ta_data.ta_sep;

    nis_freeresult(res);

    res = nis_first_entry(name);
    if (res == NULL)
    {
	perror("nis_first_entry");
	exit(3);
    }

    do
    {
	if (verbose)
	    nis_print_result(res);
	else
	{
	    int i;
	    
	    for (i = 0; i < res->objects.objects_len; i++)
	      p_entry(&res->objects.objects_val[i].zo_data.objdata_u.en_data,
		      separator);
	}
    } while ((res->status == 0 || res->status == 1) &&
	     ((nres = nis_next_entry(name, &res->cookie)),
	      res = nres));

    if (res->status != NIS_SUCCESS &&
	res->status != NIS_S_SUCCESS)
    {
	if ((res->status == NIS_NOTFOUND ||
	     res->status == NIS_S_NOTFOUND) && nres)
	    exit(0);
	
        nis_perror(res->status, argv[1]);
    }
	     
    exit(0);
}

