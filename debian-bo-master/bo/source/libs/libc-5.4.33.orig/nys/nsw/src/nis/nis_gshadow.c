/*
** nis_gshadow.c              NIS+ GShadow "map" access routines
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

#include "config.h"

#ifdef ENABLE_NIS


#include <stdio.h>
#include <errno.h>
#include <grp.h>
#include <gshadow.h>
#include <rpcsvc/nis.h>
#include "xalloc.h"



static struct sgrp *parse_group(struct group *grp)
{
    static struct sgrp sgb;
    static char *nulptr = NULL;
    int i;
    

    if (sgb.sg_name)
	free(sgb.sg_name);
    if (sgb.sg_passwd)
	free(sgb.sg_passwd);

    sgb.sg_name = xstrdup(grp->gr_name);
    sgb.sg_passwd = xstrdup(grp->gr_passwd);
    
    sgb.sg_adm = &nulptr;

    if (sgb.sg_mem)
    {
	for (i = 0; sgb.sg_mem[i]; i++)
	    free(sgb.sg_mem[i]);
	
	free(sgb.sg_mem);
	sgb.sg_mem = NULL;
    }


    if (grp->gr_mem)
    {
	for (i = 0; grp->gr_mem[i]; i++)
	    ;

	if (i > 0)
	{
	    sgb.sg_mem = calloc(i, sizeof(sgb.sg_mem[0]));
	    if (sgb.sg_mem == NULL)
		return NULL;
	    
	    for (i = 0; grp->gr_mem[i]; i++)
		if ((sgb.sg_mem[i] = xstrdup(grp->gr_mem[i])) == NULL)
		    return NULL;
	}
    }
    
    if (sgb.sg_mem == NULL)
	sgb.sg_mem = &nulptr;

    return &sgb;
}



void _nis_setsgent(void)
{
    _nis_setgrent();
}


void _nis_endsgent(void)
{
    _nis_endgrent();
}


struct sgrp *_nis_getsgent(void)
{
    struct group *grp;

    grp = _nis_getgrent();
    return parse_group(grp);
}


struct sgrp *_nis_getsgnam(const char *name)
{
    struct group *grp;

    grp = _nis_getgrnam(name);
    return parse_group(grp);
}

#endif /* ENABLE_NIS */
