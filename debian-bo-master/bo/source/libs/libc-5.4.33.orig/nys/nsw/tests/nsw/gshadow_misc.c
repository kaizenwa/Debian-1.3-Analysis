/*
** gshadow_misc.c
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
#include <shadow.h>


void print_gshadow(struct sgrp *sgp)
{
    int i;
    
    printf("%s:%s:", sgp->sg_name, sgp->sg_passwd);

    if (sgp->sg_adm)
	for (i = 0; sgp->sg_adm[i]; i++)
	{
	    printf("%s", sgp->sg_adm[i]);
	    if (sgp->sg_adm[i+1])
		putchar(',');
	}

    putchar(':');
    
    if (sgp->sg_mem)
	for (i = 0; sgp->sg_mem[i]; i++)
	{
	    printf("%s", sgp->sg_mem[i]);
	    if (sgp->sg_mem[i+1])
		putchar(',');
	}
    
    putchar('\n');
}
