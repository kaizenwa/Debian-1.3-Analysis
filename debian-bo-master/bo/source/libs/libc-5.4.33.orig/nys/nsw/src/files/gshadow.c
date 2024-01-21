/*
** gshadow.c                           /etc/gshadow access functions
**
** This file is part of the NYS Library.
**
**      Copyright (c) 1993 Signum Support AB
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
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "gshadow.h"
#include "xalloc.h"
#include "misc.h"

static FILE *sfp = NULL;


static int addtolist(char ***list, int *size, int *len, char *str)
{
    if (*len + 2 >= *size)
	*size += 16;
    
    if (Xalloc(list, *size) == NULL)
    {
	*size = 0;
	*len = 0;
	return -1;
    }

    (*list)[*len] = str;
    (*list)[++*len] = NULL;

    return *len;
}


struct sgrp *sgetsgent(const char *str)
{
    static char *buf = NULL;
    static struct sgrp spb;
    static char *null = NULL;
    int len_adm;
    int len_mem;
    static int size_adm = 0;
    static int size_mem = 0;
    char *cp, *mp, *sp;


    if (Xalloc(&buf, strlen(str)+1) == 0)
	return NULL;

    strcpy(buf, str);
    
#if 0
    if (spb.sg_name)
	spb.sg_name = NULL;
    
    if (spb.sg_passwd)
	spb.sg_passwd = NULL;

    if (len_adm > 0)
    {
	len_adm = 0;
	spb.sg_adm[0] = NULL;
    }

    if (len_mem > 0)
    {
	len_mem = 0;
	spb.sg_mem[0] = NULL;
    }
#else
    /* We set up default sg_adm and sg_mem. */
    len_adm = 0;
    spb.sg_adm = &null;
    len_mem = 0;
    spb.sg_mem = &null;
#endif

    cp = strchr(sp = buf, ':');
    if (cp == NULL)
	return NULL;

    *cp++ = '\0';
    spb.sg_name = sp;
    
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return NULL;

    *cp++ = '\0';
    spb.sg_passwd = sp;

    /* Now get list of administrators */
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return &spb;
    *cp++ = '\0';

    while ((mp = strchr(sp, ',')) != NULL)
    {
	*mp++ = '\0';

	if (*sp)
	    addtolist(&spb.sg_adm, &size_adm, &len_adm, sp);

	sp = mp;
    }
    
    if (*sp)
	addtolist(&spb.sg_adm, &size_adm, &len_adm, sp);
    
    
    /* Now get list of members */
    sp = cp;

    while ((mp = strchr(sp, ',')) != NULL)
    {
	*mp++ = '\0';

	if (*sp)
	    addtolist(&spb.sg_mem, &size_mem, &len_mem, sp);

	sp = mp;
    }
    
    if (*sp)
	addtolist(&spb.sg_mem, &size_mem, &len_mem, sp);
    
    return &spb;
}


struct sgrp *fgetsgent(FILE *fp)
{
    static char *buf = NULL;
    static int size = 0;
    int len;


    len = _nsw_getline(&buf, &size, fp);
    if (len < 0)
	return NULL;
    
    return sgetsgent(buf);
}      


void _setsgent(void)
{
    if (sfp)
	rewind(sfp);
    else
	sfp = fopen(_PATH_GSHADOW, "r");
}


void _endsgent(void)
{
    if (sfp)
	fclose(sfp);
    sfp = NULL;
}


struct sgrp *_getsgent(void)
{
    if (sfp == NULL)
	_setsgent();
    
    if (sfp == NULL)
	return NULL;

    return fgetsgent(sfp);
}


struct sgrp *_getsgnam(const char *name)
{
    struct sgrp *sp;

    _setsgent();
    
    while ((sp = _getsgent()) != NULL &&
	   strcmp(sp->sg_name, name) != 0)
	;

    _endsgent();
    
    if (sp == NULL)
	errno = 0;
    
    return sp;
}


char *sputsgent(const struct sgrp *sp)
{
    static char *buf = NULL;
    int i, len;

    
    if (sp == NULL)
	return NULL;
    
    len = (strlen(sp->sg_name) + 
	   strlen(sp->sg_passwd) + 4);

    if (sp->sg_adm)
	for (i = 0; sp->sg_adm[i]; i++)
	    len += strlen(sp->sg_adm[i]) + 1;
    
    if (sp->sg_mem)
	for (i = 0; sp->sg_mem[i]; i++)
	    len += strlen(sp->sg_mem[i]) + 1;
    
    if (Xalloc(&buf, len) == NULL)
	return NULL;

    strcpy(buf, sp->sg_name);
    strcat(buf, ":");
    strcat(buf, sp->sg_passwd);
    strcat(buf, ":");

    if (sp->sg_adm)
	for (i = 0; sp->sg_adm[i]; i++)
	{
	    strcat(buf, sp->sg_adm[i]);
	    if (sp->sg_adm[i+1])
		strcat(buf, ",");
	}
    strcat(buf, ":");
    
    if (sp->sg_mem)
	for (i = 0; sp->sg_mem[i]; i++)
	{
	    strcat(buf, sp->sg_mem[i]);
	    if (sp->sg_mem[i+1])
		strcat(buf, ",");
	}
    
    return buf;
}


int fputsgent(const struct sgrp *sp, FILE *fp)
{
    char *buf;

    
    buf = sputsgent(sp);
    if (buf == NULL)
	return -1;

    if (fprintf(fp, "%s\n", buf) < 0)
	return -1;

    return 0;
}


/* John F. Haugh II shadow-suite compatibility function */
int putsgent(const struct sgrp *sp, FILE *fp)
{
    return fputsgent(sp, fp);
}
