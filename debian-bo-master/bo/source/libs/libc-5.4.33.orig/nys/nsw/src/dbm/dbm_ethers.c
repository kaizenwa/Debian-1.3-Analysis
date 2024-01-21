/*
** dbm_ethers.c              DBM Ethers map access routines
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
*/

#include "config.h"

#ifdef ENABLE_DBM

#include <stdio.h>
#include <errno.h>
#include <ethers.h>
#include <fcntl.h>
#include <stdlib.h>

#ifdef USE_GDBM
#include <gdbm.h>
#endif

#ifndef _PATH_VARDBM
#define _PATH_VARDBM "/var/dbm"
#endif

#ifdef USE_GDBM
static GDBM_FILE dbp_byname = NULL;
static GDBM_FILE dbp_byaddr = NULL;

static datum dkey, dval;
#endif


void
_dbm_setethent(void)
{
#ifdef USE_GDBM
    char path[1025];
    

    if (dbp_byaddr == NULL)
    {
	sprintf(path, "%s/ethers.byaddr", _PATH_VARDBM);
	dbp_byaddr = gdbm_open(path, 0, O_RDONLY, 0, NULL);
    }

    if (dbp_byname == NULL)
    {
	sprintf(path, "%s/ethers.byname", _PATH_VARDBM);
	dbp_byname = gdbm_open(path, 0, O_RDONLY, 0, NULL);
    }

    if (dkey.dptr)
    {
	free(dkey.dptr);
	dkey.dptr = NULL;
    }

    if (dval.dptr)
    {
	free(dval.dptr);
	dval.dptr = NULL;
    }
#endif
}


void
_dbm_endethent(void)
{
#ifdef USE_GDBM
    if (dbp_byaddr)
    {
	gdbm_close(dbp_byaddr);
	dbp_byaddr = NULL;
    }

    if (dbp_byname)
    {
	gdbm_close(dbp_byname);
	dbp_byname = NULL;
    }
#endif
}


struct ether *
_dbm_getethent(void)
{
#ifdef USE_GDBM
    datum nkey;

    
    if (dval.dptr)
    {
	free(dval.dptr);
	dval.dptr = NULL;
    }
	    
    if (dkey.dptr == NULL)
	nkey = gdbm_firstkey(dbp_byname);
    else
    {
	nkey = gdbm_nextkey(dbp_byname, dkey);
	free(dkey.dptr);
    }

    dkey = nkey;
    
    if (dkey.dptr == NULL)
	return NULL;
    
    dval = gdbm_fetch(dbp_byname, dkey);

    if (dval.dptr == NULL)
	return NULL;
    
    return (struct ether *) dval.dptr;
#else
    errno = ECONNREFUSED;
    return NULL;
#endif
}


struct ether *
_dbm_getethbyname(const char *name)
{
#ifdef USE_GDBM
    datum dkey;

    dkey.dptr = (char *) name;
    dkey.dsize = strlen(name);

    if (dval.dptr)
    {
	free(dval.dptr);
	dval.dptr = NULL;
    }
	    
    dval = gdbm_fetch(dbp_byname, dkey);
    
    return (struct ether *) dval.dptr;
#else
    errno = ECONNREFUSED;
    return NULL;
#endif
}


struct ether *
_dbm_getethbyaddr(const struct ether_addr *addr)
{
#ifdef USE_GDBM
    datum dkey;

    dkey.dptr = (char *) addr;
    dkey.dsize = sizeof(*addr);

    if (dval.dptr)
    {
	free(dval.dptr);
	dval.dptr = NULL;
    }
	    
    dval = gdbm_fetch(dbp_byname, dkey);
    
    return (struct ether *) dval.dptr;
#else
    errno = ECONNREFUSED;
    return NULL;
#endif
}

#endif /* ENABLE_DBM */
