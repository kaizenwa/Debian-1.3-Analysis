/*
** dbm_shadow.c              DBM Shadow "map" access routines
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

#ifdef ENABLE_DBM


#include <stdio.h>
#include <errno.h>
#include <shadow.h>


void
_dbm_setspent(void)
{
}


void
_dbm_endspent(void)
{
}


struct spwd *
_dbm_getspent(void)
{
    errno = ECONNREFUSED;
    return NULL;
}


struct spwd *
_dbm_getspnam(const char *name)
{
    errno = ECONNREFUSED;
    return NULL;
}

#endif /* ENABLE_DBM */
