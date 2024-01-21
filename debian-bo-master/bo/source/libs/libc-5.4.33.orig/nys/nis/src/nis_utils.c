/*
** nis_utils.c
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
#include "nis_utils.h"


struct nis_attr *nis_mkattr(char *ndx, char *val, struct nis_attr *ab)
{
    static struct nis_attr sab;

    if (!ab)
	ab = &sab;

    ab->zattr_ndx = ndx;
    ab->zattr_val.zattr_val_len = strlen(val)+1;
    ab->zattr_val.zattr_val_val = val;

    return ab;
}


char *nis_access2str(u_long acc, char *str)
{
    static char sstr[17];
    int i;
    
    if (!str)
	str = sstr;

    str[i = 16] = '\0';

    while (i > 0)
    {
	i -= 4;
	str[i+0] = (acc & NIS_READ_ACC)    ? 'r' : '-';
	str[i+1] = (acc & NIS_MODIFY_ACC)  ? 'm' : '-';
	str[i+2] = (acc & NIS_CREATE_ACC)  ? 'c' : '-';
	str[i+3] = (acc & NIS_DESTROY_ACC) ? 'd' : '-';

	acc >>= 8;
    }

    return str;
}


char *nis_enflags2str(u_long flags, char *str)
{
    static char buf[256];

    if (!str)
	str = buf;

    str[0] = '\0';
    if (!(flags & EN_BINARY))
	strcat(str, "Textual Data, ");
    if (flags & EN_CRYPT)
	strcat(str, "Encrypted, ");
    if (flags & EN_XDR)
	strcat(str, "XDR Encoded, ");
    if (flags & EN_MODIFIED)
	strcat(str, "Modified Attributes, ");
    if (flags & EN_ASN1)
	strcat(str, "ASN.1 Encoded, ");

    if (str[0])
	str[strlen(str)-2] = '\0';

    return str;
}


char *nis_taflags2str(u_long flags, char *str)
{
    static char buf[256];

    if (!str)
	str = buf;

    str[0] = '\0';
    if (!(flags & TA_BINARY))
	strcat(str, "Textual Data, ");
    if (flags & TA_CRYPT)
	strcat(str, "Encrypted, ");
    if (flags & TA_XDR)
	strcat(str, "XDR Encoded, ");
    if (flags & TA_SEARCHABLE)
	strcat(str, "Searchable, ");
    if (flags & TA_CASE)
	strcat(str, "Case Sensitive, ");
    if (flags & TA_MODIFIED)
	strcat(str, "Modified Attributes, ");
    if (flags & TA_ASN1)
	strcat(str, "ASN.1 Encoded, ");

    if (str[0])
	str[strlen(str)-2] = '\0';

    return str;
}
