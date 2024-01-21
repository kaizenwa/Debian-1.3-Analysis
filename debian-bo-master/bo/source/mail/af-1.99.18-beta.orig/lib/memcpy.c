/* Memcpy.c - Copy a block of memory
   Copyright (C) 1995, 1996 Malc Arnold.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>

/****************************************************************************/
/* RCS info. */

#ifndef lint
static char *RcsId = "$Id: memcpy.c,v 1.3 1996/08/28 17:35:38 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/*LINTLIBRARY*/
/****************************************************************************/
char *memcpy(buf, src, len)
char *buf, *src;
int len;
{
	/* Copy len bytes from src into buf */

	char *rbuf = buf;
	int c;

	/* This is pretty simple really */

	for (c = 0; c < len; c++) {
		*buf++ = *src++;
	}
	return(rbuf);
}
/****************************************************************************/
