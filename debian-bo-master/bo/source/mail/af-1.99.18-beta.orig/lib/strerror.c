/* Strerror.c - Return the text relating to an error.
   Copyright (C) 1992, 1996 Malc Arnold.

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
static char *RcsId = "$Id: strerror.c,v 1.2 1996/03/17 01:08:15 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/*LINTLIBRARY*/
/****************************************************************************/
/* Import the numbr of errors and the list of error messages */

extern int sys_nerr;
extern char *sys_errlist[];

/****************************************************************************/
char *strerror(errno)
int errno;
{
	/* Return the error message associated with errno */

	return((errno > sys_nerr) ? NULL : sys_errlist[errno]);
}
/****************************************************************************/
