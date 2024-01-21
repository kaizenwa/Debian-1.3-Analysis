/* Casefunc.c - Character case-change functions.
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


#include <ctype.h>

/****************************************************************************/
/* RCS info. */

#ifndef lint
static char *RcsId = "$Id: casefunc.c,v 1.3 1996/03/17 01:08:15 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/*LINTLIBRARY*/
/****************************************************************************/
int mklower(c)
int c;
{
	/* Return c in lowercase if it is an uppercase character */

#ifdef _tolower
	return((isascii(c) && isupper(c)) ? _tolower(c) : c);
#else /* ! _tolower */
	return((isascii(c) && isupper(c)) ? tolower(c) : c);
#endif /* ! _tolower */
}
/****************************************************************************/
int mkupper(c)
int c;
{
	/* Return c in uppercase if it is a lowercase character */

#ifdef _toupper
	return((isascii(c) && islower(c)) ? _toupper(c) : c);
#else /* ! _toupper */
	return((isascii(c) && islower(c)) ? toupper(c) : c);
#endif /* ! _toupper */
}
/****************************************************************************/
