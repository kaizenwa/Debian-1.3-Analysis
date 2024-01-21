/* Killbuf.h - Declarations for kill buffer handling in af.
   Copyright (C) 1990, 1991, 1992, 1996 Malc Arnold.

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


/****************************************************************************/
/* RCS info */

#define KILLBUFID	"$Id: killbuf.h,v 1.4 1996/03/17 01:10:47 malc Exp $"

/****************************************************************************/
/* The structure to hold the kill ring */

typedef struct killbuf {
	MESSAGE *messages;			/* Messages in kill buffer */
	struct killbuf *prev, *next;		/* List pointers */
} KILLBUF;

/****************************************************************************/
