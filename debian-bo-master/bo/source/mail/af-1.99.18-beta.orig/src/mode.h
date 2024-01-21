/* Mode.h - Declarations for mode handling in af.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996 Malc Arnold.

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

#define MODEID		"$Id: mode.h,v 1.14 1996/03/17 01:10:47 malc Exp $"

/****************************************************************************/
/* The modes used in af */

#define M_NULL		0x0000
#define M_MAIL		0x0001
#define M_TYPEOUT	0x0002
#define M_MBUF		0x0004
#define M_READONLY	0x0010
#define M_DEFINING	0x0020
#define M_NARROW	0x0040
#define M_POP3		0x0080
#define M_SHOW		0x0100
#define M_COMPLETE	0x0200
#define M_PASSWORD	0x0400
#define M_BINARY	0x0800

/* And the first and last mode values */

#define M_FIRST		0x0001
#define M_LAST		0x0800

/****************************************************************************/
/* The major mode of a set of modes */

#define MAJOR(m)	((m) & 0077)

/****************************************************************************/
/* The structure to hold the details of a mode */

typedef struct {
	char *name, *disp;		/* Real and displayed names */
	unsigned mode;			/* Value indicating mode active */
} MODE;

/****************************************************************************/
