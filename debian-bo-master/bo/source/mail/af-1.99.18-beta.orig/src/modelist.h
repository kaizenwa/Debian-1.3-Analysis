/* Modelist.h - List of internal modes for af.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Malc Arnold.

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


#ifndef lint
static char *ModelistId = "$Id: modelist.h,v 1.8 1996/03/17 01:10:47 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* The list of af modes by name */

static MODE modes[] = {
	{ "Complete", "Com", M_COMPLETE },
	{ "Defining", "Def", M_DEFINING },
	{ "Mail", "Mail", M_MAIL },
	{ "Minibuffer",	"Mbuf", M_MBUF },
	{ "Narrow", "Nrw", M_NARROW },
	{ "Password", "PW", M_PASSWORD },
	{ "Pop3", "POP3", M_POP3 },
	{ "Read-Only", "RO", M_READONLY },
	{ "Typeout", "Typeout", M_TYPEOUT },
	{ "Show", "Show", M_SHOW },
	{ NULL, NULL, M_NULL }
};

/****************************************************************************/
