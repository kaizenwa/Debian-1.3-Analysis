/* Constlist.h - List of internal constants for af.
   Copyright (C) 1995, 1996, 1997 Malc Arnold.

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
static char *ConstlistId = "$Id: constlist.h,v 1.3 1997/03/05 21:23:45 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* The list of af constants by name */

static CONSTANT constants[] = {
	{ "a", c_a },
	{ "nil", c_nil },
	{ "t", c_t },
	{ "version", c_version },
	{ NULL, NULL }
};

/****************************************************************************/
