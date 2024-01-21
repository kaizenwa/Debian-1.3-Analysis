/* Funclist.h - List of internal functions for af.
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
static char *FunclistId = "$Id: funclist.h,v 1.3 1997/03/05 21:23:45 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* The argument specifications for all functions */

static ARGSPEC a_and[] = { FT_ANY | FT_REPEATS, FT_NULL };
static ARGSPEC a_defmac[] = { FT_SYMBOL, FT_STRING | FT_LIST, FT_NULL };
static ARGSPEC a_defkey[] = { FT_SYMBOL, FT_STRING,
			      FT_SYMBOL | FT_LIST, FT_NULL };
static ARGSPEC a_equal[] = { FT_ANY, FT_ANY, FT_NULL };
static ARGSPEC a_error[] = { FT_ANY | FT_REPEATS, FT_NULL };
static ARGSPEC a_getenv[] = { FT_STRING, FT_NULL };
static ARGSPEC a_if[] = { FT_ANY, FT_ANY, FT_ANY | FT_OPTIONAL, FT_NULL };
static ARGSPEC a_message[] = { FT_ANY | FT_REPEATS, FT_NULL };
static ARGSPEC a_not[] = { FT_ANY, FT_NULL };
static ARGSPEC a_or[] = { FT_ANY | FT_REPEATS, FT_NULL };
static ARGSPEC a_progn[] = { FT_ANY | FT_REPEATS, FT_NULL };
static ARGSPEC a_quote[] = { FT_ANY, FT_NULL };
static ARGSPEC a_set[] = { FT_SYMBOL, FT_ANY, FT_NULL };
static ARGSPEC a_setq[] = { FT_SYMBOL, FT_ANY, FT_NULL };
static ARGSPEC a_system[] = { FT_STRING, FT_NULL };

/****************************************************************************/
/* The list of af functions by name */

static FUNCTION functions[] = {
	{ "and", f_and, a_and, TRUE },
	{ "define-kbd-macro", f_defmac, a_defmac, FALSE },
	{ "define-key", f_defkey, a_defkey, FALSE },
	{ "equal", f_equal, a_equal, FALSE},
	{ "error", f_error, a_error, FALSE },
	{ "getenv", f_getenv, a_getenv, FALSE },
	{ "if",  f_if, a_if, TRUE },
	{ "message", f_message, a_message, FALSE },
	{ "not", f_not, a_not, FALSE },
	{ "or", f_or, a_or, TRUE },
	{ "progn", f_progn, a_progn, FALSE },
	{ "quote", f_quote, a_quote, TRUE },
	{ "set", f_set, a_set, FALSE },
	{ "setq", f_setq, a_setq, TRUE },
	{ "system", f_system, a_system, FALSE },
	{ NULL, NULL, NULL, FALSE }
};

/****************************************************************************/
