/* Ttable.h - Declaration of af's address translation tables
   Copyright (C) 1992, 1995, 1996 Malc Arnold.

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
static char *TtableId = "$Id: ttable.h,v 1.5 1996/03/17 01:10:47 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/*
 * This file declares a table of [state][token][lookahead] pointers to
 * functions, which are used when parsing addresses.
 *
 * The table is defined incrementally.
 */
/****************************************************************************/
/* We declare the address parsing functions here */

static ATOM *group(), *bracket(), *route(), *local(), *domain(), *proute();
static ATOM *addresses(), *pops(), *queue(), *ignore(), *error();

/****************************************************************************/
/* This table is used for tokens which are always erroneous */

static PARSEFUNC tt_error[] = {
	error, error, error, error, error, error,
	error, error, error, error, error
};

/* And this for tokens which are ignored */

static PARSEFUNC tt_ignore[] = {
	ignore, ignore, ignore, ignore, ignore, ignore,
	ignore, ignore, ignore, ignore, ignore,
};

/* And this for tokens which cause a state pop */

static PARSEFUNC tt_pop[] = {
	pops, pops, pops, pops, pops, pops,
	pops, pops, pops, pops, pops
};

/****************************************************************************/
/* State INITIAL translation rules */

static PARSEFUNC tt_i_word[] = {			/* ATOM, QSTRING */
	queue, queue, error, local, local, local,
	addresses, bracket, error, group, error
};
static PARSEFUNC tt_i_at[] = {					/* AT */
	route, error, route, error, error, error,
	error, error, error, error, error
};
static PARSEFUNC tt_i_langleb[] = {				/* LANGLEB */
	bracket, bracket, error, error, error, bracket,
	error, error, bracket, error, error
};
static PARSEFUNC tt_i_colon[] = {				/* COLON */
	group, group, error, error, error, error,
	group, group, error, error, error
};

/* The general state INITIAL parsing table */

static PARSEFUNC *tt_initial[] = {
	tt_i_word, tt_i_word, tt_error, tt_error, tt_error, tt_i_at,
	tt_ignore, tt_i_langleb, tt_error, tt_i_colon, tt_error
};

/****************************************************************************/
/* State GROUP translation rules */

static PARSEFUNC tt_g_word[] = {			/* ATOM, QSTRING */
	queue, queue, error, local, local, local,
	addresses, bracket, error, error, addresses
};

/* The general state GROUP parsing table */

static PARSEFUNC *tt_group[] = {
	tt_g_word, tt_g_word, tt_error, tt_error, tt_error, tt_error,
	tt_ignore, tt_i_langleb, tt_error, tt_error, tt_pop
};

/****************************************************************************/
/* State BRACKET translation rules */

static PARSEFUNC tt_b_word[] = {			/* ATOM, QSTRING */
	error, error, error, local, local, local,
	error, error, local, error, error
};
static PARSEFUNC tt_b_at[] = {					/* AT */
	route, error, route, error, error, error,
	error, error, error, error, error
};

/* The general state BRACKET parsing table */

static PARSEFUNC *tt_bracket[] = {
	tt_b_word, tt_b_word, tt_error, tt_error, tt_error, tt_b_at,
	tt_error, tt_error, tt_pop, tt_error, tt_error
};

/****************************************************************************/
/* State ROUTE translation rules */

static PARSEFUNC tt_r_word[] = {			/* ATOM, DLITERAL */
	error, error, error, queue, error, error,
	queue, error, error, local, error
};
static PARSEFUNC tt_r_dot[] = {					/* DOT */
	queue, error, queue, error, error, error,
	error, error, error, error, error
};
static PARSEFUNC tt_r_at[] = {					/* AT */
	queue, error, queue, error, error, error,
	error, error, error, error, error
};

static PARSEFUNC tt_r_comma[] = {				/* COMMA */
	error, error, error, error, error, queue,
	error, error, error, error, error
};

/* The general state ROUTE parsing table */

static PARSEFUNC *tt_route[] = {
	tt_r_word, tt_error, tt_r_word, tt_r_dot, tt_error, tt_r_at,
	tt_r_comma, tt_error, tt_error, tt_error, tt_error
};

/****************************************************************************/
/* State LOCAL translation rules */

static PARSEFUNC tt_l_word[] = {			/* ATOM, QSTRING */
	pops, pops, error, queue, proute, domain,
	pops, bracket, pops, error, pops
};
static PARSEFUNC tt_l_dot[] = {					/* DOT */
	queue, queue, error, error, error, error, error,
	error, bracket, error, error, error
};

/* The general state LOCAL parsing table */

static PARSEFUNC *tt_local[] = {
	tt_l_word, tt_l_word, tt_error, tt_l_dot, tt_error, tt_error,
	tt_error, tt_error, tt_error, tt_error, tt_error
};

/****************************************************************************/
/* State PROUTE translation rules */

static PARSEFUNC tt_p_word[] = {			/* ATOM, DLITERAL */
	error, error, error, queue, queue, domain,
	error, error, error, error, error
};
static PARSEFUNC tt_p_dot[] = {				/* DOT, PERCENT */
	queue, error, queue, error, error, error,
	error, error, error, error, error
};
/* The general state PROUTE parsing table */

static PARSEFUNC *tt_proute[] = {
	tt_p_word, tt_error, tt_p_word, tt_p_dot, tt_p_dot, tt_error,
	tt_error, tt_error, tt_error, tt_error, tt_error
};

/****************************************************************************/
/* State DOMAIN translation rules */

static PARSEFUNC tt_d_word[] = {			/* ATOM, DLITERAL */
	pops, pops, error, queue, error, error,
	pops, pops, pops, error, pops
};
/* The general state DOMAIN parsing table */

static PARSEFUNC *tt_domain[] = {
	tt_d_word, tt_error, tt_d_word, tt_p_dot, tt_error, tt_error,
	tt_error, tt_error, tt_error, tt_error, tt_error
};

/****************************************************************************/
/* The top-level translation table */

static PARSEFUNC **ttable[] = {
	tt_initial, tt_group, tt_bracket, tt_route,
	tt_local, tt_proute, tt_domain
};

/****************************************************************************/
