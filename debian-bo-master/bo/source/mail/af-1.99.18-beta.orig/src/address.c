/* Address.c - Address checking and translation for af.
   Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

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
#include "af.h"
#include "atom.h"
#include "address.h"
#include "ttable.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: address.c,v 1.22 1997/04/20 10:32:45 malc Exp $";
static char *AddressId = ADDRESSID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc(), *xstrdup();
extern char *vstrcat(), *get_user(), *atext();
extern int strcasecmp();
extern void free(), afree(), unalias();
extern ATOM *tokenise(), *find_token(), *asearch();
extern ATOM *acut(), *adiscard(), *acopy(), *acat();
extern ATOM *acomment(), *aquote(), *add_atom(), *asplit();

#ifdef AFACK
extern char *get_host();
#else /* ! AFACK */
extern char *get_vtext();
#endif /* ! AFACK */

/* Local function declarations */

char *grp_text();
void free_glist(), free_alist(), translate();
GROUP *aparse();
static char *rparse(), *ref_text(), *set_addrs();
static char *set_canon(), *set_some(), *grp_names();
static char *grp_canon(), *addr_text(), *addr_canon();
static char *name_canon(), *local_domain();
static int uucp_to_rfc();
static void add_group(), add_address();
static void set_domain(), check_null();
static void comment_to_phrase();
static void rfc_to_percent();
static ATOM *pop_addresses();
static GROUP *remove_dups();

#ifdef MTA_NEEDS_ARGS
#ifdef MTA_NEEDS_UUCP
static char *uucp_text(), *uucp_route(), *uucp_proute();
#endif /* MTA_NEEDS_UUCP */
#endif /* MTA_NEEDS_ARGS */

/****************************************************************************/
/* Import the error flag and text for parsing and address translation */

extern int a_errno;
extern char *a_errtext;

/****************************************************************************/
/* This variable stores the list of names for the address list */

static char *a_realnames = NULL;

/****************************************************************************/
/* The parsing variables, stored as statics */

static int state = ST_INITIAL;			/* Current state */
static int in_group = FALSE;			/* In a group */
static int in_bracket = FALSE;			/* In a <..> */

static ATOM *start = NULL;	       		/* First unparsed token */
static ATOM *lookahead = NULL;			/* Lookahead token */

/****************************************************************************/
/* The variables in which to store the list we are building */

static GROUP *grp_list = NULL;			/* Overall list */
static GROUP *grp = NULL;			/* Current group */
static ADDRESS *addr = NULL;			/* Current address */

/****************************************************************************/
char *alias(addrs)
char *addrs;
{
	/*
	 * Form an RFC 822 address list from the string passed in
	 * addrs, and return it in a newly-allocated string.
	 *
	 * All addresses are converted to RFC 822, if not already
	 * in that format, and any aliases are expanded.
	 */

	return(set_addrs(addrs, TRUE, AC_TRIM));
}
/****************************************************************************/
char *canonical(addrs)
char *addrs;
{
	/*
	 * Form an RFC 822 address list from the string passed in
	 * addrs, and return it in a newly-allocated string.
	 * 
	 * All addresses are converted to canonical RFC 822, if not
	 * already in that format, but aliases are not expanded.
	 */

	return(set_addrs(addrs, FALSE, AC_FULL));
}
/****************************************************************************/
char *some_addresses(addrs, ignored)
char *addrs, *ignored;
{
	/*
	 * Takes two previously checked and canonicalised address
	 * lists and subtracts the second from the first, returning
	 * the new addresses in an allocated string
	 */

	return(set_some(addrs, ignored));
}
/****************************************************************************/
char *mail_addresses(addrs)
char *addrs;
{
	/*
	 * Form an RFC 822 address list from the string passed in
	 * addrs, and return it in a newly-allocated string.
	 *
	 * All addresses are converted to canonical RFC 822, if not
	 * already in that format, but aliases are not expanded,
	 * and full names are discarded.
	 */

	return(set_canon(addrs, FALSE));
}
/****************************************************************************/
char *local_part(addrs)
char *addrs;
{
	/*
	 * Return the local part of the first address in addrs, and
	 * return it in a newly-allocated string.
	 *
	 * The address is converted to canonical RFC 822, if not
	 * already in that format, but aliases are not expanded.
	 */

	return(set_canon(addrs, TRUE));
}
/****************************************************************************/
int count_addresses(addrs)
char *addrs;
{
	/* Return the number of addresses specified in addrs */

	int naddrs = 0;
	ATOM *alist;
	GROUP *glist, *g;
	ADDRESS *a;

	/* Tokenise the address list and terminate with a comma */

	if ((alist = tokenise(addrs)) != NULL) {
		alist = add_atom(alist, ",", AT_COMMA);
	}

	/* Parse the atom list and convert to RFC 822 */

	if (alist != NULL && (glist = aparse(alist)) != NULL) {
		/* Loop over the available groups looking for addresses */

		for (g = glist; g != NULL; g = g->next) {
			for (a = g->addresses; a != NULL; a = a->next) {
				naddrs++;
			}
		}

		/* And free the group list */

		free_glist(glist);
	}

	/* Return the address count */

	return(naddrs);
}
/****************************************************************************/
char *addrnames()
{
	/* Return the real names of the last addresses parsed */

	return(a_realnames);
}
/****************************************************************************/
char *realname(name)
char *name;
{
	/* Return a string containing name, or NULL if name is invalid */

	char *buf;
	ATOM *alist, *a;

	/* Tokenise the name */

	if ((alist = tokenise(name)) == NULL) {
		return(NULL);
	}

	/* Names can only contain white space, atoms or quoted strings */

	for (a = alist; a != NULL; a = a->next) {
		if (!IS_WS(a) && a->type != AT_ATOM
		    && a->type != AT_QSTRING) {
			alist = aquote(alist);
			break;
		}
	}

	/* Return the trimmed string */

	buf = atext(NULL, alist, AC_TRIM);
	afree(alist);

	return(buf);
}
/****************************************************************************/
char *get_addr()
{
	/* Return the user's real mail address in a static buffer */

	static char *addr = NULL;
	char *username, *domain;

	/* Free any old return buffer */

	if (addr != NULL) {
		free(addr);
	}

	/* Get the real user and domain names */

	username = get_user();
	domain = local_domain();

	/* Now build and return the address */

	addr = vstrcat(username, "@", domain, NULL);
	return(addr);
}
/****************************************************************************/
#ifdef MTA_NEEDS_ARGS
char **addr_args(args, addrs)
char **args, *addrs;
{
	/* Append each address in addrs to the argument vector args */

	int no_args, no_addrs;
	ATOM *alist;
	GROUP *glist, *g;
	ADDRESS *a;

	/* Tokenise the address list and terminate with a comma */

	if ((alist = tokenise(addrs)) != NULL) {
		alist = add_atom(alist, ",", AT_COMMA);
	}

	/* Parse the atom list and convert to RFC 822 */

	if (alist != NULL && (glist = aparse(alist)) != NULL) {
		/* Count the original number of arguments in the vector */

		for (no_args = 0; args[no_args] != NULL; no_args++) {
			/* NULL LOOP */
		}

		/* Count the number of addresses to be added */

		no_addrs = 0;
		for (g = glist; g != NULL; g = g->next) {
			for (a = g->addresses; a != NULL; a = a->next) {
				no_addrs++;
			}
		}

		/* Reallocate the argument vector */

		args = (char **) xrealloc(args, (no_args + no_addrs + 1)
					  * sizeof(char *));

		/* Now add each address as an argument */

		for (g = glist; g != NULL; g = g->next) {
			for (a = g->addresses; a != NULL; a = a->next) {
#ifdef MTA_NEEDS_UUCP
				args[no_args++] = uucp_text(a);
#else /* ! MTA_NEEDS_UUCP */
				args[no_args++] = addr_text(NULL, a, AC_FULL);
#endif /* ! MTA_NEEDS_UUCP */
			}
		}

		/* Add the terminating NULL to the vector */

		args[no_args] = NULL;

		/* Free the group list */

		free_glist(glist);
	} else if (a_errno == AERR_NONE) {
		a_errno = AERR_NULL;
	}

	/* Return the modified argument list */

	return(args);
}
#endif /* MTA_NEEDS_ARGS */
/****************************************************************************/
char *references(refs)
char *refs;
{
	/*
	 * Return the canonical form of the references given in ref,
	 * or NULL if ref does not contain valid references.
	 */

	return(rparse(refs, TRUE));
}
/****************************************************************************/
char *refextract(refs)
char *refs;
{
	/*
	 * Extract as many references as possible from those given in
	 * ref.  Returns the text of the references or NULL if none
	 * are found.
	 */

	return(rparse(refs, FALSE));
}
/****************************************************************************/
static char *rparse(refs, strict)
char *refs;
int strict;
{
	/* 
	 * Canonicalise the list of references, returning the canonical
	 * text for the reference list, or NULL if the list is not valid.
	 */

	char *buf = NULL;
	ATOM *alist, *next, *end;

	/* Tokenise the reference string */

	if ((alist = tokenise(refs)) == NULL) {
		return(NULL);
	}

	/* Loop through the list and check the tokens */

	while (alist != NULL) {
		/* Now process the atom according to type */

		if (alist->type == AT_LANGLEB) {
			/* Find the closing angle bracket */

			if ((end = asearch(alist, AT_RANGLEB)) == NULL) {
				/* Invalid angle brackets; fail */

				a_errno = RERR_BRACKET;
				if (a_errtext != NULL) {
					free(a_errtext);
					a_errtext = NULL;
				}
				afree(alist);
				if (buf != NULL) {
					free(buf);
				}
				return(NULL);
			}

			/* Now cut out the tokens of the reference */

			next = end->next;
			end->next = NULL;

			/* Canonicalise the reference */

			if ((buf = ref_text(buf, alist, strict)) == NULL
			    && strict) {
				/* Bad reference; clean up and fail */

				if (buf != NULL) {
					free(buf);
				}
				afree(next);
				return(NULL);
			}

			/* And update the atom list */

			alist = next;
		} else if (!strict || IS_WS(alist) || alist->type == AT_ATOM
			   || alist->type == AT_QSTRING) {
			/* Add this atom to the buffer */

			next = alist->next;
			alist->next = NULL;
			buf = atext(buf, alist, AC_NONE);

			/* And move on to the next atom */

			afree(alist);
			alist = next;
		} else {
			/* This token is invalid in a reference */

			afree(alist->next);
			alist->next = NULL;
			a_errno = RERR_TOKEN;
			if (a_errtext != NULL) {
				free(a_errtext);
			}
			a_errtext = atext(NULL, alist, AC_NONE);

			/* Free space and return failure */

			if (buf != NULL) {
				free(buf);
			}
			afree(alist);
			return(NULL);
		}
	}

	/* Return the generated buffer */

	return(buf);
}
/****************************************************************************/
static char *ref_text(buf, alist, strict)
char *buf;
ATOM *alist;
int strict;
{
	/* Return the canonical form of a reference */

	char *cref, *new_buf;
	char *errtext;
	GROUP *glist;

	/* Set the error text; we may need it */

	errtext = atext(NULL, alist, AC_NONE);

	/* First check that the reference is valid */

	if (strict && asearch(alist, AT_AT) == NULL) {
		a_errno = RERR_REFERENCE;
		if (a_errtext != NULL) {
			free(a_errtext);
		}
		a_errtext = errtext;
		afree(alist);
		return(NULL);
	}

	/* Terminate the reference with a comma */

	alist = add_atom(alist, ",", AT_COMMA);

	/* Get the canonical form of the reference */

	if ((glist = aparse(alist)) != NULL) {
		/* Now build the reference text */

		cref = grp_text(glist, AC_FULL);
		free_glist(glist);

		/* And add it to the buffer */

		new_buf = (buf != NULL) ? vstrcat(buf, "<", cref, ">", NULL)
					: vstrcat("<", cref, ">", NULL);

		/* Clean up and return */

		free(buf);
		free(cref);
		free(errtext);
		return(new_buf);
	} else if (strict) {
		/* Error in the reference; fail */

		a_errno = RERR_REFERENCE;
		if (a_errtext != NULL) {
			free(a_errtext);
		}
		a_errtext = errtext;
		return(NULL);
	}

	/* Now return the buffer */

	return(buf);
}
/****************************************************************************/
static char *set_addrs(addrs, aliasing, canon)
char *addrs;
int aliasing, canon;
{
	/*
	 * Actually handle address translation, canonicalisation
	 * and checking.  Does aliasing if the parameter is TRUE,
	 * and uses the canonicalisation level set in canon.
	 */

	char *new_addrs = NULL;
	ATOM *alist;
	GROUP *glist;

	/* No real names associated with this list */

	if (a_realnames != NULL) {
		free(a_realnames);
		a_realnames = NULL;
	}

	/* Tokenise the address list and terminate with a comma */

	if ((alist = tokenise(addrs)) != NULL) {
		alist = add_atom(alist, ",", AT_COMMA);
	}

	/* Parse the atom list and convert to RFC 822 */

	if (alist != NULL && (glist = aparse(alist)) != NULL) {
		if (aliasing) {
			unalias(glist, NULL);
		}
		translate(glist);
		new_addrs = grp_text(glist, canon);
		a_realnames = grp_names(glist);
		free_glist(glist);
	} else if (a_errno == AERR_NONE) {
		a_errno = AERR_NULL;
	}

	return(new_addrs);
}
/****************************************************************************/
static char *set_canon(addrs, first_local)
char *addrs;
int first_local;
{
	/*
	 * Handle address translation into the canonical form of the
	 * address, with names, routes, and comments stripped.  If
	 * first_local is set, the canonical local-part of the first
	 * address is required.
	 */

	char *new_part = NULL;
	ATOM *alist;
	GROUP *glist;

	/* Tokenise the address list and terminate with a comma */

	if ((alist = tokenise(addrs)) != NULL) {
		alist = add_atom(alist, ",", AT_COMMA);
	}

	/* Parse the atom list and convert to RFC 822 */

	if (alist != NULL && (glist = aparse(alist)) != NULL) {
		translate(glist);
		new_part = grp_canon(glist, first_local);
		free_glist(glist);
	} else if (a_errno == AERR_NONE) {
		a_errno = AERR_NULL;
	}
	return(new_part);
}
/****************************************************************************/
static char *set_some(addrs, ignored)
char *addrs, *ignored;
{
	/*
	 * Actually handle subtracting any addresses in one
	 * list from another.  Assumes that both lists are
	 * valid.
	 */

	char *new_addrs = NULL;
	GROUP *glist, *i_glist, *g;
	ADDRESS *a;

	/* Tokenise and parse the address lists */

	glist = aparse(add_atom(tokenise(addrs), ",", AT_COMMA));
	i_glist = aparse(add_atom(tokenise(ignored), ",", AT_COMMA));

	/* Remove all addresses in the ignored group */

	for (g = i_glist; g != NULL; g = g->next) {
		for (a = g->addresses; a != NULL; a = a->next) {
			glist = remove_dups(glist, a, FALSE);
		}
	}

	/* Generate the text of the new group */

	new_addrs = (glist != NULL) ? grp_text(glist, AC_FULL) : NULL;

	/* Free the space and return the new addresses */

	free_glist(glist);
	free_glist(i_glist);

	return(new_addrs);
}
/****************************************************************************/
GROUP *aparse(alist)
ATOM *alist;
{
	/* Actually parse a previously-tokenised list of atoms */

	ATOM *token;

	/* Start the parse in the initial state */

	state = ST_INITIAL;
	in_group = in_bracket = FALSE;
	grp_list = grp = NULL;
	addr = NULL;

	/* Set up the parse variables */

	start = alist;
	if ((token = find_token(alist)) == NULL) {
		afree(alist);
		return(NULL);
	}

	/* Scan through the list, handling tokens as we can */

	while (token != NULL) {
		/* Find the lookahead token */

		if ((lookahead = find_token(token->next)) == NULL) {
			/* Check for error at end of string */

			if (ttable[state][token->type] == tt_error) {
				(void) error(NULL);
				return(NULL);
			}
			break;
		}

		/*
		 * Call the function determined by the current state,
		 * the type of the current token, and the type of the
		 * lookahead token, and collect the next unparsed token.
		 */

		token = ttable[state][token->type][lookahead->type](token);
	}

	/* Free any trailing tokens in the list */

	afree(start);

	/* Delete NULL addresses */

	check_null();
	return(grp_list);
}
/****************************************************************************/
static ATOM *group(token)
ATOM *token;
{
	/* We have encountered the start of a group */

	/* Set the state */

	state = ST_GROUP;
	in_group = TRUE;

	/* Add the new group */

	add_group();

	/* Set the group name and the parse positions */

	if (token->type != AT_COLON) {
		grp->name = acut(start, lookahead);
		start = adiscard(lookahead, lookahead);
		token = find_token(start);
	} else {
		start = adiscard(start, token);
		token = lookahead;
	}

	return(token);
}
/****************************************************************************/
static ATOM *bracket(token)
ATOM *token;
{
	/* Encountered the start of a bracket address */

	int badname = FALSE;

	/* Possibly came from state LOCAL, may be erroneous */

	if (state == ST_LOCAL) {
		if (in_bracket) {
			return(error(token));
		}
		badname = TRUE;
	}

	/* Set the state */

	state = ST_BRACKET;
	in_bracket = TRUE;

	/* Add the address if  required */

	if (addr == NULL || addr->local != NULL) {
		add_address();
	}

	/* Set the address name and the parse positions */

	if (lookahead->type == AT_LANGLEB) {
		addr->name = acut(start, lookahead);
		if (badname) {
			addr->name = aquote(addr->name);
		}
		start = adiscard(lookahead, lookahead);
		token = find_token(start);
	} else {
		start = adiscard(start, token);
		token = lookahead;
	}

	return(token);
}
/****************************************************************************/
/*ARGSUSED*/
static ATOM *route(token)
ATOM *token;
{
	/* Encountered the start of a route */

	/* Add the address if  required */

	if (addr == NULL || addr->local != NULL) {
		add_address();
	}

	/* Set the state */

	state = ST_ROUTE;
	return(lookahead);
}
/****************************************************************************/
static ATOM *local(token)
ATOM *token;
{
	/* Encountered the start of a local-part */

	/* Set the state */

	state = ST_LOCAL;

	/* Push any stacked tokens as addresses or a route */

	if (lookahead->type == AT_COLON) {
		addr->route = acut(start, lookahead);
		start = adiscard(lookahead, lookahead);
		token = find_token(start);
	} else if (!in_bracket) {
		if ((start = pop_addresses(token)) == NULL) {
			return(NULL);
		}
		if (addr == NULL || addr->local != NULL) {
			add_address();
		}
	}

	return(token);
}
/****************************************************************************/
static ATOM *proute(token)
ATOM *token;
{
	/* Encountered an RFC 733 route */

	/* Set the state */

	state = ST_PROUTE;

	/* Push any stacked tokens as a local-part */

	addr->local = acut(start, lookahead);
	start = lookahead;
	token = find_token(start);

	return(token);
}
/****************************************************************************/
static ATOM *domain(token)
ATOM *token;
{
	/* Encountered the start of a domain */

	/* Push any stacked tokens as a local-part or route */

	if (state == ST_PROUTE) {
		addr->proute = acut(start, lookahead);
	} else {
		addr->local = acut(start, lookahead);
	}
	start = adiscard(lookahead, lookahead);
	token = find_token(start);

	/* Set the state */

	state = ST_DOMAIN;
	return(token);
}
/****************************************************************************/
static ATOM *addresses(token)
ATOM *token;
{
	/*
	 * Encountered a comma or semicolon;
	 * add any stacked tokens as addresses
	 */

	/* Process the addresses */

	if ((start = token = pop_addresses(lookahead)) == NULL) {
		return(NULL);
	}

	/*
	 * If the lookahead token is a semi-colon we must preserve it
	 * so that the state pop to ST_GROUP will work OK.  Otherwise
	 * the lookahead token can be discarded.
	 */

	if (lookahead->type != AT_SEMI) {
		start = adiscard(start, start);
		token = find_token(start);
	}

	return(token);
}
/****************************************************************************/
static ATOM *pops(token)
ATOM *token;
{
	/* Restore the previous state, or clean up at end-of-list */

	/* Handle incomplete tokens */

	switch (state) {
	case ST_LOCAL:
		/* Terminate the address, handling UUCP addresses */

		addr->local = acut(start, lookahead);
		start = token = lookahead;

		if (!uucp_to_rfc(addr)) {
			return(NULL);
		}
		break;
	case ST_DOMAIN:
		/* Terminate the address */

		addr->domain = acut(start, lookahead);
		start = token = lookahead;
		break;
	case ST_BRACKET:
		/* Discard any right angle bracket */

		if (token->type == AT_RANGLEB) {
			start = adiscard(start, token);
			token = lookahead;
		}

		/* Check that there is an address, if not discard */

		if (addr->local == NULL) {
			afree(addr->name);
			addr->name = NULL;
		}
		break;
	case ST_GROUP:
		/* Discard any semicolon */

		if (token->type == AT_SEMI) {
			start = adiscard(start, token);
			token = lookahead;
		}
		break;
	}

	/* Restore the previous state */

	if (in_bracket && state != ST_BRACKET) {
		state = ST_BRACKET;
	} else if (in_group && state != ST_GROUP) {
		state = ST_GROUP;
		in_bracket = FALSE;
	} else {
		state = ST_INITIAL;
		in_group = in_bracket = FALSE;
	}

	return(token);
}
/****************************************************************************/
/*ARGSUSED*/
static ATOM *queue(token)
ATOM *token;
{
	/* Queue a token until we find out what to do with it */

	return(lookahead);
}
/****************************************************************************/
static ATOM *ignore(token)
ATOM *token;
{
	/* Discard the current token */

	start = adiscard(start, token);
	return(lookahead);
}
/****************************************************************************/
/*ARGSUSED*/
static ATOM *error(token)
ATOM *token;
{
	/* Error in parsing address */

	char *buf = NULL;

	/* Set the error flag */

	switch (state) {
	case ST_BRACKET:
		a_errno = AERR_BRACKET;
		break;
	case ST_ROUTE:
	case ST_PROUTE:
		a_errno = AERR_ROUTE;
		break;
	case ST_LOCAL:
		a_errno = AERR_LOCAL;
		break;
	case ST_DOMAIN:
		a_errno = AERR_DOMAIN;
		break;
	default:
		a_errno = AERR_ADDRESS;
		break;
	}

	/* Set the error text */

	if (start != NULL) {
		/* If the lookahead token is a trailing comma then strip it */

		if (lookahead == NULL) {
			token = NULL;
		} else if (lookahead->type == AT_COMMA
			   && lookahead->next == NULL) {
			start = adiscard(start, lookahead);
			token = NULL;
		} else {
			token = lookahead->next;
		}

		start = acut(start, token);
		buf = atext(NULL, start, AC_NONE);
	} else if (addr != NULL && addr->name != NULL) {
		buf = atext(NULL, addr->name, AC_TRIM);
	}

	/* Set the address text for the error */

	if (a_errtext != NULL) {
		free(a_errtext);
	}
	a_errtext = (buf != NULL) ? xstrdup(buf) : xstrdup(END_ERRTEXT);

	/* Free all the tokens */

	free_glist(grp_list);
	grp_list = NULL;
	addr = NULL;

	afree(start);
	start = NULL;

	return(NULL);
}
/****************************************************************************/
static ATOM *pop_addresses(token)
ATOM *token;
{
	/* Build addresses from tokens from start up to token */

	ATOM *a, *b, *next;

	/* Add each address in the token list */

	a = start;
	while (a != token) {
		if (!IS_WS(a)) {
			/* Find the next address */

			next = token;
			for (b = a->next; b != token; b = b->next) {
				if (!IS_WS(b)) {
					next = a->next;
					break;
				}
			}

			/* Add the address */

			add_address();
			addr->local = acut(start, next);
			start = a = next;

			/* Convert UUCP addresses specified */

			if (!uucp_to_rfc(addr)) {
				return(NULL);
			}
		} else {
			a = a->next;
		}
	}

	return(token);
}
/****************************************************************************/
static int uucp_to_rfc(address)
ADDRESS *address;
{
	/*
	 * Convert UUCP addresses to RFC 822 format.
	 * Return TRUE on success, FALSE if the address is erroneous.
	 */

	char *pling = NULL, *errtext;
	ATOM *a, *alist, *next, *rt = NULL;
	ATOM *lcl = NULL, *dmn = NULL;

	/* Set the text for an error */

	errtext = atext(NULL, address->local, AC_FULL);

	/* Initialise for the translation */

	a = alist = address->local;
	address->local = NULL;

	/* Loop through the local-part handling each pling found */

	while (alist != NULL) {
		if (a == NULL || a->type == AT_ATOM &&
		    (pling = strchr(a->text, '!')) != NULL) {
			/* Split the atom about the pling */

			if (a == NULL) {
				next = NULL;
			} else if ((next = asplit(alist, a, pling)) == NULL) {
				(void) error(NULL);

				/* This error has a special-case type */

				a_errno = AERR_UUCP;
				free(a_errtext);
				a_errtext = errtext;

				/* Free all the tokens */

				afree(rt);
				afree(lcl);
				afree(dmn);
				afree(alist);

				/* Return the error flag */

				return(FALSE);
			}

			/* Handle the elements */

			if (dmn != NULL) {
				rt = add_atom(rt, "@", AT_AT);
				rt = acat(rt, dmn);
			}
			if (lcl != NULL) {
				dmn = lcl;
			}
			lcl = alist;
			a = alist = next;
		} else {
			a = a->next;
		}
	}

	/* Now set the address */

	address->route = rt;
	address->local = lcl;
	address->domain = dmn;

	/* Free the (unused) error text */

	free(errtext);
	return(TRUE);
}
/****************************************************************************/
static void add_group()
{
	/* Add a new group to the list of groups */

	/* Allocate the space for the new group */

	if (grp_list == NULL) {
		grp_list = grp = (GROUP *) xmalloc(sizeof(GROUP));
	} else {
		grp->next = (GROUP *) xmalloc(sizeof(GROUP));
		grp = grp->next;
	}

	/* Initialise the group */

	grp->name = grp->comment = NULL;
	grp->addresses = addr = NULL;
	grp->next = NULL;

	return;
}
/****************************************************************************/
static void add_address()
{
	/* Add a new address to the list of addresses */

	/* If there is no group then we need to create one */

	if (grp_list == NULL) {
		add_group();
	}

	/* Allocate the space for the new address */

	if (addr == NULL) {
		grp->addresses = addr = (ADDRESS *) xmalloc(sizeof(ADDRESS));
	} else {
		addr->next = (ADDRESS *) xmalloc(sizeof(ADDRESS));
		addr = addr->next;
	}

	/* Initialise the address */

	addr->name = addr->route = NULL;
	addr->local = addr->proute = addr->domain = NULL;
	addr->next = NULL;

	return;
}
/****************************************************************************/
static void check_null()
{
	/* Remove the current address and/or group if empty */

	GROUP *g;
	ADDRESS *a;

	/* Remove the address if NULL */

	if (addr != NULL && addr->local == NULL) {
		/* Free the group addresses if required */

		if (addr == grp->addresses) {
			free_alist(addr);
			grp->addresses = NULL;
		} else {
			for (a = grp->addresses; a != NULL; a = a->next) {
				if (a->next == addr) {
					a->next = NULL;
					break;
				}
			}
			free_alist(addr);
			return;			/* Group can't be null */
		}
	}

	/* Now check the group similarly */

#ifdef NO_MTA_GROUPS
	if (grp != NULL && grp->addresses == NULL) {
#else /* ! NO_MTA_GROUPS */
	if (grp != NULL && grp->name == NULL && grp->addresses == NULL) {
#endif /* ! NO_MTA_GROUPS */
		/* Free the group list if required */

		if (grp == grp_list) {
			grp_list = NULL;
		} else {
			for (g = grp_list; g != NULL; g = g->next) {
				if (g->next == grp) {
					g->next = NULL;
					break;
				}
			}
		}
		free_glist(grp);
	}

	return;
}
/****************************************************************************/
void translate(glist)
GROUP *glist;
{
	/*
	 * Convert the addresses and groups in the list to strict
	 * RFC 822 form, as modified by compile options (MTAs often
	 * don't handle RFC 822 groups or routes, and hence we must
	 * use RFC 733 ones).
	 */

	GROUP *g;
	ADDRESS *a;

	/* Loop through each available group */

	for (g = glist; g != NULL; g = g->next) {

#ifdef NO_MTA_GROUPS
		/* Turn any group name into a comment */

		if (g->name != NULL) {
			g->comment = acomment(g->name);
			g->name = NULL;
		}
#endif /* NO_MTA_GROUPS */

		/* Loop through each address in the group */

		for (a = g->addresses; a != NULL; a = a->next) {
			/* Handle malc@thing (Malc Arnold) format */

			if (a->name == NULL) {
				comment_to_phrase(a);
			}

			/* Convert RFC 822 routes to RFC 733 */

			if (a->route != NULL) {
				rfc_to_percent(a);
			}

			/* Fix any local mail with no domain specified */

			if (a->domain == NULL) {
				set_domain(a);
			}
		}
	}

	/* Now remove duplicate addresses within the list */

	for (g = glist; g != NULL; g = g->next) {
		for (a = g->addresses; a != NULL; a = a->next) {
			glist = remove_dups(glist, a, TRUE);
		}
	}

	return;
}
/****************************************************************************/
static GROUP *remove_dups(glist, address, save_names)
GROUP *glist;
ADDRESS *address;
int save_names;
{
	/*
	 * Loop through the addresses in glist, deleting any
	 * which are equivalent to address.  Return the
	 * updated group list.
	 */

	char *a_canon, *buf;
	GROUP *g, *prev_grp = NULL;
	ADDRESS *a, *prev_addr = NULL;

	/* Get the canonical form of the original address */

	a_canon = addr_canon(NULL, address);

	/* Initialise the loop variables */

	g = glist;
	a = (g != NULL) ? g->addresses : NULL;

	/* Loop through each address checking for duplicates */

	while (a != NULL) {
		/* Get the canonical address to be compared */

		buf = addr_canon(NULL, a);

		/*
		 * Check for equivalence (case-independent),
		 * ignoring the message we're checking against.
		 */

		if (a != address && !strcmp(a_canon, buf)) {
			/* Preserve any name if required */

			if (save_names && address->name == NULL
					&& a->name != NULL) {
				address->name = a->name;
				a->name = NULL;
			}

			/* Remove the duplicate address */

			if (prev_addr != NULL) {
				/* Remove the address from the list */

				prev_addr->next = a->next;
				a->next = NULL;
				free_alist(a);
			} else if ((g->addresses = a->next) == NULL) {
				/* Removed all addresses in group */

				if (prev_grp != NULL) {
					/* Remove the group from the list */

					prev_grp->next = g->next;
					g->next = NULL;
					free_glist(g);
					g = prev_grp->next;
				} else {
					/* Update glist if we can */

					if ((glist = g->next) == NULL) {
						/* Removed last address */

						return(NULL);
					}
					free_glist(g);
					g = glist;
				}
			}

			/* Move a on to the next address */

			a = (prev_addr != NULL) ? prev_addr->next :
				(g != NULL) ? g->addresses : NULL;
		} else {
			/* Check the next address */

			prev_addr = a;
			a = a->next;
		}

		/* Free the buffer */

		free(buf);

		/* May need to check next group */

		if (a == NULL && g->next != NULL) {
			prev_grp = g;
			prev_addr = NULL;
			g = g->next;
			a = g->addresses;
		}
	}

	/* Free the canonical name and return the modified list */

	free(a_canon);
	return(glist);
}
/****************************************************************************/
static void comment_to_phrase(address)
ADDRESS *address;
{
	/*
	 * Convert a trailing comment into a phrase.  This handles
	 * old style "malc@thing (Malc Arnold)" addresses, converting
	 * them into RFC 822 phrase-addresses "Malc Arnold <malc@thing>"
	 */

	char *phrase;
	int quote = FALSE;
	ATOM *rhs, *name;
	ATOM *qname, *a;

	/* Which section of the address is the rightmost? */

	rhs = (address->domain != NULL) ? address->domain : address->local;

	/* Do we have a single trailing comment? */

	if ((name = asearch(rhs, AT_COMMENT)) != NULL
	    && asearch(name->next, AT_COMMENT) == NULL
	    && find_token(name->next) == NULL) {
		/* Extract the comment from the address */

		for (a = rhs; a->next != name; a = a->next) {
			/* NULL LOOP */
		}
		a->next = NULL;

		/* Convert the comment into a text phrase */

		phrase = atext(NULL, name, AC_UNCOMMENT);

		/* Tokenise the new phrase */

		if ((address->name = tokenise(phrase)) == NULL) {
			/* Tokenise the list by words instead */
  
			address->name = add_atom(NULL, phrase, AT_ATOM);
  
			/* This phrase will need quoting */
  
			quote = TRUE;
		}
  
		/* Check if we need to quote the phrase */
  
		for (a = address->name; !quote && a != NULL; a = a->next) {
			/* Does this atom require quoting? */
  
			quote = (!IS_WS(a) && a->type != AT_ATOM
				 && a->type != AT_QSTRING);
		}
  
		/* Quote the atom list if required */

		if (quote) {
			/* Replace any name with the quoted one */

			qname = aquote(address->name);
			afree(address->name);
			address->name = qname;
		}

		/* Now append any tail to the phrase */

		address->name = acat(address->name, name->next);

		/* Free the phrase now */

		free(phrase);
	}

	return;
}
/****************************************************************************/
static void rfc_to_percent(address)
ADDRESS *address;
{
	/* Convert deprecated RFC 822 routes to percent form */

	ATOM *new_route = NULL;
	ATOM *first, *dmn, *comma, *at;

	/* Find the domain (ie the first route entry) */

	at = asearch(address->route, AT_AT);
	first = adiscard(address->route, at);
	comma = asearch(first, AT_COMMA);
	at = (comma != NULL) ? asearch(comma, AT_AT) : NULL;

	dmn = acut(first, comma);

	/* Set the start of the main route */

	first = (comma != NULL) ? adiscard(comma, comma) : NULL;
	first = (at != NULL) ? adiscard(first, at) : NULL;

	/* Loop through route domains, adding them as we go */

	while (first != NULL) {
		/* Find the first comma in the route */

		comma = asearch(first, AT_COMMA);
		at = (comma != NULL) ? asearch(comma, AT_AT) : NULL;

		/* Prepend the domain to the route list */

		first = acut(first, comma);

		/* Append the prior route */

		first = acat(first, new_route);

		/* Prepend the percent and update the new route */

		new_route = add_atom(NULL, "%", AT_PERCENT);
		new_route = acat(new_route, first);

		/* Move the start pointer on */

		first = (comma != NULL) ? adiscard(comma, comma) : NULL;
		first = (at != NULL) ? adiscard(first, at) : NULL;
	}

	/* Move the domain to the start of the new route */

	first = add_atom(NULL, "%", AT_PERCENT);
	first = acat(first, address->domain);

	/* Append the new route and form the new address */

	address->route = NULL;
	address->proute = acat(first, new_route);
	address->domain = dmn;

	return;
}
/****************************************************************************/
static void set_domain(address)
ADDRESS *address;
{
	/* Set the domain for an address that has none */

	ATOM *a, *last = NULL;

	/* Set the domain itself */

	address->domain = tokenise(local_domain());

	/* Move any trailing white space on the name to after the domain */

	for (a = address->local; a != NULL; a = a->next) {
		if (!IS_WS(a)) {
			last = a;
		}
	}

	address->domain = acat(address->domain, last->next);
	last->next = NULL;

	return;
}
/****************************************************************************/
void free_glist(glist)
GROUP *glist;
{
	/* Free the space taken up by the group list */

	if (glist != NULL) {
		free_glist(glist->next);
		afree(glist->name);
		afree(glist->comment);
		free_alist(glist->addresses);
		free(glist);
	}

	return;
}
/****************************************************************************/
void free_alist(alist)
ADDRESS *alist;
{
	/* Free the space taken up by the list */

	if (alist != NULL) {
		free_alist(alist->next);
		afree(alist->name);
		afree(alist->route);
		afree(alist->local);
		afree(alist->proute);
		afree(alist->domain);
	}

	return;
}
/****************************************************************************/
char *grp_text(glist, canon)
GROUP *glist;
int canon;
{
	/*
	 * Form an allocated string containing glist in a textual form.
	 * Canon specifies the canonicalisation level required.
	 */

	char *addrs = NULL;
	GROUP *g;
	ADDRESS *a;

	/* Loop through each group and address */

	for (g = glist; g != NULL; g = g->next) {
		/* Handle the group name, if any */

		if (g->comment != NULL) {
			addrs = atext(addrs, g->comment, AC_NONE);
			addrs = xrealloc(addrs, strlen(addrs) + 2);
			(void) strcat(addrs, " ");
		}
		if (g->name != NULL) {
			addrs = atext(addrs, g->name, AC_TRIM);
			addrs = xrealloc(addrs, strlen(addrs) + 3);
			(void) strcat(addrs, ": ");
		}

		/* Add each address in the group */

		for (a = g->addresses; a != NULL; a = a->next) {
			/* Initialise the text if required */

			addrs = (addrs == NULL) ? xstrdup("") : addrs;

			/* Handle the address name */
			if (a->name != NULL) {
				addrs = atext(addrs, a->name, AC_TRIM);
				addrs = xrealloc(addrs, strlen(addrs) + 3);
				(void) strcat(addrs, " <");
			}

			/* Put the address itself */

			addrs = addr_text(addrs, a, canon);

			/* End angle brackets around an address */

			if (a->name != NULL) {
				addrs = xrealloc(addrs, strlen(addrs) + 2);
				(void) strcat(addrs, ">");
			}

			/* Put a terminating comma if required */

			if (a->next != NULL) {
				addrs = xrealloc(addrs, strlen(addrs) + 3);
				(void) strcat(addrs, ", ");
			}
		}

		/* Put a comma or semicolon after the group as required */

		if (g->name != NULL) {
			if (g->next != NULL) {
				addrs = xrealloc(addrs, strlen(addrs) + 3);
				(void) strcat(addrs, "; ");
			} else {
				addrs = xrealloc(addrs, strlen(addrs) + 2);
				(void) strcat(addrs, ";");
			}
		} else if (g->next != NULL) {
			addrs = xrealloc(addrs, strlen(addrs) + 3);
			(void) strcat(addrs, ", ");
		}
	}

	return(addrs);
}
/****************************************************************************/
static char *grp_canon(glist, first_local)
GROUP *glist;
int first_local;
{
	/*
	 * Form an allocated string containing canonical form of
	 * glist in a textual form; either canonical addresses
	 * with no names, routes, or comments, or the canonical
	 * local-part of the first address.
	 */

	char *buf = NULL;
	GROUP *g;
	ADDRESS *a;

	/* If first_local then return the first local part */

	if (first_local) {
		return(atext(buf, glist->addresses->local, AC_FULL));
	}

	/* Loop through each group and address */

	for (g = glist; g != NULL; g = g->next) {
		/* Add each address in the group */

		for (a = g->addresses; a != NULL; a = a->next) {
			/* Add the canonical address */

			buf = addr_canon(buf, a);

			/* Put a terminating comma if required */

			if (a->next != NULL || g->next != NULL) {
				buf = xrealloc(buf, strlen(buf) + 3);
				(void) strcat(buf, ", ");
			}
		}
	}

	return(buf);
}
/****************************************************************************/
static char *grp_names(glist)
GROUP *glist;
{
	/*
	 * Form an allocated string containing the names associated
	 * with the addresses in glist.
	 */

	char *names = NULL;
	GROUP *g;
	ADDRESS *a;

	/* Loop through each group and address */

	for (g = glist; g != NULL; g = g->next) {
		/* See if the group has a name */

		if (g->name != NULL) {
			names = name_canon(names, g->name);
		} else if (g->comment != NULL) {
			names = name_canon(names, g->comment);
		} else {
			/* No group name, set names for each address */

			for (a = g->addresses; a != NULL; a = a->next) {
				names = (a->name != NULL) ?
					name_canon(names, a->name)
					: addr_canon(names, a);

				/* Put a terminating comma if required */

				if (a->next != NULL) {
					names = xrealloc(names, strlen(names)
							 + 3);
					(void) strcat(names, ", ");
				}
			}
		}

		/* Put a comma or semicolon after the group as required */

		if (g->next != NULL) {
			names = xrealloc(names, strlen(names) + 3);
			(void) strcat(names, ", ");
		}
	}

	return(names);
}
/****************************************************************************/
static char *addr_text(buf, address, canon)
char *buf;
ADDRESS *address;
int canon;
{
	/*
	 * Append address in a textual form to buf.
	 * Canon specifies the canonicalisation level required.
	 */

	/* Prepend any route */

	if (address->route != NULL) {
		buf = atext(buf, address->route, canon);
		buf = xrealloc(buf, strlen(buf) + 2);
		(void) strcat(buf, ":");
	}

	/* Append the local-part */

	buf = atext(buf, address->local, canon);

	/* Append any percent route */

	buf = atext(buf, address->proute, canon);

	/* Finally, append @domain */

	buf = xrealloc(buf, strlen(buf) + 2);
	(void) strcat(buf, "@");
	buf = atext(buf, address->domain, canon);

	/* Return the modified buffer */

	return(buf);
}
/****************************************************************************/
static char *addr_canon(buf, address)
char *buf;
ADDRESS *address;
{
	/* Append address in canonical form (local@domain) to buf */

	ATOM *dmn, *pct;

	/* Form the local part and append the @ */

	buf = atext(buf, address->local, AC_FULL);
	buf = xrealloc(buf, strlen(buf) + 2);
	(void) strcat(buf, "@");

	/* Extract domain from domain or percent route */

	if (address->proute != NULL) {
		/* Find the first token after the initial '%' */

		dmn = asearch(address->proute, AT_PERCENT);
		dmn = find_token(dmn->next);

		/* Make a copy of the route */

		dmn = acopy(dmn);

		/* Cut out the domain */

		if ((pct = asearch(dmn, AT_PERCENT)) != NULL) {
			dmn = acut(dmn, pct);
			afree(pct);
		}

		/* Append the domain to the address */

		buf = atext(buf, dmn, AC_FULL);
		afree(dmn);
	} else {
		/* No percent route, add the domain */

		buf = atext(buf, address->domain, AC_FULL);
	}

	return(buf);
}
/****************************************************************************/
static char *name_canon(buf, name)
char *buf;
ATOM *name;
{
	/*
	 * Append name in canonical form to buf.  If the name is a
	 * single quoted string or comment, then unquote or uncomment
	 * it; otherwise leave the name as is.  This seems to be the
	 * best balance between a convenient display for the user, and
	 * preserving the sender's text.
	 */

	int unquote;
	int uncomment;
	ATOM *atom;

	/* Is the name a single quoted string we can unquote? */

	unquote = ((atom = find_token(name)) != NULL &&
		   atom->type == AT_QSTRING &&
		   find_token(atom->next) == NULL);

	/* Is the name a single comment we can uncomment? */

	uncomment = ((atom = asearch(name, AT_COMMENT)) != NULL
		     && asearch(name, AT_COMMENT) == NULL
		     && find_token(name) == NULL);

	/* Now add the name to buf */

	return(atext(buf, name, (unquote) ? AC_UNQUOTE :
		     (uncomment) ? AC_UNCOMMENT : AC_TRIM));
}
/****************************************************************************/
static char *local_domain()
{
	/* Return the local domain in a static buffer */

#ifdef AFACK
	/* Return the local host name */

	return(get_host());

#else /* ! AFACK */
	/* Return the domain variable */

	return(get_vtext(V_DOMAIN));
#endif /* ! AFACK */
}
/****************************************************************************/
#ifdef MTA_NEEDS_ARGS
#ifdef MTA_NEEDS_UUCP
/****************************************************************************/
static char *uucp_text(address)
ADDRESS *address;
{
	/* Return an allocated string containing address in UUCP form */

	char *uu_addr = NULL, *domain;

	/* Get the local domain */

	domain = local_domain();

	/* Prepend any route, switching '@' to '!' */

	uu_addr = uucp_route(uu_addr, address->route);

	/* Set the domain if required */

	uu_addr = atext(uu_addr, address->domain, AC_FULL);
	if (address->proute == NULL && !strcasecmp(uu_addr, domain)) {
		free(uu_addr);
		uu_addr = NULL;
	} else {
		uu_addr = xrealloc(uu_addr, strlen(uu_addr) + 2);
		(void) strcat(uu_addr, "!");
	}

	/* Append any percent route */

	uu_addr = uucp_proute(uu_addr, address->proute);

	/* Append the local-part */

	uu_addr = atext(uu_addr, address->local, AC_FULL);

	/* Return the UUCP address text */

	return(uu_addr);
}
/****************************************************************************/
static char *uucp_route(buf, rt)
char *buf;
ATOM *rt;
{
	/* Append an RFC 822 route in UUCP form to buf */

	ATOM *new_route, *a;

	/* Find the initial '@' */

	if ((a = find_token(rt)) == NULL) {
		return(buf);
	}

	/* Find the first token after the '@' */

	if ((a = find_token(a->next)) == NULL) {
		return(buf);
	}

	/* Make a copy of the route */

	new_route = acopy(a);

	/* Loop through the route replacing @ with ! */

	for (a = new_route; a != NULL; a = a->next) {
		if (a->type == AT_AT) {
			(void) strcpy(a->text, "!");
		}
	}

	/* Append the canonical text to buf */

	buf = atext(buf, new_route, AC_FULL);

	/* Append a '!' to the route */

	buf = xrealloc(buf, strlen(buf) + 2);
	(void) strcat(buf, "!");

	/* Free the copied route and return */

	afree(new_route);
	return(buf);
}
/****************************************************************************/
static char *uucp_proute(buf, rt)
char *buf;
ATOM *rt;
{
	/* Append an RFC 733 percent route in UUCP form to buf */

	char *domain, *rstr = NULL;
	ATOM *new_route, *a, *pct;

	/* Find the first token after the initial '%' */

	a = asearch(rt, AT_PERCENT);
	a = find_token(a->next);

	/* Make a copy of the route */

	new_route = acopy(a);

	/* Loop through the route prepending each entry */

	a = new_route;
	while (a != NULL) {
		/* Cut out the next domain */

		if ((pct = asearch(a, AT_PERCENT)) != NULL) {
			a = acut(a, pct);
		}

		/* Form the canonical name for this address */

		domain = atext(NULL, a, AC_FULL);

		/* Prepend the domain to the route */

		if (rstr == NULL) {
			domain = xrealloc(domain, strlen(domain) + 2);
			(void) strcat(domain, "!");
		} else {
			domain = xrealloc(domain, strlen(domain)
					+ strlen(rstr) + 1);
			(void) strcat(domain, "!");
			(void) strcat(domain, rstr);
			free(rstr);
		}
		rstr = domain;

		/* Free the section of the route we cut out */

		afree(a);

		/* Move a on to the next token */

		a = (pct != NULL) ? adiscard(pct, pct) : NULL;
	}

	/* Append the canonical text to buf */

	if (buf == NULL) {
		buf = rstr;
	} else {
		buf = xrealloc(buf, strlen(buf) + strlen(rstr) + 1);
		(void) strcat(buf, rstr);
		free(rstr);
	}

	/* Return the modified buffer */

	return(buf);
}
/****************************************************************************/
#endif /* MTA_NEEDS_UUCP */
#endif /* MTA_NEEDS_ARGS */
/****************************************************************************/
