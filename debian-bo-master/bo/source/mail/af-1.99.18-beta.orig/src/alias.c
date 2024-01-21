/* Alias.c - Alias handling for af.
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


#include <stdio.h>
#include <ctype.h>
#include "af.h"
#include "atom.h"
#include "address.h"
#include "alias.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: alias.c,v 1.28 1996/08/28 17:44:08 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xstrdup(), *vstrcat(), *get_line();
extern char *get_home(), *atext(), *grp_text(), *a_strerror();
extern int strcasecmp();
extern void free(), free_glist(), free_alist(), afree(), translate();
extern void emsgl(), table();
extern ATOM *tokenise(), *add_atom(), *asearch();
extern ATOM *acopy(), *acut(), *adiscard(), *aquote();
extern GROUP *aparse();

#ifndef AFACK
extern char *strerror();
extern void msgl(), cmsg();
#endif /* ! AFACK */

/* Local function declarations */

void unalias(), read_afile();
static int add_alias();
static void alias_err();
static ADDRESS *addr_unalias(), *copy_group(), *copy_addrs();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* Import the user quit flag from commands.c */

extern int user_quit;

/****************************************************************************/
/* When reading alias files we store the file name here */

static char *alias_file = NULL;

/****************************************************************************/
void unalias(glist, aname)
GROUP *glist;
char *aname;
{
	/*
	 * Expand aliases in a group and address list.
	 * If aname is set then it names an alias being expanded,
	 * and so addresses matching this name are not expanded,
	 * allowing aliases such as 'hawk:Malc Arnold:hawk'.
	 */

	char *buf = NULL;
	GROUP *g = NULL;
	ADDRESS *a = NULL;

	/* Loop through each address in each group */

	for (g = glist; g != NULL; g = g->next) {
		/* Extract the address list of the group */

		a = g->addresses;

		/* Now expand each address in the list */

		while (a != NULL) {
			/* Get the full form of the address */

			buf = atext(NULL, a->route, AC_FULL);
			buf = atext(buf, a->local, AC_FULL);
			buf = atext(buf, a->proute, AC_FULL);
			buf = atext(buf, a->domain, AC_FULL);

			/* Expand the address if it is an alias */

			a = addr_unalias(buf, aname, g, a);

			/* Free the (now-redundant) buffer */

			free(buf);
		}
	}

	return;
}
/****************************************************************************/
static ADDRESS *addr_unalias(alias, aname, grp, addr)
char *alias, *aname;
GROUP *grp;
ADDRESS *addr;
{
	/* Expand a single address as required */

	ALIAS *a;

	/*
	 * Don't alias where the token equals the alias name that
	 * we are currently expanding.
	 */

	if (aname != NULL && !strcmp(alias, aname)) {
		return(addr->next);
	}

	/* Search the alias list looking for a match */

	for (a = aliases; a != NULL; a = a->next) {
		if (!strcasecmp(alias, a->a_alias)) {
			/*
			 * Found a match, check the status of the matched
			 * alias.  A_EXPANDING means that the alias is
			 * currently being expanded, and therefore there is
			 * a loop in the alias definitions.  Otherwise we
			 * may need to expand the alias to complete the
			 * current one.
			 */

			if (a->a_status == A_EXPANDING) {
				/* We have an alias expansion loop */

				alias_err("Loop", a->a_alias);
				return(addr->next);
			} else if (a->a_status != A_EXPANDED) {
				/* We need to expand the alias */

				a->a_status = A_EXPANDING;
				unalias(a->a_group, a->a_alias);
				translate(a->a_group);
				a->a_status = A_EXPANDED;
			}

			/* Generate the new addresses */

			if (addr->name == NULL &&
			    ((grp->name == NULL && grp->comment == NULL) || 
			     grp->addresses->next == NULL)) {
				/* Insert the alias as a new group */

				addr = copy_group(a->a_group, grp, addr);
			} else if (addr->name == NULL ||
				   a->a_group->addresses->next == NULL) {
				/* Insert the alias into the group */

				addr = copy_addrs(addr, a->a_group);
			} else {
				/* Can't insert group into named address */

				addr = addr->next;
			}
			return(addr);
		}
	}

	/* If we reached here there's no such alias */

	return(addr->next);
}
/****************************************************************************/
static ADDRESS *copy_group(grp, glist, addr)
GROUP *grp, *glist;
ADDRESS *addr;
{
	/*
	 * Insert a copy of grp after glist in that list.
	 * Return the next address in the group, which will be NULL.
	 */

	GROUP *new_group;
	ADDRESS *a;

	/* Handle addresses before addr */

	for (a = glist->addresses; a != NULL; a = a->next) {
		if (a->next == addr) {
			/* Make a new group and append after glist */

			new_group = (GROUP *) xmalloc(sizeof(GROUP));
			new_group->name = new_group->comment = NULL;
			new_group->addresses = addr;
			new_group->next = glist->next;

			/* Update the group containing leading addresses */

			glist->next = new_group;
			a->next = NULL;

			/* Make the new group current */

			glist = glist->next;
			break;
		}
	}

	/* Handle addresses after addr */

	if (glist->addresses->next != NULL) {
		/* Make a new group and append after glist */

		new_group = (GROUP *) xmalloc(sizeof(GROUP));
		new_group->name = new_group->comment = NULL;
		new_group->addresses = glist->addresses->next;
		new_group->next = glist->next;
			
		/* Update the group containing addr */

		glist->next = new_group;
		glist->addresses->next = NULL;
	}

	/* Copy the group name, comment and addresses */

	if (glist->name == NULL) {
		glist->name = acopy(grp->name);
	}
	if (glist->comment == NULL) {
		glist->comment = acopy(grp->comment);
	}
	(void) copy_addrs(glist->addresses, grp);

	/* Return NULL, we've processed all the group's addresses */

	return(NULL);
}
/****************************************************************************/
static ADDRESS *copy_addrs(addr, grp)
ADDRESS *addr;
GROUP *grp;
{
	/* Replace address addr with the addresses listed in grp */

	ADDRESS *a, *next;

	/* Set a equal to the first address in grp */

	a = grp->addresses;

	/* Save the next address after the one being replaced */

	next = addr->next;

	/* Free the old data in the address */

	afree(addr->route);
	afree(addr->local);
	afree(addr->proute);
	afree(addr->domain);

	/* Copy the first new address into place */

	addr->name = (addr->name != NULL) ? addr->name : acopy(a->name);
	addr->route = acopy(a->route);
	addr->local = acopy(a->local);
	addr->proute = acopy(a->proute);
	addr->domain = acopy(a->domain);

	/* Copy any new addresses after addr */

	for (a = a->next; a != NULL; a = a->next) {
		addr->next = (ADDRESS *) xmalloc(sizeof(ADDRESS));
		addr = addr->next;

		/* Copy the address details */

		addr->name = acopy(a->name);
		addr->route = acopy(a->route);
		addr->local = acopy(a->local);
		addr->proute = acopy(a->proute);
		addr->domain = acopy(a->domain);
		addr->next = next;
	}

	return(next);
}
/****************************************************************************/
void read_aliases(verbose)
int verbose;
{
	/* Main control for reading aliases */

	char *filnam;

	/* Initialise the alias list */

	aliases = NULL;

	/* Read the system alias file */

	read_afile(SYS_ALIASFILE, verbose);

	/* Read the user's alias file */

	filnam = vstrcat(get_home(NULL), "/", ALIASFILE, NULL);
	read_afile(filnam, verbose);
	free(filnam);

	return;
}
/****************************************************************************/
/*ARGSUSED*/
void read_afile(filnam, verbose)
char *filnam;
int verbose;
{
	/* Actually read an alias file */

	char *buf, *p;
	FILE *fp;
	ALIAS *a;

	/* Open the file */

	if ((fp = fopen(filnam, "r")) == NULL) {
		return;
	}

	/* Set the static file name */

	alias_file = filnam;

#ifndef AFACK
	/* Give the user a message */

	if (verbose) {
		msgl("Reading alias file ", filnam, "...", NULL);
	}
#endif /* ! AFACK */

	/* Now read through the file */

	while ((buf = get_line(fp, TRUE)) != NULL) {
		/* Add aliases on non-comment non-blank lines */

		for (p = buf; *p != '\0'; p++) {
			if (*p == ';') {	/* Comment */
				break;
			}
			if (!isspace(*p)) {	/* Alias */
				(void) add_alias(buf);
				break;
			}
		}
		/* Free the buffer */

		free(buf);
	}

	/* Close the file */

	(void) fclose(fp);

	/* Now expand the address list of each alias */

	for (a = aliases; a != NULL; a = a->next) {
		if (a->a_status != A_EXPANDED) {
			a->a_status = A_EXPANDING;
			unalias(a->a_group, a->a_alias);
			translate(a->a_group);
			a->a_status = A_EXPANDED;
		}
	}

	/* Clean up and exit */

#ifndef AFACK
	if (verbose) {
		cmsg(" Done");
	}
#endif /* ! AFACK */

	alias_file = NULL;
	return;
}
/****************************************************************************/
static int add_alias(aline)
char *aline;
{
	/* Add an alias to the list, returning TRUE for success */

	char *aname;
	ALIAS *p, *node, *prev_node;
	ATOM *alist, *rname, *addrs, *a;
	GROUP *grp;

	/* Split the line into atoms */

	if ((alist = tokenise(aline)) != NULL) {
		alist = add_atom(alist, ",", AT_COMMA);
	} else {
		alias_err("Bad definition", aline);
		return(FALSE);
	}

	/* Get the real name */

	if ((rname = asearch(alist, AT_COLON)) == NULL) {
		alias_err("Bad definition", aline);
		afree(alist);
		return(FALSE);
	}
	alist = acut(alist, rname);
	rname = adiscard(rname, rname);

	/* Convert the alias name to text */

	aname = atext(NULL, alist, AC_FULL);
	afree(alist);

	/* Get the address list */

	if ((addrs = asearch(rname, AT_COLON)) == NULL) {
		alias_err("Bad definition", aname);
		afree(rname);
		free(aname);
		return(FALSE);
	}
 	rname = acut(rname, addrs);
	addrs = adiscard(addrs, addrs);

	/* Check the real name for the alias */

	for (a = rname; a != NULL; a = a->next) {
		if (!IS_WS(a) && a->type != AT_ATOM
		    && a->type != AT_QSTRING) {
			/* The real name needs quoting */

			rname = aquote(rname);
			break;
		}
	}

	/* Parse the addresses for the alias */

	if ((grp = aparse(addrs)) == NULL) {
		alias_err(a_strerror(), aname);
		afree(rname);
		free(aname);
		return(FALSE);
	}

	/* Insert the real name for the address or group */

	if (grp->addresses->next != NULL && grp->name == NULL) {
		grp->name = rname;
	} else if (grp->addresses->next == NULL
		   && grp->addresses->name == NULL) {
		grp->addresses->name = rname;
	} else {
		afree(rname);
	}

	/* Form the new node */

	node = (ALIAS *) xmalloc(sizeof(ALIAS));
	node->a_alias = xmalloc(strlen(aname) + 1);
	(void) strcpy(node->a_alias, aname);
	node->a_group = grp;
	node->a_status = A_RAW;

	/* Insert the node in lexicographic order in the list */

	prev_node = NULL;
	for (p = aliases; p != NULL; p = p->next) {
		/* Check if this alias is a duplicate */

		if (!strcasecmp(aname, p->a_alias)) {
			free_glist(p->a_group);
			p->a_group = node->a_group;
			free(node->a_alias);
			free(node);
			return(TRUE);
		}

		/* Check if we should insert the alias here */

		if (strcmp(aname, p->a_alias) < 0) {
			/* Add the node to the list */

			node->next = p;
			if (prev_node == NULL) {
				aliases = node;
			} else {
				prev_node->next = node;
			}
			return(TRUE);
		}

		/* Set the previous node pointer */

		prev_node = p;
	}

	/* Append the node to the list */

	node->next = NULL;

	if (prev_node == NULL) {
		aliases = node;
	} else {
		prev_node->next = node;
	}

	return(TRUE);
}
/****************************************************************************/
static void alias_err(etext, aname)
char *etext, *aname;
{
	/* Display errors encountered when building the alias list */

	emsgl(alias_file, ": ", etext, " in alias ", aname, NULL);
	return;
}
/****************************************************************************/
#ifndef AFACK
int isalias(aname)
char *aname;
{
	/* Return TRUE is alias aname exists, FALSE otherwise */

	ALIAS *p;

	/* Loop through the aliases looking for aname */

	for (p = aliases; p != NULL; p = p->next) {
		if (!strcasecmp(p->a_alias, aname)) {
			return(TRUE);
		}
	}

	/* If we got here there's no such alias */

	return(FALSE);
}
/****************************************************************************/
void new_alias(aname, rname, users)
char *aname, *rname, *users;
{
	/* Interactively add a new alias to the list and ~/.afalias */

	char *aline, *filnam;
	FILE *fp;

	/* Form the alias line */

	aline = vstrcat(aname, ":", rname, ":", users, NULL);

	/* Add the alias internally */

	if (!add_alias(aline)) {
		return;
	}

	/* Find the user's alias file */

	filnam = vstrcat(get_home(NULL), "/", ALIASFILE, NULL);

	/* Give the user a message */

	msgl("Adding alias to ", filnam, "...", NULL);

	/* Open the alias file */

	if ((fp = fopen(filnam , "a")) == NULL) {
		emsgl("Can't open ", filnam, ": ", strerror(errno), NULL);
		free(filnam);
		return;
	}

	/* Add the alias */

	if (fputs(aline, fp) == EOF || putc('\n', fp) == EOF) {
		emsgl("Error writing ", filnam, ": ", strerror(errno), NULL);
		(void) fclose(fp);
		free(filnam);
		return;
	}

	/* Clean up space */

	(void) fclose(fp);
	free(aline);
	free(filnam);

	/* Confirm the write and return */

	cmsg(" Done");
	return;
}
#endif /* ! AFACK */
/****************************************************************************/
void list_aliases()
{
	/* List all defined aliases to typeout */

	char *addrs;
	ALIAS *p;

	/* Now loop over the defined aliases */

	for (p = aliases; !user_quit && p != NULL; p = p->next) {
		addrs = grp_text(p->a_group, AC_TRIM);
		table(p->a_alias, addrs);
		free(addrs);
	}

	return;
}
/****************************************************************************/
