/* Keymaps.c - Keymap handling for af.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */


#include <stdio.h>
#include <ctype.h>
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "commands.h"
#include "keymaps.h"
#include "macros.h"
#include "variable.h"
#include "mode.h"
#include "complete.h"
#include "maplist.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: keymaps.c,v 1.12 1997/03/05 21:23:45 malc Exp $";
static char *KeymapId = KEYMAPID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xstrdup(), *vstrcat();
extern char *modename(), *strkey(), *strseq();
extern char *strcanon();
extern int strcasecmp(), strncasecmp(), mklower();
extern int get_key(), get_vval();
extern unsigned cmodes();
extern void free(), free_seq(), typeout();
extern void table(), msg(), emsgl(), cmsg();
extern COMMAND *find_command(), *find_by_func();
extern FUNCTION *find_function();
extern MACRO *find_macro();
extern KEYSEQ *new_seq(), *add_seq();
extern CLIST *cmd_complete(), *add_clist();

/* Local function declarations */

char *bindings(), *strbinding();
KEYMAP *find_keymap(), *mode_keymap();
BINDING *find_binding();
static char *cmd_bindings(), *strrange();
static int same_binding(), rebind(), last_of_range();
static int def_binding(), write_binding();
static void mode_bindings();
static KEYMAP *add_keymap();
static BINDING *make_binding(), *add_binding();
static BINDING *del_binding();
static KEYMAP_DEFAULTS *def_map();

/****************************************************************************/
/* Import the current window and user quit flag from commands.c */

extern WINDOW *cwin;
extern int user_quit;

/****************************************************************************/
void init_keymaps()
{
	/* Initialise the keymaps from the defaults */

	KEYMAP *map;
	KEYMAP_DEFAULTS *d;
	CMD_BINDING *c;
	MAP_BINDING *m;
	BINDING *node;

	/* Make the keymaps and add command bindings */

	for (d = def_keymaps; d->map_name != NULL; d++) {
		/* Make the keymap */

		keymaps = add_keymap(keymaps, d->map_name);
		map = find_keymap(d->map_name);

		/* Add the command bindings */

		for (c = d->commands; c != NULL && c->func != NULL; c++) {
			node = make_binding(c->key, B_COMMAND,
					    find_by_func(c->func),
					    NULL, NULL);
			map->bindings = add_binding(map->bindings, node);
		}
	}

	/* Add the keymap bindings for each map */

	for (d = def_keymaps; d->map_name != NULL; d++) {
		/* Find the relevant keymap */

		map = find_keymap(d->map_name);

		/* Add the default keymap bindings */

		for (m = d->keymaps; m != NULL && m->map != NULL; m++) {
			node = make_binding(m->key, B_KEYMAP, NULL,
					    NULL, find_keymap(m->map));
			map->bindings = add_binding(map->bindings, node);
		}
	}

	return;
}
/****************************************************************************/
KEYMAP *find_keymap(mapname)
char *mapname;
{
	/* Return the keymap with the given name */

	KEYMAP *m;

	/* Search the keymap list for the map */

	for (m = keymaps; m != NULL; m = m->next) {
		/* Is this the map we're looking for? */

		if (!strcasecmp(m->name, mapname)) {
			return(m);
		}
	}

	/* No such keymap */

	return(NULL);
}
/****************************************************************************/
KEYMAP *mode_keymap(modes, key)
unsigned modes;
int key;
{
	/*
	 * Return the first active keymap in modes binding key,
	 * or the first keymap found if key is ANY_KEY.
	 */

	unsigned m;
	KEYMAP *map;

	/* Loop through each mode checking for a binding */

	for (m = M_COMPLETE; m; m = m >> 1) {
		/* Is the key bound in this mode's keymap? */

		if ((m & modes) && (map = find_keymap(modename(m))) != NULL
		    && (key == ANY_KEY || find_binding(map, key) != NULL)) {
			return(map);
		}
	}

	/* Return the global keymap by default */

	return(find_keymap(GLOBAL_MAP));
}
/****************************************************************************/
static KEYMAP *add_keymap(list, mapname)
KEYMAP *list;
char *mapname;
{
	/* Make a new keymap and add it to the list */

	KEYMAP *node;

	/* Add the keymap to the list */

	if (list == NULL || strcasecmp(list->name, mapname) > 0) {
		/* Make the new keymap and return at head of list */

		node = (KEYMAP *) xmalloc(sizeof(KEYMAP));
		node->name = xstrdup(mapname);
		node->bindings = NULL;
		node->next = list;
		return(node);
	}

	/* Add the new map later in the list */

	list->next = add_keymap(list->next, mapname);
	return(list);
}
/****************************************************************************/
static BINDING *make_binding(key, type, cmd, macro, map)
int key, type;
COMMAND *cmd;
MACRO *macro;
KEYMAP *map;
{
	/* Return an allocated binding of key to the command or map */

	BINDING *node;

	/* Make the new binding */

	node = (BINDING *) xmalloc(sizeof(BINDING));

	/* Set the key and type of the binding */

	node->key = key;
	node->type = type;
	node->next = NULL;

	/* Set the object pointed to by the binding */

	switch(type) {
	case B_UNBOUND:
	case B_COMMAND:
		node->object.cmd = cmd;
		break;
	case B_KEYMAP:
		node->object.map = map;
		break;
	case B_MACRO:
		node->object.macro = macro;
		break;
	}

	return(node);
}
/****************************************************************************/
static BINDING *add_binding(list, node)
BINDING *list, *node;
{
	/* Add an enty to the list of bindings */

	/* Add the node if appropriate */

	if (list == NULL || list->key > node->key) {
		node->next = list;
		return(node);
	} else if (list->key == node->key) {
		node->next = list->next;
		free(list);
		return(node);
	}

	/* Add the node later in the list */

	list->next = add_binding(list->next, node);
	return(list);
}
/****************************************************************************/
static BINDING *del_binding(list, key)
BINDING *list;
int key;
{
	/* Delete key from the bindings in map */

	BINDING *node;

	/* Delete the entry if required */

	if (list == NULL || list->key > key) {
		return(list);
	} else if (list->key == key) {
		node = list->next;
		free(list);
		return(node);
	}

	/* Search for the key later in the list */

	list->next = del_binding(list->next, key);
	return(list);
}
/****************************************************************************/
BINDING *find_binding(map, key)
KEYMAP *map;
int key;
{
	/* Return the binding of key in map */

	BINDING *case_binding = NULL, *b;

	/* Convert metacharacters to the meta prefix character */

	if (!isascii(key)) {
		/* Find the meta-prefix's binding in the map */

		b = find_binding(map, get_vval(V_METACHAR));

		/* And return the key's binding in that map */

		if (b != NULL && IS_KEYMAP(b)) {
			/* Recurse to find the binding */

			return(find_binding(b->object.map, toascii(key)));
		}

		/* Invalid prefix key used; return error */

		return(NULL);
	}

	/* Search the bindings for the key */

	for (b = map->bindings; b != NULL; b = b->next) {
		/* Check for a (possibly case-folded) binding */

		if (b->key == key) {
			return(IS_UNBOUND(b) ? NULL : b);
		}
		if (b->key == mklower(key)) {
			case_binding = b;
		}
	}

	/* Return any case_folded binding found */

	return((IS_UNBOUND(case_binding)) ? NULL : case_binding);
}
/****************************************************************************/
BINDING *dereference(map, mode, keys)
KEYMAP *map;
unsigned mode;
KEYSEQ *keys;
{
	/* Return the result of derefencing keys in keymaps */

	int key;
	KEYMAP *lmap = map, *gmap = NULL;
	BINDING *local = NULL, *global = NULL;
	
	/* If no map is supplied then use the defaults */

	if (map == NULL && keys->len > 0) {
		lmap = mode_keymap(mode, keys->keys[0]);
		gmap = find_keymap(GLOBAL_MAP);
	}

	/* Loop over the keys in the sequence */

	for (key = 0; lmap != NULL && key < keys->len; key++) {
		/* Get the local and global bindings in the maps */

		local = find_binding(lmap, keys->keys[key]);
		global = (gmap != NULL) ? find_binding(gmap,
				keys->keys[key]) : NULL;
		local = (local == NULL || local->key != key &&
			 global != NULL && IS_ACTIVE(global, mode)
			 && global->key == key) ? global : local;

		/* And update the keymaps if required */

		lmap = (local != NULL && IS_KEYMAP(local))
				? local->object.map : NULL;
		gmap = (global != NULL && IS_KEYMAP(global))
				? global->object.map : NULL;
	}

	/* Now return the binding, if we found one */

	return((!(keys->len) || key < keys->len) ? NULL : local);
}
/****************************************************************************/
char *strbinding(binding)
BINDING *binding;
{
	/* Return the name of the command bound by the binding */

	switch (binding->type) {
	case B_COMMAND:
		return(binding->object.cmd->name);
	case B_KEYMAP:
		return(binding->object.map->name);
	case B_MACRO:
		return(strcanon(binding->object.macro->name, SK_READSYM));
	default:
		return(NULL);
	}
	/*NOTREACHED*/
}
/****************************************************************************/
static int same_binding(bind1, bind2)
BINDING *bind1, *bind2;
{
	/* Return TRUE if the two bindings are equivalent */

	/* Check the type of the bindings */

	if (bind1->type != bind2->type) {
		return(FALSE);
	}

	/* Check the content */

	switch (bind1->type) {
	case B_UNBOUND:
		return(TRUE);
	case B_COMMAND:
		return(bind1->object.cmd == bind2->object.cmd);
	case B_KEYMAP:
		return(bind1->object.map == bind2->object.map);
	case B_MACRO:
		return(bind1->object.macro == bind2->object.macro);
	default:
		return(FALSE);
	}
	/*NOTREACHED*/
}
/****************************************************************************/
int new_keymap(mapname)
char *mapname;
{
	/* Handle adding a named keymap to the list */

	/* Check if a bindable object of that name exists */

	if (find_command(mapname) != NULL || find_function(mapname) != NULL) {
		/* Can't overwrite an existing function */

		emsgl("Can't create ", mapname,
		      ": already defined as a function", NULL);
		return(FALSE);
	} else if (find_keymap(mapname) != NULL) {
		/* Can't overwrite an existing keymap */

		emsgl("Can't create ", mapname,
		      ": already defined as a keymap", NULL);
		return(FALSE);
	} else if (find_macro(mapname) != NULL) {
		/* Can't overwrite an existing macro */

		emsgl("Can't create ", mapname,
		      ": already defined as a keyboard macro", NULL);
		return(FALSE);
	}

	/* Add the keymap and return */

	keymaps = add_keymap(keymaps, mapname);
	return(TRUE);
}
/****************************************************************************/
int set_key(mapname, keys, bindname)
char *mapname;
KEYSEQ *keys;
char *bindname;
{
	/* Set a key binding for keys in the keymap */

	int key = 0, asckey = 0;
	int metakey = EOF;
	KEYMAP *map;
	BINDING *binding;

	/* Find the base keymap for the keys */

	if ((map = find_keymap(mapname)) == NULL) {
		emsgl("No keymap ", mapname, NULL);
		return(FALSE);
	}

	/* Now dereference the keys to find the map */

	while (key < keys->len) {
		/* Set up the key to dereference */

		asckey = (metakey != EOF) ? metakey : keys->keys[key++];
		metakey = (!isascii(asckey)) ? toascii(asckey) : EOF;
		asckey = (!isascii(asckey)) ? get_vval(V_METACHAR) : asckey;

		/* And dereference the key */

		if ((binding = find_binding(map, asckey)) == NULL
		    || !IS_KEYMAP(binding)) {
			break;
		}

		/* Update the keymap if required */

		if (key < keys->len && metakey == EOF) {
			map = binding->object.map;
		}
	}

	/* Check for an error in the key sequence */

	if (!(keys->len) || key < keys->len || metakey != EOF) {
		emsgl("Key sequence ", strseq(keys, SK_KEYSEQ),
		      " uses invalid prefix characters", NULL);
		return(FALSE);
	}

	/* Rebind the key and return status */

	return(rebind(map, asckey, bindname));
}
/****************************************************************************/
static int rebind(map, key, bindname)
KEYMAP *map;
char *bindname;
int key;
{
	/* Bind key in map to run command bindname */

	COMMAND *new_cmd;
	MACRO *new_macro;
	KEYMAP *new_map;
	BINDING *node;

	/* Handle rebinding based on arguments */

	if (bindname == NULL &&
	    (!isupper(key) || find_binding(map, mklower(key)) == NULL)) {
		/* Simply remove the binding */

		map->bindings = del_binding(map->bindings, key);
		return(TRUE);
	} else if (bindname == NULL) {
		/* Need to explicitly unbind upper-case keys */

		node = make_binding(key, B_UNBOUND, NULL, NULL, NULL);
	} else if ((new_cmd = find_command(bindname)) != NULL) {
		/* Make the binding */

		node = make_binding(key, B_COMMAND, new_cmd, NULL, NULL);
	} else if ((new_map = find_keymap(bindname)) != NULL) {
		/* Add the keymap binding to the list */

		node = make_binding(key, B_KEYMAP, NULL, NULL, new_map);
	} else if ((new_macro = find_macro(bindname)) != NULL) {
		/* Make the binding */

		node = make_binding(key, B_MACRO, NULL, new_macro, NULL);
	} else {
		/* No such command or keymap */

		emsgl("No command ", bindname, NULL);
		return(FALSE);
	}

	/* Add the new binding of the key to the map */

	map->bindings = add_binding(map->bindings, node);
	return(TRUE);
}
/****************************************************************************/
KEYSEQ *get_prefix(map, mode, prompt)
KEYMAP *map;
unsigned mode;
char *prompt;
{
	/* Get and return a keymap prefix string */

	int key, metakey = EOF;
	int initialise = TRUE;
	KEYSEQ *seq = NULL;
	KEYMAP *lmap = map;
	KEYMAP *gmap = NULL;
	BINDING *local, *global;

	/* Output the prompt */

	msg(prompt);

	/* Loop until the key is complete */

	do {
		/* Get a key if required */

		key = (metakey != EOF) ? metakey : get_key();

		/* Add and show the key */

		if (metakey == EOF) {
			seq = add_seq(seq, key);
			cmsg(strkey(key, SK_KEYSEQ));
			cmsg(" ");
		}

		/* Set up the key to dereference */

		metakey = (!isascii(key)) ? toascii(key) : EOF;
		key = (!isascii(key)) ? get_vval(V_METACHAR) : key;

		/* If no map is supplied then use the defaults */

		if (map == NULL && initialise) {
			lmap = mode_keymap(mode, key);
			gmap = find_keymap(GLOBAL_MAP);
			initialise = FALSE;
		}

		/* Dereference the key in each map */

		local = find_binding(lmap, key);
		global = (gmap != NULL) ? find_binding(gmap, key) : NULL;

		/* Decide which binding to use */

		local = (local == NULL || local->key != key &&
			 global != NULL && IS_ACTIVE(global, mode)
			 && global->key == key) ? global : local;

		/* And update the keymaps if required */

		lmap = (local != NULL && IS_KEYMAP(local))
			? local->object.map : NULL;
		gmap = (global != NULL && IS_KEYMAP(global))
			? global->object.map : NULL;
	} while (local != NULL && IS_KEYMAP(local));

	/* Check for an error in the key sequence */

	if (metakey != EOF) {
		emsgl("Key sequence ", strseq(seq, SK_KEYSEQ),
		      " uses invalid prefix characters", NULL);
		return(NULL);
	}

	/* And return the complete key sequence */

	return(seq);
}
/****************************************************************************/
void list_keymaps()
{
	/* List all availabe keymaps to typeout */

	KEYMAP *map;

	/* Loop through the keymaps and display them */

	for (map = keymaps; !user_quit && map != NULL; map = map->next) {
		table(map->name, bindings(map->name));
	}

	return;
}
/****************************************************************************/
void list_bindings()
{
	/* List all defined key bindings to typeout */

	/* List the bindings in the global keymaps */

	typeout("Global bindings:\n");
	mode_bindings(find_keymap(GLOBAL_MAP), NULL, NULL);

	/* List the bindings in the mail keymaps */

	typeout("\nBindings local to mode mail:\n");
	mode_bindings(find_keymap(MAIL_MAP), NULL, NULL);

	/* List the bindings in the typeout keymaps */

	typeout("\nBindings local to mode typeout:\n");
	mode_bindings(find_keymap(TYPEOUT_MAP), NULL, NULL);

	/* List the bindings in the minibuffer keymaps */

	typeout("\nBindings local to mode minibuffer:\n");
	mode_bindings(find_keymap(MBUF_MAP), NULL, NULL);

	return;
}
/****************************************************************************/
static void mode_bindings(map, scanned, prefix)
KEYMAP *map, *scanned;
KEYSEQ *prefix;
{
	/* List all the bindings in map active in modes */

	char *range;
	int lastkey, found = FALSE;
	KEYMAP *m;
	BINDING *b = map->bindings;
	KEYSEQ *seq;

	/* Check if the map has previously been scanned */

	for (m = scanned; m != NULL; m = m->next) {
		if (!strcasecmp(m->name, map->name)) {
			return;
		}
	}

	/* Add the keymap to the list of scanned maps */
	
	scanned = add_keymap(scanned, map->name);

	/* Loop through all the defined bindings */

	while (b != NULL) {
		if (!IS_UNBOUND(b)) {
			/* Output a newline if first binding found */

			if (!found) {
				typeout("\n");
				found = TRUE;
			}

			/* Scan for further identical bindings */

			lastkey = last_of_range(map, b);
			range = strrange(prefix, b->key, lastkey);

			/* Output the binding */

			table(range, strbinding(b));
		} else {
			lastkey = b->key;
		}

		/* Skip bindings within range */

		while (b != NULL && b->key <= lastkey) {
			b = b->next;
		}
	}

	/* Now search for further keymaps to scan */

	for (b = map->bindings; b != NULL; b = b->next) {
		if (IS_KEYMAP(b)) {
			seq = new_seq(prefix, b->key);
			mode_bindings(b->object.map, scanned, seq);
			free_seq(seq);
		}
	}

	/* Free the list of scanned keymaps if at top level */

	while (prefix == NULL && scanned != NULL) {
		m = scanned->next;
		free(scanned);
		scanned = m;
	}

	return;
}
/****************************************************************************/
char *bindings(cmdname)
char *cmdname;
{
	/* Return the bindings of the named command in a static buffer */

	static char *buf = NULL;

	BINDING binding;

	/* Free the return buffer */

	if (buf != NULL) {
		free(buf);
	}

	/* Find the object to search for */

	binding.type = B_COMMAND;
	if ((binding.object.cmd = find_command(cmdname)) == NULL) {
		binding.type = B_KEYMAP;
		if ((binding.object.map = find_keymap(cmdname)) == NULL) {
			binding.type = B_MACRO;
			binding.object.macro = find_macro(cmdname);
		}
	}

	/* Actually list any bindings */

	buf = cmd_bindings(NULL, find_keymap(GLOBAL_MAP),
			   &binding, MI_GLOBAL, NULL, NULL);
	buf = cmd_bindings(buf, find_keymap(MAIL_MAP),
			   &binding, MI_MAIL, NULL, NULL);
	buf = cmd_bindings(buf, find_keymap(TYPEOUT_MAP),
			   &binding, MI_TYPEOUT, NULL, NULL);
	buf = cmd_bindings(buf, find_keymap(MBUF_MAP),
			   &binding, MI_MBUF, NULL, NULL);
	return(buf);
}
/****************************************************************************/
static char *cmd_bindings(buf, map, cbinding, imode, scanned, prefix)
char *buf;
KEYMAP *map, *scanned;
BINDING *cbinding;
int imode;
KEYSEQ *prefix;
{
	/* Return the bindings of cmd in map */

	char *range, *rbuf, *newbuf;
	int lastkey;
	KEYMAP *m;
	BINDING *b;
	KEYSEQ *seq;

	/* Check if the map has previously been scanned */

	for (m = scanned; m != NULL; m = m->next) {
		if (!strcasecmp(m->name, map->name)) {
			return(buf);
		}
	}

	/* Add the keymap to the list of scanned maps */
	
	scanned = add_keymap(scanned, map->name);

	/* Loop through all the defined bindings */

	b = map->bindings;
	while (b != NULL) {
		if (same_binding(b, cbinding)) {
			/* Scan for further identical bindings */

			lastkey = last_of_range(map, b);
			range = strrange(prefix, b->key, lastkey);

			/* Add the range to the buffer */

			rbuf = vstrcat(indicators[imode].left, range,
				       indicators[imode].right, NULL);

			if (buf == NULL) {
				buf = rbuf;
			} else {
				newbuf = vstrcat(buf, ", ", rbuf, NULL);
				free(buf);
				free(rbuf);
				buf = newbuf;
			}

			/* Skip bindings within range */

			while (b != NULL && b->key <= lastkey) {
				b = b->next;
			}
		} else {
			b = b->next;
		}
	}

	/* Now search for further keymaps to scan */

 	for (b = map->bindings; b != NULL; b = b->next) {
		if (IS_KEYMAP(b)) {
			seq = new_seq(prefix, b->key);
			buf = cmd_bindings(buf, b->object.map, cbinding,
					   imode, scanned, seq);
			free_seq(seq);
		}
	}

	/* Free the list of scanned keymaps if at top level */

	while (prefix == NULL && scanned != NULL) {
		m = scanned->next;
		free(scanned);
		scanned = m;
	}

	return(buf);
}
/****************************************************************************/
static int last_of_range(map, binding)
KEYMAP *map;
BINDING *binding;
{
	/*
	 * Return the last key following binding which ends
	 * a range of consecutive bindings all bound to the same
	 * object, or the key itself if there is no such range.
	 */

	int key, lastkey = binding->key;
	BINDING *b;

	/* Start with the first key after binding */

	key = lastkey + 1;

	/* Scan through all bindings after the base */

	while ((b = find_binding(map, key)) != NULL) {
		if (!same_binding(b, binding)) {
			break;
		}

		/* Set the last key in the range */

		lastkey = key++;
	}

	/* Return the last element of the range */

	return(lastkey);
}
/****************************************************************************/
static char *strrange(prefix, first, last)
KEYSEQ *prefix;
int first, last;
{
	/* Return first .. last in a static buffer */

	static char *buf = NULL;

	char *key1, *key2;
	KEYSEQ *seq;

	/* Free any previous return buffer */

	if (buf != NULL) {
		free(buf);
	}

	/* Get the value of the first key */

	seq = new_seq(prefix, first);
	key1 = xstrdup(strseq(seq, SK_KEYSEQ));
	free_seq(seq);

	/* Get the value of the last key, if any */

	if (last != first) {
		seq = new_seq(prefix, last);
		key2 = xstrdup(strseq(seq, SK_KEYSEQ));
		free_seq(seq);

		/* Fill the buffer and free space */

		buf = vstrcat(key1, " .. ", key2, NULL);
		free(key1);
		free(key2);
	} else {
		/* Range only consists of one key */

		buf = key1;
	}

	/* Return the buffer */

	return(buf);
}
/****************************************************************************/
int user_keymaps(fp)
FILE *fp;
{
	/* Write all non-default keymaps to fp */

	int found = FALSE;
	KEYMAP *m;

	/* Loop through each keymap checking if default */

	for (m = keymaps; m != NULL; m = m->next) {
		/* Don't write default maps */

		if (def_map(m->name) == NULL) {
			/* Output a header if first user map found */

			if (!found && fputs("\n; Keymaps\n", fp) == EOF) {
				return(FALSE);
			}
			found = TRUE;

			/* Output the definition of the keymap */

			if (fprintf(fp, "(make-keymap '%s)\n",
				    strcanon(m->name, SK_READSYM)) == EOF) {
				return(FALSE);
			}
		}
	}

	/* All keymaps written */

	return(TRUE);
}
/****************************************************************************/
int user_bindings(fp)
FILE *fp;
{
	/* Write all non-default key bindings to fp */

	int found = FALSE;
	KEYMAP *map;
	BINDING *b;
	KEYMAP_DEFAULTS *d;
	CMD_BINDING *c;
	MAP_BINDING *m;

	/* Loop through each keymap writing modified bindings */

	for (map = keymaps; map != NULL; map = map->next) {
		/* Loop through each binding in the map */

		for (b = map->bindings; b != NULL; b = b->next) {
			if (!def_binding(map, b)) {
				if (!write_binding(fp, map, b,
						   b->key, found)) {
					return(FALSE);
				}
				found = TRUE;
			}
		}
	}

	/* Now check for deleted bindings */

	for (d = def_keymaps; d->map_name != NULL; d++) {
		/* Find the map to compare the bindings in */

		map = find_keymap(d->map_name);

		/* Check the command bindings */

		for (c = d->commands; c != NULL && c->func != NULL; c++) {
			if (find_binding(map, c->key) == NULL) {
				if (!write_binding(fp, map, NULL,
						   c->key, found)) {
					return(FALSE);
				}
				found = FALSE;
			}
		}

		/* Check the keymap bindings */

		for (m = d->keymaps; m != NULL && m->map != NULL; m++) {
			if (find_binding(map, m->key) == NULL) {
				if (!write_binding(fp, map, NULL,
						   m->key, found)) {
					return(FALSE);
				}
				found = FALSE;
			}
		}
	}

	return(TRUE);
}
/****************************************************************************/
static KEYMAP_DEFAULTS *def_map(mapname)
char *mapname;
{
	/* Return the default for the named keymap */

	KEYMAP_DEFAULTS *d;

	for (d = def_keymaps; d->map_name != NULL; d++) {
		if (!strcasecmp(d->map_name, mapname)) {
			return(d);
		}
	}

	/* Keymap is not a default map */

	return(NULL);
}
/****************************************************************************/
static int def_binding(map, bind)
KEYMAP *map;
BINDING *bind;
{
	/* Return TRUE if key is bound to the default in map */

	KEYMAP_DEFAULTS *d;
	CMD_BINDING *c;
	MAP_BINDING *m;

	/* Get the defaults specification for the map */

	if ((d = def_map(map->name)) == NULL) {
		return(FALSE);
	}

	/* Find any default binding of the key */

	if (bind->type == B_COMMAND) {
		for (c = d->commands; c->func != NULL; c++) {
			if (c->key == bind->key) {
				return(c->func == bind->object.cmd->func);
			}
		}
	} else if (bind->type == B_KEYMAP) {
		for (m = d->keymaps; m != NULL && m->map != NULL; m++) {
			if (m->key == bind->key) {
				return(!strcasecmp(m->map,
					bind->object.map->name));
			}
		}
	}

	/* Binding not specified */

	return(FALSE);
}
/****************************************************************************/
static int write_binding(fp, map, binding, key, found)
FILE *fp;
KEYMAP *map;
BINDING *binding;
int key, found;
{
	/* Write details of the binding to fp */

	char *objname, *mapname;
	static char *undef = "nil";
	int status;
	
	/* Write a header if first binding found */

	if (!found && fputs("\n; Key Bindings\n", fp) == EOF) {
		return(FALSE);
	}

	/* Set up the object and keymap names */

	objname = (IS_UNBOUND(binding)) ? xstrdup(undef) :
		xstrdup(strbinding(binding));
	mapname = xstrdup(strcanon(map->name, SK_READSYM));

	/* Output the definition of the keymap */

	status = (fprintf(fp, "(define-key '%s \"%s\" '%s)\n", mapname,
			  strkey(key, SK_READKEY), objname) != EOF);

	/* Free buffers and return status */

	free(objname);
	free(mapname);
	return(status);
}
/****************************************************************************/
CLIST *map_complete(list, base)
CLIST *list;
char *base;
{
	/* Return a list of keymaps completing base */

	KEYMAP *map;

	/* Build the list of possible values */

	for (map = keymaps; map != NULL; map = map->next) {
		/* Is this keymap a possible completion? */

		if (!strncasecmp(base, map->name, strlen(base))) {
			list = add_clist(list, map->name, FALSE);
		}
	}

	return(list);
}
/****************************************************************************/
CLIST *cmdmap_complete(list, base)
CLIST *list;
char *base;
{
	/* Return a list of commands or keymaps completing base */

	list = cmd_complete(list, base);
	list = map_complete(list, base);

	return(list);
}
/****************************************************************************/
