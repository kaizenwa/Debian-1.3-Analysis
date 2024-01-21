/* Tags.c - Tag handling functions for af.
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


#include <stdio.h>
#include <ctype.h>
#include "af.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include "tags.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: tags.c,v 1.11 1997/05/05 02:50:01 malc Exp $";
static char *TagId = TAGID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *xrealloc(), *xstrdup();
extern char *get_vtext(), *strkey();
extern unsigned count_messages();
extern void free(), emsg(), emsgl();

/* Local function declarations */

char *strtags();
int tagset();
void free_tlist(), free_texpr();
TAG_LIST *taglist();
TAG_EXPR *tagexpr();

static char *ttoken(), *tparen();
static void free_refs();
static TAG_EXPR *make_tnode(), *add_texpr();
static REFERENCE *add_refs(), *find_refs();
static MESSAGE *find_msg();

/****************************************************************************/
void set_sys_tags(message)
MESSAGE *message;
{
	/* Set the system tags for a message according to its status */

	char buf[MAX_SYS_TAGS + 1], *t = buf;

	/* Calculate the tags in alphabetic order */

	if (message->deleted) {
		*t++ = TAG_DELETED;
	}
	if (message->bad) {
		*t++ = TAG_ERROR;
	}
	if (message->nontext) {
		*t++ = TAG_MIME;
	}
	if (message->new) {
		*t++ = TAG_NEW;
	} else if (!message->read) {
		*t++ = TAG_UNREAD;
	}
	if (message->forwarded) {
		*t++ = TAG_FORWARDED;
	}
	if (message->printed) {
		*t++ = TAG_PRINTED;
	}
	if (message->replied) {
		*t++ = TAG_REPLIED;
	}
	if (message->saved) {
		*t++ = TAG_SAVED;
	}
	*t = '\0';

	/* Update the message's system tags */

	if (message->sys_tags != NULL) {
		free(message->sys_tags);
	}
	message->sys_tags = (t > buf) ? xstrdup(buf) : NULL;
	return;
}
/****************************************************************************/
TAG_LIST *taglist(tags, value)
char *tags;
int value;
{
	/* Check tags and form a tag list from it */

	char *t;
	int i;
	TAG_LIST *tlist;

	/* Allocate and zero the tag list */

	tlist = (TAG_LIST *) xmalloc(MAX_USER_TAGS * sizeof(TAG_LIST));
	for (i = 0; i < MAX_USER_TAGS; i++) {
		tlist[i] = TL_IGNORE;
	}

	/* Fill the tag list from the tags supplied */

	for (t = ttoken(tags); t != NULL; t = ttoken(t + 1)) {
		if ((int) TAGNO(*t) < 0) {
			emsgl("Invalid tag `", strkey(*t, SK_DISPLAY),
			      				"'", NULL);
			free(tlist);
			return(NULL);
		}
		tlist[TAGNO(*t)] = value;
	}

	/* Return the tag list */

	return(tlist);
}
/****************************************************************************/
void mask_tags(tlist)
TAG_LIST *tlist;
{
	/* Unset all non-persistent tags in the tag list */

	int i;
	TAG_LIST *plist;

	/* Get the list of persistent tags */

	plist = taglist(get_vtext(V_PTAGS), TL_SET);

	/* Unset the non-persistent tags */

	for (i = 0; i < MAX_USER_TAGS; i++) {
		tlist[i] += plist[i] + TL_UNSET;
	}

	free_tlist(plist);
	return;
}
/****************************************************************************/
int set_tags(message, tlist)
MESSAGE *message;
TAG_LIST *tlist;
{
	/* Set or unset tags in message according to tlist */

	char *tags;
	int i, changed = FALSE;
	TAG_LIST *old_tags;

	/* Get the old tags of the message */

	old_tags = taglist(message->user_tags, TL_SET);

	/* Update the tag list with the new tags */

	for (i = 0; i < MAX_USER_TAGS; i++) {
		old_tags[i] += tlist[i];
	}

	/* Generate the new tag string for the message */

	tags = strtags(old_tags);
	free_tlist(old_tags);

	/* Update the message's tags if they have changed */

	if (message->user_tags != NULL) {
		changed = (tags == NULL || strcmp(message->user_tags, tags));
		free(message->user_tags);
		message->user_tags = tags;
	} else if (tags != NULL) {
		changed = TRUE;
		message->user_tags = tags;
	}
		
	/* Return whether the tags were modified */

	return(changed);
}
/****************************************************************************/
MESSAGE **thread(message)
MESSAGE *message;
{
	/* Build a thread based on the message specified */

	int no_messages = 0, msg_found;
	MESSAGE *first = message, *m;
	MESSAGE **new_thread;
	REFERENCE *refs = NULL;

	/* First find the first message in the list */

	while (first->prev != NULL) {
		first = first->prev;
	}

	/* Allocate the return buffer to be big enough */

	new_thread = (MESSAGE **) xmalloc((count_messages(first, FALSE)
					   + 1) * sizeof (MESSAGE *));

	/* Add the original message to the lists */

	new_thread[no_messages++] = message;
	new_thread[no_messages] = NULL;
	refs = add_refs(refs, message->refs);

	/* Now loop until we've processed the entire thread */

	do {
		/* We haven't found a threaded message yet */

		msg_found = FALSE;

		/* Loop over every message in the buffer */

		for (m = first; m != NULL; m = m->next) {
			/* Is this message part of the thread? */

			if (m->visible && find_msg(new_thread, m) == NULL
				&& find_refs(refs, m->refs) != NULL) {
				/* Add the message to the thread */

				new_thread[no_messages++] = m;
				new_thread[no_messages] = NULL;
				refs = add_refs(refs, m->refs);
				msg_found = TRUE;
			}
		}
	} while (msg_found);

	/* Free the list of references */

	free_refs(refs);

	/* Resize and return the thread */

	new_thread = (MESSAGE **) xrealloc(new_thread,
		(no_messages + 1) * sizeof(MESSAGE *));
	return(new_thread);
}
/****************************************************************************/
static REFERENCE *add_refs(reflist, refs)
REFERENCE *reflist;
char *refs[];
{
	/* Add the message ids in refs to the list in ids */

	int r;
	REFERENCE *node, *n;

	/* Loop over the supplied references */

	for (r = 0; r < NO_REFERENCES; r++) {
		/* First check that this reference is set */

		if (refs[r] == NULL) {
			continue;
		}

		/* Create a new node in the list */

		node = (REFERENCE *) xmalloc(sizeof(REFERENCE));
		node->ref = refs[r];

		/* Do we want to prepend the node to the list? */

		if (reflist == NULL || strcmp(reflist->ref, node->ref) < 0) {
			node->next = reflist;
			reflist = node;
			continue;
		}

		/* Find the position for the node in the list */

		for (n = reflist; n != NULL; n = n->next) {
			/* Don't add duplicate nodes to the list */

			if (!strcmp(n->ref, node->ref)) {
				free(node);
				break;
			}

			/* Do we want to add the new node here? */

			if (n->next == NULL || strcmp(n->next->ref,
						      node->ref) < 0) {
				node->next = n->next;
				n->next = node;
				break;
			}
		}
	}

	/* And return the updated list */

	return(reflist);
}
/****************************************************************************/
static REFERENCE *find_refs(reflist, refs)
REFERENCE *reflist;
char *refs[];
{
	/* Find the first reference listed in reflist and refs */

	int r;
	REFERENCE *ref;

	/* Loop over the supplied references */

	for (r = 0; r < NO_REFERENCES; r++) {
		/* Find the reference in the list */

		for (ref = reflist; refs[r] != NULL && ref != NULL;
						ref = ref->next) {
			/* Does this reference match? */

			if (!strcmp(refs[r], ref->ref)) {
				return(ref);
			}
		}
	}

	/* No matching reference found */
	
	return(NULL);
}
/****************************************************************************/
static MESSAGE *find_msg(thread, message)
MESSAGE **thread, *message;
{
	/* Find a given message within a thread */

	MESSAGE **m;

	/* Loop over each message in the thread */

	for (m = thread; *m != NULL; m++) {
		/* Is this the message we're looking for? */

		if (*m == message) {
			return(*m);
		}
	}

	/* No match for the message in the list */

	return(NULL);
}
/****************************************************************************/
static void free_refs(reflist)
REFERENCE *reflist;
{
	/* Free the reference list refs */

	REFERENCE *ref = reflist, *next;

	/* Free each reference in the list */

	while (ref != NULL) {
		next = ref->next;
		free(ref);
		ref = next;
	}
	return;
}
/****************************************************************************/
char *strtags(tlist)
TAG_LIST *tlist;
{
	/* Return the tags set in tlist as an allocated string */

	char *buf, *t;
	int len, i;

	/* Allocate the return buffer */

	t = buf = xmalloc(MAX_USER_TAGS + 1);

	/* Fill the buffer with any set tags */

	for (i = 0; i < MAX_USER_TAGS; i++) {
		if (tlist[i] > 0) {
			*t++ = TAGCHAR(i);
		}
	}
	*t = '\0';

	/* Resize the buffer as required */

	if ((len = strlen(buf)) > 0) {
		buf = xrealloc(buf, len + 1);
	} else {
		free(buf);
		buf = NULL;
	}

	/* Return the string */

	return(buf);
}
/****************************************************************************/
void free_tlist(tlist)
TAG_LIST *tlist;
{
	/* Free the space taken up by a tag list */

	free(tlist);
	return;
}
/****************************************************************************/
TAG_EXPR *tagexpr(win, tags)
WINDOW *win;
char *tags;
{
	/*
	 * Check tags and form a tag expression from them.
	 * The tag expression is in effect a parse tree of the
	 * expression supplied in tags, with parentheses handled
	 * at this level, rather than during descent of the tree.
	 */

	static int depth = 0;		/* Paren recursion depth */
	int last_op = TE_AND;		/* Make sure it's all valid */
	TAG_EXPR *texpr = NULL, *node;
	MESSAGE *m;

	/* Initialise the current token and recursion depth */

	tags = ttoken(tags);
	depth++;

	/* Loop until the end of the expression */

	while (tags != NULL && *tags != EXPR_RPAREN) {
		/* Make a node from the token */

		if ((node = make_tnode(*tags, last_op)) == NULL) {
			free_texpr(texpr);
			depth--;
			return(NULL);
		}

		/* Update the token and last operator */

		tags = ttoken(tags + 1);
		last_op = node->operator;

		/* Form the subtree for a parenthesis */

		if (node->operator == TE_PAREN) {
			if ((node->right = tagexpr(win, tags)) == NULL) {
				free_texpr(texpr);
				free_texpr(node);
				depth--;
				return(NULL);
			}
			tags = tparen(tags);
		}

		/* Add the node to the expression tree */

		texpr = add_texpr(texpr, node);
	}

	/* Decrement the paren recursion level */

	depth--;

	/* Check for errors at end of expression */

	if (!BEFOREEND(last_op) || depth == 0 && tags != NULL
	    || depth > 0 && tags == NULL) {
		emsgl("Invalid expression: ", (tags != NULL) ?
		      "`)'" : "end", " unexpected", NULL);
		free_texpr(texpr);
		return(NULL);
	}

	/* If the window is NULL then all is well */

	if (win == NULL) {
		return(texpr);
	}

	/* Otherwise, check that the expression matches a message */

	for (m = win->buf->messages; m->text != NULL; m = m->next) {
		if (tagset(m, texpr)) {
			return(texpr);
		}
	}

	/* Expression generates an empty tagset */

	emsg("(No messages in tagset)");
	free_texpr(texpr);

	return(NULL);
}
/****************************************************************************/
int tagset(message, texpr)
MESSAGE *message;
TAG_EXPR *texpr;
{
	/*
	 * Return TRUE if message is matched by texpr.
	 * This is achieved by recursive descent of the parse
	 * tree generated for the tag expression.
	 */

	/* How we handle each node depends on the operator */

	switch(texpr->operator) {
	case TE_TAG:
		/* TRUE if the tag is active on the message */

		return(message->sys_tags != NULL
		       && strchr(message->sys_tags, texpr->tag) != NULL
		       || message->user_tags != NULL
		       && strchr(message->user_tags, texpr->tag) != NULL);
	case TE_PAREN:
		/* TRUE if right subtree is TRUE */

		return(tagset(message, texpr->right));
	case TE_NOT:
		/* TRUE if right subtree is FALSE */

		return(!tagset(message, texpr->right));
	case TE_AND:
		/* TRUE if left subtree and right subtree TRUE */

		return(tagset(message, texpr->left)
		       && tagset(message, texpr->right));
	case TE_XOR:
		/* TRUE if left subtree or right subtree TRUE */

		return((tagset(message, texpr->left))
		       ? !tagset(message, texpr->right)
		       : tagset(message, texpr->right));
	case TE_OR:
		/* TRUE if left subtree or right subtree TRUE */

		return(tagset(message, texpr->left)
		       || tagset(message, texpr->right));
	}
	/*NOTREACHED*/
}
/****************************************************************************/
void free_texpr(texpr)
TAG_EXPR *texpr;
{
	/* Free a tag expression */

	if (texpr != NULL) {
		free_texpr(texpr->left);
		free_texpr(texpr->right);
		free(texpr);
	}

	return;
}
/****************************************************************************/
static char *ttoken(text)
char *text;
{
	/* Return the next token in text, or NULL if none */

	char *t;

	/* Search for the next non-whitespace character */

	for (t = text; t != NULL && isspace(*t); t++) {
		/* NULL LOOP */
	}

	/* Return the start of any token */

	return((t != NULL && *t != '\0') ? t : NULL);
}
/****************************************************************************/
static char *tparen(text)
char *text;
{
	/* Return the token after a parenthesised expression */

	char *t = text;
	int depth = 1;

	/* Search for the next token */

	while (t != NULL && depth > 0) {
		/* Update the paren nesting depth */

		if (*t == EXPR_LPAREN) {
			depth++;
		} else if (*t == EXPR_RPAREN) {
			depth--;
		}
		t = ttoken(t + 1);
	}

	/* Return the start of any token or NULL if none */

	return(t);
}
/****************************************************************************/
static TAG_EXPR *make_tnode(token, last_op)
int token, last_op;
{
	/* Make a TAG_EXPR node from the token, checking as required */

	char *te_ops = EXPR_OPS, *op;
	int operator = TE_TAG, tag = '\0';
	TAG_EXPR *node;

	/* Check if the token is a tag or operator */

	if (strchr(EXPR_TAGS, token) != NULL) {
		tag = token;
	} else if ((op = strchr(te_ops, token)) != NULL) {
		/* te_ops is ordered so index gives operator */

		operator = (op - te_ops);
	} else {
		emsgl("Invalid expression: `", strkey(token, SK_KEYSEQ),
		      "' illegal", NULL);
		return(NULL);
	}

	/* Now check the token for syntax errors */

	if (ISOP(last_op) && !FOLLOWOP(operator) ||
	    ISATOM(last_op) && !FOLLOWATOM(operator)) {
		emsgl("Invalid expression: `", strkey(token, SK_KEYSEQ),
		      "' unexpected", NULL);
		return(NULL);
	}

	/* Make a node for the new token */

	node = (TAG_EXPR *) xmalloc(sizeof(TAG_EXPR));
	node->tag = tag;
	node->operator = operator;
	node->left = node->right = NULL;

	return(node);
}
/****************************************************************************/
static TAG_EXPR *add_texpr(texpr, node)
TAG_EXPR *texpr, *node;
{
	/* Add node to the tag expression texpr */

	/* Push the node onto the tree if possible */

	if (texpr == NULL || EXPR_PUSH(texpr->operator, node->operator)) {
		node->left = texpr;
		return(node);
	}

	/* Try adding the node to the right subtree */

	texpr->right = add_texpr(texpr->right, node);
	return(texpr);
}
/****************************************************************************/
