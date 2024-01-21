/* Mailsort.c - Routines to sort messages for af.
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
#include "mailsort.h"
#include "keyseq.h"
#include "functions.h"
#include "variable.h"
#include "tags.h"
#include "complete.h"
#include STRING_HDR

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: mailsort.c,v 1.8 1997/04/20 10:32:45 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *vstrcat();
extern int strcasecmp(), strncasecmp();
extern int get_vval(), tagset();
extern void free(), qsort(), msg();
extern void msgl(), emsgl(), cmsg();
extern CLIST *add_clist();

/* Local function declarations */

static void do_sort();
static SORT *find_sort();

/****************************************************************************/
/* Is this sort reverse-ordered? */

static int sort_reverse = FALSE;

/****************************************************************************/
int sort_msgs(win, stype, sortname, start, end, texpr)
WINDOW *win;
char *stype, *sortname;
MESSAGE *start, *end;
TAG_EXPR *texpr;
{
	/* Handle sorting of messages */

	SORT *sort;

	/* Get the details of the selected sort type */

	if ((sort = find_sort(sortname)) == NULL) {
		emsgl("No sort type ", sortname, NULL);
		return(FALSE);
	}

	/* Let the user know what we're doing */

	msgl("Sorting ", stype, "...", NULL);

	/* Set the 'search is reverse flag as appropriate */

	sort_reverse = sort->reverse;

	/* Actually do the sort */

	do_sort(win, win->buf, sort, start, end, texpr);

	/* Confirm the sort and return success */

	cmsg(" Done");
	return(TRUE);
}
/****************************************************************************/
int sort_default(buf, sortname)
MAILBUF *buf;
char *sortname;
{
	/* Handle default sorting of buffers */

	SORT *sort;

	/* Get the details of the specified sort type */

	if ((sort = find_sort(sortname)) == NULL) {
		emsgl("No sort type ", sortname, NULL);
		return(FALSE);
	}

	/* Let the user know what we're doing */

	msg("Sorting messages...");

	/* Set the 'search is reverse flag as appropriate */

	sort_reverse = sort->reverse;

	/* Actually do the sort */

	do_sort(NULL, buf, sort, buf->messages, NULL, NULL);

	/* Confirm the sort and return success */

	cmsg(" Done");
	return(TRUE);
}
/****************************************************************************/
static void do_sort(win, buf, sort, start, end, texpr)
WINDOW *win;
MAILBUF *buf;
SORT *sort;
MESSAGE *start, *end;
TAG_EXPR *texpr;
{
	/*
	 * Actually sort a list of messages.  This is done by
	 * copying details of the messages to be sorted into two
	 * arrays of structures, one to be sorted and one holding
	 * information about the connectivity of the messages.
	 * We then use qsort to sort the array to be sorted, and
	 * the information in the second array is then used to
	 * restore the connectivity of the list as required.
	 *
	 * This is all complicated by the fact that we may not be
	 * sorting continuous lists of messages.  The idea is that
	 * pointers to messages to be sorted are made NULL, to be
	 * updated later, while pointers to messages not included
	 * in the sort are left intact to be restored after sorting.
	 */
	
	int sortlen = 0, s = 0;
	MESSAGE *m, *prev_msg;
	SORT_MSG *tosort;
	SORT_INFO *sortinfo;

	/* Count the number of messages to be sorted */

	for (m = start; m->text != NULL && m != end; m = m->next) {
		if (m->visible && (texpr == NULL || tagset(m, texpr))) {
			sortlen++;
		}
	}

	/* Allocate arrays to hold details of the messages */

	tosort = (SORT_MSG *) xmalloc(sortlen * sizeof(SORT_MSG));
	sortinfo = (SORT_INFO *) xmalloc(sortlen * sizeof(SORT_INFO));

	/* Copy details of the messages into the arrays */

	for (m = start; m->text != NULL && m != end; m = m->next) {
		if (m->visible && (texpr == NULL || tagset(m, texpr))) {
			/* Add the message and info to the arrays */

			tosort[s].msg = m;
			tosort[s].index = s;
			sortinfo[s].prev = m->prev;
			sortinfo[s].next = m->next;

			/* Unset pointers to messages to be sorted */
		
			if (s > 0 && m->prev == tosort[s - 1].msg) {
				sortinfo[s].prev = NULL;
				sortinfo[s - 1].next = NULL;
			}

			/* Set the other information for the sort */

			sortinfo[s].point = (win != NULL && win->point == m);
			sortinfo[s].mark = (win != NULL && win->mark == m);
			sortinfo[s++].first = (win != NULL &&
					       win->first == m);
		}
	}

	/* Now use qsort to sort the array */

	qsort(tosort, sortlen, sizeof(SORT_MSG), sort->func);

	/* Update the buffer if the first message was sorted */

	if (sortinfo[0].prev == NULL) {
		buf->messages = tosort[0].msg;
	}
	prev_msg = NULL;

	/* Restore the connectivity of the sorted messages */

	for (s = 0; s < sortlen; s++) {
		/* Restore previous and next pointers */

		if ((tosort[s].msg->prev = sortinfo[s].prev) != NULL
		    || (tosort[s].msg->prev = prev_msg) != NULL) {
			tosort[s].msg->prev->next = tosort[s].msg;
		}
		if ((tosort[s].msg->next = sortinfo[s].next) != NULL) {
			tosort[s].msg->next->prev = tosort[s].msg;
		}
		prev_msg = tosort[s].msg;

		/* Restore positions of point, mark, and first */

		if (win != NULL && sortinfo[s].point) {
			win->point = tosort[s].msg;
		}
		if (win != NULL && sortinfo[s].mark) {
			win->mark = tosort[s].msg;
		}
		if (win != NULL && sortinfo[s].first) {
			win->first = tosort[s].msg;
		}
	}

	/* Free the arrays and return */

	free(tosort);
	free(sortinfo);
	return;
}
/****************************************************************************/
static SORT *find_sort(sortname)
char *sortname;
{
	/* Return the SORT entry relating to the named sort type */
	
	SORT *sort;

	/* Check the list of sort types */

	for (sort = sorts; sort->name != NULL; sort++) {
		/* Is this the sort type we're looking for? */

		if (!strcasecmp(sort->name, sortname)) {
			return(sort);
		}
	}

	/* No such sort type */

	return(NULL);
}
/****************************************************************************/
CLIST *sort_complete(list, base)
CLIST *list;
char *base;
{
	/* Return a list of sort types completing base */

	SORT *sort;

	/* Build the list of possible values */

	for (sort = sorts; sort->name != NULL; sort++) {
		/* Does this sort type complete the base? */

		if (!strncasecmp(base, sort->name, strlen(base))) {
			list = add_clist(list, sort->name, FALSE);
		}
	}

	return(list);
}
/****************************************************************************/
static int ms_address(m1, m2)
SORT_MSG *m1, *m2;
{
	/* Compare two messages by sender address*/

	int order;

	/* Compare the address of the messages */

	if ((order = strcasecmp(m1->msg->addr, m2->msg->addr)) == 0) {
		/* Addresses are identical, return original ordering */

		return(m1->index - m2->index);
	}
	return((sort_reverse) ? -order : order);
}
/****************************************************************************/
static int ms_date(m1, m2)
SORT_MSG *m1, *m2;
{
	/* Compare two messages by date */

	long d1, d2;
	int order, local;

	/* Check that both messages are dated */

	if (m1->msg->date == NULL && m2->msg->date == NULL) {
		return(m1->index - m2->index);
	} else if (m1->msg->date == NULL) {
		return(SC_GREATER_THAN);
	} else if (m2->msg->date == NULL) {
		return(SC_LESS_THAN);
	}

	/* Correct the values of the dates for time zones */

	local = get_vval(V_SHOW_LTIME);
	d1 = (local) ? m1->msg->date->d_date
		     : m1->msg->date->d_date + m1->msg->date->d_zone;
	d1 = (!local && d1 < 0) ? d1 - m1->msg->date->d_zone : d1;
	d2 = (local) ? m2->msg->date->d_date
		     : m2->msg->date->d_date + m2->msg->date->d_zone;
	d2 = (!local && d2 < 0) ? d2 - m2->msg->date->d_zone : d2;

	/* Now return the comparison value */

	order = (d1 == d2) ? m1->index - m2->index :
		(d1 < d2) ? SC_LESS_THAN : SC_GREATER_THAN;
	return((sort_reverse) ? -order : order);
}
/****************************************************************************/
static int ms_lines(m1, m2)
SORT_MSG *m1, *m2;
{
	/* Compare two messages by number of lines */

	int order, l1 = 0, l2 = 0;
	MSG_TEXT *t;

	/* Calculate the number of lines in each message */

	for (t = m1->msg->text; t != NULL; t = t->next) {
		l1++;
	}
	for (t = m2->msg->text; t != NULL; t = t->next) {
		l2++;
	}

	/* Calculate the ordering of the message */

	if ((order = l1 - l2) == 0) {
		order = m1->index - m2->index;
	}
	return(order);
}
/****************************************************************************/
static int ms_mailbox(m1, m2)
SORT_MSG *m1, *m2;
{
	/* Compare two messages by mailbox ordering */

	return((sort_reverse) ? m2->msg->pos - m1->msg->pos
			      : m1->msg->pos - m2->msg->pos);
}
/****************************************************************************/
static int ms_sender(m1, m2)
SORT_MSG *m1, *m2;
{
	/* Compare two messages by sender name */

	int order;

	/* Compare the sender of the messages */

	if ((order = strcasecmp(m1->msg->from, m2->msg->from)) == 0) {
		/* Senders are identical, return original ordering */

		return(m1->index - m2->index);
	}
	return((sort_reverse) ? -order : order);
}
/****************************************************************************/
static int ms_status(m1, m2)
SORT_MSG *m1, *m2;
{
	/* Compare two messages by system tags */

	int order;

	/* Check that both messages have system tags */

	if (m1->msg->sys_tags == NULL && m2->msg->sys_tags == NULL) {
		return(m1->index - m2->index);
	} else if (m1->msg->sys_tags == NULL) {
		return(SC_GREATER_THAN);
	} else if (m2->msg->sys_tags == NULL) {
		return(SC_LESS_THAN);
	}

	/* Compare the system tags of the messages */

	if ((order = strcasecmp(m1->msg->sys_tags, m2->msg->sys_tags)) == 0) {
		/* System tags identical, return original ordering */

		return(m1->index - m2->index);
	}
	return((sort_reverse) ? -order : order);
}
/****************************************************************************/
static int ms_subject(m1, m2)
SORT_MSG *m1, *m2;
{
	/* Compare two messages by subject */

	char *s1, *s2;
	int rlen, pl1, pl2;
	int order;

	/* Get the lengths of the reply prefix */

	rlen = strlen(REPLY_PFX);

	/* Does either subject begin with a reply prefix? */

	pl1 = (!strncasecmp(m1->msg->subject, REPLY_PFX, rlen)) ? rlen : 0;
	pl2 = (!strncasecmp(m2->msg->subject, REPLY_PFX, rlen)) ? rlen : 0;

	/* Set up the subjects, ignoring reply prefixes */

	s1 = m1->msg->subject + pl1;
	s2 = m2->msg->subject + pl2;

	/* Skip white space at the start of the subjects */

	while (isspace(*s1)) {
		s1++;
	}
	while (isspace(*s2)) {
		s2++;
	}

	/* Compare the subjects of the messages */

	if ((order = strcasecmp(s1, s2)) == 0) {
		if ((order = (pl1 - pl2)) == 0) {
			/* Subjects ientical, return original ordering */

			return(m1->index - m2->index);
		}
	}

	return((sort_reverse) ? -order : order);
}
/****************************************************************************/
static int ms_tags(m1, m2)
SORT_MSG *m1, *m2;
{
	/* Compare two messages by user tags */

	int order;

	/* Check that both messages have user tags */

	if (m1->msg->user_tags == NULL && m2->msg->user_tags == NULL) {
		return(m1->index - m2->index);
	} else if (m1->msg->user_tags == NULL) {
		return(SC_GREATER_THAN);
	} else if (m2->msg->user_tags == NULL) {
		return(SC_LESS_THAN);
	}

	/* Compare the user tags of the messages */

	if (!(order = strcasecmp(m1->msg->user_tags, m2->msg->user_tags))) {
		/* User tags identical, return original ordering */

		return(m1->index - m2->index);
	}
	return((sort_reverse) ? -order : order);
}
/****************************************************************************/
