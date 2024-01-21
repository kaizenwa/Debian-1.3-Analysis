/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USERS OWN RISK.
 */
#include "global.h"
#include "util.h"
#include "resource.h"
#include "thread.h"

ARTICLE *next_in_thread_preorder(ARTICLE *art)
{
    if (A_CHILD1(art))
	return A_CHILD1(art);

    while (!A_SIBLING(art))
	if (!(art = A_PARENT(art)))
	    return NULL;

    return A_SIBLING(art);
}

ARTICLE *next_in_subthread_preorder(ARTICLE *art, ARTICLE *thread)
{
    if (A_CHILD1(art))
	return A_CHILD1(art);

    if (art == thread)
	return NULL;

    while (!A_SIBLING(art)) {
	if (!(art = A_PARENT(art)))
	    return NULL;
	if (art == thread)
	    return NULL;
    }

    return A_SIBLING(art);
}

ARTICLE *preorder_skip_subthread(ARTICLE *art)
{
    while (!A_SIBLING(art))
	if (!(art = A_PARENT(art)))
	    return NULL;

    return A_SIBLING(art);
}

ARTICLE *next_in_thread_wrap(ARTICLE *art)
{
    int height = 0;
    int flag = FALSE;

    do {
	while (!A_SIBLING(art) && A_PARENT(art)) {
	    art = A_PARENT(art);
	    height++;
	}

	if (A_SIBLING(art))
	    art = A_SIBLING(art);
	else {
	    if (flag)
		return NULL;
	    art = art->subject->thread;
	    flag = True;
	    height++;
	}

	while (height > 0 && A_CHILD1(art)) {
	    art = A_CHILD1(art);
	    height--;
	}
    } while (height > 0);

    return art;
}

ARTICLE *next_in_subthread_wrap(ARTICLE *art, ARTICLE *thread)
{
    int height = 0;
    int flag = FALSE;

    if (art == thread)
	return A_CHILD1(thread);

    do {
	while (!A_SIBLING(art) && A_PARENT(art) != thread) {
	    art = A_PARENT(art);
	    height++;
	}

	if (A_SIBLING(art))
	    return A_SIBLING(art);
	else {
	    if (flag)
		return NULL;
	    flag = True;
	    if (thread)
		art = A_CHILD1(thread);
	    else
		art = art->subject->thread;
	    height++;
	}

	while (height > 0 && A_CHILD1(art)) {
	    art = A_CHILD1(art);
	    height--;
	}
    } while (height > 0);

    return art;
}

ARTICLE *next_in_thread_dont_wrap(ARTICLE *art)
{
    int	height = 0;

    do {
	while (!A_SIBLING(art) && A_PARENT(art)) {
	    art = A_PARENT(art);
	    height++;
	}

	if (A_SIBLING(art))
	    art = A_SIBLING(art);
	else
	    return NULL;

	while (height > 0 && A_CHILD1(art)) {
	    art = A_CHILD1(art);
	    height--;
	}
    } while (height > 0);

    return art;
}

static ARTICLE *prev_sibling(ARTICLE *art)
{
    ARTICLE	*result = A_PARENT(art);

    if (result)
	result = A_CHILD1(result);
    else
	result = art->subject->thread;

    while (result && A_SIBLING(result) != art)
	result = A_SIBLING(result);

    return result;
}

ARTICLE *prev_in_thread_dont_wrap(ARTICLE *art)
{
    int		height = 0;
    ARTICLE	*prev;

    do {
	while (!(prev = prev_sibling(art)) && A_PARENT(art)) {
	    art = A_PARENT(art);
	    height++;
	}

	if (prev)
	    art = prev;
	else
	    return NULL;

	while (height > 0 && A_CHILD1(art)) {
	    art = A_CHILD1(art);
	    while (A_SIBLING(art))
		art = A_SIBLING(art);
	    height--;
	}
    } while (height > 0);

    return art;
}

void update_subj_hot_value(SUBJECT *subj)
{
    ARTICLE	*art;

    subj->pixmap = None;
    for (art = subj->thread ; art ; art = next_in_thread_preorder(art)) {
	if (art->subject == subj && art->from &&
	    !art->read && art->pixmap != None) {
	    subj->pixmap = art->pixmap;
	    break;
	}
    }
}

long mark_subject_unread(SUBJECT *subj)
{
    ARTICLE	*art = subj->thread;
    long	n = 0;
    
    while (art) {
	if (art->subject == subj && art->from && art->read) {
	    art->read = FALSE;
	    if (art->pixmap != None)
		global.n_hot++;
	    art->subject->no_unread++;
	    n++;
	}
	art = next_in_thread_preorder(art);
    }

    global.curr_group->no_unread += n;

    return n;
}

long mark_sub_subject_unread(ARTICLE *thread, SUBJECT *subj)
{
    ARTICLE	*art = thread;
    long	n = 0;

    while (art) {
	if (art->subject == subj && art->from && art->read) {
	    art->read = FALSE;
	    if (art->pixmap != None)
		global.n_hot++;
	    subj->no_unread++;
	    n++;
	}
	art = next_in_subthread_preorder(art, thread);
    }

    global.curr_group->no_unread += n;

    return n;
}

long mark_subject_read(SUBJECT *subj, int xref, int kill)
{
    ARTICLE	*art = subj->thread;
    long	n = 0;
    
    while (art) {
	if (art->subject == subj && art->from &&
	    !art->read && (art->pixmap == None || !kill)) {
	    art->read = TRUE;
	    if (xref)
		process_xref(art);
	    if (kill)
		art->killed = TRUE;
	    if (art->pixmap != None)
		global.n_hot--;
	    art->subject->no_unread--;
	    n++;
	}
	art = next_in_thread_preorder(art);
    }

    global.curr_group->no_unread -= n;

    return n;
}

long mark_sub_subject_read(ARTICLE *thread, SUBJECT *subj, int xref, int kill)
{
    ARTICLE	*art = thread;
    long	n = 0;

    while (art) {
	if (art->subject == subj && art->from &&
	    !art->read && (art->pixmap == None || !kill)) {
	    art->read = TRUE;
	    if (xref)
		process_xref(art);
	    if (kill)
		art->killed = TRUE;
	    if (art->pixmap != None)
		global.n_hot--;
	    subj->no_unread--;
	    n++;
	}
	art = next_in_subthread_preorder(art, thread);
    }

    global.curr_group->no_unread -= n;

    return n;
}

long mark_thread_read(ARTICLE *art, int xref, int kill)
{
    long	n = 0;

    while (art) {
	if (art->from && !art->read && (art->pixmap == None || !kill)) {
	    art->read = TRUE;
	    if (xref)
		process_xref(art);
	    if (kill)
		art->killed = TRUE;
	    if (art->pixmap != None)
		global.n_hot--;
	    art->subject->no_unread--;
	    n++;
	}
	art = next_in_thread_preorder(art);
    }

    global.curr_group->no_unread -= n;

    return n;
}

long mark_subthread_read(ARTICLE *art, int xref, int kill)
{
    ARTICLE	*thread = art;
    long	n = 0;

    while (art) {
	if (art->from && !art->read && (art->pixmap == None || !kill)) {
	    art->read = TRUE;
	    if (xref)
		process_xref(art);
	    if (kill)
		art->killed = TRUE;
	    if (art->pixmap != None)
		global.n_hot--;
	    art->subject->no_unread--;
	    n++;
	}
	art = next_in_subthread_preorder(art, thread);
    }

    global.curr_group->no_unread -= n;

    return n;
}

long mark_subject_hot(SUBJECT *subj, Pixmap pixmap)
{
    ARTICLE	*art = subj->thread;
    long	n = 0;

    while (art) {
	if (art->from && art->subject == subj && art->pixmap == None) {
	    art->pixmap = pixmap;
	    if (!art->read)
		n++;
	}
	art = next_in_thread_preorder(art);
    }

    return n;
}

long mark_thread_hot(ARTICLE *art, Pixmap pixmap)
{
    long	n = 0;

    while (art) {
	if (art->from && art->pixmap == None) {
	    art->pixmap = pixmap;
	    if (!art->read)
		n++;
	}
	art = next_in_thread_preorder(art);
    }

    return n;
}

long mark_subthread_hot(ARTICLE *art, Pixmap pixmap)
{
    ARTICLE	*thread = art;
    long	n = 0;

    while (art) {
	if (art->from && art->pixmap == None) {
	    art->pixmap = pixmap;
	    if (!art->read)
		n++;
	}
	art = next_in_subthread_preorder(art, thread);
    }

    return n;
}

static void add_to_read_arts(GROUP *group, long no)
{
    ART_LIST_NODE *loop, *prev;

    for (prev = NULL, loop = group->read_arts ;
	 loop ; prev = loop, loop = loop->next)
	if (no <= loop->last)
	    break;

    if (loop && no >= loop->first)
	return;

    if (loop && no == loop->first - 1) {
	loop->first--;
	if (prev && prev->last + 1 == loop->first) {
	    prev->last = loop->last;
	    prev->next = loop->next;
	    XtFree((char *)loop);
	}
    } else if (prev && no == prev->last + 1) {
	prev->last++;
	/* no need to check for merging */
    } else {
	ART_LIST_NODE *temp;

	temp = (ART_LIST_NODE *)XtMalloc(sizeof *temp);
	temp->first = temp->last = no;
	temp->next = loop;
	if (prev)
	    prev->next = temp;
	else
	    group->read_arts = temp;
    }
}

void process_xref(ARTICLE *art)
{
    char	*xref;
    char	*c1, *c2;
    long	i, art_no;

    c1 = xref = art->xref;
    art->xref = NULL;

    while (c1 && *c1 == ' ')
	c1++;
    while (c1 && (c2 = strchr(c1, ':')) ) {
	*(c2++) = '\0';
	for (i = 0 ; i < global.no_groups ; i++) {
	    if (!global.groups[i]->subscribed) {
		if ( (c1 = strchr(c2, ' ')) )
		    c1++;
		break;
	    }
	    if (case_strcmp(global.groups[i]->name, c1) == 0 &&
		global.groups[i] != global.curr_group) {
		if ( (c1 = strchr(c2, ' ')) )
		    *(c1++) = '\0';
		art_no = atol(c2);
		add_to_read_arts(global.groups[i], art_no);
		break;
	    }
	}
	while (c1 && *c1 == ' ')
	    c1++;
    }
    
    XtFree(xref);
}

void fake_xref(ARTICLE *art, char **headers, int n)
{
    if (art->xref)
	return;

    while (n-- > 0)
	if (case_strncmp(*headers, "Xref:", 5) != 0)
	    headers++;
	else {
	    char	*c = *headers + 5;

	    while (*c == ' ' || *c == '\t')
		c++;
	    c = strchr(c, ' ');
	    while (*c == ' ' || *c == '\t')
		c++;
	    if (*c != '\0')
		art->xref = XtNewString(c);
	    break;
	}
}

void free_read_arts_list(GROUP *group)
{
    ART_LIST_NODE *next, *loop = group->read_arts;

    group->read_arts = NULL;

    while (loop) {
	next = loop->next;
	XtFree((char *)loop);
	loop = next;
    }
}

static ART_LIST_NODE *get_read_arts_list(ARTICLE *art)
{
    ART_LIST_NODE *temp;

    while (art->next && !art->next->read && art->next->no == art->no + 1)
	art = art->next;

    if (!art->next)
	return NULL;

    temp = (ART_LIST_NODE *)XtMalloc(sizeof *temp);
    temp->first = art->no + 1;

    if (art->next->read) {
	while (art->next && art->next->read)
	    art = art->next;

	if (art->next) {
	    temp->last = art->next->no - 1;
	    temp->next = get_read_arts_list(art->next);
	} else {
	    temp->last = global.curr_group->last_art;
	    temp->next = NULL;
	}
    } else {
	temp->last = art->next->no - 1;
	temp->next = get_read_arts_list(art->next);
    }

    return temp;
}

ART_LIST_NODE *create_read_arts_list(void)
{
    ARTICLE		*art, *arts = get_articles(main_thr);
    ART_LIST_NODE	*list, *last;

    art = arts;
    while (art && !art->read)
	art = art->next;
    if (!art) {
	if (!res_ask_how_many() || !arts || arts->no <= 1)
	    return NULL;

	list = (ART_LIST_NODE *)XtMalloc(sizeof *list);
	list->first = 1;
	list->last = arts->no - 1;
	list->next = NULL;
	return list;
    }

    art = arts;
    if (art->no == 1 && !art->read)
	list = get_read_arts_list(art);
    else {
	list = (ART_LIST_NODE *)XtMalloc(sizeof *list);
	list->first = 1;

	while (art && art->read)
	    art = art->next;

	if (art) {
	    list->last = art->no - 1;
	    list->next = get_read_arts_list(art);
	} else {
	    list->last = global.curr_group->last_art;
	    list->next = NULL;
	}
    }

    if (!list || !arts)
	return NULL;

    /*
     *  Hack in case last articles(s) cancelled...
     */

    last = list;
    while (last->next)
	last = last->next;

    art = arts;
    while (art->next)
	art = art->next;

    if (art->no < global.curr_group->last_art)
	if (last->last == art->no)
	    last->last = global.curr_group->last_art;
	else if (last->last < art->no) {
	    last->next = (ART_LIST_NODE *)XtMalloc(sizeof *last);
	    last = last->next;
	    last->first = art->no + 1;
	    last->last = global.curr_group->last_art;
	    last->next = NULL;
	}

    return list;
}

ARTICLE *first_unread_article_with_subject(SUBJECT *subj)
{
    ARTICLE	*art, *first = NULL;

    for (art = subj->thread ; art ; art = next_in_thread_preorder(art))
	if (art->tree_data.label && art->subject == subj)
	    if (!art->read)
		return art;
	    else if (!first)
		first = art;

    return first;
}

/*********************************************************************/

void ascii_nlower(char *c, long n)
{
    while (n-- > 0) {
	if (IS_UPPER(*c))
	    *c = LOWER(*c);
	c++;
    }
}

void ascii_lower(char *c)
{
    while (*c != '\0') {
	if (IS_UPPER(*c))
	    *c = LOWER(*c);
	c++;
    }
}

void memcpy_lower(char *dest, char *src, long n)
{
    while (n-- > 0) {
	*dest++ = TO_LOWER(*src);
	src++;
    }
}

int case_strncmp(const char *c1, const char *c2, long n)
{
    while (n--) {
	int	tmp = TO_LOWER(*c1) - TO_LOWER(*c2);

	if (tmp != 0)
	    return tmp;

	if (*c1 == '\0')
	    return 0;

	c1++;
	c2++;
    }

    return 0;
}

int case_strcmp(const char *c1, const char *c2)
{
    for (;;) {
	int	tmp = TO_LOWER(*c1) - TO_LOWER(*c2);

	if (tmp != 0)
	    return tmp;

	if (*c1 == '\0')
	    return 0;

	c1++;
	c2++;
    }
}

int case_lstrncmp(const char *c1, const char *c2, long n)
{
    while (n--) {
	int	tmp = TO_LOWER(*c1) - (unsigned char)*c2;

	if (tmp != 0)
	    return tmp;

	if (*c1 == '\0')
	    return 0;

	c1++;
	c2++;
    }

    return 0;
}

int case_lstrcmp(const char *c1, const char *c2)
{
    for (;;) {
	int	tmp = TO_LOWER(*c1) - (unsigned char)*c2;

	if (tmp != 0)
	    return tmp;

	if (*c1 == '\0')
	    return 0;

	c1++;
	c2++;
    }
}

int case_lhassub(const char *haystack, const char *needle)
{
    int	ch = *needle++;
    int	n = strlen(needle);

    while (*haystack != '\0')
	if (TO_LOWER(*haystack) == ch &&
	    case_lstrncmp(haystack + 1, needle, n) == 0)
	    return True;
	else
	    haystack++;

    return False;
}

