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
#include "thread.h"
#include "util.h"
#include "widgets.h"
#include "../Widgets/ArtTree.h"

static ARTICLE	**tagged_articles = NULL;
static long	no_tagged = 0;
static long	no_alloc = 0;

static void add_tag(ARTICLE *art)
{
    if (no_alloc < no_tagged + 3) {
	long	i = no_alloc;

	no_alloc = 2 * (no_alloc + 1);
	tagged_articles =
	    (ARTICLE **)XtRealloc((char *)tagged_articles,
				  no_alloc * sizeof(ARTICLE*));
	while (i < no_alloc)
	    tagged_articles[i++] = NULL;
    }

    tagged_articles[no_tagged++] = art;
    art->subject->has_tagged = True;
    update_subj_entry(art->subject);
}

static void remove_tag(ARTICLE *art)
{
    long	i, n = no_tagged;
    int		found_subj = False;

    for (i = 0 ; i < n ; i++) {
	if (tagged_articles[i] == art) {
	    while (i < n) {
		tagged_articles[i] = tagged_articles[i + 1];
		if (tagged_articles[i] &&
		    tagged_articles[i]->subject == art->subject)
		    found_subj = True;
		i++;
	    }
	    no_tagged--;
	    art->subject->has_tagged = found_subj;
	    if (!found_subj)
		update_subj_entry(art->subject);
	    return;
	}

	if (tagged_articles[i]->subject == art->subject)
	    found_subj = True;
    }
}

static int is_tagged(ARTICLE *art)
{
    ARTICLE	**tagged = tagged_articles;
    long	n = no_tagged;

    if (art->subject->has_tagged)
	while (n > 0) {
	    if (*tagged == art)
		return True;
	    tagged++;
	    n--;
	}

    return False;
}

/*************************************************************************/

void mark_tagged_articles(ARTICLE *thread)
{
    long	i, n = no_tagged;

    for (i = 0 ; i < n ; i++)
	if (tagged_articles[i]->subject->thread == thread)
	    ArtTreeNodeSetOuter(main_widgets.arttree,
				(ART_TREE_NODE *)tagged_articles[i], True);
}

void clear_tagged_articles(void)
{
    SUBJECT	*subj;
    ARTICLE	*thread;
    long	i;

    switch (global.mode) {
    case NewsModeThread: /* clear the tree */
	if (global.curr_subj)
	    thread = global.curr_subj->thread;
	else if (global.curr_art)
	    thread = global.curr_art->subject->thread;
	else
	    thread = NULL;

	for (i = 0 ; i < no_tagged ; i++)
	    if (tagged_articles[i]->subject->thread == thread)
		ArtTreeNodeSetOuter(main_widgets.arttree,
				    (ART_TREE_NODE *)tagged_articles[i],
				    False);

	/* fall through */
    case NewsModeGroup: /* clear the list */
	for (subj = get_subjects(main_thr) ; subj ; subj = subj->next) {
	    if (subj->has_tagged) {
		subj->has_tagged = False;
		update_subj_entry(subj);
	    }
	}
	break;
    default:
	break;
    }

    XtFree((char *)tagged_articles);
    tagged_articles = NULL;
    no_tagged = 0;
    no_alloc = 0;
}

void tag_hot_articles(void)
{
    ARTICLE	*art;

    for (art = get_articles(main_thr) ; art ; art = art->next)
	if (art->from && !art->read && art->pixmap != None)
	    add_tag(art);
}

void untag_article(ARTICLE *art)
{
    remove_tag(art);
}

ARTICLE **get_tagged_articles(void)
{
    return tagged_articles;
}

long no_tagged_articles(void)
{
    return no_tagged;
}

void arttree_tag_callback(Widget gw,
			  XtPointer client_data,
			  XtPointer call_data)
{
    ARTICLE	*art = (ARTICLE *)call_data;

    if (!art)
	XBell(display, 0);
    else if (!art->from)
	set_message("Can't tag a fake article!", True);
    else {
	int	tagged = ArtTreeNodeGetOuter(main_widgets.arttree,
					     (ART_TREE_NODE *)art);

	if (tagged)
	    add_tag(art);
	else
	    remove_tag(art);

	if (no_tagged == 1)
	    set_message("1 tagged article.", False);
	else {
	    char	buffer[80];

	    sprintf(buffer, "%ld tagged articles.", no_tagged);
	    set_message(buffer, False);
	}
    }
}

void action_tag_subject(Widget w, XEvent *event,
			String *params, Cardinal *no_params)
{
    ARTICLE	*art;
    int		only_unread = True;
    char	message[80];

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    if (!global.curr_subj) {
	set_message("No selected subject!", True);
	return;
    }

    if (*no_params > 0)
	only_unread = False;

    for (art = global.curr_subj->thread ; art ;
	 art = next_in_thread_preorder(art)) {
	if (art->from && !(only_unread && art->read) &&
	    !is_tagged(art) && art->subject == global.curr_subj) {
	    add_tag(art);
	    if (global.mode == NewsModeThread)
		ArtTreeNodeSetOuter(main_widgets.arttree,
				    (ART_TREE_NODE *)art, True);
	}
    }

    update_subj_entry(global.curr_subj);

    sprintf(message, "%ld tagged articles", no_tagged);
    set_message(message, False);
}

void action_untag_subject(Widget w, XEvent *event,
			  String *params, Cardinal *no_params)
{
    ARTICLE	*art;
    char	message[80];

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    if (!global.curr_subj) {
	set_message("No selected subject!", True);
	return;
    }

    for (art = global.curr_subj->thread ; art ;
	 art = next_in_thread_preorder(art)) {
	if (art->from && art->subject == global.curr_subj) {
	    remove_tag(art);
	    if (global.mode == NewsModeThread)
		ArtTreeNodeSetOuter(main_widgets.arttree,
				    (ART_TREE_NODE *)art, False);
	}
    }

    update_subj_entry(global.curr_subj);

    sprintf(message, "%ld tagged articles", no_tagged);
    set_message(message, False);
}

void action_tag_thread(Widget w, XEvent *event,
		       String *params, Cardinal *no_params)
{
    ARTICLE	*art;
    SUBJECT	*subj;
    int		only_unread = True;
    char	message[80];

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    if (!global.curr_subj) {
	set_message("No selected subject!", True);
	return;
    }

    if (*no_params > 0)
	only_unread = False;

    for (art = global.curr_subj->thread ; art ;
	 art = next_in_thread_preorder(art)) {
	if (art->from && !(only_unread && art->read) && !is_tagged(art)) {
	    add_tag(art);
	    if (global.mode == NewsModeThread)
		ArtTreeNodeSetOuter(main_widgets.arttree,
				    (ART_TREE_NODE *)art, True);
	}
    }

    subj = global.curr_subj;
    art = subj->thread;
    while (subj->prev && subj->prev->thread == art)
	subj = subj->prev;

    while (subj && subj->thread == art) {
	update_subj_entry(subj);
	subj = subj->next;
    }

    sprintf(message, "%ld tagged articles", no_tagged);
    set_message(message, False);
}

void action_untag_thread(Widget w, XEvent *event,
			 String *params, Cardinal *no_params)
{
    ARTICLE	*art;
    SUBJECT	*subj = global.curr_subj;
    char	message[80];

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    if (!subj) {
	set_message("No selected subject!", True);
	return;
    }

    for (art = subj->thread ; art ; art = next_in_thread_preorder(art)) {
	if (art->from) {
	    remove_tag(art);
	    if (global.mode == NewsModeThread)
		ArtTreeNodeSetOuter(main_widgets.arttree,
				    (ART_TREE_NODE *)art, False);
	}
    }

    art = subj->thread;
    while (subj->prev && subj->prev->thread == art)
	subj = subj->prev;

    while (subj && subj->thread == art) {
	update_subj_entry(subj);
	subj = subj->next;
    }

    sprintf(message, "%ld tagged articles", no_tagged);
    set_message(message, False);
}

/*************************************************************************/

#define HIST_SIZE	32

static ARTICLE		*history[HIST_SIZE];
static unsigned int	h_end = 0;
static unsigned int	h_size = 0;

void clear_history(void)
{
    h_end = h_size = 0;
}

ARTICLE *history_pop(void)
{
    if (h_size == 0)
	return NULL;

    h_size--;
    h_end--;
    h_end %= HIST_SIZE;

    return history[h_end];
}

ARTICLE *history_peek(void)
{
    if (h_size == 0)
	return NULL;

    return history[(h_end - 1) % HIST_SIZE];
}

void history_push(ARTICLE *art)
{
    if (h_size < HIST_SIZE)
	h_size++;

    history[h_end++] = art;
    h_end %= HIST_SIZE;
}
