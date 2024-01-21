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
#include "child.h"
#include "file.h"
#include "k_I.h"
/*#include "k_file.h"*/
#include "k_kill.h"
#include "resource.h"
#include "util.h"
#include "thread.h"

#define THREAD_HAS_UNREAD(subj) \
((subj)->no_unread != 0 || thread_has_unread((subj)))

static int thread_has_unread(SUBJECT *subj)
{
    ARTICLE	*thr = subj->thread;

    for (subj = subj->next ; subj && subj->thread == thr ; subj = subj->next)
	if (subj->no_unread != 0)
	    return True;

    return False;
}

/*********************************************************************/

static ARTICLE *get_by_msgid(KILL_NODE *node)
{
    ARTICLE	*art;

    art = find_article(node->expr_str + 1, strlen(node->expr_str) - 2);
    node->expired = !art;

    return art;
}

static long m_a_kill(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    ARTICLE	*art = get_by_msgid(node);

    if (!art || !art->from || art->read || art->pixmap != None)
	return 0;

    art->read = True;
    art->killed = True;
    global.curr_group->no_unread--;
    art->subject->no_unread--;

    return 1;
}

static long m_a_hot(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    ARTICLE	*art = get_by_msgid(node);

    if (!art || !art->from || art->read || art->pixmap != None)
	return 0;

    art->pixmap = node->pixmap;

    return 1;
}

static long m_s_kill(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    ARTICLE	*art = get_by_msgid(node);

    if (!art || art->subject->no_unread == 0)
	return 0;

    return mark_subject_read(art->subject, False, True);
}

static long m_s_hot(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    ARTICLE	*art = get_by_msgid(node);

    if (!art)
	return 0;

    return mark_subject_hot(art->subject, node->pixmap);
}

static long m_T_kill(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    ARTICLE	*art = get_by_msgid(node);

    if (!art)
	return 0;

    return mark_thread_read(art->subject->thread, False, True);
}

static long m_T_hot(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    ARTICLE	*art = get_by_msgid(node);

    if (!art)
	return 0;

    return mark_thread_hot(art->subject->thread, node->pixmap);
}

static long m_t_kill(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    ARTICLE	*art = get_by_msgid(node);

    if (!art)
	return 0;

    return mark_subthread_read(art, False, True);
}

static long m_t_hot(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    ARTICLE	*art = get_by_msgid(node);

    if (!art)
	return 0;

    return mark_subthread_hot(art, node->pixmap);
}

/*********************************************************************/

static long s_s_kill(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;

    while (subj) {
	if (subj->no_unread != 0 &&
	    regexec(node->expr_re, subj->subject, 0, NULL, 0) == 0)
	    n += mark_subject_read(subj, False, True);
	subj = subj->next;
    }

    return n;
}

static long s_s_hot(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;

    while (subj) {
	if (subj->no_unread != 0 &&
	    regexec(node->expr_re, subj->subject, 0, NULL, 0) == 0)
	    n += mark_subject_hot(subj, node->pixmap);
	subj = subj->next;
    }

    return n;
}

static long s_T_kill(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;

    while (subj) {
	ARTICLE	*thr = subj->thread;
	int	has_unread = THREAD_HAS_UNREAD(subj);

	do {
	    if (has_unread &&
		regexec(node->expr_re, subj->subject, 0, NULL, 0) == 0) {
		n += mark_thread_read(subj->thread, False, True);
		has_unread = False;
	    }
	    subj = subj->next;
	} while (subj && subj->thread == thr);
    }

    return n;
}

static long s_T_hot(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;

    while (subj) {
	ARTICLE	*thr = subj->thread;
	int	has_unread = THREAD_HAS_UNREAD(subj);

	do {
	    if (has_unread &&
		regexec(node->expr_re, subj->subject, 0, NULL, 0) == 0) {
		n += mark_thread_hot(subj->thread, node->pixmap);
		has_unread = False;
	    }
	    subj = subj->next;
	} while (subj && subj->thread == thr);
    }

    return n;
}

static long s_t_kill(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;

    while (subj) {
	if (regexec(node->expr_re, subj->subject, 0, NULL, 0) == 0) {
	    ARTICLE	*art = subj->thread;

	    while (art)
		if (art->subject != subj)
		    art = next_in_thread_preorder(art);
		else {
		    n += mark_subthread_read(art, False, True);
		    art = preorder_skip_subthread(art);
		}
	}
	subj = subj->next;
    }

    return n;
}

static long s_t_hot(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;

    while (subj) {
	if (regexec(node->expr_re, subj->subject, 0, NULL, 0) == 0) {
	    ARTICLE	*art = subj->thread;

	    while (art)
		if (art->subject != subj)
		    art = next_in_thread_preorder(art);
		else {
		    n += mark_subthread_hot(art, node->pixmap);
		    art = preorder_skip_subthread(art);
		}
	}
	subj = subj->next;
    }

    return n;
}

/*********************************************************************/

static long f_a_kill(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;
    ARTICLE	*art;

    for (art = articles ; art ; art = art->next) {
	if (art->from && !art->read && art->pixmap == None &&
	    regexec(node->expr_re, art->from, 0, NULL, 0) == 0) {
	    art->read = True;
	    art->killed = True;
	    n++;
	    global.curr_group->no_unread--;
	    art->subject->no_unread--;
	}
    }

    return n;
}

static long f_a_hot(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;
    ARTICLE	*art;

    for (art = articles ; art ; art = art->next)
	if (art->from && art->pixmap == None && !art->read &&
	    regexec(node->expr_re, art->from, 0, NULL, 0) == 0) {
	    art->pixmap = node->pixmap;
	    n++;
	}

    return n;
}

static long f_s_kill(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;
    ARTICLE	*art;

    for (art = articles ; art ; art = art->next)
	if (art->from && regexec(node->expr_re, art->from, 0, NULL, 0) == 0)
	    n += mark_subject_read(art->subject, False, True);

    return n;
}

static long f_s_hot(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;
    ARTICLE	*art;

    for (art = articles ; art ; art = art->next)
	if (art->from && regexec(node->expr_re, art->from, 0, NULL, 0) == 0)
	    global.n_hot += mark_subject_hot(art->subject, node->pixmap);

    return n;
}

static long f_T_kill(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;
    ARTICLE	*art, *thr;

    while (subj) {
	thr = subj->thread;
	if (THREAD_HAS_UNREAD(subj))
	    for (art = thr ; art ; art = next_in_thread_preorder(art))
		if (art->from &&
		    regexec(node->expr_re, art->from, 0, NULL, 0) == 0) {
		    n += mark_thread_read(subj->thread, False, True);
		    break;
		}
	do {
	    subj = subj->next;
	} while (subj && subj->thread == thr);
    }

    return n;
}

static long f_T_hot(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;
    ARTICLE	*art, *thr;

    while (subj) {
	thr = subj->thread;
	if (THREAD_HAS_UNREAD(subj))
	    for (art = thr ; art ; art = next_in_thread_preorder(art))
		if (art->from &&
		    regexec(node->expr_re, art->from, 0, NULL, 0) == 0) {
		    global.n_hot +=
			mark_thread_hot(subj->thread, node->pixmap);
		    break;
		}
	do {
	    subj = subj->next;
	} while (subj && subj->thread == thr);
    }

    return n;
}

static long f_t_kill(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;
    ARTICLE	*art, *thr;

    while (subj) {
	thr = art = subj->thread;
	while (art)
	    if (art->from &&
		regexec(node->expr_re, art->from, 0, NULL, 0) == 0) {
		n += mark_subthread_read(art, False, True);
		art = preorder_skip_subthread(art);
	    } else
		art = next_in_thread_preorder(art);
	do {
	    subj = subj->next;
	} while (subj && subj->thread == thr);
    }

    return n;
}

static long f_t_hot(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;
    ARTICLE	*art, *thr;

    while (subj) {
	thr = art = subj->thread;
	while (art)
	    if (art->from &&
		regexec(node->expr_re, art->from, 0, NULL, 0) == 0) {
		global.n_hot += mark_subthread_hot(art, node->pixmap);
		art = preorder_skip_subthread(art);
	    } else
		art = next_in_thread_preorder(art);
	do {
	    subj = subj->next;
	} while (subj && subj->thread == thr);
    }

    return n;
}

/*********************************************************************/

static long x_a_kill(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;
    ARTICLE	*art;

    for (art = articles ; art ; art = art->next) {
	if (art->xref && !art->read && art->pixmap == None &&
	    regexec(node->expr_re, art->xref, 0, NULL, 0) == 0) {
	    art->read = True;
	    art->killed = True;
	    global.curr_group->no_unread--;
	    art->subject->no_unread--;
	    n++;
	}
    }

    return n;
}

static long x_a_hot(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;
    ARTICLE	*art;

    for (art = articles ; art ; art = art->next)
	if (art->xref && art->pixmap == None &&
	    regexec(node->expr_re, art->xref, 0, NULL, 0) == 0) {
	    art->pixmap = node->pixmap;
	    if (!art->read)
		global.n_hot++;
	}

    return n;
}

static long x_s_kill(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;
    ARTICLE	*art;

    while (subj) {
	if (subj->no_unread != 0)
	    for (art = subj->thread ; art ;
		 art = next_in_thread_preorder(art)) {
		if (art->subject == subj && art->xref &&
		    regexec(node->expr_re, art->xref, 0, NULL, 0) == 0) {
		    n += mark_subject_read(subj, False, True);
		    break;
		}
	    }
	subj = subj->next;
    }

    return n;
}

static long x_s_hot(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;
    ARTICLE	*art;

    while (subj) {
	if (subj->no_unread != 0)
	    for (art = subj->thread ; art ;
		 art = next_in_thread_preorder(art)) {
		if (art->subject == subj && art->xref &&
		    regexec(node->expr_re, art->xref, 0, NULL, 0) == 0) {
		    global.n_hot += mark_subject_hot(subj, node->pixmap);
		    break;
		}
	    }
	subj = subj->next;
    }

    return n;
}

static long x_T_kill(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;
    ARTICLE	*art, *thr;

    while (subj) {
	thr = subj->thread;
	if (THREAD_HAS_UNREAD(subj))
	    for (art = thr ; art ; art = next_in_thread_preorder(art))
		if (art->xref &&
		    regexec(node->expr_re, art->xref, 0, NULL, 0) == 0) {
		    n += mark_thread_read(subj->thread, False, True);
		    break;
		}
	do {
	    subj = subj->next;
	} while (subj && subj->thread == thr);
    }

    return n;
}

static long x_T_hot(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;
    ARTICLE	*art, *thr;

    while (subj) {
	thr = subj->thread;
	if (THREAD_HAS_UNREAD(subj))
	    for (art = thr ; art ; art = next_in_thread_preorder(art))
		if (art->xref &&
		    regexec(node->expr_re, art->xref, 0, NULL, 0) == 0) {
		    global.n_hot +=
			mark_thread_hot(subj->thread, node->pixmap);
		    break;
		}
	do {
	    subj = subj->next;
	} while (subj && subj->thread == thr);
    }

    return n;
}

static long x_t_kill(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;
    ARTICLE	*art, *thr;

    while (subj) {
	thr = art = subj->thread;
	if (THREAD_HAS_UNREAD(subj))
	    while (art)
		if (art->xref &&
		    regexec(node->expr_re, art->xref, 0, NULL, 0) == 0) {
		    n += mark_subthread_read(art, False, True);
		    art = preorder_skip_subthread(art);
		} else
		    art = next_in_thread_preorder(art);
	do {
	    subj = subj->next;
	} while (subj && subj->thread == thr);
    }

    return n;
}

static long x_t_hot(KILL_NODE *node, ARTICLE *articles, SUBJECT *subj)
{
    long	n = 0;
    ARTICLE	*art, *thr;

    while (subj) {
	thr = art = subj->thread;
	if (THREAD_HAS_UNREAD(subj))
	    while (art)
		if (art->xref &&
		    regexec(node->expr_re, art->xref, 0, NULL, 0) == 0) {
		    global.n_hot += mark_subthread_hot(art, node->pixmap);
		    art = preorder_skip_subthread(art);
		} else
		    art = next_in_thread_preorder(art);
	do {
	    subj = subj->next;
	} while (subj && subj->thread == thr);
    }

    return n;
}

/*********************************************************************/

const KillFunc kill_funcs[4][4] = {
    {m_a_kill,  m_s_kill,  m_T_kill,  m_t_kill},
    {s_s_kill,  s_s_kill,  s_T_kill,  s_t_kill},
    {f_a_kill,	f_s_kill,  f_T_kill,  f_t_kill},
    {x_a_kill,	x_s_kill,  x_T_kill,  x_t_kill},
};

const KillFunc hot_funcs[4][4] = {
    {m_a_hot,   m_s_hot,   m_T_hot,   m_t_hot},
    {s_s_hot,   s_s_hot,   s_T_hot,   s_t_hot},
    {f_a_hot,   f_s_hot,   f_T_hot,   f_t_hot},
    {x_a_hot,   x_s_hot,   x_T_hot,   x_t_hot},
};
