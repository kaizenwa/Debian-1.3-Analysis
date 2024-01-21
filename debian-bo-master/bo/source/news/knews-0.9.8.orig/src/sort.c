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
#include <limits.h>
#include "parse.h"
#include "resource.h"
#include "sort.h"
#include "thread.h"
#include "util.h"

typedef union {
    char		*str;
    unsigned long	n;
} SORT_VAL;

typedef struct {
    SUBJECT	*first;
    SUBJECT	*last;
    SORT_VAL	val;
} SORT_NODE;

typedef int		(*CompFunc)(const void*, const void*);
typedef SORT_VAL	(*SortValFunc)(const SORT_NODE*);

typedef struct {
    CompFunc	comp_func;
    SortValFunc	val_func;
} SortFuncs;

static void		sub_sort(SORT_NODE*);
static SortFuncs	get_sort_funcs(char*);

void sort_threads(void)
{
    SORT_NODE	*sort;
    SUBJECT	*subj;
    long	n, i;
    char	*sort_name;

    subj = get_subjects(main_thr);
    if (!subj)
	return;

    n = 0;
    while (subj) {
	ARTICLE	*thr = subj->thread;

	n++;
	while (subj && subj->thread == thr)
	    subj = subj->next;
    }

    sort = (SORT_NODE *)XtMalloc(n * sizeof sort[0]);

    subj = get_subjects(main_thr);
    i = 0;
    while (subj) {
	ARTICLE	*thr = subj->thread;

	subj->disp = 0;
	sort[i].first = subj;
	while (subj->next && subj->next->thread == thr) {
	    subj = subj->next;
	    subj->disp = 0;
	}
	sort[i].last = subj;
	subj = subj->next;
	sort[i++].last->next = NULL;
    }

    for (i = 0 ; i < n ; i++)
	sub_sort(sort + i);

    sort_name = res_sort_threads();
    if (sort_name && case_lstrcmp(sort_name, "none") != 0) {
	SortFuncs	funcs = get_sort_funcs(sort_name);

	if (!funcs.comp_func)
	    fprintf(stderr, "knews: unknown sort method: %s\n", sort_name);
	else {
	    if (funcs.val_func)
		for (i = 0 ; i < n ; i++)
		    sort[i].val = funcs.val_func(sort + i);
	    qsort(sort, n, sizeof sort[0], funcs.comp_func);
	}
    }

    set_subjects(main_thr, sort[0].first);
    for (i = 0 ; i < n - 1 ; i++)
	sort[i].last->next = sort[i+1].first;
    sort[n-1].last->next = NULL;

    XtFree((char *)sort);

    subj = get_subjects(main_thr);
    subj->prev = NULL;
    subj->disp = -1;
    while (subj->next) {
	subj->next->prev = subj;
	subj = subj->next;
	subj->disp = -1;
    }
}

static void sub_sort(SORT_NODE *node)
{
    SUBJECT	*subj = node->first;
    SUBJECT	*first = NULL, *last = subj;
    ARTICLE	*art;
    long	n = 1;

    for (art = subj->thread ; art ; art = next_in_thread_preorder(art))
	if (art->from && art->subject->disp == 0 && !art->read)
	    art->subject->disp = n++;

    for (art = subj->thread ; art ; art = next_in_thread_preorder(art))
	if (art->from && art->subject->disp == 0)
	    art->subject->disp = n++;

    while (n-- > 1) {
	SUBJECT	*sn = subj;

	if (sn->disp == n)
	    subj = subj->next;
	else {
	    SUBJECT	*tmp;

	    while (sn->next->disp != n)
		sn = sn->next;
	    tmp = sn->next;
	    sn->next = sn->next->next;
	    sn = tmp;
	}

	if (!first)
	    last = sn;
	sn->next = first;
	first = sn;
    }

    node->first = first;
    node->last  = last;
}

/*************************************************************************/

static SORT_VAL val_thrsize_unread(const SORT_NODE *node)
{
    SUBJECT	*subj = node->first;
    SORT_VAL	val;

    val.n = subj->no_unread;

    while (subj->next && subj->next->thread == subj->thread) {
	subj = subj->next;
	val.n += subj->no_unread;
    }

    return val;
}

static SORT_VAL val_thrsize(const SORT_NODE *node)
{
    SUBJECT	*subj = node->first;
    SORT_VAL	val;
    ARTICLE	*art;

    val.n = 0;
    for (art = subj->thread ; art ; art = next_in_thread_preorder(art))
	if (art->from)
	    val.n++;

    return val;
}

static SORT_VAL val_avgdate(const SORT_NODE *node)
{
    SUBJECT		*subj = node->first;
    SORT_VAL		val;
    long		n = 0;
    ARTICLE		*art;

    if (sizeof(long) * CHAR_BIT > 60) { /* ~ 64 bit longs */
	val.n = 0;

	for (art = subj->thread ; art ; art = next_in_thread_preorder(art))
	    if (art->from && !art->read && art->date != PARSEDATE_ERROR) {
		val.n += art->date;
		n++;
	    }

	if (n <= 0)
	    n = 1;

	val.n /= n;
    } else {
	unsigned long	upper = 0;
	unsigned long	lower = 0;

	for (art = subj->thread ; art ; art = next_in_thread_preorder(art))
	    if (art->from && !art->read && art->date != PARSEDATE_ERROR) {
		unsigned long	date = art->date;

		lower += date & 0xffff;
		date >>= 16;
		upper += date;
		n++;
	    }

	if (n <= 0)
	    n = 1;

	val.n = ((upper / n) << 16) + lower / n;
    }

    return val;
}

static SORT_VAL val_nhot(const SORT_NODE *node)
{
    SUBJECT	*subj = node->first;
    SORT_VAL	val;
    ARTICLE	*art;

    val.n = 0;
    for (art = subj->thread ; art ; art = next_in_thread_preorder(art))
	if (art->from && !art->read && art->pixmap != None)
	    val.n++;

    return val;
}

static SORT_VAL val_subject(const SORT_NODE *node)
{
    SUBJECT	*subj = node->first;
    SORT_VAL	val;

    val.str = subj->subject;

    return val;
}

static ARTICLE *first_unread(ARTICLE*);

static SORT_VAL val_author(const SORT_NODE *node)
{
    SUBJECT	*subj = node->first;
    SORT_VAL	val;
    ARTICLE	*art = first_unread(subj->thread);

    if (!art)
	val.str = "";
    else {
	char	*c = art->tree_data.label;

	if (res_show_number_lines()) {
	    while ((unsigned)*c - '0' < 10)
		c++;
	    while (*c == ' ')
		c++;
	}

	val.str = c;
    }

    return val;
}

static SORT_VAL val_date(const SORT_NODE *node)
{
    SUBJECT	*subj = node->first;
    SORT_VAL	val;
    ARTICLE	*art = first_unread(subj->thread);

    if (art)
	val.n = art->date;
    else
	val.n = PARSEDATE_ERROR;

    return val;
}

/*************************************************************************/

static int cmp_str(const void *v1, const void *v2)
{
    const SORT_NODE	*s1 = v1;
    const SORT_NODE	*s2 = v2;

    return case_strcmp(s1->val.str, s2->val.str);
}

static int cmp_str_neg(const void *v1, const void *v2)
{
    const SORT_NODE	*s1 = v1;
    const SORT_NODE	*s2 = v2;

    return - case_strcmp(s1->val.str, s2->val.str);
}

static int cmp_n(const void *v1, const void *v2)
{
    const SORT_NODE	*s1 = v1;
    const SORT_NODE	*s2 = v2;

    if (s1->val.n < s2->val.n)
	return 1;
    if (s1->val.n > s2->val.n)
	return -1;

    return 0;
}

static int cmp_n_neg(const void *v1, const void *v2)
{
    const SORT_NODE	*s1 = v1;
    const SORT_NODE	*s2 = v2;

    if (s1->val.n < s2->val.n)
	return -1;
    if (s1->val.n > s2->val.n)
	return 1;

    return 0;
}

/*************************************************************************/

static SortFuncs get_sort_funcs(char *sort)
{
    static struct {
	const char	*name;
	CompFunc	pos;
	CompFunc	neg;
	SortValFunc	val;
    } funcs[] = {
	{"author",        cmp_str,    cmp_str_neg,  val_author},
	{"average-date",  cmp_n_neg,  cmp_n,        val_avgdate},
	{"date",          cmp_n_neg,  cmp_n,        val_date},
	{"full-size",     cmp_n,      cmp_n_neg,    val_thrsize},
	{"hot",           cmp_n,      cmp_n_neg,    val_nhot},
	{"size",          cmp_n,      cmp_n_neg,    val_thrsize_unread},
	{"subject",       cmp_str,    cmp_str_neg,  val_subject},
    };
    SortFuncs	res = {NULL, };
    int		i = XtNumber(funcs);
    int		neg = False;

    if (*sort == '+')
	sort++;
    else if (*sort == '-') {
	sort++;
	neg = True;
    }

    while (i-- > 0)
	if (case_lstrcmp(sort, funcs[i].name) == 0) {
	    res.comp_func = neg ? funcs[i].neg : funcs[i].pos;
	    res.val_func = funcs[i].val;
	    break;
	}

    return res;
}

static ARTICLE *first_unread(ARTICLE *thread)
{
    ARTICLE	*first = NULL;

    while (thread) {
	if (thread->from) {
	    if (!thread->read)
		return thread;
	    if (!first)
		first = thread;
	}
	thread = next_in_thread_preorder(thread);
    }

    return first;
}
