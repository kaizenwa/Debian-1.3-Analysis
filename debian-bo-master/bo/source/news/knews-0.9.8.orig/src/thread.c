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

/*
 * The general threading algorithm, and in particular the functions
 * link_child(), unlink_child() and thread_article(), are modified
 * from the source to trn.
 */

#include "global.h"
#include <X11/Shell.h>
#include "ahead.h"
#include "cache.h"
#include "codes.h"
#include "connect.h"
#include "font.h"
#include "k_file.h"
#include "parse.h"
#include "read.h"
#include "resource.h"
#include "server.h"
#include "sort.h"
#include "thread.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/Dialogue.h"
#include "../Widgets/Message.h"
#include "../Widgets/Util.h"

#define HASH_SIZE  237

#undef  HEAD_DEBUG
#define HEAD_DEBUG   0
#undef  ART_BYTES
#define ART_BYTES    0
#undef  DATE_ERRORS
#define DATE_ERRORS  0

struct THREAD_CONTEXT {
    ARTICLE		*articles;
    SUBJECT		*subjects;
    ARTICLE		*art_ht[HASH_SIZE];
    SUBJECT		*subj_ht[HASH_SIZE];
    ARTICLE		*last_art;
    SUBJECT		*last_subj;
    SUBJECT		*fake_had_subject;
    char		*refs;
    long		total;
    long		n_done;
    char		*charset;
};

THREAD_CONTEXT	*main_thr = NULL;

/*************************************************************************/

static long str_hash(const char *str, long len)
{
    unsigned long	hash = 0;

    while (len-- > 0)
	hash += *str++ & 0xdfu;

    return hash % HASH_SIZE;
}

static void art_hash_table_clear(ARTICLE **art_ht) {
    ARTICLE	*loop, *next;
    long	i;

    for (i = 0 ; i < HASH_SIZE ; i++) {
	for (loop = art_ht[i] ; loop ; loop = next) {
	    next = loop->hash_next;
	    XtFree(loop->msgid);
	    XtFree(loop->xref);
	    XtFree(loop->from);
	    XtFree(loop->tree_data.label);
	    XtFree((char *)loop);
	}
	art_ht[i] = NULL;
    }
}

static ARTICLE **art_hash_find(ARTICLE **art_ht, const char *msgid, long len)
{
    ARTICLE	*loop, *prev = NULL;
    ARTICLE	**hash_ptr;

    hash_ptr = &art_ht[str_hash(msgid, len)];
    for (loop = *hash_ptr ; loop ; prev = loop, loop = loop->hash_next)
	if (len == loop->hash_len && memcmp(msgid, loop->msgid, len) == 0)
	    break;

    return prev ? &prev->hash_next : hash_ptr;
}

static void subj_hash_table_clear(SUBJECT **subj_ht) {
    SUBJECT	*loop, *next;
    long	i;

    for (i = 0 ; i < HASH_SIZE ; i++) {
	for (loop = subj_ht[i] ; loop ; loop = next) {
	    next = loop->hash_next;
	    XtFree(loop->subject);
	    XtFree((char *)loop);
	}
	subj_ht[i] = NULL;
    }
}

static SUBJECT **subj_hash_find(SUBJECT **subj_ht,
				const char *subject,
				long len)
{
    SUBJECT	*loop, *prev = NULL;
    SUBJECT	**hash_ptr;

    hash_ptr = &subj_ht[str_hash(subject, len)];
    for (loop = *hash_ptr ; loop ; prev = loop, loop = loop->hash_next)
	if (loop->hash_len == len &&
	    strncmp(subject, loop->subject, len) == 0)
	    break;

    return prev ? &prev->hash_next : hash_ptr;
}

void clear_thread_context(THREAD_CONTEXT *context)
{
    context->articles = NULL;
    context->subjects = NULL;
    art_hash_table_clear(context->art_ht);
    subj_hash_table_clear(context->subj_ht);
    context->last_art = NULL;
    context->last_subj = NULL;
    context->fake_had_subject = NULL;
    context->refs = NULL;
    context->total = 1;
    context->n_done = 0;
    context->charset = NULL;
}

static void nullify_thread_context(THREAD_CONTEXT *context)
{
    int i;

    context->articles = NULL;
    context->subjects = NULL;
    context->last_art = NULL;
    context->last_subj = NULL;
    context->fake_had_subject = NULL;
    context->refs = NULL;
    context->total = 1;
    context->n_done = 0;
    context->charset = NULL;

    for (i = 0 ; i < HASH_SIZE ; i++) {
	context->art_ht[i] = NULL;
	context->subj_ht[i] = NULL;
    }
}

THREAD_CONTEXT *create_thread_context(void)
{
    THREAD_CONTEXT	*ret;

    ret = (THREAD_CONTEXT *)XtMalloc(sizeof *main_thr);
    nullify_thread_context(ret);

    return ret;
}

ARTICLE *get_articles(THREAD_CONTEXT *context)
{
    return context->articles;
}

SUBJECT *get_subjects(THREAD_CONTEXT *context)
{
    return context->subjects;
}

void set_subjects(THREAD_CONTEXT *context, SUBJECT *subjects)
{
    context->subjects = subjects;
}

ARTICLE *find_article(const char *msgid, long msgid_len)
{
    return *art_hash_find(main_thr->art_ht, msgid, msgid_len);
}

char *get_refs(THREAD_CONTEXT *context)
{
    return context->refs;
}

static ARTICLE	*parse_xover_line(THREAD_CONTEXT*, char*);
static void	 thread_article(THREAD_CONTEXT*, ARTICLE*);

char *thread_from_file(SERVER *server, long first_art)
{
    char	*buffer;

    main_thr->charset = res_default_charset();
    if (!main_thr->charset)
	main_thr->charset = "iso-8859-1";

    while ((buffer = server_read(server)) && !IS_DOT(buffer)) {
	ARTICLE	*art;

	if (atol(buffer) < first_art)
	    continue;

	art = parse_xover_line(main_thr, buffer);
	if (art) {
	    if (main_thr->last_art)
		main_thr->last_art->next = art;
	    else
		main_thr->articles = art;
	    main_thr->last_art = art;

	    thread_article(main_thr, art);
	}
    }

    return buffer;
}

/*************************************************************************/

static char *get_msgid(char *beg, char *end)
{
    char	*mid;

    while (*beg == ' ')
	beg++;
    if (beg >= end || *beg++ != '<')
	return NULL;
    while (end > beg && *end == ' ')
	*end-- = '\0';
    if (*end != '>')
	return NULL;
    *end = '\0';

    mid = strchr(beg, '@');
    if (mid)
	while (++mid < end)
	    if (IS_UPPER(*mid))
		*mid = LOWER(*mid);

    return beg;
}

/* replaces ascii 0-32 & 127 and iso-8859-x 128-159 & 255 with ' ' */
static void munge_nonprintable(char *str)
{
    unsigned char	*c = (unsigned char *)str;

    while (*c != '\0') {
	if ((*c & 0x60) == 0 || *c == '\b')
	    *c = ' ';
	c++;
    }
}

static void overview_error(long art_no, char *string)
{
    fputs("Bad overview record", stderr);
    if (art_no)
	fprintf(stderr, ", article no %ld", art_no);
    fprintf(stderr, "; %s\n", string);
}

static void unlink_child(ARTICLE *child)
{
    ARTICLE	*last = A_PARENT(child);

    if (last) {
	if (child == A_CHILD1(last))
	    CHILD1(last) = SIBLING(child);
	else {
	    last = A_CHILD1(last);
	    while (child != A_SIBLING(last))
		last = A_SIBLING(last);
	    SIBLING(last) = SIBLING(child);
	}
    } else {
	SUBJECT	*subj = child->subject;

	if ((last = subj->thread) == child) {
	    do {
		subj->thread = A_SIBLING(child);
		subj = subj->prev;
	    } while (subj && subj->thread == last);

	    subj = child->subject->next;

	    while (subj && subj->thread == last) {
		subj->thread = A_SIBLING(child);
		subj = subj->next;
	    }
	} else {
	    while (child != A_SIBLING(last))
		last = A_SIBLING(last);
	    SIBLING(last) = SIBLING(child);
	}
    }
}

static void link_child(ARTICLE *child)
{
    ARTICLE	*art = A_PARENT(child);

    if (art) {
	art = A_CHILD1(art);
	if (!art || child->date < art->date) {
	    SIBLING(child) = (ART_TREE_NODE *)art;
	    CHILD1(A_PARENT(child)) = (ART_TREE_NODE *)child;
	} else {
	    while (A_SIBLING(art) && A_SIBLING(art)->date <= child->date)
		art = A_SIBLING(art);
	    SIBLING(child) = SIBLING(art);
	    SIBLING(art) = (ART_TREE_NODE *)child;
	}
    } else {
	SUBJECT	*subj = child->subject;

	art = subj->thread;
	if (!art || child->date < art->date) {
	    do {
		subj->thread = child;
		subj = subj->prev;
	    } while (subj && subj->thread == art);

	    subj = child->subject->next;

	    while (subj && subj->thread == art) {
		subj->thread = child;
		subj = subj->next;
	    }
	    SIBLING(child) = (ART_TREE_NODE *)art;
	} else {
	    while (A_SIBLING(art) && A_SIBLING(art)->date <= child->date)
		art = A_SIBLING(art);
	    SIBLING(child) = SIBLING(art);
	    SIBLING(art) = (ART_TREE_NODE *)child;
	}
    }
}

static void merge_threads(THREAD_CONTEXT *context, SUBJECT *s1, SUBJECT *s2)
{
    SUBJECT *s;
    ARTICLE *t1 = s1->thread;
    ARTICLE *t2 = s2->thread;

    while (s1->next && s1->next->thread == t1)
	s1 = s1->next;
    while (s2->prev && s2->prev->thread == t2)
	s2 = s2->prev;

    s = s2;
    s->thread = t1;
    while (s->next && s->next->thread == t2) {
	s = s->next;
	s->thread = t1;
    }

    if (s2->prev)
	s2->prev->next = s->next;
    else
	context->subjects = s->next;
    if (s->next)
	s->next->prev = s2->prev;

    if ( (s->next = s1->next) )
	s->next->prev = s;
    s1->next = s2;
    s2->prev = s1;

    for (t1 = t2 ; t1 ; t1 = t2) {
	t2 = A_SIBLING(t2);
	link_child(t1);
    }
}

static SUBJECT *get_subject(THREAD_CONTEXT *context, char *subj)
{
    SUBJECT	**sp;

    decode_rfc1522(subj, context->charset);
    munge_nonprintable(subj);
    subj = eat_re(subj);

    if (!*(sp = subj_hash_find(context->subj_ht, subj, strlen(subj)))) {
	(*sp) = (SUBJECT *)XtMalloc(sizeof(SUBJECT));
	(*sp)->hash_next = NULL;
	(*sp)->next = NULL;
	(*sp)->prev = NULL;
	(*sp)->thread = NULL;
	(*sp)->no_unread = 0;
	(*sp)->disp = 0;
	(*sp)->pixmap = None;
	(*sp)->has_tagged = False;
	(*sp)->subject = XtNewString(subj);
	(*sp)->hash_len = strlen(subj);
	if (context->subjects) {
	    if (!context->last_subj || context->last_subj->next) {
		/* subjects have been reordered by merge_threads */
		context->last_subj = context->subjects;
		while (context->last_subj->next)
		    context->last_subj = context->last_subj->next;
	    }

	    (*sp)->prev = context->last_subj;
	    context->last_subj->next = *sp;
	    context->last_subj = *sp;
	} else {
	    context->subjects = *sp;
	    context->last_subj = context->subjects;
	}
    }

    return *sp;
}

static ARTICLE *parse_xover_line(THREAD_CONTEXT *context, char *buffer)
{
    ARTICLE	*art;
    ARTICLE	**fake_p;
    char	*c1, *c2;
    long	art_no, lines;
    char	*subject, *from, *msgid;
    time_t	date;
    long	msgid_len;
#if ART_BYTES
    long	bytes;
#endif

    if (!(c1 = strchr(buffer, '\t')) || !IS_DIGIT(buffer[0])) {
	overview_error(0, "no article number.");
	return NULL;
    }
    *c1++ = '\0';
    art_no = atol(buffer);

    if (!(c2 = strchr(c1, '\t'))) {
	overview_error(art_no, "no Subject: field.");
	return NULL;
    }
    *c2++ = '\0';
    subject = c1;

    if (!(c1 = strchr(c2, '\t'))) {
	overview_error(art_no, "no From: field.");
	return NULL;
    }
    *c1++ = '\0';
    decode_rfc1522(c2, context->charset);
    munge_nonprintable(c2);
    from = c2;

    if (!(c2 = strchr(c1, '\t'))) {
	overview_error(art_no, "no Date: field.");
	return NULL;
    }
    *c2++ = '\0';
    date = parsedate(c1);
#if DATE_ERRORS
    if (date == PARSEDATE_ERROR)
	fprintf(stderr, "Bad date: %s\n", c1);
#endif

    if (!(c1 = strchr(c2, '\t'))) {
	overview_error(art_no, "no Message-Id: field.");
	return NULL;
    }
    *c1 = '\0';
    if (!(msgid = get_msgid(c2, c1 - 1))) {
	overview_error(art_no, "syntax error in Message-Id: field.");
	return NULL;
    }
    msgid_len = strlen(msgid);
    c1++;

    if (!(c2 = strchr(c1, '\t'))) {
	context->refs = NULL;
	overview_error(art_no, "no References: field.");
	return NULL;
    }
    *c2++ = '\0';
    context->refs = c1;

    if (!(c1 = strchr(c2, '\t'))) {
	overview_error(art_no, "no Bytes: field.");
	return NULL;
    }
    *c1++ = '\0';
#if ART_BYTES
    bytes = atol(c2);
#endif

    if ( (c2 = strchr(c1, '\t')) )
	*(c2++) = '\0';
    lines = atol(c1);

    if ( (art = *(fake_p = art_hash_find(context->art_ht,
					 msgid, msgid_len))) ) {
	if (art->from) {
	    fprintf(stderr,
		    "Bad overview record, articles %ld and %ld have "
		    "identical message-id's:\n\t<%s>\n",
		    art_no, art->no, msgid);
	    return NULL; /* duplicate Message-ID */
	}
	context->fake_had_subject = art->subject;
    } else {
	art = *fake_p = (ARTICLE *)XtMalloc(sizeof(ARTICLE));
	art->tree_data.hook = NULL;
	art->tree_data.label = NULL;
	PARENT(art) = NULL;
	SIBLING(art) = NULL;
	CHILD1(art) = NULL;
	art->hash_next = NULL;
	art->next = NULL;
	art->xref = NULL;
	art->pixmap = None;
	art->read = FALSE;
	art->killed = FALSE;
	art->msgid = memcpy(XtMalloc(msgid_len + 1), msgid, msgid_len + 1);
	art->hash_len = msgid_len;
	context->fake_had_subject = NULL;
    }

    art->no = art_no;
    art->subject = get_subject(context, subject);
    art->from = XtNewString(from);
    art->date = date;
    art->lines = lines;
#if ART_BYTES
    art->bytes = bytes;
#endif

    while (c2) {
	c1 = c2;
	if ( (c2 = strchr(c1, '\t')) )
	    *(c2++) = '\0';
	if (case_lstrncmp(c1, "xref: ", 6) == 0) {
	    c1 = strchr(c1 + 6, ' ');
	    if (c1) {
		c1++;
		art->xref = XtNewString(c1);
	    }
	    break;
	}
    }

    return art;
}

static void thread_article(THREAD_CONTEXT *context, ARTICLE *art)
{
    ARTICLE	*a, *last;

    if (context->fake_had_subject) {
	ARTICLE *stop;

	if (context->fake_had_subject->thread != art->subject->thread)
	    merge_threads(context, context->fake_had_subject, art->subject);
	a = A_PARENT(art);
	while (a && !a->from && !A_SIBLING(A_CHILD1(a)))
	    a = A_PARENT(a);
	stop = a;
	unlink_child(art);
	for (a = A_PARENT(art) ; a != stop ; a = last) {
	    unlink_child(a);
	    last = A_PARENT(a);
	    a->date = 0;
	    a->subject = NULL;
	    PARENT(a) = NULL;
	}
	PARENT(art) = NULL;
	SIBLING(art) = NULL;
    }

    if (context->refs) {
	ARTICLE	**ap;
	char	*c, *end;

	last = art;
	a = NULL;
	end = context->refs + strlen(context->refs) - 1;
	for (;;) {
	    char	*msgid;
	    long	msgid_len;
	    
	    for (c = end ; c >= context->refs && *c != '<' ; c--);
	    if (!(msgid = get_msgid(c, end)))
		break;
	    msgid_len = strlen(msgid);

	    ap = art_hash_find(context->art_ht, msgid, msgid_len);
	    a = *ap;
	    if (a) {
		if ((a->date != 0 && !a->subject) || a == art) {
		    if ((a = last) == art) a = NULL;
		    continue;
		}
	    } else {
		a = *ap = (ARTICLE *)XtMalloc(sizeof(ARTICLE));
		a->tree_data.hook = NULL;
		a->tree_data.label = NULL;
		PARENT(a) = NULL;
		SIBLING(a) = NULL;
		CHILD1(a) = NULL;
		a->hash_next = NULL;
		a->next = NULL;
		a->from = NULL;
		a->xref = NULL;
		a->subject = NULL;
		a->no = 0;
		a->pixmap = None;
		a->lines = 0;
#if ART_BYTES
		a->bytes = 0;
#endif
		a->read = FALSE;
		a->killed = FALSE;
		a->hash_len = msgid_len;
		a->msgid = memcpy(XtMalloc(msgid_len + 1), msgid,
				  msgid_len + 1);
	    }
	    PARENT(last) = (ART_TREE_NODE *)a;
	    link_child(last);
	    if (a->subject)
		break;
	    a->date = art->date;
	    last = a;
	    end = c - 1;
	}
	if (!a) { /* no references */
	    link_child(art);
	    return;
	}
	if (a->subject) {
	    if (art->subject->thread != a->subject->thread)
		merge_threads(context, art->subject, a->subject);
	} else {
	    a->subject = art->subject;
	    link_child(a);
	}

	for (a = A_PARENT(art) ; a && !a->subject ; a = A_PARENT(a))
	    a->subject = art->subject;

	while (a && art != A_PARENT(a))
	    a = A_PARENT(a);
	if (a) {
	    unlink_child(a);
	    PARENT(a) = NULL;
	    link_child(a);
	}
    } else {
	link_child(art);
    }
}

static void mark_read_articles(THREAD_CONTEXT *context, GROUP *group)
{
    SUBJECT		*subj = context->subjects;
    ARTICLE		*art = context->articles;
    long		n = 0;
    ART_LIST_NODE	*loop = group->read_arts;

    while (subj) {
	subj->no_unread = 0;
	subj = subj->next;
    }

    while (art && loop) {
	while (art && art->no < loop->first) {
	    art->read = False;
	    if (art->from) {
		n++;
		art->subject->no_unread++;
	    }
	    art = art->next;
	}

	while (art && art->no >= loop->first && art->no <= loop->last) {
	    if (art->from)
		art->read = True;
	    art = art->next;
	}

	loop = loop->next;
    }

    while (art) {
	art->read = False;
	if (art->from) {
	    n++;
	    art->subject->no_unread++;
	}
	art = art->next;
    }

    group->no_unread = n;
}

void fix_author(ARTICLE *art, char *from, int show_no_lines)
{
    const char	*author;
    char	buffer[160];
    char	*c;
    long	len = 0;

    if (!show_no_lines)
	c = buffer;
    else {
	sprintf(buffer, "%4hu  ", art->lines);
	c = buffer + strlen(buffer);
    }

    author = parse_author(from, &len);
    if (len > sizeof buffer - 16)
	len = sizeof buffer - 16;
    memcpy(c, author, len);
    c[len] = '\0';

    art->tree_data.label = XtNewString(buffer);
}

static void fix_authors(THREAD_CONTEXT *context)
{
    ARTICLE	*art;
    int		show_number_lines = res_show_number_lines();

    for (art = context->articles ; art ; art = art->next)
	if (art->from)
	    fix_author(art, art->from, show_number_lines);
}

static char *xover_articles(void)
{
    char	command[512];
    char	*buffer;
    ARTICLE	*art;
    long	i, j, m;

    j = m = main_thr->total / 32;
    i = main_thr->n_done * 100;

    while ((buffer = server_read(main_server)) && !IS_DOT(buffer)) {
	art = parse_xover_line(main_thr, buffer);
	if (art) {
	    if (main_thr->last_art)
		main_thr->last_art->next = art;
	    else
		main_thr->articles = art;
	    main_thr->last_art = art;

	    thread_article(main_thr, art);
	}

	if (j-- <= 0) {
	    sprintf(command, "Reading group... %3ld%%",
		    i / main_thr->total);
	    set_message(command, False);
	    j = m;
	}
	i += 100;
    }

    if (!buffer)
	return NULL;
    
    return CODE_TO_STR(NNTP_OK_GROUP);
}

/*************************************************************************/

#define MAX_CHUNK_SIZE	64

static int put_chunk_head(long max)
{
    char	command[64 * 12 + 1];
    char	*c;
    int		n;

    if (max <= 0)
	return 0;

    if (max > global.chunk_size)
	max = global.chunk_size;
    if (max > MAX_CHUNK_SIZE)
	max = MAX_CHUNK_SIZE;

    for (n = 0, c = command ; n < max ; n++, c += 12)
	sprintf(c, "HEAD\r\nNEXT\r\n");

    *c = '\0';
    server_write(main_server, command);

    return max;
}

static ARTICLE *gather_headers(long art_no, THREAD_CONTEXT *context,
			       char *subject, char *from,
			       time_t date, char *msgid,
			       long lines, char *xref)
{
    long	msgid_len = strlen(msgid);
    ARTICLE   **fake_p = art_hash_find(context->art_ht, msgid, msgid_len);
    ARTICLE    *art = *fake_p;

    if (art) {
	if (art->from) {
	    overview_error(art_no, "duplicate Message-Id.");
	    return NULL;
	}
	context->fake_had_subject = art->subject;
    } else {
	*fake_p = (ARTICLE *)XtMalloc(sizeof(ARTICLE));
	art = *fake_p;
	art->tree_data.hook = NULL;
	art->tree_data.label = NULL;
	PARENT(art) = NULL;
	SIBLING(art) = NULL;
	CHILD1(art) = NULL;
	art->hash_next = NULL;
	art->next = NULL;
	art->msgid = memcpy(XtMalloc(msgid_len + 1), msgid, msgid_len + 1);
	art->hash_len = msgid_len;
	context->fake_had_subject = NULL;
    }

    art->subject = get_subject(context, subject);
    decode_rfc1522(from, context->charset);
    munge_nonprintable(from);
    art->from = XtNewString(from);
    art->xref = xref ? XtNewString(xref) : NULL;
    art->date = date;
    art->lines = lines;
#if ART_BYTES
    art->bytes = 0;
#endif
    art->pixmap = None;
    art->read = False;
    art->killed = False;
    art->no = art_no;

    return art;
}

static char *join_cont_lines(char *buffer)
{
    while ((buffer = strchr(buffer, '\n')))
	if (IS_SPACE(buffer[1])) {
	    if (buffer[-1] == '\r')
		buffer[-1] = ' ';
	    *buffer++ = ' ';
	} else {
	    if (buffer[-1] == '\r')
		buffer[-1] = '\0';
	    *buffer++ = '\0';
	    if (*buffer == '\0')
		return NULL;
	    else
		return buffer;
	    break;
	}

    return NULL;
}

ARTICLE *parse_head(long art_no, THREAD_CONTEXT *context, char *buffer)
{
    char	*from = NULL, *subject = NULL, *xref = NULL, *refs = NULL;
    char	*msgid = NULL;
    long	/*bytes = -1,*/ lines = -1;
    time_t	date = -1;

    while (buffer) {
	unsigned char	tmp;
	int		skip = False;

	tmp = *buffer;
	if (islower(tmp))
	    tmp = toupper(tmp);

	switch (*buffer) {
#if ART_BYTES
	case 'B':
	    if (case_lstrncmp(buffer, "bytes:", 6) == 0) {
		buffer += 6;
		bytes = atol(buffer);
	    }
	    break;
#endif
	case 'D':
	    if (case_lstrncmp(buffer, "date:", 5) == 0) {
		buffer += 5;
		while (IS_SPACE(*buffer))
		    buffer++;
		date = parsedate(buffer);
	    }
	    break;
	case 'F':
	    if (!from && case_lstrncmp(buffer, "from:", 5) == 0) {
		buffer += 5;
		while (IS_SPACE(*buffer))
		    buffer++;
		from = buffer;
		skip = False;
	    }
	    break;
	case 'L':
	    if (case_lstrncmp(buffer, "lines:", 6) == 0) {
		buffer += 6;
		lines = atol(buffer);
	    }
	    break;
	case 'M':
	    if (case_lstrncmp(buffer, "message-id:", 11) == 0) {
		char	*p;

		buffer += 11;
		while (IS_SPACE(*buffer))
		    buffer++;
		p = strchr(buffer, '\n');
		if (!p || *--p != '\r')
		    break;
		*p = '\0';
		msgid = get_msgid(buffer, p - 1);
		*p = '\r';
		buffer = p;
	    }
	    break;
	case 'R':
	    if (!refs && case_lstrncmp(buffer, "references:", 11) == 0) {
		buffer += 11;
		while (IS_SPACE(*buffer))
		    buffer++;
		refs = buffer;
		skip = False;
	    }
	    break;
	case 'S':
	    if (!subject && case_lstrncmp(buffer, "subject:", 8) == 0) {
		buffer += 8;
		while (IS_SPACE(*buffer))
		    buffer++;
		subject = buffer;
		skip = False;
	    }
	    break;
	case 'X':
	    if (!xref && case_lstrncmp(buffer, "xref:", 5) == 0) {
		char	*c;

		buffer += 5;
		while (IS_SPACE(*buffer))
		    buffer++;
		c = strchr(buffer, ' ');
		if (c) {
		    xref = c + 1;
		    skip = False;
		}
	    }
	    break;
	}

	buffer = join_cont_lines(buffer);
    }

    if (!from || !subject || !msgid)
	return NULL;

    munge_nonprintable(from);
    context->refs = refs;

    return gather_headers(art_no, context, subject, from,
			  date, msgid, lines, xref);
}

static char *head_articles(void)
{
    char	command[512];
    char	*buffer;
    ARTICLE	*art = NULL;
    long	n_pending, n_to_put;
    long	max_count, count;

    n_pending = put_chunk_head(main_thr->total);
    n_to_put = main_thr->total - n_pending;

    count = max_count = main_thr->total / 32;

    for (;;) {
	n_pending--;

	if (HEAD_DEBUG && global.head_debug)
	    fprintf(stderr, "n_pending=%ld,  n_to_put=%ld\n",
		    n_pending, n_to_put);

	if (n_pending <= 0) {
	    n_pending = 1;
	    n_to_put = 0;
	    server_write(main_server, "HEAD\r\nNEXT\r\n");
	} else if (n_pending < global.chunk_size && n_to_put > 0) {
	    long	temp;

	    temp = put_chunk_head(n_to_put);
	    n_pending += temp;
	    n_to_put -= temp;
	}

	buffer = server_read(main_server);
	if (!buffer)
	    return NULL;

	if (HEAD_DEBUG && global.head_debug)
	    fprintf(stderr, "%s\n", buffer);

	if (atoi(buffer) == NNTP_OK_HEAD) {
	    long	art_no, status;
	    int		tmp;

	    tmp = sscanf(buffer, "%ld%ld", &status, &art_no);
	    if (tmp > 0 && status == NNTP_OK_HEAD) {
		buffer = server_read_chunk(main_server);
		if (tmp >= 2 && art_no > 0 &&
		    (art = parse_head(art_no, main_thr, buffer))) {
		    if (main_thr->last_art)
			main_thr->last_art->next = art;
		    else
			main_thr->articles = art;
		    main_thr->last_art = art;

		    thread_article(main_thr, art);
		    main_thr->refs = NULL;
		}
	    }
	}

	buffer = server_read(main_server);
	if (!buffer)
	    break;

	if (HEAD_DEBUG && global.head_debug)
	    fprintf(stderr, "%s\n", buffer);

	if (atoi(buffer) != NNTP_OK_NOTEXT)
	    break;

	main_thr->n_done++;

	if (count-- <= 0) {
	    sprintf(command,
		    "Reading group slowly... %3ld%%",
		    (100 * main_thr->n_done) / main_thr->total);
	    set_message(command, False);
	    count = max_count;
	}
    }

    if (!buffer)
	return NULL;

    if (HEAD_DEBUG && global.head_debug && n_pending > 0)
	fprintf(stderr, "%ld pending, snarfing them.\n", n_pending);

    while (n_pending-- > 0) { /* next returned no next article */
	buffer = server_read(main_server);
	if (!buffer)
	    return NULL;

	if (HEAD_DEBUG && global.head_debug)
	    fprintf(stderr, "%s\n", buffer);

	if (atoi(buffer) == NNTP_OK_HEAD) {
	    buffer = server_read_chunk(main_server);
	    if (!buffer)
		return NULL;
	}

	buffer = server_read(main_server); /* reply to next */
	if (!buffer)
	    return NULL;

	if (HEAD_DEBUG && global.head_debug)
	    fprintf(stderr, "%s\n", buffer);
    }
    
    return CODE_TO_STR(NNTP_OK_GROUP);
}

static char *nntp_enter_group(char *reply)
{
    char	command[512];

    if (!reply) {
	sprintf(command, "GROUP %s\r\n", global.curr_group->name);
	reply = server_comm(main_server, command, True);
    }

    if (!reply || atoi(reply) != NNTP_OK_GROUP)
	return reply;

    sscanf(reply, "%*d%ld%ld%ld",
	   &main_thr->total,
	   &global.curr_group->first_art,
	   &global.curr_group->last_art);
    if (main_thr->total <= 0)
	main_thr->total = 1;

    global.curr_art = NULL;
    global.curr_subj = NULL;

    return reply;
}

static char *thread_articles(long asked_first)
{
    char	command[512];
    char	*buffer;
    long	first = global.curr_group->first_art;
    long	last  = global.curr_group->last_art;

    main_thr->charset = res_default_charset();
    if (!main_thr->charset)
	main_thr->charset = "iso-8859-1";

    if (asked_first < 0 || asked_first > global.curr_group->last_art)
	asked_first = global.curr_group->last_art;

    if (global.xover_supported) {
	if (main_thr->last_art)
	    first = main_thr->last_art->no + 1;
	else if (asked_first != 0)
	    first = asked_first;

	if (first > last)
	    return CODE_TO_STR(NNTP_OK_GROUP);

	sprintf(command, "XOVER %ld-%ld\r\n", first, last);
	buffer = server_comm(main_server, command, True);
	if (!buffer)
	    return NULL;
	if (atoi(buffer) == NNTP_OK_XOVER) {
	    buffer = xover_articles();
	    if (!buffer || atoi(buffer) != NNTP_OK_GROUP ||
		!main_thr->last_art ||
		main_thr->last_art->no >= last)
		return buffer;
	    /* else fall through to get the last few by HEAD */
	}
    }

    if (main_thr->last_art || asked_first != 0) {
	/* stat the first article we want */
	long	n;

	if (main_thr->last_art)
	    first = main_thr->last_art->no + 1;
	else
	    first = asked_first;
	/* this should succeed on the first pass */
	for (n = first ; n <= last ; n++) {
	    sprintf(command, "STAT %ld\r\n", first);
	    server_write(main_server, command);
	    buffer = server_read(main_server);
	    if (!buffer)
		return NULL;
	    if (atoi(buffer) == NNTP_OK_NOTEXT)
		break;
	    if (atoi(buffer) != NNTP_ERR_NOART)
		return buffer;
	}

	if (n > last)
	    return CODE_TO_STR(NNTP_OK_GROUP); /* weird... */
    }

    return head_articles();
}

static void popup_ask_how_many(void);

void read_group(char *reply, int may_ask, long first)
{
    char	message[256];
    int		threaded;

    if (!global.curr_group ||
	(threaded = thread_ahead_check(global.curr_group)) < 0)
	return;

    res_enter_group(global.curr_group->name);
    font_enter_group();

    set_busy(True);

    if (may_ask) {
	set_message("Server contacted, waiting for response...", False);
	reply = nntp_enter_group(reply);
    }

    if (reply && atoi(reply) == NNTP_OK_GROUP) {
	if (!threaded && may_ask && res_ask_how_many()) {
	    unset_busy();
	    popup_ask_how_many();
	    return;
	}

	reply = thread_articles(first);
    }

    if (!reply) {
	clear_thread_context(main_thr);
	reconnect_server(True);
	unset_busy();
    } else if (atoi(reply) == NNTP_OK_GROUP) {
	change_interruptible(False);
	set_message("Killing...", False);
	mark_read_articles(main_thr, global.curr_group);
	fix_authors(main_thr);
	kill_articles(global.curr_group);
	set_message("Sorting...", False);
	sort_threads();
	unset_busy();

	sprintf(message,
		"Group %s with %ld unread articles, "
		"%ld killed, %ld hot.",
		global.curr_group->name,
		global.curr_group->no_unread,
		global.n_killed, global.n_hot);
	set_message(message, False);
	setNewsModeGroup(False);

	cache_enter_group();
    } else {
	unset_busy();

	if (strlen(reply) > 200)
	    reply[200] = '\0';
	sprintf(message,
		"Error: message from server is %s", reply);
	set_message(message, True);
	clear_thread_context(main_thr);
    }
}

/*************************************************************************/

static void ask_dialogue_callback(Widget w,
				  XtPointer client_data,
				  XtPointer call_data)
{
    DialogueReport	*report = (DialogueReport *)call_data;
    long		first;

    XtPopdown(w);
    XtDestroyWidget(w);
    XFlush(display);

    switch (report->reply) {
    case DialogueReplyRight:   /* Cancel */
    case DialogueReplyClose:
	set_standard_message();
	break;
    case DialogueReplyMiddle:  /* All */
	set_message("Reading group...", False);
	read_group(CODE_TO_STR(NNTP_OK_GROUP), False, 0);
	break;
    case DialogueReplyLeft:    /* OK */
    case DialogueReplyEnter:
    case DialogueReplyTab:
	if (sscanf(report->buffer, "%ld", &first) != 1) {
	    XBell(display, 0);
	    return;
	}
	set_message("Reading group...", False);
	read_group(CODE_TO_STR(NNTP_OK_GROUP), False, first);
	break;
    }
}

static void popup_ask_how_many(void)
{
    Widget	w = NULL;
    char	message[128];
    char	first_buf[32];
    Arg		arg[8];
    long	first;

    if (global.curr_group->read_arts)
	first = global.curr_group->read_arts->last + 1;
    else
	first = global.curr_group->first_art;
    sprintf(first_buf, "%ld", first);

    sprintf(message,
	    "Range of available articles:  %ld - %ld\n"
	    "First unread article:  %ld\n\n"
	    "Start threading at which article?",
	    global.curr_group->first_art,
	    global.curr_group->last_art,
	    first);

    XtSetArg(arg[0], XtNmessage, message);
    XtSetArg(arg[1], XtNbuffer, first_buf);
    XtSetArg(arg[2], XtNleftLabel, "OK");
    XtSetArg(arg[3], XtNmiddleLabel, "All");
    XtSetArg(arg[4], XtNrightLabel, "Cancel");
    XtSetArg(arg[5], XtNcolormap, global.cmap);
    XtSetArg(arg[6], XtNvisual, global.visual);
    XtSetArg(arg[7], XtNdepth, global.depth);
    w = XtCreatePopupShell("askhowmany", dialogueWidgetClass,
			   main_widgets.shell, arg, 8);
    XtAddCallback(w, XtNcallback, ask_dialogue_callback, NULL);
    popup_under_pointer(w, XtGrabExclusive);
}
