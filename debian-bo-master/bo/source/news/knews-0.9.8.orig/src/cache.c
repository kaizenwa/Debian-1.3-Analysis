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
#include "bg.h"
#include "cache.h"
#include "codes.h"
#include "file.h"
#include "resource.h"
#include "server.h"
#include "thread.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/Message.h"
#include "../Widgets/Notice.h"
#include "../Widgets/Util.h"

#define MAX_CACHE	32

typedef struct {
    long		no;
    char		*file_name;
    unsigned char	status;
} ART_CACHE_NODE;

enum {
    StatusEmpty,
    StatusOk,
    StatusDoing
};

static ART_CACHE_NODE	ahead_cache[MAX_CACHE] = {{0, }, };
static ART_CACHE_NODE	trail_cache[MAX_CACHE] = {{0, }, };
static int		ahead_size;
static int		trail_size;
static char		*cache_dir;
static int		cache_dir_len;

static int	cache_proc(SERVER *server);
static Widget	cache_notice = NULL;

static void show_cache_stats(void);

static char *get_fn(void)
{
    static unsigned long	count = 0;
    char	*fn;

    fn = XtMalloc(cache_dir_len + 8);
    sprintf(fn, "%s/%lu", cache_dir, count++);

    return fn;
}

static void clear_cache_node(ART_CACHE_NODE *node)
{
    if (node->file_name) {
	unlink_expand(node->file_name);
	XtFree(node->file_name);
    }
    node->file_name = NULL;
    node->no = 0;
    node->status = StatusEmpty;
}

static int cache_find(ART_CACHE_NODE *cache, int size, long no)
{
    int	i;

    for (i = 0 ; i < size ; i++)
	if (cache[i].no == no)
	    return i;

    return -1;
}

static int make_room(ART_CACHE_NODE *cache, int size)
{
    if (size <= 0)
	return False;

    if (cache[0].file_name) {
	if (cache[size - 1].no > 0)
	    clear_cache_node(cache + size - 1);
	if (size - 1 > 0)
	    memmove(cache + 1, cache, (size - 1) * sizeof cache[0]);
	cache[0].file_name = NULL;
    }

    cache[0].no = 0;
    cache[0].status = StatusEmpty;

    return True;
}

static void cache_clear(void)
{
    int	i;

    for (i = 0 ; i < MAX_CACHE ; i++) {
	clear_cache_node(ahead_cache + i);
	clear_cache_node(trail_cache + i);
    }

    ahead_size = 0;
    trail_size = 0;
    cache_dir = NULL;
}

FILE *cache_get_file(long art_no)
{
    FILE	*fp;
    char	*fn;

    if (cache_find(ahead_cache, ahead_size, art_no) >= 0 ||
	cache_find(trail_cache, trail_size, art_no) >= 0)
	return NULL;

    if (!make_room(trail_cache, trail_size)) {
	if (cache_notice)
	    show_cache_stats();
	return NULL;
    }

    fn = get_fn();
    fp = fopen_expand(fn, "w", True);
    if (!fp) {
	if (cache_notice)
	    show_cache_stats();
	XtFree(fn);
	return NULL;
    }

    trail_cache[0].file_name = fn;
    trail_cache[0].no = art_no;
    trail_cache[0].status = StatusDoing;
    if (cache_notice)
	show_cache_stats();

    return fp;
}

SERVER *cache_get_server(long art_no, int is_read)
{
    int		i, fd = -1;

    if (trail_size > 0) {
	i = cache_find(trail_cache, trail_size, art_no);
	if (i >= 0) {
	    fd = open_expand(trail_cache[i].file_name, O_RDONLY, True);
	    return fd < 0 ? NULL : server_create(fd);
	}
    }

    if (ahead_size <= 0)
	return NULL;

    i = cache_find(ahead_cache, ahead_size, art_no);
    if (i < 0) {
	ART_CACHE_NODE	*node = ahead_cache + ahead_size - 1;

	if (!is_read || (ahead_size == 1 && node->status == StatusDoing))
	    return NULL;

	clear_cache_node(node);
	bg_nudge(cache_proc);

	return NULL;
    }

    if (ahead_cache[i].status != StatusOk)
	return NULL;

    fd = open_expand(ahead_cache[i].file_name, O_RDONLY, True);
    if (trail_size <= 0 || !make_room(trail_cache, trail_size))
	clear_cache_node(ahead_cache + i);
    else {
	trail_cache[0] = ahead_cache[i];
	ahead_cache[i].file_name = NULL;
	ahead_cache[i].no = 0;
	ahead_cache[i].status = StatusEmpty;
    }
    if (cache_notice)
	show_cache_stats();

    return fd < 0 ? NULL : server_create(fd);
}

void cache_fetch_done(long art_no)
{
    int	i;

    i = cache_find(trail_cache, trail_size, art_no);
    if (i >= 0)
	trail_cache[i].status = StatusOk;
    else
	fprintf(stderr, "done but no node %ld\n", art_no);

    if (cache_notice)
	show_cache_stats();
}

void cache_fetch_failed(long art_no)
{
    int	i;

    i = cache_find(trail_cache, trail_size, art_no);
    if (i >= 0)
	clear_cache_node(trail_cache + i);

    if (cache_notice)
	show_cache_stats();
}

/**************************************************************************/

typedef enum {
    CacheStateNone,
    CacheStateSentArticle,
    CacheStateDoingArticle
} CacheState;

static CacheState	state = CacheStateNone;
static int		snarfing = False;
static FILE		*fp = NULL;

static void current_done(int ok)
{
    if (!fp)
	return;

    fclose(fp);
    fp = NULL;
    if (!snarfing)
	ahead_cache[0].status = ok ? StatusOk : StatusEmpty;

    if (cache_notice)
	show_cache_stats();
}

static int cached(long no)
{
    int	i;

    for (i = 0 ; i < ahead_size ; i++)
	if (ahead_cache[i].no == no)
	    return True;
    for (i = 0 ; i < trail_size ; i++)
	if (trail_cache[i].no == no)
	    return True;

    return False;
}

static long find_art_to_read(void)
{
    SUBJECT	*subj;
    ARTICLE	*art;

    if (global.mode != NewsModeGroup && global.mode != NewsModeThread)
	return 0;

    art = global.curr_art;
    if (!art)
	return 0;

    subj = art->subject;
    art = next_in_thread_preorder(art);
    do {
	while (art && (!art->from || art->read || cached(art->no)))
	    art = next_in_thread_preorder(art);
	if (art)
	    break;

	while (subj->next && subj->next->thread == subj->thread)
	    subj = subj->next;
	subj = subj->next;
	if (subj)
	    art = subj->thread;
    } while (subj);

    return art ? art->no : 0;
}

static int start_article(SERVER *server)
{
    char		command[32];
    long		no;
    int			tmp, busy;

    state = CacheStateNone;
    if ((global.mode != NewsModeGroup && global.mode != NewsModeThread) ||
	ahead_size <= 0 || (no = find_art_to_read()) <= 0 ||
	ahead_cache[ahead_size - 1].status != StatusEmpty ||
	!make_room(ahead_cache, ahead_size))
	return False;

    if (bg_in_group(NULL, NULL, NULL) != global.curr_group) {
	bg_start_group(global.curr_group);
	return True;
    }

    ahead_cache[0].file_name = get_fn();
    ahead_cache[0].no = no;
    ahead_cache[0].status = StatusEmpty;
    fp = fopen_expand(ahead_cache[0].file_name, "w", True);

    sprintf(command, "ARTICLE %ld\r\n", no);
    busy = global.busy;
    if (!busy)
	global.busy = True;
    tmp = server_write(server, command);
    if (!busy)
	global.busy = False;

    if (tmp < 0) {
	if (cache_notice)
	    show_cache_stats();
	return False;
    }

    ahead_cache[0].status = StatusDoing;
    state = CacheStateSentArticle;
    if (cache_notice)
	show_cache_stats();

    return True;
}

static int cache_proc(SERVER *server)
{
    char	*buffer = NULL;

    if (!server) {
	current_done(False);
	snarfing = False;
	state = CacheStateNone;
	return False;
    }

    do {
	switch (state) {
	case CacheStateNone:
	    return start_article(server);
	case CacheStateSentArticle:
	    buffer = server_get_line(server);
	    if (!buffer)
		break;

	    if (atoi(buffer) != NNTP_OK_ARTICLE) {
		current_done(False);
		return start_article(server);
	    }

	    state = CacheStateDoingArticle;
	    break;
	case CacheStateDoingArticle:
	    buffer = server_get_line(server);
	    if (!buffer)
		break;

	    if (fp)
		fprintf(fp, "%s\r\n", buffer);

	    if (!IS_DOT(buffer))
		break;

	    current_done(!snarfing);
	    state = CacheStateNone;

	    return False;
	}
    } while (buffer);

    return True;
}

void cache_leave_group(void)
{
    if (state != CacheStateNone) {
	current_done(False);
	snarfing = True;
    }

    cache_clear();

    if (cache_notice)
	show_cache_stats();
}

void cache_enter_group(void)
{
    int		tmp;

    tmp = res_cache_ahead_size();
    if (tmp > MAX_CACHE)
	tmp = MAX_CACHE;
    else if (tmp < 0)
	tmp = 0;
    ahead_size = tmp;

    tmp = res_cache_trail_size();
    if (tmp > MAX_CACHE)
	tmp = MAX_CACHE;
    else if (tmp < 0)
	tmp = 0;
    trail_size = tmp;

    cache_dir = res_cache_dir();
    if (!cache_dir)
	cache_dir = "/tmp";
    cache_dir_len = strlen(cache_dir);

    if (global.show_cache)
	popup_cache_stats();

    if (ahead_size > 0)
	bg_nudge(cache_proc);
}

int cache_todo(void)
{
    if (global.mode != NewsModeGroup && global.mode != NewsModeThread)
	return False;

    return find_art_to_read() > 0;
}

/**************************************************************************/

static Widget	message_widget = NULL;

#define TRAIL_PREFIX "Trail cache:  "
#define AHEAD_PREFIX "Ahead cache:  "
static char	cache_message[] =
TRAIL_PREFIX "                                \n"
AHEAD_PREFIX "                                ";
#define TRAIL_OFFSET (sizeof TRAIL_PREFIX - 1)
#define AHEAD_OFFSET (sizeof AHEAD_PREFIX - 1 + sizeof cache_message / 2)

static void cache_notice_callback(Widget w,
				  XtPointer client_data,
				  XtPointer call_data)
{
    XtPopdown(w);
}

void popup_cache_stats(void)
{
    if (!cache_notice)
	cache_notice =
	    popup_notice("cachestats", cache_message, NULL, NULL, NULL, 0,
			 cache_notice_callback, NULL, XtGrabNone);
    else if (is_popped_up(cache_notice))
	return;
    else
	XtPopup(cache_notice, XtGrabNone);

    message_widget = NoticeMessageWidget(cache_notice);

    show_cache_stats();
}

static void show_cache_stats(void)
{
    int		i;

    if (!message_widget)
	return;

    for (i = 0 ; i < trail_size ; i++)
	cache_message[TRAIL_OFFSET + i] = "-+*"[trail_cache[i].status];
    while (i < MAX_CACHE)
	cache_message[TRAIL_OFFSET + i++] = ' ';

    for (i = 0 ; i < ahead_size ; i++)
	cache_message[AHEAD_OFFSET + i] = "-+*"[ahead_cache[i].status];
    while (i < MAX_CACHE)
	cache_message[AHEAD_OFFSET + i++] = ' ';

    MessageSetAndRedraw(message_widget, cache_message, False);
}

