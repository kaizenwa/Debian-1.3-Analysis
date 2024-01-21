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
#include <sys/stat.h>
#include "ahead.h"
#include "bg.h"
#include "codes.h"
#include "expand.h"
#include "file.h"
#include "newsrc.h"
#include "parse.h"
#include "resource.h"
#include "server.h"
#include "thread.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/ScrList.h"

typedef enum {
    AheadStateNone,
    AheadStateSentGroup,
    AheadStateSentXover,
    AheadStateDoingXover,
    AheadStateSentHead,
    AheadStateDoingHead,
    AheadStateSentNext
} AheadState;

enum {
    AheadFlagNoop,
    AheadFlagScheduled,
    AheadFlagThreading,
    AheadFlagDone
};

typedef struct GROUP_AHEAD_NODE GROUP_AHEAD_NODE;
struct GROUP_AHEAD_NODE {
    GROUP_AHEAD_NODE	*next;
    GROUP		*group;
    char		*file_name;
    unsigned char	flag;
    unsigned char	needs_update;
};

static GROUP_AHEAD_NODE	*schedule = NULL, *current = NULL;

static AheadState	ahead_state = AheadStateNone;
static THREAD_CONTEXT	*bogus_context = NULL;
static long		n_to_put = 0;
static long		n_pending = 0;
static long		total = 1;
static long		n_done = 0;
static long		hit_end;
static long		curr_art_no = -1;
static FILE		*fp = NULL;

static void close_fp(void)
{
    if (fp)
	fclose(fp);
    fp = NULL;	
}

static void print_refs(FILE *fp, ARTICLE *art)
{
    if (!art)
	return;

    print_refs(fp, A_PARENT(art));
    fprintf(fp, "<%s> ", art->msgid);
}

static void print_xover_line(FILE *fp, ARTICLE *art, char *refs)
{
    char	time_buf[32];

    fprintf(fp, "%ld\t%s\t%s\t%s\t<%s>\t",
	    art->no, art->subject->subject, art->from,
	    time_t_to_date(art->date, time_buf), art->msgid);
    if (refs)
	fputs(refs, fp);
    else
	print_refs(fp, A_PARENT(art));
    fprintf(fp, "\t\t%hu", art->lines);
    if (art->xref)
	fprintf(fp, "\tXref: %s\r\n", art->xref);
    else
	fputs("\r\n", fp);

}

static char *get_tmp_filename(char *group_name)
{
    char	*cache_dir = res_cache_dir();
    char	*ret;

    ret = XtMalloc(strlen(cache_dir) + strlen(group_name) + 2);
    sprintf(ret, "%s/%s", cache_dir, group_name);

    return ret;
}

static void clear_bogus_context(int create)
{
    if (bogus_context)
	clear_thread_context(bogus_context);
    else if (create)
	bogus_context = create_thread_context();
}

static GROUP_AHEAD_NODE *create_ahead_node(GROUP *group, int flag)
{
    GROUP_AHEAD_NODE	*temp, *last = schedule;

    temp = (GROUP_AHEAD_NODE *)XtMalloc(sizeof *temp);
    temp->next         = NULL;
    temp->group        = group;
    temp->file_name    = get_tmp_filename(group->name);
    temp->flag         = flag;
    temp->needs_update = False;

    group->ahead_flag = True;

    if (!last)
	schedule = temp;
    else {
	while (last->next)
	    last = last->next;
	last->next = temp;
    }

    return temp;
}

static void remove_ahead_node(GROUP_AHEAD_NODE *node, int do_unlink)
{
    GROUP	*group = node->group;

    if (schedule)
	if (node == schedule)
	    schedule = schedule->next;
	else {
	    GROUP_AHEAD_NODE	*prev = schedule;

	    while (prev->next && prev->next != node)
		prev = prev->next;

	    if (prev->next)
		prev->next = prev->next->next;
	}

    if (node->file_name) {
	if (do_unlink)
	    unlink_expand(node->file_name);
	XtFree(node->file_name);
    }
    node->next = NULL;
    if (node->group)
	node->group->ahead_flag = False;
    node->group = NULL;
    node->file_name = NULL;
    XtFree((char *)node);
    update_group_entry(group);
}

static void remove_current_node(void)
{
    clear_bogus_context(False);
    if (current)
	remove_ahead_node(current, True);
    current = NULL;
    close_fp();
}

static GROUP_AHEAD_NODE *get_scheduled_node(void)
{
    GROUP_AHEAD_NODE	*node;

    for (node = schedule ; node ; node = node->next)
	if (node->flag == AheadFlagScheduled)
	    return node;

    return NULL;
}

static GROUP_AHEAD_NODE *find_ahead_node(GROUP *group)
{
    GROUP_AHEAD_NODE	*node;

    for (node = schedule ; node ; node = node->next)
	if (node->group == group)
	    return node;

    return NULL;
}

#define HEAD_NEXT	12
#define MAX_HEAD_NEXT	16

static int put_head_next(SERVER *server)
{
    char	buffer[MAX_HEAD_NEXT * HEAD_NEXT + 1];
    char	*c;
    long	i, n = n_to_put;
    int		busy;

    if (hit_end || n_pending >= MAX_HEAD_NEXT)
	return True;

    if (n == 0)
	n = 1;
    else {
	if (n > MAX_HEAD_NEXT)
	    n = MAX_HEAD_NEXT;
	n_to_put -= n;
    }
    n_pending += n;

    buffer[0] = '\0';
    for (i = 0, c = buffer ; i < n ; i++, c += HEAD_NEXT)
	strcpy(c, "HEAD\r\nNEXT\r\n");

    busy = global.busy;
    if (!busy)
	global.busy = True;
    i = server_write(server, buffer);
    if (!busy)
	global.busy = False;

    return i == 0;
}

static int thread_ahead_start_head(SERVER *server)
{
    clear_bogus_context(True);
    n_pending = 0;
    n_to_put = total;
    hit_end = False;

    return put_head_next(server);
}

static int start_xover(SERVER *server)
{
    long		first, last;
    GROUP		*group;
    GROUP_AHEAD_NODE	*node;

    ahead_state = AheadStateNone;
    group = bg_in_group(&total, &first, &last);
    if (!group)
	return False;
    node = find_ahead_node(group);
    if (!node || node->flag != AheadFlagThreading)
	return False;

    if (total <= 0)
	total = 1;

    if (global.xover_supported) {
	char	command[512];
	int	tmp, busy = global.busy;

	sprintf(command, "XOVER %ld-%ld\r\n", first, last);
	if (!busy)
	    global.busy = True;
	tmp = server_write(server, command);
	if (!busy)
	    global.busy = False;

	if (tmp == 0) {
	    ahead_state = AheadStateSentXover;
	    return True;
	}

	current->flag = AheadFlagScheduled;
	update_group_entry(current->group);
	ahead_state = AheadStateNone;
	return False;
    }

    ahead_state = AheadStateSentHead;
    thread_ahead_start_head(server); /* check return? */
    return True;
}

static int start_group(SERVER *server)
{
    char		message[512];
    GROUP_AHEAD_NODE	*node;
    GROUP		*group;

    n_to_put = 0;
    n_pending = 0;
    n_done = 0;
    ahead_state = AheadStateNone;

    while ((node = get_scheduled_node())) {
	fp = fopen_expand(node->file_name, "w", True);
	if (fp)
	    break;
    }

    if (!node)
	return False;

    current = node;
    node->flag = AheadFlagThreading;
    update_group_entry(node->group);

    group = bg_in_group(NULL, NULL, NULL);
    if (total <= 0)
	total = 1;

    if (group && group == node->group)
	return start_xover(server);

    bg_start_group(node->group);
    ahead_state = AheadStateSentGroup;
    sprintf(message, "Threading %s...", node->group->name);
    set_message(message, False);

    return True;
}

static int thread_ahead_proc(SERVER *server)
{
    char	*buffer = NULL;
    long	status;
    int		tmp;
    ARTICLE	*art;

    if (!server) {
	remove_current_node();
	ahead_state = AheadStateNone;
	return False;
    }

    do {
	switch (ahead_state) {
	case AheadStateNone:
	    return start_group(server);
	case AheadStateSentGroup:
	    return start_xover(server);
	case AheadStateSentXover:
	    buffer = server_get_line(server);
	    if (!buffer)
		break;

	    if (atoi(buffer) == NNTP_OK_XOVER) {
		ahead_state = AheadStateDoingXover;
		break;
	    }

	    /* fall back to head */
	    thread_ahead_start_head(server); /* check return? */
	    return True;
	case AheadStateDoingXover:
	    buffer = server_get_line(server);
	    if (!buffer)
		break;

	    n_done++;
	    if (fp)
		fprintf(fp, "%s\r\n", buffer);

	    if (IS_DOT(buffer)) {
		current->flag = AheadFlagDone;
		update_group_entry(current->group);
		current = NULL;
		close_fp();
		ahead_state = AheadStateNone;
		return False;
	    }
	    break;
	case AheadStateSentHead:
	    buffer = server_get_line(server);
	    if (!buffer)
		break;

	    tmp = sscanf(buffer, "%ld%ld", &status, &curr_art_no);
	    if (tmp <= 0 || status != NNTP_OK_HEAD)
		ahead_state = AheadStateSentNext;
	    else {
		ahead_state = AheadStateDoingHead;
		if (tmp == 1 || hit_end)
		    curr_art_no = -1;
	    }

	    if (!hit_end) {
		n_done++;
		if (n_pending < MAX_HEAD_NEXT  && !put_head_next(server)) {
		    ahead_state = AheadStateNone;
		    return False;
		}
	    }
	    break;
	case AheadStateDoingHead:
	    buffer = server_get_chunk(server);
	    if (!buffer)
		break;

	    if (curr_art_no > 0) {
		art = parse_head(curr_art_no, bogus_context, buffer);
		if (art && fp)
		    print_xover_line(fp, art, get_refs(bogus_context));
	    }
	    curr_art_no = -1;
	    ahead_state = AheadStateSentNext;
	    break;
	case AheadStateSentNext:
	    buffer = server_get_line(server);
	    if (!buffer)
		break;

	    n_pending--;
	    if (!hit_end) {
		status = atoi(buffer);
		if (status != NNTP_OK_NOTEXT) /* maybe better checking */
		    hit_end = True;
	    }

	    if (hit_end && n_pending <= 0) {
		current->flag = AheadFlagDone;
		update_group_entry(current->group);
		current = NULL;
		close_fp();
		ahead_state = AheadStateNone;
		return False;
	    }

	    ahead_state = AheadStateSentHead;
	    break;
	}
    } while (buffer);

    return True;
}

void action_schedule_thread_ahead(Widget w, XEvent *event,
				  String *params, Cardinal *no_params)
{
    GROUP		*group = NULL;
    GROUP_AHEAD_NODE	*node;
    long		n, i;

    switch (global.mode) {
    case NewsModeConnected:
	if (w != main_widgets.group_list)
	    group = global.curr_group;
	else {
	    n = ScrListEventToIndex(w, event);
	    if (n < 0)
		break;
	    for (i = 0 ; i < global.no_groups ; i++)
		if (global.groups[i]->disp == n) {
		    group = global.groups[i];
		    break;
		}
	}
	break;
    case NewsModeAllgroups:
	if (w != main_widgets.group_list)
	    group = global.curr_group;
	else {
	    n = ScrListEventToIndex(w, event);
	    if (n >= 0 && n < global.no_groups)
		group = global.groups[n];
	}
	break;
    case NewsModeSomegroups:
	n = ScrListEventToIndex(w, event);
	if (n < 0)
	    break;
	for (i = 0 ; i < global.no_groups ; i++)
	    if (global.groups[i]->disp == n) {
		group = global.groups[i];
		break;
	    }
	break;
    case NewsModeDisconnected:
    case NewsModeGroup:
    case NewsModeThread:
    case NewsModeNewgroups:
	if (global.bell)
	    XBell(display, 0);
	return;
    }

    if (!group) {
	if (global.bell)
	    XBell(display, 0);
	return;
    }

    node = find_ahead_node(group);
    if (node) {
	char	message[128];

	switch (node->flag) {
	case AheadFlagNoop:
	    break;
	case AheadFlagDone:
	    set_message("That group has already been threaded!", True);
	    return;
	case AheadFlagScheduled:
	    set_message("That group is already scheduled for "
			"thread ahead.", True);
	    return;
	case AheadFlagThreading:
	    sprintf(message, "That group is beeing threaded, %ld%% done!",
		    100 * n_done / total);
	    set_message(message, False);
	    return;
	}
    }

    if (!node)
	create_ahead_node(group, AheadFlagScheduled);
    update_group_entry(group);

    bg_nudge(thread_ahead_proc);
}

char thread_ahead_char(GROUP *group)
{
    GROUP_AHEAD_NODE	*node = find_ahead_node(group);

    if (node)
	switch (node->flag) {
	case AheadFlagNoop:
	    return ' ';
	case AheadFlagScheduled:
	    return '-';
	case AheadFlagThreading:
	    return '*';
	case AheadFlagDone:
	    return '+';
	}

    return ' ';
}

void thread_ahead_shutdown(void)
{
    int	save = res_save_thread_info();

    n_pending = 0;
    n_to_put = 0;
    curr_art_no = -1;
    clear_bogus_context(False);
    close_fp();

    while (schedule)
	remove_ahead_node(schedule, !save);
}

int thread_ahead_check(GROUP *group)
{
    GROUP_AHEAD_NODE	*node = find_ahead_node(group);
    char		message[128];
    int			threaded = False;
    SERVER		*server;
    int			fd;

    if (node)
	switch (node->flag) {
	case AheadFlagNoop:
	    break;
	case AheadFlagThreading:
	    sprintf(message,
		    "Thread ahead in progress %ld%% done, try later.",
		    100 * n_done / total);
	    set_message(message, False);
	    return -1;
	case AheadFlagScheduled:
	    node->flag = AheadFlagNoop;
	    update_group_entry(group);
	    break;
	case AheadFlagDone:
	    fd = open_expand(node->file_name, O_RDONLY, True);
	    if (fd < 0)
		break;

	    set_message("Threading from file...", False);
	    set_busy(False);
	    server = server_create(fd);
	    thread_from_file(server, group->first_art);
	    server_free(server);
	    unset_busy();
	    threaded = True;
	    break;
	}

    return threaded;
}

void thread_ahead_leave_group(GROUP *group)
{
    GROUP_AHEAD_NODE	*node = find_ahead_node(group);
    int			keep = res_keep_thread_info(group->subscribed);
    int			save = group->subscribed && res_save_thread_info();

    if (!save && !keep) {
	if (node)
	    remove_ahead_node(node, True);
    } else {
	if (!node)
	    node = create_ahead_node(group, AheadFlagNoop);
	if (node->flag == AheadFlagNoop || node->flag == AheadFlagScheduled ||
	    (node->flag == AheadFlagDone &&save && node->needs_update)) {
	    FILE	*fp;

	    fp = fopen_expand(node->file_name, "w", True);
	    if (!fp)
		fputs("knews: couldn't create temp file "
		      "for thread data.\n", stderr);
	    else {
		ARTICLE	*art;

		for (art = get_articles(main_thr) ; art ; art = art->next)
		    print_xover_line(fp, art, NULL);
		fclose(fp);
		node->flag = AheadFlagDone;
		node->needs_update = False;
	    }
	}
    }

    clear_thread_context(main_thr);
    global.curr_group = group;
}

static int scan_group(GROUP *group, int scan)
{
    struct stat		stat_buf;
    GROUP_AHEAD_NODE	*node;
    char		*path;
    int			tmp;

    node = create_ahead_node(group, AheadFlagScheduled);

    if (!scan || !(path = expand_path(node->file_name)))
	return True;

    tmp = stat(path, &stat_buf);
    if (tmp < 0 && errno != ENOENT)
	perror(path);
    XtFree(path);
    if (tmp < 0)
	return True;
    node->flag = AheadFlagDone;
    if (scan)
	node->needs_update = True;

    return False;
}

void thread_ahead_init(void)
{
    char	**groups = res_thread_ahead_groups();
    int		scan     = res_save_thread_info();
    int		doit     = False;

    if ((!groups || !*groups) && !scan)
	return;

    if (scan || case_lstrcmp(*groups, "all") == 0) {
	int	all = scan || **groups == 'A';
	int	n;

	for (n = 0 ; n < global.no_groups ; n++) {
	    if (!global.groups[n]->subscribed)
		break;
	    if (!all && global.groups[n]->no_unread <= 0)
		continue;

	    doit |= scan_group(global.groups[n], scan);
	    update_group_entry(global.groups[n]);
	    if (scan)
		XFlush(display);
	}
    } else {
	while (*groups) {
	    GROUP	*group = find_group(*groups);

	    if (!group)
		fprintf(stderr, "thread_ahead: couldn't find group %s\n",
			*groups);
	    else {
		doit |= scan_group(group, False);
		update_group_entry(group);
		if (scan)
		    XFlush(display);
	    }

	    groups++;
	}
    }

    if (doit)
	bg_nudge(thread_ahead_proc);
}

int thread_ahead_todo(void)
{
    return get_scheduled_node() != NULL;
}
