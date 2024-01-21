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
#include "expand.h"
#include "file.h"
#include "k_I.h"
#include "k_edit.h"
#include "k_file.h"
#include "k_kill.h"
#include "k_node.h"
#include "resource.h"
#include "server.h"
#include "util.h"
#include "thread.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/Compat.h"
#include "../Widgets/Util.h"

static long		  n_files          = 0;
static long		  n_alloc          = 0;
static KILL_FILE	**kill_files       = NULL;
static KILL_FILE	 *global_kill_file = NULL;

static KILL_FILE *read_kill_file(GROUP *group)
{
    SERVER	*s;
    KILL_FILE	*file;
    char	*line;
    char	*file_name;
    long	n = 0;
    int		fd;

    if (group)
	file_name = global.group_kill_file_templ;
    else {
	file_name = res_kill_file();
	if (!file_name)
	    file_name = global.kill_file_templ;
    }

    file = (KILL_FILE *)XtMalloc(sizeof *file);
    file->n         = 0;
    file->nodes     = NULL;
    file->group     = group;
    file->file_name = expand_path(file_name);
    file->w         = NULL;
    file->expire    = res_expire_kills() && !res_ask_how_many();
    file->stay_up   = False;
    file->dirty     = False;

    if (!file->file_name)
	return file;

    fd = open(file->file_name, O_RDONLY);
    if (fd < 0) {
	if (errno != ENOENT)
	    perror(file->file_name);
	return file;
    }

    s = server_create(fd);
    while ((line = server_read(s))) {
	KILL_NODE	*node = parse_kill_line(line, group == NULL);

	if (!node)
	    continue;

	if (n < file->n + 8)
	    file->nodes = (KILL_NODE **)XtRealloc((char *)file->nodes,
						  (n = file->n + 8) *
						  sizeof file->nodes[0]);
	file->nodes[file->n++] = node;
    }
    server_free(s);

    return file;
}

KILL_FILE *get_kill_file(GROUP *group)
{
    long	n;

    if (!group)
	return global_kill_file;

    for (n = 0 ; n < n_files ; n++)
	if (kill_files[n]->group == group)
	    return kill_files[n];

    if (n_alloc < n_files + 8)
	kill_files =
	    (KILL_FILE **)XtRealloc((char *)kill_files,
				    (n_alloc = n_files + 8) *
				    sizeof *kill_files);

    return kill_files[n_files++] = read_kill_file(group);
}

static void free_kill_file(KILL_FILE *file)
{
    long	n;

    if (file->w) {
	destroy_kill_widgets(file->w);
	file->w = NULL;
    }

    for (n = 0 ; n < file->n ; n++)
	free_kill_node(file->nodes[n]);

    XtFree((char *)file->nodes);
    XtFree(file->file_name);
    file->nodes = NULL;
    file->file_name = NULL;
    XtFree((char *)file);
}

static int update_kill_file(KILL_FILE *file)
{
    FILE	*fp;
    long	n;
    int		ok;

    if (!file->file_name)
	return False;

    unlink(file->file_name);
    if (file->n <= 0)
	return True;

    fp = fopen_mkdir(file->file_name, "w", True);
    if (!fp) {
	perror(file->file_name);
	return False;
    }

    for (n = 0 ; n < file->n ; n++)
	fprint_kill_node(fp, file->nodes[n], file->expire);

    ok = (fclose(fp) >= 0);
    if (ok)
	file->dirty = False;

    return ok;
}

void read_global_kill_file(void)
{
    global_kill_file = read_kill_file(NULL);
}

int update_kill_files(void)
{
    int		ok = True;
    long	n;

    if (global_kill_file && global_kill_file->dirty &&
	!update_kill_file(global_kill_file))
	ok = False;

    for (n = 0 ; n < n_files ; n++)
	if (kill_files[n]->dirty)
	    if (!update_kill_file(kill_files[n]))
		ok = False;

    return ok;
}

void kill_exit_group(GROUP *group)
{
    KILL_FILE	*file;
    long	n;

    for (n = 0 ; n < n_files ; n++)
	if (kill_files[n]->group == group)
	    break;

    if (n >= n_files)
	return;

    file = kill_files[n];
    if (file->stay_up)
	return;
    if (file->w)
	popdown_kill_editor(file->w);
    if (group->subscribed)
	return;

    if (file->dirty)
	if (!update_kill_file(file)) {
	    popup_title_notice(NULL, "Failed to update kill file!", True);
	    return;
	}

    free_kill_file(file);
    n_files--;
    if (n < n_files)
	memmove(kill_files + n, kill_files + n + 1,
		(n_files - n) * sizeof kill_files[0]);
    kill_files[n_files] = NULL;
}

void kill_cleanup(void)
{
    long	n;

    free_kill_file(global_kill_file);
    global_kill_file = NULL;

    for (n = 0 ; n < n_files ; n++)
	free_kill_file(kill_files[n]);

    XtFree((char *)kill_files);
    kill_files = NULL;
    n_files    = 0;
    n_alloc    = 0;
}

void kill_articles(GROUP *group)
{
    long	n_killed = 0;
    long	n_hot    = 0;
    ARTICLE	*arts  = get_articles(main_thr);
    SUBJECT	*subjs = get_subjects(main_thr);
    SUBJECT	*subj;
    long	i, n;
    KILL_FILE	*file = get_kill_file(group);
    KILL_NODE	**nodes;

    n     = global_kill_file->n;
    nodes = global_kill_file->nodes;
    for (i = 0 ; i < n ; i++) {
	KILL_NODE	*node = nodes[i];

	if (node->expired ||
	    (node->group_str &&
	     (!node->group_re ||
	      regexec(node->group_re, group->name, 0, NULL, 0) != 0)))
	    continue;

	if (node->hot)
	    n_hot +=
		hot_funcs[node->field][node->scope](node, arts, subjs);
	else
	    n_killed +=
		kill_funcs[node->field][node->scope](node, arts, subjs);

	if (file->expire && node->expired)
	    file->dirty = True;
    }

    n     = file->n;
    nodes = file->nodes;
    for (i = 0 ; i < n ; i++) {
	KILL_NODE	*node = nodes[i];

	if (node->expired)
	    continue;

	if (node->hot)
	    n_hot +=
		hot_funcs[node->field][node->scope](node, arts, subjs);
	else
	    n_killed +=
		kill_funcs[node->field][node->scope](node, arts, subjs);

	if (file->expire && node->expired)
	    file->dirty = True;
    }

    for (subj = subjs ; subj ; subj = subj->next) {
	subj->pixmap = None;
	update_subj_hot_value(subj);
    }

    global.n_killed = n_killed;
    global.n_hot    = n_hot;
}

void kill_edit_popup(GROUP *group)
{
    KILL_FILE	*file = get_kill_file(group);

    popup_kill_editor(file);
}

/*********************************************************************/

int add_kill_node(KILL_FILE	*file,
		  int		 append,
		  int		 field,
		  int		 scope,
		  int		 hot,
		  char		*color,
		  char		*expr,
		  char		*group)
{
    KILL_NODE	*node;
    regex_t	*expr_re, *group_re;
    int		code;

    if (field == KillFieldMsgid) {
	expr_re = NULL;
	if (expr[0] != '<' || expr[strlen(expr) - 1] != '>') {
	    set_message("Bad Message-Id!", True);
	    return False;
	}
    } else {
	expr_re = (regex_t *)XtMalloc(sizeof *expr_re);
	code = regcomp(expr_re, expr, REGEXP_COMPILE_FLAGS);
	if (code != 0) {
	    popup_regexpnotice(code, expr_re);
	    XtFree((char *)expr_re);
	    return False;
	}
    }

    if (!group)
	group_re = NULL;
    else {
	group_re = (regex_t *)XtMalloc(sizeof *group_re);
	code = regcomp(group_re, group, REGEXP_COMPILE_FLAGS);
	if (code != 0) {
	    popup_regexpnotice(code, group_re);
	    XtFree((char *)expr_re);
	    XtFree((char *)group_re);
	    return False;
	}
    }

    node = (KILL_NODE *)XtMalloc(sizeof *node);
    node->expr_str      = XtNewString(expr);
    node->expr_re       = expr_re;
    node->group_str     = group ? XtNewString(group) : NULL;
    node->group_re      = group_re;
    node->color         = color ? XtNewString(color) : NULL;
    node->pixel         = global.default_hot_pixel;
    node->pixmap        = None;
    node->field         = field;
    node->scope         = scope;
    node->hot           = hot;
    node->expired       = False;
    node->alloced_pixel = False;
    if (node->color)
	alloc_hot_pixel(node, True);

    file->dirty = True;
    file->nodes =
	(KILL_NODE **)XtRealloc((char *)file->nodes,
				(file->n + 1) * sizeof file->nodes[0]);
    if (append)
	file->nodes[file->n++] = node;
    else {
	if (file->n > 0)
	    memmove(file->nodes + 1, file->nodes,
		    file->n * sizeof file->nodes[0]);
	file->n++;
	file->nodes[0] = node;
    }

    if (file->w)
	kill_editor_notify_add(file, append);

    return True;
}
