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
#include "expand.h"
#include "k_I.h"
#include "k_action.h"
#include "k_file.h"
#include "k_node.h"
#include "newsrc.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"

static const char *scope_str[] =
{"article",	"subject",	"thread",	"subthread"};
static const char *field_str[] =
{"Message-Id",	"Subject",	"From",		"Xref"};

static void add_kill(String *params, int no_params, int append)
{
    KILL_FILE	*file;
    char	buffer[128];
    int		scope, field, hot;
    char	*expr;

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread)) {
	XBell(display, 0);
	return;
    }

    if (!global.curr_art) {
	set_message("No selected article!", True);
	return;
    } else if (!global.curr_art->from) {
	set_message("That's a fake article!", True);
	return;
    }

    if (no_params != 2 && no_params != 3) {
	if (append)
	    set_message("kill-append() called with "
			"wrong number of arguments!", True);
	else
	    set_message("kill-prepend() called with "
			"wrong number of arguments!", True);
	return;
    }

    if (case_lstrcmp(params[0], "from") == 0) {
	field = KillFieldFrom;
	expr = regexp_escape_string(global.curr_art->from, True);
    } else if (case_lstrcmp(params[0], "subject") == 0) {
	field = KillFieldSubject;
	expr = regexp_escape_string(global.curr_art->subject->subject, True);
    } else if (case_lstrcmp(params[0], "message-id") == 0) {
	field = KillFieldMsgid;
	expr = XtMalloc(global.curr_art->hash_len + 4);
	sprintf(expr, "<%s>", global.curr_art->msgid);
    } else {
	sprintf(buffer, "kill-%s(): unknown field: ",
		append ? "append" : "prepend");
	strncat(buffer, params[0], 40);
	set_message(buffer, True);
	return;
    }

    if (case_lstrcmp(params[0], "article") == 0)
	scope = KillScopeArticle;
    else if (case_lstrcmp(params[1], "thread") == 0)
	scope = KillScopeThread;
    else if (case_lstrcmp(params[1], "subthread") == 0)
	scope = KillScopeSubthread;
    else if (case_lstrcmp(params[1], "subject") == 0)
	scope = KillScopeSubject;
    else {
	sprintf(buffer, "kill-%s(): unknown scope: ",
		append ? "append" : "prepend");
	strncat(buffer, params[1], 40);
	set_message(buffer, True);
	XtFree(expr);
	return;
    }

    file = get_kill_file(global.curr_group);
    hot = no_params == 3;
    if (add_kill_node(file, append, field, scope, hot,
		      hot ? params[2] : NULL, expr, NULL)) {
	sprintf(buffer, "%s %s-scope %s %s entry to kill file.",
		append ? "Appended" : "Prepended",
		scope_str[scope], field_str[field], hot ? "hot" : "kill");
	set_message(buffer, False);
    }
    XtFree(expr);
}

void action_kill_append(Widget    w,
			XEvent   *event,
			String   *params,
			Cardinal *no_params)
{
    add_kill(params, *no_params, True);
}

void action_kill_prepend(Widget    w,
			 XEvent   *event,
			 String   *params,
			 Cardinal *no_params)
{
    add_kill(params, *no_params, False);
}

void action_popup_kill(Widget      w,
		       XEvent     *event,
		       String     *params,
		       Cardinal   *no_params)
{
    GROUP	*group;

    if (global.busy || global.mode == NewsModeDisconnected)
	return;

    if (*no_params != 1)
	group = NULL;
    else {
	group = find_group(params[0]);
	if (!group) {
	    set_message("No such group!", True);
	    return;
	}
    }

    kill_edit_popup(group);
}
