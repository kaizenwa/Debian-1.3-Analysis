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
#include "actions.h"
#include "procs.h"
#include "read.h"
#include "search.h"
#include "server.h"
#include "thread.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/ArtText.h"
#include "../Widgets/Scrollable.h"
#include "../Widgets/ScrList.h"
#include "../Widgets/Util.h"

void action_tree_up(Widget w, XEvent *event,
		    String *params, Cardinal *no_params)
{
    ARTICLE	*art;

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    art = global.curr_art;
    if (!art) {
	if (global.bell)
	    XBell(display, 0);
	return;
    }

    art = prev_in_thread_dont_wrap(art);
    if (art) {
	set_curr_art(art, True);
	if (*no_params != 1 || (!art->from && params[0][0] != 'f'))
	    art = NULL;
	read_article(art, False, NULL, NULL);
    }
}

void action_tree_down(Widget w, XEvent *event,
		      String *params, Cardinal *no_params)
{
    ARTICLE	*art;

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    art = global.curr_art;
    if (!art) {
	if (global.bell)
	    XBell(display, 0);
	return;
    }

    art = next_in_thread_dont_wrap(art);
    if (art) {
	set_curr_art(art, True);
	if (*no_params != 1 || (!art->from && params[0][0] != 'f'))
	    art = NULL;
	read_article(art, False, NULL, NULL);
    }
}

void action_tree_left(Widget w, XEvent *event,
		      String *params, Cardinal *no_params)
{
    ARTICLE	*art;

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    art = global.curr_art;
    if (!art) {
	if (global.bell)
	    XBell(display, 0);
	return;
    }

    art = A_PARENT(art);
    if (art) {
	set_curr_art(art, True);
	if (*no_params != 1 || (!art->from && params[0][0] != 'f'))
	    art = NULL;
	read_article(art, False, NULL, NULL);
    }
}

void action_tree_right(Widget w, XEvent *event,
		       String *params, Cardinal *no_params)
{
    ARTICLE	*art;

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;


    art = global.curr_art;
    if (!art) {
	if (global.bell)
	    XBell(display, 0);
	return;
    }

    art = A_CHILD1(art);
    if (art) {
	set_curr_art(art, True);
	if (*no_params != 1 || (!art->from && params[0][0] != 'f'))
	    art = NULL;
	read_article(art, False, NULL, NULL);
    }
}

void action_tree_down_right(Widget w, XEvent *event,
			    String *params, Cardinal *no_params)
{
    ARTICLE	*art;

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    art = global.curr_art;
    if (!art) {
	if (global.bell)
	    XBell(display, 0);
	return;
    }

    art = A_CHILD1(art);
    if (art) {
	while (A_SIBLING(art))
	    art = A_SIBLING(art);

	set_curr_art(art, True);
	if (*no_params != 1 || (!art->from && params[0][0] != 'f'))
	    art = NULL;
	read_article(art, False, NULL, NULL);
    }
}

static void do_list(long step)
{
    long	sel, n;
    Widget	w;
    SUBJECT	*loop;

    switch (global.mode) {
    case NewsModeGroup:
	w = main_widgets.thread_list;
	global.curr_subj = NULL;
	global.curr_art = NULL;
	break;
    case NewsModeAllgroups:
    case NewsModeConnected:
	w = main_widgets.group_list;
	global.curr_group = NULL;
	break;
    default:
	return;
    }

    sel = ScrListGetFirstSelected(w);
    if (sel < 0)
	sel = 0;
    else {
	ScrListSetSelected(w, sel, False);
	sel += step;
    }
    if (sel < 0)
	sel = 0;
    else {
	n = ScrollableGetVSize(w);
	if (n > 0 && sel >= n)
	    sel = n - 1;
    }
    ScrListMakeVisible(w, sel);
    ScrListSetSelected(w, sel, True);
    sel = ScrListGetFirstSelected(w);
    if (sel < 0)
	return;

    switch (global.mode) {
    case NewsModeConnected:
	for (n = 0 ; n < global.no_groups ; n++)
	    if (!global.groups[n]->subscribed)
		break;
	    else if (global.groups[n]->disp == sel) {
		global.curr_group = global.groups[n];
		break;
	    }
	break;
    case NewsModeGroup:
	for (loop = get_subjects(main_thr) ; loop ; loop = loop->next)
	    if (loop->disp == sel)
		break;
	global.curr_subj = loop;
	global.curr_art = NULL;
	break;
    case NewsModeAllgroups:
	if (sel <= global.no_groups)
	    global.curr_group = global.groups[sel];
	break;
    case NewsModeSomegroups:
	for (n = 0 ; n < global.no_groups ; n++)
	    if (global.groups[n]->disp == sel) {
		global.curr_group = global.groups[n];
		break;
	    }
	break;
    default:
	break;
    }
}

void action_list_up(Widget w, XEvent *event,
		    String *params, Cardinal *no_params)
{
    long	step = 1;

    if (global.busy)
	return;

    if (no_params && *no_params == 1 &&
	params[0][0] >= '0' && params[0][0] <= '9')
	if (!strchr(params[0], '.'))
	    step = atol(params[0]);
	else {
	    step = ScrollableGetVShown(global.mode == NewsModeGroup ?
				       main_widgets.thread_list :
				       main_widgets.group_list);
	    step *= atof(params[0]);
	}

    do_list(-step);
}

void action_list_down(Widget w, XEvent *event,
		      String *params, Cardinal *no_params)
{
    long	step = 1;

    if (global.busy)
	return;

    if (no_params && *no_params == 1 &&
	params[0][0] >= '0' && params[0][0] <= '9')
	if (!strchr(params[0], '.'))
	    step = atol(params[0]);
	else {
	    step = ScrollableGetVShown(global.mode == NewsModeGroup ?
				       main_widgets.thread_list :
				       main_widgets.group_list);
	    step *= atof(params[0]);
	}

    do_list(step);
}

void action_tree_or_list_up(Widget w, XEvent *event,
			    String *params, Cardinal *no_params)
{
    if (global.mode == NewsModeThread)
	action_tree_up(w, event, params, no_params);
    else
	action_list_up(w, NULL, NULL, NULL);
}

void action_tree_or_list_down(Widget w, XEvent *event,
			      String *params, Cardinal *no_params)
{
    if (global.mode == NewsModeThread)
	action_tree_down(w, event, params, no_params);
    else
	action_list_down(w, NULL, NULL, NULL);
}

void action_exit_mode(Widget w, XEvent *event,
		      String *params, Cardinal *no_params)
{
    if (global.busy)
	return;

    switch (global.mode) {
    case NewsModeDisconnected:
    case NewsModeGroup:
    case NewsModeAllgroups:
    case NewsModeSomegroups:
    case NewsModeNewgroups:
	knapp0_callback(w, NULL, NULL);
	break;
    case NewsModeThread:
    case NewsModeConnected:
	knapp1_callback(w, NULL, NULL);
	break;
    }
}

void action_enter_mode(Widget w, XEvent *event,
		       String *params, Cardinal *no_params)
{
    if (global.busy)
	return;

    switch (global.mode) {
    case NewsModeGroup:
	if (global.curr_subj &&
	    (!global.curr_art ||
	     global.curr_art->subject->thread != global.curr_subj->thread)) {
	    ARTICLE	*art = global.curr_subj->thread;

	    set_curr_art(art, False);
	    if (*no_params != 1 || (!art->from && params[0][0] != 'f'))
		art = NULL;
	    read_article(art, False, NULL, NULL);
	}	    
	knapp1_callback(w, NULL, NULL);
	break;
    case NewsModeDisconnected:
	knapp1_callback(w, NULL, NULL);
	break;
    case NewsModeConnected:
    case NewsModeSomegroups:
    case NewsModeAllgroups:
	knapp7_callback(w, NULL, NULL);
	break;
    case NewsModeNewgroups:
    case NewsModeThread:
	break;
    }
}

void action_tree_left_or_exit_mode(Widget w, XEvent *event,
				   String *params, Cardinal *no_params)
{
    if (global.busy)
	return;

    if (global.mode == NewsModeThread &&
	global.curr_art && A_PARENT(global.curr_art))
	action_tree_left(w, event, params, no_params);
    else
	action_exit_mode(w, event, params, no_params);
}

void action_tree_right_or_enter_mode(Widget w, XEvent *event,
				     String *params, Cardinal *no_params)
{
    if (global.busy)
	return;

    if (global.mode == NewsModeThread &&
	global.curr_art && A_CHILD1(global.curr_art))
	action_tree_right(w, event, params, no_params);
    else
	action_enter_mode(w, event, params, no_params);
}

void action_goto_next_hot(Widget w, XEvent *event,
			  String *params, Cardinal *no_params)
{
    ARTICLE	*art;
    SUBJECT	*subj;
    int		new_thread;

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    new_thread = global.curr_subj == NULL;
    art = global.curr_art;
    if (art)
	subj = art->subject;
    else
	subj = global.curr_subj;

    if (!subj) {
	set_message("No thread selected!", True);
	return;
    }

    if (art) {
	while ((art = next_in_thread_preorder(art)))
	    if (art->from && !art->read && art->pixmap != None)
		break;

	if (!art) {
	    while (subj->next && subj->next->thread == subj->thread)
		subj = subj->next;
	    subj = subj->next;
	}
    }

    if (!art) {
	new_thread = True;
	while (subj) {
	    for (art = subj->thread ; art ; art = next_in_thread_preorder(art))
		if (art->from && ! art->read && art->pixmap != None)
		    break;

	    if (art)
		break;

	    while (subj->next && subj->next->thread == subj->thread)
		subj = subj->next;
	    subj = subj->next;
	}
    }

    set_curr_art(art, True);
    if (!art)
	set_message("No more hot articles.", True);
    else {
	if (global.mode == NewsModeThread && new_thread)
	    setNewsModeThread();
	read_article(art, *no_params != 0, NULL, NULL);
    }
}

void action_view_thread(Widget w, XEvent *event,
			String *params, Cardinal *no_params)
{
    if (no_params == 0 || params[0][0] == 't' || params[0][0] == 'T') {
	if (global.mode != NewsModeGroup &&
	    global.mode != NewsModeThread)
	    return;
    } else if (params[0][0] == 'y' || params[0][0] == 'Y') {
	if (global.mode != NewsModeGroup)
	    return;
    } else if (params[0][0] == 'n' || params[0][0] == 'N') {
	if (global.mode != NewsModeThread)
	    return;
    }

    knapp1_callback(w, NULL, NULL);
}

void action_change_size(Widget w, XEvent *event,
			String *params, Cardinal *no_params)
{
    Dimension	height;
    long	y;
    Arg		arg;

    if (*no_params != 1 || (!IS_DIGIT(params[0][0]) && params[0][0] != '-')) {
	if (global.bell)
	    XBell(display, 0);
	return;
    }

    y = atoi(params[0]);
    XtSetArg(arg, XtNheight, &height);
    XtGetValues(main_widgets.top_layout, &arg, 1);

    y += height;
    if (y < 0)
	y = 1;
    else if (y > 32767)
	y = 32767;

    XtSetArg(arg, XtNheight, y);
    XtSetValues(main_widgets.top_layout, &arg, 1);
}

void action_popup_find_group(Widget w, XEvent *event,
			     String *params, Cardinal *no_params)
{
    if (global.busy)
	return;

    popup_find_group();
}

void action_do_the_right_thing(Widget w, XEvent *event,
			       String *params, Cardinal *no_params)
{
    long	n, shown, pos;

    switch (global.mode) {
    case NewsModeDisconnected:
	if (global.busy)
	    return;
	knapp1_callback(w, NULL, NULL);
	break;
    case NewsModeConnected:
	if (global.busy)
	    return;
	n = ScrollableGetVSize(main_widgets.group_list);
	if (n <= 0)  /* no unread groups; rescan */
	    knapp8_callback(w, NULL, NULL);
	else
	    knapp7_callback(w, NULL, NULL);
	break;
    case NewsModeGroup:
    case NewsModeThread:
	n     = ScrollableGetVSize(main_widgets.text);
	shown = ScrollableGetVShown(main_widgets.text);
	pos   = ScrollableGetVPos(main_widgets.text);
	if (global.curr_art && n > 0 && pos + shown < n)
	    ScrollablePage(main_widgets.text,
			   *no_params > 0 ? atof(params[0]) : 0.95);
	else if (!global.busy)
	    knapp7_callback(w, NULL, NULL);
	break;
    case NewsModeAllgroups:
    case NewsModeSomegroups:
	if (global.busy)
	    return;
	knapp7_callback(w, NULL, NULL);
	break;
    case NewsModeNewgroups:
	break;
    }
}
