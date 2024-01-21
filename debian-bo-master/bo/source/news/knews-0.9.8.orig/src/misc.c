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
#include <X11/Shell.h>
#include "actions.h"
#include "cache.h"
#include "codes.h"
#include "connect.h"
#include "misc.h"
#include "newsrc.h"
#include "partial.h"
#include "procs.h"
#include "read.h"
#include "resource.h"
#include "save.h"
#include "search.h"
#include "server.h"
#include "tag.h"
#include "thread.h"
#include "util.h"
#include "uudecode.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/ArtText.h"
#include "../Widgets/ArtTree.h"
#include "../Widgets/Dialogue.h"
#include "../Widgets/Menu.h"
#include "../Widgets/MenuShell.h"
#include "../Widgets/Message.h"
#include "../Widgets/Notice.h"
#include "../Widgets/PullRight.h"
#include "../Widgets/ScrBar.h"
#include "../Widgets/ScrList.h"
#include "../Widgets/Scrollable.h"
#include "../Widgets/SeparatorG.h"
#include "../Widgets/StringG.h"
#include "../Widgets/ToggleG.h"
#include "../Widgets/Util.h"

#undef  EXTRA_MENU
#define EXTRA_MENU 0

typedef enum {
    MarkScopeArticle,
    MarkScopeSubject,
    MarkScopeThread,
    MarkScopeSubthread,
    MarkScopeTagged,
    MarkScopeAllarticles,
    MarkScopeKilled,
    MarkScopeToCurrent,
    MarkScopeNonTagged,
    MarkScopeCold
} MarkScope;

static Widget misc_menu1;
static Widget misc_menu2;
static Widget head_toggle;
static Widget keep_thr_toggle;
static Widget subs_unsubs;
static Widget ask_how_many;
static Widget msgid_lookup_dialogue;

static void msgid_dialogue_callback(Widget, XtPointer, XtPointer);

void update_misc_menu(NewsMode mode)
{
    int	subscr;

    switch (mode) {
    case NewsModeDisconnected:
	break;
    case NewsModeConnected:
    case NewsModeAllgroups:
    case NewsModeSomegroups:
    case NewsModeNewgroups:
	ToggleGadgetSet(ask_how_many, res_ask_how_many());
	break;
    case NewsModeGroup:
    case NewsModeThread:
	if (global.curr_group)
	    subscr = global.curr_group->subscribed;
	else
	    subscr = False;

	ToggleGadgetSet(head_toggle, res_full_header());
	ToggleGadgetSet(keep_thr_toggle, res_keep_thread_info(subscr));

	if (global.curr_group) {
	    Arg	arg;

	    arg.name = XtNlabel;
	    if (global.curr_group->subscribed)
		arg.value = (XtArgVal)"Unsubscribe";
	    else
		arg.value = (XtArgVal)"Subscribe";
	    XtSetValues(subs_unsubs, &arg, 1);
	}
	break;
    }
}

static void toggle_subs_callback(Widget w,
				 XtPointer client_data,
				 XtPointer call_data)
{
    int	temp;

    if (global.busy || !global.curr_group ||
	(global.mode != NewsModeGroup && global.mode != NewsModeThread))
	return;

    temp = global.curr_group->subscribed;
    global.curr_group->subscribed = !temp;
    sort_groups();
    if (temp)
	set_message("Unsubscribed.", False);
    else
	set_message("Subscribed.",   False);
    update_misc_menu(global.mode);
}

static void print_callback(Widget w,
			   XtPointer client_data,
			   XtPointer call_data)
{
    String	params[3];
    Cardinal	no_params;

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    if (!global.curr_art) {
	set_message("No selected article!", True);
	return;
    }

    params[0] = global.print_command;
    params[1] = "heb";
    no_params = 2;

    action_pipe(w, NULL, params, &no_params);
}

static void full_header_callback(Widget w,
				 XtPointer client_data,
				 XtPointer call_data)
{
    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode == NewsModeThread))
	return;

    if (global.curr_art)
	read_article(global.curr_art, True, NULL, NULL);
    else
	set_message("No selected article!", True);
}

static void rot13_callback(Widget w,
			   XtPointer client_data,
			   XtPointer call_data)
{
    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    ArtTextRot13(main_widgets.text);
}

static void clear_tags_callback(Widget w,
				XtPointer client_data,
				XtPointer call_data)
{
    if (global.busy ||(global.mode != NewsModeGroup &&
		       global.mode != NewsModeThread))
	return;

    clear_tagged_articles();
    set_message("Cleared tagged articles.", False);
}

static void tag_hot_callback(Widget w,
			     XtPointer client_data,
			     XtPointer call_data)
{
    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    tag_hot_articles();
    set_standard_message();
}

static void show_cache_callback(Widget w,
				XtPointer client_data,
				XtPointer call_data)
{
    popup_cache_stats();
}

static void toggle_head_callback(Widget w,
				 XtPointer client_data,
				 XtPointer call_data)
{
    if (global.busy || !call_data || (global.mode != NewsModeGroup &&
				      global.mode != NewsModeThread))
	return;

    res_set_full_header(*(Boolean *)call_data);
}

static void keep_thr_callback(Widget w,
			      XtPointer client_data,
			      XtPointer call_data)
{
    if (global.busy || !call_data || (global.mode != NewsModeGroup
				      && global.mode == NewsModeThread))
	return;

    res_set_keep_thread_info(*(Boolean *)call_data);
}

static void mark_read_callback(Widget w,
			       XtPointer client_data,
			       XtPointer call_data)
{
    MarkScope	scope = (MarkScope)client_data;
    ARTICLE	*art = global.curr_art;
    ARTICLE	**arts;
    SUBJECT	*subj;
    long	n;
    int		xref = res_process_xrefs();

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    if (art)
	subj = art->subject;
    else
	subj = global.curr_subj;

    set_busy(False);

    switch (scope) {
    case MarkScopeArticle:
	if (!art)
	    set_message("No selected article!", True);
	else if (!art->from)
	    set_message("That's a fake article!", True);
	else {
	    if (!art->read) {
		art->read = True;
		subj->no_unread--;
		global.curr_group->no_unread--;
		if (xref)
		    process_xref(art);
		if (art->pixmap != None) {
		    global.n_hot--;
		    update_subj_hot_value(subj);
		}
		update_subj_entry(subj);
		if (global.mode == NewsModeThread) {
		    ArtTreeNodeSetInner(main_widgets.arttree,
					(ART_TREE_NODE *)art, False);
		    ArtTreeNodeSetPixmap(main_widgets.arttree,
					 (ART_TREE_NODE *)art, None);
		}
	    }
	    set_standard_message();
	}
	break;
    case MarkScopeSubject:
	if (!subj)
	    set_message("No selected subject!", True);
	else {
	    mark_subject_read(subj, xref, False);
	    update_subj_hot_value(subj);
	    update_subj_entry(subj);
	    if (global.mode == NewsModeThread)
		setNewsModeThread();
	    set_standard_message();
	}
	break;
    case MarkScopeThread:
	if (!subj)
	    set_message("No selected subject!", True);
	else {
	    art = subj->thread;

	    while (subj->prev && subj->prev->thread == art)
		subj = subj->prev;
	    while (subj && subj->thread == art) {
		mark_subject_read(subj, xref, False);
		update_subj_hot_value(subj);
		update_subj_entry(subj);
		subj = subj->next;
	    }
	    if (global.mode == NewsModeThread)
		setNewsModeThread();
	    set_standard_message();
	}
	break;
    case MarkScopeSubthread:
	if (!art) 
	    set_message("No selected article!", True);
	else if (!art->from)
	    set_message("That's a fake article!", True);
	else {
	    SUBJECT	*loop = subj;

	    while (loop->prev && loop->prev->thread == subj->thread)
		loop = loop->prev;
	    while (loop && loop->thread == subj->thread) {
		mark_sub_subject_read(art, loop, xref, False);
		update_subj_hot_value(loop);
		update_subj_entry(loop);
		loop = loop->next;
	    }
	    if (global.mode == NewsModeThread)
		setNewsModeThread();
	    set_standard_message();
	}
	break;
    case MarkScopeTagged:
	n = no_tagged_articles();
	if (n <= 0)
	    set_message("No tagged articles!", True);
	else {
	    long	i;

	    arts = get_tagged_articles();
	    for (i = 0 ; i < n ; i++) if (arts[i]->from && !arts[i]->read) {
		arts[i]->read = True;
		arts[i]->subject->no_unread--;
		global.curr_group->no_unread--;
		if (xref)
		    process_xref(arts[i]);
		if (arts[i]->pixmap != None) {
		    global.n_hot--;
		    update_subj_hot_value(arts[i]->subject);
		}
		update_subj_entry(arts[i]->subject);
		if (global.mode == NewsModeThread) {
		    ArtTreeNodeSetInner(main_widgets.arttree,
					(ART_TREE_NODE *)arts[i], False);
		    ArtTreeNodeSetPixmap(main_widgets.arttree,
					 (ART_TREE_NODE *)arts[i], None);
		}
	    }

	    clear_tagged_articles();
	    set_standard_message();
	}
	break;
    case MarkScopeAllarticles:
	for (art = get_articles(main_thr) ; art ; art = art->next) {
	    if (xref)
		process_xref(art);
	    art->read = True;
	}

	for (subj = get_subjects(main_thr) ; subj ; subj = subj->next) {
	    subj->no_unread = 0;
	    subj->pixmap = None;
	}

	global.curr_group->no_unread = 0;
	global.curr_subj = NULL;
	global.curr_art = NULL;
	global.n_hot = 0;

	global.mode = NewsModeGroup; /* in case we're in NewsModeThread */
	setNewsModeGroup(False);

	set_standard_message();
	break;
    case MarkScopeKilled: /*notreached*/
	break;
    case MarkScopeToCurrent:
	if (!subj)
	    set_message("No selected subject!", True);
	else {
	    SUBJECT	*loop;

	    art = subj->thread;
	    while (subj && subj->thread == art)
		subj = subj->next;

	    for (loop = get_subjects(main_thr) ;
		 loop && loop != subj ;
		 loop = loop->next) {
		mark_subject_read(loop, xref, False);
		update_subj_hot_value(loop);
		update_subj_entry(loop);
	    }

	    if (global.mode == NewsModeThread)
		setNewsModeThread();
	    set_standard_message();
	}
	break;
    case MarkScopeNonTagged:
	n = no_tagged_articles();
	arts = get_tagged_articles();

	for (subj = get_subjects(main_thr) ; subj ; subj = subj->next) {
	    if (!subj->has_tagged)
		mark_subject_read(subj, xref, False);
	    else {
		long	no = 0;

		for (art = subj->thread ; art ;
		     art = next_in_thread_preorder(art)) {
		    long	i;

		    if (!art->from || art->subject != subj || art->read)
			continue;
		    for (i = 0 ; i < n ; i++)
			if (arts[i] == art)
			    break;

		    if (i == n) {
			art->read = True;
			if (art->pixmap != None)
			    global.n_hot--;
			no++;
		    }
		}
		subj->no_unread -= no;
		global.curr_group->no_unread -= no;
	    }
	    update_subj_hot_value(subj);
	    update_subj_entry(subj);
	}

	if (global.mode == NewsModeThread)
	    setNewsModeThread();
	set_standard_message();
	break;
    case MarkScopeCold:
	n = 0;
	for (art = get_articles(main_thr) ; art ; art = art->next)
	    if (art->from && !art->read && art->pixmap == None) {
		art->read = True;
		art->subject->no_unread--;
		n++;
	    }

	global.curr_group->no_unread -= n;

	for (subj = get_subjects(main_thr) ; subj ; subj = subj->next) {
	    update_subj_hot_value(subj);
	    update_subj_entry(subj);
	}

	if (global.mode == NewsModeThread)
	    setNewsModeThread();
	set_standard_message();
	break;
    }

    unset_busy();
}

static void mark_unread_callback(Widget w,
				 XtPointer client_data,
				 XtPointer call_data)
{
    MarkScope	scope = (MarkScope)client_data;
    ARTICLE	*art = global.curr_art;
    SUBJECT	*subj = global.curr_subj;
    long	n;

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    if (art)
	subj = art->subject;

    set_busy(False);

    switch (scope) {
    case MarkScopeArticle:
	if (!art)
	    set_message("No selected article!", True);
	else if (!art->from)
	    set_message("That's a fake article!", True);
	else {
	    if (art->read) {
		art->read = False;
		subj->no_unread++;
		global.curr_group->no_unread++;
		if (art->pixmap != None) {
		    global.n_hot++;
		    update_subj_hot_value(subj);
		}
		update_subj_entry(subj);
		if (global.mode == NewsModeThread) {
		    ArtTreeNodeSetInner(main_widgets.arttree,
					(ART_TREE_NODE *)art, True);
		    if (art->pixmap != None)
			ArtTreeNodeSetPixmap(main_widgets.arttree,
					     (ART_TREE_NODE *)art,
					     art->pixmap);
		}
	    }
	    set_standard_message();
	}
	break;
    case MarkScopeSubject:
	if (!subj)
	    set_message("No selected subject!", True);
	else {
	    mark_subject_unread(subj);
	    update_subj_hot_value(subj);
	    update_subj_entry(subj);
	    if (global.mode == NewsModeThread)
		setNewsModeThread();
	    set_standard_message();
	}
	break;
    case MarkScopeThread:
	if (!subj)
	    set_message("No selected subject!", True);
	else {
	    art = subj->thread;

	    while (subj->prev && subj->prev->thread == art)
		subj = subj->prev;
	    while (subj && subj->thread == art) {
		mark_subject_unread(subj);
		update_subj_hot_value(subj);
		update_subj_entry(subj);
		subj = subj->next;
	    }
	    if (global.mode == NewsModeThread)
		setNewsModeThread();
	    set_standard_message();
	}
	break;
    case MarkScopeSubthread:
	if (!art)
	    set_message("No selected article!", True);
	else if (!art->from)
	    set_message("That's a fake article!", True);
	else {
	    SUBJECT	*loop = subj;

	    while (loop->prev && loop->prev->thread == subj->thread)
		loop = loop->prev;
	    while (loop && loop->thread == subj->thread) {
		mark_sub_subject_unread(art, loop);
		update_subj_hot_value(loop);
		update_subj_entry(loop);
		loop = loop->next;
	    }
	    if (global.mode == NewsModeThread)
		setNewsModeThread();
	    set_standard_message();
	}
	break;
    case MarkScopeTagged:
	n = no_tagged_articles();
	if (n <= 0)
	    set_message("No tagged articles!", True);
	else {
	    ARTICLE	**arts = get_tagged_articles();
	    long	i;

	    for (i = 0 ; i < n ; i++) if (arts[i]->from && arts[i]->read) {
		arts[i]->read = False;
		arts[i]->subject->no_unread++;
		global.curr_group->no_unread++;
		if (arts[i]->pixmap != None) {
		    global.n_hot++;
		    update_subj_hot_value(arts[i]->subject);
		}
		update_subj_entry(arts[i]->subject);
		if (global.mode == NewsModeThread) {
		    ArtTreeNodeSetInner(main_widgets.arttree,
					(ART_TREE_NODE *)arts[i], True);
		    if (arts[i]->pixmap != None)
			ArtTreeNodeSetPixmap(main_widgets.arttree,
					     (ART_TREE_NODE *)arts[i],
					     arts[i]->pixmap);
		}
	    }

	    clear_tagged_articles();
	    set_standard_message();
	}
	break;
    case MarkScopeAllarticles:
	for (subj = get_subjects(main_thr) ; subj ; subj = subj->next) {
	    subj->no_unread = 0;
	    subj->pixmap = 0;
	}
	global.n_hot = 0;
	for (art = get_articles(main_thr) ; art ; art = art->next)
	    art->read = True;

	for (n = 0, subj = get_subjects(main_thr) ; subj ; subj = subj->next) {
	    if (subj->prev && subj->prev->thread == subj->thread)
		continue;
	    for (art = subj->thread ; art ; art = next_in_thread_wrap(art)) {
		if (art->from && art->read) {
		    art->read = False;
		    art->subject->no_unread++;
		    n++;
		    if (art->pixmap != None) {
			global.n_hot++;
			if (art->subject->pixmap == None)
			    art->subject->pixmap = art->pixmap;
		    }
		}
	    }
	}

	global.curr_group->no_unread = n;
	global.curr_subj = NULL;
	global.curr_art = NULL;

	global.mode = NewsModeGroup; /* in case we're in NewsModeThread */
	setNewsModeGroup(False);

	set_standard_message();
	break;
    case MarkScopeKilled:
	for (n = 0, art = get_articles(main_thr) ; art ; art = art->next) {
	    if (art->read && art->killed && art->from) {
		art->read = False;
		if (art->pixmap != None)
		    global.n_hot++;  /* notreached? */
		art->subject->no_unread++;
		n++;
	    }
	}

	global.curr_group->no_unread += n;
	global.curr_subj = NULL;
	global.curr_art = NULL;

	global.mode = NewsModeGroup; /* in case we're in NewsModeThread */
	setNewsModeGroup(False);

	set_standard_message();
	break;
    case MarkScopeToCurrent: /* not reached */
    case MarkScopeNonTagged:
    case MarkScopeCold:
	break;
    }

    unset_busy();
}

static void unsubscribe_callback(Widget w,
				 XtPointer client_data,
				 XtPointer call_data)
{
    action_unsubscribe(w, NULL, NULL, NULL);
}

static void catchup_callback(Widget w,
			     XtPointer client_data,
			     XtPointer call_data)
{
    action_catchup(w, NULL, NULL, NULL);
}

static void msgid_lookup_callback(Widget w,
				  XtPointer client_data,
				  XtPointer call_data)
{
    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    if (!msgid_lookup_dialogue)
	msgid_lookup_dialogue =
	    popup_dialogue("msgidlookup", "Find article by Message-ID",
			   "Find", "Clear", "Close",
			   msgid_dialogue_callback, NULL, XtGrabNone);
    else if (!is_popped_up(msgid_lookup_dialogue))
	XtPopup(msgid_lookup_dialogue, XtGrabNone);
}

static void add_to_partial_callback(Widget w,
				    XtPointer client_data,
				    XtPointer call_data)
{
    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    partial_build_cache();
}

static void findgroup_callback(Widget w,
			       XtPointer client_data,
			       XtPointer call_data)
{
    if (!w)
	return;

    popup_find_group();
}

static void ask_how_many_callback(Widget w,
				  XtPointer client_data,
				  XtPointer call_data)
{
    Boolean	*set = (Boolean *)call_data;

    if (global.busy || (global.mode != NewsModeConnected &&
			global.mode != NewsModeNewgroups &&
			global.mode != NewsModeAllgroups))
	return;

    if (set)
	res_set_ask_how_many(*set);
}

#if EXTRA_MENU
static void extra_menu_callback(Widget w,
				XtPointer client_data,
				XtPointer call_data)
{
    char	*action = StringGadgetCommand(w);

    if (!action) {
	set_message("No action specified for that menu entry!", True);
	return;
    }

    popup_title_notice("extra", "Not yet implemented", 3);
}
#endif

void create_misc_menu1(Widget parent)
{
    Widget	menu, subshell, submenu;
    Widget	temp;
    Arg		args[4];

    XtSetArg(args[0], XtNcolormap, global.cmap);
    XtSetArg(args[1], XtNvisual, global.visual);
    XtSetArg(args[2], XtNdepth, global.depth);
    misc_menu1 =
	XtCreatePopupShell("miscshell1", menuShellWidgetClass,
			   parent, args, 3);
    menu = XtCreateManagedWidget("miscmenu1", menuWidgetClass,
				 misc_menu1, NULL, 0);

#if EXTRA_MENU
    if (global.extra_menu_size > 0) {
	XtSetArg(args[0], XtNmenuName, "extrashell");
	temp = MenuCreateGadget("extra", pullRightGadgetClass, menu, args, 1);
	create_simple_menu(parent, "extra", global.extra_menu_size,
			   extra_menu_callback, NULL);
	MenuCreateGadget("separator", separatorGadgetClass, menu, NULL, 0);
    }
#endif

    temp = MenuCreateGadget("fullheader", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback, full_header_callback, NULL);
    temp = MenuCreateGadget("rot13", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback, rot13_callback, NULL);
    temp = MenuCreateGadget("cleartags", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback, clear_tags_callback, NULL);
    temp = MenuCreateGadget("uudecode", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback, uudecode_callback, NULL);
    if (global.print_command && global.print_command != '\0') {
	temp = MenuCreateGadget("print", stringGadgetClass, menu, NULL, 0);
	XtAddCallback(temp, XtNcallback, print_callback, NULL);
    }
    XtSetArg(args[0], XtNlabel, "Unsubscribe");
    subs_unsubs = MenuCreateGadget("unsubs", stringGadgetClass, menu, args, 1);
    XtAddCallback(subs_unsubs, XtNcallback, toggle_subs_callback, NULL);
    temp = MenuCreateGadget("taghot", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback, tag_hot_callback, NULL);
    temp = MenuCreateGadget("addtompcache", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback, add_to_partial_callback, NULL);
    temp = MenuCreateGadget("msgidlookup", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback, msgid_lookup_callback, NULL);
    temp = MenuCreateGadget("showcache", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback, show_cache_callback, NULL);
    MenuCreateGadget("separator", separatorGadgetClass, menu, NULL, 0);
    XtSetArg(args[0], XtNmenuName, "markreadshell");
    MenuCreateGadget("markread", pullRightGadgetClass, menu, args, 1);
    XtSetArg(args[0], XtNmenuName, "markunreadshell");
    MenuCreateGadget("markunread", pullRightGadgetClass, menu, args, 1);
    MenuCreateGadget("separator", separatorGadgetClass, menu, NULL, 0);
    head_toggle =
	MenuCreateGadget("headertoggle", toggleGadgetClass, menu, NULL, 0);
    XtAddCallback(head_toggle, XtNcallback, toggle_head_callback, NULL);
    keep_thr_toggle =
	MenuCreateGadget("thrinfo", toggleGadgetClass, menu, NULL, 0);
    XtAddCallback(keep_thr_toggle, XtNcallback, keep_thr_callback, NULL);

    XtSetArg(args[0], XtNcolormap, global.cmap);
    XtSetArg(args[1], XtNvisual, global.visual);
    XtSetArg(args[2], XtNdepth, global.depth);
    subshell =
	XtCreatePopupShell("markreadshell", menuShellWidgetClass,
			   parent, args, 3);
    submenu =
	XtCreateManagedWidget("markreadmenu", menuWidgetClass,
			      subshell, NULL, 0);

    temp = MenuCreateGadget("article", stringGadgetClass, submenu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  mark_read_callback, (XtPointer)MarkScopeArticle);
    temp = MenuCreateGadget("subject", stringGadgetClass, submenu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  mark_read_callback, (XtPointer)MarkScopeSubject);
    temp = MenuCreateGadget("thread", stringGadgetClass, submenu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  mark_read_callback, (XtPointer)MarkScopeThread);
    temp = MenuCreateGadget("subthread", stringGadgetClass, submenu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  mark_read_callback, (XtPointer)MarkScopeSubthread);
    temp = MenuCreateGadget("tagged", stringGadgetClass, submenu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  mark_read_callback, (XtPointer)MarkScopeTagged);
    temp =
	MenuCreateGadget("allarticles", stringGadgetClass, submenu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  mark_read_callback, (XtPointer)MarkScopeAllarticles);
    temp = MenuCreateGadget("tocurrent", stringGadgetClass, submenu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  mark_read_callback, (XtPointer)MarkScopeToCurrent);
    temp = MenuCreateGadget("nontagged", stringGadgetClass, submenu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  mark_read_callback, (XtPointer)MarkScopeNonTagged);
    temp = MenuCreateGadget("cold", stringGadgetClass, submenu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  mark_read_callback, (XtPointer)MarkScopeCold);

    XtSetArg(args[0], XtNcolormap, global.cmap);
    XtSetArg(args[1], XtNvisual, global.visual);
    XtSetArg(args[2], XtNdepth, global.depth);
    subshell =
	XtCreatePopupShell("markunreadshell", menuShellWidgetClass,
			   parent, args, 3);
    submenu =
	XtCreateManagedWidget("markunreadmenu", menuWidgetClass,
			      subshell, NULL, 0);

    temp = MenuCreateGadget("article", stringGadgetClass, submenu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  mark_unread_callback, (XtPointer)MarkScopeArticle);
    temp = MenuCreateGadget("subject", stringGadgetClass, submenu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  mark_unread_callback, (XtPointer)MarkScopeSubject);
    temp = MenuCreateGadget("thread", stringGadgetClass, submenu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  mark_unread_callback, (XtPointer)MarkScopeThread);
    temp = MenuCreateGadget("subthread", stringGadgetClass, submenu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  mark_unread_callback, (XtPointer)MarkScopeSubthread);
    temp = MenuCreateGadget("tagged", stringGadgetClass, submenu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  mark_unread_callback, (XtPointer)MarkScopeTagged);
    temp =
	MenuCreateGadget("allarticles", stringGadgetClass, submenu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  mark_unread_callback, (XtPointer)MarkScopeAllarticles);
    temp = MenuCreateGadget("killed", stringGadgetClass, submenu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  mark_unread_callback, (XtPointer)MarkScopeKilled);
}

void create_misc_menu2(Widget parent)
{
    Widget	menu, temp;
    Arg		args[4];

    XtSetArg(args[0], XtNcolormap, global.cmap);
    XtSetArg(args[1], XtNvisual, global.visual);
    XtSetArg(args[2], XtNdepth, global.depth);
    misc_menu2 =
	XtCreatePopupShell("miscshell2", menuShellWidgetClass,
			   parent, args, 3);
    menu = XtCreateManagedWidget("miscmenu2", menuWidgetClass,
				 misc_menu2, NULL, 0);

    temp = MenuCreateGadget("unsubscribe", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback, unsubscribe_callback, NULL);

    temp = MenuCreateGadget("catchup", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback, catchup_callback, NULL);

    temp = MenuCreateGadget("findgroup", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback, findgroup_callback, NULL);

    ask_how_many =
	MenuCreateGadget("askhowmany", toggleGadgetClass, menu, NULL, 0);
    XtAddCallback(ask_how_many, XtNcallback, ask_how_many_callback, NULL);
}

void action_mark_read_article(Widget w, XEvent *event,
			      String *params, Cardinal *no_params)
{
    mark_read_callback(w, (XtPointer)MarkScopeArticle, NULL);
}

void action_mark_read_subject(Widget w, XEvent *event,
			      String *params, Cardinal *no_params)
{
    mark_read_callback(w, (XtPointer)MarkScopeSubject, NULL);
}

void action_mark_read_thread(Widget w, XEvent *event,
			     String *params, Cardinal *no_params)
{
    mark_read_callback(w, (XtPointer)MarkScopeThread, NULL);
}

void action_mark_read_subthread(Widget w, XEvent *event,
				String *params, Cardinal *no_params)
{
    mark_read_callback(w, (XtPointer)MarkScopeSubthread, NULL);
}

void action_mark_read_tagged(Widget w, XEvent *event,
			     String *params, Cardinal *no_params)
{
    mark_read_callback(w, (XtPointer)MarkScopeTagged, NULL);
}

void action_mark_read_all(Widget w, XEvent *event,
			  String *params, Cardinal *no_params)
{
    mark_read_callback(w, (XtPointer)MarkScopeAllarticles, NULL);
}

void action_mark_read_to_current(Widget w, XEvent *event,
				 String *params, Cardinal *no_params)
{
    mark_read_callback(w, (XtPointer)MarkScopeToCurrent, NULL);
}

void action_mark_read_non_tagged(Widget w, XEvent *event,
				 String *params, Cardinal *no_params)
{
    mark_read_callback(w, (XtPointer)MarkScopeNonTagged, NULL);
}

void action_mark_read_cold(Widget w, XEvent *event,
			   String *params, Cardinal *no_params)
{
    mark_read_callback(w, (XtPointer)MarkScopeCold, NULL);
}

void action_mark_unread_article(Widget w, XEvent *event,
				String *params, Cardinal *no_params)
{
    mark_unread_callback(w, (XtPointer)MarkScopeArticle, NULL);
}

void action_mark_unread_subject(Widget w, XEvent *event,
				String *params, Cardinal *no_params)
{
    mark_unread_callback(w, (XtPointer)MarkScopeSubject, NULL);
}

void action_mark_unread_thread(Widget w, XEvent *event,
			       String *params, Cardinal *no_params)
{
    mark_unread_callback(w, (XtPointer)MarkScopeThread, NULL);
}

void action_mark_unread_subthread(Widget w, XEvent *event,
				  String *params, Cardinal *no_params)
{
    mark_unread_callback(w, (XtPointer)MarkScopeSubthread, NULL);
}

void action_mark_unread_tagged(Widget w, XEvent *event,
			       String *params, Cardinal *no_params)
{
    mark_unread_callback(w, (XtPointer)MarkScopeTagged, NULL);
}

void action_mark_unread_all(Widget w, XEvent *event,
			    String *params, Cardinal *no_params)
{
    mark_unread_callback(w, (XtPointer)MarkScopeAllarticles, NULL);
}

void action_mark_unread_killed(Widget w, XEvent *event,
			       String *params, Cardinal *no_params)
{
    mark_unread_callback(w, (XtPointer)MarkScopeKilled, NULL);
}

void action_clear_tagged(Widget w, XEvent *event,
			       String *params, Cardinal *no_params)
{
    clear_tags_callback(w, NULL, NULL);
}

void action_catchup(Widget w, XEvent *event,
		    String *params, Cardinal *no_params)
{
    GROUP	*g;
    long	n;

    if (global.busy)
	return;

    switch (global.mode) {
    case NewsModeDisconnected:
	break;
    case NewsModeConnected:
	set_curr_group();
	g = global.curr_group;
	if (!g) {
	    set_message ("No selected group!", True);
	    break;
	}

	free_read_arts_list(g);
	g->read_arts = NULL;
	if (g->last_art >= g->first_art &&
	    g->last_art > 0) {
	    g->read_arts =
		(ART_LIST_NODE *)XtMalloc(sizeof(ART_LIST_NODE));
	    g->read_arts->first = 1;
	    g->read_arts->last = g->last_art;
	    g->read_arts->next = NULL;
	}
	setNewsModeConnected();
	break;
    case NewsModeGroup:
    case NewsModeThread:
	knapp6_callback(w, NULL, NULL);
	break;
    case NewsModeAllgroups:
	n = ScrListGetFirstSelected(main_widgets.group_list);
	if (n < 0) {
	    set_message("No selected groups.", True);
	    return;
	}

	while (n >= 0) {
	    g = global.groups[n];
	    free_read_arts_list(g);
	    g->read_arts = NULL;
	    if (g->last_art >= g->first_art && g->last_art > 0) {
		g->read_arts =
		    (ART_LIST_NODE *)XtMalloc(sizeof g->read_arts[0]);
		g->read_arts->first = 1;
		g->read_arts->last = g->last_art;
		g->read_arts->next = NULL;
	    }
	    n = ScrListGetNextSelected(main_widgets.group_list, n);
	}

	set_message("Catched up.", False);
	break;
    case NewsModeNewgroups:
    case NewsModeSomegroups:
	break;
    }
}

void action_unsubscribe(Widget w, XEvent *event,
			String *params, Cardinal *no_params)
{
    if (global.busy)
	return;

    switch (global.mode) {
    case NewsModeDisconnected:
	break;
    case NewsModeConnected:
	set_curr_group();
	if (!global.curr_group) {
	    set_message("No selected group!", True);
	    return;
	}

	global.curr_group->subscribed = False;
	sort_groups();
	setNewsModeConnected();
	break;
    case NewsModeGroup:
    case NewsModeThread:
	set_curr_group();
	if (global.curr_group && global.curr_group->subscribed)
	    toggle_subs_callback(w, NULL, NULL);
	break;
    case NewsModeAllgroups:
    case NewsModeNewgroups:
    case NewsModeSomegroups:
	knapp2_callback(w, NULL, NULL);
	break;
    }
}

void action_subscribe(Widget w, XEvent *event,
		      String *params, Cardinal *no_params)
{
    if (global.busy)
	return;

    switch (global.mode) {
    case NewsModeDisconnected:
    case NewsModeConnected:
	break;
    case NewsModeGroup:
    case NewsModeThread:
	if (global.curr_group && !global.curr_group->subscribed)
	    toggle_subs_callback(w, NULL, NULL);
	break;
    case NewsModeAllgroups:
    case NewsModeNewgroups:
    case NewsModeSomegroups:
	knapp1_callback(w, NULL, NULL);
	break;
    }
}

void action_tag_hot(Widget w, XEvent *event,
		    String *params, Cardinal *no_params)
{
    tag_hot_callback(w, NULL, NULL);
}

/*********************************************************************/

static void msgid_dialogue_callback(Widget w, 
				    XtPointer client_data,
				    XtPointer call_data)
{
    char		buffer[512];
    DialogueReport	*report = (DialogueReport *)call_data;
    char		*msgid = report->buffer;
    char		*reply, *c;
    long		n;
    ARTICLE		*art;

    Arg			arg;

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    switch (report->reply) {
    case DialogueReplyRight:
    case DialogueReplyClose:
	popdown_msgid_dialogue();
	break;
    case DialogueReplyMiddle:
	XtSetArg(arg, XtNbuffer, "");
	XtSetValues(w, &arg, 1);
	break;
    case DialogueReplyLeft:
    case DialogueReplyEnter:
    case DialogueReplyTab:
	popdown_msgid_dialogue();
	if (!msgid || msgid[0] == '\0')
	    break;
	if (*msgid == '<')
	    msgid++;

	n = strlen(msgid);
	if (n > 480) {
	    XBell(display, 0);
	    break;
	}

	msgid = memcpy(XtMalloc(n + 1), msgid, n + 1);
	if (msgid[n - 1] == '>')
	    msgid[--n] = '\0';

	c = strchr(msgid, '@');
	if (c)
	    ascii_lower(c);

	art = find_article(msgid, n);
	set_curr_art(art, True);
	if (art) {
            if (read_article(art, False, NULL, NULL) &&
		global.mode == NewsModeThread)
		setNewsModeThread();
	    XtFree(msgid);
	    break;
 	}

	set_busy(True);
	set_message("Asking server for article...", False);
	sprintf(buffer, "ARTICLE <%s>\r\n", msgid);
	XtFree(msgid);
	reply = server_comm(main_server, buffer, True);
	if (reply)
	    if (atoi(reply) == NNTP_OK_ARTICLE) {
		reply = do_mime(NULL, main_server, server_read(main_server),
				False, NULL, 0, NULL);
		set_standard_message();
	    } else {
		if (strlen(reply) > 200)
		    reply[200] = '\0';
		sprintf(buffer, "Error, message from server is: %s", reply);
		set_message(buffer, True);
	    }

	if (!reply) {
	    reconnect_server(True);
	    unset_busy();
	    return;
	}

	unset_busy();
	break;
    }
}

void popdown_msgid_dialogue(void)
{
    if (msgid_lookup_dialogue && is_popped_up(msgid_lookup_dialogue))
	XtPopdown(msgid_lookup_dialogue);
}
