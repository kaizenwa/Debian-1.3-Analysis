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
#include "ahead.h"
#include "cache.h"
#include "codes.h"
#include "connect.h"
#include "ftp.h"
#include "k_file.h"
#include "newsrc.h"
#include "partial.h"
#include "read.h"
#include "resource.h"
#include "save.h"
#include "search.h"
#include "server.h"
#include "tag.h"
#include "thread.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/ArtText.h"
#include "../Widgets/ArtTree.h"
#include "../Widgets/Notice.h"
#include "../Widgets/Sash.h"
#include "../Widgets/ScrList.h"

static int confirm_quit_group(void)
{
    char	*c = global.confirm_quit_group;

    if (!c)
	return False;

    if (case_lstrcmp(c, "true") == 0)
	return True;
    if (case_lstrcmp(c, "tagged") == 0)
	return no_tagged_articles() != 0;
    if (case_lstrcmp(c, "false") == 0)
	return True;

    return True;
}

static void leave_group(int catchup)
{
    set_busy(False);

    popdown_save();
    popdown_search();
    partial_clear_cache();
    clear_tagged_articles();
    clear_history();

    free_read_arts_list(global.curr_group);

    if (!catchup)
	global.curr_group->read_arts = create_read_arts_list();
    else {
	ART_LIST_NODE	*temp;
	ARTICLE		*art;

	temp = global.curr_group->read_arts =
	    (ART_LIST_NODE *)XtMalloc(sizeof *temp);
	temp->next  = NULL;
	temp->first = 1;
	temp->last  = global.curr_group->last_art;

	if (res_process_xrefs())
	    for (art = get_articles(main_thr) ; art ; art = art->next)
		if (!art->read)
		    process_xref(art);
    }

    cache_leave_group();
    thread_ahead_leave_group(global.curr_group);
    kill_exit_group(global.curr_group);
    global.curr_art = NULL;
    global.curr_subj = NULL;
    res_enter_group("none");

    if (catchup || global.curr_group->subscribed)
	setNewsModeConnected();
    else
	setNewsModeAllgroups(NULL);

    unset_busy();

    if (global.mode == NewsModeConnected)
	check_if_rescan_due();
}

static void confirm_callback(Widget    w,
			     XtPointer client_data,
			     XtPointer call_data)
{
    NoticeReply	reply = (NoticeReply)call_data;
    int		catchup = (int)client_data;

    unset_busy();
    XtPopdown(w);
    XtDestroyWidget(w);

    if (reply != NoticeReplyLeft)
	return;

    leave_group(catchup);
}

static void do_confirm(char *msg, int catchup)
{
    set_busy(False);
    popup_notice("confirm", msg, "Yes", "No", NULL, 0,
		 confirm_callback, (XtPointer)catchup, XtGrabExclusive);
}

/* Quit   Done */
void knapp0_callback(Widget gw, XtPointer client_data, XtPointer call_data)
{
    if (global.busy)
	return;

    switch (global.mode) {
    case NewsModeDisconnected: /* Quit */
	disconnect(True);
	break;
    case NewsModeConnected: /* Quit */
	disconnect(True);
	break;
    case NewsModeGroup: /* Done */
	if (confirm_quit_group())
	    do_confirm("Really exit group?", False);
	else
	    leave_group(False);
	break;
    case NewsModeAllgroups: /* Done */
    case NewsModeSomegroups: /* Done */
	popdown_search();
	setNewsModeConnected();
	check_if_rescan_due();
	break;
    case NewsModeNewgroups: /* Done */
	XtFree((char *)global.new_groups);
	global.new_groups = NULL;
	global.no_new_groups = 0;
	sort_groups();
	setNewsModeConnected();
	check_if_rescan_due();
	break;
    case NewsModeThread:
	break;
    }
}

/* Connect   Disconnect   Subscribe   View Thread  Back */
void knapp1_callback(Widget gw, XtPointer client_data, XtPointer call_data)
{
    long	i, j;

    switch (global.mode) {
    case NewsModeConnected: /* Disconnect */
	if (global.busy)
	    return;
	if (!ftp_put())
	    return;
	disconnect(False);
	break;
    case NewsModeDisconnected: /* Connect */
	if (global.busy)
	    return;

	set_standard_message();
	popup_connect_dialogue();
	break;
    case NewsModeThread: /* View Thread */
	setNewsModeGroup(False /* not used */);
	break;
    case NewsModeGroup: /* Back */
	if (global.curr_subj)
	    setNewsModeThread();
	break;
    case NewsModeAllgroups: /* Subscribe */
	if (global.busy)
	    return;

	i = ScrListGetFirstSelected(main_widgets.group_list);
	if (i < 0) {
	    set_message("No selected groups!", True);
	    return;
	}

	set_busy(False);
	do {
	    global.groups[i]->subscribed = True;
	    i = ScrListGetNextSelected(main_widgets.group_list, i);
	} while (i >= 0);
	sort_groups();
	global.curr_group = NULL;
	setNewsModeAllgroups(NULL);
	set_standard_message();
	unset_busy();
	return;
    case NewsModeSomegroups: /* Subscribe */
	if (global.busy)
	    return;

	i = ScrListGetFirstSelected(main_widgets.group_list);
	if (i < 0) {
	    set_message("No selected groups!", True);
	    return;
	}

	set_busy(False);
	j = 0;
	do {
	    for ( ; j < global.no_groups ; j++)
		if (global.groups[j]->disp == i) {
		    global.groups[j]->subscribed = True;
		    break;
		}
	    i = ScrListGetNextSelected(main_widgets.group_list, i);
	} while (i >= 0);
	sort_groups();
	global.curr_group = NULL;
	setNewsModeConnected();
	set_standard_message();
	unset_busy();
	break;
    case NewsModeNewgroups: /* Subscribe */
	if (global.busy)
	    return;

	i = ScrListGetFirstSelected(main_widgets.group_list);
	if (i < 0) {
	    set_message("No selected groups!", True);
	    return;
	}

	set_busy(False);
	do {
	    for (j = 0 ; j < global.no_groups ; j++)
		if (global.groups[j]->disp == i) {
		    global.groups[j]->subscribed = True;
		    break;
		}
	    i = ScrListGetNextSelected(main_widgets.group_list, i);
	} while (i >= 0);
	setNewsModeNewgroups();
	unset_busy();
	break;
    }
}

/* Unsubscribe   All threads   All groups */
void knapp2_callback(Widget gw, XtPointer client_data, XtPointer call_data)
{
    long	i, j;
    SUBJECT	*subj;

    if (global.busy)
	return;

    switch (global.mode) {
    case NewsModeConnected: /* All groups */
	set_busy(False);
	global.curr_group = NULL;
	setNewsModeAllgroups(NULL);
	unset_busy();
	break;
    case NewsModeGroup: /* All threads */
	for (subj = get_subjects(main_thr) ;
	     subj ; subj = subj->next)
	    if (subj->disp < 0)
		break;

	set_curr_art(NULL, False);
	set_curr_subj(NULL);
	setNewsModeGroup(subj != NULL);
	set_standard_message();
	break;
    case NewsModeAllgroups: /* Unsubscribe */
	i = ScrListGetFirstSelected(main_widgets.group_list);

	if (i < 0) {
	    set_message("No selected groups!", True);
	    return;
	}

	set_busy(False);
	do {
	    global.groups[i]->subscribed = False;
	    i = ScrListGetNextSelected(main_widgets.group_list, i);
	} while (i >= 0);
	sort_groups();
	global.curr_group = NULL;
	setNewsModeAllgroups(NULL);
	set_standard_message();
	unset_busy();
	break;
    case NewsModeSomegroups: /* Unsubscribe */
	if (global.busy)
	    return;

	i = ScrListGetFirstSelected(main_widgets.group_list);
	if (i < 0) {
	    set_message("No selected groups!", True);
	    return;
	}

	set_busy(False);
	j = 0;
	do {
	    for ( ; j < global.no_groups ; j++)
		if (global.groups[j]->disp == i) {
		    global.groups[j]->subscribed = True;
		    break;
		}
	    i = ScrListGetNextSelected(main_widgets.group_list, i);
	} while (i >= 0);
	sort_groups();
	global.curr_group = NULL;
	setNewsModeConnected();
	set_standard_message();
	unset_busy();
	break;
    case NewsModeNewgroups: /* Unsubscribe */
	i = ScrListGetFirstSelected(main_widgets.group_list);
	if (i < 0) {
	    set_message("No selected groups!", True);
	    return;
	}

	set_busy(False);
	do {
	    for (j = 0 ; j < global.no_groups ; j++)
		if (global.groups[j]->disp == i) {
		    global.groups[j]->subscribed = False;
		    break;
		}
	    i = ScrListGetNextSelected(main_widgets.group_list, i);
	} while (i >= 0);
	setNewsModeNewgroups();
	unset_busy();
	break;
    case NewsModeDisconnected:
    case NewsModeThread:
	break;
    }
}

/* Kill... */
void knapp5_callback(Widget gw, XtPointer client_data, XtPointer call_data)
{
    switch (global.mode) {
    case NewsModeDisconnected:
	break;
    case NewsModeConnected:
    case NewsModeAllgroups:
    case NewsModeSomegroups:
    case NewsModeNewgroups:
	kill_edit_popup(NULL);
	break;
    case NewsModeGroup:
    case NewsModeThread:
	kill_edit_popup(global.curr_group);
	break;
    }
}

/* Update   Catchup */
void knapp6_callback(Widget gw, XtPointer client_data, XtPointer call_data)
{
    char	*msg;

    if (global.busy)
	return;

    switch (global.mode) {
    case NewsModeDisconnected:
    case NewsModeThread:
	break;
    case NewsModeGroup: /* Catchup */
	if (global.confirm_catchup && global.curr_group->no_unread > 0)
	    do_confirm("Really catchup?", True);
	else
	    leave_group(True);
	break;
    case NewsModeConnected: /* Update */
    case NewsModeSomegroups:
    case NewsModeAllgroups:
    case NewsModeNewgroups:
	set_busy(False);
	msg = do_update();
	set_message(msg ? msg : "Newsrc and kill files updated.", !!msg);
	unset_busy();
	break;
    }
}

/* Read/goto group   Next unread */
void knapp7_callback(Widget gw, XtPointer client_data, XtPointer call_data)
{
    ARTICLE	*art;
    int		new_thread;

    if (global.busy)
	return;

    switch (global.mode) {
    case NewsModeConnected: /* Read group */
	set_curr_group();
	/*
	 * FALL THROUGH
	 */
    case NewsModeAllgroups:
    case NewsModeSomegroups:
	if (!global.curr_group) {
	    set_message("No selected group!", True);
	    return;
	}

	popdown_find_group();
	popdown_search();
	read_group(NULL, True, 0);
	break;
    case NewsModeGroup:  /* Next unread */
    case NewsModeThread:
	art = global.curr_art;
	new_thread = False;

	if (!art) {
	    if (global.curr_subj)
		art = global.curr_subj->thread;
	    new_thread = True;
	}

	while (art) {
	    if (art->from && !art->read)
		break;
	    art = next_in_thread_preorder(art);
	}

	if (!art) {
	    SUBJECT	*subj = NULL;
	    ARTICLE	*stop = NULL;

	    new_thread = True;
	    if (global.curr_subj) {		
		stop = global.curr_subj->thread;
		subj = global.curr_subj->next;
		while (subj && subj->thread == stop)
		    subj = subj->next;
	    }

	    if (!subj) {
		subj = get_subjects(main_thr);
		stop = NULL;
	    }

	    if (subj)
		while (subj->thread != stop) {
		    for (art = subj->thread ; art ;
			 art = next_in_thread_preorder(art))
			if (art->from && !art->read)
			    break;
		    if (art)
			break;

		    do {
			subj = subj->next;
		    } while (subj && subj->prev->thread == subj->thread);
		    if (!subj)
			if (stop)
			    subj = get_subjects(main_thr);
			else
			    break;
		}
	}

	set_curr_art(art, True);
	if (art) {
	    if (global.mode == NewsModeThread && new_thread)
		setNewsModeThread();
	    read_article(art, False, NULL, NULL);
	} else {
	    if (global.mode == NewsModeThread)
		setNewsModeGroup(False);
	    else
		knapp0_callback(NULL, NULL, NULL); /* Done */
	}

	break;
    case NewsModeDisconnected:
    case NewsModeNewgroups:
	break;
    }
}

/*  Rescan   Previous */
void knapp8_callback(Widget gw, XtPointer client_data, XtPointer call_data)
{
    ARTICLE	*art, *old;
    int		new_thread;
    char	*buffer;

    if (global.busy)
	return;

    switch (global.mode) {
    case NewsModeGroup:  /* Previous */
    case NewsModeThread:
	art = history_pop();
	old = global.curr_art;
	if (old && art == old) {
	    art = history_pop();
	    if (!art)
		history_push(old);
	}

	if (!art) {
	    set_message("No previous article!", True);
	    return;
	}

	if (old && old->from && old->read) {
	    old->read = False;
	    old->subject->no_unread++;
	    global.curr_group->no_unread++;
	    if (old->pixmap != None) {
		global.n_hot++;
		update_subj_hot_value(old->subject);
	    }
	    update_subj_entry(old->subject);
	    if (global.mode == NewsModeThread) {
		ArtTreeNodeSetInner(main_widgets.arttree,
				    (ART_TREE_NODE *)old, True);
		if (old->pixmap != None)
		    ArtTreeNodeSetPixmap(main_widgets.arttree,
					 (ART_TREE_NODE *)old,
					 old->pixmap);
	    }
	}

	if (old && old->subject->thread == art->subject->thread)
	    new_thread = False;
	else
	    new_thread = True;

	set_curr_art(art, True);
	if (read_article(art, False, NULL, NULL)) {
	    if (global.mode == NewsModeThread) {
		if (new_thread)
		    setNewsModeThread();
	    } else
		set_curr_art(art, False);
	}
	break;
    case NewsModeConnected: /* Rescan */
	remove_rescan_timeout();

	set_busy(True);
	set_message("Server contacted, waiting for response...", False);
	buffer = rescan();
	if (!buffer) {
	    reconnect_server(True);
	    unset_busy();
	    return;
	}
	unset_busy();

	if (atoi(buffer) == NNTP_OK_GROUPS)
	    setNewsModeConnected();
	else {
	    char	message[128];

	    if (strlen(buffer) > 80) buffer[80] = '\0';
	    sprintf(message, "Error!  Message from server is: %s",
		    buffer);
	    set_message(message, True);
	}
	break;
    case NewsModeDisconnected:
    case NewsModeAllgroups:
    case NewsModeSomegroups:
    case NewsModeNewgroups:
	break;
    }
}

/* Save... */
void knapp10_callback(Widget gw, XtPointer client_data, XtPointer call_data)
{
    switch (global.mode) {
    case NewsModeGroup:
    case NewsModeThread:
	popup_save();
	break;
    case NewsModeDisconnected:
    case NewsModeConnected:
    case NewsModeAllgroups:
    case NewsModeSomegroups:
    case NewsModeNewgroups:
	if (global.bell)
	    XBell(display, 0);
	break;
    }
}

/* Search... */
void knapp11_callback(Widget gw, XtPointer client_data, XtPointer call_data)
{
    switch (global.mode) {
    case NewsModeGroup:
    case NewsModeThread:
    case NewsModeAllgroups:
	popup_search();
	break;
    case NewsModeDisconnected:
    case NewsModeConnected:
    case NewsModeSomegroups:
    case NewsModeNewgroups:
	if (global.bell)
	    XBell(display, 0);
	break;
    }
}

void thread_list_sel_callback(Widget gw,
			      XtPointer client_data,
			      XtPointer call_data)
{
    long	row = (long)call_data;
    SUBJECT	*loop = global.curr_subj;

    if (global.busy || global.mode != NewsModeGroup)
	return;

    if (!loop || loop->disp != row)
	for (loop = get_subjects(main_thr) ; loop ; loop = loop->next)
	    if (loop->disp == row)
		break;

    if (!loop || !global.curr_art ||
	global.curr_art->subject->thread != loop->thread)
	global.curr_art = NULL;

    global.curr_subj = loop;
}

void thread_list_callback(Widget gw,
			  XtPointer client_data,
			  XtPointer call_data)
{
    long	row = (long)call_data;
    SUBJECT	*subj;
    ARTICLE	*art = NULL, *first = NULL;

    if (global.busy || global.mode != NewsModeGroup)
	return;

    if (global.curr_subj && global.curr_subj->disp == row)
	subj = global.curr_subj;
    else {
	subj = get_subjects(main_thr);
	while (subj)
	    if (subj->disp == row)
		break;
	    else
		subj = subj->next;
	if (!subj)
	    return;
	global.curr_subj = subj;
    }

    for (art = subj->thread ; art ; art = next_in_thread_preorder(art))
	if (art->subject == subj && art->from)
	    if (!art->read)
		break;
	    else if (!first)
		first = art;

    if (!art) {
	art = global.curr_art;
	if (!art || art->subject != subj)
	    art = first;
	else
	    while ((art = next_in_thread_preorder(art)))
		if (art->from && art->subject == subj)
		    break;
    }

    global.curr_art = art;
    read_article(art, False, NULL, NULL);

    if (!art && global.bell)
	XBell(display, 0);
}

void arttree_sel_callback(Widget gw,
			  XtPointer client_data,
			  XtPointer call_data)
{
    ARTICLE	*art = (ARTICLE *)call_data;

    if (global.busy || global.mode != NewsModeThread)
	return;

    set_curr_art(art, False);
    read_article(art, False, NULL, NULL);
}

void group_list_callback(Widget gw,
			 XtPointer client_data,
			 XtPointer call_data)
{
    long	row = (long)call_data;
    long	n;

    if (global.busy)
	return;

    global.curr_group = NULL;

    switch (global.mode) {
    case NewsModeDisconnected:
    case NewsModeGroup:
    case NewsModeThread:
    case NewsModeNewgroups:
	break;
    case NewsModeAllgroups:
	popdown_search();
	global.curr_group = global.groups[row];
	break;
    case NewsModeConnected:
    case NewsModeSomegroups:
	for (n = 0 ; n < global.no_groups ; n++)
	    if (global.groups[n]->disp == row) {
		global.curr_group = global.groups[n];
		break;
	    }
	break;
    }

    if (!global.curr_group)
	return;

    read_group(NULL, True, 0);
}

void group_list_sel_callback(Widget w,
			     XtPointer client_data,
			     XtPointer call_data)
{
    long	row, i;

    if (global.busy)
	return;

    switch (global.mode) {
    case NewsModeDisconnected:
    case NewsModeNewgroups:
    case NewsModeGroup:
    case NewsModeThread:
	break;
    case NewsModeConnected:
    case NewsModeSomegroups:
	row = (long)call_data;
	global.curr_group = NULL;
	if (row < 0)
	    break;
	for (i = 0 ; i < global.no_groups ; i++)
	    if (global.groups[i]->disp == row) {
		global.curr_group = global.groups[i];
		return;
	    }
	break;
    case NewsModeAllgroups:
	row = (long)call_data;
	if (row >= 0 && row < global.no_groups)
	    global.curr_group = global.groups[row];
	else
	    global.curr_group = NULL;
	break;
    }
}

void group_list_dnd_callback(Widget w,
			     XtPointer client_data,
			     XtPointer call_data)
{
    long	*index = (long *)call_data;
    GROUP	*temp;
    long	i;

    index[2] = False;
    switch (global.mode) {
    case NewsModeAllgroups:
	if (global.groups[index[0]]->subscribed &&
	    global.groups[index[1]]->subscribed) {
	    index[2] = True;
	    temp = global.groups[index[0]];
	    if (index[0] < index[1])
		for (i = index[0] ; i < index[1] ; i++)
		    global.groups[i] = global.groups[i + 1];
	    else
		for (i = index[0] ; i > index[1] ; i--)
		    global.groups[i] = global.groups[i - 1];
	    global.groups[index[1]] = temp;
	    break;
	}
	/*
	 * FALL THROUGH
	 */
    case NewsModeDisconnected:
    case NewsModeConnected:
    case NewsModeGroup:
    case NewsModeThread:
    case NewsModeSomegroups:
    case NewsModeNewgroups:
	if (global.bell)
	    XBell(display, 0);
	break;
    }
}

void delete_window_callback(Widget w,
			    XtPointer client_data,
			    XtPointer call_data)
{
    if (global.busy)
	return;

    disconnect(True);
}

void sash_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
    int			y, i1, i2, i3;
    unsigned int	mods;
    Window		w1, w2;
    Arg			arg;
    Dimension		height;

    /*
     *  Avoid race condition.
     */

    XQueryPointer(display, XtWindow(w),
		  &w1, &w2, &i1, &i2, &i3, &y, &mods);
    if (y == 0 || mods == 0)
	return;

    XtSetArg(arg, XtNheight, &height);
    XtGetValues(main_widgets.top_layout, &arg, 1);
    y += height;
    if (y <= 0)
	y = 1;
    else if (y > 32767)
	y = 32767;
    XtSetArg(arg, XtNheight, y);
    XtSetValues(main_widgets.top_layout, &arg, 1);
}
