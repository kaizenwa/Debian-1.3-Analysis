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
#include "ahead.h"
#include "misc.h"
#include "newsrc.h"
#include "p_menu.h"
#include "procs.h"
#include "resource.h"
#include "save.h"
#include "tag.h"
#include "thread.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/ArtText.h"
#include "../Widgets/ArtTree.h"
#include "../Widgets/Knapp.h"
#include "../Widgets/Layout.h"
#include "../Widgets/Manager.h"
#include "../Widgets/MenuKnapp.h"
#include "../Widgets/Message.h"
#include "../Widgets/Sash.h"
#include "../Widgets/Scrollable.h"
#include "../Widgets/ScrBar.h"
#include "../Widgets/ScrList.h"
#include "../Widgets/Util.h"

#include "sysdeps.h"

#if HAVE_XMU && XtSpecificationRelease > 4
#include <X11/Xmu/Editres.h>
#endif

static Widget	top_vbar, top_hbar, message_line;
static Widget	text_hbar, text_vbar;

void set_message(char *message, int beep)
{
    MessageSetAndRedraw(message_line, message, beep && global.bell);
}

#define DESCRIPTION(desc)	((desc) ? (desc) : "")
#define ARTTREE_STEPSIZE	16
#define LINE_LEN		256

static int	group_name_len	= 1;
static int	subject_len	= 1;

struct main_widgets main_widgets;

static void print_group_info_subscr(char *buffer, int len, GROUP *group)
{
    int	n;

    *buffer++ = group->ahead_flag ? thread_ahead_char(group) : ' ';
    *buffer++ = ' ';
    len -= 2;

    n = strlen(group->name);
    if (n >= group_name_len)
	n = group_name_len;
    memcpy(buffer, group->name, n);
    if (n < group_name_len)
	memset(buffer + n, ' ', group_name_len - n);
    buffer += group_name_len;
    len -= group_name_len;

    sprintf(buffer, "%6ld", group->no_unread);
    n = strlen(buffer);
    buffer += n;
    *buffer++ = ' ';
    len -= n + 1;

    if (group->description) {
	n = strlen(group->description);
	if (n > len - 2)
	    n = len - 2;
	memcpy(buffer, group->description, n);
	buffer += n;
    }

    *buffer = '\0';
}

static void print_group_info_all(char *buffer, int len, GROUP *group)
{
    long	num;
    int		n;

    num = group->last_art - group->first_art + 1;
    if (num < 0)
	num = 0;

    memcpy(buffer, group->subscribed ? "[s] " : "    ", 4);
    buffer += 4;
    *buffer++ = group->ahead_flag ? thread_ahead_char(group) : ' ';
    *buffer++ = ' ';
    len -= 6;

    n = strlen(group->name);
    if (n >= group_name_len)
	n = group_name_len;
    memcpy(buffer, group->name, n);
    if (n < group_name_len)
	memset(buffer + n, ' ', group_name_len - n);
    buffer += group_name_len;
    len -= group_name_len;

    sprintf(buffer, "%6ld", num);
    n = strlen(buffer);
    buffer += n;
    *buffer++ = ' ';
    len -= n + 1;

    if (group->description) {
	n = strlen(group->description);
	if (n > len - 2)
	    n = len - 2;
	memcpy(buffer, group->description, n);
	buffer += n;
    }

    *buffer = '\0';
}

static void print_subj_info(char *buffer, int len, SUBJECT *subj)
{
    int		indent = False;
    SUBJECT	*loop = subj->prev;
    ARTICLE	*first = first_unread_article_with_subject(subj);
    int		n, i;

    if (subj->no_unread == 0)
	strcpy(buffer, "   ");
    else
	sprintf(buffer, "%3ld", subj->no_unread);
    n = strlen(buffer);
    buffer += n;
    len -= n;
    *buffer++ = subj->has_tagged ? '*' : ' ';
    *buffer++ = ' ';

    for (loop = subj->prev ;
	 loop && loop->thread == subj->thread ;
	 loop = loop->prev)
	if (loop->disp >= 0) {
	    indent = True;
	    break;
	}

    n = subject_len;
    if (indent) {
	*buffer++ = ' ';
	*buffer++ = ' ';
	len -= 2;
	n -= 2;
    }

    if (first && A_PARENT(first) && A_PARENT(first)->subject == subj) {
	memcpy(buffer, "Re: ", 4);
	buffer += 4;
	len -= 4;
	n -= 4;
    }

    i = strlen(subj->subject);
    if (i >= n)
	i = n;
    memcpy(buffer, subj->subject, i);
    if (i < n)
	memset(buffer + i, ' ', n - i);
    buffer += n;
    *buffer++ = ' ';
    len -= subject_len + 1;

    if (first && first->tree_data.label) {
	char	*author = first->tree_data.label;

	n = strlen(author);
	if (n > len - 2)
	    n = len - 2;
	memcpy(buffer, author, n);
	buffer += n;
    }

    *buffer = '\0';
}

static void knapp_set_mode(NewsMode mode)
{
    Arg	arg;

    switch (mode) {
    case NewsModeDisconnected:
	XtSetMappedWhenManaged(main_widgets.knapp[2], False);
	XtSetMappedWhenManaged(main_widgets.knapp[6], False);
	XtSetMappedWhenManaged(main_widgets.knapp[7], False);
	XtSetMappedWhenManaged(main_widgets.knapp[8], False);

	KnappSetLabelNo(main_widgets.knapp[0], 0, True);
	KnappSetLabelNo(main_widgets.knapp[1], 0, True);

	KnappSetSensitive(main_widgets.knapp[3], False);
	KnappSetSensitive(main_widgets.knapp[5], False);
	KnappSetSensitive(main_widgets.knapp[10], False);
	KnappSetSensitive(main_widgets.knapp[11], False);

	XtSetArg(arg, XtNmenuName, "miscshell2");
	break;
    case NewsModeConnected:
	XtSetMappedWhenManaged(main_widgets.knapp[2], True);
	XtSetMappedWhenManaged(main_widgets.knapp[6], True);
	XtSetMappedWhenManaged(main_widgets.knapp[7], True);
	XtSetMappedWhenManaged(main_widgets.knapp[8], True);

	KnappSetLabelNo(main_widgets.knapp[0], 0, True);
	KnappSetLabelNo(main_widgets.knapp[1], 1, True);
	KnappSetLabelNo(main_widgets.knapp[2], 0, True);
	KnappSetLabelNo(main_widgets.knapp[6], 0, True);
	KnappSetLabelNo(main_widgets.knapp[7], 0, True);
	KnappSetLabelNo(main_widgets.knapp[8], 0, True);

	KnappSetSensitive(main_widgets.knapp[3], True);
	KnappSetSensitive(main_widgets.knapp[5], True);
	KnappSetSensitive(main_widgets.knapp[10], False);
	KnappSetSensitive(main_widgets.knapp[11], False);

	XtSetArg(arg, XtNmenuName, "miscshell2");
	break;
    case NewsModeGroup:
	XtSetMappedWhenManaged(main_widgets.knapp[2], True);
	XtSetMappedWhenManaged(main_widgets.knapp[6], True);
	XtSetMappedWhenManaged(main_widgets.knapp[7], True);
	XtSetMappedWhenManaged(main_widgets.knapp[8], True);

	KnappSetLabelNo(main_widgets.knapp[0], 1, True);
	KnappSetLabelNo(main_widgets.knapp[1], 2, True);
	KnappSetLabelNo(main_widgets.knapp[2], 1, True);
	KnappSetLabelNo(main_widgets.knapp[6], 1, True);
	KnappSetLabelNo(main_widgets.knapp[7], 1, True);
	KnappSetLabelNo(main_widgets.knapp[8], 1, True);

	KnappSetSensitive(main_widgets.knapp[3], True);
	KnappSetSensitive(main_widgets.knapp[5], True);
	KnappSetSensitive(main_widgets.knapp[10], True);
	KnappSetSensitive(main_widgets.knapp[11], True);

	XtSetArg(arg, XtNmenuName, "miscshell1");
	break;
    case NewsModeThread:
	XtSetMappedWhenManaged(main_widgets.knapp[2], True);
	XtSetMappedWhenManaged(main_widgets.knapp[6], True);
	XtSetMappedWhenManaged(main_widgets.knapp[7], True);
	XtSetMappedWhenManaged(main_widgets.knapp[8], True);

	KnappSetLabelNo(main_widgets.knapp[0], 1, False);
	KnappSetLabelNo(main_widgets.knapp[1], 3, True);
	KnappSetLabelNo(main_widgets.knapp[2], 1, False);
	KnappSetLabelNo(main_widgets.knapp[6], 1, False);
	KnappSetLabelNo(main_widgets.knapp[7], 1, True);
	KnappSetLabelNo(main_widgets.knapp[8], 1, True);

	KnappSetSensitive(main_widgets.knapp[3], True);
	KnappSetSensitive(main_widgets.knapp[5], True);
	KnappSetSensitive(main_widgets.knapp[10], True);
	KnappSetSensitive(main_widgets.knapp[11], True);

	XtSetArg(arg, XtNmenuName, "miscshell1");
	break;
    case NewsModeAllgroups:
    case NewsModeSomegroups:
	XtSetMappedWhenManaged(main_widgets.knapp[2], True);
	XtSetMappedWhenManaged(main_widgets.knapp[6], True);
	XtSetMappedWhenManaged(main_widgets.knapp[7], True);
	XtSetMappedWhenManaged(main_widgets.knapp[8], False);

	KnappSetLabelNo(main_widgets.knapp[0], 1, True);
	KnappSetLabelNo(main_widgets.knapp[1], 4, True);
	KnappSetLabelNo(main_widgets.knapp[2], 2, True);
	KnappSetLabelNo(main_widgets.knapp[6], 0, True);
	KnappSetLabelNo(main_widgets.knapp[7], 2, True);

	KnappSetSensitive(main_widgets.knapp[3], True);
	KnappSetSensitive(main_widgets.knapp[5], True);
	KnappSetSensitive(main_widgets.knapp[10], False);
	KnappSetSensitive(main_widgets.knapp[11], True);

	XtSetArg(arg, XtNmenuName, "miscshell2");
	break;
    case NewsModeNewgroups:
	XtSetMappedWhenManaged(main_widgets.knapp[2], True);
	XtSetMappedWhenManaged(main_widgets.knapp[6], False);
	XtSetMappedWhenManaged(main_widgets.knapp[7], False);
	XtSetMappedWhenManaged(main_widgets.knapp[8], False);

	KnappSetLabelNo(main_widgets.knapp[0], 1, True);
	KnappSetLabelNo(main_widgets.knapp[1], 4, True);
	KnappSetLabelNo(main_widgets.knapp[2], 2, True);

	KnappSetSensitive(main_widgets.knapp[3], False);
	KnappSetSensitive(main_widgets.knapp[5], True);
	KnappSetSensitive(main_widgets.knapp[10], False);
	KnappSetSensitive(main_widgets.knapp[11], False);

	XtSetArg(arg, XtNmenuName, "miscshell2");
	break;
    }

    XtSetValues(main_widgets.knapp[3], &arg, 1);
}

void setNewsModeDisconnected(void)
{
    global.mode = NewsModeDisconnected;
    ArtTextClearLines(main_widgets.text);
    ScrListClearLines(main_widgets.thread_list);
    ScrListClearLines(main_widgets.group_list);
    XtUnmanageChild(main_widgets.arttree);
    XtUnmanageChild(main_widgets.thread_list);
    XtUnmanageChild(main_widgets.group_list);
    knapp_set_mode(NewsModeDisconnected);
    XtSetSensitive(top_vbar, False);
    XtSetSensitive(top_hbar, False);
    ScrBarSetLengthsAndPos(top_hbar, 0, 0, 0);
    ScrBarSetLengthsAndPos(top_vbar, 0, 0, 0);
}

void update_group_entry(GROUP *group)
{
    char	buffer[LINE_LEN];

    switch (global.mode) {
    case NewsModeConnected:
	if (group->disp >= 0) {
	    print_group_info_subscr(buffer, sizeof buffer, group);
	    ScrListSetLine(main_widgets.group_list, group->disp,
			   buffer, None);
	}
	break;
    case NewsModeAllgroups:
    case NewsModeSomegroups:
	if (group->disp >= 0) {
	    print_group_info_all(buffer, sizeof buffer,  group);
	    ScrListSetLine(main_widgets.group_list, group->disp,
			   buffer, None);
	}
	break;
    case NewsModeDisconnected:
    case NewsModeNewgroups:
    case NewsModeGroup:
    case NewsModeThread:
	break;
    }
}

void setNewsModeConnected(void)
{
    Arg		args[4];
    long	n, sel = -1;
    int		flag = False;

    group_name_len = res_group_name_columns();
    if (group_name_len <= 0 || group_name_len > 128)
	group_name_len = 128;

    global.mode = NewsModeConnected;

    XtUnmanageChild(main_widgets.arttree);
    XtUnmanageChild(main_widgets.thread_list);
    ArtTextClearLines(main_widgets.text);
    ScrListClearLines(main_widgets.thread_list);
    ScrListClearLines(main_widgets.group_list);
    XtSetSensitive(top_hbar, True);
    XtSetSensitive(top_vbar, True);
    XtSetArg(args[0], XtNstepSize, 1);
    XtSetArg(args[1], XtNsensitive, True);
    XtSetValues(top_vbar, args, 2);
    ScrBarSetLengthsAndPos(top_hbar, 0, 0, 0);
    ScrBarSetLengthsAndPos(top_vbar, 0, 0, 0);
    knapp_set_mode(NewsModeConnected);
    update_misc_menu(NewsModeConnected);
    XtSetArg(args[0], XtNatMostOne, True);
    XtSetArg(args[1], XtNatLeastOne, True);
    XtSetArg(args[2], XtNallowDnd, False);
    XtSetValues(main_widgets.group_list, args, 3);
    XtManageChild(main_widgets.group_list);

    ScrollableSuspend(main_widgets.group_list);

    for (n = 0 ; n < global.no_groups ; n++) {
	GROUP	*group = global.groups[n];
	char	buffer[LINE_LEN];

	if (!group->subscribed)
	    break;

	calc_no_unread(group);
	if (group == global.curr_group)
	    flag = True;
	if (group->no_unread <= 0)
	    group->disp = -1;
	else {
	    print_group_info_subscr(buffer, sizeof buffer, group);
	    group->disp = ScrListAddLine(main_widgets.group_list,
					 buffer, None);
	    if (flag) {
		sel = group->disp;
		flag = False;
	    }
	}
    }

    ScrollableResume(main_widgets.group_list);

    while (n < global.no_groups)
	global.groups[n++]->disp = -1;

    if (sel < 0) {
	global.curr_group = NULL;
	for (n = 0 ; n < global.no_groups ; n++) {
	    if (!global.groups[n]->subscribed) {
		sel = 0;
		break;
	    } else if (global.groups[n]->disp == 0) {
		global.curr_group = global.groups[n];
		sel = 0;
		break;
	    }
	}
    }

    if (sel >= 0) {
	ScrListSetSelected(main_widgets.group_list, sel, True);
	ScrListMakeVisible(main_widgets.group_list, sel);
    }
    Remanage(main_widgets.top_manager);

    set_standard_message();
}

void update_subj_entry(SUBJECT *subj)
{
    long	row = subj->disp;
    char	buffer[LINE_LEN];

    if (row >= 0) {
	print_subj_info(buffer, sizeof buffer, subj);
	ScrListSetLine(main_widgets.thread_list, row, buffer, subj->pixmap);
    }
}

void setNewsModeGroup(int show_all)
{
    SUBJECT	*loop;
    long	n;
    Arg		args[2];

    XtSetSensitive(top_hbar, True);
    XtSetSensitive(top_vbar, True);
    XtSetArg(args[0], XtNstepSize, 1);
    XtSetValues(top_vbar, args, 1);

    if (global.mode != NewsModeThread) {
	subject_len = res_subject_columns();
	if (subject_len <= 0 || subject_len > 128)
	    subject_len = 128;

	global.mode = NewsModeGroup;
	ScrListClearLines(main_widgets.group_list);
	ScrListClearLines(main_widgets.thread_list);
	XtUnmanageChild(main_widgets.arttree);
	XtUnmanageChild(main_widgets.group_list);
	XtManageChild(main_widgets.thread_list);
	ArtTreeSetTree(main_widgets.arttree, NULL);
	knapp_set_mode(NewsModeGroup);
	update_misc_menu(NewsModeGroup);

	ScrollableSuspend(main_widgets.thread_list);

	for (n = 0, loop = get_subjects(main_thr) ; loop ; loop = loop->next) {
	    char	buffer[LINE_LEN];

	    if (!show_all && loop->no_unread == 0) {
		loop->disp = -1;
		continue;
	    }

	    print_subj_info(buffer, sizeof buffer, loop);
	    loop->disp =
		ScrListAddLine(main_widgets.thread_list, buffer, loop->pixmap);
	    if (n == 0)
		global.curr_subj = loop;

	    n++;
	}

	ScrollableResume(main_widgets.thread_list);
	Remanage(main_widgets.top_manager);
	/* message already set by get_articles */
    } else {
	global.mode = NewsModeGroup;
	XtUnmanageChild(main_widgets.arttree);
	ArtTreeSetTree(main_widgets.arttree, NULL);
	XtManageChild(main_widgets.thread_list);
	ScrollableResume(main_widgets.thread_list);
	set_curr_subj(global.curr_subj);
	knapp_set_mode(NewsModeGroup);

	set_standard_message();
    }
}

void setNewsModeThread(void)
{
    Arg		args[4];

    global.mode = NewsModeThread;
    XtUnmanageChild(main_widgets.group_list);
    XtUnmanageChild(main_widgets.thread_list);
    ArtTreeSetTree(main_widgets.arttree, NULL);
    ScrollableSuspend(main_widgets.arttree);
    XtSetArg(args[0], XtNtree, global.curr_subj->thread);
    XtSetArg(args[1], XtNx, 0);
    XtSetArg(args[2], XtNy, 0);
    XtSetValues(main_widgets.arttree, args, 3);
    set_tree_stuff(global.curr_subj->thread);
    mark_tagged_articles(global.curr_subj->thread);

    XtSetArg(args[0], XtNstepSize, ARTTREE_STEPSIZE);
    XtSetArg(args[1], XtNsensitive, True);
    XtSetValues(top_hbar, args, 2);
    XtSetValues(top_vbar, args, 2);

    if (global.curr_art) {
	ArtTreeNodeMakeVisible(main_widgets.arttree,
			       (ART_TREE_NODE *)global.curr_art);
	ArtTreeNodeSetSelected(main_widgets.arttree,
			       (ART_TREE_NODE *)global.curr_art, True);
    }

    XtManageChild(main_widgets.arttree);
    ScrollableResume(main_widgets.arttree);
    knapp_set_mode(NewsModeThread);
    set_standard_message();
}

void setNewsModeAllgroups(regex_t *re)
{
    Arg		args[8];
    long	n, first = -1;

    group_name_len = res_group_name_columns();
    if (group_name_len <= 0 || group_name_len > 128)
	group_name_len = 128;

    if (re)
	global.mode = NewsModeSomegroups;
    else
	global.mode = NewsModeAllgroups;

    XtSetArg(args[0], XtNnAlloc, global.no_groups + 8);
    XtSetValues(main_widgets.group_list, args, 1);

    XtSetSensitive(top_vbar, True);
    XtSetSensitive(top_hbar, True);

    ScrListClearLines(main_widgets.thread_list);
    ArtTextClearLines(main_widgets.text);
    XtUnmanageChild(main_widgets.arttree);
    XtUnmanageChild(main_widgets.thread_list);
    knapp_set_mode(global.mode);
    update_misc_menu(global.mode);
    ScrListClearLines(main_widgets.group_list);
    XtSetArg(args[0], XtNatMostOne, False);
    XtSetArg(args[1], XtNatLeastOne, False);
    XtSetArg(args[2], XtNallowDnd, re == NULL);
    XtSetValues(main_widgets.group_list, args, 3);
    XtManageChild(main_widgets.group_list);
    XtSetArg(args[0], XtNstepSize, 1);
    XtSetValues(top_vbar, args, 1);
    ScrBarSetLengthsAndPos(top_hbar, 0, 0, 0);

    ScrollableSuspend(main_widgets.group_list);

    for (n = 0 ; n < global.no_groups ; n++) {
	GROUP	*group = global.groups[n];
	char	buffer[LINE_LEN];

	if (re && regexec(re, group->name, 0, NULL, 0) != 0)
	    group->disp = -1;
	else {
	    print_group_info_all(buffer, sizeof buffer, group);
	    group->disp =
		ScrListAddLine(main_widgets.group_list, buffer, None);
	    if (group == global.curr_group)
		first = group->disp;
	}
    }

    ScrollableResume(main_widgets.group_list);
    if (first >= 0) {
	ScrListSetSelected(main_widgets.group_list, first, True);
	ScrListMakeVisible(main_widgets.group_list, first);
    }
    Remanage(main_widgets.top_manager);

    set_standard_message();
}

void setNewsModeNewgroups(void)
{
    Arg		args[8];
    long	n;

    group_name_len = res_group_name_columns();
    if (group_name_len <= 0 || group_name_len > 128)
	group_name_len = 128;

    global.mode = NewsModeNewgroups;

    for (n = 0 ; n < global.no_groups ; n++) {
	global.groups[n]->disp = -1;
	calc_no_unread(global.groups[n]);
    }

    XtSetArg(args[0], XtNnAlloc, global.no_new_groups + 8);
    XtSetValues(main_widgets.group_list, args, 1);

    XtSetSensitive(top_vbar, True);
    XtSetSensitive(top_hbar, True);

    ScrListClearLines(main_widgets.thread_list);
    ScrListClearLines(main_widgets.group_list);
    ArtTextClearLines(main_widgets.text);
    XtUnmanageChild(main_widgets.arttree);
    XtUnmanageChild(main_widgets.thread_list);
    knapp_set_mode(NewsModeNewgroups);
    update_misc_menu(NewsModeNewgroups);
    XtSetArg(args[0], XtNatMostOne, False);
    XtSetArg(args[1], XtNatLeastOne, False);
    XtSetArg(args[2], XtNallowDnd, False);
    XtSetValues(main_widgets.group_list, args, 3);
    XtManageChild(main_widgets.group_list);
    XtSetArg(args[0], XtNstepSize, 1);
    XtSetValues(top_vbar, args, 1);
    ScrBarSetLengthsAndPos(top_hbar, 0, 0, 0);

    ScrollableSuspend(main_widgets.group_list);

    for (n = 0 ; n < global.no_new_groups ; n++) {
	GROUP	*group = global.new_groups[n];
	char	buffer[LINE_LEN];

	print_group_info_all(buffer, sizeof buffer, group);
	group->disp = ScrListAddLine(main_widgets.group_list, buffer, None);
    }

    ScrollableResume(main_widgets.group_list);
    Remanage(main_widgets.top_manager);

    set_message("New groups found.", False);
}

void create_main_widgets(void)
{
    static char	*knapp_name[] = {
	"knapp0",	"knapp1",	"knapp2",	"misc",
	"post",		"kill",		"knapp6",	"knapp7",
	"knapp8",	"abort",	"save",		"search",
    };
    Widget	main_layout, knapp_layout, text_layout;
    Widget	temp, text_manager;
    Arg		args[10];
    int		one_win = !global.separate_windows;
    int		i;

    if (one_win) {
	main_widgets.second_shell = NULL;
	main_layout =
	    XtVaCreateManagedWidget("singlelayout", layoutWidgetClass,
				    main_widgets.shell,
				    XtVaTypedArg, XtNlayout, XtRString,
#include "layouts/single.h"
				    (int)sizeof(String), (void *)0);
    } else {
	XtSetArg(args[0], XtNinput, True);
	XtSetArg(args[1], XtNcolormap, global.cmap);
	XtSetArg(args[2], XtNvisual, global.visual);
	XtSetArg(args[3], XtNdepth, global.depth);
	main_widgets.second_shell =
	    XtCreatePopupShell("second", topLevelShellWidgetClass,
			       main_widgets.shell, args, 4);
	main_layout =
	    XtVaCreateManagedWidget("doublelayout", layoutWidgetClass,
				    main_widgets.shell,
				    XtVaTypedArg, XtNlayout, XtRString,
#include "layouts/double.h"
				    (int)sizeof(String), (void *)0);
    }

    text_layout =
	XtVaCreateManagedWidget("textlayout", layoutWidgetClass,
				main_widgets.second_shell ?
				main_widgets.second_shell : main_layout,
				XtVaTypedArg, XtNlayout, XtRString,
#include "layouts/text.h"
				(int)sizeof(String), (void *)0);

    main_widgets.top_layout =
	XtVaCreateManagedWidget("toplayout", layoutWidgetClass, main_layout,
				XtVaTypedArg, XtNlayout, XtRString,
#include "layouts/top.h"
				(int)sizeof(String), (void *)0);

    XtSetArg(args[0], XtNcenter, False);
    message_line =
	XtCreateManagedWidget("message", messageWidgetClass,
			      main_layout, args, 1);
    knapp_layout =
	XtVaCreateManagedWidget("knapplayout", layoutWidgetClass, main_layout,
				XtVaTypedArg, XtNlayout, XtRString,
#include "layouts/knapp.h"
				(int)sizeof(String), (void *)0);

    main_widgets.top_manager =
	XtCreateManagedWidget("topmanager", managerWidgetClass,
			      main_widgets.top_layout, NULL, 0);

    temp = XtCreateManagedWidget("sash1", sashWidgetClass,
				 main_layout, NULL, 0);
    XtAddCallback(temp, XtNcallback, sash_callback, NULL);
    temp = XtCreateManagedWidget("sash2", sashWidgetClass,
				 main_layout, NULL, 0);
    XtAddCallback(temp, XtNcallback, sash_callback, NULL);
    if (one_win) {
	temp = XtCreateManagedWidget("sash3", sashWidgetClass,
				     main_layout, NULL, 0);
	XtAddCallback(temp, XtNcallback, sash_callback, NULL);
    }

    top_vbar = XtCreateManagedWidget("topvbar", scrBarWidgetClass,
				     main_widgets.top_layout, NULL, 0);
    XtSetArg(args[0], XtNvertical, False);
    top_hbar = XtCreateManagedWidget("tophbar", scrBarWidgetClass,
				     main_widgets.top_layout, args, 1);

    XtSetArg(args[0], XtNcontainHoriz, False);
    XtSetArg(args[1], XtNcontainVert, False);
    XtSetArg(args[2], XtNpixmapWidth, HOT_PIXMAP_SIZE);
    XtSetArg(args[3], XtNpixmapHeight, HOT_PIXMAP_SIZE);
    XtSetArg(args[4], XtNdepthOne, False);
    XtSetArg(args[5], XtNhBar, top_hbar);
    XtSetArg(args[6], XtNvBar, top_vbar);
    main_widgets.arttree =
	XtCreateWidget("arttree", artTreeWidgetClass,
		       main_widgets.top_manager, args, 7);
    XtSetArg(args[1], XtNcontainVert, True);
    XtSetArg(args[7], XtNusePixmaps, False);
    main_widgets.group_list =
	XtCreateManagedWidget("grouplist", scrListWidgetClass,
			      main_widgets.top_manager, args, 8);
    XtSetArg(args[7], XtNusePixmaps, True);
    XtSetArg(args[8], XtNatLeastOne, True);
    XtSetArg(args[9], XtNatMostOne, True);
    main_widgets.thread_list =
	XtCreateWidget("threadlist", scrListWidgetClass,
		       main_widgets.top_manager, args, 9);

    text_vbar = XtCreateManagedWidget("textvbar", scrBarWidgetClass,
				      text_layout, NULL, 0);
    XtSetArg(args[0], XtNvertical, False);
    text_hbar = XtCreateManagedWidget("texthbar", scrBarWidgetClass,
				      text_layout, args, 1);
    text_manager =
	XtCreateManagedWidget("textmanager", managerWidgetClass,
			      text_layout, NULL, 0);
    XtSetArg(args[0], XtNhBar, text_hbar);
    XtSetArg(args[1], XtNvBar, text_vbar);
    XtSetArg(args[2], XtNcontainVert, True);
    XtSetArg(args[3], XtNcontainHoriz, False);
    main_widgets.text =
	XtCreateManagedWidget("text", artTextWidgetClass,
			      text_manager, args, 4);

    XtSetArg(args[0], XtNresizable, False);
    for (i = 0 ; i < 12 ; i++) {
	int	has_menu = False;

	if (i == 3) {
	    XtSetArg(args[1], XtNmenuName, NULL);
	    has_menu = True;
	} else if (i == 4) {
	    XtSetArg(args[1], XtNmenuName, "postshell");
	    has_menu = True;
	}

	main_widgets.knapp[i] =
	    XtCreateManagedWidget(knapp_name[i], has_menu ?
				  menuKnappWidgetClass : knappWidgetClass,
				  knapp_layout, args, has_menu ? 2 : 1);
    }

    XtRealizeWidget(main_widgets.shell);
    add_WM_DELETE_WINDOW_callback(main_widgets.shell,
				  delete_window_callback, NULL);
    XtInstallAllAccelerators(main_widgets.shell, main_widgets.shell);

    if (main_widgets.second_shell) {
	XtRealizeWidget(main_widgets.second_shell);
	add_WM_DELETE_WINDOW_callback(main_widgets.second_shell,
				      delete_window_callback, NULL);
	XtInstallAllAccelerators(main_widgets.second_shell,
				 main_widgets.shell);
    }

    create_misc_menu1(main_widgets.shell);
    create_misc_menu2(main_widgets.shell);
    create_post_menu(main_widgets.shell);

    XtAddCallback(main_widgets.knapp[0], XtNcallback,
		  knapp0_callback, NULL);
    XtAddCallback(main_widgets.knapp[1], XtNcallback,
		  knapp1_callback, NULL);
    XtAddCallback(main_widgets.knapp[2], XtNcallback,
		  knapp2_callback, NULL);
    XtAddCallback(main_widgets.knapp[5], XtNcallback,
		  knapp5_callback, NULL);
    XtAddCallback(main_widgets.knapp[6], XtNcallback,
		  knapp6_callback, NULL);
    XtAddCallback(main_widgets.knapp[7], XtNcallback,
		  knapp7_callback, NULL);
    XtAddCallback(main_widgets.knapp[8], XtNcallback,
		  knapp8_callback, NULL);
    XtAddCallback(main_widgets.knapp[9], XtNcallback,
		  abort_callback, NULL);
    XtAddCallback(main_widgets.knapp[10], XtNcallback,
		  knapp10_callback, NULL);
    XtAddCallback(main_widgets.knapp[11], XtNcallback,
		  knapp11_callback, NULL);
    XtAddCallback(main_widgets.text, XtNurlCallback,
		  text_url_callback, NULL);
    XtAddCallback(main_widgets.arttree, XtNselectCallback,
		  arttree_sel_callback, NULL);
    XtAddCallback(main_widgets.arttree, XtNouterCallback,
		  arttree_tag_callback, NULL);
    XtAddCallback(main_widgets.thread_list, XtNselectCallback,
		  thread_list_sel_callback, NULL);
    XtAddCallback(main_widgets.thread_list, XtNcallback,
		  thread_list_callback, NULL);
    XtAddCallback(main_widgets.group_list, XtNcallback,
		  group_list_callback, NULL);
    XtAddCallback(main_widgets.group_list, XtNselectCallback,
		  group_list_sel_callback, NULL);
    XtAddCallback(main_widgets.group_list, XtNdndCallback,
		  group_list_dnd_callback, NULL);

#if HAVE_XMU && (XtSpecificationRelease > 4)
    XtAddEventHandler(main_widgets.shell, (EventMask)0, True,
		      _XEditResCheckMessages, NULL);
    if (main_widgets.second_shell)
	XtAddEventHandler(main_widgets.second_shell, (EventMask)0,
			  True, _XEditResCheckMessages, NULL);

#endif
}

void purge_hot(Pixmap pixmap)
{
    SUBJECT	*subj;
    ARTICLE	*art;

    if (global.mode != NewsModeGroup && global.mode != NewsModeThread)
	return;

    ScrListPurgePixmap(main_widgets.thread_list, pixmap);

    for (art = get_articles(main_thr) ; art ; art = art->next)
	if (art->pixmap == pixmap)
	    art->pixmap = 0;

    for (subj = get_subjects(main_thr) ; subj ; subj = subj->next)
	if (subj->pixmap == pixmap)
	    update_subj_hot_value(subj);

    if (global.mode == NewsModeThread && global.curr_subj) {
	for (art = global.curr_subj->thread ; art ;
	     art = next_in_thread_preorder(art))
	    if (ArtTreeNodeGetPixmap(main_widgets.arttree,
				     (ART_TREE_NODE *)art) == pixmap)
		ArtTreeNodeSetPixmap(main_widgets.arttree,
				     (ART_TREE_NODE *)art, None);

	update_subj_hot_value(global.curr_subj);
    }
}
