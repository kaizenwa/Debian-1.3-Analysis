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
#include "codes.h"
#include "connect.h"
#include "newsrc.h"
#include "read.h"
#include "search.h"
#include "server.h"
#include "tag.h"
#include "thread.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/Dialogue.h"
#include "../Widgets/FileSel.h"
#include "../Widgets/Knapp.h"
#include "../Widgets/Layout.h"
#include "../Widgets/Message.h"
#include "../Widgets/Notice.h"
#include "../Widgets/Sash.h"
#include "../Widgets/Scrollable.h"
#include "../Widgets/ScrBar.h"
#include "../Widgets/ScrList.h"
#include "../Widgets/TextField.h"
#include "../Widgets/Toggle.h"
#include "../Widgets/Util.h"

typedef enum {
    SearchModeRegexp,
    SearchModeXpat,
    SearchModeNewsgroups
} SearchMode;

typedef enum {
    SearchScopeAll,
    SearchScopeThread,
    SearchScopeSubthread,
    SearchScopeTagged
} SearchScope;

static SearchMode	search_mode = SearchModeRegexp;
static SearchScope	search_scope = SearchScopeAll;
static int		only_unread = True;
static int		stopped = False;
static int		doing_xpat = False;

static long	*xpats = NULL;
static long	n_xpats = 0;
static long	curr_xpat = -1;

static struct {
    Widget	shell;
    /***/
    Widget	regexp_field;
    Widget	wildcard_field;
    Widget	header_field;
    /***/
    Widget	from_toggle;
    Widget	subject_toggle;
    Widget	head_toggle;
    Widget	body_toggle;
    Widget	unread_toggle;
    /***/
    Widget	all_scope;
    Widget	thread_scope;
    Widget	subthread_scope;
    Widget	tagged_scope;
    /***/
    Widget	search_knapp;
    Widget	submit_knapp;
    Widget	clear_knapp;
    Widget	next_knapp;
    Widget	first_knapp;
    Widget	stop_knapp;
} search_widgets;

static ARTICLE *next_in_order(ARTICLE*);
static void	set_no_art_message(void);

/*************************************************************************/

static void clear_xpat_data(void)
{
    XtFree((char *)xpats);
    xpats = NULL;
    n_xpats = 0;
    curr_xpat = -1;
}

static void set_xpat_message(int error)
{
    if (curr_xpat >= 0 && n_xpats > 0) {
	char	message[80];

	if (error)
	    sprintf(message, "Couldn't find match %ld", curr_xpat + 1);
	else
	    sprintf(message, "Match %ld of %ld", curr_xpat + 1, n_xpats);
	set_message(message, error);
    }
}

static ARTICLE *no_to_art(long no, int even_if_read)
{
    ARTICLE	*art;

    for (art = get_articles(main_thr) ; art ; art = art->next)
	if (art->no == no) {
	    if (art->read && !even_if_read)
		return NULL;
	    else
		return art;
	}

    return NULL;
}

static void set_scope_toggle(SearchScope scope, int set)
{
    Widget	w = NULL;

    switch (scope) {
    case SearchScopeAll:
	w = search_widgets.all_scope;
	break;
    case SearchScopeThread:
	w = search_widgets.thread_scope;
	break;
    case SearchScopeSubthread:
	w = search_widgets.subthread_scope;
	break;
    case SearchScopeTagged:
	w = search_widgets.tagged_scope;
	break;
    default:
	return;
    }

    ToggleSet(w, set);
}

static void do_subj_toggle(int set)
{
    XtSetSensitive(search_widgets.unread_toggle, !set);
    XtSetSensitive(search_widgets.all_scope, !set);
    XtSetSensitive(search_widgets.thread_scope, !set);
    XtSetSensitive(search_widgets.subthread_scope, !set);
    XtSetSensitive(search_widgets.tagged_scope, !set);
}

static void set_search_mode(SearchMode mode)
{
    int   tmp;

    search_mode = mode;

    if (mode != SearchModeXpat)
	clear_xpat_data();

    XtSetSensitive(search_widgets.header_field,
		   mode != SearchModeNewsgroups);
    XtSetSensitive(search_widgets.wildcard_field,
		   mode != SearchModeNewsgroups);

    XtSetSensitive(search_widgets.from_toggle, mode == SearchModeRegexp);
    XtSetSensitive(search_widgets.subject_toggle, mode == SearchModeRegexp);
    XtSetSensitive(search_widgets.head_toggle, mode == SearchModeRegexp);
    XtSetSensitive(search_widgets.body_toggle, mode == SearchModeRegexp);

    if (mode == SearchModeRegexp)
	tmp = !ToggleGet(search_widgets.subject_toggle);
    else
	tmp = False;

    XtSetSensitive(search_widgets.unread_toggle, tmp);
    XtSetSensitive(search_widgets.all_scope, tmp);
    XtSetSensitive(search_widgets.thread_scope, tmp);
    XtSetSensitive(search_widgets.subthread_scope, tmp);
    XtSetSensitive(search_widgets.tagged_scope, tmp);

    KnappSetSensitive(search_widgets.search_knapp, mode != SearchModeXpat);
    KnappSetSensitive(search_widgets.submit_knapp, mode == SearchModeXpat);
    KnappSetSensitive(search_widgets.clear_knapp,
		      mode == SearchModeXpat && xpats != NULL);
    KnappSetSensitive(search_widgets.next_knapp,
		      mode == SearchModeXpat &&
		      curr_xpat >= 0 && curr_xpat < n_xpats - 1);
    KnappSetSensitive(search_widgets.first_knapp,
		      mode == SearchModeXpat && n_xpats > 0);
}

static int valid_header(char *c)
{
    while (*c != '\0')
	if (!isalpha((unsigned char)*c) &&
	    !isdigit((unsigned char)*c) &&
	    *c != '-' && *c != '/')
	    return False;
	else
	    c++;

    return True;
}

/*************************************************************************/

static void do_group_search(regex_t *re)
{
    long	i;

    i = ScrListGetFirstSelected(main_widgets.group_list);
    if (i < 0)
	i = 0;
    else
	ScrListSetSelected(main_widgets.group_list, i++, False);

    while (i < global.no_groups) {
	if (regexec(re, global.groups[i]->name, 0, NULL, 0) == 0)
	    break;
	i++;
    }

    if (i >= global.no_groups)
	set_message("No match!", True);
    else {
	global.curr_group = global.groups[i];
	ScrListSetSelected(main_widgets.group_list, i, True);
	ScrListMakeVisible(main_widgets.group_list, i);
    }
}

static void do_subj_search(regex_t *re)
{
    SUBJECT	*subj;

    subj = global.curr_subj;
    if (subj)
	subj = subj->next;
    if (!subj)
	subj = get_subjects(main_thr);

    while (subj) {
	if (subj->disp >= 0 && regexec(re, subj->subject, 0, NULL, 0) == 0)
	    break;
	subj = subj->next;
    }

    if (!subj)
	set_message("No matches!", True);
    else {
	global.curr_art = NULL;
	if (global.mode != NewsModeThread)
	    set_curr_subj(subj);
	else {
	    global.curr_subj = subj;
	    setNewsModeGroup(False);
	}
    }
}

static void do_from_search(regex_t *re)
{
    ARTICLE	*art;

    art = next_in_order(NULL);
    if (!art) {
	set_no_art_message();
	return;
    }

    do {
	if (art->from && regexec(re, art->from, 0, NULL, 0) == 0)
	    break;
	art = next_in_order(art);
    } while (art);

    if (!art)
	set_message("No matches!", True);
    else {
	set_curr_art(art, True);
	if (global.mode == NewsModeThread)
	    setNewsModeThread();
	read_article(art, False, NULL, NULL);
    }    
}

static void do_art_search(regex_t *re)
{
    char	command[80], message[80];
    char	*cp, *mp, *reply;
    regmatch_t	pmatch[1];
    ARTICLE	*art;
    long	sel_data[3];
    long	n = 0, n_arts = 0;
    int		head = True;
    int		in_head = False;

    if (ToggleGet(search_widgets.head_toggle))
	if (ToggleGet(search_widgets.body_toggle))
	    strcpy(command, "ARTICLE ");
	else
	    strcpy(command, "HEAD ");
    else
	if (ToggleGet(search_widgets.body_toggle)) {
	    strcpy(command, "BODY ");
	    head = False;
	} else {
	    set_message("Nothing to search!", True);
	    return;
	}
    cp = command + strlen(command);

    art = next_in_order(NULL);
    if (!art) {
	set_no_art_message();
	return;
    }

    strcpy(message, "Searching...   ");
    mp = message + strlen(message);

    set_busy(True);
    stopped = False;
    XtSetSensitive(search_widgets.stop_knapp, True);

    do {
	sprintf(mp, "%ld", n_arts++);
	set_message(message, False);

	sprintf(cp, "%ld\r\n", art->no);
	reply = server_comm(main_server, command, True);

	if (!reply)
	    break;

	n = atoi(reply);
	if (n != NNTP_OK_ARTICLE && n != NNTP_OK_HEAD && n != NNTP_OK_BODY)
	    fprintf(stderr,
		    "knews: Failed to get article %ld, "
		    "message from server is: %s\n", art->no, reply);
	else {
	    n = 0;
	    in_head = head;

	    reply = server_read(main_server);
	    while (reply && !IS_DOT(reply)) {
		if (regexec(re, reply, 1, pmatch, 0) == 0)
		    break;
		n++;
		if (in_head && reply[0] == '\0') {
		    in_head = False;
		    n = 0;
		}
		reply = server_read(main_server);
	    }

	    if (!reply || !IS_DOT(reply))
		break;
	}

	art = next_in_order(art);
    } while (!stopped && art);

    while (reply && !IS_DOT(reply))
	reply = server_read(main_server);

    XtSetSensitive(search_widgets.stop_knapp, False);

    if (!reply) {
	reconnect_server(True);
	unset_busy();
	return;
    }

    unset_busy();

    if (!art) {
	set_message("No matches!", True);
	return;
    }

    if (stopped) {
	set_message("Search stopped!", False);
	return;
    }

    if (in_head)
	n = - n - 1;
    sel_data[0] = n;
    sel_data[1] = pmatch[0].rm_so;
    sel_data[2] = pmatch[0].rm_eo - 1;

    set_curr_art(art, True);
    if (global.mode == NewsModeThread)
	setNewsModeThread();
    read_article(art, True, sel_data, NULL);
}

static void do_xpat_search(char *header, char *wildcard)
{
    char	command[512];
    char	*buffer, *c;
    long	n_alloc = 0;

    clear_xpat_data();
    KnappSetSensitive(search_widgets.first_knapp, False);
    KnappSetSensitive(search_widgets.clear_knapp, False);
    KnappSetSensitive(search_widgets.next_knapp, False);

    if (strlen(header) + strlen(wildcard) >
	sizeof(command) - 64) {
	set_message("Header or wildcard too long!", True);
	return;
    }

    if (!global.curr_group ||
	global.curr_group->first_art > global.curr_group->last_art) {
	set_message("No articles!", True);
	return;
    }

    sprintf(command, "XPAT %s %ld-%ld %s\r\n",
	    header,
	    global.curr_group->first_art,
	    global.curr_group->last_art,
	    wildcard);

    set_busy(True);
    set_message("Server contacted, waiting for response, "
		"this may take some time...", False);
    buffer = server_comm(main_server, command, True);
    if (!buffer) {
	reconnect_server(True);
	unset_busy();
	return;
    }

    if (atoi(buffer) != NNTP_OK_HEAD) {
	unset_busy();
	if (strlen(buffer) > 100)
	    buffer[100] = '\0';
	sprintf(command, "Error!  Message from server is: %s",
		buffer);
	set_message(command, True);
	return;
    }

    sprintf(command, "Number of matches:  ");
    c = command + strlen(command);
    n_xpats = 0;

    doing_xpat = True;
    buffer = server_read(main_server);
    while (buffer && !IS_DOT(buffer)) {
	long	no;

	if (sscanf(buffer, "%ld", &no) == 1 &&
	    no_to_art(no, False)) {
	    if (n_xpats > n_alloc - 2) {
		n_alloc = 2 * (n_alloc + 1);
		xpats =
		    (long *)XtRealloc((char *)xpats,
				      n_alloc * sizeof(long));
	    }

	    xpats[n_xpats++] = no;
	    sprintf(c, "%ld", n_xpats);
	    set_message(command, False);
	}

	buffer = server_read(main_server);
    }
    doing_xpat = False;

    if (!buffer) {
	clear_xpat_data();
	reconnect_server(True);
	unset_busy();
	return;
    }

    unset_busy();

    if (n_xpats == 0)
	set_message("No matches!", True);
    else {
	ARTICLE	*art = no_to_art(xpats[0], True);

	curr_xpat = 0;

	set_curr_art(art, True);
	if (global.mode == NewsModeThread)
	    setNewsModeThread();
	read_article(art, False, NULL, NULL);
	set_xpat_message(art == NULL);
	KnappSetSensitive(search_widgets.submit_knapp, False);
	KnappSetSensitive(search_widgets.first_knapp, True);
	KnappSetSensitive(search_widgets.next_knapp, n_xpats > 1);
	KnappSetSensitive(search_widgets.clear_knapp, True);
    }
}

static void submit_knapp_callback(Widget w,
				  XtPointer client_data,
				  XtPointer call_data)
{

    if (global.busy)
	return;

    if (search_mode == SearchModeXpat) {
	char	*header, *wildcard;

	if (global.mode != NewsModeGroup &&
	    global.mode != NewsModeThread)
	    return;
	
	header   = TextFieldGetBuffer(search_widgets.header_field);
	wildcard = TextFieldGetBuffer(search_widgets.wildcard_field);

	if (header && wildcard)
	    if (valid_header(header))
		do_xpat_search(header, wildcard);
	    else
		set_message("Invalid header name!", True);
	else if (global.bell)
		XBell(display, 0);

	XtFree(header);
	XtFree(wildcard);
    }
}

static void search_knapp_callback(Widget w,
				  XtPointer client_data,
				  XtPointer call_data)
{
    char	*buffer;
    regex_t	re;
    int		code;

    if (global.busy || global.mode == SearchModeXpat)
	return;

    buffer = TextFieldGetBuffer(search_widgets.regexp_field);
    code = regcomp(&re, buffer,
		   (global.icase_regexps ? REG_ICASE : 0) | REG_EXTENDED);
    XtFree(buffer);

    if (code != 0) {
	popup_regexpnotice(code, &re);
	return;
    }

    if (search_mode == SearchModeNewsgroups)
	do_group_search(&re);
    else if (ToggleGet(search_widgets.subject_toggle))
	do_subj_search(&re);
    else if (ToggleGet(search_widgets.from_toggle))
	do_from_search(&re);
    else
	do_art_search(&re);

    regfree(&re);
}

static void close_knapp_callback(Widget w,
				 XtPointer client_data,
				 XtPointer call_data)
{
    if (!doing_xpat)
	popdown_search();
}

static void stop_knapp_callback(Widget w,
				XtPointer client_data,
				XtPointer call_data)
{
    stopped = True;
    XtSetSensitive(w, False);
    set_message("Search stopped, waiting for last article...", True);
}

static void clear_knapp_callback(Widget w,
				 XtPointer client_data,
				 XtPointer call_data)
{
    if (!global.busy && search_mode == SearchModeXpat) {
	clear_xpat_data();
	KnappSetSensitive(search_widgets.submit_knapp, True);
	KnappSetSensitive(search_widgets.clear_knapp, False);
	KnappSetSensitive(search_widgets.next_knapp, False);
	KnappSetSensitive(search_widgets.first_knapp, False);
    }
}

static void first_knapp_callback(Widget w,
				 XtPointer client_data,
				 XtPointer call_data)
{
    if (!global.busy && search_mode == SearchModeXpat && n_xpats > 0) {
	ARTICLE	*art = no_to_art(xpats[0], True);

	curr_xpat = 0;

	set_curr_art(art, True);
	if (global.mode == NewsModeThread)
	    setNewsModeThread();
	read_article(art, False, NULL, NULL);
	set_xpat_message(art == NULL);
	KnappSetSensitive(search_widgets.next_knapp, n_xpats > 1);
    }
}

static void next_knapp_callback(Widget w,
				XtPointer client_data,
				XtPointer call_data)
{
    if (!global.busy && search_mode == SearchModeXpat &&
	curr_xpat >= 0 && curr_xpat + 1 < n_xpats) {
	ARTICLE	*art = no_to_art(xpats[++curr_xpat], True);

	set_curr_art(art, True);
	if (global.mode == NewsModeThread)
	    setNewsModeThread();
	read_article(art, False, NULL, NULL);
	set_xpat_message(art == NULL);
	KnappSetSensitive(search_widgets.next_knapp, curr_xpat + 1 < n_xpats);
    }
}

static void from_toggle_callback(Widget w,
				 XtPointer client_data,
				 XtPointer call_data)
{
    Boolean	*set = (Boolean *)call_data;

    if (global.busy || !set)
	return;

    *set = !*set;
    if (*set) {
	ToggleSet(search_widgets.subject_toggle, False);
	ToggleSet(search_widgets.head_toggle, False);
	ToggleSet(search_widgets.body_toggle, False);
	do_subj_toggle(False);
    }
}

static void subject_toggle_callback(Widget w,
				    XtPointer client_data,
				    XtPointer call_data)
{
    Boolean	*set = (Boolean *)call_data;

    if (global.busy || !set)
	return;

    *set = !*set;
    if (*set) {
	ToggleSet(search_widgets.from_toggle, False);
	ToggleSet(search_widgets.head_toggle, False);
	ToggleSet(search_widgets.body_toggle, False);
    }

    do_subj_toggle(*set);
}

static void head_toggle_callback(Widget w,
				 XtPointer client_data,
				 XtPointer call_data)
{
    Boolean	*set = (Boolean *)call_data;

    if (global.busy || !set)
	return;

    *set = !*set;
    if (*set) {
	ToggleSet(search_widgets.from_toggle, False);
	ToggleSet(search_widgets.subject_toggle, False);
	do_subj_toggle(False);
    }
}

static void body_toggle_callback(Widget w,
				 XtPointer client_data,
				 XtPointer call_data)
{
    Boolean	*set = (Boolean *)call_data;

    if (global.busy || !set)
	return;

    *set = !*set;
    if (*set) {
	ToggleSet(search_widgets.from_toggle, False);
	ToggleSet(search_widgets.subject_toggle, False);
	do_subj_toggle(False);
    }
}

static void xpat_field_callback(Widget w,
				XtPointer client_data,
				XtPointer call_data)
{
    if (global.busy)
	return;

    if (w == search_widgets.wildcard_field)
	XtSetKeyboardFocus(search_widgets.shell,
			   search_widgets.header_field);
    else
	XtSetKeyboardFocus(search_widgets.shell,
			   search_widgets.wildcard_field);
}

static void focus_callback(Widget w,
			   XtPointer client_data,
			   XtPointer call_data)
{
    if (global.busy)
	return;

    if (global.mode == NewsModeAllgroups)
	set_search_mode(SearchModeNewsgroups);
    else if (w == search_widgets.regexp_field)
	set_search_mode(SearchModeRegexp);
    else
	set_search_mode(SearchModeXpat);
}

static void scope_callback(Widget w,
			   XtPointer client_data,
			   XtPointer call_data)
{
    SearchScope	scope = (SearchScope)client_data;

    if (global.busy || scope == search_scope)
	return;

    set_scope_toggle(search_scope, False);
    search_scope = scope;
    set_scope_toggle(search_scope, True);
}

static void unread_toggle_callback(Widget w,
				   XtPointer client_data,
				   XtPointer call_data)
{
    Boolean	*set = (Boolean *)call_data;

    if (global.busy || !set)
	return;

    *set = !*set;
    only_unread = *set;
}

static void tab_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    if (ToggleGet(search_widgets.from_toggle)) {
	ToggleSet(search_widgets.from_toggle, False);
	ToggleSet(search_widgets.subject_toggle, True);
	do_subj_toggle(True);
    } else if (ToggleGet(search_widgets.subject_toggle)) {
	ToggleSet(search_widgets.subject_toggle, False);
	ToggleSet(search_widgets.head_toggle, True);
	do_subj_toggle(False);
    } else if (ToggleGet(search_widgets.head_toggle)) {
	if (ToggleGet(search_widgets.body_toggle))
	    ToggleSet(search_widgets.head_toggle, False);
	else
	    ToggleSet(search_widgets.body_toggle, True);
    } else {
	ToggleSet(search_widgets.body_toggle, False);
	ToggleSet(search_widgets.from_toggle, True);
    }
}

/*************************************************************************/
			   
static void create_search_widgets(void)
{
    Arg		args[8];
    Widget	layout;
    Widget	w;

    XtSetArg(args[0], XtNallowShellResize, True);
    XtSetArg(args[1], XtNinput, True);
    XtSetArg(args[2], XtNcolormap, global.cmap);
    XtSetArg(args[3], XtNvisual, global.visual);
    XtSetArg(args[4], XtNdepth, global.depth);
    search_widgets.shell =
	XtCreatePopupShell("searchshell", topLevelShellWidgetClass,
			   main_widgets.shell, args, 5);

    layout =
	XtVaCreateManagedWidget("searchlayout", layoutWidgetClass,
				search_widgets.shell,
				XtVaTypedArg, XtNlayout, XtRString,
#include "layouts/search.h"
				(int)sizeof(String), (void *)0);

    XtCreateManagedWidget("regexptitle", messageWidgetClass, layout, NULL, 0);
    XtCreateManagedWidget("xpattitle", messageWidgetClass, layout, NULL, 0);
    XtCreateManagedWidget("sash", sashWidgetClass, layout, NULL, 0);

    XtSetArg(args[0], XtNcenter, False);
    XtCreateManagedWidget("regexpmessage", messageWidgetClass,
			  layout, args, 1);
    XtCreateManagedWidget("wildcardmessage", messageWidgetClass,
			  layout, args, 1);
    XtCreateManagedWidget("headermessage", messageWidgetClass,
			  layout, args, 1);

    XtSetArg(args[0], XtNfocusRoot, search_widgets.shell);
    XtSetArg(args[1], XtNsingleLine, True);

    search_widgets.regexp_field =
	XtCreateManagedWidget("regexpfield", textFieldWidgetClass,
			      layout, args, 2);
    XtAddCallback(search_widgets.regexp_field, XtNcallback,
		  search_knapp_callback, NULL);
    XtAddCallback(search_widgets.regexp_field, XtNtabCallback,
		  tab_callback, NULL);
    XtAddCallback(search_widgets.regexp_field, XtNfocusCallback,
		  focus_callback, NULL);

    search_widgets.wildcard_field =
	XtCreateManagedWidget("wildcardfield", textFieldWidgetClass,
			      layout, args, 2);
    XtAddCallback(search_widgets.wildcard_field, XtNcallback,
		  xpat_field_callback, NULL);
    XtAddCallback(search_widgets.wildcard_field, XtNtabCallback,
		  xpat_field_callback, NULL);
    XtAddCallback(search_widgets.wildcard_field, XtNfocusCallback,
		  focus_callback, NULL);

    search_widgets.header_field =
	XtCreateManagedWidget("headerfield", textFieldWidgetClass,
			      layout, args, 2);
    XtAddCallback(search_widgets.header_field, XtNcallback,
		  xpat_field_callback, NULL);
    XtAddCallback(search_widgets.header_field, XtNtabCallback,
		  xpat_field_callback, NULL);
    XtAddCallback(search_widgets.header_field, XtNfocusCallback,
		  focus_callback, NULL);

    search_widgets.search_knapp =
	XtCreateManagedWidget("search", knappWidgetClass, layout, NULL, 0);
    XtAddCallback(search_widgets.search_knapp, XtNcallback,
		  search_knapp_callback, NULL);

    search_widgets.submit_knapp =
	XtCreateManagedWidget("submit", knappWidgetClass, layout, NULL, 0);
    XtAddCallback(search_widgets.submit_knapp, XtNcallback,
		  submit_knapp_callback, NULL);

    search_widgets.clear_knapp =
	XtCreateManagedWidget("clear", knappWidgetClass, layout, NULL, 0);
    XtAddCallback(search_widgets.clear_knapp, XtNcallback,
		  clear_knapp_callback, NULL);

    search_widgets.next_knapp =
	XtCreateManagedWidget("next", knappWidgetClass, layout, NULL, 0);
    XtAddCallback(search_widgets.next_knapp, XtNcallback,
		  next_knapp_callback, NULL);

    search_widgets.first_knapp =
	XtCreateManagedWidget("first", knappWidgetClass, layout, NULL, 0);
    XtAddCallback(search_widgets.first_knapp, XtNcallback,
		  first_knapp_callback, NULL);

    w = XtCreateManagedWidget("close", knappWidgetClass, layout, NULL, 0);
    XtAddCallback(w, XtNcallback, close_knapp_callback, NULL);

    XtSetArg(args[0], XtNsensitive, False);
    search_widgets.stop_knapp =
	XtCreateManagedWidget("stop", knappWidgetClass, layout, args, 1);
    XtAddCallback(search_widgets.stop_knapp, XtNcallback,
		  stop_knapp_callback, NULL);

    search_widgets.unread_toggle =
	XtCreateManagedWidget("unreadtoggle", toggleWidgetClass,
			      layout, NULL, 0);
    XtAddCallback(search_widgets.unread_toggle, XtNcallback,
		  unread_toggle_callback, NULL);

    /***/

    search_widgets.from_toggle =
	XtCreateManagedWidget("fromtoggle", toggleWidgetClass,
			      layout, NULL, 0);
    XtAddCallback(search_widgets.from_toggle, XtNcallback,
		  from_toggle_callback, NULL);

    search_widgets.subject_toggle =
	XtCreateManagedWidget("subjecttoggle", toggleWidgetClass,
			      layout, NULL, 0);
    XtAddCallback(search_widgets.subject_toggle, XtNcallback,
		  subject_toggle_callback, NULL);

    search_widgets.head_toggle =
	XtCreateManagedWidget("headtoggle", toggleWidgetClass,
			      layout, NULL, 0);
    XtAddCallback(search_widgets.head_toggle, XtNcallback,
		  head_toggle_callback, NULL);

    search_widgets.body_toggle =
	XtCreateManagedWidget("bodytoggle", toggleWidgetClass,
			      layout, NULL, 0);
    XtAddCallback(search_widgets.body_toggle, XtNcallback,
		  body_toggle_callback, NULL);

    if (ToggleGet(search_widgets.head_toggle) ||
	ToggleGet(search_widgets.body_toggle)) {
	ToggleSet(search_widgets.from_toggle, False);
	ToggleSet(search_widgets.subject_toggle, False);
    } else {
	ToggleSet(search_widgets.head_toggle, False);
	ToggleSet(search_widgets.body_toggle, False);
	ToggleSet(search_widgets.subject_toggle,
		  !ToggleGet(search_widgets.from_toggle));
    }

    /***/

    search_widgets.all_scope =
	XtCreateManagedWidget("allscope", toggleWidgetClass,
			      layout, NULL, 0);
    XtAddCallback(search_widgets.all_scope, XtNcallback,
		  scope_callback, (XtPointer)SearchScopeAll);

    search_widgets.thread_scope =
	XtCreateManagedWidget("threadscope", toggleWidgetClass,
			      layout, NULL, 0);
    XtAddCallback(search_widgets.thread_scope, XtNcallback,
		  scope_callback, (XtPointer)SearchScopeThread);

    search_widgets.subthread_scope =
	XtCreateManagedWidget("subthreadscope", toggleWidgetClass,
			      layout, NULL, 0);
    XtAddCallback(search_widgets.subthread_scope, XtNcallback,
		  scope_callback, (XtPointer)SearchScopeSubthread);

    search_widgets.tagged_scope =
	XtCreateManagedWidget("taggedscope", toggleWidgetClass,
			      layout, NULL, 0);
    XtAddCallback(search_widgets.tagged_scope, XtNcallback,
		  scope_callback, (XtPointer)SearchScopeTagged);

    XtSetKeyboardFocus(search_widgets.shell,search_widgets.regexp_field);
    XtRealizeWidget(search_widgets.shell);
    XtInstallAllAccelerators(search_widgets.shell, search_widgets.shell);

    add_WM_DELETE_WINDOW_callback(search_widgets.shell,
				  close_knapp_callback, NULL);

    if (global.busy)
	set_busy_search(True);
}

void popup_search(void)
{
    if (global.busy && global.mode == NewsModeAllgroups)
	return;

    if (!search_widgets.shell) {
	create_search_widgets();
	set_search_mode(SearchModeRegexp);
    }

    if (global.mode == NewsModeAllgroups)
	set_search_mode(SearchModeNewsgroups);
    else if (search_mode == SearchModeNewsgroups)
	set_search_mode(SearchModeRegexp);

    XtPopup(search_widgets.shell, XtGrabNone);

    if (global.busy)
	set_busy_search(True);
}

void popdown_search(void)
{
    if (!search_widgets.shell)
	return;

    if (search_mode == SearchModeXpat) {
	clear_xpat_data();
	set_search_mode(SearchModeXpat);
    }
    XtPopdown(search_widgets.shell);
}

void set_busy_search(int busy)
{
    if (!search_widgets.shell)
	return;

    XDefineCursor(display, XtWindow(search_widgets.shell),
		  busy ? global.busy_cursor : global.cursor);
    KnappSetActive(search_widgets.search_knapp, !busy);
    KnappSetActive(search_widgets.submit_knapp, !busy);
    KnappSetActive(search_widgets.clear_knapp, !busy);
    KnappSetActive(search_widgets.next_knapp, !busy);
    KnappSetActive(search_widgets.first_knapp, !busy);
    TextFieldSetActive(search_widgets.regexp_field, !busy);
    TextFieldSetActive(search_widgets.header_field, !busy);
    TextFieldSetActive(search_widgets.wildcard_field, !busy);
}

/*************************************************************************/

static Widget find_group_widget = NULL;

static long getnmatching(char *c1, char *c2)
{
    long	n = 0;

    while (*c1 == *c2 && *c1 != '\0') {
	c1++;
	c2++;
	n++;
    }

    return n;
}

static void find_group_callback(Widget w,
				XtPointer client_data,
				XtPointer call_data)
{
    DialogueReport	*report = (DialogueReport *)call_data;
    Arg			arg;

    if (global.busy || !report ||
	(global.mode != NewsModeConnected &&
	 global.mode != NewsModeAllgroups &&
	 global.mode != NewsModeSomegroups))
	return;

    switch (report->reply) {
    case DialogueReplyEnter: /* find group */
    case DialogueReplyLeft:
	if (!report->buffer || report->buffer[0] == '\0')
	    XtPopdown(w);
	else if (!report->buffer || strlen(report->buffer) > 500) {
	    if (global.bell)
		XBell(display, 0);
	} else {
	    char	command[512];
	    char	*reply;

	    set_busy(True);
	    set_message("Asking server about group...", False);
	    sprintf(command, "GROUP %s\r\n", report->buffer);
	    reply = server_comm(main_server, command, True);
	    if (!reply) {
		reconnect_server(True);
		unset_busy();
		break;
	    }
	    unset_busy();

	    if (atoi(reply) == NNTP_OK_GROUP) {
		XtPopdown(w);

		global.curr_group = find_group(report->buffer);
		if (!global.curr_group) {
		    global.curr_group = create_group(report->buffer);
		    sort_groups();
		}

		if (global.mode == NewsModeAllgroups)
		    setNewsModeAllgroups(NULL); /* if we're aborted later */
		read_group(reply, True, 0);
	    } else {
		if (strlen(reply) > 80)
		    reply[80] = '\0';
		sprintf(command, "Error!  Message from server is: %s", reply);
		set_message(command, True);
	    }
	}
	break;
    case DialogueReplyMiddle: /* regexp */
	{
	    regex_t	re;
	    int		code;

	    code = regcomp(&re, report->buffer, REGEXP_COMPILE_FLAGS);
	    if (code != 0) {
		popup_regexpnotice(code, &re);
		return;
	    }

	    XtPopdown(w);
	    setNewsModeAllgroups(&re);
	    regfree(&re);
	}
	break;
    case DialogueReplyTab: /* complete */
	if (report->buffer && report->buffer[0] != '\0') {
	    long	len = strlen(report->buffer);
	    long	first, last, n, sub, first_sub;
	    char	*c;

	    for (first = 0 ; first < global.no_groups ; first++)
		if (!global.groups[first]->subscribed)
		    break;

	    while (first < global.no_groups) {
		if (strncmp(global.groups[first]->name,
			    report->buffer, len) == 0)
		    break;
		first++;
	    }

	    for (last = first + 1 ; last < global.no_groups ; last++)
		if (strncmp(global.groups[last]->name,
			    report->buffer, len) != 0)
		    break;

	    if (first == global.no_groups) {
		c = NULL;
		n = 0;
		first = -1;
	    } else {
		c = global.groups[first]->name;
		n = getnmatching(c, global.groups[last-1]->name);
	    }

	    first_sub = -1;
	    for (sub = 0 ; sub < global.no_groups ; sub++)
		if (!global.groups[sub]->subscribed)
		    break;
		else if (strncmp(global.groups[sub]->name,
				 report->buffer, len) == 0) {
		    if (c) {
			long	temp;

			temp = getnmatching(c, global.groups[sub]->name);
			if (temp < n)
			    n = temp;
		    } else {
			c = global.groups[sub]->name;
			n = strlen(c);
		    }
		    if (first_sub < 0)
			first_sub = sub;
		}

	    if (!c || n > 500) {
		if (global.bell)
		    XBell(display, 0);
	    } else {
		char	buffer[512];

		memcpy(buffer, c, n);
		buffer[n] = '\0';
		XtSetArg(arg, XtNbuffer, buffer);
		XtSetValues(w, &arg, 1);

		if (first < 0)
		    first = first_sub;
		if (global.mode == NewsModeAllgroups)
		    ScrollableSetVPos(main_widgets.group_list, first);
	    }
	}
	break;
    case DialogueReplyClose: /* cancel */
    case DialogueReplyRight:
	XtPopdown(w);
	break;
    }
}

void popup_find_group(void)
{
    if (global.busy ||
	(global.mode != NewsModeConnected &&
	 global.mode != NewsModeAllgroups &&
	 global.mode != NewsModeSomegroups))
	return;

    if (find_group_widget)
	popup_under_pointer(find_group_widget, XtGrabNone);
    else
	find_group_widget =
	    popup_dialogue("findgroup", "Find group(s) by name/regexp",
			   "Goto group", "Regexp", "Cancel",
			   find_group_callback, NULL, XtGrabNone);
}

void popdown_find_group(void)
{
    if (find_group_widget)
	XtPopdown(find_group_widget);
}

/*************************************************************************/

static int want_subj(SUBJECT *subj)
{
    int		visible    = False;
    int		has_unread = !only_unread;
    ARTICLE	*thr       = subj->thread;

    do {
	if (subj->disp >= 0) {
	    visible = True;
	    if (has_unread)
		return True;
	}
	if (subj->no_unread > 0) {
	    has_unread = True;
	    if (visible)
		return True;
	}
	subj = subj->next;
    } while (subj && subj->thread == thr);

    return False;
}

static SUBJECT *next_subject(SUBJECT *subj)
{
    do {
	while (subj->next && subj->next->thread == subj->thread)
	    subj = subj->next;
	subj = subj->next;
    } while (subj && !want_subj(subj));

    return subj;
}

static ARTICLE *next_in_order(ARTICLE *art)
{
    SUBJECT	*subj;
    ARTICLE	**arts;
    long	n;

    switch (search_scope) {
    case SearchScopeAll:
	if (!art)
	    art = global.curr_art;
	if (art) {
	    subj = art->subject;
	    art = next_in_thread_preorder(art);
	} else {
	    subj = global.curr_subj;
	    if (subj)
		art = subj->thread;
	    else
		return NULL;
	}

	while (subj) {
	    while (art && (!art->from || (only_unread && art->read)))
		art = next_in_thread_preorder(art);

	    if (art)
		break;

	    subj = next_subject(subj);
	    if (subj)
		art = subj->thread;
	}
	break;
    case SearchScopeThread:
	if (art)
	    art = next_in_thread_preorder(art);
	else {
	    art = global.curr_art;
	    if (art)
		art = art->next;
	    else if (global.curr_subj)
		art = global.curr_subj->thread;
	}

	while (art && (!art->from || (only_unread && art->read)))
	    art = next_in_thread_preorder(art);
	break;
    case SearchScopeSubthread:
	{
	    static ARTICLE	*thread;

	    if (art)
		art = next_in_subthread_preorder(art, thread);
	    else
		art = thread = global.curr_art;

	    while (art && (!art->from || (only_unread && art->read)))
		art = next_in_subthread_preorder(art, thread);
	}
	break;
    case SearchScopeTagged:
	n = no_tagged_articles();

	if (n <= 0)
	    return NULL;
	arts = get_tagged_articles();

	while (n-- > 0) {
	    art = *arts++;
	    if (art->from && (!only_unread || !art->read)) {
		untag_article(art);
		return art;
	    }
	}

	return NULL;
    }

    return art;
}

static void set_no_art_message(void)
{
    char	*p = "No more articles to search!";

    if (search_scope == SearchScopeTagged && no_tagged_articles() <= 0)
	    p = "No tagged articles!";

    set_message(p, True);
}
