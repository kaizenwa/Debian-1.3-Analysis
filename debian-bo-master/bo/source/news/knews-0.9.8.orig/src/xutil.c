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
#include "bg.h"
#include "cache.h"
#include "codes.h"
#include "connect.h"
#include "expand.h"
#include "file.h"
#include "k_file.h"
#include "newsrc.h"
#include "partial.h"
#include "p_setup.h"
#include "resource.h"
#include "save.h"
#include "search.h"
#include "server.h"
#include "tag.h"
#include "thread.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/ArtTree.h"
#include "../Widgets/Dialogue.h"
#include "../Widgets/Knapp.h"
#include "../Widgets/Menu.h"
#include "../Widgets/MenuShell.h"
#include "../Widgets/Notice.h"
#include "../Widgets/Scrollable.h"
#include "../Widgets/ScrList.h"
#include "../Widgets/Shadow.h"
#include "../Widgets/StringG.h"
#include "../Widgets/Util.h"

#if HAVE_XPM
#include <X11/xpm.h>
#endif

void set_curr_art(ARTICLE *art, int center)
{
    if (global.mode == NewsModeThread) {
	if (global.curr_art && global.curr_art != art)
	    ArtTreeNodeSetSelected(main_widgets.arttree,
				   (ART_TREE_NODE *)global.curr_art, False);

	if (art) {
	    ArtTreeNodeSetSelected(main_widgets.arttree,
				   (ART_TREE_NODE *)art, True);
	    if (center)
		ArtTreeNodeMakeVisible(main_widgets.arttree,
				       (ART_TREE_NODE*)art);
	}
    }

    global.curr_art = art;
    if (art)
	set_curr_subj(art->subject);
}

void set_curr_subj(SUBJECT *subj)
{
    global.curr_subj = subj;
    if (subj && subj->disp >= 0) {
	ScrListMakeVisible(main_widgets.thread_list, subj->disp);
	ScrListSetSelected(main_widgets.thread_list, subj->disp, True);
    }
}

void set_tree_stuff(ARTICLE *thr)
{
    while (thr) {
	if (thr->from && !thr->read) {
	    ArtTreeNodeSetInner(main_widgets.arttree,
				(ART_TREE_NODE *)thr, True);
	    if (!thr->read && thr->pixmap != None)
		ArtTreeNodeSetPixmap(main_widgets.arttree,
				     (ART_TREE_NODE *)thr, thr->pixmap);
	}
	if (A_PARENT(thr) && A_PARENT(thr)->subject != thr->subject) {
	    ArtTreeNodeSetDashed(main_widgets.arttree,
				 (ART_TREE_NODE *)thr, True);
	}

	if (A_CHILD1(thr))
	    set_tree_stuff(A_CHILD1(thr));
	thr = A_SIBLING(thr);
    }
}

void set_curr_group(void)
{
    long	i, n;

    switch (global.mode) {
    case NewsModeConnected:
    case NewsModeAllgroups:
    case NewsModeSomegroups:
    case NewsModeNewgroups:
	n = ScrListGetFirstSelected(main_widgets.group_list);
	if (!global.curr_group || global.curr_group->disp != n) {
	    global.curr_group = NULL;
	    if (n < 0)
		break;
	    for (i = 0 ; i < global.no_groups ; i++)
		if (global.groups[i]->disp == n) {
		    global.curr_group = global.groups[i];
		    break;
		}
	}
	break;
    case NewsModeGroup:
    case NewsModeThread:
    case NewsModeDisconnected:
	break;
    }
}

void realize_fake(ARTICLE *art, char **headers, int n)
{
    if (art->from || art->tree_data.label)
	return;

    while (n-- > 0)
	if (case_lstrncmp(*headers, "from:", 5) != 0)
	    headers++;
	else {
	    char	*c = *headers + 5;

	    while (*c ==  ' ' || *c == '\t')
		c++;
	    if (*c != '\0') {
		fix_author(art, c, res_show_number_lines());
		if (global.mode == NewsModeThread)
		    ArtTreeNodeNotifyLabel(main_widgets.arttree,
					   (ART_TREE_NODE *)art);
	    }
	    break;
	}
}

/*****************************************************************************/

void change_interruptible(int interruptible)
{
    XtSetSensitive(main_widgets.knapp[9], interruptible);
    ShadowRedrawWidget(main_widgets.knapp[9]);
}

void set_busy(int interruptible)
{
    if (global.busy) {
	fputs("knews: internal error: set_busy when already busy!\n", stderr);
	return;
    }

    global.busy = True;

    XDefineCursor(display, XtWindow(main_widgets.shell), global.busy_cursor);
    if (main_widgets.second_shell)
	XDefineCursor(display, XtWindow(main_widgets.second_shell),
		      global.busy_cursor);
    set_busy_save(True);
    set_busy_search(True);

    if (interruptible)
	KnappSetSensitive(main_widgets.knapp[9], True);

    KnappSetActive(main_widgets.knapp[0], False);
    KnappSetActive(main_widgets.knapp[1],
		   global.mode == NewsModeGroup ||
		   global.mode == NewsModeThread);
    KnappSetActive(main_widgets.knapp[2], False);
    KnappSetActive(main_widgets.knapp[3], False);
    KnappSetActive(main_widgets.knapp[4], False);
    KnappSetActive(main_widgets.knapp[6], False);
    KnappSetActive(main_widgets.knapp[7], False);
    KnappSetActive(main_widgets.knapp[8], False);
    KnappSetActive(main_widgets.knapp[11], global.mode != NewsModeAllgroups);
    ArtTreeSetActive(main_widgets.arttree, False);
    ScrListSetActive(main_widgets.group_list, False);
    ScrListSetActive(main_widgets.thread_list, False);

    XFlush(display);
}

void unset_busy(void)
{
    if (!global.busy) {
	fputs("knews: internal error: unset_busy when not busy\n", stderr);
	return;
    }

    global.busy = False;

    XDefineCursor(display, XtWindow(main_widgets.shell), global.cursor);
    if (main_widgets.second_shell)
	XDefineCursor(display, XtWindow(main_widgets.second_shell),
		      global.cursor);
    set_busy_save(False);
    set_busy_search(False);

    KnappSetSensitive(main_widgets.knapp[9], False);

    KnappSetActive(main_widgets.knapp[0], True);
    KnappSetActive(main_widgets.knapp[1], True);
    KnappSetActive(main_widgets.knapp[2], True);
    KnappSetActive(main_widgets.knapp[3], True);
    KnappSetActive(main_widgets.knapp[4], True);
    KnappSetActive(main_widgets.knapp[6], True);
    KnappSetActive(main_widgets.knapp[7], True);
    KnappSetActive(main_widgets.knapp[8], True);
    KnappSetActive(main_widgets.knapp[11], True);
    ArtTreeSetActive(main_widgets.arttree, True);
    ScrListSetActive(main_widgets.group_list, True);
    ScrListSetActive(main_widgets.thread_list, True);

    XFlush(display);
}

void set_standard_message(void)
{
    char	message[512];
    long	n;

    switch (global.mode) {
    case NewsModeDisconnected:
	set_message("Knews " KNEWS_VERSION ".   "
		    "Copyright 1995, 1996 Karl-Johan Johnsson.", False);
	break;
    case NewsModeConnected:
	if (global.groups && global.groups[0] &&
	    global.groups[0]->subscribed)
	    set_message("Subscribed groups "
			"with unread articles displayed.",
			False);
	else
	    set_message("No subscribed groups.", False);
	break;
    case NewsModeGroup:
	n = ScrollableGetVSize(main_widgets.thread_list);
	sprintf(message,
		"%s with %ld unread articles, %ld subjects.  %ld hot.",
		global.curr_group->name, global.curr_group->no_unread,
		n, global.n_hot);
	set_message(message, False);
	break;
    case NewsModeThread:
	if (!global.curr_subj)
	    return;
	sprintf(message, "%ld unread articles with Subject: ",
		global.curr_subj->no_unread);
	strncat(message, global.curr_subj->subject, 128);
	set_message(message, False);
	break;
    case NewsModeAllgroups:
	set_message("All groups displayed.", False);
	break;
    case NewsModeSomegroups:
	set_message("Groups matching regexp displayed.", False);
	break;
    case NewsModeNewgroups:
	set_message("New groups displayed.", False);
	break;
    }
}

/*****************************************************************************/

char *do_update(void)
{
    int	newsrc_ok = update_newsrc();
    int	kill_ok   = update_kill_files();

    if (newsrc_ok)
	if (kill_ok)
	    return NULL;
	else
	    return "Failed to update kill files!   Newsrc file updated.";
    else
	if (kill_ok)
	    return "Failed to update newsrc!   Kill files updated.";
	else
	    return "Failed to update newsrc and kill files!";
}

static void update_failed_callback(Widget w,
				  XtPointer client_data,
				  XtPointer call_data)
{
    NoticeReply	reply = (NoticeReply)call_data;
    int		allow_abort = (int)client_data;

    XtPopdown(w);
    XtDestroyWidget(w);

    switch (reply) {
    case NoticeReplyLeft:   /* Retry */
	global_cleanup(True, allow_abort);
	break;
    case NoticeReplyMiddle: /* Disconnect */
	global_cleanup(False, allow_abort);
	break;
    case NoticeReplyTimeout:
    case NoticeReplyRight:
    case NoticeReplyClose:  /* abort */
	if (!allow_abort)
	    exit(1);
	break;
    }
}

int global_cleanup(int try_to_update, int allow_abort)
{
    QuitFunc	qf = main_server ? server_get_quit_func(main_server) : NULL;
    long	i;
    char	*err_msg;

    if (global.mode == NewsModeGroup || global.mode == NewsModeThread) {
	free_read_arts_list(global.curr_group);
	global.curr_group->read_arts = create_read_arts_list();
    }

    if (global.mode == NewsModeDisconnected)
	return True;

    if (try_to_update && (err_msg = do_update())) {
	char	message[128];

	set_message(err_msg, True);
	sprintf(message, "%s\n\nReally disconnect?", err_msg);
	popup_notice("verify", message, "Retry", "Disconnect",
		     allow_abort ? "Abort" : NULL, 0,
		     update_failed_callback, (XtPointer)allow_abort,
		     XtGrabExclusive);
	return False;
    }

    if (qf)
	qf(main_server);

    server_close(main_server);
    XtFree(global.serv_addr);
    global.serv_addr = NULL;

    clear_thread_context(main_thr);
    bg_shutdown();
    thread_ahead_shutdown();
    cache_leave_group();
    popdown_find_group();
    kill_cleanup();
    popdown_save();
    popdown_search();
    partial_clear_cache();
    clear_tagged_articles();
    clear_history();

    for (i = 0 ; i < global.no_groups ; i++) {
	XtFree(global.groups[i]->name);
	XtFree(global.groups[i]->description);
	free_read_arts_list(global.groups[i]);
	XtFree((char *)global.groups[i]);
    }
    XtFree((char *)global.groups);
    global.groups = NULL;
    global.no_groups = 0;
    global.max_groups = 0;
    global.curr_group = NULL;
    XtFree((char *)global.new_groups);
    global.new_groups = NULL;
    global.no_new_groups = 0;

    global.curr_art = NULL;
    global.curr_subj = NULL;

    setNewsModeDisconnected();

    return True;
}

static void disconnect_verify_callback(Widget w,
				       XtPointer client_data,
				       XtPointer call_data)
{
    NoticeReply	reply = (NoticeReply)call_data;
    long	do_exit = (long)client_data;

    XtPopdown(w);
    XtDestroyWidget(w);

    if (reply == NoticeReplyLeft && global_cleanup(True, True) && do_exit)
	exit(0);

    set_standard_message();
}

static void have_posts_callback(Widget w,
				XtPointer client_data,
				XtPointer call_data)
{
    NoticeReply	reply = (NoticeReply)call_data;
    long	do_exit = (long)client_data;

    XtPopdown(w);
    XtDestroyWidget(w);

    if (reply == NoticeReplyLeft)
	if (do_exit)
	    exit(0);
	else if (global_cleanup(True, True))
	    set_standard_message();
}

void disconnect(int do_exit)
{
    if (outstanding_posts()) {
	char	message[128];

	sprintf(message, "You have not posted all articles!\n\n%s anyway?",
		do_exit ? "Quit" : "Disconnect");
	popup_notice("verify", message,
		     do_exit ? "Quit" : "Disconnect", NULL, "Abort", 0,
		     have_posts_callback, (XtPointer)do_exit,
		     XtGrabExclusive);
	return;
    }

    if (global.confirm_quit) {
	popup_notice("disconnectverify",
		     do_exit ? "Really quit?" : "Really disconnect?",
		     do_exit ? "Quit" : "Disconnect", NULL, "Abort", 0,
		     disconnect_verify_callback, (XtPointer)do_exit,
		     XtGrabExclusive);
	return;
    }

    if (global.mode == NewsModeDisconnected)
	exit(0);
    if (!global_cleanup(True, True))
	return;
    if (do_exit)
	exit(0);
    set_standard_message();
}

/*****************************************************************************/

static XtIntervalId	rescan_timeout_id = 0;
static int		rescan_due = False;

static void rescan_timeout_callback(XtPointer client_data, XtIntervalId *id)
{
    char	*buffer;

    rescan_timeout_id = 0;
    rescan_due = False;
    if (global.mode == NewsModeDisconnected || res_rescan_timeout() <= 0)
	return;

    if (global.mode != NewsModeConnected || global.busy) {
	rescan_due = True;
	return; /* no op */
    }

    set_busy(True);
    set_message("Automatic rescan in progress...", False);
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

	if (strlen(buffer) > 80)
	    buffer[80] = '\0';
	sprintf(message, "Error!  Message from server is: %s",
		buffer);
	set_message(message, True);
    }
}

void add_rescan_timeout(void)
{
    int	timeout = res_rescan_timeout();

    rescan_due = False;

    if (rescan_timeout_id != 0)
	XtRemoveTimeOut(rescan_timeout_id);

    if (timeout > 0)
	rescan_timeout_id =
	    XtAppAddTimeOut(app_cont, timeout * 60000L,
			    rescan_timeout_callback, NULL);
    else
	rescan_timeout_id = 0;	
}

void remove_rescan_timeout(void)
{
    if (rescan_timeout_id != 0)
	XtRemoveTimeOut(rescan_timeout_id);
    rescan_timeout_id = 0;
    rescan_due = False;
}

void check_if_rescan_due(void)
{
    int	timeout = res_rescan_timeout();

    if (timeout <= 0)
	return;

    if (rescan_timeout_id == 0) {
	if (rescan_due)
	    rescan_timeout_id =
		XtAppAddTimeOut(app_cont, 2000,	rescan_timeout_callback, NULL);
	else
	    add_rescan_timeout();
    }
}

/*********************************************************************/

void destroy_pixmap(XtAppContext app_cont, XrmValue *to, XtPointer data,
		    XrmValue *args, Cardinal *no_args)
{
    /* ... */
}

Boolean cvt_string_to_pixmap(Display *disp,
			     XrmValue *args, Cardinal *n_args,
			     XrmValue *from, XrmValue *to,
			     XtPointer *closure)
{
#if !HAVE_XPM
    return False;
#else
    static Pixmap	pixmap;
    char		*path;
    char		*file_name = (char *)from->addr;
    XpmAttributes	attr;
    int			tmp;

    if (!file_name)
	return False;

    if (to->addr && to->size < sizeof pixmap) {
	to->size = sizeof pixmap;
	return False;
    }

    path = expand_path(file_name);
    if (!path)
	return False;

    attr.valuemask = XpmVisual | XpmColormap | XpmDepth;
    attr.visual    = global.visual;
    attr.colormap  = global.cmap;
    attr.depth     = global.depth;

    tmp = XpmReadFileToPixmap(disp, DefaultRootWindow(disp), path,
			      &pixmap, NULL, &attr);
    if (tmp < 0) {
	char	*msg = "unknown error";

	switch (tmp) {
#ifdef XpmOpenFailed
	case XpmOpenFailed:
	    msg = "couldn't open file";
	    break;
#endif
#ifdef XpmFileInvalid
	case XpmFileInvalid:
	    msg = "file invalid";
	    break;
#endif
#ifdef XpmNoMemory
	case XpmNoMemory:
	    msg = "no memory";
	    break;
#endif
#ifdef XpmColorFailed
	case XpmColorFailed:
	    msg = "color error";
	    break;
#endif
	default:
	    break;
	}
	fprintf(stderr, "XpmReadFileToPixmap failed on %s: %s\n", path, msg);
	XtFree(path);
	return False;
    }

#ifdef XpmColorError
    if (tmp == XpmColorError)
	fprintf(stderr, "XpmReadFileToPixmap warning %s: color error\n", path);
#endif

    XtFree(path);

    to->size = sizeof pixmap;
    if (to->addr)
	*(Pixmap *)to->addr = pixmap;
    else
	to->addr = (XPointer)&pixmap;

    return True;
#endif
}

/*************************************************************************/

static void notice_callback(Widget w,
			    XtPointer client_data,
			    XtPointer call_data)
{
    XtPopdown(w);
    XtDestroyWidget(w);
}

Widget popup_notice(char		*name,
		    char		*message,
		    char		*left,
		    char		*middle,
		    char		*right,
		    long		 timeout,
		    XtCallbackProc	 callback,
		    XtPointer		 client_data,
		    XtGrabKind		 grab)
{
    Widget	result;
    Arg		args[10];
    int		n;

    n = 0;
    XtSetArg(args[n], XtNmessage, message);                 n++;
    XtSetArg(args[n], XtNleftLabel, left);                  n++;
    XtSetArg(args[n], XtNmiddleLabel, middle);              n++;
    XtSetArg(args[n], XtNrightLabel, right);                n++;
    XtSetArg(args[n], XtNtimeout, timeout);                 n++;
    XtSetArg(args[n], XtNcolormap, global.cmap);            n++;
    XtSetArg(args[n], XtNvisual, global.visual);            n++;
    XtSetArg(args[n], XtNdepth, global.depth);              n++;
    XtSetArg(args[n], XtNtransientFor, main_widgets.shell); n++;

    result =
	XtCreatePopupShell(name, noticeWidgetClass,
			   main_widgets.shell, args, n);
    XtRealizeWidget(result);
    if (!callback)
	callback = notice_callback;
    XtAddCallback(result, XtNcallback, callback, client_data);

    popup_under_pointer(result, grab);

    return result;
}

Widget popup_dialogue(char		*name,
		      char		*message,
		      char		*left,
		      char		*middle,
		      char		*right,
		      XtCallbackProc	 callback,
		      XtPointer		 client_data,
		      XtGrabKind	 grab)
{
    Widget	result;
    Arg		args[10];
    int		n;

    n = 0;
    XtSetArg(args[n], XtNmessage, message);                 n++;
    XtSetArg(args[n], XtNleftLabel, left);                  n++;
    XtSetArg(args[n], XtNmiddleLabel, middle);              n++;
    XtSetArg(args[n], XtNrightLabel, right);                n++;
    XtSetArg(args[n], XtNcolormap, global.cmap);            n++;
    XtSetArg(args[n], XtNvisual, global.visual);            n++;
    XtSetArg(args[n], XtNdepth, global.depth);              n++;
    XtSetArg(args[n], XtNtransientFor, main_widgets.shell); n++;

    result =
	XtCreatePopupShell(name, dialogueWidgetClass,
			   main_widgets.shell, args, n);
    XtRealizeWidget(result);
    if (callback)
	XtAddCallback(result, XtNcallback, callback, client_data);

    popup_under_pointer(result, grab);

    return result;
}

void popup_colornotice(int bad_color)
{
    if (bad_color && global.bell)
	XBell(display, 0);
    popup_notice("colornotice",
		 bad_color ? "Error: No such color." :
		 "Warning: Cannot allocate colormap entry.",
		 "OK", NULL, NULL, 2000, NULL, NULL, XtGrabNone);
}

void popup_regexpnotice(int errcode, const regex_t *re)
{
    char	buffer[1024];
    long	len;

    strcpy(buffer, "Parse error in regexp:\n\n");
    len = strlen(buffer);

    regerror(errcode, re, buffer + len, sizeof buffer - len);

    popup_notice("regexpnotice", buffer, "OK", NULL, NULL, 3000,
		 NULL, NULL, XtGrabNone);

    
}

void popup_title_notice(char *title, char *body, int beep)
{
    char	buffer[1024];

    if (!title)
	buffer[0] = '\0';
    else {
	strcpy(buffer, title);
	strcat(buffer, ":\n\n");
    }
    strncat(buffer, body, 500);
    popup_notice("notice", buffer, "OK", NULL, NULL, 10000,
		 NULL, NULL, XtGrabNone);
    if (beep && global.bell)
	XBell(display, 0);
}

void stderr_popup(char *stderr_buf, long timeout)
{
    if (timeout < 0)
	timeout = global.stderr_timeout;

    if (stderr_buf && stderr_buf[0] != '\0' && timeout >= 0)
	popup_notice("stderr", stderr_buf, "Ok", NULL, NULL,
		     timeout, NULL, NULL, XtGrabNone);
}

/*************************************************************************/

Widget create_simple_menu(Widget parent, char *prefix, int size,
			  XtCallbackProc callback, void *client_data)
{
    char	name[128];
    Widget	shell, menu, temp;
    Arg		args[4];
    int		i;

    if (size <= 0)
	return NULL;

    XtSetArg(args[0], XtNcolormap, global.cmap);
    XtSetArg(args[1], XtNvisual, global.visual);
    XtSetArg(args[2], XtNdepth, global.depth);
    sprintf(name, "%sshell", prefix);
    shell = XtCreatePopupShell(name, menuShellWidgetClass, parent, args, 3);
    sprintf(name, "%smenu", prefix);
    menu = XtCreateManagedWidget(name, menuWidgetClass, shell, NULL, 0);

    for (i = 0 ; i < size ; i++) {
	sprintf(name, "%s%d", prefix, i);
	temp = MenuCreateGadget(name, stringGadgetClass, menu, NULL, 0);
	XtAddCallback(temp, XtNcallback, callback, client_data);
    }

    return shell;
}

