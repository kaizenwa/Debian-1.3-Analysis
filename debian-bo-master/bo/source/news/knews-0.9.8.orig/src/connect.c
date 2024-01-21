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
#include "child.h"
#include "codes.h"
#include "connect.h"
#include "expand.h"
#include "ftp.h"
#include "k_file.h"
#include "newsrc.h"
#include "resource.h"
#include "file.h"
#include "server.h"
#include "sysdeps.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/Compat.h"
#include "../Widgets/Dialogue.h"
#include "../Widgets/Notice.h"
#include "../Widgets/Util.h"

static int nntp_init(void)
{
    char	message[256];
    char	*buffer;
    int		status;

    buffer = server_read(main_server);
    if (!buffer) {
	if (server_aborted(main_server))
	    set_message("Aborted.", True);
	else
	    set_message("Error: Connection to server broken!", True);
	return False;
    }
    status = atoi(buffer);
    if (status != NNTP_OK_CANPOST && status != NNTP_OK_NOPOST) {
	if (strlen(buffer) > 100)
	    buffer[100] = '\0';
	sprintf(message, "Error: Message from server is: %s", buffer);
	set_message(message, True);
	return False;
    }
    global.posting_allowed = (status == NNTP_OK_CANPOST);

    buffer = server_comm(main_server, "MODE READER\r\n", False);
    if (!buffer) {
	if (server_aborted(main_server))
	    set_message("Aborted.", True);
	else
	    set_message("Error: Connection to server broken!", True);
	return False;
    }
    status = atoi(buffer);
    if (status == NNTP_OK_CANPOST)
	global.posting_allowed = True;
    else if (status == NNTP_OK_NOPOST)
	global.posting_allowed = False;
    else if (status == NNTP_ERR_ACCESS) {
	if (strlen(buffer) > 100)
	    buffer[100] = '\0';
	sprintf(message, "Error: Message from server is: %s", buffer);
	set_message(message, True);
	return False;
    }

    buffer = server_comm(main_server, "XOVER\r\n", False);
    if (!buffer) {
	if (server_aborted(main_server))
	    set_message("Aborted.", True);
	else
	    set_message("Error: Connection to server broken!", True);
	return False;
    }
    status = atoi(buffer);
    if (status == NNTP_ERR_COMMAND)
	global.xover_supported = False;
    else {
	global.xover_supported = True;
	if (status == NNTP_OK_XOVER) {
	    while (buffer && !IS_DOT(buffer))
		buffer = server_read(main_server);
	    if (!buffer) {
		if (server_aborted(main_server))
		    set_message("Aborted.", True);
		else
		    set_message("Error: Connection to server broken!", True);
		return False;
	    }
	}
    }

    if (global.head_debug)
	global.xover_supported = False;

    return True;
}

static char *get_server_command(char *res_name)
{
    XrmName	name_list[3];
    XrmClass	class_list[3];
    XrmQuark	rep;
    XrmDatabase	db;
    XrmValue	val;

    while (*res_name == ' ' ||
	   *res_name == '\t')
	res_name++;

    if (*res_name == '\0' ||
	strchr(res_name, '.') ||
	strchr(res_name, '*') ||
	strchr(res_name, ' ') ||
	strchr(res_name, '\t') ||
	strchr(res_name, '/'))
	return NULL;

    class_list[0] = XrmPermStringToQuark("Knews");
    class_list[1] = XrmPermStringToQuark("Server");
    class_list[2] = NULLQUARK;

    name_list[0] = XrmStringToQuark(XtName(main_widgets.shell));
    name_list[1] = XrmStringToQuark(res_name);
    name_list[2] = NULLQUARK;

    db = XtScreenDatabase(XtScreen(main_widgets.shell));

    if (!XrmQGetResource(db, name_list, class_list, &rep, &val))
	return NULL;

    return (char *)val.addr;
}

void connect_server(void)
{
    char	*path;
    int		status = -1;
    int		read_active;

    set_busy(True);
    suspend_child_contexts();

    server_close(main_server);
    if (global.nntp_server[0] != '#') {
	set_message("Looking up hostname...", False);
	global.serv_addr = get_host(global.nntp_server, NNTP_PORT, True);
	if (!global.serv_addr)
	    set_message("Error!  No such host!", True);
	else
	    status = server_open(main_server, global.serv_addr, 2);
    } else {
	char	*cmd = get_server_command(global.nntp_server);

	if (cmd)
	    status = server_fork(main_server, cmd, 2);
	else
	    set_message("Error!  No server command specified!", True);
    }

    if (status < 0) {
	if (server_aborted(main_server))
	    set_message("Aborted!", True);
	unset_busy();
	resume_child_contexts();
	return;
    }

    path = expand_path(global.config_file);
    if (!path)
	res_load(NULL);
    else {
	if (res_load(path) < 0)
	    perror(path);
	XtFree(path);
    }

    (void)ftp_get();

    status = nntp_init();
    if (!status) {
	global_cleanup(False, False);
	unset_busy();
	resume_child_contexts();
	return;
    }

    read_active = res_read_active_file();
    if (!read_active) {
	if (global.posting_allowed)
	    set_message("Connected to server; "
			"retrieving groups from newsrc...", False);
	else
	    set_message("Connected to server, posting not allowed; "
			"retrieving groups from newsrc...", False);
	status = get_newsgroups_from_newsrc();
	if (status != NNTP_OK_GROUPS && status != -1)
	    read_active = True;
    }

    if (read_active) {
	if (global.posting_allowed)
	    set_message("Connected to server; reading active file...", False);
	else
	    set_message("Connected to server, posting not allowed; "
			"reading active file", False);

	status = get_newsgroups();
    }

    if (status == NNTP_OK_GROUPS && get_descriptions() < 0)
	status = -1;

    if (read_active && status == NNTP_OK_GROUPS) {
	set_message("Reading newsrc file...", False);
	parse_newsrc(False);
    }

    if (status == NNTP_OK_GROUPS) {
	set_message("Reading global kill file...", False);
	read_global_kill_file();

	if (res_check_for_new_groups()) {
	    set_message("Checking for new groups...", False);
	    status = check_for_new_groups();
	    if (status < 0) {
		if (server_aborted(main_server))
		    set_message("Aborted.", True);
		else
		    set_message("Connection to server broken!", True);
		global_cleanup(False, False);
		unset_busy();
		resume_child_contexts();
		return;
	    }
	}

	if (global.new_groups)
	    setNewsModeNewgroups();
	else
	    setNewsModeConnected();

	thread_ahead_init();
    } else {
	if (status < 0)
	    if (server_aborted(main_server))
		set_message("Aborted.", True);
	    else
		set_message("Connection to server broken!", True);
	else
	    if (read_active)
		set_message("Failed to read active file!", True);
	    else
		set_message("Failed to read newsrc file!", True);
	global_cleanup(False, False);
    }

    unset_busy();
    resume_child_contexts();
    add_rescan_timeout();
}

int reconnect_server(int cleanup_on_error)
{
    int		status = -1;

    if (server_aborted(main_server))
	set_message("Aborted, reconnecting to server...", True);
    else
	set_message("Reconnecting to server...", False);
    suspend_child_contexts(); /* may happen twice */

    server_close(main_server);
    if (global.nntp_server[0] != '#') {
	int	i = 4;

	status = server_open(main_server, global.serv_addr, 1);
	for (i = 0 ; status < 0 && i < 4 && timed_out(errno) ; i++) {
	    set_message("Connection timed out, still trying...", True);
	    status = server_open(main_server, global.serv_addr, 1);
	}
    } else {
	char	*cmd = get_server_command(global.nntp_server);

	if (cmd)
	    status = server_fork(main_server, cmd, 2);
	else
	    set_message("Error!  No server command specified!", True);
    }

    if (status < 0) {
	if (server_aborted(main_server))
	    set_message("Aborted!", True);
	/* else server_open will have done it */
	resume_child_contexts();
	if (cleanup_on_error)
	    global_cleanup(True, False);
	return -1;
    }

    status = nntp_init();
    if (!status) {
	resume_child_contexts();
	if (cleanup_on_error)
	    global_cleanup(True, False);
	/* nntp_init will have set_message */
	return -1;
    }

    if (global.mode == NewsModeGroup || global.mode == NewsModeThread) {
	char	message[512];
	char	*buffer;

	set_message("Reentering newsgroup...", False);
	sprintf(message, "GROUP %s\r\n", global.curr_group->name);
	buffer = server_comm(main_server, message, False);
	if (!buffer) {
	    set_message("Failed to reenter to server!", True);
	    resume_child_contexts();
	    if (cleanup_on_error)
		global_cleanup(True, False);
	    return -1;
	}

	status = atoi(buffer);
	if (status != NNTP_OK_GROUP)
	    setNewsModeConnected();
    }

    if (cleanup_on_error)
	set_standard_message();
    resume_child_contexts();

    return 0;
}

/*************************************************************************/

static void connect_dialogue_callback(Widget w,
				      XtPointer client_data,
				      XtPointer call_data)
{
    DialogueReport	*report = (DialogueReport *)call_data;
    Arg			arg;

    if (global.busy)
	return;

    switch (report->reply) {
    case DialogueReplyMiddle:
	XtSetArg(arg, XtNbuffer, "");
	XtSetValues(w, &arg, 1);
	break;
    case DialogueReplyLeft:
    case DialogueReplyEnter:
	XtFree(global.nntp_server);
	global.nntp_server = NULL;
	XtPopdown(w);
	if (report->buffer && report->buffer[0] != '\0') {
	    global.nntp_server = XtNewString(report->buffer);
	    connect_server();
	}
	break;
    case DialogueReplyRight:
    case DialogueReplyClose:
	XtPopdown(w);
	break;
    case DialogueReplyTab:
	break;
    }
}

static Widget connect_dialogue = NULL;

void popup_connect_dialogue(void)
{
    if (!connect_dialogue)
	connect_dialogue =
	    popup_dialogue("connect", "Connect to nntpserver:",
			   "Connect", "Clear", "Cancel",
			   connect_dialogue_callback, NULL, XtGrabNone);
    else if (!is_popped_up(connect_dialogue))
	popup_under_pointer(connect_dialogue, XtGrabNone);
}
