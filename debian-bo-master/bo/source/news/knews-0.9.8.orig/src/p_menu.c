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
#include "cache.h"
#include "codes.h"
#include "connect.h"
#include "file.h"
#include "p_I.h"
#include "p_menu.h"
#include "p_popup.h"
#include "p_post.h"
#include "p_setup.h"
#include "resource.h"
#include "server.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/ArtText.h"
#include "../Widgets/Dialogue.h"
#include "../Widgets/Menu.h"
#include "../Widgets/MenuG.h"
#include "../Widgets/MenuShell.h"
#include "../Widgets/Notice.h"
#include "../Widgets/SeparatorG.h"
#include "../Widgets/StringG.h"
#include "../Widgets/ToggleG.h"
#include "../Widgets/PullRight.h"

static Widget		include_toggle, quote_sig_toggle;

static int generic_post_check(int needs_art, FILE **fp, char **file_name)
{
    if (global.busy)
	XBell(display, 0);
    else if (!global.domain_name)
	set_message("Domain name not available, posting will not be allowed!",
		    True);
    else if (!global.user_id)
	set_message("User id not available, posting will not be allowed!",
		    True);
    else if (needs_art &&
	     global.mode != NewsModeGroup &&
	     global.mode != NewsModeThread)
	set_message("Not in a newsgroup!", True);
    else if (needs_art && !global.curr_art)
	set_message("No selected article!", True);
    else if (needs_art && !global.curr_art->from)
	set_message("That's a fake article!", True);
    else if (fp && !(*fp = create_temp_file(file_name)))
	set_message("Failed to create temporary file!", True);
    else
	return True;

    return False;
}

static void forward_dialogue_callback(Widget w,
				      XtPointer client_data,
				      XtPointer call_data)
{
    DialogueReport	*report = (DialogueReport *)call_data;
    String		param_buf[2];
    String		*params = param_buf;
    Cardinal		no_params;

    XtPopdown(w);
    XtDestroyWidget(w);

    if (!report || (report->reply == DialogueReplyClose ||
		    report->reply == DialogueReplyRight))
	return;

    if (report->buffer)
	param_buf[0] = report->buffer;
    else
	param_buf[0] = "";
    if (report->reply != DialogueReplyMiddle)
	no_params = 1;
    else {
	no_params = 2;
	param_buf[1] = "edit";
    }

    action_forward_by_mail(NULL, NULL, params, &no_params);
}

static void post_menu_callback(Widget gw,
			       XtPointer client_data,
			       XtPointer call_data)
{
    PostContext	*context;
    int		flags = (long)client_data;
    FILE	*fp          = NULL;
    char	*file_name   = NULL;
    int		quote        = ToggleGadgetGet(include_toggle);
    int		quote_sig    = ToggleGadgetGet(quote_sig_toggle);

    if (!generic_post_check(True, &fp, &file_name))
	return;

    context = create_post_context(flags, file_name);
    post_setup(context, fp, global.curr_art, quote, quote_sig, False);
    fclose(fp);
}

static void post_new_callback(Widget w,
			      XtPointer client_data,
			      XtPointer call_data)
{
    PostContext *context;
    char        *file_name = NULL;
    FILE        *fp        = NULL;
    char	*full_name = res_full_name();

    if (!generic_post_check(False, &fp, &file_name))
	return;

    context = create_post_context(POST, file_name);

    if (!full_name)
	full_name = "";

    context->line = 1 + insert_extra_headers(fp, NULL);
    fprintf(fp,
	    "From: %s@%s (%s)\n"
	    "Subject: \n"
	    "Newsgroups: %s\n"
	    "\n",
	    global.mail_name, global.domain_name, full_name,
	    (global.mode == NewsModeGroup ||
	     global.mode == NewsModeThread) ?
	    global.curr_group->name : "");
    context->line++;
    append_signature(fp);
    fclose(fp);
    fork_editor(context);
}

static void fwd_default_callback(Widget		w,
				 XtPointer	client_data,
				 XtPointer	call_data)
{
    if (!generic_post_check(True, NULL, NULL))
	return;

    popup_dialogue("forward", "Forward article to:", "Mail", "Edit", "Cancel",
		   forward_dialogue_callback, NULL, XtGrabExclusive);
}

static void fwd_menu_callback(Widget	w,
			      XtPointer	client_data,
			      XtPointer	call_data)
{
    char	*to     = (char *)call_data;
    Cardinal	no_params;
    String	params[2];

    if (!to) {
	XBell(display, 0);
	return;
    }

    params[0] = to;
    params[1] = StringGadgetCommand(w);
    no_params = params[1] ? 2 : 1;
    action_forward_by_mail(w, NULL, params, &no_params);
}

static void cancel_post_callback(Widget w,
				 XtPointer client_data,
				 XtPointer call_data)
{
    PostContext	*context;
    FILE	*fp        = NULL;
    char	*file_name = NULL;
    char	*full_name = res_full_name();

    if (!generic_post_check(True, &fp, &file_name))
	return;

    context = create_post_context(POST, file_name);
    context->line = insert_extra_headers(fp, global.curr_art);
    fprintf(fp,
	    "From: %s@%s (%s)\n"
	    "Subject: cmsg cancel <%s>\n"
	    "Control: cancel <%s>\n"
	    "Newsgroups: %s\n"
	    "\n"
	    "Article canceled from within knews.  [put reason here]\n",
	    global.mail_name, global.domain_name,
	    full_name ? full_name : "",
	    global.curr_art->msgid,
	    global.curr_art->msgid,
	    global.curr_group->name);
    context->line += 6;
    fclose(fp);
    fork_editor(context);
}

static void supersede_callback(Widget w,
			       XtPointer client_data,
			       XtPointer call_data)
{
    PostContext	*context;
    FILE	*fp          = NULL;
    char	*file_name   = NULL;
    int		quote        = ToggleGadgetGet(include_toggle);
    int		quote_sig    = ToggleGadgetGet(quote_sig_toggle);

    if (!generic_post_check(True, &fp, &file_name))
	return;

    context = create_post_context(POST, file_name);
    post_setup(context, fp, global.curr_art, quote, quote_sig, True);
    fclose(fp);
}

/*************************************************************************/

void create_post_menu(Widget main_shell)
{
    Widget	post_menu, menu, temp, fwd;
    Arg		args[4];

    XtSetArg(args[0], XtNcolormap, global.cmap);
    XtSetArg(args[1], XtNvisual, global.visual);
    XtSetArg(args[2], XtNdepth, global.depth);
    post_menu =
	XtCreatePopupShell("postshell", menuShellWidgetClass,
			   main_shell, args, 3);
    menu = XtCreateManagedWidget("postmenu", menuWidgetClass,
				 post_menu, NULL, 0);

    temp = MenuCreateGadget("followup", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback, post_menu_callback, (XtPointer)POST);

    temp = MenuCreateGadget("mailreply", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback, post_menu_callback, (XtPointer)MAIL);

    temp = MenuCreateGadget("followupreply", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback,
		  post_menu_callback, (XtPointer)(POST | MAIL));

    temp = MenuCreateGadget("postnew", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback, post_new_callback, NULL);

    temp = MenuCreateGadget("cancel", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback, cancel_post_callback, NULL);

    temp = MenuCreateGadget("supersede", stringGadgetClass, menu, NULL, 0);
    XtAddCallback(temp, XtNcallback, supersede_callback, NULL);

    if (global.forward_menu_size <= 0)
	fwd = MenuCreateGadget("forward", stringGadgetClass, menu, NULL, 0);
    else {
	XtSetArg(args[0], XtNmenuName, "forwardshell");
	fwd = MenuCreateGadget("forward", pullRightGadgetClass, menu, args, 1);
	create_simple_menu(menu, "forward", global.forward_menu_size,
			   fwd_menu_callback, NULL);
    }
    XtAddCallback(fwd, XtNpostPopdownCallback, fwd_default_callback, NULL);

    MenuCreateGadget("separator", separatorGadgetClass, menu, NULL, 0);
    include_toggle =
	MenuCreateGadget("quotetoggle", toggleGadgetClass, menu, NULL, 0);
    quote_sig_toggle =
	MenuCreateGadget("quotesig", toggleGadgetClass, menu, NULL, 0);
}

static void do_incl(String *params, Cardinal *no_params)
{
    if (*no_params > 0) {
	if (case_lstrcmp(params[0], "true") == 0 ||
	    case_lstrcmp(params[0], "yes"))
	    ToggleGadgetSet(include_toggle, True);
	else if (case_lstrcmp(params[0], "false") == 0 ||
		 case_lstrcmp(params[0], "no") == 0)
	    ToggleGadgetSet(include_toggle, False);
    }
}

void action_followup(Widget w, XEvent *event,
		     String *params, Cardinal *no_params)
{
    int	old = ToggleGadgetGet(include_toggle);

    do_incl(params, no_params);
    post_menu_callback(w, (XtPointer)POST, NULL);
    ToggleGadgetSet(include_toggle, old);
}

void action_reply(Widget w, XEvent *event,
		  String *params, Cardinal *no_params)
{
    int	old = ToggleGadgetGet(include_toggle);

    do_incl(params, no_params);
    post_menu_callback(w, (XtPointer)MAIL, NULL);
    ToggleGadgetSet(include_toggle, old);
}

void action_followup_and_reply(Widget w, XEvent *event,
			       String *params, Cardinal *no_params)
{
    int	old = ToggleGadgetGet(include_toggle);

    do_incl(params, no_params);
    post_menu_callback(w, (XtPointer)(POST|MAIL), NULL);
    ToggleGadgetSet(include_toggle, old);
}

void action_post_new(Widget w, XEvent *event,
		     String *params, Cardinal *no_params)
{
    post_new_callback(w, NULL, NULL);
}

void action_forward_by_mail(Widget w, XEvent *event,
			    String *params, Cardinal *no_params)
{
    PostContext		*context;
    ARTICLE		*art;
    SERVER		*server;
    FILE		*fp        = NULL;
    char		*file_name = NULL;
    char		*full_name = res_full_name();
    char		*to  = *no_params <= 0 ? NULL : params[0];
    int			edit;
    char		*eol, *buffer;
    int			ok;

    if (*no_params <= 1)
	edit = False;
    else {
	int	n = strlen(params[1]);

	if (case_lstrncmp(params[1], "edit", n) == 0)
	    edit = True;
	else if (case_lstrncmp(params[1], "mail", n) == 0)
	    edit = False;
	else {
	    fprintf(stderr,
		    "knews: bad second parameter to forward-by-mail() \n"
		    "       action procedure: '%s'\n", params[1]);
	    return;
	}
    }

    eol = edit ? "\n" : "\r\n";

    if (!generic_post_check(True, &fp, &file_name))
	return;

    context = create_post_context(MAIL | ORIG_MAIL, file_name);
    art = global.curr_art;
    if (!edit && context->file_name)
	unlink(context->file_name);

    if (!full_name)
	full_name = "";

    fprintf(fp,
	    "X-Newsreader: knews " KNEWS_VERSION "%s"
	    "From: %s@%s (%s)%s"
	    "Subject: %s%s (fwd)%s"
	    "To: %s%s",
	    eol,
	    global.mail_name, global.domain_name, full_name, eol,
	    PARENT(art) ? "Re: " : "", art->subject->subject, eol,
	    to ? to : "", eol);
    context->line += 4;

    if (global.mime_forward) {
	fprintf(fp,
		"MIME-Version: 1.0%s"
		"Content-Type: message/rfc822%s"
		"Content-Transfer-Encoding: 8bit%s"
		"%s",
		eol, eol, eol, eol);
	context->line += 4;
    } else {
	fprintf(fp, "%s---Forwarded message---%s", eol, eol);
	context->line += 2;
    }

    set_busy(True);

    server = cache_get_server(art->no, False);
    if (!server) {
	char	command[64];

	server = main_server;
	sprintf(command, "ARTICLE %ld\r\n", art->no);
	buffer = server_comm(server, command, True);
	if (!buffer || atoi(buffer) != NNTP_OK_ARTICLE) {
	    if (!buffer)
		reconnect_server(True);
	    else
		popup_title_notice("Couldn't retrieve article, \n"
				   "message from server is", buffer, True);
	    unset_busy();
	    fclose(fp);
	    free_post_context(context);
	    return;
	}
    }

    while ((buffer = server_read(server)) && !IS_DOT(buffer)) {
	if (*buffer == '.' && !edit)
	    buffer++;
	fprintf(fp, "%s%s", buffer, eol);
    }

    if (server != main_server)
	server_free(server);
    else if (!buffer) {
	fclose(fp);
	reconnect_server(True);
	unset_busy();
	free_post_context(context);
	return;
    }

    unset_busy();

    if (fflush(fp) < 0) {
	set_message("Error with file!", True);
	free_post_context(context);
	fclose(fp);
	return;
    }

    if (edit) {
	fclose(fp);
	fork_editor(context);
	return;
    }

    set_message("Mailing...", False);
    ok = post_to_agent(MAIL_COMMAND, fp);
    if (ok)
	set_message("Mailing was successful.", False);
    else
	set_message("Mail failed!", True);

    fclose(fp);
    free_post_context(context);

}
