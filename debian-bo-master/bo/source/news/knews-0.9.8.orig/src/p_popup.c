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
#include <sys/wait.h>
#include "child.h"
#include "file.h"
#include "mailcap.h"
#include "p_I.h"
#include "p_attach.h"
#include "p_check.h"
#include "p_popup.h"
#include "p_post.h"
#include "p_setup.h"
#include "parse.h"
#include "resource.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/ArtText.h"
#include "../Widgets/Knapp.h"
#include "../Widgets/FileSel.h"
#include "../Widgets/Layout.h"
#include "../Widgets/Manager.h"
#include "../Widgets/MenuKnapp.h"
#include "../Widgets/Message.h"
#include "../Widgets/Sash.h"
#include "../Widgets/Scrollable.h"
#include "../Widgets/ScrBar.h"
#include "../Widgets/ScrList.h"
#include "../Widgets/StringG.h"
#include "../Widgets/TextField.h"
#include "../Widgets/Toggle.h"
#include "../Widgets/Util.h"

typedef struct PostWidgets {
    Widget	shell;
    Widget	text;
    Widget	message;
    Widget	post_knapp;
    Widget	edit_knapp;
    Widget	misc_knapp;
    Widget	attach_knapp;
    Widget	detach_knapp;
    Widget	cancel_knapp;
    /* */
    Widget	list_manager;
    Widget	list;
    /* */
    Widget	type_field;
    Widget	inline_toggle;
    Widget	attach_toggle;
    Widget	name_field;
    Widget	descr_field;
    Widget	type_knapp;
    /* */
    Widget	none_toggle;
    Widget	base64_toggle;
    Widget	uue_toggle;
    Widget	qp_toggle;
    /* */
    Widget	file_sel;
} PostWidgets;

static void post_knapp_callback(Widget, XtPointer, XtPointer);
static void edit_knapp_callback(Widget, XtPointer, XtPointer);
static void attach_knapp_callback(Widget, XtPointer, XtPointer);
static void detach_knapp_callback(Widget, XtPointer, XtPointer);
static void cancel_knapp_callback(Widget, XtPointer, XtPointer);
static void attach_list_callback(Widget, XtPointer, XtPointer);
static void tab_callback(Widget, XtPointer, XtPointer);
static void type_field_callback(Widget, XtPointer, XtPointer);
static void name_field_callback(Widget, XtPointer, XtPointer);
static void descr_field_callback(Widget, XtPointer, XtPointer);
static void disp_toggle_callback(Widget, XtPointer, XtPointer);
static void enc_toggle_callback(Widget, XtPointer, XtPointer);
static void misc_menu_callback(Widget, XtPointer, XtPointer);

void destroy_post_widgets(PostWidgets *w)
{
    if (is_popped_up(w->shell))
	XtPopdown(w->shell);
    XtDestroyWidget(w->shell);
}

static void create_post_widgets(PostContext *context)
{
    PostWidgets	*w;
    Widget	layout, hbar, vbar, mgr;
    Arg		args[8];

    w = (PostWidgets *)XtMalloc(sizeof *w);
    context->widgets = w;
    w->file_sel = NULL;

    XtSetArg(args[0], XtNcolormap, global.cmap);
    XtSetArg(args[1], XtNvisual, global.visual);
    XtSetArg(args[2], XtNdepth, global.depth);
    XtSetArg(args[3], XtNinput, True);
    w->shell = XtCreatePopupShell("postpopup", topLevelShellWidgetClass,
				  main_widgets.shell, args, 4);

    layout = XtVaCreateManagedWidget("postlayout", layoutWidgetClass,
				     w->shell,
				     XtVaTypedArg, XtNlayout, XtRString,
#include "layouts/post.h"
				     (int)sizeof(String),
				     (void *)0);

    XtCreateManagedWidget("posttitle", messageWidgetClass, layout, NULL, 0);

    mgr = XtCreateManagedWidget("posttextmgr", managerWidgetClass,
				layout, NULL, 0);

    XtSetArg(args[0], XtNvertical, False);
    hbar = XtCreateManagedWidget("texthbar", scrBarWidgetClass,
				 layout, args, 1);
    vbar = XtCreateManagedWidget("textvbar", scrBarWidgetClass,
				 layout, NULL, 0);
    XtSetArg(args[0], XtNvBar, vbar);
    XtSetArg(args[1], XtNhBar, hbar);
    w->text =
	XtCreateManagedWidget("posttext", artTextWidgetClass, mgr, args, 2);
    XtSetArg(args[0], XtNbuffer, "");
    XtSetArg(args[1], XtNcenter, False);
    w->message = XtCreateManagedWidget("postmessage", messageWidgetClass,
				       layout, args, 2);
    w->post_knapp = XtCreateManagedWidget("post", knappWidgetClass,
					  layout, NULL, 0);
    XtAddCallback(w->post_knapp, XtNcallback,
		  post_knapp_callback, (XtPointer)context);
    w->edit_knapp = XtCreateManagedWidget("edit", knappWidgetClass,
					  layout, NULL, 0);
    XtAddCallback(w->edit_knapp, XtNcallback,
		  edit_knapp_callback, (XtPointer)context);
    w->attach_knapp = XtCreateManagedWidget("attach", knappWidgetClass,
					    layout, NULL, 0);
    XtAddCallback(w->attach_knapp, XtNcallback,
		  attach_knapp_callback, (XtPointer)context);
    w->detach_knapp = XtCreateManagedWidget("detach", knappWidgetClass,
					    layout, NULL, 0);
    XtAddCallback(w->detach_knapp, XtNcallback,
		  detach_knapp_callback, (XtPointer)context);
    w->cancel_knapp = XtCreateManagedWidget("cancel", knappWidgetClass,
					    layout, NULL, 0);
    XtAddCallback(w->cancel_knapp, XtNcallback,
		  cancel_knapp_callback, (XtPointer)context);
    XtSetArg(args[0], XtNmenuName, "miscshell");
    w->misc_knapp = XtCreateManagedWidget("misc", menuKnappWidgetClass,
					  layout, args, 1);

    XtCreateManagedWidget("attachtitle", messageWidgetClass,
			  layout, NULL, 0);
    w->list_manager =
	XtCreateManagedWidget("manager", managerWidgetClass,
			      layout, NULL, 0);

    vbar = XtCreateManagedWidget("listvbar", scrBarWidgetClass,
				 layout, NULL, 0);
    XtSetArg(args[0], XtNvertical, False);
    hbar = XtCreateManagedWidget("listhbar", scrBarWidgetClass,
				 layout, args, 1);
    XtSetArg(args[0], XtNatMostOne, True);
    XtSetArg(args[1], XtNallowDnd, False);
    XtSetArg(args[2], XtNcontainHoriz, False);
    XtSetArg(args[3], XtNcontainVert, True);
    XtSetArg(args[4], XtNhBar, hbar);
    XtSetArg(args[5], XtNvBar, vbar);
    w->list = XtCreateManagedWidget("attachlist", scrListWidgetClass,
				    w->list_manager, args, 6);
    XtAddCallback(w->list, XtNselectCallback,
		  attach_list_callback, (XtPointer)context);

    XtCreateManagedWidget("typetitle", messageWidgetClass,
			  layout, NULL, 0);
    XtCreateManagedWidget("nametitle", messageWidgetClass,
			  layout, NULL, 0);
    XtCreateManagedWidget("descrtitle", messageWidgetClass,
			  layout, NULL, 0);
    XtCreateManagedWidget("disptitle", messageWidgetClass,
			  layout, NULL, 0);
    XtCreateManagedWidget("enctitle", messageWidgetClass,
			  layout, NULL, 0);

    XtSetArg(args[0], XtNfocusRoot, w->shell);
    XtSetArg(args[1], XtNsingleLine, True);
    w->type_field = XtCreateManagedWidget("typefield", textFieldWidgetClass,
					  layout, args, 2);
    XtAddCallback(w->type_field, XtNcallback,
		  type_field_callback, (XtPointer)context);
    XtAddCallback(w->type_field, XtNtabCallback,
		  tab_callback, (XtPointer)context);
    w->name_field = XtCreateManagedWidget("namefield", textFieldWidgetClass,
					  layout, args, 2);
    XtAddCallback(w->name_field, XtNcallback,
		  name_field_callback, (XtPointer)context);
    XtAddCallback(w->name_field, XtNtabCallback,
		  tab_callback, (XtPointer)context);
    w->descr_field = XtCreateManagedWidget("descrfield", textFieldWidgetClass,
					  layout, args, 2);
    XtAddCallback(w->descr_field, XtNcallback,
		  descr_field_callback, (XtPointer)context);
    XtAddCallback(w->descr_field, XtNtabCallback,
		  tab_callback, (XtPointer)context);

    w->inline_toggle =
	XtCreateManagedWidget("inlinetoggle", toggleWidgetClass,
			      layout, NULL, 0);
    XtAddCallback(w->inline_toggle, XtNcallback,
		  disp_toggle_callback, (XtPointer)context);
    w->attach_toggle =
	XtCreateManagedWidget("attachtoggle", toggleWidgetClass,
			      layout, NULL, 0);
    XtAddCallback(w->attach_toggle, XtNcallback,
		  disp_toggle_callback, (XtPointer)context);

    w->none_toggle =
	XtCreateManagedWidget("nonetoggle", toggleWidgetClass,
			      layout, NULL, 0);
    XtAddCallback(w->none_toggle, XtNcallback,
		  enc_toggle_callback, (XtPointer)context);
    w->base64_toggle =
	XtCreateManagedWidget("base64toggle", toggleWidgetClass,
			      layout, NULL, 0);
    XtAddCallback(w->base64_toggle, XtNcallback,
		  enc_toggle_callback, (XtPointer)context);
    w->uue_toggle =
	XtCreateManagedWidget("uuetoggle", toggleWidgetClass,
			      layout, NULL, 0);
    XtAddCallback(w->uue_toggle, XtNcallback,
		  enc_toggle_callback, (XtPointer)context);
    w->qp_toggle =
	XtCreateManagedWidget("qptoggle", toggleWidgetClass,
			      layout, NULL, 0);
    XtAddCallback(w->qp_toggle, XtNcallback,
		  enc_toggle_callback, (XtPointer)context);

    XtSetArg(args[0], XtNmenuName, "typeshell");
    w->type_knapp =
	XtCreateManagedWidget("type", menuKnappWidgetClass,
			      layout, args, 1);
    create_simple_menu(w->type_knapp, "type", global.type_menu_size,
		       type_field_callback, context);

    XtCreateManagedWidget("sash1", sashWidgetClass, layout, NULL, 0);
    XtCreateManagedWidget("sash2", sashWidgetClass, layout, NULL, 0);
    XtCreateManagedWidget("sash3", sashWidgetClass, layout, NULL, 0);
    XtCreateManagedWidget("sash4", sashWidgetClass, layout, NULL, 0);

    create_simple_menu(w->misc_knapp, "misc", global.post_misc_menu_size,
		       misc_menu_callback, context);

    XtRealizeWidget(w->shell);
    add_WM_DELETE_WINDOW_callback(w->shell, cancel_knapp_callback,
				  (XtPointer)context);
}

static void post_set_busy(PostContext *context)
{
    PostWidgets	*w = context->widgets;

    if (context->busy) {
	fputs("knews: internal error: post context already busy.\n", stderr);
	return;
    }

    context->busy = True;
    if (!w)
	return;

    XDefineCursor(display, XtWindow(w->shell), global.busy_cursor);
    KnappSetActive(w->post_knapp, False);
    KnappSetActive(w->edit_knapp, False);
    KnappSetActive(w->misc_knapp, False);
    KnappSetActive(w->attach_knapp, False);
    KnappSetActive(w->detach_knapp, False);
    KnappSetActive(w->cancel_knapp, False);
    KnappSetActive(w->type_knapp, False);
    TextFieldSetActive(w->type_field, False);
    TextFieldSetActive(w->name_field, False);
    TextFieldSetActive(w->descr_field, False);
    XtSetSensitive(w->type_field, False);
    XtSetSensitive(w->name_field, False);
    XtSetSensitive(w->descr_field, False);
    ScrListSetActive(w->list, False);
}

static void post_unset_busy(PostContext *context)
{
    PostWidgets	*w = context->widgets;

    if (!context->busy) {
	fputs("knews: internal error: post context not busy.\n", stderr);
	return;
    }

    context->busy = False;
    if (!w)
	return;

    XDefineCursor(display, XtWindow(w->shell), global.cursor);
    KnappSetActive(w->post_knapp, True);
    KnappSetActive(w->edit_knapp, True);
    KnappSetActive(w->misc_knapp, True);
    KnappSetActive(w->attach_knapp, True);
    KnappSetActive(w->detach_knapp, True);
    KnappSetActive(w->cancel_knapp, True);
    KnappSetActive(w->type_knapp, True);
    TextFieldSetActive(w->type_field, True);
    TextFieldSetActive(w->name_field, True);
    TextFieldSetActive(w->descr_field, True);
    XtSetSensitive(w->type_field, True);
    XtSetSensitive(w->name_field, True);
    XtSetSensitive(w->descr_field, True);
    ScrListSetActive(w->list, True);
}

static void set_ready_message(PostContext*, int);

static void post_set_message(PostContext *context, char *message, int beep)
{
    if (message[0] == '\0')
	set_ready_message(context, beep);
    else
	MessageSetAndRedraw(context->widgets->message, message,
			    beep && global.bell);
}

static void set_post_knapp_label(PostContext *context)
{
    int		label;

    if (!(context->flags & POST) || (context->flags & POST_DONE))
	label = 1;
    else if (!(context->flags & MAIL) || (context->flags & MAIL_DONE))
	label = 0;
    else
	label = 2;

    KnappSetLabelNo(context->widgets->post_knapp, label,
		    context->flags & OK_TO_POST);
}

static void set_ready_message(PostContext *context, int beep)
{
    char	*msg;

    if (!(context->flags & OK_TO_POST))
	if (beep)
	    msg = "Error in article/mail!";
	else
	    msg = "Error in article/mail...";
    else {
	beep = False;
	if (!(context->flags & POST) || (context->flags & POST_DONE))
	    msg = "Ready to mail.";
	else if (!(context->flags & MAIL) || (context->flags & MAIL_DONE))
	    msg = "Ready to post.";
	else
	    msg = "Ready to post and mail.";
    }

    post_set_message(context, msg, beep);
}

static void set_enc_toggles(PostWidgets *w,
			    PostAttachment *pa)
{
    int		enc = attach_get_enc(pa);

    ToggleSet(w->none_toggle, enc == MimeEncNone);
    ToggleSet(w->base64_toggle, enc == MimeEncBase64);
    ToggleSet(w->qp_toggle, enc == MimeEncQP);
    ToggleSet(w->uue_toggle, enc == MimeEncUue);
}

static void set_disp_toggles(PostWidgets *w,
			     PostAttachment *pa)
{
    int		is_inline, is_attach;

    is_inline = pa ? attach_is_inline(pa) : False;
    is_attach = pa ? !is_inline : False;

    ToggleSet(w->inline_toggle, is_inline);
    ToggleSet(w->attach_toggle, is_attach);
}

static void set_attach_widgets(PostWidgets *w,
			       PostAttachment *pa)
{
    set_enc_toggles(w, pa);
    set_disp_toggles(w, pa);
    TextFieldSetBuffer(w->type_field, attach_get_type(pa));
    TextFieldSetBuffer(w->name_field, attach_get_name(pa));
    TextFieldSetBuffer(w->descr_field, attach_get_descr(pa));
    TextFieldSetActive(w->type_field, pa != NULL);
    TextFieldSetActive(w->name_field, pa != NULL);
    TextFieldSetActive(w->descr_field, pa != NULL);
    if (!pa)
	XtSetKeyboardFocus(w->shell, NULL);
    XtSetSensitive(w->type_field, pa != NULL);
    XtSetSensitive(w->name_field, pa != NULL);
    XtSetSensitive(w->descr_field, pa != NULL);
    if (pa)
	XtSetKeyboardFocus(w->shell, w->type_field);
    KnappSetSensitive(w->detach_knapp, pa != NULL);
    KnappSetSensitive(w->type_knapp, pa != NULL);
}

static void do_art_check(PostContext *context)
{
    PostWidgets	*w = context->widgets;
    char	*art = NULL;
    int		fd;

    context->flags &= ~OK_TO_POST;
    if (context->art) {
	free((char *)context->art); /* :-( */
	context->art = NULL;
    }

    fd = open(context->file_name, O_RDONLY);
    if (fd < 0)
	perror(context->file_name);
    else {
	art = snarf_file(fd, NULL);
	close(fd);
    }

    context->art = art;
    ArtTextClearLines(context->widgets->text);
    if (!art)
	ArtTextAddLine(context->widgets->text, "Couldn't open file!",
		       NULL, global.alert_pixel);
    else
	check_article_to_post(context, w->text);
    set_post_knapp_label(context);
    set_ready_message(context, True);
    if (!is_popped_up(context->widgets->shell))
	popup_under_pointer(context->widgets->shell, XtGrabNone);
}

/*************************************************************************/

static void post_knapp_callback(Widget w,
				XtPointer client_data,
				XtPointer call_data)
{
    PostContext	*context = (PostContext *)client_data;
    char	*agent = res_posting_agent();
    FILE	*fp = NULL;
    int		ok = True;
    int		do_post, do_mail;

    do_post = (context->flags & POST) && !(context->flags & POST_DONE);
    do_mail = (context->flags & MAIL) && !(context->flags & MAIL_DONE);

    if (global.busy || context->busy  ||
	!(context->flags & OK_TO_POST) ||
	(!do_post && !do_mail)) {
	XBell(display, 0);
	return;
    }

    if (global.mode == NewsModeDisconnected && do_post && !agent) {
	post_set_message(context, "Not connected!", True);
	return;
    }

    fp = dump_art_to_file(context);
    if (!fp) {
	post_set_message(context, "Couldn't create temp file!", True);
	return;
    }

    post_set_busy(context);

    if (do_post) {
	post_set_message(context, "Posting article...", False);
	ok = agent ? post_to_agent(agent, fp) : post_article(fp);
	if (ok)
	    context->flags |= POST_DONE;
	else {
	    post_set_message(context, "Posting failed!", False);
	    set_message("Posting failed!", True);
	}
    }

    if (ok && do_mail) {
	post_set_message(context, "Mailing article...", False);
	ok = post_to_agent(MAIL_COMMAND, fp);

	if (ok)
	    context->flags |= MAIL_DONE;
	else {
	    char	*msg;

	    if (do_post)
		msg = "Article posted, but mailing failed!";
	    else
		msg = "Mailing failed!";

	    post_set_message(context, msg, False);
	    set_message(msg, True);
	}
    }

    post_unset_busy(context);
    fclose(fp);

    if (!ok)
	return;

    if (do_mail && do_post)
	set_message("Post and mail were successful.", False);
    else if (do_post)
	set_message("Post was successful.", False);
    else
	set_message("Mail was successful.", False);

    free_post_context(context);
}

static void edit_knapp_callback(Widget w,
				XtPointer client_data,
				XtPointer call_data)
{
    PostContext	*context = (PostContext *)client_data;

    if (context->busy) {
	XBell(display, 0);
	return;
    }

    post_set_message(context, "Started editor...", False);
    post_set_busy(context);
    fork_editor(context);
}

static void file_sel_callback(Widget gw,
			      XtPointer client_data,
			      XtPointer call_data)
{
    PostContext 	*context = (PostContext *)client_data;
    PostWidgets		*w = context->widgets;
    char		*file_name = (char *)call_data;
    PostAttachment	*pa;
    char		buffer[256];

    if (!context->busy) {
	XBell(display, 0);
	return;
    }

    XtPopdown(w->file_sel);
    post_unset_busy(context);

    if (!file_name) {
	set_ready_message(context, False);
	return;
    }

    pa = create_attachment(file_name, buffer);
    post_set_message(context, buffer, !pa);
    if (pa) {
	int	n = ++context->n_attachments;

	context->attachments =
	    (PostAttachment **)XtRealloc((char *)context->attachments,
					 n * sizeof context->attachments[0]);
	context->attachments[n - 1] = pa;
	print_attach_info(pa, buffer);
	ScrListAddLine(w->list, buffer, None);
	Remanage(w->list_manager);
	ScrListSetSelected(w->list, n - 1, True);
	ScrListMakeVisible(w->list, n - 1);
	set_attach_widgets(w, pa);
    }
}

static void attach_knapp_callback(Widget gw,
				  XtPointer client_data,
				  XtPointer call_data)
{
    PostContext	*context = (PostContext *)client_data;
    PostWidgets	*w = context->widgets;

    if (context->busy) {
	XBell(display, 0);
	return;
    }

    post_set_message(context, "Choose a file...", False);
    post_set_busy(context);

    if (!w->file_sel) {
	Arg	args[4];

	XtSetArg(args[0], XtNcolormap, global.cmap);
	XtSetArg(args[1], XtNvisual, global.visual);
	XtSetArg(args[2], XtNdepth, global.depth);
	w->file_sel = XtCreatePopupShell("filesel", fileSelWidgetClass,
					 w->shell, args, 3);
	XtAddCallback(w->file_sel, XtNcallback,
		      file_sel_callback, (XtPointer)context);
	XtRealizeWidget(w->file_sel);
    }

    popup_under_pointer(w->file_sel, XtGrabNone);
}

static void detach_knapp_callback(Widget gw,
				  XtPointer client_data,
				  XtPointer call_data)
{
    PostContext		*context = (PostContext *)client_data;
    PostWidgets		*w = context->widgets;
    PostAttachment	*pa;
    long		row = ScrListGetFirstSelected(w->list);
    long		n = context->n_attachments;

    if (context->busy || row < 0 || row > n) {
	XBell(display, 0);
	return;
    }

    pa = context->attachments[row];
    free_attachment(pa);
    ScrListDeleteLine(w->list, row);
    Remanage(w->list_manager);
    if (--context->n_attachments <= 0) {
	XtFree((char *)context->attachments);
	context->attachments = NULL;
    } else {
	if (row < n + 1)
	    memmove(context->attachments + row, context->attachments + row + 1,
		    (n - row - 1) * sizeof context->attachments[0]);
    }
    post_set_message(context, "Attachment removed.", False);
    set_attach_widgets(w, NULL);
}

static void cancel_knapp_callback(Widget w,
				  XtPointer client_data,
				  XtPointer call_data)
{
    PostContext	*context = (PostContext *)client_data;

    if (context->busy) {
	XBell(display, 0);
	return;
    }

    set_message("Post/Mail aborted.", True);
    free_post_context(context);
}

static void attach_list_callback(Widget w,
				 XtPointer client_data,
				 XtPointer call_data)
{
    PostContext		*context = (PostContext *)client_data;
    long		row = (long)call_data;
    PostAttachment	*pa;

    if (context->busy)
	return;

    if (row >= 0 && row < context->n_attachments && ScrListGetSelected(w, row))
	pa = context->attachments[row];
    else
	pa = NULL;

    set_attach_widgets(context->widgets, pa);
    set_ready_message(context, False);
}

static void tab_callback(Widget field,
			 XtPointer client_data,
			 XtPointer call_data)
{
    PostContext		*context = (PostContext *)client_data;
    PostWidgets		*w = context->widgets;
    Widget		new = NULL;
    char		*buffer = NULL;
    PostAttachment	*pa;
    long		row;

    row = ScrListGetFirstSelected(w->list);
    if (row < 0 || row >= context->n_attachments)
	pa = NULL;
    else
	pa = context->attachments[row];

    if (field == w->type_field) {
	new = w->descr_field;
	buffer = attach_get_type(pa);
    } else if (field == w->descr_field) {
	new = w->name_field;
	buffer = attach_get_descr(pa);
    } else if (field == w->name_field) {
	new = w->type_field;
	buffer = attach_get_name(pa);
    }

    if (new)
	XtSetKeyboardFocus(w->shell, new);
    if (buffer)
	TextFieldSetBuffer(field, buffer);
}

static void type_field_callback(Widget gw,
				XtPointer client_data,
				XtPointer call_data)
{
    PostContext		*context = (PostContext *)client_data;
    char		*buffer = (char *)call_data;
    PostWidgets		*w = context->widgets;
    int			sel = ScrListGetFirstSelected(w->list);
    PostAttachment	*pa;
    char		msg[256];
    int			ok;

    if (context->busy || sel < 0)
	return;

    pa = context->attachments[sel];
    ok = attach_set_type(pa, buffer, msg);
    post_set_message(context, msg, !ok);
    if (ok) {
	char	*tmp;

	print_attach_info(pa, msg);
	ScrListSetLine(w->list, sel, msg, None);
	tmp = attach_get_type(pa);
	TextFieldSetBuffer(w->type_field, tmp);
	XtSetKeyboardFocus(w->shell, w->descr_field);
    }
}

static void descr_field_callback(Widget gw,
				XtPointer client_data,
				XtPointer call_data)
{
    PostContext		*context = (PostContext *)client_data;
    char		*buffer = (char *)call_data;
    PostWidgets		*w = context->widgets;
    int			sel = ScrListGetFirstSelected(w->list);
    PostAttachment	*pa;
    char		msg[256];
    int			ok;

    if (context->busy || sel < 0)
	return;

    pa = context->attachments[sel];
    ok = attach_set_descr(pa, buffer, msg);
    post_set_message(context, msg, !ok);
    if (ok) {
	char	*tmp;

	print_attach_info(pa, msg);
	ScrListSetLine(w->list, sel, msg, None);
	tmp = attach_get_descr(pa);
	TextFieldSetBuffer(w->descr_field, tmp);
	XtSetKeyboardFocus(w->shell, w->name_field);
    }
}

static void name_field_callback(Widget gw,
				XtPointer client_data,
				XtPointer call_data)
{
    PostContext		*context = (PostContext *)client_data;
    char		*buffer = (char *)call_data;
    PostWidgets		*w = context->widgets;
    int			sel = ScrListGetFirstSelected(w->list);
    PostAttachment	*pa;
    char		msg[256];
    int			ok;

    if (context->busy || sel < 0)
	return;

    pa = context->attachments[sel];
    ok = attach_set_name(pa, buffer, msg);
    post_set_message(context, msg, !ok);
    if (ok) {
	char	*tmp;

	print_attach_info(pa, msg);
	ScrListSetLine(w->list, sel, msg, None);
	tmp = attach_get_name(pa);
	TextFieldSetBuffer(w->name_field, tmp);
	XtSetKeyboardFocus(w->shell, w->type_field);
    }
}

static void disp_toggle_callback(Widget gw,
				 XtPointer client_data,
				 XtPointer call_data)
{
    PostContext		*context = (PostContext *)client_data;
    PostWidgets		*w = context->widgets;
    int			sel = ScrListGetFirstSelected(w->list);
    PostAttachment	*pa;
    char		msg[256];
    int			is_inline = gw == w->inline_toggle;
    int			ok;

    if (context->busy || sel < 0)
	return;

    pa = context->attachments[sel];
    ok = attach_set_inline(pa, is_inline, msg);
    post_set_message(context, msg, !ok);
    if (ok) {
	print_attach_info(pa, msg);
	ScrListSetLine(w->list, sel, msg, None);
    }
    set_disp_toggles(w, pa);
}

static void enc_toggle_callback(Widget gw,
				XtPointer client_data,
				XtPointer call_data)
{
    PostContext		*context = (PostContext *)client_data;
    PostWidgets		*w = context->widgets;
    int			sel = ScrListGetFirstSelected(w->list);
    PostAttachment	*pa;
    char		msg[256];
    int			ok, enc;

    if (context->busy || sel < 0)
	return;

    if (gw == w->none_toggle)
	enc = MimeEncNone;
    else if (gw == w->base64_toggle)
	enc = MimeEncBase64;
    else if (gw == w->uue_toggle)
	enc = MimeEncUue;
    else if (gw == w->qp_toggle)
	enc = MimeEncQP;
    else {
	XBell(display, 0);
	return;
    }

    pa = context->attachments[sel];
    ok = attach_set_enc(pa, enc, msg);
    post_set_message(context, msg, !ok);
    if (ok) {
	print_attach_info(pa, msg);
	ScrListSetLine(w->list, sel, msg, None);
    }
    set_enc_toggles(w, pa);
}

/*********************************************************************/

static void edit_exit_callback(void *client_data,
			       int status,
			       char *stderr_buf)
{
    PostContext	*context = (PostContext *)client_data;
    char	message[128];
    int		ok = False;

    if (WIFEXITED(status))
	switch (WEXITSTATUS(status)) {
	case 0:
	    ok = True;
	    break;
	case 127:
	    strcpy(message, "Failed to start editor!");
	    break;
	default:
	    strcpy(message, "Editor exited abnormally!");
	    break;
	}
    else if (WIFSIGNALED(status))
	sprintf(message, "Editor caught %s!", signal_string(WTERMSIG(status)));
    else
	strcpy(message, "Unknown problem with editor!");

    post_unset_busy(context);

    if (!ok) {
	set_message(message, True);
	stderr_popup(stderr_buf, 0);
	if (!context->widgets) {
	    free_post_context(context);
	    return;
	}
    }

    if (!context->widgets) {
	create_post_widgets(context);
	set_attach_widgets(context->widgets, NULL);
    }

    do_art_check(context);
    if (!ok)
	post_set_message(context, message, True);
}

void fork_editor(PostContext *context)
{
    pid_t	pid;

    pid = fork_nicely(context, edit_exit_callback, True);

    if (pid < 0) { /* fork failed */
	popup_notice("notice", "Failed to start editor:\n\nfork failed",
		     NULL, NULL, NULL, 5000, NULL, NULL, XtGrabNone);
	return;
    }

    if (pid == 0) { /* child */
	char	editcmd[512];
	char	*p1, *p2;

	p1 = strstr(global.edit_command, "%s");
	p2 = strstr(global.edit_command, "%i");

	if (p1)
	    if (p2)
		if (p1 < p2)
		    sprintf(editcmd, global.edit_command,
			    context->file_name, context->line);
		else
		    sprintf(editcmd, global.edit_command,
			    context->line, context->file_name);
	    else
		sprintf(editcmd, global.edit_command, context->file_name);
	else if (p2)
	    sprintf(editcmd, global.edit_command, context->line);
	else
	    sprintf(editcmd, global.edit_command);

	execl(BIN_SH, "sh", "-c", editcmd, (char *)NULL);
	perror("knews: execl " BIN_SH);
	_exit(127);
    }

    /*
     *  Parent
     */

    if (global.mode == NewsModeDisconnected)
	set_message("Editor started.  Warning: not connected.", False);
    else if (global.posting_allowed || !(context->flags & POST))
	set_message("Editor started.", False);
    else
	set_message("Editor started.  Warning: this server "
		    "doesn't allow posting.", False);
}

/*********************************************************************/

typedef struct {
    PostContext	*context;
    char	*command;
    int		is_ro;
} CmdContext;

static void command_exit_callback(void	*client_data,
				  int	 status,
				  char	*stderr_buf)
{
    CmdContext	*cmd = client_data;
    PostContext	*context = cmd->context;
    char	*command = cmd->command;
    int		is_ro = cmd->is_ro;
    char	message[256];

    XtFree((char *)cmd);
    cmd = NULL;

    post_unset_busy(context);

    status = WIFEXITED(status) ? WEXITSTATUS(status) : 1;

    if (strlen(command) > 200)
	command[200] = '\0';

    if (status == 0) {
	sprintf(message, "'%s' exited OK.", command);
	popup_title_notice(NULL, message, False);
    } else {
	sprintf(message, "'%s' exited abnormally", command);
	popup_title_notice(NULL, message, True);
	stderr_popup(stderr_buf, 0);
    }

    if (is_ro)
	set_ready_message(context, False);
    else
	do_art_check(context);

    XtFree(command);
}

static void misc_menu_callback(Widget w,
			       XtPointer client_data,
			       XtPointer call_data)
{
    PostContext	*context = (PostContext *)client_data;
    char	*command = StringGadgetCommand(w);
    char	*expn[3];
    CmdContext	*cmd;
    pid_t	pid;
    int		is_ro;

    if (context->busy) {
	XBell(display, 0);
	return;
    }
    if (!command) {
	post_set_message(context, "No command associated with that "
			 "menu entry!", True);
	return;
    }

    is_ro = !strstr(command, "%s");
    expn[0] = context->file_name;
    expn[1] = context->q_str;
    expn[2] = context->qq_str;
    command = expn_tmpl(command, 3, "sqQ", expn);

    cmd = (CmdContext *)XtMalloc(sizeof *cmd);
    cmd->context = context;
    cmd->command = command;
    cmd->is_ro   = is_ro;

    pid = fork_nicely(cmd, command_exit_callback, True);

    if (pid < 0) {
	post_set_message(context, "Fork failed!", True);
	XtFree(command);
	XtFree((char *)cmd);
	return;
    }

    if (pid == 0) { /* child */
	if (is_ro) {
	    int	fd;

	    fd = open(context->file_name, O_RDONLY);
	    if (fd < 0) {
		perror(context->file_name);
		_exit(126);
	    }

	    if (fd != STDIN_FILENO) {
		if (dup2(fd, STDIN_FILENO) != STDIN_FILENO) {
		    perror("dup2");
		    _exit(127);
		}
		close(fd);
	    }
	}

	execl(BIN_SH, "sh", "-c", command, (char *)NULL);
	perror("knews: execl " BIN_SH);
	_exit(127);
    }

    /*
     *  Parent.
     */

    post_set_message(context, "Starting command...", False);
    post_set_busy(context);
}
