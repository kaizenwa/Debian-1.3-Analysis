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
#include "color.h"
#include "expand.h"
#include "k_I.h"
#include "k_edit.h"
#include "k_file.h"
#include "k_node.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/Compat.h"
#include "../Widgets/Knapp.h"
#include "../Widgets/Layout.h"
#include "../Widgets/Manager.h"
#include "../Widgets/Menu.h"
#include "../Widgets/Message.h"
#include "../Widgets/MenuKnapp.h"
#include "../Widgets/MenuShell.h"
#include "../Widgets/Scrollable.h"
#include "../Widgets/ScrBar.h"
#include "../Widgets/ScrList.h"
#include "../Widgets/SeparatorG.h"
#include "../Widgets/StringG.h"
#include "../Widgets/TextField.h"
#include "../Widgets/Toggle.h"
#include "../Widgets/Util.h"

typedef struct KILL_WIDGETS {
    Widget	shell;
    Widget	listmgr;
    Widget	list;
    /***/
    Widget	field_knapp;
    Widget	messageid_field;
    Widget	subject_field;
    Widget	from_field;
    Widget	xref_field;
    /***/
    Widget	scope_knapp;
    Widget	article_scope;
    Widget	subject_scope;
    Widget	thread_scope;
    Widget	subthread_scope;
    /***/
    Widget	action_knapp;
    Widget	expr_knapp;
    Widget	group_knapp;
    /***/
    Widget	group_field;
    Widget	expr_field;
    Widget	color_field;
    /***/
    Widget	stayup;
    Widget	add_knapp;
    Widget	delete_knapp;
} KILL_WIDGETS;

#define SAME_TYPE(f1, f2) (((f1) == KillFieldMsgid) == \
			   ((f2) == KillFieldMsgid))

static void set_field(KILL_WIDGETS *w, int field)
{
    KnappSetLabelNo(w->field_knapp, field, True);
    KnappSetLabelNo(w->expr_knapp, field == KillFieldMsgid, True);
}

static void set_field_sens(KILL_WIDGETS *w, int field)
{
    int	msgid = field == KillFieldMsgid;
    int	all_sens = field < 0;

    XtSetSensitive(w->messageid_field, all_sens || msgid);
    XtSetSensitive(w->subject_field, all_sens || !msgid);
    XtSetSensitive(w->from_field, all_sens || !msgid);
    XtSetSensitive(w->xref_field, all_sens || !msgid);
}

static void set_scope(KILL_WIDGETS *w, int scope)
{
    KnappSetLabelNo(w->scope_knapp, scope, True);
}

static void set_action(KILL_WIDGETS *w, int hot, char *color)
{
    if (!hot)
	color = NULL;
    else if (!color)
	color = "";

    ToggleSet(w->action_knapp, hot);
    KnappSetLabelNo(w->action_knapp, hot, True);
    XtSetSensitive(w->color_field, color != NULL);
    TextFieldSetBuffer(w->color_field, color);
}

static void set_knapp_sens(KILL_WIDGETS *w, long n)
{
    KnappSetSensitive(w->add_knapp, n < 0);
    KnappSetSensitive(w->delete_knapp, n >= 0);
}

static void set_kill_controls(KILL_FILE *file, long n)
{
    KILL_WIDGETS	*w = file->w;
    KILL_NODE		*node = n < 0 ? NULL : file->nodes[n];

    set_field(w, node ? node->field : KillFieldFrom);
    set_field_sens(w, node ? node->field : -1);
    set_scope(w, node ? node->scope : KillScopeArticle);
    set_action(w, node ? node->hot : False, node ? node->color : NULL);
    if (w->group_field) {
	char	*grp;

	if (node)
	    grp = node->group_str;
	else if (global.curr_group)
	    grp = regexp_escape_string(global.curr_group->name, True);
	else
	    grp = NULL;
	TextFieldSetBuffer(w->group_field, grp);
	if (!node)
	    XtFree(grp);
    }
    TextFieldSetBuffer(w->expr_field, node ? node->expr_str : NULL);
    if (n < 0) {
	n = ScrListGetFirstSelected(w->list);
	if (n >= 0)
	    ScrListSetSelected(w->list, n, False);
    }
    set_knapp_sens(w, ScrListGetFirstSelected(w->list));
}

static void update_list_entry(KILL_FILE *file, long n)
{
    char	buffer[256];

    sprint_kill_node(file->nodes[n], buffer, sizeof buffer);
    ScrListSetLine(file->w->list, n, buffer, file->nodes[n]->pixmap);
}

/*********************************************************************/

static void list_callback(Widget    gw,
			  XtPointer client_data,
			  XtPointer call_data)
{
    KILL_FILE		*file = (KILL_FILE *)client_data;
    KILL_WIDGETS	*w = file->w;
    long		n = (long)call_data;

    if (ScrListGetSelected(w->list, n))
	set_kill_controls(file, n);
    else {
	KnappSetSensitive(w->add_knapp, True);
	KnappSetSensitive(w->delete_knapp, False);
    }
}

static void list_dnd_callback(Widget    gw,
			      XtPointer client_data,
			      XtPointer call_data)
{
    KILL_FILE	*file = (KILL_FILE *)client_data;
    long	*index = (long *)call_data;
    long	n;
    KILL_NODE	*temp;

    if (!index || index[0] < 0 || index[1] < 0 ||
	index[0] >= file->n || index[1] >= file->n)
	return;

    file->dirty = True;
    index[2] = True;
    temp = file->nodes[index[0]];
    if (index[0] < index[1])
	for (n = index[0] ; n < index[1] ; n++)
	    file->nodes[n] = file->nodes[n + 1];
    else
	for (n = index[0] ; n > index[1] ; n--)
	    file->nodes[n] = file->nodes[n - 1];
    file->nodes[index[1]] = temp;
}

static void add_callback(Widget    gw,
			 XtPointer client_data,
			 XtPointer call_data)
{
    KILL_FILE		*file = (KILL_FILE *)client_data;
    KILL_WIDGETS	*w = file->w;
    char		*expr, *group, *color;
    int			hot = ToggleGet(w->action_knapp);

    expr  = TextFieldGetBuffer(w->expr_field);
    group = w->group_field ? TextFieldGetBuffer(w->group_field) : NULL;
    color = hot ? TextFieldGetBuffer(w->color_field) : NULL;
    add_kill_node(file, False, KnappGetLabelNo(w->field_knapp),
		  KnappGetLabelNo(w->expr_knapp), hot, color,
		  expr, group);
    XtFree(expr);
    XtFree(group);
    XtFree(color);
}

static void delete_callback(Widget    gw,
			    XtPointer client_data,
			    XtPointer call_data)
{
    KILL_FILE		*file = (KILL_FILE *)client_data;
    KILL_WIDGETS	*w = file->w;
    long		n = ScrListGetFirstSelected(w->list);
    KILL_NODE		*node = n < 0 ? NULL : file->nodes[n];

    if (!node)
	return;

    file->dirty = True;
    if (node->pixmap != None)
	purge_hot(node->pixmap);
    free_kill_node(node);
    file->n--;
    if (n < file->n)
	memmove(file->nodes + n, file->nodes + n + 1,
		(file->n - n) * sizeof file->nodes[0]);
    file->nodes[file->n] = NULL;
    ScrListDeleteLine(w->list, n);
    set_knapp_sens(w, -1);
}

static void clear_callback(Widget    gw,
			   XtPointer client_data,
			   XtPointer call_data)
{
    KILL_FILE		*file = (KILL_FILE *)client_data;

    set_kill_controls(file, -1);
}

static void close_callback(Widget    gw,
			   XtPointer client_data,
			   XtPointer call_data)
{
    KILL_FILE	*file = (KILL_FILE *)client_data;

    XtPopdown(file->w->shell);
}

static void stayup_callback(Widget    gw,
			    XtPointer client_data,
			    XtPointer call_data)
{
    KILL_FILE	*file = (KILL_FILE *)client_data;
    Boolean	*set = (Boolean *)call_data;

    if (set)
	file->stay_up = *set = !*set;
}

static void field_callback(Widget    gw,
			   XtPointer client_data,
			   XtPointer call_data)
{
    KILL_FILE	*file = (KILL_FILE *)client_data;
    long	n = ScrListGetFirstSelected(file->w->list);
    KILL_NODE	*node = n < 0 ? NULL : file->nodes[n];
    int		field;

    if (gw == file->w->messageid_field)
	field = KillFieldMsgid;
    else if (gw == file->w->subject_field)
	field = KillFieldSubject;
    else if (gw == file->w->from_field)
	field = KillFieldFrom;
    else if (gw == file->w->xref_field)
	field = KillFieldXref;
    else
	return;

    if (node && !SAME_TYPE(field, node->field))
	return;

    set_field(file->w, field);
    if (node) {
	file->dirty = True;
	node->field = field;
	update_list_entry(file, n);
    }
}

static void scope_callback(Widget    gw,
			   XtPointer client_data,
			   XtPointer call_data)
{
    KILL_FILE	*file = (KILL_FILE *)client_data;
    long	n = ScrListGetFirstSelected(file->w->list);
    KILL_NODE	*node = n < 0 ? NULL : file->nodes[n];
    int		scope;

    if (gw == file->w->article_scope)
	scope = KillScopeArticle;
    else if (gw == file->w->subject_scope)
	scope = KillScopeSubject;
    else if (gw == file->w->thread_scope)
	scope = KillScopeThread;
    else if (gw == file->w->subthread_scope)
	scope = KillScopeSubthread;
    else
	return;

    set_scope(file->w, scope);
    if (node) {
	file->dirty = True;
	node->scope = scope;
	update_list_entry(file, n);
    }
}

static void action_callback(Widget    gw,
			    XtPointer client_data,
			    XtPointer call_data)
{
    KILL_FILE		*file = (KILL_FILE *)client_data;
    Boolean		*set = (Boolean *)call_data;
    KILL_WIDGETS	*w = file->w;
    long		n = ScrListGetFirstSelected(w->list);
    KILL_NODE		*node = n < 0 ? NULL : file->nodes[n];
    int			hot;

    if (!set)
	return;

    hot = *set = !*set;

    KnappSetLabelNo(w->action_knapp, hot, True);
    if (!hot) {
	TextFieldSetBuffer(w->color_field, "");
	XtSetKeyboardFocus(w->shell, w->expr_field);
    }
    XtSetSensitive(w->color_field, hot);
    if (hot)
	XtSetKeyboardFocus(w->shell, w->color_field);

    if (node) {
	file->dirty = True;
	node->hot = hot;
	if (hot) {
	    if (!node->alloced_pixel)
		node->pixel = global.default_hot_pixel;
	    fix_node_pixmap(node);
	} else {
	    if (node->alloced_pixel) {
		unsigned long	pixel = node->pixel;

		XFreeColors(display, global.cmap, &pixel, 1, 0);
		node->alloced_pixel = False;
		node->pixel = global.default_hot_pixel;
	    }
	    if (node->pixmap != None) {
		purge_hot(node->pixmap);
		XFreePixmap(display, node->pixmap);
		node->pixmap = None;
	    }
	}
	update_list_entry(file, n);
    }
}

static void tab_callback(Widget    gw,
			 XtPointer client_data,
			 XtPointer call_data)
{
    KILL_FILE		*file = (KILL_FILE *)client_data;
    KILL_WIDGETS	*w = file->w;
    Widget		focus;

    if (gw == w->color_field)
	focus = w->group_field;
    else if (w->group_field && gw == w->group_field)
	focus = w->expr_field;
    else if (gw == w->expr_field)
	focus = ToggleGet(w->action_knapp) ? w->color_field : w->group_field;
    else
	return;

    if (!focus)  /* group_field == NULL */
	focus = w->expr_field;

    XtSetKeyboardFocus(w->shell, focus);
}

static void group_field_callback(Widget    gw,
				 XtPointer client_data,
				 XtPointer call_data)
{
    KILL_FILE		*file = (KILL_FILE *)client_data;
    char		*group = (char *)call_data;
    KILL_WIDGETS	*w = file->w;
    long		n = ScrListGetFirstSelected(w->list);
    KILL_NODE		*node = n < 0 ? NULL : file->nodes[n];
    regex_t		re;
    int			code;

    code = regcomp(&re, group, REGEXP_COMPILE_FLAGS);
    if (code != 0) {
	popup_regexpnotice(code, &re);
	return;
    }

    if (!node)
	regfree(&re);
    else {
	file->dirty = True;
	XtFree(node->group_str);
	node->group_str = XtNewString(group);
	if (node->group_re)
	    regfree(node->group_re);
	else
	    node->group_re = (regex_t *)XtMalloc(sizeof *node->group_re);
	memcpy(node->group_re, &re, sizeof re);
	update_list_entry(file, n);
    }
}

static void group_knapp_callback(Widget    gw,
				 XtPointer client_data,
				 XtPointer call_data)
{
    KILL_FILE		*file = (KILL_FILE *)client_data;
    KILL_WIDGETS	*w = file->w;

    if (!global.curr_group)
	XBell(display, 0);
    else {
	char	*grp = regexp_escape_string(global.curr_group->name, True);

	XtSetKeyboardFocus(w->shell, w->group_field);
	TextFieldSetBuffer(w->group_field, grp);
	group_field_callback(w->group_field, (XtPointer)file, (XtPointer)grp);
	XtFree(grp);
    }
}

static void expr_field_callback(Widget    gw,
				XtPointer client_data,
				XtPointer call_data)
{
    KILL_FILE	*file = (KILL_FILE *)client_data;
    KILL_NODE	*node;
    char	*buffer = (char *)call_data;
    long	n = ScrListGetFirstSelected(file->w->list);
    int		field;

    if (!buffer)
	buffer = "";

    node = (n < 0 || n >= file->n) ? NULL : file->nodes[n];
    field = node ? node->field : KnappGetLabelNo(file->w->field_knapp);

    if (!node)
	if (buffer[0] != '\0')
	    set_field_sens(file->w, field);
	else {
	    set_field_sens(file->w, -1);
	    return;
	}

    if (field == KillFieldMsgid) {
	if (buffer[0] != '<' || buffer[strlen(buffer) - 1] != '>') {
	    set_message("Bad Message-ID!", True);
	    return;
	}

	if (node) {
	    file->dirty = True;
	    XtFree(node->expr_str);
	    node->expr_str = XtNewString(buffer);
	    update_list_entry(file, n);
	}
    } else {
	regex_t	re;
	int	code;

	code = regcomp(&re, buffer, REGEXP_COMPILE_FLAGS);
	if (code != 0) {
	    popup_regexpnotice(code, &re);
	    return;
	}

	if (!node)
	    regfree(&re);
	else {
	    file->dirty = True;
	    XtFree(node->expr_str);
	    node->expr_str = XtNewString(buffer);
	    if (node->expr_re)
		regfree(node->expr_re);
	    else
		node->expr_re = (regex_t *)XtMalloc(sizeof *node->expr_re);
	    memcpy(node->expr_re, &re, sizeof re);
	    update_list_entry(file, n);
	}
    }
}

static void expr_knapp_callback(Widget    gw,
				XtPointer client_data,
				XtPointer call_data)
{
    KILL_FILE		*file = (KILL_FILE *)client_data;
    KILL_WIDGETS	*w = file->w;
    char		*expr = NULL;

    if (global.mode != NewsModeGroup && global.mode != NewsModeThread)
	return;

    switch (KnappGetLabelNo(w->field_knapp)) {
    case KillFieldMsgid:
	if (global.curr_art) {
	    expr = XtMalloc(global.curr_art->hash_len + 4);
	    sprintf(expr, "<%s>", global.curr_art->msgid);
	}
	break;
    case KillFieldSubject:
	if (global.curr_subj)
	    expr = regexp_escape_string(global.curr_subj->subject, True);
	break;
    case KillFieldFrom:
	if (global.curr_art)
	    expr = regexp_escape_string(global.curr_art->from, True);
	break;
    case KillFieldXref:
	if (global.curr_art && global.curr_art->xref)
	    expr = regexp_escape_string(global.curr_art->xref, True);
	break;
    default:
	return;
    }

    if (!expr)
	XBell(display, 0);
    else {
	XtSetKeyboardFocus(w->shell, w->expr_field);
	TextFieldSetBuffer(w->expr_field, expr);
	expr_field_callback(w->expr_field, (XtPointer)file, (XtPointer)expr);
	XtFree(expr);
    }
}

static void color_callback(Widget    gw,
			   XtPointer client_data,
			   XtPointer call_data)
{
    KILL_FILE		*file = (KILL_FILE *)client_data;
    char		*color = (char *)call_data;
    KILL_WIDGETS	*w = file->w;
    long		n = ScrListGetFirstSelected(w->list);
    KILL_NODE		*node = n < 0 ? NULL : file->nodes[n];
    XColor		col;

    if (!XParseColor(display, global.cmap, color, &col)) {
	popup_colornotice(True);
	return;
    }

    if (node) {
	if (!node->hot)
	    return;

	file->dirty = True;
	XtFree(node->color);
	node->color = XtNewString(color);
	if (node->alloced_pixel) {
	    unsigned long	pixel = node->pixel;

	    XFreeColors(display, global.cmap, &pixel, 1, 0);
	    node->alloced_pixel = False;
	}

	if (XAllocColor(display, global.cmap, &col)) {
	    node->alloced_pixel = True;
	    node->pixel = col.pixel;
	} else {
	    popup_colornotice(False);
	    node->pixel = get_closest_color(&col);
	}
	fix_node_pixmap(node);
	update_list_entry(file, n);
    }

    XtSetKeyboardFocus(w->shell, w->expr_field);
}

/*********************************************************************/

static void init_list(KILL_FILE *file)
{
    KILL_WIDGETS	*w = file->w;
    char		buffer[256];
    long		n;

    ScrListClearLines(w->list);
    for (n = 0 ; n < file->n ; n++) {
	sprint_kill_node(file->nodes[n], buffer, sizeof buffer);
	ScrListAddLine(w->list, buffer, file->nodes[n]->pixmap);
    }
    Remanage(w->listmgr);
}

static void create_kill_widgets(KILL_FILE *file)
{
    KILL_WIDGETS	*w;
    Widget		layout, temp, vbar, hbar;
    Arg			args[12];

    file->w = w = (KILL_WIDGETS *)XtMalloc(sizeof *w);

    XtSetArg(args[0], XtNcolormap, global.cmap);
    XtSetArg(args[1], XtNvisual, global.visual);
    XtSetArg(args[2], XtNdepth, global.depth);
    XtSetArg(args[3], XtNinput, True);
    w->shell = XtCreatePopupShell("killeditor", topLevelShellWidgetClass,
				  main_widgets.shell, args, 4);

    layout =
	XtVaCreateManagedWidget("killayout", layoutWidgetClass,
				w->shell, XtVaTypedArg, XtNlayout, XtRString,
				file->group ?
#include "layouts/kill.h"
				:
#include "layouts/killg.h"
				(int)sizeof(char*), (void *)0);

    XtSetArg(args[0], XtNbuffer,
	     file->group ? file->group->name : "Global kill file");
    XtCreateManagedWidget("killtitle", messageWidgetClass, layout, args, 1);

    XtSetArg(args[0], XtNvertical, False);
    hbar = XtCreateManagedWidget("hbar", scrBarWidgetClass, layout, args, 1);
    vbar = XtCreateManagedWidget("vbar", scrBarWidgetClass, layout, NULL, 0);

    w->listmgr =
	XtCreateManagedWidget("killistmgr", managerWidgetClass,
			      layout, NULL, 0);

    XtSetArg(args[0], XtNhBar, hbar);
    XtSetArg(args[1], XtNvBar, vbar);
    XtSetArg(args[2], XtNpixmapWidth, HOT_PIXMAP_SIZE);
    XtSetArg(args[3], XtNpixmapHeight, HOT_PIXMAP_SIZE);
    XtSetArg(args[4], XtNdepthOne, False);
    XtSetArg(args[5], XtNallowDnd, True);
    XtSetArg(args[6], XtNusePixmaps, True);
    XtSetArg(args[7], XtNatMostOne, True);
    XtSetArg(args[8], XtNatLeastOne, False);
    XtSetArg(args[9], XtNcontainHoriz, False);
    XtSetArg(args[10], XtNcontainVert, True);
    w->list =
	XtCreateManagedWidget("killist", scrListWidgetClass,
			      w->listmgr, args, 11);
    XtAddCallback(w->list, XtNselectCallback, list_callback, (XtPointer)file);
    XtAddCallback(w->list, XtNdndCallback, list_dnd_callback, (XtPointer)file);

    w->add_knapp =
	XtCreateManagedWidget("add", knappWidgetClass, layout, NULL, 0);
    XtAddCallback(w->add_knapp, XtNcallback,
		  add_callback, (XtPointer)file);
    w->delete_knapp =
	XtCreateManagedWidget("delete", knappWidgetClass, layout, NULL, 0);
    XtAddCallback(w->delete_knapp, XtNcallback,
		  delete_callback, (XtPointer)file);
    temp =
	XtCreateManagedWidget("clear", knappWidgetClass, layout, NULL, 0);
    XtAddCallback(temp, XtNcallback, clear_callback, (XtPointer)file);
    temp =
	XtCreateManagedWidget("close", knappWidgetClass, layout, NULL, 0);
    XtAddCallback(temp, XtNcallback, close_callback, (XtPointer)file);
    if (!file->group)
	w->stayup = NULL;
    else {
	w->stayup =
	    XtCreateManagedWidget("stayup", toggleWidgetClass,
				  layout, NULL, 0);
	XtAddCallback(w->stayup, XtNcallback,
		      stayup_callback, (XtPointer)file);
	file->stay_up = ToggleGet(w->stayup);
    }

    XtCreateManagedWidget("fieldmessage", messageWidgetClass, layout, NULL, 0);
    
    XtSetArg(args[0], XtNmenuName, "fieldshell");
    XtSetArg(args[1], XtNresizable, False);
    w->field_knapp =
	XtCreateManagedWidget("fieldknapp", menuKnappWidgetClass,
			      layout, args, 2);

    XtCreateManagedWidget("scopemessage", messageWidgetClass, layout, NULL, 0);

    XtSetArg(args[0], XtNmenuName, "scopeshell");
    XtSetArg(args[1], XtNresizable, False);
    w->scope_knapp =
	XtCreateManagedWidget("scopeknapp", menuKnappWidgetClass,
			      layout, args, 2);

    XtSetArg(args[0], XtNresizable, False);
    w->action_knapp =
	XtCreateManagedWidget("actionknapp", toggleWidgetClass,
			      layout, args, 1);
    XtAddCallback(w->action_knapp, XtNcallback,
		  action_callback, (XtPointer)file);

    XtSetArg(args[0], XtNcolormap, global.cmap);
    XtSetArg(args[1], XtNvisual, global.visual);
    XtSetArg(args[2], XtNdepth, global.depth);
    temp =
	XtCreatePopupShell("fieldshell", menuShellWidgetClass,
			   w->shell, args, 3);

    temp = XtCreateManagedWidget("fieldmenu", menuWidgetClass, temp, NULL, 0);

    w->messageid_field =
	MenuCreateGadget("messageid", stringGadgetClass, temp, NULL, 0);
    XtAddCallback(w->messageid_field, XtNcallback,
		  field_callback, (XtPointer)file);
    MenuCreateGadget("sep", separatorGadgetClass, temp, NULL, 0);
    w->subject_field =
	MenuCreateGadget("subject", stringGadgetClass, temp, NULL, 0);
    XtAddCallback(w->subject_field, XtNcallback,
		  field_callback, (XtPointer)file);
    w->from_field =
	MenuCreateGadget("from", stringGadgetClass, temp, NULL, 0);
    XtAddCallback(w->from_field, XtNcallback, field_callback, (XtPointer)file);
    w->xref_field =
	MenuCreateGadget("xref", stringGadgetClass, temp, NULL, 0);
    XtAddCallback(w->xref_field, XtNcallback, field_callback, (XtPointer)file);

    XtSetArg(args[0], XtNcolormap, global.cmap);
    XtSetArg(args[1], XtNvisual, global.visual);
    XtSetArg(args[2], XtNdepth, global.depth);
    temp =
	XtCreatePopupShell("scopeshell", menuShellWidgetClass,
			   w->shell, args, 3);

    temp = XtCreateManagedWidget("scopemenu", menuWidgetClass, temp, NULL, 0);

    w->article_scope =
	MenuCreateGadget("article", stringGadgetClass, temp, NULL, 0);
    XtAddCallback(w->article_scope, XtNcallback,
		  scope_callback, (XtPointer)file);
    w->subject_scope =
	MenuCreateGadget("subject", stringGadgetClass, temp, NULL, 0);
    XtAddCallback(w->subject_scope, XtNcallback,
		  scope_callback, (XtPointer)file);
    w->thread_scope =
	MenuCreateGadget("thread", stringGadgetClass, temp, NULL, 0);
    XtAddCallback(w->thread_scope, XtNcallback,
		  scope_callback, (XtPointer)file);
    w->subthread_scope =
	MenuCreateGadget("subthread", stringGadgetClass, temp, NULL, 0);
    XtAddCallback(w->subthread_scope, XtNcallback,
		  scope_callback, (XtPointer)file);

    if (file->group)
	w->group_field = NULL;
    else {
	XtSetArg(args[0], XtNresizable, False);
	w->group_knapp =
	    XtCreateManagedWidget("groupknapp", knappWidgetClass,
				  layout, args, 1);
	XtAddCallback(w->group_knapp, XtNcallback,
		      group_knapp_callback, (XtPointer)file);

	XtSetArg(args[0], XtNfocusRoot, w->shell);
	XtSetArg(args[1], XtNsingleLine, True);
	w->group_field =
	    XtCreateManagedWidget("groupfield", textFieldWidgetClass,
				  layout, args, 2);
	XtAddCallback(w->group_field, XtNcallback,
		      group_field_callback, (XtPointer)file);
	XtAddCallback(w->group_field, XtNtabCallback,
		      tab_callback, (XtPointer)file);
    }

    XtSetArg(args[0], XtNresizable, False);
    XtSetArg(args[1], XtNjustify, JustifyTypeLeft);
    w->expr_knapp =
	XtCreateManagedWidget("exprknapp", knappWidgetClass, layout, args, 2);
    XtAddCallback(w->expr_knapp, XtNcallback,
		  expr_knapp_callback, (XtPointer)file);

    XtSetArg(args[0], XtNfocusRoot, w->shell);
    XtSetArg(args[1], XtNsingleLine, True);
    w->expr_field =
	XtCreateManagedWidget("exprfield", textFieldWidgetClass,
			      layout, args, 2);
    XtAddCallback(w->expr_field, XtNcallback,
		  expr_field_callback, (XtPointer)file);
    XtAddCallback(w->expr_field, XtNtabCallback,
		  tab_callback, (XtPointer)file);

    XtCreateManagedWidget("colormessage", messageWidgetClass, layout, NULL, 0);

    XtSetArg(args[0], XtNfocusRoot, w->shell);
    XtSetArg(args[1], XtNsingleLine, True);
    XtSetArg(args[2], XtNpreferredChars, 12);
    w->color_field =
	XtCreateManagedWidget("colorfield", textFieldWidgetClass,
			      layout, args, 3);
    XtAddCallback(w->color_field, XtNcallback,
		  color_callback, (XtPointer)file);
    XtAddCallback(w->color_field, XtNtabCallback,
		  tab_callback, (XtPointer)file);

    XtSetKeyboardFocus(w->shell, w->expr_field);
    XtRealizeWidget(w->shell);
    XtInstallAllAccelerators(w->shell, w->shell);
    add_WM_DELETE_WINDOW_callback(w->shell, close_callback, (XtPointer)file);

    init_list(file);
    set_kill_controls(file, -1);
}

void destroy_kill_widgets(KILL_WIDGETS *w)
{
    if (is_popped_up(w->shell))
	XtPopdown(w->shell);
    XtDestroyWidget(w->shell);
    XtFree((char *)w);
}

void popup_kill_editor(KILL_FILE *file)
{
    if (!file->w)
	create_kill_widgets(file);
    if (!is_popped_up(file->w->shell))
	XtPopup(file->w->shell, XtGrabNone);
}

void popdown_kill_editor(KILL_WIDGETS *w)
{
    if (is_popped_up(w->shell))
	XtPopdown(w->shell);
}

void kill_editor_notify_add(KILL_FILE *file, int append)
{
    if (!append) {
	init_list(file);
	set_kill_controls(file, 0);
    } else {
	char		buffer[256];
	KILL_NODE	*node = file->nodes[append ? file->n - 1 : 0];

	sprint_kill_node(node, buffer, sizeof buffer);
	ScrListAddLine(file->w->list, buffer, node->pixmap);
	Remanage(file->w->listmgr);
    }
}
