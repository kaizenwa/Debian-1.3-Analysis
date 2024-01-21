/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/
/****************************************************************************
 * This module is based on Twm, but has been siginificantly modified 
 * by Rob Nation
 ****************************************************************************/
/****************************************************************************
 * The win_list function is
 * by Rob Nation
 * A little of it is borrowed from ctwm.
 * Copyright 1993 Robert Nation. No restrictions are placed on this code,
 * as long as the copyright notice is preserved
 ***********************************************************************/
/***********************************************************************
 * The rest of it is all my fault -- MLM
 * mwm - "LessTif Window Manager"
 ***********************************************************************/

#include "mwm.h"

#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

#include <Xm/MessageB.h>

/*
 * this only works if what you're assigning to/comparing with is an int
 */
#ifndef INT_MAX
#define INT_MAX		((int)(~0U>>1))
#endif
#ifndef INT_MIN
#define INT_MIN		(~0)
#endif

extern int      menuFromFrameOrWindowOrTitlebar;
extern          DoHandlePageing;
extern XtAppContext app;
extern volatile int alarmed;

extern char   **g_argv;

/*
 * Does `string' match `pattern'? '*' in pattern matches any sub-string
 * (including the null string) '?' matches any single char. For use
 * by filenameforall. Note that '*' matches across directory boundaries
 *
 * This code donated by  Paul Hudson <paulh@harlequin.co.uk>    
 * It is public domain, no strings attached. No guarantees either.
 */
static int
match_pattern(char *pattern, char *string)
{
    if (string == NULL) {
	if (pattern == NULL)
	    return TRUE;
	else if (strcmp(pattern, "*") == 0)
	    return TRUE;
	else
	    return FALSE;
    }
    if (pattern == NULL)
	return TRUE;

    while (*string && *pattern) {
	if (*pattern == '?') {
	    /* match any character */
	    pattern += 1;
	    string += 1;
	}
	else if (*pattern == '*') {
	    /* see if the rest of the pattern matches any trailing substring
	       of the string. */
	    pattern += 1;
	    if (*pattern == 0) {
		return TRUE;	/* trailing * must match rest */
	    }
	    while (*string) {
		if (match_pattern(pattern, string)) {
		    return TRUE;
		}
		string++;
	    }
	    return FALSE;
	}
	else {
	    if (*pattern == '\\')
		pattern++; /* has strange, but harmless effects if the last
			      character is a '\\' */
	    if (*pattern++ != *string++) {
		return FALSE;
	    }
	}
    }
    if ((*pattern == 0) && (*string == 0))
	return TRUE;
    if ((*string == 0) && (strcmp(pattern, "*") == 0))
	return TRUE;
    return FALSE;
}

/*
 * Checks the function "function", and sees if it
 * is an allowed function for window t,  according to the motif way of life.
 * This routine is used to decide if we should refuse to perform a function.
 */
static int
function_allowed(int function, MwmWindow * t)
{

    if ((function == F_RESIZE) && (t) &&
	(!(t->functions & MWM_FUNC_RESIZE)))
	return 0;

    if ((function == F_MOVE) && (t) &&
	(!(t->functions & MWM_FUNC_MOVE)))
	return 0;

    if ((function == F_ICONIFY) && (t) &&
	(!(t->flags & ICONIFIED)) &&
	(!(t->functions & MWM_FUNC_MINIMIZE)))
	return 0;

    if ((function == F_MAXIMIZE) && (t) &&
	(!(t->functions & MWM_FUNC_MAXIMIZE)))
	return 0;

    if ((function == F_CLOSE) && (t) &&
	(!(t->functions & MWM_FUNC_CLOSE)))
	return 0;

    return 1;
}

/*
 * wait for the quit timeout to see if a window will exit itself
 */
static void
wait_quit_timeout(ScreenInfo *sinfo, MwmWindow *win)
{
#ifndef HAVE_GETITIMER
    struct itimerval
    { 
      struct timeval it_value;
    };
#endif
    XEvent          event;
    struct itimerval value;
    fd_set          in_fdset, out_fdset;
    Window          child;
    int             retval, i;
    ScreenInfo      *scr;
    int             timeout;

    timeout = Mwm.quit_timeout * 1000;

    while (True) {

	/* Do this prior to the select() call, in case the timer already
	 * expired, in which case the select would never return. */
	if (alarmed) {
	    alarmed = False;

	    for (i = 0; i < Mwm.number_of_screens; i++)
	    {
		scr = Mwm.screen_info[i];
		XQueryPointer(dpy, scr->root_win, &JunkRoot, &child,
			      &JunkX, &JunkY, &JunkX, &JunkY, &JunkMask);

		if ((scr->mwm_focus != NULL) &&
		    (child == scr->mwm_focus->frame)) {
		    if (!(scr->mwm_focus->flags & VISIBLE) &&
			scr->mwm_focus->focus_auto_raise) {
			WIN_Raise(scr, scr->mwm_focus);
			PAGER_Clear(scr);
		    }
		}
	    }
	    continue;
	}

	value.it_value.tv_usec = 10000;
	value.it_value.tv_sec = 0;

	FD_ZERO(&in_fdset);
	FD_SET(x_fd, &in_fdset);
	FD_ZERO(&out_fdset);

	/* Do this IMMEDIATELY prior to select, to prevent any nasty
	 * queued up X events from just hanging around waiting to be
	 * flushed */
	XFlush(dpy);
	if (XPending(dpy)) {
	    XNextEvent(dpy, &event);
	    MISC_StashEventTime(&event);

	    if ((event.type == UnmapNotify || event.type == DestroyNotify) &&
		event.xany.window == win->w) {
		EVENT_Dispatch(&event);
		return;
	    }
	    EVENT_Dispatch(&event);
	}

	/* Zap all those zombies! */
	/* If we get to here, then there are no X events waiting to be
	 * processed.  Just take a moment to check for dead children. */
	ReapChildren();

	XFlush(dpy);


#ifdef __hpux
	retval = select(fd_width, (int *) &in_fdset, 0, 0, &value.it_value);
#else
	retval = select(fd_width, &in_fdset, 0, 0, &value.it_value);
#endif
	timeout -= value.it_value.tv_usec;

	if (timeout <= 0) {
	    if (XGetGeometry(dpy, win->w, &JunkRoot, &JunkX, &JunkY,
			     &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth) == 0)
		WIN_DestroyWindow(sinfo, win);
	    else
		XKillClient(dpy, win->w);
	    XSync(dpy, 0);
	    return;
	}
    }
}

/*
 * circulate a window
 */
MwmWindow      *
circulate(ScreenInfo *scr, MwmWindow * tmp_win, char *action, Bool Direction)
{
    MwmWindow      *t, *selected;
    Bool            found;
    int             count, pass = 1;
    int             base, best;

    tmp_win = MISC_RootOfTree(tmp_win);
    while (pass < 3) {
	if (tmp_win)
	    base = tmp_win->focus_sequence;
	else
	    base = -1;
	if (Direction == DOWN)
	    best = -1;
	else
	    best = 10000;
	selected = tmp_win;

	/* move focus to the next window */
	found = FALSE;
	t = tmp_win;
	count = 0;
	while (count < 3) {
	    if (Direction == DOWN) {
		if ((t == (MwmWindow *) 0) || (t->next == NULL)) {
		    t = scr->mwm_root.next;
		    count++;
		}
		else
		    t = t->next;
	    }
	    else {		/* Direction Up */
		if ((t == (MwmWindow *) 0) || (t == &scr->mwm_root) ||
		    (t->prev == &scr->mwm_root) || (t->prev == (MwmWindow *) NULL)) {
		    for (t = scr->mwm_root.next; t->next != (MwmWindow *) NULL; t = t->next);
		    count++;
		}
		else
		    t = t->prev;
	    }
	    found = TRUE;

	    if (t->Desk != scr->current_desk)
		found = False;

	    if ((t) && (t->wmhints) && (t->wmhints->flags & InputHint) &&
		(t->wmhints->input == False) &&
		!(t->flags & WM_TAKES_FOCUS))
		found = False;

	    if (t->flags & CIRCULATESKIP)
		found = FALSE;
	    /* optional skip over icons */

	    if ((t->flags & ICONIFIED) && (scr->flags & CirculateSkipIcons))
		found = FALSE;


	    /* Make CirculateUp and CirculateDown take args. by Y.NOMURA */
	    if (action && (strlen(action) > 0) &&
		!(match_pattern(action, t->name)) &&
		!(match_pattern(action, t->icon_label)) &&
		t->class.res_name &&
		!(match_pattern(action, t->class.res_name)))
		found = FALSE;
	    if ((found) && (Direction == DOWN) && (t->focus_sequence > best)) {
		best = t->focus_sequence;
		selected = t;
	    }
	    if ((found) && (Direction != DOWN) && (t->focus_sequence < best)
		&& (t->focus_sequence > base)) {
		best = t->focus_sequence;
		selected = t;
	    }
	}
	if ((selected) && (selected == tmp_win) && (base > 0)) {
	    if (Direction == DOWN) {
		MISC_SetFocusSequence(scr);
		tmp_win->focus_sequence = 0;
	    }
	    else {
		MwmWindow      *temp;

		temp = scr->mwm_root.next;
		while (temp != NULL) {
		    temp->focus_sequence++;
		    if (temp == tmp_win)
			temp->focus_sequence = 0;
		    temp = temp->next;
		}
	    }
	    pass++;
	}
	else
	    pass = 3;
    }

    return selected;
}

/***********************************************************************
 *
 *  Procedure:
 *	(Un)maximize a window.
 *
 ***********************************************************************/
static void
maximize(ScreenInfo *scr, MwmWindow * tmp_win, int val1, int val2,
	 int val1_unit, int val2_unit)
{
    int             new_width, new_height, new_x, new_y;

    if (tmp_win->flags & MAXIMIZED) {
	tmp_win->flags &= ~MAXIMIZED;
	DEC_ConfigureDecorations(scr, tmp_win, tmp_win->orig_x, tmp_win->orig_y,
				 tmp_win->orig_wd, tmp_win->orig_ht, TRUE);
	DEC_DrawDecorations(scr, tmp_win, True, True, True, None);
    }
    else {
	new_width = tmp_win->frame_width;
	new_height = tmp_win->frame_height;
	new_x = tmp_win->frame_x;
	new_y = tmp_win->frame_y;
	if (val1 > 0) {
	    new_width = val1 * val1_unit / 100 - 2;
	    new_x = 0;
	}
	if (val2 > 0) {
	    new_height = val2 * val2_unit / 100 - 2;
	    new_y = 0;
	}
	if ((val1 == 0) && (val2 == 0)) {
	    new_x = 0;
	    new_y = 0;
	    new_height = scr->d_height - 2;
	    new_width = scr->d_width - 2;
	}
	tmp_win->flags |= MAXIMIZED;
	WIN_ConstrainWindow(scr, tmp_win, &new_width, &new_height);
	DEC_ConfigureDecorations(scr, tmp_win, new_x, new_y, new_width, new_height, TRUE);
	DEC_DrawDecorations(scr, tmp_win, scr->mwm_highlight == tmp_win,
		  True, True, tmp_win->maximizeb);
    }
    PAGER_Clear(scr);
}

/*
 * Start a window move operation
 */
static void
move(ScreenInfo *scr, XEvent * eventp, Window w, MwmWindow *tmp_win,
     int context, int val1, int val2, int val1_unit, int val2_unit)
{
    int             FinalX, FinalY;

    /* gotta have a window */
    if (tmp_win == NULL)
	return;

    w = tmp_win->frame;
    if (tmp_win->flags & ICONIFIED) {
	if (tmp_win->icon_pixmap_w != None) {
	    XUnmapWindow(dpy, tmp_win->icon_w);
	    w = tmp_win->icon_pixmap_w;
	}
	else
	    w = tmp_win->icon_w;
    }

    if ((val1 != 0) || (val2 != 0)) {
	FinalX = val1 * val1_unit / 100;
	FinalY = val2 * val2_unit / 100;
    }
    else
	MOVE_Interactive(scr, &w, tmp_win, &FinalX, &FinalY, eventp);

    if (w == tmp_win->frame) {
	DEC_ConfigureDecorations(scr, tmp_win, FinalX, FinalY,
		   tmp_win->frame_width, tmp_win->frame_height, FALSE);
    }
    else {			/* icon window */
	tmp_win->flags |= ICON_MOVED;
	tmp_win->icon_x_loc = FinalX;
	tmp_win->icon_xl_loc = FinalX -
	    (tmp_win->icon_w_width - tmp_win->icon_p_width) / 2;
	tmp_win->icon_y_loc = FinalY;
	XMoveWindow(dpy, tmp_win->icon_w,
		    tmp_win->icon_xl_loc, FinalY + tmp_win->icon_p_height);
	if (tmp_win->icon_pixmap_w != None) {
	    XMapWindow(dpy, tmp_win->icon_w);
	    XMoveWindow(dpy, tmp_win->icon_pixmap_w, tmp_win->icon_x_loc, FinalY);
	    XMapWindow(dpy, w);
	}

    }

    PAGER_Clear(scr);

    return;
}

static void
cancel_cb(Widget w, XtPointer calldata, XtPointer cbs)
{
    *((Boolean *)calldata) = True;
}

static void
restart_cb(Widget w, XtPointer calldata, XtPointer cbs)
{
    MWM_Done(1, g_argv[0]);
}

static void
restart(ScreenInfo *scr)
{
    static Widget   restart_mb = NULL;
    Boolean finished = False;
    Dimension wd, ht;

    MISC_Ungrab(scr);

    if (restart_mb == NULL) {
	Widget          tmp;
	XmString        lab;
	Arg             args[4];

	XtSetArg(args[0], XmNmwmDecorations, MWM_DECOR_BORDER);
	XtSetArg(args[1], XmNmwmFunctions, 0);
	XtSetArg(args[2], XmNdialogStyle, XmDIALOG_SYSTEM_MODAL);
	XtSetArg(args[3], XmNdefaultPosition, False);
	restart_mb = XmCreateQuestionDialog(toplevel, "restart_question",
					    args, 4);

	if (!restart_mb)
	    MWM_Done(0, NULL);

	tmp = XmMessageBoxGetChild(restart_mb, XmDIALOG_HELP_BUTTON);
	XtUnmanageChild(tmp);

	tmp = XmMessageBoxGetChild(restart_mb, XmDIALOG_OK_BUTTON);
	XtAddCallback(tmp, XmNactivateCallback,
		      restart_cb, NULL);

	tmp = XmMessageBoxGetChild(restart_mb, XmDIALOG_CANCEL_BUTTON);
	XtAddCallback(tmp, XmNactivateCallback,
		      cancel_cb, (XtPointer)&finished);

	tmp = XmMessageBoxGetChild(restart_mb, XmDIALOG_MESSAGE_LABEL);
	lab = XmStringCreateSimple("Restart Mwm?");
	XtVaSetValues(tmp, XmNlabelString, lab, NULL);

	XmStringFree(lab);

	XtRealizeWidget(restart_mb);

	XtVaGetValues(restart_mb,
		      XmNwidth, &wd, XmNheight, &ht, NULL);

	XtVaSetValues(XtParent(restart_mb),
		      XmNx, (scr->d_width - wd) / 2 - Mwm.frame_border_width,
		      XmNy, (scr->d_height - ht) / 2 - Mwm.frame_border_width,
		      NULL);
    }

    XtManageChild(restart_mb);
    scr->restart_win = XtWindow(XtParent(restart_mb));

    while (!finished) {
	XEvent   event;

	if (EVENT_Next(&event))
	    EVENT_Dispatch(&event);
    }
}

static void
quit_cb(Widget w, XtPointer calldata, XtPointer cbs)
{
    MWM_Done(0, NULL);
}

static void
quit(ScreenInfo *scr)
{
    static Widget   quit_mb = NULL;
    Boolean finished = False;
    Dimension wd, ht;

    MISC_Ungrab(scr);

    if (quit_mb == NULL) {
	Widget          tmp;
	XmString        lab;
	Arg             args[4];

	XtSetArg(args[0], XmNmwmDecorations, MWM_DECOR_BORDER);
	XtSetArg(args[1], XmNmwmFunctions, 0);
	XtSetArg(args[2], XmNdialogStyle, XmDIALOG_SYSTEM_MODAL);
	XtSetArg(args[3], XmNdefaultPosition, False);
	quit_mb = XmCreateQuestionDialog(toplevel, "quit_question",
					     args, 4);

	if (!quit_mb)
	    MWM_Done(0, NULL);
	tmp = XmMessageBoxGetChild(quit_mb, XmDIALOG_HELP_BUTTON);
	if (tmp)
	    XtUnmanageChild(tmp);

	tmp = XmMessageBoxGetChild(quit_mb, XmDIALOG_OK_BUTTON);
	XtAddCallback(tmp, XmNactivateCallback, quit_cb, NULL);

	tmp = XmMessageBoxGetChild(quit_mb, XmDIALOG_CANCEL_BUTTON);
	XtAddCallback(tmp, XmNactivateCallback,
		      cancel_cb, (XtPointer)&finished);

	tmp = XmMessageBoxGetChild(quit_mb, XmDIALOG_MESSAGE_LABEL);
	lab = XmStringCreateSimple("QUIT Mwm?");
	XtVaSetValues(tmp, XmNlabelString, lab, NULL);

	XmStringFree(lab);

	XtRealizeWidget(quit_mb);

	XtVaGetValues(quit_mb, XmNwidth, &wd, XmNheight, &ht, NULL);

	XtVaSetValues(XtParent(quit_mb),
		      XmNx, (scr->d_width - wd) / 2 - Mwm.frame_border_width,
		      XmNy, (scr->d_height - ht) / 2 - Mwm.frame_border_width,
		      NULL);
    }

    XtManageChild(quit_mb);
    scr->quit_win = XtWindow(XtParent(quit_mb));

    while (!finished) {
	XEvent   event;

	if (EVENT_Next(&event))
	    EVENT_Dispatch(&event);
    }
}

static void
noset_cb(Widget w, XtPointer calldata, XtPointer cbs)
{
    *((Boolean *)calldata) = True;
}

static void
toggle_behavior(Widget w, XtPointer calldata, XtPointer cbs)
{
    ScreenInfo *scr = (ScreenInfo *)calldata;

    if (PROP_GetBehavior(scr) & MWM_INFO_STARTUP_STANDARD)
	PROP_SetBehavior(scr, True);
    else
	PROP_SetBehavior(scr, False);
    XSync(dpy, 0);

    MWM_Done(1, g_argv[0]);
}

static void
set_behavior(ScreenInfo *scr)
{
    static Widget   toggle_mb = NULL;
    Boolean finished = False;
    Dimension wd, ht;

    MISC_Ungrab(scr);

    if (toggle_mb == NULL) {
	Widget          tmp;
	XmString        lab;
	Arg             args[4];

	XtSetArg(args[0], XmNmwmDecorations, MWM_DECOR_BORDER);
	XtSetArg(args[1], XmNmwmFunctions, 0);
	XtSetArg(args[2], XmNdialogStyle, XmDIALOG_SYSTEM_MODAL);
	XtSetArg(args[3], XmNdefaultPosition, False);
	toggle_mb = XmCreateQuestionDialog(toplevel, "toggle_question",
					    args, 4);

	if (!toggle_mb)
	    MWM_Done(0, NULL);

	tmp = XmMessageBoxGetChild(toggle_mb, XmDIALOG_HELP_BUTTON);
	XtUnmanageChild(tmp);

	tmp = XmMessageBoxGetChild(toggle_mb, XmDIALOG_OK_BUTTON);
	XtAddCallback(tmp, XmNactivateCallback,
		      toggle_behavior, scr);

	tmp = XmMessageBoxGetChild(toggle_mb, XmDIALOG_CANCEL_BUTTON);
	XtAddCallback(tmp, XmNactivateCallback,
		      noset_cb, (XtPointer)&finished);

	tmp = XmMessageBoxGetChild(toggle_mb, XmDIALOG_MESSAGE_LABEL);

	if (PROP_GetBehavior(scr) & MWM_INFO_STARTUP_STANDARD)
	    lab = XmStringCreateSimple("Toggle to Custom Behavior?");
	else
	    lab = XmStringCreateSimple("Toggle to Default Behavior?");
	XtVaSetValues(tmp, XmNlabelString, lab, NULL);

	XmStringFree(lab);

	XtRealizeWidget(toggle_mb);

	XtVaGetValues(toggle_mb,
		      XmNwidth, &wd, XmNheight, &ht, NULL);

	XtVaSetValues(XtParent(toggle_mb),
		      XmNx, (scr->d_width - wd) / 2 - Mwm.frame_border_width,
		      XmNy, (scr->d_height - ht) / 2 - Mwm.frame_border_width,
		      NULL);
    }

    XtManageChild(toggle_mb);
    scr->toggle_win = XtWindow(XtParent(toggle_mb));

    while (!finished) {
	XEvent   event;

	if (EVENT_Next(&event))
	    EVENT_Dispatch(&event);
    }
}
/*
 * Change by PRB (pete@tecc.co.uk), 31/10/93.  Prepend a hot key
 * specifier to each item in the list.  This means allocating the
 * memory for each item (& freeing it) rather than just using the window
 * title directly.
 */
static void
win_list(ScreenInfo *scr, int val1, int val2)
{
    MenuRoot       *mr;
    MenuItem       *mi, *tmp;
    MwmWindow      *t;
    char           *tname;
    char            loc[40], *name = NULL;
    int             dwidth, dheight;
    char            tlabel[50];
    int             last_desk_done = INT_MIN;
    int             next_desk;

    char           *t_hot;	/* Menu label with hotkey added */
    char            scut = '0';	/* Current short cut key */

    sprintf(tlabel, "CurrentDesk: %d", scr->current_desk);
    mr = MENU_Create(tlabel);
    MENU_AddItem(scr, mr, tlabel, "Geometry", NULL, F_TITLE, 0, 0, 's', 's');

    next_desk = 0;
    while (next_desk != INT_MAX) {
	/* Sort window list by desktop number */
	if ((val1 < 2) && (val1 > -2)) {
	    next_desk = INT_MAX;
	    for (t = scr->mwm_root.next; t != NULL; t = t->next) {
		if ((t->Desk > last_desk_done) && (t->Desk < next_desk))
		    next_desk = t->Desk;
	    }
	}
	else if ((val1 < 4) && (val1 > -4)) {
	    if (last_desk_done == INT_MIN)
		next_desk = scr->current_desk;
	    else
		next_desk = INT_MAX;
	}
	else {
	    if (last_desk_done == INT_MIN)
		next_desk = val2;
	    else
		next_desk = INT_MAX;
	}
	last_desk_done = next_desk;
	for (t = scr->mwm_root.next; t != NULL; t = t->next) {
	    if ((t->Desk == next_desk) &&
		(!(t->flags & WINDOWLISTSKIP))) {
		if (++scut == ('9' + 1))
		    scut = 'A';	/* Next shortcut key */
		if (val1 % 2 != 0)
		    name = t->icon_label;
		else
		    name = t->name;
		t_hot = XtMalloc(strlen(name) + 8);
		sprintf(t_hot, "%c.  %s", scut, name);		/* Generate label */

		tname = XtMalloc(40);
		tname[0] = 0;
		if (t->flags & ICONIFIED)
		    strcpy(tname, "(");
		sprintf(loc, "%d:", t->Desk);
		strcat(tname, loc);
		if (t->frame_x >= 0)
		    sprintf(loc, "+%d", t->frame_x);
		else
		    sprintf(loc, "%d", t->frame_x);
		strcat(tname, loc);
		if (t->frame_y >= 0)
		    sprintf(loc, "+%d", t->frame_y);
		else
		    sprintf(loc, "%d", t->frame_y);
		strcat(tname, loc);
		dheight = t->frame_height - t->title_height -
				2 * t->boundary_width -
				2 * t->matte_width;
		dwidth = t->frame_width -
				2 * t->boundary_width -
				2 * t->matte_width;

		dwidth -= t->hints.base_width;
		dheight -= t->hints.base_height;

		dwidth /= t->hints.width_inc;
		dheight /= t->hints.height_inc;

		sprintf(loc, "x%d", dwidth);
		strcat(tname, loc);
		sprintf(loc, "x%d", dheight);
		strcat(tname, loc);
		if (t->flags & ICONIFIED)
		    strcat(tname, ")");

		MENU_AddItem(scr, mr, t_hot, tname, NULL, F_RAISE_IT,
			     (long) t, (long) (t->w), 's', 's');
	    }
	}
    }

    MENU_Realize(scr, mr);

    MENU_PopupMenu(scr, mr);

    XDestroyWindow(dpy, mr->w);
    XDeleteContext(dpy, mr->w, MenuContext);
    /* need to free the window list ? */
    mi = mr->first;
    while (mi != NULL) {
	tmp = mi->next;
	if (mi->func != F_TITLE) {
	    if (mi->item != NULL)
		free(mi->item);
	    if (mi->item2 != NULL)
		free(mi->item2);
	}
	free(mi);
	mi = tmp;
    }
    free(mr);
}

/*
 * execute a mwm built in function
 */
void
FUNC_Execute(ScreenInfo *scr, int func, char *action, Window in_w,
	     MwmWindow *tmp_win, XEvent *eventp, unsigned long context,
	     long val1, long val2, int val1_unit, int val2_unit, MenuRoot *menu)
{
    MwmWindow      *t, *temp;
    int             x, y;
    Window          w;
    int             delta_x, delta_y;
    int             warp_x = 0, warp_y = 0;
    Pixel           TextColor, BackColor;
    Pixmap          BackPixmap;

    /* Defer Execution may wish to alter this value */
    w = in_w;

    switch (func) {
    case F_NOP:
    case F_TITLE:
	break;

    case F_BEEP:
	XBell(dpy, scr->screen);
	break;

    case F_CHANGE_WINDOWS_DESK:
	if (tmp_win == NULL)
	    break;

	DT_WindowChangingDesks(scr, tmp_win, val1);
	break;

    case F_CIRCULATE_UP:
	t = circulate(scr, tmp_win, action, UP);
	if (t)
	    WIN_ChangeFocus(scr, t, 0);
	break;

    case F_CIRCULATE_DOWN:
	t = circulate(scr, tmp_win, action, DOWN);
	if (t)
	    WIN_ChangeFocus(scr, t, 0);
	break;

    case F_CLOSE:
	if (tmp_win == NULL)
	    break;

	if (function_allowed(func, tmp_win) == 0) {
	    XBell(dpy, scr->screen);
	    break;
	}

	/* Dont delete the pager - it crashes the program! */
	if ((tmp_win->w == scr->pager_win) || (tmp_win == scr->mwm_pager))
	    break;

	if (tmp_win->flags & WM_DELS_WINDOW || tmp_win->flags & WM_SAVE_SELF) {
	    if (tmp_win->flags & WM_DELS_WINDOW)
		PROP_SendClientMessage(tmp_win->w, XA_WM_DELETE_WINDOW, CurrentTime);
	    else if (tmp_win->flags & WM_SAVE_SELF)
		PROP_SendClientMessage(tmp_win->w, XA_WM_SAVE_YOURSELF, CurrentTime);

	    wait_quit_timeout(scr, tmp_win);

	    break;
	}

	if (XGetGeometry(dpy, tmp_win->w, &JunkRoot, &JunkX, &JunkY,
			 &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth) == 0)
	    WIN_DestroyWindow(scr, tmp_win);
	else
	    XKillClient(dpy, tmp_win->w);
	XSync(dpy, 0);
	break;

    case F_DESK:
	DT_ChangeDesks(scr, val1, val2);
	break;

    case F_EXEC:
	{
	    char *shell;
	    extern char *getenv();

	    if ((shell = getenv(MWM_SHELL_NAME)) == NULL) {
		if ((shell = getenv(SHELL_NAME)) == NULL) {
		    shell = DEFAULT_SHELL;
		}
	    }
	    XGrabPointer(dpy, scr->root_win, True,
			    ButtonPressMask | ButtonReleaseMask,
			    GrabModeAsync, GrabModeAsync,
			    scr->root_win, scr->cursors[WAIT_CURS], CurrentTime);
	    XSync(dpy, 0);

	    if (!fork()) {
		if (execl(shell, shell, "-c", action, (char *) 0) == -1)
			exit(100);
	    }
	    XUngrabPointer(dpy, CurrentTime);
	    XSync(dpy, 0);
	}
	break;

    case F_FOCUS:
	WIN_ChangeFocus(scr, tmp_win, 0);
	break;

    case F_FOCUS_COLOR:
	fprintf(stderr, "FOCUS_COLOR NOT SUPPORTED YET\n");
	break;

    case F_GOTO_PAGE:
	/* back up 1 virtual desktop page */
	x = val1 * scr->d_width;
	y = val2 * scr->d_height;
	PAGER_MoveViewPort(scr, x, y, True);
	break;

    case F_ICONIFY:
	if (tmp_win == NULL)
	    break;
	if (tmp_win->flags & ICONIFIED) {
	    if (val1 <= 0)
		ICON_DeIconify(scr, tmp_win);
	}
	else {
	    if (function_allowed(func, tmp_win) == 0) {
		XBell(dpy, scr->screen);
		break;
	    }
	    if (val1 >= 0) {
		if (function_allowed(func, tmp_win) == 0) {
		    XBell(dpy, scr->screen);
		    break;
		}
		ICON_Iconify(scr, tmp_win,
			eventp->xbutton.x_root - 5,
			eventp->xbutton.y_root - 5);
	    }
	}
	break;

    case F_LOWER:
	if (tmp_win == NULL)
	    break;
	WIN_Lower(scr, MISC_RootOfTree(tmp_win));

	break;

    case F_MAXIMIZE:
	if (tmp_win == NULL)
	    break;

	if (function_allowed(func, tmp_win) == 0) {
	    XBell(dpy, scr->screen);
	    break;
	}
	maximize(scr, tmp_win, val1, val2, val1_unit, val2_unit);
	break;

    case F_MOVE:
	if (tmp_win == NULL)
	    break;

	if (function_allowed(func, tmp_win) == 0) {
	    XBell(dpy, scr->screen);
	    break;
	}

	move(scr, eventp, w, tmp_win, context, val1, val2, val1_unit, val2_unit);
	break;

    case F_MOVECURSOR:
	XQueryPointer(dpy, scr->root_win, &JunkRoot, &JunkChild,
		      &x, &y, &JunkX, &JunkY, &JunkMask);
	delta_x = 0;
	delta_y = 0;
	warp_x = 0;
	warp_y = 0;
	if (x >= scr->d_width - 2) {
	    delta_x = scr->edge_scroll_x;
	    warp_x = scr->edge_scroll_x - 4;
	}
	if (y >= scr->d_height - 2) {
	    delta_y = scr->edge_scroll_y;
	    warp_y = scr->edge_scroll_y - 4;
	}
	if (x < 2) {
	    delta_x = -scr->edge_scroll_x;
	    warp_x = -scr->edge_scroll_x + 4;
	}
	if (y < 2) {
	    delta_y = -scr->edge_scroll_y;
	    warp_y = -scr->edge_scroll_y + 4;
	}
	if (scr->virt_x + delta_x < 0)
	    delta_x = -scr->virt_x;
	if (scr->virt_y + delta_y < 0)
	    delta_y = -scr->virt_y;
	if (scr->virt_x + delta_x > scr->virt_x_max)
	    delta_x = scr->virt_x_max - scr->virt_x;
	if (scr->virt_y + delta_y > scr->virt_y_max)
	    delta_y = scr->virt_y_max - scr->virt_y;
	if ((delta_x != 0) || (delta_y != 0)) {
	    PAGER_MoveViewPort(scr, scr->virt_x + delta_x, scr->virt_y + delta_y, True);
	    XWarpPointer(dpy, scr->root_win, scr->root_win, 0, 0, scr->d_width,
			 scr->d_height,
			 x - warp_x,
			 y - warp_y);
	}
	XWarpPointer(dpy, scr->root_win, scr->root_win, 0, 0, scr->d_width,
		   scr->d_height, x + val1 * val1_unit / 100 - warp_x,
		     y + val2 * val2_unit / 100 - warp_y);

	break;

    case F_NEXT_CMAP:
	fprintf(stderr, "NEXT_COLORMAP NOT SUPPORTED YET\n");
	break;

    case F_NEXT_KEY:
	fprintf(stderr, "NEXT_KEY NOT SUPPORTED YET\n");
	break;

    case F_NORMALIZE:
	fprintf(stderr, "NORMALIZE NOT SUPPORTED YET\n");
	break;

    case F_NORM_AND_RAISE:
	fprintf(stderr, "NORM_AND_RAISE NOT SUPPORTED YET\n");
	break;

    case F_PACK_ICONS:
	fprintf(stderr, "PACK_ICONS NOT SUPPORTED YET\n");
	break;

    case F_PASS_KEYS:
	fprintf(stderr, "PASS_KEYS NOT SUPPORTED YET\n");
	break;

    case F_POPUP:
	MENU_Reset();
	menuFromFrameOrWindowOrTitlebar = FALSE;
	MENU_PopupMenu(scr, menu);
	break;

    case F_PREV_CMAP:
	fprintf(stderr, "PREV_CMAP NOT SUPPORTED YET\n");
	break;

    case F_PREV_KEY:
	fprintf(stderr, "PREV_KEY NOT SUPPORTED YET\n");
	break;

    case F_QUIT:
	if (Mwm.show_feedback & MWM_FEEDBACK_QUIT)
	    quit(scr);
	else
	    MWM_Done(0, NULL);
	break;

    case F_RAISE:
	if (tmp_win)
	    WIN_Raise(scr, MISC_RootOfTree(tmp_win));

	break;

    case F_RAISE_IT:
	if (val1 != 0) {
	    WIN_ChangeFocus(scr, (MwmWindow *) val1, 0);
	    if (((MwmWindow *) (val1))->flags & ICONIFIED) {
		ICON_DeIconify(scr, (MwmWindow *) val1);
		WIN_ChangeFocus(scr, (MwmWindow *) val1, 0);
	    }
	}
	break;

    case F_RAISELOWER:
	if (tmp_win == NULL)
	    break;

	if ((tmp_win == scr->mwm_last_raised) ||
	    (tmp_win->flags & VISIBLE)) {
	    WIN_Lower(scr, tmp_win);
	}
	else
	    WIN_Raise(scr, tmp_win);

	break;

    case F_REFRESH:
	{
	    XSetWindowAttributes attributes;
	    unsigned long   valuemask;

	    valuemask = (CWBackPixel);
	    attributes.background_pixel = scr->components[MWM_BORDER].foreground;
	    attributes.backing_store = NotUseful;
	    w = XCreateWindow(dpy, scr->root_win, 0, 0,
			      (unsigned int) scr->d_width,
			      (unsigned int) scr->d_height,
			      (unsigned int) 0,
			      CopyFromParent, (unsigned int) CopyFromParent,
			      (Visual *) CopyFromParent, valuemask,
			      &attributes);
	    XMapWindow(dpy, w);
	    XDestroyWindow(dpy, w);
	    XFlush(dpy);
	}
	break;

    case F_REFRESH_WIN:
	fprintf(stderr, "REFRESH_WIN NOT SUPPORTED YET\n");
	break;

    case F_RESIZE:
	if (tmp_win == NULL)
	    break;
	if (function_allowed(func, tmp_win) == 0) {
	    XBell(dpy, scr->screen);
	    break;
	}
	tmp_win->flags &= ~MAXIMIZED;
	RESIZE_EventLoop(scr, w, tmp_win, val1, val2, val1_unit, val2_unit);
	break;

    case F_RESTART:
	if (Mwm.show_feedback & MWM_FEEDBACK_RESTART)
	    restart(scr);
	else
	    MWM_Done(1, g_argv[0]);
	break;

    case F_RESTORE_AND_RAISE:
	fprintf(stderr, "RESTORE_AND_RAISE NOT SUPPORTED YET\n");
	break;

    case F_SCROLL:
	if ((val1 > -100000) && (val1 < 100000))
	    x = scr->virt_x + val1 * val1_unit / 100;
	else
	    x = scr->virt_x + (val1 / 1000) * val1_unit / 100;

	if ((val2 > -100000) && (val2 < 100000))
	    y = scr->virt_y + val2 * val2_unit / 100;
	else
	    y = scr->virt_y + (val2 / 1000) * val2_unit / 100;

	if (((val1 <= -100000) || (val1 >= 100000)) && (x > scr->virt_x_max)) {
	    x = 0;
	    y += scr->d_height;
	    if (y > scr->virt_y_max)
		y = 0;
	}
	if (((val1 <= -100000) || (val1 >= 100000)) && (x < 0)) {
	    x = scr->virt_x_max;
	    y -= scr->d_height;
	    if (y < 0)
		y = scr->virt_y_max;
	}
	if (((val2 <= -100000) || (val2 >= 100000)) && (y > scr->virt_y_max)) {
	    y = 0;
	    x += scr->d_width;
	    if (x > scr->virt_x_max)
		x = 0;
	}
	if (((val2 <= -100000) || (val2 >= 100000)) && (y < 0)) {
	    y = scr->virt_y_max;
	    x -= scr->d_width;
	    if (x < 0)
		x = scr->virt_x_max;
	}
	PAGER_MoveViewPort(scr, x, y, True);
	break;

    case F_SCREEN:
	fprintf(stderr, "SCREEN NOT SUPPORTED YET\n");
	break;

    case F_SEND_MSG:
	if (tmp_win && (tmp_win->flags & MWM_MESSAGES) &&
	    PROP_VerifyMwmMessage(tmp_win, atoi(action)))
	    PROP_SendMwmMessage(tmp_win->w, atoi(action), CurrentTime);
	break;

    case F_SET_BEHAVIOR:
	if (Mwm.show_feedback & MWM_FEEDBACK_BEHAVIOR)
	    set_behavior(scr);
	else
	    toggle_behavior(NULL, (XtPointer)scr, NULL);
	break;

    case F_STICK:
	if (tmp_win == NULL)
	    break;

	if (scr->mwm_highlight != tmp_win) {
	    /* Need to make DEC_DrawDecorations change the window back color */
	    temp = scr->mwm_highlight;
	    DEC_DrawDecorations(scr, tmp_win, True, True, True, None);
	    DEC_DrawDecorations(scr, tmp_win, False, True, True, None);
	    DEC_DrawDecorations(scr, temp, True, True, True, None);
	}
	PAGER_UpdateView(scr, tmp_win);

	/* Need to re-draw pager_view in case the window
	 * is unsticking */
	if (scr->mwm_highlight == tmp_win) {
	    TextColor = scr->components[MWM_PAGER].active_foreground;
	    BackPixmap = scr->components[MWM_PAGER].active_background_pixmap;
	    BackColor = scr->components[MWM_PAGER].active_background;
	}
	else {
	    TextColor = scr->components[MWM_PAGER].foreground;
	    BackPixmap = scr->components[MWM_PAGER].background_pixmap;
	    BackColor = scr->components[MWM_PAGER].background;
	}
	if (scr->d_depth < 2 && BackPixmap != XmUNSPECIFIED_PIXMAP)
	    XSetWindowBackgroundPixmap(dpy, tmp_win->pager_view, BackPixmap);
	else
	    XSetWindowBackground(dpy, tmp_win->pager_view, BackColor);
	XClearWindow(dpy, tmp_win->pager_view);
	if ((tmp_win->icon_label != NULL) &&
	    (scr->components[MWM_PAGER].f_height > 0)) {
	    XDrawString(dpy, tmp_win->pager_view,
			scr->components[MWM_PAGER].normal_GC,
			2, scr->components[MWM_PAGER].f_y + 2,
			tmp_win->icon_label, strlen(tmp_win->icon_label));
	}
	break;

    case F_TOGGLE_PAGE:
	if (DoHandlePageing)
	    DoHandlePageing = 0;
	else
	    DoHandlePageing = 1;
	PAN_CheckBounds(scr);
	break;

    case F_WARP:
	t = circulate(scr, tmp_win, action, DOWN);
	if ((t) && (t->flags & ICONIFIED)) {
	    WIN_ChangeFocus(scr, t, 0);
	    ICON_DeIconify(scr, t);
	}
	if (t)
	    WIN_ChangeFocus(scr, t, 0);
	break;

    case F_W_POPUP:
	MENU_Reset();
	menuFromFrameOrWindowOrTitlebar = TRUE;
	if (tmp_win && tmp_win->custom_menu)
	    menu = tmp_win->custom_menu;
	MENU_WinMenu(scr, menu, tmp_win,
		     (eventp->type == ButtonPress ||
		      eventp->type == ButtonRelease) &&
		     scr->event_context != C_MENUB);
	break;

    case F_WINDOWLIST:
	win_list(scr, val1, val2);
	break;
    }

    /* Only wait for an all-buttons-up condition after calls from
     * regular built-ins, not from complex-functions or modules. */
#if 0
    MISC_WaitForButtonsUp(scr);
#endif

    return;
}
