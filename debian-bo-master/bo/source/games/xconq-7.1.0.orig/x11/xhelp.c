/* Help for the X11 interface to Xconq.
   Copyright (C) 1991, 1992, 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
#include "xconq.h"
void update_help PARAMS ((Side *side));

/* (should go to ui.h?  and node stack code too?) */
#define NODESTACKSIZE 100

static void describe_map PARAMS ((int arg, char *key, char *buf));

static void help_next_callback PARAMS ((Widget w, XtPointer client_data,
				       XtPointer call_data));
static void help_prev_callback PARAMS ((Widget w, XtPointer client_data,
				       XtPointer call_data));
static void help_back_callback PARAMS ((Widget w, XtPointer client_data,
				       XtPointer call_data));
static void help_close_callback PARAMS ((Widget w, XtPointer client_data,
				        XtPointer call_data));
static void interp_help_command PARAMS ((Side *side, int ch));
static void jump_to_help_node PARAMS ((Side *side, int i));

static void help_topic_callback PARAMS ((Widget w, XtPointer dummy,
					XawListReturnStruct *list));

static char **nodetitles;

static void
describe_map(arg, key, buf)
int arg;
char *key, *buf;
{
#define SCB(s) strcat(buf,s)
    SCB("In move mode:\n");
    SCB("  The next unit that can do anything will be selected automatically.\n");
    SCB("  Left-click on a destination to move the selected unit there.\n");
    SCB("  To select a unit, center-click it.\n");
    SCB("\n");
    SCB("In look mode:\n");
    SCB("  Left-click on a unit to make the current one.\n");
    SCB("  To move it, right-click, use 'm'ove command or 'z' to switch back ");
    SCB("to move mode.\n");
    SCB("\n");
    SCB("In design mode:\n");
    SCB("  Left-click to perform the tool-specific action: add unit or paint ");
    SCB("terrain, population, or feature.\n");
    SCB("  Shift-left-click to perform the tool-specific current object ");
    SCB("selection: pick current unit type, terrain type, side, or feature.\n");
    SCB("\n");
    SCB("In any mode:\n");
    SCB("  Center-click on a unit or cell to make the current one.\n");
    SCB("  Drag-shift-center-click to measure distance (gasp! hold down Shift, ");
    SCB("press center button at origin, drag to destination, release button ");
    SCB("and Shift).\n");
    SCB("  Right-click on a destination to move the selected unit there.\n");
    SCB("  Move the mouse while holding Meta to change the current cell.\n");
    SCB("  Press a keypad key to scroll the look.\n");
#undef SCB
}

static void 
help_topic_callback (w, dummy, list)
Widget w;
XtPointer dummy;
XawListReturnStruct *list;
{
    Side *side;

    if (!find_side_via_widget(w, &side))
      return;

    jump_to_help_node(side, list->list_index);
}

static void 
jump_to_help_node(side, i)
Side *side;
int i;
{
    i = i % side->ui->nodenumber;
    if (i < 0)
      i += side->ui->nodenumber;
    XawListHighlight(side->ui->help_topicList, i);
    side->ui->curhelpnode = side->ui->nodestack[i];
    side->ui->nodestackpos = i;
    update_help(side);
}

void
create_help(side)
Side *side;
{
    int i;
    HelpNode *hnp;

    add_help_node("map", describe_map, 0, first_help_node);

    /* initialize node stack */
    if (side->ui->nodestack == NULL)
      side->ui->nodestack =
	(HelpNode **) xmalloc(1000 * sizeof(HelpNode *));

    for (i = 0, hnp = first_help_node;
	 hnp && (hnp != first_help_node || i == 0); hnp = hnp->next) {
	side->ui->nodestack[i++] = hnp;
    }
    side->ui->nodenumber = i;

    nodetitles = (char **) xmalloc((side->ui->nodenumber+1)*sizeof(char *));
    for (i=0; i<side->ui->nodenumber; i++) {
	nodetitles[i] = side->ui->nodestack[i]->key;
    }
    nodetitles[side->ui->nodenumber] = NULL;

#if 0
    /* useless with widgetified help, use resource file to change defaults */

    side->ui->helpw = 500;  side->ui->helph = 700;
    if ( 0 /* !empty_string(geospec) */) {
	/* This map has been given a preferred size - work to it. */
	flags = XParseGeometry(geospec, &sx, &sy, &w, &h);
	if (flags & WidthValue)
	  side->ui->helpw = w;
	if (flags & HeightValue)
	  side->ui->helph = h;
	/* Note that we allow the caller to specify geometries
	   that may be larger than the available screen, on the
	   theory that people specifying geometries explicitly
	   know what they're doing. */
    } else {
        int w, h;

	/* Shrink to fit display if the default is too big. */
	w = DisplayWidth(side->ui->dpy, side->ui->screen);
	h = DisplayHeight(side->ui->dpy, side->ui->screen);
	/* Leave a little room around the edges, but not too much;
	   Xconq help needs all the screen space it can get. */
	side->ui->helpw = min(side->ui->helpw, w - w / 16);
	side->ui->helph = min(side->ui->helph, h - h / 16);
    }
#endif

    side->ui->help_shell =
      XtVaCreatePopupShell("helpShell", topLevelShellWidgetClass,
			   side->ui->shell,  NULL);
    side->ui->help_form = 
      XtVaCreateManagedWidget("form", formWidgetClass,
			      side->ui->help_shell,  NULL);

    side->ui->help_title =
      XtVaCreateManagedWidget("title", labelWidgetClass, side->ui->help_form,
			      XtNlabel, "",
			      XtNtop,    XawChainTop, 
			      XtNbottom, XawChainTop, 
			      XtNleft,   XawChainLeft, 
			      XtNright,  XawChainRight, 
			      NULL);

    side->ui->help_topicPort =
      XtVaCreateManagedWidget("topicPort", viewportWidgetClass,
			      side->ui->help_form,
			      XtNallowVert, True,
			      XtNfromVert, side->ui->help_title,
			      XtNtop,    XawChainTop, 
			      XtNbottom, XawChainBottom, 
			      XtNleft,   XawChainLeft, 
			      XtNright,  XawChainLeft, 
			      NULL);

    side->ui->help_topicList =
      XtVaCreateManagedWidget("topicList", listWidgetClass,
			      side->ui->help_topicPort,
			      XtNbottom, XawChainTop, 
			      XtNverticalList, True,
			      XtNforceColumns, True,
			      XtNdefaultColumns, 1,
			      NULL);
    XawListChange(side->ui->help_topicList,
		  nodetitles, side->ui->nodenumber, 0, True);
    XtAddCallback(side->ui->help_topicList, XtNcallback,
		  (XtCallbackProc) help_topic_callback, (XtPointer) NULL);

    side->ui->help_text =
      XtVaCreateManagedWidget("text", asciiTextWidgetClass, side->ui->help_form,
			      XtNdisplayCaret, False,
			      XtNeditType, XawtextRead,
/*			      XtNscrollHorizontal, XawtextScrollWhenNeeded, */
			      XtNscrollVertical, XawtextScrollAlways,
			      XtNwrap, XawtextWrapWord,
			      XtNfromVert,  side->ui->help_title,
			      XtNfromHoriz, side->ui->help_topicPort,
			      XtNtop,    XawChainTop, 
			      XtNbottom, XawChainBottom, 
			      XtNleft,   XawChainLeft, 
			      XtNright,  XawChainRight, 
			      NULL);

    side->ui->help_next =
      XtVaCreateManagedWidget("next", commandWidgetClass, side->ui->help_form,
			      XtNlabel, "Next",
			      XtNfromVert, side->ui->help_text,
			      XtNtop,    XawChainBottom, 
			      XtNbottom, XawChainBottom, 
			      XtNleft,   XawChainLeft, 
			      XtNright,  XawChainLeft, 
			      NULL);
    XtAddCallback(side->ui->help_next, XtNcallback, help_next_callback, NULL);
    side->ui->help_prev =
      XtVaCreateManagedWidget("prev", commandWidgetClass, side->ui->help_form,
			      XtNlabel, "Prev",
			      XtNfromHoriz, side->ui->help_next,
			      XtNfromVert,  side->ui->help_text,
			      XtNtop,    XawChainBottom, 
			      XtNbottom, XawChainBottom, 
			      XtNleft,   XawChainLeft, 
			      XtNright,  XawChainLeft, 
			      NULL);
    XtAddCallback(side->ui->help_prev, XtNcallback, help_prev_callback, NULL);
    side->ui->help_back =
      XtVaCreateManagedWidget("back", commandWidgetClass, side->ui->help_form,
			      XtNlabel, "Back",
			      XtNfromHoriz, side->ui->help_prev,
			      XtNfromVert,  side->ui->help_text,
			      XtNtop,    XawChainBottom, 
			      XtNbottom, XawChainBottom, 
			      XtNleft,   XawChainLeft, 
			      XtNright,  XawChainLeft, 
			      NULL);
    XtAddCallback(side->ui->help_back, XtNcallback, help_back_callback, NULL);
    side->ui->help_close =
      XtVaCreateManagedWidget("close", commandWidgetClass, side->ui->help_form,
			      XtNlabel, "Close",
			      XtNfromHoriz, side->ui->help_back,
			      XtNfromVert,  side->ui->help_text,
			      XtNtop,    XawChainBottom, 
			      XtNbottom, XawChainBottom, 
			      XtNleft,   XawChainLeft, 
			      XtNright,  XawChainLeft, 
			      NULL);
    XtAddCallback(side->ui->help_close, XtNcallback, help_close_callback, NULL);

    XtRealizeWidget(side->ui->help_shell);

    /* which is the most useful help page to display at startup? */
    jump_to_help_node (side, 2);
}

static void 
help_next_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Side *side;

    if (find_side_via_widget(w, &side)) {
	interp_help_command(side, 'n');
    }
}

static void 
help_prev_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Side *side;

    if (find_side_via_widget(w, &side)) {
	interp_help_command(side, 'p');
    }
}

static void 
help_back_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Side *side;

    if (find_side_via_widget(w, &side)) {
	interp_help_command(side, 'b');
    }
}

static void 
help_close_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Side *side;

    if (find_side_via_widget(w, &side)) {
	popdown_help(side);
    }
}

/* Keyboard command interpretation for the help window. */

static void
interp_help_command(side, ch)
Side *side;
int ch;
{
    int i;

    switch (ch) {
      case 'n':
        i = side->ui->nodestackpos+1;
        break;
      case 'p':
        i = side->ui->nodestackpos-1;
	break;
      case 'b':
	if (side->ui->nodestackpos <= 0) {
	    beep(side);
	    return;
	}
	i = side->ui->nodestackpos-1;
	break;
      case 't':
	i = 0;
	break;
      default:
	abort();  /* should never happen */
    }

    jump_to_help_node(side, i);
}

void
update_help(side)
Side *side;
{
    char *keystr, *helpstr;

    if (!side->ui->help_shell)
      return;
    if (side->ui->curhelpnode == NULL)
      return; /* (should never happen?) */

    /* Update the topic title widget. */
    keystr = side->ui->curhelpnode->key;
    if (keystr == NULL)
      keystr = "???";
    nargs = 0;
    XtSetArg(tmpargs[nargs], XtNlabel, keystr);  nargs++;
    XtSetValues(side->ui->help_title, tmpargs, nargs);

    /* Update the help info proper. */
    helpstr = get_help_text(side->ui->curhelpnode);
    if (helpstr == NULL)
      helpstr = "???";
    /* This shouldn't be necessary, but apparently some text widgets
       go berserk when handed zero-length strings.  (This happens
       on several different platforms.) */
    if (strlen(helpstr) == 0)
      helpstr = " ";
    nargs = 0;
    XtSetArg(tmpargs[nargs], XtNstring, helpstr);  nargs++;
    XtSetArg(tmpargs[nargs], XtNlength, strlen(helpstr));  nargs++;
    XtSetArg(tmpargs[nargs], XtNinsertPosition, 0);  nargs++;
    XtSetValues(side->ui->help_text, tmpargs, nargs);
}

void
popup_help(side)
Side *side;
{
    if (!side->ui->help_shell) {
	create_help(side);
	update_help(side);

	XSetWMProtocols(side->ui->dpy, XtWindow(side->ui->help_shell),
			&side->ui->kill_atom, 1);
	XtOverrideTranslations(side->ui->help_shell,
		XtParseTranslationTable("<Message>WM_PROTOCOLS: wm-quit()"));
    }
    XtPopup(side->ui->help_shell, XtGrabNone);
    /* Expose event will cause window contents to be drawn now. */
}

void
popdown_help(side)
Side *side;
{
    if (side->ui->help_shell)
      XtPopdown(side->ui->help_shell);
}
