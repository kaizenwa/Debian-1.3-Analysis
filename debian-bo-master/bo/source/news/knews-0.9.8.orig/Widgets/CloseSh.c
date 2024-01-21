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
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#include "Compat.h"
#include "Util.h"
#include "CloseShP.h"

static XtResource resources[] = {
    {XtNinput, XtCInput, XtRBool, sizeof(Bool),
     XtOffsetOf(CloseShellRec, wm.wm_hints.input),
     XtRImmediate, (XtPointer)True},
    {XtNallowShellResize, XtCAllowShellResize, XtRBoolean, sizeof(Boolean),
     XtOffsetOf(CloseShellRec, shell.allow_shell_resize),
     XtRImmediate, (XtPointer)True},
#define offset(field) XtOffsetOf(CloseShellRec, close_shell.field)
    {XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
     offset(cursor), XtRString, (XtPointer)"top_left_arrow"},
    {XtNcloseCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(close_callback), XtRImmediate, (XtPointer)NULL},
#undef offset
};

static void	ClassPartInitialize(WidgetClass);
static void	Realize(Widget, XtValueMask*, XSetWindowAttributes*);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);

static void	close_window(Widget, XEvent*, String*, Cardinal*);

static XtActionsRec actions[] = {
    {"close-window",	close_window},
};

static char translations[] =
"<Message>WM_PROTOCOLS:	close-window() \n";

CloseShellClassRec closeShellClassRec = {
    {                                   /* core fields                  */
        (WidgetClass) &transientShellClassRec, /* superclass            */
        "CloseShell",                   /* class_name                   */
        sizeof(CloseShellRec),	        /* widget_size                  */
        NULL,                           /* class_initialize             */
        ClassPartInitialize,		/* class_part_initialize        */
        FALSE,                          /* class_inited                 */
        NULL,				/* initialize                   */
        NULL,                           /* initialize_hook              */
        Realize,                        /* realize                      */
        actions,                        /* actions                      */
        XtNumber(actions),              /* num_actions                  */
        resources,                      /* resources                    */
        XtNumber(resources),            /* num_resources                */
        NULLQUARK,                      /* xrm_class                    */
        TRUE,                           /* compress_motion              */
	FALSE,				/* compress_exposure		*/
        TRUE,                           /* compress_enterleave          */
        FALSE,                          /* visible_interest             */
        NULL,				/* destroy                      */
        XtInheritResize,                /* resize                       */
        NULL,	                        /* expose                       */
        SetValues,                      /* set_values                   */
        NULL,                           /* set_values_hook              */
        XtInheritSetValuesAlmost,       /* set_values_almost            */
        NULL,                           /* get_values_hook              */
        NULL,                           /* accept_focus                 */
        XtVersion,                      /* version                      */
        NULL,                           /* callback_private             */
        translations,                   /* tm_table                     */
        NULL,                           /* query_geometry               */
        XtInheritDisplayAccelerator,    /* display_accelerator          */
        NULL                            /* extension                    */
    },
    {					/* composite fields		*/
	XtInheritGeometryManager,	/* geometry_manager		*/
	XtInheritChangeManaged,		/* change_managed		*/
	XtInheritInsertChild,		/* insert_child			*/
	XtInheritDeleteChild,		/* delete_child			*/
	NULL,				/* extension			*/
    },
    {					/* shell fields			*/
	NULL,				/* extension			*/
    },
    {					/* wm shell fields		*/
	NULL,				/* extension			*/
    },
    {					/* vendor shell fields		*/
	NULL,				/* extension			*/
    },
    {					/* transient shell fields	*/
	NULL,				/* extension			*/
    },
    {					/* close_shell fields		*/
	NULL,				/* close_window			*/
	NULL,				/* extension			*/
    }
};

WidgetClass closeShellWidgetClass = (WidgetClass)&closeShellClassRec;

/*************************************************************************/

static void close_window(Widget gw, XEvent *event,
			 String *params, Cardinal *no_params)
{
    CloseShellWidget		w = (CloseShellWidget)gw;
    CloseShellWidgetClass	class;

    class = (CloseShellWidgetClass)XtClass(w);
    if (!class->close_shell_class.close_window) {
	XtCallCallbackList((Widget)w, w->close_shell.close_callback, NULL);
	return;
    }

    if (event->type == ClientMessage) {
	Display	*disp = XtDisplay(w);
	Atom	wm_delete_window = intern_atom(disp, "WM_DELETE_WINDOW");
	Atom	wm_protocols     = intern_atom(disp, "WM_PROTOCOLS");

	if (event->xclient.message_type == wm_protocols &&
	    event->xclient.data.l[0]    == wm_delete_window)
	    class->close_shell_class.close_window((Widget)w);
    }
}

/*************************************************************************/

static void ClassPartInitialize(WidgetClass gclass)
{
    CloseShellWidgetClass	class, super;

    class = (CloseShellWidgetClass)gclass;
    super = (CloseShellWidgetClass)class->core_class.superclass;

    if (class->close_shell_class.close_window == XtInheritCloseWindow)
	class->close_shell_class.close_window =
	    super->close_shell_class.close_window;
}

static void Realize(Widget gw, XtValueMask *mask,
		    XSetWindowAttributes *attributes)
{
    CloseShellWidget	w = (CloseShellWidget)gw;
    Display		*disp = XtDisplay(w);
    Atom		wm_delete_window;

    if (w->close_shell.cursor != None) {
	*mask |= CWCursor;
	attributes->cursor = w->close_shell.cursor;
    }

    transientShellWidgetClass->core_class.realize((Widget)w, mask, attributes);

    wm_delete_window = intern_atom(disp, "WM_DELETE_WINDOW");
    XSetWMProtocols(disp, XtWindow(w), &wm_delete_window, 1);
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    Boolean		redisplay = False;
    CloseShellWidget	new       = (CloseShellWidget)gnew;
    CloseShellWidget	current   = (CloseShellWidget)gcurrent;

    if (new->close_shell.cursor != current->close_shell.cursor)
	CloseShellSetCursor((Widget)new, new->close_shell.cursor);

    return redisplay;
}

/*************************************************************************/

void CloseShellSetCursor(Widget gw, Cursor cursor)
{
    CloseShellWidget	w = (CloseShellWidget)gw;
    Display		*disp = XtDisplay(w);
    Window		win = XtWindow(w);

    w->close_shell.cursor = cursor;
    if (XtIsRealized((Widget)w))
	XDefineCursor(disp, win, cursor);
}
