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
#include "KnappP.h"
#include "Layout.h"
#include "Message.h"
#include "Util.h"

#include "NoticeP.h"

static XtResource resources[] = {
#define offset(field) XtOffsetOf(NoticeRec, notice.field)
    {XtNcallback, XtCCallback, XtRCallback, sizeof(XtPointer),
     offset(callback), XtRCallback, (XtPointer)NULL},
    {XtNmessage, XtCMessage, XtRString, sizeof(String),
     offset(message), XtRImmediate, (XtPointer)NULL},
    {XtNleftLabel, XtCLabel, XtRBoolean, sizeof(String),
     offset(left_label), XtRImmediate, (XtPointer)NULL},
    {XtNmiddleLabel, XtCLabel, XtRBoolean, sizeof(String),
     offset(middle_label), XtRImmediate, (XtPointer)NULL},
    {XtNrightLabel, XtCLabel, XtRBoolean, sizeof(String),
     offset(right_label), XtRImmediate, (XtPointer)NULL},
    {XtNtimeout, XtCTimeout, XtRLong, sizeof(long),
     offset(timeout), XtRImmediate, (XtPointer)0},
    {XtNleftKnapp, XtCWidget, XtRWidget, sizeof(Widget),
     offset(left_knapp), XtRImmediate, (XtPointer)NULL},
    {XtNmiddleKnapp, XtCWidget, XtRWidget, sizeof(Widget),
     offset(middle_knapp), XtRImmediate, (XtPointer)NULL},
    {XtNrightKnapp, XtCWidget, XtRWidget, sizeof(Widget),
     offset(right_knapp), XtRImmediate, (XtPointer)NULL},
#undef offset
};

static void	Initialize(Widget, Widget, ArgList, Cardinal*);
static void	Destroy(Widget);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);

static void	close_notice(Widget);

NoticeClassRec noticeClassRec = {
    {                                   /* core fields                  */
        (WidgetClass) &closeShellClassRec, /* superclass                */
        "Notice",                       /* class_name                   */
        sizeof(NoticeRec),	        /* widget_size                  */
        NULL,                           /* class_initialize             */
        NULL,                           /* class_part_initialize        */
        FALSE,                          /* class_inited                 */
        Initialize,                     /* initialize                   */
        NULL,                           /* initialize_hook              */
        XtInheritRealize,               /* realize                      */
        NULL,                           /* actions                      */
        0,                              /* num_actions                  */
        resources,                      /* resources                    */
        XtNumber(resources),            /* num_resources                */
        NULLQUARK,                      /* xrm_class                    */
        TRUE,                           /* compress_motion              */
	FALSE,				/* compress_exposure		*/
        TRUE,                           /* compress_enterleave          */
        FALSE,                          /* visible_interest             */
        Destroy,                        /* destroy                      */
        XtInheritResize,                /* resize                       */
        NULL,	                        /* expose                       */
        SetValues,                      /* set_values                   */
        NULL,                           /* set_values_hook              */
        XtInheritSetValuesAlmost,       /* set_values_almost            */
        NULL,                           /* get_values_hook              */
        NULL,                           /* accept_focus                 */
        XtVersion,                      /* version                      */
        NULL,                           /* callback_private             */
        XtInheritTranslations,		/* tm_table                     */
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
    {
	close_notice,			/* close_window			*/
	NULL,				/* extension			*/
    },
    {					/* notice fields		*/
	NULL,				/* extension			*/
    }
};

WidgetClass noticeWidgetClass = (WidgetClass)&noticeClassRec;

/*************************************************************************/

static char *layout_string[] = {
"vertical { "
"	message "
"} ",
"vertical { "
"	height knapp1 <+inf-inf> "
"	horizontal { "
"		height knapp1 <+inf-inf> "
"		message "
"		height knapp1 <+inf-inf> "
"	} "
"	height knapp1 <+inf-inf> "
"	horizontal { "
"		height knapp1 <+inff-inff> "
"		knapp1 "
"		height knapp1 <+inff-inff> "
"	} "
"	height knapp1 <+inf-inf> "
"}",
"vertical { "
"	height knapp1 <+inf-inf> "
"	horizontal { "
"		height knapp1 <+inf-inf> "
"		message "
"		height knapp1 <+inf-inf> "
"	} "
"	height knapp1 <+inf-inf> "
"	horizontal { "
"		height knapp1 <+inff-inff> "
"		knapp1 "
"		height knapp1 <+3inff-inff> "
"		knapp2 "
"		height knapp1 <+inff-inff> "
"	} "
"	height knapp1 <+inf-inf> "
"}",
"vertical { "
"	height knapp1 <+inf-inf> "
"	horizontal { "
"		height knapp1 <+inf-inf> "
"		message "
"		height knapp1 <+inf-inf> "
"	} "
"	height knapp1 <+inf-inf> "
"	horizontal { "
"		height knapp1 <+inff-inff> "
"		knapp1 "
"		height knapp1 <+3inff-inff> "
"		knapp2 "
"		height knapp1 <+3inff-inff> "
"		knapp3 "
"		height knapp1 <+inff-inff> "
"	} "
"	height knapp1 <+inf-inf> "
"} "};

static void timeout_callback(XtPointer client_data, XtIntervalId *id)
{
    NoticeWidget	w = (NoticeWidget)client_data;
    XtCallbackList	c_list = w->notice.callback;

    w->notice.timer = 0;
    if (c_list)
	XtCallCallbackList((Widget)w, c_list, (XtPointer)NoticeReplyTimeout);
    else
	XtDestroyWidget((Widget)w);
}

static void close_notice(Widget gw)
{
    NoticeWidget	w = (NoticeWidget)gw;
    XtCallbackList	c_list = w->notice.callback;

    if (c_list)
	XtCallCallbackList((Widget)w, c_list, (XtPointer)NoticeReplyClose);
    else
	XtDestroyWidget((Widget)w);
}

static void knapp_callback(Widget knapp,
			   XtPointer client_data,
			   XtPointer call_data)
{
    NoticeWidget	w = (NoticeWidget)client_data;
    XtCallbackList	c_list = w->notice.callback;

    if (!c_list)
	XtDestroyWidget((Widget)w);
    else if (knapp == w->notice.left_knapp)
	XtCallCallbackList((Widget)w, c_list, (XtPointer)NoticeReplyLeft);
    else if (knapp == w->notice.middle_knapp)
	XtCallCallbackList((Widget)w, c_list, (XtPointer)NoticeReplyMiddle);
    else if (knapp == w->notice.right_knapp)
	XtCallCallbackList((Widget)w, c_list, (XtPointer)NoticeReplyRight);
}

/*************************************************************************/

static void Initialize(Widget grequest, Widget gnew,
		       ArgList gargs, Cardinal *no_args)
{
    static char		*name[] = {"knapp1", "knapp2", "knapp3"};
    NoticeWidget	new = (NoticeWidget)gnew;
    Arg			args[2];
    int			n;

    n = 0;
    if (new->notice.left_label)
	n++;
    if (new->notice.middle_label)
	n++;
    if (new->notice.right_label)
	n++;

    new->notice.layout =
	XtVaCreateManagedWidget("layout", layoutWidgetClass,
				(Widget)new,
				XtVaTypedArg, XtNlayout, XtRString,
				layout_string[n], (int)sizeof(String),
				(void *)0);

    n = 0;
    if (!new->notice.left_label)
	new->notice.left_knapp = NULL;
    else {
	XtSetArg(args[0], XtNlabel, new->notice.left_label);
	new->notice.left_knapp =
	    XtCreateManagedWidget(name[n], knappWidgetClass,
				  new->notice.layout, args, 1);
	XtAddCallback(new->notice.left_knapp, XtNcallback,
		      knapp_callback, (XtPointer)new);
	n++;
    }

    if (!new->notice.middle_label)
	new->notice.middle_knapp = NULL;
    else {
	XtSetArg(args[0], XtNlabel, new->notice.middle_label);
	new->notice.middle_knapp =
	    XtCreateManagedWidget(name[n], knappWidgetClass,
				  new->notice.layout, args, 1);
	XtAddCallback(new->notice.middle_knapp, XtNcallback,
		      knapp_callback, (XtPointer)new);
	n++;
    }

    if (!new->notice.right_label)
	new->notice.right_knapp = NULL;
    else {
	XtSetArg(args[0], XtNlabel, new->notice.right_label);
	new->notice.right_knapp =
	    XtCreateManagedWidget(name[n], knappWidgetClass,
				  new->notice.layout, args, 1);
	XtAddCallback(new->notice.right_knapp, XtNcallback,
		      knapp_callback, (XtPointer)new);
	n++;
    }

    XtSetArg(args[0], XtNbuffer, new->notice.message);
    XtSetArg(args[1], XtNborderWidth, 0);
    new->notice.message_widget =
	XtCreateManagedWidget("message", messageWidgetClass,
			      new->notice.layout, args, 2);
    new->notice.message = NULL;

    if (new->notice.timeout != 0)
	new->notice.timer =
	    XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)new),
			    (unsigned long)new->notice.timeout,
			    timeout_callback, (XtPointer)new);
    else
	new->notice.timer = 0;
}

static void Destroy(Widget gw)
{
    NoticeWidget	w = (NoticeWidget)gw;

    if (w->notice.timer > 0)
	XtRemoveTimeOut(w->notice.timer);
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    Boolean		redisplay = False;
    NoticeWidget	new = (NoticeWidget)gnew;

    if (new->notice.message) {
	MessageSetAndRedraw(new->notice.message_widget,
			    new->notice.message, False);
	new->notice.message = NULL;
    }

    return redisplay;
}

/*************************************************************************/

void NoticeSetMessage(Widget gw, String message)
{
    NoticeWidget	w = (NoticeWidget)gw;
    Arg			arg;

    XtSetArg(arg, XtNbuffer, message);
    XtSetValues(w->notice.message_widget, &arg, 1);
}

void NoticeSetLeftLabel(Widget gw, String label)
{
    NoticeWidget	w = (NoticeWidget)gw;
    Arg			arg;

    if (w->notice.left_knapp) {
	XtSetArg(arg, XtNlabel, label);
	XtSetValues(w->notice.left_knapp, &arg ,1);
    }
}

Widget NoticeMessageWidget(Widget gw)
{
    NoticeWidget	w = (NoticeWidget)gw;

    return w->notice.message_widget;
}
