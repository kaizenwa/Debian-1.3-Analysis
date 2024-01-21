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
#include "Knapp.h"
#include "Layout.h"
#include "Message.h"
#include "TextField.h"
#include "Util.h"

#include "DialogueP.h"

static XtResource resources[] = {
#define offset(field) XtOffsetOf(DialogueRec, dialogue.field)
    {XtNcallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(callback), XtRCallback, (XtPointer)NULL},
    {XtNmessage, XtCMessage, XtRString, sizeof(String),
     offset(message), XtRImmediate, (XtPointer)NULL},
    {XtNbuffer, XtCBuffer, XtRString, sizeof(String),
     offset(buffer), XtRImmediate, (XtPointer)NULL},
    {XtNleftLabel, XtCLabel, XtRBoolean, sizeof(String),
     offset(left_label), XtRImmediate, (XtPointer)NULL},
    {XtNmiddleLabel, XtCLabel, XtRBoolean, sizeof(String),
     offset(middle_label), XtRImmediate, (XtPointer)NULL},
    {XtNrightLabel, XtCLabel, XtRBoolean, sizeof(String),
     offset(right_label), XtRImmediate, (XtPointer)NULL},
#undef offset
};

static void	Initialize(Widget, Widget, ArgList, Cardinal*);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);

static void	close_dialogue(Widget);

DialogueClassRec dialogueClassRec = {
    {                                   /* core fields                  */
        (WidgetClass) &closeShellClassRec, /* superclass                */
        "Dialogue",                     /* class_name                   */
        sizeof(DialogueRec),	        /* widget_size                  */
        NULL,                           /* class_initialize             */
        NULL,                           /* class_part_initialize        */
        FALSE,                          /* class_inited                 */
        Initialize,                     /* initialize                   */
        NULL,                           /* initialize_hook              */
        XtInheritRealize,               /* realize                      */
        NULL,	                        /* actions                      */
        0,                              /* num_actions                  */
        resources,                      /* resources                    */
        XtNumber(resources),            /* num_resources                */
        NULLQUARK,                      /* xrm_class                    */
        TRUE,                           /* compress_motion              */
	FALSE,				/* compress_exposure		*/
        TRUE,                           /* compress_enterleave          */
        FALSE,                          /* visible_interest             */
        NULL,                           /* destroy                      */
        XtInheritResize,                /* resize                       */
        NULL,	                        /* expose                       */
        SetValues,                      /* set_values                   */
        NULL,                           /* set_values_hook              */
        XtInheritSetValuesAlmost,       /* set_values_almost            */
        NULL,				/* get_values_hook              */
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
	close_dialogue,			/* close_window			*/
	NULL,				/* extension			*/
    },
    {					/* dialogue fields		*/
	NULL,				/* extension			*/
    }
};

WidgetClass dialogueWidgetClass = (WidgetClass)&dialogueClassRec;

/*************************************************************************/

static const char *layout_string =
"vertical { "
"	height left <+inf-inf> "
"	horizontal { "
"		height left <+inf-inf> "
"		message "
"		height left <+inf-inf> "
"	} "
"	height left <+inf-inf> "
"	horizontal { "
"		height left "
"		textfield <+inf-inf*> "
"		height left "
"	} "
"	height left <+inf-inf> "
"	horizontal { "
"		height left <+inf-inf> "
"		left "
"		height left <+3inf-inf> "
"		middle "
"		height left <+3inf-inf> "
"		right "
"		height left <+inf-inf> "
"	} "
"	height left <+inf-inf> "
"} ";

static void call_callbacks(DialogueWidget w, DialogueReply reply)
{
    XtCallbackList	c_list = w->dialogue.callback;
    DialogueReport	report;

    if (!c_list)
	return;

    report.reply = reply;
    report.buffer = TextFieldGetBuffer(w->dialogue.text_field);
    XtCallCallbackList((Widget)w, c_list, (XtPointer)&report);
    XtFree(report.buffer);
}

static void close_dialogue(Widget gw)
{
    DialogueWidget	w = (DialogueWidget)gw;

    call_callbacks(w, DialogueReplyClose);
}

static void knapp_callback(Widget knapp,
			   XtPointer client_data,
			   XtPointer call_data)
{
    DialogueWidget	w = (DialogueWidget)client_data;

    if (knapp == w->dialogue.left_knapp)
	call_callbacks(w, DialogueReplyLeft);
    else if (knapp == w->dialogue.middle_knapp)
	call_callbacks(w, DialogueReplyMiddle);
    else if (knapp == w->dialogue.right_knapp)
	call_callbacks(w, DialogueReplyRight);
}

static void enter_callback(Widget gw,
			   XtPointer client_data,
			   XtPointer call_data)
{
    DialogueWidget	w = (DialogueWidget)client_data;
    XtCallbackList	c_list = w->dialogue.callback;
    DialogueReport	report;

    report.reply = DialogueReplyEnter;
    report.buffer = (String)call_data;

    if (c_list)
	XtCallCallbackList((Widget)w, c_list, (XtPointer)&report);
}

static void tab_callback(Widget gw,
			 XtPointer client_data,
			 XtPointer call_data)
{
    DialogueWidget	w = (DialogueWidget)client_data;
    XtCallbackList	c_list = w->dialogue.callback;
    DialogueReport	report;

    report.reply = DialogueReplyTab;
    report.buffer = (String)call_data;

    if (c_list)
	XtCallCallbackList((Widget)w, c_list, (XtPointer)&report);
}

/*************************************************************************/

static void Initialize(Widget grequest, Widget gnew,
		       ArgList gargs, Cardinal *no_args)
{
    DialogueWidget	new = (DialogueWidget)gnew;
    Arg			args[2];

    new->dialogue.layout =
	XtVaCreateManagedWidget("layout", layoutWidgetClass,
				(Widget)new,
				XtVaTypedArg, XtNlayout, XtRString,
				layout_string, (int)sizeof(String),
				(void *)0);

    XtSetArg(args[0], XtNlabel, new->dialogue.left_label);
    new->dialogue.left_knapp =
	XtCreateManagedWidget("left", knappWidgetClass,
			      new->dialogue.layout, args, 1);
    XtAddCallback(new->dialogue.left_knapp, XtNcallback,
		  knapp_callback, (XtPointer)new);

    XtSetArg(args[0], XtNlabel, new->dialogue.middle_label);
    new->dialogue.middle_knapp =
	XtCreateManagedWidget("middle", knappWidgetClass,
			      new->dialogue.layout, args, 1);
    XtAddCallback(new->dialogue.middle_knapp, XtNcallback,
		  knapp_callback, (XtPointer)new);

    XtSetArg(args[0], XtNlabel, new->dialogue.right_label);
    new->dialogue.right_knapp =
	XtCreateManagedWidget("right", knappWidgetClass,
			      new->dialogue.layout, args, 1);
    XtAddCallback(new->dialogue.right_knapp, XtNcallback,
		  knapp_callback, (XtPointer)new);

    XtSetArg(args[0], XtNbuffer, new->dialogue.buffer);
    XtSetArg(args[1], XtNsingleLine, True);
    new->dialogue.text_field =
	XtCreateManagedWidget("textfield", textFieldWidgetClass,
			      new->dialogue.layout, args, 2);
    XtAddCallback(new->dialogue.text_field, XtNcallback,
		  enter_callback, (XtPointer)new);
    XtAddCallback(new->dialogue.text_field, XtNtabCallback,
		  tab_callback, (XtPointer)new);

    XtSetArg(args[0], XtNbuffer, new->dialogue.message);
    new->dialogue.message_widget =
	XtCreateManagedWidget("message", messageWidgetClass,
			      new->dialogue.layout, args, 1);
    new->dialogue.message = NULL;

    XtSetKeyboardFocus((Widget)new, new->dialogue.text_field);
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    Boolean		redisplay = False;
    DialogueWidget	new = (DialogueWidget)gnew;
    DialogueWidget	current = (DialogueWidget)gcurrent;

    if (new->dialogue.message != current->dialogue.message) {
	Arg	arg;

	XtSetArg(arg, XtNbuffer, new->dialogue.message);
	new->dialogue.message = NULL;
	XtSetValues(new->dialogue.message_widget, &arg, 1);
    }

    if (new->dialogue.buffer != current->dialogue.buffer) {
	Arg	arg;

	XtSetArg(arg, XtNbuffer, new->dialogue.buffer);
	new->dialogue.buffer = NULL;
	XtSetValues(new->dialogue.text_field, &arg, 1);
    }

    return redisplay;
}

/*************************************************************************/

Widget DialogueGetTextField(Widget gw)
{
    DialogueWidget	w = (DialogueWidget)gw;

    return w->dialogue.text_field;
}

