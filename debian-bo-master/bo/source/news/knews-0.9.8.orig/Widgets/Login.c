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

#include "LoginP.h"

static XtResource resources[] = {
#define offset(field) XtOffsetOf(LoginRec, login.field)
    {XtNcallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(callback), XtRCallback, (XtPointer)NULL},
    {XtNmessage, XtCMessage, XtRString, sizeof(String),
     offset(message), XtRImmediate, (XtPointer)""},
    {XtNuserNameBuffer, XtCBuffer, XtRString, sizeof(String),
     offset(username_buffer), XtRImmediate, (XtPointer)""},
    {XtNpassWordBuffer, XtCBuffer, XtRString, sizeof(String),
     offset(password_buffer), XtRImmediate, (XtPointer)""},
    {XtNuserNameLabel, XtCLabel, XtRString, sizeof(String),
     offset(username_label), XtRImmediate, (XtPointer)"Username:"},
    {XtNpassWordLabel, XtCLabel, XtRString, sizeof(String),
     offset(password_label), XtRImmediate, (XtPointer)"Password:"},
    {XtNleftLabel, XtCLabel, XtRBoolean, sizeof(String),
     offset(left_label), XtRImmediate, (XtPointer)"left"},
    {XtNmiddleLabel, XtCLabel, XtRBoolean, sizeof(String),
     offset(middle_label), XtRImmediate, (XtPointer)"middle"},
    {XtNrightLabel, XtCLabel, XtRBoolean, sizeof(String),
     offset(right_label), XtRImmediate, (XtPointer)"right"},
    {XtNfieldWidth, XtCWidth, XtRCardinal, sizeof(Cardinal),
     offset(field_width), XtRImmediate, (XtPointer)16},
#undef offset
};

static void	Initialize(Widget, Widget, ArgList, Cardinal*);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);

static void	close_login(Widget);

LoginClassRec loginClassRec = {
    {                                   /* core fields                  */
        (WidgetClass) &closeShellClassRec, /* superclass                */
        "Login",			/* class_name                   */
        sizeof(LoginRec),	        /* widget_size                  */
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
	close_login,			/* close_window			*/
	NULL,				/* extension			*/
    },
    {					/* dialogue fields		*/
	NULL,				/* extension			*/
    }
};

WidgetClass loginWidgetClass = (WidgetClass)&loginClassRec;

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
"		vertical { "
"			0 <+inf> "
"			usernamemessage "
"			0 <+inf> "
"		} "
"		height left "
"		(width passwordmessage - width usernamemessage) "
"		usernamefield <+inf-inf*> "
"		height left "
"	} "
"	(height left / 2) <+inf-inf> "
"	horizontal { "
"		height left "
"		vertical { "
"			0 <+inf> "
"			passwordmessage "
"			0 <+inf> "
"		} "
"		height left "
"		passwordfield <+inf-inf*> "
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

static void call_callbacks(LoginWidget w, LoginReply reply)
{
    XtCallbackList	c_list = w->login.callback;
    LoginReport		report = {0, };

    if (!c_list)
	return;

    report.reply = reply;
    report.username = TextFieldGetBuffer(w->login.username_field);
    report.password = TextFieldGetBuffer(w->login.password_field);
    XtCallCallbackList((Widget)w, c_list, (XtPointer)&report);
    XtFree(report.username);
    XtFree(report.password);
}

static void close_login(Widget gw)
{
    LoginWidget	w = (LoginWidget)gw;

    call_callbacks(w, LoginReplyClose);
}

static void knapp_callback(Widget knapp,
			   XtPointer client_data,
			   XtPointer call_data)
{
    LoginWidget	w = (LoginWidget)client_data;

    if (knapp == w->login.left_knapp)
	call_callbacks(w, LoginReplyLeft);
    else if (knapp == w->login.middle_knapp)
	call_callbacks(w, LoginReplyMiddle);
    else if (knapp == w->login.right_knapp)
	call_callbacks(w, LoginReplyRight);
}

static void username_callback(Widget gw,
			      XtPointer client_data,
			      XtPointer call_data)
{
    LoginWidget		w = (LoginWidget)client_data;

    XtSetKeyboardFocus((Widget)w, w->login.password_field);
}

static void password_callback(Widget gw,
			      XtPointer client_data,
			      XtPointer call_data)
{
    LoginWidget		w = (LoginWidget)client_data;

    call_callbacks(w, LoginReplyEnter);
}

static void tab_callback(Widget gw,
			 XtPointer client_data,
			 XtPointer call_data)
{
    LoginWidget		w = (LoginWidget)client_data;
    Widget		focus = NULL;

    if (gw == w->login.password_field)
	focus = w->login.username_field;
    else if (gw == w->login.username_field)
	focus = w->login.password_field;

    if (focus)
	XtSetKeyboardFocus((Widget)w, focus);
}

/*************************************************************************/

static void Initialize(Widget grequest, Widget gnew,
		       ArgList gargs, Cardinal *no_args)
{
    LoginWidget	new = (LoginWidget)gnew;
    Arg		args[6];

    new->login.layout =
	XtVaCreateManagedWidget("layout", layoutWidgetClass,
				(Widget)new,
				XtVaTypedArg, XtNlayout, XtRString,
				layout_string, (int)sizeof(String),
				(void *)0);

    XtSetArg(args[0], XtNlabel, new->login.left_label);
    new->login.left_knapp =
	XtCreateManagedWidget("left", knappWidgetClass,
			      new->login.layout, args, 1);
    XtAddCallback(new->login.left_knapp, XtNcallback,
		  knapp_callback, (XtPointer)new);

    XtSetArg(args[0], XtNlabel, new->login.middle_label);
    new->login.middle_knapp =
	XtCreateManagedWidget("middle", knappWidgetClass,
			      new->login.layout, args, 1);
    XtAddCallback(new->login.middle_knapp, XtNcallback,
		  knapp_callback, (XtPointer)new);

    XtSetArg(args[0], XtNlabel, new->login.right_label);
    new->login.right_knapp =
	XtCreateManagedWidget("right", knappWidgetClass,
			      new->login.layout, args, 1);
    XtAddCallback(new->login.right_knapp, XtNcallback,
		  knapp_callback, (XtPointer)new);

    XtSetArg(args[0], XtNfocusRoot, new);
    XtSetArg(args[1], XtNbuffer, new->login.username_buffer);
    XtSetArg(args[2], XtNpreferredChars, new->login.field_width);
    XtSetArg(args[3], XtNsingleLine, True);
    new->login.username_field =
	XtCreateManagedWidget("usernamefield", textFieldWidgetClass,
			      new->login.layout, args, 4);
    XtAddCallback(new->login.username_field, XtNcallback,
		  username_callback, (XtPointer)new);
    XtAddCallback(new->login.username_field, XtNtabCallback,
		  tab_callback, (XtPointer)new);

    XtSetArg(args[0], XtNfocusRoot, new);
    XtSetArg(args[1], XtNbuffer, new->login.password_buffer);
    XtSetArg(args[2], XtNpreferredChars, new->login.field_width);
    XtSetArg(args[3], XtNsingleLine, True);
    XtSetArg(args[4], XtNechoOff, True);
    new->login.password_field =
	XtCreateManagedWidget("passwordfield", textFieldWidgetClass,
			      new->login.layout, args, 5);
    XtAddCallback(new->login.password_field, XtNcallback,
		  password_callback, (XtPointer)new);
    XtAddCallback(new->login.password_field, XtNtabCallback,
		  tab_callback, (XtPointer)new);

    XtSetArg(args[0], XtNborderWidth, 0);
    XtSetArg(args[1], XtNbuffer, new->login.message);
    new->login.message_widget =
	XtCreateManagedWidget("message", messageWidgetClass,
			      new->login.layout, args, 2);
    new->login.message = NULL;

    XtSetArg(args[0], XtNborderWidth, 0);
    XtSetArg(args[1], XtNbuffer, new->login.username_label);
    XtCreateManagedWidget("usernamemessage", messageWidgetClass,
			  new->login.layout, args, 2);
    new->login.username_label = NULL;

    XtSetArg(args[0], XtNborderWidth, 0);
    XtSetArg(args[1], XtNbuffer, new->login.password_label);
    XtCreateManagedWidget("passwordmessage", messageWidgetClass,
			  new->login.layout, args, 2);
    new->login.password_label = NULL;

    XtSetKeyboardFocus((Widget)new, new->login.username_field);
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    Boolean	redisplay = False;
    LoginWidget	new = (LoginWidget)gnew;
    LoginWidget	current = (LoginWidget)gcurrent;

    if (new->login.message != current->login.message) {
	Arg	arg;

	XtSetArg(arg, XtNbuffer, new->login.message);
	new->login.message = NULL;
	XtSetValues(new->login.message_widget, &arg, 1);
    }

    if (new->login.username_buffer != current->login.username_buffer) {
	Arg	arg;

	XtSetArg(arg, XtNbuffer, new->login.username_buffer);
	new->login.username_buffer = NULL;
	XtSetValues(new->login.username_field, &arg, 1);
    }

    if (new->login.password_buffer != current->login.password_buffer) {
	Arg	arg;

	XtSetArg(arg, XtNbuffer, new->login.password_buffer);
	new->login.password_buffer = NULL;
	XtSetValues(new->login.password_field, &arg, 1);
    }

    return redisplay;
}
