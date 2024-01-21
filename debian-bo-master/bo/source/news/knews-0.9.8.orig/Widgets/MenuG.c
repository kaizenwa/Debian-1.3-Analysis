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
#include "MenuGP.h"

static XtResource resources[] = {
#define offset(field) XtOffsetOf(MenuGadgetRec, menu_g.field)
    {XtNcallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(callback), XtRImmediate, (XtPointer)NULL},
    {XtNpostPopdownCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(post_popdown_callback), XtRImmediate, (XtPointer)NULL},
    {XtNlabel, XtCLabel, XtRString, sizeof(String),
     offset(label), XtRImmediate, (XtPointer)NULL},
#undef offset
};

static void	ClassPartInitialize(WidgetClass);
static void	Initialize(Widget, Widget, ArgList, Cardinal*);
static void	Destroy(Widget);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);
static void	GenericProc(MenuGadget);
static int	Notify(MenuGadget);
static int	PostNotify(MenuGadget);
static void	SetActive(MenuGadget, int);

MenuGadgetClassRec menuGadgetClassRec = {
    { /* rectObj fields */
        (WidgetClass) &rectObjClassRec, /* superclass                   */
        "MenuGadget",                   /* class_name                   */
        sizeof(MenuGadgetRec),	        /* widget_size                  */
        NULL,                           /* class_initialize             */
        ClassPartInitialize,            /* class_part_initialize        */
        FALSE,                          /* class_inited                 */
        Initialize,                     /* initialize                   */
        NULL,                           /* initialize_hook              */
        NULL,                           /* rect1                        */
        NULL,                           /* rect2                        */
        0,                              /* rect3                        */
        resources,                      /* resources                    */
        XtNumber(resources),            /* num_resources                */
        NULLQUARK,                      /* xrm_class                    */
        FALSE,                          /* rect4                        */
        FALSE,                          /* rect5                        */
        FALSE,                          /* rect6                        */
        FALSE,                          /* rect7                        */
        Destroy,    			/* destroy                      */
        NULL,                           /* resize                       */
        NULL,                           /* expose                       */
        SetValues,			/* set_values                   */
        NULL,                           /* set_values_hook              */
        XtInheritSetValuesAlmost,       /* set_values_almost            */
        NULL,                           /* get_values_hook              */
        NULL,                           /* rect9                        */
        XtVersion,                      /* version                      */
        NULL,                           /* callback_private             */
        NULL,                           /* rect10                       */
        NULL,                           /* query_geometry               */
        NULL,                           /* rect11                       */
        NULL,                           /* extension                    */
    },
    { /* menu_g fields */
        GenericProc,			/* change_hl			*/
	GenericProc,			/* popdown			*/
	Notify,				/* notify			*/
	PostNotify,			/* post_notify			*/
	SetActive,			/* set_active			*/
	False,				/* ignore_leave			*/
	NULL,				/* extension			*/
    }
};

WidgetClass menuGadgetClass = (WidgetClass)&menuGadgetClassRec;

/*************************************************************************/

static void ClassPartInitialize(WidgetClass gclass)
{
    MenuGadgetClass	class, super;

    class = (MenuGadgetClass)gclass;
    super = (MenuGadgetClass)class->rect_class.superclass;

    if (class->menu_g_class.change_hl    == XtInheritChangeHl)
	class->menu_g_class.change_hl    = super->menu_g_class.change_hl;
    if (class->menu_g_class.popdown      == XtInheritPopdown)
	class->menu_g_class.popdown      = super->menu_g_class.popdown;
    if (class->menu_g_class.notify       == XtInheritNotify)
	class->menu_g_class.notify       = super->menu_g_class.notify;
    if (class->menu_g_class.post_notify  == XtInheritPostNotify)
	class->menu_g_class.post_notify  = super->menu_g_class.post_notify;
    if (class->menu_g_class.set_active   == XtInheritSetActive)
	class->menu_g_class.set_active   = super->menu_g_class.set_active;
}

static void Initialize(Widget    grequest,
		       Widget    gnew,
		       ArgList   args,
		       Cardinal *no_args)
{
    MenuGadget	new = (MenuGadget)gnew;

    new->menu_g.hl     = False;
    new->menu_g.inside = False;
    new->menu_g.active = False;
    new->menu_g.label  = new->menu_g.label ?
	XtNewString(new->menu_g.label) : XtNewString(XtName((Widget)new));
}

static void Destroy(Widget gw)
{
    MenuGadget	g = (MenuGadget)gw;

    XtFree(g->menu_g.label);
}

static Boolean SetValues(Widget	   gcurrent,
			 Widget	   grequest,
			 Widget	   gnew,
			 ArgList   args,
			 Cardinal *num_args)
{
    int		redisplay = False;
    MenuGadget	new       = (MenuGadget)gnew;
    MenuGadget	current   = (MenuGadget)gcurrent;

    if (new->menu_g.label != current->menu_g.label) {
	XtFree(current->menu_g.label);
	new->menu_g.label = new->menu_g.label ?
	    XtNewString(new->menu_g.label) : XtNewString(XtName((Widget)new));
	redisplay = True;
    }

    return redisplay;
}

static void GenericProc(MenuGadget g)
{
}

static int Notify(MenuGadget g)
{
    XtCallbackList	c_list = g->menu_g.callback;

    if (!g->menu_g.inside)
	return False;

    if (c_list)
	XtCallCallbackList((Widget)g, c_list, (XtPointer)g->menu_g.label);

    return True;
}

static int PostNotify(MenuGadget g)
{
    XtCallbackList	c_list = g->menu_g.post_popdown_callback;

    if (!g->menu_g.inside)
	return False;

    if (c_list)
	XtCallCallbackList((Widget)g, c_list, (XtPointer)g->menu_g.label);

    return True;
}

void SetActive(MenuGadget g, int active)
{
    g->menu_g.active = active;
}
