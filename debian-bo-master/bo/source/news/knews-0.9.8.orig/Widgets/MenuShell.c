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

#include <stdio.h>

#include "Compat.h"
#include "Util.h"

#include "MenuI.h"
#include "MenuShellP.h"

static XtResource resources[]=
{
    {XtNborderWidth, XtCBorderWidth, XtRDimension, sizeof(Dimension),
     XtOffsetOf(MenuShellRec, core.border_width),
     XtRImmediate, (XtPointer)0},
    {XtNallowShellResize, XtCAllowShellResize, XtRBoolean, sizeof(Boolean),
     XtOffsetOf(MenuShellRec, shell.allow_shell_resize),
     XtRImmediate, (XtPointer)True},
#define offset(field) XtOffsetOf(MenuShellRec, popdown.field)
#undef offset
};

MenuShellClassRec menuShellClassRec = {
    { /* core fields		*/
	(WidgetClass) &overrideShellClassRec,	/* superclass		*/
	"MenuShell",				/* class_name		*/
	sizeof(MenuShellRec),			/* widget_size		*/
	NULL,					/* class_initialize	*/
	NULL,					/* class_part_initialize*/
	FALSE,					/* class_inited		*/
	NULL,					/* initialize		*/
	NULL,					/* initialize_hook	*/
	XtInheritRealize,			/* realize		*/
	NULL,					/* actions		*/
	0,					/* num_actions		*/
	resources,				/* resources		*/
	XtNumber(resources),			/* num_resources	*/
	NULLQUARK,				/* xrm_class		*/
	FALSE,					/* compress_motion	*/
	TRUE,					/* compress_exposure	*/
	FALSE,					/* compress_enterleave	*/
	FALSE,					/* visible_interest	*/
	NULL,					/* destroy		*/
	XtInheritResize,			/* resize		*/
	NULL,					/* expose		*/
	NULL,					/* set_values		*/
	NULL,					/* set_values_hook	*/
	XtInheritSetValuesAlmost,		/* set_values_almost	*/
	NULL,					/* get_values_hook	*/
	NULL,					/* accept_focus		*/
	XtVersion,				/* version		*/
	NULL,					/* callback_private	*/
	NULL,					/* tm_table		*/
	XtInheritQueryGeometry,			/* query_geometry	*/
	XtInheritDisplayAccelerator,		/* display_accelerator	*/
	NULL,					/* extension		*/
    },
    { /* composite fields	*/
	XtInheritGeometryManager,		/* geometry_manager	*/
	XtInheritChangeManaged,			/* change_managed	*/
	XtInheritInsertChild,			/* insert_child		*/
	XtInheritDeleteChild,			/* delete_child		*/
	NULL,					/* extension		*/
    },
    { /* shell fields		*/
	NULL,					/* extension		*/
    },
    { /* override_shell fields	*/
	NULL,					/* extension		*/
    },
    { /* menu_shell fields	*/
	NULL,					/* extension		*/
    }
};

WidgetClass menuShellWidgetClass = (WidgetClass)&menuShellClassRec;

/******************************************************************/

static Widget get_child(MenuShellWidget w)
{
    int	i;

    for (i = 0 ; i < w->composite.num_children ; i++)
	if (XtIsManaged(w->composite.children[i]))
	    return w->composite.children[i];

    return NULL;
}

int NotifyMenuShell(Widget gw)
{
    MenuShellWidget	w = (MenuShellWidget)gw;
    Widget		child = get_child(w);

    return !child || NotifyMenu(child);
}

int PostNotifyMenuShell(Widget gw)
{
    MenuShellWidget	w = (MenuShellWidget)gw;
    Widget		child = get_child(w);

    return !child || PostNotifyMenu(child);
}

void PopdownMenuShell(Widget gw)
{
    MenuShellWidget	w = (MenuShellWidget)gw;
    Widget		child = get_child(w);

    if (child)
	PopdownMenu(child);
    if (w->shell.popped_up)
	XtPopdown((Widget)w);
}

void SetActiveMenuShell(Widget gw, int active)
{
    MenuShellWidget	w = (MenuShellWidget)gw;
    Widget		child = get_child(w);

    if (child)
	SetActiveMenu(child, active);
}
