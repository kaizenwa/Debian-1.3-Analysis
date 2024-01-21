/*

Copyright (c) 1989, 1994  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

*/

/*
 * XpmToggle.c - XpmToggle button widget
 * 
 * Adapted by Pierre Ficheux (pierre@rd.lectra.fr) from
 * 	   Chris D. Peterson
 *         MIT X Consortium 
 *         kit@expo.lcs.mit.edu
 *  
 * Date:   January 12, 1989
 *
 */
static char rcsid[] = "$Id: XpmToggle.c,v 1.1 1996/09/14 22:27:47 pierre Exp $";

#include <stdio.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/XawInit.h>
#include <X11/Xmu/Converters.h>
#include <X11/Xmu/Misc.h>

#include "XpmToggleP.h"

/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

/* Private Data */

/* 
 * The order of toggle and notify are important, as the state has
 * to be set when we call the notify proc.
 */

static char defaultTranslations[] =
     "<Btn1Down>,<Btn1Up>:   toggle() notify()";

#define offset(field) XtOffsetOf(XpmToggleRec, field)

static XtResource resources[] = { 
   {XtNstate, XtCState, XtRBoolean, sizeof(Boolean), 
      offset(xpmbutton.set), XtRString, "off"},
   {XtNradioGroup, XtCWidget, XtRWidget, sizeof(Widget), 
      offset(xpmtoggle.widget), XtRWidget, (XtPointer) NULL },
   {XtNradioData, XtCRadioData, XtRPointer, sizeof(XtPointer), 
      offset(xpmtoggle.radio_data), XtRPointer, (XtPointer) NULL },
};

#undef offset


static void XpmToggle(), Initialize(), Notify(), XpmToggleSet();
static void XpmToggleDestroy(), ClassInit();
static Boolean SetValues();

/* Functions for handling the Radio Group. */

static RadioGroup * GetRadioGroup();
static void CreateRadioGroup(), AddToRadioGroup(), TurnOffRadioSiblings();
static void RemoveFromRadioGroup();

static XtActionsRec actionsList[] =
{
  {"toggle",	        XpmToggle},
  {"notify",	        Notify},
  {"set",	        XpmToggleSet},
};

#define SuperClass ((XpmButtonWidgetClass)&xpmButtonClassRec)

XpmToggleClassRec xpmToggleClassRec = {
  {
    (WidgetClass) SuperClass,		/* superclass		  */	
    "XpmToggle",				/* class_name		  */
    sizeof(XpmToggleRec),			/* size			  */
    ClassInit,				/* class_initialize	  */
    NULL,				/* class_part_initialize  */
    FALSE,				/* class_inited		  */
    Initialize,				/* initialize		  */
    NULL,				/* initialize_hook	  */
    XtInheritRealize,			/* realize		  */
    actionsList,			/* actions		  */
    XtNumber(actionsList),		/* num_actions		  */
    resources,				/* resources		  */
    XtNumber(resources),		/* resource_count	  */
    NULLQUARK,				/* xrm_class		  */
    FALSE,				/* compress_motion	  */
    TRUE,				/* compress_exposure	  */
    TRUE,				/* compress_enterleave    */
    FALSE,				/* visible_interest	  */
    NULL,         			/* destroy		  */
    XtInheritResize,			/* resize		  */
    XtInheritExpose,			/* expose		  */
    SetValues,				/* set_values		  */
    NULL,				/* set_values_hook	  */
    XtInheritSetValuesAlmost,		/* set_values_almost	  */
    NULL,				/* get_values_hook	  */
    NULL,				/* accept_focus		  */
    XtVersion,				/* version		  */
    NULL,				/* callback_private	  */
    defaultTranslations,		/* tm_table		  */
    XtInheritQueryGeometry,		/* query_geometry	  */
    XtInheritDisplayAccelerator,	/* display_accelerator	  */
    NULL				/* extension		  */
  },  /* CoreClass fields initialization */
  {
    XtInheritChangeSensitive		/* change_sensitive	  */ 
  },  /* SimpleClass fields initialization */
  {
    0                                     /* field not used    */
  },  /* XpmButtonClass fields initialization */
  {
      NULL,			        /* Set Procedure. */
      NULL,			        /* Unset Procedure. */
      NULL			        /* extension. */
  }  /* XpmToggleClass fields initialization */
};

  /* for public consumption */
WidgetClass xpmToggleWidgetClass = (WidgetClass) &xpmToggleClassRec;

/****************************************************************
 *
 * Private Procedures
 *
 ****************************************************************/

static void
ClassInit()
{
  XtActionList actions;
  Cardinal num_actions;
  Cardinal i;
  XpmToggleWidgetClass class = (XpmToggleWidgetClass) xpmToggleWidgetClass;
  static XtConvertArgRec parentCvtArgs[] = {
      {XtBaseOffset, (XtPointer)XtOffsetOf(WidgetRec, core.parent),
	   sizeof(Widget)}
  };

  XawInitializeWidgetSet();
  XtSetTypeConverter(XtRString, XtRWidget, XmuNewCvtStringToWidget,
		     parentCvtArgs, XtNumber(parentCvtArgs), XtCacheNone,
		     (XtDestructor)NULL);
/* 
 * Find the set and unset actions in the xpmbutton widget's action table. 
 */

  XtGetActionList(xpmButtonWidgetClass, &actions, &num_actions);

  for (i = 0 ; i < num_actions ; i++) {
    if (streq(actions[i].string, "set"))
	class->xpmtoggle_class.Set = actions[i].proc;
    if (streq(actions[i].string, "unset")) 
	class->xpmtoggle_class.Unset = actions[i].proc;

    if ( (class->xpmtoggle_class.Set != NULL) &&
	 (class->xpmtoggle_class.Unset != NULL) ) {
	XtFree((char *) actions);
	return;
    }
  }  

/* We should never get here. */
  XtError("Aborting, due to errors resolving bindings in the XpmToggle widget.");
}

/*ARGSUSED*/
static void Initialize(request, new, args, num_args)
 Widget request, new;
 ArgList args;
 Cardinal *num_args;
{
    XpmToggleWidget tw = (XpmToggleWidget) new;
    XpmToggleWidget tw_req = (XpmToggleWidget) request;

    tw->xpmtoggle.radio_group = NULL;

    if (tw->xpmtoggle.radio_data == NULL) 
      tw->xpmtoggle.radio_data = (XtPointer) new->core.name;

    if (tw->xpmtoggle.widget != NULL) {
      if ( GetRadioGroup(tw->xpmtoggle.widget) == NULL) 
	CreateRadioGroup(new, tw->xpmtoggle.widget);
      else
	AddToRadioGroup( GetRadioGroup(tw->xpmtoggle.widget), new);
    }      
    XtAddCallback(new, XtNdestroyCallback, XpmToggleDestroy, (XtPointer)NULL);

/*
 * Command widget assumes that the widget is unset, so we only 
 * have to handle the case where it needs to be set.
 *
 * If this widget is in a radio group then it may cause another
 * widget to be unset, thus calling the notify proceedure.
 *
 * I want to set the toggle if the user set the state to "On" in 
 * the resource group, reguardless of what my ancestors did.
 */

    if (tw_req->xpmbutton.set)
      XpmToggleSet(new, (XEvent *)NULL, (String *)NULL, (Cardinal *)0);
}

/************************************************************
 *
 *  Action Procedures
 *
 ************************************************************/

/* ARGSUSED */
static void 
XpmToggleSet(w,event,params,num_params)
Widget w;
XEvent *event;
String *params;		/* unused */
Cardinal *num_params;	/* unused */
{
    XpmToggleWidgetClass class = (XpmToggleWidgetClass) w->core.widget_class;

    TurnOffRadioSiblings(w);
    class->xpmtoggle_class.Set(w, event, NULL, 0);
}

/* ARGSUSED */
static void 
XpmToggle(w,event,params,num_params)
Widget w;
XEvent *event;
String *params;		/* unused */
Cardinal *num_params;	/* unused */
{
  XpmToggleWidget tw = (XpmToggleWidget)w;
  XpmToggleWidgetClass class = (XpmToggleWidgetClass) w->core.widget_class;

  if (tw->xpmbutton.set) 
    class->xpmtoggle_class.Unset(w, event, NULL, 0);
  else 
    XpmToggleSet(w, event, params, num_params);
}

/* ARGSUSED */
static void Notify(w,event,params,num_params)
Widget w;
XEvent *event;
String *params;		/* unused */
Cardinal *num_params;	/* unused */
{
  XpmToggleWidget tw = (XpmToggleWidget) w;
  long antilint = tw->xpmbutton.set;

  XtCallCallbacks( w, XtNcallback, (XtPointer) antilint );
}

/************************************************************
 *
 * Set specified arguments into widget
 *
 ***********************************************************/

/* ARGSUSED */
static Boolean 
SetValues (current, request, new, args, num_args)
Widget current, request, new;
ArgList args;
Cardinal *num_args;
{
    XpmToggleWidget oldtw = (XpmToggleWidget) current;
    XpmToggleWidget tw = (XpmToggleWidget) new;
    XpmToggleWidget rtw = (XpmToggleWidget) request;

    if (oldtw->xpmtoggle.widget != tw->xpmtoggle.widget)
      xpmToggleChangeRadioGroup(new, tw->xpmtoggle.widget);

    if (!tw->core.sensitive && oldtw->core.sensitive && rtw->xpmbutton.set)
	tw->xpmbutton.set = True;

    if (oldtw->xpmbutton.set != tw->xpmbutton.set) {
	tw->xpmbutton.set = oldtw->xpmbutton.set;
	XpmToggle(new, (XEvent *)NULL, (String *)NULL, (Cardinal *)0);
    }
    return(FALSE);
}

/*	Function Name: XpmToggleDestroy
 *	Description: Destroy Callback for toggle widget.
 *	Arguments: w - the toggle widget that is being destroyed.
 *                 junk, grabage - not used.
 *	Returns: none.
 */

/* ARGSUSED */
static void
XpmToggleDestroy(w, junk, garbage)
Widget w;
XtPointer junk, garbage;
{
  RemoveFromRadioGroup(w);
}

/************************************************************
 *
 * Below are all the private procedures that handle 
 * radio toggle buttons.
 *
 ************************************************************/

/*	Function Name: GetRadioGroup
 *	Description: Gets the radio group associated with a give toggle
 *                   widget.
 *	Arguments: w - the toggle widget who's radio group we are getting.
 *	Returns: the radio group associated with this toggle group.
 */

static RadioGroup *
GetRadioGroup(w)
Widget w;
{
  XpmToggleWidget tw = (XpmToggleWidget) w;

  if (tw == NULL) return(NULL);
  return( tw->xpmtoggle.radio_group );
}

/*	Function Name: CreateRadioGroup
 *	Description: Creates a radio group. give two widgets.
 *	Arguments: w1, w2 - the toggle widgets to add to the radio group.
 *	Returns: none.
 * 
 *      NOTE:  A pointer to the group is added to each widget's radio_group
 *             field.
 */

static void
CreateRadioGroup(w1, w2)
Widget w1, w2;
{
  char error_buf[BUFSIZ];
  XpmToggleWidget tw1 = (XpmToggleWidget) w1;
  XpmToggleWidget tw2 = (XpmToggleWidget) w2;

  if ( (tw1->xpmtoggle.radio_group != NULL) || (tw2->xpmtoggle.radio_group != NULL) ) {
    (void) sprintf(error_buf, "%s %s", "XpmToggle Widget Error - Attempting",
	    "to create a new toggle group, when one already exists.");
    XtWarning(error_buf);
  }

  AddToRadioGroup( (RadioGroup *)NULL, w1 );
  AddToRadioGroup( GetRadioGroup(w1), w2 );
}

/*	Function Name: AddToRadioGroup
 *	Description: Adds a toggle to the radio group.
 *	Arguments: group - any element of the radio group the we are adding to.
 *                 w - the new toggle widget to add to the group.
 *	Returns: none.
 */

static void
AddToRadioGroup(group, w)
RadioGroup * group;
Widget w;
{
  XpmToggleWidget tw = (XpmToggleWidget) w;
  RadioGroup * local;

  local = (RadioGroup *) XtMalloc( sizeof(RadioGroup) );
  local->widget = w;
  tw->xpmtoggle.radio_group = local;

  if (group == NULL) {		/* Creating new group. */
    group = local;
    group->next = NULL;
    group->prev = NULL;
    return;
  }
  local->prev = group;		/* Adding to previous group. */
  if ((local->next = group->next) != NULL)
      local->next->prev = local;
  group->next = local;
}

/*	Function Name: TurnOffRadioSiblings
 *	Description: Deactivates all radio siblings.
 *	Arguments: widget - a toggle widget.
 *	Returns: none.
 */

static void
TurnOffRadioSiblings(w)
Widget w;
{
  RadioGroup * group;
  XpmToggleWidgetClass class = (XpmToggleWidgetClass) w->core.widget_class;

  if ( (group = GetRadioGroup(w)) == NULL)  /* Punt if there is no group */
    return;

  /* Go to the top of the group. */

  for ( ; group->prev != NULL ; group = group->prev );

  while ( group != NULL ) {
    XpmToggleWidget local_tog = (XpmToggleWidget) group->widget;
    if ( local_tog->xpmbutton.set ) {
      class->xpmtoggle_class.Unset(group->widget, NULL, NULL, 0);
      Notify( group->widget, (XEvent *)NULL, (String *)NULL, (Cardinal *)0);
    }
    group = group->next;
  }
}

/*	Function Name: RemoveFromRadioGroup
 *	Description: Removes a toggle from a RadioGroup.
 *	Arguments: w - the toggle widget to remove.
 *	Returns: none.
 */

static void
RemoveFromRadioGroup(w)
Widget w;
{
  RadioGroup * group = GetRadioGroup(w);
  if (group != NULL) {
    if (group->prev != NULL)
      (group->prev)->next = group->next;
    if (group->next != NULL)
      (group->next)->prev = group->prev;
    XtFree((char *) group);
  }
}

/************************************************************
 *
 * Public Routines
 *
 ************************************************************/
   
/*	Function Name: xpmToggleChangeRadioGroup
 *	Description: Allows a toggle widget to change radio groups.
 *	Arguments: w - The toggle widget to change groups.
 *                 radio_group - any widget in the new group.
 *	Returns: none.
 */

void
#if NeedFunctionPrototypes
xpmToggleChangeRadioGroup(Widget w, Widget radio_group)
#else
xpmToggleChangeRadioGroup(w, radio_group)
Widget w, radio_group;
#endif
{
  XpmToggleWidget tw = (XpmToggleWidget) w;
  RadioGroup * group;

  RemoveFromRadioGroup(w);

/*
 * If the toggle that we are about to add is set then we will 
 * unset all toggles in the new radio group.
 */

  if ( tw->xpmbutton.set && radio_group != NULL )
    xpmToggleUnsetCurrent(radio_group);

  if (radio_group != NULL)
      if ((group = GetRadioGroup(radio_group)) == NULL)
	  CreateRadioGroup(w, radio_group);
      else AddToRadioGroup(group, w);
}

/*	Function Name: xpmToggleGetCurrent
 *	Description: Returns the RadioData associated with the toggle
 *                   widget that is currently active in a toggle group.
 *	Arguments: w - any toggle widget in the toggle group.
 *	Returns: The XtNradioData associated with the toggle widget.
 */

XtPointer
#if NeedFunctionPrototypes
xpmToggleGetCurrent(Widget w)
#else
xpmToggleGetCurrent(w)
Widget w;
#endif
{
  RadioGroup * group;

  if ( (group = GetRadioGroup(w)) == NULL) return(NULL);
  for ( ; group->prev != NULL ; group = group->prev);

  while ( group != NULL ) {
    XpmToggleWidget local_tog = (XpmToggleWidget) group->widget;
    if ( local_tog->xpmbutton.set )
      return( local_tog->xpmtoggle.radio_data );
    group = group->next;
  }
  return(NULL);
}

/*	Function Name: xpmToggleSetCurrent
 *	Description: Sets the XpmToggle widget associated with the
 *                   radio_data specified.
 *	Arguments: radio_group - any toggle widget in the toggle group.
 *                 radio_data - radio data of the toggle widget to set.
 *	Returns: none.
 */

void
#if NeedFunctionPrototypes
xpmToggleSetCurrent(Widget radio_group, XtPointer radio_data)
#else
xpmToggleSetCurrent(radio_group, radio_data)
Widget radio_group;
XtPointer radio_data;
#endif
{
  RadioGroup * group;
  XpmToggleWidget local_tog; 

/* Special case of no radio group. */

  if ( (group = GetRadioGroup(radio_group)) == NULL) {
    local_tog = (XpmToggleWidget) radio_group;
    if ( (local_tog->xpmtoggle.radio_data == radio_data) )     
      if (!local_tog->xpmbutton.set) {
	XpmToggleSet((Widget) local_tog, (XEvent *)NULL, (String *)NULL, (Cardinal *)0);
	Notify((Widget) local_tog, (XEvent *)NULL, (String *)NULL, (Cardinal *)0);
      }
    return;
  }

/*
 * find top of radio_roup 
 */

  for ( ; group->prev != NULL ; group = group->prev);

/*
 * search for matching radio data.
 */

  while ( group != NULL ) {
    local_tog = (XpmToggleWidget) group->widget;
    if ( (local_tog->xpmtoggle.radio_data == radio_data) ) {
      if (!local_tog->xpmbutton.set) { /* if not already set. */
	XpmToggleSet((Widget) local_tog, (XEvent *)NULL, (String *)NULL, (Cardinal *)0);
	Notify((Widget) local_tog, (XEvent *)NULL, (String *)NULL, (Cardinal *)0);
      }
      return;			/* found it, done */
    }
    group = group->next;
  }
}
 
/*	Function Name: xpmToggleUnsetCurrent
 *	Description: Unsets all XpmToggles in the radio_group specified.
 *	Arguments: radio_group - any toggle widget in the toggle group.
 *	Returns: none.
 */

void
#if NeedFunctionPrototypes
xpmToggleUnsetCurrent(Widget radio_group)
#else
xpmToggleUnsetCurrent(radio_group)
Widget radio_group;
#endif
{
  XpmToggleWidgetClass class;
  XpmToggleWidget local_tog = (XpmToggleWidget) radio_group;

  /* Special Case no radio group. */

  if (local_tog->xpmbutton.set) {
    class = (XpmToggleWidgetClass) local_tog->core.widget_class;
    class->xpmtoggle_class.Unset(radio_group, NULL, NULL, 0);
    Notify(radio_group, (XEvent *)NULL, (String *)NULL, (Cardinal *)0);
  }
  if ( GetRadioGroup(radio_group) == NULL) return;
  TurnOffRadioSiblings(radio_group);
}

