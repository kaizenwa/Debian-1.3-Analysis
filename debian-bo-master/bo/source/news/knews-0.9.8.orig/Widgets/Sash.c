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
#include "SashP.h"

static XtResource resources[] = {
    {XtNshadowWidth, XtCShadowWidth, XtRDimension, sizeof(Dimension),
     XtOffsetOf(SashRec, shadow.shadow_width), XtRImmediate, (XtPointer)1},
#define offset(field) XtOffsetOf(SashRec, sash.field)
    {XtNcallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(callback), XtRImmediate, (XtPointer)NULL},
    {XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
     offset(cursor), XtRString, (XtPointer)"hand2"},
    {XtNpreferredWidth, XtCWidth, XtRDimension, sizeof(Dimension),
     offset(pref_width), XtRImmediate, (XtPointer)3},
    {XtNpreferredHeight, XtCHeight, XtRDimension, sizeof(Dimension),
     offset(pref_height), XtRImmediate, (XtPointer)3},
#undef offset
};

static void Initialize(Widget, Widget, ArgList, Cardinal*);
static void Realize(Widget, XtValueMask*, XSetWindowAttributes*);
static void Redisplay(Widget, XEvent*, Region);
static Boolean SetValues(Widget, Widget, Widget, ArgList, Cardinal*);
static XtGeometryResult QueryGeometry(Widget, XtWidgetGeometry*,
				      XtWidgetGeometry*);

static void sash_action(Widget, XEvent*, String*, Cardinal*);

static XtActionsRec actions[] = {
    {"sash-action",	sash_action},
};

static char translations[] =
"<Btn1Down>:	sash-action() \n"
"<Btn1Motion>:	sash-action() \n";

SashClassRec sashClassRec = {
    { /* core fields */
        (WidgetClass) &shadowClassRec,  /* superclass                   */
        "Sash",                         /* class_name                   */
        sizeof(SashRec),	        /* widget_size                  */
        NULL,                           /* class_initialize             */
        NULL,                           /* class_part_initialize        */
        FALSE,                          /* class_inited                 */
        Initialize,                     /* initialize                   */
        NULL,                           /* initialize_hook              */
        Realize,                        /* realize                      */
        actions,                        /* actions                      */
        XtNumber(actions),              /* num_actions                  */
        resources,                      /* resources                    */
        XtNumber(resources),            /* num_resources                */
        NULLQUARK,                      /* xrm_class                    */
        TRUE,                           /* compress_motion              */
#if (XtSpecificationRelease < 4)
        TRUE,                           /* compress_exposure            */
#elif (XtSpecificationRelease < 6)
	XtExposeCompressMaximal,	/* compress_exposure		*/
#else
	XtExposeCompressMaximal | XtExposeNoRegion, /* compress_exposure*/
#endif
        TRUE,                           /* compress_enterleave          */
        FALSE,                          /* visible_interest             */
        NULL,    			/* destroy                      */
        NULL,                           /* resize                       */
        Redisplay,                      /* expose                       */
        SetValues,                      /* set_values                   */
        NULL,                           /* set_values_hook              */
        XtInheritSetValuesAlmost,       /* set_values_almost            */
        NULL,                           /* get_values_hook              */
        NULL,                           /* accept_focus                 */
        XtVersion,                      /* version                      */
        NULL,                           /* callback_private             */
        translations,			/* tm_table                     */
        QueryGeometry,                  /* query_geometry               */
        XtInheritDisplayAccelerator,    /* display_accelerator          */
        NULL                            /* extension                    */
    },
    { /* shadow fields */
	XtInheritPixelOffset,		/* pixel offset			*/
	True,				/* use_arm_for_background	*/
	XtInheritAllocShadowColors,	/* alloc_shadow_colors		*/
	XtInheritAllocShadowPixmaps,	/* alloc_shadow_pixmaps		*/
	XtInheritAllocArmColor,		/* alloc_arm_color		*/
	XtInheritAllocArmPixmap,	/* alloc_arm_pixmap		*/
	XtInheritAllocGCs,		/* alloc_gcs			*/
	NULL,				/* extension			*/
    },
    { /* sash fields */
        0                               /* empty                        */
    }
};

WidgetClass sashWidgetClass = (WidgetClass)&sashClassRec;

/*************************************************************************/

static void sash_action(Widget gw, XEvent *event,
			String *params, Cardinal *no_params)
{
    SashWidget		w = (SashWidget)gw;
    XtCallbackList	c_list = w->sash.callback;

    if (c_list) {
	SashCallDataRec	call_data;

	call_data.event     = event;
	call_data.params    = params;
	call_data.no_params = no_params;
	XtCallCallbackList((Widget)w, c_list, (XtPointer)&call_data);
    }
}

/*************************************************************************/

static void Initialize(Widget request, Widget new,
		       ArgList args, Cardinal *no_args)
{
    if (new->core.height == 0)
	new->core.height = 1;
    if (new->core.width  == 0)
	new->core.width  = 1;
}

static void Realize(Widget gw, XtValueMask *mask,
		    XSetWindowAttributes *attributes)
{
    SashWidget	w = (SashWidget)gw;

    if (w->sash.cursor != None) {
	attributes->cursor = w->sash.cursor;
	*mask |= CWCursor;
    }

    shadowWidgetClass->core_class.realize((Widget)w, mask, attributes);
}

static void Redisplay(Widget gw, XEvent *event, Region region)
{
    SashWidget		w = (SashWidget)gw;

    if (!XtIsRealized((Widget)w))
	return;

    ShadowDrawShadows((ShadowWidget)w, 0, 0,
		      w->core.width, w->core.height, True);
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    SashWidget	current = (SashWidget)gcurrent;
    SashWidget	new = (SashWidget)gnew;

    if (new->sash.cursor != current->sash.cursor &&
	XtIsRealized((Widget)new))
	XDefineCursor(XtDisplay(new), XtWindow(new), new->sash.cursor);

    return False;
}

static XtGeometryResult QueryGeometry(Widget gw,
				      XtWidgetGeometry *intended,
				      XtWidgetGeometry *preferred)
{
    SashWidget	w = (SashWidget)gw;
    Dimension	intended_width, intended_height;

    preferred->request_mode = 0;

    if (w->sash.pref_width > 0) {
	preferred->width = w->sash.pref_width;
	preferred->request_mode |= CWWidth;
    } else {
	preferred->width = w->core.width;
    }

    if (w->sash.pref_height > 0) {
	preferred->height = w->sash.pref_height;
	preferred->request_mode |= CWHeight;
    } else {
	preferred->height = w->core.height;
    }

    if (intended->request_mode & CWWidth)
	intended_width = intended->width;
    else
	intended_width = w->core.width;
    if (intended->request_mode & CWHeight)
	intended_height = intended->height;
    else
	intended_height = w->core.height;

    if (preferred->width == w->core.width &&
        preferred->height == w->core.height)
	return XtGeometryNo;
    else if (preferred->width == intended_width &&
             preferred->height == intended_height)
	return XtGeometryYes;
    else
	return XtGeometryAlmost;
}
