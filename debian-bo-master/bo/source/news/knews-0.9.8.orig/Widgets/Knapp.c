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
#include "KnappP.h"

static XtResource resources[] = {
    {XtNborderWidth, XtCBorderWidth, XtRDimension, sizeof(Dimension),
     XtOffsetOf(KnappRec, core.border_width), XtRImmediate, (XtPointer)1},
#define offset(field) XtOffsetOf(KnappRec, knapp.field)
    {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(foreground_pixel), XtRString, XtDefaultForeground},
    {XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
     offset(font), XtRString, XtDefaultFont},
    {XtNlabel, XtCLabel, XtRString, sizeof(String),
     offset(label), XtRString, (XtPointer)NULL},
    {XtNleftMargin, XtCLeftMargin, XtRDimension, sizeof(Dimension),
     offset(left_margin), XtRImmediate, (XtPointer)8},
    {XtNrightMargin, XtCRightMargin, XtRDimension, sizeof(Dimension),
     offset(right_margin), XtRImmediate, (XtPointer)8},
    {XtNinternalHeight, XtCInternalHeight, XtRDimension, sizeof(Dimension),
     offset(internal_height), XtRImmediate, (XtPointer)2},
    {XtNcallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(callback), XtRCallback, (XtPointer)NULL},
    {XtNresizable, XtCResizable, XtRBoolean, sizeof(Boolean),
     offset(resizable), XtRImmediate, (XtPointer)True},
    {XtNjustify, XtCJustify, XtRJustify, sizeof(JustifyType),
     offset(justify), XtRImmediate, (XtPointer)JustifyTypeLeft},
#undef offset
};

static void ClassInitialize(void);
static void Initialize(Widget, Widget, ArgList, Cardinal*);
static void Destroy(Widget);
static void Resize(Widget);
static void Redisplay(Widget, XEvent*, Region);
static Boolean SetValues(Widget, Widget, Widget, ArgList, Cardinal*);
static XtGeometryResult QueryGeometry(Widget,
				      XtWidgetGeometry*,
				      XtWidgetGeometry*);

static void set(Widget, XEvent*, String*, Cardinal*);
static void reset(Widget, XEvent*, String*, Cardinal*);
static void abort_action(Widget, XEvent*, String*, Cardinal*);
static void notify(Widget, XEvent*, String*, Cardinal*);

static XtActionsRec actions[] = {
    {"set",	set},
    {"reset",	reset},
    {"abort",	abort_action},
    {"notify",	notify},
};

static char translations[] =
"<BtnDown>:	set() \n"
"<BtnUp>:	notify() reset() \n"
"<LeaveWindow>:	abort() \n";

KnappClassRec knappClassRec = {
    {                                   /* core fields                  */
        (WidgetClass) &shadowClassRec,  /* superclass                   */
        "Knapp",                        /* class_name                   */
        sizeof(KnappRec),	        /* widget_size                  */
        ClassInitialize,                /* class_initialize             */
        NULL,                           /* class_part_initialize        */
        FALSE,                          /* class_inited                 */
        Initialize,                     /* initialize                   */
        NULL,                           /* initialize_hook              */
        XtInheritRealize,               /* realize                      */
        actions,                        /* actions                      */
        XtNumber(actions),              /* num_actions                  */
        resources,                      /* resources                    */
        XtNumber(resources),            /* num_resources                */
        NULLQUARK,                      /* xrm_class                    */
        TRUE,                           /* compress_motion              */
#if (XtSpecificationRelease < 4)
	True,				/* compress_exposure		*/
#elif (XtSpecificationRelease < 6)
	XtExposeCompressMaximal,	/* compress_exposure		*/
#else
        XtExposeCompressMaximal | XtExposeNoRegion, /* compress_exposure*/
#endif
        TRUE,                           /* compress_enterleave          */
        FALSE,                          /* visible_interest             */
        Destroy,                        /* destroy                      */
        Resize,                         /* resize                       */
        Redisplay,                      /* expose                       */
        SetValues,                      /* set_values                   */
        NULL,                           /* set_values_hook              */
        XtInheritSetValuesAlmost,       /* set_values_almost            */
        NULL,                           /* get_values_hook              */
        NULL,                           /* accept_focus                 */
        XtVersion,                      /* version                      */
        NULL,                           /* callback_private             */
        translations,                   /* tm_table                     */
        QueryGeometry,                  /* query_geometry               */
        XtInheritDisplayAccelerator,    /* display_accelerator          */
        NULL                            /* extension                    */
    },
    {					/* shadow fields		*/
	XtInheritPixelOffset,		/* pixel_offset			*/
	False,				/* use_arm_for_background	*/
	XtInheritAllocShadowColors,	/* alloc_shadow_colors		*/
	XtInheritAllocShadowPixmaps,	/* alloc_shadow_pixmaps		*/
	XtInheritAllocArmColor,		/* alloc_arm_color		*/
	XtInheritAllocArmPixmap,	/* alloc_arm_pixmap		*/
	XtInheritAllocGCs,		/* alloc_gcs			*/
	NULL,				/* extension			*/
    },
    {                                   /* knapp fields                 */
        0                               /* extension                    */
    }
};

WidgetClass knappWidgetClass = (WidgetClass)&knappClassRec;

/*************************************************************************/

static void set(Widget gw, XEvent *event,
		String *params, Cardinal *no_params)
{
    KnappWidget	w = (KnappWidget)gw;

    if (w->knapp.active) {
	w->knapp.set = True;
	XClearWindow(XtDisplay(w), XtWindow(w));
	w->core.widget_class->core_class.expose((Widget)w, NULL, 0);
    }
}

static void abort_action(Widget gw, XEvent *event,
			 String *params, Cardinal *no_params)
{
    KnappWidget	w = (KnappWidget)gw;

    if (!w->knapp.calling_callbacks && w->knapp.set) {
	w->knapp.set = False;
	XClearWindow(XtDisplay(w), XtWindow(w));
	w->core.widget_class->core_class.expose((Widget)w, NULL, 0);
    }
}

static void reset(Widget gw, XEvent *event,
		  String *params, Cardinal *no_params)
{
    KnappWidget	w = (KnappWidget)gw;

    if (!w->knapp.calling_callbacks && w->knapp.set) {
	w->knapp.set = False;
	XClearWindow(XtDisplay(w), XtWindow(w));
	w->core.widget_class->core_class.expose((Widget)w, NULL, 0);
    }
}

static void notify(Widget gw, XEvent *event,
		   String *params, Cardinal *no_params)
{
    KnappWidget		w = (KnappWidget)gw;
    XtCallbackList	c_list = w->knapp.callback;

    if (!w->knapp.set || !w->knapp.active || !c_list)
	return;

    w->knapp.calling_callbacks = True;
    XtCallCallbackList((Widget)w, c_list, NULL);
    w->knapp.calling_callbacks = False;
}

/*************************************************************************/

static void init_gcs(KnappWidget w)
{
    XGCValues	values;

    values.foreground = w->knapp.foreground_pixel;
    values.font = w->knapp.font->fid;
    w->knapp.default_gc = XtGetGC((Widget)w, GCForeground | GCFont, &values);
    values.stipple = w->knapp.stipple;
    values.fill_style = FillStippled;
    w->knapp.gray_gc = XtGetGC((Widget)w,
			       GCForeground | GCFont | GCFillStyle | GCStipple,
			       &values);
}

static void free_gcs(KnappWidget w)
{
    XtReleaseGC((Widget)w, w->knapp.default_gc);
    XtReleaseGC((Widget)w, w->knapp.gray_gc);
}

static void compute_label_x(KnappWidget w)
{
    switch (w->knapp.justify) {
    case JustifyTypeCenter:
	w->knapp.label_x =
	    (int)(w->core.width - w->knapp.label_width +
		  w->knapp.left_margin - w->knapp.right_margin) / 2;
	break;
    case JustifyTypeRight:
	w->knapp.label_x =
	    w->core.width - (w->shadow.shadow_width +
			     w->knapp.right_margin + w->knapp.label_width);
	break;
    case JustifyTypeLeft:
    default:
	w->knapp.label_x = w->shadow.shadow_width + w->knapp.left_margin;
	break;
    }
}

static void init_labels(KnappWidget w, char *label)
{
    int	max = 0, n = 0;

    if (!label)
	label = w->core.name;

    do {
	char	*c = strchr(label, '\n');
	int	len, tmp;

	if (c)
	    len = c - label;
	else
	    len = strlen(label);

	if (n + 2 > w->knapp.no_labels) {
	    int	i = w->knapp.no_labels;

	    w->knapp.no_labels = 2 * (i + 2);
	    w->knapp.labels =
		(char **)XtRealloc((char *)w->knapp.labels,
				   w->knapp.no_labels * sizeof(char *));
	    while (i < w->knapp.no_labels)
		w->knapp.labels[i++] = NULL;
	}

	w->knapp.labels[n] = XtMalloc(len + 1);
	memcpy(w->knapp.labels[n], label, len);
	w->knapp.labels[n][len] = '\0';
	n++;

	tmp = XTextWidth(w->knapp.font, label, len);
	if (tmp > max)
	    max = tmp;

	label = c;
	if (label)
	    label++;
    } while (label);

    w->knapp.no_labels = n;
    w->knapp.max_width = max;
}

void free_labels(KnappWidget w)
{
    int	n;

    for (n = 0 ; n < w->knapp.no_labels ; n++)
	XtFree(w->knapp.labels[n]);
    XtFree((char *)w->knapp.labels);
    w->knapp.labels = NULL;
    w->knapp.no_labels = 0;
    w->knapp.label_no = 0;
}

void compute_label_width(KnappWidget w)
{
    int	n = w->knapp.label_no;

    if (w->knapp.no_labels == 0) {
	w->knapp.label_width = 0;
	return;
    }

    if (n < 0 || n >= w->knapp.no_labels)
	n = 0;

    w->knapp.label_width = XTextWidth(w->knapp.font, w->knapp.labels[n],
				      strlen(w->knapp.labels[n]));
}

/*************************************************************************/

static void ClassInitialize()
{
    XtSetTypeConverter(XtRString, XtRJustify, cvt_string_to_justify,
		       NULL, 0, XtCacheAll, NULL);
}

static void Initialize(Widget grequest, Widget gnew,
		       ArgList args, Cardinal *no_args)
{
    KnappWidget		new = (KnappWidget)gnew;

    new->knapp.stipple = create_stipple(XtScreen(new));
    init_gcs(new);
    new->knapp.set = False;
    new->knapp.calling_callbacks = False;
    new->knapp.active = True;

    new->knapp.labels = NULL;
    new->knapp.no_labels = 0;
    new->knapp.label_no = 0;

    init_labels(new, new->knapp.label);
    new->knapp.label = NULL;
    compute_label_width(new);

    if (new->core.width == 0)
	new->core.width =
	    new->knapp.max_width + 2 * new->shadow.shadow_width +
	    new->knapp.left_margin + new->knapp.right_margin;

    if (new->core.height == 0)
	new->core.height =
	    2 * (new->shadow.shadow_width + new->knapp.internal_height) +
	    new->knapp.font->ascent + new->knapp.font->descent;

    compute_label_x(new);
}

static void Destroy(Widget gw)
{
    KnappWidget	w = (KnappWidget)gw;

    free_labels(w);
    free_gcs(w);
    release_stipple(XtScreen(w), w->knapp.stipple);
}

static void Resize(Widget gw)
{
    KnappWidget	w = (KnappWidget)gw;

    compute_label_x(w);
}

static void Redisplay(Widget gw, XEvent *event, Region region)
{
    KnappWidget		w = (KnappWidget)gw;
    Display		*disp = XtDisplay(w);
    Window		win = XtWindow(w);
    int			sw = w->shadow.shadow_width;

    if (!XtIsRealized((Widget)w) ||
	(int)w->core.width < 2 * sw ||
	(int)w->core.height < 2 * sw)
	return;

    if (w->knapp.set && w->shadow.arm_gc != 0)
	XFillRectangle(disp, win, w->shadow.arm_gc, sw, sw,
		       w->core.width - 2 * sw, w->core.height - 2 * sw);

    if (w->knapp.label_no >= 0 && w->knapp.label_no < w->knapp.no_labels) {
	char	*label = w->knapp.labels[w->knapp.label_no];

	XDrawString(disp, win,
		    w->core.sensitive ?
		    w->knapp.default_gc : w->knapp.gray_gc,
		    w->knapp.label_x,
		    (int)(w->core.height - w->knapp.font->descent +
			  w->knapp.font->ascent) / 2 + (w->knapp.set ? 1 : 0),
		    label, strlen(label));
    }

    ShadowDrawShadows((ShadowWidget)w, 0, 0,
		      w->core.width, w->core.height, w->knapp.set);
}

static Boolean SetValues(Widget	   gcurrent,
			 Widget    grequest,
			 Widget    gnew,
			 ArgList   args,
			 Cardinal *num_args)
{
    KnappWidget	new = (KnappWidget)gnew;
    KnappWidget	current = (KnappWidget)gcurrent;
    Boolean	redisplay = False;

    if (new->shadow.shadow_width != current->shadow.shadow_width ||
	new->knapp.font != current->knapp.font ||
	new->knapp.label != current->knapp.label ||
	new->knapp.left_margin != current->knapp.left_margin ||
	new->knapp.right_margin != current->knapp.right_margin ||
	new->knapp.internal_height != current->knapp.internal_height) {
	if (new->knapp.label != current->knapp.label) {
	    free_labels(current);
	    init_labels(new, new->knapp.label);
	    new->knapp.label = NULL;
	    compute_label_width(new);
	}

	compute_label_x(new);

	if (new->knapp.resizable) {
	    new->core.width =
		new->knapp.max_width + 2 * new->shadow.shadow_width +
		new->knapp.left_margin + new->knapp.right_margin;
	    new->core.height =
		2 * (new->shadow.shadow_width +
		     new->knapp.internal_height) +
		new->knapp.font->ascent + new->knapp.font->descent;
	}

	redisplay = True;
    } else if (new->knapp.justify != current->knapp.justify) {
	compute_label_x(new);
	redisplay = True;
    }

    if (XtIsSensitive((Widget)new) != XtIsSensitive((Widget)current)) {
	redisplay = True;
	if (!XtIsSensitive((Widget)new))
	    new->knapp.set = False;
    }

    return redisplay;
}

static XtGeometryResult QueryGeometry(Widget gw,
				      XtWidgetGeometry *intended,
				      XtWidgetGeometry *preferred)
{
    KnappWidget	w = (KnappWidget)gw;
    Dimension	intended_width, intended_height;

    preferred->request_mode = CWWidth | CWHeight;
    preferred->width = w->knapp.max_width + 2 * w->shadow.shadow_width +
	w->knapp.left_margin + w->knapp.right_margin;
    preferred->height = w->knapp.font->ascent + w->knapp.font->descent +
	2 * (w->shadow.shadow_width + w->knapp.internal_height);

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

/*************************************************************************/

void KnappSetLabel(Widget gw, char *label)
{
    KnappWidget	w = (KnappWidget)gw;

    init_labels(w, label);
    compute_label_width(w);
    compute_label_x(w);

    if (w->knapp.resizable) {
	Dimension	width, height;

	width = w->knapp.max_width + 2 * w->shadow.shadow_width +
	    w->knapp.left_margin + w->knapp.right_margin;
	height = 2 * (w->shadow.shadow_width + w->knapp.internal_height) +
	    w->knapp.font->ascent + w->knapp.font->descent;

	(void)XtMakeResizeRequest((Widget)w, width, height, NULL, NULL);
    }

    if (XtIsRealized((Widget)w)) {
	XClearWindow(XtDisplay(w), XtWindow(w));
	w->core.widget_class->core_class.expose((Widget)w, NULL, 0);
    }
}

void KnappSetSensitive(Widget gw, int sensitive)
{
    KnappWidget	w = (KnappWidget)gw;

    if (gw->core.sensitive == sensitive)
	return;

    gw->core.sensitive = sensitive;
    if (!sensitive)
	w->knapp.set = False;
    if (XtIsRealized((Widget)w)) {
	XClearWindow(XtDisplay(w), XtWindow(w));
	w->core.widget_class->core_class.expose((Widget)w, NULL, 0);
    }
}

void KnappSetActive(Widget gw, int active)
{
    KnappWidget	w = (KnappWidget)gw;

    w->knapp.active = active;
}

void KnappSetLabelNo(Widget gw, int label_no, int sensitive)
{
    KnappWidget	w = (KnappWidget)gw;

    w->knapp.label_no = label_no;
    compute_label_width(w);
    compute_label_x(w);
    w->core.sensitive = sensitive;
    if (XtIsRealized((Widget)w)) {
	XClearWindow(XtDisplay(w), XtWindow(w));
	w->core.widget_class->core_class.expose((Widget)w, NULL, 0);
    }
}

int KnappGetLabelNo(Widget gw)
{
    KnappWidget	w = (KnappWidget)gw;

    return w->knapp.label_no;
}
