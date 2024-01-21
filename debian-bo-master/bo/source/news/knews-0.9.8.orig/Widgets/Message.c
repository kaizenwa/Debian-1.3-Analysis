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
#include <X11/Xos.h>

#include "Compat.h"
#include "MessageP.h"

static XtResource resources[] = {
    {XtNborderWidth, XtCBorderWidth, XtRDimension, sizeof(Dimension),
     XtOffsetOf(MessageRec, core.border_width), XtRImmediate, (XtPointer)0},
#define offset(field) XtOffsetOf(MessageRec, message.field)
    {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(foreground_pixel), XtRString, XtDefaultForeground},
    {XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
     offset(font), XtRString, XtDefaultFont},
    {XtNbuffer, XtCBuffer, XtRString, sizeof(String),
     offset(buffer), XtRString, (XtPointer)NULL},
    {XtNinternalWidth, XtCInternalWidth, XtRDimension, sizeof(Dimension),
     offset(internal_width), XtRImmediate, (XtPointer)8},
    {XtNinternalHeight, XtCInternalHeight, XtRDimension, sizeof(Dimension),
     offset(internal_height), XtRImmediate, (XtPointer)2},
    {XtNpreferredChars, XtCPreferredChars, XtRDimension, sizeof(Dimension),
     offset(pref_chars), XtRImmediate, (XtPointer)0},
    {XtNcenter, XtCCenter, XtRBoolean, sizeof(Boolean),
     offset(center), XtRImmediate, (XtPointer)True},
#undef offset
};

static void Initialize(Widget, Widget, ArgList, Cardinal*);
static void Destroy(Widget);
static void Redisplay(Widget, XEvent*, Region);
static Boolean SetValues(Widget, Widget, Widget, ArgList, Cardinal*);
static XtGeometryResult QueryGeometry(Widget, XtWidgetGeometry*,
				      XtWidgetGeometry*);

MessageClassRec messageClassRec = {
    {                                   /* core fields                  */
        (WidgetClass) &widgetClassRec,  /* superclass                   */
        "Message",                      /* class_name                   */
        sizeof(MessageRec),	        /* widget_size                  */
        NULL,                           /* class_initialize             */
        NULL,                           /* class_part_initialize        */
        FALSE,                          /* class_inited                 */
        Initialize,                     /* initialize                   */
        NULL,                           /* initialize_hook              */
        XtInheritRealize,               /* realize                      */
        NULL,                           /* actions                      */
        0,				/* num_actions                  */
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
        Destroy,                        /* destroy                      */
        False,				/* resize                       */
        Redisplay,                      /* expose                       */
        SetValues,                      /* set_values                   */
        NULL,                           /* set_values_hook              */
        XtInheritSetValuesAlmost,       /* set_values_almost            */
        NULL,                           /* get_values_hook              */
        NULL,                           /* accept_focus                 */
        XtVersion,                      /* version                      */
        NULL,                           /* callback_private             */
        NULL,				/* tm_table                     */
        QueryGeometry,			/* query_geometry               */
        XtInheritDisplayAccelerator,    /* display_accelerator          */
        NULL                            /* extension                    */
    },
    {                                   /* message fields                 */
        0                               /* extension                    */
    }
};

WidgetClass messageWidgetClass = (WidgetClass)&messageClassRec;

/*************************************************************************/

static void init_gcs(MessageWidget w)
{
    XGCValues	values;

    values.foreground = w->message.foreground_pixel;
    values.font = w->message.font->fid;
    w->message.default_gc = XtGetGC((Widget)w, GCForeground | GCFont, &values);
}

static void free_gcs(MessageWidget w)
{
    XtReleaseGC((Widget)w, w->message.default_gc);
}

static void copy_message(MessageWidget w, char *message)
{
    char	*c;
    long	n, i;

    if (!message)
	message = "";
    n = strlen(message);
    if (n + 8 > w->message.n_alloc) {
	w->message.n_alloc = n + 8;
	w->message.buffer = XtRealloc(w->message.buffer, n + 8);
    }
    memcpy(w->message.buffer, message, n + 1);

    c = strchr(w->message.buffer, '\n');
    i = 1;
    while (c) {
	c = strchr(c + 1, '\n');
	i++;
    }
    w->message.rows = i;
}

static void get_preferred_sizes(MessageWidget w,
				Dimension *width, Dimension *height)
{
    char	*c = w->message.buffer;

    *width = 0;
    if (w->message.pref_chars > 0)
	*width = w->message.pref_chars * w->message.font->max_bounds.width;
    else if (c) {
	while (True) {
	    char	*p = strchr(c, '\n');
	    int		len;
	    Dimension	temp;
	    
	    if (p)
		len = p - c;
	    else
		len = strlen(c);

	    temp = XTextWidth(w->message.font, c, len);
	    if (*width < temp)
		*width = temp;
	    
	    if (p)
		c = p + 1;
	    else
		break;
	}
    }
    *width += 2 * w->message.internal_width;

    if (w->message.rows > 1)
	*height = w->message.rows *
	    (w->message.font->ascent +
	     w->message.font->descent);
    else
	*height = w->message.font->ascent +
	    w->message.font->descent;
    *height += 2 * w->message.internal_height;
}

/*************************************************************************/

static void Initialize(Widget grequest, Widget gnew,
		       ArgList args, Cardinal *no_args)
{
    MessageWidget	new = (MessageWidget)gnew;
    char		*old = new->message.buffer;

    if (!old)
	old = XtName((Widget)new);

    new->message.rows = 1;
    new->message.buffer = NULL;
    new->message.n_alloc = 0;
    copy_message(new, old);

    init_gcs(new);

    if (new->core.width == 0 || new->core.height == 0) {
	Dimension	width, height;

	get_preferred_sizes(new, &width, &height);
	if (new->core.width  == 0)
	    new->core.width  = width;
	if (new->core.height == 0)
	    new->core.height = height;
    }
}

static void Destroy(Widget gw)
{
    MessageWidget	w = (MessageWidget)gw;

    free_gcs(w);
    XtFree(w->message.buffer);
}

static void Redisplay(Widget gw, XEvent *event, Region region)
{
    MessageWidget	w = (MessageWidget)gw;
    Display		*disp = XtDisplay(w);
    Window		win = XtWindow(w);
    char		*c = w->message.buffer;
    int			x, y, dy;

    if (!XtIsRealized((Widget)w) || !c)
	return;

    x = w->message.internal_width;
    y = w->message.internal_height + w->message.font->ascent;
    dy = w->message.font->descent + w->message.font->ascent;

    for(;;) {
	char	*p = strchr(c, '\n');
	int	len;

	if (p)
	    len = p - c;
	else
	    len = strlen(c);

	if (w->message.center)
	    x = ((int)w->core.width - XTextWidth(w->message.font, c, len)) / 2;
	XDrawString(disp, win, w->message.default_gc,
		    x, y, c, len);

	y += dy;
	if (!p)
	    break;
	c = p + 1;
    }
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    MessageWidget	new = (MessageWidget)gnew;
    MessageWidget	current = (MessageWidget)gcurrent;
    Boolean		redisplay = False;
    char		*new_buffer = new->message.buffer;
    char		*old_buffer = current->message.buffer;

    if (new_buffer != old_buffer) {
	new->message.buffer = old_buffer;
	copy_message(new, new_buffer);
	redisplay = True;
    }

    if (redisplay) {
	Dimension	width, height;

	get_preferred_sizes(new, &width, &height);
	new->core.width  = width;
	new->core.height = height;
    }

    if (new->message.font != current->message.font)
	redisplay = True;

    return redisplay;
}

static XtGeometryResult QueryGeometry(Widget gw,
				      XtWidgetGeometry *intended,
				      XtWidgetGeometry *preferred)
{
    MessageWidget	w = (MessageWidget)gw;
    Dimension		width, height;
    Dimension		intended_height, intended_width;

    get_preferred_sizes(w, &width, &height);

    preferred->request_mode = CWWidth | CWHeight;
    preferred->width = width;
    preferred->height = height;

    if (intended->request_mode & CWWidth)
	intended_width = intended->width;
    else
	intended_width = w->core.width;
    if (intended->request_mode & CWHeight)
	intended_height = intended->height;
    else
	intended_height = w->core.height;

    if (width == w->core.width && height == w->core.height)
	return XtGeometryNo;
    else if (width == intended_width && height == intended_height)
	return XtGeometryYes;
    else
	return XtGeometryAlmost;
}

/*************************************************************************/

void MessageSetAndRedraw(Widget gw, char *buffer, int bell)
{
    MessageWidget	w = (MessageWidget)gw;
    Display		*disp = XtDisplay(w);
    Window		win = XtWindow(w);

    copy_message(w, buffer);
    XClearWindow(disp, win);
    Redisplay((Widget)w, NULL, NULL);
    if (bell)
	XBell(disp, 0);
    XFlush(disp);
}
