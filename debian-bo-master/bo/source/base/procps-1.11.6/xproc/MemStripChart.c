/***********************************************************************
 *
 * MemStripChart Widget
 * from StripChart Widget derived 
 *
 * Author:  Hans-Helmut B"uhmann 20. Jan. 1996
 *
 ***********************************************************************/

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/XawInit.h>
#include "MemStripCharP.h"
#include <X11/Xfuncs.h>

#define MS_PER_SEC 1000

/* Private Data */

#define offset(field) XtOffsetOf(MemStripChartRec, field)

static XtResource resources[] = {
    {XtNwidth, XtCWidth, XtRDimension, sizeof(Dimension),
	offset(core.width), XtRImmediate, (XtPointer) 120},
    {XtNheight, XtCHeight, XtRDimension, sizeof(Dimension),
	offset(core.height), XtRImmediate, (XtPointer) 120},
    {XtNupdate, XtCInterval, XtRInt, sizeof(int),
        offset(mem_strip_chart.update), XtRImmediate, (XtPointer) 10},
    {XtNminScale, XtCScale, XtRInt, sizeof(int),
        offset(mem_strip_chart.min_scale), XtRImmediate, (XtPointer) 1},
    {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(mem_strip_chart.fgpixel), XtRString, XtDefaultForeground},
    {XtNhighlight, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(mem_strip_chart.hipixel), XtRString, XtDefaultForeground},
    {XtNcodecolor, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(mem_strip_chart.codepixel), XtRString, XtDefaultForeground},
    {XtNcachedcolor, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(mem_strip_chart.cachedpixel), XtRString, XtDefaultBackground},
    {XtNbuffercolor, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(mem_strip_chart.bufferpixel), XtRString, XtDefaultForeground},
    {XtNfreecolor, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(mem_strip_chart.freepixel), XtRString, XtDefaultBackground},
    {XtNswapcolor, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(mem_strip_chart.swappixel), XtRString, XtDefaultForeground},
    {XtNgetValue, XtCCallback, XtRCallback, sizeof(XtPointer),
        offset(mem_strip_chart.get_value), XtRImmediate, (XtPointer) NULL},
    {XtNjumpScroll, XtCJumpScroll, XtRInt, sizeof(int),
        offset(mem_strip_chart.jump_val), XtRImmediate, (XtPointer) DEFAULT_JUMP},
};

#undef offset

static void Initialize(), Destroy(), Redisplay(), MoveChart(), SetPoints();
static Boolean SetValues();
static int repaint_window();

MemStripChartClassRec memStripChartClassRec = {
    { /* core fields */
    /* superclass		*/	(WidgetClass) &simpleClassRec,
    /* class_name		*/	"MemStripChart",
    /* size			*/	sizeof(MemStripChartRec),
    /* class_initialize		*/	XawInitializeWidgetSet,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	XtInheritRealize,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	XtExposeCompressMultiple |
	                                XtExposeGraphicsExposeMerged,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	SetPoints,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	NULL,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	NULL,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
    },
    { /* Simple class fields */
    /* change_sensitive		*/	XtInheritChangeSensitive
    }
};

WidgetClass memStripChartWidgetClass = (WidgetClass) &memStripChartClassRec;

/****************************************************************
 *
 * Private Procedures
 *
 ****************************************************************/

static void draw_it();

/*	Function Name: CreateGC
 *	Description: Creates the GC's
 *	Arguments: w - the mem strip chart widget.
 *                 which - which GC's to create.
 *	Returns: none
 */

static void
CreateGC(w, which)
MemStripChartWidget w;
unsigned int which;
{
  XGCValues	myXGCV;

  if (which & FOREGROUND) {
    myXGCV.foreground = w->mem_strip_chart.fgpixel;
    w->mem_strip_chart.fgGC = XtGetGC((Widget) w, GCForeground, &myXGCV);
  }

  if (which & HIGHLIGHT) {
    myXGCV.foreground = w->mem_strip_chart.hipixel;
    w->mem_strip_chart.hiGC = XtGetGC((Widget) w, GCForeground, &myXGCV);
  }

  if (which & CODE) {
    myXGCV.foreground = w->mem_strip_chart.codepixel;
    w->mem_strip_chart.codeGC = XtGetGC((Widget) w, GCForeground, &myXGCV);
  }

  if (which & SHARED) {
    myXGCV.foreground = w->mem_strip_chart.cachedpixel;
    w->mem_strip_chart.cachedGC = XtGetGC((Widget) w, GCForeground, &myXGCV);
  }

  if (which & BUFFER) {
    myXGCV.foreground = w->mem_strip_chart.bufferpixel;
    w->mem_strip_chart.bufferGC = XtGetGC((Widget) w, GCForeground, &myXGCV);
  }

  if (which & FREE) {
    myXGCV.foreground = w->mem_strip_chart.freepixel;
    w->mem_strip_chart.freeGC = XtGetGC((Widget) w, GCForeground, &myXGCV);
  }

  if (which & SWAP) {
    myXGCV.foreground = w->mem_strip_chart.swappixel;
    w->mem_strip_chart.swapGC = XtGetGC((Widget) w, GCForeground, &myXGCV);
  }

}

/*	Function Name: DestroyGC
 *	Description: Destroys the GC's
 *	Arguments: w - the mem strip chart widget.
 *                 which - which GC's to destroy.
 *	Returns: none
 */

static void
DestroyGC(w, which)
MemStripChartWidget w;
unsigned int which;
{
  if (which & FOREGROUND) 
    XtReleaseGC((Widget) w, w->mem_strip_chart.fgGC);

  if (which & HIGHLIGHT) 
    XtReleaseGC((Widget) w, w->mem_strip_chart.hiGC);

  if (which & CODE) {
    XtReleaseGC((Widget) w, w->mem_strip_chart.codeGC);
  }

  if (which & SHARED) {
    XtReleaseGC((Widget) w, w->mem_strip_chart.cachedGC);
  }

  if (which & BUFFER) {
    XtReleaseGC((Widget) w, w->mem_strip_chart.bufferGC);
  }

  if (which & FREE) {
    XtReleaseGC((Widget) w, w->mem_strip_chart.freeGC);
  }

  if (which & SWAP) {
    XtReleaseGC((Widget) w, w->mem_strip_chart.swapGC);
  }
}


/*	Function Name: DrawMemStrip
 *	Description: Draw a mem strip
 *	Arguments: w - the mem strip chart widget.
 *                 x - the x-position.
 *	Returns: none
 */

static void
DrawMemStrip(w, x)
MemStripChartWidget w;
unsigned int x;
{
       int top, bottom;

       bottom = w->core.height;
       if (w->mem_strip_chart.valuedata[x].code != 0.0) {
	 top = (int) (w->core.height
		      - (int)(w->core.height * w->mem_strip_chart.valuedata[x].code)
		      / w->mem_strip_chart.scale);

	 XFillRectangle(XtDisplay(w), XtWindow(w), w->mem_strip_chart.codeGC,
			x, top, (unsigned int) 1, bottom - top);
	 bottom = top;
       }

       if (w->mem_strip_chart.valuedata[x].cached != 0.0) {
	 top = (int) (w->core.height
		      - (int)(w->core.height * (w->mem_strip_chart.valuedata[x].code
						+ w->mem_strip_chart.valuedata[x].cached))
		      / w->mem_strip_chart.scale);
	 XFillRectangle(XtDisplay(w), XtWindow(w), w->mem_strip_chart.cachedGC,
			x, top, (unsigned int) 1, bottom - top);

	 bottom = top;
       }

       if (w->mem_strip_chart.valuedata[x].buffer != 0.0) {
	 top = (int) (w->core.height
		      - (int)(w->core.height * (1.0 - w->mem_strip_chart.valuedata[x].free)) 
		      / w->mem_strip_chart.scale);
	 XFillRectangle(XtDisplay(w), XtWindow(w), w->mem_strip_chart.bufferGC,
			x, top, (unsigned int) 1, bottom - top);
	 bottom = top;
       }

       if (w->mem_strip_chart.valuedata[x].free != 0.0) {
	 top = (int) (w->core.height
		      - (int)(w->core.height)
		      / w->mem_strip_chart.scale);
	 XFillRectangle(XtDisplay(w), XtWindow(w), w->mem_strip_chart.freeGC,
			x, top, (unsigned int) 1, bottom - top);
	 bottom = top;
       }
       if (w->mem_strip_chart.valuedata[x].swap != 0.0) {
	 top = (int) (w->core.height
		      - (int)(w->core.height * (1.0 + w->mem_strip_chart.valuedata[x].swap)) 
		      / w->mem_strip_chart.scale);
	 XFillRectangle(XtDisplay(w), XtWindow(w), w->mem_strip_chart.swapGC,
			x, top, (unsigned int) 1, bottom - top);
       }
}

/* ARGSUSED */
static void Initialize (greq, gnew, args, num_args)
    Widget greq, gnew;
    ArgList args;
    Cardinal *num_args;
{
    MemStripChartWidget w = (MemStripChartWidget)gnew;

    if (w->mem_strip_chart.update > 0)
        w->mem_strip_chart.interval_id = XtAppAddTimeOut(
					XtWidgetToApplicationContext(gnew),
					w->mem_strip_chart.update * MS_PER_SEC, 
					draw_it, (XtPointer) gnew);
    CreateGC(w, (unsigned int) ALL_GCS);

    w->mem_strip_chart.scale = w->mem_strip_chart.min_scale;
    w->mem_strip_chart.interval = 0;
    w->mem_strip_chart.max_value = 0.0;
    w->mem_strip_chart.points = NULL;
    SetPoints(w);
}
 
static void Destroy (gw)
     Widget gw;
{
     MemStripChartWidget w = (MemStripChartWidget)gw;

     if (w->mem_strip_chart.update > 0)
         XtRemoveTimeOut (w->mem_strip_chart.interval_id);
     if (w->mem_strip_chart.points)
	 XtFree((char *) w->mem_strip_chart.points);
     DestroyGC(w, (unsigned int) ALL_GCS);
}

/*
 * NOTE: This function really needs to recieve graphics exposure 
 *       events, but since this is not easily supported until R4 I am
 *       going to hold off until then.
 */

/* ARGSUSED */
static void Redisplay(w, event, region)
     Widget w;
     XEvent *event;
     Region region;
{
    if (event->type == GraphicsExpose)
	(void) repaint_window ((MemStripChartWidget)w, event->xgraphicsexpose.x,
			       event->xgraphicsexpose.width);
    else
	(void) repaint_window ((MemStripChartWidget)w, event->xexpose.x,
			       event->xexpose.width);
}

/* ARGSUSED */
static void 
draw_it(client_data, id)
XtPointer client_data;
XtIntervalId *id;		/* unused */
{
   MemStripChartWidget w = (MemStripChartWidget)client_data;
   MemStripChartCallbackData value;
   double usedmem;
   
   if (w->mem_strip_chart.update > 0)
       w->mem_strip_chart.interval_id =
       XtAppAddTimeOut(XtWidgetToApplicationContext( (Widget) w),
		       w->mem_strip_chart.update * MS_PER_SEC, draw_it,client_data);

   if (w->mem_strip_chart.interval >= (int)w->core.width)
       MoveChart( (MemStripChartWidget) w, TRUE);

   /* Get the value, stash the point and draw corresponding line. */

   if (w->mem_strip_chart.get_value == NULL)
       return;

   XtCallCallbacks( (Widget)w, XtNgetValue, (XtPointer)&value );

   /* 
    * Keep w->mem_strip_chart.max_value up to date, and if this data 
    * point is off the graph, change the scale to make it fit. 
    */
   
   usedmem = 1.0 + value.swap;
   if (usedmem > w->mem_strip_chart.max_value) {
       w->mem_strip_chart.max_value = usedmem;
       if (XtIsRealized((Widget)w) && 
	   w->mem_strip_chart.max_value > w->mem_strip_chart.scale) {
	   XClearWindow( XtDisplay (w), XtWindow (w));
	   w->mem_strip_chart.interval = repaint_window(w, 0, (int) w->core.width);
       }
   }

   w->mem_strip_chart.valuedata[w->mem_strip_chart.interval] = value;
   if (XtIsRealized((Widget)w)) {

       DrawMemStrip(w, w->mem_strip_chart.interval, value);
       
       /*
	* Fill in the graph lines we just painted over.
	*/

       if (w->mem_strip_chart.points != NULL) {
	   w->mem_strip_chart.points[0].x = w->mem_strip_chart.interval;
	   XDrawPoints(XtDisplay(w), XtWindow(w), w->mem_strip_chart.hiGC,
		       w->mem_strip_chart.points, w->mem_strip_chart.scale - 1,
		       CoordModePrevious);
       }

       XFlush(XtDisplay(w));		    /* Flush output buffers */
   }
   w->mem_strip_chart.interval++;		    /* Next point */
} /* draw_it */

/* Blts data according to current size, then redraws the stripChart window.
 * Next represents the number of valid points in data.  Returns the (possibly)
 * adjusted value of next.  If next is 0, this routine draws an empty window
 * (scale - 1 lines for graph).  If next is less than the current window width,
 * the returned value is identical to the initial value of next and data is
 * unchanged.  Otherwise keeps half a window's worth of data.  If data is
 * changed, then w->mem_strip_chart.max_value is updated to reflect the
 * largest data point.
 */

static int 
repaint_window(w, left, width)
MemStripChartWidget w;
int left, width;
{
    int i, j;
    int next = w->mem_strip_chart.interval;
    int scale = w->mem_strip_chart.scale;
    int scalewidth = 0;

    /* Compute the minimum scale required to graph the data, but don't go
       lower than min_scale. */
    if (w->mem_strip_chart.interval != 0 || scale <= (int)w->mem_strip_chart.max_value)
      scale = ((int) (w->mem_strip_chart.max_value)) + 1;
    if (scale < w->mem_strip_chart.min_scale)
      scale = w->mem_strip_chart.min_scale;

    if (scale != w->mem_strip_chart.scale) {
      w->mem_strip_chart.scale = scale;
      left = 0;
      width = next;
      scalewidth = w->core.width;

      SetPoints(w);

      if (XtIsRealized ((Widget) w)) 
	XClearWindow (XtDisplay (w), XtWindow (w));

    }

    if (XtIsRealized((Widget)w)) {
	Display *dpy = XtDisplay(w);
	Window win = XtWindow(w);

	width += left - 1;
	if (!scalewidth) scalewidth = width;

	if (next < ++width) width = next;

	/* Draw data point lines. */
	for (i = left; i < width; i++) {
	  DrawMemStrip(w, i, w->mem_strip_chart.valuedata[i]);
       	}

	/* Draw graph reference lines */
	for (i = 1; i < w->mem_strip_chart.scale; i++) {
	    j = i * ((int)w->core.height / w->mem_strip_chart.scale);
	    XDrawLine(dpy, win, w->mem_strip_chart.hiGC, left, j, scalewidth, j);
	}
    }
    return(next);
}

/*	Function Name: MoveChart
 *	Description: moves the chart over when it would run off the end.
 *	Arguments: w - the load widget.
 *                 blit - blit the bits? (TRUE/FALSE).
 *	Returns: none.
 */

static void
MoveChart(w, blit)
MemStripChartWidget w;
Boolean blit;
{
    double old_max;
    int left, i, j;
    int next = w->mem_strip_chart.interval;

    if (!XtIsRealized((Widget) w)) return;

    if (w->mem_strip_chart.jump_val < 0) w->mem_strip_chart.jump_val = DEFAULT_JUMP;
    if (w->mem_strip_chart.jump_val == DEFAULT_JUMP)
        j = w->core.width >> 1; /* Half the window width. */
    else {
        j = w->core.width - w->mem_strip_chart.jump_val;
	if (j < 0) j = 0;
    }

    (void) memmove((char *)(w->mem_strip_chart.valuedata), 
		   (char *)(w->mem_strip_chart.valuedata + next - j),
		   j * sizeof(MemStripChartCallbackData));
    next = w->mem_strip_chart.interval = j;
	
    /*
     * Since we just lost some data, recompute the 
     * w->mem_strip_chart.max_value. 
     */

    old_max = w->mem_strip_chart.max_value;
    w->mem_strip_chart.max_value = 0.0;
    for (i = 0; i < next; i++) {
      if (w->mem_strip_chart.valuedata[i].swap + 1.0 > w->mem_strip_chart.max_value) 
	w->mem_strip_chart.max_value = w->mem_strip_chart.valuedata[i].swap + 1.0;
    }

    if (!blit) return;		/* we are done... */

    if ( ((int) old_max) != ( (int) w->mem_strip_chart.max_value) ) {
      XClearWindow(XtDisplay(w), XtWindow(w));
      repaint_window(w, 0, (int) w->core.width);
      return;
    }

    XCopyArea(XtDisplay((Widget)w), XtWindow((Widget)w), XtWindow((Widget)w),
	      w->mem_strip_chart.hiGC, (int) w->core.width - j, 0,
	      (unsigned int) j, (unsigned int) w->core.height,
	      0, 0);

    XClearArea(XtDisplay((Widget)w), XtWindow((Widget)w), 
	       (int) j, 0, 
	       (unsigned int) w->core.width - j, (unsigned int)w->core.height,
	       FALSE);

    /* Draw graph reference lines */
    left = j;
    for (i = 1; i < w->mem_strip_chart.scale; i++) {
      j = i * ((int)w->core.height / w->mem_strip_chart.scale);
      XDrawLine(XtDisplay((Widget) w), XtWindow( (Widget) w),
		w->mem_strip_chart.hiGC, left, j, (int)w->core.width, j);
    }
    return;
}

/* ARGSUSED */
static Boolean SetValues (current, request, new, args, num_args)
    Widget current, request, new;
    ArgList args;
    Cardinal *num_args;
{
    MemStripChartWidget old = (MemStripChartWidget)current;
    MemStripChartWidget w = (MemStripChartWidget)new;
    Boolean ret_val = FALSE;
    unsigned int new_gc = NO_GCS;

    if (w->mem_strip_chart.update != old->mem_strip_chart.update) {
	if (old->mem_strip_chart.update > 0)
	    XtRemoveTimeOut (old->mem_strip_chart.interval_id);
	if (w->mem_strip_chart.update > 0)
	    w->mem_strip_chart.interval_id =
		XtAppAddTimeOut(XtWidgetToApplicationContext(new),
				w->mem_strip_chart.update * MS_PER_SEC,
				draw_it, (XtPointer)w);
    }

    if ( w->mem_strip_chart.min_scale > (int) ((w->mem_strip_chart.max_value) + 1) )
      ret_val = TRUE;
     
    if ( w->mem_strip_chart.fgpixel != old->mem_strip_chart.fgpixel ) {
      new_gc |= FOREGROUND;
      ret_val = True;
    }
    
    if ( w->mem_strip_chart.hipixel != old->mem_strip_chart.hipixel ) {
      new_gc |= HIGHLIGHT;
      ret_val = True;
    }

    if ( w->mem_strip_chart.codepixel != old->mem_strip_chart.codepixel ) {
      new_gc |= CODE;
      ret_val = True;
    }

    if ( w->mem_strip_chart.cachedpixel != old->mem_strip_chart.cachedpixel ) {
      new_gc |= SHARED;
      ret_val = True;
    }

    if ( w->mem_strip_chart.bufferpixel != old->mem_strip_chart.bufferpixel ) {
      new_gc |= BUFFER;
      ret_val = True;
    }

    if ( w->mem_strip_chart.freepixel != old->mem_strip_chart.freepixel ) {
      new_gc |= FREE;
      ret_val = True;
    }

    if ( w->mem_strip_chart.swappixel != old->mem_strip_chart.swappixel ) {
      new_gc |= SWAP;
      ret_val = True;
    }

    DestroyGC(old, new_gc);
    CreateGC(w, new_gc);

    return( ret_val );
}

/*	Function Name: SetPoints
 *	Description: Sets up the polypoint that will be used to draw in
 *                   the graph lines.
 *	Arguments: w - the MemStripChart widget.
 *	Returns: none.
 */

#define HEIGHT ( (unsigned int) w->core.height)

static void
SetPoints(widget)
Widget widget;
{
    MemStripChartWidget w = (MemStripChartWidget) widget;
    XPoint * points;
    Cardinal size;
    int i;

    if (w->mem_strip_chart.scale <= 1) { /* no scale lines. */
	XtFree ((char *) w->mem_strip_chart.points);
	w->mem_strip_chart.points = NULL;
	return;
    }
    
    size = sizeof(XPoint) * (w->mem_strip_chart.scale - 1);

    points = (XPoint *) XtRealloc( (XtPointer) w->mem_strip_chart.points, size);
    w->mem_strip_chart.points = points;

    /* Draw graph reference lines into clip mask */

    for (i = 1; i < w->mem_strip_chart.scale; i++) {
	points[i - 1].x = 0;
	points[i - 1].y = HEIGHT / w->mem_strip_chart.scale;
    }
}
