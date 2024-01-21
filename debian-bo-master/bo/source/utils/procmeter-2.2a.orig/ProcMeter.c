/***************************************
  $Header: /home/amb/procmeter/RCS/ProcMeter.c 2.4 1996/09/21 20:42:53 amb Exp $

  ProcMeter Widget Source file (for ProcMeter 2.2).
  ******************/ /******************
  Written by Andrew M. Bishop

  This file Copyright 1996 Andrew M. Bishop
  It may be distributed under the GNU Public License, version 2, or
  any higher version.  See section COPYING of the GNU Public license
  for conditions under which this file may be redistributed.
  ***************************************/


#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/CoreP.h>

#include "ProcMeterP.h"

static void Initialize(ProcMeterWidget request,ProcMeterWidget new);
static void Destroy(ProcMeterWidget w);
static Boolean SetValues(ProcMeterWidget current,ProcMeterWidget request,ProcMeterWidget new);
static void SizeMeter(ProcMeterWidget w);
static void Resize(ProcMeterWidget w);
static void Redisplay(ProcMeterWidget w,XEvent *event,Region region);
static void Update(ProcMeterWidget w,Boolean scroll);

static XtResource resources[]=
{
 /* The foreground parts. */

 {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
  XtOffset(ProcMeterWidget,procmeter.foreground_pixel),XtRString,XtDefaultForeground},

 /* The line style */

 {XtNsolid, XtCSolid, XtRBoolean, sizeof(Boolean),
  XtOffset(ProcMeterWidget,procmeter.line_solid), XtRString, "TRUE" },

 /* The grid parts. */

 {XtNgrid, XtCGrid, XtRPixel, sizeof(Pixel),
  XtOffset(ProcMeterWidget,procmeter.grid_pixel),XtRString,XtDefaultForeground},
 {XtNgridMin, XtCGridMin, XtRInt, sizeof(int),
  XtOffset(ProcMeterWidget,procmeter.grid_min), XtRString, "1" },

 /* The label parts. */

 {XtNlabel, XtCLabel, XtRString, sizeof(XtPointer),
  XtOffset(ProcMeterWidget,procmeter.label), XtRString, NULL },
 {XtNlabelPosition, XtCLabelPosition, XtRInt, sizeof(int),
  XtOffset(ProcMeterWidget,procmeter.label_pos), XtRString, "-1" },
 {XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct*),
  XtOffset(ProcMeterWidget,procmeter.label_font), XtRString, "-*-*-*-r-normal-sans-8-*-*-*-p-*-*-*"},
};

/*+ The actual ProcMeter Widget Class Record. +*/
ProcMeterClassRec procMeterClassRec=
{
 {
  (WidgetClass) &widgetClassRec,
  "ProcMeter",
  sizeof(ProcMeterRec),
  NULL,
  NULL,
  FALSE,
  (XtInitProc)Initialize,
  NULL,
  XtInheritRealize,
  NULL,
  0,
  resources,
  XtNumber(resources),
  NULLQUARK,
  TRUE,
  XtExposeCompressMaximal|XtExposeGraphicsExpose,
  TRUE,
  TRUE,
  (XtWidgetProc)Destroy,
  (XtWidgetProc)Resize,
  (XtExposeProc)Redisplay,
  (XtSetValuesFunc)SetValues,
  NULL,
  XtInheritSetValuesAlmost,
  NULL,
  NULL,
  XtVersion,
  NULL,
  XtInheritTranslations,
  NULL,
  NULL,
  NULL,
 },
 {
  0
 }
};

/*+ The actual ProcMeter Widget Class Record masquerading as a WidgetClass type. +*/
WidgetClass procMeterWidgetClass=(WidgetClass)&procMeterClassRec;


/*++++++++++++++++++++++++++++++++++++++
  Initialise a new ProcMeter Widget.

  ProcMeterWidget request The requested parameters.

  ProcMeterWidget new The new parameters that are to be filled in.
  ++++++++++++++++++++++++++++++++++++++*/

static void Initialize(ProcMeterWidget request,ProcMeterWidget new)
{
 XGCValues values;

 /* The core widget parts. */

 if(request->core.width ==0)
    new->core.width=100;
 if(request->core.height==0)
    new->core.height=100;

 /* The label parts. */

 if((new->procmeter.label_pos!=ProcMeterLabelTop) &&
    (new->procmeter.label_pos!=ProcMeterLabelNone) &&
    (new->procmeter.label_pos!=ProcMeterLabelBottom))
    new->procmeter.label_pos=ProcMeterLabelNone;

 new->procmeter.label=XtNewString(request->procmeter.label);

 /* The foreground parts. */

 values.font=new->procmeter.label_font->fid;
 values.foreground=new->procmeter.foreground_pixel;
 values.background=new->core.background_pixel;
 new->procmeter.foreground_gc=XtGetGC((Widget)new,GCForeground|GCBackground|GCFont,&values);

 /* The grid parts. */

 values.foreground=new->procmeter.grid_pixel;
 values.background=new->core.background_pixel;
 new->procmeter.grid_gc=XtGetGC((Widget)new,GCForeground|GCBackground,&values);

 if(request->procmeter.grid_min<0)
    new->procmeter.grid_min=-request->procmeter.grid_min,
    new->procmeter.grid_drawn=0;
 else
    new->procmeter.grid_drawn=1;
 if(request->procmeter.grid_min==0)
    new->procmeter.grid_min=1;

 new->procmeter.grid_num=new->procmeter.grid_min;

 /* The data parts. */

 new->procmeter.data_num=new->core.width;
 new->procmeter.data=(unsigned short*)XtCalloc(new->procmeter.data_num,sizeof(unsigned short));
 new->procmeter.data_max=0;
 new->procmeter.data_index=0;

 /* The rest of the sizing. */

 SizeMeter(new);
}


/*++++++++++++++++++++++++++++++++++++++
  Destroy a ProcMeter Widget.

  ProcMeterWidget w The Widget to destroy.
  ++++++++++++++++++++++++++++++++++++++*/

static void Destroy(ProcMeterWidget w)
{
 XtFree((XtPointer)w->procmeter.label);
 XtReleaseGC((Widget)w,w->procmeter.foreground_gc);
 XtReleaseGC((Widget)w,w->procmeter.grid_gc);
 XtFree((XtPointer)w->procmeter.data);
}


/*++++++++++++++++++++++++++++++++++++++
  The setvalues procedure that is used to set the values internal to the Widget.

  Boolean SetValues Returns True if the Widget is to be redrawn.

  ProcMeterWidget current The current Widget values.

  ProcMeterWidget request The requested Widget values.

  ProcMeterWidget new The new Widget values to be set up.
  ++++++++++++++++++++++++++++++++++++++*/

static Boolean SetValues(ProcMeterWidget current,ProcMeterWidget request,ProcMeterWidget new)
{
 Boolean redraw=False;

 /* The label parts. */

 if(request->procmeter.label_pos!=current->procmeter.label_pos)
   {
    if((request->procmeter.label_pos!=ProcMeterLabelTop) &&
       (request->procmeter.label_pos!=ProcMeterLabelNone) &&
       (request->procmeter.label_pos!=ProcMeterLabelBottom))
       new->procmeter.label_pos=ProcMeterLabelNone;

    redraw=True;
   }

 if(request->procmeter.label!=current->procmeter.label)
   {
    XtFree((XtPointer)new->procmeter.label);
    new->procmeter.label=XtNewString(request->procmeter.label);

    redraw=True;
   }

 /* The foreground parts. */

 if((request->procmeter.label_font      !=current->procmeter.label_font)||
    (request->procmeter.foreground_pixel!=current->procmeter.foreground_pixel))
   {
    XGCValues xgcv;

    XGetGCValues(XtDisplay(new),new->procmeter.foreground_gc,GCForeground|GCBackground|GCFont,&xgcv);
    XtReleaseGC((Widget)new,new->procmeter.foreground_gc);
    xgcv.font=request->procmeter.label_font->fid;
    xgcv.foreground=request->procmeter.foreground_pixel;
    xgcv.background=request->core.background_pixel;
    new->procmeter.foreground_gc=XtGetGC((Widget)new,GCForeground|GCBackground|GCFont,&xgcv);

    redraw=True;
   }

 /* The line style */

 if(request->procmeter.line_solid!=current->procmeter.line_solid)
    redraw=True;

 /* The grid parts. */

 if(request->procmeter.grid_pixel!=current->procmeter.grid_pixel)
   {
    XGCValues xgcv;

    XGetGCValues(XtDisplay(new),new->procmeter.grid_gc,GCForeground|GCBackground,&xgcv);
    XtReleaseGC((Widget)new,new->procmeter.foreground_gc);
    xgcv.foreground=request->procmeter.grid_pixel;
    xgcv.background=request->core.background_pixel;
    new->procmeter.grid_gc=XtGetGC((Widget)new,GCForeground|GCBackground,&xgcv);

    redraw=True;
   }

 if(request->procmeter.grid_min!=current->procmeter.grid_min)
   {
    if(request->procmeter.grid_min<0)
       new->procmeter.grid_min=-request->procmeter.grid_min,
       new->procmeter.grid_drawn=0;
    else
       new->procmeter.grid_drawn=1;
    if(request->procmeter.grid_min==0)
       new->procmeter.grid_min=1;

    if(new->procmeter.grid_min>=new->procmeter.grid_num)
       new->procmeter.grid_num=new->procmeter.grid_min;

    redraw=True;
   }

 if(redraw)
    SizeMeter(new);

 return(redraw);
}


/*++++++++++++++++++++++++++++++++++++++
  Resize the ProcMeter Widget.

  ProcMeterWidget w The Widget that is resized.
  ++++++++++++++++++++++++++++++++++++++*/

static void Resize(ProcMeterWidget w)
{
 if(w->procmeter.data_num!=w->core.width)
   {
    int i,old_num=w->procmeter.data_num;
    unsigned short* old_data=w->procmeter.data;

    w->procmeter.data_num=w->core.width;
    w->procmeter.data=(unsigned short*)XtCalloc(w->procmeter.data_num,sizeof(unsigned short));

    if(w->procmeter.data_num<old_num)
       i=w->procmeter.data_num;
    else
       i=old_num;

    for(;i>0;i--)
       w->procmeter.data[(-i+w->procmeter.data_num)%w->procmeter.data_num]=old_data[(w->procmeter.data_index-i+old_num)%old_num];

    w->procmeter.data_index=0;

    XtFree((XtPointer)old_data);

    for(i=w->procmeter.data_max=0;i<w->procmeter.data_num;i++)
       if(w->procmeter.data[i]>w->procmeter.data_max)
          w->procmeter.data_max=w->procmeter.data[i];

    w->procmeter.grid_num=(w->procmeter.data_max+ProcMeterWidgetScale-1)/ProcMeterWidgetScale;

    if(w->procmeter.grid_num<w->procmeter.grid_min)
       w->procmeter.grid_num=w->procmeter.grid_min;
   }

 SizeMeter(w);
}


/*++++++++++++++++++++++++++++++++++++++
  Redisplay the ProcMeter Widget.

  ProcMeterWidget w The Widget to redisplay.

  XEvent *event The event that caused the redisplay.

  Region region The region that was exposed.
  ++++++++++++++++++++++++++++++++++++++*/

static void Redisplay(ProcMeterWidget w,XEvent *event,Region region)
{
 if(w->core.visible)
    Update(w,False);
}


/*++++++++++++++++++++++++++++++++++++++
  Perform all of the sizing on the Widget when it is created/resized.

  ProcMeterWidget w THe Widget to resize.
  ++++++++++++++++++++++++++++++++++++++*/

static void SizeMeter(ProcMeterWidget w)
{
 /* The label parts. */

 if(w->procmeter.label_pos)
   {
    w->procmeter.label_height=w->procmeter.label_font->ascent+w->procmeter.label_font->descent+4;
    w->procmeter.label_x=(w->core.width-XTextWidth(w->procmeter.label_font,w->procmeter.label,(int)strlen(w->procmeter.label)))/2;
    if(w->procmeter.label_pos==ProcMeterLabelTop)
       w->procmeter.label_y=w->procmeter.label_height-3;
    else
       w->procmeter.label_y=w->core.height-2;
   }
 else
   {
    w->procmeter.label_height=0;
    w->procmeter.label_x=0;
    w->procmeter.label_y=0;
   }

 /* The grid parts. */

 w->procmeter.grid_height=w->core.height-w->procmeter.label_height;

 w->procmeter.grid_max=w->procmeter.grid_height/3;

 if(w->procmeter.label_pos==ProcMeterLabelTop)
    w->procmeter.grid_start=w->procmeter.label_height;
 else
    w->procmeter.grid_start=0;

 if(w->procmeter.grid_num>w->procmeter.grid_max && w->procmeter.grid_drawn)
    w->procmeter.grid_drawn=-1;
 if(w->procmeter.grid_num<=w->procmeter.grid_max && w->procmeter.grid_drawn)
    w->procmeter.grid_drawn=1;
}


/*++++++++++++++++++++++++++++++++++++++
  Update the display.

  ProcMeterWidget w The Widget to update.

  Boolean scroll If true then scroll the window else just draw.
  ++++++++++++++++++++++++++++++++++++++*/

static void Update(ProcMeterWidget w,Boolean scroll)
{
 if(w->core.visible)
   {
    int i;
    int scale=ProcMeterWidgetScale*w->procmeter.grid_num;
    unsigned short val;
    Position pos;

    if(scroll)
      {
       val=w->procmeter.data[(w->procmeter.data_num-1+w->procmeter.data_index)%w->procmeter.data_num];
       pos=val*w->procmeter.grid_height/scale;

       XCopyArea(XtDisplay(w),XtWindow(w),XtWindow(w),w->procmeter.foreground_gc,
                 1,w->procmeter.grid_start,(unsigned)(w->core.width-1),w->procmeter.grid_height,
                 0,w->procmeter.grid_start);

       XClearArea(XtDisplay(w),XtWindow(w),
                  w->core.width-1,w->procmeter.grid_start,1,w->procmeter.grid_height,
                  False);

       if(w->procmeter.line_solid)
          XDrawLine(XtDisplay(w),XtWindow(w),w->procmeter.foreground_gc,
                    (signed)(w->procmeter.data_num-1),w->procmeter.grid_height+w->procmeter.grid_start,
                    (signed)(w->procmeter.data_num-1),w->procmeter.grid_height+w->procmeter.grid_start-pos);
       else
         {
          unsigned short oldval=w->procmeter.data[(w->procmeter.data_num-2+w->procmeter.data_index)%w->procmeter.data_num];
          Position oldpos=oldval*w->procmeter.grid_height/scale;

          XDrawLine(XtDisplay(w),XtWindow(w),w->procmeter.foreground_gc,
                    (signed)(w->procmeter.data_num-1),w->procmeter.grid_height+w->procmeter.grid_start-oldpos,
                    (signed)(w->procmeter.data_num-1),w->procmeter.grid_height+w->procmeter.grid_start-pos);
         }

       if(w->procmeter.grid_drawn==1)
          for(i=1;i<w->procmeter.grid_num;i++)
            {
             pos=i*w->procmeter.grid_height/w->procmeter.grid_num;
             XDrawPoint(XtDisplay(w),XtWindow(w),w->procmeter.grid_gc,
                        w->core.width-1,w->procmeter.grid_height+w->procmeter.grid_start-pos);
            }
       else
          if(w->procmeter.grid_drawn==-1)
            {
             pos=w->procmeter.grid_max*w->procmeter.grid_height/w->procmeter.grid_num;
             XDrawPoint(XtDisplay(w),XtWindow(w),w->procmeter.grid_gc,
                        w->core.width-1,w->procmeter.grid_height+w->procmeter.grid_start-pos);
            }
      }
    else
      {
       XClearWindow(XtDisplay(w),XtWindow(w));

       for(i=0;i<w->procmeter.data_num;i++)
         {
          val=w->procmeter.data[(i+w->procmeter.data_index)%w->procmeter.data_num];
          pos=val*w->procmeter.grid_height/scale;

          if(w->procmeter.line_solid)
             XDrawLine(XtDisplay(w),XtWindow(w),w->procmeter.foreground_gc,
                       i,w->procmeter.grid_height+w->procmeter.grid_start,
                       i,w->procmeter.grid_height+w->procmeter.grid_start-pos);
          else if(i)
            {
             unsigned short oldval=w->procmeter.data[(i-1+w->procmeter.data_index)%w->procmeter.data_num];
             Position oldpos=oldval*w->procmeter.grid_height/scale;

             XDrawLine(XtDisplay(w),XtWindow(w),w->procmeter.foreground_gc,
                       i,w->procmeter.grid_height+w->procmeter.grid_start-oldpos,
                       i,w->procmeter.grid_height+w->procmeter.grid_start-pos);
            }
         }

       if(w->procmeter.grid_drawn==1)
          for(i=1;i<w->procmeter.grid_num;i++)
            {
             pos=i*w->procmeter.grid_height/w->procmeter.grid_num;
             XDrawLine(XtDisplay(w),XtWindow(w),w->procmeter.grid_gc,
                       0            ,w->procmeter.grid_height+w->procmeter.grid_start-pos,
                       w->core.width,w->procmeter.grid_height+w->procmeter.grid_start-pos);
            }
       else
          if(w->procmeter.grid_drawn==-1)
            {
             pos=w->procmeter.grid_max*w->procmeter.grid_height/w->procmeter.grid_num;
             XDrawLine(XtDisplay(w),XtWindow(w),w->procmeter.grid_gc,
                       0            ,w->procmeter.grid_height+w->procmeter.grid_start-pos,
                       w->core.width,w->procmeter.grid_height+w->procmeter.grid_start-pos);
            }

       if(w->procmeter.label_pos)
         {
          XDrawString(XtDisplay(w),XtWindow(w),w->procmeter.foreground_gc,
                      w->procmeter.label_x,w->procmeter.label_y,
                      w->procmeter.label,(int)strlen(w->procmeter.label));

          if(w->procmeter.label_pos==ProcMeterLabelTop)
             XDrawLine(XtDisplay(w),XtWindow(w),w->procmeter.foreground_gc,
                       0            ,w->procmeter.label_height-1,
                       w->core.width,w->procmeter.label_height-1);
          else
             XDrawLine(XtDisplay(w),XtWindow(w),w->procmeter.foreground_gc,
                       0            ,w->procmeter.grid_height,
                       w->core.width,w->procmeter.grid_height);
         }
      }
   }
}


/*++++++++++++++++++++++++++++++++++++++
  Add a data point to the ProcMeter Widget.

  Widget pmw The ProcMeter Widget.

  unsigned short datum The data point to add.
  ++++++++++++++++++++++++++++++++++++++*/

void ProcMeterWidgetAddDatum(Widget pmw,unsigned short datum)
{
 ProcMeterWidget w=(ProcMeterWidget)pmw;
 unsigned short old_datum,new_data_max=w->procmeter.data_max;
 int i;

 old_datum=w->procmeter.data[w->procmeter.data_index];
 w->procmeter.data[w->procmeter.data_index]=datum;

 w->procmeter.data_index=(w->procmeter.data_index+1)%w->procmeter.data_num;

 if(datum>new_data_max)
    new_data_max=datum;
 else
    if(old_datum==new_data_max)
       for(i=new_data_max=0;i<w->procmeter.data_num;i++)
          if(w->procmeter.data[i]>new_data_max)
             new_data_max=w->procmeter.data[i];

 if(new_data_max!=w->procmeter.data_max)
   {
    int new_grid_num=(new_data_max+ProcMeterWidgetScale-1)/ProcMeterWidgetScale;

    if(new_grid_num<w->procmeter.grid_min)
       new_grid_num=w->procmeter.grid_min;

    w->procmeter.data_max=new_data_max;

    if(new_grid_num!=w->procmeter.grid_num)
      {
       w->procmeter.grid_num=new_grid_num;

       if(w->procmeter.grid_num>w->procmeter.grid_max && w->procmeter.grid_drawn)
          w->procmeter.grid_drawn=-1;
       if(w->procmeter.grid_num<=w->procmeter.grid_max && w->procmeter.grid_drawn)
          w->procmeter.grid_drawn=1;

       Update(w,False);
      }
   }

 Update(w,True);
}
