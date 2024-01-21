

/************************************************************************* 
 * Version 1.3  on  20.2.1996
 * (c) 1996 Pralay Kanti Dakua
 *     pralay@teil.soft.net 
 *     
 * Credits
 * -------
 * The keyboard translations are added by Andreas Zeller (zeller@comsoft.de).
 * -------
 *
 * This program is free software; permission hereby granted, free of
 * charge, to any person obtaining a copy of this software and 
 * associated documentations to copy, modify and  distribute it under 
 * the terms of the GNU General Public License as published by 
 * the Free Software Foundation, subject to the following conditions: 

 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.

 * AS THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
 * FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN
 * OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
 * PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED
 * OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS
 * TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU,  SHOULD THE
 * PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,
 * REPAIR OR CORRECTION.
 *
 **************************************************************************/




#include <stdio.h>
#include <string.h>
#include <Xm/XmP.h>
#include <X11/StringDefs.h>
#include "DSScaleP.h"

/****** Widget Methods *************/

static void Initialize(Widget,Widget,ArgList,Cardinal *);
static void ReDisplay(Widget,XExposeEvent *,Region );
static void Destroy(Widget);
static void Resize(Widget);
static Boolean SetValues(Widget,Widget, Widget,ArgList,Cardinal *);
static XtGeometryResult GeometryManager(Widget, XtWidgetGeometry*, XtWidgetGeometry*);


/******* Internal Functions *************/

static void redraw_all(XmDoubleSliderScaleWidget);
static void set_initial_size(XmDoubleSliderScaleWidget,XmDoubleSliderScaleWidget,ArgList,Cardinal *);
static void set_hor_init_size(XmDoubleSliderScaleWidget,ArgList,Cardinal *);
static void set_ver_init_size(XmDoubleSliderScaleWidget,ArgList,Cardinal *);
static void create_children(XmDoubleSliderScaleWidget);
static void create_trough_pixmap(XmDoubleSliderScaleWidget);
static void create_slider_pixmap(XmDoubleSliderScaleWidget);
static void create_GC(XmDoubleSliderScaleWidget);
static void draw_slot(XmDoubleSliderScaleWidget);
static void draw_hor_lower_slider_motion(Widget, XEvent *);
static void draw_ver_lower_slider_motion(Widget, XEvent *);
static void draw_hor_upper_slider_motion(Widget, XEvent *);
static void draw_ver_upper_slider_motion(Widget, XEvent *);
static void draw_slider_shadow(XmDoubleSliderScaleWidget);
static void show_upper_value(XmDoubleSliderScaleWidget); 
static void show_lower_value(XmDoubleSliderScaleWidget); 
static void erase_prev_upper_value(XmDoubleSliderScaleWidget); 
static void erase_prev_lower_value(XmDoubleSliderScaleWidget); 
static void calculate_hor_slider_positions(XmDoubleSliderScaleWidget);
static void calculate_ver_slider_positions(XmDoubleSliderScaleWidget);
static void set_scale_values(XmDoubleSliderScaleWidget,XmDoubleSliderScaleWidget,ArgList,Cardinal *);
static void set_new_hor_orientation(XmDoubleSliderScaleWidget,XmDoubleSliderScaleWidget);
static void set_new_ver_orientation(XmDoubleSliderScaleWidget,XmDoubleSliderScaleWidget);
static Position get_ver_trough_x(XmDoubleSliderScaleWidget);
static Position get_hor_trough_y(XmDoubleSliderScaleWidget);
static Boolean if_set(ArgList,Cardinal *,char *);
static void configure_trough(XmDoubleSliderScaleWidget, ArgList, Cardinal *);
static void change_background_GC(XmDoubleSliderScaleWidget);
static void change_GCfonts(XmDoubleSliderScaleWidget);
static void erase_hor_lower_slider(XmDoubleSliderScaleWidget,int);
static void erase_ver_lower_slider(XmDoubleSliderScaleWidget,int);
static void erase_hor_upper_slider(XmDoubleSliderScaleWidget,int);
static void erase_ver_upper_slider(XmDoubleSliderScaleWidget,int);
static void draw_hor_lower_slider(XmDoubleSliderScaleWidget);
static void draw_ver_lower_slider(XmDoubleSliderScaleWidget);
static void draw_hor_upper_slider(XmDoubleSliderScaleWidget);
static void draw_ver_upper_slider(XmDoubleSliderScaleWidget);


#define offset(field) XtOffsetOf(XmDoubleSliderScaleRec, field)

#define ACTIONPROC(proc)  static void proc(Widget,XEvent*,String*,Cardinal*)
ACTIONPROC(Btn1DownEvent);
ACTIONPROC(Btn1MotionEvent);
ACTIONPROC(Btn1UpEvent);
ACTIONPROC(ProcessKDown);
ACTIONPROC(ProcessKRight);
ACTIONPROC(ProcessKUp);
ACTIONPROC(ProcessKLeft);
ACTIONPROC(ProcessKShiftDown);
ACTIONPROC(ProcessKShiftRight);
ACTIONPROC(ProcessKShiftUp);
ACTIONPROC(ProcessKShiftLeft);

static XtResource resources[]={
{
XmNlowerDragCallback,
XmCLowerDragCallback,
XtRCallback,
sizeof(XtPointer),
offset(dsscale.lower_drag_callback),
XtRCallback,
NULL
},
{
XmNupperDragCallback,
XmCUpperDragCallback,
XtRCallback,
sizeof(XtPointer),
offset(dsscale.upper_drag_callback),
XtRCallback,
NULL
},
{
XmNlowerValueChangedCallback,
XmCLowerValueChangedCallback,
XtRCallback,
sizeof(XtPointer),
offset(dsscale.lower_value_changed_callback),
XtRCallback,
NULL
},
{
XmNupperValueChangedCallback,
XmCUpperValueChangedCallback,
XtRCallback,
sizeof(XtPointer),
offset(dsscale.upper_value_changed_callback),
XtRCallback,
NULL
},
{
XmNminimum,
XmCMinimum,
XmRInt,
sizeof(int),
offset(dsscale.minimum_value),
XmRString,
"0",
},
{
XmNmaximum,
XmCMaximum,
XmRInt,
sizeof(int),
offset(dsscale.maximum_value),
XmRString,
"100",
},
{
XmNlowerValue,
XmCLowerValue,
XmRInt,
sizeof(int),
offset(dsscale.lower_value),
XmRString,
"0",
},
{
XmNupperValue,
XmCUpperValue,
XmRInt,
sizeof(int),
offset(dsscale.upper_value),
XmRString,
"100",
},
{
XmNorientation,
XmCOrientation,
XmROrientation,
sizeof(int),
offset(dsscale.orientation),
XmRImmediate,
(XtPointer)XmHORIZONTAL
},
{
XmNprocessingDirection,
XmCProcessingDirection,
XmRProcessingDirection,
sizeof(int),
offset(dsscale.processing_direction),
XmRImmediate,
(XtPointer)XmMAX_ON_RIGHT
},
{
XmNfontList,
XmCFontList,
XmRFontList,
sizeof(XmFontList),
offset(dsscale.font_list),
XmRString,
"Fixed",
},
{
XmNshowValues,
XmCShowValues,
XmRBoolean,
sizeof(Boolean),
offset(dsscale.show_values),
XmRImmediate,
(XtPointer)True,
},
{
XmNscaleAlignment,
XmCScaleAlignment,
XmRScaleAlignment,
sizeof(int),
offset(dsscale.scale_alignment),
XmRImmediate,
(XtPointer)XmALIGNMENT_END,
},
{
XmNscaleWidth,
XmCScaleWidth,
XmRDimension,
sizeof(Dimension),
offset(dsscale.scale_width),
XmRString,
"0",
},
{
XmNscaleHeight,
XmCScaleHeight,
XmRDimension,
sizeof(Dimension),
offset(dsscale.scale_height),
XmRString,
"0",
}
};


typedef struct _DoubleSliderScaleConstraintPart{
int i;
}DoubleSliderScaleConstraintPart;

typedef struct _DoubleSliderScaleConstraintRec{
DoubleSliderScaleConstraintPart dsscale;
}DoubleSliderScaleConstraintRec;


XmDoubleSliderScaleClassRec xmDoubleSliderScaleClassRec={
{ /***** core class ******/
(WidgetClass) &xmManagerClassRec, 		/* super class */
"XmDoubleSliderScale",				/* class name */
sizeof(XmDoubleSliderScaleRec),			/* widget size */
NULL,    					/* class initialize */
NULL, 						/* class part initialize */
False,						/* class inited */
(XtInitProc)Initialize,				/* initialize */
NULL,						/* initialize hook */
XtInheritRealize,				/* realize */
NULL,						/* actions */
0,						/* num actions */
resources, 					/*  resources */
XtNumber(resources),   				/* num resources */
NULLQUARK,					/* xrm class */
True,						/* compress motion */
XtExposeCompressMultiple,			/* compress exposure */
True,						/* compress enter leave */
False,						/* visible interest */
(XtWidgetProc)Destroy,				/* destroy */
(XtWidgetProc)Resize,				/* resize */
(XtExposeProc)ReDisplay,			/* expose */
(XtSetValuesFunc)SetValues,			/* set values */
NULL,						/* set values hook */
XtInheritSetValuesAlmost,			/* set values almost */
NULL,						/* get values hook */
XtInheritAcceptFocus,				/* accept focus */
XtVersion,					/* version */
NULL,						/* callback private */
XtInheritTranslations,				/* tm table */
XtInheritQueryGeometry,				/* query geometry */
XtInheritDisplayAccelerator, 			/* display accelerator */
NULL						/* extension */
},
{ /***** composite class part *****/
GeometryManager, 		/* XtInheritGeometryManager, geomerty manager */
XtInheritChangeManaged,				/* change managed */
XtInheritInsertChild,				/* insert child */
XtInheritDeleteChild,				/* delete child */
NULL						/* extension */
},
{ /**** Constraint Class Part *****/
NULL,       					/* constraint resource list     */
0,         					/* number of constraints in list */
0,						/* size of constraint record     */
NULL,						/* constraint initialization   */
NULL,						/* constraint destroy proc     */
NULL,						/* constraint set_values proc   */
NULL						/* pointer to extension record  */
},
{ /****** Manager Class Part *****/
XtInheritTranslations,				/* translations */
NULL,						/* syn_resources  */
0,						/* num_syn_resources */
NULL,						/* syn_constraint_resources */
0,						/* num_syn_constraint_resources */
(XmParentProcessProc)NULL,			/* parent_process */
NULL						/* extension */
},
{ /****** DoubleSliderScale class part ****/
0,   						/* extension */
},
};

WidgetClass xmDoubleSliderScaleWidgetClass=(WidgetClass) &xmDoubleSliderScaleClassRec;

static XtActionsRec actions[]={
{"Btn1DownEvent",Btn1DownEvent},
{"Btn1MotionEvent",Btn1MotionEvent},
{"ProcessKDown",ProcessKDown},
{"ProcessKRight",ProcessKRight},
{"ProcessKUp",ProcessKUp},
{"ProcessKLeft",ProcessKLeft},
{"ProcessKShiftDown",ProcessKShiftDown},
{"ProcessKShiftRight",ProcessKShiftRight},
{"ProcessKShiftUp",ProcessKShiftUp},
{"ProcessKShiftLeft",ProcessKShiftLeft},
{"Btn1UpEvent",Btn1UpEvent}
};

static char translations[]=
"<Btn1Down>: Btn1DownEvent()    \n\
<Btn1Motion>: Btn1MotionEvent()    \n\
~Shift<Key>osfDown: ProcessKDown()    \n\
~Shift<Key>osfRight: ProcessKRight()    \n\
~Shift<Key>osfUp: ProcessKUp()    \n\
~Shift<Key>osfLeft: ProcessKLeft()    \n\
Shift<Key>osfDown: ProcessKShiftDown()    \n\
Shift<Key>osfRight: ProcessKShiftRight()    \n\
Shift<Key>osfUp: ProcessKShiftUp()    \n\
Shift<Key>osfLeft: ProcessKShiftLeft()    \n\
<Btn1Up>: Btn1UpEvent() \n";

/***************************/

/*** ------------------------------------------------------
     Key Pressed Events
     ----------------------------------------------------***/

static void ProcessKDown(Widget w,XEvent *event,String *params,Cardinal *nparams)
{
XmDoubleSliderScaleWidget wi=(XmDoubleSliderScaleWidget)w->core.parent;
XmDoubleSliderScaleCallbackStruct call_data;

if(wi->dsscale.orientation == XmHORIZONTAL) return;

  switch(wi->dsscale.processing_direction)
  {
   case XmMAX_ON_TOP:
    if (wi->dsscale.lower_value > wi->dsscale.minimum_value)
    {

       XmDoubleSliderScaleSetLowerValue((Widget)wi, wi->dsscale.lower_value -1);

       call_data.reason = XmCR_LOWER_VALUE_CHANGED;
       call_data.event = event;
       call_data.value = wi->dsscale.lower_value;
       XtCallCallbacks((Widget)wi,XmNlowerValueChangedCallback,&call_data);
    }
   break;

   case XmMAX_ON_BOTTOM:
      if (wi->dsscale.lower_value < wi->dsscale.upper_value)
    {
       XmDoubleSliderScaleSetLowerValue((Widget)wi, wi->dsscale.lower_value + 1);

       call_data.reason = XmCR_LOWER_VALUE_CHANGED;
       call_data.event = event;
       call_data.value = wi->dsscale.lower_value;
       XtCallCallbacks((Widget)wi,XmNlowerValueChangedCallback,&call_data);
    }
   break;
  } /* switch */ 

}


static void ProcessKLeft(Widget w,XEvent *event,String *params,Cardinal *nparams)
{
XmDoubleSliderScaleWidget wi=(XmDoubleSliderScaleWidget)w->core.parent;
XmDoubleSliderScaleCallbackStruct call_data;

if(wi->dsscale.orientation == XmVERTICAL) return;

  switch(wi->dsscale.processing_direction)
  {
   case XmMAX_ON_LEFT:

    if (wi->dsscale.lower_value < wi->dsscale.upper_value)
    {
       XmDoubleSliderScaleSetLowerValue((Widget)wi, wi->dsscale.lower_value + 1);

       call_data.reason = XmCR_LOWER_VALUE_CHANGED;
       call_data.event = event;
       call_data.value = wi->dsscale.lower_value;
       XtCallCallbacks((Widget)wi,XmNlowerValueChangedCallback,&call_data);
    }
   break;

      case XmMAX_ON_RIGHT:
    if (wi->dsscale.lower_value > wi->dsscale.minimum_value)
    {
       XmDoubleSliderScaleSetLowerValue((Widget)wi, wi->dsscale.lower_value -1);

       call_data.reason = XmCR_LOWER_VALUE_CHANGED;
       call_data.event = event;
       call_data.value = wi->dsscale.lower_value;
       XtCallCallbacks((Widget)wi,XmNlowerValueChangedCallback,&call_data);
    }
   break;
  } /* switch */

}



static void ProcessKUp(Widget w,XEvent *event,String *params,Cardinal *nparams)
{
XmDoubleSliderScaleWidget wi=(XmDoubleSliderScaleWidget)w->core.parent;
XmDoubleSliderScaleCallbackStruct call_data;

if(wi->dsscale.orientation == XmHORIZONTAL) return;

  switch(wi->dsscale.processing_direction)
  {
   case XmMAX_ON_TOP:

    if (wi->dsscale.lower_value < wi->dsscale.upper_value)
    {
       XmDoubleSliderScaleSetLowerValue((Widget)wi, wi->dsscale.lower_value + 1);

       call_data.reason = XmCR_LOWER_VALUE_CHANGED;
       call_data.event = event;
       call_data.value = wi->dsscale.lower_value;
       XtCallCallbacks((Widget)wi,XmNlowerValueChangedCallback,&call_data);
    }
   break;

      case XmMAX_ON_BOTTOM:
    if (wi->dsscale.lower_value > wi->dsscale.minimum_value)
    {
       XmDoubleSliderScaleSetLowerValue((Widget)wi, wi->dsscale.lower_value -1);

       call_data.reason = XmCR_LOWER_VALUE_CHANGED;
       call_data.event = event;
       call_data.value = wi->dsscale.lower_value;
       XtCallCallbacks((Widget)wi,XmNlowerValueChangedCallback,&call_data);
    }
   break;
  } /* switch */
}



static void ProcessKRight(Widget w,XEvent *event,String *params,Cardinal *nparams)
{
XmDoubleSliderScaleWidget wi=(XmDoubleSliderScaleWidget)w->core.parent;
XmDoubleSliderScaleCallbackStruct call_data;

if(wi->dsscale.orientation == XmVERTICAL) return;

  switch(wi->dsscale.processing_direction)
  {
   case XmMAX_ON_LEFT:
    if (wi->dsscale.lower_value > wi->dsscale.minimum_value)
    {
       XmDoubleSliderScaleSetLowerValue((Widget)wi, wi->dsscale.lower_value -1);

       call_data.reason = XmCR_LOWER_VALUE_CHANGED;
       call_data.event = event;
       call_data.value = wi->dsscale.lower_value;
       XtCallCallbacks((Widget)wi,XmNlowerValueChangedCallback,&call_data);
    }
   break;

   case XmMAX_ON_RIGHT:
      if (wi->dsscale.lower_value < wi->dsscale.upper_value)
    {
       XmDoubleSliderScaleSetLowerValue((Widget)wi, wi->dsscale.lower_value + 1);

       call_data.reason = XmCR_LOWER_VALUE_CHANGED;
       call_data.event = event;
       call_data.value = wi->dsscale.lower_value;
       XtCallCallbacks((Widget)wi,XmNlowerValueChangedCallback,&call_data);
    }
   break;
  } /* switch */
}



static void ProcessKShiftUp(Widget w,XEvent *event,String *params,Cardinal *nparams)
{
XmDoubleSliderScaleWidget wi=(XmDoubleSliderScaleWidget)w->core.parent;
XmDoubleSliderScaleCallbackStruct call_data;

 if(wi->dsscale.orientation == XmHORIZONTAL) return;

switch(wi->dsscale.processing_direction)
  {
   case XmMAX_ON_TOP:

    if (wi->dsscale.upper_value < wi->dsscale.maximum_value)
    {
       XmDoubleSliderScaleSetUpperValue((Widget)wi, wi->dsscale.upper_value + 1);

       call_data.reason = XmCR_UPPER_VALUE_CHANGED;
       call_data.event = event;
       call_data.value = wi->dsscale.upper_value;
       XtCallCallbacks((Widget)wi,XmNupperValueChangedCallback,&call_data);
   }
  break;

  case XmMAX_ON_BOTTOM:
    if (wi->dsscale.upper_value > wi->dsscale.lower_value)
    {
       XmDoubleSliderScaleSetUpperValue((Widget)wi, wi->dsscale.upper_value - 1);

       call_data.reason = XmCR_UPPER_VALUE_CHANGED;
       call_data.event = event;
       call_data.value = wi->dsscale.upper_value;
       XtCallCallbacks((Widget)wi,XmNupperValueChangedCallback,&call_data);
   }
  break;
 } /* switch */
}

static void ProcessKShiftRight(Widget w,XEvent *event,String *params,Cardinal *nparams)
{
XmDoubleSliderScaleWidget wi=(XmDoubleSliderScaleWidget)w->core.parent;
XmDoubleSliderScaleCallbackStruct call_data;

 if(wi->dsscale.orientation == XmVERTICAL) return;

switch(wi->dsscale.processing_direction)
  {
   case XmMAX_ON_RIGHT:

    if (wi->dsscale.upper_value < wi->dsscale.maximum_value)
    {
       XmDoubleSliderScaleSetUpperValue((Widget)wi, wi->dsscale.upper_value + 1);

       call_data.reason = XmCR_UPPER_VALUE_CHANGED;
       call_data.event = event;
       call_data.value = wi->dsscale.upper_value;
       XtCallCallbacks((Widget)wi,XmNupperValueChangedCallback,&call_data);
   }
  break;

  case XmMAX_ON_LEFT:
    if (wi->dsscale.upper_value > wi->dsscale.lower_value)
    {
       XmDoubleSliderScaleSetUpperValue((Widget)wi, wi->dsscale.upper_value - 1);

       call_data.reason = XmCR_UPPER_VALUE_CHANGED;
       call_data.event = event;
       call_data.value = wi->dsscale.upper_value;
       XtCallCallbacks((Widget)wi,XmNupperValueChangedCallback,&call_data);
   }
  break;
 } /* switch */
}


static void ProcessKShiftDown(Widget w,XEvent *event,String *params,Cardinal *nparams)
{
XmDoubleSliderScaleWidget wi=(XmDoubleSliderScaleWidget)w->core.parent;
XmDoubleSliderScaleCallbackStruct call_data;

 if(wi->dsscale.orientation == XmHORIZONTAL) return;

switch(wi->dsscale.processing_direction)
  {
   case XmMAX_ON_TOP:

    if (wi->dsscale.upper_value > wi->dsscale.lower_value)
    {
       XmDoubleSliderScaleSetUpperValue((Widget)wi, wi->dsscale.upper_value - 1);

       call_data.reason = XmCR_UPPER_VALUE_CHANGED;
       call_data.event = event;
       call_data.value = wi->dsscale.upper_value;
       XtCallCallbacks((Widget)wi,XmNupperValueChangedCallback,&call_data);
   }
   break;
   
   case XmMAX_ON_BOTTOM:

    if (wi->dsscale.upper_value < wi->dsscale.maximum_value)
    {
       XmDoubleSliderScaleSetUpperValue((Widget)wi, wi->dsscale.upper_value + 1);

       call_data.reason = XmCR_UPPER_VALUE_CHANGED;
       call_data.event = event;
       call_data.value = wi->dsscale.upper_value;
       XtCallCallbacks((Widget)wi,XmNupperValueChangedCallback,&call_data);
   }
  break;
  } /* siwtch */
}


static void ProcessKShiftLeft(Widget w,XEvent *event,String *params,Cardinal *nparams)
{
XmDoubleSliderScaleWidget wi=(XmDoubleSliderScaleWidget)w->core.parent;
XmDoubleSliderScaleCallbackStruct call_data;

 if(wi->dsscale.orientation == XmVERTICAL) return;

switch(wi->dsscale.processing_direction)
  {
   case XmMAX_ON_RIGHT:

    if (wi->dsscale.upper_value > wi->dsscale.lower_value)
    {
       XmDoubleSliderScaleSetUpperValue((Widget)wi, wi->dsscale.upper_value - 1);

       call_data.reason = XmCR_UPPER_VALUE_CHANGED;
       call_data.event = event;
       call_data.value = wi->dsscale.upper_value;
       XtCallCallbacks((Widget)wi,XmNupperValueChangedCallback,&call_data);
   }
   break;
  
   case XmMAX_ON_LEFT:

    if (wi->dsscale.upper_value < wi->dsscale.maximum_value)
    {
       XmDoubleSliderScaleSetUpperValue((Widget)wi, wi->dsscale.upper_value + 1);

       call_data.reason = XmCR_UPPER_VALUE_CHANGED;
       call_data.event = event;
       call_data.value = wi->dsscale.upper_value;
       XtCallCallbacks((Widget)wi,XmNupperValueChangedCallback,&call_data);
   }
  break;
  } /* siwtch */
}



/*** ------------------------------------------------------
     Button Motion Event
     ----------------------------------------------------***/

static void Btn1MotionEvent(Widget w,XEvent *event,String *params,Cardinal *nparams)
{
XmDoubleSliderScaleWidget wi=(XmDoubleSliderScaleWidget)w->core.parent;
XmDoubleSliderScaleCallbackStruct call_data;

/**** --------------------------------------------
       Move lower slider and change the values only if the 
       the lower_slider_on is True. then call the XmNlowerDragCallback 
        procs
       ------------------------------------------****/

if(wi->dsscale.lower_slider_on)
	{
	if(wi->dsscale.show_values) erase_prev_lower_value(wi);

	if(wi->dsscale.orientation==XmHORIZONTAL) 
			draw_hor_lower_slider_motion(w,event);
	else if(wi->dsscale.orientation==XmVERTICAL) 
			draw_ver_lower_slider_motion(w,event);

	if(wi->dsscale.show_values) show_lower_value(wi);

	call_data.reason = XmCR_LOWER_DRAG;
	call_data.event = event;
	call_data.value = wi->dsscale.lower_value;
	XtCallCallbacks(XtParent(w),XmNlowerDragCallback,&call_data);
	}
/**** --------------------------------------------
       Move upper slider and change the values only if the 
       the upper_slider_on is True. then call the XmNupperDragCallback
       procs.
       ------------------------------------------****/

else if(wi->dsscale.upper_slider_on)
	{

	if(wi->dsscale.show_values) erase_prev_upper_value(wi);

	if(wi->dsscale.orientation==XmHORIZONTAL) draw_hor_upper_slider_motion(w,event);
	else if(wi->dsscale.orientation==XmVERTICAL) draw_ver_upper_slider_motion(w,event);

	if(wi->dsscale.show_values) show_upper_value(wi);

	call_data.reason = XmCR_UPPER_DRAG;
	call_data.event = event;
	call_data.value = wi->dsscale.upper_value;
	XtCallCallbacks(XtParent(w),XmNupperDragCallback,&call_data);
	}

}

/**********************************/

static void Btn1DownEvent(Widget w,XEvent *event,String *params,Cardinal *nparams)
{
XmDoubleSliderScaleWidget wi=(XmDoubleSliderScaleWidget)w->core.parent;
int x,y;

x=event->xbutton.x;
y=event->xbutton.y;

wi->dsscale.lower_slider_on=False;
wi->dsscale.upper_slider_on=False;

switch(wi->dsscale.orientation)
{
case XmHORIZONTAL:
	if(x > wi->dsscale.lower_x  && x < (wi->dsscale.lower_x + wi->dsscale.slider_size_x))
		{
		wi->dsscale.offset_x=x - wi->dsscale.lower_x ;
		wi->dsscale.lower_slider_on=True;
		}
	else if(x > wi->dsscale.upper_x && x < (wi->dsscale.upper_x + wi->dsscale.slider_size_x))
		{
		wi->dsscale.offset_x=x - wi->dsscale.upper_x ;
		wi->dsscale.upper_slider_on=True;
		}
break;

case XmVERTICAL:
	if(y > wi->dsscale.lower_y   && y < (wi->dsscale.lower_y + wi->dsscale.slider_size_y))
		{
		wi->dsscale.offset_y= y - wi->dsscale.lower_y;
		wi->dsscale.lower_slider_on=True;
		}

	if(y > wi->dsscale.upper_y   && y < (wi->dsscale.upper_y + wi->dsscale.slider_size_y))
		{
		wi->dsscale.offset_y= y - wi->dsscale.upper_y;
		wi->dsscale.upper_slider_on=True;
		}
break;
} /* switch */

}

/*******************************/

static void Btn1UpEvent(Widget w,XEvent *event,String *params,Cardinal *nparams)
{
XmDoubleSliderScaleWidget wi=(XmDoubleSliderScaleWidget)w->core.parent;
XmDoubleSliderScaleCallbackStruct call_data;

if(wi->dsscale.lower_slider_on)
	{
	if(wi->dsscale.orientation==XmHORIZONTAL)
			draw_hor_lower_slider_motion(w,event);
	else if(wi->dsscale.orientation==XmVERTICAL)
			draw_ver_lower_slider_motion(w,event);

	call_data.reason = XmCR_LOWER_VALUE_CHANGED;
	call_data.event = event;
	call_data.value = wi->dsscale.lower_value;
	XtCallCallbacks(XtParent(w),XmNlowerValueChangedCallback,&call_data);
	}
if(wi->dsscale.upper_slider_on)
	{
	if(wi->dsscale.orientation==XmHORIZONTAL)
			draw_hor_upper_slider_motion(w,event);
	else if(wi->dsscale.orientation==XmVERTICAL)
			draw_ver_upper_slider_motion(w,event);

	call_data.reason = XmCR_UPPER_VALUE_CHANGED;
	call_data.event = event;
	call_data.value = wi->dsscale.upper_value;
	XtCallCallbacks(XtParent(w),XmNupperValueChangedCallback,&call_data);
	}

wi->dsscale.lower_slider_on=False;
wi->dsscale.upper_slider_on=False;
}

/*****************************************/

static void draw_hor_lower_slider_motion(Widget w, XEvent *event)
{
XmDoubleSliderScaleWidget parent=(XmDoubleSliderScaleWidget)w->core.parent;
int x,y;
int lo_limit = 0,up_limit = 0;
float deno, nomi = 0;
Boolean recal=False;


x=event->xmotion.x;
y=event->xmotion.y;

if(parent->dsscale.processing_direction==XmMAX_ON_RIGHT)
	{
	up_limit = parent->dsscale.upper_x - (parent->dsscale.slider_size_x - parent->dsscale.offset_x);
	lo_limit=  parent->dsscale.offset_x;
	}
else if(parent->dsscale.processing_direction==XmMAX_ON_LEFT)
	{
	up_limit= w->core.width - (parent->dsscale.slider_size_x - parent->dsscale.offset_x);
	lo_limit = parent->dsscale.upper_x + parent->dsscale.slider_size_x + parent->dsscale.offset_x;
	}

if(x<=up_limit && x>=lo_limit)
	{
	XCopyArea(XtDisplay(w),
			parent->dsscale.slider,XtWindow(parent->composite.children[0]),
			parent->manager.background_GC,
			0,0,
			parent->dsscale.slider_size_x,parent->dsscale.slider_size_y,
			x - parent->dsscale.offset_x,0);


	if((x - parent->dsscale.offset_x )> parent->dsscale.lower_x)
		{
		XCopyArea(XtDisplay(w),
			parent->dsscale.pixmap,XtWindow(parent->composite.children[0]),
			parent->dsscale.foreground_GC,
			parent->dsscale.lower_x ,0,
			x - parent->dsscale.offset_x -parent->dsscale.lower_x , 
			parent->composite.children[0]->core.height,
			parent->dsscale.lower_x ,0);
		}

else if((x - parent->dsscale.offset_x ) < parent->dsscale.lower_x)
		{
		XCopyArea(XtDisplay(w),
			parent->dsscale.pixmap,XtWindow(parent->composite.children[0]),
			parent->dsscale.foreground_GC,
			x + parent->dsscale.slider_size_x - parent->dsscale.offset_x,0,
			parent->dsscale.lower_x  -x + parent->dsscale.offset_x, 
			parent->composite.children[0]->core.height,
			x + parent->dsscale.slider_size_x - parent->dsscale.offset_x,0);
		}

	parent->dsscale.lower_x=x - parent->dsscale.offset_x ;
	recal = True;
	} /* if */

else if(x > up_limit && (parent->dsscale.lower_x+ parent->dsscale.offset_x)<up_limit )
		{
		XCopyArea(XtDisplay(w),
			parent->dsscale.slider,XtWindow(parent->composite.children[0]),
	                parent->manager.background_GC,
                        0,0,
                        parent->dsscale.slider_size_x,parent->dsscale.slider_size_y,
                        up_limit  - parent->dsscale.offset_x,0);

		XCopyArea(XtDisplay(w),
			parent->dsscale.pixmap,XtWindow(parent->composite.children[0]),
                        parent->dsscale.foreground_GC,
                        parent->dsscale.lower_x ,0,
		        up_limit-parent->dsscale.offset_x -parent->dsscale.lower_x,
			parent->composite.children[0]->core.height,
	                parent->dsscale.lower_x ,0);

		parent->dsscale.lower_x=up_limit -parent->dsscale.offset_x  ;
		recal = True;
		}

else if(x < lo_limit && (parent->dsscale.lower_x+parent->dsscale.offset_x) > lo_limit)
	{
		XCopyArea(XtDisplay(w),
			parent->dsscale.slider,XtWindow(parent->composite.children[0]),
	                parent->manager.background_GC,
                        0,0,
                        parent->dsscale.slider_size_x,parent->dsscale.slider_size_y,
                        lo_limit  - parent->dsscale.offset_x,0);

		XCopyArea(XtDisplay(w),
			parent->dsscale.pixmap,XtWindow(parent->composite.children[0]),
                        parent->dsscale.foreground_GC,
                        lo_limit+ parent->dsscale.slider_size_x - parent->dsscale.offset_x,0,
		        parent->dsscale.lower_x  -x + parent->dsscale.offset_x, 
			parent->composite.children[0]->core.height,
                        lo_limit + parent->dsscale.slider_size_x - parent->dsscale.offset_x,0);

		parent->dsscale.lower_x=lo_limit - parent->dsscale.offset_x ;
		recal = True;
	}

if(recal)
	{
	deno = (float)(parent->composite.children[0]->core.width - 2*parent->dsscale.slider_size_x);

	if(parent->dsscale.processing_direction==XmMAX_ON_RIGHT)
		{
		 nomi = (float)((parent->dsscale.maximum_value 
				- parent->dsscale.minimum_value)*parent->dsscale.lower_x);
		}
	else if(parent->dsscale.processing_direction==XmMAX_ON_LEFT)
		{
		nomi = (float)((parent->dsscale.maximum_value - parent->dsscale.minimum_value)
			*(parent->composite.children[0]->core.width 
			- parent->dsscale.lower_x - parent->dsscale.slider_size_x));
		}
	parent->dsscale.lower_value = (int)(nomi/deno +0.5) + parent->dsscale.minimum_value;
	}

}
/*****************************************/
static void draw_ver_lower_slider_motion(Widget w, XEvent *event)
{
XmDoubleSliderScaleWidget parent=(XmDoubleSliderScaleWidget)w->core.parent;
int x,y;
int lo_limit = 0,up_limit = 0;
float deno,nomi = 0;
Boolean recal=False;


x=event->xmotion.x;
y=event->xmotion.y;

if(parent->dsscale.processing_direction==XmMAX_ON_TOP)
	{
	up_limit= w->core.height - (parent->dsscale.slider_size_y - parent->dsscale.offset_y);
	lo_limit = parent->dsscale.upper_y + parent->dsscale.slider_size_y + parent->dsscale.offset_y;
	}

else if(parent->dsscale.processing_direction==XmMAX_ON_BOTTOM)
        {
	up_limit = parent->dsscale.upper_y - (parent->dsscale.slider_size_y - parent->dsscale.offset_y);
        lo_limit=  parent->dsscale.offset_y ;
        }


if(y<=up_limit && y>=lo_limit)
	{
	/* ... copy slider at new position.... */
	XCopyArea(XtDisplay(w),
		parent->dsscale.slider,XtWindow(parent->composite.children[0]),
		parent->manager.background_GC,
		0,0,
		parent->dsscale.slider_size_x,parent->dsscale.slider_size_y,
		0,y - parent->dsscale.offset_y);

	/*** ... fill up .....***/
	if((y - parent->dsscale.offset_y )> parent->dsscale.lower_y)
		{
		XCopyArea(XtDisplay(w),
			parent->dsscale.pixmap,XtWindow(parent->composite.children[0]),
			parent->dsscale.foreground_GC,
			0,parent->dsscale.lower_y,
			parent->composite.children[0]->core.width,
			y - parent->dsscale.offset_y -parent->dsscale.lower_y ,
			0,parent->dsscale.lower_y );
		}

	else if((y - parent->dsscale.offset_y ) < parent->dsscale.lower_y)
		{
		XCopyArea(XtDisplay(w),
			parent->dsscale.pixmap,XtWindow(parent->composite.children[0]),
			parent->dsscale.foreground_GC,
			0,y + parent->dsscale.slider_size_y - parent->dsscale.offset_y,
			parent->composite.children[0]->core.width, 
			parent->dsscale.lower_y  -y + parent->dsscale.offset_y,
			0,y + parent->dsscale.slider_size_y - parent->dsscale.offset_y);
		}
	parent->dsscale.lower_y=y - parent->dsscale.offset_y ;
	recal = True;
	} /* if */

else if(y < lo_limit && (parent->dsscale.lower_y+parent->dsscale.offset_y) > lo_limit)  
	{
	XCopyArea(XtDisplay(w),
		parent->dsscale.slider,XtWindow(parent->composite.children[0]),
	        parent->manager.background_GC,
       		0,0,
	        parent->dsscale.slider_size_x,parent->dsscale.slider_size_y,
	        0,lo_limit - parent->dsscale.offset_y);

	XCopyArea(XtDisplay(w),
		parent->dsscale.pixmap,XtWindow(parent->composite.children[0]),
       		parent->dsscale.foreground_GC,
	        0,lo_limit + parent->dsscale.slider_size_y - parent->dsscale.offset_y,
       		parent->composite.children[0]->core.width,
		parent->dsscale.lower_y -lo_limit +parent->dsscale.offset_y,
	        0,lo_limit + parent->dsscale.slider_size_y - parent->dsscale.offset_y);

	parent->dsscale.lower_y=lo_limit - parent->dsscale.offset_y ;
	recal=True;
	}

else if(y > up_limit && (parent->dsscale.lower_y+parent->dsscale.offset_y) < up_limit)
	{
	XCopyArea(XtDisplay(w),
		parent->dsscale.slider,XtWindow(parent->composite.children[0]),
	        parent->manager.background_GC,
       		0,0,
	        parent->dsscale.slider_size_x,parent->dsscale.slider_size_y,
       		0,up_limit - parent->dsscale.offset_y);

	XCopyArea(XtDisplay(w),
		parent->dsscale.pixmap,XtWindow(parent->composite.children[0]),
	        parent->dsscale.foreground_GC,
       		0,parent->dsscale.lower_y,
	        parent->composite.children[0]->core.width,
		up_limit -parent->dsscale.offset_y -parent->dsscale.lower_y ,
	        0,parent->dsscale.lower_y );

	parent->dsscale.lower_y=up_limit - parent->dsscale.offset_y ;
	recal=True;
	}


if(recal)
	{
	deno = (float)(parent->composite.children[0]->core.height - 2*parent->dsscale.slider_size_y);

	if(parent->dsscale.processing_direction==XmMAX_ON_TOP)
		{
		nomi =(float)((parent->dsscale.maximum_value - parent->dsscale.minimum_value)
		*(parent->composite.children[0]->core.height 
		- parent->dsscale.lower_y - parent->dsscale.slider_size_y));
		}
	else if(parent->dsscale.processing_direction==XmMAX_ON_BOTTOM)
		{
		nomi = (float)((parent->dsscale.maximum_value 
			- parent->dsscale.minimum_value)*parent->dsscale.lower_y);
		}
	parent->dsscale.lower_value = (int)(nomi/deno +0.5) + parent->dsscale.minimum_value;
	}
}

/***************************************/

static void draw_hor_upper_slider_motion(Widget w, XEvent *event)
{
XmDoubleSliderScaleWidget parent=(XmDoubleSliderScaleWidget)w->core.parent;
int x,y;
int lo_limit = 0,up_limit = 0;
float nomi = 0,deno;
Boolean recal = False;

x=event->xmotion.x;
y=event->xmotion.y;

if(parent->dsscale.processing_direction==XmMAX_ON_RIGHT)
	{
	up_limit= w->core.width - (parent->dsscale.slider_size_x - parent->dsscale.offset_x);
	lo_limit= parent->dsscale.lower_x + parent->dsscale.slider_size_x +  parent->dsscale.offset_x;
	}
else if(parent->dsscale.processing_direction==XmMAX_ON_LEFT)
	{
	up_limit= parent->dsscale.lower_x - (parent->dsscale.slider_size_x - parent->dsscale.offset_x);
	lo_limit = parent->dsscale.offset_x;
	}

if(x<=up_limit && x>=lo_limit)
	{
	/** ... copy the slider .... ***/
	XCopyArea(XtDisplay(w),
		parent->dsscale.slider,XtWindow(parent->composite.children[0]),
                parent->manager.background_GC,
                0,0,
                parent->dsscale.slider_size_x,parent->dsscale.slider_size_y,
                x - parent->dsscale.offset_x,0);

	/*** fill up .... ***/
	if((x - parent->dsscale.offset_x )> parent->dsscale.upper_x)
		{
		XCopyArea(XtDisplay(w),
			parent->dsscale.pixmap,XtWindow(parent->composite.children[0]),
                        parent->dsscale.foreground_GC,
                        parent->dsscale.upper_x ,0,
		        x - parent->dsscale.offset_x -parent->dsscale.upper_x , 
			parent->dsscale.slider_size_y,
	                parent->dsscale.upper_x ,0);
		}

	else if((x - parent->dsscale.offset_x ) < parent->dsscale.upper_x)
		{
		XCopyArea(XtDisplay(w),
			parent->dsscale.pixmap,XtWindow(parent->composite.children[0]),
                        parent->dsscale.foreground_GC,
                        x + parent->dsscale.slider_size_x - parent->dsscale.offset_x,0,
		        parent->dsscale.upper_x  -x + parent->dsscale.offset_x, 
			parent->dsscale.slider_size_y,
                        x + parent->dsscale.slider_size_x - parent->dsscale.offset_x,0);
		}

	parent->dsscale.upper_x = x - parent->dsscale.offset_x ;
	recal = True;
	} /* if */

else if( x > up_limit && (parent->dsscale.upper_x+ parent->dsscale.offset_x) < up_limit)  
	{
	XCopyArea(XtDisplay(w),
		parent->dsscale.slider,XtWindow(parent->composite.children[0]),
                parent->manager.background_GC,
                0,0,
                parent->dsscale.slider_size_x,parent->dsscale.slider_size_y,
                up_limit - parent->dsscale.offset_x,0);

	XCopyArea(XtDisplay(w),
		parent->dsscale.pixmap,XtWindow(parent->composite.children[0]),
                parent->dsscale.foreground_GC,
                parent->dsscale.upper_x ,0,
	        up_limit - parent->dsscale.offset_x -parent->dsscale.upper_x , 
		parent->dsscale.slider_size_y,
                parent->dsscale.upper_x ,0);

	parent->dsscale.upper_x = up_limit - parent->dsscale.offset_x ;
	recal = True;
	}

else if( x < lo_limit && (parent->dsscale.upper_x+ parent->dsscale.offset_x) > lo_limit)
	{
	XCopyArea(XtDisplay(w),
		parent->dsscale.slider,XtWindow(parent->composite.children[0]),
                parent->manager.background_GC,
                0,0,
                parent->dsscale.slider_size_x,parent->dsscale.slider_size_y,
                lo_limit - parent->dsscale.offset_x,0);

	XCopyArea(XtDisplay(w),
		parent->dsscale.pixmap,XtWindow(parent->composite.children[0]),
                parent->dsscale.foreground_GC,
                lo_limit + parent->dsscale.slider_size_x - parent->dsscale.offset_x,0,
	        parent->dsscale.upper_x  -x + parent->dsscale.offset_x, 
		parent->dsscale.slider_size_y,
                lo_limit + parent->dsscale.slider_size_x - parent->dsscale.offset_x,0);

	parent->dsscale.upper_x = lo_limit - parent->dsscale.offset_x ;
	recal = True;
	}

if(recal)
	{
	deno = (float)(parent->composite.children[0]->core.width - 2*parent->dsscale.slider_size_x);

	if(parent->dsscale.processing_direction==XmMAX_ON_RIGHT)
		{
		nomi = (float)((parent->dsscale.maximum_value - parent->dsscale.minimum_value)
			*(parent->dsscale.upper_x - parent->dsscale.slider_size_x)); 

		}
	else if(parent->dsscale.processing_direction==XmMAX_ON_LEFT)
		{
		nomi = (float)((parent->dsscale.maximum_value - parent->dsscale.minimum_value)
			*(parent->composite.children[0]->core.width 
			- 2*parent->dsscale.slider_size_x - parent->dsscale.upper_x));

		}
	parent->dsscale.upper_value = (int)(nomi/deno +0.5) + parent->dsscale.minimum_value;
	}
}

/***************************************/

static void draw_ver_upper_slider_motion(Widget w, XEvent *event)
{
XmDoubleSliderScaleWidget parent=(XmDoubleSliderScaleWidget)w->core.parent;
int x,y;
int lo_limit = 0,up_limit = 0;
float nomi = 0,deno;
Boolean recal=False;

x=event->xmotion.x;
y=event->xmotion.y;

if(parent->dsscale.processing_direction==XmMAX_ON_TOP)
        {
        up_limit= parent->dsscale.lower_y - (parent->dsscale.slider_size_y - parent->dsscale.offset_y);
        lo_limit = parent->dsscale.offset_y;
        }

else if(parent->dsscale.processing_direction==XmMAX_ON_BOTTOM)
        {
        up_limit = w->core.height - (parent->dsscale.slider_size_y - parent->dsscale.offset_y);
        lo_limit = parent->dsscale.lower_y + parent->dsscale.slider_size_y +  parent->dsscale.offset_y ;
        }


if(y<=up_limit && y>=lo_limit)
	{
	/* ... copy slider at new position.... */
	XCopyArea(XtDisplay(w),
		parent->dsscale.slider,XtWindow(parent->composite.children[0]),
		parent->manager.background_GC,
		0,0,
		parent->dsscale.slider_size_x,parent->dsscale.slider_size_y,
		0,y - parent->dsscale.offset_y);

	/*** ... fill up .....***/
	if((y - parent->dsscale.offset_y )> parent->dsscale.upper_y)
		{
		XCopyArea(XtDisplay(w),
			parent->dsscale.pixmap,XtWindow(parent->composite.children[0]),
			parent->dsscale.foreground_GC,
			0,parent->dsscale.upper_y,
			parent->dsscale.slider_size_x, 
			y - parent->dsscale.offset_y - parent->dsscale.upper_y ,
			0,parent->dsscale.upper_y );
		}

	else if((y - parent->dsscale.offset_y ) < parent->dsscale.upper_y)
		{
		XCopyArea(XtDisplay(w),
			parent->dsscale.pixmap,XtWindow(parent->composite.children[0]),
			parent->dsscale.foreground_GC,
			0, y + parent->dsscale.slider_size_y - parent->dsscale.offset_y,
			parent->dsscale.slider_size_x, 
			parent->dsscale.upper_y  - y + parent->dsscale.offset_y,
			0,y + parent->dsscale.slider_size_y - parent->dsscale.offset_y);
		}
	parent->dsscale.upper_y = y - parent->dsscale.offset_y ;
	recal=True;
	} /* if */

else if(y < lo_limit && (parent->dsscale.upper_y + parent->dsscale.offset_y) > lo_limit)
	{
	XCopyArea(XtDisplay(w),
		parent->dsscale.slider,XtWindow(parent->composite.children[0]),
                parent->manager.background_GC,
                0,0,
                parent->dsscale.slider_size_x,parent->dsscale.slider_size_y,
                0,lo_limit - parent->dsscale.offset_y);

	XCopyArea(XtDisplay(w),
		parent->dsscale.pixmap,XtWindow(parent->composite.children[0]),
                parent->dsscale.foreground_GC,
                0, lo_limit + parent->dsscale.slider_size_y - parent->dsscale.offset_y,
                parent->dsscale.slider_size_x, 
		parent->dsscale.upper_y  - lo_limit + parent->dsscale.offset_y,
                0,lo_limit + parent->dsscale.slider_size_y - parent->dsscale.offset_y);

	parent->dsscale.upper_y = lo_limit - parent->dsscale.offset_y ;
	recal=True;
	} 

else if(y > up_limit && (parent->dsscale.upper_y + parent->dsscale.offset_y) < up_limit)
	{
	XCopyArea(XtDisplay(w),
		parent->dsscale.slider,XtWindow(parent->composite.children[0]),
                parent->manager.background_GC,
                0,0,
                parent->dsscale.slider_size_x,parent->dsscale.slider_size_y,
                0,up_limit - parent->dsscale.offset_y);

	XCopyArea(XtDisplay(w),
		parent->dsscale.pixmap,XtWindow(parent->composite.children[0]),
                parent->dsscale.foreground_GC,
                0,parent->dsscale.upper_y,
                parent->dsscale.slider_size_x, 
		up_limit - parent->dsscale.offset_y - parent->dsscale.upper_y ,
                0,parent->dsscale.upper_y );

	parent->dsscale.upper_y = up_limit - parent->dsscale.offset_y ;
	recal=True;
	}

if(recal)
	{
	deno = (float)(parent->composite.children[0]->core.height - 2*parent->dsscale.slider_size_y);

	if(parent->dsscale.processing_direction==XmMAX_ON_TOP)
		{
		nomi = (float)((parent->dsscale.maximum_value - parent->dsscale.minimum_value)
			*(parent->composite.children[0]->core.height 
			- parent->dsscale.upper_y - 2*parent->dsscale.slider_size_y));
		}
	else if(parent->dsscale.processing_direction==XmMAX_ON_BOTTOM)
		{
		nomi = (float)((parent->dsscale.maximum_value - parent->dsscale.minimum_value)
			*(parent->dsscale.upper_y - parent->dsscale.slider_size_y));
		}
	parent->dsscale.upper_value = (int)(nomi/deno +0.5) + parent->dsscale.minimum_value;
	}
}

/***************************************/

static void Initialize(Widget treq,Widget tnew,ArgList args,Cardinal *nargs)
{
XmDoubleSliderScaleWidget win=(XmDoubleSliderScaleWidget)tnew;
XmDoubleSliderScaleWidget rqwin=(XmDoubleSliderScaleWidget)treq;

create_children(win);
set_initial_size(win,rqwin,args,nargs);
create_GC(win);
change_background_GC(win);
create_trough_pixmap(win);
create_slider_pixmap(win);
draw_slot(win);
}

/*************************************/

static void create_children(XmDoubleSliderScaleWidget win)
{
XtTranslations  child_translations;
Arg arg[3];
int i;

i=0;
XtSetArg(arg[i], XmNbackground,win->core.background_pixel);i++;
XtSetArg(arg[i], XmNhighlightThickness, 2);i++;
XtSetArg(arg[i], XmNtraversalOn, True);i++;
XtCreateManagedWidget("child1",xmPrimitiveWidgetClass,(Widget)win,arg,i);

XtAppAddActions(XtWidgetToApplicationContext(win->composite.children[0]),actions,XtNumber(actions));
child_translations=XtParseTranslationTable(translations);
XtOverrideTranslations(win->composite.children[0],child_translations);

}

/****************************/

static void set_initial_size(XmDoubleSliderScaleWidget nw,XmDoubleSliderScaleWidget rw,ArgList args,Cardinal *nargs)
{

nw->manager.shadow_thickness=2;

/*** orientation ****/
if(nw->dsscale.orientation != XmHORIZONTAL && nw->dsscale.orientation !=XmVERTICAL && nw->dsscale.orientation !=(-1))
{
	XtWarning("Invalid orientation-type");
	nw->dsscale.orientation = XmHORIZONTAL;
} 


set_scale_values(nw,rw,args,nargs);

switch(nw->dsscale.orientation)
{
case XmHORIZONTAL:
	set_hor_init_size(nw,args,nargs);
break;

case XmVERTICAL:
	set_ver_init_size(nw,args,nargs);
break;
} /* sw */

}
/*****************************************/

static void set_hor_init_size(XmDoubleSliderScaleWidget nw,ArgList args,Cardinal *nargs)
{
Position y_co;


	/*** processing direction ****/ 

	if(nw->dsscale.processing_direction !=XmMAX_ON_RIGHT && nw->dsscale.processing_direction !=XmMAX_ON_LEFT)
	{
        if(if_set(args, nargs, XmNprocessingDirection))
		XtWarning("For horizontal orientation the processing direction can be\n\t either XmMAX_ON_RIGHT or XmMAX_ON_LEFT");
        nw->dsscale.processing_direction=XmMAX_ON_RIGHT;
	}


	/*** scale ****/
    if(nw->dsscale.scale_height > nw->core.height)
        {
        XtWarning("The scale height is greater than window height");
        nw->dsscale.slider_thickness = nw->core.height;
        }
    else if(nw->dsscale.scale_height <= 0)
        {
        if(if_set(args,nargs,XmNscaleHeight))
                XtWarning("The scale height is less than or equal to zero");
        nw->dsscale.slider_thickness =15;
        }
     else
       nw->dsscale.slider_thickness = nw->dsscale.scale_height;

       nw->dsscale.slot_thickness = (int)(nw->dsscale.slider_thickness/5.0 + 0.5);
	if( nw->dsscale.slot_thickness ==0)  nw->dsscale.slot_thickness =1;

	/** trough ***/
	nw->composite.children[0]->core.height=nw->dsscale.slider_thickness;

	 if(if_set(args,nargs,XmNscaleWidth) && nw->dsscale.scale_width <= (nw->core.width -10))
		nw->composite.children[0]->core.width=nw->dsscale.scale_width;
	else 
		nw->composite.children[0]->core.width= nw->core.width -10;

	nw->composite.children[0]->core.x=(Position)( (nw->core.width - nw->composite.children[0]->core.width)/2.0 +0.5) ;

        nw->dsscale.slider_length = (nw->composite.children[0]->core.width >= 100) ? 30:20;

	if(nw->dsscale.scale_alignment!= XmALIGNMENT_BEGINNING && nw->dsscale.scale_alignment!= XmALIGNMENT_CENTER && nw->dsscale.scale_alignment!= XmALIGNMENT_END)
		{
		XtWarning("Invalid alignment type");
		nw->dsscale.scale_alignment = XmALIGNMENT_END;
		}

		/** must be after setting alignment ****/

		y_co = get_hor_trough_y(nw);
		nw->composite.children[0]->core.y = y_co;

	/** end trough ***/

nw->dsscale.slider_size_x = nw->dsscale.slider_length;
nw->dsscale.slider_size_y = nw->dsscale.slider_thickness;

calculate_hor_slider_positions(nw);
}
/*************************************/
static Position get_hor_trough_y(XmDoubleSliderScaleWidget nw)
{
Position y_co = 0;
Dimension font_height;
XmString xmstring;

          if(nw->dsscale.scale_alignment == XmALIGNMENT_BEGINNING)
                {
                xmstring = XmStringCreateLocalized("1");
                font_height = XmStringHeight(nw->dsscale.font_list, xmstring);
                XmStringFree(xmstring);

                y_co = font_height + 5;
                        if(y_co <0) y_co = 0;
                }
        else if(nw->dsscale.scale_alignment == XmALIGNMENT_CENTER)
                {
                y_co = (int)((nw->core.height - nw->dsscale.slider_thickness)/2.0 +0.5);
                }
        else if(nw->dsscale.scale_alignment == XmALIGNMENT_END)
                {
                y_co = nw->core.height - nw->dsscale.slider_thickness - 5;
                if(y_co <0) y_co = 0;
                }

return(y_co);
}

/*************************************/

static void calculate_hor_slider_positions(XmDoubleSliderScaleWidget nw)
{
float value_span;
float pixel_span;
float l_nomi,u_nomi;

	value_span=(float)(nw->dsscale.maximum_value - nw->dsscale.minimum_value);
	pixel_span=(float)(nw->composite.children[0]->core.width - 2*nw->dsscale.slider_size_x);

	if(nw->dsscale.processing_direction==XmMAX_ON_RIGHT)
	{
	l_nomi = pixel_span * (nw->dsscale.lower_value - nw->dsscale.minimum_value);
	nw->dsscale.lower_x = (int)(l_nomi/value_span +0.5);
	
	u_nomi = pixel_span*(nw->dsscale.upper_value - nw->dsscale.minimum_value);
	nw->dsscale.upper_x = (int)(u_nomi/value_span +0.5) + nw->dsscale.slider_size_x;
	}
        else if(nw->dsscale.processing_direction==XmMAX_ON_LEFT)
        {
	l_nomi = pixel_span*(nw->dsscale.lower_value - nw->dsscale.minimum_value);
        nw->dsscale.lower_x = nw->composite.children[0]->core.width 
			- (int)(l_nomi/value_span +0.5) 
			- nw->dsscale.slider_size_x;        

	u_nomi = pixel_span*(nw->dsscale.upper_value - nw->dsscale.minimum_value);
        nw->dsscale.upper_x = nw->composite.children[0]->core.width 
			- (int)(u_nomi/value_span+0.5) 
			- 2*nw->dsscale.slider_size_x;        
	}

}
/**************************************************/
static void set_ver_init_size(XmDoubleSliderScaleWidget nw,ArgList args,Cardinal *nargs)
{
Position x_co;


	/**** processing direction ****************/
        if(nw->dsscale.processing_direction !=XmMAX_ON_TOP && nw->dsscale.processing_direction !=XmMAX_ON_BOTTOM)
        {
	if(if_set(args, nargs, XmNprocessingDirection))
       		 XtWarning("For vertical orientation the processing direction can be\n\t either XmMAX_ON_TOP or XmMAX_ON_BOTTOM");
        nw->dsscale.processing_direction=XmMAX_ON_TOP;
        }


	/*** scale ****/

   if(nw->dsscale.scale_width > nw->core.width)
        {
        XtWarning("The scale width is greater than window width");
        nw->dsscale.slider_thickness = nw->core.width;
        }
   else if(nw->dsscale.scale_width <= 0)
        {
         if(if_set(args, nargs,XmNscaleWidth))
            XtWarning("The scale width is less than or equal to zero");
         nw->dsscale.slider_thickness =15;
        }
     else
       nw->dsscale.slider_thickness = nw->dsscale.scale_width;

       nw->dsscale.slot_thickness = (int)(nw->dsscale.slider_thickness/5.0 + 0.5);
	if( nw->dsscale.slot_thickness ==0)  nw->dsscale.slot_thickness =1;



	/** trough ***/

	nw->composite.children[0]->core.width = nw->dsscale.slider_thickness;


        if(nw->dsscale.scale_alignment!= XmALIGNMENT_BEGINNING && nw->dsscale.scale_alignment!= XmALIGNMENT_CENTER && nw->dsscale.scale_alignment!= XmALIGNMENT_END)
        {
        XtWarning("Invalid alignment type");
        nw->dsscale.scale_alignment = XmALIGNMENT_END;
        }
	
	x_co = get_ver_trough_x(nw);

	nw->composite.children[0]->core.x = x_co;
	

        if(if_set(args,nargs,XmNscaleHeight) && nw->dsscale.scale_height <= (nw->core.height -10))
		nw->composite.children[0]->core.height = nw->dsscale.scale_height;

	else
		nw->composite.children[0]->core.height = nw->core.height - 10;

	nw->composite.children[0]->core.y = (Position)((nw->core.height - nw->composite.children[0]->core.height)/2.0 + 0.5);

        nw->dsscale.slider_length = (nw->composite.children[0]->core.height >= 100) ? 30:20;

	nw->dsscale.slider_size_x = nw->dsscale.slider_thickness;
	nw->dsscale.slider_size_y = nw->dsscale.slider_length;

calculate_ver_slider_positions(nw);
}
/***************************************/

static Position get_ver_trough_x(XmDoubleSliderScaleWidget nw)
{
Position x_co = 0;
Dimension font_width;
XmString xmstring;
char string[10];


          if(nw->dsscale.scale_alignment == XmALIGNMENT_BEGINNING)
                {
                sprintf(string,"%d",nw->dsscale.maximum_value);
                 xmstring = XmStringCreateLocalized(string);
                  font_width = XmStringWidth(nw->dsscale.font_list, xmstring);
                XmStringFree(xmstring);

                x_co = font_width +5;
                if(x_co > nw->core.width - nw->dsscale.slider_thickness)
                         x_co = nw->core.width - nw->dsscale.slider_thickness;
                }
        else if(nw->dsscale.scale_alignment == XmALIGNMENT_CENTER)
                {
                x_co = (int)((nw->core.width - nw->dsscale.slider_thickness)/2.0 +0.5);
                }
        else if(nw->dsscale.scale_alignment == XmALIGNMENT_END)
                {
                x_co =  nw->core.width - nw->dsscale.slider_thickness - 5;
                }

return(x_co); 
}
/************************************/
static void calculate_ver_slider_positions(XmDoubleSliderScaleWidget nw)
{
float value_span;
float pixel_span;
float l_nomi,u_nomi;

        value_span=(float)(nw->dsscale.maximum_value - nw->dsscale.minimum_value);
        pixel_span=(float)(nw->composite.children[0]->core.height - 2*nw->dsscale.slider_length);


	if(nw->dsscale.processing_direction==XmMAX_ON_TOP)
	{
	l_nomi = pixel_span*(nw->dsscale.lower_value - nw->dsscale.minimum_value);
	nw->dsscale.lower_y = nw->composite.children[0]->core.height 
			- (int)(l_nomi/value_span +0.5) 
			- nw->dsscale.slider_size_y;
	
	u_nomi = pixel_span*(nw->dsscale.upper_value - nw->dsscale.minimum_value);
	nw->dsscale.upper_y = nw->composite.children[0]->core.height 
			- (int)(u_nomi/value_span +0.5) 
			- 2*nw->dsscale.slider_size_y;
	}
	else if(nw->dsscale.processing_direction==XmMAX_ON_BOTTOM)
	{
	l_nomi = pixel_span*(nw->dsscale.lower_value - nw->dsscale.minimum_value);
	nw->dsscale.lower_y = (int)(l_nomi/value_span +0.5);

	u_nomi = pixel_span*(nw->dsscale.upper_value - nw->dsscale.minimum_value);
	nw->dsscale.upper_y = (int)(u_nomi/value_span +0.5) + nw->dsscale.slider_size_y ;
	}

}
/*******************************/
static void show_lower_value(XmDoubleSliderScaleWidget nw)
{
Position x_lo = 0,y_lo = 0;
Dimension height, width_lo;
XmString val_lo;
char str_lo[10];

if(!(XtIsRealized(nw))) return;

sprintf(str_lo,"%d",nw->dsscale.lower_value);

val_lo = XmStringCreateLocalized(str_lo);

height = XmStringHeight(nw->dsscale.font_list, val_lo);

width_lo = XmStringWidth(nw->dsscale.font_list, val_lo); 

if(nw->dsscale.orientation==XmHORIZONTAL)
{
	if(nw->dsscale.processing_direction == XmMAX_ON_RIGHT)
	{
	x_lo = nw->composite.children[0]->core.x + nw->dsscale.lower_x + nw->dsscale.slider_size_x - width_lo ;
	}
	else if(nw->dsscale.processing_direction == XmMAX_ON_LEFT)
	{
	x_lo = nw->composite.children[0]->core.x + nw->dsscale.lower_x ;
	}

	y_lo = nw->composite.children[0]->core.y - height;
} /* if HOR */	

else if(nw->dsscale.orientation==XmVERTICAL)
{
	if(nw->dsscale.processing_direction == XmMAX_ON_TOP)
	{
	y_lo = nw->composite.children[0]->core.y + nw->dsscale.lower_y;
	}

	else if(nw->dsscale.processing_direction == XmMAX_ON_BOTTOM)
	{
	y_lo = nw->composite.children[0]->core.y + nw->dsscale.lower_y + nw->dsscale.slider_size_y - height;
	}

	x_lo = nw->composite.children[0]->core.x - width_lo;
} /* if VER */

XmStringDraw(XtDisplay((Widget)nw), XtWindow((Widget)nw),
		nw->dsscale.font_list, 
		val_lo,
		nw->dsscale.foreground_GC, 
		x_lo, y_lo, width_lo, 
		XmALIGNMENT_END, XmSTRING_DIRECTION_L_TO_R, NULL);

XmStringFree(val_lo);

nw->dsscale.show_lower_value_x = x_lo;
nw->dsscale.show_lower_value_y = y_lo;

}
/***************************/
static void erase_prev_lower_value(XmDoubleSliderScaleWidget nw)
{
Dimension width_lo;
XmString val_lo;
char str_lo[10];

if(!(XtIsRealized(nw))) return;

sprintf(str_lo,"%d",nw->dsscale.lower_value);

val_lo = XmStringCreateLocalized(str_lo);

width_lo = XmStringWidth(nw->dsscale.font_list, val_lo); 

XmStringDraw(XtDisplay((Widget)nw), XtWindow((Widget)nw),
		nw->dsscale.font_list, 
		val_lo,
		nw->manager.background_GC, 
		nw->dsscale.show_lower_value_x,nw->dsscale.show_lower_value_y, width_lo,
		XmALIGNMENT_END, XmSTRING_DIRECTION_L_TO_R, NULL);

XmStringFree(val_lo);
}
/**********************************/
static void show_upper_value(XmDoubleSliderScaleWidget nw)
{
Position x_up = 0,y_up = 0;
Dimension height, width_up;
XmString val_up;
char str_up[10];

if(!(XtIsRealized(nw))) return;

sprintf(str_up,"%d",nw->dsscale.upper_value);

val_up = XmStringCreateLocalized(str_up);

height = XmStringHeight(nw->dsscale.font_list, val_up);

width_up = XmStringWidth(nw->dsscale.font_list, val_up); 

if(nw->dsscale.orientation==XmHORIZONTAL)
{
	if(nw->dsscale.processing_direction == XmMAX_ON_RIGHT)
	{
	x_up = nw->composite.children[0]->core.x + nw->dsscale.upper_x ;
	}
	else if(nw->dsscale.processing_direction == XmMAX_ON_LEFT)
	{
	x_up = nw->composite.children[0]->core.x + nw->dsscale.upper_x + nw->dsscale.slider_size_x - width_up;
	}

	y_up = nw->composite.children[0]->core.y - height;
} /* if HOR */	

else if(nw->dsscale.orientation==XmVERTICAL)
{
	if(nw->dsscale.processing_direction == XmMAX_ON_TOP)
	{
	y_up = nw->composite.children[0]->core.y + nw->dsscale.upper_y + nw->dsscale.slider_size_y - height ;
	}

	else if(nw->dsscale.processing_direction == XmMAX_ON_BOTTOM)
	{
	y_up = nw->composite.children[0]->core.y + nw->dsscale.upper_y ;
	}

	x_up = nw->composite.children[0]->core.x - width_up;
} /* if VER */

XmStringDraw(XtDisplay((Widget)nw), XtWindow((Widget)nw),
		nw->dsscale.font_list, 
		val_up,
		nw->dsscale.foreground_GC, 
		x_up, y_up, width_up, 
		XmALIGNMENT_END, XmSTRING_DIRECTION_L_TO_R, NULL);

XmStringFree(val_up);

nw->dsscale.show_upper_value_x = x_up;
nw->dsscale.show_upper_value_y = y_up;
}

/*********************/
static void erase_prev_upper_value(XmDoubleSliderScaleWidget nw)
{
Dimension width_up;
XmString val_up;
char str_up[10];

if(!(XtIsRealized(nw))) return;

sprintf(str_up,"%d",nw->dsscale.upper_value);

val_up = XmStringCreateLocalized(str_up);

width_up = XmStringWidth(nw->dsscale.font_list, val_up); 

XmStringDraw(XtDisplay((Widget)nw), XtWindow((Widget)nw),
		nw->dsscale.font_list, 
		val_up,
		nw->manager.background_GC, 
		nw->dsscale.show_upper_value_x,nw->dsscale.show_upper_value_y,width_up,
		XmALIGNMENT_END, XmSTRING_DIRECTION_L_TO_R, NULL);

XmStringFree(val_up);
}

/********************************/

static void create_trough_pixmap(XmDoubleSliderScaleWidget wid)
{
int depth,screen_number;

screen_number=DefaultScreen(XtDisplay((Widget)wid));
depth=wid->core.depth;


wid->dsscale.pixmap=XCreatePixmap(XtDisplay((Widget)wid),
		RootWindow(XtDisplay((Widget)wid),screen_number),
		wid->composite.children[0]->core.width,
		wid->composite.children[0]->core.height,
			depth);

}

/**************************/

static void create_slider_pixmap(XmDoubleSliderScaleWidget wid)
{
int depth,screen_number;

screen_number=DefaultScreen(XtDisplay((Widget)wid));
depth=wid->core.depth;

wid->dsscale.slider=XCreatePixmap(XtDisplay((Widget)wid),
                RootWindow(XtDisplay((Widget)wid),screen_number),
                wid->dsscale.slider_size_x,
                wid->dsscale.slider_size_y,
                        depth);

draw_slider_shadow(wid);

}

/*********************************/

static void draw_slider_shadow(XmDoubleSliderScaleWidget wid)
{
XPoint point[5];
int line_pos,ii,number;

XFillRectangle(XtDisplay((Widget)wid),wid->dsscale.slider,
                                wid->manager.background_GC, 0,0,
                                 wid->dsscale.slider_size_x,
                                wid->dsscale.slider_size_y);

point[0].x=0; point[0].y=0;
point[1].x=wid->dsscale.slider_size_x; point[1].y=0;
point[2].x=wid->dsscale.slider_size_x - wid->manager.shadow_thickness; 
				point[2].y=wid->manager.shadow_thickness;
point[3].x=wid->manager.shadow_thickness; 
				point[3].y=wid->manager.shadow_thickness;
point[4].x=0; point[4].y=0;

XFillPolygon(XtDisplay((Widget)wid),wid->dsscale.slider,
                        wid->manager.top_shadow_GC,point,5,Convex,CoordModeOrigin);

point[0].x=0   ; point[0].y=0  ;
point[1].x=wid->manager.shadow_thickness ; 
				point[1].y=wid->manager.shadow_thickness ;
point[2].x= wid->manager.shadow_thickness ; 
			point[2].y= wid->dsscale.slider_size_y - wid->manager.shadow_thickness;
point[3].x= 0 ; point[3].y= wid->dsscale.slider_size_y;
point[4].x= 0  ; point[4].y= 0 ;
XFillPolygon(XtDisplay((Widget)wid),wid->dsscale.slider,
                        wid->manager.top_shadow_GC,point,5,Convex,CoordModeOrigin);


point[0].x=wid->dsscale.slider_size_x; point[0].y=0;
point[1].x=wid->dsscale.slider_size_x; point[1].y=wid->dsscale.slider_size_y;
point[2].x=wid->dsscale.slider_size_x - wid->manager.shadow_thickness; 
			point[2].y= wid->dsscale.slider_size_y - wid->manager.shadow_thickness;
point[3].x=wid->dsscale.slider_size_x - wid->manager.shadow_thickness;  
			point[3].y=wid->manager.shadow_thickness;
point[4].x=wid->dsscale.slider_size_x; point[4].y=0;

XFillPolygon(XtDisplay((Widget)wid),wid->dsscale.slider,
                        wid->manager.bottom_shadow_GC,point,5,Convex,CoordModeOrigin);


point[0].x=0  ; point[0].y=wid->dsscale.slider_size_y  ;
point[1].x=wid->dsscale.slider_size_x ; point[1].y=wid->dsscale.slider_size_y;
point[2].x= wid->dsscale.slider_size_x - wid->manager.shadow_thickness ; 
			point[2].y=wid->dsscale.slider_size_y - wid->manager.shadow_thickness;
point[3].x= wid->manager.shadow_thickness ; 
			point[3].y= wid->dsscale.slider_size_y - wid->manager.shadow_thickness;
point[4].x= 0 ; point[4].y= wid->dsscale.slider_size_y ;
XFillPolygon(XtDisplay((Widget)wid),wid->dsscale.slider,
                        wid->manager.bottom_shadow_GC,point,5,Convex,CoordModeOrigin);

/***..... fancy lines on slider ......***/
if(wid->dsscale.orientation==XmHORIZONTAL)
{
	if( wid->dsscale.slider_size_x <= 20) number = 2;
	else if(wid->dsscale.slider_size_x > 20) number = 3;

        line_pos = (int)(((float)wid->dsscale.slider_size_x - 4.0*(float)number)/2.0 + 0.5);
			
	for(ii=0;ii<number;ii++)
	{
	XDrawLine(XtDisplay((Widget)wid),wid->dsscale.slider,
			wid->manager.top_shadow_GC,
			line_pos,wid->manager.shadow_thickness,
			line_pos,wid->dsscale.slider_size_y - wid->manager.shadow_thickness);
	line_pos+=2;

	XDrawLine(XtDisplay((Widget)wid),wid->dsscale.slider,
			wid->manager.bottom_shadow_GC,
			line_pos,wid->manager.shadow_thickness,
			line_pos,wid->dsscale.slider_size_y - wid->manager.shadow_thickness);
	line_pos+=2;
	} /* for */
} /* if */
else if(wid->dsscale.orientation==XmVERTICAL)
{
        if( wid->dsscale.slider_size_y <= 20) number = 2;
        else if(wid->dsscale.slider_size_y > 20) number = 3;

        line_pos = (int)(((float)wid->dsscale.slider_size_y - 4.0*(float)number)/2.0 + 0.5);
       
        for(ii=0;ii<number;ii++)
        {
        XDrawLine(XtDisplay((Widget)wid),wid->dsscale.slider,
                        wid->manager.top_shadow_GC,
                        wid->manager.shadow_thickness,line_pos,
                        wid->dsscale.slider_size_x - wid->manager.shadow_thickness,line_pos);
        line_pos+=2;

        XDrawLine(XtDisplay((Widget)wid),wid->dsscale.slider,
                        wid->manager.bottom_shadow_GC,
                        wid->manager.shadow_thickness,line_pos,
                        wid->dsscale.slider_size_x - wid->manager.shadow_thickness,line_pos);
        line_pos+=2;
        } /* for */
} /* else if */



}

/***************************/

static void create_GC(XmDoubleSliderScaleWidget wid)
{
Pixel background=wid->composite.children[0]->core.background_pixel;
XtGCMask mask;
XGCValues values;
XFontStruct *font_struct;
XmFontType font_type;
XmFontContext font_context;
XmFontListEntry font_list_entry;
Boolean status;
XtPointer pointer;
char *font_name;
XFontSet *font_set;

status=XmFontListInitFontContext(&font_context, wid->dsscale.font_list);

font_list_entry= XmFontListNextEntry(font_context);

pointer = XmFontListEntryGetFont(font_list_entry, &font_type);

if(font_type == XmFONT_IS_FONT)
        {
        font_struct = (XFontStruct *)pointer;
        values.font=font_struct->fid;
        }
else if(font_type == XmFONT_IS_FONTSET)
        {
        font_set = (XFontSet *) pointer;
        font_name = XBaseFontNameListOfFontSet(*font_set);
        values.font= XLoadFont(XtDisplay(wid), font_name);
        }


values.foreground=wid->manager.foreground;
values.background=background;

mask=GCForeground|GCBackground|GCFont;

wid->dsscale.foreground_GC=XtGetGC((Widget)wid,mask,&values);

XmFontListFreeFontContext(font_context);

}
/******************************************/

static void change_background_GC(XmDoubleSliderScaleWidget wid)
{
XGCValues value;
XFontStruct *font_struct;
XmFontType font_type;
XmFontContext font_context;
XmFontListEntry font_list_entry;
Boolean status;
XtPointer pointer;
char *font_name;
XFontSet *font_set;

status=XmFontListInitFontContext(&font_context, wid->dsscale.font_list);

font_list_entry= XmFontListNextEntry(font_context);

pointer = XmFontListEntryGetFont(font_list_entry, &font_type);

if(font_type == XmFONT_IS_FONT)
        {
        font_struct = (XFontStruct *)pointer;
        value.font=font_struct->fid;
        }
else if(font_type == XmFONT_IS_FONTSET)
        {
        font_set = (XFontSet *) pointer;
        font_name = XBaseFontNameListOfFontSet(*font_set);
        value.font= XLoadFont(XtDisplay(wid), font_name);
        }

XChangeGC(XtDisplay(wid), wid->manager.background_GC,GCFont, &value);

XmFontListFreeFontContext(font_context);

}
/******************************************/
static void change_GCfonts(XmDoubleSliderScaleWidget wid)
{
XGCValues value;
XFontStruct *font_struct;
XmFontType font_type;
XmFontContext font_context;
XmFontListEntry font_list_entry;
Boolean status;
XtPointer pointer;
char *font_name;
XFontSet *font_set;

status=XmFontListInitFontContext(&font_context, wid->dsscale.font_list);

font_list_entry= XmFontListNextEntry(font_context);

pointer = XmFontListEntryGetFont(font_list_entry, &font_type);

if(font_type == XmFONT_IS_FONT)
        {
        font_struct = (XFontStruct *)pointer;
        value.font=font_struct->fid;
        }
else if(font_type == XmFONT_IS_FONTSET)
        {
        font_set = (XFontSet *) pointer;
        font_name = XBaseFontNameListOfFontSet(*font_set);
        value.font= XLoadFont(XtDisplay(wid), font_name);
        }

XChangeGC(XtDisplay(wid), wid->manager.background_GC,GCFont, &value);
XChangeGC(XtDisplay(wid), wid->dsscale.foreground_GC,GCFont, &value);

XmFontListFreeFontContext(font_context);

}
/*****************************************/
static void draw_slot(XmDoubleSliderScaleWidget wid)
{
int x = 0,y = 0;
int width = 0,height = 0;

XFillRectangle(XtDisplay((Widget)wid),wid->dsscale.pixmap,
                                wid->manager.background_GC, 0,0,
                                wid->composite.children[0]->core.width,
                                wid->composite.children[0]->core.height);


switch(wid->dsscale.orientation)
{
case XmHORIZONTAL:
x=0;
y=(int)((float)((int)wid->composite.children[0]->core.height - (int)wid->dsscale.slot_thickness)/2.0 +0.5);
width=wid->composite.children[0]->core.width;
height= wid->dsscale.slot_thickness;
break;

case XmVERTICAL:
y=0;
x=(int)((float)(wid->composite.children[0]->core.width - wid->dsscale.slot_thickness)/2.0 +0.5);
width=wid->dsscale.slot_thickness;
height=wid->composite.children[0]->core.height;
break;
}  /* switch */

XFillRectangle(XtDisplay(wid),wid->dsscale.pixmap,wid->dsscale.foreground_GC,
              x,y,
		width ,
		height);		

}
/********************************************/

static void ReDisplay(Widget w,XExposeEvent *event,Region region)
{
XmDoubleSliderScaleWidget wid=(XmDoubleSliderScaleWidget)w;

if(!XtIsRealized(wid)) return;
redraw_all(wid);

}

/********************************************/

static void redraw_all(XmDoubleSliderScaleWidget wid)
{
int x_lower_pos = 0,y_lower_pos = 0;
int x_upper_pos = 0,y_upper_pos = 0;

/** ..... copy the trough ....... ***/
XCopyArea(XtDisplay((Widget)wid),
	wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
	wid->dsscale.foreground_GC,
	0,0,
	wid->composite.children[0]->core.width,wid->composite.children[0]->core.height,
	0,0);

/***... copy the slider ..... ***/
switch(wid->dsscale.orientation)
{
case XmHORIZONTAL:
	x_lower_pos = wid->dsscale.lower_x;
	y_lower_pos = 0;
	x_upper_pos = wid->dsscale.upper_x;
	y_upper_pos = 0;
break;

case XmVERTICAL:
	x_lower_pos = 0;
	y_lower_pos = wid->dsscale.lower_y;
	x_upper_pos = 0;
	y_upper_pos = wid->dsscale.upper_y;
break;
} /* switch */

XCopyArea(XtDisplay((Widget)wid),wid->dsscale.slider,XtWindow(wid->composite.children[0]),
			wid->manager.background_GC,
				0,0,
				wid->dsscale.slider_size_x,wid->dsscale.slider_size_y,
				x_lower_pos,y_lower_pos);

XCopyArea(XtDisplay((Widget)wid),wid->dsscale.slider,XtWindow(wid->composite.children[0]),
			wid->manager.background_GC,
				0,0,
				wid->dsscale.slider_size_x,wid->dsscale.slider_size_y,
				x_upper_pos,y_upper_pos);

if(wid->dsscale.show_values) 
{
show_lower_value(wid);
show_upper_value(wid);
}

}

/*************************/
static void Destroy(Widget w)
{
XmDoubleSliderScaleWidget wid=(XmDoubleSliderScaleWidget)w;

XmFontListFree(wid->dsscale.font_list);
XtRemoveAllCallbacks(w,XmNupperValueChangedCallback);
XtRemoveAllCallbacks(w,XmNlowerValueChangedCallback);
XtRemoveAllCallbacks(w,XmNupperDragCallback);
XtRemoveAllCallbacks(w,XmNlowerDragCallback);
XFreePixmap(XtDisplay(w), wid->dsscale.pixmap);
XFreePixmap(XtDisplay(w), wid->dsscale.slider);
XtReleaseGC(w, wid->dsscale.foreground_GC);
XtDestroyWidget(wid->composite.children[0]);
}

/******************************/
static void Resize(Widget w)
{
XmDoubleSliderScaleWidget wid=(XmDoubleSliderScaleWidget)w;
Position y_co = 0,x_co = 0;
Dimension width = 0, height = 0,save_dimension = 0;

if(wid->dsscale.orientation == XmHORIZONTAL)
{
        /** trough ***/
	if(wid->core.width > wid->dsscale.scale_width && wid->dsscale.scale_width != 0)
		{
		width = wid->dsscale.scale_width;	
		}
        else
                {
                width= wid->core.width -10;
                }

	save_dimension = wid->composite.children[0]->core.width;
	height = wid->dsscale.slider_thickness;

	x_co = (Position)((wid->core.width - width)/2.0 + 0.5);
        if(x_co < 0) x_co =0;

        y_co = get_hor_trough_y(wid);
} /* if HOR */

else  if(wid->dsscale.orientation == XmVERTICAL)
{
        /** trough ***/
	if(wid->core.height >  wid->dsscale.scale_height && wid->dsscale.scale_height !=0)
		{
		height = wid->dsscale.scale_height;
		}
	else
		{
		height = wid->core.height - 10;
		}

	save_dimension = wid->composite.children[0]->core.height;
	width = wid->dsscale.slider_thickness;

        x_co = get_ver_trough_x(wid);

	y_co = (Position)((wid->core.height - height)/2.0 + 0.5);
	if(y_co < 0) y_co =0;

} /* if VER */

XtConfigureWidget(wid->composite.children[0], x_co, y_co,width,height,0);

if(wid->dsscale.orientation == XmHORIZONTAL)
	{
	calculate_hor_slider_positions(wid);

        if(save_dimension != wid->composite.children[0]->core.width)
	        {
	          XFreePixmap(XtDisplay((Widget)wid), wid->dsscale.pixmap);
       		  create_trough_pixmap(wid);
	          draw_slot(wid);
       		}
	}

else  if(wid->dsscale.orientation == XmVERTICAL)
	{
	calculate_ver_slider_positions(wid);

        if(save_dimension != wid->composite.children[0]->core.height)
                {
                  XFreePixmap(XtDisplay((Widget)wid), wid->dsscale.pixmap);
                  create_trough_pixmap(wid);
                  draw_slot(wid);
                }

	}

if(XtIsRealized(wid))
	{
	if(wid->dsscale.show_values)
		{
		erase_prev_lower_value(wid);
		erase_prev_upper_value(wid);
		}
	redraw_all(wid);
	}
}

/**********************************/

static Boolean SetValues(Widget current,Widget request, Widget new,ArgList args,Cardinal *nargs)
{
XmDoubleSliderScaleWidget curw=(XmDoubleSliderScaleWidget)current;
XmDoubleSliderScaleWidget neww=(XmDoubleSliderScaleWidget)new;
Arg arg[1];
Position x_co,y_co;
Boolean redraw;


/***** orientation *****/
if(curw->dsscale.orientation == XmVERTICAL && neww->dsscale.orientation == XmHORIZONTAL)
	{
	set_new_hor_orientation(curw,neww);
          XFreePixmap(XtDisplay((Widget)neww), neww->dsscale.pixmap);
          create_trough_pixmap(neww);
          XFreePixmap(XtDisplay((Widget)neww), neww->dsscale.slider);
          create_slider_pixmap(neww);
          draw_slot(neww);
	}

else if(curw->dsscale.orientation == XmHORIZONTAL && neww->dsscale.orientation == XmVERTICAL)
	{
	set_new_ver_orientation(curw,neww);
          XFreePixmap(XtDisplay((Widget)neww), neww->dsscale.pixmap);
          create_trough_pixmap(neww);
          XFreePixmap(XtDisplay((Widget)neww), neww->dsscale.slider);
          create_slider_pixmap(neww);
          draw_slot(neww);
	}

/*** scale alignment *****/

if(curw->dsscale.scale_alignment != neww->dsscale.scale_alignment)
	{
        if(neww->dsscale.scale_alignment!= XmALIGNMENT_BEGINNING && neww->dsscale.scale_alignment!= XmALIGNMENT_CENTER && neww->dsscale.scale_alignment!= XmALIGNMENT_END)
       		{
	        XtWarning("Invalid alignment type");
	        neww->dsscale.scale_alignment = curw->dsscale.scale_alignment;
       		}
	
	if(neww->dsscale.orientation== XmHORIZONTAL)
		{
		y_co = get_hor_trough_y(neww);

		XtMoveWidget(neww->composite.children[0],
			neww->composite.children[0]->core.x, y_co);
		}
        else if(neww->dsscale.orientation== XmVERTICAL)
                {
                x_co = get_ver_trough_x(neww);

                XtMoveWidget(neww->composite.children[0],
                        x_co, neww->composite.children[0]->core.y);
                }
	}

/****** processing direction ****/

if(neww->dsscale.orientation == XmHORIZONTAL && (neww->dsscale.processing_direction != XmMAX_ON_RIGHT && neww->dsscale.processing_direction != XmMAX_ON_LEFT))
   {
     if(curw->dsscale.orientation == XmHORIZONTAL)
	{
	XtWarning("For horizontal orientation the processing direction can be\n\t either XmMAX_ON_RIGHT or XmMAX_ON_LEFT");
	neww->dsscale.processing_direction = curw->dsscale.processing_direction;
	}
     else if(curw->dsscale.orientation == XmVERTICAL)
	{
        neww->dsscale.processing_direction=XmMAX_ON_RIGHT;
	}
   }
else if(neww->dsscale.orientation == XmVERTICAL && (neww->dsscale.processing_direction != XmMAX_ON_TOP && neww->dsscale.processing_direction != XmMAX_ON_BOTTOM))
   {
     if(curw->dsscale.orientation == XmVERTICAL)
	{
	XtWarning("For vertical orientation the processing direction can be\n\t either XmMAX_ON_TOP or XmMAX_ON_BOTTOM");
	neww->dsscale.processing_direction = curw->dsscale.processing_direction;
	}
     else if(curw->dsscale.orientation == XmHORIZONTAL)
        {
        neww->dsscale.processing_direction=XmMAX_ON_TOP;
	}
   }

/*** foreground GC *****/

if(neww->manager.foreground != curw->manager.foreground )
	{
	XtReleaseGC((Widget)curw, curw->dsscale.foreground_GC);
	create_GC(neww);
	draw_slot(neww);
	}

/***  background  ***/

if(neww->core.background_pixel != curw->core.background_pixel)
	{
	change_background_GC(neww);
	draw_slider_shadow(neww);
	draw_slot(neww);

	XtSetArg(arg[0], XmNbackground, neww->core.background_pixel);
	XtSetValues(neww->composite.children[0],arg,1);
	}

/*** font list *************/

if(neww->dsscale.font_list != curw->dsscale.font_list)
	{
	change_GCfonts(neww);
	}

/***** values *****/

        if(neww->dsscale.minimum_value >= neww->dsscale.maximum_value)
        {
        XtWarning("The minimum value is greater than or equal to maximum value");
        neww->dsscale.minimum_value = neww->dsscale.maximum_value -1;
        }

	if(neww->dsscale.upper_value > neww->dsscale.maximum_value)
	{
	XtWarning("The upper value is greater than the maximum value");
	neww->dsscale.upper_value = neww->dsscale.maximum_value;
	}	

        if(neww->dsscale.upper_value < neww->dsscale.minimum_value)
        {
        XtWarning("The upper value is greater than the maximum value");
        neww->dsscale.upper_value = neww->dsscale.minimum_value;
        }

        if(neww->dsscale.lower_value < neww->dsscale.minimum_value)
        {
        XtWarning("The lower value is less than the minimum value"); 
        neww->dsscale.lower_value = neww->dsscale.minimum_value;        
        }       

	if(neww->dsscale.upper_value < neww->dsscale.lower_value)
	{
	XtWarning("The lower value is greater than upper value");
	neww->dsscale.lower_value = neww->dsscale.upper_value;
	}

	/**** scale width and height *****/
if((neww->dsscale.scale_width != curw->dsscale.scale_width ) || (neww->dsscale.scale_height != curw->dsscale.scale_height) || (neww->dsscale.scale_width != curw->dsscale.scale_width && neww->dsscale.scale_height != curw->dsscale.scale_height))
		configure_trough(neww, args, nargs);

if(neww->dsscale.orientation == XmHORIZONTAL)
	{
	calculate_hor_slider_positions(neww);
	}
if(neww->dsscale.orientation == XmVERTICAL)
	{
	calculate_ver_slider_positions(neww);
	}


if(XtIsRealized(curw)) redraw = True;
else redraw = False;

return(redraw); 
}

/**********************************/
static void configure_trough(XmDoubleSliderScaleWidget wid, ArgList args, Cardinal *nargs)
{

Position y_co = 0,x_co = 0;
Dimension width = 0, height = 0,save_width = 0, save_height = 0;

	save_width = wid->composite.children[0]->core.width;
	save_height = wid->composite.children[0]->core.height;

if(wid->dsscale.orientation == XmHORIZONTAL)
{
        /** trough ***/
	if(wid->core.width > wid->dsscale.scale_width && wid->dsscale.scale_width != 0)
		width = wid->dsscale.scale_width;	

        else     width= wid->core.width -10;

	if( wid->dsscale.scale_height !=0)
	{
		if(wid->core.height >= wid->dsscale.scale_height)
		{
		height = wid->dsscale.scale_height;
		}
		else
		{
		XtWarning("The scale height is greater than window height");
		height = wid->core.height;
		}
	wid->dsscale.slider_thickness = height;
	}
	else height =wid->dsscale.slider_thickness;


	x_co = (Position)((wid->core.width - width)/2.0 + 0.5);
        if(x_co < 0) x_co =0;

        y_co = get_hor_trough_y(wid);

	wid->dsscale.slider_size_x = wid->dsscale.slider_length;
	wid->dsscale.slider_size_y = wid->dsscale.slider_thickness;
} /* if HOR */

else  if(wid->dsscale.orientation == XmVERTICAL)
{
        /** trough ***/
	if(wid->core.height >  wid->dsscale.scale_height && wid->dsscale.scale_height !=0)
		height = wid->dsscale.scale_height;

	else    height = wid->core.height - 10;

	if(wid->dsscale.scale_width !=0)
	{
		if(wid->core.height >= wid->dsscale.scale_width)
		{
		width = wid->dsscale.scale_width;
		}
               else
                {
                XtWarning("The scale width is greater than window width");
                width = wid->core.width;
                }
	wid->dsscale.slider_thickness = width;
	}
	else width = wid->dsscale.slider_thickness;

        x_co = get_ver_trough_x(wid);

	y_co = (Position)((wid->core.height - height)/2.0 + 0.5);
	if(y_co < 0) y_co =0;

	wid->dsscale.slider_size_x = wid->dsscale.slider_thickness;
	wid->dsscale.slider_size_y = wid->dsscale.slider_length;
} /* if VER */

XtConfigureWidget(wid->composite.children[0], x_co, y_co,width,height,0);

wid->dsscale.slot_thickness = (int)(wid->dsscale.slider_thickness/2.0 +0.5);
if(wid->dsscale.slot_thickness == 0) wid->dsscale.slot_thickness =1;


XFreePixmap(XtDisplay((Widget)wid), wid->dsscale.pixmap);
XFreePixmap(XtDisplay((Widget)wid), wid->dsscale.slider);
create_trough_pixmap(wid);
create_slider_pixmap(wid);
draw_slot(wid);

}
/***********************************/

static void set_scale_values(XmDoubleSliderScaleWidget nw,XmDoubleSliderScaleWidget rw,ArgList args,Cardinal *nargs)
{
int i;
Boolean assigned;

        if(nw->dsscale.minimum_value >= nw->dsscale.maximum_value)
                {
                XtWarning("The minimum value is greater than or equal to maximum value");
                nw->dsscale.minimum_value = nw->dsscale.maximum_value - 1;
                nw->dsscale.lower_value = nw->dsscale.minimum_value;
                }

                /*** check whether the values have been set ***/
i=0;
assigned=False;
        while(i<(int)(*nargs) &&  !assigned )
        {
                if(strcmp(args[i].name,XmNupperValue)==0)
                        {
                        if(nw->dsscale.upper_value > nw->dsscale.maximum_value)
                                {
                                XtWarning("The upper value is greater than maximum value");
                                nw->dsscale.upper_value = nw->dsscale.maximum_value;
                                }
                        else if(nw->dsscale.upper_value < nw->dsscale.minimum_value)
                                {
                                XtWarning("The upper value is less than minimum value");
                                nw->dsscale.upper_value = nw->dsscale.minimum_value;
                                }
                        assigned = True;
                        } /* if strcmp */
i++;
        } /* while  */
if(!assigned) nw->dsscale.upper_value = nw->dsscale.maximum_value;


i=0;
assigned=False;

        while(i< (int)(*nargs) && !assigned)
        {
                if(strcmp(args[i].name,XmNlowerValue)==0)
                {
                        if(nw->dsscale.lower_value < nw->dsscale.minimum_value)
                        {
                        XtWarning("The lower value is less than minimum value");
                        nw->dsscale.lower_value = nw->dsscale.minimum_value;
                        }
                        else if(nw->dsscale.lower_value > nw->dsscale.upper_value)
                        {
                        XtWarning("The lower value is greater than upper value");
                        nw->dsscale.lower_value = nw->dsscale.upper_value;
                        }
                assigned = True;
                } /* if strcmp */
i++;
        } /* while */

if(!assigned) nw->dsscale.lower_value = nw->dsscale.minimum_value;

}
/****************************************/
static void set_new_hor_orientation(XmDoubleSliderScaleWidget cw,XmDoubleSliderScaleWidget nw)
{
Position y_co,x_co;
Dimension width,height;


        /*** processing direction ****/
        if(nw->dsscale.processing_direction !=XmMAX_ON_RIGHT && nw->dsscale.processing_direction !=XmMAX_ON_LEFT)
        {
        nw->dsscale.processing_direction=XmMAX_ON_RIGHT;
        }

        /** width *****/
        if(nw->core.width < 2*nw->dsscale.slider_length)
        	{
                nw->core.width = 2*nw->dsscale.slider_length +10;
        	}

        /*** height  ****/
        if(nw->core.height < nw->dsscale.slider_thickness)
        	{
                nw->core.height = nw->dsscale.slider_thickness;
        	}

        /** trough ***/
        height=nw->dsscale.slider_thickness;
        width=nw->core.width - 10;
        x_co=5;

	y_co = get_hor_trough_y(nw);

XtConfigureWidget(nw->composite.children[0],
		x_co, y_co,
		width, height, 0);
			
        /** end trough ***/

        nw->dsscale.slider_size_x = nw->dsscale.slider_length;
        nw->dsscale.slider_size_y = nw->dsscale.slider_thickness;
}
/***************************************/
static void set_new_ver_orientation(XmDoubleSliderScaleWidget cw,XmDoubleSliderScaleWidget nw)
{
Position y_co,x_co;
Dimension width,height;

        if(nw->dsscale.processing_direction !=XmMAX_ON_TOP && nw->dsscale.processing_direction !=XmMAX_ON_BOTTOM)
        {
        nw->dsscale.processing_direction=XmMAX_ON_TOP;
        }

        /** width **/
        if(nw->core.width < nw->dsscale.slider_thickness)
        {
                nw->core.width = nw->dsscale.slider_thickness;
        }

        /*** height ***/
        if(nw->core.height < 2*nw->dsscale.slider_length)
        {
                nw->core.height = 2*nw->dsscale.slider_length +10;
        }

        /** trough ***/

        width = nw->dsscale.slider_thickness;

        x_co = get_ver_trough_x(nw);

        height = nw->core.height - 10;
        y_co = 5;

XtConfigureWidget(nw->composite.children[0],
		x_co, y_co,
		width, height, 0);

        nw->dsscale.slider_size_x = nw->dsscale.slider_thickness;
        nw->dsscale.slider_size_y = nw->dsscale.slider_length;
}
/***********************************************/

static Boolean if_set(ArgList args, Cardinal *nargs, char *res)
{
int i;

        for(i=0; i<(int)(*nargs); i++)
        {
                if(strcmp(args[i].name,res)==0) return(True);
        } /* for */
return(False);
}

/************************************************/

static XtGeometryResult GeometryManager(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *prefer)
{

if(request->request_mode & CWX)
		w->core.x = request->x;

if(request->request_mode & CWY)
		w->core.y = request->y;

if(request->request_mode & CWHeight)
		w->core.height = request->height;

if(request->request_mode & CWWidth)
       		w->core.width = request->width;

if(request->request_mode & CWBorderWidth)
	        w->core.border_width = request->border_width;

return(XtGeometryYes);
}

/*************************************************/
static void erase_hor_lower_slider(XmDoubleSliderScaleWidget wid, int newval)
{

int pixel,val_diff;

if(!XtIsRealized(wid)) return;

if(newval < wid->dsscale.minimum_value) newval = wid->dsscale.minimum_value;
if(newval > wid->dsscale.upper_value) newval = wid->dsscale.upper_value;


val_diff = newval - wid->dsscale.lower_value;
 
   switch(wid->dsscale.processing_direction)
   {
   case XmMAX_ON_RIGHT:
	if(val_diff >= 0)
	{
          pixel = (int)(0.5 + (float)(val_diff)*(float)(wid->composite.children[0]->core.width
                                       - 2*wid->dsscale.slider_size_x)/
                                      (float)(wid->dsscale.maximum_value - wid->dsscale.minimum_value));

                XCopyArea(XtDisplay(wid),
                        wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
                        wid->dsscale.foreground_GC,
                        wid->dsscale.lower_x -1,0,
                        pixel+1, wid->composite.children[0]->core.height,
                        wid->dsscale.lower_x -1,0);
	}
	else 
	{
          pixel = (int)(0.5 + (float)(-val_diff)*(float)(wid->composite.children[0]->core.width
                                         - 2*wid->dsscale.slider_size_x)/
                                      (float)(wid->dsscale.maximum_value - wid->dsscale.minimum_value));

                XCopyArea(XtDisplay(wid),
                        wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
                        wid->dsscale.foreground_GC,
                        wid->dsscale.lower_x + wid->dsscale.slider_size_x - pixel,0,
                        pixel+1, wid->composite.children[0]->core.height,
                        wid->dsscale.lower_x + wid->dsscale.slider_size_x - pixel ,0);
	}
	break;

   case XmMAX_ON_LEFT:

        if(val_diff < 0)
        {
          pixel = (int)(0.5 + (float)(-val_diff)*(float)(wid->composite.children[0]->core.width
                                            - 2*wid->dsscale.slider_size_x)/
                                      (float)(wid->dsscale.maximum_value - wid->dsscale.minimum_value));

                XCopyArea(XtDisplay(wid),
                        wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
                        wid->dsscale.foreground_GC,
                        wid->dsscale.lower_x -1 ,0,
                        pixel+1, wid->composite.children[0]->core.height,
                        wid->dsscale.lower_x -1,0);
        }
        else
        {
          pixel = (int)(0.5 + (float)(val_diff)*(float)(wid->composite.children[0]->core.width
                                            - 2*wid->dsscale.slider_size_x)/
                                      (float)(wid->dsscale.maximum_value - wid->dsscale.minimum_value));

                XCopyArea(XtDisplay(wid),
                        wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
                        wid->dsscale.foreground_GC,
                        wid->dsscale.lower_x + wid->dsscale.slider_size_x - pixel,0,
                        pixel+1, wid->composite.children[0]->core.height,
                        wid->dsscale.lower_x + wid->dsscale.slider_size_x - pixel ,0);
        }
        break;

	}
}

/******************************/

static void erase_ver_lower_slider(XmDoubleSliderScaleWidget wid,int newval)
{
int pixel,val_diff;

if(!XtIsRealized(wid)) return;

if(newval < wid->dsscale.minimum_value) newval = wid->dsscale.minimum_value;
if(newval > wid->dsscale.upper_value) newval = wid->dsscale.upper_value;


val_diff = newval - wid->dsscale.lower_value;

   switch(wid->dsscale.processing_direction)
   {
   case XmMAX_ON_BOTTOM:
        if(val_diff >= 0)
        {
          pixel = (int)(0.5 + (float)(val_diff)*(float)(wid->composite.children[0]->core.height
                                            - 2*wid->dsscale.slider_size_y)/
                                      (float)(wid->dsscale.maximum_value - wid->dsscale.minimum_value));

                XCopyArea(XtDisplay(wid),
                        wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
                        wid->dsscale.foreground_GC,
                        0, wid->dsscale.lower_y -1,
                        wid->composite.children[0]->core.width, pixel+1,
                        0,wid->dsscale.lower_y -1);
        }
        else
        {
          pixel = (int)(0.5 + (float)(-val_diff)*(float)(wid->composite.children[0]->core.height
                                         - 2*wid->dsscale.slider_size_y)/
                                      (float)(wid->dsscale.maximum_value - wid->dsscale.minimum_value));

                XCopyArea(XtDisplay(wid),
                        wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
                        wid->dsscale.foreground_GC,
                        0, wid->dsscale.lower_y + wid->dsscale.slider_size_y - pixel,
                        wid->composite.children[0]->core.width, pixel+1,
                        0, wid->dsscale.lower_y + wid->dsscale.slider_size_y - pixel);
        }
        break;

   case XmMAX_ON_TOP:

        if(val_diff < 0)
        {
          pixel = (int)(0.5 + (float)(-val_diff)*(float)(wid->composite.children[0]->core.height
                                            - 2*wid->dsscale.slider_size_y)/
                                      (float)(wid->dsscale.maximum_value - wid->dsscale.minimum_value));

                XCopyArea(XtDisplay(wid),
                        wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
                        wid->dsscale.foreground_GC,
                        0, wid->dsscale.lower_y -1,
                        wid->composite.children[0]->core.width, pixel+1,
                        0,wid->dsscale.lower_y -1);

        }
        else
        {
          pixel = (int)(0.5 + (float)(val_diff)*(float)(wid->composite.children[0]->core.height
                                         - 2*wid->dsscale.slider_size_y)/
                                      (float)(wid->dsscale.maximum_value - wid->dsscale.minimum_value));

                XCopyArea(XtDisplay(wid),
                        wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
                        wid->dsscale.foreground_GC,
                        0, wid->dsscale.lower_y + wid->dsscale.slider_size_y - pixel,
                        wid->composite.children[0]->core.width, pixel+1,
                        0, wid->dsscale.lower_y + wid->dsscale.slider_size_y - pixel);
        }
        break;

        }

}

static void erase_hor_upper_slider(XmDoubleSliderScaleWidget wid,int newval)
{

int pixel,val_diff;

if(!XtIsRealized(wid)) return;

if(newval > wid->dsscale.maximum_value) newval = wid->dsscale.maximum_value;
if(newval < wid->dsscale.lower_value) newval = wid->dsscale.lower_value;

val_diff = newval - wid->dsscale.upper_value;

   switch(wid->dsscale.processing_direction)
   {
   case XmMAX_ON_RIGHT:
        if(val_diff >= 0)
        {
          pixel = (int)(0.5 + (float)(val_diff)*(float)(wid->composite.children[0]->core.width
                                       - 2*wid->dsscale.slider_size_x)/
                                      (float)(wid->dsscale.maximum_value - wid->dsscale.minimum_value));

                XCopyArea(XtDisplay(wid),
                        wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
                        wid->dsscale.foreground_GC,
                        wid->dsscale.upper_x -1 ,0,
                        pixel+1, wid->composite.children[0]->core.height,
                        wid->dsscale.upper_x -1,0);
        }
        else
        {
          pixel = (int)(0.5 + (float)(-val_diff)*(float)(wid->composite.children[0]->core.width
                                          - 2*wid->dsscale.slider_size_x)/
                                      (float)(wid->dsscale.maximum_value - wid->dsscale.minimum_value));

                XCopyArea(XtDisplay(wid),
                        wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
                        wid->dsscale.foreground_GC,
                        wid->dsscale.upper_x + wid->dsscale.slider_size_x - pixel,0,
                        pixel+1, wid->composite.children[0]->core.height,
                        wid->dsscale.upper_x + wid->dsscale.slider_size_x - pixel,0);
        }
        break;

   case XmMAX_ON_LEFT:

        if(val_diff < 0)
        {
          pixel = (int)(0.5 + (float)(-val_diff)*(float)(wid->composite.children[0]->core.width
                                       - 2*wid->dsscale.slider_size_x)/
                                      (float)(wid->dsscale.maximum_value - wid->dsscale.minimum_value));

                XCopyArea(XtDisplay(wid),
                        wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
                        wid->dsscale.foreground_GC,
                        wid->dsscale.upper_x -1,0,
                        pixel+1, wid->composite.children[0]->core.height,
                        wid->dsscale.upper_x -1,0);
        }
        else
        {
          pixel = (int)(0.5 + (float)(val_diff)*(float)(wid->composite.children[0]->core.width
                                          - 2*wid->dsscale.slider_size_x)/
                                      (float)(wid->dsscale.maximum_value - wid->dsscale.minimum_value));

                XCopyArea(XtDisplay(wid),
                        wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
                        wid->dsscale.foreground_GC,
                        wid->dsscale.upper_x + wid->dsscale.slider_size_x - pixel,0,
                        pixel+1, wid->composite.children[0]->core.height,
                        wid->dsscale.upper_x + wid->dsscale.slider_size_x - pixel ,0);
        }
        break;

        }

}

static void erase_ver_upper_slider(XmDoubleSliderScaleWidget wid,int newval)
{
int pixel,val_diff;

if(!XtIsRealized(wid)) return;

if(newval > wid->dsscale.maximum_value) newval = wid->dsscale.maximum_value;
if(newval < wid->dsscale.lower_value) newval = wid->dsscale.lower_value;

val_diff = newval - wid->dsscale.upper_value;

   switch(wid->dsscale.processing_direction)
   {
   case XmMAX_ON_BOTTOM:
        if(val_diff >= 0)
        {
          pixel = (int)(0.5 + (float)(val_diff)*(float)(wid->composite.children[0]->core.height
                                         - 2*wid->dsscale.slider_size_y)/
                                      (float)(wid->dsscale.maximum_value - wid->dsscale.minimum_value));

                XCopyArea(XtDisplay(wid),
                        wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
                        wid->dsscale.foreground_GC,
                        0, wid->dsscale.upper_y -1,
                        wid->composite.children[0]->core.width, pixel+1,
                        0,wid->dsscale.upper_y -1);
        }
        else
        {
          pixel = (int)(0.5 + (float)(-val_diff)*(float)(wid->composite.children[0]->core.height
                                          - 2*wid->dsscale.slider_size_y)/
                                      (float)(wid->dsscale.maximum_value - wid->dsscale.minimum_value));

                XCopyArea(XtDisplay(wid),
                        wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
                        wid->dsscale.foreground_GC,
                        0, wid->dsscale.upper_y + wid->dsscale.slider_size_y - pixel,
                        wid->composite.children[0]->core.width, pixel+1,
                        0, wid->dsscale.upper_y + wid->dsscale.slider_size_y - pixel);
        }
        break;
   case XmMAX_ON_TOP:

        if(val_diff < 0)
        {
          pixel = (int)(0.5 + (float)(-val_diff)*(float)(wid->composite.children[0]->core.height
                                              - 2*wid->dsscale.slider_size_y)/
                                      (float)(wid->dsscale.maximum_value - wid->dsscale.minimum_value));

                XCopyArea(XtDisplay(wid),
                        wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
                        wid->dsscale.foreground_GC,
                        0, wid->dsscale.upper_y -1,
                        wid->composite.children[0]->core.width, pixel+1,
                        0,wid->dsscale.upper_y -1);

        }
        else
        {
          pixel = (int)(0.5 + (float)(val_diff)*(float)(wid->composite.children[0]->core.height
                                          - 2*wid->dsscale.slider_size_y)/
                                      (float)(wid->dsscale.maximum_value - wid->dsscale.minimum_value));

                XCopyArea(XtDisplay(wid),
                        wid->dsscale.pixmap,XtWindow(wid->composite.children[0]),
                        wid->dsscale.foreground_GC,
                        0, wid->dsscale.upper_y + wid->dsscale.slider_size_y - pixel,
                        wid->composite.children[0]->core.width, pixel+1,
                        0, wid->dsscale.upper_y + wid->dsscale.slider_size_y - pixel);
        }
        break;

        }

}

static void draw_hor_lower_slider(XmDoubleSliderScaleWidget wid)
{
if(!XtIsRealized(wid)) return;

        XCopyArea(XtDisplay(wid),
                        wid->dsscale.slider,XtWindow(wid->composite.children[0]),
                        wid->manager.background_GC,
                        0,0,
                        wid->dsscale.slider_size_x,wid->dsscale.slider_size_y,
                        wid->dsscale.lower_x,0);
}

static void draw_ver_lower_slider(XmDoubleSliderScaleWidget wid)
{
if(!XtIsRealized(wid)) return;

        XCopyArea(XtDisplay(wid),
                        wid->dsscale.slider,XtWindow(wid->composite.children[0]),
                        wid->manager.background_GC,
                        0,0,
                        wid->dsscale.slider_size_x,wid->dsscale.slider_size_y,
                        0,wid->dsscale.lower_y);


}

static void draw_hor_upper_slider(XmDoubleSliderScaleWidget wid)
{
if(!XtIsRealized(wid)) return;

        XCopyArea(XtDisplay(wid),
                        wid->dsscale.slider,XtWindow(wid->composite.children[0]),
                        wid->manager.background_GC,
                        0,0,
                        wid->dsscale.slider_size_x,wid->dsscale.slider_size_y,
                        wid->dsscale.upper_x,0);

}

static void draw_ver_upper_slider(XmDoubleSliderScaleWidget wid)
{
if(!XtIsRealized(wid)) return;

        XCopyArea(XtDisplay(wid),
                        wid->dsscale.slider,XtWindow(wid->composite.children[0]),
                        wid->manager.background_GC,
                        0,0,
                        wid->dsscale.slider_size_x,wid->dsscale.slider_size_y,
                        0,wid->dsscale.upper_y);
}



/******** APPLICATION FUNCTIONS ****************/

extern void XmDoubleSliderScaleSetLowerValue(Widget w, int value)
{
XmDoubleSliderScaleWidget wid=(XmDoubleSliderScaleWidget)w; 

if(wid->dsscale.show_values) erase_prev_lower_value(wid);


if(wid->dsscale.orientation == XmHORIZONTAL)
                 erase_hor_lower_slider(wid,value);

else if(wid->dsscale.orientation == XmVERTICAL)
                 erase_ver_lower_slider(wid,value);

if(value >= wid->dsscale.minimum_value)
{
  wid->dsscale.lower_value = (value > wid->dsscale.upper_value) ? wid->dsscale.upper_value: value;
}
else wid->dsscale.lower_value = wid->dsscale.minimum_value;

if(wid->dsscale.orientation == XmHORIZONTAL)
	{
	calculate_hor_slider_positions(wid);
                 draw_hor_lower_slider(wid);
	}

else if(wid->dsscale.orientation == XmVERTICAL)
	{
	calculate_ver_slider_positions(wid);
                 draw_ver_lower_slider(wid);
	}

if(wid->dsscale.show_values) show_lower_value(wid);

}

extern void XmDoubleSliderScaleSetUpperValue(Widget w, int value)
{
XmDoubleSliderScaleWidget wid=(XmDoubleSliderScaleWidget)w; 

if(wid->dsscale.show_values) erase_prev_upper_value(wid);

if(wid->dsscale.orientation == XmHORIZONTAL)
                 erase_hor_upper_slider(wid,value);

else if(wid->dsscale.orientation == XmVERTICAL)
                 erase_ver_upper_slider(wid,value);

if(value <= wid->dsscale.maximum_value)
{
   wid->dsscale.upper_value = (value < wid->dsscale.lower_value) ? wid->dsscale.lower_value: value;
}
else wid->dsscale.upper_value = wid->dsscale.maximum_value;

if(wid->dsscale.orientation == XmHORIZONTAL)
	{
	calculate_hor_slider_positions(wid);
                 draw_hor_upper_slider(wid);
	}

else if(wid->dsscale.orientation == XmVERTICAL)
	{
	calculate_ver_slider_positions(wid);
                 draw_ver_upper_slider(wid);
	}

if(wid->dsscale.show_values) show_upper_value(wid);

}

extern void XmDoubleSliderScaleGetLowerValue(Widget w, int *value)
{
XmDoubleSliderScaleWidget wid=(XmDoubleSliderScaleWidget)w; 

*value = wid->dsscale.lower_value;
}

extern void XmDoubleSliderScaleGetUpperValue(Widget w, int *value)
{
XmDoubleSliderScaleWidget wid=(XmDoubleSliderScaleWidget)w; 

*value = wid->dsscale.upper_value;
}

/*********************END OF FILE *********************/

