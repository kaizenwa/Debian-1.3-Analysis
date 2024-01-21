/* 
 * Frame.c - Frame composite widget
 * 
 */

#include <stdio.h>

#include "paths.h"
#include INC_X11(IntrinsicP.h)
#include INC_X11(StringDefs.h)
#include INC_XMU(Converters.h)
#include INC_XMU(CharSet.h)
#include INC_XAW(XawInit.h)
#include "FrameP.h"

/*
#define MESSAGES
*/
#include "message.h"

/****************************************************************
 *
 * Frame Resources
 *
 ****************************************************************/
#define offset(name) XtOffsetOf(FrameRec, frame.name)
static XtResource resources[] = {
    {XtNhSpace, XtCHSpace, XtRDimension, sizeof(Dimension),
        offset(h_space_nat), XtRImmediate, (XtPointer)4 },
    {XtNvSpace, XtCVSpace, XtRDimension, sizeof(Dimension),
        offset(v_space_nat), XtRImmediate, (XtPointer)4 },
    {XtNframeType, XtCFrameType, XtRFrameType, sizeof(XawFrameType),
        offset(frame_type), XtRImmediate, (XtPointer) XawCHISELED },
    {XtNshadowWidth, XtCShadowWidth, XtRDimension, sizeof(Dimension),
	offset(shadow_width_nat), XtRImmediate, (XtPointer) 2},
    {XtNtopShadowPixel, XtCTopShadowPixel, XtRPixel, sizeof(Pixel),
	offset(top_shadow_pixel), XtRString, XtDefaultForeground},
    {XtNbottomShadowPixel, XtCBottomShadowPixel, XtRPixel, sizeof(Pixel),
	offset(bot_shadow_pixel), XtRString, XtDefaultForeground},
    {XtNresize, XtCBoolean, XtRBoolean, sizeof(Boolean),
	offset(resize), XtRImmediate, (XtPointer) True},
};
#undef offset

#define FW_FRAME       fw->frame
#define FW_CORE        fw->core
#define FW_COMPOSITE   fw->composite
  
#define FW_HSPACE      FW_FRAME.h_space
#define FW_VSPACE      FW_FRAME.v_space
#define FW_SHADOW      FW_FRAME.shadow_width
#define FW_RESIZE      FW_FRAME.resize

#define FW_NAT_HSPACE  FW_FRAME.h_space_nat
#define FW_NAT_VSPACE  FW_FRAME.v_space_nat
#define FW_NAT_SHADOW  FW_FRAME.shadow_width_nat
#define FW_CHILD_NAT_WIDTH FW_FRAME.child_width_nat
#define FW_CHILD_NAT_HEIGHT FW_FRAME.child_height_nat
#define FW_CHILD_NAT_BORDER FW_FRAME.child_border_nat

#define FW_CHILD_P     FW_COMPOSITE.children
#define FW_CHILD       (*(FW_CHILD_P))

/***************************************************************************
 *
 * Frame  class record
 *
 ***************************************************************************/

static void ClassInitialize();
static void Resize();
static void Redisplay();
static void Initialize();
static void InsertChild();
static void ChangeManaged();
static XtGeometryResult GeometryManager();
static XtGeometryResult QueryGeometry();
static XtGeometryResult LayoutFrame();
static void Destroy();
static void GetDesiredSizeOfChild();
static void GetNaturalSize();

#define SuperClass ((CompositeWidgetClass)&compositeClassRec)

FrameClassRec frameClassRec = {
  {
    /* superclass	  */	(WidgetClass)SuperClass,
    /* class_name	  */	"Frame",
    /* size		  */	sizeof(FrameRec),
    /* class_initialize	  */	ClassInitialize,
    /* class_part_init    */	NULL,
    /* class inited	  */	FALSE,
    /* initialize	  */	Initialize,
    /* initialize_hook	  */	NULL,		
    /* realize		  */	XtInheritRealize,
    /* actions		  */	NULL,
    /* num_actions	  */	0,
    /* resources	  */	resources,
    /* resource_count	  */	XtNumber(resources),
    /* xrm_class	  */	NULLQUARK,
    /* compress_motion	  */	FALSE,
    /* compress_exposure  */	XtExposeCompressMultiple,
    /* compress_enterleave*/	FALSE,
    /* visible_interest	  */	FALSE,
    /* destroy		  */	Destroy,
    /* resize		  */	Resize,
    /* expose		  */	Redisplay,
    /* set_values	  */	NULL,
    /* set_values_hook	  */	NULL,			
    /* set_values_almost  */	XtInheritSetValuesAlmost,  
    /* get_values_hook	  */	NULL,
    /* accept_focus	  */	NULL,
    /* intrinsics version */	XtVersion,
    /* callback offsets	  */	NULL,
    /* tm_table		  */	NULL,
    /* query_geometry	  */	QueryGeometry,
    /* display_accelerator*/	XtInheritDisplayAccelerator,
    /* extension	  */	NULL
  },
  { /* composite_class fields */
    /* geometry_manager	  */	GeometryManager,
    /* change_managed	  */	ChangeManaged,
    /* insert_child	  */	InsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension	  */	NULL
  },
  { /* frame_class fields */
    /* dummy              */    NULL
  }
};

WidgetClass frameWidgetClass =	(WidgetClass) (&frameClassRec);

/****************************************************************
 * Private Routines
 ****************************************************************/

/*---------------------------------------------------*/
/* cvtStringToFrameType */
/*---------------------------------------------------*/

#define done(type, value)  {			\
   if (to->addr != NULL) {			\
      if (to->size < sizeof(type)) {		\
	 to->size = sizeof(type);		\
	 return False;				\
      }						\
      *(type*)(to->addr) = (value);		\
   } else {					\
      static type static_val;			\
      static_val = (value);			\
      to->addr = (XtPointer)&static_val;	\
   }						\
   to->size = sizeof(type);			\
   ENDMESSAGE(cvtStringToFrameType)		\
   return True;					\
}

static  Boolean
cvtStringToFrameType ( display, args, num_args,  from, to, converter_data)
   Display *display;
   XrmValuePtr args;
   Cardinal *num_args;
   XrmValuePtr from;
   XrmValuePtr to;
   XtPointer *converter_data;
{
   String s = (String) from->addr;

   BEGINMESSAGE(cvtStringToFrameType)
   if (XmuCompareISOLatin1(s, "raised")   == 0) done(XawFrameType, XawRAISED);
   if (XmuCompareISOLatin1(s, "sunken")   == 0) done(XawFrameType, XawSUNKEN);
   if (XmuCompareISOLatin1(s, "chiseled") == 0) done(XawFrameType, XawCHISELED);
   if (XmuCompareISOLatin1(s, "ledged")   == 0) done(XawFrameType, XawLEDGED);
   if (XmuCompareISOLatin1(s, "massiveRaised") == 0) done(XawFrameType,XawFrameMassiveRaised);
   XtDisplayStringConversionWarning(display, s, XtRFrameType);
   done(XawFrameType, XawRAISED);
}

/*---------------------------------------------------*/
/* ClassInitialize */
/*---------------------------------------------------*/

static void
ClassInitialize()
{
   BEGINMESSAGE(ClassInitialize)
   XawInitializeWidgetSet();
   XtSetTypeConverter(XtRString, XtRFrameType, cvtStringToFrameType,
		     (XtConvertArgList)NULL, 0, XtCacheNone, NULL);
   ENDMESSAGE(ClassInitialize)
}

/*---------------------------------------------------*/
/* Initialize */
/*---------------------------------------------------*/

GC
shadow_getGC (w,pixel)
    Widget w;
    Pixel  pixel;
{
   Screen    *scn = XtScreen (w);
   XtGCMask  valuemask;
   XGCValues myXGCV;
   GC        gc;

   BEGINMESSAGE(shadow_getGC)
   if (DefaultDepthOfScreen(scn) > 1) {
      valuemask = GCForeground;
      myXGCV.foreground = pixel;
      gc = XtGetGC(w, valuemask, &myXGCV);
   }
   else gc = (GC) NULL;
   ENDMESSAGE(shadow_getGC)
   return gc;
}

static void
Initialize(request, new, args, num_args)
   Widget request, new;
   ArgList args;
   Cardinal *num_args;
{
   FrameWidget fw = (FrameWidget) new;

   BEGINMESSAGE(Initialize)
   FW_FRAME.top_shadow_GC = shadow_getGC(new,FW_FRAME.top_shadow_pixel);
   FW_FRAME.bot_shadow_GC = shadow_getGC(new,FW_FRAME.bot_shadow_pixel);
   if (!FW_FRAME.top_shadow_GC || !FW_FRAME.bot_shadow_GC) {
      INFMESSAGE(disallowing shadows)
      FW_FRAME.shadow_width_nat = 0;
   }

   if (FW_CORE.width == 0)  FW_CORE.width  = 1;  
   if (FW_CORE.height == 0) FW_CORE.height = 1;
   FW_SHADOW = FW_HSPACE = FW_VSPACE = 0;
   FW_CHILD_NAT_WIDTH = FW_CHILD_NAT_HEIGHT = FW_CHILD_NAT_BORDER = 0;

   ENDMESSAGE(Initialize)
}

/*---------------------------------------------------*/
/* Destroy */
/*---------------------------------------------------*/

static void
Destroy (w)
   Widget w;
{
   FrameWidget fw = (FrameWidget) w;

   BEGINMESSAGE(Destroy)
   XtReleaseGC(w,FW_FRAME.top_shadow_GC);
   XtReleaseGC(w,FW_FRAME.bot_shadow_GC);
   ENDMESSAGE(Destroy)
}

/*---------------------------------------------------*/
/* Resize */
/*---------------------------------------------------*/

#define MIN_CHILD 4

static void 
Resize(w)
   Widget w;
{
   int x,y,cw,ch,hs,vs,bw,sw;
   FrameWidget fw = (FrameWidget) w;

   BEGINMESSAGE(Resize)

   hs  = (int)(FW_NAT_HSPACE);
   vs  = (int)(FW_NAT_VSPACE);
   sw  = (int)(FW_NAT_SHADOW);
   bw  = (int)(FW_CHILD_NAT_BORDER);

   ch  = (int)(FW_CORE.height) - 2*vs -2*sw;
   cw  = (int)(FW_CORE.width)  - 2*hs -2*sw;

   if ((ch<MIN_CHILD || cw<MIN_CHILD) && (hs>0||vs>0||sw>0)) {
      INFMESSAGE(adjusting size of decorations)
      while (ch<MIN_CHILD && vs>0) { ch +=2; --vs; }
      while (cw<MIN_CHILD && hs>0) { cw +=2; --hs; }
      while ((cw<MIN_CHILD || ch<MIN_CHILD) && sw>0) { ch+=2; cw +=2; --sw; }
   }

   if (cw <= 0 || ch <= 0) {
      INFMESSAGE(child too small; will place it off screen)
      cw = ch = 1;
      sw = hs = vs = 0;
      x = -1 - 2*bw;
      y = -1 - 2*bw;
   } else {
      x = hs+sw-bw;
      y = vs+sw-bw;
   }
   IIMESSAGE(hs,vs)
   IIMESSAGE(cw,ch)
   IIMESSAGE(bw,sw)

   FW_HSPACE= (Dimension) hs;
   FW_VSPACE= (Dimension) vs;
   FW_SHADOW= (Dimension) sw;

   XtConfigureWidget(FW_CHILD,x,y,((Dimension)cw),((Dimension)ch),((Dimension)bw));

   ENDMESSAGE(Resize)
}

/*---------------------------------------------------*/
/* Redisplay */
/*---------------------------------------------------*/

#define topPolygon(i,xx,yy)		\
  top_polygon[i].x = (short) (xx);	\
  top_polygon[i].y = (short) (yy)

#define bottomPolygon(i,xx,yy)		\
  bottom_polygon[i].x = (short) (xx);	\
  bottom_polygon[i].y = (short) (yy)

static void 
DrawFrame (gw, x, y, w, h, frame_type, t, lightgc, darkgc)
   Widget       gw;
   Position     x;
   Position     y;
   Dimension    w; 
   Dimension    h;
   XawFrameType frame_type;
   Dimension    t;
   GC           lightgc;
   GC           darkgc;
{
   XPoint top_polygon[6];
   XPoint bottom_polygon[6];
  
   BEGINMESSAGE(DrawFrame)

   if (lightgc == (GC)NULL ){
      XtWarning("DrawFrame: lightgc is NULL.");
      return;
   }
   if (darkgc == (GC)NULL ){
      XtWarning("DrawFrame: darkgc is NULL.");
      return;
   }
   if (!XtIsRealized(gw)) {
      XtWarning("DrawFrame: widget is not realized.");
      return;
   }
  
   if (frame_type == XawRAISED || frame_type == XawSUNKEN ) {
      topPolygon (0,x    ,y    ); bottomPolygon (0,x+w  ,y+h  ); 
      topPolygon (1,x+w  ,y    ); bottomPolygon (1,x    ,y+h  );
      topPolygon (2,x+w-t,y+t  ); bottomPolygon (2,x+t  ,y+h-t);
      topPolygon (3,x+t  ,y+t  ); bottomPolygon (3,x+w-t,y+h-t);
      topPolygon (4,x+t  ,y+h-t); bottomPolygon (4,x+w-t,y+t  );
      topPolygon (5,x    ,y+h  ); bottomPolygon (5,x+w  ,y    );
      if (frame_type == XawSUNKEN) {
         XFillPolygon(XtDisplayOfObject(gw), XtWindowOfObject(gw), darkgc,
		      top_polygon, 6, Nonconvex, CoordModeOrigin);
         XFillPolygon(XtDisplayOfObject(gw), XtWindowOfObject(gw), lightgc,
	    	      bottom_polygon, 6, Nonconvex, CoordModeOrigin);
      } else {
	 XFillPolygon(XtDisplayOfObject(gw), XtWindowOfObject(gw), lightgc,
		      top_polygon, 6, Nonconvex, CoordModeOrigin);
	 XFillPolygon(XtDisplayOfObject(gw), XtWindowOfObject(gw), darkgc,
		      bottom_polygon, 6, Nonconvex, CoordModeOrigin);
      }
   }
   else if ( frame_type == XawFrameMassiveRaised) {
     if (t>=3) {
        Dimension it,mt,ot;
        ot = 1;
        it = 1;
        mt = t-ot-it;
        DrawFrame(gw, x, y, w, h, XawRAISED, ot, lightgc, darkgc);
        DrawFrame(gw,(Position)(x+mt+ot), (Position)(y+mt+ot),
		  (Dimension)(w-2*mt-2*ot), (Dimension)(h-2*mt-2*ot),
		  XawSUNKEN, it, lightgc, darkgc);
     }
   }
   else if ( frame_type == XawLEDGED ) {
     Dimension it,ot;
     it = ot = t/2;
     if (t&1) it += 1;
     DrawFrame(gw, x, y, w, h, XawRAISED, ot, lightgc, darkgc);
     DrawFrame(gw,(Position)(x+ot), (Position)(y+ot),
		  (Dimension)(w-2*ot), (Dimension)(h-2*ot),
		  XawSUNKEN, it, lightgc, darkgc);
   }
   else if ( frame_type == XawCHISELED ) {
     Dimension it,ot;
     it = ot = t/2;
     if (t&1) it += 1;
     DrawFrame(gw, x, y, w, h, XawSUNKEN, ot, lightgc, darkgc);
     DrawFrame(gw,(Position)(x+ot),(Position)(y+ot),
		  (Dimension)(w-2*ot), (Dimension)(h-2*ot),
		  XawRAISED, it, lightgc, darkgc);
   }

   ENDMESSAGE(DrawFrame)

}
#undef topPolygon
#undef bottomPolygon

static void
Redisplay(w, event, region)
   Widget w;
   XEvent *event;		/* unused */
   Region region;		/* unused */
{
   FrameWidget fw = (FrameWidget) w;
   int wh,ww,sw,bw;

   BEGINMESSAGE(Redisplay)

   bw = (int)FW_CORE.border_width;
   ww = (int)FW_CORE.width;
   wh = (int)FW_CORE.height;
   sw = (int)FW_SHADOW; 
   if (sw == 0 || 2*sw>ww || 2*sw>wh) {
      INFMESSAGE(not enough space to display anything) ENDMESSAGE(Redisplay)
      return;
   }
   DrawFrame(w,
	     (Position)0,
	     (Position)0,
	     (Dimension)ww,
	     (Dimension)wh,
	     FW_FRAME.frame_type,
	     (Dimension)sw,
	     FW_FRAME.top_shadow_GC,
	     FW_FRAME.bot_shadow_GC);
   ENDMESSAGE(Redisplay)
}

/*---------------------------------------------------*/
/* GetDesiredSizeOfChild */
/*---------------------------------------------------*/

static void
GetDesiredSizeOfChild(child)
   Widget child;
{
   FrameWidget fw;

   BEGINMESSAGE(GetDesiredSizeOfChild)
   fw = (FrameWidget) XtParent(child);
   if (XtIsManaged(child)) {
      XtWidgetGeometry desired;
      INFSMESSAGE(is managed,XtName(child))
      XtQueryGeometry (child, (XtWidgetGeometry *)NULL, &desired);
      FW_CHILD_NAT_BORDER = desired.border_width;
      FW_CHILD_NAT_WIDTH  = desired.width;
      FW_CHILD_NAT_HEIGHT = desired.height;
   } else {
      INFSMESSAGE(not managed,XtName(child))
      FW_CHILD_NAT_BORDER = 0;
      FW_CHILD_NAT_WIDTH  = 0;
      FW_CHILD_NAT_HEIGHT = 0;
   }
   IIMESSAGE(FW_CHILD_NAT_WIDTH,FW_CHILD_NAT_HEIGHT)
   IMESSAGE(FW_CHILD_NAT_BORDER)
   ENDMESSAGE(GetDesiredSizeOfChild)
}

/*---------------------------------------------------*/
/* InsertChild */
/*---------------------------------------------------*/

static void
InsertChild(child)
   Widget  child;
{
   BEGINMESSAGE(InsertChild)
   (*SuperClass->composite_class.insert_child) (child);
   GetDesiredSizeOfChild(child);
   ENDMESSAGE(InsertChild)
}

/*---------------------------------------------------*/
/* GetNaturalSize */
/*---------------------------------------------------*/

static void
GetNaturalSize(fw,wP,hP)
   FrameWidget fw;
   Dimension *wP;
   Dimension *hP;
{
   BEGINMESSAGE(GetNaturalSize)
   *wP = FW_CHILD_NAT_WIDTH  + 2*FW_NAT_SHADOW +2*FW_NAT_HSPACE;
   *hP = FW_CHILD_NAT_HEIGHT + 2*FW_NAT_SHADOW +2*FW_NAT_VSPACE;
   ENDMESSAGE(GetNaturalSize)
}

/*---------------------------------------------------*/
/* ChangeManaged */
/*---------------------------------------------------*/

static void
ChangeManaged(w)
   Widget w;
{
   FrameWidget fw = (FrameWidget) w;

   BEGINMESSAGE(ChangeManaged)
   GetDesiredSizeOfChild(FW_CHILD);
   LayoutFrame(fw);
   ENDMESSAGE(ChangeManaged)
}

/*---------------------------------------------------*/
/* GeometryManager */
/*---------------------------------------------------*/

#define IS_REQUEST(fff) (request->request_mode & fff)

static XtGeometryResult
GeometryManager(child,request,geometry_return)
   Widget child;
   XtWidgetGeometry *request, *geometry_return;
{
   FrameWidget fw;
   XtGeometryResult answer;
   int changed;

   BEGINMESSAGE(GeometryManager)

   INFSMESSAGE(received request from child, XtName(child))

   if (!(request->request_mode & (CWWidth | CWHeight | CWBorderWidth))) {
      INFMESSAGE(request not of interest) ENDMESSAGE(GeometryManager)
      return XtGeometryYes;
   }
   if (request->request_mode & XtCWQueryOnly) {
      /* query requests are not properly implemented ... ###jp### */
      INFMESSAGE(request is query only and will be denied) ENDMESSAGE(GeometryManager)
      return XtGeometryNo;
   }
   INFIIMESSAGE(current size of child:,child->core.width,child->core.height)

   fw = (FrameWidget) XtParent(child);
   changed = 0;
   if (IS_REQUEST(CWBorderWidth)) {
      IIMESSAGE(request->border_width,child->core.border_width)
      FW_CHILD_NAT_BORDER = request->border_width;
      if (FW_CHILD_NAT_BORDER != child->core.border_width) changed = 1;
   }
   if (IS_REQUEST(CWWidth)) {
      IIMESSAGE(request->width,child->core.width)
      FW_CHILD_NAT_WIDTH = request->width;
      if (FW_CHILD_NAT_WIDTH != child->core.width) changed = 1;
   }
   if (IS_REQUEST(CWHeight)) {
      IIMESSAGE(request->height,child->core.height)
      FW_CHILD_NAT_HEIGHT = request->height;
      if (FW_CHILD_NAT_HEIGHT != child->core.height) changed = 1;
   }

   if (changed) {
      answer = LayoutFrame(fw);
      INFIIMESSAGE(new size of child:,child->core.width,child->core.height)
      INFIMESSAGE(new border width of child:,child->core.border_width)
      ENDMESSAGE(GeometryManager)
      return answer;
   } else {
      ENDMESSAGE(GeometryManager)
      return XtGeometryYes;
   }
}

/*---------------------------------------------------*/
/* QueryGeometry */
/*---------------------------------------------------*/

static XtGeometryResult
QueryGeometry(w,request,preferred_return)
   Widget w;
   XtWidgetGeometry *request, *preferred_return;
{
   FrameWidget fw = (FrameWidget)w;
   Dimension nw,nh;
   BEGINMESSAGE(QueryGeometry)

   GetNaturalSize(fw,&nw,&nh);
   preferred_return->request_mode = (CWWidth|CWHeight);
   preferred_return->width  = nw;
   preferred_return->height = nh; 
   if (    !(request->request_mode & CWWidth)
        || !(request->request_mode & CWHeight) 
        || ((request->request_mode & CWWidth)  && nw != request->width)
        || ((request->request_mode & CWHeight) && nh != request->height)
      ) {
      INFMESSAGE(XtGeometryAlmost) ENDMESSAGE(QueryGeometry)
      return XtGeometryAlmost;
   }
   if ((nw == w->core.width) && (nh == w->core.height)) {
      INFMESSAGE(XtGeometryNo) ENDMESSAGE(QueryGeometry)
      return XtGeometryNo;
   }

   INFMESSAGE(XtGeometryYes) ENDMESSAGE(QueryGeometry)
   return XtGeometryYes;
}

/*---------------------------------------------------*/
/* LayoutFrame */
/*---------------------------------------------------*/

static XtGeometryResult
LayoutFrame(fw)
   FrameWidget fw;
{
   XtWidgetGeometry request;
   XtGeometryResult answer;

   BEGINMESSAGE(LayoutFrame)

   GetNaturalSize(fw,&request.width,&request.height);

   if (FW_RESIZE && (request.width != fw->core.width || request.height != fw->core.height)) {
      request.request_mode = (CWWidth | CWHeight);
      INFMESSAGE(will request new geometry from parent)
      answer = XtMakeGeometryRequest((Widget) fw, &request, &request);
      switch (answer) {
         case XtGeometryYes:
            INFMESSAGE(XtGeometryYes)
            INFMESSAGE(parent reconfigured window)
            break;
         case XtGeometryAlmost:
            INFIIMESSAGE(XtGeometryAlmost:,request.width,request.height)
            INFMESSAGE(requesting approval of these values)
            answer = XtMakeGeometryRequest((Widget) fw, &request, &request);
            if (answer!=XtGeometryYes) {
               INFIIMESSAGE(parent proposes,request.width,request.height)
               fprintf(stderr,"FrameWidget: Warning, parent didn't accept the size he proposed.");
               INFMESSAGE(giving up)
               answer = XtGeometryNo;
            } else {
               INFMESSAGE(XtGeometryYes)
               INFMESSAGE(parent reconfigured window)
            }
            break;
         case XtGeometryNo:
            INFMESSAGE(XtGeometryNo)
            answer = XtGeometryNo;
            break;
         case XtGeometryDone:
            INFMESSAGE(XtGeometryDone)
	      /* never reached */
            break;
      }
      if (answer == XtGeometryYes) {
         Resize((Widget)fw);
         answer = XtGeometryDone;
      }
   } else {
     INFMESSAGE(XtGeometryDone)
     Resize((Widget)fw);
     answer = XtGeometryDone;
   }

   ENDMESSAGE(LayoutFrame)
   return answer;
}
