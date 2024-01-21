//===============================================================
// vbaseGLCanvas - a basic GL window canvas for drawing
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

// @@#include <v/vbglcnv.h>		// our header
#include "vbglcnv.h"		// our header
#include <v/vctlclrs.h>

extern "C"
{

// OpenGL stuff here

#ifdef Motif
#include <Xm/ScrollBar.h>
#include <Xm/DrawingA.h>
#define sbWidgetClass xmScrollBarWidgetClass
#define sbThickness 15
#else
#include <X11/Xaw/Paned.h>

#ifdef USE3D
#include <v/awscl3d.h>
#define sbWidgetClass scrollbar3dWidgetClass
#define sbThickness 15
#else
#include <X11/Xaw/Scrollbar.h>
#define sbWidgetClass scrollbarWidgetClass
#define sbThickness 10
#endif
#endif

#include <v/canvas.h>		// our own canvas widget

}
#include <v/vapp.h>		// need access to the app
#include <v/vwindow.h>		// we are part of a vWindow

    VCursor vBaseGLCanvasPane::_currentCursor = -1;	// for cursor

// Put the X dependent Cursor tables here...

   static unsigned int VtoXmap[] =
      {
	XC_X_cursor,  XC_top_left_arrow, XC_center_ptr, XC_crosshair,
	XC_sb_h_double_arrow, XC_hand2, XC_xterm, XC_icon,
	XC_sb_v_double_arrow, XC_pencil, XC_question_arrow,
	XC_sizing, XC_watch, XC_X_cursor
      };

    static Cursor XCursors[maxCursors+1];

    XVisualInfo* vBaseGLCanvasPane::_visinfo = 0;	// one time visual
    int vBaseGLCanvasPane::_doubleBuffer = 0;		// if we have double buffering
    unsigned int vBaseGLCanvasPane::_vGLmode = 0;

//================>>> vBaseGLCanvasPane::vBaseGLCanvasPane <<<========================
  vBaseGLCanvasPane::vBaseGLCanvasPane(unsigned int vGLmode,
   PaneType pt) : vPane(pt)
  {
    SysDebug(Constructor,"vBaseGLCanvasPane::vBaseGLCanvasPane() constructor\n")

    const unsigned int defGLmode = (vGL_RGB | vGL_DoubleBuffer);

    if (_currentCursor == -1)		// Do we need to create cursors?
      {
	_currentCursor = CA_None;
	for (int i = 0 ; i <= maxCursors ; ++i)  // Create all cursors
	  {
	    XCursors[i] = XCreateFontCursor(theApp->display(),VtoXmap[i]);
	    // Get the fg and bg to make cursor match
	  }
      }

    // One shot initialization of X Visual

    if (_visinfo == 0)	// Haven't gotten a visual yet
      {
	_vGLmode = ((vGLmode == vGL_Default) ? defGLmode : vGLmode);

	if (_vGLmode & vGL_Indexed)	// Indexed mode *********************
	  {
	    _visinfo = chooseIndexed(_vGLmode);
	    if (_visinfo == 0 && (_vGLmode & vGL_DoubleBuffer))
	      {
		_vGLmode &= ~vGL_DoubleBuffer;	// try without double buffer
		_visinfo = chooseIndexed(_vGLmode);
	      }
	  }
	else				// RGB mode ********************
	  {
	    _visinfo = chooseRGB(_vGLmode);
	    if (_visinfo == 0 && (_vGLmode & vGL_Alpha))	// try to get rid of this
	      {
		_vGLmode &= ~vGL_Alpha;	// zap the alpha plane
		_visinfo = chooseRGB(_vGLmode);
	      }
	    if (_visinfo == 0 && (_vGLmode & vGL_DoubleBuffer))
	      {
		_vGLmode &= ~vGL_DoubleBuffer;	// try without double buffer
		_visinfo = chooseRGB(_vGLmode);
	      }
	  }

	if (_visinfo == 0)
	    XtAppError(theApp->appContext(), "Can't create Visual for GL.");
	else
	    _doubleBuffer = (_vGLmode & vGL_DoubleBuffer);
      }
  }

//================>>> vBaseGLCanvasPane::~vBaseGLCanvasPane <<<===================
  vBaseGLCanvasPane::~vBaseGLCanvasPane()
  {
    SysDebug(Destructor,"vBaseGLCanvasPane::~vBaseGLCanvasPane() destructor\n")
  }

//==================>>> vBaseGLCanvasPane::chooseIndexed <<<=======================
  XVisualInfo* vBaseGLCanvasPane::chooseIndexed(unsigned int glMode)
  {

    static int bufSizeList[] = {16, 12, 8, 4, 2, 1, 0};
    int list[32];
    int n = 0;
    XVisualInfo* vi;

    list[n++] = GLX_BUFFER_SIZE;
    list[n++] = 1;		// we will dynamically change this
    if (glMode & vGL_DoubleBuffer)
      {
	list[n++] = GLX_DOUBLEBUFFER;
      }
    if (glMode & vGL_Stereo)
      {
	list[n++] = GLX_STEREO;
      }
    if (glMode & vGL_Depth)
      {
	list[n++] = GLX_DEPTH_SIZE;
	list[n++] = 1;
      }
    if (glMode & vGL_Stencil)
      {
	list[n++] = GLX_STENCIL_SIZE;
	list[n++] = 1;
      }
    list[n] = (int) None; /* terminate list */

    /* glXChooseVisual specify GLX_BUFFER_SIZE prefers the
    "smallest index buffer of at least the specified size".
    This would be reasonable if GLUT allowed the user to
    specify the required buffe size, but GLUT's display mode
    is too simplistic (easy to use?). GLUT should try to find
    the "largest".  So start with a large buffer size and
    shrink until we find a matching one that exists. */

    for (int i = 0; bufSizeList[i]; i++)
      {
	// Assumes list[1] is where GLX_BUFFER_SIZE parameter is.
	list[1] = bufSizeList[i];
	vi = glXChooseVisual(theApp->display(), 
	    DefaultScreen(theApp->display()), list);
	if (vi)
	  {
	    return vi;
	  }
      }
    return 0;
  }

//==================>>> vBaseGLCanvasPane::chooseRGB <<<=======================
  XVisualInfo* vBaseGLCanvasPane::chooseRGB(unsigned int glMode)
  {

    int list[32];
    int n = 0;
    XVisualInfo* vi;

    list[n++] = GLX_RGBA;
    list[n++] = GLX_RED_SIZE;
    list[n++] = 1;
    list[n++] = GLX_GREEN_SIZE;
    list[n++] = 1;
    list[n++] = GLX_BLUE_SIZE;
    list[n++] = 1;
    if (glMode & vGL_Alpha)
      {
	list[n++] = GLX_ALPHA_SIZE;
	list[n++] = 1;
      }

    if (glMode & vGL_DoubleBuffer)
      {
	list[n++] = GLX_DOUBLEBUFFER;
      }
    if (glMode & vGL_Stereo)
      {
	list[n++] = GLX_STEREO;
      }
    if (glMode & vGL_Depth)
      {
	list[n++] = GLX_DEPTH_SIZE;
	list[n++] = 1;
      }
    if (glMode & vGL_Stencil)
      {
	list[n++] = GLX_STENCIL_SIZE;
	list[n++] = 1;
      }
    if (glMode & vGL_Accum)
      {
	list[n++] = GLX_ACCUM_RED_SIZE;
	list[n++] = 1;
	list[n++] = GLX_ACCUM_GREEN_SIZE;
	list[n++] = 1;
	list[n++] = GLX_ACCUM_BLUE_SIZE;
	list[n++] = 1;
	if (glMode & vGL_Alpha)
	  {
	    list[n++] = GLX_ACCUM_ALPHA_SIZE;
	    list[n++] = 1;
	  }
      }

    list[n] = (int) None; /* terminate list */

    vi = glXChooseVisual(theApp->display(), 
	    DefaultScreen(theApp->display()), list);
    return vi;
  }

//==================>>> vBaseGLCanvasPane::initialize <<<==========================
  void vBaseGLCanvasPane::initialize(vWindow* pWindow, Widget pWidget)
  {

    XtCallbackRec HscrollProcList[] =
      {
	{ CHScrollProcCB, this },
	{ (XtCallbackProc) 0, (XtPointer) 0}
      };
    XtCallbackRec VscrollProcList[] =
      {
	{ CVScrollProcCB, this },
	{ (XtCallbackProc) 0, (XtPointer) 0}
      };

#ifndef Motif		// ATHENA
    XtCallbackRec HjumpProcList[] =
      {
	{ CHJumpProcCB, this },
	{ (XtCallbackProc) 0, (XtPointer) 0}
      };

    XtCallbackRec VjumpProcList[] =
      {
	{ CVJumpProcCB, this },
	{ (XtCallbackProc) 0, (XtPointer) 0}
      };
#endif

    _HScrlShown = 100;	//@@@@
    _HScrlTop =  1;
    _VScrlShown = 100;
    _VScrlTop =  1;

     // now, build the menu items in the widget provided

    vPane::initialize(pWindow,pWidget);	// initialize base class

    _drawCanvas = XtVaCreateManagedWidget(
	"vBaseGLCanvasPane",
	canvasWidgetClass,	// widget class 
	_baseW,			// parent widget
#ifdef Motif
	XmNallowResize,1,
#else
	XtNshowGrip, FALSE,
	XtNallowResize,1,
#endif
	XtNhSpace, 1,
	XtNvSpace, 1,
	XtNheight,(_parentWin->getWinHeight())+16,
	XtNwidth,(_parentWin->getWinWidth())+16,
	NULL);			// argument list

     
    // --------------------------------------------------------------
    _hasFocus = 0;	// we don't start out with focus
    _mouseDown = 0;	// mouse is not down

    Dimension curW, curH;

    _drawWindow = XtVaCreateManagedWidget(
	"vCanvasDrawGL",		// widget name
	glwDrawingAreaWidgetClass,	// widget class 
	_drawCanvas,		// parent widget
	GLwNvisualInfo, _visinfo,
	XtNheight, _parentWin->getWinHeight(),
	XtNwidth, _parentWin->getWinWidth(),
	NULL);			// argument list


    // --------------------------------------------------------------


#ifdef Motif
    _drawVScroll = XtVaCreateManagedWidget(
	"vCanvasVScroll",				// name
	sbWidgetClass,			// class
	_drawCanvas,			// parent

	XmNvalueChangedCallback, VscrollProcList,	// callback for scrolling
	XmNdecrementCallback, VscrollProcList,	// callback for scrolling
	XmNincrementCallback, VscrollProcList,	// callback for scrolling
	XmNpageDecrementCallback, VscrollProcList,	// callback for scrolling
	XmNpageIncrementCallback, VscrollProcList,	// callback for scrolling

	XmNheight, _parentWin->getWinHeight(),
	XmNwidth, sbThickness,	// scrollbar param
	XmNorientation,XmVERTICAL,
	XmNmaximum,100,
	NULL);

    _drawHScroll = XtVaCreateManagedWidget(
	"vCanvasHScroll",
	sbWidgetClass,
	_drawCanvas,

	XmNvalueChangedCallback, HscrollProcList,	// callback for scrolling
	XmNdecrementCallback, HscrollProcList,	// callback for scrolling
	XmNincrementCallback, HscrollProcList,	// callback for scrolling
	XmNpageDecrementCallback, HscrollProcList,	// callback for scrolling
	XmNpageIncrementCallback, HscrollProcList,	// callback for scrolling

	XmNorientation,XmHORIZONTAL,
	XmNheight, sbThickness,	// scrollbar param
	XmNwidth, _parentWin->getWinWidth(),
	XmNmaximum,100,
	NULL);

#else
    _drawVScroll = XtVaCreateManagedWidget(
	"vCanvasVScroll",				// name
	sbWidgetClass,			// class
	_drawCanvas,			// parent
	XtNscrollProc, VscrollProcList,	// callback for scrolling
	XtNjumpProc, VjumpProcList,	// callback for scrolling
	XtNheight, _parentWin->getWinHeight(),
	XtNthickness, sbThickness,	// scrollbar param
#ifdef USE3D
	XtNbackground,_vControlFace,
	XtNscrollbar3dBackground, _vControlBG,
#endif
	NULL);

    _drawHScroll = XtVaCreateManagedWidget(
	"vCanvasHScroll",
	sbWidgetClass,
	_drawCanvas,
	XtNscrollProc, HscrollProcList,	// callback for scrolling
	XtNjumpProc, HjumpProcList,	// callback for scrolling
	XtNorientation,XtorientHorizontal,	// scrollbar param
	XtNthickness, sbThickness,	// scrollbar param
	XtNwidth, _parentWin->getWinWidth(),
#ifdef USE3D
	XtNbackground,_vControlFace,
	XtNscrollbar3dBackground, _vControlBG,
#endif
	NULL);

#endif


    // handle exposure events - requires redrawing

    XtAddEventHandler(_drawWindow, ExposureMask, 0,
	CExposeEV,(XtPointer)this);

    // The graphics init event
    XtAddCallback(_drawWindow,GLwNginitCallback, CgraphicsInit,(XtPointer)this);

    // get mouse events
    XtAddEventHandler(_drawWindow, ButtonPressMask, 0,
	CMouseDownEV,(XtPointer)this);
    XtAddEventHandler(_drawWindow, PointerMotionMask, 0,
	CMouseMoveEV,(XtPointer)this);
    XtAddEventHandler(_drawWindow, ButtonReleaseMask, 0,
	CMouseUpEV,(XtPointer)this);
    XtAddEventHandler(_drawWindow, EnterWindowMask, 0,
	CEnterEV,(XtPointer)this);
    XtAddEventHandler(_drawWindow, LeaveWindowMask, 0,
	CLeaveEV,(XtPointer)this);

#ifdef Motif
    XtAddEventHandler(_drawWindow,KeyPressMask, 0,
	CNVKeyInEV,(XtPointer)this);
#endif

    XtAddCallback(_drawCanvas, XtNcallback, CchangeCB, (XtPointer)this);

    _HOn = _VOn = 0;			// both start out on

    SetHScroll(_HScrlShown, 0);	// initial values for scroll bar
    SetVScroll(_VScrlShown, 0);

    XtUnmanageChild(_drawHScroll);	// Unmanage the widget
    XtUnmanageChild(_drawVScroll);	// Unmanage the widget

    XtVaGetValues(_drawCanvas,		// get values for the draw canvas
	XtNwidth, &curW,		// its new h and w
	XtNheight, &curH,
	NULL);

    _height = curH;	// update the local stuff
    _parentWin->setWinHeight(_height);		// update the local stuff
    _width = curW;
    _parentWin->setWinWidth(_width);

    glxContext = 0;
    _Drawable = 0;
  }

//==================>>> vBaseGLCanvasPane::graphicsInit <<<========================
  void vBaseGLCanvasPane::graphicsInit(void)
  {
    XVisualInfo *visinfo;

    // Perform only the most basic operations, including getting the context

    XtVaGetValues(_drawWindow, GLwNvisualInfo, &visinfo, NULL);

    glxContext = glXCreateContext(theApp->display(), visinfo,
        0,					// No sharing.
        1);					// Direct rendering if possible.

    vglMakeCurrent();
  }

//==================>>> vBaseGLCanvasPane::graphicsInit <<<========================
  void vBaseGLCanvasPane::vglFlush(void)
  {
    
    if (_doubleBuffer)
	glXSwapBuffers(theApp->display(), XtWindow(_drawWindow));
    else
	glFlush();

    /* Avoid indirect rendering latency from queuing. */
    if (!glXIsDirect(theApp->display(), glxContext))
	glFinish();


  }

//==================>>> vBaseGLCanvasPane::ChangeCB <<<========================
  void vBaseGLCanvasPane::ChangeCB(void)
  {
    Dimension curW, curH;

    XtVaGetValues(_drawWindow,		// get values for the draw canvas
	XtNwidth, &curW,		// its new h and w
	XtNheight, &curH,
	NULL);

    if (curW > 0x7FFF)			// supposed to be unsigned, but
	curW = 0;			// can get confused
    if (curH > 0x7FFF)			// supposed to be unsigned, but
	curH = 0;			// can get confused

    _height = curH;	// update the local stuff
    _parentWin->setWinHeight(_height);		// update the local stuff
    _width = curW;
    _parentWin->setWinWidth(_width);

    Resize((int)curW, (int)curH);	// Call the virtual function

  }

//====================>>> vBaseGLCanvasPane::ShowPane <<<======================
  void vBaseGLCanvasPane::ShowPane(int OnOrOff)
  {
    if (OnOrOff)
      {
	XtManageChild(_drawCanvas);
      }
    else
      {
	XtUnmanageChild(_drawCanvas);
      }
  }

//=====================>>> vBaseGLCanvasPane::SetWidthHeight <<<============================
  void vBaseGLCanvasPane::SetWidthHeight(int width, int height)
  {
    Dimension curW = width;
    Dimension curH = height;

    _height = curH;	// update the local stuff
    _parentWin->setWinHeight(_height);		// update the local stuff
    _width = curW;
    _parentWin->setWinWidth(_width + 16);

    XtVaSetValues(_baseW,			// parent widget
	XtNwidth, curW+16,
	NULL);
   
    XtVaSetValues(_drawCanvas,	// get values for the draw canvas
	XtNwidth, curW,		// its new h and w
	XtNheight, curH,
	NULL);

  }

//------------------------- cursor Stuff ---------------------------------
//=====================>>> vBaseGLCanvasPane::SetCursor <<<============================
  void vBaseGLCanvasPane::SetCursor(VCursor id)
  {
    // Set to current cursor

    if (id < CA_None || id > VC_LAST)
      {
	SysDebug1(BadVals,"vBaseGLCanvasPane::SetCursor(id=%d)\n",id)
	return;
      }

    if (id == CA_None)		// special case
	UnSetCursor();

    SysDebug1(WindowEvents,"vBaseGLCanvasPane::SetCursor(id=%d)\n",id)

    _currentCursor = id;

    XDefineCursor(theApp->display(), XtWindow(_drawWindow), XCursors[id]);
  }

//=====================>>> vBaseGLCanvasPane::UnSetCursor <<<============================
  void vBaseGLCanvasPane::UnSetCursor(void)
  {
    XUndefineCursor(theApp->display(), XtWindow(_drawWindow));
    _currentCursor = CA_None;
  }

//------------------------- Scrolling Stuff ---------------------------------
//====================>>> vBaseGLCanvasPane::HJumpProcCB <<<=========================
  void vBaseGLCanvasPane::HJumpProcCB(float fpercent, int motifValue)
  {
    // This is called with a fraction from 0 to 1.0 indicating
    // how far the user has moved the scroll bar left or right.
    // I can't get the Athena scroll widget to work how I want, so
    // I'll live with how it seems to want to work: the shown
    // fraction only is "correct" when it will all fit. The top
    // will always be right - but 100 is at the bottom with the
    // shown drawn out of the scroll box. Sigh...

    int shown = _HScrlShown;
    int max_top = 100 - shown;
    int top;

#ifdef Motif
    int percent = (int)motifValue;
#else
    int percent = (int)(fpercent * 100.);
#endif

    if (percent > max_top)
      {
	percent = max_top;
      }

    if (max_top == 0)
	top = 0;
    else
	top = (percent * 100) / max_top;

    if (top != _HScrlTop)	// actually changed things
      {
	HPage(shown, top);
      }
  }

//=======================>>> vBaseGLCanvasPane::HPage <<<============================
  void vBaseGLCanvasPane::HPage(int Shown, int Top)
  {
    // This is the main way a user app gets scrolling events, and
    // will usually override this guy to handle what is actually
    // drawn in the canvas window.
    // This should then be called to actually change the display.

    SysDebug2(WindowEvents,"vBaseGLCanvasPane::HPage(Shown: %d, Top: %d)\n",Shown, Top);

    SetHScroll(Shown,Top);
  }

//=======================>>> vBaseGLCanvasPane::HScroll <<<==========================
  void vBaseGLCanvasPane::HScroll(int step)
  {
    // This is the way the user app gets step events - either page or
    // single step events. The default doesn't have to do anything.

    SysDebug1(WindowEvents,"vBaseGLCanvasPane::HScroll(%d)\n",step);

  }

//=====================>>> vBaseGLCanvasPane::ShowHScroll <<<=========================
  void vBaseGLCanvasPane::ShowHScroll(int OnOff)
  {
    if (OnOff)			// Turn on
      {
	if (_HOn)		// Already on
	    return;
	_HOn = 1;
	XtManageChild(_drawHScroll);	// Unmanage the widget
      }
    else			// Turn off
      {
	if (!_HOn)		// Already off
	    return;
	_HOn = 0;
	XtUnmanageChild(_drawHScroll);	// Unmanage the widget
      }

    Dimension curW, curH;

    XtVaGetValues(_drawWindow,		// get values for the draw canvas
	XtNwidth, &curW,		// its new h and w
	XtNheight, &curH,
	NULL);

    _height = curH;			// update the local stuff
    _parentWin->setWinHeight(_height);	// update the local stuff
    _width = curW;
    _parentWin->setWinWidth(_width);


    XClearArea(XtDisplay(_drawWindow), XtWindow(_drawWindow),
	0, 0, _width, _height, 1);

  }

//=====================>>> vBaseGLCanvasPane::ShowVScroll <<<=========================
  void vBaseGLCanvasPane::ShowVScroll(int OnOff)
  {
    if (OnOff)			// Turn on
      {
	if (_VOn)		// Already on
	    return;
	_VOn = 1;
	XtManageChild(_drawVScroll);	// Unmanage the widget
      }
    else			// Turn off
      {
	if (!_VOn)		// Already off
	    return;
	_VOn = 0;
	XtUnmanageChild(_drawVScroll);	// Unmanage the widget
      }

    Dimension curW, curH;

    XtVaGetValues(_drawWindow,		// get values for the draw canvas
	XtNwidth, &curW,		// its new h and w
	XtNheight, &curH,
	NULL);

    _height = curH;			// update the local stuff
    _parentWin->setWinHeight(_height);	// update the local stuff
    _width = curW;
    _parentWin->setWinWidth(_width);

    XClearArea(XtDisplay(_drawWindow), XtWindow(_drawWindow),
	0, 0, _width, _height, 1);
  }

//=====================>>> vBaseGLCanvasPane::SetHScroll <<<=========================
  void vBaseGLCanvasPane::SetHScroll(int Shown, int Top)
  {
    // Make sure we are setting to valid values, and are changing things!

    if (Shown < 0 || Shown > 100 || Top < 0 || Top > 100)
	return;

    _HScrlShown = Shown;
    if (Shown < 5)		// it must be something that shows
	_HScrlShown = 5;

    if (Shown == 100)		// Map top to space we have available
	_HScrlTop = 0;
    else
	_HScrlTop = Top;

    DrawHScroll(_HScrlShown, _HScrlTop);

  }

//=====================>>> vBaseGLCanvasPane::DrawHScroll <<<========================
  void vBaseGLCanvasPane::DrawHScroll(int Shown, int Top)
  {
    Arg args[3];		// Used to pass float value

#ifdef Motif
    float shown = (float) Shown;
    int iTop;

    if (Top == 0)
	iTop = 0;
    else if (Top == 100)
	iTop = 100 - Shown;
    else
	iTop = (int) ((100. - shown)*((float)Top / 100.));

    XtVaSetValues(_drawHScroll,
	XmNsliderSize,Shown,
	XmNvalue,iTop,
	NULL);
#else
    union
      {
	XtArgVal arg_value;
	float float_value;
      } shown_value, top_value;

    float shown = float(Shown) / 100.;
    float top;

    if (Top == 0)
	top = 0.;
    else
	top = (1. - shown)*(float(Top) / 100.);

    shown_value.float_value = shown;
    top_value.float_value = top;

    if (sizeof (float) > sizeof (XtArgVal))
      {
	XtSetArg(args[0], XtNtopOfThumb, &top_value);
	XtSetArg(args[1], XtNshown, &shown_value);
      }
    else
      {
	XtSetArg(args[0], XtNtopOfThumb, top_value.arg_value);
	XtSetArg(args[1], XtNshown, shown_value.arg_value);
      }

    XtSetValues(_drawHScroll, args, 2);		// change it!
#endif

  }

//====================>>> vBaseGLCanvasPane::VJumpProcCB <<<=======================
  void vBaseGLCanvasPane::VJumpProcCB(float fpercent, int motifValue)
  {
    // This code must be used with the 3D Widgets since they
    // actually give the correct values -- we need to map
    // the value supplied from the wiget to a 0 - 100 range
    // required by the V specs.


    int shown = _VScrlShown;
    int max_top = 100 - shown;
    int top;

#ifdef Motif
    int percent = (int)motifValue;
#else
    int percent = (int)(fpercent * 100.);
#endif

    if (percent > max_top)
      {
	percent = max_top;
      }

    if (max_top == 0)
	top = 0;
    else
	top = (percent * 100) / max_top;

    if (top != _VScrlTop)	// actually changed things
      {
	VPage(shown, top);
      }
  }

//========================>>> vBaseGLCanvasPane::VPage <<<===========================
  void vBaseGLCanvasPane::VPage(int Shown, int Top)
  {

    SysDebug2(WindowEvents,"vBaseGLCanvasPane::VPage(Shown: %d, Top: %d)\n",Shown, Top);

    SetVScroll(Shown,Top);

  }

//========================>>> vBaseGLCanvasPane::VScroll <<<=========================
  void vBaseGLCanvasPane::VScroll(int step)
  {
    // This is the way the user app gets step events - either page or
    // single step events. The default doesn't have to do anything.

    SysDebug1(WindowEvents,"vBaseGLCanvasPane::VScroll(%d)\n",step);

  }

//=====================>>> vBaseGLCanvasPane::GetVScroll <<<=========================
  int vBaseGLCanvasPane::GetVScroll(int& Shown, int& Top)
  {
    Shown = _VScrlShown; Top = _VScrlTop;
    return _VOn;
  }

//=====================>>> vBaseGLCanvasPane::GetHScroll <<<=========================
  int vBaseGLCanvasPane::GetHScroll(int& Shown, int& Top)
  {
    Shown = _HScrlShown; Top = _HScrlTop;
    return _HOn;
  }

//=====================>>> vBaseGLCanvasPane::SetVScroll <<<=========================
  void vBaseGLCanvasPane::SetVScroll(int Shown, int Top)
  {
    // Make sure we are setting to valid values, and are changing things!

    if (Shown < 0 || Shown > 100 || Top < 0 || Top > 100)
	return;

    _VScrlShown = Shown;
    if (Shown < 5)
	_VScrlShown = 5;

    if (Shown == 100)		// Map top to space we have available
	_VScrlTop = 0;
    else
	_VScrlTop = Top;

    DrawVScroll(_VScrlShown, _VScrlTop);
  }

//======================>>> vBaseGLCanvasPane::DrawVScroll <<<=======================
  void vBaseGLCanvasPane::DrawVScroll(int Shown, int Top)
  {

#ifdef Motif
    float shown = (float) Shown;
    int iTop;

    if (Top == 0)
	iTop = 0;
    else if (Top == 100)
	iTop = 100 - Shown;
    else
	iTop = (int) ((100. - shown)*((float)Top / 100.));
	
    XtVaSetValues(_drawVScroll,
	XmNsliderSize,Shown,
	XmNvalue,iTop,
	NULL);
#else
    Arg args[3];		// Used to pass float value

    union
      {
	XtArgVal arg_value;
	float float_value;
      } shown_value, top_value;

    float shown = float(Shown) / 100.;
    float top;


    if (Top == 0)
	top = 0.;
    else
	top = (1. - shown)*(float(Top) / 100.);

    shown_value.float_value = shown;
    top_value.float_value = top;

    if (sizeof (float) > sizeof (XtArgVal))
      {
	XtSetArg(args[0], XtNtopOfThumb, &top_value);
	XtSetArg(args[1], XtNshown, &shown_value);
      }
    else
      {
	XtSetArg(args[0], XtNtopOfThumb, top_value.arg_value);
	XtSetArg(args[1], XtNshown, shown_value.arg_value);
      }

    XtSetValues(_drawVScroll, args, 2);		// change it!
#endif

  }

//====================>>> vBaseGLCanvasPane::HScrollProcCB <<<=======================
  void vBaseGLCanvasPane::HScrollProcCB(int position)
  {
    if (position  > 0)		// Scroll right a page
	HScroll(1);
    else
	HScroll(-1);		// Scroll left a page
  }

//====================>>> vBaseGLCanvasPane::VScrollProcCB <<<=======================
  void vBaseGLCanvasPane::VScrollProcCB(int position)
  {
    if (position  > 0)		// scroll list down one page
	VScroll(1);
    else
	VScroll(-1);		// scroll list up one page
  }

//=====================>>> vBaseGLCanvasPane::ExposeEV <<<==========================
  void vBaseGLCanvasPane::ExposeEV(int x, int y, int width, int height)
  {
    Redraw(x, y , width, height);
  }

//=====================>>> vBaseGLCanvasPane::EnterFocus <<<========================
  void vBaseGLCanvasPane::EnterFocus()
  {
    SysDebug(WindowEvents,"vBaseGLCanvasPane::EnterFocus()\n")
    if (_currentCursor != CA_None)
	SetCursor(_currentCursor);
  }

//=====================>>> vBaseGLCanvasPane::EnterEV <<<==========================
  void vBaseGLCanvasPane::EnterEV()
  {
    if (_hasFocus)		// don't double enter focus
	return;
    _hasFocus = 1;

#ifdef Motif
    XmProcessTraversal(_drawWindow,XmTRAVERSE_CURRENT);	// Xm bug, call twice!
    XmProcessTraversal(_drawWindow,XmTRAVERSE_CURRENT);
#endif

    EnterFocus();		// call the virtual function
  }

//=====================>>> vBaseGLCanvasPane::LeaveFocus <<<=======================
  void vBaseGLCanvasPane::LeaveFocus()
  {
    SysDebug(WindowEvents,"vBaseGLCanvasPane::LeaveFocus()\n")
  }

//=====================>>> vBaseGLCanvasPane::LeaveEV <<<==========================
  void vBaseGLCanvasPane::LeaveEV()
  {
    if (!_hasFocus)		// don't double leave focus
	return;
    _hasFocus = 0;

    LeaveFocus();		// call the virtual function
  }

//=====================>>> vBaseGLCanvasPane::MouseDownEV <<<=======================
  void vBaseGLCanvasPane::MouseDownEV(int x, int y, int button)
  {
    // We track mouse down internally - X should do it, but
    // sometimes it seems to get the mouse down withoud sending
    // the event, making MouseMove not work as advertised.
    if (_mouseDown)
	return;
    _mouseDown = 1;
    MouseDown(x,y,button);
  }

//=====================>>> vBaseGLCanvasPane::MouseUpEV <<<=========================
  void vBaseGLCanvasPane::MouseUpEV(int x, int y, int button)
  {
    if (!_mouseDown)
	return;
    _mouseDown = 0;
    MouseUp(x,y,button);
  }

//=====================>>> vBaseGLCanvasPane::MouseMoveEV <<<=======================
  void vBaseGLCanvasPane::MouseMoveEV(int x, int y, int button)
  {
    if (_mouseDown && button != 0)		// only if down
	MouseMove(x,y,button);
    else
      {
	MouseMotion(x,y);
      }
  }

//=====================>>> vBaseGLCanvasPane::MouseDown <<<==========================
  void vBaseGLCanvasPane::MouseDown(int x, int y, int button)
  {
    // Mouse events in vBaseGLCanvasPane are no-ops.

    SysDebug3(MouseEvents,"vBaseGLCanvasPane::MouseDown(x:%d,y:%d,btn:%d)\n",x,y,button)
  }

//=====================>>> vBaseGLCanvasPane::MouseUp <<<============================
  void vBaseGLCanvasPane::MouseUp(int x, int y, int button)
  {
    // Mouse events in vBaseGLCanvasPane are no-ops.

    SysDebug3(MouseEvents,"vBaseGLCanvasPane::MouseUp(x:%d,y:%d,btn:%d)\n",x,y,button)
  }

//=====================>>> vBaseGLCanvasPane::MouseMove <<<==========================
  void vBaseGLCanvasPane::MouseMove(int x, int y, int button)
  {
    // Mouse events in vBaseGLCanvasPane are no-ops.

    SysDebug3(MouseEvents,"vBaseGLCanvasPane::MouseMove(x:%d,y:%d,btn:%d)\n",x,y,button)
  }

//=======================>>> vBaseGLCanvasPane::Redraw <<<==========================
  void vBaseGLCanvasPane::Redraw(int x, int y, int width, int height)
  {
    // Redraw in vBaseGLCanvasPane is a no-op.
    
#ifdef vDEBUG		// Don't have a SysDebug4 macro, so do it by hand
    if (DebugState.WindowEvents && DebugState.System)
	fprintf(stderr,"vBaseGLCanvasPane::Redraw(x=%d, y=%d, w=%d, h=%d)\n",x,y,width,height);
#endif

  }

//=====================>>> vBaseGLCanvasPane::Resize <<<============================
  void vBaseGLCanvasPane::Resize(int newW, int newH)
  {
    // This is the routine the user will override to intercept size changes

    vglMakeCurrent();
    glXWaitX();
    glViewport(0,0,newW,newH);

  }

extern "C"
{
//==============================>>> CchangeCB <<<==============================
  void CchangeCB(Widget w, XtPointer client_data, XtPointer call_data)
  { 
    //	change callback
    //
    // client_data will have the this pointer of our object
    // call_data will point to a structure that handles
    // resize, scrollbars, and exposure

    ((vBaseGLCanvasPane*)client_data)->ChangeCB();
  }

//============================>>> CHJumpProcCB <<<=============================
  void CHJumpProcCB(Widget w, XtPointer This, XtPointer pc_ptr)
  {
    float percent = *(float*)pc_ptr;	// get the percent back

    ((vBaseGLCanvasPane*)This)->HJumpProcCB(percent,0);
  }

//============================>>> CHScollProcCB <<<=============================
  void CHScrollProcCB(Widget w, XtPointer This, XtPointer position)
  {
#ifdef Motif
    XmScrollBarCallbackStruct* cbp = (XmScrollBarCallbackStruct*)position;
    switch (cbp->reason)
      {
	case XmCR_DECREMENT:
	case XmCR_PAGE_DECREMENT:
	  {
	    ((vBaseGLCanvasPane*)This)->HScrollProcCB(-1);
	    break;
	  }
	case XmCR_INCREMENT:
	case XmCR_PAGE_INCREMENT:
	  {
	    ((vBaseGLCanvasPane*)This)->HScrollProcCB(1);
	    break;
	  }
	case XmCR_VALUE_CHANGED:
	  {
	    ((vBaseGLCanvasPane*)This)->HJumpProcCB(0.0, cbp->value);
	    break;
	  }
      }
#else
    int pos = (int)position;
    ((vBaseGLCanvasPane*)This)->HScrollProcCB(pos);
#endif
  }

//============================>>> CJumpProcCB <<<=============================
  void CVJumpProcCB(Widget w, XtPointer This, XtPointer pc_ptr)
  {
    float percent = *(float*)pc_ptr;	// get the percent back

    ((vBaseGLCanvasPane*)This)->VJumpProcCB(percent,0);
  }

//============================>>> CVScollProcCB <<<=============================
  void CVScrollProcCB(Widget w, XtPointer This, XtPointer position)
  {
#ifdef Motif
    XmScrollBarCallbackStruct* cbp = (XmScrollBarCallbackStruct*)position;
    switch (cbp->reason)
      {
	case XmCR_DECREMENT:
	case XmCR_PAGE_DECREMENT:
	  {
	    ((vBaseGLCanvasPane*)This)->VScrollProcCB(-1);
	    break;
	  }
	case XmCR_INCREMENT:
	case XmCR_PAGE_INCREMENT:
	  {
	    ((vBaseGLCanvasPane*)This)->VScrollProcCB(1);
	    break;
	  }
	case XmCR_VALUE_CHANGED:
	  {
	    ((vBaseGLCanvasPane*)This)->VJumpProcCB(0.0, cbp->value);
	    break;
	  }
      }
#else
    int pos = (int)position;
    ((vBaseGLCanvasPane*)This)->VScrollProcCB(pos);
#endif
  }

//==============================>>> CExposeEV <<<==============================
  void CExposeEV(Widget w, XtPointer client_data, XEvent* event, char *x)
  { 

#ifndef Motif		// ATHENA

    //	Exposure event
    //
    // client_data will have the this pointer of our object

    XExposeEvent* xp;

    xp = (XExposeEvent*)event;

    if (xp->count != 0)		// ignore all but last expose event
	return;

    ((vBaseGLCanvasPane*)client_data)->ExposeEV(xp->x, xp->y, xp->width, xp->height);
#endif
  }

//==============================>>> CgraphicsInit <<<=============================
  void CgraphicsInit(Widget w, XtPointer clientData, XtPointer call)
  {
    ((vBaseGLCanvasPane*)clientData)->graphicsInit();
  }

//==============================>>> CEnterEV <<<=============================
  void CEnterEV(Widget w, XtPointer client_data, XEvent* event, char *x)
  { 
    //	Enter event
    //
    // client_data will have the this pointer of our object

    ((vBaseGLCanvasPane*)client_data)->EnterEV();
  }

//==============================>>> CLeaveEV <<<==============================
  void CLeaveEV(Widget w, XtPointer client_data, XEvent* event, char *x)
  { 
    //	Leave event
    //
    // client_data will have the this pointer of our object

    ((vBaseGLCanvasPane*)client_data)->LeaveEV();
  }

//=============================>>> CMouseDownEV <<<============================
  void CMouseDownEV(Widget w, XtPointer client_data, XEvent* event, char *x)
  { 
    // Mouse down event
#ifndef Motif
    XButtonEvent* xp;

    xp = (XButtonEvent*)event;

    ((vBaseGLCanvasPane*)client_data)->MouseDownEV(xp->x, xp->y, xp->button);
#endif
  }

//==============================>>> CMouseUpEV <<<===========================
  void CMouseUpEV(Widget w, XtPointer client_data, XEvent* event, char *x)
  { 
#ifndef Motif
    // Mouse up event
    XButtonEvent* xp;

    xp = (XButtonEvent*)event;

    ((vBaseGLCanvasPane*)client_data)->MouseUpEV(xp->x, xp->y, xp->button);
#endif
  }

//============================>>> CMouseMoveEV <<<==========================
  void CMouseMoveEV(Widget w, XtPointer client_data, XEvent* event, char *x)
  { 
#ifndef Motif
    // Mouse down event
    XMotionEvent* xp;

    xp = (XMotionEvent*)event;

    int button = 0;		// assume button 1

    if (xp->state & Button1Mask)
	button = 1;
    else if (xp->state & Button2Mask)
	button = 2;
    else if (xp->state & Button3Mask)
	button = 3;

    ((vBaseGLCanvasPane*)client_data)->MouseMoveEV(xp->x, xp->y, button);
#endif
  }

//============================>>> CNVKeyInEV <<<==========================
  void CNVKeyInEV(Widget w, XtPointer client_data, XEvent* event, char *x)
  { 
    //	KeyPress Event Handler
    //
    // client_data will have the this pointer of our object

#ifdef Motif
fprintf(stderr,"CNVKeyin\n");
#else
//    XKeyPressedEvent *kp;
//
//    kp = (XKeyPressedEvent*)event;
//    vWindow* thisWindow = (vWindow*) client_data;	// get back this
//
//    thisWindow->KeyInEV(kp->keycode, kp->state);
#endif
  }
}
