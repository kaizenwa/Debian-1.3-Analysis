//===============================================================
// vprogrsc.cxx	- Progress Bar
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vxutil.h>	// Motif/Athena mapping
#include <v/vprogrsc.h>	// our definitions
#include <v/vcmdprnt.h>	// a command parent
#include <v/vapp.h>
#include <v/vfont.h>
#include <v/vutil.h>
#include <v/vctlclrs.h>

extern "C"
{
#include <X11/Xlib.h>
#include <X11/StringDefs.h>

#ifdef Motif
#include <Xm/Xm.h>
#include <Xm/ScrollBar.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#else

#include <X11/cursorfont.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Label.h>
#ifdef USE3D
#include <v/awsld3d.h>
#else
#include <X11/Xaw/Scrollbar.h>
#endif
#endif
}

#ifdef Motif	// --------------------------------------

#define lblWidgetClass xmLabelWidgetClass
#define Nheight XmNheight
#define Nlabel XmNlabelString
#define NmappedWhenManaged XmNmappedWhenManaged
#define Nwidth XmNwidth
//@@ These use athena parent for now
#define Nresizable(x) XmNrecomputeSize,x,

#define setLabel(x,y) XmString x = XmStringCreateSimple(y);
#define freeLabel(x) XmStringFree(x);

#else	//------------------------------------------------------

#define lblWidgetClass labelWidgetClass
#define Nheight XtNheight
#define Nlabel XtNlabel
#define NmappedWhenManaged XtNmappedWhenManaged
#define Nwidth XtNwidth
#define Nresizable(x) XtNresizable,x,
#define setLabel(x,y) char* x = y;
#define freeLabel(x)

#endif	// -----------------------------------------------------

static Cursor barCursor = 0;

//=================>>> vProgressCmd::vProgressCmd <<<=======================
  vProgressCmd::vProgressCmd(vCmdParent* dp, CommandObject* dc) :
	    vCmd(dp, dc)
  {
    initialize();			// and initialize
  }

//=======================>>> vProgressCmd::~vProgressCmd <<<=======================
  vProgressCmd::~vProgressCmd()
  {

    SysDebug(Constructor,"vProgressCmd::~vProgressCmd() Destructor\n")

  }

//=====================>>> vProgressCmd::initialize <<<=======================
  void vProgressCmd::initialize()
  {

    SysDebug(Constructor,"vProgressCmd::vProgressCmd() constructor\n")

    CopyToLocal();		// make local copies

    // Create bitmaps if need to

#ifndef Motif
    if (barCursor == 0)
      {
	barCursor = XCreateFontCursor(theApp->display(),XC_top_left_arrow);
      }
#endif

    Widget WfHoriz = _parentWin->getWidgetFromId(dlgCmd->cRightOf);
    Widget WfVert = _parentWin->getWidgetFromId(dlgCmd->cBelow);

    _isVert = (dlgCmd->attrs & CA_Vertical);
    _perCent = (dlgCmd->attrs & CA_Percent);

    Dimension ProgressHeight = (dlgCmd->attrs & CA_Large) ? 180 : 110;
    if ((dlgCmd->attrs) & CA_Small)
	ProgressHeight = 60;

    if (dlgCmd->size > 0)
	ProgressHeight = dlgCmd->size;		// they gave size

    _ScrlTop = 0;		// Always at top

    int* minMax = (int *)_itemList;	// giving range?
    _minVal = 0; _maxVal = 100;			// default min/max
    if (minMax != 0)				// They gave a range list
      {
	_minVal = minMax[0];
	_maxVal = minMax[1];
      }
    if (_minVal > _maxVal)
      {
	SysDebug2(BadVals,"vProgressCmd:vProgressCmd - bad range %d to %d\n",_minVal,_maxVal)
	_minVal = 0; _maxVal = 100;	// make some sense!
      }

    if (_retVal < _minVal)	// set a legal value for the top
	_curVal = _minVal;
    else if (_retVal > _maxVal)
	_curVal = _maxVal;
    else
	_curVal = _retVal;

    int map = !(dlgCmd->attrs & CA_Hidden);

    // Create the surrounding box
#ifdef Motif
    wCmd = XtVaCreateManagedWidget(
	"vProgressBox",
	xmRowColumnWidgetClass,
	wParent,			// parent
	XmNorientation, (_isVert) ? XmVERTICAL : XmHORIZONTAL,
	Nbelow(WfVert)			// Form constratints
	NrightOf(WfHoriz)
	XmNspacing,0,
	NmappedWhenManaged, map,
        NULL);
#else
    wCmd = XtVaCreateManagedWidget(
	"vProgressBox",
	boxWidgetClass,
	wParent,			// parent
	XtNorientation, (_isVert) ? XtorientVertical : XtorientHorizontal,
	Nbelow(WfVert)			// Form constratints
	NrightOf(WfHoriz)
	XtNresizable, TRUE,
	XtNhSpace,0,
	XtNvSpace,0,
#ifdef USE3D
	XtNborderWidth,0,
	XtNbackground, _vControlFace,
#else
	XtNbackground, _vControlBG,
#endif
	NmappedWhenManaged, map,
        NULL);
#endif

    int fw = 0;

    if ((dlgCmd->attrs) & CA_NoLabel)
	_wLabel = 0;
    else
      {

	setLabel(tmp, "   ");

	fw = XTextWidth(theApp->_XDefaultFont,"   ",3);

	_wLabel = XtVaCreateManagedWidget(
	    "vProgressLabel",		// name
	    lblWidgetClass,	// class
	    wCmd,			// parent
#ifdef Motif
	    XmNborderWidth,1,
	    XmNmarginTop,1,
	    XmNmarginBottom,1,
	    XmNalignment,XmALIGNMENT_CENTER,
#else
	    XtNinternalHeight,1,	// Narrow height
	    XtNbackground, _vControlBG,
#endif
	    Nwidth,fw+4,		// width of "   "
	    Nresizable(FALSE)		// don't resize
	    Nlabel,tmp,		// blank label
	    NULL);
       freeLabel(tmp)
      }

    int thickness = 15;

    int temp;
    if (_isVert && fw != 0)
	thickness = fw + 6;
    else
	thickness = theApp->_XDefaultFont->ascent +
		    theApp->_XDefaultFont->descent + 4;

#ifdef Motif
    int h = thickness;
    int w = ProgressHeight;
    if (_isVert)		// Different hw
      {
	w = thickness; h = ProgressHeight;
      }
    
    _wScroll = XtVaCreateManagedWidget(
	"vProgressBar",		// name
	xmScrollBarWidgetClass,	// class
	wCmd,			// parent
	XmNorientation, (_isVert) ? XmVERTICAL : XmHORIZONTAL,
	XmNminimum,0,		// go from 0 to 100
	XmNmaximum,100,
	XmNshowArrows,False,	// No arrows
	XmNheight, h,
	XmNwidth, w,
//	XmNsensitive, 0,		// not sensitive to input
	XmNborderWidth, 0,		// no border
	NULL);
    if (_isVert)
	XtVaSetValues(_wScroll,XmNprocessingDirection,XmMAX_ON_TOP,NULL);
    else
	XtVaSetValues(_wScroll,XmNprocessingDirection,XmMAX_ON_RIGHT,NULL);

#else		// Athena
    _wScroll = XtVaCreateManagedWidget(
	"vProgressBar",		// name
#ifdef USE3D
	slider3dWidgetClass,	// class
#else
	scrollbarWidgetClass,	// class
#endif
	wCmd,			// parent
	XtNorientation, (_isVert) ? XtorientVertical : XtorientHorizontal,
	XtNminimumThumb,1,	// 0 doesn't work! causes NO thumb
	XtNthickness, thickness,	// how wide
	XtNlength,ProgressHeight,	// high as Progress
	XtNresizable, TRUE,
	XtNsensitive, 0,		// not sensitive to input
	XtNborderWidth, 0,		// no border
	XtNthumb,None,
	XtNbackground, _vControlFace,
	XtNscrollDCursor, barCursor,
	XtNscrollHCursor, barCursor,
	XtNscrollLCursor, barCursor,
	XtNscrollRCursor, barCursor,
	XtNscrollUCursor, barCursor,
	XtNscrollVCursor, barCursor,
#ifdef USE3D
	XtNisProgress,1,
	XtNslider3dBackground, _vDialogBG,
	XtNforeground,_vControlBG,
#else
#endif
	NULL);
#ifndef USE3D
    if (theApp->Xdepth() > 1)
	XtVaSetValues(_wScroll,
	    XtNforeground, _vControlFace,
	    NULL);
#endif
	
#endif

    SetCmdVal(_curVal,Value);	// set scroll and label
    wBox = 0;
  }

//==================>>> vProgressCmd::GetCmdValue <<<=========================
  int vProgressCmd::GetCmdValue(ItemVal id)
  {
    if (id != _cmdId)
	return -1;
    return _curVal;
  }

//=====================>>> vProgressCmd::mapVal <<<=========================
  int vProgressCmd::mapVal(int val)
  {
    float range = (float)(_maxVal - _minVal);
    if (range == 0)
	return 100;
    float fval = (float)(val - _minVal) * (100. / range);
    return (int) fval;
  }

//=====================>>> vProgressCmd::SetCmdVal <<<=========================
  void vProgressCmd::SetCmdVal(ItemVal val, ItemSetType st)
  {

    SysDebug2(Misc,"vProgressCmd::SetCmdVal(val:%d, type:%d)\n",val,st)

    if (st == Value)
      {
	if (val > _maxVal || val < _minVal )
	    return;
	_retVal =
	_curVal = val;
	// Now set appropriate _curVal, scroll, and highlight
	_ScrlShown = mapVal(_curVal);
	SetScroll(_ScrlShown, _ScrlTop);

	// Set label if there!
	if (_wLabel != 0)		// has a label
	  {
	    char buff[20];
	    IntToStr(_curVal,buff);	// string representation
	    if (_perCent)
		strcat(buff,"%");
	    setLabel(tmp,buff)
	    XtVaSetValues(_wLabel, 
		Nlabel,tmp,
#ifdef Motif
		XmNalignment,XmALIGNMENT_CENTER,
#endif
		NULL);
	    freeLabel(tmp)
	  }
      }
    else if (st == Hidden)		// hide or unhide
      {
	if (val)
	  {
	    XtUnmapWidget(wCmd);	// unmap this widget
	  }
	else
	  {
	    XtMapWidget(wCmd);	// unmap this widget
	  }
      }
  }

//====================>>> vProgressCmd::SetScroll <<<=======================
  void vProgressCmd::SetScroll(int Shown, int Top)
  {
    Arg args[3];		// Used to pass float value

    int shown = Shown;
    int top = Top;		// work with local copies

#ifdef Motif
    XmScrollBarSetValues(_wScroll,top,shown,1,1,0);
#else
    if (_isVert)
      {
	top = 100 - shown;
#ifndef USE3D
	if (top >= 100)		// Bug in Xt scroll bar
	    top = 99;
#endif
      }

    union
      {
	XtArgVal arg_value;
	float float_value;
      } shown_value, top_value;

    shown_value.float_value = float (shown / 100.0);
    top_value.float_value = float (top / 100.0);

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

    XtSetValues(_wScroll, args, 2);		// change it!
#endif
  }
