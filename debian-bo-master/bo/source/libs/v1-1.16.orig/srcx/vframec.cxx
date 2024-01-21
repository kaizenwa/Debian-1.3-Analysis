//===============================================================
// vframec.cxx	- a frame for holding commands
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vxutil.h>	// Motif/Athena mapping
#include <v/vapp.h>
#include <v/vframec.h>	// our definitions
#include <v/vcmdprnt.h>	// a command parent
#include <v/vctlclrs.h>

extern "C"
{
#include <X11/StringDefs.h>

#ifdef Motif
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#else
#include <X11/Xaw/Form.h>
#endif

}

#ifdef Motif	// --------------------------------------

#define formWidgetC xmFormWidgetClass
#define Ncallback XmNactivateCallback
#define Nheight XmNheight
#define Nlabel XmNlabelString
#define NmappedWhenManaged XmNmappedWhenManaged
#define Nwidth XmNwidth
#define NborderWidth XmNborderWidth

//@@ These use athena parent for now
#define Nresizable(x) // XtNresizable,x,

#define setLabel(x,y) XmString x = XmStringCreateSimple(y);
#define freeLabel(x) XmStringFree(x);

#else	//------------------------------------------------------

#define formWidgetC formWidgetClass
#define NborderWidth XtNborderWidth
#define Ncallback XtNcallback
#define Nheight XtNheight
#define Nlabel XtNlabel
#define NmappedWhenManaged XtNmappedWhenManaged
#define Nwidth XtNwidth
#define Nresizable(x) XtNresizable,x,
#define setLabel(x,y) char* x = y;
#define freeLabel(x)

#endif	// -----------------------------------------------------


//=====================>>> vFrameCmd::vFrameCmd <<<=======================
  vFrameCmd::vFrameCmd(vCmdParent* dp, CommandObject* dc) :
    vCmd(dp, dc)
  {
    initialize();
  }

//=====================>>> vFrameCmd::~vFrameCmd <<<=======================
  vFrameCmd::~vFrameCmd()
  {

    SysDebug(Destructor,"vFrameCmd::~vFrameCmd() destructor\n")

  }

//=====================>>> vFrameCmd::initialize <<<=======================
  void vFrameCmd::initialize(void)
  {
    // build a button command for use in a parent window


    SysDebug(Constructor,"vFrameCmd::vFrameCmd() constructor\n")

    CopyToLocal();

    Widget WfHoriz = _parentWin->getWidgetFromId(dlgCmd->cRightOf);
    Widget WfVert = _parentWin->getWidgetFromId(dlgCmd->cBelow);

    int border = (dlgCmd->attrs & CA_NoBorder) ? 0 : 1;    // border?

    int map = !(dlgCmd->attrs & CA_Hidden);

#ifdef Motif
    _Shadow = 0;
    if (border > 0)
      {
	_Shadow = XtVaCreateManagedWidget(
	    "vFrameShadow",			// name
	    xmFrameWidgetClass,		// class
	    wParent,			// parent
	    Nbelow(WfVert)			// Form constratints
	    NrightOf(WfHoriz)
	    Nresizable(TRUE)
	    NmappedWhenManaged, map,
	    XmNshadowType,XmSHADOW_ETCHED_IN,
	    NULL);
	wBox = XtVaCreateManagedWidget(
	    "vFrame",			// name
	    formWidgetC,			// class
	    _Shadow,			// parent
	    NrightOf(WfHoriz)
	    Nresizable(TRUE)
	    Nresizable(TRUE)
	    NmappedWhenManaged, map,
	    NULL);
      }
    else
#endif

    wBox = XtVaCreateManagedWidget(
	"vFrame",			// name
	formWidgetC,			// class
	wParent,			// parent
	Nbelow(WfVert)			// Form constratints
	NrightOf(WfHoriz)
	Nresizable(TRUE)
	NmappedWhenManaged, map,
	NborderWidth, border,
	NULL);

#ifdef Motif
    if (dlgCmd->attrs & CA_NoSpace)	// Tight spacing?
      {
	XtVaSetValues(wBox,		// the widget to set
	    XmNverticalSpacing,0,
	    XmNhorizontalSpacing,0,
	    NULL);
      }
    else
      {
	XtVaSetValues(wBox,		// the widget to set
	    XmNverticalSpacing,3,
	    XmNhorizontalSpacing,3,
	    NULL);
      }
#else
    if (dlgCmd->attrs & CA_NoSpace)	// Tight spacing?
      {
	XtVaSetValues(wBox,		// the widget to set
	    XtNdefaultDistance,0,
	    NULL);
      }
    XtVaSetValues(wBox,         // the widget to set
          XtNbackground, _vDialogBG, NULL);  

#endif
    wCmd = 0;				// wCmd not used
  }

//================>>> vFrameCmd::SetCmdVal <<<============================
  void vFrameCmd::SetCmdVal(ItemVal val, ItemSetType st)
  {

    SysDebug1(Misc,"vFrameCmd::SetCmdVal(val:%d)\n",val)

    if (st == Hidden)		// hide or unhide
      {
	if (val)
	  {
	    ::XtUnmapWidget(wBox);	// unmap this widget
#ifdef Motif
	    if (_Shadow)
		::XtUnmapWidget(_Shadow);	// unmap this widget
#endif
	  }
	else
	  {
	    ::XtMapWidget(wBox);	// unmap this widget
#ifdef Motif
	    if (_Shadow)
		::XtMapWidget(_Shadow);	// unmap this widget
#endif
	  }
      }
    else if (st == Value && dlgCmd->cmdType == C_ToggleFrame)   // Toggle?
      {
	if (val)                // have to hide/show me first
	  {
	    ::XtMapWidget(wBox);	// unmap this widget
#ifdef Motif
	    if (_Shadow)
		::XtMapWidget(_Shadow);	// unmap this widget
#endif
	  }
	else
	  {
	    ::XtUnmapWidget(wBox);	// unmap this widget
#ifdef Motif
	    if (_Shadow)
		::XtUnmapWidget(_Shadow);	// unmap this widget
#endif
	  }
	_parentWin->SetFrameChildren(_cmdId,val); // and now set value of childe
      }
 }
