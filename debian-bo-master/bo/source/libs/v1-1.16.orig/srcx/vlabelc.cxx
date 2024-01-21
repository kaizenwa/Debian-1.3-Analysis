//===============================================================
// vlabelc.cxx	- label Cmd
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vxutil.h>	// Motif/Athena mapping
#include <v/vlabelc.h>	// our definitions
#include <v/vcmdprnt.h>	// a command parent
#include <v/vicon.h>	// for icons
#include <v/vctlclrs.h>

#undef USE3D		// doesn't work yet...

extern "C"
{

#ifdef Motif
#include <Xm/Xm.h>
#include <Xm/Label.h>
#else
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Form.h>
#endif

#ifdef USE3D
#include <v/awlbl3d.h>
#endif
}

#ifdef Motif	// --------------------------------------

#define lblWidgetClass xmLabelWidgetClass
#define NborderWidth XmNborderWidth
#define Ncallback XmNactivateCallback
#define Nheight XmNheight
#define Nlabel XmNlabelString
#define NmappedWhenManaged XmNmappedWhenManaged
#define Nwidth XmNwidth
//@@ These use athena parent for now
#define Nresizable(x) // XtNresizable,x,

#define setLabel(x,y) XmString x = XmStringCreateSimple(y);
#define freeLabel(x) XmStringFree(x);

#else	//------------------------------------------------------

#define lblWidgetClass labelWidgetClass
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

//=====================>>> vLabelCmd::vLabelCmd <<<=======================
  vLabelCmd::vLabelCmd(vCmdParent* dp, CommandObject* dc):
	    vCmd(dp, dc)
  {
    initialize();
  }

//=====================>>> vLabelCmd::~vLabelCmd <<<=======================
  vLabelCmd::~vLabelCmd()
  {

    SysDebug(Destructor,"vLabelCmd::~vLabelCmd() destructor\n")
  }

//=====================>>> vLabelCmd::initialize <<<=======================
  void vLabelCmd::initialize(void)
  {
    // build a button command for use in a parent window


    SysDebug(Constructor,"vLabelCmd::vLabelCmd() constructor\n")

    CopyToLocal();		// make local copies

    Widget WfHoriz = _parentWin->getWidgetFromId(dlgCmd->cRightOf);
    Widget WfVert = _parentWin->getWidgetFromId(dlgCmd->cBelow);

    Pixmap pixmap = 0;		// normally, no pixmap

    int map = !(dlgCmd->attrs & CA_Hidden);

    if (dlgCmd->cmdType == C_Icon && _itemList != 0)	// icon
      {
	vIcon* ip = (vIcon *) _itemList;	// to access bitmap

	pixmap = ip->GetXPM(1);			// get the pixmap

	wCmd = XtVaCreateManagedWidget(
	    "vLabel",		// name
	    lblWidgetClass,	// class
	    wParent,		// parent
	    Nbelow(WfVert)		// Form constratints
	    NrightOf(WfHoriz)
#ifdef Motif
	    XmNlabelType, XmPIXMAP,
	    XmNlabelPixmap, pixmap,
	    XmNmarginTop,0,
	    XmNmarginBottom,0,
	    XmNmarginLeft,0,
	    XmNmarginRight,0,
#else
	    XtNbitmap, pixmap,	// the pixmap
	    XtNinternalWidth,0,
	    XtNinternalHeight,0,
	    XtNbackground, _vDialogBG,
#endif
	    NborderWidth,0,	// label only, no border
	    Nresizable(TRUE)
	    NmappedWhenManaged, map,
	    NULL);
      }
    else if (dlgCmd->cmdType == C_Blank)
      {
	// a blank is always unmapped
	setLabel(tmp,_title)

	wCmd = XtVaCreateManagedWidget(
	    "vLabel",			// name
	    lblWidgetClass,		// class
	    wParent,			// parent
	    Nbelow(WfVert)		// Form constratints
	    NrightOf(WfHoriz)
	    Nlabel, tmp,		// label
	    NborderWidth,0,		// label only, no border
	    NmappedWhenManaged,FALSE, // just the space!
#ifdef Motif
	    XmNmarginTop,2,
	    XmNmarginBottom,2,
	    XmNmarginLeft,2,
	    XmNmarginRight,2,
#else
	    XtNinternalWidth,2,
	    XtNinternalHeight,3,  // adjust to make same height as button
#endif
	    Nresizable(TRUE)
	    NULL);
	freeLabel(tmp)
      }
    else			// (dlgCmd->cmdType == C_Label)
      {
	int borderW = (dlgCmd->cmdType == C_BoxedLabel) ? 1 : 0;
	    
	setLabel(tmp,_title)
#ifdef USE3D
	if (dlgCmd->cmdType == C_BoxedLabel)
	  {
	    wCmd = XtVaCreateManagedWidget(
	        "vLabel",			// name
		label3dWidgetClass,		// class
		wParent,			// parent
		Nbelow(WfVert)		// Form constratints
		NrightOf(WfHoriz)
		XtNlabel, tmp,		// label
		"shadowWidth",2,	// border width
//		XtNinternalWidth,2,
//		XtNinternalHeight,3,  // adjust to make same height as button
		Nresizable(TRUE)
		NmappedWhenManaged, map,
		NULL);
	  }
	else
#endif
	wCmd = XtVaCreateManagedWidget(
	    "vLabel",			// name
	    lblWidgetClass,		// class
	    wParent,			// parent
	    Nbelow(WfVert)		// Form constratints
	    NrightOf(WfHoriz)
	    Nlabel, tmp,		// label
	    NborderWidth,borderW,	// border width
#ifdef Motif
	    XmNmarginTop,2,
	    XmNmarginBottom,2,
	    XmNmarginLeft,2,
	    XmNmarginRight,2,
#else
	    XtNinternalWidth,2,
	    XtNinternalHeight,3,  // adjust to make same height as button
#endif
	    Nresizable(TRUE)
	    NmappedWhenManaged, map,
	    NULL);
	freeLabel(tmp)
      }
#ifndef Motif
     XtVaSetValues(wCmd,         // the widget to set
          XtNbackground, _vDialogBG, NULL);
#endif

    wBox = 0;		// no Box, but make copy for placement

  }

//================>>> vLabelCmd::SetCmdVal <<<============================
  void vLabelCmd::SetCmdVal(ItemVal val, ItemSetType st)
  {

    if (st == Hidden)		// hide or unhide
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

//================>>> vLabelCmd::SetCmdStr <<<============================
  void vLabelCmd::SetCmdStr(char* str)
  {

    SysDebug1(Misc,"vLabelCmd::SetCmdStr(str:%s)\n",str)

    _title = str;
    setLabel(tmp,str)
    XtVaSetValues(wCmd,		// the widget to set
	Nlabel, tmp, // label
	NULL);
    freeLabel(tmp)
  }
