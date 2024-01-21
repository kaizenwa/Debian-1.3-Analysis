//===============================================================
// vCommandPane - the button bar pane class used by the vWindow class
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vcmdpane.h>
#include <v/vctlclrs.h>

extern "C"
{
#ifdef Motif
#include <Xm/RowColumn.h>
#else		// *** ATHENA

#include <X11/Xaw/Box.h>
#include <X11/Xaw/Paned.h>
#endif
}

#include <v/vwindow.h>		// we are part of a window
#include <v/vfont.h>

#ifndef Motif
#define gray_width 2
#define gray_height 2
static char gray_bits[] = {
   0x01, 0x02};
static Pixmap gray_bitmap = 0;
#endif

//===================>>> vCommandPane::vCommandPane <<<====================
  vCommandPane::vCommandPane(CommandObject* cList) :
	vPane(P_Commands), vCmdParent()
  {
    SysDebug(Constructor,"vCommandPane::vCommandPane() constructor\n")

    _origList = cList;		// handle to list of commands
  }

//===================>>> vCommandPane::~vCommandPane <<<====================
  vCommandPane::~vCommandPane()
  {
    SysDebug(Destructor,"vCommandPane::~vCommandPane() destructor\n")
  }

//==================>>> vCommandPane::ProcessCmd <<<=======================
  void vCommandPane::ProcessCmd(ItemVal id, ItemVal rv, CmdType ct)
  {
    // simple interface between command objects and the
    // inherited vCmdParent ProcessCmd to the CommandObject which does the work

    _parentWin->WindowCommand(id, rv, ct);
  }

//====================>>> vCommandPane::GetPaneValue <<<======================
  int vCommandPane::GetPaneValue(ItemVal id, int& val)
  {
    if (HasId(id))		// make sure the id is in the pane
      {
	val = GetValue(id);
	return 1;		// assume 0 if not found
      }
    else
	return 0;	// can't find this id
  }

//===================>>> vCommandPane::SetPaneValue <<<========================
  void vCommandPane::SetPaneValue(ItemVal id, int val, ItemSetType setType)
  {
    SetValue(id,val,setType);
  }

//====================>>> vCommandPane::SetPaneString <<<======================
  void vCommandPane::SetPaneString(ItemVal id, char* str)
  {
    SetString(id,str);
  }

//====================>>> vCommandPane::ShowPane <<<======================
  void vCommandPane::ShowPane(int OnOrOff)
  {
    if (OnOrOff)
      {
	XtManageChild(_wDialog);
      }
    else
      {
	XtUnmanageChild(_wDialog);
      }
  }

//====================>>> vCommandPane::initialize <<<=======================
  void vCommandPane::initialize(vWindow* pWin, Widget pWidget)
  {
     // now, build the menu items in the widget provided

    DlgCmdList*	curCmd;

#ifndef Motif	
    if (gray_bitmap == 0)		// haven't built the pixmap yet
      {
	gray_bitmap = XCreatePixmapFromBitmapData(
	    theApp->display(),				// our display
	    DefaultRootWindow(theApp->display()),	// a Drawable
	    gray_bits,		// the gray bitmap data
	    gray_width,		// from the file
	    gray_height,
	    (unsigned long) theApp->Xfg(),	// Forground color
	    (unsigned long) theApp->Xbg(),	// Background color
	    DefaultDepth(theApp->display(),DefaultScreen(theApp->display())));

      }
#endif

    vPane::initialize(pWin, pWidget);	// initialize base class

    // Now the Command bar that will have Commands

#ifdef Motif
    _wDialog = XtVaCreateManagedWidget(
	"vCommandPane",
	xmRowColumnWidgetClass,		// widget class
	_baseW,			// parent widget
	// PanedWindow constrants
	XmNallowResize,1,
	XmNskipAdjust,1,
	// RowColumn values
	XmNspacing,2,
	XmNmarginHeight,1,
	XmNmarginWidth,1,
	XmNnumColumns,1,
	XmNadjustMargin,0,
	XmNadjustLast,0,
	XmNorientation,XmHORIZONTAL,
	XmNpacking,XmPACK_TIGHT,
	XmNisAligned,0,
	NULL);			// argument list
#else
    _wDialog = XtVaCreateManagedWidget(
	"vCommandPane",
	boxWidgetClass,		// widget class
	_baseW,			// parent widget
	XtNshowGrip,FALSE,
	XtNallowResize,1,
	XtNresizeToPreferred,1,
	XtNskipAdjust,TRUE,
	XtNvSpace,2,
	NULL);			// argument list

    // set the background pattern
    if (theApp->Xdepth() <= 1)
        XtVaSetValues(_wDialog,         // the widget to set
          XtNbackgroundPixmap, gray_bitmap, NULL);
    else
        XtVaSetValues(_wDialog,         // the widget to set
          XtNbackground, _vDialogBG, NULL);  
#endif

    for (int ix = 0 ; _origList && (_origList[ix].cmdType != C_EndOfList) ;
		 ++ix)
      {
	curCmd = new DlgCmdList;		// get a new cell

	curCmd->nextDCL = _cmdList;		// add in at front
	_cmdList = curCmd;	

	curCmd->cmdP = AddCmd(&_origList[ix]);

      }
  }
