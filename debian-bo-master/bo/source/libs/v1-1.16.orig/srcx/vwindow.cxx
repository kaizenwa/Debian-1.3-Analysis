//===============================================================
// vWindow.cxx - vWindow class functions - X11R5
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vapp.h>		// we are a friend of App
#include <v/vwindow.h>		// our header
#include <v/vpane.h>		// we have panes
#include <v/vx2vkey.h>		// to map x key to v key
#include <v/vkeys.h>
#include <v/vctlclrs.h>

extern "C"
{
#include <X11/Xlib.h>
#include <X11/Core.h>
#include <X11/Shell.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>


#ifdef Motif
#include <Xm/PanedW.h>
#include <Xm/RowColumn.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Form.h>
#else
#ifdef USE3D
#define	cmdWidgetClass command3dWidgetClass
#include <v/awcmd3d.h>

#else		// ATHENA

#define	cmdWidgetClass commandWidgetClass
#include <X11/Xaw/Command.h>
#endif

#include <X11/Xaw/Box.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Form.h>
#endif
}

// Define static data of the class

    int vWindow::_numWindows = 0;
    vWindow* vWindow::_WinList = 0;

#ifdef USE_CLOSE_BOX
#define close_width 13
#define close_height 12
static char close_bits[] = {
   0xff, 0x1f, 0x01, 0x10, 0x0d, 0x16, 0x19, 0x13, 0xb1, 0x11, 0xe1, 0x10,
   0xe1, 0x10, 0xb1, 0x11, 0x19, 0x13, 0x0d, 0x16, 0x01, 0x10, 0xff, 0x1f};
static Pixmap close_bitmap = 0;
#endif

#ifndef Motif
#define gray_width 2
#define gray_height 2
static char gray_bits[] = {
   0x01, 0x02};
static Pixmap gray_bitmap = 0;
#endif


//======================>>> vWindow::vWindow <<<=======================
  vWindow::vWindow(char *name, int width, int height, WindowType wintype ) :
    vBaseWindow(name)		// constructor
  {

    SysDebug2(Constructor,"vWindow::vWindow(height %d, width %d) constructor\n",height,width)

    // make widgets null
    _menuPane = 0;
    _menuBar = 0;		// the button bar widget
    _closeButton = 0;		// close button widget

    _paneList = 0;		// no panes yet

    // Set height and width using default if 0 supplied
    _WinHeight = height ? height : theApp->DefaultHeight();
    _WinWidth = width ? width : theApp->DefaultWidth();

    _wType = wintype;

    _NextWin = _WinList;	// link in at top
    _WinList = this;

     _vHandle = 0;
     wm_delete_added = 0;

     initialize();		// set this guy up
  }

//======================>>> vWindow::vWindow <<<=======================
  vWindow::vWindow(const vWindow& w) : 		// Copy Constructor
    vBaseWindow(w)
  {
    // Copy is shallow, but we track that it is a copy
    vSysError("vWindow - V semantics do not support copy constructors!");
  }

//======================>>> vWindow::~vWindow <<<=======================
  vWindow::~vWindow()			// destructor
  {

    SysDebug(Destructor,"vWindow::~vWindow destructor\n")

    if (_vHandle == 0)
	return;
    PaneList* temp;
    for (PaneList* pl = _paneList ; pl != 0 ; )
      {
	temp = pl;
	pl = pl->nextPL;
	delete temp;		// delete the PaneLists we allocated
      }
  }

//======================>>> vWindow::AddPane <<<=======================
  void vWindow::AddPane(vPane* add_pane)
  {
    // Add a pane to the window

    PaneList* newPane = new PaneList;	// add a new pane to the list

    if (_topLevel == 0)			// no top level yet
      {
	vSysWarning("In vWindow::AddPane - topLevel not defined yet.");
	return;
      }

    SysDebug1(Build,"vWindow::AddPane() to %s\n",_name)

    newPane->pane = add_pane;		// this is the pane we are adding

    newPane->nextPL = _paneList;	// add new pane in at top
    _paneList = newPane;

    if (add_pane->_paneType == P_Menu)	// P_Menu is special
      {
	vAddMenuBar();			// Make sure we got one
	_menuPane = add_pane;		// Remember menu pane
	add_pane->initialize(this, _menuBar);
#ifdef Motif
	XtManageChild(_menuBar);
#endif
      }
    else
	add_pane->initialize(this, _topLevel);	// add to _toplevel paneWidget
  }

//======================>>> vWindow::initialize <<<=======================
  void vWindow::initialize(void)
  {
    // All top level windows in this frame are implemented
    // as popup shells based on the vApp's base widget.

    // Create a shell window that will serve as basis for all windows

    if (_wType == CMDWINDOW)
      {
	_vHandle = XtVaCreatePopupShell(
	    _name,				// use name supplied
	    applicationShellWidgetClass,	// an app shell
	    theApp->vHandle(),		// based on App window
	    XtNtitle,_name,
	    XtNallowShellResize, TRUE,
	    NULL);
      }
    else
      {
	_vHandle = XtVaCreatePopupShell(
	    _name,				// use name supplied
	    topLevelShellWidgetClass,		// a toplevel shell
	    theApp->vHandle(),		// based on App window
	    XtNtitle,_name,
	    XtNallowShellResize, TRUE,
	    XtNwidth, _WinWidth,
	    XtNheight, _WinHeight,
	    NULL);
      }

    // Now the outer window to manage the children 

#ifdef Motif
    _topLevel = XtVaCreateManagedWidget(
	"vWindow",	// Widget name
	xmPanedWindowWidgetClass,	// widget class
	_vHandle,		// parent widget
	XmNsashHeight,5, XmNsashWidth,8,
	XmNsashIndent,-8, XmNsashShadowThickness,2,
        XmNspacing,4,
	NULL);
    if (theApp->_frameWidth > 0 && theApp->_frameHeight > 0)
      {
	XtVaSetValues(_topLevel,		// the widget to set
	  XmNwidth, theApp->_frameWidth,
	  XmNheight, theApp->_frameHeight,
	  NULL);
      }

#else		// **** Athena

    _topLevel = XtVaCreateManagedWidget(
	"vWindow",	// Widget name
	panedWidgetClass,	// widget class
	_vHandle,		// parent widget
        XtNvSpace,0,
	NULL);
    if (theApp->_frameWidth > 0 && theApp->_frameHeight > 0)
      {
	XtVaSetValues(_topLevel,		// the widget to set
	  XtNwidth, theApp->_frameWidth,
	  XtNheight, theApp->_frameHeight,
	  NULL);
      }

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

    // Add the delete protocol
//    wm_delete_window = XInternAtom(theApp->display(), "WM_DELETE_WINDOW",
//        False);

//    XSetWMProtocols(theApp->display(), XtWindow(_topLevel),
//                    &wm_delete_window, 1);

#ifndef Motif
    XtAddEventHandler(_topLevel,KeyPressMask, 0,
	CKeyInEV,(XtPointer)this);
#endif

    ++_numWindows;		// bump how many windows up
  }

//============================>>> vWindow::vAddMenuBar <<<==========================
  void vWindow::vAddMenuBar(void)
  {
    if (_menuBar != 0)
	return;

    // Now the menu bar that will have close and menus
#ifdef Motif
#define MotifMenuBars
#ifdef MotifMenuBars
    _menuBar = XmCreateMenuBar(_topLevel,"vMenuBar",NULL,0);
#else
    _menuBar = XtVaCreateManagedWidget(
	"vMenuBar",
	xmRowColumnWidgetClass,		// widget class
	_topLevel,			// parent widget
	XmNspacing,2,
	XmNmarginHeight,1,
	XmNmarginWidth,1,
	XmNnumColumns,1,
	XmNadjustMargin,0,
	XmNadjustLast,0,
	XmNorientation,XmHORIZONTAL,
	XmNpacking,XmPACK_TIGHT,
	XmNisAligned,0,
	XmNallowResize,1,
	NULL);			// argument list
#endif

#else		//**** ATHENA

    _menuBar = XtVaCreateManagedWidget(
	"vMenuBar",
	boxWidgetClass,		// widget class
	_topLevel,		// parent widget
	XtNshowGrip,FALSE,
	XtNborderWidth,0,
	XtNvSpace, 0,
	XtNallowResize,1,	// Allow to resize
	XtNskipAdjust,TRUE,	// Don't readjust if possible
	XtNresizeToPreferred,1,  // try to make it original size
	NULL);			// argument list

    // set the background pattern
    if (theApp->Xdepth() <= 1)
	XtVaSetValues(_menuBar,		// the widget to set
	  XtNbackgroundPixmap, gray_bitmap, NULL);
    else
	XtVaSetValues(_menuBar,		// the widget to set
	  XtNbackground, _vMenuBarBG, NULL);
#endif

    // Now the Close button 

#ifndef USE_CLOSE_BOX
    _closeButton = 0;			// don't use close box
#else
    if (close_bitmap == 0)		// haven't built the pixmap yet
      {
	close_bitmap = XCreatePixmapFromBitmapData(
	    XtDisplay(_vHandle),	// our display
	    DefaultRootWindow(XtDisplay(_vHandle)),  // a Drawable
	    close_bits,			// the close bitmap data
	    close_width,		// from the file
	    close_height,
	    theApp->Xfg(),_vControlFace,
	    DefaultDepth(theApp->display(),DefaultScreen(theApp->display())));
      }

    _closeButton = XtVaCreateManagedWidget(
	"vCloseButton",
	cmdWidgetClass,		// widget class 
	_menuBar,		// parent widget
	XtNbitmap,close_bitmap,	// an icon
	XtNinternalHeight, 1,
	XtNinternalWidth, 1,
	XtNborderWidth,0,
	XtNbackground, _vControlFace,
	NULL);			// argument list

    XtAddCallback(_closeButton, XtNcallback, CcloseCB, (XtPointer)this);
#endif
  }

//============================>>> vWindow::CloseWin <<<==========================
  void vWindow::CloseWin(void)
  {
    //	close button class callback
    //
    vWindow* w_list;

    // close base window, which cleans up some stuff

    SysDebug1(Build,"vWindow::CloseWin() - %s.\n",_name)

    closeBaseWindow();		// close this window

    --_numWindows;
    // Remove from active list

    if (_WinList == this)		// first one on list
      {
	_WinList = _WinList->_NextWin;	// point to next
      }
    else
      {
	for (w_list = _WinList ; w_list != 0 ; w_list = w_list->_NextWin)
	  {
	    if (w_list->_NextWin == this)
	      {
		// remove from list
		w_list->_NextWin = (w_list->_NextWin)->_NextWin;
		break;
	      }
	  }
      }

  }

extern "C"
{
#ifdef USE_CLOSE_BOX
//============================>>> CcloseCB <<<==========================
  void CcloseCB(Widget w, XtPointer client_data, XtPointer call_data)
  { 
    //	close button "C" Call Back
    //
    // client_data will have the this pointer of our object

    vWindow* thisWindow = (vWindow*) client_data;	// get back this
    if (theApp->IsHelpWin(thisWindow))
	theApp->CloseHelpWin(thisWindow);
    else
	theApp->CloseAppWin(thisWindow);
  }
#endif

//============================>>> CwmDeleteCB <<<==========================
  void CwmDeleteCB(Widget w, XtPointer client_data, XEvent* e)
  { 
    //	WM_DELETE protocol
    //
    // client_data will have the this pointer of our object
    // Apparently things get recursive with WM_DELETE, so we
    // need the inUse flag.

    static int inUse = 0;
    XClientMessageEvent* c = (XClientMessageEvent*)e;
    
    if (inUse || e->type != ClientMessage || 
	c->message_type != theApp->vx_wm_protocols)
	return;

    inUse = 1;

    vWindow* thisWindow = (vWindow*) client_data;	// get back this

    if (theApp->IsHelpWin(thisWindow))
	theApp->CloseHelpWin(thisWindow);
    else
	theApp->CloseAppWin(thisWindow);

    inUse = 0;
  }
}

//======================>>> vWindow::ShowWindow <<<=======================
  void vWindow::ShowWindow()
  {
    int t1, t2;

    // X11R5 Paned doesn't seem to quite work right. These
    // calles force the pane to resize its children.

#ifndef Motif
    XawPanedSetRefigureMode(_topLevel, 0);
    XawPanedSetRefigureMode(_topLevel, 1);
//    XawPanedGetMinMax(_topLevel,&t1,&t2);
#endif

    vBaseWindow::ShowWindow();


    // Setup protocol stuff for this window

    if (!wm_delete_added)
      {
	wm_delete_added = 1;
	XSetWMProtocols(theApp->display(), XtWindow(_vHandle),
	    &theApp->vx_wm_delete_window, 1);
	XtAddEventHandler(_vHandle, (EventMask)0L, 1,
	    (XtEventHandler)CwmDeleteCB, (XtPointer)this);
      }
  }

//======================>>> vWindow::ShowPane <<<=======================
  void vWindow::ShowPane(vPane* wpane, int OnOrOff)
  {
    // Show or hide this pane

    // Search all panes until we find this one
    for (PaneList* pl = _paneList ; pl != 0 ; pl = pl->nextPL)
      {
	if (pl->pane == wpane)
	  {
	    (pl->pane)->ShowPane(OnOrOff);	// let each pane show/hide
	    return;
	  }
      }

    return;
  }

//====================>>> vWindow::GetValue <<<======================
  int vWindow::GetValue(ItemVal id)
  {
    // scan all buttons in this window to retrieve the what value

    int retval = 0;		// default to 0

    // Search all panes until we find this id
    for (PaneList* pl = _paneList ; pl != 0 ; pl = pl->nextPL)
      {
	if ((pl->pane)->GetPaneValue(id, retval))
	  {
	    return retval;
	  }
      }

    return 0;
  }

//====================>>> vWindow::GetMenuThis <<<======================
  vMenuPane* vWindow::GetMenuThis(Widget w)
  {
    // scan menu object to see if it has the widget

    vMenuPane* mypane;
    int retval = 0;		// default to 0

    for (PaneList* pl = _paneList ; pl != 0 ; pl = pl->nextPL)
      {
	if ((pl->pane)->_paneType == P_Menu)
	  {
	    vMenuPane* menup = (vMenuPane*) pl->pane;
	    if ((mypane = menup->GetMenuThis(w)) != 0)
		return mypane;
	  }
      }

    return 0;
  }

//=========================>>> vWindow::KeyIn <<<==========================
  void vWindow::KeyIn(vKey keysym, unsigned int shift)
  {
    // General interface to a keyboard event

#ifdef vDEBUG
    if (vk_IsModifier(keysym))
	return;
    if (keysym < ' ')
      {
	SysDebug1(Misc,"vWindow::KeyIn: ^%c",keysym+ '@')
      }
    else if (keysym < 128)
      {
	SysDebug1(Misc,"vWindow::KeyIn: %c",keysym)
      }
    else
      {
	SysDebug1(Misc,"vWindow::KeyIn:<%x>",keysym);
      }
    SysDebug2(Misc," shift=%d in window %s.\n",shift,_name);
#endif

    theApp->KeyIn(this, keysym, shift);	// pass on unprocessed keys to app
  }

//=======================>>> vWindow::KeyInEV <<<==========================
  void vWindow::KeyInEV(unsigned int keycode, unsigned int state)
  {
    // Local interface to a keyboard event

    vKey vkey;
    KeySym keysym;
    Modifiers modifiers_used;
    ItemVal id;

    XtTranslateKeycode(theApp->display(),	// the display
		keycode,			// the keycode input
		state,				// and the shift state
		&modifiers_used,		// used in translation
		&keysym);			// what we care about

    vkey = MapXtoVkey(keysym, state);

    if (vkey == vk_None)
	return;

    if (_menuPane && ((vMenuPane*)_menuPane)->CheckAccel(vkey, state, id))
	MenuCommand(id);
    else
	KeyIn(vkey,state);			// pass on to general code
  }

extern "C"
{
//============================>>> CKeyInEV <<<==========================
  void CKeyInEV(Widget w, XtPointer client_data, XEvent* event, char *x)
  { 
    //	KeyPress Event Handler
    //
    // client_data will have the this pointer of our object

    XKeyPressedEvent *kp;

    kp = (XKeyPressedEvent*)event;
    vWindow* thisWindow = (vWindow*) client_data;	// get back this

    thisWindow->KeyInEV(kp->keycode, kp->state);
  }
}

//======================>>> vWindow::MenuCommand <<<==========================
  void vWindow::MenuCommand(ItemVal id)
  {
    //	Menu Commands go through here first, then the regular WindowCommand

    WindowCommand(id, id, C_Menu);
  }

//================>>> vWindow::GetPosition <<<========================
  void vWindow::GetPosition(int& left, int& top, int& width, int& height)
  {
    Dimension w,h,b;
    Position l,t;

    XtVaGetValues(_vHandle, XtNwidth, &w, XtNheight, &h,
		  XtNx, &l, XtNy, &t, XtNborderWidth, &b, NULL);
    
    left = l; top = t; width = w+b+b; height = h+b+b;

  }

//================>>> vWindow::SetPosition <<<========================
  void vWindow::SetPosition(int left, int top)
  {
   Position l = left;
   Position t = top;
   XtVaSetValues(_vHandle,XtNx,l, XtNy,t,NULL);
  } 

//================>>> vWindow::RaiseWindow <<<========================
  void vWindow::RaiseWindow(void)
  {
    // raise this window to the top of all other windows

    XRaiseWindow(XtDisplay(_vHandle),	// our display
	    XtWindow(_vHandle));	// our window

  }

//================>>> vWindow::SetString <<<========================
  void vWindow::SetString(ItemVal id, char* str) 
  {
    for (PaneList* pl = _paneList ; pl != 0 ; pl = pl->nextPL)
      {
	(pl->pane)->SetPaneString(id, str);
      }
  }

//================>>> vWindow::SetStringAll <<<========================
  void vWindow::SetStringAll(ItemVal id, char* str) 
  {
    // search all buttons in all windows to set to correct value

    for (vWindow* w_list = _WinList ; w_list != 0 ; w_list = w_list->_NextWin)
      {
	// in each window, search each pane list
	for (PaneList* pl = w_list->_paneList ; pl != 0 ; pl = pl->nextPL)
	  {
	    (pl->pane)->SetPaneString(id, str);
	  }
      }
  }

//======================>>> vWindow::SetTitle <<<==========================
  void vWindow::SetTitle(char* title)
  {
    // set the title in the title bar
  
    XtVaSetValues(_vHandle,
	XtNtitle,title,
	NULL);
  }

//==================>>> vWindow::SetValue <<<========================
  void vWindow::SetValue(ItemVal id, int val, ItemSetType setType)
  {
    for (PaneList* pl = _paneList ; pl != 0 ; pl = pl->nextPL)
      {
	(pl->pane)->SetPaneValue(id, val, setType);
      }
  }

//==================>>> vWindow::SetValueAll <<<========================
  void vWindow::SetValueAll(ItemVal id, int val, ItemSetType setType)
  {
    // search all windows to set all buttons
    for (vWindow* w_list = _WinList ; w_list != 0 ; w_list = w_list->_NextWin)
      {
	// in each window, search each pane list
	for (PaneList* pl = w_list->_paneList ; pl != 0 ; pl = pl->nextPL)
	  {
	    (pl->pane)->SetPaneValue(id, val, setType);
	  }
      }
  }

//======================>>> vWindow::WindowCommand <<<==========================
  void vWindow::WindowCommand(ItemVal id, ItemVal retval, CmdType ctype)
  {

    SysDebug1(CmdEvents,"vWindow::WindowCommand(id: %d)\n",id)

    theApp->AppCommand(this, id, retval, ctype); // Pass on unprocessed cmds to app
  }
