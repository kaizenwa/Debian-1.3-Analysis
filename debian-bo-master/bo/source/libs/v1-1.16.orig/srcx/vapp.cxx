//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
//                                                                         //
// VV         VV                                       VV         VV       //
//  VV       VV     V - A Portable C++ GUI Framework    VV       VV        //
//   VV     VV           designed and written by         VV     VV         //
//    VV   VV                                             VV   VV          //
//     VV VV              Bruce E. Wampler, Ph.D.          VV VV           //
//      VVV               e-mail: wampler@cs.unm.edu        VVV            //
//       V                                                   V             //
//                                                                         //
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
//                                                                         //
// vapp.cxx - The vApp control object (for X11R5)                          //
//                                                                         //
// Copyright (C) 1995,1996  Bruce E. Wampler                               //
//                                                                         //
// This file is part of the V C++ GUI Framework.                           //
//                                                                         //
// This library is free software; you can redistribute it and/or           //
// modify it under the terms of the GNU Library General Public             //
// License as published by the Free Software Foundation; either            //
// version 2 of the License, or (at your option) any later version.        //
//                                                                         //
// This library is distributed in the hope that it will be useful,         //
// but WITHOUT ANY WARRANTY; without even the implied warranty of          //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       //
// Library General Public License for more details.                        //
//                                                                         //
// You should have received a copy of the GNU Library General Public       //
// License along with this library (see COPYING.LIB); if not, write to the //
// Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. //
//                                                                         //
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//

#include <v/vapp.h>		// my header file
#include <v/vwindow.h>		// Win header
#include <v/vcmdwin.h>
#include <v/vctlclrs.h>
#include <v/vcolor.h>

#ifdef Motif
#include <Xm/Xm.h>
#endif
//#include <X11/StringDefs.h>

#include <stdio.h>
#include <stdlib.h>

    // Globals available to the world

    vApp* theApp = NULL;	// to be filled in upon instantiation
    DebugMask DebugState;

    // Define the default color scheme - shades of gray
    // #define UseBlueColors to get a Motif-like look for the default

#ifdef UseBlueColors

#define vLightGray "LightSteelBlue2"	// status bar background (204, 80% gray)
#define vMedGray "LightSteelBlue"	// dialog, command bar background (191,75%)
#define vDarkGray "LightSteelBlue3"	// Menu bar, button faces (179,70%)
#define vDarkShadow "LightSteelBlue4"	// dark shadows for buttons
#define vLightShadow "LightSteelBlue1"	// light shadows for buttons (222,87%)

#else		// Default V Gray scheme

#define vLightGray "#CCCCCCCCCCCC"	// status bar background (204, 80% gray)
#define vMedGray "#BFBFBFBFBFBF"	// dialog, command bar background (191,75%)
#define vDarkGray "#B3B3B3B3B3B3"	// Menu bar, button faces (179,70%)
#define vDarkShadow "#7F7F7F7F7F7F"	// dark shadows for buttons (127,50%)
#define vLightShadow "#DEDEDEDEDEDE"	// light shadows for buttons (222,87%)

#endif

    // define the structures we need to recover the XtDefaultFont info

    typedef struct
     { 
	Pixel copy_fg;
	Pixel copy_bg;

	Pixel dialogBG;
	Pixel statusBarBG;
	Pixel menuBarBG;
	Pixel controlBG;
	Pixel controlFace;
	Pixel lightControlShadow;
	Pixel darkControlShadow;

	XFontStruct *font;
     } AppData;

    static AppData app_data;

    static XtResource app_resources[] =
    {
      {
	XtNforeground,
	XtCForeground,
	XtRPixel,
	sizeof(Pixel),
	XtOffsetOf(AppData,copy_fg),
	XtRString,
	XtDefaultForeground
      },
      {
	XtNbackground,
	XtCBackground,
	XtRPixel,
	sizeof(Pixel),
	XtOffsetOf(AppData,copy_bg),
	XtRString,
	XtDefaultBackground
      },

//--------------------------------------------
      {
	"vDialogBG",
	XtCForeground,
	XtRPixel,
	sizeof(Pixel),
	XtOffsetOf(AppData,dialogBG),
	XtRString,
	vMedGray
      },
      {
	"vStatusBarBG",
	XtCForeground,
	XtRPixel,
	sizeof(Pixel),
	XtOffsetOf(AppData,statusBarBG),
	XtRString,
	vLightGray
      },
      {
	"vMenuBarBG",
	XtCForeground,
	XtRPixel,
	sizeof(Pixel),
	XtOffsetOf(AppData,menuBarBG),
	XtRString,
	vDarkGray
      },
      {
	"vControlBG",
	XtCForeground,
	XtRPixel,
	sizeof(Pixel),
	XtOffsetOf(AppData,controlBG),
	XtRString,
	vDarkGray
      },
      {
	"vControlFace",
	XtCForeground,
	XtRPixel,
	sizeof(Pixel),
	XtOffsetOf(AppData,controlFace),
	XtRString,
	vMedGray
      },
      {
	"vLightControlShadow",
	XtCForeground,
	XtRPixel,
	sizeof(Pixel),
	XtOffsetOf(AppData,lightControlShadow),
	XtRString,
	vLightShadow
      },
      {
	"vDarkControlShadow",
	XtCForeground,
	XtRPixel,
	sizeof(Pixel),
	XtOffsetOf(AppData,darkControlShadow),
	XtRString,
	vDarkShadow
      },
//--------------------------------------------

      {
	XtNfont,
	XtCFont,
	XtRFontStruct,
	sizeof(XFontStruct*),
	XtOffsetOf(AppData, font),
	XtRString,
	XtDefaultFont
      },
    };

   // Define three shades of gray used to draw dialogs and controls.
   // LightGray is for the status bar background
   // Med Gray is for the dialog and command bar background
   // DarkGray is for the menu bar and button faces


  Pixel _vStatusBarBG;
  Pixel _vDialogBG;
  Pixel _vMenuBarBG;
  Pixel _vControlBG;
  Pixel _vControlFace;
  Pixel _vLightControlShadow;
  Pixel _vDarkControlShadow;

  static char copyright[] =
    "****> Copyright (C) 1995 Bruce E. Wampler; under terms of the\
    GNU Library General Public License, version 2 <****";


//========================>>> vApp::vApp <<<=======================
  vApp::vApp(char* appName, int simSDI, int fh, int fw) : 
	vBaseItem(appName)	// constructor
  {
    // First, set the global pointer to the main App. This happens
    // when the user declares the instance of the app, either from
    // a vApp object direct, or an object from a class derived
    // from the vApp class.

    theApp = this;		// this is our object

    // now the data members

    _workTimer = 0;		// timer for work slice (BEW: 7/27/96)
    _running = 0;		// we are running
    _display = NULL;
    _appContext = NULL;
    _WindowList = NULL;		// no windows registered
    _simSDI = simSDI;		// not used in X

    _frameWidth = fw; _frameHeight = fh;
    _DefaultHeight = 150;	// default sizes for canvas window
    _DefaultWidth = 350;


    // Set which debug items to show

    DebugState.System = 1;			// System debug messages
    DebugState.CmdEvents = 0;			// Show command events (buttons, etc.)
    DebugState.MouseEvents = 0;			// Show mouse events
    DebugState.WindowEvents = 0;		// Window events (resize, etc.)
    DebugState.Build = 0;			// Define/Build window
    DebugState.BadVals= 0;			// Error values
    DebugState.Misc = 0;			// Misc stuff
    DebugState.Text = 0;			// Text events
    DebugState.Constructor = 0;			// Show constructors
    DebugState.Destructor = 0;			// Show destructors
    DebugState.User = 1;			// Debug user events
    DebugState.UserApp1 = 0;			// Level 1 User App
    DebugState.UserApp2 = 0;			// Level 2 User App
    DebugState.UserApp3 = 0;

  }

//========================>>> vApp::initialize <<<=======================
  void vApp::initialize(int& argc, char** argv)
  {
    // Main interface to the parent windowing system

    static XrmOptionDescRec Options[] = {
	{"-vDebug","*none",XrmoptionSepArg,(XPointer) NULL}
      };

    int local_argc = argc;

    for (int argn = 1 ; argn < argc ; ++argn)	// look for vDebug switch
      {
	if (strcmp(argv[argn],"-vDebug") == 0)
	  {
	    // Turn them all off
	    DebugState.System = 0;			// System debug messages
	    DebugState.CmdEvents = 0;			// Show command events (buttons, etc.)
	    DebugState.MouseEvents = 0;			// Show mouse events
	    DebugState.WindowEvents = 0;		// Window events (resize, etc.)
	    DebugState.Build = 0;			// Define/Build window
	    DebugState.BadVals= 0;			// Error values
	    DebugState.Misc = 0;			// Misc stuff
	    DebugState.Text = 0;			// Text events
	    DebugState.Constructor = 0;			// Show constructors
	    DebugState.Destructor = 0;			// Show destructors
	    DebugState.User = 0;			// Debug user events
	    DebugState.UserApp1 = 0;			// Level 1 User App
	    DebugState.UserApp2 = 0;			// Level 2 User App
	    DebugState.UserApp3 = 0;
	    argn++;
	    for (char* cp = argv[argn] ; *cp ; ++cp)
	      {
		switch (*cp)
		  {
		    case 'A':				// All system
			DebugState.System = 1;		// System debug messages
			DebugState.CmdEvents = 1;	// Show command events (buttons, etc.)
			DebugState.MouseEvents = 1;	// Show mouse events
			DebugState.WindowEvents = 1;	// Window events (resize, etc.)
			DebugState.Build = 1;		// Define/Build window
			DebugState.BadVals= 1;		// Error values
			DebugState.Misc = 1;		// (Other) Misc stuff
			DebugState.Text = 1;		// Text events
			DebugState.Constructor = 1;	// Show constructors
			DebugState.Destructor = 1;	// Show destructors
			break;
		    case 'S':
			DebugState.System = 1;		// System debug messages
			break;
		    case 'c':
			DebugState.CmdEvents = 1;	// Show command events (buttons, etc.)
			break;
		    case 'm':
			DebugState.MouseEvents = 1;	// Show mouse events
			break;
		    case 'w':
			DebugState.WindowEvents = 1;	// Window events (resize, etc.)
			break;
		    case 'b':
			DebugState.Build = 1;		// Define/Build window
			break;
		    case 'v':
			DebugState.BadVals= 1;		// Error values
			break;
		    case 'o':
			DebugState.Misc = 1;		// (Other) Misc stuff
			break;
		    case 't':
			DebugState.Text = 1;		// Text events
			break;
		    case 'C':
			DebugState.Constructor = 1;	// Show constructors
			break;
		    case 'D':
			DebugState.Destructor = 1;	// Show destructors
			break;
		    case 'U':
			DebugState.User = 1;		// Debug user events
			break;
		    case '1':
			DebugState.UserApp1 = 1;	// Level 1 User App
			break;
		    case '2':
			DebugState.UserApp2 = 1;	// Level 2 User App
			break;
		    case '3':
			DebugState.UserApp3 = 1;
			break;
		  }
	      }
	    break;		// done with list
	  }
      }

    _vHandle = XtAppInitialize(
	&_appContext,		// The Xt App Context
	_name,		// my name
	Options, XtNumber(Options),
	&local_argc, argv,	// XtAppInitialize eats it args (e.g., -font)
	NULL, NULL, 0);

    argc = local_argc;		// fix argc count

    // Now retrieve the XFontStruct of the XtDefaultFont

    XtVaGetApplicationResources
	(_vHandle, &app_data, app_resources, XtNumber(app_resources), NULL);

    _XDefaultFont = app_data.font;	// specified by -fn or default settings

    // We've now created a top level shell. We want to make it
    // invisible in the center of the screen (for possible aid in
    // placement of other windows later)

    _display = XtDisplay(_vHandle);	// Get the display
    _xwindow = 0;

    _DisplayHeight = DisplayHeight(_display,0);
    _DisplayWidth = DisplayWidth(_display,0);

    XtVaSetValues(_vHandle,		// this Widget
	XtNmappedWhenManaged, FALSE,	// invisible
	XtNx, _DisplayWidth/2,		// middle of screen
	XtNy, _DisplayHeight/2,
	XtNwidth, 1,			// and very small
	XtNheight, 1,
#ifdef Motif
	XmNkeyboardFocusPolicy,XmPOINTER,
#endif
	NULL);

    // Set default height and width to a 24x80 window

    vFont tempSys(vfDefaultSystem);
    int asc, des;
    int dh = tempSys.XFontH(asc, des);

    if (dh > 0)
	_DefaultHeight = (dh * 24) + 6;		// make default 24 lines

    int dw = tempSys.XTextW("c");
    if (dw > 0)
	_DefaultWidth = (dw * 80) + 6;		// 80 columns

    // Now realize the shell window so childern will work.

    XtRealizeWidget(_vHandle);

    // Add the delete protocol
    vx_wm_delete_window = XInternAtom(XtDisplay(_vHandle), "WM_DELETE_WINDOW",
        False);
    vx_wm_protocols = XInternAtom(XtDisplay(_vHandle), "WM_PROTOCOLS",
        False);

    XSetWMProtocols(XtDisplay(_vHandle), XtWindow(_vHandle),
                    &vx_wm_delete_window, 1);
    // Color stuff

    XtVaGetValues(_vHandle,
	XtNdepth, &_depth,
	NULL);

    _colormap = DefaultColormapOfScreen(
	XScreenOfDisplay(_display, DefaultScreen(_display)));           

    if (_depth <= 1)
      {
	_vStatusBarBG =
	_vDialogBG =
	_vMenuBarBG =
	_vControlBG =
	_vControlFace =
	_vLightControlShadow =
	_backgroundPix = XWhitePixel(theApp->display(),0);
	_vDarkControlShadow =
	_foregroundPix = XBlackPixel(theApp->display(),0);
      }
    else
      {
	_backgroundPix = app_data.copy_bg;
	_foregroundPix = app_data.copy_fg;

	// Get the control colors
	_vDialogBG = app_data.dialogBG;
	_vStatusBarBG = app_data.statusBarBG;
	_vMenuBarBG = app_data.menuBarBG;
	_vControlBG = app_data.controlBG;
	_vControlFace = app_data.controlFace;
	_vLightControlShadow = app_data.lightControlShadow;
	_vDarkControlShadow = app_data.darkControlShadow;
      }

    for (int icx = 0 ; icx < 16 ; ++icx)
      {
	(void) vStdColors[icx].pixel();	// Force all colors to map
      }

    _running = 1;

  }

//========================>>> vApp::xWindow <<<=======================
  Window vApp::xWindow()
  {
    return (_xwindow == 0 ? (_xwindow = XtWindow(_vHandle)) : _xwindow);
  }

//========================>>> vApp::NewAppWin <<<=======================
  vWindow* vApp::NewAppWin(vWindow* win, char* name, int w, int h, 
	vAppWinInfo* winInfo)
  {
    vWindow* thisWin = win;
    vAppWinInfo* awinfo = winInfo;

    SysDebug1(Build,"vApp::NewAppWin(%s)\n",name);

    if (!thisWin)		// Not created
	thisWin = new vCmdWindow(name, w, h);

    if (!winInfo)
	awinfo = new vAppWinInfo(name);

    registerWindow(thisWin, awinfo);	// register this window
    return thisWin;
  }

//========================>>> vApp::NewHelpWin <<<=======================
  vWindow* vApp::NewHelpWin(vWindow* win, char* name, int h, int w) 
  {
    vWindow* thisWin = win;

    SysDebug1(Build,"vApp::NewHelpWin(%s)\n",name);

    if (!thisWin)		// Not created
	return 0;
    registerWindow(thisWin, 0);	// register this window
    return thisWin;
  }

//=======================>>> vApp::CheckEvents <<<========================
  void vApp::CheckEvents()
  {
    // Periodically call this from a compute bound app to allow
    // events to get through.
    XEvent an_event;

    // Clean up event queue of events we care about.
    while (XtAppPending(theApp->appContext()) & XtIMXEvent)
      {
	XtAppNextEvent(theApp->appContext(),&an_event); // get events
	XtDispatchEvent(&an_event);             // and dispatch them 
      }
  }

//======================>>> vApp::Exit <<<=======================
  void vApp::Exit(void)
  {
    // Close All registered windows and exit

    WindList *curWin;
    vWindow *tmp;
    
    SysDebug(Build,"vApp::Exit()\n");

    for (curWin = _WindowList ; curWin !=0 ; curWin = _WindowList)
      {
	tmp = curWin->window;
        if (curWin->info == 0)		// return assocated info ptr
	    CloseHelpWin(tmp);		// different for Help
	else
	    CloseAppWin(tmp);		// use local or derived close app
      }

    exit(0);
  }

//========================>>> vApp::IsHelpWin <<<=======================
  int vApp::IsHelpWin(vWindow *Win)
  {
    WindList *curWin;

    for (curWin = _WindowList ; curWin !=0 ; curWin = curWin->nextWinList)
      {
	if (curWin->window == Win)
	  {
	    return (curWin->info == 0);	// Help if 0
	  }
      }
    return 0;
  }

//========================>>> vApp::CloseAppWin <<<=======================
  void vApp::CloseAppWin(vWindow* win)
  {

    SysDebug(Build,"vApp::CloseAppWin()\n");

    win->CloseWin();			// let the window close itself
    unregisterWindow(win);		// take it off the list
    delete win;				// free the window
  }

//========================>>> vApp::CloseHelpWin <<<=======================
  void vApp::CloseHelpWin(vWindow* win)
  {

    SysDebug(Build,"vApp::CloseAppWin()\n");

    win->CloseWin();			// let the window close itself
    unregisterWindow(win);		// take it off the list
    delete win;				// free the window
  }

//===========================>>> vApp::AppCommand <<<=========================
  void vApp::AppCommand(vWindow* win, ItemVal id, ItemVal retval, CmdType ctype)
  {
    SysDebug1(CmdEvents,"vApp::AppCmd(id: %d)\n",id);
  }

//========================>>> vApp::GetDefaultFont <<<=======================
  vFont vApp::GetDefaultFont()
  {
    vFont defFont(vfDefaultSystem);
    defFont.LoadFont();			// make sure loaded
    return defFont;
  }

//========================>>> vApp::getAppWinInfo <<<=======================
  vAppWinInfo* vApp::getAppWinInfo(vWindow *Win)
  {
    WindList *curWin;

    for (curWin = _WindowList ; curWin !=0 ; curWin = curWin->nextWinList)
      {
	if (curWin->window == Win)
	  {
	    return curWin->info;	// return assocated info ptr
	  }
      }
    return 0;
  }

//===========================>>> vApp::KeyIn  <<<===========================
  void vApp::KeyIn(vWindow* win, vKey key, unsigned int shift)
  {
    SysDebug(Misc,"vApp::KeyIn\n");
  }

//========================>>> vApp::registerWindow <<<=======================
  void vApp::registerWindow(vWindow *Win, vAppWinInfo *awinfo)
  {
    WindList* newList = new WindList;	// new cell to add to list

    SysDebug1(Misc,"vApp::registerWindow - %s\n",Win->name())

    newList->window = Win;			// remember the window
    newList->info = awinfo;			// and its info class

    newList->nextWinList = _WindowList;		// link in at front
    _WindowList = newList;
  }

//========================>>> vApp::unregisterWindow <<<=======================
  void vApp::unregisterWindow(vWindow *Win)
  {
    WindList *curWin, *tmp, *last, *next;

    last = 0;

    for (curWin = _WindowList ; curWin !=0 ; curWin = next)
      {
	next = curWin->nextWinList;
	if (curWin->window == Win)
	  {

	    SysDebug1(Misc,"vApp::unregisterWindow - %s\n",Win->name())

	    tmp = curWin;

	    if (curWin == _WindowList)
		_WindowList = curWin->nextWinList;
	    else
		last->nextWinList = curWin->nextWinList;

	    delete curWin->info;	// free the info space
	    delete tmp;			// free the list space
	  }
	last = curWin;
      }
  }

//========================>>> vApp::SendWindowCommandAll <<<=======================
  void vApp::SendWindowCommandAll(ItemVal id, int val, CmdType ctype)
  {
    // send a command to all windows
    for (WindList* curWin = _WindowList ; curWin !=0 ; curWin = curWin->nextWinList)
      {
	(curWin->window)->WindowCommand(id, val, ctype);
      }
  }

//========================>>> vApp::SetValueAll <<<=======================
  void vApp::SetValueAll(ItemVal id, int val, ItemSetType setType)
  {
    for (WindList* curWin = _WindowList ; curWin !=0 ; curWin = curWin->nextWinList)
      {
	(curWin->window)->SetValue(id, val, setType);
      }
  }

//======================>>> vApp::SetAppTitle <<<==========================
  void vApp::SetAppTitle(char* title)
  {
    // set the title in the title bar -- this is a no-op on some platforms
  } 

//========================>>> vApp::SetStringAll <<<=======================
  void vApp::SetStringAll(ItemVal id, char* str)
  {
    for (WindList* curWin = _WindowList ; curWin !=0 ; curWin = curWin->nextWinList)
      {
	(curWin->window)->SetString(id, str);
      }
  }

//========================>>> vApp::ShowList <<<=======================
  int vApp::ShowList(void)
  {
    // This is a utility routine to show current information

    fprintf(stderr,"Registered windows:\n");
    for (WindList* curWin = _WindowList ; curWin !=0 ; curWin = curWin->nextWinList)
      {
	fprintf(stderr,"    %s\n",(curWin->window)->name());
      }
    return 1;
  }

//====================>>> _appWorkTimer::TimerTick <<<====================
  void _appWorkTimer::TimerTick()
  {

   theApp->DispatchWork();
  }

//========================>>> vApp::EnableWorkSlice <<<====================
  int vApp::EnableWorkSlice(long slice)
  {

    if (slice > 0)
      {
	if (_workTimer == 0)		// First time to start timer
	  {
	    _workTimer = new _appWorkTimer;
	  }
	return _workTimer->TimerSet(slice);
      }
    else
      {
	if (_workTimer)
	    _workTimer->TimerStop();
      }
    return 1;
  }

//========================>>> vApp::DispatchWork <<<=======================
  void vApp::DispatchWork(void)
  {

    WorkSlice();		// Work Slice for App
    // Call WorkSlice for all windows
    for (WindList* curWin = _WindowList ; curWin !=0 ;
	     curWin = curWin->nextWinList)
      {
	(curWin->window)->WorkSlice();
      }
  }

//========================>>> vApp::GetMenuThis <<<=======================
  vMenuPane* vApp::GetMenuThis(Widget w)
  {
    // This is a utility routine to get the this of a menu widget

    vMenuPane* mypane;

    for (WindList* curWin = _WindowList ; curWin !=0 ; curWin = curWin->nextWinList)
      {
	 if ((mypane = (curWin->window)->GetMenuThis(w)) != 0)
	    return mypane;
      }
    return 0;
  }

//========================>>> vApp::doXEventLoop <<<=======================
  void vApp::doXEventLoop(void)
  {
    // This is where we grab and handle events from the
    // parent windowing system

    XtAppMainLoop(_appContext);		// Xt's main loop handler
  }

//#########################################################################

//======================>>> main <<<======================================
  int main(int argc, char** argv)
  {
    int Xargc = argc;
    int retcode;

    theApp->initialize(Xargc,argv);	// Create top level widget

    if ((retcode = AppMain(Xargc,argv)) != 0)	// call the app main program
	exit(retcode);

    theApp->doXEventLoop();		// And enter the event loop
  }

//#########################################################################
// Utilities
//=========================>>> vSysWarning <<<============================
  void vSysWarning(char* msg)
  {
    XtAppWarning(theApp->appContext(), msg);
  }

//=========================>>> vSys <<<============================
  void vSysError(char* msg)
  {
    XtAppWarning(theApp->appContext(), msg);
    exit(99);
  }
