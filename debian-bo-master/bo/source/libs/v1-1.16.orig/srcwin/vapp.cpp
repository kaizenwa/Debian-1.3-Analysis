//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
//                                                     VV         VV       //
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
// vapp.cxx - The vApp control object (For Windows)                        //
//                                                                         //
// Copyright (C) 1995, 1996  Bruce E. Wampler                              //
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
#include <v/vwin32.h>		// for Win 32 stuff
#include <v/vapp.h>		// my header file
#include <v/vwindow.h>		// Win header
#include <v/vfont.h>		// for font stuff
#include <v/vcmdwin.h>
#include <v/vcmdpane.h>
#include <v/vthislst.h>

#include <stdlib.h>

    // Globals available to the world

    vApp* theApp = NULL;	// to be filled in upon instantiation
    DebugMask DebugState;

    int CMain(HANDLE hInstance, HANDLE hPrevInstance,
		      LPSTR lpszCmdLine, int nCmdShow);

    static char szMdiFrameClass [] = "MdiFrameClass";	// To give it a name
    static char szMdiWindowClass [] = "MdiWindowClass";	// To give it a name
    static _destroyed;		// for destructor


    static char copyright[] =
      "****> Copyright (C) 1995, 1996 Bruce E. Wampler; under terms of the\
 GNU Library General Public License, version 2 <****";

//========================>>> vApp::vApp <<<=======================
  vApp::vApp(char* appName, int simSDI, int frameWidth, int frameHeight) :
	vBaseItem(appName)	// constructor
  {
    // First, set the global pointer to the main App. This happens
    // when the user declares the instance of the app, either from
    // a vApp object direct, or an object from a class derived
    // from the vApp class.

    theApp = this;		// this is our object

    // now the data members

    _curThis = 0;
    _running = 0;		// we are running
    _WindowList = NULL;		// no windows registered
    _CmdPaneList = NULL;	// no command panes registered
    _inExit = 0;
    _simSDI = simSDI;

    _frameWidth = (frameWidth > 0) ? frameWidth + 20 : 0;
    _frameHeight = (frameHeight > 0) ? frameHeight + 20 : 0;

    _DefaultHeight = frameHeight;	// default sizes for canvas window
    _DefaultWidth = frameWidth;
    _destroyed = 0;		// Not destroyed yet

    _Frame = 0;	                // Not inited yet.

    // Set which debug items to show

    DebugState.System = 0;			// System debug messages
    DebugState.CmdEvents = 0;			// Show command events (buttons, etc.)
    DebugState.MouseEvents = 0;		  	// Show mouse events
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

  }

//========================>>> vApp::initialize <<<=======================
  void vApp::initialize(int& argc, char** argv,
	HANDLE hInstance, HANDLE hPrevInstance, int nCmdShow)
  {
    // Main interface to the parent windowing system

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

	    for (char* cp = argv[argn+1] ; *cp ; ++cp)
	      {
		switch (*cp)
		  {
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
	    
	    // Now fixup the argument list and break loop
	    int ia;
	    for (ia = argn ; ia < argc ; ++ia)
		argv[ia] = argv[ia+2];
	    argv[ia] = 0;
	    argc -= 2;	// eat the -vDebug args
	    break;
	  }

      }

    // This is pretty much the standard Windows startup code
    // required by all Windows applications.

    WNDCLASS wndclass;

    _vHandle = hInstance;

    if (!hPrevInstance)
      {
	// Register the MDI frame window class

	_appicon = ::LoadIcon((HINSTANCE)hInstance,"vAppIcon");
	if (_appicon == NULL)
	    _appicon = ::LoadIcon(NULL,IDI_APPLICATION);
	_winicon = ::LoadIcon((HINSTANCE)hInstance,"vWindowIcon");
	if (_winicon == NULL)
	    _winicon = ::LoadIcon(NULL,IDI_APPLICATION);

	wndclass.style         = CS_HREDRAW | CS_VREDRAW;
	wndclass.lpfnWndProc   = (WNDPROC) PMdiFrameProc;
	wndclass.cbClsExtra    = 0;
	wndclass.cbWndExtra    = 0;
	wndclass.hInstance     = (HINSTANCE) hInstance;
	wndclass.hIcon         = _appicon;
	wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW);
	wndclass.hbrBackground = (HBRUSH)(COLOR_APPWORKSPACE + 1);
	wndclass.lpszMenuName  = NULL;
	wndclass.lpszClassName = szMdiFrameClass;

	RegisterClass(&wndclass);

	// Register the MDI window class

	wndclass.style         = CS_HREDRAW | CS_VREDRAW ;
	wndclass.lpfnWndProc   = (WNDPROC) PMdiWindowProc;
	wndclass.cbClsExtra    = 0;
	wndclass.cbWndExtra    = sizeof(void*);
	wndclass.hInstance     = (HINSTANCE) hInstance;
	wndclass.hIcon         = _winicon;
	wndclass.hCursor       = NULL; //LoadCursor (NULL, IDC_ARROW);
	wndclass.hbrBackground = (HBRUSH)::GetStockObject(WHITE_BRUSH);
	wndclass.lpszMenuName  = NULL;
	wndclass.lpszClassName = szMdiWindowClass;

	RegisterClass(&wndclass);

      }

    hFrameMenu = ::CreateMenu();		// A File menu
    hFrameSubMenu = ::CreateMenu();	// Its submenu

    // Define a very simple default MDI menu

   // ::AppendMenu(hFrameSubMenu, MF_STRING, M_WindowsReserved1, "E&xit");

  //  ::AppendMenu(hFrameMenu, MF_POPUP, (UINT)hFrameSubMenu, "&File");

    hAccel = 0;				// No accelerators
    _workTimer = 0;

    // Create the frame window

    _Frame = ::CreateWindow(szMdiFrameClass, _name,
	WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN | WS_MAXIMIZE |
	WS_VISIBLE | WS_MAXIMIZEBOX | WS_MINIMIZEBOX,
	CW_USEDEFAULT, CW_USEDEFAULT,
       //	_DefaultWidth,_DefaultHeight,
	(_frameWidth == 0) ? CW_USEDEFAULT : _frameWidth,
	(_frameHeight == 0) ? CW_USEDEFAULT : _frameHeight,
	NULL, hFrameMenu, (HINSTANCE)hInstance, NULL);


    RECT frameRect;
    ::GetWindowRect(_Frame, &frameRect);
    //::GetClientRect(_Frame, &frameRect);
    if (_DefaultHeight <= 0)
      {
        _DefaultHeight = frameRect.bottom - frameRect.top - 200;
        if (_DefaultHeight < 50)
            _DefaultHeight = 50;
      }
    if (_DefaultWidth <= 0)
      {
        _DefaultWidth = frameRect.right - frameRect.left - 50;
        if (_DefaultWidth < 50)
            _DefaultWidth = 50;
      }

    _Client = ::GetWindow(_Frame, GW_CHILD);

    // Create some pens and brushes used for drawing custom controls
    // and dialogs.

    _WhitePen = ::CreatePen(PS_SOLID,1, RGB(255,255,255));
    _GrayPen = ::CreatePen(PS_SOLID,1, RGB(128,128,128));
    _ShadowPen = ::CreatePen(PS_INSIDEFRAME, 1,::GetSysColor(COLOR_BTNSHADOW));
    if (::GetSysColor(COLOR_BTNHIGHLIGHT) == ::GetSysColor(COLOR_BTNFACE))
	_LightPen = ::CreatePen(PS_INSIDEFRAME, 1,RGB(255,255,255));
    else
	_LightPen = ::CreatePen(PS_INSIDEFRAME, 1,::GetSysColor(COLOR_BTNHIGHLIGHT));
    _BarBrush = ::CreateSolidBrush(::GetSysColor(COLOR_BTNFACE));

    ::ShowWindow(_Frame, nCmdShow);

    ::UpdateWindow(_Frame);

    _running = 1;
  }

//======================>>> vApp::~vApp <<<=======================
  vApp::~vApp()
  {
    // WARNING! This destructor never gets called automatically,
    // at least with Borland C++ 4.5.
    SysDebug(Destructor,"vApp::~vApp destructor\n")

   _destroyed = 1;

  }

//======================>>> vApp::Exit <<<=======================
  void vApp::Exit(void)
  {
    // Close All registered windows and exit

    WindList *curWin;
    vWindow *tmp;
    
    SysDebug(Build,"vApp::Exit()\n")

    if (_workTimer)		// Stop events first
      {
        _workTimer->TimerStop();
        delete _workTimer;
        _workTimer = 0;
      }

    _inExit = 1;			// Kludge - CloseAppWin needs this

    for (curWin = _WindowList ; curWin !=0 ; curWin = _WindowList)
      {
	tmp = curWin->window;
	if (IsHelpWin(tmp))
	    CloseHelpWin(tmp);
	else
	    CloseAppWin(tmp);		// use local or derived close app
      }

    _inExit = 0;			// done now, so can exit
    AppExit(0);
  }

//======================>>> vApp::AppExit <<<=======================
  void vApp::AppExit(int exitVal)
  {

    if (!_inExit)		// Only ONE, please!
      {
	::PostQuitMessage(exitVal);
      }
  }

//========================>>> vApp::CheckEvents <<<=======================
  void vApp::CheckEvents()
  {
    extern int vChkWinDlgMsgs(MSG*);
    extern int vChkCmdPaneMsgs(MSG*);
    MSG msg;

    // Enter the modified message loop

    while (::PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
      {

	if (!vChkWinDlgMsgs(&msg) &&		// Dialogs and cmd panes handle
	  !vChkCmdPaneMsgs(&msg) &&             // their own messages
	  !::TranslateMDISysAccel(_Client, &msg) &&
	  !::TranslateAccelerator(_Frame, (HACCEL)hAccel, &msg)) // probably don't need?
	  {
	    ::TranslateMessage(&msg);
	    ::DispatchMessage(&msg);
	  }
      }
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

    SysDebug(Build,"vApp::CloseHelpWin()\n");

    win->CloseWin();			// let the window close itself
    unregisterWindow(win);		// take it off the list
    delete win;				// free the window
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

//===========================>>> vApp::AppCommand <<<=========================
  void vApp::AppCommand(vWindow* win, ItemVal id, ItemVal retval, CmdType ctype)
  {
    // Do nothing by default.

    SysDebug1(CmdEvents,"vApp::AppCmd(id: %d)\n",id);
  }

//========================>>> vApp::GetDefaultFont <<<=======================
  vFont vApp::GetDefaultFont()
  {
    vFont sysF(vfDefaultSystem);	// construct a system font instance
    return sysF; 		      	// return it
  }

//========================>>> vApp::getAppWinInfo <<<=======================
  vAppWinInfo* vApp::getAppWinInfo(vWindow *Win)
  {
    // Search list to find associated vAppWinInfo.

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
    // Do nothing by default.
    SysDebug(Misc,"vApp::KeyIn\n");
  }

//========================>>> vApp::NewAppWin <<<=======================
  vWindow* vApp::NewAppWin(vWindow* win, char* name, int w, int h,
	vAppWinInfo* winInfo)
  {
    // The derived vApp needs to call this.

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
  vWindow* vApp::NewHelpWin(vWindow* win, char* name, int w, int h)
  {
    vWindow* thisWin = win;

    SysDebug1(Build,"vApp::NewHelpWin(%s)\n",name);

    if (!thisWin)		// Not created
	return 0;
    registerWindow(thisWin, 0);	// register this window
    return thisWin;
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
    // Scan window list to unregister this window and free some space 

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

//========================>>> vApp::registerCmdPane <<<======================
  void vApp::registerCmdPane(vCommandPane* cmdPane)
  {
    CmdPaneList* newList = new CmdPaneList;	// new cell to add to list

    SysDebug(Misc,"vApp::registerCmdPane\n")

    newList->commandPane = cmdPane;		// remember the cmd pane

    newList->nextCPList = _CmdPaneList;		// link in at front
    _CmdPaneList = newList;
  }

//========================>>> vApp::unregisterCmdPane <<<=======================
  void vApp::unregisterCmdPane(vCommandPane* cmdPane)
  {
    // Scan pane list to unregister this window and free some space
    CmdPaneList *curCP, *tmp, *last, *next;

    last = 0;

    for (curCP = _CmdPaneList ; curCP !=0 ; curCP = next)
      {
	next = curCP->nextCPList;
	if (curCP->commandPane == cmdPane)
	  {

	    SysDebug(Misc,"vApp::unregisterCmdPane\n")

	    tmp = curCP;

	    if (curCP == _CmdPaneList)
		_CmdPaneList = curCP->nextCPList;
	    else
		last->nextCPList = curCP->nextCPList;

	    delete tmp;			// free the list space
	  }
	last = curCP;
      }
  }

//========================>>> vApp::selectCmdPanes <<<=======================
  void vApp::selectCmdPanes(vWindow* parent)
  {
    // This is needed by the MDI interface conventions.

    // This will turn off _isShown for panes that are children of other windows
    // and turn onn all cmd panes that are in our window
    CmdPaneList *curCP;

    // First, turn off all command panes in other windows

    for (curCP = _CmdPaneList ; curCP !=0 ; curCP = curCP->nextCPList)
      {
	if ((curCP->commandPane)->_parentWin != parent)
	  {
	    (curCP->commandPane)->_isShown = 0;
	    if (::IsWindow((curCP->commandPane)->_wDialog) &&
	       ::IsWindowVisible((curCP->commandPane)->_wDialog))
		   ::ShowWindow((curCP->commandPane)->_wDialog, SW_HIDE);
	  }
      }

    // Now, turn ours on
    for (curCP = _CmdPaneList ; curCP !=0 ; curCP = curCP->nextCPList)
      {
	if ((curCP->commandPane)->_parentWin == parent)
	  {
	    (curCP->commandPane)->_isShown = 1;
	    if (::IsWindow((curCP->commandPane)->_wDialog) &&
	       !::IsWindowVisible((curCP->commandPane)->_wDialog))
		   ::ShowWindow((curCP->commandPane)->_wDialog, SW_SHOW);
	  }
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
    // Set a Value in all windows
    for (WindList* curWin = _WindowList ; curWin !=0 ; curWin = curWin->nextWinList)
      {
	(curWin->window)->SetValue(id, val, setType);
      }
  }

//======================>>> vApp::SetAppTitle <<<==========================
  void vApp::SetAppTitle(char* title)
  {
    // set the title in the title bar -- this is a no-op on some platforms

    ::SetWindowText(_Frame, title);
  
  }

//========================>>> vApp::SetValueAll <<<=======================
  void vApp::SetStringAll(ItemVal id, char* str)
  {
    // Set a string in all windows
    for (WindList* curWin = _WindowList ; curWin !=0 ; curWin = curWin->nextWinList)
      {
	(curWin->window)->SetString(id, str);
      }
  }

//========================>>> vApp::ShowList <<<=======================
  int vApp::ShowList(void)
  {
    // This is a utility routine to show current information
#ifdef HAS_PRINTF
    fprintf(stderr,"Registered windows:\n");
    for (WindList* curWin = _WindowList ; curWin !=0 ; curWin = curWin->nextWinList)
      {
	fprintf(stderr,"    %s\n",(curWin->window)->name());
      }
#endif
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
          {
	    _workTimer->TimerStop();
            delete _workTimer;
            _workTimer = 0;
          }
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

//========================>>> vApp::doEventLoop <<<=======================
  int vApp::doEventLoop(void)
  {
    // This is where we grab and handle events from the
    // parent windowing system
    extern int vChkWinDlgMsgs(MSG*);
    extern int vChkCmdPaneMsgs(MSG*);
    MSG msg;

    // Enter the modified message loop

    while (::GetMessage(&msg, NULL, 0, 0))
      {

	if (!vChkWinDlgMsgs(&msg) &&		// Dialogs and cmd panes handle
	  !vChkCmdPaneMsgs(&msg) &&             // their own messages
	  !::TranslateMDISysAccel(_Client, &msg) &&
	  !::TranslateAccelerator(_Frame, (HACCEL)hAccel, &msg)) // probably don't need?
	  {
	    ::TranslateMessage(&msg);
	    ::DispatchMessage(&msg);
	  }
      }

    // The "Honor System" that Windows requires of returning every tiny
    // resource you use really sucks. It took hours to figure out this
    // way of freeing resources. Turns out that the destructor for the static
    // vApp object never gets called once PostQuitMessage is called. We do come
    // through here, however, so here is where we can return these
    // resources.  A reasonable operating system recovers system
    // resources used by a process when it terminates. Not Windows...

    if (_workTimer)
      {
	_workTimer->TimerStop();
	delete _workTimer;
      }

    ::DestroyIcon(_appicon);
    ::DestroyIcon(_winicon);

    ::DeleteObject(_WhitePen);
    ::DeleteObject(_GrayPen);
    ::DeleteObject(_LightPen);
    ::DeleteObject(_ShadowPen);
    ::DeleteObject(_BarBrush);

    ::DestroyMenu(hFrameSubMenu);
    ::DestroyMenu(hFrameMenu);

    ::DestroyWindow(_Client);
    ::DestroyWindow(_Frame);

    return msg.wParam;
  }

//======================>>> PMdiFrameProc <<<================================
  long FAR PASCAL _export PMdiFrameProc(HWND hwnd, UINT message,
				UINT wParam, LPARAM lParam)
  {
    return theApp->MdiFrameProc(hwnd, message, wParam, lParam);
  }

//======================>>> vApp::OnCreate <<<============================
  int vApp::OnCreate(HWND hwnd, CREATESTRUCT FAR* lpCreateStruct)
  {

    CLIENTCREATESTRUCT clientcreate;

    clientcreate.hWindowMenu  = hFrameSubMenu;
    clientcreate.idFirstChild = M_FIRSTCHILD;

    if (_simSDI)
      {
        _Client = ::CreateWindow("MDICLIENT", NULL,
	  WS_CHILD | WS_CLIPCHILDREN | WS_VISIBLE | WS_CLIPSIBLINGS |
	  WS_HSCROLL | WS_VSCROLL | MDIS_ALLCHILDSTYLES,
	  0, 0, 0, 0, hwnd, NULL, (HINSTANCE)_vHandle,
	  (LPSTR) &clientcreate);
      }
    else
      {
	_Client = ::CreateWindow("MDICLIENT", NULL,
	  WS_CHILD | WS_CLIPCHILDREN | WS_VISIBLE | WS_CLIPSIBLINGS |
	  WS_HSCROLL | WS_VSCROLL,
	  0, 0, 0, 0, hwnd, NULL, (HINSTANCE)_vHandle,
	  (LPSTR) &clientcreate);
      }

    return _Client != NULL;
  }

// Following are routines to handle Windows messages

//======================>>> vApp::OnClose <<<============================
  void vApp::OnClose(HWND hwnd)
  {
    Exit();
  }

//======================>>> vApp::OnQueryEndSession <<<============================
  int vApp::OnQueryEndSession(HWND hwnd)
  {
    return 1;
  }

//======================>>> vApp::OnDestroy <<<============================
  void vApp::OnDestroy(HWND hwnd)
  {

  }

//======================>>> vApp::OnSysCommand <<<========================
  void vApp::OnSysCommand(HWND hwnd, UINT cmd, int x, int y)
  {
    // Set focus to frame window.  This causes any comboboxes
    // in dialogs to be closed.
    ::SetFocus(_Frame);
    FORWARD_WM_SYSCOMMAND(hwnd, cmd, x, y, Frame_DefProc);
  }

//======================>>> vApp::OnSize <<<========================
  void vApp::OnSize(HWND hwnd, UINT state, int cx, int cy)
  {
   // Force MDI Child window to be resized.
   FORWARD_vWM_ResizeMDIClient(hwnd, SendMessage);
  }

//======================>>> vApp::OnCommand <<<========================
  void vApp::OnCommand(HWND hwnd, int id, HWND hwndCtl, UINT codeNotify)
  {
    switch (id)
      {

	case M_WindowsReserved1:		// Exit the program
	    FORWARD_WM_CLOSE(hwnd, SendMessage);
	    return;

	// Messages for arranging child MDI windows
	case M_Tile:
	    (void)FORWARD_WM_MDITILE(_Client, WM_MDITILE, SendMessage);
	    return;

	case M_Cascade:
	    (void)FORWARD_WM_MDICASCADE(_Client,0,SendMessage);
	    return;

	case M_Arrange:
	    (void)FORWARD_WM_MDIICONARRANGE(_Client, SendMessage);
	    return;

	default:            // Pass to active child

	    HWND hwndChild = (HWND)::SendMessage(_Client,
		WM_MDIGETACTIVE, 0, 0L);

	    if (::IsWindow(hwndChild) && id < M_FIRSTCHILD)
	      {
	        FORWARD_WM_COMMAND(hwndChild, id, hwndCtl, codeNotify,
		    SendMessage);
		//return;
	      }
      }
    FORWARD_WM_COMMAND(hwnd, id, hwndCtl, codeNotify, Frame_DefProc);
  }

//======================>>> vApp::Frame_DefProc <<<========================
  LRESULT vApp::Frame_DefProc (HWND hwnd, UINT uMsg,
	WPARAM wParam, LPARAM lParam)
  {
    return ::DefFrameProc(hwnd, _Client, uMsg, wParam, lParam);
  }

//======================>>> vApp::OnResizeMDIClient <<<============================
  void vApp::OnResizeMDIClient(HWND hwnd)
  {
    // This is called whenever the Frame is resized, or when the state of
    // a status bar or command pane changes.
    RECT rc;

    vCommandPane* cp;
    CmdPaneList *curCPL;

    if (!::IsWindow(_Client))
	return;

   if (!_Frame)
      return;

    ::GetClientRect(_Frame, &rc);		 // the rect of the Frame

    int topY = 0;			// Need to track space used by
    int botY = rc.bottom;               // cmd bars and status bars

    // Scan the cmdpane list to reposition all cmd bars and status bars

    for (curCPL = _CmdPaneList ; curCPL != 0 ; curCPL = curCPL->nextCPList)
      {
	cp = curCPL->commandPane;
	if (cp->_isShown && cp->_isDisplayed &&
		   ::IsWindow(cp->HwndDialog()) && ::IsWindowVisible(cp->HwndDialog()))
	  {
	    if (cp->paneType() != P_Status)	// Cmd bar
	      {
		::MoveWindow(cp->HwndDialog(), 0,topY,rc.right,
		   cp->_windowH + topY,1);
		topY += cp->_windowH;
		cp->_windowY = topY;
	      }                                	// Status bar
	    else
	      {
		botY -= cp->_windowH;
		::MoveWindow(cp->HwndDialog(), 0, botY,rc.right,
		   cp->_windowH + botY,1);
		cp->_windowY = botY;
	      }
	  }
      }

    ::MoveWindow(_Client, 0, topY, rc.right, botY - topY , 1);
  }

//======================>>> vApp::MdiFrameProc <<<============================
  long vApp::MdiFrameProc(HWND hwnd, UINT uMsg, UINT wParam, LPARAM lParam)
  {
    switch (uMsg)
     {
      HANDLE_MSG(hwnd, WM_CREATE,           OnCreate);
      HANDLE_MSG(hwnd, WM_CLOSE,            OnClose);
      HANDLE_MSG(hwnd, WM_QUERYENDSESSION,  OnQueryEndSession);
      HANDLE_MSG(hwnd, WM_DESTROY,          OnDestroy);
      HANDLE_MSG(hwnd, WM_SYSCOMMAND,       OnSysCommand);
      HANDLE_MSG(hwnd, WM_SIZE,             OnSize);
      HANDLE_MSG(hwnd, WM_COMMAND,          OnCommand);
      HANDLE_MSG(hwnd, vWM_ResizeMDIClient, OnResizeMDIClient);
    }
   return(::DefFrameProc(hwnd, _Client, uMsg, wParam, lParam));
  }

//======================>>> PMdiWindowProc <<<================================
  long FAR PASCAL _export PMdiWindowProc(HWND hwnd, UINT message,
				WPARAM wParam, LPARAM lParam)
  {
    // Kludgy stuff to get things rolling. Part of hhis code is needed to
    // get the first mouse click to activate the window, but not
    // send a mousedown message.  Windows can be UGLY...

    static vWindow* createThis = 0;
    vWindow* thisWin = (vWindow*)::GetWindowLong(hwnd, 0);

    if (!thisWin && message == WM_CREATE)
      {
	// We need to intercept the message here to recover the vWindow
	// this of the window. It is sent in the lParam of the CREATESTRUCT.
	CREATESTRUCT* cs = (CREATESTRUCT*)lParam;	// fetch the ptr
	// now convert to MDICREATESTRUCT
	MDICREATESTRUCT* mdics = (MDICREATESTRUCT*)cs->lpCreateParams;

	createThis = thisWin = (vWindow*)mdics->lParam;	// remember the this!
      }
    else if (createThis != 0 && message == WM_MDIACTIVATE)
      {
	thisWin = createThis;
	createThis = 0;
      }

    if (!thisWin)
      {

	return ::DefMDIChildProc(hwnd, message, wParam, lParam);
      }
    else
	return thisWin->MdiWindowProc(hwnd, message, wParam, lParam);
   }


//#########################################################################

//======================>>> WinMain <<<======================================
#ifdef _WIN32
  int PASCAL WinMain(HANDLE hInstance, HANDLE hPrevInstance,
		      LPSTR lpszCmdLine, int nCmdShow)
#else
  int PASCAL WinMain(HANDLE hInstance, HANDLE hPrevInstance,
		      LPSTR lpszCmdLine, int nCmdShow)
#endif
  {
   return CMain(hInstance, hPrevInstance, lpszCmdLine, nCmdShow);
  }


//======================>>> CMain <<<======================================
  int CMain(HANDLE hInstance, HANDLE hPrevInstance,
		      LPSTR lpszCmdLine, int nCmdShow)
  {
    // CMain is a C++ style wrapper -- makes the "friend" declaration nicer
    int argc = 0;
    char** argv = new char*[50];	// up to 50 args
    char name[200];

    int retcode;

    // Split command line into tokens, as in usual main(argc, argv)

    int cmdLen = strlen((char *)lpszCmdLine);
    char *buf = new char[cmdLen + 1];

    strcpy(buf, (char*) lpszCmdLine);		// make copy

    // Get application name

    ::GetModuleFileName((HINSTANCE)hInstance, name, 199);

    argv[argc++] = name;

    // Now split argument string

    {
    char *token;
    const char *IFS = " \t\r\n";
    if ((token = strtok(buf, IFS)) != NULL)
      {
	do
	  {
	    if (*token != '\0' && strchr(IFS, *token) == NULL)
	      argv[argc++] = token;
	  } while ((token = strtok(NULL, IFS)) != NULL);
      }
    }

    argv[argc] = NULL;  // argv[] is NULL terminated list!

    theApp->initialize(argc,argv,
	hInstance, hPrevInstance, nCmdShow);	// Create top level widget

    if ((retcode = AppMain(argc,argv)) != 0)	// call the app main program
	return (retcode);

    return theApp->doEventLoop();		// And enter the event loop
  }

//#########################################################################
// Utilities
//=========================>>> vSysWarning <<<============================
  void vSysWarning(char* msg)
  {
    ::MessageBox(0, msg, "V", MB_ICONEXCLAMATION | MB_OK);
  }

//=========================>>> vSys <<<============================
  void vSysError(char* msg)
  {
    ::MessageBox(0, msg,"V", MB_ICONEXCLAMATION | MB_OK);
    theApp->AppExit(99);
  }

//#########################################################################
