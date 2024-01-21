//===============================================================
// vapp.h - the vapp base object - Windows version
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VAPP_H
#define VAPP_H

#include <v/vbaseitm.h>	// our parent class
#include <v/vfont.h>	// our parent class
#include <v/vawinfo.h>
#include <v/vmenu.h>
#include <v/vtimer.h>

#define MaxAppFonts FontIdsUsed

#ifdef _WIN32
#define _export // win32 doesn't always like export
#endif

    extern int AppMain(int, char**);

    class vWindow;
    class vCommandPane;

    typedef struct WindList
      {
	vWindow* window;
	vAppWinInfo* info;
	WindList* nextWinList;
      } WindList;

    typedef struct CmdPaneList
      {
	vCommandPane* commandPane;
	CmdPaneList* nextCPList;
      } CmdPaneList;

    long CALLBACK _export PMdiFrameProc(HWND, UINT, UINT, LPARAM);
    long CALLBACK _export PMdiWindowProc(HWND, UINT, UINT, LPARAM);

    class _appWorkTimer : public vTimer
      {
      public:		//---------------------------------------- public
	_appWorkTimer() {}
	~_appWorkTimer() {}
	virtual void TimerTick();
      private:		//--------------------------------------- private
      };

    class vApp : public vBaseItem
      {
	// give friend access to some special things
	friend class vWindow; // Provide access for vWindow class
	friend class vMenuPane; // Menu Pane needs access
	friend class vCommandPane;
	friend int CMain(HANDLE hInstance, HANDLE hPrevInstance,
		      LPSTR lpszCmdLine, int nCmdShow);	// give main access
	friend long WINAPI _export PMdiFrameProc(HWND, UINT, UINT, LPARAM);
	friend long WINAPI _export PMdiWindowProc(HWND, UINT, UINT, LPARAM);
	friend class _appWorkTimer;
	

      public:		//---------------------------------------- public

	vApp(char* appName, int simSDI = 0, int frameHeight = 0, int frameWidth = 0);	// constructor
	virtual ~vApp();	// destructor
	  
	// Methods to override

	virtual void AppCommand(vWindow* win, ItemVal id, ItemVal retval,
				CmdType ctype);
	virtual void CloseAppWin(vWindow* win);
	virtual void CloseHelpWin(vWindow* win);
	int IsHelpWin(vWindow *Win);
	virtual void Exit(void);
	virtual void KeyIn(vWindow* win, vKey key, unsigned int shift);
	virtual vWindow* NewAppWin(vWindow* win, char* name, int w = 0, int h = 0,
		vAppWinInfo* winInfo = 0);
	virtual  vWindow* NewHelpWin(vWindow* win, char* name, int w, int h);

	// Utlity methods

	vFont GetDefaultFont();

	void GetVVersion(int& major, int& minor)
           { major = V_VersMajor; minor = V_VersMinor;}
	int DefaultHeight() { return _DefaultHeight; }
	int DefaultWidth() {return _DefaultWidth; }
	int IsRunning() {return _running;}  // see if app is running
        void SendWindowCommandAll(ItemVal id, int val, CmdType ctype);
	void SetValueAll(ItemVal id, int val, ItemSetType setType);
        void SetAppTitle(char* title);
	void SetStringAll(ItemVal id, char* str);
	int ShowList();
	vAppWinInfo *getAppWinInfo(vWindow* Win);
	void AppExit(int exitVal);
	int InExit() { return _inExit; }

	// Tasking
	void CheckEvents();

	int EnableWorkSlice(long slice);
	virtual void WorkSlice() {}	// No op by default

	// Windows stuff

	HWND winHwnd() { return _Frame; }
	HWND winClientHwnd() { return _Client; }
        int simSDI() {return _simSDI;}

	HMENU AppFrameMenu() {return hFrameMenu;}

	HPEN _WhitePen;
	HPEN _GrayPen;		// for drawing stuff
	HPEN _LightPen;
        HPEN _ShadowPen;
	HBRUSH _BarBrush;


      protected:	//--------------------------------------- protected

	WindList* _WindowList;		// List of "top level" windows
	CmdPaneList* _CmdPaneList;	// List of command panes

	int _running;			// if we are running

	// The following two are called from main.
	void initialize(int& argc, char** argv,
		HANDLE hInstance, HANDLE hPrevInstance, int nCmdShow);
	int doEventLoop();

      private:		//--------------------------------------- private

	int _DefaultHeight;
	int _DefaultWidth;
        int _frameWidth, _frameHeight;
	int _inExit;		  	// If in exit procudure
	int _simSDI;			// if Simulate SDI interface

	_appWorkTimer* _workTimer;	// timer for work slice

	void registerWindow(vWindow* Win, vAppWinInfo* awinfo);
	void unregisterWindow(vWindow* Win);
	void DispatchWork(void);

	void registerCmdPane(vCommandPane* cmdPane);
	void unregisterCmdPane(vCommandPane* cmdPane);
	void selectCmdPanes(vWindow* parent);

	// Windows stuff

        long MdiFrameProc(HWND hwnd, UINT message,
				UINT wParam, LPARAM lParam);
	int OnCreate(HWND hwnd, CREATESTRUCT FAR* lpCreateStruct);
	void OnClose(HWND hwnd);
	int OnQueryEndSession(HWND hwnd);
	void OnDestroy(HWND hwnd);
	void OnSysCommand(HWND hwnd, UINT cmd, int x, int y);
	void OnSize(HWND hwnd, UINT state, int cx, int cy);
	void OnCommand(HWND hwnd, int id, HWND hwndCtl, UINT codeNotify);
	LRESULT Frame_DefProc (HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	void OnResizeMDIClient(HWND hwnd);

	HMENU hFrameMenu, hFrameSubMenu;	// Default menu
	HWND  _Frame, _Client ;		// Need these for MDI frame
	HANDLE hAccel;
	vWindow* _curThis;	// Needed to fake things out at the start

        HICON _appicon, _winicon; 
      };

    extern vApp *theApp;		// Pointer to single global instance
#endif
