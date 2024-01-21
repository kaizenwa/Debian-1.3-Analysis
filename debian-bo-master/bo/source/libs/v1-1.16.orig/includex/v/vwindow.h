//===============================================================
// vWindow.h - vWin class definitions - X11R5
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VWINDOW_H
#define VWINDOW_H

#include <v/v_defs.h>
#include <v/vbasewin.h>		// we are derived from vBaseWindow
#include <v/vapp.h>		// we are a friend of the app class

#include <v/vmenu.h>	// so we can get to GetMenuThis

    // We will allow a Button Bar and a Status Bar in Windows

    class vPane;
    class vCanvasPane;

    typedef struct PaneList
      {
	vPane* pane;		// pointer to the pane object
	PaneList *nextPL;	// next pane
      } PaneList;

    extern "C"
      {
#ifdef USE_CLOSE_CB
	void CcloseCB(Widget w, XtPointer client_data, XtPointer call_data);
#endif
	void CwmDeleteCB(Widget w, XtPointer client_data, XEvent* e);
	void CKeyInEV(Widget w, XtPointer client_data, XEvent* event, char *x);
      }

    class vWindow : public vBaseWindow
      {
	// gnu c++ doesn't seem to let these work right

#ifdef USE_CLOSE_CB
	friend void CcloseCB(Widget w, XtPointer client_data, XtPointer call_data);
#endif
	friend void CwmDeleteCB(Widget w, XtPointer client_data, XEvent* e);
	friend void CKeyInEV(Widget w, XtPointer client_data, XEvent* event, char *x);

	friend class vPane;
	friend class vCanvasPane;
	friend class vBaseGLCanvasPane;

      public:		//---------------------------------------- public
	vWindow(char *name = "+", int width = 0, int height = 0,
	  WindowType wintype = WINDOW);	// Constructor

        vWindow(const vWindow&);		// Copy Constructor

	virtual ~vWindow();		// Destructor

	// Methods to Override

	virtual void KeyIn(vKey keysym, unsigned int shift);  // KeyIn method
	virtual void MenuCommand(ItemVal id);
	virtual void WindowCommand(ItemVal id, ItemVal retval, CmdType ctype);
	virtual void WorkSlice(void) {}

	// Utility Methods

	virtual void AddPane(vPane* add_pane);	// add a pane to the window
	virtual int GetValue(ItemVal id);
	virtual void GetPosition(int& left, int& top, int& width, int& height);
	virtual void SetPosition(int left, int top);
  	virtual void RaiseWindow(void);	// raise this window
	virtual void SetString(ItemVal id, char* str);
	virtual void SetStringAll(ItemVal id, char* str);
	virtual void SetTitle(char* title);
	virtual void SetValue(ItemVal id, int val, ItemSetType setType);
	virtual void SetValueAll(ItemVal id, int val, ItemSetType setType);
	void ShowPane(vPane* wpane, int OnOrOff);
	virtual void ShowWindow();

	// Other Methods

	virtual void CloseWin(void);	// callback for close button

	// Internal Methods

	vMenuPane* GetMenuThis(Widget w);
	void vAddMenuBar(void);

	int getWinHeight() {return _WinHeight;}
	int getWinWidth() {return _WinWidth;}
	void setWinHeight(const int h) {_WinHeight = h;}
	void setWinWidth(const int w) {_WinWidth = w;}

      protected:	//--------------------------------------- protected


	Widget _menuBar;	// the menu bar widget
	Widget _closeButton;	// close button widget

	int _WinHeight;		// height of draw window
	int _WinWidth;		// width of draw window

      private:		//--------------------------------------- private

	int wm_delete_added;

	void initialize(void);

	static int _numWindows;	// how many total windows are up

	static vWindow* _WinList;	// all windows

	PaneList* _paneList;		// panes for this window

	vPane* _menuPane;		// pointer to our menu

	vWindow* _NextWin;		// link to next window

	void KeyInEV(unsigned int keycode, unsigned int state);

      };
#endif
