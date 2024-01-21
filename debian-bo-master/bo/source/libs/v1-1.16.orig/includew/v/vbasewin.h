//===============================================================
// vbasewin.h - vBaseWindow class definitions - Windows
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VBASEWIN_H
#define VBASEWIN_H

#include <v/vbaseitm.h>		// base info class

    class vBaseWindow : public vBaseItem
      {
      public:		//---------------------------------------- public

	vBaseWindow(char* name);		// Constructor
	vBaseWindow(const vBaseWindow& w);	// Copy Constructor
	virtual ~vBaseWindow();	// Destructor

	virtual void initialize(void) {}	// Pure virtual function

	virtual void ShowWindow(void) { showBaseWindow(); }

	void closeBaseWindow(void);		// Kill window

	WindowType wType() {return _wType;}	// what kind of window we are

        HWND winHwnd() {return _myHwnd;}


      protected:	//--------------------------------------- protected

	HWND _myHwnd;		// my MDI window
	WindowType _wType;	// what kind of window we are building

	void showBaseWindow(void)
	  {  }	// Pop us up


      private:		//--------------------------------------- private

      };
#endif
