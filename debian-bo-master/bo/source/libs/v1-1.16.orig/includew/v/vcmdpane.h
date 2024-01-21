//===============================================================
// vcmdpane.h - Command Pane class .h file - Windows
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VCMDPANE_H
#define VCMDPANE_H

#include <v/v_defs.h>

#include <v/vpane.h>		// we are derived from a pane
#include <v/vcmdprnt.h>		// and from cmdParent

    class vWindow;		// we are part-of a vWindow

    class vCommandPane : public vPane, public vCmdParent
      {
        friend vCommandPane* vGetLowestCmdPane();
	friend void vSetShownCmdPane(HWND MdiWindow, int OnOff);
        friend class vApp;

      public:		//---------------------------------------- public

	vCommandPane(CommandObject* cList);
	vCommandPane(vStatus* s_bar);		// status bar version
 	virtual ~vCommandPane();

   	virtual void ProcessCmd(ItemVal id, ItemVal rv, CmdType ct);
	virtual void initialize(vWindow* pWin, HWND mdiChildWin);
	virtual void ShowPane(int OnOrOff);

	// Windows utility

	int DynamicDlgProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

      protected:	//--------------------------------------- protected

	virtual int GetPaneValue(ItemVal id, int& val);
	virtual void SetPaneValue(ItemVal id, int val, ItemSetType setType);
	virtual void SetPaneString(ItemVal id, char* str);


      private:		//--------------------------------------- private

	int OnInitDialog (HWND hwnd, HWND hwndFocus, LPARAM lParam);
	void OnEnable (HWND hwnd, BOOL fEnable);
	int OnPaint (HWND hwnd);
	void OnCommand (HWND hwnd, int id, HWND hwndCtl, WORD codeNotify);

	CommandObject* _origList;		// handle to list of commands
	CommandObject* _cmdObj;			// for translated status pane
        int _cmdObjCount;

	HWND _associatedMdiWindow;

	int _isDisplayed;			// If vWindow thinks it is displayed
        int _isShown;				// If it has been drawn

      };
#endif
