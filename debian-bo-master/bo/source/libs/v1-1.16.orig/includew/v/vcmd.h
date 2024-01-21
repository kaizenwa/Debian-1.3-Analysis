//===============================================================
// vcmd.h - Base class for Cmds - normally used on Dialogs - Windows
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VCMD_H
#define VCMD_H

#include <v/v_defs.h>	// basic definitons

    const int StdMargin = 4;	// Margin to leave on controls

    class vCmdParent;
    class vDialog;

    class vCmd
      {

	friend class vCmdParent;
	friend class vDialog;
        friend class vCommandPane;

      public:		//---------------------------------------- public

	vCmd(vCmdParent* dp, CommandObject* co);

	virtual ~vCmd() {}

	virtual void initialize(void) {}

        void CopyToLocal(void);

	// define these to interface with specific Pane's setters

	virtual int GetCmdValue(ItemVal iv) {return 0;}

	virtual void ResetItemValue(void) { dlgCmd->retVal = _origVal;}

	virtual void SaveItemValue(void) { _origVal = dlgCmd->retVal;}

	virtual void SetCmdVal(ItemVal val, ItemSetType st) {}

	virtual void SetCmdStr(char* s) {}

	virtual void CmdCallback(int id, WORD codeNotify);

	// for OWNERDRAW controls

	virtual int MEASUREITEM(int id, MEASUREITEMSTRUCT* mis) {return 0;}

	virtual int DRAWITEM(int id, DRAWITEMSTRUCT* dis) {return 0;}

	virtual HWND GetMyHwnd(ItemVal id);

      protected:	//--------------------------------------- protected

        int LabelWidth(char* lbl);

	vCmdParent*	_parentWin;	// parent window of this object
	char*		_title;		// current title
	void*		_itemList;

	CommandObject*	dlgCmd;		// pointer to the static definition

	int		_Sensitive;
	ItemVal		_cmdId;		// original id
	ItemVal		_origVal;	// original value of object
	ItemVal		_retVal;	// the value we are using

	HWND		wParent;	// parent widget used for this Cmd
	HWND		wCmd;		// main widget for this Cmd

	int _x, _y,  _h, _w;		// size of the object
        int _CtrlOffset;		// Offset into dialog structure

      private:		//--------------------------------------- private


      };
#endif
