//===============================================================
// vcmdpane.h - Command Pane class .h file - X11R5
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

      public:		//---------------------------------------- public

	vCommandPane(CommandObject* cList);
 	virtual ~vCommandPane();

   	virtual void ProcessCmd(ItemVal id, ItemVal rv, CmdType ct);
	virtual void initialize(vWindow* pWin, Widget pWidget);
	virtual void ShowPane(int OnOrOff);

      protected:	//--------------------------------------- protected

	virtual int GetPaneValue(ItemVal id, int& val);
	virtual void SetPaneValue(ItemVal id, int val, ItemSetType setType);
	virtual void SetPaneString(ItemVal id, char* str);


      private:		//--------------------------------------- private

	CommandObject* _origList;		// handle to list of commands

      };
#endif
