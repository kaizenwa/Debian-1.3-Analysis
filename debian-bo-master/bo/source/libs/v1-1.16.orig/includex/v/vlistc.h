//===============================================================
// vlistc.h - List Command class
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VLISTC_H
#define VLISTC_H

#include <v/vcmd.h>	// We are derived from vcmd

extern "C"		// wrap in "C"
{
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
}

    const int MAX_LIST = 8;	// put 8 items in the list

    class vCmdParent;

    extern "C"
     {
	void CListCmdCB(Widget w, XtPointer This, XtPointer lr);
	void CJumpProcCB(Widget w, XtPointer This, XtPointer pc_ptr);
	void CScrollProcCB(Widget w, XtPointer This, XtPointer position);
      }

    class vListCmd : public vCmd
      {
	friend void CListCmdCB(Widget w, XtPointer This, XtPointer lr);
	friend void CJumpProcCB(Widget w, XtPointer This, XtPointer pc_ptr);
	friend void CScrollProcCB(Widget w, XtPointer This, XtPointer position);

      public:		//---------------------------------------- public

	vListCmd(vCmdParent* dp, CommandObject* co);

	virtual ~vListCmd();

	virtual void initialize(void);

	virtual int GetCmdValue(ItemVal id);
	virtual void SetCmdVal(ItemVal val, ItemSetType st);

      protected:	//--------------------------------------- protected


      private:		//--------------------------------------- private

	Widget _wScroll;		// Scroll widget for list

	int _ScrlTop;			// Position of top of thumb
	int _ScrlShown;		// Portion of thumb shown
	int _scale;			// scale factor
	float _fscale;			// scale factor


	int _workMapsTo;		// _workList[0] maps to _fullList[n]
	char* _workList[9];		// we will make an 8 item list
	char** _fullList;		// alias pointer to full list
	int _maxWidth;			// max width of any item
	int _numItems;			// number of items on list
	int _curSelection;		// index of current selected item

	void SetScroll(int Shown, int Top);	// set scroll values
	void SetupList(void);
	void DoScroll(int cnt);		// scroll list by n lines

	void ListCmdCB(int index);	// callback for user action
	void ScrollProcCB(int position);
	void JumpProcCB(int percent);

      };
#endif
