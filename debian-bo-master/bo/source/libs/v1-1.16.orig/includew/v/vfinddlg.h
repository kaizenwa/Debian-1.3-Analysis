//===============================================================
// vFind.h - reply modal dialog
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef vFind_H
#define vFind_H
#include <v/vmodald.h>

    class vApp;
    class vFindDialog : protected vModalDialog
      {
      public:		//---------------------------------------- public
	vFindDialog(vBaseWindow* bw, char* title = "Find") : vModalDialog(bw, title)
	   { added = 0;}
	vFindDialog(vApp *aw, char* title = "Find") : vModalDialog(aw, title)
	   { added = 0;}
	~vFindDialog() {}

	int AskFindPat(char* reply, const int maxlen, int& caseSens);

      protected:	//--------------------------------------- protected

      private:		//--------------------------------------- private

 	int added;
	virtual void DialogCommand(ItemVal id, ItemVal val, CmdType ctype);

      };
#endif
