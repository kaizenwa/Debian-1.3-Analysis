//===============================================================
// vreply.h - reply modal dialog - Windows
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VREPLY_H
#define VREPLY_H
#include <v/vmodald.h>

    class vApp;

    class vReplyDialog : protected vModalDialog
      {
      public:		//---------------------------------------- public
	vReplyDialog(vBaseWindow* bw, char* title = "Reply") :
	    vModalDialog(bw, title) { added = 0;}
	vReplyDialog(vApp* aw, char* title = "Reply") :
	    vModalDialog(aw, title) {added = 0;}
	~vReplyDialog() {}

	int Reply(const char* msg, char* reply, const int maxlen);

      protected:	//--------------------------------------- protected

      private:		//--------------------------------------- private

	virtual void DialogCommand(ItemVal id, ItemVal val, CmdType ctype);
	int added;

      };
#endif
