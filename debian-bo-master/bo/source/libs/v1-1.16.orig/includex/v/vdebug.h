//===============================================================
// vdebug.h - set up debug
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VDEBUG_H
#define VDEBUG_H

#include <v/vmodald.h>

    class vApp;
    class vDebugDialog : public vModalDialog
      {
      public:		//---------------------------------------- public
	vDebugDialog(vBaseWindow* bw,char* title = "Debugging Options") : 
		vModalDialog(bw,title) {  _cmdsAdded = 0; };
	vDebugDialog(vApp* aw,char* title = "Debugging Options") : 
		vModalDialog(aw,title) {  _cmdsAdded = 0; };

	~vDebugDialog() {};

  	void vDebugDialog::SetDebug();

	virtual void DialogCommand(ItemVal id, ItemVal val, CmdType ctype);
	
      protected:	//--------------------------------------- protected

      private:		//--------------------------------------- private
	int _cmdsAdded;
      };
#endif
