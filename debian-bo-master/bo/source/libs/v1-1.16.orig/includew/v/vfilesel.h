//===============================================================
// vfilesel.h - reply modal dialog - Windows
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VFILESEL_H
#define VFILESEL_H
#include <v/v_defs.h>
#include <commdlg.h>


    class vApp;
    class vBaseWindow;

    class vFileSelect
      {
      public:		//---------------------------------------- public
	vFileSelect(vBaseWindow* bw, char* title = "File Select");

	vFileSelect(vApp* aw, char* title = "File Select");
	~vFileSelect() {}

	int FileSelect(const char* msg, char* filename,
	    const int maxlen, char** filter, int& filterIndex);
	int FileSelectSave(const char* msg, char* filename,
	    const int maxlen, char** filter, int& filterIndex);

      protected:	//--------------------------------------- protected

      private:		//--------------------------------------- private

	HWND _parentHWND;

	OPENFILENAME ofn;	// Windows struct

	void init();

	void buildFilter(char* fbuff, char** filter, int& fi);

	void strcatEOS(char* to, char *from);


      };
#endif
