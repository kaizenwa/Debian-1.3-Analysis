//===============================================================
// vfontsel.h - font select modal dialog - Windows
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VSELFONT_H
#define VSELFONT_H
#include <v/v_defs.h>
#include <v/vfont.h>
#include <commdlg.h>


    class vApp;
    class vBaseWindow;

    class vFontSelect
      {
      public:		//---------------------------------------- public
	vFontSelect(vBaseWindow* bw, char* title = "Select Font");
	vFontSelect(vApp* aw, char* title = "Select Font");
	~vFontSelect() {}

	int FontSelect(vFont& font, const char* msg = "Select Font" );

      protected:	//--------------------------------------- protected

      private:		//--------------------------------------- private
	HWND _parentHWND;
	CHOOSEFONT _cf;
	LOGFONT _lf;

	void init();

      };
#endif
