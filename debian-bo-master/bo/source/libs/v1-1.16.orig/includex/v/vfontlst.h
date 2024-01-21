//===============================================================
// vfontlst.h - Font List for loaded fonts
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================
#ifndef VFONTLST_H
#define VFONTLST_H
#include <v/vfont.h>

  typedef struct fontList
    {
      vFont font;
      fontList* next;
    } fontList;

  class vFontList
    {
    public:		//---------------------------------------- public
      vFontList();
      ~vFontList();

      void AddFont(vFont& f);
      vFont* FindFont(vFont& ff);

    protected:		//--------------------------------------- protected
    private:		//--------------------------------------- private
      fontList* _list;
    };
#endif
