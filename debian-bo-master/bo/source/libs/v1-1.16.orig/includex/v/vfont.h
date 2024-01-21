//===============================================================
// vfont.h - The font class - X11R5
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VFONT_H
#define VFONT_H

#include <X11/Xlib.h>

    class vCanvasPane;	// the vCanvasPane needs to the the X font
    class vApp;		// the vApp needs Xfont, too
    class vListCmd;
    class vComboBoxCmd;

    enum vFontID		// various font related ids
      {
	vfDefaultSystem,	// the default system font
	vfDefaultFixed,		// the system default fixed font
	vfDefaultVariable,	// the system default variable font
	vfSerif,		// serifed font - TimesRoman
	vfSansSerif,		// SansSerif - Swiss or Helvetica
	vfFixed,		// fixed font - Courier
	vfDecorative,		// decorative - dingbat
	vfOtherFont,		// for all other fonts
	vfNormal,		// normal style, weight
	vfBold,			// boldface
	vfItalic,		// italic style
	vfEndOfList
      };


    class vFont		// make the font stuff a class to make it portable
      {
	friend class vCanvasPane;
	friend class vApp;
	friend class vFontList;

      public:		//---------------------------------------- public
	vFont(vFontID fam = vfDefaultFixed, int size = 10,
	   vFontID sty = vfNormal, vFontID wt = vfNormal, int und = 0);

	vFont(const vFont &ft);		// copy constructor

	~vFont();

	vFont& operator =(const vFont& ft);  // vFont = vFont
	int operator ==(const vFont& ft)
	  { return (_family == ft._family && _style == ft._style &&
	    _weight == ft._weight && _pointSize == ft._pointSize  &&
	    _underlined == ft._underlined); }

	int operator !=(const vFont& ft)
	  { return !(_family == ft._family && _style == ft._style &&
	    _weight == ft._weight && _pointSize == ft._pointSize  &&
	    _underlined == ft._underlined); }


	vFontID GetFamily() { return _family; }

	int GetPointSize() { return _pointSize; }

	vFontID GetStyle() { return _style; }

	vFontID GetWeight() { return _weight; }

	int GetUnderlined() { return _underlined; }

	void SetFontValues(vFontID fam = vfDefaultFixed, int size = 10,
	   vFontID sty = vfNormal, vFontID wt = vfNormal, int und = 0);

	// Following should be used only internally

	void LoadFont();	// make sure font with given id is loaded

	int XTextW(char* str);
	int XFontH(int& asc, int& des);

	XFontStruct* GetXFont() { return _XFont;}

      protected:	//--------------------------------------- protected

      private:		//--------------------------------------- private
	vFontID _family;	// family - vfSerif, etc.
	vFontID _style; 	// style - nomal, italic
	vFontID _weight;	// weight - normal, light, bold
	int _pointSize;		// point size of font
	int _underlined;	// if underlined or not

	XFontStruct* _XFont;		// windows handle to font

      };

#endif
