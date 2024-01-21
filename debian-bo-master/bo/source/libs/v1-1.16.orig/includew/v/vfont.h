//===============================================================
// vfont.h - The font class - Windows
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

#include <windows.h>
#include <commdlg.h>	// For CHOOSEFONT

    class vCanvasPane;	// the vCanvasPane needs to the the X font
    class vApp;		// the vApp needs Xfont, too
    class vListCmd;
    class vComboBoxCmd;

    enum vFontID		// various font related ids
      {
        vfDefaultSystem,	// system default (fixed for Windows)
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
	void SetWinFontValues(LOGFONT& lf,  CHOOSEFONT& cf);

	void LoadFont(HDC dc, int isPrinterDC);	// make sure font with given id is loaded

	HFONT GetHFONT() { return _hFont; }
	LOGFONT GetLOGFONT() { return _lf; }
	COLORREF GetFontColor() { return _fontColor; }

      protected:	//--------------------------------------- protected

      private:		//--------------------------------------- private
	vFontID _family;	// family - vfSerif, etc.
	vFontID _style; 	// style - nomal, italic, slant
	vFontID _weight;	// weight - normal, light, bold
	int _pointSize;		// point size of font
	int _underlined;	// if underlined or not

	int _isStockFont;	// if a stock font

	COLORREF _fontColor;

	HFONT _hFont;		// windows handle to font
	LOGFONT _lf;		// Window full font inf
  };

#endif
