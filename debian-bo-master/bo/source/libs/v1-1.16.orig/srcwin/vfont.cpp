//===============================================================
// vfont.cxx - The font class - Windows
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vwin32.h>		// for Win 32 stuff
#include <v/vapp.h>
#include <v/vfont.h>

//=======================>>> vFont::vFont <<<=============================
  vFont::vFont(vFontID fam, int size, vFontID sty, vFontID wt, int und)
  {
    SysDebug(Constructor,"vFont::vFont() constructor\n")

    _family = fam; _pointSize = size; _style = sty; _weight = wt;
    _underlined = und; _isStockFont = 0; _hFont = 0;
    _fontColor = 0;
    memset(&_lf, 0, sizeof(LOGFONT));

  }

//=====================>>> vFont::vFont <<<===========================
  vFont::vFont(const vFont& ft)
  {
    // Copy constructor - needs to delete object if already
    // created, and copy the values as needed

    if (_hFont != 0 && !_isStockFont)
      ::DeleteObject(_hFont);
    _isStockFont = 0; _hFont = 0;
    _family = ft._family; _style = ft._style; _weight = ft._weight;
    _pointSize = ft._pointSize; _underlined = ft._underlined;
    _fontColor = ft._fontColor;
    _lf = ft._lf;

  }

//=====================>>> vFont::= <<<===========================
  vFont& vFont::operator =(const vFont& ft)
  {
    if (this == &ft)     // assigning to self
      {
	return *this;
      }

    // just like the copy constructor

    if (_hFont != 0 && !_isStockFont)
      ::DeleteObject(_hFont);
    _isStockFont = 0; _hFont = 0;
    _family = ft._family; _style = ft._style; _weight = ft._weight;
    _pointSize = ft._pointSize; _underlined = ft._underlined;
    _fontColor = ft._fontColor;
    _lf = ft._lf;

    return *this;
  }

//=====================>>> vFont::SetFontValues <<<===========================
  void vFont::SetFontValues(vFontID fam, int size, vFontID sty, vFontID wt,
    int und)
  {
    if (_hFont != 0 && !_isStockFont)
	::DeleteObject(_hFont);
    _isStockFont = 0; _hFont = 0;
    _family = fam; _style = sty; _weight = wt;
    _pointSize = size; _underlined = und;
    _fontColor = 0;
  }

//=====================>>> vFont::SetWinFontValues <<<========================
  void vFont::SetWinFontValues(LOGFONT& lf, CHOOSEFONT& cf)
  {
    if (_hFont != 0 && !_isStockFont)
	::DeleteObject(_hFont);
    _lf = lf;

    _isStockFont = 0; _hFont = 0;

    // Map to whatever

    // First, set what will be true no matter what

    _pointSize = cf.iPointSize / 10;
    _style = lf.lfItalic ? vfItalic : vfNormal;
    _underlined = lf.lfUnderline;
    _fontColor = cf.rgbColors;

    // Now set things that might make it NOT a V font.

    if (lf.lfWeight == FW_NORMAL)
	_weight = vfNormal;
    else if (lf.lfWeight == FW_BOLD)
	_weight = vfBold;
    else
      {
	_weight = vfNormal;
	goto WinNative;
      }
    if (lf.lfStrikeOut)
	goto WinNative;

    // Now, determine if it is a V family

    if (lf.lfPitchAndFamily == (DEFAULT_PITCH | FF_ROMAN) &&
	strcmp(lf.lfFaceName,"Times New Roman") == 0)
      {
	_family = vfSerif;
	return;
      }
    else if (lf.lfPitchAndFamily == (DEFAULT_PITCH | FF_SWISS) &&
	strcmp(lf.lfFaceName,"Arial") == 0)
      {
	_family = vfSansSerif;
	return;
      }
    else if (lf.lfPitchAndFamily == (DEFAULT_PITCH | FF_MODERN) &&
	strcmp(lf.lfFaceName,"Courier New") == 0)
      {
	_family = vfFixed;
	return;
      }
    else if (lf.lfPitchAndFamily == (DEFAULT_PITCH | FF_DECORATIVE) &&
	strcmp(lf.lfFaceName,"Wingdings") == 0 && lf.lfCharSet == SYMBOL_CHARSET)
      {
	_family = vfDecorative;
	return;
      }

  WinNative:			// a Native Windows font
      _family = vfOtherFont;
      return;

  }

//=======================>>> vFont::~vFont <<<=============================
  vFont::~vFont()
  {
    SysDebug(Destructor,"vFont::~vFont() destructor\n")
    if (_hFont != 0 && !_isStockFont)
      ::DeleteObject(_hFont);		// free the font resource
  }

//=======================>>> vFont::LoadFont <<<=============================
  void vFont::LoadFont(HDC dc, int isPrinterDC)
  {
    // Create the stuff needed for interal font stuff

    if (_hFont != 0)
	return;

    SysDebug(Misc,"Loading font\n");

    BYTE ff_italic = 0;
    int ff_weight = 0;
    int ff_family = 0;
    char *ff_face = NULL;
    BYTE chrSet = ANSI_CHARSET;

    _isStockFont = 0;	// assume not a stock font


    switch (_family)
      {
	case vfDefaultVariable:	// default font
	    if (isPrinterDC)
	      {
		ff_family = FF_SWISS;
		ff_face = "Arial";
	      }
	    else
	      {
		_isStockFont = 1;
		_hFont = (HFONT) ::GetStockObject(SYSTEM_FONT);
	      }
	    break;

	// The rest of the fonts will be Windows TrueType fonts

	case vfSerif:	// Serif font
	    ff_family = FF_ROMAN;
	    ff_face = "Times New Roman" ;
	    break;

	case vfSansSerif:	// SansSerif Font
	      ff_family = FF_SWISS;
	      ff_face = "Arial";
	      break;

	case vfFixed:		// Fixed
	     ff_family = FF_MODERN;
	     ff_face = "Courier New";
	     break;

	case vfDecorative:	// Decorative
	     ff_family = FF_DECORATIVE;
	     ff_face = "Wingdings";
	     chrSet = SYMBOL_CHARSET;
	     break;

	case vfOtherFont:	// set by font picker dialog
	  {
	    if (isPrinterDC)	// Need to override this to be same as
		_lf.lfHeight = _pointSize*12/10; // other "V" fonts
	    _hFont = ::CreateFontIndirect(&_lf);
            if (_hFont == NULL)		// just in case
	      {
		_hFont = (HFONT) ::GetStockObject(SYSTEM_FONT);
		_isStockFont = 1;
	      }
	    return;
	  }

	case vfDefaultFixed:	// default font
	case vfDefaultSystem:
	default:
            if (isPrinterDC)
	      {
		ff_family = FF_MODERN;
	        ff_face = "Courier New" ;
	      }
	    else
	      {
		_isStockFont = 1;
		_hFont = (HFONT) ::GetStockObject(SYSTEM_FIXED_FONT);
	      }
	    break;
      }

    // Now, set the rest of the font attributes

    ff_italic = (_style == vfItalic) ? 1 : 0;
					      ;
    if (_weight == vfNormal)
      ff_weight = FW_NORMAL;
    else if (_weight == vfBold)
      ff_weight = FW_BOLD;

    if (!_isStockFont)		// load non stock fonts
      {
	int nHeight;
	// This is a bit of a kludge, but these values give fonts that
	// are very close to the same physical print size as the PostScript
        // version. It doesn't look too bad!
	if (isPrinterDC)
	    nHeight = _pointSize*12/10;
	else
	    nHeight = _pointSize*GetDeviceCaps(dc, LOGPIXELSY)/72;
	_hFont = ::CreateFont(nHeight, 0, 0, 0,ff_weight,ff_italic,
		(BYTE)_underlined,
                0, chrSet, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
		PROOF_QUALITY, DEFAULT_PITCH | ff_family, ff_face);
      }
    if (_hFont == NULL)		// just in case
      {
	_hFont = (HFONT) ::GetStockObject(SYSTEM_FONT);
	_isStockFont = 1;
      }
  }


//########################################################################
