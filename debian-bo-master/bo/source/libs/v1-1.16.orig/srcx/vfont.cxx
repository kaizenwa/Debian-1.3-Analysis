//===============================================================
// vfont.cxx - The font class - X
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vapp.h>
#include <v/vfont.h>
#include <v/vfontlst.h>
#include <v/vutil.h>

  static vFontList _FontList;		// ONE font list

//=======================>>> vFont::vFont <<<=============================
  vFont::vFont(vFontID fam, int size, vFontID sty, vFontID wt, int und)
  {
    SysDebug(Constructor,"vFont::vFont() constructor\n")

    _family = fam; _pointSize = size; _style = sty; _weight = wt;
    _underlined = und; _XFont = 0;

  }

//=====================>>> vFont::vFont <<<===========================
  vFont::vFont(const vFont& ft)
  {
    // Copy constructor - needs to delete object if already
    // created, and copy the values as needed

    _XFont = ft._XFont;
    _family = ft._family; _style = ft._style; _weight = ft._weight;
    _pointSize = ft._pointSize; _underlined = ft._underlined;

  }

//=====================>>> vFont::= <<<===========================
  vFont& vFont::operator =(const vFont& ft)
  {
    if (this == &ft)     // assigning to self
      {
	return *this;
      }

    // just like the copy constructor

    _XFont = ft._XFont;
    _family = ft._family; _style = ft._style; _weight = ft._weight;
    _pointSize = ft._pointSize; _underlined = ft._underlined;

    return *this;
  }

//=====================>>> vFont::SetFontValues <<<===========================
  void vFont::SetFontValues(vFontID fam, int size, vFontID sty, vFontID wt,
    int und)
  {
    _XFont = 0;		// changing characteristics, so need to reload

    SysDebug(Misc,"SetFontValues()\n");

    _family = fam; _style = sty; _weight = wt;
    _pointSize = size; _underlined = und;
  }

//=======================>>> vFont::~vFont <<<=============================
  vFont::~vFont()
  {
    SysDebug(Destructor,"vFont::~vFont() destructor\n")
  }

//=======================>>> vFont::LoadFont <<<=============================
  void vFont::LoadFont()
  {
    // Create the stuff needed for interal font stuff

    if (_XFont != 0)
	return;

    // First, check for default fonts...

    vFont* fptr = _FontList.FindFont(*this);	// see if we already had this font

    if (fptr)					// found one
      {
	*this = *fptr;				// copy it
	return;
      }

    if (_family == vfDefaultSystem)	// default system font
      {
	_XFont = theApp->_XDefaultFont;
      }
    else if (_family == vfDefaultVariable)	// the system default variable font
      {
	_XFont = XLoadQueryFont(theApp->display(), "variable");
      }
    else if (_family == vfDefaultFixed)	// the system default variable font
      {
	_XFont = XLoadQueryFont(theApp->display(), "fixed");
      }
    if (_XFont)
      {
	_FontList.AddFont(*this);
	return;				// all done now
      }

    // To here, then we have to construct an X font descriptor string

    char XFontStr[120];
    char XAltFontStr[120];

    strcpy(XFontStr,"-*-");		// lead "-*-"

    switch (_family)		// add family string
      {
	case vfSerif:			// serifed font - TimesRoman
	    strcat(XFontStr,"times-");	// font family name
	    break;
	case vfSansSerif:		// SansSerif - Swiss or Helvetica
	    strcat(XFontStr,"lucida-");	// font family name
	    break;
	case vfFixed:		// fixed font - Courier
	    strcat(XFontStr,"courier-");	// font family name
	    break;
	case vfDecorative:		// decorative - dingbat
	    strcat(XFontStr,"symbol-");	// font family name
	    break;
	default:
	    strcat(XFontStr,"fixed-");	// font family name
	    break;
      }

    strcpy(XAltFontStr,XFontStr);	// for alternate processing

    switch (_weight)		// weight is next
      {
	case vfNormal:		// normal style, weight
	    strcat(XFontStr,"medium-");
	    break;
	case vfBold:			// boldface
	    strcat(XFontStr,"bold-");
	    break;
	default:
	    strcat(XFontStr,"*-");
	    break;
      }

    switch (_style)
      {
	case vfNormal:		// normal style
	   strcat(XFontStr,"r-");
	   break;
	case vfItalic:		// italic style
	   strcat(XFontStr,"i-");
	   break;
	default:
	   strcat(XFontStr,"*-");
	   break;
      }

    
    strcat(XFontStr,"normal-*-*-"); // up to point size now

    strcat(XAltFontStr,"*-*-*-*-*-");

    char pt[24];

    IntToStr(_pointSize,pt);
    strcat(pt,"0-*-*-*-*-*-*");		// *10 plus rest of string
    strcat(XFontStr,pt);
    strcat(XAltFontStr,pt);

    SysDebug1(Misc,"Loading font: %s\n", XFontStr);
    // Try to load the font with the full specification string
    _XFont = XLoadQueryFont(theApp->display(), XFontStr);

    if (!_XFont)			// Try just the family and pt size
      {
	SysDebug1(Misc,"  trying: %s\n",XAltFontStr);
	_XFont = XLoadQueryFont(theApp->display(), XAltFontStr);
      }

    if (!_XFont)		    // Final bail out is just plain fixed
      {
	SysDebug(Misc,"  Load fails - defaulting to fixed\n");
	_XFont = XLoadQueryFont(theApp->display(), "fixed");
      }

    _FontList.AddFont(*this);		// and add to list
    
  }

//========================>>> vFont::XTextW <<<==========================
  int vFont::XTextW(char* str)
  {
    int len = strlen(str);      // strlen

    LoadFont();			// must be loaded

    // call X to get the length
    return XTextWidth(_XFont, str, len);
  }

//====================>>> vFont::XFontH <<<=============================
  int vFont::XFontH(int& asc, int& des)
  {
    // Return total height of this font. V will use total height, which
    // is most often made up of ascent + descent.  This is too much
    // detail for the kind of apps V will support.
	
    LoadFont();			// must be loaded

    asc = _XFont->ascent;
    des = _XFont->descent;

    int h = _XFont->ascent + _XFont->descent; // &asc might == &des

    SysDebug2(Misc,"Font height %d+%d\n",_XFont->ascent,_XFont->descent);

    return h;
  }

//########################################################################


//======================>>> vFontList::vFontList <<<=======================
  vFontList::vFontList()
  {
    // This will keep a list of this pointers accessed by some id
    _list = 0;
  }

//======================>>> vFontList::~vFontList <<<=======================
  vFontList::~vFontList()
  {
    // This will keep a list of this pointers accessed by some id
    fontList* next;
    for (fontList* tl = _list ; tl != 0 ; tl = next)
      {
	next = tl->next;	// get next one
	delete tl;
      }
  }

//======================>>> vFontList::AddFont <<<=======================
  void vFontList::AddFont(vFont& f)
  {
    // Add to list

    if (f._XFont == 0)		// don't add non-loaded fonts
	return;

    fontList* newList = new fontList;

    newList->font = f;			// copy of the font
    newList->next = _list;
    _list = newList;
  }

//======================>>> vFontList::FindFont <<<=======================
  vFont* vFontList::FindFont(vFont& ff)
  {
    // see if we have a matching font loaded

    for (fontList* fl = _list ; fl != 0 ; fl = fl->next)
      {
	if (fl->font._family == ff._family &&
	    fl->font._style == ff._style &&
	    fl->font._weight == ff._weight &&
	    fl->font._pointSize == ff._pointSize &&
	    fl->font._underlined == ff._underlined)
	  {
	    return &fl->font;
	  }
      }
    return 0;
  }

//########################################################################
