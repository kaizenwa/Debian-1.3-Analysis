//===============================================================
// vicon.cpp - icon class
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vwin32.h>		// for Win 32 stuff
#include <v/vicon.h>

//=============================>>> vIcon::vIcon <<<=====================
  vIcon::vIcon()
  {
    _hbm = 0;
  }

//=============================>>> vIcon::vIcon  <<<=====================
  vIcon::vIcon(unsigned char* ic, int h, int w, int d, IconType it)
  {
    icon = ic; height = h; width = w; depth = d; iType = it;
    _hbm = 0;
  }

//=============================>>> vIcon::~vIcon <<<=====================
  vIcon::~vIcon()
  {
    if (_hbm != 0) ::DeleteObject(_hbm);
  }

//=============================>>> vIcon::NeedBMPBytes <<<==================
  int vIcon::NeedBMPBytes()
  {
    // calculate how many bytes of storage a BMP will take

    int needwords = width / 16;
    if ((width - ((width/16)*16)) != 0)	// need one more word if remainder > 0
	++needwords;
    return (needwords * 2) * height;	// convert to bytes
  }

//=============================>>> vIcon::vIconToBMP  <<<=====================
  void vIcon::vIconToBMP(HDC iDC)
  {
    static unsigned char Xmask[] = {0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80};
    static unsigned char Wmask[] = {0x80,0x40,0x20,0x10,0x08,0x04,0x02,0x01};


    //@@ This needs to be completely redone using DIBs instead of BMPS.
    //@@ Everyone that counts on this needs to switch, too.
    //@@ For now, we will draw with the wrong color map!

    if (depth == 8)		// 8 bit icons
      {
	unsigned char* vbytes = icon;
	BYTE red[256], green[256], blue[256];

	unsigned int numColors = (unsigned int) *vbytes++ + 1; // how many colors to map

	for (unsigned int ix = 0 ; ix < numColors ; ++ix) // map the colors
	  {
	     red[ix] = *vbytes++; green[ix] = *vbytes++; blue[ix] = *vbytes++;
	  }

	// Colors mapped, vbytes now points to beginning of data

	HDC bmDC = ::CreateCompatibleDC(iDC); 	// A memory DC to draw into

	// First, create a blank bitmap compatible with the
	// current context that is size of bitmap.

	_hbm = ::CreateCompatibleBitmap(iDC, width, height);

	HBITMAP oldBM = ::SelectObject(bmDC,_hbm);	// Select bm into dc
	// I don't know why, but this seems to need to be inverted!
       //	::SetROP2(bmDC, R2_NOTCOPYPEN);	// notcopy pen

	COLORREF wcolor;
	int bmIndex;

	for (int r8 = 0 ; r8 < height ; ++r8)
	    for (int c8 = 0 ; c8 < width ; c8++)
	      {
		bmIndex = vbytes[(r8*width)+c8];    // Index to colors
		// Now map that color to a COLORREF

		wcolor = RGB(red[bmIndex],green[bmIndex],blue[bmIndex]);
		(void)::SetPixel(bmDC,c8,r8,wcolor);	    // Draw into it
	      }

	::SelectObject(bmDC,oldBM);
	::DeleteDC(bmDC);
      }
    else if (depth > 8)		// 24 bit icons
      {
	unsigned char R, G, B;
	unsigned char* vbytes = icon;
	HDC bmDC = ::CreateCompatibleDC(iDC);

	// First, create a blank bitmap compatible with the
	// current context that is size of bitmap.

	_hbm = ::CreateCompatibleBitmap(iDC, width, height);

	HBITMAP oldBM = ::SelectObject(bmDC,_hbm);	// Select bm into dc
       //	::SetROP2(bmDC, R2_NOTCOPYPEN);	// notcopy pen

	COLORREF wcolor;

	for (int r8 = 0 ; r8 < height ; ++r8)
	    for (int c8 = 0 ; c8 < width ; c8++)
	      {
		R = *vbytes++; G = *vbytes++; B = *vbytes++;
		// Now map that color to a COLORREF
		wcolor = RGB(R,G,B);
		(void)::SetPixel(bmDC,c8,r8,wcolor);	    // Draw into it
	      }

	::SelectObject(bmDC,oldBM);
	::DeleteDC(bmDC);
      }
    else if (depth == 1)
      {
	// We will use depth == 1 as the default case since if the
	// depth isn't legal, we will at least get some kind of picture
	// of the right size.

	unsigned char* bmbits = new unsigned char[NeedBMPBytes()];	// allocate space

	// Now need to copy bytes over, padding to even (words)

	unsigned char X,W;
	unsigned char* from = icon;
	unsigned char* to = bmbits;

	int rowbytes = width / 8;	// number of bytes
	if ((width & 7) != 0)
	    ++rowbytes;		// bytes padded, need to bump

	int rowwords = rowbytes / 2;
	if ((rowbytes & 1) != 0)
	    ++rowwords;

	for (int row = 0 ; row < height ; ++row)	// copy by row
	  {
	    for (int col = 0 ; col < rowbytes ; ++col)
	      {
		X = *from++;		// copy of X byte
		W = 0;
		for (int ix = 0 ; ix < 8 ; ++ix) // reverse bits
		  {
		    if (X & Xmask[ix])
			W |= Wmask[ix];
		  }
		*to++ = W;
	      }
	    // Finished a row, need to pad to even bytes
	    if ((rowbytes & 1) != 0)
		*to++ = 0xff;		// pad with 0's
	  }

	// Now, create the bitmap
	_hbm = ::CreateBitmap(width, height, 1, 1, bmbits);
	delete [] bmbits;
      }

  }
