//===============================================================
// vicon.h - Icon class - Windows
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================
#ifndef VICON_H
#define VICON_H
#include <windows.h>


    enum IconType {BitMap, ColorMap};	// types of icons

    class vIcon	// an icon
      {
	//friend class vWinDC;
	//friend class vLabelCmd;
	//friend class vButtonCmd;

      public:		//---------------------------------------- public
	vIcon();
	vIcon(unsigned char* ic, int h, int w, int d = 1, IconType it = BitMap);
	~vIcon();
	IconType iType;		// type of icon
	int height;		// height in pixels
	int width;		// width in pixels
	int depth;		// bits per pixel
	unsigned char* icon;	// ptr to icon array

	HBITMAP GetIconHBM(HDC iDC) {if (_hbm == 0) vIconToBMP(iDC); return _hbm;}

      protected:	//--------------------------------------- protected
      private:		//--------------------------------------- private

	int NeedBMPBytes();
	void vIconToBMP(HDC iDC);

	HBITMAP _hbm;
      };
#endif
