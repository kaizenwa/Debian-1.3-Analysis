//===============================================================
// vcolor.h: vColor class header for drawing - Windows
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VCOLOR_H
#define VCOLOR_H

#include <windows.h>		// V 1.15a

// For use in Color buttons
const int M_Black = 32020;
const int M_Red = 32021;
const int M_DimRed = 32022;
const int M_Green = 32023;
const int M_DimGreen = 32024;
const int M_Blue = 32025;
const int M_DimBlue = 32026;
const int M_Yellow = 32027;
const int M_DimYellow = 32028;
const int M_Magenta = 32029;
const int M_DimMagenta = 32030;
const int M_Cyan = 32031;
const int M_DimCyan = 32032;
const int M_DarkGray = 32033;
const int M_MedGray = 32034;
const int M_White = 32035;
const int M_ColorFrame = 32036;

// Index into array
const int vC_Black = 0;
const int vC_Red = 1;
const int vC_DimRed = 2;
const int vC_Green = 3;
const int vC_DimGreen = 4;
const int vC_Blue = 5;
const int vC_DimBlue = 6;
const int vC_Yellow = 7;
const int vC_DimYellow = 8;
const int vC_Magenta = 9;
const int vC_DimMagenta = 10;
const int vC_Cyan = 11;
const int vC_DimCyan = 12;
const int vC_DarkGray = 13;
const int vC_MedGray = 14;
const int vC_White = 15;

    class vPen;

    class vColor
      {
	friend class vPen;
	friend class vBrush;
      public:		//---------------------------------------- public
	vColor(unsigned int rd = 0, unsigned int gr = 0, unsigned int bl = 0);
	~vColor() {}

	// Default shallow copy constructor and assignment ok.

	int operator ==(const vColor& c2)
	  {return (_r == c2._r && _g == c2._g && _b == c2._b);}
	int operator !=(const vColor& c2)
	  {return (_r != c2._r || _g != c2._g || _b != c2._b);}
	void ResetColor(unsigned int rd = 0, unsigned int gr = 0,
	     unsigned int bl = 0);
	void ResetColor(vColor& c);
	void Set(unsigned int rd = 0, unsigned int gr = 0, unsigned int bl = 0);
	void Set(vColor& c);
	void SetR(unsigned int rd = 0);
	void SetG(unsigned int gr = 0);
	void SetB(unsigned int bl = 0);
	int BitsOfColor();

	unsigned int r() { return (unsigned int) _r;}
	unsigned int g() { return (unsigned int) _g;}
	unsigned int b() { return (unsigned int) _b;}

	COLORREF pixel() { return _pixel; }

      protected:	//--------------------------------------- protected

	unsigned char _r;
	unsigned char _g;
	unsigned char _b;
	unsigned char _padtoevenbyte;

      private:		//--------------------------------------- private
	COLORREF _pixel;

      };

    extern vColor vStdColors[16];
    extern char* vColorNames[16];
#endif
