//===============================================================
// vdc.h: vDC class .h file - Windows
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VDC_H
#define VDC_H

#include <v/v_defs.h>           // Basic defs
#include <v/vfont.h>            // for fonts
#include <v/vpen.h>             // for our pen
#include <v/vbrush.h>           // for our brush
#include <v/vprinter.h>

    class vIcon;
    class vColor;

    typedef unsigned char ChrAttr;      // An attribute for a display char
    typedef unsigned char Chr;          // A character to display

    typedef struct dChar                // A display Char
      {
        ChrAttr attr;                   // The attribute of this char
        Chr chr;                        // the character
      } dChar;

const ChrAttr ChNormal = 0;
const ChrAttr ChReverse = 0x01;		// True Reverse
const ChrAttr ChHighlight = 0x02;	// Contrasting highlight
const ChrAttr ChDimColor = 0x80;        // dim color (half intensity)
const ChrAttr ChRed = 0x40;             // various colors
const ChrAttr ChGreen = 0x20;
const ChrAttr ChBlue = 0x10;
const ChrAttr ChYellow = 0x60;
const ChrAttr ChMagenta = 0x50;
const ChrAttr ChCyan = 0x30;
const ChrAttr ChGray = 0xF0;
const ChrAttr ChWhite = 0x70;

    class vMemoryDC;

    class vDC
      {
        friend class vWinDC;
      public:           //---------------------------------------- public
        vDC(void) { _Mult = 1; _Div = 1; _hasScale = 0; _tx = 0 ; _ty = 0;
                _physHeight = _physWidth = 0;}

        virtual ~vDC(void) {}

        // Drawing
        virtual void Clear(void) = 0;
	virtual void ClearRect(int left, int top, int width, int height) = 0;
        virtual void CopyFromMemoryDC(vMemoryDC* vmemdc, int destX, int destY,
	    int srcX = 0, int srcY = 0, int srcW = 0, int srcH = 0) // V:1.13
	    {}
	 
#ifdef DRAWARC
        virtual void DrawArc(int xx1, int yy1, int xx2, int yy2, int xxc, int yyc) = 0;
#endif
        virtual void DrawAttrText(int x, int y, char* text, const ChrAttr attr) = 0;
        virtual void DrawText(int x, int y, char* text) = 0;
        virtual void DrawEllipse(int x, int y, int width, int height) = 0;
        virtual void DrawIcon(int x, int y, vIcon& icon) {};
        virtual void DrawLine(int x, int y, int xend , int yend) = 0;
	virtual void DrawLines(vLine* lineList, int count) = 0;
	virtual void DrawColorPoints(int x, int y, int nPoints, vColor* pointList) {}

        virtual void DrawPoint (int x, int y) = 0;
	virtual void DrawPoints (vPoint* pointList, int count) = 0;
        virtual void DrawPolygon (int n, vPoint points[],
                int fillStyle = vAlternate) = 0;
        virtual void DrawRoundedRectangle(int x, int y,
                int width, int height, int radius = 10) = 0;
        virtual void DrawRectangle(int x, int y, int width, int height) = 0;
        virtual void DrawRectangles(vRect* rectList, int count) = 0;

        // Not all DrawingCanvases need Rubber Lines!
        virtual void DrawRubberLine(int x, int y, int xend, int yend) {}
        virtual void DrawRubberEllipse(int x, int y, int width, int height) {}
        virtual void DrawRubberPoint(int x, int y) {}
        virtual void DrawRubberRectangle(int x, int y, int width, int height) {}
        
        // Printer
        virtual int BeginPrinting() {return 0;}
        virtual void EndPrinting() {}
        virtual void BeginPage() {}
        virtual void EndPage() {}
     // virtual void SetPrinter(vPrinter& printer){}

        // Misc

        virtual int GetPhysHeight() { return _physHeight; }
        virtual int GetPhysWidth() { return _physWidth; }

        virtual void SetBackground(vColor& color) {}
        virtual void SetScale(int mult, int div)
         {_Mult = mult ; _Div = div ; _hasScale = (_Mult != 1 || _Div != 1); }
        virtual void GetScale(int& m, int& d) {m = _Mult; d=_Div;}

        void SetTranslate(int x, int y) { _tx = x; _ty = y; }
        void SetTransX(int x) {_tx = x;}
        void SetTransY(int y) {_ty = y;}
        void GetTranslate(int& x, int& y) {x = _tx; y = _ty;}
        int GetTransX() { return _tx; }
        int GetTransY() { return _ty; }

        virtual void SetFont(vFont& vf) {}
        virtual vFont GetFont() { return _font; }       // Get text font id

        virtual void SetPen(vPen& pen) {}
        virtual vPen GetPen() { return _pen; }

        virtual void SetBrush(vBrush& brush) {}
        virtual vBrush GetBrush() { return _brush; }

        virtual int TextHeight(int& asc, int& des) {return 0;}
        virtual int TextWidth(char* str) {return 0;}


      protected:        //--------------------------------------- protected

        int _Mult;                      // scaling factors
        int _Div;
        int _tx;                // translation
        int _ty;
        int _hasScale;                  // if have scale
        int _physHeight;                // maximum height of canvas
        int _physWidth;                 // max width of canvas

        vBrush _brush;                  // the painting brush
        vFont _font;                    // the current font
        vPen _pen;                      // the drawing pen

        int Scale(int x) { return (_hasScale) ? ((x*_Mult)/_Div) : x; }

      private:          //--------------------------------------- private

      };
#endif
