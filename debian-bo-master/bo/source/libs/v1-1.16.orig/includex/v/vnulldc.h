//===============================================================
// vnulldc.h: a NULL DC class .h file - All platforms
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VNULLDC_H
#define VNULLDC_H

#include <v/vdc.h>

    class vNullDC : public vDC
      {
      public:		//---------------------------------------- public
	vNullDC(void) {}
	virtual ~vNullDC() {}

	// Drawing
	virtual void Clear(void) {}
	virtual void ClearRect(int left, int top, int width, int height) {}
  	virtual void DrawAttrText(int x, int y, char* text, const ChrAttr attr) {}
  	virtual void DrawText(int x, int y, char* text) {}
	virtual void DrawEllipse(int x, int y, int width, int height) {}
	virtual void DrawIcon(int x, int y, vIcon& icon) {}
  	virtual void DrawLine(int x, int y, int xend , int yend) {}
  	virtual void DrawLines(vLine* lineList, int count);
	virtual void DrawPoint (int x, int y) {}
	virtual void DrawPoints (vPoints& pointList, int count);
	virtual void DrawPolygon (int n, vPoint points[],
		int fillStyle = vAlternate) {}
	virtual void DrawRoundedRectangle(int x, int y,
		int width, int height, int radius = 10) {}
	virtual void DrawRectangle(int x, int y, int width, int height) {}
	virtual void DrawRectangles(vRect* rectList, int count);

	// Misc

	virtual int GetPhysHeight() { return 0; }
	virtual int GetPhysWidth() { return 0; }

	virtual void SetBackground(vColor& color) {}

	virtual void SetScale(int mult, int div) {}
	virtual void GetScale(int& m, int& d) { m = 1; d = 1; }

	void SetTranslate(int x, int y) { }
	void SetTransX(int x) {}
	void SetTransY(int y) {}
	void GetTranslate(int& x, int& y) {x = 0; y = 0;}
	int GetTransX() { return 0; }
	int GetTransY() { return 0; }

 	virtual void SetFont(vFont& vf) {}
	virtual vFont GetFont() { return _vf; }

	virtual void SetPen(vPen& pen) {}
	virtual vPen GetPen() { return _vp; }

	virtual void SetBrush(vBrush& brush) {};
	virtual vBrush GetBrush() { return _vb; }

	virtual int TextHeight(int& asc, int& des) {asc = des = 0; return 0;}
	virtual int TextWidth(char* str) {return 0;}

	private:
	  vFont _vf;
	  vBrush _vb;
	  vPen _vp;
      };
#endif
