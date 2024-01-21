//===============================================================
// vicon.h - Class for Icons
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

#include <v/vapp.h>

    class vIcon	// an icon
      {
	friend class vCanvasPaneDC;
	friend class vLabelCmd;
        friend class vButtonCmd;

      public:		//---------------------------------------- public
	vIcon(unsigned char* ic, int h, int w, int d = 1);
	~vIcon();

	int height;		// height in pixels
	int width;		// width in pixels
	int depth;		// bits per pixel
	unsigned char* icon;	// ptr to icon array

	Pixmap GetXPM(int cmd = 0);	// X version

      protected:	//--------------------------------------- protected
      private:		//--------------------------------------- private
	Pixmap _pm;
	static GC _GC;
	int _cmd;

      };
#endif
