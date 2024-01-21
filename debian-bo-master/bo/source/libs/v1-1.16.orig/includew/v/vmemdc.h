//===============================================================
// vcpdc.h: MemoryDC class .h file - X11R5
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VMEMDC_H
#define VMEMDC_H

#include <v/vwindc.h>

    class vMemory;

    class vMemoryDC : public vWinDC
      {

      public:		//---------------------------------------- public

	vMemoryDC(int width = 256, int height = 256);

	virtual ~vMemoryDC();

        virtual void Clear(void);
        virtual void ClearRect(int left, int top, int width, int height);
        virtual void SetBackground(vColor& color);
        virtual void SetFont(vFont& vf);

	virtual void GetHDC();
	virtual void ReleaseHDC();

	int CreatedOK() {return _memBM != 0;}

      protected:	//--------------------------------------- protected


      private:		//--------------------------------------- private

	HBITMAP _memBM;
	HBITMAP _oldBM;
      };
#endif
