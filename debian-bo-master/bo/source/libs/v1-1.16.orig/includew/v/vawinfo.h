//===============================================================
// vawinfo.h - vAppWinInfo utility class for tracking windows
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VAWINFO_H
#define VAWINFO_H

#include <v/v_defs.h>	// include basic defs

    class vAppWinInfo
      {
      public:		//---------------------------------------- public

	vAppWinInfo(char* name = "", void* ptr = 0);
	virtual ~vAppWinInfo();

	virtual char* infoName() {return _infoName;}

      protected:	//--------------------------------------- protected

	char*	_infoName;		// name of item
        void* _ptr;

      private:		//--------------------------------------- private

      };
#endif
