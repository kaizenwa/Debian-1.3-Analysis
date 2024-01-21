//===============================================================
// vbaseitm.h - the base class that holds handle information
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VBASEITEM_H
#define VBASEITEM_H

#include <v/v_defs.h>	// include basic defs


    class vBaseItem
      {
      public:		//---------------------------------------- public

	vBaseItem(const vBaseItem& b);	// Copy constructor
	virtual ~vBaseItem();		// needed to free name

	const HANDLE vHandle() { return _vHandle; }
	const char* name() { return (const char*) _name;}

      protected:	//--------------------------------------- protected

	char*	_name;		// name of item
	HANDLE	_vHandle;	// Window HANDLE widget
	int _copied;

	vBaseItem(char* name);	// Protected makes this a base class
				// prevents instantiation

      private:		//--------------------------------------- private
      };
#endif
