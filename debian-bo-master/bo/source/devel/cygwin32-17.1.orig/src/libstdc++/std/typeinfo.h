// RTTI support for -*- C++ -*-
// Copyright (C) 1994, 1995 Free Software Foundation

// This file is part of the GNU ANSI C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

// As a special exception, if you link this library with files
// compiled with a GNU compiler to produce an executable, this does not cause
// the resulting executable to be covered by the GNU General Public License.
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.

// Written by Kung Hsu based upon the specification in the 20 September 1994
// C++ working paper, ANSI document X3J16/94-0158.
// Rewritten by Jason Merrill.

#ifndef __TYPEINFO__
#define __TYPEINFO__

#ifdef __GNUG__
#pragma interface "std/typeinfo.h"
#endif

#include <exception>
#include <stdexcept>

extern "C++" {

class type_info {
private:
  // assigning type_info is not supported.  made private.
  type_info& operator= (const type_info&);
  type_info (const type_info&);

protected:
  type_info (const char *n): _name (n) { }

  const char *_name;

public:
  // destructor
  virtual ~type_info () {}
    
  bool before (const type_info& arg);
  const char* name () const
    { return _name; }
  bool operator== (const type_info& arg) const 
    { return &arg == this; }
  bool operator!= (const type_info& arg) const 
    { return &arg != this; }
};

class bad_cast : public logic_error {
public:
  bad_cast(const char *what_arg): logic_error (what_arg) { }
  virtual ~bad_cast() { }
};

class bad_typeid : public logic_error {
 public:
  bad_typeid (): logic_error ("bad_typeid") { }
  virtual ~bad_typeid () { }
};

} // extern "C++"

#endif
