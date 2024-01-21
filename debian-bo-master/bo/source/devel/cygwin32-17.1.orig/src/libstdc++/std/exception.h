// Exception Handling header for -*- C++ -*-
// Copyright (C) 1995 Free Software Foundation

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

#ifndef __EXCEPTION__
#define __EXCEPTION__

#ifdef __GNUG__
#pragma interface "std/exception.h"
#endif

extern "C++" {

class exception {
public:
  typedef void (*raise_handler)(exception&);
  static raise_handler set_raise_handler(raise_handler handler_arg);
  exception (const char *what_arg = ""): desc (what_arg) { }
  exception (const exception& o): desc (o.what ()) { }
  virtual ~exception() { }
  virtual const char* what() const { return desc; }
private:
  const char *desc;
};

class bad_exception : public exception {
public:
  bad_exception () { }
  virtual ~bad_exception() { }
  bad_exception(const char *what_arg): exception (what_arg) { }
};

typedef void (*terminate_handler) ();
typedef void (*unexpected_handler) ();

terminate_handler set_terminate (terminate_handler);
void terminate (void);
unexpected_handler set_unexpected (unexpected_handler);
void unexpected (void);
bool uncaught_exception ();
} // extern "C++"

#endif
