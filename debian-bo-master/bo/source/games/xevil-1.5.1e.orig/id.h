// "id.h" Object locator id.

/*    Copyright (C) 1994,1995,1996  Steve Hardt

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    Steve Hardt 
    hardts@mit.edu (valid until Nov. 1996)
    hardts@netscape.com
    hardts@alum.mit.edu
    http://graphics.lcs.mit.edu/~hardts/xevil.html
*/

#ifndef ID_H
#define ID_H

// No '#pragma interface' because there is no "id.cc".

class Locator;
class Identifier {
  friend class Locator;

 public:
  Identifier() {index = INVALID;}
  Boolean operator == (const Identifier &other) const
  {return (other.index == index) && (other.unique == unique);}
  Boolean operator != (const Identifier &other) const
  {return (other.index != index) || (other.unique != unique);}
  
 private:  
  enum {INVALID = -1};
  int index; 
  int unique;
};


class Id: public Identifier {
public:
  Id() {};
};


class IntelId: public Identifier {
public:
  IntelId() {};
};
#endif
