// "area.h" 

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

#ifndef AREA_H
#define AREA_H

#ifndef NO_PRAGMAS
#pragma interface
#endif


// Include Files
#include "utils.h"
#include "coord.h"


// Defines
#define AR_WSQUARES_MAX 20
enum ARshape {AR_RECT,AR_UNDEFINED};
enum ARsig {AR_NO_SIG, AR_CLOSE, AR_FAILURE, AR_BAD_SHAPE};



// Data structures
struct TouchingListItem {
  Loc list[AR_WSQUARES_MAX];
  int num;
};

struct TouchingList {
  TouchingListItem r,dn,l,up; 
};


class Area;
class Avoid;



// Class Declarations
class Area {
 public:
  Size operator - (const Area &) const;
/* EFFECTS: Subtract areas according to their upper left corners. */
  
  Boolean operator == (const Area &) const;
  
  Area() {shape = AR_UNDEFINED;}
  /* EFFECTS: Create undefined area. */

  Area(ARshape c,const Pos &p,const Size &s);
  /* REQUIRES: c is AR_RECT. */
  /* EFFECTS: Creates a rectangle area at position p of size s. */

  Area(ARshape c,const Loc &loc,const Dim &dim);
  /* REQUIRES: c is AR_RECT. */
  /* EFFECTS: Creates a rectangle area at loc and dim. */

  void get_rect(Pos &p,Size &s) const;
  /* MODIFIES: p,s */

  Box get_box() const;
  /* EFFECTS: Returns the box of wsquares that contains the area. */

  void wsquares(Loc list[AR_WSQUARES_MAX],int &nitems) const;
  /* MODIFIES: list, nitems */
  /* EFFECTS: After completion, the first nitems of list contain the locations
     of all the wsquares covered by the area.  The initial values of list 
     and nitems are ignored.  wsquares are not necessarily inside the world. */

  Boolean overlap(const Area &) const;
  /* EFFECTS: Returns True if *this overlaps with r, False otherwise. */

  Boolean overlap(const Loc &loc) const;
  /* EFFECTS: Returns True if loc overlaps with any of the area.  Returns 
     False otherwise. */

  Boolean overlap(const Box &box) const;
  /* EFFECTS: Returns True if box overlaps with any of the area.  Returns 
     False otherwise. */

  Boolean overlap(const Pos &pos) const;

  Loc middle_wsquare() const;
  /* EFFECTS: Returns the location of the middle wsquare in the area. */

  ARsig avoid_wsquare(Avoid &avoid,const Loc &loc) const;
  /* MODIFIES: avoid */
  /* EFFECTS: Returns AR_NO_SIG if the wsquare at loc does not overlap with 
     the area.  If loc overlaps in the area, but not in a nearby area, returns
     AR_CLOSE and sets avoid to be the offsets necessary to make the area no 
     longer touch the wsquare. */

  int avoid_wsquare_dir(const Loc &loc,Dir dir) const;
  /* EFFECTS: Returns the amount *this will have to be shited in dir to avoid 
     loc. */

  Size avoid(const Area &area) const;
  /* EFFECTS: Returns the smallest offset necessary to move *this in order to
     avoid area. */

  Size avoid_no_up(const Area &) const;
  /* EFFECTS: Like avoid(), but will not avoid in the up direction. */

  Pos adjacent_rect(const Size &s,Dir dir) const;
  /* EFFECTS: Return the upper left corner of the rectangle of size s that
     touches *this such that the direction from *this to the rectangle is 
     dir. */

  Pos get_middle() const;
  /* EFFECTS: Returns the middle of the area. */

  void touching_wsquares(TouchingList &list) const;
  /* MODIFIES: list */
  /* EFFECTS: Sets r,dn,l,up to be the lists of all wsquares that are exactly 
     touching the area.  The directions are set from the area to the touching 
     wsquare.  I.e. A loc listed under list.r would be touching the 
     right side of the area.  The list are ordered such that the smallest 
     row and column indexes come first.  I.e. top->bottom and left->right. */

  void shift(const Size &offset)
    {assert(shape == AR_RECT); pos = pos + offset;}
  /* EFFECTS: Shift the area by offset. */

  void set_middle(const Pos &pos);
  /* EFFECTS: Move the area so that pos will be the middle. */

  Dir dir_to(const Pos &pos) const;
  Dir dir_to(const Area &area) const;
  /* EFFECTS: Returns the direction from this to pos or to area. 
     Will return one of {CO_R .. CO_UP_R, CO_air}.  I.e. 8 directions or 
     CO_air. */

  Area combine(const Area &a) const;
  /* EFFECTS: Returns minimum sized rectangular area that contains a and 
     this. */


 private:
  ARshape shape;
  Pos pos;
  Size size;
};



class Avoid {
friend class Area;

public:    
  void maximize(const Avoid &avoid);
  /* EFFECTS: Set all directions of *this to be to max of *this and avoid. */

  Size offset_rank(int rank = 0) const;
  /* REQUIRES: 0 <= rank < 4 */
  /* EFFECTS: Return the offset corresponding to the rankth minimum direction
     of *this using one dimensions.  E.g. rank = 0 gives the minimum.  rank
     = 1 gives the second minimum, etc. */
  
  Size offset_dir(Dir d) const;
  /* REQUIRES: d in {CO_R, CO_DN, CO_L, CO_UP} */
  /* EFFECTS: Return the offset corresponding to d. */

  int get_dir(Dir d) const;
  /* REQUIRES: d in {CO_R, CO_DN, CO_L, CO_UP} */
  /* EFFECTS: Returns the offset in the specified direction. */


private:
  /* Direction from area to wsquare.  All are positive.  r and dn are 
     therefore the negative of the corresponding offsets. */
  int r,dn,l,up; 
};
#endif



