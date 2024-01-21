// "area.C"  
// Also has class Avoid.

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

#ifndef NO_PRAGMAS
#pragma implementation "area.h"
#endif


// Include Files
extern "C" {
#include <stdlib.h> // For the abs() function.
#include <limits.h> // For INT_MAX
}
#include <iostream.h>
#include "utils.h"
#include "coord.h"
#include "area.h"



Size Area::operator - (const Area &other) const
{
  assert ((shape == AR_RECT) && (other.shape == AR_RECT));
  return pos - other.pos;
}



Boolean Area::operator == (const Area &other) const 
{
  assert(shape == AR_RECT && other.shape == AR_RECT);
  
  return pos == other.pos && size == other.size;
}



Area::Area(ARshape sh,const Pos &p,const Size &s)
{
  assert(sh == AR_RECT);
  
  shape = AR_RECT;
  pos = p;
  size = s;
}



Area::Area(ARshape sh,const Loc &loc,const Dim &dim)
{
  assert(sh == AR_RECT);
  
  shape = AR_RECT;
  pos.x = loc.c * WSQUARE_WIDTH;
  pos.y = loc.r * WSQUARE_HEIGHT;
  size.width = dim.colMax * WSQUARE_WIDTH;
  size.height = dim.rowMax * WSQUARE_HEIGHT;
}



void Area::get_rect(Pos &p,Size &s) const
{
  assert (shape == AR_RECT);
  p = pos;
  s = size;
}



Box Area::get_box() const
{
  assert (shape == AR_RECT);

  Loc loc;
  loc.r = (int)floor(pos.y * WSQUARE_HEIGHT_INV);
  loc.c = (int)floor(pos.x * WSQUARE_WIDTH_INV);

  // Always has dimension of at least 1x1.
  Loc finish;
  finish.r = (int)ceil((pos.y + size.height) * WSQUARE_HEIGHT_INV);
  finish.c = (int)ceil((pos.x + size.width) * WSQUARE_WIDTH_INV);
  
  Dim dim(finish.r - loc.r,finish.c - loc.c);
  
  Box box(loc,dim);
  return box;
}



void Area::wsquares(Loc list[AR_WSQUARES_MAX],int &nitems) const
{
  nitems = 0;

  assert(shape == AR_RECT);
  Loc loc;
  for (loc.r = (int)floor(pos.y * WSQUARE_HEIGHT_INV);
       loc.r * WSQUARE_HEIGHT < pos.y + size.height;
       loc.r++)
    for (loc.c = (int)floor(pos.x * WSQUARE_WIDTH_INV);
	 loc.c * WSQUARE_WIDTH < pos.x + size.width;
	 loc.c++)
      {
	if (nitems >= AR_WSQUARES_MAX)
	  {
//	    cerr << "Warning: Area:wsquares: Too many wsquares." << endl;
	    return;
	  }
	list[nitems] = loc;
	nitems++;
      }
}



Boolean Area::overlap(const Area &r) const
{
  assert((shape == AR_RECT) && (r.shape == AR_RECT));

  if ((pos.x + size.width <= r.pos.x) ||
      (r.pos.x + r.size.width <= pos.x) ||
      (pos.y + size.height <= r.pos.y) ||
      (r.pos.y + r.size.height <= pos.y))
    return False;
  else
    return True;
}



Boolean Area::overlap(const Loc &loc) const
{
  Pos test;
  Boolean ret;

  assert(shape == AR_RECT);
  
  test.x = loc.c * WSQUARE_WIDTH;
  test.y = loc.r * WSQUARE_HEIGHT;

  if ((test.x >= pos.x + size.width) || (test.y >= pos.y + size.height) ||
      (test.x + WSQUARE_WIDTH <= pos.x) || (test.y + WSQUARE_HEIGHT <= pos.y))
    ret = False;
  else
    ret = True;

  return ret;
}



Boolean Area::overlap(const Box &box) const
{
  Pos testPos;
  Size testSize;
  Boolean ret;

  assert(shape == AR_RECT);
  
  testPos.x = box.loc.c * WSQUARE_WIDTH;
  testPos.y = box.loc.r * WSQUARE_HEIGHT;
  testSize.width = box.dim.colMax * WSQUARE_WIDTH;
  testSize.height = box.dim.rowMax * WSQUARE_HEIGHT;

  if ((testPos.x >= pos.x + size.width) || 
      (testPos.y >= pos.y + size.height) ||
      (testPos.x + testSize.width <= pos.x) || 
      (testPos.y + testSize.height <= pos.y))
    ret = False;
  else
    ret = True;
  
  return ret;
}



Boolean Area::overlap(const Pos &p) const
{
  assert(shape == AR_RECT);

  if ((pos.x + size.width <= p.x) ||
      (pos.x > p.x) ||
      (pos.y + size.height <= p.y) ||
      (pos.y > p.y))
      return False;
  else
    return True;
}



Loc Area::middle_wsquare() const
{
  assert (shape == AR_RECT);

  Loc ret;
  ret.r = (int)floor((pos.y + size.height * 0.5) * WSQUARE_HEIGHT_INV);
  ret.c = (int)floor((pos.x + size.width * 0.5) * WSQUARE_WIDTH_INV);
  return ret;
}



ARsig Area::avoid_wsquare(Avoid &avoid,const Loc &loc) const
{
  assert(shape == AR_RECT);
  
  if (! overlap(loc))
    return AR_NO_SIG;

  avoid.r = -(loc.c * WSQUARE_WIDTH - size.width - pos.x);
  avoid.dn = -(loc.r * WSQUARE_HEIGHT - size.height - pos.y);
  avoid.l = (loc.c + 1) * WSQUARE_WIDTH - pos.x;
  avoid.up = (loc.r + 1) * WSQUARE_HEIGHT - pos.y;
  
  return AR_CLOSE;
}



int Area::avoid_wsquare_dir(const Loc &loc,Dir dir) const
{
  assert(shape == AR_RECT);
  assert(overlap(loc));

  switch (dir) {
  case CO_R:
    return (loc.c + 1) * WSQUARE_WIDTH - pos.x;
  case CO_DN:
    return (loc.r + 1) * WSQUARE_HEIGHT - pos.y;
  case CO_L:
    return -(loc.c * WSQUARE_WIDTH - size.width - pos.x);
  case CO_UP:
    return -(loc.r * WSQUARE_HEIGHT - size.height - pos.y);
  };

  assert(0);
  return CO_air;
}



Size Area::avoid(const Area &other) const
{
  assert((shape == AR_RECT) && (other.shape == AR_RECT));

  Avoid avoid;
  avoid.r = -(other.pos.x  - size.width - pos.x);
  avoid.dn = -(other.pos.y - size.height - pos.y);
  avoid.l = other.pos.x + other.size.width - pos.x;
  avoid.up = other.pos.y + other.size.height - pos.y;

  return avoid.offset_rank();
}



Size Area::avoid_no_up(const Area &other) const
{
  assert((shape == AR_RECT) && (other.shape == AR_RECT));

  Avoid avoid;
  avoid.r = -(other.pos.x  - size.width - pos.x);
  avoid.dn = INT_MAX;
  avoid.l = other.pos.x + other.size.width - pos.x;
  avoid.up = other.pos.y + other.size.height - pos.y;

  return avoid.offset_rank();
}



Pos Area::adjacent_rect(const Size &otherSize,Dir dir) const
{
  assert (shape == AR_RECT);

  Pos middle = get_middle();
  Pos ret;

  switch (dir) {
  case CO_UP_R_R:
  case CO_R:
  case CO_DN_R_R:
    ret.x = pos.x + size.width;
    ret.y = middle.y - (int)(0.5 * otherSize.height);
    break;
  case CO_DN_R:
    ret = pos + size;
    break;
  case CO_DN_DN_R:
  case CO_DN:
  case CO_DN_DN_L:
    ret.x = middle.x - (int)(0.5 * otherSize.width);
    ret.y = pos.y + size.height;
    break;
  case CO_DN_L:
    ret.x = pos.x - otherSize.width;
    ret.y = pos.y + size.height;
    break;
  case CO_DN_L_L:
  case CO_L:
  case CO_UP_L_L:
    ret.x = pos.x - otherSize.width;
    ret.y = middle.y - (int)(0.5 * otherSize.height);
    break;
  case CO_UP_L:
    ret = pos - otherSize;
    break;
  case CO_UP_UP_L:
  case CO_UP:
  case CO_UP_UP_R:
    ret.x = middle.x - (int)(0.5 * otherSize.width);
    ret.y = pos.y - otherSize.height;
    break;
  case CO_UP_R:
    ret.x = pos.x + size.width;
    ret.y = pos.y - otherSize.height;
    break;
  default: 
    assert(0);
    break;
  };

  return ret;
}



Pos Area::get_middle() const
{
  assert(shape == AR_RECT);

  Pos ret(pos.x + (int)floor(size.width * 0.5),
	  pos.y + (int)floor(size.height * 0.5));
  return ret;
}



void Area::touching_wsquares(TouchingList &list) const
{
  assert(shape == AR_RECT);
  
  list.r.num = list.dn.num = list.l.num = list.up.num = 0;
  
  // Right side.
  if ((pos.x + size.width) % WSQUARE_WIDTH == 0)
    {
      Loc loc;
      loc.c = (pos.x + size.width) / WSQUARE_WIDTH;
      
      for (loc.r = (int)floor(pos.y * WSQUARE_HEIGHT_INV);
	   loc.r * WSQUARE_HEIGHT < pos.y + size.height;
	   loc.r++)
	{
	  list.r.list[list.r.num] = loc;
	  list.r.num++;
	}
    }

  // Bottom.
  if ((pos.y + size.height) % WSQUARE_HEIGHT == 0)
    {
      Loc loc;
      loc.r = (pos.y + size.height) / WSQUARE_HEIGHT;
      
      for (loc.c = (int)floor(pos.x * WSQUARE_WIDTH_INV);
	   loc.c * WSQUARE_WIDTH < pos.x + size.width;
	   loc.c++)
	{
	  list.dn.list[list.dn.num] = loc;
	  list.dn.num++;
	}
    }

  // Left side.
  if (pos.x % WSQUARE_WIDTH == 0)
    {
      Loc loc;
      loc.c = pos.x / WSQUARE_WIDTH - 1;
      
      for (loc.r = (int)floor(pos.y * WSQUARE_HEIGHT_INV);
	   loc.r * WSQUARE_HEIGHT < pos.y + size.height;
	   loc.r++)
	{
	  list.l.list[list.l.num] = loc;
	  list.l.num++;
	}
    }

  // Top.
  if (pos.y % WSQUARE_HEIGHT == 0)
    {
      Loc loc;
      loc.r = pos.y / WSQUARE_HEIGHT - 1;
      
      for (loc.c = (int)floor(pos.x * WSQUARE_WIDTH_INV);
	   loc.c * WSQUARE_WIDTH < pos.x + size.width;
	   loc.c++)
	{
	  list.up.list[list.up.num] = loc;
	  list.up.num++;
	}
    }
}



void Area::set_middle(const Pos &mid)
{
  assert(shape == AR_RECT);

  pos.x = mid.x - (int)floor(size.width * 0.5);
  pos.y = mid.y - (int)floor(size.height * 0.5);
}



Dir Area::dir_to(const Pos &other) const
{
  assert(shape == AR_RECT);

  int xdir,ydir;

  if (other.x >= pos.x + size.width)
    xdir = 1;
  else if (other.x < pos.x)
    xdir = -1;
  else
    xdir = 0;

  if (other.y >= pos.y + size.height)
    ydir = 1;
  else if (other.y < pos.y)
    ydir = -1;
  else
    ydir = 0;

  switch (xdir) {
  case 1:
    switch (ydir) {
    case -1:
      return CO_UP_R;
    case 0:
      return CO_R;
    case 1:
      return CO_DN_R;
    };
  case 0:
    switch (ydir) {
    case -1:
      return CO_UP;
    case 0:
      return CO_air;
    case 1:
      return CO_DN;
    };
  case -1:
    switch (ydir) {
    case -1:
      return CO_UP_L;
    case 0:
      return CO_L;
    case 1:
      return CO_DN_L;
    };
  };

  assert(0);
  return CO_air;
}



Dir Area::dir_to(const Area &other) const
{
  assert((shape == AR_RECT) && (other.shape == AR_RECT));

  int xdir,ydir;

  if (other.pos.x >= pos.x + size.width)
    xdir = 1;
  else if (other.pos.x + other.size.width <= pos.x)
    xdir = -1;
  else
    xdir = 0;

  if (other.pos.y >= pos.y + size.height)
    ydir = 1;
  else if (other.pos.y + other.size.height <= pos.y)
    ydir = -1;
  else
    ydir = 0;

  switch (xdir) {
  case 1:
    switch (ydir) {
    case -1:
      return CO_UP_R;
    case 0:
      return CO_R;
    case 1:
      return CO_DN_R;
    };
  case 0:
    switch (ydir) {
    case -1:
      return CO_UP;
    case 0:
      return CO_air;
    case 1:
      return CO_DN;
    };
  case -1:
    switch (ydir) {
    case -1:
      return CO_UP_L;
    case 0:
      return CO_L;
    case 1:
      return CO_DN_L;
    };
  };

  assert(0);
  return CO_air;
}



Area Area::combine(const Area &a) const
{
  Pos pos1;
  pos1.x = min(pos.x,a.pos.x);
  pos1.y = min(pos.y,a.pos.y);

  Pos pos2;
  pos2.x = max(pos.x + size.width,a.pos.x + a.size.width);
  pos2.y = max(pos.y + size.height,a.pos.y + a.size.height);

  Size s = pos2 - pos1;

  Area ret(AR_RECT,pos1,s);
  return ret;
}




/////////////////////////////// Avoid ///////////////////////////////////////
void Avoid::maximize(const Avoid &avoid)
{
  r = max(r,avoid.r);
  dn = max(dn,avoid.dn);
  l = max(l,avoid.l);
  up = max(up,avoid.up);
}



Size Avoid::offset_rank(int rank) const
{
  assert ((rank >= 0) && (rank < 4));

  int dists[4];
  dists[0] = r;
  dists[1] = dn;
  dists[2] = l;
  dists[3] = up;
  Utils::insertionSort(dists,4);
  int m = dists[rank];      
  
  Size ret;
  if (m == r)
    {
      ret.width = -r;
      ret.height = 0;
      return ret;
    }
  if (m == dn)
    {
      ret.width = 0;
      ret.height = -dn;
      return ret;
    }
  if (m == l)
    {
      ret.width = l;
      ret.height = 0;
      return ret;
    }
  if (m == up)
    {
      ret.width = 0;
      ret.height = up;
      return ret;
    }
  assert(0);
  ret.width = ret.height = 0;
  return ret;
}



Size Avoid::offset_dir(Dir dir) const
{
  Size ret;
  switch(dir)
    {
    case CO_R:
      ret.width = -r;
      ret.height = 0;
      break;
    case CO_DN:
      ret.width = 0;
      ret.height = -dn;
      break;
    case CO_L:
      ret.width = l;
      ret.height = 0;
      break;
    case CO_UP:
      ret.width = 0;
      ret.height = up;
      break;
    default:
      assert(0);
    };
  return ret;
}



int Avoid::get_dir(Dir dir) const
{
  switch (dir) {
  case CO_R:
    return r;
  case CO_DN:
    return dn;
  case CO_L:
    return l;
  case CO_UP:
    return up;
  };
  assert(0);
  return CO_air;
}


