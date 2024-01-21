// "coord.C"

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
#pragma implementation "coord.h"
#endif


// Include Files
#include "utils.h"
#include "coord.h"
#include "area.h"



// Functions.
void Stats::add_death(time_t birthTime)
{
  if (enabled) {
    time_t lifespan = time(NULL) - birthTime;
    
    if (deaths > 0)
      aveLifespan = (aveLifespan * deaths + lifespan) / (deaths + 1);
    else
      aveLifespan = lifespan;
    
    deaths++;
  }
}



Boolean Stats::enabled = False;



int Pos::distance(const Pos &p) const
{
  Size diff = *this - p;
  assert (diff.abs_2() >= 0);
  
  return (int)sqrt(diff.width * diff.width + diff.height * diff.height);
}



int Pos::distance_2(const Pos &p) const
{
  Size diff = *this - p;
  
  return diff.width * diff.width + diff.height * diff.height;
}



Dir Size::get_dir()
{
  if (!width && !height)
    return CO_air;

  if (height < 0.5 * width)
    if (height > -2 * width)
      if (height < -0.5 * width)
	return CO_UP_R;
      else
	return CO_R;
    else
      if (height < 2 * width)
	return CO_UP;
      else
	return CO_UP_L;
  else
    if (height < -2 * width)
      if (height < -0.5 * width)
	return CO_L;
      else
	return CO_DN_L;
    else
      if (height < 2 * width)
	return CO_DN_R;
      else
	return CO_DN;
}



void Size::get_dirs_4(Dir &d1,Dir &d2)
{
  if (!width && !height)
    {
      d1 = d2 = CO_air;
      return;
    }

  if (width)
    d1 = (width > 0) ? CO_R : CO_L;
  
  if (height)
    d2 = (height > 0) ? CO_DN : CO_UP;
  else
    d2 = d1;

  if (!width)
    d1 = d2;
}



float Size::cross(const Vel &vel)
{
  return width * vel.dy - height * vel.dx;
}



Boolean Box::overlap(const Loc &l)
{
  if ((l.c >= loc.c) && (l.c < loc.c + dim.colMax) &&
      (l.r >= loc.r) && (l.r < loc.r + dim.rowMax))
    return True;
  else
    return False;
}



Vel Vel::shrink(float k) const
{
  Vel ret = *this;

  if (ret.dx > k)
    ret.dx -= k;
  else if (ret.dx < -k)
    ret.dx += k;
  else
    ret.dx = 0;

  if (ret.dy > k)
    ret.dy -= k;
  else if (ret.dy < -k)
    ret.dy += k;
  else
    ret.dy = 0;

  return ret;
}



void Vel::damp(float k)
{
  if (dx > k)
    dx -= k;
  else if (dx < -k)
    dx += k;
  else
    dx = 0;

  if (dy > k)
    dy -= k;
  else if (dy < -k)
    dy += k;
  else
    dy = 0;
}



Boolean Vel::is_zero() const
{
  Boolean ret;
  if ((dx == 0) && (dy == 0))
    ret = True;
  else
    ret = False;
  return ret;
}



Dir Vel::get_dir() const
{
  if (!dx && !dy)
    return CO_air;

  if (dy < 0.5 * dx)
    if (dy > -2 * dx)
      if (dy < -0.5 * dx)
	return CO_UP_R;
      else
	return CO_R;
    else
      if (dy < 2 * dx)
	return CO_UP;
      else
	return CO_UP_L;
  else
    if (dy < -2 * dx)
      if (dy < -0.5 * dx)
	return CO_L;
      else
	return CO_DN_L;
    else
      if (dy < 2 * dx)
	return CO_DN_R;
      else
	return CO_DN;
}



void Vel::limit(float k)
{
  assert (k >= 0);

  if (dx > k)
    dx = k;
  if (dx < -k)
    dx = -k;
  if (dy > k)
    dy = k;
  if (dy < -k)
    dy = -k;
}



void Vel::get_dirs_4(Dir in[4],Dir out[4],int &inNum,int &outNum)
{
  inNum = 0;
  outNum = 0;

  if (dx > 0)
    {
      in[inNum] = CO_R;
      inNum++;
    }
  else
    {
      out[outNum] = CO_R;
      outNum++;
    }

  if (dy > 0)
    {
      in[inNum] = CO_DN;
      inNum++;
    }
  else
    {
      out[outNum] = CO_DN;
      outNum++;
    }

  if (dx < 0)
    {
      in[inNum] = CO_L;
      inNum++;
    }
  else
    {
      out[outNum] = CO_L;
      outNum++;
    }

  if (dy < 0)
    {
      in[inNum] = CO_UP;
      inNum++;
    }
  else
    {
      out[outNum] = CO_UP;
      outNum++;
    }

  assert(inNum + outNum == 4);
}



Boolean operator == (const Loc &l1, const Loc &l2)
{
  return l1.r == l2.r && l1.c == l2.c;
}



Boolean operator == (const Pos &p1, const Pos &p2)
{
  return p1.x == p2.x && p1.y == p2.y;
}



Boolean operator == (const Vel &v1, const Vel &v2)
{
  return v1.dx == v2.dx && v1.dy == v2.dy;
}



Boolean operator == (const Size &s1, const Size &s2)
{
  if ((s1.width == s2.width) && (s1.height == s2.height))
    return True;
  else
    return False;
}



Boolean operator == (const GLoc &g1,const GLoc &g2)
{
  return (g1.horiz == g2.horiz) && (g1.vert == g2.vert);
}



Boolean operator != (const GLoc &g1,const GLoc &g2)
{
  return (g1.horiz != g2.horiz) || (g1.vert != g2.vert);
}



 Pos operator + (const Pos &pos,const Size &size)
{
  Pos ret;
  ret.x = pos.x + size.width;
  ret.y = pos.y + size.height;
  return ret;
}



 Pos operator - (const Pos &pos,const Size &size)
{
  Pos ret;
  ret.x = pos.x - size.width;
  ret.y = pos.y - size.height;
  return ret;
}



Size operator - (const Pos &p1,const Pos &p2)
{
  Size ret;
  ret.width = p1.x - p2.x;
  ret.height = p1.y - p2.y;
  return ret;
}



Pos operator + (const Pos &pos, const Vel &vel)
{
  Pos ret;
  
  /* We want it to round towards zero so that it does the same thing for 
     something going to the right as to the left. */
  ret.x = pos.x + (int)vel.dx;
  ret.y = pos.y + (int)vel.dy;
  /*  ret.x = pos.x + (int)trunc(vel.dx);
      ret.y = pos.y + (int)trunc(vel.dy);
      */
  return ret;
}

     

Size operator * (float k,const Size &size)
{
  Size ret;
  ret.width = (int)floor(k * size.width);
  ret.height = (int)floor(k * size.height);
  return ret;
}



Vel operator + (const Vel &v1,const Vel &v2)
{
  Vel ret(v1.dx + v2.dx,v1.dy + v2.dy);
  return ret;
}



Vel operator + (const Vel &vel, const Acc &acc)
{
  Vel ret(vel.dx + acc.ddx,vel.dy + acc.ddy);
  return ret;
}

     

Vel operator * (float k,const Vel &vel)
{
  Vel ret(k * vel.dx, k * vel.dy);
  return ret;
}



Vel operator / (float k,const Vel &vel)
{
  Vel ret(k / vel.dx, k / vel.dy);
  return ret;
}



Vel operator + (float k,const Vel &vel)
{
  Vel ret(k + vel.dx,k + vel.dy);
  return ret;
}



Acc operator * (int k,const Acc &acc)
{
  Acc ret;
  ret.ddx = k * acc.ddx;
  ret.ddy = k * acc.ddy;
  return ret;
}



Dir Coord::dir_opposite(Dir dir)
{
  if (dir == CO_air)
    return CO_air;

  assert(dir >= CO_R && dir < CO_DIR_MAX);

  return ((dir - CO_R + CO_DIR_PURE / 2) % CO_DIR_PURE) + CO_R;
}



Dir Coord::movement_dir_4(Dir dir)
{
  switch(dir) {
  case CO_center:
  case CO_air:
  case CO_climb:
  case CO_r:
  case CO_dn:
  case CO_l:
  case CO_up:
    return CO_air;

  case CO_center_R:
  case CO_dn_R:
  case CO_up_R:
  case CO_air_R:
  case CO_climb_R:
  case CO_UP_R:
  case CO_UP_R_R:
  case CO_R:
  case CO_DN_R_R:
  case CO_DN_R:
    return CO_R;

  case CO_air_DN:
  case CO_r_DN:
  case CO_l_DN:
  case CO_climb_DN:
  case CO_DN_DN_R:
  case CO_DN:
  case CO_DN_DN_L:
    return CO_DN;
    
  case CO_center_L:
  case CO_dn_L:
  case CO_up_L:
  case CO_air_L:
  case CO_climb_L:
  case CO_UP_L:
  case CO_UP_L_L:
  case CO_L:
  case CO_DN_L_L:
  case CO_DN_L:
    return CO_L;

  case CO_air_UP:
  case CO_r_UP:
  case CO_l_UP:
  case CO_climb_UP:
  case CO_UP_UP_R:
  case CO_UP:
  case CO_UP_UP_L:
    return CO_UP;

  default:
    assert(0);
    return CO_air;
  }
}



Pos Coord::shot_initial_pos(const Area &area,
			  const Size &shotSize,Dir shotDir)
{
  // Return value;
  Pos ret;

  Pos pos;
  Size size;
  area.get_rect(pos,size);
  
  // Set x coord.
  switch (shotDir) {
  case CO_UP_R:
  case CO_UP_R_R:
  case CO_R:
  case CO_DN_R_R:
  case CO_DN_R:
    if (shotSize.width <= size.width)
      ret.x = pos.x + size.width - shotSize.width;
    else
      ret.x = pos.x;
    break;

  case CO_UP_L:
  case CO_UP_L_L:
  case CO_L:
  case CO_DN_L_L:
  case CO_DN_L:
    if (shotSize.width <= size.width)
      ret.x = pos.x;
    else
      ret.x = pos.x + size.width - shotSize.width;
    break;
    
  case CO_UP_UP_L:
  case CO_UP:
  case CO_UP_UP_R:
  case CO_DN_DN_L:
  case CO_DN:
  case CO_DN_DN_R:
  case CO_air:
    ret.x = (int)(pos.x + size.width * .5 - shotSize.width * .5);
    break;
    
  default:
    assert(0);
  }

  // Set y coord.
  switch (shotDir) {
  case CO_DN_L:
  case CO_DN_DN_L:
  case CO_DN:
  case CO_DN_DN_R:
  case CO_DN_R:
    if (shotSize.height <= size.height)
      ret.y = pos.y + size.height - shotSize.height;
    else
      ret.y = pos.y;
    break;
    
  case CO_UP_L:
  case CO_UP_UP_L:
  case CO_UP:
  case CO_UP_UP_R:
  case CO_UP_R:
    if (shotSize.height <= size.height)
      ret.y = pos.y;
    else
      ret.y = pos.y + size.height - shotSize.height;
    break;

  case CO_UP_R_R:
  case CO_R:
  case CO_DN_R_R:
  case CO_UP_L_L:
  case CO_L:
  case CO_DN_L_L:
  case CO_air:
    ret.y = (int)(pos.y + size.height * .5 - shotSize.height * .5);
    break;
    
  default:
    assert(0);
  }

  return ret;
}
