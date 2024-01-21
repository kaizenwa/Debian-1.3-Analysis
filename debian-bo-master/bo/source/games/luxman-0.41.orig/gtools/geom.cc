/*
   geom.cc

   This file is part of libgtools.
   
   Copyright (C) 1994,1995 Frank McIngvale (frankm@nuance.com)
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <gtools/geom.h>

Point::Point()
{
  x = y = 0;
}

Point::Point( int X, int Y )
{
  x = X;
  y = Y;
}

Point::Point( Point &p )
{
  x = p.x;
  y = p.y;
}

void Point::set( Point& p )
{
  x = p.x;
  y = p.y;
}

Rect::Rect()
{
  a.x = a.y = b.x = b.y = 0;
}

Rect::Rect( int x1, int y1, int x2, int y2 )
{
  a.x = x1;
  a.y = y1;
  b.x = x2;
  b.y = y2;
}

Rect::Rect( Point& p1, Point& p2 )
{
  a.x = p1.x;
  a.y = p1.y;
  b.x = p2.x;
  b.y = p2.y;
}

Rect& Rect::operator=( ExtRect& er )
{
  a.x = er.x;
  a.y = er.y;
  b.x = er.x + er.w - 1;
  b.y = er.y + er.h - 1;

  return *this;
}

int Rect::w()
{
  return b.x - a.x;
}

int Rect::h()
{
  return b.y - a.y;
}

void Rect::set( Rect &r )
{
  a.x = r.a.x;
  a.y = r.a.y;
  b.x = r.b.x;
  b.y = r.b.y;
}

ExtRect::ExtRect( int X, int Y, int W, int H )
{
  x = X;
  y = Y;
  w = W;
  h = H;
}

