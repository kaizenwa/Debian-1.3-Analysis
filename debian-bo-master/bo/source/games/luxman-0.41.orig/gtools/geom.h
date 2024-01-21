/*
   geom.h

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

#ifndef _geom_h_
#define _geom_h_

struct Point {
  int x, y;

  Point();
  Point( int X, int Y );
  Point( Point &p );

  void set( Point& );  
};

struct ExtRect {
  int x, y, w, h;

  ExtRect( int X, int Y, int W, int H );
};  

struct Rect {
  Point a;
  Point b;
  
  Rect();
  Rect( int x1, int y1, int x2, int y2 );
  Rect( Point& p1, Point &p2 );
  Rect& operator=( ExtRect & );

  int w();
  int h();
  
  void set( Rect& r );
};

#endif
