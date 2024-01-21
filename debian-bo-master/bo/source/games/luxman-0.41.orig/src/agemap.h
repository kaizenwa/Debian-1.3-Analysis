/*
   agemap.h

   This file is part of LuxMan.
   
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

#ifndef _agemap_h_
#define _agemap_h_

class AgeMap {

 public:
  AgeMap( int W, int H );
  ~AgeMap();

  /*
   * Do not pass invalid coordinates to these!
   * (For efficiency, there is no checking)
   */
  int set( int x, int y, int val );
  int get( int x, int y );

  int W();
  int H();
  
  /* Set all ages to 0 except age `except' which is left unchanged */
  void clear( int except );

#ifdef DEBUG
  void verify_magic();
#endif

 protected:
  int *rofs;

  int w, h;
  int *map;
  int map_bytes;		/* W * H * sizeof(int) */

};  

#endif
