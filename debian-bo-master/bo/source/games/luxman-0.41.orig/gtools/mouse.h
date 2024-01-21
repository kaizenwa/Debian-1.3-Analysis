/*
   mouse.h

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

#ifndef _mouse_h_
#define _mouse_h_

#include <gtools/bitmap.h>

class Mouse {

 public:
  Mouse();
  ~Mouse();

  int update();
  int x();
  int y();
  int btn();

  int hide();
  int show();

  int ok();
  
 protected:
  int last_x, last_y;
  int drawn;
  int valid;
  
  Bitmap *cursor;
  Bitmap *save;
  
  int erase();
};

#endif

