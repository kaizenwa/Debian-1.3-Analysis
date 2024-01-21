/*
   button.h

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

#ifndef _button_h_
#define _button_h_

#include <gtools/handle.h>
#include <gtools/screen.h>
#include <gtools/geom.h>
#include <gtools/bitmap.h>
#include <gtools/font.h>

#include <stdio.h>

class CommandButton : public Handler {

 public:
  /* This version is just a white rectangle */
  CommandButton( ExtRect& r, void (*callback_fn)(void*,int), int cmd,
				void *Obj=NULL );
  /*
   * This version uses a bitmap as a background.
   * NOTE!! Bitmap is deleted in destructor.
   */
  CommandButton( int x, int y, Bitmap *Map, void (*callback_fn)(void*,int),
				int cmd, void *Obj=NULL );
  /*
   * This version uses text as a background.
   * NOTE!! The passed font is deleted in destructor.
   */
  CommandButton( ExtRect &r, char *str, Font *font,
				void (*callback_fn)(void*,int), int cmd, void *Obj=NULL );
  
  virtual ~CommandButton();

  int handle_mouse( int x, int y );

 protected:
  void (*callback)(void*,int);
  Rect rect;
  void *obj;
  int cmd;
  Bitmap *map;
  Font *font;
};

class ColorButton : public Handler {
 public:
  ColorButton( ExtRect &r, void (*callback_fn)(void*,int), int fill_color,
			  void *Obj=NULL );
  virtual ~ColorButton();

  int handle_mouse( int x, int y );

 protected:
  void (*callback)(void*,int);
  Rect rect;
  int clr;
  void *obj;
};

#endif	/* _button_h_ */
