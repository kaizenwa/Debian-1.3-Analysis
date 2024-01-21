/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; see the file COPYING.LIB.  If
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
*/

#ifndef _McColors_h_
#define _McColors_h_

/* These are the default colors, if nothing is found in the resourceDB */
#define COLORS { "gray80","black","white","red","green","blue","yellow",\
		 "gray94", "gray40", "gray72", "turquoise4", "bisque", \
		 "black" }

enum {
  COL_BACKGROUND,
  COL_FOREGROUND,
  COL_WHITE,
  COL_RED,
  COL_GREEN,
  COL_BLUE,
  COL_YELLOW,
  COL_BRIGHT,
  COL_DARK,
  COL_SELECTED,
  COL_MENU,
  COL_TIP,
  COL_FOCUS,

  MAX_COLORS
};

#endif /* _McColors_h_ */
