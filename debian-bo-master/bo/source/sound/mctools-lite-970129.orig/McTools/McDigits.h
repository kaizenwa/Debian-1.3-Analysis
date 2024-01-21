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

#ifndef _McDigits_h_
#define _McDigits_h_

#define DIG_START		0x1000
#define DIG_LEFT_ARROW		0x1000
#define DIG_RIGHT_ARROW		0x1001
#define DIG_LEFT_FILLED_ARROW	0x1002
#define DIG_RIGHT_FILLED_ARROW	0x1003

extern void McPutNumber(McWindow *mcw, Window win, GC gc, int n, int x, int y);
extern void McSetupDigits(McWindow *mcw);

#endif /* _McDigits_h_ */
