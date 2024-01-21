/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

  McUtils.h

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

#ifndef _McUtils_h_
#define _McUtils_h_

extern McGadget *MakeButton(McWindow *mcw, int x, int y, int w, int h,
			    int id, string_t label,
			    void (*callback)(struct McGadget *));

extern McGadget *MakeMessage(McWindow *mcw, int x, int y, int w, int h,
			     int id, string_t label);

struct McLine;
extern void MakeTexts(McWindow *mcw, int x, int y,
		      struct McLine *line, int lines);
extern McGadget *MakeText(McWindow *mcw, int x, int y, int id, string_t label);
extern McGadget *MakeRText(McWindow *mcw, int x, int y, int id,string_t label);

#endif /* _McUtils_h_ */
