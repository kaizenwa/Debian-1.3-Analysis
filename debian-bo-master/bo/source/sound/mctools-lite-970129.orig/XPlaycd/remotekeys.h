/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

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

enum {
  RK_EXIT,
  RK_EJECT,
  RK_REW,
  RK_PLAY,
  RK_STOP,
  RK_CUE,
  RK_PAUSE,
  RK_SHUFFLE,
  RK_REPEAT,
  RK_PREV,
  RK_NEXT,
  RK_DISPLAY,
  RK_VOLUP,
  RK_VOLDOWN,
  NUM_KEYSYMS
};

#define RKEYS {\
  "CD.OPERATE",\
  "CD.EJECT",\
  "CD.REW",\
  "CD.PLAY",\
  "CD.STOP",\
  "CD.CUE",\
  "CD.PAUSE",\
  "CD.SHUFFLE",\
  "CD.REPEAT",\
  "CD.PREV",\
  "CD.NEXT",\
  "CD.DISPLAY",\
  "CD.VOLUP",\
  "CD.VOLDOWN",\
}
