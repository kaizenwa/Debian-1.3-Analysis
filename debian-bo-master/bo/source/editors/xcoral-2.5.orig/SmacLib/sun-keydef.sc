/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: sun_keydef.sc
   Path: /home/c/X11/xcoral-2.19/SmacLib/sun_keydef.sc
   Description: 
   Created: Sun Jul 31 12:05:48 DST
   Author: Bruno Pages
   Modified: Sun Jul 31 12:05:49 DST
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Bruno Pages

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

   ######################################################################## */

void sun_keydef(char * mode)
{
  key_def(mode, "L1", "abort");
  key_def(mode, "L7", "new_window");
  key_def(mode, "L9", "forward_search");
  key_def(mode, "L4", "undo");
  key_def(mode, "L6", "copy_region");
  key_def(mode, "L8", "paste_region");
  key_def(mode, "L10", "kill_region");
}

{
  sun_keydef("default");
  sun_keydef("C-mode");
  sun_keydef("C++mode");
  sun_keydef("Latex");
}
