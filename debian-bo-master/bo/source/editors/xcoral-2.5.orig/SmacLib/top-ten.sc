/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: top_ten.sc
   Path: /home/c/X11/xcoral-2.19/SmacLib/top_ten.sc
   Description: 
   Created: Sun Jul 31 12:23:25 DST
   Author: Bruno Pages
   Modified: Sun Jul 31 12:23:26 DST
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

void top_ten()
{
  void * f;
  void * ttf[10];
  int ttc[10];
  int i;
  
  for (i = 0; i != 10; i += 1) { ttf[i] = 0; ttc[i] = 0; }
  
  init_function_list();
  while (f = function_list())
    for (i = 0; i != 10; i += 1)
      if (function_percent(f) > ttc[i]) {
        int j;
        
        for (j = 9; j != i; j -= 1) {
          ttf[j] = ttf[j - 1];
          ttc[j] = ttc[j - 1];
        }
        ttc[i] = function_percent(f);
        ttf[i] = f;
        break;
      }
  
  for (i = 0; (i != 10) && ttf[i]; i += 1) {
    char * name = function_name(ttf[i]);
    
    printf("%s : %d%%\n", name, ttc[i]);
    free(name);
  }
}
