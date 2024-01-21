/* ########################################################################

		    SMAC FILE USED BY XCORAL EDITOR

   File: keydef-ext.sc
   Path: /home/c/X11/xcoral-2.2/SmacLib/keydef-ext.sc
   Description: 
   Created: Sun Aug  7 16:08:16 MET 1994
   Author: Thierry Emery
   Modified: Sun Aug  7 16:08:17 MET 1994
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: global set key utilities

   Requires: 

   Defines: globalize_mode, global_key_def

   Suggested bindings: 

   Procedure: call global_key_def instead of key_def to define a key in all
	      `modes'.

   ########################################################################

   Copyright (c) : Thierry Emery

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


/* -------------------------------------------------------------------------
   vector of all mode names
   ------------------------------------------------------------------------- */

char* modes [256];

/* -------------------------------------------------------------------------
   number of modes (size of `modes' vector)
   ------------------------------------------------------------------------- */

int nb_modes=0;

/* -------------------------------------------------------------------------
   make mode <mode_name> global : add it to the `modes' vector
   ------------------------------------------------------------------------- */

void globalize_mode(char* mode_name) {
  
  modes[nb_modes++] = mode_name;
}

/* -------------------------------------------------------------------------
   define key in all `modes'
   ------------------------------------------------------------------------- */

void global_key_def(char* key_name, char* command_name) {
  
  int index;
  
  for (index=0; index<nb_modes; ++index)
    key_def(modes[index],key_name,command_name);
}

