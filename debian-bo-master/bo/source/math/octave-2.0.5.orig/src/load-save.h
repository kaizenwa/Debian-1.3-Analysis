/*

Copyright (C) 1996 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_load_save_h)
#define octave_load_save_h 1

class ostream;

class octave_value;

#include <string>

extern int save_ascii_data (ostream& os, const octave_value& t,
			    const string& name = string (),
			    int strip_nan_and_inf = 0,
			    int mark_as_global = 0, int precision = 0);

extern int save_three_d (ostream& os, const octave_value& t,
			 int parametric = 0);

extern void save_user_variables (void);

extern void symbols_of_load_save (void);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
