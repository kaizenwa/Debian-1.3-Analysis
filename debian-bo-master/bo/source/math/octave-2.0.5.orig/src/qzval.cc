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

// Written by A. S. Hodel <scotte@eng.auburn.edu>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "oct-obj.h"

DEFUN_DLD (qzval, args, ,
  "X = qzval (A, B)\n\
\n\
compute generalized eigenvalues of the matrix pencil (A - lambda B).\n\
A and B must be real matrices.")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      octave_value arg_a = args(0);
      octave_value arg_b = args(1);

      Matrix a = arg_a.matrix_value ();
      Matrix b = arg_b.matrix_value ();

      if (! error_state)
	{
	  ComplexColumnVector tmp = Qzval (a, b);

	  if (! error_state)
	    retval = tmp;
	}
    }
  else
    print_usage ("qzval");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
