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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "FSQP.h"

#include "defun-dld.h"
#include "error.h"
#include "help.h"
#include "oct-obj.h"

#ifndef FSQP_MISSING

// Global pointers for user defined functions required by fsqp.
// static tree *fsqp_objective;
// static tree *fsqp_constraints;

double
fsqp_objective_function (const ColumnVector&)
{
  return 0.0;
}

ColumnVector
fsqp_constraint_function (const ColumnVector&)
{
  ColumnVector retval;
  return retval;
}

#endif

#if defined (FSQP_MISSING)
DEFUN_DLD (fsqp, , ,
  "This function requires FSQP, which is not freely\n\
redistributable.  For more information, read the file\n\
libcruft/fsqp/README.MISSING in the source distribution.")
#else
DEFUN_DLD (fsqp, , ,
  "[X, PHI] = fsqp (X, PHI [, LB, UB] [, LB, A, UB] [, LB, G, UB])\n\
\n\
Groups of arguments surrounded in `[]' are optional, but\n\
must appear in the same relative order shown above.")
#endif
{
/*

Handle all of the following:

  1. fsqp (x, phi)
  2. fsqp (x, phi, lb, ub)
  3. fsqp (x, phi, lb, ub, llb, c, lub)
  4. fsqp (x, phi, lb, ub, llb, c, lub, nllb, g, nlub)
  5. fsqp (x, phi, lb, ub,              nllb, g, nlub)
  6. fsqp (x, phi,         llb, c, lub, nllb, g, nlub)
  7. fsqp (x, phi,         llb, c, lub)
  8. fsqp (x, phi,                      nllb, g, nlub)

*/

  octave_value_list retval;

  error ("fsqp: not implemented yet");

  return retval;
}

#if defined (FSQP_MISSING)
DEFUN_DLD (fsqp_options, , ,
  "This function requires FSQP, which is not freely\n\
redistributable.  For more information, read the file\n\
libcruft/fsqp/README.MISSING in the source distribution.")
#else
DEFUN_DLD (fsqp_options, , ,
  "fsqp_options (KEYWORD, VALUE)\n\
\n\
Set or show options for fsqp.  Keywords may be abbreviated\n\
to the shortest match.")
#endif
{
  octave_value_list retval;

  error ("fsqp_options: not implemented yet");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
