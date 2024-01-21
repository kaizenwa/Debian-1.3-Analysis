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

#if !defined (octave_tree_mat_h)
#define octave_tree_mat_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

class octave_value;
class tree_return_list;

class tree_walker;

#include <SLList.h>

#include "pt-exp.h"

// General matrices.  This allows us to construct matrices from
// other matrices, variables, and functions.

class
tree_matrix_row : public SLList<tree_expression *>
{
public:

  tree_matrix_row (tree_expression *e = 0) : SLList<tree_expression *> ()
    {
      if (e)
	append (e);
    }

  ~tree_matrix_row (void) { }

  bool all_elements_are_constant (void) const;

  tree_return_list *to_return_list (void);

  void accept (tree_walker& tw);
};

class
tree_matrix : public tree_expression, public SLList<tree_matrix_row *>
{
public:

  tree_matrix (tree_matrix_row *mr = 0)
    : tree_expression (), SLList<tree_matrix_row *> ()
      {
	if (mr)
	  append (mr);
      }

  ~tree_matrix (void) { }

  bool all_elements_are_constant (void) const;

  octave_value eval (bool print);

  void accept (tree_walker& tw);
};

extern void symbols_of_pt_mat (void);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
