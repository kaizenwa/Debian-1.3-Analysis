## Copyright (C) 1996 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## usage: polyinteg (p)
##
## Returns the coefficients of the integral the polynomial whose coefficients
## are represented by the vector p.
##
## The constant of integration is zero.
##
## SEE ALSO: poly, polyderiv, polyreduce, roots, conv, deconv, residue,
##           filter, polyval, polyvalm

## Author: Tony Richardson <amr@mpl.ucsd.edu>
## Created: June 1994
## Adapted-By: jwe

function p = polyinteg (p)

  if(nargin != 1)
    usage ("polyinteg (vector)");
  endif

  if (! (is_vector (p) || isempty (p)))
    error ("argument must be a vector");
  endif

  lp = length (p);

  if (lp == 0)
    p = [];
    return;
  end

  if (rows (p) > 1)
    ## Convert to column vector
    p = p.';
  endif

  p = [ p, 0 ] ./ [ lp:-1:1, 1 ];

endfunction
