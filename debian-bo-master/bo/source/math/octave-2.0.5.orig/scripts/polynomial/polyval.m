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

## usage: polyval (c, x)
##
## Evaluate a polynomial.
##
## In octave, a polynomial is represented by it's coefficients (arranged
## in descending order). For example a vector c of length n+1 corresponds
## to the following nth order polynomial
##
##   p(x) = c(1) x^n + ... + c(n) x + c(n+1).
##
## polyval(c,x) will evaluate the polynomial at the specified value of x.
##
## If x is a vector or matrix, the polynomial is evaluated at each of the
## elements of x.
##
## SEE ALSO: polyvalm, poly, roots, conv, deconv, residue, filter,
##           polyderiv, polyinteg

## Author: Tony Richardson <amr@mpl.ucsd.edu>
## Created: June 1994
## Adapted-By: jwe

function y = polyval (c, x)

  if (nargin != 2)
    usage ("polyval (c, x)");
  endif

  if (! (is_vector (c) || isempty (c)))
    error ("poly: first argument must be a vector.");
  endif

  if (isempty (x))
    y = [];
    return;
  endif

  if (length (c) == 0)
    y = c;
    return;
  endif

  n = length (c);
  y = c (1) * ones (rows (x), columns (x));
  for index = 2:n
    y = c (index) + x .* y;
  endfor

endfunction
