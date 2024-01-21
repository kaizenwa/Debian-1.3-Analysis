## Copyright (C) 1995, 1996  Kurt Hornik
## 
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
## 
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details. 
## 
## You should have received a copy of the GNU General Public License
## along with this file.  If not, write to the Free Software Foundation,
## 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

## usage: duplication_matrix (n)
## 
## Returns the duplication matrix D_n which is the unique n^2 by
## n*(n+1)/2 matrix such that D_n * vech (A) = vec (A) for all
## symmetric n by n matrices A.
## 
## See Magnus and Neudecker (1988), Matrix differential calculus with
## applications in statistics and econometrics.

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 8 May 1995
## Adapged-By: jwe

function d = duplication_matrix (n)

  if (nargin != 1)
    usage ("duplication_matrix (n)");
  endif

  if (! (is_scalar (n) && n == round (n) && n > 0))
    error ("duplication_matrix: n must be a positive integer");
  endif

  d = zeros (n * n, n * (n + 1) / 2);

  ## It is clearly possible to make this a LOT faster!
  count = 0;
  for j = 1 : n
    d ((j - 1) * n + j, count + j) = 1;
    for i = (j + 1) : n
      d ((j - 1) * n + i, count + i) = 1;
      d ((i - 1) * n + j, count + i) = 1;
    endfor
    count = count + n - j;
  endfor

endfunction
