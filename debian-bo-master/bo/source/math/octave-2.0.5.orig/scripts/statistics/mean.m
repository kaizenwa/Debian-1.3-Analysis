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

## usage: mean (a)
##
## For vector arguments, return the mean the values.
##
## For matrix arguments, return a row vector containing the mean for
## each column.
##
## See also: median, std

## Author: jwe

function retval = mean (a)

  if (nargin != 1)
    usage ("mean (a)");
  endif

  [nr, nc] = size (a);
  if (nr == 1 || nc == 1)
    retval = sum (a) / length (a);
  elseif (nr > 0 && nc > 0)
    retval = sum (a) / nr;
  else
    error ("mean: invalid matrix argument");
  endif

endfunction
