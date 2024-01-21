## Copyright (C) 1993, 1994, 1995 John W. Eaton
## 
## This file is part of Octave.
## 
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
## 
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
## 
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

## usage: linspace (x1, x2, n)
##
## Return a vector of n equally spaced points between x1 and x2
## inclusive. 
##
## If the final argument is omitted, n = 100 is assumed.
##
## All three arguments must be scalars.
##
## See also: logspace

## Author: jwe

function retval = linspace (x1, x2, n)

  if (nargin == 2)
    npoints = 100;
  elseif (nargin == 3)
    if (length (n) == 1)
      npoints = fix (n);
    else
      error ("linspace: arguments must be scalars");
    endif
  else
    usage ("linspace (x1, x2 [, n])");
  endif

  if (npoints < 2)
    error ("linspace: npoints must be greater than 2");
  endif

  ## In some cases x1 + delta * (npoints - 1) will not be equal to x2,
  ## so we cheat and force the last value to be x2.

  if (length (x1) == 1 && length (x2) == 1)
    delta = (x2 - x1) / (npoints - 1);
    retval = [x1+(0:npoints-2)*delta, x2];
  else
    error ("linspace: arguments must be scalars");
  endif

endfunction
