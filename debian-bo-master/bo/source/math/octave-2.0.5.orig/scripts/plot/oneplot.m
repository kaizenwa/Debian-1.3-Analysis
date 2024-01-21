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

## usage: oneplot
##
## NOTE: this will work only with gnuplot installed with
##       multiplot patch
##
## Switches from multiplot (if in  multiplot mode) to single plot
## mode

## Author: Vinayak Dutt <Dutt.Vinayak@mayo.EDU>
## Created: 3 July 95
## Adapted-By: jwe

function oneplot ()

  if (! gnuplot_has_multiplot)
    error ("oneplot: gnuplot does not appear to support this feature");
  endif

  global multiplot_mode

  gset nomultiplot;
  gset size 1, 1;
  gset origin 0, 0;
  multiplot_mode = 0;

endfunction
