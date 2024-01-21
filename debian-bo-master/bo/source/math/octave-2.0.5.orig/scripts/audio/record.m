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

## usage:  X = record (sec [, sampling_rate])
##
## Records sec seconds of audio into the vector X.
## The default value for the sampling_rate is 8000, ie. 8kHz.
## The program waits for you to hit the ENTER key, then the recording
## starts immediatly.

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Created: 19 September 1994
## Adapted-By: jwe

function X = record (sec, sampling_rate)

  if (nargin == 1)
    sampling_rate = 8000;
  elseif (nargin != 2)
    usage ("X = record (sec [, sampling_rate])");
  endif

  unwind_protect

    file = tmpnam ();

    input ("Please hit ENTER and speak afterwards!\n", 1);

    cmd = sprintf ("dd if=/dev/dsp of=%s bs=%d count=%d",
                   file, sampling_rate, sec)

    system (cmd);

    num = fopen (file, "r");

    [Y, c] = fread (num, sampling_rate * sec, "uchar");

    fclose (num);

  unwind_protect_cleanup

    unlink (file);

  end_unwind_protect

  X = Y - 127;

endfunction
