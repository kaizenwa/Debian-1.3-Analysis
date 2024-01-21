/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

     This program is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation; either version 2, or (at your option)
     any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the Free Software
     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#include <X11/Xlib.h>
#include <McTools/McApp.h>
#include <McTools/McBitmap.h>
#include "icons.h"

#include "bitmaps/cdlogo.xbm"
#include "bitmaps/eject.xbm"
#include "bitmaps/newcd.xbm"
#include "bitmaps/noload.xbm"
#include "bitmaps/ff.xbm"
#include "bitmaps/speaker.xbm"
#include "bitmaps/speaker_mask.xbm"
#include "bitmaps/pause.xbm"
#include "bitmaps/play.xbm"
#include "bitmaps/rewind.xbm"
#include "bitmaps/shuffle.xbm"
#include "bitmaps/stop.xbm"
#include "bitmaps/repeat.xbm"
#include "bitmaps/volume.xbm"
#include "bitmaps/clock.xbm"
#include "bitmaps/prev.xbm"
#include "bitmaps/next.xbm"
#include "bitmaps/letter_t.xbm"
#include "bitmaps/letter_tr.xbm"
#include "bitmaps/letter_e.xbm"
#include "bitmaps/letter_r.xbm"

McBitmap pmap[] = {
    MCBITMAP(cdlogo_bits, cdlogo_width, cdlogo_height, 0, 0),
    MCBITMAP(speaker_bits, speaker_width, speaker_height, 0, 0),
    MCBITMAP(speaker_mask_bits, speaker_mask_width, speaker_mask_height, 0, 0),
    MCBITMAP(eject_bits, eject_width, eject_height, 8, 2),
    MCBITMAP(newcd_bits, newcd_width, newcd_height, 8, 2),
    MCBITMAP(noload_bits, noload_width, noload_height, 8, 2),
    MCBITMAP(ff_bits, ff_width, ff_height, 8, 2),
    MCBITMAP(pause_bits, pause_width, pause_height, 8, 2),
    MCBITMAP(play_bits, play_width, play_height, 14, 2),
    MCBITMAP(rewind_bits, rewind_width, rewind_height, 8, 2),
    MCBITMAP(shuffle_bits, shuffle_width, shuffle_height, 8, 2),
    MCBITMAP(stop_bits, stop_width, stop_height, 14, 1),
    MCBITMAP(repeat_bits, repeat_width, repeat_height, 8, 2),
    MCBITMAP(volume_bits, volume_width, volume_height, 0, 0),
    MCBITMAP(clock_bits, clock_width, clock_height, 0, 0),
    MCBITMAP(prev_bits, prev_width, prev_height, 8, 2),
    MCBITMAP(next_bits, next_width, next_height, 8, 2),
    MCBITMAP(letter_t_bits, letter_t_width, letter_t_height, 2, 2),
    MCBITMAP(letter_tr_bits, letter_tr_width, letter_tr_height, 2, 2),
    MCBITMAP(letter_e_bits, letter_e_width, letter_e_height, 2, 2),
    MCBITMAP(letter_r_bits, letter_r_width, letter_r_height, 2, 2),
  };





