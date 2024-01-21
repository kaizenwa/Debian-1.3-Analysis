/* Copyright (C) 1994 - 1996
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/**************************************************************************/

#include <X11/Xlib.h>
#include <X11/xpm.h>

#include <McTools/McApp.h>
#include <McTools/McBitmap.h>
#include "icons.h"

#include "pixmaps/aux.xpm"
#include "pixmaps/blaster.xpm"
#include "pixmaps/cd.xpm"
#include "pixmaps/fm.xpm"
#include "pixmaps/mic.xpm"
#include "pixmaps/mon.xpm"
#include "pixmaps/tape.xpm"
#include "pixmaps/wave.xpm"
#include "pixmaps/speaker.xpm"
#include "pixmaps/mix_play.xpm"
#include "pixmaps/mix_rec.xpm"
#include "pixmaps/bass.xpm"
#include "pixmaps/treble.xpm"
#include "pixmaps/vol.xpm"
#include "pixmaps/unknown.xpm"

#include "bitmaps/speaker.xbm"
#include "bitmaps/speaker_mask.xbm"
#include "bitmaps/wide.xbm"
#include "bitmaps/wideoff.xbm"
#include "bitmaps/mute.xbm"
#include "bitmaps/volume.xbm"

McBitmap pmap[] = {
  MCBITMAP(speaker_bits, speaker_width, speaker_height, 0, 0),
  MCBITMAP(speaker_mask_bits, speaker_mask_width, speaker_mask_height, 0, 0),
  MCBITMAP(wide_bits, wide_width, wide_height, 5, 2),
  MCBITMAP(mute_bits, mute_width, mute_height, 8, 2),
  MCBITMAP(volume_bits, volume_width, volume_height, 8, 2),
  MCBITMAP(wideoff_bits, wideoff_width, wideoff_height, 8, 2),

  MCPIXMAP(vol_xpm,                                  0, 0), /*vol*/
  MCPIXMAP(bass_xpm,                                 0, 0), /*bass*/
  MCPIXMAP(treble_xpm,                               0, 0), /*treble*/
  MCPIXMAP(fm_xpm,                                   0, 0), /*synth*/
  MCPIXMAP(wave_xpm,                                 0, 0), /*pcm*/
  MCPIXMAP(speaker_xpm,                              0, 0), /*spkr*/
  MCPIXMAP(aux_xpm,                                  0, 0), /*line*/
  MCPIXMAP(mic_xpm,                                  0, 0), /*mic*/
  MCPIXMAP(cd_xpm,                                   0, 0), /*cd*/
  MCPIXMAP(mon_xpm,                                  0, 0), /*mix*/
  MCPIXMAP(blaster_xpm,                              0, 0), /*pcm2*/
  MCPIXMAP(tape_xpm,                                 0, 0), /*rec*/
  MCPIXMAP(unknown_xpm,                              0, 0), /*unknown*/

  MCPIXMAP(mix_play_xpm,                             0, 0),
  MCPIXMAP(mix_rec_xpm,                              0, 0),
};






