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

enum {
  PIX_ICON,
  PIX_ICONMASK,
  PIX_WIDE,
  PIX_MUTE,
  PIX_NOMUTE,
  PIX_NOWIDE,

  PIX_VOL,
  PIX_BASS,
  PIX_TREBLE,
  PIX_SYNTH,
  PIX_PCM,
  PIX_SPEAKER,
  PIX_LINE,
  PIX_MIC,
  PIX_CD,
  PIX_MIX,
  PIX_PCM2,
  PIX_REC,
  PIX_UNKNOWN,

  PIX_MIX_PLAY,
  PIX_MIX_REC,

  NUM_BITMAPS
};

#define NUM_ICONS (PIX_REC - PIX_VOL + 1)

extern McBitmap pmap[];




