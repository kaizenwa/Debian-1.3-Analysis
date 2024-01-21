/*
 * Programm XBLAST V2.1.3 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * July 11th 1996
 * started August 1993
 *
 * File: expl.c
 * bitmap data of bombs and explosions
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as by published
 * by the Free Software Foundation; either version 2; or (at your option)
 * any later version
 *
 * This program is distributed in the hope that it will entertaining,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILTY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Publis License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */



#define _EXPL_C

#include "include.h"
#include "mytypes.h"
#include "expl.h"

/* 
 *  explosion bitmap mask 
 */

#include "bitmap/explosion/expl00_mask.xbm"
#include "bitmap/explosion/expl01_mask.xbm"
#include "bitmap/explosion/expl02_mask.xbm"
#include "bitmap/explosion/expl03_mask.xbm"
#include "bitmap/explosion/expl04_mask.xbm"
#include "bitmap/explosion/expl05_mask.xbm"
#include "bitmap/explosion/expl06_mask.xbm"
#include "bitmap/explosion/expl07_mask.xbm"
#include "bitmap/explosion/expl08_mask.xbm"
#include "bitmap/explosion/expl09_mask.xbm"
#include "bitmap/explosion/expl0a_mask.xbm"
#include "bitmap/explosion/expl0b_mask.xbm"
#include "bitmap/explosion/expl0c_mask.xbm"
#include "bitmap/explosion/expl0d_mask.xbm"
#include "bitmap/explosion/expl0e_mask.xbm"
#include "bitmap/explosion/expl0f_mask.xbm"
BitmapStruct expl_mask[MAX_EXPLOSION] = {
  { expl00_mask_width, expl00_mask_height, expl00_mask_bits },
  { expl01_mask_width, expl01_mask_height, expl01_mask_bits },
  { expl02_mask_width, expl02_mask_height, expl02_mask_bits },
  { expl03_mask_width, expl03_mask_height, expl03_mask_bits },
  { expl04_mask_width, expl04_mask_height, expl04_mask_bits },
  { expl05_mask_width, expl05_mask_height, expl05_mask_bits },
  { expl06_mask_width, expl06_mask_height, expl06_mask_bits },
  { expl07_mask_width, expl07_mask_height, expl07_mask_bits },
  { expl08_mask_width, expl08_mask_height, expl08_mask_bits },
  { expl09_mask_width, expl09_mask_height, expl09_mask_bits },
  { expl0a_mask_width, expl0a_mask_height, expl0a_mask_bits },
  { expl0b_mask_width, expl0b_mask_height, expl0b_mask_bits },
  { expl0c_mask_width, expl0c_mask_height, expl0c_mask_bits },
  { expl0d_mask_width, expl0d_mask_height, expl0d_mask_bits },
  { expl0e_mask_width, expl0e_mask_height, expl0e_mask_bits },
  { expl0f_mask_width, expl0f_mask_height, expl0f_mask_bits },
};



/* 
 *  explosion bitmap
 */
#include "bitmap/explosion/expl00.xbm"
#include "bitmap/explosion/expl01.xbm"
#include "bitmap/explosion/expl02.xbm"
#include "bitmap/explosion/expl03.xbm"
#include "bitmap/explosion/expl04.xbm"
#include "bitmap/explosion/expl05.xbm"
#include "bitmap/explosion/expl06.xbm"
#include "bitmap/explosion/expl07.xbm"
#include "bitmap/explosion/expl08.xbm"
#include "bitmap/explosion/expl09.xbm"
#include "bitmap/explosion/expl0a.xbm"
#include "bitmap/explosion/expl0b.xbm"
#include "bitmap/explosion/expl0c.xbm"
#include "bitmap/explosion/expl0d.xbm"
#include "bitmap/explosion/expl0e.xbm"
#include "bitmap/explosion/expl0f.xbm"

BitmapStruct expl_bits[MAX_EXPLOSION] = {
  { expl00_width, expl00_height, expl00_bits },
  { expl01_width, expl01_height, expl01_bits },
  { expl02_width, expl02_height, expl02_bits },
  { expl03_width, expl03_height, expl03_bits },
  { expl04_width, expl04_height, expl04_bits },
  { expl05_width, expl05_height, expl05_bits },
  { expl06_width, expl06_height, expl06_bits },
  { expl07_width, expl07_height, expl07_bits },
  { expl08_width, expl08_height, expl08_bits },
  { expl09_width, expl09_height, expl09_bits },
  { expl0a_width, expl0a_height, expl0a_bits },
  { expl0b_width, expl0b_height, expl0b_bits },
  { expl0c_width, expl0c_height, expl0c_bits },
  { expl0d_width, expl0d_height, expl0d_bits },
  { expl0e_width, expl0e_height, expl0e_bits },
  { expl0f_width, expl0f_height, expl0f_bits },
};

/* 
 *  explosion bitmap
 */
#include "bitmap/color/explosion/expl00_A.xbm"
#include "bitmap/color/explosion/expl01_A.xbm"
#include "bitmap/color/explosion/expl02_A.xbm"
#include "bitmap/color/explosion/expl03_A.xbm"
#include "bitmap/color/explosion/expl04_A.xbm"
#include "bitmap/color/explosion/expl05_A.xbm"
#include "bitmap/color/explosion/expl06_A.xbm"
#include "bitmap/color/explosion/expl07_A.xbm"
#include "bitmap/color/explosion/expl08_A.xbm"
#include "bitmap/color/explosion/expl09_A.xbm"
#include "bitmap/color/explosion/expl0a_A.xbm"
#include "bitmap/color/explosion/expl0b_A.xbm"
#include "bitmap/color/explosion/expl0c_A.xbm"
#include "bitmap/color/explosion/expl0d_A.xbm"
#include "bitmap/color/explosion/expl0e_A.xbm"
#include "bitmap/color/explosion/expl0f_A.xbm"

BitmapStruct expl_addon[MAX_EXPLOSION] = {
  { expl00_A_width, expl00_A_height, expl00_A_bits },
  { expl01_A_width, expl01_A_height, expl01_A_bits },
  { expl02_A_width, expl02_A_height, expl02_A_bits },
  { expl03_A_width, expl03_A_height, expl03_A_bits },
  { expl04_A_width, expl04_A_height, expl04_A_bits },
  { expl05_A_width, expl05_A_height, expl05_A_bits },
  { expl06_A_width, expl06_A_height, expl06_A_bits },
  { expl07_A_width, expl07_A_height, expl07_A_bits },
  { expl08_A_width, expl08_A_height, expl08_A_bits },
  { expl09_A_width, expl09_A_height, expl09_A_bits },
  { expl0a_A_width, expl0a_A_height, expl0a_A_bits },
  { expl0b_A_width, expl0b_A_height, expl0b_A_bits },
  { expl0c_A_width, expl0c_A_height, expl0c_A_bits },
  { expl0d_A_width, expl0d_A_height, expl0d_A_bits },
  { expl0e_A_width, expl0e_A_height, expl0e_A_bits },
  { expl0f_A_width, expl0f_A_height, expl0f_A_bits },
};

/*
 * bomb bitmaps
 */
#include "bitmap/explosion/bomb1_mask.xbm"
#include "bitmap/explosion/bomb2_mask.xbm"

BitmapStruct bomb_mask[MAX_BOMBS] = {
  { bomb1_mask_width, bomb1_mask_height, bomb1_mask_bits },
  { bomb2_mask_width, bomb2_mask_height, bomb2_mask_bits },
};

#include "bitmap/explosion/bomb1.xbm"
#include "bitmap/explosion/bomb2.xbm"

BitmapStruct bomb_bits[MAX_BOMBS] = {
  { bomb1_width, bomb1_height, bomb1_bits },
  { bomb2_width, bomb2_height, bomb2_bits },
};

#include "bitmap/color/explosion/bomb1_A.xbm"
#include "bitmap/color/explosion/bomb2_A.xbm"

BitmapStruct bomb_addon[MAX_BOMBS] = {
  { bomb1_A_width, bomb1_A_height, bomb1_A_bits },
  { bomb2_A_width, bomb2_A_height, bomb2_A_bits },
};
/*
 * end of file expl.c
 */
