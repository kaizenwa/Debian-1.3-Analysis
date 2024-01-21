/* Copyright (C) 1992 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gxlum.h */
/* Luminance computation parameters for Ghostscript */

/* Color weights used for computing luminance. */
#define lum_red_weight	30
#define lum_green_weight	59
#define lum_blue_weight	11
#define lum_all_weights	(lum_red_weight + lum_green_weight + lum_blue_weight)
