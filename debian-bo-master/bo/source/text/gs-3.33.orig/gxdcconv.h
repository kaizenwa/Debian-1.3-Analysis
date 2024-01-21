/* Copyright (C) 1992, 1993 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gxdcconv.h */
/* Internal device color conversion interfaces */
#include "gxfrac.h"

/* Color space conversion routines */
frac color_rgb_to_gray(P4(frac r, frac g, frac b,
  const gs_state *pgs));
void color_rgb_to_cmyk(P5(frac r, frac g, frac b,
  const gs_state *pgs, frac cmyk[4]));
frac color_cmyk_to_gray(P5(frac c, frac m, frac y, frac k,
  const gs_state *pgs));
void color_cmyk_to_rgb(P6(frac c, frac m, frac y, frac k,
  const gs_state *pgs, frac rgb[3]));
