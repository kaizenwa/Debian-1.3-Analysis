/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gxdither.h */
/* Interface to gxdither.c */

/* Render a gray, possibly by halftoning. */
int gx_render_gray(P3(frac, gx_device_color *, const gs_state *));

/* Render a color, possibly by halftoning. */
int gx_render_color(P7(frac, frac, frac, frac, bool, gx_device_color *, const gs_state *));
#define gx_render_rgb(r, g, b, pdevc, pgs)\
  gx_render_color(r, g, b, frac_0, false, pdevc, pgs)
#define gx_render_cmyk(c, m, y, k, pdevc, pgs)\
  gx_render_color(c, m, y, k, true, pdevc, pgs)
