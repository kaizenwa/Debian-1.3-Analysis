/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gxdraw.h */
/* Interface to drawing routines in gxdraw.c */
/* Requires gxfixed.h, gxdcolor.h */

extern int gx_fill_trapezoid_fixed(P9(fixed fx0, fixed fw0, fixed fy0,
				       fixed fx1, fixed fw1, fixed fh,
				       bool swap_axes,
				       const gx_device_color *pdevc,
				       const gs_state *pgs));
extern int gx_fill_pgram_fixed(P8(fixed px, fixed py, fixed ax, fixed ay,
				  fixed bx, fixed by,
				  const gx_device_color *pdevc,
				  const gs_state *pgs));
extern int gx_draw_line_fixed(P6(fixed ixf, fixed iyf,
				 fixed itoxf, fixed itoyf,
				 const gx_device_color *pdevc,
				 const gs_state *pgs));
