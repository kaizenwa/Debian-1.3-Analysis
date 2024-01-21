/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gspath2.h */
/* Client interface to Level 2 path facilities */
/* Requires gsmatrix.h */

/* Miscellaneous */
int gs_setbbox(P5(gs_state *, floatp, floatp, floatp, floatp));

/* Rectangles */
int gs_rectappend(P3(gs_state *, const gs_rect *, uint));
int gs_rectclip(P3(gs_state *, const gs_rect *, uint));
int gs_rectfill(P3(gs_state *, const gs_rect *, uint));
int gs_rectstroke(P4(gs_state *, const gs_rect *, uint, const gs_matrix *));
