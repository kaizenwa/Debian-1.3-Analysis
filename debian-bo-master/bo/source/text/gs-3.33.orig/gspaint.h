/* Copyright (C) 1989, 1992, 1993 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gspaint.h */
/* Client interface to painting functions */
/* Requires gsstate.h */

/* Painting */
int	gs_erasepage(P1(gs_state *)),
	gs_fillpage(P1(gs_state *)),
	gs_fill(P1(gs_state *)),
	gs_eofill(P1(gs_state *)),
	gs_stroke(P1(gs_state *));

/* Image tracing */
int	gs_imagepath(P4(gs_state *, int, int, const byte *));
