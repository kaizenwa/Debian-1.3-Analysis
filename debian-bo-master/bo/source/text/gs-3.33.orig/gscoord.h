/* Copyright (C) 1989, 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gscoord.h */
/* Client interface to coordinate system operations */
/* Requires gsmatrix.h and gsstate.h */

/* Coordinate system modification */
int	gs_initmatrix(P1(gs_state *)),
	gs_defaultmatrix(P2(const gs_state *, gs_matrix *)),
	gs_currentmatrix(P2(const gs_state *, gs_matrix *)),
	gs_setmatrix(P2(gs_state *, const gs_matrix *)),
	gs_translate(P3(gs_state *, floatp, floatp)),
	gs_scale(P3(gs_state *, floatp, floatp)),
	gs_rotate(P2(gs_state *, floatp)),
	gs_concat(P2(gs_state *, const gs_matrix *));
/* Extensions */
int	gs_setdefaultmatrix(P2(gs_state *, const gs_matrix *)),
	gs_currentcharmatrix(P3(gs_state *, gs_matrix *, int)),
	gs_setcharmatrix(P2(gs_state *, const gs_matrix *)),
	gs_settocharmatrix(P1(gs_state *));

/* Coordinate transformation */
int	gs_transform(P4(gs_state *, floatp, floatp, gs_point *)),
	gs_dtransform(P4(gs_state *, floatp, floatp, gs_point *)),
	gs_itransform(P4(gs_state *, floatp, floatp, gs_point *)),
	gs_idtransform(P4(gs_state *, floatp, floatp, gs_point *));
