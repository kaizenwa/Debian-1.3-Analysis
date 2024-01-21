/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gxcoord.h */
/* Internal coordinate system procedures */
/* Requires gxmatrix.h and gzstate.h */
#include "gscoord.h"

/* Set the translation to a fixed value, and translate any existing path. */
/* Used by gschar.c to prepare for a BuildChar or BuildGlyph procedure. */
int gx_translate_to_fixed(P3(gs_state *, fixed, fixed));

/* Scale the CTM and character matrix for oversampling. */
int gx_scale_char_matrix(P3(gs_state *, int, int));

/* Compute the coefficients for fast fixed-point distance transformations */
/* from a transformation matrix. */
int gx_matrix_to_fixed_coeff(P3(const gs_matrix *, fixed_coeff *, int));
