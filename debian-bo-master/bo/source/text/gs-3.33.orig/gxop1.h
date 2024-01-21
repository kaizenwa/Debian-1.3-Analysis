/* Copyright (C) 1991, 1992 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gxop1.h */
/* Type 1 state shared between interpreter and compiled fonts. */

/*
 * The current point (px,py) in the Type 1 interpreter state is not
 * necessarily the same as the current position in the path being built up.
 * Specifically, (px,py) may not reflect adjustments for hinting,
 * whereas the current path position does reflect those adjustments.
 */

/* Define the shared Type 1 interpreter state. */
#define max_coeff_bits 11		/* max coefficient in char space */
typedef struct gs_op1_state_s {
	struct gx_path_s *ppath;
	struct gs_type1_state_s *pis;
	fixed_coeff fc;
	fixed ctx, cty;			/* character origin (device space) */
	fixed px, py;			/* current point (device space) */
} gs_op1_state;
typedef gs_op1_state _ss *is_ptr;
