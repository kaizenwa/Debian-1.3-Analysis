/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gshsb.h */
/* Client interface to HSB color routines */

int	gs_sethsbcolor(P4(gs_state *, floatp, floatp, floatp)),
	gs_currenthsbcolor(P2(const gs_state *, float [3]));
