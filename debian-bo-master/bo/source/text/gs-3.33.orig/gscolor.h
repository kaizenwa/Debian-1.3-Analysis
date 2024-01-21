/* Copyright (C) 1991, 1992, 1993 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gscolor.h */
/* Client interface to color routines */

#ifndef gscolor_INCLUDED
#  define gscolor_INCLUDED

#include "gxtmap.h"

/* Color and gray interface */
int	gs_setgray(P2(gs_state *, floatp));
float	gs_currentgray(P1(const gs_state *));
int	gs_setrgbcolor(P4(gs_state *, floatp, floatp, floatp)),
	gs_currentrgbcolor(P2(const gs_state *, float [3])),
	gs_setalpha(P2(gs_state *, floatp));
float	gs_currentalpha(P1(const gs_state *));
/* Transfer function */
int	gs_settransfer(P2(gs_state *, gs_mapping_proc)),
	gs_settransfer_remap(P3(gs_state *, gs_mapping_proc, bool));
gs_mapping_proc	gs_currenttransfer(P1(const gs_state *));

#endif					/* gscolor_INCLUDED */
