/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gsht1.h */
/* Extended public interface to halftones */

#ifndef gsht1_INCLUDED
#  define gsht1_INCLUDED

#include "gsht.h"

/* Procedural interface */
int	gs_setcolorscreen(P2(gs_state *, gs_colorscreen_halftone *));
int	gs_currentcolorscreen(P2(gs_state *, gs_colorscreen_halftone *));

/* We include sethalftone here, even though it is a Level 2 feature, */
/* because it turns out to be convenient to define setcolorscreen */
/* using sethalftone. */
int	gs_sethalftone(P2(gs_state *, gs_halftone *));
int	gs_currenthalftone(P2(gs_state *, gs_halftone *));

#endif					/* gsht1_INCLUDED */
