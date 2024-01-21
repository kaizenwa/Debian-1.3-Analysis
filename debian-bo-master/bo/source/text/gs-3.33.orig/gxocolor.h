/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gxocolor.h */
/* Concrete color representation for Ghostscript */

#ifndef gxocolor_INCLUDED
#  define gxocolor_INCLUDED

#include "gxfrac.h"

/*
 * A concrete color is a color that has been reduced to a device space
 * (gray, RGB, CMYK, or separation) but has not yet been subjected to
 * space conversion, transfer, or halftoning.  This is the level of
 * abstraction at which image resampling occurs.  (Currently,
 * this is the *only* use for concrete colors.)
 *
 * Concrete colors identify their color space, because they might have been
 * produced by converting an Indexed or Separation color.
 */

/* Define an opaque type for a color space. */
/* (The actual definition is in gscspace.h.) */
#ifndef gs_color_space_DEFINED
#  define gs_color_space_DEFINED
typedef struct gs_color_space_s gs_color_space;
#endif

#ifndef gx_concrete_color_DEFINED
#  define gx_concrete_color_DEFINED
typedef struct gx_concrete_color_s gx_concrete_color;
#endif

struct gx_concrete_color_s {
	frac values[4];
	const gs_color_space *color_space;
};

/* Concrete colors only exist transiently, */
/* so they don't need a structure type. */

#endif					/* gxocolor_INCLUDED */
