/* Copyright (C) 1992, 1993, 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gxfmap.h */
/* Fraction map representation for Ghostscript */

#ifndef gxfmap_INCLUDED
#  define gxfmap_INCLUDED

#include "gsrefct.h"
#include "gxfrac.h"
#include "gxtmap.h"

/*
 * Define a cached map from fracs to fracs.  Level 1 uses this only
 * for the transfer function; level 2 also uses it for black generation
 * and undercolor removal.  Note that reference counting macros must
 * be used to allocate, free, and assign references to gx_transfer_maps.
 */
/* log2... must not be greater than frac_bits. */
#define log2_transfer_map_size 8
#define transfer_map_size (1 << log2_transfer_map_size)
/*typedef struct gx_transfer_map_s gx_transfer_map;*/	/* in gxtmap.h */
struct gx_transfer_map_s {
	rc_header rc;
	gs_mapping_proc proc;		/* typedef is in gxtmap.h */
	frac values[transfer_map_size];
};
/* We export st_transfer_map for gscolor.c. */
extern_st(st_transfer_map);
#define public_st_transfer_map() /* in gsstate.c */\
  gs_public_st_simple(st_transfer_map, gx_transfer_map, "gx_transfer_map")

/* Map a color fraction through a transfer map. */
frac gx_color_frac_map(P2(frac, const frac *));
#define gx_map_color_frac(pgs,cf,m)\
  gx_color_frac_map(cf, &pgs->m->values[0])

/* Map a color fraction expressed as a byte through a transfer map. */
/* (We don't use this anywhere right now.) */
/****************
#if log2_transfer_map_size <= 8
#  define byte_to_tmx(b) ((b) >> (8 - log2_transfer_map_size))
#else
#  define byte_to_tmx(b)\
	(((b) << (log2_transfer_map_size - 8)) +\
	 ((b) >> (16 - log2_transfer_map_size)))
#endif
#define gx_map_color_frac_byte(pgs,b,m)\
 (pgs->m->values[byte_to_tmx(b)])
 ****************/

/* Map a floating point value through a transfer map. */
#define gx_map_color_float(pmap,v)\
  ((pmap)->values[(int)((v) * transfer_map_size + 0.5)] / frac_1_float)

#endif					/* gxfmap_INCLUDED */
