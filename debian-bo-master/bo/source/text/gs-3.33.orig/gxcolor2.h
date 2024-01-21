/* Copyright (C) 1993, 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gxcolor2.h */
/* Internal definitions for Level 2 color routines for Ghostscript library */
/* (requires gxfixed.h) */
#include "gscolor2.h"
#include "gsrefct.h"
#include "gxbitmap.h"

/* Cache for Indexed color with procedure, or Separation color. */
struct gs_indexed_map_s {
	rc_header rc;
	union {
		int (*lookup_index)(P3(const gs_indexed_params *, int, float *));
		int (*tint_transform)(P3(const gs_separation_params *, floatp, float *));
	} proc;
	uint num_values; /* base_space->type->num_components * (hival + 1) */
	float *values;	/* actually [num_values] */
};
#define private_st_indexed_map() /* in zcsindex.c */\
  gs_private_st_ptrs1(st_indexed_map, gs_indexed_map, "gs_indexed_map",\
    indexed_map_enum_ptrs, indexed_map_reloc_ptrs, values)

/*
 * We define 'tiling space' as the space in which (0,0) is the origin of
 * the key pattern cell and in which coordinate (i,j) is displaced by
 * i * XStep + j * YStep from the origin.  In this space, it is easy to
 * compute a (rectangular) set of tile copies that cover a (rectangular)
 * region to be tiled.
 */

/* Implementation of Pattern instances. */
struct gs_pattern_instance_s {
	rc_header rc;
	gs_client_pattern template;
	/* Following are created by makepattern */
	gs_state *saved;
	gs_matrix matrix;		/* tiling space -> device space */
	gs_rect bbox;			/* bbox of tile in tiling space */
	gs_point offset;		/* of tile from matrix.tx/y */
	gs_int_point size;		/* in device coordinates */
	gx_bitmap_id id;		/* key for cached bitmap */
					/* (= id of mask) */
};
/* The following is only public for a type test in the interpreter */
/* (.buildpattern operator). */
extern_st(st_pattern_instance);
#define public_st_pattern_instance() /* in gspcolor.c */\
  gs_public_st_ptrs_add1(st_pattern_instance, gs_pattern_instance,\
    "pattern instance", pattern_instance_enum_ptrs,\
    pattern_instance_reloc_ptrs, st_client_pattern, template, saved)
