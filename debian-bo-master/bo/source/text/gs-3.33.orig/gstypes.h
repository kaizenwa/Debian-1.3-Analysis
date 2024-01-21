/* Copyright (C) 1989, 1990, 1993, 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gstypes.h */
/* Miscellaneous common types for Ghostscript library */

#ifndef gstypes_INCLUDED
#  define gstypes_INCLUDED

/*
 * Define a sensible representation of a string, as opposed to
 * the C char * type (which can't store arbitrary data, represent
 * substrings, or perform concatenation without destroying aliases).
 */
typedef struct gs_string_s {
	byte *data;
	uint size;
} gs_string;
typedef struct gs_const_string_s {
	const byte *data;
	uint size;
} gs_const_string;

/*
 * Define types for Cartesian points.
 */
typedef struct gs_point_s {
	double x, y;
} gs_point;
typedef struct gs_int_point_s {
	int x, y;
} gs_int_point;

/*
 * Define a scale for oversampling.  Clients don't actually use this,
 * but this seemed like the handiest place for it.
 */
typedef struct gs_log2_scale_point_s {
	int x, y;
} gs_log2_scale_point;

/*
 * Define types for rectangles in the Cartesian plane.
 * Note that rectangles are half-open, i.e.: their width is
 * q.x-p.x and their height is q.y-p.y; they include the points
 * (x,y) such that p.x<=x<q.x and p.y<=y<q.y.
 */
typedef struct gs_rect_s {
	gs_point p, q;			/* origin point, corner point */
} gs_rect;
typedef struct gs_int_rect_s {
	gs_int_point p, q;
} gs_int_rect;

#endif					/* gstypes_INCLUDED */
