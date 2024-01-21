/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* spdiffx.h */
/* Pixel differencing filter state definition */
/* Requires strimpl.h */

/* PixelDifferenceDecode / PixelDifferenceEncode */
typedef struct stream_PDiff_state_s {
	stream_state_common;
		/* The client sets the following before initialization. */
	int Colors;		/* # of colors, 1..4 */
	int BitsPerComponent;	/* 1, 2, 4, 8 */
	int Columns;
		/* The init procedure computes the following. */
	uint row_count;		/* # of bytes per row */
	byte end_mask;		/* mask for left-over bits in last byte */
	int case_index;		/* switch index for case dispatch */
		/* The following are updated dynamically. */
	uint row_left;		/* # of bytes left in row */
	byte s0, s1, s2, s3;	/* previous sample */
} stream_PDiff_state;
#define private_st_PDiff_state()	/* in spdiff.c */\
  gs_private_st_simple(st_PDiff_state, stream_PDiff_state,\
    "PixelDifferenceEncode/Decode state")
extern const stream_template s_PDiffD_template;
extern const stream_template s_PDiffE_template;
