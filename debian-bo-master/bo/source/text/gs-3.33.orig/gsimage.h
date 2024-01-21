/* Copyright (C) 1992, 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gsimage.h */
/* Client interface to image painting */
/* Requires gscspace.h, gsstate.h, and gsmatrix.h */

/* The image painting interface uses an enumeration style: */
/* the client initializes an enumerator, then supplies data incrementally. */
typedef struct gs_image_enum_s gs_image_enum;
gs_image_enum *	gs_image_enum_alloc(P2(gs_memory_t *, client_name_t));
/* image_init and imagemask_init return 1 for an empty image, */
/* 0 normally, <0 on error. */
int	gs_image_init(P10(gs_image_enum *penum, gs_state *pgs,
			  int width, int height, int bits_per_component,
			  bool multi, const gs_color_space *pcs,
			  const float *decode, bool interpolate,
			  gs_matrix *pmat));
int	gs_imagemask_init(P8(gs_image_enum *penum, gs_state *pgs,
			     int width, int height, bool invert,
			     bool interpolate, gs_matrix *pmat, bool adjust));
int	gs_image_next(P4(gs_image_enum *penum, const byte *dbytes,
			 uint dsize, uint *pused));
/* Clean up after processing an image. */
void	gs_image_cleanup(P1(gs_image_enum *penum));
