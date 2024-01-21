/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* sjpegd.c */
/* Interface routines for IJG decoding code. */
#include "stdio_.h"
#include "string_.h"
#include "jpeglib.h"
#include "jerror.h"
#include "gx.h"
#include "gserrors.h"
#include "strimpl.h"
#include "sdct.h"
#include "sjpeg.h"

/*
 * Interface routines.  This layer of routines exists solely to limit
 * side-effects from using setjmp.
 */

int
gs_jpeg_create_decompress (stream_DCT_state *st)
{	/* Initialize error handling */
	gs_jpeg_error_setup(st);
	/* Establish the setjmp return context for gs_jpeg_error_exit to use. */
	if (setjmp(st->data.common->exit_jmpbuf))
		return_error(gs_jpeg_log_error(st));

	jpeg_create_decompress(&st->data.decompress->dinfo);
	return 0;
}

int
gs_jpeg_read_header (stream_DCT_state *st,
		     boolean require_image)
{	if (setjmp(st->data.common->exit_jmpbuf))
		return_error(gs_jpeg_log_error(st));
	return jpeg_read_header(&st->data.decompress->dinfo, require_image);
}

int
gs_jpeg_start_decompress (stream_DCT_state *st)
{	if (setjmp(st->data.common->exit_jmpbuf))
		return_error(gs_jpeg_log_error(st));
	jpeg_start_decompress(&st->data.decompress->dinfo);
	return 0;
}

int
gs_jpeg_read_scanlines (stream_DCT_state *st,
			JSAMPARRAY scanlines,
			int max_lines)
{	if (setjmp(st->data.common->exit_jmpbuf))
		return_error(gs_jpeg_log_error(st));
	return (int) jpeg_read_scanlines(&st->data.decompress->dinfo,
					 scanlines, (JDIMENSION) max_lines);
}

int
gs_jpeg_finish_decompress (stream_DCT_state *st)
{	if (setjmp(st->data.common->exit_jmpbuf))
		return_error(gs_jpeg_log_error(st));
	return (int) jpeg_finish_decompress(&st->data.decompress->dinfo);
}
