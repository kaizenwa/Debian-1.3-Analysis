/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USER'S OWN RISK.
 */
#include "global.h"
#include <setjmp.h>
#include "color.h"
#include "font.h"
#include "jpeg.h"
#include "widgets.h"
#include "../Widgets/ArtText.h"

#if !HAVE_JPEG

Pixmap do_jpeg(char *data, long len, long *wp, long *hp)
{
    return None;
}

#else

#include <jpeglib.h>

#if BITS_IN_JSAMPLE != 8
#error "BITS_IN_JSAMPLE must be 8"
#endif

/*
 *  libjpeg can't dither to greyscale colormap...
 */

static unsigned char	*jpg_cmap[3]      = {0, };
static unsigned int	 jpg_cols_inited  = False;

static void init_jpg_cmap(void)
{
    unsigned int	n;

    if (jpg_cols_inited)
	return;
    jpg_cols_inited = True;

    if (!cmap) /* true color */
	return;

    jpg_cmap[0] = (unsigned char *)XtMalloc(cmap_size);
    jpg_cmap[1] = (unsigned char *)XtMalloc(cmap_size);
    jpg_cmap[2] = (unsigned char *)XtMalloc(cmap_size);

    for (n = 0 ; n < cmap_size ; n++) {
	jpg_cmap[0][n] = cmap[n].r;
	jpg_cmap[1][n] = cmap[n].g;
	jpg_cmap[2][n] = cmap[n].b;
    }
}

/*********************************************************************/

struct jpg_error_mgr {
    struct jpeg_error_mgr	jerr;
    jmp_buf			jump_buffer;
};

static void jpg_error_exit(j_common_ptr cinfo)
{
    struct jpg_error_mgr	*err_mgr = (struct jpg_error_mgr *)cinfo->err;

    longjmp(err_mgr->jump_buffer, 1);
}

static void jpg_no_op(j_decompress_ptr cinfo)
{
}

static boolean jpg_fill_input_buffer(j_decompress_ptr cinfo)
{
    struct jpg_error_mgr	*err_mgr = (struct jpg_error_mgr *)cinfo->err;

    longjmp(err_mgr->jump_buffer, 1);
}

static void jpg_skip_input_data(j_decompress_ptr cinfo, long num_bytes)
{
    if (num_bytes < 0)
	return;
    if (num_bytes >= cinfo->src->bytes_in_buffer)
	num_bytes = cinfo->src->bytes_in_buffer;
    cinfo->src->bytes_in_buffer -= num_bytes;
    cinfo->src->next_input_byte += num_bytes;
}

Pixmap do_jpeg(char *data, long len, long *wp, long *hp)
{
    struct jpeg_decompress_struct	cinfo;
    struct jpg_error_mgr		err_mgr;
    struct jpeg_source_mgr		src_mgr;
    Pixmap		pixmap;
    void *volatile	vol_pic  = NULL;
    volatile long	vol_w    = 0;
    volatile long	vol_h    = 0;
    volatile int	vol_did  = False;
    volatile int	grey     = False;

    init_jpg_cmap();

    cinfo.err = jpeg_std_error(&err_mgr.jerr);
    err_mgr.jerr.error_exit = jpg_error_exit;

    if (setjmp(err_mgr.jump_buffer))
	ArtTextAddLine(main_widgets.text, "[knews: jpeg error.]",
		       ascii_font->body_font, global.alert_pixel);
    else {
	long		w, h;
	int		col, did;
	unsigned char	*pic, *row;

	src_mgr.next_input_byte   = (unsigned char *)data;
	src_mgr.bytes_in_buffer   = len;
	src_mgr.init_source       = jpg_no_op;
	src_mgr.fill_input_buffer = jpg_fill_input_buffer;
	src_mgr.skip_input_data   = jpg_skip_input_data;
	src_mgr.resync_to_restart = jpeg_resync_to_restart;
	src_mgr.term_source       = jpg_no_op;
	jpeg_create_decompress(&cinfo);
	cinfo.src = &src_mgr;

	jpeg_read_header(&cinfo, True);

	if (cinfo.jpeg_color_space == JCS_GRAYSCALE)
	    /*
	     *  libjpeg can't convert from grayscale to rgb...
	     */
	    grey = True;
	else {
	    cinfo.out_color_space = JCS_RGB;
	    if (jpg_cmap) {
		cinfo.quantize_colors         = True;
		cinfo.colormap                = jpg_cmap;
		cinfo.actual_number_of_colors = cmap_size;
	    }
	}

	jpeg_start_decompress(&cinfo);
	vol_w = w = cinfo.output_width;
	vol_h = h = cinfo.output_height;
	if (cinfo.output_components != 1 && cinfo.output_components != 3) {
	    fprintf(stderr, "knews: output_components=%d\n",
		    cinfo.output_components);
	    longjmp(err_mgr.jump_buffer, 1);
	}
	col = cinfo.output_components == 3;

	did = False;
	vol_pic = pic = (unsigned char *)XtMalloc(w * h * (col ? 3 : 1));
	while (cinfo.output_scanline < h) {
	    row = pic + cinfo.output_scanline * w * (col ? 3 : 1);
	    jpeg_read_scanlines(&cinfo, &row, 1);
	    if (!did)
		vol_did = did = True;
	}
	jpeg_finish_decompress(&cinfo);
    }

    if (!vol_did)
	pixmap = None;
    else {
	unsigned char	*pic = vol_pic;
	unsigned int	w    = vol_w;
	unsigned int	h    = vol_h;

	*wp = w;
	*hp = h;
	if (grey)
	    pixmap = put_grey_image(pic, w, h);
	else if (!jpg_cmap)
	    pixmap = put_24_image(pic, w, h);
	else {
	    long	i, n = w * h;

	    for (i = 0 ; i < n ; i++)
		pic[i] = cmap[pic[i]].pixel;

	    pixmap = put_8_image(pic, w, h);
	}
    }

    jpeg_destroy_decompress(&cinfo);
    XtFree((char *)vol_pic);

    return pixmap;
}

#endif
