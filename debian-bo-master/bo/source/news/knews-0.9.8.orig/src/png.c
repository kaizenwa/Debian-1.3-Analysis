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
#include "file.h"
#include "font.h"
#include "png.h"
#include "widgets.h"
#include "../Widgets/ArtText.h"

#if !HAVE_PNG

Pixmap do_png(char *data, long len, long *wp, long *hp)
{
    return None;
}

#else

#include <png.h>

static unsigned int	 p_cmap_inited = False;
static png_color	*p_cmap        = NULL;

static void init_png_cmap(void)
{
    unsigned int	i;

    if (p_cmap_inited)
	return;
    p_cmap_inited = True;

    if (!cmap)
	return;

    p_cmap = (png_color *)XtMalloc(cmap_size * sizeof p_cmap[0]);
    for (i = 0 ; i < cmap_size ; i++) {
	p_cmap[i].red   = cmap[i].r;
	p_cmap[i].green = cmap[i].g;
	p_cmap[i].blue  = cmap[i].b;
    }
}

/*
 * libpng can only read from files...
 */
static FILE *dump_for_png(char *data, long len)
{
    FILE	*fp;
    char	*fname;
    int		fd;

    fd = create_temp_fd(&fname);
    if (fd < 0)
	return NULL;
    unlink(fname);
    if (writen(fd, data, len) < 0) {
	close(fd);
	return NULL;
    }
    if (lseek(fd, 0, SEEK_SET) < 0) {
	perror("lseek");
	close(fd);
	return NULL;
    }
    fp = fdopen(fd, "r");
    if (!fp) {
	perror("fdopen");
	close(fd);
	return NULL;
    }

    return fp;
}

Pixmap do_png(char *data, long len, long *wp, long *hp)
{
    png_struct		p_str;
    png_info		p_info;
    Pixmap		pixmap;
    FILE *volatile	vol_fp  = NULL;
    void *volatile	vol_pic = NULL;
    void *volatile	vol_pal = NULL;
    volatile int	vol_pn  = 0;
    volatile long	vol_w   = 0;
    volatile long	vol_h   = 0;
    volatile int	vol_did = False;
    volatile int	grey    = False;

    init_png_cmap();

    if (!(vol_fp = dump_for_png(data, len))) {
	ArtTextAddLine(main_widgets.text, "[knews: temp file error.]",
		       ascii_font->body_font, global.alert_pixel);
	return None;
    }

    if (setjmp(p_str.jmpbuf))
	ArtTextAddLine(main_widgets.text, "[knews: png error.]",
		       ascii_font->body_font, global.alert_pixel);
    else {
	unsigned char	*pic, *row;
	long		w, h;
	int		did;
	unsigned int	per_line = 0;
	unsigned int	i, j, pass;

	png_read_init(&p_str);
	png_info_init(&p_info);

	png_init_io(&p_str, vol_fp);
	png_read_info(&p_str, &p_info);

	vol_w = w = p_info.width;
	vol_h = h = p_info.height;

	if (p_info.bit_depth == 16)
	    png_set_strip_16(&p_str);
	else if (p_info.bit_depth < 8)
	    png_set_packing(&p_str);

	if (p_info.valid & PNG_INFO_bKGD)
	    png_set_background(&p_str, &p_info.background,
			       PNG_BACKGROUND_GAMMA_FILE, True, 1.0);
	else {
	    static png_color_16	bg = {0, };
	    png_set_background(&p_str, &bg,
			       PNG_BACKGROUND_GAMMA_SCREEN, False, 1.0);
	}

	per_line = w;

	if (!(p_info.color_type & PNG_COLOR_MASK_COLOR)) { /* grey image */
	    grey = True;
	    png_set_expand(&p_str);
	} else if (!p_cmap) { /* true color visual */
	    if (p_info.color_type == PNG_COLOR_TYPE_PALETTE)
		png_set_expand(&p_str);
	    per_line *= 3;
	} else if (p_info.color_type & PNG_COLOR_MASK_PALETTE) {
	    CMAP_ENTRY	*pal;
	    int		i, pn;

	    pn  = p_info.num_palette;
	    pal = (CMAP_ENTRY *)XtMalloc(pn * sizeof *pal);
	    for (i = 0 ; i < pn ; i++) {
		pal[i].r = p_info.palette[i].red;
		pal[i].g = p_info.palette[i].green;
		pal[i].b = p_info.palette[i].blue;
	    }
	    vol_pal = pal;
	    vol_pn  = pn;
	} else {
	    png_set_dither(&p_str, p_cmap, cmap_size,
			   cmap_size, NULL, True);
	}

	pass = png_set_interlace_handling(&p_str);
	png_start_read_image(&p_str);

	vol_pic = pic = (unsigned char *)XtMalloc(h * per_line);

	did = False;
	for (i = 0 ; i < pass ; i++) {
	    row = pic;
	    for (j = 0 ; j < h ; j++) {
		png_read_row(&p_str, NULL, row);
		if (!did)
		    vol_did = did = True;
		row += per_line;
	    }
	}

	png_read_end(&p_str, NULL);
    }

    if (!vol_did)
	pixmap = None;
    else {
	unsigned char	*pic = vol_pic;
	CMAP_ENTRY	*pal = vol_pal;
	unsigned int	w    = vol_w;
	unsigned int	h    = vol_h;

	*wp = w;
	*hp = h;

	if (grey)
	    pixmap = put_grey_image(pic, w, h);
	else if (!p_cmap)
	    pixmap = put_24_image(pic, w, h);
	else if (pal)
	    pixmap = put_cmapped_image(pic, w, h, pal, vol_pn);
	else {
	    long	i, n = w * h;

	    for (i = 0 ; i < n ; i++)
		pic[i] = cmap[pic[i]].pixel;

	    pixmap = put_8_image(pic, w, h);
	}
    }

    png_read_destroy(&p_str, &p_info, NULL);
    fclose((FILE *)vol_fp);
    XtFree((char *)vol_pic);
    XtFree((char *)vol_pal);

    return pixmap;
}

#endif
