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

/*
 *  From the gif89a spec:
 *
 *      "The Graphics Interchange Format(c) is the Copyright property of
 *      CompuServe Incorporated. GIF(sm) is a Service Mark property of
 *      CompuServe Incorporated."
 */


#include "global.h"
#include "color.h"
#include "font.h"
#include "gif.h"
#include "gif_I.h"
#include "widgets.h"
#include "../Widgets/ArtText.h"

Pixmap do_gif(char *data, long len, long *wp, long *hp)
{
    Pixmap		pixmap;
    unsigned char	*pic;
    struct gif_info	g_str;
    long		w, h, n, size;

    gif_init(&g_str, (unsigned char *)data, len);

    if (gif_read_header(&g_str) < 0) {
	ArtTextAddLine(main_widgets.text, "[knews: not a gif.]",
		       ascii_font->body_font, global.alert_pixel);
	return None;
    }

    if (g_str.cmap_size == 0) {
	ArtTextAddLine(main_widgets.text, "[knews: no colormap in gif.]",
		       ascii_font->body_font, global.alert_pixel);
	return None;
    }

    w = *wp = g_str.width;
    h = *hp = g_str.height;
    size = w * h;
    pic = (unsigned char *)XtMalloc(size);


    n = gif_read_image(&g_str, pic);
    if (n < size)
	if (!g_str.err_str)
	    ArtTextAddLine(main_widgets.text, "[knews: truncated gif stream.]",
			   ascii_font->body_font, global.alert_pixel);
	else {
	    ArtTextAddLine(main_widgets.text, "[knews: ",
			   ascii_font->body_font, global.alert_pixel);
	    ArtTextAppendToLast(main_widgets.text, g_str.err_str);
	    ArtTextAppendToLast(main_widgets.text, ".]");
	}

    pixmap = put_cmapped_image(pic, w, h, g_str.cmap, g_str.cmap_size);

    XtFree((char *)pic);

    return pixmap;
}

/********************************************************************/

static int init_decoder(struct gif_info *g)
{
    unsigned int	i;

    if (g->src_len <= 0)
	return -1;

    g->min_code_size  = NEXT_BYTE(g);
    if (g->min_code_size < 2 || g->min_code_size > 11) {
	g->err_str = "nonsensical code size in gif image";
	return -1;
    }
    g->clear_code     = 1u << g->min_code_size;
    g->end_code       = g->clear_code + 1;
    g->code_size      = g->min_code_size + 1;
    g->code_mask      = (1u << g->code_size) - 1;
    g->table_size     = g->end_code + 1;
    g->prev_code      = NULL_CODE;

    g->bits   = 0;
    g->n_bits = 0;
    g->n_buf  = 0;
    g->sp     = 0;
    for (i = 0 ; i < TABLE_SIZE ; i++) {
	g->table[i].prefix = NULL_CODE;
	g->table[i].suffix = i;
    }

    return 0;
}

static int push_string(struct gif_info *g, unsigned int code)
{
    unsigned int	i = 0;

    while (code < TABLE_SIZE && g->table[code].prefix != NULL_CODE) {
	if (++i == 0xffffu) {
	    g->err_str = "gif decoder infinite loop";
	    return -1;
	}
	if (g->sp >= STACK_SIZE)
	    break;
	g->stack[g->sp++] = g->table[code].suffix;
	code = g->table[code].prefix;
    }

    if (code >= TABLE_SIZE) {
	g->err_str = "gif decode table overflow";
	return -1;
    }
    if (g->sp >= STACK_SIZE) {
	g->err_str = "gif decode stack overflow";
	return -1;
    }

    return g->stack[g->sp++] = g->table[code].suffix;
}

static int get_byte(struct gif_info *g)
{
    int		code;

    while (g->sp == 0) {
	while (g->n_bits < g->code_size) {
	    if (g->n_buf == 0) {
		if (g->src_len <= 0)
		    return -1;
		g->n_buf = NEXT_BYTE(g);
		if (g->n_buf == 0)
		    return -1;  /* end of data */
		g->buf = g->src_buf;
		g->src_buf += g->n_buf;
		g->src_len -= g->n_buf;
	    }

	    g->n_buf--;
	    g->bits   |= *g->buf++ << g->n_bits;
	    g->n_bits += 8;
	}

	code = g->bits & g->code_mask;
	g->bits >>= g->code_size;
	g->n_bits -= g->code_size;

	if (code == g->end_code)
	    return -1;

	if (code == g->clear_code) {
	    g->code_size  = g->min_code_size + 1;
	    g->code_mask  = (1u << g->code_size) - 1;
	    g->prev_code  = NULL_CODE;
	    g->table_size = g->end_code + 1;
	} else if (g->prev_code == NULL_CODE) {
	    if (push_string(g, code) < 0)
		return -1;
	    g->prev_code = code;
	} else {
	    int	first, sp;

	    if (code < g->table_size)
		first = push_string(g, code);
	    else {
		sp = g->sp;
		if (g->sp >= STACK_SIZE) {
		    g->err_str = "gif decode stack overflow";
		    return -1;
		}
		g->stack[g->sp++] = -1;
		first = push_string(g, g->prev_code);
		if (first < 0)
		    return -1;
		g->stack[sp] = first;
	    }

	    if (g->table_size >= TABLE_SIZE) {
		g->err_str = "gif decode table overflow";
		return -1;
	    }

	    g->table[g->table_size].prefix = g->prev_code;
	    g->table[g->table_size].suffix = first;
	    if (g->table_size == g->code_mask && g->code_size < 12) {
		g->code_size++;
		g->code_mask = (1u << g->code_size) - 1;
	    }
	    g->table_size++;

	    g->prev_code = code;
	}
    }

    return g->stack[--g->sp];
}


static int read_cmap(struct gif_info *g, int misc_bits)
{
    unsigned int	i;

    if (!(misc_bits & 0x80))
	return 0;

    g->cmap_size = 1u << ((misc_bits & 0x07) + 1);
    g->src_len -= 3 * g->cmap_size;
    if (g->src_len < 0)
	return -1;

    for (i = 0 ; i < g->cmap_size ; i++) {
	g->cmap[i].r = *g->src_buf++;
	g->cmap[i].g = *g->src_buf++;
	g->cmap[i].b = *g->src_buf++;
    }

    return 0;
}

static int read_image_header(struct gif_info *g)
{
    unsigned int	misc_bits;

    if (g->src_len < 9)
	return -1;

    (void)NEXT_BYTE(g); (void)NEXT_BYTE(g);  /* image x pos */
    (void)NEXT_BYTE(g); (void)NEXT_BYTE(g);  /* image y pos */
    g->width   = NEXT_BYTE(g);
    g->width  += NEXT_BYTE(g) << 8;
    g->height  = NEXT_BYTE(g);
    g->height += NEXT_BYTE(g) << 8;
    misc_bits  = NEXT_BYTE(g);

    g->interlaced = (misc_bits & 0x40) ? True : False;

    if (read_cmap(g, misc_bits) < 0) /* local colormap */
	return -1;

    return 0;
}

static int skip_extension(struct gif_info *g)
{
    unsigned int	n;

    (void)NEXT_BYTE(g);  /* type of extension */
    if (g->src_len < 0)
	return -1;

    while ((n = NEXT_BYTE(g)) != 0) {
	g->src_buf += n;
	g->src_len -= n;
	if (g->src_len <= 0)
	    return -1;
    }

    return 0;
}

int gif_read_header(struct gif_info *g)
{
    unsigned int	misc_bits;

    if (g->src_len < 13 ||
	(memcmp(g->src_buf, "GIF89a", 6) != 0 &&
	 memcmp(g->src_buf, "GIF87a", 6) != 0))
	return -1;

    g->src_buf += 6;
    g->src_len -= 6;

    /*
     *  Ignore junk in screen descriptor.
     */
    (void)NEXT_BYTE(g); (void)NEXT_BYTE(g);  /* screen width  */
    (void)NEXT_BYTE(g); (void)NEXT_BYTE(g);  /* screen height */
    misc_bits = NEXT_BYTE(g);
    (void)NEXT_BYTE(g);                      /* background    */
    (void)NEXT_BYTE(g);                      /* aspect ratio  */

    if (read_cmap(g, misc_bits) < 0) /* global colormap */
	return -1;

    while (g->src_len-- > 0)
	switch (*g->src_buf++) {
	case ';': /* Terminator */
	    return -1;
	case '!': /* Extension */
	    if (skip_extension(g) < 0)
		return -1;
	    break;
	case ',': /* Start of first image */
	    return read_image_header(g);
	default:  /* Invalid */
	    return -1;
	}

    return -1;
}

long gif_read_image(struct gif_info *g, unsigned char *dest)
{
    unsigned int	w = g->width;
    unsigned int	h = g->height;
    long		n = 0;
    int			byte;

    if (init_decoder(g) < 0)
	return 0;

    if (!g->interlaced) {
	long	size = w * h;

	while (n < size) {
	    byte = GET_BYTE(g);
	    if (byte < 0)
		return n;
	    dest[n++] = byte;
	}
    } else {
	static const char	start[] = {0, 4, 2, 1};
	static const char	step[]  = {8, 8, 4, 2};
	unsigned int	pass, x, y;
	long		n_step;

	for (pass = 0 ; pass < 4 ; pass++) {
	    n      = start[pass] * w;
	    n_step = (step[pass] - 1) * w;
	    for (y = start[pass] ; y < h ; y += step[pass], n += n_step)
		for (x = 0 ; x < w ; x++) {
		    byte = get_byte(g);
		    if (byte < 0)
			return n;
		    dest[n++] = byte;
		}
	}
    }

    return n;
}

void gif_init(struct gif_info *g, const unsigned char *data, long len)
{
    g->src_buf = data;
    g->src_len = len;
    g->err_str = NULL;
}
