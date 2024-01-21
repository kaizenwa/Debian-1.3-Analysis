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
 * SOFTWARE IS AT THE USERS OWN RISK.
 */
#include "global.h"
#include "../Widgets/Compat.h"
#include "color.h"
#include "util.h"
#include "widgets.h"

static void set_defaults(Screen *screen)
{
    global.visual = DefaultVisualOfScreen(screen);
    global.cmap   = DefaultColormapOfScreen(screen);
    global.depth  = DefaultDepthOfScreen(screen);
}

static int find_visual(Display *disp, Screen *screen, int class, int depth)
{
    XVisualInfo	info;
    int		screen_no = XScreenNumberOfScreen(screen);

    if (!XMatchVisualInfo(disp, screen_no, depth, class, &info))
	return False;

    global.visual = info.visual;

    return True;
}

static char *get_resource(XrmDatabase db, char *res_name, char *res_class)
{
    XrmValue		val;
    XrmRepresentation	rep;
    XrmQuark		name[3], class[3];

    name[0] = XrmPermStringToQuark("knews");
    name[1] = XrmPermStringToQuark(res_name);
    name[2] = 0;
    class[0] = XrmPermStringToQuark("Knews");
    class[1] = XrmPermStringToQuark(res_class);
    class[2] = 0;

    if (XrmQGetResource(db, name, class, &rep, &val))
	return (char *)val.addr;

    return NULL;
}

static int str_to_class(char *class)
{
#define DO(name) if (strcmp(class, #name) == 0) return name;
    DO(StaticGray);
    DO(GrayScale);
    DO(StaticColor);
    DO(PseudoColor);
    DO(TrueColor);
    DO(DirectColor);
#undef DO

    return -1;
}

void color_init(Display *disp)
{
    Screen		*screen = DefaultScreenOfDisplay(disp);
    XrmDatabase		db = XtScreenDatabase(screen);
    char		*visual_class = NULL;
    char		*visual_depth = NULL;
    char		*install      = NULL;
    int			depth, class;

    set_defaults(screen);
    if (!db)
	return;

    visual_class = get_resource(db, "visualClass", "VisualClass");
    visual_depth = get_resource(db, "visualDepth", "VisualDepth");

    if (!visual_depth)
	depth = global.depth;
    else if (sscanf(visual_depth, "%d", &depth) != 1) {
	fprintf(stderr, "knews: visualDepth is not a number: %s\n",
		visual_depth);
	return;
    }

    if (!visual_class)
	class = -1;
    else if ((class = str_to_class(visual_class)) < 0) {
	fprintf(stderr, "knews: no visual type called %s.\n", visual_class);
	return;
    }

    if (visual_class) {
	if (!find_visual(disp, screen, class, depth)) {
	    fprintf(stderr,
		    "knews: couldn't find %d-bit %s visual, using default.\n",
		    depth, visual_class);
	    return;
	}
    } else if (visual_depth) {
	fputs("knews: visualDepth given without visualClass, ignoring.\n",
	      stderr);
	return;
    } else {
	install = get_resource(db, "installCmap", "InstallCmap");
	if (!install || case_lstrcmp(install, "true") != 0)
	    return;
    }

    global.cmap = XCreateColormap(disp, RootWindowOfScreen(screen),
				  global.visual, AllocNone);
}

/*********************************************************************/

CMAP_ENTRY	*cmap      = NULL;
int		 cmap_size = 0;

static unsigned int	 r_sh_l = 0;
static unsigned int	 r_sh_r = 0;
static unsigned int	 g_sh_l = 0;
static unsigned int	 g_sh_r = 0;
static unsigned int	 b_sh_l = 0;
static unsigned int	 b_sh_r = 0;
static unsigned long	 red_mask;
static unsigned long	 green_mask;
static unsigned long	 blue_mask;

#define RGB_TO_PIXEL(r, g, b)	((((r) >> r_sh_r) << r_sh_l) |  \
				 (((g) >> g_sh_r) << g_sh_l) |  \
				 (((b) >> b_sh_r) << b_sh_l))

#define N_GREYS		17

static void disable_images(char *msg)
{
    fprintf(stderr, "knews: %s, disabling inline images.\n", msg);
    global.inline_images = False;
}

static void free_pixels(CMAP_ENTRY *cols, unsigned int n)
{
    unsigned long	pixels[256];
    unsigned int	i;

    if (n == 0)
	return;

    for (i = 0 ; i < n ; i++)
	pixels[i] = cols[i].pixel;
    XFreeColors(display, global.cmap, pixels, n, 0);
}

static void set_shifts(unsigned long  mask,
		       unsigned int  *lp,
		       unsigned int  *rp)
{
    unsigned int	n_ones = 0, n_zeroes = 0;

    while (!(mask & 1)) {
	mask >>= 1;
	n_zeroes++;
    }
    mask++;
    while (!(mask & 1)) {
	mask >>= 1;
	n_ones++;
    }

    if (n_ones < 8) {
	*rp = 8 - n_ones;
	*lp = n_zeroes;
    } else {
	*rp = 0;
	*lp = n_zeroes + n_ones - 8;
    }
}

static void query_cmap(void)
{
    XColor		cols[256];
    unsigned int	i;

    cmap_size = global.visual->map_entries;
    if (cmap_size > 256) /* should not happen */
	cmap_size = 256;

    for (i = 0 ; i < cmap_size ; i++) {
	cols[i].pixel = i;
	cols[i].flags = DoRed|DoGreen|DoBlue;
    }
    XQueryColors(display, global.cmap, cols, cmap_size);

    cmap = (CMAP_ENTRY *)XtMalloc(cmap_size * sizeof cmap[0]);
    for (i = 0 ; i < cmap_size ; i++) {
	cmap[i].pixel = cols[i].pixel;
	cmap[i].r = cols[i].red   / 256;
	cmap[i].g = cols[i].green / 256;
	cmap[i].b = cols[i].blue  / 256;
    }
}

static int alloc_greys(CMAP_ENTRY *cmap, unsigned int n)
{
    XColor		cols[256];
    unsigned int	i;

    for (i = 0 ; i < n ; i++) {
	cols[i].red = cols[i].green = cols[i].blue = i * 65535ul / (n - 1);
	cols[i].flags = DoRed|DoGreen|DoBlue;
    }

    for (i = 0 ; i < n ; i++)
	if (XAllocColor(display, global.cmap, cols + i)) {
	    cmap[i].pixel = cols[i].pixel;
	    cmap[i].r = cmap[i].g = cmap[i].b = 
		(cols[i].red + cols[i].green + cols[i].blue) / (3 * 256);
	} else {
	    free_pixels(cmap, i);
	    return False;
	}

    return True;
}

static int alloc_grey_ramp(unsigned int max_size)
{
    cmap_size = global.visual->map_entries;
    if (cmap_size > max_size)
	cmap_size = max_size;
    if (cmap_size > 256)
	cmap_size = 256;

    cmap = (CMAP_ENTRY *)XtMalloc(cmap_size * sizeof cmap[0]);
    while (cmap_size >= N_GREYS)
	if (alloc_greys(cmap, cmap_size))
	    break;
	else
	    cmap_size -= 8;

    if (cmap_size >= N_GREYS)
	return True;

    disable_images("Couldn't allocate greys");
    XtFree((char *)cmap);
    cmap = NULL;
    cmap_size = 0;

    return False;
}

static unsigned long rgb_dist(const CMAP_ENTRY *c1, const CMAP_ENTRY *c2)
{
    unsigned long	d = 0;
    int			tmp;

    tmp  = (int)c1->r - (int)c2->r;
    d   += tmp * tmp;
    tmp  = (int)c1->g - (int)c2->g;
    d   += tmp * tmp;
    tmp  = (int)c1->b - (int)c2->b;
    d   += tmp * tmp;

    return d;
}

static void compute_dither(unsigned char *dither,
			   CMAP_ENTRY    *from,
			   unsigned int   n_from,
			   CMAP_ENTRY    *to,
			   unsigned int   n_to)
{
    unsigned int	i, j;

    for (i = 0 ; i < n_from ; i++) {
	unsigned int	min = 0;
	unsigned long	d   = rgb_dist(from + i, to);

	for (j = 1 ; j < n_to ; j++) {
	    unsigned long	d1 = rgb_dist(from + i, to + j);

	    if (d1 < d) {
		d = d1;
		min = j;
	    }
	}
	dither[i] = to[min].pixel;
    }
}

static int alloc_rgb_cube(CMAP_ENTRY	*cols,
			  unsigned int	 n_reds,
			  unsigned int	 n_greens,
			  unsigned int	 n_blues)
{
    XColor		col;
    unsigned int	r, g, b, n = 0;

    n_reds--;
    n_greens--;
    n_blues--;

    for (r = 0 ; r <= n_reds ; r++)
	for (g = 0 ; g <= n_greens ; g++)
	    for (b = 0 ; b <= n_blues ; b++) {
		col.red   = r * 65535ul / n_reds;
		col.green = g * 65535ul / n_greens;
		col.blue  = b * 65535ul / n_blues;

		if (col.red == col.green && col.green == col.blue)
		    continue;

		if (!XAllocColor(display, global.cmap, &col)) {
		    free_pixels(cols, n);
#if 0
		    fprintf(stderr, "knews: Failed to alloc %ux%ux%u RGB "
			    "cube.\n", ++n_reds, ++n_greens, ++n_blues);
#endif
		    return 0;
		}

		cols[n].pixel = col.pixel;
		cols[n].r = col.red   / 256;
		cols[n].g = col.green / 256;
		cols[n].b = col.blue  / 256;
		n++;
	    }

    return n;
}

static int alloc_rgb_colors(int max_size)
{
    static struct {
	unsigned char	n_r, n_g, n_b;
    } c[] = {
	{7, 8, 4},
	{6, 6, 6},
	{6, 7, 5},
	{4, 8, 4},
	{5, 5, 5},
	{4, 4, 4},
	{4, 4, 3},
	{3, 4, 3},
	{3, 3, 3},
    };
    XColor		col;
    unsigned int	i, n = 0;

    cmap = (CMAP_ENTRY *)XtMalloc(256 * sizeof cmap[0]);

    for (n = 0 ; n < N_GREYS ; n++) {
	col.red = col.green = col.blue = n * 65535ul / (N_GREYS - 1);
	col.flags = DoRed|DoGreen|DoBlue;

	if (!XAllocColor(display, global.cmap, &col)) {
	    free_pixels(cmap, n);
	    XtFree((char *)cmap);
	    cmap = NULL;
	    return False;
	}

	cmap[n].pixel = col.pixel;
	cmap[n].r = col.red   / 256;
	cmap[n].g = col.green / 256;
	cmap[n].b = col.blue  / 256;
    }

    max_size -= n;

    for (i = 0 ; i < XtNumber(c) ; i++) {
	int	cn = c[i].n_r * c[i].n_g * c[i].n_b;

	if (cn > max_size)
	    continue;
	cn = alloc_rgb_cube(cmap + n, c[i].n_r, c[i].n_g, c[i].n_b);
	if (cn == 0)
	    continue;

	n += cn;
	break;
    }

    cmap_size = n;
    cmap = (CMAP_ENTRY *)XtRealloc((char *)cmap, cmap_size * sizeof cmap[0]);

    return True;
}

static void color_hack(unsigned int max_size)
{
    XColor		cols[256];
    unsigned int	i;

    if (cmap_size > 256)
	cmap_size = 256;

    for (i = 0 ; i < cmap_size ; i++) {
	cols[i].pixel = i;
	cols[i].flags = DoRed|DoGreen|DoBlue;
    }
    XQueryColors(display, global.cmap, cols, cmap_size);

    cmap = (CMAP_ENTRY *)XtMalloc(cmap_size * sizeof cmap[0]);
    for (i = 0 ; i < cmap_size ; i++) {
	(void)XAllocColor(display, global.cmap, cols + i);
	cmap[i].pixel = cols[i].pixel;
	cmap[i].r = cols[i].red   / 256;
	cmap[i].g = cols[i].green / 256;
	cmap[i].b = cols[i].blue  / 256;
    }
}

void alloc_colors(void)
{
    red_mask   = global.visual->red_mask;
    green_mask = global.visual->green_mask;
    blue_mask  = global.visual->blue_mask;

    if (!global.inline_images || global.n_cols <= 0) {
	global.n_cols = 0;
	global.inline_images = False;
	return;
    }

    if (global.depth < 4) {
	disable_images("display depth too small");
	return;
    }

    if (global.n_cols > 256)
	global.n_cols = 256;

    switch (global.visual->class) {
    case StaticGray:
	if (global.depth > 8) {
	    disable_images("StaticGray visual deeper than 8 bits");
	    return;
	}
	query_cmap();
	break;
    case GrayScale:
	if (global.color_hack)
	    color_hack(global.n_cols);
	else if (!alloc_grey_ramp(global.n_cols)) {
	    fputs("knews: Couldn't allocate sufficient colors, "
		  "trying colorHack.\n", stderr);
	    color_hack(global.n_cols);
	}
	break;
    case StaticColor:
	if (global.depth > 8) {
	    disable_images("StaticColor visual deeper than 8 bits");
	    return;
	}
	query_cmap();
	break;
    case PseudoColor:
	if (global.color_hack)
	    color_hack(global.n_cols);
	else if (!alloc_rgb_colors(global.n_cols)) {
	    fputs("knews: Couldn't allocate sufficient colors, "
		  "trying colorHack.\n", stderr);
	    color_hack(global.n_cols);
	}
	break;
    case TrueColor:
	set_shifts(red_mask,   &r_sh_l, &r_sh_r);
	set_shifts(green_mask, &g_sh_l, &g_sh_r);
	set_shifts(blue_mask,  &b_sh_l, &b_sh_r);
	break;
    case DirectColor:
	disable_images("DirectColor visuals are weird");
	break;
    }

#if 0
    if (cmap_size > 0)
	fprintf(stderr, "knews: allocated %d colors.\n", cmap_size);
#endif
}

/*********************************************************************/

Pixmap put_8_image(unsigned char *pic, long w, long h)
{
    Pixmap	pixmap;
    XImage	image;

    /*
     *  Hope this works...
     */
    image.width            = w;
    image.height           = h;
    image.xoffset          = 0;
    image.format           = ZPixmap;
    image.data             = (char *)pic;
    image.byte_order       = ImageByteOrder(display);
    image.bitmap_unit      = BitmapUnit(display);
    image.bitmap_bit_order = BitmapBitOrder(display);
    image.bitmap_pad       = 8;
    image.depth            = global.depth;
    image.bytes_per_line   = w;
    image.bits_per_pixel   = 8;
    image.red_mask         = red_mask;
    image.green_mask       = green_mask;
    image.blue_mask        = blue_mask;
    image.obdata           = NULL;
    image.f.create_image   = NULL;
    image.f.destroy_image  = NULL;
    image.f.get_pixel      = NULL;
    image.f.put_pixel      = NULL;
    image.f.sub_image      = NULL;
    image.f.add_pixel      = NULL;

    pixmap = XCreatePixmap(display, XtWindow(main_widgets.shell),
			   w, h, global.depth);
    XPutImage(display, pixmap, global.gc, &image, 0, 0, 0, 0, w, h);

    return pixmap;
}

static XImage *alloc_ximage(long w, long h)
{
    XImage	*img;

    img = XCreateImage(display, global.visual, global.depth,
		       ZPixmap, 0, NULL, w, h, BitmapPad(display), 0);
    img->data = XtMalloc(h * img->bytes_per_line);

    return img;
}

static Pixmap image_to_pixmap(XImage *img, long w, long h)
{
    Window	win    = XtWindow(main_widgets.shell);
    Pixmap	pixmap = XCreatePixmap(display, win, w, h, global.depth);

    XPutImage(display, pixmap, global.gc, img, 0, 0, 0, 0, w, h);
    XDestroyImage(img);

    return pixmap;
}

/*
 *  Put TrueColor image.
 */
Pixmap put_24_image(unsigned char *pic, long w, long h)
{
    XImage	*img = alloc_ximage(w, h);
    long	x, y;

    for (y = 0 ; y < h ; y++)
	for (x = 0 ; x < w ; x++, pic += 3)
	    XPutPixel(img, x, y, RGB_TO_PIXEL(pic[0], pic[1], pic[2]));

    return image_to_pixmap(img, w, h);
}

Pixmap put_grey_image(unsigned char *pic, long w, long h)
{
    if (cmap) {
	unsigned char	dither[256];
	CMAP_ENTRY	greys[256];
	long		i, n = w * h;

	for (i = 0 ; i < 256 ; i++) {
	    greys[i].pixel = i;
	    greys[i].r = greys[i].g = greys[i].b = i;
	}
	compute_dither(dither, greys, 256, cmap, cmap_size);

	for (i = 0 ; i < n ; i++)
	    pic[i] = dither[pic[i]];

	return put_8_image(pic, w, h);
    } else {
	XImage	*img = alloc_ximage(w, h);
	long	x, y;

	for (y = 0 ; y < h ; y++)
	    for (x = 0 ; x < w ; x++, pic++)
		XPutPixel(img, x, y, RGB_TO_PIXEL(*pic, *pic, *pic));

	return image_to_pixmap(img, w, h);
    }
}

Pixmap put_cmapped_image(unsigned char	*pic,
			 long		 w,
			 long		 h,
			 CMAP_ENTRY	*pal,
			 unsigned int	 pal_size)
{
    if (cmap) {
	long		i, n = w * h;
	unsigned char	dither[256] = {0, };

	compute_dither(dither, pal, pal_size, cmap, cmap_size);
	for (i = 0 ; i < n ; i++)
	    pic[i] = dither[pic[i]];

	return put_8_image(pic, w, h);
    } else {
	XImage	*img = alloc_ximage(w, h);
	long	x, y;

	for (y = 0 ; y < h ; y++)
	    for (x = 0 ; x < w ; x++, pic++)
		XPutPixel(img, x, y,
			  RGB_TO_PIXEL(pal[*pic].r, pal[*pic].g, pal[*pic].b));

	return image_to_pixmap(img, w, h);
    }
}

Pixel get_closest_color(XColor *col)
{
    CMAP_ENTRY		rgb;
    unsigned char	dither;

    if (!global.inline_images)
	return global.default_hot_pixel;

    rgb.r = col->red   / 256;
    rgb.g = col->green / 256;
    rgb.b = col->blue  / 256;

    if (!cmap)
	return RGB_TO_PIXEL(rgb.r, rgb.g, rgb. b);
    else {
	compute_dither(&dither, &rgb, 1, cmap, cmap_size);
	return dither;
    }
}
