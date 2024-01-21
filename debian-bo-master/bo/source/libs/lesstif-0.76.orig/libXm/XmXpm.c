/*
 * Copyright (C) 1989-95 GROUPE BULL
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * GROUPE BULL BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Except as contained in this notice, the name of GROUPE BULL shall not be
 * used in advertising or otherwise to promote the sale, use or other dealings
 * in this Software without prior written authorization from GROUPE BULL.
 */

#include <LTconfig.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_STRING_H
#include <string.h>
#define ANSI_STRING
#else
#include <strings.h>
#endif
#include <X11/Xfuncs.h>
#ifdef VMS
#include <unixio.h>
#include <file.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>

#include <X11/Xlib.h>
#include <X11/IntrinsicP.h>

#include <XmI/XmI.h>
#include <XmI/XmXpm.h>

typedef struct {
    int cols_index;
    long closeness;
} CloseColor;

#define XPMMAXCMTLEN BUFSIZ
typedef struct {
    unsigned int type;
    union {
	FILE *file;
	char **data;
    }     stream;
    char *cptr;
    unsigned int line;
    int CommentLength;
    char Comment[XPMMAXCMTLEN];
    char *Bcmt, *Ecmt, Bos, Eos;
    int format;			/* 1 if XPM1, 0 otherwise */
} xpmData;

#define XPMARRAY 0
#define XPMFILE  1
#define XPMPIPE  2
#define XPMBUFFER 3

#define EOL '\n'
#define TAB '\t'
#define SPC ' '

typedef struct {
    char *type;			/* key word */
    char *Bcmt;			/* string beginning comments */
    char *Ecmt;			/* string ending comments */
    char Bos;			/* character beginning strings */
    char Eos;			/* character ending strings */
    char *Strs;			/* strings separator */
    char *Dec;			/* data declaration string */
    char *Boa;			/* string beginning assignment */
    char *Eoa;			/* string ending assignment */
} xpmDataType;

static xpmDataType xpmDataTypes[] =
{
    { "", "!", "\n", '\0', '\n', "", "", "", "" },	/* Natural type */
    { "C", "/*", "*/", '"', '"', ",\n", "static char *", "[] = {\n", "};\n" },
    { "Lisp", ";", "\n", '"', '"', "\n", "(setq ", " '(\n", "))\n" },
#ifdef VMS
    { NULL }
#else
    { NULL, NULL, NULL, 0, 0, NULL, NULL, NULL, NULL }
#endif
};

/*
 * rgb values and ascii names (from rgb text file) rgb values,
 * range of 0 -> 65535 color mnemonic of rgb value
 */
typedef struct {
    int r, g, b;
    char *name;
} xpmRgbName;

/* Maximum number of rgb mnemonics allowed in rgb text file. */
#define MAX_RGBNAMES 1024

static char *_XmxpmColorKeys[] = {
    "s",				/* key #1: symbol */
    "m",				/* key #2: mono visual */
    "g4",				/* key #3: 4 grays visual */
    "g",				/* key #4: gray visual */
    "c",				/* key #5: color visual */
};

#define TRANSPARENT_COLOR "None"	/* this must be a string! */

/* number of _XmxpmColorKeys */
#define NKEYS 5

/*
 * Macros
 *
 * The XYNORMALIZE macro determines whether XY format data requires
 * normalization and calls a routine to do so if needed. The logic in
 * this module is designed for LSBFirst byte and bit order, so
 * normalization is done as required to present the data in this order.
 *
 * The ZNORMALIZE macro performs byte and nibble order normalization if
 * required for Z format data.
 *
 * The XYINDEX macro computes the index to the starting byte (char) boundary
 * for a bitmap_unit containing a pixel with coordinates x and y for image
 * data in XY format.
 *
 * The ZINDEX* macros compute the index to the starting byte (char) boundary
 * for a pixel with coordinates x and y for image data in ZPixmap format.
 *
 */

#define XYNORMALIZE(bp, img) \
    if ((img->byte_order == MSBFirst) || (img->bitmap_bit_order == MSBFirst)) \
	_Xmxpm_xynormalizeimagebits((unsigned char *)(bp), img)

#define ZNORMALIZE(bp, img) \
    if (img->byte_order == MSBFirst) \
	_Xmxpm_znormalizeimagebits((unsigned char *)(bp), img)

#define XYINDEX(x, y, img) \
    ((y) * img->bytes_per_line) + \
    (((x) + img->xoffset) / img->bitmap_unit) * (img->bitmap_unit >> 3)

#define ZINDEX(x, y, img) ((y) * img->bytes_per_line) + \
    (((x) * img->bits_per_pixel) >> 3)

#define ZINDEX32(x, y, img) ((y) * img->bytes_per_line) + ((x) << 2)

#define ZINDEX16(x, y, img) ((y) * img->bytes_per_line) + ((x) << 1)

#define ZINDEX8(x, y, img) ((y) * img->bytes_per_line) + (x)

#define ZINDEX1(x, y, img) ((y) * img->bytes_per_line) + ((x) >> 3)

/*
 * there are structures and functions related to hastable code
 */
typedef struct _xpmHashAtom {
    char *name;
    void *data;
} *xpmHashAtom;

typedef struct {
    int size;
    int limit;
    int used;
    xpmHashAtom *atomTable;
} xpmHashTable;

#define HashAtomData(i) ((void *)i)
#define HashColorIndex(slot) ((unsigned int)((*slot)->data))
#define USE_HASHTABLE (cpp > 2 && ncolors > 4)

/*
 * Hash function definition:
 * HASH_FUNCTION: hash function, hash = hashcode, hp = pointer on char,
 *				 hash2 = temporary for hashcode.
 * INITIAL_TABLE_SIZE in slots
 * HASH_TABLE_GROWS how hash table grows.
 */
/* Mock lisp function */
#define HASH_FUNCTION 	  hash = (hash << 5) - hash + *hp++;
/* #define INITIAL_HASH_SIZE 2017 */
#define INITIAL_HASH_SIZE 256		/* should be enough for colors */
#define HASH_TABLE_GROWS  size = size * 2;

/* aho-sethi-ullman's HPJ (sizes should be primes)*/
#ifdef notdef
#define HASH_FUNCTION	hash <<= 4; hash += *hp++; \
    if(hash2 = hash & 0xf0000000) hash ^= (hash2 >> 24) ^ hash2;
#define INITIAL_HASH_SIZE 4095		/* should be 2^n - 1 */
#define HASH_TABLE_GROWS  size = size << 1 + 1;
#endif

/* GNU emacs function */
/*
#define HASH_FUNCTION 	  hash = (hash << 3) + (hash >> 28) + *hp++;
#define INITIAL_HASH_SIZE 2017
#define HASH_TABLE_GROWS  size = size * 2;
*/

/*
 * The hash table is used to store atoms via their NAME:
 *
 * NAME --hash--> ATOM |--name--> "foo"
 *		       |--data--> any value which has to be stored
 *
 */

/*
 * basic IO
 */
#define xpmGetC(mdata) \
	((!mdata->type || mdata->type == XPMBUFFER) ? \
	 (*mdata->cptr++) : (getc(mdata->stream.file)))

/*
 * The functions
 *
 * _XReverse_Bytes(bpt, nb)
 * _Xmxpm_xynormalizeimagebits(bp, img)
 * xpm_znormalizeimagebits(bp, img)
 *
 * are written from X11R5 MIT's code (XImUtil.c)
 *
 * The idea is to have faster functions than the standard XPutPixel function
 * to build the image data. Indeed we can speed up things by suppressing tests
 * performed for each pixel. We do the same tests but at the image level.
 * We also assume that we use only ZPixmap images with null offsets.
 */
static unsigned char _Xconst _reverse_byte[0x100] = {
    0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0,
    0x10, 0x90, 0x50, 0xd0, 0x30, 0xb0, 0x70, 0xf0,
    0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8,
    0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8,
    0x04, 0x84, 0x44, 0xc4, 0x24, 0xa4, 0x64, 0xe4,
    0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4,
    0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec,
    0x1c, 0x9c, 0x5c, 0xdc, 0x3c, 0xbc, 0x7c, 0xfc,
    0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2,
    0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2,
    0x0a, 0x8a, 0x4a, 0xca, 0x2a, 0xaa, 0x6a, 0xea,
    0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa,
    0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6,
    0x16, 0x96, 0x56, 0xd6, 0x36, 0xb6, 0x76, 0xf6,
    0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee,
    0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe,
    0x01, 0x81, 0x41, 0xc1, 0x21, 0xa1, 0x61, 0xe1,
    0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1,
    0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9,
    0x19, 0x99, 0x59, 0xd9, 0x39, 0xb9, 0x79, 0xf9,
    0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5,
    0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5,
    0x0d, 0x8d, 0x4d, 0xcd, 0x2d, 0xad, 0x6d, 0xed,
    0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd,
    0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3,
    0x13, 0x93, 0x53, 0xd3, 0x33, 0xb3, 0x73, 0xf3,
    0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb,
    0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb,
    0x07, 0x87, 0x47, 0xc7, 0x27, 0xa7, 0x67, 0xe7,
    0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7,
    0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef,
    0x1f, 0x9f, 0x5f, 0xdf, 0x3f, 0xbf, 0x7f, 0xff
};

static unsigned char _Xconst _lomask[0x09] = {
0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff};
static unsigned char _Xconst _himask[0x09] = {
0xff, 0xfe, 0xfc, 0xf8, 0xf0, 0xe0, 0xc0, 0x80, 0x00};

/*
 * scanning macros and data
 */
#define MAXPRINTABLE 92			/* number of printable ascii chars
					 * minus \ and " for string compat
					 * and ? to avoid ANSI trigraphs. */

static char *printable =
" .XoO+@#$%&*=-;:>,<1234567890qwertyuipasdfghjklzxcvbnmMNBVCZ\
ASDFGHJKLPIUYTREWQ!~^/()_`'][{}|";

/*
 * printable begin with a space, so in most case, due to my algorithm, when
 * the number of different colors is less than MAXPRINTABLE, it will give a
 * char follow by "nothing" (a space) in the readable xpm file
 */
typedef struct {
    Pixel *pixels;
    unsigned int *pixelindex;
    unsigned int size;
    unsigned int ncolors;
    unsigned int mask_pixel;		/* whether there is or not */
} PixelsMap;

static unsigned long _Xconst low_bits_table[] = {
    0x00000000, 0x00000001, 0x00000003, 0x00000007,
    0x0000000f, 0x0000001f, 0x0000003f, 0x0000007f,
    0x000000ff, 0x000001ff, 0x000003ff, 0x000007ff,
    0x00000fff, 0x00001fff, 0x00003fff, 0x00007fff,
    0x0000ffff, 0x0001ffff, 0x0003ffff, 0x0007ffff,
    0x000fffff, 0x001fffff, 0x003fffff, 0x007fffff,
    0x00ffffff, 0x01ffffff, 0x03ffffff, 0x07ffffff,
    0x0fffffff, 0x1fffffff, 0x3fffffff, 0x7fffffff,
    0xffffffff
};

/************************** LOW LEVEL FUNCTIONS ***************************/
static int
_XReverse_Bytes(bpt, nb)
    register unsigned char *bpt;
    register int nb;
{
    do {
	*bpt = _reverse_byte[*bpt];
	bpt++;
    } while (--nb > 0);
    return 0;
}


static void
_Xmxpm_xynormalizeimagebits(bp, img)
    register unsigned char *bp;
    register XImage *img;
{
    register unsigned char c;

    if (img->byte_order != img->bitmap_bit_order) {
	switch (img->bitmap_unit) {

	case 16:
	    c = *bp;
	    *bp = *(bp + 1);
	    *(bp + 1) = c;
	    break;

	case 32:
	    c = *(bp + 3);
	    *(bp + 3) = *bp;
	    *bp = c;
	    c = *(bp + 2);
	    *(bp + 2) = *(bp + 1);
	    *(bp + 1) = c;
	    break;
	}
    }
    if (img->bitmap_bit_order == MSBFirst)
	_XReverse_Bytes(bp, img->bitmap_unit >> 3);
}

static void
_Xmxpm_znormalizeimagebits(bp, img)
    register unsigned char *bp;
    register XImage *img;
{
    register unsigned char c;

    switch (img->bits_per_pixel) {

    case 2:
	_XReverse_Bytes(bp, 1);
	break;

    case 4:
	*bp = ((*bp >> 4) & 0xF) | ((*bp << 4) & ~0xF);
	break;

    case 16:
	c = *bp;
	*bp = *(bp + 1);
	*(bp + 1) = c;
	break;

    case 24:
	c = *(bp + 2);
	*(bp + 2) = *bp;
	*bp = c;
	break;

    case 32:
	c = *(bp + 3);
	*(bp + 3) = *bp;
	*bp = c;
	c = *(bp + 2);
	*(bp + 2) = *(bp + 1);
	*(bp + 1) = c;
	break;
    }
}

static void
_putbits(src, dstoffset, numbits, dst)
    register char *src;			/* address of source bit string */
    int dstoffset;			/* bit offset into destination;
					 * range is 0-31 */
    register int numbits;		/* number of bits to copy to
					 * destination */
    register char *dst;			/* address of destination bit string */
{
    register unsigned char chlo, chhi;
    int hibits;

    dst = dst + (dstoffset >> 3);
    dstoffset = dstoffset & 7;
    hibits = 8 - dstoffset;
    chlo = *dst & _lomask[dstoffset];
    for (;;) {
	chhi = (*src << dstoffset) & _himask[dstoffset];
	if (numbits <= hibits) {
	    chhi = chhi & _lomask[dstoffset + numbits];
	    *dst = (*dst & _himask[dstoffset + numbits]) | chlo | chhi;
	    break;
	}
	*dst = chhi | chlo;
	dst++;
	numbits = numbits - hibits;
	chlo = (unsigned char) (*src & _himask[hibits]) >> hibits;
	src++;
	if (numbits <= dstoffset) {
	    chlo = chlo & _lomask[numbits];
	    *dst = (*dst & _himask[numbits]) | chlo;
	    break;
	}
	numbits = numbits - dstoffset;
    }
}

/*
 * open the given file to be read as an xpmData which is returned.
 */
static int
OpenReadFile(filename, mdata)
    char *filename;
    xpmData *mdata;
{

    if (!filename)
    {
	mdata->stream.file = (stdin);
	mdata->type = XPMFILE;
    }
    else
    {
	if (!(mdata->stream.file = fopen(filename, "r")))
	{
	    return (XmXpmOpenFailed);
	}
	mdata->type = XPMFILE;
    }
    mdata->CommentLength = 0;
    return (XmXpmSuccess);
}

/*
 * open the given array to be read or written as an xpmData which is returned
 */
static void
OpenArray(data, mdata)
    char **data;
    xpmData *mdata;
{
    mdata->type = XPMARRAY;
    mdata->stream.data = data;
    mdata->cptr = *data;
    mdata->line = 0;
    mdata->CommentLength = 0;
    mdata->Bcmt = mdata->Ecmt = NULL;
    mdata->Bos = mdata->Eos = '\0';
    mdata->format = 0;			/* this can only be XmXpm 2 or 3 */
}

/*
 * close the file related to the xpmData if any
 */
static void
_XmxpmDataClose(mdata)
    xpmData *mdata;
{
    switch (mdata->type) {
    case XPMFILE:
	if (mdata->stream.file != (stdin))
	    fclose(mdata->stream.file);
	break;
    }
}

/*
 * return the default color key related to the given visual
 */
static int
_XmxpmVisualType(visual)
    Visual *visual;
{
    switch (visual->class) {
    case StaticGray:
    case GrayScale:
	switch (visual->map_entries) {
	case 2:
	    return (XPM_MONO);
	case 4:
	    return (XPM_GRAY4);
	default:
	    return (XPM_GRAY);
	}
    default:
	return (XPM_COLOR);
    }
}

static int
closeness_cmp(a, b)
    _Xconst void *a, *b;
{
    CloseColor *x = (CloseColor *) a, *y = (CloseColor *) b;

    /* cast to int as qsort requires */
    return (int) (x->closeness - y->closeness);
}

/*
 * set a close color in case the exact one can't be set
 * return 0 if success, 1 otherwise.
 */
static int
SetCloseColor(display, colormap, visual, col, image_pixel, mask_pixel,
	      alloc_pixels, nalloc_pixels, attributes, cols, ncols)
    Display *display;
    Colormap colormap;
    Visual *visual;
    XColor *col;
    Pixel *image_pixel, *mask_pixel;
    Pixel *alloc_pixels;
    unsigned int *nalloc_pixels;
    XmXpmAttributes *attributes;
    XColor *cols;
    int ncols;
{

    /*
     * Allocation failed, so try close colors. To get here the visual must
     * be GreyScale, PseudoColor or DirectColor (or perhaps StaticColor?
     * What about sharing systems like QDSS?). Beware: we have to treat
     * DirectColor differently.
     */


    long int red_closeness, green_closeness, blue_closeness;
    int n;

    if (attributes && (attributes->valuemask & XmXpmCloseness))
	red_closeness = green_closeness = blue_closeness =
	    attributes->closeness;
    else {
	red_closeness = attributes->red_closeness;
	green_closeness = attributes->green_closeness;
	blue_closeness = attributes->blue_closeness;
    }


    /*
     * We sort the colormap by closeness and try to allocate the color
     * closest to the target. If the allocation of this close color fails,
     * which almost never happens, then one of two scenarios is possible.
     * Either the colormap must have changed (since the last close color
     * allocation or possibly while we were sorting the colormap), or the
     * color is allocated as Read/Write by some other client. (Note: X
     * _should_ allow clients to check if a particular color is Read/Write,
     * but it doesn't! :-( ). We cannot determine which of these scenarios
     * occurred, so we try the next closest color, and so on, until no more
     * colors are within closeness of the target. If we knew that the
     * colormap had changed, we could skip this sequence.
     * 
     * If _none_ of the colors within closeness of the target can be allocated,
     * then we can finally be pretty sure that the colormap has actually
     * changed. In this case we try to allocate the original color (again),
     * then try the closecolor stuff (again)...
     * 
     * In theory it would be possible for an infinite loop to occur if another
     * process kept changing the colormap every time we sorted it, so we set
     * a maximum on the number of iterations. After this many tries, we use
     * XGrabServer() to ensure that the colormap remains unchanged.
     * 
     * This approach gives particularly bad worst case performance - as many as
     * <MaximumIterations> colormap reads and sorts may be needed, and as
     * many as <MaximumIterations> * <ColormapSize> attempted allocations
     * may fail. On an 8-bit system, this means as many as 3 colormap reads,
     * 3 sorts and 768 failed allocations per execution of this code!
     * Luckily, my experiments show that in general use in a typical 8-bit
     * color environment only about 1 in every 10000 allocations fails to
     * succeed in the fastest possible time. So virtually every time what
     * actually happens is a single sort followed by a successful allocate.
     * The very first allocation also costs a colormap read, but no further
     * reads are usually necessary.
     */

#define ITERATIONS 2			/* more than one is almost never
					 * necessary */

    for (n = 0; n <= ITERATIONS; ++n) {
	CloseColor *closenesses =
	    (CloseColor *) XtCalloc(ncols, sizeof(CloseColor));
	int i, c;

	for (i = 0; i < ncols; ++i) {	/* build & sort closenesses table */
#define COLOR_FACTOR       3
#define BRIGHTNESS_FACTOR  1

	    closenesses[i].cols_index = i;
	    closenesses[i].closeness =
		COLOR_FACTOR * (abs((long) col->red - (long) cols[i].red)
				+ abs((long) col->green - (long) cols[i].green)
				+ abs((long) col->blue - (long) cols[i].blue))
		+ BRIGHTNESS_FACTOR * abs(((long) col->red +
					   (long) col->green +
					   (long) col->blue)
					   - ((long) cols[i].red +
					      (long) cols[i].green +
					      (long) cols[i].blue));
	}
	qsort(closenesses, ncols, sizeof(CloseColor), closeness_cmp);

	i = 0;
	c = closenesses[i].cols_index;
	while ((long) cols[c].red >= (long) col->red - red_closeness &&
	       (long) cols[c].red <= (long) col->red + red_closeness &&
	       (long) cols[c].green >= (long) col->green - green_closeness &&
	       (long) cols[c].green <= (long) col->green + green_closeness &&
	       (long) cols[c].blue >= (long) col->blue - blue_closeness &&
	       (long) cols[c].blue <= (long) col->blue + blue_closeness) {
	    if (XAllocColor(display, colormap, &cols[c])) {
		if (n == ITERATIONS)
		    XUngrabServer(display);
		XtFree((char *)closenesses);
		*image_pixel = cols[c].pixel;
		*mask_pixel = 1;
		alloc_pixels[(*nalloc_pixels)++] = cols[c].pixel;
		return (0);
	    } else {
		++i;
		if (i == ncols)
		    break;
		c = closenesses[i].cols_index;
	    }
	}

	/* Couldn't allocate _any_ of the close colors! */

	if (n == ITERATIONS)
	    XUngrabServer(display);
	XtFree((char *)closenesses);

	if (i == 0 || i == ncols)	/* no color close enough or cannot */
	    return (1);			/* alloc any color (full of r/w's) */

	if (XAllocColor(display, colormap, col)) {
	    *image_pixel = col->pixel;
	    *mask_pixel = 1;
	    alloc_pixels[(*nalloc_pixels)++] = col->pixel;
	    return (0);
	} else {			/* colormap has probably changed, so
					 * re-read... */
	    if (n == ITERATIONS - 1)
		XGrabServer(display);

#if 0
	    if (visual->class == DirectColor) {
		/* TODO */
	    } else
#endif
		XQueryColors(display, colormap, cols, ncols);
	}
    }
    return (1);
}

#define USE_CLOSECOLOR attributes && \
(((attributes->valuemask & XmXpmCloseness) && attributes->closeness != 0) \
 || ((attributes->valuemask & XmXpmRGBCloseness) && \
     (attributes->red_closeness != 0 \
      || attributes->green_closeness != 0 \
      || attributes->blue_closeness != 0)))

/*
 * set the color pixel related to the given colorname,
 * return 0 if success, 1 otherwise.
 */

static int
SetColor(display, colormap, visual, colorname, color_index,
	 image_pixel, mask_pixel, mask_pixel_index,
	 alloc_pixels, nalloc_pixels, used_pixels, nused_pixels,
	 attributes, cols, ncols)
    Display *display;
    Colormap colormap;
    Visual *visual;
    char *colorname;
    unsigned int color_index;
    Pixel *image_pixel, *mask_pixel;
    unsigned int *mask_pixel_index;
    Pixel *alloc_pixels;
    unsigned int *nalloc_pixels;
    Pixel *used_pixels;
    unsigned int *nused_pixels;
    XmXpmAttributes *attributes;
    XColor *cols;
    int ncols;
{
    XColor xcolor;

    if (strcasecmp(colorname, TRANSPARENT_COLOR)) {
	if (!XParseColor(display, colormap, colorname, &xcolor))
	    return (1);
	if (!XAllocColor(display, colormap, &xcolor)) {
	    if (USE_CLOSECOLOR)
		return (SetCloseColor(display, colormap, visual, &xcolor,
				      image_pixel, mask_pixel,
				      alloc_pixels, nalloc_pixels,
				      attributes, cols, ncols));
	    else
		return (1);
	} else
	    alloc_pixels[(*nalloc_pixels)++] = xcolor.pixel;
	*image_pixel = xcolor.pixel;
	*mask_pixel = 1;
	used_pixels[(*nused_pixels)++] = xcolor.pixel;
    } else {
	*image_pixel = 0;
	*mask_pixel = 0;
	/* store the color table index */
	*mask_pixel_index = color_index;
    }
    return (0);
}


static int
ImageCreateColors(display, attributes, colors, ncolors, image_pixels, mask_pixels,
	     mask_pixel_index, alloc_pixels, nalloc_pixels,
	     used_pixels, nused_pixels)
    Display *display;
    XmXpmAttributes *attributes;
    XmXpmColor *colors;
    unsigned int ncolors;
    Pixel *image_pixels;
    Pixel *mask_pixels;
    unsigned int *mask_pixel_index;
    Pixel *alloc_pixels;
    unsigned int *nalloc_pixels;
    Pixel *used_pixels;
    unsigned int *nused_pixels;
{
    /* variables stored in the XmXpmAttributes structure */
    Visual *visual;
    Colormap colormap;
    XmXpmColorSymbol *colorsymbols = NULL;
    unsigned int numsymbols;

    char *colorname;
    unsigned int color, key;
    Bool pixel_defined;
    XmXpmColorSymbol *symbol = NULL;
    char **defaults;
    int ErrorStatus = XmXpmSuccess;
    char *s;
    int default_index;

    XColor *cols = NULL;
    unsigned int ncols = 0;

    /*
     * retrieve information from the XmXpmAttributes
     */
    if (attributes && attributes->valuemask & XmXpmColorSymbols) {
	colorsymbols = attributes->colorsymbols;
	numsymbols = attributes->numsymbols;
    } else
	numsymbols = 0;

    if (attributes && attributes->valuemask & XmXpmVisual)
	visual = attributes->visual;
    else
	visual = XDefaultVisual(display, XDefaultScreen(display));

    if (attributes && attributes->valuemask & XmXpmColormap)
	colormap = attributes->colormap;
    else
	colormap = XDefaultColormap(display, XDefaultScreen(display));

    if (attributes && attributes->valuemask & XmXpmColorKey)
	key = attributes->color_key;
    else
	key = _XmxpmVisualType(visual);

    if (USE_CLOSECOLOR) {
	/* originally from SetCloseColor */
#if 0
	if (visual->class == DirectColor) {

	    /*
	     * TODO: Implement close colors for DirectColor visuals. This is
	     * difficult situation. Chances are that we will never get here,
	     * because any machine that supports DirectColor will probably
	     * also support TrueColor (and probably PseudoColor). Also,
	     * DirectColor colormaps can be very large, so looking for close
	     * colors may be too slow.
	     */
	} else {
#endif
	    int i;

	    ncols = visual->map_entries;
	    cols = (XColor *) XtCalloc(ncols, sizeof(XColor));
	    for (i = 0; i < ncols; ++i)
		cols[i].pixel = i;
	    XQueryColors(display, colormap, cols, ncols);
#if 0
	}
#endif
    }

    switch (key) {
    case XPM_MONO:
	default_index = 2;
	break;
    case XPM_GRAY4:
	default_index = 3;
	break;
    case XPM_GRAY:
	default_index = 4;
	break;
    case XPM_COLOR:
    default:
	default_index = 5;
	break;
    }

    for (color = 0; color < ncolors; color++, colors++,
					 image_pixels++, mask_pixels++) {
	colorname = NULL;
	pixel_defined = False;
	defaults = (char **) colors;

	/*
	 * look for a defined symbol
	 */
	if (numsymbols) {

	    unsigned int n;

	    s = defaults[1];
	    for (n = 0, symbol = colorsymbols; n < numsymbols; n++, symbol++) {
		if (symbol->name && s && !strcmp(symbol->name, s))
		    /* override name */
		    break;
		if (!symbol->name && symbol->value) {	/* override value */
		    int def_index = default_index;

		    while (defaults[def_index] == NULL)	/* find defined
							 * colorname */
			--def_index;
		    if (def_index < 2) {/* nothing towards mono, so try
					 * towards color */
			def_index = default_index + 1;
			while (def_index <= 5 && defaults[def_index] == NULL)
			    ++def_index;
		    }
		    if (def_index >= 2 && defaults[def_index] != NULL &&
			!strcasecmp(symbol->value, defaults[def_index]))
			break;
		}
	    }
	    if (n != numsymbols) {
		if (symbol->name && symbol->value)
		    colorname = symbol->value;
		else
		    pixel_defined = True;
	    }
	}
	if (!pixel_defined) {		/* pixel not given as symbol value */

	    unsigned int k;

	    if (colorname) {		/* colorname given as symbol value */
		if (!SetColor(display, colormap, visual, colorname, color,
			      image_pixels, mask_pixels, mask_pixel_index,
			      alloc_pixels, nalloc_pixels, used_pixels,
			      nused_pixels, attributes, cols, ncols))
		    pixel_defined = True;
		else
		    ErrorStatus = XmXpmColorError;
	    }
	    k = key;
	    while (!pixel_defined && k > 1) {
		if (defaults[k]) {
		    if (!SetColor(display, colormap, visual, defaults[k],
				  color, image_pixels, mask_pixels,
				  mask_pixel_index, alloc_pixels,
				  nalloc_pixels, used_pixels, nused_pixels,
				  attributes, cols, ncols)) {
			pixel_defined = True;
			break;
		    } else
			ErrorStatus = XmXpmColorError;
		}
		k--;
	    }
	    k = key + 1;
	    while (!pixel_defined && k < NKEYS + 1) {
		if (defaults[k]) {
		    if (!SetColor(display, colormap, visual, defaults[k],
				  color, image_pixels, mask_pixels,
				  mask_pixel_index, alloc_pixels,
				  nalloc_pixels, used_pixels, nused_pixels,
				  attributes, cols, ncols)) {
			pixel_defined = True;
			break;
		    } else
			ErrorStatus = XmXpmColorError;
		}
		k++;
	    }
	    if (!pixel_defined) {
		if (cols)
		    XtFree((char *)cols);
		return (XmXpmColorFailed);
	    }
	} else {
	    /* simply use the given pixel */
	    *image_pixels = symbol->pixel;
	    /* the following makes the mask to be built even if none
	       is given a particular pixel */
	    if (symbol->value
		&& !strcasecmp(symbol->value, TRANSPARENT_COLOR)) {
		*mask_pixels = 0;
		*mask_pixel_index = color;
	    } else
		*mask_pixels = 1;
	    used_pixels[(*nused_pixels)++] = *image_pixels;
	}
    }
    if (cols)
	XtFree((char *)cols);
    return (ErrorStatus);
}

static int
CreateColors(dataptr, data_size, colors, ncolors, cpp)
    char **dataptr;
    unsigned int *data_size;
    XmXpmColor *colors;
    unsigned int ncolors;
    unsigned int cpp;
{
    char buf[BUFSIZ];
    unsigned int a, key, l;
    char *s, *s2;
    char **defaults;

    for (a = 0; a < ncolors; a++, colors++, dataptr++) {

	defaults = (char **) colors;
	strncpy(buf, *defaults++, cpp);
	s = buf + cpp;

	for (key = 1; key <= NKEYS; key++, defaults++) {
	    if ((s2 = *defaults)) {
		sprintf(s, "\t%s %s", _XmxpmColorKeys[key - 1], s2);
		s += strlen(s);
	    }
	}
	l = strlen(buf) + 1;
	s = (char *) XtMalloc(l);
	if (!s)
	    return (XmXpmNoMemory);
	*data_size += l;
	strcpy(s, buf);
	*dataptr = s;
    }
    return (XmXpmSuccess);
}

static void
CreatePixels(dataptr, width, height, cpp, pixels, colors)
    char **dataptr;
    unsigned int width;
    unsigned int height;
    unsigned int cpp;
    unsigned int *pixels;
    XmXpmColor *colors;
{
    char *s;
    unsigned int x, y, h, offset;

    h = height - 1;
    offset = width * cpp + 1;
    for (y = 0; y < h; y++, dataptr++) {
	s = *dataptr;
	for (x = 0; x < width; x++, pixels++) {
	    strncpy(s, colors[*pixels].string, cpp);
	    s += cpp;
	}
	*s = '\0';
	*(dataptr + 1) = *dataptr + offset;
    }
    /* duplicate some code to avoid a test in the loop */
    s = *dataptr;
    for (x = 0; x < width; x++, pixels++) {
	strncpy(s, colors[*pixels].string, cpp);
	s += cpp;
    }
    *s = '\0';
}

static void
CountExtensions(ext, num, ext_size, ext_nlines)
    XmXpmExtension *ext;
    unsigned int num;
    unsigned int *ext_size;
    unsigned int *ext_nlines;
{
    unsigned int x, y, a, size, nlines;
    char **line;

    size = 0;
    nlines = 0;
    for (x = 0; x < num; x++, ext++) {
	/* 1 for the name */
	nlines += ext->nlines + 1;
	/* 8 = 7 (for "XPMEXT ") + 1 (for 0) */
	size += strlen(ext->name) + 8;
	a = ext->nlines;
	for (y = 0, line = ext->lines; y < a; y++, line++)
	    size += strlen(*line) + 1;
    }
    /* 10 and 1 are for the ending "XPMENDEXT" */
    *ext_size = size + 10;
    *ext_nlines = nlines + 1;
}

static void
CreateExtensions(dataptr, offset, ext, num, ext_nlines)
    char **dataptr;
    unsigned int offset;
    XmXpmExtension *ext;
    unsigned int num;
    unsigned int ext_nlines;
{
    unsigned int x, y, a, b;
    char **line;

    *(dataptr + 1) = *dataptr + offset;
    dataptr++;
    a = 0;
    for (x = 0; x < num; x++, ext++) {
	sprintf(*dataptr, "XPMEXT %s", ext->name);
	a++;
	if (a < ext_nlines)
	    *(dataptr + 1) = *dataptr + strlen(ext->name) + 8;
	dataptr++;
	b = ext->nlines;
	for (y = 0, line = ext->lines; y < b; y++, line++) {
	    strcpy(*dataptr, *line);
	    a++;
	    if (a < ext_nlines)
		*(dataptr + 1) = *dataptr + strlen(*line) + 1;
	    dataptr++;
	}
    }
    strcpy(*dataptr, "XPMENDEXT");
}


/*
 * Create an XImage
 */
static int
CreateXImage(display, visual, depth, width, height, image_return)
    Display *display;
    Visual *visual;
    unsigned int depth;
    unsigned int width;
    unsigned int height;
    XImage **image_return;
{
    int bitmap_pad;

    /* first get bitmap_pad */
    if (depth > 16)
	bitmap_pad = 32;
    else if (depth > 8)
	bitmap_pad = 16;
    else
	bitmap_pad = 8;

    /* then create the XImage with data = NULL and bytes_per_line = 0 */
    *image_return = XCreateImage(display, visual, depth, ZPixmap, 0, 0,
				 width, height, bitmap_pad, 0);
    if (!*image_return)
	return (XmXpmNoMemory);

    /* now that bytes_per_line must have been set properly alloc data */
    (*image_return)->data =
	(char *) XtMalloc((*image_return)->bytes_per_line * height);

    if (!(*image_return)->data) {
	XDestroyImage(*image_return);
	*image_return = NULL;
	return (XmXpmNoMemory);
    }
    return (XmXpmSuccess);
}

/*
 * Default method to write pixels into a Z image data structure.
 * The algorithm used is:
 *
 *	copy the destination bitmap_unit or Zpixel to temp
 *	normalize temp if needed
 *	copy the pixel bits into the temp
 *	renormalize temp if needed
 *	copy the temp back into the destination image data
 */

static void
SetImagePixels(image, width, height, pixelindex, pixels)
    XImage *image;
    unsigned int width;
    unsigned int height;
    unsigned int *pixelindex;
    Pixel *pixels;
{
    register char *src;
    register char *dst;
    register unsigned int *iptr;
    register int x, y, i;
    register char *data;
    Pixel pixel, px;
    int nbytes, depth, ibu, ibpp;

    data = image->data;
    iptr = pixelindex;
    depth = image->depth;
    if (depth == 1) {
	ibu = image->bitmap_unit;
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		pixel = pixels[*iptr];
		for (i = 0, px = pixel; i < sizeof(unsigned long);
		     i++, px >>= 8)
		    ((unsigned char *) &pixel)[i] = px;
		src = &data[XYINDEX(x, y, image)];
		dst = (char *) &px;
		px = 0;
		nbytes = ibu >> 3;
		for (i = nbytes; --i >= 0;)
		    *dst++ = *src++;
		XYNORMALIZE(&px, image);
		_putbits((char *) &pixel, (x % ibu), 1, (char *) &px);
		XYNORMALIZE(&px, image);
		src = (char *) &px;
		dst = &data[XYINDEX(x, y, image)];
		for (i = nbytes; --i >= 0;)
		    *dst++ = *src++;
	    }
    } else {
	ibpp = image->bits_per_pixel;
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		pixel = pixels[*iptr];
		if (depth == 4)
		    pixel &= 0xf;
		for (i = 0, px = pixel; i < sizeof(unsigned long); i++,
		     px >>= 8)
		    ((unsigned char *) &pixel)[i] = px;
		src = &data[ZINDEX(x, y, image)];
		dst = (char *) &px;
		px = 0;
		nbytes = (ibpp + 7) >> 3;
		for (i = nbytes; --i >= 0;)
		    *dst++ = *src++;
		ZNORMALIZE(&px, image);
		_putbits((char *) &pixel, (x * ibpp) & 7, ibpp, (char *) &px);
		ZNORMALIZE(&px, image);
		src = (char *) &px;
		dst = &data[ZINDEX(x, y, image)];
		for (i = nbytes; --i >= 0;)
		    *dst++ = *src++;
	    }
    }
}

/*
 * write pixels into a 32-bits Z image data structure
 */

#if !defined(WORD64) && !defined(LONG64)
/* this item is static but deterministic so let it slide; doesn't
 * hurt re-entrancy of this library. Note if it is actually const then would
 * be OK under rules of ANSI-C but probably not C++ which may not
 * want to allocate space for it.
 */
static unsigned long /* constant */ RTXmXpm_byteorderpixel = MSBFirst << 24;

#endif

/*
   WITHOUT_SPEEDUPS is a flag to be turned on if you wish to use the original
   3.2e code - by default you get the speeded-up version.
*/

static void
SetImagePixels32(image, width, height, pixelindex, pixels)
    XImage *image;
    unsigned int width;
    unsigned int height;
    unsigned int *pixelindex;
    Pixel *pixels;
{
    unsigned char *data;
    unsigned int *iptr;
    int y;
    Pixel pixel;

#ifdef WITHOUT_SPEEDUPS

    int x;
    unsigned char *addr;

    data = (unsigned char *) image->data;
    iptr = pixelindex;
#if !defined(WORD64) && !defined(LONG64)
    if (*((char *) &RTXmXpm_byteorderpixel) == image->byte_order) {
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		addr = &data[ZINDEX32(x, y, image)];
		*((unsigned long *) addr) = pixels[*iptr];
	    }
    } else
#endif
    if (image->byte_order == MSBFirst)
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		addr = &data[ZINDEX32(x, y, image)];
		pixel = pixels[*iptr];
		addr[0] = pixel >> 24;
		addr[1] = pixel >> 16;
		addr[2] = pixel >> 8;
		addr[3] = pixel;
	    }
    else
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		addr = &data[ZINDEX32(x, y, image)];
		pixel = pixels[*iptr];
		addr[0] = pixel;
		addr[1] = pixel >> 8;
		addr[2] = pixel >> 16;
		addr[3] = pixel >> 24;
	    }

#else  /* WITHOUT_SPEEDUPS */

    int bpl = image->bytes_per_line;
    unsigned char *data_ptr, *max_data;

    data = (unsigned char *) image->data;
    iptr = pixelindex;
#if !defined(WORD64) && !defined(LONG64)
    if (*((char *) &RTXmXpm_byteorderpixel) == image->byte_order) {
	for (y = 0; y < height; y++) {
	    data_ptr = data;
	    max_data = data_ptr + (width << 2);

	    while (data_ptr < max_data) {
		*((unsigned long *) data_ptr) = pixels[*(iptr++)];
		data_ptr += (1 << 2);
	    }
	    data += bpl;
	}
    } else
#endif
    if (image->byte_order == MSBFirst)
	for (y = 0; y < height; y++) {
	    data_ptr = data;
	    max_data = data_ptr + (width << 2);

	    while (data_ptr < max_data) {
		pixel = pixels[*(iptr++)];

		*data_ptr++ = pixel >> 24;
		*data_ptr++ = pixel >> 16;
		*data_ptr++ = pixel >> 8;
		*data_ptr++ = pixel;

	    }
	    data += bpl;
	}
    else
	for (y = 0; y < height; y++) {
	    data_ptr = data;
	    max_data = data_ptr + (width << 2);

	    while (data_ptr < max_data) {
		pixel = pixels[*(iptr++)];

		*data_ptr++ = pixel;
		*data_ptr++ = pixel >> 8;
		*data_ptr++ = pixel >> 16;
		*data_ptr++ = pixel >> 24;
	    }
	    data += bpl;
	}

#endif /* WITHOUT_SPEEDUPS */
}

/*
 * write pixels into a 16-bits Z image data structure
 */

static void
SetImagePixels16(image, width, height, pixelindex, pixels)
    XImage *image;
    unsigned int width;
    unsigned int height;
    unsigned int *pixelindex;
    Pixel *pixels;
{
    unsigned char *data;
    unsigned int *iptr;
    int y;

#ifdef WITHOUT_SPEEDUPS

    int x;
    unsigned char *addr;

    data = (unsigned char *) image->data;
    iptr = pixelindex;
    if (image->byte_order == MSBFirst)
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		addr = &data[ZINDEX16(x, y, image)];
		addr[0] = pixels[*iptr] >> 8;
		addr[1] = pixels[*iptr];
	    }
    else
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		addr = &data[ZINDEX16(x, y, image)];
		addr[0] = pixels[*iptr];
		addr[1] = pixels[*iptr] >> 8;
	    }

#else  /* WITHOUT_SPEEDUPS */

    Pixel pixel;

    int bpl = image->bytes_per_line;
    unsigned char *data_ptr, *max_data;

    data = (unsigned char *) image->data;
    iptr = pixelindex;
    if (image->byte_order == MSBFirst)
	for (y = 0; y < height; y++) {
	    data_ptr = data;
	    max_data = data_ptr + (width << 1);

	    while (data_ptr < max_data) {
		pixel = pixels[*(iptr++)];

		data_ptr[0] = pixel >> 8;
		data_ptr[1] = pixel;

		data_ptr += (1 << 1);
	    }
	    data += bpl;
	}
    else
	for (y = 0; y < height; y++) {
	    data_ptr = data;
	    max_data = data_ptr + (width << 1);

	    while (data_ptr < max_data) {
		pixel = pixels[*(iptr++)];

		data_ptr[0] = pixel;
		data_ptr[1] = pixel >> 8;

		data_ptr += (1 << 1);
	    }
	    data += bpl;
	}

#endif /* WITHOUT_SPEEDUPS */
}

/*
 * write pixels into a 8-bits Z image data structure
 */

static void
SetImagePixels8(image, width, height, pixelindex, pixels)
    XImage *image;
    unsigned int width;
    unsigned int height;
    unsigned int *pixelindex;
    Pixel *pixels;
{
    char *data;
    unsigned int *iptr;
    int y;

#ifdef WITHOUT_SPEEDUPS

    int x;

    data = image->data;
    iptr = pixelindex;
    for (y = 0; y < height; y++)
	for (x = 0; x < width; x++, iptr++)
	    data[ZINDEX8(x, y, image)] = pixels[*iptr];

#else  /* WITHOUT_SPEEDUPS */

    int bpl = image->bytes_per_line;
    char *data_ptr, *max_data;

    data = image->data;
    iptr = pixelindex;

    for (y = 0; y < height; y++) {
	data_ptr = data;
	max_data = data_ptr + width;

	while (data_ptr < max_data)
	    *(data_ptr++) = pixels[*(iptr++)];

	data += bpl;
    }

#endif /* WITHOUT_SPEEDUPS */
}

/*
 * write pixels into a 1-bit depth image data structure and **offset null**
 */

static void
SetImagePixels1(image, width, height, pixelindex, pixels)
    XImage *image;
    unsigned int width;
    unsigned int height;
    unsigned int *pixelindex;
    Pixel *pixels;
{
    if (image->byte_order != image->bitmap_bit_order)
	SetImagePixels(image, width, height, pixelindex, pixels);
    else {
	unsigned int *iptr;
	int y;
	char *data;

#ifdef WITHOUT_SPEEDUPS

	int x;

	data = image->data;
	iptr = pixelindex;
	if (image->bitmap_bit_order == MSBFirst)
	    for (y = 0; y < height; y++)
		for (x = 0; x < width; x++, iptr++) {
		    if (pixels[*iptr] & 1)
			data[ZINDEX1(x, y, image)] |= 0x80 >> (x & 7);
		    else
			data[ZINDEX1(x, y, image)] &= ~(0x80 >> (x & 7));
		}
	else
	    for (y = 0; y < height; y++)
		for (x = 0; x < width; x++, iptr++) {
		    if (pixels[*iptr] & 1)
			data[ZINDEX1(x, y, image)] |= 1 << (x & 7);
		    else
			data[ZINDEX1(x, y, image)] &= ~(1 << (x & 7));
		}

#else  /* WITHOUT_SPEEDUPS */

	char value;
	char *data_ptr, *max_data;
	int bpl = image->bytes_per_line;
	int diff, count;

	data = image->data;
	iptr = pixelindex;

	diff = width & 7;
	width >>= 3;

	if (image->bitmap_bit_order == MSBFirst)
	    for (y = 0; y < height; y++) {
		data_ptr = data;
		max_data = data_ptr + width;
		while (data_ptr < max_data) {
		    value = 0;

		    value = (value << 1) | (pixels[*(iptr++)] & 1);
		    value = (value << 1) | (pixels[*(iptr++)] & 1);
		    value = (value << 1) | (pixels[*(iptr++)] & 1);
		    value = (value << 1) | (pixels[*(iptr++)] & 1);
		    value = (value << 1) | (pixels[*(iptr++)] & 1);
		    value = (value << 1) | (pixels[*(iptr++)] & 1);
		    value = (value << 1) | (pixels[*(iptr++)] & 1);
		    value = (value << 1) | (pixels[*(iptr++)] & 1);

		    *(data_ptr++) = value;
		}
		if (diff) {
		    value = 0;
		    for (count = 0; count < diff; count++) {
			if (pixels[*(iptr++)] & 1)
			    value |= (0x80 >> count);
		    }
		    *(data_ptr) = value;
		}
		data += bpl;
	    }
	else
	    for (y = 0; y < height; y++) {
		data_ptr = data;
		max_data = data_ptr + width;
		while (data_ptr < max_data) {
		    value = 0;
		    iptr += 8;

		    value = (value << 1) | (pixels[*(--iptr)] & 1);
		    value = (value << 1) | (pixels[*(--iptr)] & 1);
		    value = (value << 1) | (pixels[*(--iptr)] & 1);
		    value = (value << 1) | (pixels[*(--iptr)] & 1);
		    value = (value << 1) | (pixels[*(--iptr)] & 1);
		    value = (value << 1) | (pixels[*(--iptr)] & 1);
		    value = (value << 1) | (pixels[*(--iptr)] & 1);
		    value = (value << 1) | (pixels[*(--iptr)] & 1);

		    iptr += 8;
		    *(data_ptr++) = value;
		}
		if (diff) {
		    value = 0;
		    for (count = 0; count < diff; count++) {
			if (pixels[*(iptr++)] & 1)
			    value |= (1 << count);
		    }
		    *(data_ptr) = value;
		}
		data += bpl;
	    }

#endif /* WITHOUT_SPEEDUPS */
    }
}

static int
ParseComment(mdata)
    xpmData *mdata;
{
    if (mdata->type == XPMBUFFER) {
	register char c;
	register unsigned int n = 0;
	unsigned int notend;
	char *s, *s2;

	s = mdata->Comment;
	*s = mdata->Bcmt[0];

	/* skip the string beginning comment */
	s2 = mdata->Bcmt;
	do {
	    c = *mdata->cptr++;
	    *++s = c;
	    n++;
	    s2++;
	} while (c == *s2 && *s2 != '\0' && c);

	if (*s2 != '\0') {
	    /* this wasn't the beginning of a comment */
	    mdata->cptr -= n;
	    return 0;
	}
	/* store comment */
	mdata->Comment[0] = *s;
	s = mdata->Comment;
	notend = 1;
	n = 0;
	while (notend) {
	    s2 = mdata->Ecmt;
	    while (*s != *s2 && c) {
		c = *mdata->cptr++;
		if (n == XPMMAXCMTLEN - 1)  { /* forget it */
		    s = mdata->Comment;
		    n = 0;
		}
		*++s = c;
		n++;
	    }
	    mdata->CommentLength = n;
	    do {
		c = *mdata->cptr++;
		if (n == XPMMAXCMTLEN - 1)  { /* forget it */
		    s = mdata->Comment;
		    n = 0;
		}
		*++s = c;
		n++;
		s2++;
	    } while (c == *s2 && *s2 != '\0' && c);
	    if (*s2 == '\0') {
		/* this is the end of the comment */
		notend = 0;
		mdata->cptr--;
	    }
	}
	return 0;
    } else {
	FILE *file = mdata->stream.file;
	register int c;
	register unsigned int n = 0, a;
	unsigned int notend;
	char *s, *s2;

	s = mdata->Comment;
	*s = mdata->Bcmt[0];

	/* skip the string beginning comment */
	s2 = mdata->Bcmt;
	do {
	    c = getc(file);
	    *++s = c;
	    n++;
	    s2++;
	} while (c == *s2 && *s2 != '\0' && c != EOF);

	if (*s2 != '\0') {
	    /* this wasn't the beginning of a comment */
	    /* put characters back in the order that we got them */
	    for (a = n; a > 0; a--, s--)
		ungetc(*s, file);
	    return 0;
	}
	/* store comment */
	mdata->Comment[0] = *s;
	s = mdata->Comment;
	notend = 1;
	n = 0;
	while (notend) {
	    s2 = mdata->Ecmt;
	    while (*s != *s2 && c != EOF) {
		c = getc(file);
		if (n == XPMMAXCMTLEN - 1)  { /* forget it */
		    s = mdata->Comment;
		    n = 0;
		}
		*++s = c;
		n++;
	    }
	    mdata->CommentLength = n;
	    do {
		c = getc(file);
		if (n == XPMMAXCMTLEN - 1)  { /* forget it */
		    s = mdata->Comment;
		    n = 0;
		}
		*++s = c;
		n++;
		s2++;
	    } while (c == *s2 && *s2 != '\0' && c != EOF);
	    if (*s2 == '\0') {
		/* this is the end of the comment */
		notend = 0;
		ungetc(*s, file);
	    }
	}
	return 0;
    }
}

/*
 * skip to the end of the current string and the beginning of the next one
 */
static int
_XmxpmNextString(mdata)
    xpmData *mdata;
{
    if (!mdata->type)
	mdata->cptr = (mdata->stream.data)[++mdata->line];
    else if (mdata->type == XPMBUFFER) {
	register char c;

	/* get to the end of the current string */
	if (mdata->Eos)
	    while ((c = *mdata->cptr++) && c != mdata->Eos);

	/*
	 * then get to the beginning of the next string looking for possible
	 * comment
	 */
	if (mdata->Bos) {
	    while ((c = *mdata->cptr++) && c != mdata->Bos)
		if (mdata->Bcmt && c == mdata->Bcmt[0])
		    ParseComment(mdata);
	} else if (mdata->Bcmt) {	/* XPM2 natural */
	    while ((c = *mdata->cptr++) == mdata->Bcmt[0])
		ParseComment(mdata);
	    mdata->cptr--;
	}
    } else {
	register int c;
	FILE *file = mdata->stream.file;

	/* get to the end of the current string */
	if (mdata->Eos)
	    while ((c = getc(file)) != mdata->Eos && c != EOF);

	/*
	 * then get to the beginning of the next string looking for possible
	 * comment
	 */
	if (mdata->Bos) {
	    while ((c = getc(file)) != mdata->Bos && c != EOF)
		if (mdata->Bcmt && c == mdata->Bcmt[0])
		    ParseComment(mdata);

	} else if (mdata->Bcmt) {	/* XPM2 natural */
	    while ((c = getc(file)) == mdata->Bcmt[0])
		ParseComment(mdata);
	    ungetc(c, file);
	}
    }
    return 0;
}

/*
 * skip whitespace and return the following word
 */
static unsigned int
_XmxpmNextWord(mdata, buf, buflen)
    xpmData *mdata;
    char *buf;
    unsigned int buflen;
{
    register unsigned int n = 0;
    int c;

    if (!mdata->type || mdata->type == XPMBUFFER) {
	while (isspace(c = *mdata->cptr) && c != mdata->Eos)
	    mdata->cptr++;
	do {
	    c = *mdata->cptr++;
	    *buf++ = c;
	    n++;
	} while (!isspace(c) && c != mdata->Eos && n < buflen);
	n--;
	mdata->cptr--;
    } else {
	FILE *file = mdata->stream.file;

	while ((c = getc(file)) != EOF && isspace(c) && c != mdata->Eos);
	while (!isspace(c) && c != mdata->Eos && c != EOF && n < buflen) {
	    *buf++ = c;
	    n++;
	    c = getc(file);
	}
	ungetc(c, file);
    }
    return (n);
}

static unsigned int
_Xmxpmatoui(p, l, ui_return)
    register char *p;
    unsigned int l;
    unsigned int *ui_return;
{
    register unsigned int n, i;

    n = 0;
    for (i = 0; i < l; i++)
	if (*p >= '0' && *p <= '9')
	    n = n * 10 + *p++ - '0';
	else
	    break;

    if (i != 0 && i == l) {
	*ui_return = n;
	return 1;
    } else
	return 0;
}

/*
 * skip whitespace and compute the following unsigned int,
 * returns 1 if one is found and 0 if not
 */
static int
_XmxpmNextUI(mdata, ui_return)
    xpmData *mdata;
    unsigned int *ui_return;
{
    char buf[BUFSIZ];
    int l;

    l = _XmxpmNextWord(mdata, buf, BUFSIZ);
    return _Xmxpmatoui(buf, l, ui_return);
}

/*
 * return end of string - WARNING: malloc!
 */
static int
_XmxpmGetString(mdata, sptr, l)
    xpmData *mdata;
    char **sptr;
    unsigned int *l;
{
    unsigned int i, n = 0;
    int c;
    char *p = NULL, *q, buf[BUFSIZ];

    if (!mdata->type || mdata->type == XPMBUFFER) {
	if (mdata->cptr) {
	    char *start = mdata->cptr;
	    while ((c = *mdata->cptr) && c != mdata->Eos)
		mdata->cptr++;
	    n = mdata->cptr - start + 1;
	    p = (char *) XtMalloc(n);
	    if (!p)
		return (XmXpmNoMemory);
	    strncpy(p, start, n);
	    if (mdata->type)		/* XPMBUFFER */
		p[n - 1] = '\0';
	}
    } else {
	FILE *file = mdata->stream.file;

	if ((c = getc(file)) == EOF)
	    return (XmXpmFileInvalid);

	i = 0;
	q = buf;
	p = (char *) XtMalloc(1);
	while (c != mdata->Eos && c != EOF) {
	    if (i == BUFSIZ) {
		/* get to the end of the buffer */
		/* malloc needed memory */
		q = (char *) XtRealloc(p, n + i);
		if (!q) {
		    XtFree(p);
		    return (XmXpmNoMemory);
		}
		p = q;
		q += n;
		/* and copy what we already have */
		strncpy(q, buf, i);
		n += i;
		i = 0;
		q = buf;
	    }
	    *q++ = c;
	    i++;
	    c = getc(file);
	}
	if (c == EOF) {
	    XtFree(p);
	    return (XmXpmFileInvalid);
	}
	if (n + i != 0) {
	    /* malloc needed memory */
	    q = (char *) XtRealloc(p, n + i + 1);
	    if (!q) {
		XtFree(p);
		return (XmXpmNoMemory);
	    }
	    p = q;
	    q += n;
	    /* and copy the buffer */
	    strncpy(q, buf, i);
	    n += i;
	    p[n++] = '\0';
	} else {
	    *p = '\0';
	    n = 1;
	}
	ungetc(c, file);
    }
    *sptr = p;
    *l = n;
    return (XmXpmSuccess);
}

/*
 * get the current comment line
 */
static int
_XmxpmGetCmt(mdata, cmt)
    xpmData *mdata;
    char **cmt;
{
    if (!mdata->type)
	*cmt = NULL;
    else if (mdata->CommentLength) {
	*cmt = (char *) XtMalloc(mdata->CommentLength + 1);
	strncpy(*cmt, mdata->Comment, mdata->CommentLength);
	(*cmt)[mdata->CommentLength] = '\0';
	mdata->CommentLength = 0;
    } else
	*cmt = NULL;
    return 0;
}

/*
 * parse xpm header
 */
static int
_XmxpmParseHeader(mdata)
    xpmData *mdata;
{
    char buf[BUFSIZ];
    int l, n = 0;

    if (mdata->type) {
	mdata->Bos = '\0';
	mdata->Eos = '\n';
	mdata->Bcmt = mdata->Ecmt = NULL;
	l = _XmxpmNextWord(mdata, buf, BUFSIZ);
	if (l == 7 && !strncmp("#define", buf, 7)) {
	    /* this maybe an XPM 1 file */
	    char *ptr;

	    l = _XmxpmNextWord(mdata, buf, BUFSIZ);
	    if (!l)
		return (XmXpmFileInvalid);
	    buf[l] = '\0';
	    ptr = strrchr(buf, '_');
	    if (!ptr || strncmp("_format", ptr, l - (ptr - buf)))
		return XmXpmFileInvalid;
	    /* this is definitely an XPM 1 file */
	    mdata->format = 1;
	    n = 1;			/* handle XPM1 as mainly XPM2 C */
	} else {

	    /*
	     * skip the first word, get the second one, and see if this is
	     * XPM 2 or 3
	     */
	    l = _XmxpmNextWord(mdata, buf, BUFSIZ);
	    if ((l == 3 && !strncmp("XPM", buf, 3)) ||
		(l == 4 && !strncmp("XPM2", buf, 4))) {
		if (l == 3)
		    n = 1;		/* handle XPM as XPM2 C */
		else {
		    /* get the type key word */
		    l = _XmxpmNextWord(mdata, buf, BUFSIZ);

		    /*
		     * get infos about this type
		     */
		    while (xpmDataTypes[n].type
			   && strncmp(xpmDataTypes[n].type, buf, l))
			n++;
		}
		mdata->format = 0;
	    } else
		/* nope this is not an XPM file */
		return XmXpmFileInvalid;
	}
	if (xpmDataTypes[n].type) {
	    if (n == 0) {		/* natural type */
		mdata->Bcmt = xpmDataTypes[n].Bcmt;
		mdata->Ecmt = xpmDataTypes[n].Ecmt;
		_XmxpmNextString(mdata);	/* skip the end of the headerline */
		mdata->Bos = xpmDataTypes[n].Bos;
		mdata->Eos = xpmDataTypes[n].Eos;
	    } else {
		mdata->Bcmt = xpmDataTypes[n].Bcmt;
		mdata->Ecmt = xpmDataTypes[n].Ecmt;
		if (!mdata->format) {	/* XPM 2 or 3 */
		    mdata->Bos = xpmDataTypes[n].Bos;
		    mdata->Eos = '\0';
		    /* get to the beginning of the first string */
		    _XmxpmNextString(mdata);
		    mdata->Eos = xpmDataTypes[n].Eos;
		} else			/* XPM 1 skip end of line */
		    _XmxpmNextString(mdata);
	    }
	} else
	    /* we don't know about that type of XPM file... */
	    return XmXpmFileInvalid;
    }
    return XmXpmSuccess;
}

/*
 * Create a colortable compatible with the old style colortable
 */
static int
CreateOldColorTable(ct, ncolors, oldct)
    XmXpmColor *ct;
    int ncolors;
    XmXpmColor ***oldct;
{
    XmXpmColor **colorTable, **color;
    int a;

    colorTable = (XmXpmColor **) XtMalloc(ncolors * sizeof(XmXpmColor *));
    if (!colorTable) {
	*oldct = NULL;
	return (XmXpmNoMemory);
    }
    for (a = 0, color = colorTable; a < ncolors; a++, color++, ct++)
	*color = ct;
    *oldct = colorTable;
    return (XmXpmSuccess);
}

static void
FreeOldColorTable(colorTable, ncolors)
    XmXpmColor **colorTable;
    int ncolors;
{
    int a, b;
    XmXpmColor **color;
    char **sptr;

    if (colorTable) {
	for (a = 0, color = colorTable; a < ncolors; a++, color++) {
	    for (b = 0, sptr = (char **) *color; b <= NKEYS; b++, sptr++)
		if (*sptr)
		    XtFree(*sptr);
	}
	XtFree((char *)*colorTable);
	XtFree((char *)colorTable);
    }
}

/*
 * Free the computed color table
 */
void
_XmxpmFreeColorTable(colorTable, ncolors)
    XmXpmColor *colorTable;
    int ncolors;
{
    int a, b;
    XmXpmColor *color;
    char **sptr;

    if (colorTable) {
	for (a = 0, color = colorTable; a < ncolors; a++, color++) {
	    for (b = 0, sptr = (char **) color; b <= NKEYS; b++, sptr++)
		if (*sptr)
		    XtFree(*sptr);
	}
	XtFree((char *)colorTable);
    }
}

static xpmHashAtom
AtomMake(name, data)			/* makes an atom */
    char *name;				/* WARNING: is just pointed to */
    void *data;
{
    xpmHashAtom object = (xpmHashAtom) XtMalloc(sizeof(struct _xpmHashAtom));

    if (object) {
	object->name = name;
	object->data = data;
    }
    return object;
}

/*
 * _XmxpmHashSlot gives the slot (pointer to xpmHashAtom) of a name
 * (slot points to NULL if it is not defined)
 *
 */
static xpmHashAtom *
_XmxpmHashSlot(table, s)
    xpmHashTable *table;
    char *s;
{
    xpmHashAtom *atomTable = table->atomTable;
    unsigned int hash;
    xpmHashAtom *p;
    char *hp = s;
    char *ns;

    hash = 0;
    while (*hp) {			/* computes hash function */
	HASH_FUNCTION
    }
    p = atomTable + hash % table->size;
    while (*p) {
	ns = (*p)->name;
	if (ns[0] == s[0] && strcmp(ns, s) == 0)
	    break;
	p--;
	if (p < atomTable)
	    p = atomTable + table->size - 1;
    }
    return p;
}

static int
HashTableGrows(table)
    xpmHashTable *table;
{
    xpmHashAtom *atomTable = table->atomTable;
    int size = table->size;
    xpmHashAtom *t, *p;
    int i;
    int oldSize = size;

    t = atomTable;
    HASH_TABLE_GROWS
	table->size = size;
    table->limit = size / 3;
    atomTable = (xpmHashAtom *) XtMalloc(size * sizeof(*atomTable));
    if (!atomTable)
	return (XmXpmNoMemory);
    table->atomTable = atomTable;
    for (p = atomTable + size; p > atomTable;)
	*--p = NULL;
    for (i = 0, p = t; i < oldSize; i++, p++)
	if (*p) {
	    xpmHashAtom *ps = _XmxpmHashSlot(table, (*p)->name);

	    *ps = *p;
	}
    XtFree((char *)t);
    return (XmXpmSuccess);
}

/*
 * _XmxpmHashIntern(table, name, data)
 * an xpmHashAtom is created if name doesn't exist, with the given data.
 */
static int
_XmxpmHashIntern(table, tag, data)
    xpmHashTable *table;
    char *tag;
    void *data;
{
    xpmHashAtom *slot;

    if (!*(slot = _XmxpmHashSlot(table, tag))) {
	/* undefined, make a new atom with the given data */
	if (!(*slot = AtomMake(tag, data)))
	    return (XmXpmNoMemory);
	if (table->used >= table->limit) {
	    int ErrorStatus;

	    if ((ErrorStatus = HashTableGrows(table)) != XmXpmSuccess)
		return (ErrorStatus);
	    table->used++;
	    return (XmXpmSuccess);
	}
	table->used++;
    }
    return (XmXpmSuccess);
}

/*
 *  must be called before allocating any atom
 */
static int
_XmxpmHashTableInit(table)
    xpmHashTable *table;
{
    xpmHashAtom *p;
    xpmHashAtom *atomTable;

    table->size = INITIAL_HASH_SIZE;
    table->limit = table->size / 3;
    table->used = 0;
    atomTable = (xpmHashAtom *) XtMalloc(table->size * sizeof(*atomTable));
    if (!atomTable)
	return (XmXpmNoMemory);
    for (p = atomTable + table->size; p > atomTable;)
	*--p = NULL;
    table->atomTable = atomTable;
    return (XmXpmSuccess);
}

/*
 *   frees a hashtable and all the stored atoms
 */
static void
_XmxpmHashTableFree(table)
    xpmHashTable *table;
{
    xpmHashAtom *p;
    xpmHashAtom *atomTable = table->atomTable;

    for (p = atomTable + table->size; p > atomTable;)
	if (*--p)
	    XtFree((char *)*p);
    XtFree((char *)atomTable);
    table->atomTable = NULL;
}

/*
 * Free array of extensions
 */
void
_XmXpmFreeExtensions(extensions, nextensions)
    XmXpmExtension *extensions;
    int nextensions;
{
    unsigned int i, j, nlines;
    XmXpmExtension *ext;
    char **sptr;

    if (extensions) {
	for (i = 0, ext = extensions; i < nextensions; i++, ext++) {
	    if (ext->name)
		XtFree(ext->name);
	    nlines = ext->nlines;
	    for (j = 0, sptr = ext->lines; j < nlines; j++, sptr++)
		if (*sptr)
		    XtFree(*sptr);
	    if (ext->lines)
		XtFree((char *)ext->lines);
	}
	XtFree((char *)extensions);
    }
}

static int
ParseValues(data, width, height, ncolors, cpp,
	    x_hotspot, y_hotspot, hotspot, extensions)
    xpmData *data;
    unsigned int *width, *height, *ncolors, *cpp;
    unsigned int *x_hotspot, *y_hotspot, *hotspot;
    unsigned int *extensions;
{
    unsigned int l;
    char buf[BUFSIZ];

    if (!data->format) {		/* XPM 2 or 3 */

	/*
	 * read values: width, height, ncolors, chars_per_pixel
	 */
	if (!(_XmxpmNextUI(data, width) && _XmxpmNextUI(data, height)
	      && _XmxpmNextUI(data, ncolors) && _XmxpmNextUI(data, cpp)))
	    return (XmXpmFileInvalid);

	/*
	 * read optional information (hotspot and/or XPMEXT) if any
	 */
	l = _XmxpmNextWord(data, buf, BUFSIZ);
	if (l) {
	    *extensions = (l == 6 && !strncmp("XPMEXT", buf, 6));
	    if (*extensions)
		*hotspot = (_XmxpmNextUI(data, x_hotspot)
			    && _XmxpmNextUI(data, y_hotspot));
	    else {
		*hotspot = (_Xmxpmatoui(buf, l, x_hotspot)
			    && _XmxpmNextUI(data, y_hotspot));
		l = _XmxpmNextWord(data, buf, BUFSIZ);
		*extensions = (l == 6 && !strncmp("XPMEXT", buf, 6));
	    }
	}
    } else {

	/*
	 * XPM 1 file read values: width, height, ncolors, chars_per_pixel
	 */
	int i;
	char *ptr;
	Bool got_one, saw_width = False, saw_height = False;
	Bool saw_ncolors = False, saw_chars_per_pixel = False;

	for (i = 0; i < 4; i++) {
	    l = _XmxpmNextWord(data, buf, BUFSIZ);
	    if (l != 7 || strncmp("#define", buf, 7))
		return (XmXpmFileInvalid);
	    l = _XmxpmNextWord(data, buf, BUFSIZ);
	    if (!l)
		return (XmXpmFileInvalid);
	    buf[l] = '\0';
	    ptr = buf;
	    got_one = False;
	    while (!got_one) {
		ptr = strchr(ptr, '_');
		if (!ptr)
		    return (XmXpmFileInvalid);
		switch (l - (ptr - buf)) {
		case 6:
		    if (saw_width || strncmp("_width", ptr, 6)
			|| !_XmxpmNextUI(data, width))
			return (XmXpmFileInvalid);
		    else
			saw_width = True;
		    got_one = True;
		    break;
		case 7:
		    if (saw_height || strncmp("_height", ptr, 7)
			|| !_XmxpmNextUI(data, height))
			return (XmXpmFileInvalid);
		    else
			saw_height = True;
		    got_one = True;
		    break;
		case 8:
		    if (saw_ncolors || strncmp("_ncolors", ptr, 8)
			|| !_XmxpmNextUI(data, ncolors))
			return (XmXpmFileInvalid);
		    else
			saw_ncolors = True;
		    got_one = True;
		    break;
		case 16:
		    if (saw_chars_per_pixel
			|| strncmp("_chars_per_pixel", ptr, 16)
			|| !_XmxpmNextUI(data, cpp))
			return (XmXpmFileInvalid);
		    else
			saw_chars_per_pixel = True;
		    got_one = True;
		    break;
		default:
		    ptr++;
		}
	    }
	    /* skip the end of line */
	    _XmxpmNextString(data);
	}
	if (!saw_width || !saw_height || !saw_ncolors || !saw_chars_per_pixel)
	  return (XmXpmFileInvalid);

	*hotspot = 0;
	*extensions = 0;
    }
    return (XmXpmSuccess);
}

static int
ParseColors(data, ncolors, cpp, colorTablePtr, hashtable)
    xpmData *data;
    unsigned int ncolors;
    unsigned int cpp;
    XmXpmColor **colorTablePtr;
    xpmHashTable *hashtable;
{
    unsigned int key = 0, l, a, b;
    unsigned int curkey;		/* current color key */
    unsigned int lastwaskey;		/* key read */
    char buf[BUFSIZ];
    char curbuf[BUFSIZ];		/* current buffer */
    char **sptr, *s;
    XmXpmColor *color;
    XmXpmColor *colorTable;
    char **defaults;
    int ErrorStatus;

    colorTable = (XmXpmColor *) XtCalloc(ncolors, sizeof(XmXpmColor));
    if (!colorTable)
	return (XmXpmNoMemory);

    if (!data->format) {		/* XPM 2 or 3 */
	for (a = 0, color = colorTable; a < ncolors; a++, color++) {
	    _XmxpmNextString(data);	/* skip the line */

	    /*
	     * read pixel value
	     */
	    color->string = (char *) XtMalloc(cpp + 1);
	    if (!color->string) {
		_XmxpmFreeColorTable(colorTable, ncolors);
		return (XmXpmNoMemory);
	    }
	    for (b = 0, s = color->string; b < cpp; b++, s++)
		*s = xpmGetC(data);
	    *s = '\0';

	    /*
	     * store the string in the hashtable with its color index number
	     */
	    if (USE_HASHTABLE) {
		ErrorStatus =
		    _XmxpmHashIntern(hashtable, color->string, HashAtomData(a));
		if (ErrorStatus != XmXpmSuccess) {
		    _XmxpmFreeColorTable(colorTable, ncolors);
		    return (ErrorStatus);
		}
	    }

	    /*
	     * read color keys and values
	     */
	    defaults = (char **) color;
	    curkey = 0;
	    lastwaskey = 0;
	    *curbuf = '\0';		/* init curbuf */
	    while ((l = _XmxpmNextWord(data, buf, BUFSIZ))) {
		if (!lastwaskey) {
		    for (key = 0, sptr = _XmxpmColorKeys; key < NKEYS; key++,
			 sptr++)
			if ((strlen(*sptr) == l) && (!strncmp(*sptr, buf, l)))
			    break;
		}
		if (!lastwaskey && key < NKEYS) {	/* open new key */
		    if (curkey) {	/* flush string */
			s = (char *) XtMalloc(strlen(curbuf) + 1);
			if (!s) {
			    _XmxpmFreeColorTable(colorTable, ncolors);
			    return (XmXpmNoMemory);
			}
			defaults[curkey] = s;
			strcpy(s, curbuf);
		    }
		    curkey = key + 1;	/* set new key  */
		    *curbuf = '\0';	/* reset curbuf */
		    lastwaskey = 1;
		} else {
		    if (!curkey) {	/* key without value */
			_XmxpmFreeColorTable(colorTable, ncolors);
			return (XmXpmFileInvalid);
		    }
		    if (!lastwaskey)
			strcat(curbuf, " ");	/* append space */
		    buf[l] = '\0';
		    strcat(curbuf, buf);/* append buf */
		    lastwaskey = 0;
		}
	    }
	    if (!curkey) {		/* key without value */
		_XmxpmFreeColorTable(colorTable, ncolors);
		return (XmXpmFileInvalid);
	    }
	    s = defaults[curkey] = (char *) XtMalloc(strlen(curbuf) + 1);
	    if (!s) {
		_XmxpmFreeColorTable(colorTable, ncolors);
		return (XmXpmNoMemory);
	    }
	    strcpy(s, curbuf);
	}
    } else {				/* XPM 1 */
	/* get to the beginning of the first string */
	data->Bos = '"';
	data->Eos = '\0';
	_XmxpmNextString(data);
	data->Eos = '"';
	for (a = 0, color = colorTable; a < ncolors; a++, color++) {

	    /*
	     * read pixel value
	     */
	    color->string = (char *) XtMalloc(cpp + 1);
	    if (!color->string) {
		_XmxpmFreeColorTable(colorTable, ncolors);
		return (XmXpmNoMemory);
	    }
	    for (b = 0, s = color->string; b < cpp; b++, s++)
		*s = xpmGetC(data);
	    *s = '\0';

	    /*
	     * store the string in the hashtable with its color index number
	     */
	    if (USE_HASHTABLE) {
		ErrorStatus =
		    _XmxpmHashIntern(hashtable, color->string, HashAtomData(a));
		if (ErrorStatus != XmXpmSuccess) {
		    _XmxpmFreeColorTable(colorTable, ncolors);
		    return (ErrorStatus);
		}
	    }

	    /*
	     * read color values
	     */
	    _XmxpmNextString(data);	/* get to the next string */
	    *curbuf = '\0';		/* init curbuf */
	    while ((l = _XmxpmNextWord(data, buf, BUFSIZ))) {
		if (*curbuf != '\0')
		    strcat(curbuf, " ");/* append space */
		buf[l] = '\0';
		strcat(curbuf, buf);	/* append buf */
	    }
	    s = (char *) XtMalloc(strlen(curbuf) + 1);
	    if (!s) {
		_XmxpmFreeColorTable(colorTable, ncolors);
		return (XmXpmNoMemory);
	    }
	    strcpy(s, curbuf);
	    color->c_color = s;
	    *curbuf = '\0';		/* reset curbuf */
	    if (a < ncolors - 1)
		_XmxpmNextString(data);	/* get to the next string */
	}
    }
    *colorTablePtr = colorTable;
    return (XmXpmSuccess);
}

static int
ParsePixels(data, width, height, ncolors, cpp, colorTable, hashtable, pixels)
    xpmData *data;
    unsigned int width;
    unsigned int height;
    unsigned int ncolors;
    unsigned int cpp;
    XmXpmColor *colorTable;
    xpmHashTable *hashtable;
    unsigned int **pixels;
{
    unsigned int *iptr, *iptr2;
    unsigned int a, x, y;

    iptr2 = (unsigned int *) XtMalloc(sizeof(unsigned int) * width * height);

    if (!iptr2)
	return (XmXpmNoMemory);

    iptr = iptr2;

    switch (cpp) {

    case (1):				/* Optimize for single character
					 * colors */
	{
	    unsigned short colidx[256];

	    bzero(colidx, 256 * sizeof(short));
	    for (a = 0; a < ncolors; a++)
		colidx[(unsigned char)colorTable[a].string[0]] = a + 1;

	    for (y = 0; y < height; y++) {
		_XmxpmNextString(data);
		for (x = 0; x < width; x++, iptr++) {
		    int c = xpmGetC(data);

		    if (c > 0 && c < 256 && colidx[c] != 0)
			*iptr = colidx[c] - 1;
		    else {
			XtFree((char *)iptr2);
			return (XmXpmFileInvalid);
		    }
		}
	    }
	}
	break;

    case (2):				/* Optimize for double character
					 * colors */
	{

/* free all allocated pointers at all exits */
#define FREE_CIDX {int f; for (f = 0; f < 256; f++) \
if (cidx[f]) XtFree((char *)cidx[f]);}

	    /* array of pointers malloced by need */
	    unsigned short *cidx[256];
	    int char1;

	    bzero(cidx, 256 * sizeof(unsigned short *)); /* init */
	    for (a = 0; a < ncolors; a++) {
		char1 = colorTable[a].string[0];
		if (cidx[char1] == NULL) { /* get new memory */
		    cidx[char1] = (unsigned short *)
			XtCalloc(256, sizeof(unsigned short));
		    if (cidx[char1] == NULL) { /* new block failed */
			FREE_CIDX;
			XtFree((char *)iptr2);
			return (XmXpmNoMemory);
		    }
		}
		cidx[char1][(unsigned char)colorTable[a].string[1]] = a + 1;
	    }

	    for (y = 0; y < height; y++) {
		_XmxpmNextString(data);
		for (x = 0; x < width; x++, iptr++) {
		    int cc1 = xpmGetC(data);
		    if (cc1 > 0 && cc1 < 256) {
			int cc2 = xpmGetC(data);
			if (cc2 > 0 && cc2 < 256 && cidx[cc1][cc2] != 0)
			    *iptr = cidx[cc1][cc2] - 1;
			else {
			    FREE_CIDX;
			    XtFree((char *)iptr2);
			    return (XmXpmFileInvalid);
			}
		    } else {
			FREE_CIDX;
			XtFree((char *)iptr2);
			return (XmXpmFileInvalid);
		    }
		}
	    }
	    FREE_CIDX;
	}
	break;

    default:				/* Non-optimized case of long color
					 * names */
	{
	    char *s;
	    char buf[BUFSIZ];

	    buf[cpp] = '\0';
	    if (USE_HASHTABLE) {
		xpmHashAtom *slot;

		for (y = 0; y < height; y++) {
		    _XmxpmNextString(data);
		    for (x = 0; x < width; x++, iptr++) {
			for (a = 0, s = buf; a < cpp; a++, s++)
			    *s = xpmGetC(data);
			slot = _XmxpmHashSlot(hashtable, buf);
			if (!*slot) {	/* no color matches */
			    XtFree((char *)iptr2);
			    return (XmXpmFileInvalid);
			}
			*iptr = HashColorIndex(slot);
		    }
		}
	    } else {
		for (y = 0; y < height; y++) {
		    _XmxpmNextString(data);
		    for (x = 0; x < width; x++, iptr++) {
			for (a = 0, s = buf; a < cpp; a++, s++)
			    *s = xpmGetC(data);
			for (a = 0; a < ncolors; a++)
			    if (!strcmp(colorTable[a].string, buf))
				break;
			if (a == ncolors) {	/* no color matches */
			    XtFree((char *)iptr2);
			    return (XmXpmFileInvalid);
			}
			*iptr = a;
		    }
		}
	    }
	}
	break;
    }
    *pixels = iptr2;
    return (XmXpmSuccess);
}

static int
ParseExtensions(data, extensions, nextensions)
    xpmData *data;
    XmXpmExtension **extensions;
    unsigned int *nextensions;
{
    XmXpmExtension *exts = NULL, *ext;
    unsigned int num = 0;
    unsigned int nlines, a, l, notstart, notend = 0;
    int status;
    char *string, *s, *s2, **sp;

    _XmxpmNextString(data);
    exts = (XmXpmExtension *) XtMalloc(sizeof(XmXpmExtension));
    /* get the whole string */
    status = _XmxpmGetString(data, &string, &l);
    if (status != XmXpmSuccess) {
	XtFree((char *)exts);
	return (status);
    }
    /* look for the key word XPMEXT, skip lines before this */
    while ((notstart = strncmp("XPMEXT", string, 6))
	   && (notend = strncmp("XPMENDEXT", string, 9))) {
	XtFree(string);
	_XmxpmNextString(data);
	status = _XmxpmGetString(data, &string, &l);
	if (status != XmXpmSuccess) {
	    XtFree((char *)exts);
	    return (status);
	}
    }
    if (!notstart)
	notend = strncmp("XPMENDEXT", string, 9);
    while (!notstart && notend) {
	/* there starts an extension */
	ext = (XmXpmExtension *)
	    XtRealloc((char *)exts, (num + 1) * sizeof(XmXpmExtension));
	if (!ext) {
	    XtFree(string);
	    _XmXpmFreeExtensions(exts, num);
	    return (XmXpmNoMemory);
	}
	exts = ext;
	ext += num;
	/* skip whitespace and store its name */
	s2 = s = string + 6;
	while (isspace(*s2))
	    s2++;
	a = s2 - s;
	ext->name = (char *) XtMalloc(l - a - 6);
	if (!ext->name) {
	    XtFree(string);
	    ext->lines = NULL;
	    ext->nlines = 0;
	    _XmXpmFreeExtensions(exts, num + 1);
	    return (XmXpmNoMemory);
	}
	strncpy(ext->name, s + a, l - a - 6);
	XtFree(string);
	/* now store the related lines */
	_XmxpmNextString(data);
	status = _XmxpmGetString(data, &string, &l);
	if (status != XmXpmSuccess) {
	    ext->lines = NULL;
	    ext->nlines = 0;
	    _XmXpmFreeExtensions(exts, num + 1);
	    return (status);
	}
	ext->lines = (char **) XtMalloc(sizeof(char *));
	nlines = 0;
	while ((notstart = strncmp("XPMEXT", string, 6))
	       && (notend = strncmp("XPMENDEXT", string, 9))) {
	    sp = (char **)
		XtRealloc((char *)ext->lines, (nlines + 1) * sizeof(char *));
	    if (!sp) {
		XtFree(string);
		ext->nlines = nlines;
		_XmXpmFreeExtensions(exts, num + 1);
		return (XmXpmNoMemory);
	    }
	    ext->lines = sp;
	    ext->lines[nlines] = string;
	    nlines++;
	    _XmxpmNextString(data);
	    status = _XmxpmGetString(data, &string, &l);
	    if (status != XmXpmSuccess) {
		ext->nlines = nlines;
		_XmXpmFreeExtensions(exts, num + 1);
		return (status);
	    }
	}
	if (!nlines) {
	    XtFree((char *)ext->lines);
	    ext->lines = NULL;
	}
	ext->nlines = nlines;
	num++;
    }
    if (!num) {
	XtFree(string);
	XtFree((char *)exts);
	exts = NULL;
    } else if (!notend)
	XtFree(string);
    *nextensions = num;
    *extensions = exts;
    return (XmXpmSuccess);
}

/* function call in case of error, frees only locally allocated variables */
#undef RETURN
#define RETURN(status) \
{ \
    if (colorTable) _XmxpmFreeColorTable(colorTable, ncolors); \
    if (pixelindex) XtFree((char *)pixelindex); \
    if (hints_cmt)  XtFree((char *)hints_cmt); \
    if (colors_cmt) XtFree((char *)colors_cmt); \
    if (pixels_cmt) XtFree((char *)pixels_cmt); \
    return(status); \
}

/*
 * This function parses an XmXpm file or data and store the found informations
 * in an an XmXpmImage structure which is returned.
 */
static int
_XmxpmParseData(data, image, info)
    xpmData *data;
    XmXpmImage *image;
    XmXpmInfo *info;
{
    /* variables to return */
    unsigned int width, height, ncolors, cpp;
    unsigned int x_hotspot, y_hotspot, hotspot = 0, extensions = 0;
    XmXpmColor *colorTable = NULL;
    unsigned int *pixelindex = NULL;
    char *hints_cmt = NULL;
    char *colors_cmt = NULL;
    char *pixels_cmt = NULL;

    unsigned int cmts;
    int ErrorStatus;
    xpmHashTable hashtable;

    cmts = info && (info->valuemask & XmXpmReturnComments);

    /*
     * parse the header
     */
    ErrorStatus = _XmxpmParseHeader(data);
    if (ErrorStatus != XmXpmSuccess)
	return (ErrorStatus);

    /*
     * read values
     */
    ErrorStatus = ParseValues(data, &width, &height, &ncolors, &cpp,
			    &x_hotspot, &y_hotspot, &hotspot, &extensions);
    if (ErrorStatus != XmXpmSuccess)
	return (ErrorStatus);

    /*
     * store the hints comment line
     */
    if (cmts)
	_XmxpmGetCmt(data, &hints_cmt);

    /*
     * init the hastable
     */
    if (USE_HASHTABLE) {
	ErrorStatus = _XmxpmHashTableInit(&hashtable);
	if (ErrorStatus != XmXpmSuccess)
	    return (ErrorStatus);
    }

    /*
     * read colors
     */
    ErrorStatus = ParseColors(data, ncolors, cpp, &colorTable, &hashtable);
    if (ErrorStatus != XmXpmSuccess) {
	if (USE_HASHTABLE)
	    _XmxpmHashTableFree(&hashtable);
	RETURN(ErrorStatus);
    }

    /*
     * store the colors comment line
     */
    if (cmts)
	_XmxpmGetCmt(data, &colors_cmt);

    /*
     * read pixels and index them on color number
     */
    ErrorStatus = ParsePixels(data, width, height, ncolors, cpp, colorTable,
			      &hashtable, &pixelindex);

    /*
     * free the hastable
     */
    if (USE_HASHTABLE)
	_XmxpmHashTableFree(&hashtable);

    if (ErrorStatus != XmXpmSuccess)
	RETURN(ErrorStatus);

    /*
     * store the pixels comment line
     */
    if (cmts)
	_XmxpmGetCmt(data, &pixels_cmt);

    /*
     * parse extensions
     */
    if (info && (info->valuemask & XmXpmReturnExtensions))
	if (extensions) {
	    ErrorStatus = ParseExtensions(data, &info->extensions,
					  &info->nextensions);
	    if (ErrorStatus != XmXpmSuccess)
		RETURN(ErrorStatus);
	} else {
	    info->extensions = NULL;
	    info->nextensions = 0;
	}

    /*
     * store found informations in the XmXpmImage structure
     */
    image->width = width;
    image->height = height;
    image->cpp = cpp;
    image->ncolors = ncolors;
    image->colorTable = colorTable;
    image->data = pixelindex;

    if (info) {
	if (cmts) {
	    info->hints_cmt = hints_cmt;
	    info->colors_cmt = colors_cmt;
	    info->pixels_cmt = pixels_cmt;
	}
	if (hotspot) {
	    info->x_hotspot = x_hotspot;
	    info->y_hotspot = y_hotspot;
	    info->valuemask |= XmXpmHotspot;
	}
    }
    return (XmXpmSuccess);
}

/*
 * Init returned data to free safely later on
 */
static void
_XmxpmInitXpmImage(image)
    XmXpmImage *image;
{
    image->ncolors = 0;
    image->colorTable = NULL;
    image->data = NULL;
}

/*
 * Free the XpmImage data which have been allocated
 */
static void
_XmXpmFreeXpmImage(image)
    XmXpmImage *image;
{
    if (image->colorTable)
	_XmxpmFreeColorTable(image->colorTable, image->ncolors);
    XtFree((char *)image->data);
    image->data = NULL;
}

/*
 * Init returned data to free safely later on
 */
static void
_XmxpmInitXpmInfo(info)
    XmXpmInfo *info;
{
    if (info) {
	info->hints_cmt = NULL;
	info->colors_cmt = NULL;
	info->pixels_cmt = NULL;
	info->extensions = NULL;
	info->nextensions = 0;
    }
}

/*
 * Free the XpmInfo data which have been allocated
 */
static void
_XmXpmFreeXpmInfo(info)
    XmXpmInfo *info;
{
    if (info) {
	if (info->valuemask & XmXpmComments) {
	    if (info->hints_cmt) {
		XtFree(info->hints_cmt);
		info->hints_cmt = NULL;
	    }
	    if (info->colors_cmt) {
		XtFree(info->colors_cmt);
		info->colors_cmt = NULL;
	    }
	    if (info->pixels_cmt) {
		XtFree(info->pixels_cmt);
		info->pixels_cmt = NULL;
	    }
	}
	if (info->valuemask & XmXpmReturnExtensions && info->nextensions) {
	    _XmXpmFreeExtensions(info->extensions, info->nextensions);
	    info->extensions = NULL;
	    info->nextensions = 0;
	}
	info->valuemask = 0;
    }
}

/*
 * Set the XpmInfo valuemask to retrieve required info
 */
static void
_XmxpmSetInfoMask(info, attributes)
    XmXpmInfo *info;
    XmXpmAttributes *attributes;
{
    info->valuemask = 0;
    if (attributes->valuemask & XmXpmReturnInfos)
	info->valuemask |= XmXpmReturnComments;
    if (attributes->valuemask & XmXpmReturnExtensions)
	info->valuemask |= XmXpmReturnExtensions;
}

/*
 * Fill in the XpmInfo with the XpmAttributes
 */
static void
_XmxpmSetInfo(info, attributes)
    XmXpmInfo *info;
    XmXpmAttributes *attributes;
{
    info->valuemask = 0;
    if (attributes->valuemask & XmXpmInfos) {
	info->valuemask |= XmXpmComments | XmXpmColorTable;
	info->hints_cmt = attributes->hints_cmt;
	info->colors_cmt = attributes->colors_cmt;
	info->pixels_cmt = attributes->pixels_cmt;
    }
    if (attributes->valuemask & XmXpmExtensions) {
	info->valuemask |= XmXpmExtensions;
	info->extensions = attributes->extensions;
	info->nextensions = attributes->nextensions;
    }
    if (attributes->valuemask & XmXpmHotspot) {
	info->valuemask |= XmXpmHotspot;
	info->x_hotspot = attributes->x_hotspot;
	info->y_hotspot = attributes->y_hotspot;
    }
}

static int
_XmXpmReadFileToXpmImage(filename, image, info)
    char *filename;
    XmXpmImage *image;
    XmXpmInfo *info;
{
    xpmData mdata;
    int ErrorStatus;

    /* init returned values */
    _XmxpmInitXpmImage(image);
    _XmxpmInitXpmInfo(info);

    /* open file to read */
    if ((ErrorStatus = OpenReadFile(filename, &mdata)) != XmXpmSuccess)
	return (ErrorStatus);

    /* create the XmXpmImage from the XmXpmData */
    ErrorStatus = _XmxpmParseData(&mdata, image, info);

    _XmxpmDataClose(&mdata);

    return (ErrorStatus);
}

/* function call in case of error, frees only locally allocated variables */
#undef RETURN
#define RETURN(status) \
{ \
    if (ximage) XDestroyImage(ximage); \
    if (shapeimage) XDestroyImage(shapeimage); \
    if (image_pixels) XtFree((char *)image_pixels); \
    if (mask_pixels) XtFree((char *)mask_pixels); \
    if (nalloc_pixels) \
	XFreeColors(display, colormap, alloc_pixels, nalloc_pixels, 0); \
    if (alloc_pixels) XtFree((char *)alloc_pixels); \
    if (used_pixels) XtFree((char *)used_pixels); \
    return (status); \
}

static int
_XmXpmCreateImageFromXpmImage(display, image,
			   image_return, shapeimage_return, attributes)
    Display *display;
    XmXpmImage *image;
    XImage **image_return;
    XImage **shapeimage_return;
    XmXpmAttributes *attributes;
{
    /* variables stored in the XmXpmAttributes structure */
    Visual *visual;
    Colormap colormap;
    unsigned int depth;

    /* variables to return */
    XImage *ximage = NULL;
    XImage *shapeimage = NULL;
    unsigned int mask_pixel_index = XmXpmUndefPixel;
    int ErrorStatus;

    /* calculation variables */
    Pixel *image_pixels = NULL;
    Pixel *mask_pixels = NULL;
    Pixel *alloc_pixels = NULL;
    Pixel *used_pixels = NULL;
    unsigned int nalloc_pixels = 0;
    unsigned int nused_pixels = 0;

    /* initialize return values */
    if (image_return)
	*image_return = NULL;
    if (shapeimage_return)
	*shapeimage_return = NULL;

    /* retrieve information from the XmXpmAttributes */
    if (attributes && (attributes->valuemask & XmXpmVisual))
	visual = attributes->visual;
    else
	visual = XDefaultVisual(display, XDefaultScreen(display));

    if (attributes && (attributes->valuemask & XmXpmColormap))
	colormap = attributes->colormap;
    else
	colormap = XDefaultColormap(display, XDefaultScreen(display));

    if (attributes && (attributes->valuemask & XmXpmDepth))
	depth = attributes->depth;
    else
	depth = XDefaultDepth(display, XDefaultScreen(display));

    ErrorStatus = XmXpmSuccess;

    /* malloc pixels index tables */
    image_pixels = (Pixel *) XtMalloc(sizeof(Pixel) * image->ncolors);
    if (!image_pixels)
	return (XmXpmNoMemory);

    mask_pixels = (Pixel *) XtMalloc(sizeof(Pixel) * image->ncolors);
    if (!mask_pixels)
	RETURN(XmXpmNoMemory);

    /* maximum of allocated pixels will be the number of colors */
    alloc_pixels = (Pixel *) XtMalloc(sizeof(Pixel) * image->ncolors);
    if (!alloc_pixels)
	RETURN(XmXpmNoMemory);

    /* maximum of allocated pixels will be the number of colors */
    used_pixels = (Pixel *) XtMalloc(sizeof(Pixel) * image->ncolors);
    if (!used_pixels)
	RETURN(XmXpmNoMemory);

    /* get pixel colors, store them in index tables */
    ErrorStatus = ImageCreateColors(display, attributes, image->colorTable,
			       image->ncolors, image_pixels, mask_pixels,
			       &mask_pixel_index, alloc_pixels, &nalloc_pixels,
			       used_pixels, &nused_pixels);

    if (ErrorStatus != XmXpmSuccess
	&& (ErrorStatus < 0 || (attributes
				&& (attributes->valuemask & XmXpmExactColors)
				&& attributes->exactColors)))
	RETURN(ErrorStatus);

    /* create the ximage */
    if (image_return) {
	ErrorStatus = CreateXImage(display, visual, depth,
				   image->width, image->height, &ximage);
	if (ErrorStatus != XmXpmSuccess)
	    RETURN(ErrorStatus);

	/*
	 * set the ximage data
	 * 
	 * In case depth is 1 or bits_per_pixel is 4, 6, 8, 24 or 32 use
	 * optimized functions, otherwise use slower but sure general one.
	 * 
	 */

	if (ximage->depth == 1)
	    SetImagePixels1(ximage, image->width, image->height,
			    image->data, image_pixels);
	else if (ximage->bits_per_pixel == 8)
	    SetImagePixels8(ximage, image->width, image->height,
			    image->data, image_pixels);
	else if (ximage->bits_per_pixel == 16)
	    SetImagePixels16(ximage, image->width, image->height,
			     image->data, image_pixels);
	else if (ximage->bits_per_pixel == 32)
	    SetImagePixels32(ximage, image->width, image->height,
			     image->data, image_pixels);
	else
	    SetImagePixels(ximage, image->width, image->height,
			   image->data, image_pixels);
    }
    /* create the shape mask image */
    if (mask_pixel_index != XmXpmUndefPixel && shapeimage_return) {
	ErrorStatus = CreateXImage(display, visual, 1, image->width,
				   image->height, &shapeimage);
	if (ErrorStatus != XmXpmSuccess)
	    RETURN(ErrorStatus);

	SetImagePixels1(shapeimage, image->width, image->height,
			image->data, mask_pixels);

    }
    XtFree((char *)image_pixels);
    XtFree((char *)mask_pixels);

    /* if requested return used pixels in the XmXpmAttributes structure */
    if (attributes && (attributes->valuemask & XmXpmReturnPixels ||
	/* 3.2 backward compatibility code */
	attributes->valuemask & XmXpmReturnInfos)) {
	/* end 3.2 bc */
	attributes->pixels = used_pixels;
	attributes->npixels = nused_pixels;
	attributes->mask_pixel = mask_pixel_index;
    } else
	XtFree((char *)used_pixels);

    /* if requested return alloc'ed pixels in the XmXpmAttributes structure */
    if (attributes && (attributes->valuemask & XmXpmReturnAllocPixels)) {
	attributes->alloc_pixels = alloc_pixels;
	attributes->nalloc_pixels = nalloc_pixels;
    } else
	XtFree((char *)alloc_pixels);

    /* return created images */
    if (image_return)
	*image_return = ximage;
    if (shapeimage_return)
	*shapeimage_return = shapeimage;

    return (ErrorStatus);
}

#undef RETURN
#define RETURN(status) \
{ \
    if (header) { \
	for (l = 0; l < header_nlines; l++) \
	    if (header[l]) \
		XtFree((char *)header[l]); \
		XtFree((char *)header); \
    } \
    return(status); \
}

static int
_XmXpmCreateDataFromXpmImage(data_return, image, info)
    char ***data_return;
    XmXpmImage *image;
    XmXpmInfo *info;
{
    /* calculation variables */
    int ErrorStatus;
    char buf[BUFSIZ];
    char **header = NULL, **data, **sptr, **sptr2, *s;
    unsigned int header_size, header_nlines;
    unsigned int data_size, data_nlines;
    unsigned int extensions = 0, ext_size = 0, ext_nlines = 0;
    unsigned int offset, l, n;

    *data_return = NULL;

    extensions = info && (info->valuemask & XmXpmExtensions)
	&& info->nextensions;

    /* compute the number of extensions lines and size */
    if (extensions)
	CountExtensions(info->extensions, info->nextensions,
			&ext_size, &ext_nlines);

    /*
     * alloc a temporary array of char pointer for the header section which
     * is the hints line + the color table lines
     */
    header_nlines = 1 + image->ncolors;
    header_size = sizeof(char *) * header_nlines;
    header = (char **) XtCalloc(header_size, sizeof(char *));
    if (!header)
	return (XmXpmNoMemory);

    /* print the hints line */
    s = buf;
    sprintf(s, "%d %d %d %d", image->width, image->height,
	    image->ncolors, image->cpp);
    s += strlen(s);

    if (info && (info->valuemask & XmXpmHotspot)) {
	sprintf(s, " %d %d", info->x_hotspot, info->y_hotspot);
	s += strlen(s);
    }
    if (extensions)
	sprintf(s, " XPMEXT");

    l = strlen(buf) + 1;
    *header = (char *) XtMalloc(l);
    if (!*header)
	RETURN(XmXpmNoMemory);
    header_size += l;
    strcpy(*header, buf);

    /* print colors */
    ErrorStatus = CreateColors(header + 1, &header_size,
			       image->colorTable, image->ncolors, image->cpp);

    if (ErrorStatus != XmXpmSuccess)
	RETURN(ErrorStatus);

    /* now we know the size needed, alloc the data and copy the header lines */
    offset = image->width * image->cpp + 1;
    data_size = header_size + (image->height + ext_nlines) * sizeof(char *)
	+ image->height * offset + ext_size;

    data = (char **) XtMalloc(data_size);
    if (!data)
	RETURN(XmXpmNoMemory);

    data_nlines = header_nlines + image->height + ext_nlines;
    *data = (char *) (data + data_nlines);
    n = image->ncolors;
    for (l = 0, sptr = data, sptr2 = header; l <= n; l++, sptr++, sptr2++) {
	strcpy(*sptr, *sptr2);
	*(sptr + 1) = *sptr + strlen(*sptr2) + 1;
    }

    /* print pixels */
    data[header_nlines] = (char *) data + header_size
	+ (image->height + ext_nlines) * sizeof(char *);

    CreatePixels(data + header_nlines, image->width, image->height,
		 image->cpp, image->data, image->colorTable);

    /* print extensions */
    if (extensions)
	CreateExtensions(data + header_nlines + image->height - 1, offset,
			 info->extensions, info->nextensions,
			 ext_nlines);

    *data_return = data;

    RETURN(XmXpmSuccess);
}

static int
_XmXpmCreateXpmImageFromData(data, image, info)
    char **data;
    XmXpmImage *image;
    XmXpmInfo *info;
{
    xpmData mdata;
    int ErrorStatus;

    /* init returned values */
    _XmxpmInitXpmImage(image);
    _XmxpmInitXpmInfo(info);

    /* open data */
    OpenArray(data, &mdata);

    /* create the XmXpmImage from the XmXpmData */
    ErrorStatus = _XmxpmParseData(&mdata, image, info);

    return (ErrorStatus);
}

/*
 * Init returned data to free safely later on
 */
static void
_XmxpmInitAttributes(attributes)
    XmXpmAttributes *attributes;
{
    if (attributes) {
	attributes->pixels = NULL;
	attributes->npixels = 0;
	attributes->colorTable = NULL;
	attributes->ncolors = 0;
	/* 3.2 backward compatibility code */
	attributes->hints_cmt = NULL;
	attributes->colors_cmt = NULL;
	attributes->pixels_cmt = NULL;
	/* end 3.2 bc */
	attributes->extensions = NULL;
	attributes->nextensions = 0;
	attributes->alloc_pixels = NULL;
	attributes->nalloc_pixels = 0;
    }
}

/*
 * Fill in the XmXpmAttributes with the XmXpmImage and the XmXpmInfo
 */
void
_XmxpmSetAttributes(attributes, image, info)
    XmXpmAttributes *attributes;
    XmXpmImage *image;
    XmXpmInfo *info;
{
    if (attributes->valuemask & XmXpmReturnColorTable) {
	attributes->colorTable = image->colorTable;
	attributes->ncolors = image->ncolors;

	/* avoid deletion of copied data */
	image->ncolors = 0;
	image->colorTable = NULL;
    }
    /* 3.2 backward compatibility code */
    else if (attributes->valuemask & XmXpmReturnInfos) {
	int ErrorStatus;

	ErrorStatus = CreateOldColorTable(image->colorTable, image->ncolors,
					  (XmXpmColor ***)
					  &attributes->colorTable);

	/* if error just say we can't return requested data */
	if (ErrorStatus != XmXpmSuccess) {
	    attributes->valuemask &= ~XmXpmReturnInfos;
	    if (!(attributes->valuemask & XmXpmReturnPixels)) {
		XtFree((char *)attributes->pixels);
		attributes->pixels = NULL;
		attributes->npixels = 0;
	    }
	    attributes->ncolors = 0;
	} else {
	    attributes->ncolors = image->ncolors;
	    attributes->hints_cmt = info->hints_cmt;
	    attributes->colors_cmt = info->colors_cmt;
	    attributes->pixels_cmt = info->pixels_cmt;

	    /* avoid deletion of copied data */
	    image->ncolors = 0;
	    image->colorTable = NULL;
	    info->hints_cmt = NULL;
	    info->colors_cmt = NULL;
	    info->pixels_cmt = NULL;
	}
    }
    /* end 3.2 bc */
    if (attributes->valuemask & XmXpmReturnExtensions) {
	attributes->extensions = info->extensions;
	attributes->nextensions = info->nextensions;

	/* avoid deletion of copied data */
	info->extensions = NULL;
	info->nextensions = 0;
    }
    if (info->valuemask & XmXpmHotspot) {
	attributes->valuemask |= XmXpmHotspot;
	attributes->x_hotspot = info->x_hotspot;
	attributes->y_hotspot = info->y_hotspot;
    }
    attributes->valuemask |= XmXpmCharsPerPixel;
    attributes->cpp = image->cpp;
    attributes->valuemask |= XmXpmSize;
    attributes->width = image->width;
    attributes->height = image->height;
}

/*
 * Read a rgb text file.  It stores the rgb values (0->65535)
 * and the rgb mnemonics (malloc'ed) into the "rgbn" array.  Returns the
 * number of entries stored.
 */
static int
_XmxpmReadRgbNames(rgb_fname, rgbn)
    char *rgb_fname;
    xpmRgbName rgbn[];

{
    FILE *rgbf;
    int n, items, red, green, blue;
    char line[512], name[512], *rgbname, *s1, *s2;
    xpmRgbName *rgb;

    /* Open the rgb text file.  Abort if error. */
    if ((rgbf = fopen(rgb_fname, "r")) == NULL)
	return 0;

    /* Loop reading each line in the file. */
    n = 0;
    rgb = rgbn; 
    /* Quit if rgb text file has too many entries. */
    while (fgets(line, sizeof(line), rgbf) && n < MAX_RGBNAMES) {

	/* Skip silently if line is bad. */
	items = sscanf(line, "%d %d %d %[^\n]\n", &red, &green, &blue, name);
	if (items != 4)
	    continue;

	/*
	 * Make sure rgb values are within 0->255 range. Skip silently if
	 * bad.
	 */
	if (red < 0 || red > 0xFF ||
	    green < 0 || green > 0xFF ||
	    blue < 0 || blue > 0xFF)
	    continue;

	/* Allocate memory for ascii name. If error give up here. */
	if (!(rgbname = (char *) XtMalloc(strlen(name) + 1)))
	    break;

	/* Copy string to ascii name and lowercase it. */
	for (s1 = name, s2 = rgbname; *s1; s1++)
	    *s2++ = tolower(*s1);
	*s2 = '\0';

	/* Save the rgb values and ascii name in the array. */
	rgb->r = red * 257;		/* 65535/255 = 257 */
	rgb->g = green * 257;
	rgb->b = blue * 257;
	rgb->name = rgbname;
	rgb++;
	n++;
    }

    fclose(rgbf);

    /* Return the number of read rgb names. */
    return n < 0 ? 0 : n;
}

/*
 * Return the color name corresponding to the given rgb values
 */
static char *
_XmxpmGetRgbName(rgbn, rgbn_max, red, green, blue)
    xpmRgbName rgbn[];			/* rgb mnemonics from rgb text file */
    int rgbn_max;			/* number of rgb mnemonics in table */
    int red, green, blue;		/* rgb values */

{
    int i;
    xpmRgbName *rgb;

    /*
     * Just perform a dumb linear search over the rgb values of the color
     * mnemonics.  One could speed things up by sorting the rgb values and
     * using a binary search, or building a hash table, etc...
     */
    for (i = 0, rgb = rgbn; i < rgbn_max; i++, rgb++)
	if (red == rgb->r && green == rgb->g && blue == rgb->b)
	    return rgb->name;

    /* if not found return NULL */
    return NULL;
}

/*
 * Free the strings which have been malloc'ed in xpmReadRgbNames
 */
static void
_XmxpmFreeRgbNames(rgbn, rgbn_max)
    xpmRgbName rgbn[];
    int rgbn_max;

{
    int i;
    xpmRgbName *rgb;

    for (i = 0, rgb = rgbn; i < rgbn_max; i++, rgb++)
	XtFree(rgb->name);
}

/*
 * This function stores the given pixel in the given arrays which are grown
 * if not large enough.
 */
static int
storePixel(pixel, pmap, index_return)
    Pixel pixel;
    PixelsMap *pmap;
    unsigned int *index_return;
{
    unsigned int i;
    Pixel *p;
    unsigned int ncolors;

    if (*index_return) {		/* this is a transparent pixel! */
	*index_return = 0;
	return 0;
    }
    ncolors = pmap->ncolors;
    p = pmap->pixels + pmap->mask_pixel;
    for (i = pmap->mask_pixel; i < ncolors; i++, p++)
	if (*p == pixel)
	    break;
    if (i == ncolors) {
	if (ncolors >= pmap->size) {
	    pmap->size *= 2;
	    p = (Pixel *) XtRealloc((char *)pmap->pixels, sizeof(Pixel) * pmap->size);
	    if (!p)
		return (1);
	    pmap->pixels = p;

	}
	(pmap->pixels)[ncolors] = pixel;
	pmap->ncolors++;
    }
    *index_return = i;
    return 0;
}

static int
storeMaskPixel(pixel, pmap, index_return)
    Pixel pixel;
    PixelsMap *pmap;
    unsigned int *index_return;
{
    if (!pixel) {
	if (!pmap->ncolors) {
	    pmap->ncolors = 1;
	    (pmap->pixels)[0] = 0;
	    pmap->mask_pixel = 1;
	}
	*index_return = 1;
    } else
	*index_return = 0;
    return 0;
}

static int
ScanTransparentColor(color, cpp, attributes)
    XmXpmColor *color;
    unsigned int cpp;
    XmXpmAttributes *attributes;
{
    char *s;
    unsigned int a, b, c;

    /* first get a character string */
    a = 0;
    if (!(s = color->string = (char *) XtMalloc(cpp + 1)))
	return (XmXpmNoMemory);
    *s++ = printable[c = a % MAXPRINTABLE];
    for (b = 1; b < cpp; b++, s++)
	*s = printable[c = ((a - c) / MAXPRINTABLE) % MAXPRINTABLE];
    *s = '\0';

    /* then retreive related info from the attributes if any */
    if (attributes && (attributes->valuemask & XmXpmColorTable
			/* 3.2 backward compatibility code */
		       || attributes->valuemask & XmXpmInfos)
			/* end 3.2 bc */
	&& attributes->mask_pixel != XmXpmUndefPixel) {

	unsigned int key;
	char **defaults = (char **) color;
	char **mask_defaults;

	/* 3.2 backward compatibility code */
	if (attributes->valuemask & XmXpmColorTable)
	/* end 3.2 bc */
	    mask_defaults = (char **) (
		attributes->colorTable + attributes->mask_pixel);
	/* 3.2 backward compatibility code */
	else
	    mask_defaults = (char **)
		((XmXpmColor **) attributes->colorTable)[attributes->mask_pixel];
	/* end 3.2 bc */
	for (key = 1; key <= NKEYS; key++) {
	    if ((s = mask_defaults[key])) {
		defaults[key] = (char *) XtNewString(s);
		if (!defaults[key])
		    return (XmXpmNoMemory);
	    }
	}
    } else {
	color->c_color = (char *) XtNewString(TRANSPARENT_COLOR);
	if (!color->c_color)
	    return (XmXpmNoMemory);
    }
    return (XmXpmSuccess);
}

static int
ScanOtherColors(display, colors, ncolors, pixels, mask, cpp, attributes)
    Display *display;
    XmXpmColor *colors;
    int ncolors;
    Pixel *pixels;
    unsigned int mask;
    unsigned int cpp;
    XmXpmAttributes *attributes;
{
    /* variables stored in the XmXpmAttributes structure */
    Colormap colormap;
    char *rgb_fname;

    xpmRgbName rgbn[MAX_RGBNAMES];
    int rgbn_max = 0;
    unsigned int i, j, c, i2;
    XmXpmColor *color;
    XColor *xcolors = NULL, *xcolor;
    char *colorname, *s;
    XmXpmColor *colorTable = NULL, **oldColorTable = NULL;
    unsigned int ancolors = 0;
    Pixel *apixels = NULL;
    unsigned int mask_pixel = 0;
    Bool found;

    /* retrieve information from the XmXpmAttributes */
    if (attributes && (attributes->valuemask & XmXpmColormap))
	colormap = attributes->colormap;
    else
	colormap = XDefaultColormap(display, XDefaultScreen(display));
    if (attributes && (attributes->valuemask & XmXpmRgbFilename))
	rgb_fname = attributes->rgb_fname;
    else
	rgb_fname = NULL;

    /* start from the right element */
    if (mask) {
	colors++;
	ncolors--;
	pixels++;
    }

    /* first get character strings and rgb values */
    xcolors = (XColor *) XtMalloc(sizeof(XColor) * ncolors);
    if (!xcolors)
	return (XmXpmNoMemory);

    for (i = 0, i2 = mask, color = colors, xcolor = xcolors;
	 i < ncolors; i++, i2++, color++, xcolor++, pixels++) {

	if (!(s = color->string = (char *) XtMalloc(cpp + 1))) {
	    XtFree((char *)xcolors);
	    return (XmXpmNoMemory);
	}
	*s++ = printable[c = i2 % MAXPRINTABLE];
	for (j = 1; j < cpp; j++, s++)
	    *s = printable[c = ((i2 - c) / MAXPRINTABLE) % MAXPRINTABLE];
	*s = '\0';

	xcolor->pixel = *pixels;
    }
    XQueryColors(display, colormap, xcolors, ncolors);

    /* read the rgb file if any was specified */
    if (rgb_fname)
	rgbn_max = _XmxpmReadRgbNames(attributes->rgb_fname, rgbn);

    if (attributes && attributes->valuemask & XmXpmColorTable) {
	colorTable = attributes->colorTable;
	ancolors = attributes->ncolors;
	apixels = attributes->pixels;
	mask_pixel = attributes->mask_pixel;
    }
    /* 3.2 backward compatibility code */
    else if (attributes && attributes->valuemask & XmXpmInfos) {
	oldColorTable = (XmXpmColor **) attributes->colorTable;
	ancolors = attributes->ncolors;
	apixels = attributes->pixels;
	mask_pixel = attributes->mask_pixel;
    }
    /* end 3.2 bc */

    for (i = 0, color = colors, xcolor = xcolors; i < ncolors;
						  i++, color++, xcolor++) {

	/* look for related info from the attributes if any */
	found = False;
	if (ancolors) {
	    unsigned int offset = 0;

	    for (j = 0; j < ancolors; j++) {
		if (j == mask_pixel) {
		    offset = 1;
		    continue;
		}
		if (apixels[j - offset] == xcolor->pixel)
		    break;
	    }
	    if (j != ancolors) {
		unsigned int key;
		char **defaults = (char **) color;
		char **adefaults;

		/* 3.2 backward compatibility code */
		if (oldColorTable)
		    adefaults = (char **) oldColorTable[j];
		else
		/* end 3.2 bc */
		    adefaults = (char **) (colorTable + j);

		found = True;
		for (key = 1; key <= NKEYS; key++) {
		    if ((s = adefaults[key]))
			defaults[key] = (char *) XtNewString(s);
		}
	    }
	}
	if (!found) {
	    /* if nothing found look for a color name */
	    colorname = NULL;
	    if (rgbn_max)
		colorname = _XmxpmGetRgbName(rgbn, rgbn_max, xcolor->red,
					  xcolor->green, xcolor->blue);
	    if (colorname)
		color->c_color = (char *) XtNewString(colorname);
	    else {
		/* at last store the rgb value */
		char buf[BUFSIZ];
		sprintf(buf, "#%04X%04X%04X",
			xcolor->red, xcolor->green, xcolor->blue);
		color->c_color = (char *) XtNewString(buf);
	    }
	    if (!color->c_color) {
		XtFree((char *)xcolors);
		_XmxpmFreeRgbNames(rgbn, rgbn_max);
		return (XmXpmNoMemory);
	    }
	}
    }

    XtFree((char *)xcolors);
    _XmxpmFreeRgbNames(rgbn, rgbn_max);
    return (XmXpmSuccess);
}

/*
 * The functions below are written from X11R5 MIT's code (XImUtil.c)
 *
 * The idea is to have faster functions than the standard XGetPixel function
 * to scan the image data. Indeed we can speed up things by suppressing tests
 * performed for each pixel. We do exactly the same tests but at the image
 * level. Assuming that we use only ZPixmap images.
 *
 * Default method to scan pixels of a Z image data structure.
 * The algorithm used is:
 *
 *	copy the source bitmap_unit or Zpixel into temp
 *	normalize temp if needed
 *	extract the pixel bits into return value
 *
 */
static int
GetImagePixels(image, width, height, pmap)
    XImage *image;
    unsigned int width;
    unsigned int height;
    PixelsMap *pmap;
{
    char *src;
    char *dst;
    unsigned int *iptr;
    char *data;
    int x, y, i;
    int bits, depth, ibu, ibpp, offset;
    unsigned long lbt;
    Pixel pixel, px;

    data = image->data;
    iptr = pmap->pixelindex;
    depth = image->depth;
    lbt = low_bits_table[depth];
    ibpp = image->bits_per_pixel;
    offset = image->xoffset;

    if ((image->bits_per_pixel | image->depth) == 1) {
	ibu = image->bitmap_unit;
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		src = &data[XYINDEX(x, y, image)];
		dst = (char *) &pixel;
		pixel = 0;
		for (i = ibu >> 3; --i >= 0;)
		    *dst++ = *src++;
		XYNORMALIZE(&pixel, image);
		bits = (x + offset) % ibu;
		pixel = ((((char *) &pixel)[bits >> 3]) >> (bits & 7)) & 1;
		if (ibpp != depth)
		    pixel &= lbt;
		if (storePixel(pixel, pmap, iptr))
		    return (XmXpmNoMemory);
	    }
    } else if (image->format == XYPixmap) {
	int nbytes, bpl, j;
	long plane = 0;
	ibu = image->bitmap_unit;
	nbytes = ibu >> 3;
	bpl = image->bytes_per_line;
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		pixel = 0;
		plane = 0;
		for (i = depth; --i >= 0;) {
		    src = &data[XYINDEX(x, y, image) + plane];
		    dst = (char *) &px;
		    px = 0;
		    for (j = nbytes; --j >= 0;)
			*dst++ = *src++;
		    XYNORMALIZE(&px, image);
		    bits = (x + offset) % ibu;
		    pixel = (pixel << 1) |
			    (((((char *) &px)[bits >> 3]) >> (bits & 7)) & 1);
		    plane = plane + (bpl * height);
		}
		if (ibpp != depth)
		    pixel &= lbt;
		if (storePixel(pixel, pmap, iptr))
		    return (XmXpmNoMemory);
	    }
    } else if (image->format == ZPixmap) {
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		src = &data[ZINDEX(x, y, image)];
		dst = (char *) &px;
		px = 0;
		for (i = (ibpp + 7) >> 3; --i >= 0;)
		    *dst++ = *src++;
		ZNORMALIZE(&px, image);
		pixel = 0;
		for (i = sizeof(unsigned long); --i >= 0;)
		    pixel = (pixel << 8) | ((unsigned char *) &px)[i];
		if (ibpp == 4) {
		    if (x & 1)
			pixel >>= 4;
		    else
			pixel &= 0xf;
		}
		if (ibpp != depth)
		    pixel &= lbt;
		if (storePixel(pixel, pmap, iptr))
		    return (XmXpmNoMemory);
	    }
    } else
	return (XmXpmColorError); /* actually a bad image */
    return (XmXpmSuccess);
}

/*
 * scan pixels of a 32-bits Z image data structure
 */
#if !defined(WORD64) && !defined(LONG64)
static unsigned long byteorderpixel = MSBFirst << 24;
#endif

static int
GetImagePixels32(image, width, height, pmap)
    XImage *image;
    unsigned int width;
    unsigned int height;
    PixelsMap *pmap;
{
    unsigned char *addr;
    unsigned char *data;
    unsigned int *iptr;
    int x, y;
    unsigned long lbt;
    Pixel pixel;
    int depth;

    data = (unsigned char *) image->data;
    iptr = pmap->pixelindex;
    depth = image->depth;
    lbt = low_bits_table[depth];
#if !defined(WORD64) && !defined(LONG64)
    if (*((char *) &byteorderpixel) == image->byte_order) {
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		addr = &data[ZINDEX32(x, y, image)];
		pixel = *((unsigned long *) addr);
		if (depth != 32)
		    pixel &= lbt;
		if (storePixel(pixel, pmap, iptr))
		    return (XmXpmNoMemory);
	    }
    } else
#endif
    if (image->byte_order == MSBFirst)
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		addr = &data[ZINDEX32(x, y, image)];
		pixel = ((unsigned long) addr[0] << 24 |
			 (unsigned long) addr[1] << 16 |
			 (unsigned long) addr[2] << 8 |
			 addr[4]);
		if (depth != 32)
		    pixel &= lbt;
		if (storePixel(pixel, pmap, iptr))
		    return (XmXpmNoMemory);
	    }
    else
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		addr = &data[ZINDEX32(x, y, image)];
		pixel = (addr[0] |
			 (unsigned long) addr[1] << 8 |
			 (unsigned long) addr[2] << 16 |
			 (unsigned long) addr[3] << 24);
		if (depth != 32)
		    pixel &= lbt;
		if (storePixel(pixel, pmap, iptr))
		    return (XmXpmNoMemory);
	    }
    return (XmXpmSuccess);
}

/*
 * scan pixels of a 16-bits Z image data structure
 */

static int
GetImagePixels16(image, width, height, pmap)
    XImage *image;
    unsigned int width;
    unsigned int height;
    PixelsMap *pmap;
{
    unsigned char *addr;
    unsigned char *data;
    unsigned int *iptr;
    int x, y;
    unsigned long lbt;
    Pixel pixel;
    int depth;

    data = (unsigned char *) image->data;
    iptr = pmap->pixelindex;
    depth = image->depth;
    lbt = low_bits_table[depth];
    if (image->byte_order == MSBFirst)
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		addr = &data[ZINDEX16(x, y, image)];
		pixel = addr[0] << 8 | addr[1];
		if (depth != 16)
		    pixel &= lbt;
		if (storePixel(pixel, pmap, iptr))
		    return (XmXpmNoMemory);
	    }
    else
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		addr = &data[ZINDEX16(x, y, image)];
		pixel = addr[0] | addr[1] << 8;
		if (depth != 16)
		    pixel &= lbt;
		if (storePixel(pixel, pmap, iptr))
		    return (XmXpmNoMemory);
	    }
    return (XmXpmSuccess);
}

/*
 * scan pixels of a 8-bits Z image data structure
 */

static int
GetImagePixels8(image, width, height, pmap)
    XImage *image;
    unsigned int width;
    unsigned int height;
    PixelsMap *pmap;
{
    unsigned int *iptr;
    unsigned char *data;
    int x, y;
    unsigned long lbt;
    Pixel pixel;
    int depth;

    data = (unsigned char *) image->data;
    iptr = pmap->pixelindex;
    depth = image->depth;
    lbt = low_bits_table[depth];
    for (y = 0; y < height; y++)
	for (x = 0; x < width; x++, iptr++) {
	    pixel = data[ZINDEX8(x, y, image)];
	    if (depth != 8)
		pixel &= lbt;
	    if (storePixel(pixel, pmap, iptr))
		return (XmXpmNoMemory);
	}
    return (XmXpmSuccess);
}

/*
 * scan pixels of a 1-bit depth Z image data structure
 */

static int
GetImagePixels1(image, width, height, pmap, storeFunc)
    XImage *image;
    unsigned int width;
    unsigned int height;
    PixelsMap *pmap;
    int (*storeFunc) ();

{
    unsigned int *iptr;
    int x, y;
    char *data;
    Pixel pixel;
    int xoff, yoff, offset, bpl;

    data = image->data;
    iptr = pmap->pixelindex;
    offset = image->xoffset;
    bpl = image->bytes_per_line;

    if (image->bitmap_bit_order == MSBFirst)
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		xoff = x + offset;
		yoff = y * bpl + (xoff >> 3);
		xoff &= 7;
		pixel = (data[yoff] & (0x80 >> xoff)) ? 1 : 0;
		if ((*storeFunc) (pixel, pmap, iptr))
		    return (XmXpmNoMemory);
	    }
    else
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		xoff = x + offset;
		yoff = y * bpl + (xoff >> 3);
		xoff &= 7;
		pixel = (data[yoff] & (1 << xoff)) ? 1 : 0;
		if ((*storeFunc) (pixel, pmap, iptr))
		    return (XmXpmNoMemory);
	    }
    return (XmXpmSuccess);
}

/* function call in case of error, frees only locally allocated variables */
#undef RETURN
#define RETURN(status) \
{ \
    if (pmap.pixelindex) XtFree((char *)pmap.pixelindex); \
    if (pmap.pixels) XtFree((char *)pmap.pixels); \
    if (colorTable) _XmxpmFreeColorTable(colorTable, pmap.ncolors); \
    return(status); \
}

/*
 * This function scans the given image and stores the found informations in
 * the given XmXpmImage structure.
 */
static int
_XmXpmCreateXpmImageFromImage(display, image, shapeimage,
			   xpmimage, attributes)
    Display *display;
    XImage *image;
    XImage *shapeimage;
    XmXpmImage *xpmimage;
    XmXpmAttributes *attributes;
{
    /* variables stored in the XmXpmAttributes structure */
    unsigned int cpp;

    /* variables to return */
    PixelsMap pmap;
    XmXpmColor *colorTable = NULL;
    int ErrorStatus = XmXpmSuccess;

    /* calculation variables */
    unsigned int width = 0;
    unsigned int height = 0;
    unsigned int cppm;			/* minimum chars per pixel */
    unsigned int c;

    /* initialize pmap */
    pmap.pixels = NULL;
    pmap.pixelindex = NULL;
    pmap.size = 256;			/* should be enough most of the time */
    pmap.ncolors = 0;
    pmap.mask_pixel = 0;

    /*
     * get geometry
     */
    if (image) {
	width = image->width;
	height = image->height;
    } else if (shapeimage) {
	width = shapeimage->width;
	height = shapeimage->height;
    }

    /*
     * retrieve information from the XmXpmAttributes
     */
    if (attributes && (attributes->valuemask & XmXpmCharsPerPixel
			/* 3.2 backward compatibility code */
		       || attributes->valuemask & XmXpmInfos))
			/* end 3.2 bc */
	cpp = attributes->cpp;
    else
	cpp = 0;

    pmap.pixelindex =
	(unsigned int *) XtCalloc(width * height, sizeof(unsigned int));
    if (!pmap.pixelindex)
	RETURN(XmXpmNoMemory);

    pmap.pixels = (Pixel *) XtMalloc(sizeof(Pixel) * pmap.size);
    if (!pmap.pixels)
	RETURN(XmXpmNoMemory);

    /*
     * scan shape mask if any
     */
    if (shapeimage) {
	ErrorStatus = GetImagePixels1(shapeimage, width, height, &pmap,
				      storeMaskPixel);
	if (ErrorStatus != XmXpmSuccess)
	    RETURN(ErrorStatus);
    }

    /*
     * scan the image data
     * 
     * In case depth is 1 or bits_per_pixel is 4, 6, 8, 24 or 32 use optimized
     * functions, otherwise use slower but sure general one.
     * 
     */

    if (image) {
	if (((image->bits_per_pixel | image->depth) == 1)  &&
	    (image->byte_order == image->bitmap_bit_order))
	    ErrorStatus = GetImagePixels1(image, width, height, &pmap,
					  storePixel);
	else if (image->format == ZPixmap) {
	    if (image->bits_per_pixel == 8)
		ErrorStatus = GetImagePixels8(image, width, height, &pmap);
	    else if (image->bits_per_pixel == 16)
		ErrorStatus = GetImagePixels16(image, width, height, &pmap);
	    else if (image->bits_per_pixel == 32)
		ErrorStatus = GetImagePixels32(image, width, height, &pmap);
	} else
	    ErrorStatus = GetImagePixels(image, width, height, &pmap);
	if (ErrorStatus != XmXpmSuccess)
	    RETURN(ErrorStatus);
    }

    /*
     * get rgb values and a string of char, and possibly a name for each
     * color
     */

    colorTable = (XmXpmColor *) XtCalloc(pmap.ncolors, sizeof(XmXpmColor));
    if (!colorTable)
	RETURN(XmXpmNoMemory);

    /* compute the minimal cpp */
    for (cppm = 1, c = MAXPRINTABLE; pmap.ncolors > c; cppm++)
	c *= MAXPRINTABLE;
    if (cpp < cppm)
	cpp = cppm;

    if (pmap.mask_pixel) {
	ErrorStatus = ScanTransparentColor(colorTable, cpp, attributes);
	if (ErrorStatus != XmXpmSuccess)
	    RETURN(ErrorStatus);
    }

    ErrorStatus = ScanOtherColors(display, colorTable, pmap.ncolors,
				  pmap.pixels, pmap.mask_pixel, cpp,
				  attributes);
    if (ErrorStatus != XmXpmSuccess)
	RETURN(ErrorStatus);

    /*
     * store found informations in the XmXpmImage structure
     */
    xpmimage->width = width;
    xpmimage->height = height;
    xpmimage->cpp = cpp;
    xpmimage->ncolors = pmap.ncolors;
    xpmimage->colorTable = colorTable;
    xpmimage->data = pmap.pixelindex;

    XtFree((char *)pmap.pixels);
    return (XmXpmSuccess);
}

static void
_XmxpmCreateImageFromPixmap(display, pixmap, ximage_return, width, height)
    Display *display;
    Pixmap pixmap;
    XImage **ximage_return;
    unsigned int *width;
    unsigned int *height;
{
    unsigned int dum;
    int dummy;
    Window win;

    if (*width == 0 && *height == 0)
	XGetGeometry(display, pixmap, &win, &dummy, &dummy,
		     width, height, &dum, &dum);

    *ximage_return = XGetImage(display, pixmap, 0, 0, *width, *height,
			       AllPlanes, ZPixmap);
}

/*************************** EXTERNAL INTERFACE ****************************/
/*
 * Free the XmXpmAttributes structure members
 * but not the structure itself
 */
void
_XmXpmFreeAttributes(attributes)
    XmXpmAttributes *attributes;
{
    if (attributes->valuemask & XmXpmReturnPixels && attributes->npixels) {
	XtFree((char *)attributes->pixels);
	attributes->pixels = NULL;
	attributes->npixels = 0;
    }
    if (attributes->valuemask & XmXpmReturnColorTable) {
	_XmxpmFreeColorTable(attributes->colorTable, attributes->ncolors);
	attributes->colorTable = NULL;
	attributes->ncolors = 0;
    }
    /* 3.2 backward compatibility code */
    else if (attributes->valuemask & XmXpmInfos) {
	if (attributes->colorTable) {
	    FreeOldColorTable((XmXpmColor **) attributes->colorTable,
			      attributes->ncolors);
	    attributes->colorTable = NULL;
	    attributes->ncolors = 0;
	}
	if (attributes->hints_cmt) {
	    XtFree(attributes->hints_cmt);
	    attributes->hints_cmt = NULL;
	}
	if (attributes->colors_cmt) {
	    XtFree(attributes->colors_cmt);
	    attributes->colors_cmt = NULL;
	}
	if (attributes->pixels_cmt) {
	    XtFree(attributes->pixels_cmt);
	    attributes->pixels_cmt = NULL;
	}
	if (attributes->pixels) {
	    XtFree((char *)attributes->pixels);
	    attributes->pixels = NULL;
	    attributes->npixels = 0;
	}
    }
    /* end 3.2 bc */
    if (attributes->valuemask & XmXpmReturnExtensions
	&& attributes->nextensions) {
	_XmXpmFreeExtensions(attributes->extensions, attributes->nextensions);
	attributes->extensions = NULL;
	attributes->nextensions = 0;
    }
    if (attributes->valuemask & XmXpmReturnAllocPixels
	&& attributes->nalloc_pixels) {
	XtFree((char *)attributes->alloc_pixels);
	attributes->alloc_pixels = NULL;
	attributes->nalloc_pixels = 0;
    }
    attributes->valuemask = 0;
}

/*
 * create data from an image
 */
int
_XmXpmCreateDataFromImage(display, data_return, image, shapeimage, attributes)
    Display *display;
    char ***data_return;
    XImage *image;
    XImage *shapeimage;
    XmXpmAttributes *attributes;
{
    XmXpmImage xpmimage;
    XmXpmInfo info;
    int ErrorStatus;

    /* initialize return value */
    if (data_return)
	*data_return = NULL;

    /* create an XmXpmImage from the image */
    ErrorStatus = _XmXpmCreateXpmImageFromImage(display, image, shapeimage,
					     &xpmimage, attributes);
    if (ErrorStatus != XmXpmSuccess)
	return (ErrorStatus);

    /* create the data from the XmXpmImage */
    if (attributes) {
	_XmxpmSetInfo(&info, attributes);
	ErrorStatus = _XmXpmCreateDataFromXpmImage(data_return, &xpmimage, &info);
    } else
	ErrorStatus = _XmXpmCreateDataFromXpmImage(data_return, &xpmimage, NULL);

    /* free the XmXpmImage */
    _XmXpmFreeXpmImage(&xpmimage);

    return (ErrorStatus);
}

/*
 * create data from a pixmap
 */
int
_XmXpmCreateDataFromPixmap(display, data_return, pixmap, shapemask, attributes)
    Display *display;
    char ***data_return;
    Pixmap pixmap;
    Pixmap shapemask;
    XmXpmAttributes *attributes;
{
    XImage *ximage = NULL;
    XImage *shapeimage = NULL;
    unsigned int width = 0;
    unsigned int height = 0;
    int ErrorStatus;

    /* get geometry */
    if (attributes && attributes->valuemask & XmXpmSize) {
	width = attributes->width;
	height = attributes->height;
    }
    /* get the ximages */
    if (pixmap)
	_XmxpmCreateImageFromPixmap(display, pixmap, &ximage, &width, &height);
    if (shapemask)
	_XmxpmCreateImageFromPixmap(display, shapemask, &shapeimage,
				 &width, &height);

    /* create the data */
    ErrorStatus = _XmXpmCreateDataFromImage(display, data_return, ximage,
					 shapeimage, attributes);

    /* destroy the ximages */
    if (ximage)
	XDestroyImage(ximage);
    if (shapeimage)
	XDestroyImage(shapeimage);

    return (ErrorStatus);
}

/*
 * create an XImage from data
 */
int
_XmXpmCreateImageFromData(display, data, image_return,
		       shapeimage_return, attributes)
    Display *display;
    char **data;
    XImage **image_return;
    XImage **shapeimage_return;
    XmXpmAttributes *attributes;
{
    XmXpmImage image;
    XmXpmInfo info;
    int ErrorStatus;

    /* create an XmXpmImage from the file */
    if (attributes) {
	_XmxpmInitAttributes(attributes);
	_XmxpmSetInfoMask(&info, attributes);
	ErrorStatus = _XmXpmCreateXpmImageFromData(data, &image, &info);
    } else
	ErrorStatus = _XmXpmCreateXpmImageFromData(data, &image, NULL);

    if (ErrorStatus != XmXpmSuccess)
	return (ErrorStatus);

    /* create the related ximages */
    ErrorStatus = _XmXpmCreateImageFromXpmImage(display, &image,
					     image_return, shapeimage_return,
					     attributes);
    if (attributes) {
	if (ErrorStatus >= 0)		/* no fatal error */
	    _XmxpmSetAttributes(attributes, &image, &info);
	_XmXpmFreeXpmInfo(&info);
    }
    _XmXpmFreeXpmImage(&image);

    return (ErrorStatus);
}

/*
 * read a pixmap file into an XImage
 */
int
_XmXpmReadFileToImage(display, filename,
		   image_return, shapeimage_return, attributes)
    Display *display;
    char *filename;
    XImage **image_return;
    XImage **shapeimage_return;
    XmXpmAttributes *attributes;
{
    XmXpmImage image;
    XmXpmInfo info;
    int ErrorStatus;

    /* create an XmXpmImage from the file */
    if (attributes) {
	_XmxpmInitAttributes(attributes);
	_XmxpmSetInfoMask(&info, attributes);
	ErrorStatus = _XmXpmReadFileToXpmImage(filename, &image, &info);
    } else
	ErrorStatus = _XmXpmReadFileToXpmImage(filename, &image, NULL);

    if (ErrorStatus != XmXpmSuccess)
	return (ErrorStatus);

    /* create the related ximages */
    ErrorStatus = _XmXpmCreateImageFromXpmImage(display, &image,
					     image_return, shapeimage_return,
					     attributes);
    if (attributes) {
	if (ErrorStatus >= 0)		/* no fatal error */
	    _XmxpmSetAttributes(attributes, &image, &info);
	_XmXpmFreeXpmInfo(&info);
    }
    /* free the XmXpmImage */
    _XmXpmFreeXpmImage(&image);

    return (ErrorStatus);
}


static void
_XmxpmCreatePixmapFromImage(display, d, ximage, pixmap_return)
    Display *display;
    Drawable d;
    XImage *ximage;
    Pixmap *pixmap_return;
{
    GC gc;
    XGCValues values;

    *pixmap_return = XCreatePixmap(display, d, ximage->width,
                                   ximage->height, ximage->depth);
    /* set fg and bg in case we have an XYBitmap */
    values.foreground = 1;
    values.background = 0;
    gc = XCreateGC(display, *pixmap_return,
                   GCForeground | GCBackground, &values);
       
    XPutImage(display, *pixmap_return, gc, ximage, 0, 0, 0, 0,
              ximage->width, ximage->height);
       
    XFreeGC(display, gc);
}   

int
_XmXpmReadFileToPixmap(display, d, filename, pixmap_return,
                    shapemask_return, attributes)
    Display *display;
    Drawable d;
    char *filename;
    Pixmap *pixmap_return;
    Pixmap *shapemask_return;
    XmXpmAttributes *attributes;
{   
    XImage *ximage, *shapeimage;
    int ErrorStatus;
    
    /* initialize return values */
    if (pixmap_return)
        *pixmap_return = 0;
    if (shapemask_return)
        *shapemask_return = 0;
       
    /* create the images */
    ErrorStatus = _XmXpmReadFileToImage(display, filename,
                                     (pixmap_return ? &ximage : NULL),
                                     (shapemask_return ? &shapeimage : NULL),
                                     attributes);
       
    if (ErrorStatus < 0)                /* fatal error */
        return (ErrorStatus);
       
    /* create the pixmaps and destroy images */
    if (pixmap_return && ximage) {
        _XmxpmCreatePixmapFromImage(display, d, ximage, pixmap_return);
        XDestroyImage(ximage);
    }  
    if (shapemask_return && shapeimage) {
        _XmxpmCreatePixmapFromImage(display, d, shapeimage, shapemask_return);
        XDestroyImage(shapeimage);
    }  
    return (ErrorStatus);
}   

