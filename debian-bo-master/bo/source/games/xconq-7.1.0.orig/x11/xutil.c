/* Utilities needed for X11 utility programs.
   Copyright (C) 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "config.h"
#include "misc.h"
#include "lisp.h"
#include "imf.h"
#include "xutil.h"
#include "ximf.h"

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#include <X11/Xmu/Drawing.h>

#ifdef HAVE_XPM
#include <X11/xpm.h>
#endif

extern int numimages;
char *outdirname = NULL;
char *imflib = "";

LibraryPath *xconq_libs;

/* this is needed to resolve
   extern char spbuf[];
   declared in kernel/unix.c */
char spbuf[BUFSIZE];

void
close_displays()
{
}

int
write_entire_game_state(fname)
char *fname;
{
    return 0;
}

/* Need these so we can link in Lisp reader code. */

void
announce_read_progress()
{
}

void
low_init_error(str)
char *str;
{
    fprintf(stderr, "Error: %s.\n", str);
    fflush(stderr);
}

/* A warning just gets displayed, no other action is taken. */

void
low_init_warning(str)
char *str;
{
    fprintf(stdout, "Warning: %s.\n", str);
    fflush(stdout);
}

/* A run error is fatal. */

void
low_run_error(str)
char *str;
{
    fprintf(stderr, "Error: %s.\n", str);
    fflush(stderr);
    exit(1);
}

/* Runtime warnings are for when it's important to bug the players,
   usually a problem with Xconq or a game design. */

void
low_run_warning(str)
char *str;
{
    fprintf(stdout, "Warning: %s.\n", str);
    fflush(stdout);
}

/* Fake definitions of unneeded routines called by lisp.c. */

int
keyword_code(str)
char *str;
{
    run_warning("fake keyword_code being called");
    return 0;
}

/* Make the table so keyword lookup works. */

struct a_key {
    char *name;
} keywordtable[] = {

#undef  DEF_KWD
#define DEF_KWD(NAME,code)  { NAME },

#include "keyword.def"

    { NULL }
};

char *
keyword_name(k)
enum keywords k;
{
    return keywordtable[k].name;
}

void
init_predefined_symbols()
{
}

int
lazy_bind(sym)
Obj *sym;
{
    run_warning("fake lazy_bind being called");
    return FALSE;
}

void
prealloc_debug()
{
}

/* read/write images in X format. */

Image *
read_xbm_file(filename, imf, hook)
char *filename;
ImageFamily *imf;
readimf_hook hook;
{
    FILE *ifp;
    int rows, cols, rowbytes;
    unsigned int urows, ucols;
    unsigned char *urawdata;
    char *rawdata;
    Image *img;

    if (!imf)
      return NULL;
    if (!(ifp = fopen(filename, "r")))
      return NULL;

    if (XmuReadBitmapData(ifp, &ucols, &urows, &urawdata, NULL, NULL)
	!= BitmapSuccess)
      return NULL;
    cols = ucols;
    rows = urows;
    rawdata = (char *) urawdata;
    fclose(ifp);
    img = get_img(imf, cols, rows);
    if (!img)
      return NULL;
    if (hook) {
	/* force image re-generation */
	img->monodata = img->colrdata = img->maskdata = lispnil;
    }
    rowbytes = (cols + 7) / 8;
    if (filename[strlen(filename)-1] == 'm') {
	img->rawmaskdata = xmalloc(rows * rowbytes);
	memcpy(img->rawmaskdata, rawdata, rows * rowbytes);
    } else {
	img->rawmonodata = xmalloc(rows * rowbytes);
	memcpy(img->rawmonodata, rawdata, rows * rowbytes);
    }
    return img;
}

Image *
read_xpm_file(filename, imf, hook)
char *filename;
ImageFamily *imf;
readimf_hook hook;
{
#ifdef HAVE_XPM
    int c, numbytes, *dp, r, ri, cols, rows;
    char *rp, rmask;
    XpmImage xpmimage;
    XpmInfo info;
    Image *img;

    if (!imf)
      return NULL;
    if (XpmReadFileToXpmImage(filename, &xpmimage, &info) != XpmSuccess) {
	return NULL;
    }
    cols = xpmimage.width;
    rows = xpmimage.height;
    img = get_img(imf, cols, rows);
    if (!img)  return NULL;
    if (hook) {
	/* force image re-generation */
	img->monodata = img->colrdata = img->maskdata = lispnil;
    }
    img->actualw = cols;
    img->actualh = rows;
    img->numcolors = xpmimage.ncolors;
    if (xpmimage.ncolors>256) {
	low_run_warning("Image with more then 256 colors not supported\n");
	return NULL;
    } else if (xpmimage.ncolors>16) {
	img->pixelsize = 8;
    } else if (xpmimage.ncolors>4) {
	img->pixelsize = 4;
    } else if (xpmimage.ncolors>2) {
	img->pixelsize = 2;
    } else {
	img->pixelsize = 1;
    }
    img->rowbytes = (img->w*img->pixelsize + 7) / 8;
    img->rawpalette = (int *) xmalloc(img->numcolors*4*sizeof(int));
    for (c=0; c<img->numcolors; c++) {
	img->rawpalette[4*c] = c;
	parse_xpm_colors(xpmimage.colorTable[c].c_color,
			 &img->rawpalette[4*c+1],
			 &img->rawpalette[4*c+2],
			 &img->rawpalette[4*c+3]);
    }
    /* this seems to be Stan's default ;-) */
    img->rawpalette[4*(img->numcolors-1)] = (1<<img->pixelsize)-1;
    numbytes = img->h * img->rowbytes;
    img->rawcolrdata = xmalloc(numbytes);
    memset(img->rawcolrdata, '\0', numbytes);
    rp = img->rawcolrdata;
    dp = (int *) xpmimage.data;
    rmask = (1<<img->pixelsize) - 1;
    for (r=0; r<img->h; r++) {
	ri = 8 - img->pixelsize;
	for (c=0; c<img->w; c++) {
	    *rp |= (img->rawpalette[4*(*dp)] & rmask)<<ri;
	    dp++;
	    if (ri) {
		ri -= img->pixelsize;
	    } else {
		ri = 8 - img->pixelsize;
		rp++;
	    }
	}
	if ((img->pixelsize*img->w)%8) {
	    rp++;
	}
    }

    return img;
#else
    return NULL;
#endif
}

/* half-witted replacement for XParseColor, which we can't use
   since we could have no display open */
void
parse_xpm_colors(name, r, g, b)
char *name;
int *r;
int *g; 
int *b; 
{
    int n, q;
    char xpmcbuf[32];

    /* sometimes "pixmap" comes up with these: */
    if (!strcmp(name,"white") || !strcmp(name,"White")) {
	*r = *g = *b = 0xffff;
	return;
    }
    if (!strcmp(name,"black") || !strcmp(name,"Black")) {
	*r = *g = *b = 0;
	return;
    }


    if (name[0]!='#') {
	fprintf(stderr, "Error parsing color %s\n", name);
	return;
    }
    name++;
    n = strlen(name);
    if (n%3) {
	fprintf(stderr, "Error parsing color %s\n", name);
	return;
    }
    n /= 3;
    sprintf(xpmcbuf, "%%%dx%%%dx%%%dx", n, n, n);
    q = sscanf(name, xpmcbuf, r, g, b);
    if (q!=3) {
	fprintf(stderr, "Error parsing color %s\n", name);
	return;
    }
    if (n<4) {
	*r <<= 4*(4-n);
	*g <<= 4*(4-n);
	*b <<= 4*(4-n);
    }
}

void
write_x11_bitmaps(imf, mkfiles)
ImageFamily *imf;
int mkfiles;
{
    int w, h, rowbytes, numbytes;
    char ext[20], fname[255];
    FILE *fp;
    Image *img;

    if (imf == NULL || imf->name == NULL) return;
    for (img = imf->images; img != NULL; img = img->next) {
	w = img->w;  h = img->h;
	/* unified size marker in extension.  Massimo */
	sprintf(ext, ".%dx%d", w, h);
	if (img->monodata != lispnil && img->rawmonodata == NULL) {
	    rowbytes = (w + 7) / 8;
	    numbytes = h * rowbytes;
	    img->rawmonodata = xmalloc(numbytes);
	    interp_bytes(img->monodata, numbytes, img->rawmonodata, 0);
	    reverse_bit_endianness(img->rawmonodata, numbytes);
	}
	if (img->rawmonodata) {
	    if (mkfiles) {
		sprintf(fname, "%s/%s%s.b", outdirname, imf->name, ext);
		fp = fopen(fname, "w");
	    } else {
		fp = stdout;
	    }
	    if (fp != NULL) {
		write_xbm_file(fp, imf->name, w, h, img->rawmonodata);
		if (fp != stdout)
		  fclose(fp);
	    }
	}
	if (img->maskdata != lispnil && img->rawmaskdata == NULL) {
	    rowbytes = (w + 7) / 8;
	    numbytes = h * rowbytes;
	    img->rawmaskdata = xmalloc(numbytes);
	    interp_bytes(img->maskdata, numbytes, img->rawmaskdata, 0);
	    reverse_bit_endianness(img->rawmaskdata, numbytes);
	}
	if (img->rawmaskdata) {
	    if (mkfiles) {
		sprintf(fname, "%s/%s%s.m", outdirname, imf->name, ext);
		fp = fopen(fname, "w");
	    } else {
		fp = stdout;
	    }
	    if (fp != NULL) {
		write_xbm_file(fp, imf->name, w, h, img->rawmaskdata);
		if (fp != stdout)
		  fclose(fp);
	    }
	}
	if (img->colrdata != lispnil && img->rawcolrdata == NULL) {
	    rowbytes = (w * img->pixelsize + 7) / 8;
	    numbytes = h * rowbytes;
	    img->rawcolrdata = xmalloc(numbytes);
	    interp_bytes(img->colrdata, numbytes, img->rawcolrdata, 0);
	}
	if (img->rawcolrdata) {
	    if (mkfiles) {
		sprintf(fname, "%s/%s%s.xpm", outdirname, imf->name, ext);
		fp = fopen(fname, "w");
	    } else {
		fp = stdout;
	    }
	    if (fp != NULL) {
		write_xpm_file(fp, imf->name, img);
		if (fp != stdout)
		  fclose(fp);
	    }
	}
    }
}

/* Write a bitmap in more-or-less standard X11 format. */

void
write_xbm_file(fp, name, cols, rows, data)
FILE *fp;
char *name;
int cols, rows;
char *data;
{
    int row, i = 0, col;
    int bytesperline = 8;
    int byte, byte2, j, firstitem = 1;
    int numbytes = ((cols + 7) / 8) * rows;

    reverse_bit_endianness(data, numbytes);

    fprintf(fp, "#define %s_width %d\n", name, cols);
    fprintf(fp, "#define %s_height %d\n", name, rows);
    fprintf(fp, "static char %s_bits[] = {", name);
    for (row = 0; row < rows; row++) {
	for (col = 0; col < cols; col++) {
	    if (col % 8 == 0) {
		if (firstitem) {
		    firstitem = 0;
		} else {
		    putc(',', fp);
		    putc(' ', fp);
		}
		if (bytesperline >= 8) {
		    fprintf(fp, "\n   ");
		    bytesperline = 0;
		}
		/* Make little-endian bytes. */
		byte = data[i++];
		byte2 = 0;
		for (j = 0; j < 8; ++j) {
		    byte2 = (byte2 << 1) | (byte & 1);
		    byte >>= 1;
		}
		fprintf(fp, "0x%x", byte2);
		++bytesperline;
	    }
	}
    }
    fprintf(fp, "};\n" );

    reverse_bit_endianness(data, numbytes);
}

void
write_xpm_file(fp, name, img)
FILE *fp;
char *name;
Image *img;
{
    int r, ri, rc, c, numcols, rmask;
    char pch[256], *rp;
    int idx[256];
    Obj *palette, *color;

    if (img->palette==lispnil && !(img->rawpalette && img->numcolors))
      return;

    if (img->palette==lispnil) {
	numcols = img->numcolors;
    } else {
	c = 0;
	for (palette=img->palette; palette != lispnil; palette = cdr(palette)) {
	    c++;
	}
	numcols = c;
    }

    fprintf(fp, "/* XPM */\n");
    fprintf(fp, "static char * %s [] = {\n", name);
    fprintf(fp, "\"%d %d %d %d\",\n", img->w, img->h, numcols, 1);

    for (c=0; c<numcols; c++) {
	if (c<26) {
	    pch[c] = 'a' + c;
	} else if (c<52) {
	    pch[c] = 'A' + c-26;
	} else if (c<62) {
	    pch[c] = '0' + c-52;
	} else {
	    pch[c] = '#' + c-62;
	}	
    }  
 
    if (img->palette==lispnil) {
	for (c=0; c<numcols; c++) {
	    fprintf(fp, "\"%c\tc #%4.4x%4.4x%4.4x\",\n", pch[c], 
		    img->rawpalette[4*c+1], img->rawpalette[4*c+2],
		    img->rawpalette[4*c+3]);
	    idx[img->rawpalette[4*c]] = c;
	}
    } else {
	c = 0;
	for (palette=img->palette; palette != lispnil; palette = cdr(palette)) {
	    color = car(palette);
	    fprintf(fp, "\"%c\tc #%4.4x%4.4x%4.4x\",\n", pch[c], 
		    c_number(car(cdr(color))),
		    c_number(car(cdr(cdr(color)))),
		    c_number(car(cdr(cdr(cdr(color)))))
		    );
	    idx[c_number(car(color))] = c;
	    c++;
	}
    }
  
    if (numcols > 74) {
	fprintf(stderr, "write_xpm_file not implemented for %d colors\n",
		numcols);
	return;
    }

    rmask = (1 << img->pixelsize) - 1;
    rp = img->rawcolrdata;
    for (r = 0; r < img->h; r++) {
	ri = 8 - img->pixelsize;
	fputc('"', fp);
	for (c = 0; c < img->w; c++) {
	    rc = ((int) (*rp >> ri)) & rmask;
	    if (ri) {
		ri -= img->pixelsize;
	    } else {
		ri = 8 - img->pixelsize;
		rp++;
	    }
	    fputc(pch[idx[rc]], fp);
	}
	if (r == img->h - 1) {
	    fprintf(fp, "\"};\n");
	} else {
	    fprintf(fp, "\",\n");
	}
	if ((img->pixelsize * img->w) % 8) {
	    rp++;
	}
    }
}

/* Given a raw filename, come up with a image family name. */

char *
find_imf_name(rawname)
char *rawname;
{
    int lastpos;
    char *a, *b;

    b = copy_string(rawname);
    lastpos = strlen(b) - 1;

    /* Remove leading path. */
    for (a = b + lastpos - 1; a > b; --a) {
	if (*a == '/') {
	    b = a + 1;
	    break;
	}
    }

    /* Remove trailing extension. */
    if ((a = strchr(b, '.'))) {
	*a = '\0';
    }

    return b;
}

int
read_any_file(filename, hook)
char *filename;
readimf_hook hook;
{
    ImageFamily *imf;
    char *ext;
    int rc;

    /* find extension */
    for (ext = filename + strlen(filename) - 1; ext > filename; ext--) {
	if (*ext == '.') {
	    ext++;
	    break;
	}
    }
    if (ext == filename)
      ext = "";

#ifdef HAVE_XPM
    if (!strcmp(ext, "xpm")) {
	/* try a XPM file */
	imf = get_imf(find_imf_name(filename));
	if (!imf)
	  return 0;
	rc = !!read_xpm_file(filename, imf, hook);
	if (hook && rc)
	  hook(imf,0);
	return rc;
    }
#endif /* HAVE_XPM */
    if (!strcmp(ext,"b") || !strcmp(ext,"m")) {
	/* try a XBM file */
	imf = get_imf(find_imf_name(filename));
	if (!imf)
	  return 0;
	rc = !!read_xbm_file(filename, imf, hook);
	if (hook && rc)
	  hook(imf,0);
	return rc;
    } else if (strcmp(ext, "imf") == 0) {
	return load_imf_file(filename, hook);
    }
    return 0;
}

void 
reverse_rawdata(imf)
ImageFamily *imf;
{
    int numbytes;
    Image *img;

    if (imf == NULL)
      return;
    for (img = imf->images; img; img = img->next) {
	numbytes = img->h * ((img->w + 7) / 8);
	if (img->rawmonodata)
	  reverse_bit_endianness(img->rawmonodata, numbytes);
	if (img->rawmaskdata)
	  reverse_bit_endianness(img->rawmaskdata, numbytes);
    }
}
