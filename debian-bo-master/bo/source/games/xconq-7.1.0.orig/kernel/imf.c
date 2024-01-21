/* Interpretation of generic GDL images for Xconq.
   Copyright (C) 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* Note!  This file does not use the standard "conq.h" header, so can't assume
   all the usual definitions. */
 
#include "config.h"
#include "misc.h"
#include "lisp.h"
#include "imf.h"

/* RGB above this value should be considered white. */

#define WHITE_THRESHOLD (65535 - 100)

/* RGB below this value should be considered black. */

#define BLACK_THRESHOLD (    0 + 100)

/* (should remove these fixed limits someday) */

#define MAXIMAGEFAMILIES 1000

#define MAXIMAGEPALETTES 100

#define MAXIMAGECOLORS 1000

enum {
    K_MONO_,
    K_MASK_,
    K_COLR_,
    K_OTHER_
};

static ImageFamily *new_imf PARAMS ((char *name));
static int colornamecmp PARAMS ((char *str1, char *str2));
static int bitmaps_match PARAMS ((int w, int h, Obj *lispdata, char *rawdata));
static int color_matches_mono PARAMS ((Image *img));
static void write_pixmap PARAMS ((FILE *fp, int w, int h, int aw, int ah,
				 int pixelsize, int rowbytes,
				 Obj *palette, int *rawpalette, int numcolors,
				 Obj *lispdata, char *rawdata));
static void write_bitmap PARAMS ((FILE *fp, char *subtyp, int w, int h,
				 Obj *data, char *rawdata));
static void write_palette_contents PARAMS ((FILE *fp, Obj *palette,
					    int *rawpalette, int numcolors));

/* This is the array and count of known image families. */

ImageFamily **images;

int numimages = 0;

/* This is the array and count of named palettes. */

ImagePalette **palettes;

int numpalettes = 0;

/* This is the array and count of named colors. */

ImageColor **colors;

int numcolors = 0;

ImageFile *image_files;

/* Create and return an image family. */

static ImageFamily *
new_imf(name)
char *name;
{
    ImageFamily *imf;

    imf = (ImageFamily *) xmalloc(sizeof(ImageFamily));
    imf->name = name;
    imf->notes = lispnil;
    return imf;
}

ImageFamily *
clone_imf(imf)
ImageFamily *imf;
{
    Image *img, *img2, *truenext;
    ImageFamily *imf2;

    imf2 = new_imf(imf->name);
    memcpy(imf2, imf, sizeof(ImageFamily));
    imf2->images = NULL;
    /* Clone the images. */
    for (img = imf->images; img != NULL; img = img->next) {
	img2 = get_img(imf2, img->w, img->h);
	truenext = img2->next;
	memcpy(img2, img, sizeof(Image));
	/* Clear the hook, we expect that the caller of this routine
	   will supply any new hook that might be necessary. */
	img2->hook = NULL;
	/* Restore the link. */
	img2->next = truenext;
	/* Note that pointers to raw image data and suchlike can be
	   left as-is, since they should be shared by image clones. */
    }
    return imf2;
}

/* Test that the given name is a valid image family name (all alphanumeric,
   hyphens anywhere but as first char). */

int
valid_imf_name(name)
char *name;
{
    char *tmp;

    for (tmp = name; *tmp; ++tmp) {
	if (!(isalnum(*tmp)
	      || (tmp != name && *tmp == '-')))
	  return FALSE;
    }
    return TRUE;
}

/* Given a name, find or create an image family with that name. */

ImageFamily *
get_imf(name)
char *name;
{
    ImageFamily *imf = NULL;
    
    if (name == NULL) {
	init_warning("can't get an unnamed imf");
	return NULL;
    }
    if (!valid_imf_name(name)) {
	init_warning("\"%s\" is not a valid imf name", name);
	return NULL;
    }
    if (images == NULL) {
	images =
	  (ImageFamily **) xmalloc(MAXIMAGEFAMILIES * sizeof(ImageFamily *));
    }
    imf = find_imf(name);
    if (imf == NULL) {
	if (numimages >= MAXIMAGEFAMILIES) {
	    return NULL;
	}
	imf = new_imf(name);
	if (imf != NULL) {
	    images[numimages++] = imf;
	}
    }
    return imf;
}

ImageFile *
get_image_file(name)
char *name;
{
    ImageFile *imfile;
    
    if (name == NULL)
      run_error("can't get an unnamed imfile");
    for (imfile = image_files; imfile != NULL; imfile = imfile->next) {
	if (strcmp(name, imfile->name) == 0)
	  return imfile;
    }
    imfile = (ImageFile *) xmalloc(sizeof(ImageFile));
    imfile->name = copy_string(name);
    imfile->next = image_files;
    image_files = imfile;
    return imfile;
}

void
load_image_families(fp, loadnow, callback)
FILE *fp;
int loadnow;
void (*callback) PARAMS ((ImageFamily *imf, int loadnow));
{
    int done = FALSE, rslt, first = TRUE;
    char buf1[80], buf2[80];
    ImageFamily *imf;
    ImageFile *imfile;
    
    while (!done) {
	rslt = fscanf(fp, "%s %s\n", buf1, buf2);
	if (rslt != 2)
	  break;
	else if (strcmp(buf1, ".") == 0
		 && strcmp(buf2, ".") == 0)
	  done = TRUE;
	else if (first) {
	    if (strcmp(buf1, "ImageFamilyName") == 0
		&& strcmp(buf2, "FileName") == 0)
	      first = FALSE;
	    else {
		init_warning("File not a valid imf dir, will close and ignore");
		/* We've already given a warning message, so pretend we're done
		   so the format error message doesn't get displayed below. */
		done = TRUE;
		break;
	    }
	} else {
	    imf = get_imf(copy_string(buf1));
	    if (imf != NULL) {
		imfile = get_image_file(buf2);
		imf->location = imfile;
		if (loadnow && !imfile->loaded) {
		    load_imf_file(imfile->name, callback);
		    imfile->loaded = TRUE;
		} else {
		    if (callback != NULL)
		      (*callback)(imf, loadnow);
		}
	    }
	}
    }
    if (!done) {
	init_warning("Format error in imf dir near %s, will only use part",
		     (imf ? imf->name : "???"));
    }
}

/* Given a filename, open it and read/interpret all the image-related
   forms therein. */

int
load_imf_file(filename, callback)
char *filename;
void (*callback) PARAMS ((ImageFamily *imf, int loadnow));
{
    int startlineno = 1, endlineno = 1;
    Obj *form;
    FILE *fp;

    fp = fopen(filename, "r");
    if (fp != NULL) {
	/* Read everything in the file. */
	while ((form = read_form(fp, &startlineno, &endlineno)) != lispeof) {
	    interp_imf_form(form, callback);
	}
	fclose(fp);
	return TRUE;
    }
    return FALSE;
}

/* Interpret a form, looking specifically for image-related forms. */

void
interp_imf_form(form, imf_callback)
Obj *form;
void (*imf_callback) PARAMS ((ImageFamily *imf, int loadnow));
{
    Obj *head;
    ImageFamily *imf;

    /* Ignore any non-lists, we might be reading from a normal game design. */
    if (!consp(form))
      return;
    head = car(form);
    if (match_keyword(head, K_IMF)) {
	imf = interp_imf(form);
	if (imf_callback != NULL && imf != NULL)
	  (*imf_callback)(imf, TRUE);
    } else if (match_keyword(head, K_PALETTE)) {
	interp_palette(form);
    } else if (match_keyword(head, K_COLOR)) {
	interp_color(form);
    } else {
	/* Ignore any non-image forms, we might be reading from a 
	   normal game design. */
    }
}

/* Find the image family of the given name, if it exists. */

ImageFamily *
find_imf(name)
char *name;
{
    int i;

    for (i = 0; i < numimages; ++i) {
	if (strcmp(name, images[i]->name) == 0)
	  return images[i];
    }
    return NULL;
}

/* Get an image of the given size from the family, creating a new one
   if necessary. */

Image *
get_img(imf, w, h)
ImageFamily *imf;
int w, h;
{
    Image *img, *previmg = NULL;

    for (img = imf->images; img != NULL; img = img->next) {
	if (w == img->w && h == img->h)
	  return img;
	previmg = img;
    }
    /* Not found; create a new image and add it to the family. */
    img = (Image *) xmalloc(sizeof(Image));
    img->w = w;  img->h = h;
    /* Default min/max limits to actual size. */
    img->minw = img->maxw = w;  img->minh = img->maxh = h;
    img->embedx = img->embedy = -1;
    img->embedw = img->embedh = -1;
    img->monodata = img->colrdata = img->maskdata = lispnil;
    img->palette = lispnil;
    img->actualw = w;  img->actualh = h;
    img->pixelsize = img->rowbytes = 0;
    img->notes = lispnil;
    /* Rely on zeroing of xmalloc blocks to avoid clearing other fields. */
    /* Link at front of list of images. */
    if (previmg != NULL)
      previmg->next = img;
    else
      imf->images = img;
    ++(imf->numsizes);
    return img;
}

Image *
find_img(imf, w, h)
ImageFamily *imf;
int w, h;
{
    Image *img;
	
    for (img = imf->images; img != NULL; img = img->next) {
	if (w == img->w && h == img->h)
	  return img;
    }
    return NULL;
}

ImageFamily *
interp_imf(form)
Obj *form;
{
    ImageFamily *imf;

    if (stringp(cadr(form))) {
	imf = get_imf(c_string(cadr(form)));
	if (imf != NULL) {
	    interp_imf_contents(imf, cddr(form));
	}
	return imf;
    } else {
	/* garbage form */
    }
    return NULL;
}

void
interp_imf_contents(imf, clauses)
ImageFamily *imf;
Obj *clauses;
{
    Obj *rest, *clause;

    for (rest = clauses; rest != lispnil; rest = cdr(rest)) {
	clause = car(rest);
	if (consp(clause)) {
	    if (symbolp(car(clause))) {
		if (match_keyword(car(clause), K_NOTES)) {
		    imf->notes = cadr(clause);
		    /* (should complain about non-nil cddr?) */
		} else {
		    /* (should complain about syntax) */
		}
	    } else {
		interp_image(imf, car(clause), cdr(clause));
	    }
	} else {
	    /* garbage? */
	}
    }
}

void
interp_image(imf, size, parts)
ImageFamily *imf;
Obj *size, *parts;
{
    int w, h, imtype, emx, emy;
    char *name;
    Image *img;
    Obj *head, *rest, *typ, *prop, *proptype, *datalist;
    
    w = c_number(car(size));  h = c_number(cadr(size));
    img = get_img(imf, w, h);
    if (img == NULL)
      run_error("no image?");
    if (match_keyword(car(cddr(size)), K_TILE))
      img->istile = TRUE;
    for (rest = parts; rest != lispnil; rest = cdr(rest)) {
	head = car(rest);
	typ = car(head);
	imtype = K_OTHER_;
	if (match_keyword(typ, K_MONO)) {
	    imtype = K_MONO_;
	} else if (match_keyword(typ, K_MASK)) {
	    imtype = K_MASK_;
	} else if (match_keyword(typ, K_COLOR)) {
	    imtype = K_COLR_;
	} else if (match_keyword(typ, K_EMBED)) {
	    name = c_string(cadr(head));
	    if (img->embedname != NULL
		&& strcmp(img->embedname, name) != 0)
	      run_warning("Changing embed name from \"%s\" to \"%s\" in %dx%d image of \"%s\"",
			  img->embedname, name, w, h, imf->name);
	    img->embedname = name;
	} else if (match_keyword(typ, K_EMBED_AT)) {
	    emx = c_number(cadr(head));  emy = c_number(caddr(head));
	    if ((img->embedx >= 0 && emx != img->embedx)
		|| (img->embedy >= 0 && emy != img->embedy))
	      run_warning("Changing embed x,y from %d,%d to %d,%d in %dx%d image of \"%s\"",
			  img->embedx, img->embedy, emx, emy, w, h, imf->name);
	    img->embedx = emx;  img->embedy = emy;
	} else if (match_keyword(typ, K_NOTES)) {
	    img->notes = cadr(head);
	    /* (should complain about non-nil cddr?) */
	} else {
	    run_warning("unknown image property in \"%s\"", imf->name);
	}
	if (imtype == K_OTHER_)
	  continue;
	datalist = cdr(head);
	/* Interpret random image subproperties. */
	while (consp(car(datalist))) {
	    prop = car(datalist);
	    proptype = car(prop);
	    if (match_keyword(proptype, K_ACTUAL)) {
		img->actualw = c_number(cadr(prop));
		img->actualh = c_number(caddr(prop));
	    } else if (match_keyword(proptype, K_PIXEL_SIZE)) {
		img->pixelsize = c_number(cadr(prop));
	    } else if (match_keyword(proptype, K_ROW_BYTES)) {
		img->rowbytes = c_number(cadr(prop));
	    } else if (match_keyword(proptype, K_PALETTE)) {
		if (img->palette != lispnil && !equal(cdr(prop), img->palette))
		  run_warning("Changing palette in %dx%d image of \"%s\"",
			      w, h, imf->name);
		img->palette = cdr(prop);
	    } else {
		run_warning("unknown image subproperty in \"%s\"", imf->name);
	    }
	    datalist = cdr(datalist);
	}
	switch (imtype) {
	  case K_MONO_:
	    if (img->monodata != lispnil && !equal(datalist, img->monodata))
	      run_warning("Changing mono data in %dx%d image of \"%s\"",
			  w, h, imf->name);
	    img->monodata = datalist;
	    break;
	  case K_COLR_:
	    if (img->colrdata != lispnil && !equal(datalist, img->colrdata))
	      run_warning("Changing color data in %dx%d image of \"%s\"",
			  w, h, imf->name);
	    img->colrdata = datalist;
	    break;
	  case K_MASK_:
	    if (img->maskdata != lispnil && !equal(datalist, img->maskdata))
	      run_warning("Changing mask data in %dx%d image of \"%s\"",
			  w, h, imf->name);
	    img->maskdata = datalist;
	    break;
	  default:
	    break;
	}
    }
    /* Kind of a hack. */
    img->minw = img->w / 4;  img->minh = img->h / 4;
    img->maxw = img->w * 4;  img->maxh = img->h * 4;
}

void
interp_bytes(datalist, numbytes, destaddr, jump)
Obj *datalist;
int numbytes, jump;
char *destaddr;
{
    int i, j = 0;
    char *data = NULL;

    for (i = 0; i < numbytes; ++i) {
	if (data == NULL || data[j] == '\0') {
	    if (!stringp(car(datalist)))
	      return; /* (should warn somehow?) */
	    data = c_string(car(datalist));
	    j = 0;
	    datalist = cdr(datalist);
	}
	/* Just skip over slashes, which are for readability only. */
	if (data[j] == '/')
	  ++j;
	destaddr[i] = hextoi(data[j]) * 16 + hextoi(data[j+1]);
	if (jump == 1 || (jump > 0 && i % jump == 0)) {
	    i += jump;
	    /* Be neat, put a zero in the location we're jumping over. */
	    /* (doesn't work for jump > 1, but that never happens anymore?) */
	    destaddr[i] = 0;
	}
	j += 2;
    }
}

ImagePalette *
interp_palette(form)
Obj *form;
{
    Obj *elts;
    ImagePalette *imp;

    if (stringp(cadr(form))) {
	imp = get_imp(c_string(cadr(form)));
	elts = cddr(form);
	if (consp(car(elts))
	    && symbolp(car(car(elts)))) {
	    if (match_keyword(car(car(elts)), K_NOTES)) {
		imp->notes = cadr(car(elts));
	    } else {
	    }
	    elts = cdr(elts);
	}
	return imp;
    }
    return NULL;
}

ImagePalette *
new_image_palette(name)
char *name;
{
    ImagePalette *imp;

    imp = (ImagePalette *) xmalloc(sizeof(ImagePalette));
    imp->name = name;
    imp->notes = lispnil;
    return imp;
}

char *
canonical_palette_name(str)
char *str;
{
    return str;
}

/* Given a name, find or create an image palette with that name. */

ImagePalette *
get_imp(name)
char *name;
{
    ImagePalette *imp = NULL;

    if (name == NULL)
      return NULL;
    if (palettes == NULL)
      palettes =
	(ImagePalette **) xmalloc(MAXIMAGEPALETTES * sizeof(ImagePalette *));
    if ((imp = find_imp(name)) == NULL) {
	if (numpalettes >= MAXIMAGEPALETTES)
	  return NULL;
	imp = new_image_palette(canonical_palette_name(name));
	if (imp != NULL) {
	    palettes[numpalettes++] = imp;
	}
    }
    return imp;
}

/* Find the image Palette of the given name, if it exists. */

ImagePalette *
find_imp(name)
char *name;
{
    int i;

    for (i = 0; i < numpalettes; ++i) {
	if (strcmp(name, palettes[i]->name) == 0)
	  return palettes[i];
    }
    return NULL;
}

ImageColor *
interp_color(form)
Obj *form;
{
    Obj *elts;
    ImageColor *imc;

    if (stringp(cadr(form))) {
	imc = get_imc(c_string(cadr(form)));
	elts = cddr(form);
	if (consp(car(elts))
	    && symbolp(car(car(elts)))) {
	    if (match_keyword(car(car(elts)), K_NOTES)) {
		imc->notes = cadr(car(elts));
	    } else {
	    }
	    elts = cdr(elts);
	}
	imc->r = c_number(car(elts));
	imc->g = c_number(car(cdr(elts)));
	imc->b = c_number(car(cddr(elts)));
	return imc;
    }
    return NULL;
}

ImageColor *
new_image_color(name)
char *name;
{
    ImageColor *imc;

    imc = (ImageColor *) xmalloc(sizeof(ImageColor));
    imc->name = name;
    imc->notes = lispnil;
    return imc;
}

char *
canonical_color_name(str)
char *str;
{
    return str;
}

/* Given a name, find or create an image color with that name. */

ImageColor *
get_imc(name)
char *name;
{
    ImageColor *imc = NULL;

    if (name == NULL)
      return NULL;
    if (colors == NULL)
      colors =
	(ImageColor **) xmalloc(MAXIMAGECOLORS * sizeof(ImageColor *));
    if ((imc = find_imc(name)) == NULL) {
	if (numcolors >= MAXIMAGECOLORS)
	  return NULL;
	imc = new_image_color(canonical_color_name(name));
	if (imc != NULL) {
	    imc->r = imc->g = imc->b = 0;
	    colors[numcolors++] = imc;
	}
    }
    return imc;
}

/* Find the image color of the given name, if it exists. */

ImageColor *
find_imc(name)
char *name;
{
    int i;

    for (i = 0; i < numcolors; ++i) {
	if (colornamecmp(name, colors[i]->name) == 0)
	  return colors[i];
    }
    return NULL;
}

/* X-style color names have several variants, but we only want one of them,
   so this matches the variants with each other. */

static int
colornamecmp(str1, str2)
char *str1, *str2;
{
    while (*str1 != '\0' && *str2 != '\0') {
	if (*str1 == *str2) {
	    ++str1;  ++str2;
	} else if (isalpha(*str1) && isalpha(*str2)
		   && tolower(*str1) == tolower(*str2)) {
	    ++str1;  ++str2;
	} else if (*str1 == 'a' && *str2 == 'e'
		   && *(str1+1) == 'y' && *(str2+1) == 'y') {
	    ++str1;  ++str2;
	} else if (*str1 == 'e' && *str2 == 'a'
		   && *(str1+1) == 'y' && *(str2+1) == 'y') {
	    ++str1;  ++str2;
#if 0
	} else if (*str1 == ' ') {
	    ++str1;
	} else if (*str2 == ' ') {
	    ++str2;
#endif
	} else {
	    return *str1 - *str2;
	}
    }
    if (*str1 == '\0') {
	if (*str2 == '\0') {
	    return 0;
	} else {
	    return 1;
	}
    } else {
	if (*str2 == '\0') {
	    return -1;
	} else {
	    /* can never happen, but humor the compiler */
	    return 0;
	}
    }
}


/* Try to find the best of multiple images for the given bounding box. */
/* Don't return anything that won't fit in min space. */

Image *
best_image(imf, w, h)
ImageFamily *imf;
int w, h;
{
    Image *img, *best = NULL, *fallback = NULL;

    if (imf == NULL)
      return NULL;
    for (img = imf->images; img != NULL; img = img->next) {
	/* Exact matches need no further searching. */
	/* The istile test is a hack for the general problem here, which
	   is that a high-quality color image might look better, even after
	   scaling, than a b/w image that is exactly the right size. */
	if (w == img->w && h == img->h && !img->istile) {
	    return img;
	} else if (between(img->minw, w, img->maxw)
		   && between(img->minh, h, img->maxh)) {
	    /* Image is plausible - go for the largest one that will fit. */
	    if (!best || (img->w > best->w && img->h > best->h))
	      best = img;
	} else if (w >= img->minw && h >= img->minh) {
	    /* Image is not really big enough, but keep as a fallback. */
	    /* Prefer the largest fallback possible. */
	    if (!fallback || (img->w > fallback->w && img->h > fallback->h))
	      fallback = img;
	} else if (img->istile) {
	    /* Image is not really appropriate, but keep as a fallback. */
	    /* Prefer the largest fallback possible. */
	    if (!fallback || (img->w > fallback->w && img->h > fallback->h))
	      fallback = img;
	}
    }
    return (best ? best : fallback);
}

/* The comparison function for the image list just does "strcmp" order
   and *requires* that all image families be named and named uniquely. */

static int
image_name_compare(imf1, imf2)
#ifdef THINK_C
const
#endif
void *imf1, *imf2;
{
    return strcmp((*((ImageFamily **) imf1))->name,
		  (*((ImageFamily **) imf2))->name);
}

void
sort_all_images()
{
    qsort(&(images[0]), numimages, sizeof(ImageFamily *), image_name_compare);
}

/* The comparison function for the palette list just does "strcmp" order
   and *requires* that all image palettes be named and named uniquely. */

static int
palette_name_compare(imp1, imp2)
#ifdef THINK_C
const
#endif
void *imp1, *imp2;
{
    return strcmp((*((ImagePalette **) imp1))->name,
		  (*((ImagePalette **) imp2))->name);
}

void
sort_all_palettes()
{
    qsort(&(palettes[0]), numpalettes, sizeof(ImagePalette *), palette_name_compare);
}

/* The comparison function for the color list just does "strcmp" order
   and *requires* that all image palettes be named and named uniquely. */

static int
color_name_compare(imc1, imc2)
#ifdef THINK_C
const
#endif
void *imc1, *imc2;
{
    return strcmp((*((ImageColor **) imc1))->name,
		  (*((ImageColor **) imc2))->name);
}

void
sort_all_colors()
{
    qsort(&(colors[0]), numcolors, sizeof(ImageColor *),  color_name_compare);
}

/* Check Lisp-format and binary-format data for consistency. */

void
check_imf(imf)
ImageFamily *imf;
{
    Image *img;
    
    if (imf == NULL)
      return;
    if (imf->name == NULL) {
	return;
    }
    for (img = imf->images; img != NULL; img = img->next) {
	/* Check consistency of Lisp and binary data. */
	if (img->colrdata != lispnil && img->rawcolrdata) {
	    /* (should add color image comparison) */
	}
	if (img->monodata != lispnil && img->rawmonodata) {
	    if (!bitmaps_match(img->w, img->h, img->monodata, img->rawmonodata))
	      run_warning("mono bitmap data not consistent in  %dx%d image of \"%s\"",
			  img->w, img->h, imf->name);
	}
	if (img->maskdata != lispnil && img->rawmaskdata) {
	    if (!bitmaps_match(img->w, img->h, img->maskdata, img->rawmaskdata))
	      run_warning("mask bitmap data not consistent in  %dx%d image of \"%s\"",
			  img->w, img->h, imf->name);
	}
    }
}

static int
bitmaps_match(w, h, lispdata, rawdata)
int w, h;
Obj *lispdata;
char *rawdata;
{
    int i, j = 0, rowbytes, numbytes, byte, jump = 0;
    char *datastr = NULL;

    rowbytes = (w + 7) / 8;
    numbytes =  h * rowbytes;
    for (i = 0; i < numbytes; ++i) {
	if (datastr == NULL || datastr[j] == '\0') {
		if (!stringp(car(lispdata)))
		  break;
		datastr = c_string(car(lispdata));
		j = 0;
		lispdata = cdr(lispdata);
	}
	if (datastr[j] == '/')
	  ++j;
	byte = hextoi(datastr[j]) * 16 + hextoi(datastr[j+1]);
	j += 2;
	if (byte != rawdata[i])
	  return FALSE;
    }
    return TRUE;
}

/* Write out the entire image family. */

void
write_imf(fp, imf)
FILE *fp;
ImageFamily *imf;
{
    Image *img;
    
    if (imf == NULL)
      return;
    if (imf->name == NULL) {
	fprintf(fp, "; garbage image family?\n");
	return;
    }
    if (imf->notes != lispnil) {
	fprintf(fp, "(%s \"%s\"", keyword_name(K_IMF), imf->name);
	fprintf(fp, "\n  (%s ", keyword_name(K_NOTES));
	fprintlisp(fp, imf->notes);
	fprintf(fp, "))\n");
    }
    for (img = imf->images; img != NULL; img = img->next) {
	if (img->monodata != lispnil
	    || img->maskdata != lispnil
	    || img->colrdata != lispnil
	    || img->rawmonodata != NULL
	    || img->rawmaskdata != NULL
	    || img->rawcolrdata != NULL) {
	    fprintf(fp, "(%s \"%s\"", keyword_name(K_IMF), imf->name);
	    fprintf(fp, " (");
	    fprintf(fp, "(%d %d", img->w, img->h);
	    if (img->istile)
	      fprintf(fp, " %s", keyword_name(K_TILE));
	    fprintf(fp, ")");
	    if (img->embedname) {
		fprintf(fp, " (%s \"%s\")",
			keyword_name(K_EMBED), img->embedname);
	    }
	    if (img->embedx >= 0 && img->embedy >= 0) {
		fprintf(fp, " (%s %d %d)",
			keyword_name(K_EMBED_AT), img->embedx, img->embedy);
	    }
	    if (img->embedw >= 0 && img->embedh >= 0) {
		fprintf(fp, " (%s %d %d)",
			keyword_name(K_EMBED_SIZE), img->embedw, img->embedh);
	    }
	    if (img->notes != lispnil) {
		fprintf(fp, "\n  (%s ", keyword_name(K_NOTES));
		fprintlisp(fp, img->notes);
		fprintf(fp, ")\n ");
	    }
	    if ((img->colrdata != lispnil || img->rawcolrdata)
		&& !color_matches_mono(img)) {
		fprintf(fp, "\n  ");
		write_pixmap(fp, img->w, img->h, img->actualw, img->actualh,
			     img->pixelsize, img->rowbytes,
			     img->palette, img->rawpalette, img->numcolors,
			     img->colrdata, img->rawcolrdata);
	    }
	    if (img->monodata != lispnil || img->rawmonodata) {
		fprintf(fp, "\n  ");
		write_bitmap(fp, keyword_name(K_MONO), img->w, img->h,
			     img->monodata, img->rawmonodata);
	    }
	    if (img->maskdata != lispnil || img->rawmaskdata) {
		fprintf(fp, "\n  ");
		write_bitmap(fp, keyword_name(K_MASK), img->w, img->h,
			     img->maskdata, img->rawmaskdata);
	    }
	    fprintf(fp, "))\n");
	}
    }
}

static int
color_matches_mono(img)
Image *img;
{
    int i, cj, mj, rowbytes, numbytes, cbyte, mbyte, jump = 0;
    char *cdatastr = NULL, *mdatastr = NULL;
    Obj *clispdata = img->colrdata, *mlispdata = img->monodata;

    if (img->pixelsize != 1)
      return FALSE;
    /* No match possible if not a black-white-only palette. */
    if (!((img->numcolors == 2
	   && img->rawpalette != NULL
	   && img->rawpalette[0+0] == 0
	   && img->rawpalette[0+1] > WHITE_THRESHOLD
	   && img->rawpalette[0+2] > WHITE_THRESHOLD
	   && img->rawpalette[0+3] > WHITE_THRESHOLD
	   && img->rawpalette[4+0] == 1
	   && img->rawpalette[4+1] < BLACK_THRESHOLD
	   && img->rawpalette[4+2] < BLACK_THRESHOLD
	   && img->rawpalette[4+3] < BLACK_THRESHOLD)
	  || (img->palette != lispnil
	      && c_number(car(car(img->palette))) == 0
	      && c_number(cadr(car(img->palette))) > WHITE_THRESHOLD
	      && c_number(caddr(car(img->palette))) > WHITE_THRESHOLD
	      && c_number(caddr(cdr(car(img->palette)))) > WHITE_THRESHOLD
	      && c_number(car(cadr(img->palette))) == 1
	      && c_number(cadr(cadr(img->palette))) < BLACK_THRESHOLD
	      && c_number(caddr(cadr(img->palette))) < BLACK_THRESHOLD
	      && c_number(caddr(cdr(cadr(img->palette)))) < BLACK_THRESHOLD)))
      return FALSE;
    /* Now compare the contents. */
    rowbytes = (img->w + 7) / 8;
    numbytes =  img->h * rowbytes;
    cj = mj = 0;
    for (i = 0; i < numbytes; ++i) {
      if (clispdata != lispnil) {
	if (cdatastr == NULL || cdatastr[cj] == '\0') {
	    if (!stringp(car(clispdata)))
	      break;
	    cdatastr = c_string(car(clispdata));
	    cj = 0;
	    clispdata = cdr(clispdata);
	}
	if (cdatastr[cj] == '/')
	  ++cj;
	cbyte = hextoi(cdatastr[cj]) * 16 + hextoi(cdatastr[cj+1]);
	cj += 2;
      } else if (img->rawcolrdata != NULL) {
	cbyte = (img->rawcolrdata)[i];
      } else {
	return FALSE;
      }
      if (mlispdata != lispnil) {
	if (mdatastr == NULL || mdatastr[mj] == '\0') {
	    if (!stringp(car(mlispdata)))
	      break;
	    mdatastr = c_string(car(mlispdata));
	    mj = 0;
	    mlispdata = cdr(mlispdata);
	}
	if (mdatastr[mj] == '/')
	  ++mj;
	mbyte = hextoi(mdatastr[mj]) * 16 + hextoi(mdatastr[mj+1]);
	mj += 2;
      } else if (img->rawmonodata != NULL) {
	mbyte = (img->rawmonodata)[i];
      } else {
	return FALSE;
      }
      if (cbyte != mbyte)
	return FALSE;
    }
    return TRUE;
}

static void
write_pixmap(fp, w, h, actualw, actualh, pixelsize, rowbytes,
	     palette, rawpalette, numcolors, lispdata, rawdata)
FILE *fp;
int w, h, actualw, actualh, pixelsize, rowbytes, *rawpalette, numcolors;
Obj *palette, *lispdata;
char *rawdata;
{
    int dolisp, i, j = 0, numbytes, byte, jump = 0;
    char *datastr = NULL;

    actualw = (actualw != 0 ? actualw : w);
    actualh = (actualh != 0 ? actualh : h);
    dolisp = (lispdata != lispnil);	
    numbytes = actualh * rowbytes;
    fprintf(fp, "(%s", keyword_name(K_COLOR));
    if (actualw != w || actualh != h)
      fprintf(fp, " (%s %d %d)", keyword_name(K_ACTUAL), actualw, actualh);
    fprintf(fp, " (%s %d)", keyword_name(K_PIXEL_SIZE), pixelsize);
    fprintf(fp, " (%s %d)", keyword_name(K_ROW_BYTES), rowbytes);
    if (palette != lispnil || (rawpalette && numcolors))
      write_palette_contents(fp, palette, rawpalette, numcolors);
    fprintf(fp, "\n   \"");
    for (i = 0; i < numbytes; ++i) {
	if (i > 0 && i % 32 == 0)
	  fprintf(fp, "\"\n   \"");
	if (i > 0 && i % 32 != 0 && i % rowbytes == 0)
	  fprintf(fp, "/");
	if (dolisp) {
	    if (datastr == NULL || datastr[j] == '\0') {
		if (!stringp(car(lispdata)))
		  break;
		datastr = c_string(car(lispdata));
		j = 0;
		lispdata = cdr(lispdata);
	    }
	    if (datastr[j] == '/')
	      ++j;
	    byte = hextoi(datastr[j]) * 16 + hextoi(datastr[j+1]);
	    if (jump == 1 || (jump > 0 && i % jump == 0))
	      i += jump;
	    j += 2;
	} else {
	    byte = rawdata[i];
	}
	fprintf(fp, "%02x", (unsigned char) byte);
    }
    fprintf(fp, "\")");
}

static void
write_bitmap(fp, subtyp, w, h, lispdata, rawdata)
FILE *fp;
char *subtyp;
int w, h;
Obj *lispdata;
char *rawdata;
{
    int dolisp, i, j = 0, rowbytes, numbytes, byte, jump = 0;
    char *datastr = NULL;

    /* Lisp data overrides byte data. */	
    dolisp = (lispdata != lispnil);	
    rowbytes = (w + 7) / 8;
    numbytes =  h * rowbytes;
    fprintf(fp, "(%s", subtyp);
    if (w > 16 || h > 16)
      fprintf(fp, "\n  ");
    fprintf(fp, " \"");
    for (i = 0; i < numbytes; ++i) {
	if (i > 0 && i % 32 == 0)
	  fprintf(fp, "\"\n   \"");
	if (i > 0 && i % 32 != 0 && i % rowbytes == 0)
	  fprintf(fp, "/");
	if (dolisp) {
	    if (datastr == NULL || datastr[j] == '\0') {
		if (!stringp(car(lispdata)))
		  break;
		datastr = c_string(car(lispdata));
		j = 0;
		lispdata = cdr(lispdata);
	    }
	    if (datastr[j] == '/')
	      ++j;
	    byte = hextoi(datastr[j]) * 16 + hextoi(datastr[j+1]);
	    if (jump == 1 || (jump > 0 && i % jump == 0))
	      i += jump;
	    j += 2;
	} else {
	    byte = rawdata[i];
	}
	fprintf(fp, "%02x", (unsigned char) byte);
    }
    fprintf(fp, "\")");
}

static void
write_palette_contents(fp, palette, rawpalette, numcolors)
FILE *fp;
Obj *palette;
int *rawpalette, numcolors;
{
    int len, i;
    Obj *color;

    len = (palette != lispnil ? length(palette) : numcolors);
    if (len > 2)
      fprintf(fp, "\n  ");
    fprintf(fp, " (%s", keyword_name(K_PALETTE));
    if (palette != lispnil) {
	for (; palette != lispnil; palette = cdr(palette)) {
	    color = car(palette);
	    if (len > 2)
	      fprintf(fp, "\n   ");
	    fprintf(fp, " (%d %d %d %d)",
		    c_number(car(color)),
		    c_number(car(cdr(color))),
		    c_number(car(cdr(cdr(color)))),
		    c_number(car(cdr(cdr(cdr(color))))));
	}
    } else {
 	for (i = 0; i < numcolors; i++) {
	    if (len > 2)
	      fprintf(fp, "\n   ");
 	    fprintf(fp, " (%d %d %d %d)",
 		    rawpalette[4*i],
 		    rawpalette[4*i+1],
 		    rawpalette[4*i+2],
 		    rawpalette[4*i+3]);
 	}
    }
    fprintf(fp, ")");
}

void
write_imp(fp, imp)
FILE *fp;
ImagePalette *imp;
{
    if (imp == NULL)
      return;
    if (imp->name == NULL) {
	fprintf(fp, "; garbage palette?\n");
	return;
    }
    fprintf(fp, "(%s \"%s\"",
	    keyword_name(K_PALETTE), imp->name);
    if (imp->notes != lispnil) {
	fprintf(fp, "\n  (%s ", keyword_name(K_NOTES));
	fprintlisp(fp, imp->notes);
	fprintf(fp, ")\n ");
    }
    /* (should dump out palette entries here) */
    fprintf(fp, ")\n");
}

void
write_imc(fp, imc)
FILE *fp;
ImageColor *imc;
{
    if (imc == NULL)
      return;
    if (imc->name == NULL) {
	fprintf(fp, "; garbage color?\n");
	return;
    }
    fprintf(fp, "(%s \"%s\"",
	    keyword_name(K_COLOR), imc->name);
    if (imc->notes != lispnil) {
	fprintf(fp, "\n  (%s ", keyword_name(K_NOTES));
	fprintlisp(fp, imc->notes);
	fprintf(fp, ")\n ");
    }
    fprintf(fp, " %d %d %d)\n", imc->r, imc->g, imc->b);
}
