/* Definitions for Xconq images.
   Copyright (C) 1992, 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* An image family is like a finder icon family, but allows multiple kinds
   of images of an arbitrary set of sizes. */

typedef struct a_image {
    int w, h;			/* Nominal size of the image */
    int minw, minh;		/* Minimum size to scale to */
    int maxw, maxh;		/* Maximum size to scale to */
    int istile;			/* True if image may be used as tile */
    char *embedname;		/* Name of an embedded subimage (imf name) */
    int embedx, embedy;		/* Position to draw an embedded subimage */
    int embedw, embedh;		/* Size of space for embedded subimage */
    Obj *monodata;		/* Monochrome data, in GDL form */
    Obj *colrdata;		/* Color data, in GDL form */
    Obj *maskdata;		/* Mask data, in GDL form */
    int actualw, actualh;
    int pixelsize;
    int rowbytes;
    Obj *palette;
    Obj *notes;			/* designer notes about the image */
    char *rawmonodata;		/* Monochrome data, as array of bytes */
    char *rawcolrdata;		/* Color data, as array of bytes */
    char *rawmaskdata;		/* Mask data, as array of bytes */
    int *rawpalette;
    int numcolors;		/* Number of colors in raw palette */
    char *hook;			/* Pointer to interface-specific data */
    struct a_image *next;	/* Pointer to next image in family */
} Image;

typedef struct a_image_family {
    char *name;			/* Name of the family */
    int ersatz;			/* True if this image is a substitute */
    struct a_image_file *location;  /* File or whatever to look for data */
    Obj *notes;			/* designer notes about the image family */
    int numsizes;		/* Number of images in the list */
    Image *images;
} ImageFamily;

typedef struct a_image_palette {
    char *name;			/* Name of the palette */
    struct a_image_file *location;  /* File or whatever to look for data */
    Obj *notes;			/* designer notes about the palette */
    int numcolors;		/* Number of colors in the palette */
} ImagePalette;

typedef struct a_image_color {
    char *name;			/* Name of the color */
    struct a_image_file *location;  /* File or whatever to look for data */
    Obj *notes;			/* designer notes about the color */
    short defined;
    short r, g, b;
} ImageColor;

typedef struct a_image_file {
    char *name;
    int loaded;
    struct a_image_file *next;
} ImageFile;

extern ImageFamily **images;

extern int numimages;

extern ImagePalette **palettes;

extern int numpalettes;

extern ImageColor **colors;

extern int numcolors;

extern ImageFile *image_files;

#define hextoi(c) (((c) >= '0' && (c) <= '9') ? ((c) - '0') : ((c) - 'a' + 10))

typedef void (*readimf_hook) PARAMS ((ImageFamily *, int));

extern ImageFamily *clone_imf PARAMS ((ImageFamily *imf));
extern ImageFamily *get_imf PARAMS ((char *name));
extern ImageFamily *find_imf PARAMS ((char *name));
extern Image *find_img PARAMS ((ImageFamily *imf, int w, int h));
extern Image *get_img PARAMS ((ImageFamily *imf, int w, int h));
extern int valid_imf_name PARAMS ((char *name));

extern char *canonical_palette_name PARAMS ((char *str));
extern ImagePalette *new_image_palette PARAMS ((char *name));
extern ImagePalette *get_imp PARAMS ((char *name));
extern ImagePalette *find_imp PARAMS ((char *name));

extern char *canonical_color_name PARAMS ((char *str));
extern ImageColor *new_image_color PARAMS ((char *name));
extern ImageColor *get_imc PARAMS ((char *name));
extern ImageColor *find_imc PARAMS ((char *name));

extern ImageFile *get_image_file PARAMS ((char *name));
extern void load_image_families PARAMS ((FILE *fp, int loadnow,
					 readimf_hook callback));
extern int load_imf_file PARAMS ((char *filename,
				  readimf_hook callback));
extern void interp_imf_form PARAMS ((Obj *form,
				     readimf_hook callback));

extern ImageFamily *interp_imf PARAMS ((Obj *form));
extern void interp_imf_contents PARAMS ((ImageFamily *imf, Obj *form));
extern void interp_image PARAMS ((ImageFamily *imf, Obj *size, Obj *parts));
extern void interp_bytes PARAMS ((Obj *datalist, int numbytes, char *destaddr,
			  int jump));
extern ImagePalette *interp_palette PARAMS ((Obj *form));
extern ImageColor *interp_color PARAMS ((Obj *form));

extern Image *best_image PARAMS ((ImageFamily *imf, int w, int h));

extern void sort_all_images PARAMS ((void));
extern void sort_all_palettes PARAMS ((void));
extern void sort_all_colors PARAMS ((void));

extern void check_imf PARAMS ((ImageFamily *imf));

extern void write_imf PARAMS ((FILE *fp, ImageFamily *imf));
extern void write_imp PARAMS ((FILE *fp, ImagePalette *imp));
extern void write_imc PARAMS ((FILE *fp, ImageColor *imc));
