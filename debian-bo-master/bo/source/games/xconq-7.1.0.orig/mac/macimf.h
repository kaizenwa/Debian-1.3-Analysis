/* Definitions of Mac-specific data about image families in Xconq.
   Copyright (C) 1992, 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* Structure containing the Mac-specific parts of an image in an image family. */

typedef struct a_mac_image {
	Image *generic;					/* pointer back to generic image */
	int patdefined;					/* true if mono pattern data is defined */
	Pattern monopat;				/* mono pattern data */
	Handle monosicn;				/* 16x16 mono icon */
	Handle masksicn;				/* 16x16 mask icon */
	Handle monoicon;				/* 32x32 mono icon */
	Handle maskicon;				/* 32x32 mask icon */
	Handle colricon;				/* color icon (can be any size) */
	PixPatHandle colrpat;			/* color pattern (can be 8x8 up to 64x64) */
	/* (should implement all of the following) */
	PicHandle monopict;				/* mono picture */
	PicHandle colrpict;				/* color picture */
	PicHandle maskpict;				/* mask picture */
	PicHandle groupmonopict;		/* mono group picture */
	PicHandle groupcolrpict;		/* color group picture */
	PicHandle groupmaskpict;		/* mask group picture */
	int groupmonox, groupmonoy;		/* Position of mono image in a group picture */
	int groupcolrx, groupcolry;		/* Position of mono image in a group picture */
	int groupmaskx, groupmasky;		/* Position of mono image in a group picture */
} MacImage;

#ifdef MPW_C
/* dangerous way */
#define SET_IMG_PAT(mimg,i,val) ((mimg)->monopat[(i)] = (val))
#define IMG_PAT(mimg) ((unsigned char *) &((mimg)->monopat))
#define QD_PAT_ADDR(p) ((unsigned char *) p)
#endif

#ifdef THINK_C
/* dangerous way */
#define SET_IMG_PAT(mimg,i,val) ((mimg)->monopat[(i)] = (val))
#define IMG_PAT(mimg) ((unsigned char *) &((mimg)->monopat))
#define QD_PAT_ADDR(p) ((unsigned char *) p)
#endif

#ifdef __MWERKS__
#define SET_IMG_PAT(mimg,i,val) (((mimg)->monopat.pat)[(i)] = (val))
#define IMG_PAT(mimg) (&((mimg)->monopat))
#define QD_PAT_ADDR(p) ((unsigned char *) (p.pat))
#endif

#define c2p(STR,PBUF) \
  strcpy(((char *) PBUF) + 1, STR);  \
  PBUF[0] = strlen(STR);

#define p2c(PSTR,BUF)  \
  strncpy(BUF, ((char *) (PSTR) + 1), PSTR[0]);  \
  BUF[PSTR[0]] = '\0';

extern MacImage *init_mac_image(Image *img);
extern MacImage *get_mac_image(Image *img);
extern void mac_load_imf(ImageFamily *imf);
extern void mac_interp_imf(ImageFamily *imf);
extern void mac_interp_bytes(Obj *datalist, int numbytes, Handle desthandle, int jump);
extern void make_generic_image_data(ImageFamily *imf);
extern void mac_load_image_color(ImageColor *imc);
