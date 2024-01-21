/* Image families for the Mac interface to Xconq.
   Copyright (C) 1992, 1993, 1994, 1995 Stanley T. Shebs.

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

#ifdef THINK_C
#include <MacHeaders>
#else /* assume MPW */
#include <Types.h>
#include <Memory.h>
#include <Resources.h>
#include <Quickdraw.h>
#endif /* THINK_C */

#include "macimf.h"

extern int hasColorQD;

CTabHandle interp_ctab(Obj *palette);
CTabHandle synth_ctab(void);
void convert_ppat(Image *img, PixPatHandle colrpat);
void convert_cicn(Image *img, CIconHandle colricon, int *hasmono, int *hasmask);
Obj *convert_ctab(CTabHandle ctabhandle);

MacImage *
init_mac_image(Image *img)
{
	int j;
	MacImage *macimg;

	macimg = (MacImage *) xmalloc(sizeof(MacImage));
	for (j = 0; j < 8; ++j)
	  SET_IMG_PAT (macimg, j, '\0');
	macimg->generic = img;
	return macimg;
}

MacImage *
get_mac_image(Image *img)
{
	MacImage *macimg;

	if (img->hook)
	  return (MacImage *) img->hook;
	macimg = init_mac_image(img);
	img->hook = (char *) macimg;
	return macimg;
}

/* This tries to fill in the given image family from various resources.  The order
   should be "best first", so as to improve search time. */

void
mac_load_imf(imf)
ImageFamily *imf;
{
    int w, h, i, rsize, startlineno = 0, endlineno = 0;
    char tmpstrbuf[100];
    Obj *imfspec;
    Image *img;
    MacImage *macimg;
    Handle imfhandle, pathandle, sicnhandle, iconhandle, maskhandle;
    PicHandle pichandle;
    PixPatHandle ppathandle;
	PixMapHandle pmhandle;
    CIconHandle cicnhandle;
    PixMap pmap;
    Rect bounds;
    Str255 namestr, maskstr, im16x16str, resname;
	short resid;
	ResType restype;

	/* Can't do anything without a name for the image family. */
	if (imf == NULL || imf->name == NULL)
	  return;
	c2p(imf->name, namestr);
	/* The name of the mask is always formed by appending " mask". */
	sprintf(tmpstrbuf, "%s mask", imf->name);
	c2p(tmpstrbuf, maskstr);
	/* The name of the 16x16 cicn is always formed by appending " 16x16". */
	sprintf(tmpstrbuf, "%s 16x16", imf->name);
	c2p(tmpstrbuf, im16x16str);
	/* Look for and load the image family specification resource first. */
	imfhandle = (Handle) GetNamedResource('XCif', namestr);
	if (imfhandle != nil) {
		imfspec = read_form_from_string(copy_string(*imfhandle), &startlineno, &endlineno);
		interp_imf_contents(imf, imfspec);
	}
	pichandle = (PicHandle) GetNamedResource('PICT', namestr);
	if (pichandle != nil) {
		img = get_img(imf, 16, 16); /* should get real bounds */
		if (img != NULL) {
			img->minw = img->minh = 8;
			img->maxw = img->maxh = 9999;
			macimg = get_mac_image(img);
			/* (should distinguish mono and color picts somehow) */
			macimg->monopict = pichandle;
			/* Look for a mask too. */
			pichandle = (PicHandle) GetNamedResource('PICT', maskstr);
			if (pichandle != nil) {
				macimg->maskpict = pichandle;
			}
			img->istile = 0;
		}
	}
	/* (should also be able to pick up picts with rows and columns of images, but
	    would need separate resource to identify which is which) */
	/* Pick up cicns, if we can do color. */
	if (hasColorQD) {
		cicnhandle = (CIconHandle) GetNamedResource('cicn', namestr);
		if (cicnhandle != nil) {
			HLock((Handle) cicnhandle);
			/* Need to get id so we can use special trap. */
			GetResInfo((Handle) cicnhandle, &resid, &restype, resname);
			cicnhandle = GetCIcon(resid);
			pmap = (*cicnhandle)->iconPMap;
			bounds = pmap.bounds;
			w = bounds.right - bounds.left;  h = bounds.bottom - bounds.top;
			img = get_img(imf, w, h);
			if (img != NULL) {
				img->minw = img->minh = 8;
				img->maxw = img->maxh = 128;
				macimg = get_mac_image(img);
				macimg->colricon = (Handle) cicnhandle;
				/* Mask is built in, don't need to load separately. */
			}
			HUnlock((Handle) cicnhandle);
		}
		cicnhandle = (CIconHandle) GetNamedResource('cicn', im16x16str);
		if (cicnhandle != nil) {
			HLock((Handle) cicnhandle);
			/* Need to get id so we can use special trap. */
			GetResInfo((Handle) cicnhandle, &resid, &restype, resname);
			cicnhandle = GetCIcon(resid);
			pmap = (*cicnhandle)->iconPMap;
			bounds = pmap.bounds;
			w = bounds.right - bounds.left;  h = bounds.bottom - bounds.top;
			img = get_img(imf, w, h);
			if (img != NULL) {
				img->minw = img->minh = 8;
				img->maxw = img->maxh = 128;
				macimg = get_mac_image(img);
				macimg->colricon = (Handle) cicnhandle;
				/* Mask is built in, don't need to load separately. */
			}
			HUnlock((Handle) cicnhandle);
		}
	} else {
		/* (should at least try to get the mono part, without using CQD traps) */
	}
	/* Pick up ICONs. */
	iconhandle = (Handle) GetNamedResource('ICON', namestr);
	if (iconhandle != nil) {
		img = get_img(imf, 32, 32);
		if (img != NULL) {
			img->minw = img->minh = 8;
			img->maxw = img->maxh = 128;
			macimg = get_mac_image(img);
			macimg->monoicon = iconhandle;
			/* Look for a mask too. */
			iconhandle = (Handle) GetNamedResource('ICON', maskstr);
			if (iconhandle != nil) {
				macimg->maskicon = iconhandle;
			}
		}
	}
	/* Pick up SICNs. */
	sicnhandle = (Handle) GetNamedResource('SICN', namestr);
	if (sicnhandle != nil) {
		img = get_img(imf, 16, 16);
		if (img != NULL) {
			img->minw = img->minh = 8;
			img->maxw = img->maxh = 64;
			macimg = get_mac_image(img);
			/* Image itself is just the first 32 bytes, mask is second 32 if present. */
			macimg->monosicn = sicnhandle;
			rsize = SizeResource(sicnhandle);
			if (rsize >= 64) {
				maskhandle = (Handle) NewHandle(32);
				for (i = 0; i < 32; ++i) {
					(*maskhandle)[i] = (*sicnhandle)[i+32];
				}
				macimg->masksicn = maskhandle;
			} else {
				/* Mask could be separate resource, so look for it. */
				iconhandle = GetNamedResource('SICN', maskstr);
				if (iconhandle != nil) {
					macimg->masksicn = sicnhandle;
				} else {
					/* no mask to be found */
				}
			}
			img->istile = 0;
		}
	}
	/* Pick up color patterns, if we're capable of doing color. */
	if (hasColorQD) {
		ppathandle = (PixPatHandle) GetNamedResource('ppat', namestr);
		if (ppathandle != nil) {
			/* Need to get the id of the ppat so we can use special trap. */
			GetResInfo((Handle) ppathandle, &resid, &restype, resname);
			ppathandle = GetPixPat(resid);
			pmhandle = (*ppathandle)->patMap;
			switch ((*ppathandle)->patType) {
				case 0:
					/* (should put something in monodata?) */
					w = h = 8;
					break;
				case 1:
					/* Compute the actual size of the pattern. */
					bounds = (*pmhandle)->bounds;
					w = bounds.right - bounds.left;  h = bounds.bottom - bounds.top;
			}
			img = get_img(imf, w, h);
			if (img != NULL) {
				/* Indicate that we can use this pattern for any size area. */
				img->minw = img->minh = 1;
				img->maxw = img->maxh = 9999;
				img->istile = TRUE;
				macimg = get_mac_image(img);
				macimg->colrpat = ppathandle;
				if (w == 8 && h == 8) {
					/* Set the monopat. */
					for (i = 0; i < 8; ++i)
					  SET_IMG_PAT(macimg, i, QD_PAT_ADDR((*ppathandle)->pat1Data)[i]);
					macimg->patdefined = 1;
				}
			}
			/* If the pattern is larger than 8x8, we need to make an 8x8 image for
			   the mono pattern. */
			if (!(w == 8 && h == 8)) {
				img = get_img(imf, 8, 8);
				if (img != NULL) {
					/* Indicate that we can use this pattern for any size area. */
					img->minw = img->minh = 1;
					img->maxw = img->maxh = 9999;
					img->istile = TRUE;
					macimg = get_mac_image(img);
					/* Set the monopat. */
					for (i = 0; i < 8; ++i)
					  SET_IMG_PAT(macimg, i, QD_PAT_ADDR((*ppathandle)->pat1Data)[i]);
					macimg->patdefined = 1;
				}
			}
		}
	}
	/* Load a pattern, which can be used for any size area, but whose "natural" size
	   is always 8x8. */
	pathandle = GetNamedResource('PAT ', namestr);
	if (pathandle != nil) {
		img = get_img(imf, 8, 8);
		if (img != NULL) {
			img->minw = img->minh = 1;
			img->maxw = img->maxh = 9999;
			img->istile = TRUE;
			macimg = get_mac_image(img);
			if (macimg->patdefined) {
				int allzeros = TRUE;

				for (i = 0; i < 8; ++i) {
					if (((char *) &(macimg->monopat))[i] != 0) {
						allzeros = FALSE;
						break;
					}
				}
				/* A mono pattern of all zeros is a default pat from a ppat; overwrite
				   it silently. */
				if (!allzeros) {
					for (i = 0; i < 8; ++i) {
						if (((char *) &(macimg->monopat))[i] != ((char *) *pathandle)[i]) {
							run_warning("ppat/PAT mismatch for \"%s\", overwriting ppat",
										imf->name);
							break;
						}
					}
				}
				for (i = 0; i < 8; ++i)
				  SET_IMG_PAT(macimg, i, ((char *) *pathandle)[i]);
			} else {
				/* Set the monopat. */
				for (i = 0; i < 8; ++i)
				  SET_IMG_PAT(macimg, i, ((char *) *pathandle)[i]);
				macimg->patdefined = 1;
			}
		}
	}
}

/* Given an image family that already has data for it, in Lisp form,
   make platform-specific images. */

void
mac_interp_imf(imf)
ImageFamily *imf;
{
	int w, h, i, numbytes, bitrowbytes, actualw, actualh, pixelsize, rowbytes;
	int monodone, colrdone, maskdone;
	Image *img;
	MacImage *macimg;
	Handle sicnhandle, iconhandle, datahandle;
	Handle monohandle = nil, maskhandle = nil;
    PixPatHandle ppathandle;
    CIconHandle cicnhandle;
    PixMapHandle pmhandle;

	for (img = imf->images; img != NULL; img = img->next) {
		w = img->w;  h = img->h;
		actualw = img->actualw;  actualh = img->actualh;
		pixelsize = img->pixelsize;
		rowbytes = img->rowbytes;
		macimg = get_mac_image(img);
		/* Mono icons and masks are very similar; digest both here. */
		monodone = colrdone = maskdone = FALSE;
		if (w == 8 && h == 8 && img->monodata != lispnil && img->istile) {
			/* Monochrome pattern. */
			datahandle = NewHandle(8);
			memset(*datahandle, 0, 8);
			/* Read exactly 8 bytes. */
			mac_interp_bytes(img->monodata, 8, datahandle, 0);
			/* Fill in the monopat. */
			for (i = 0; i < 8; ++i)
			  SET_IMG_PAT(macimg, i, (*datahandle)[i]);
			macimg->patdefined = TRUE;
			/* Patterns have no masks, but defeat subsequent mask hacking. */
			monodone = maskdone = TRUE;
		}
		if (w == 16 && h == 16 && img->monodata != lispnil) {
			/* Shape is appropriate for a small icon, make one. */
			sicnhandle = NewHandle(32);
			memset(*sicnhandle, 0, 32);
			/* Read exactly 32 bytes. */
			mac_interp_bytes(img->monodata, 32, sicnhandle, 0);
			macimg->monosicn = sicnhandle;
			monodone = TRUE;
		}
		if (w == 16 && h == 16 && img->maskdata != lispnil) {
			/* Shape is appropriate for a small icon mask, make one. */
			sicnhandle = NewHandle(32);
			memset(*sicnhandle, 0, 32);
			/* Read exactly 32 bytes. */
			mac_interp_bytes(img->maskdata, 32, sicnhandle, 0);
			macimg->masksicn = sicnhandle;
			maskdone = TRUE;
		}
		if (w == 32 && h == 32 && img->monodata != lispnil) {
			/* Shape is appropriate for a standard icon, make one. */
			iconhandle = NewHandle(128);
			memset(*iconhandle, 0, 128);
			/* Read exactly 128 bytes. */
			mac_interp_bytes(img->monodata, 128, iconhandle, 0);
			macimg->monoicon = iconhandle;
			monodone = TRUE;
		}
		if (w == 32 && h == 32 && img->maskdata != lispnil) {
			/* Shape is appropriate for a standard icon, make one. */
			iconhandle = NewHandle(128);
			memset(*iconhandle, 0, 128);
			/* Read exactly 128 bytes. */
			mac_interp_bytes(img->maskdata, 128, iconhandle, 0);
			macimg->maskicon = iconhandle;
			maskdone = TRUE;
		}
		/* The mono and mask data are probably going to go into the cicn - save them
		   into a handle for now. */
		if (!monodone && img->monodata != lispnil) {
			if (rowbytes < 1)
			  rowbytes = ((actualw + 15) / 16) * 2;
			numbytes = actualh * (actualw <= 8 ? 2 : rowbytes);
			monohandle = NewHandle(numbytes);
			memset(*maskhandle, 0, numbytes);
			/* Read as many bytes as directed. */
			mac_interp_bytes(img->monodata, numbytes, monohandle, (actualw <= 8 ? 1 : 0));
			monodone = TRUE;
		}
		if (!maskdone && img->maskdata != lispnil) {
			if (rowbytes < 1)
			  rowbytes = ((actualw + 15) / 16) * 2;
			numbytes = actualh * (actualw <= 8 ? 2 : rowbytes);
			maskhandle = NewHandle(numbytes);
			memset(*maskhandle, 0, numbytes);
			/* Read as many bytes as directed. */
			mac_interp_bytes(img->maskdata, numbytes, maskhandle, (actualw <= 8 ? 1 : 0));
			maskdone = TRUE;
		}
		/* Limit bits per pixel to reasonable values. */
		if (pixelsize < 1 || pixelsize > 8)
		  pixelsize = 8;
		/* A sufficiently small tiling color image can be a color pattern. */
		if (hasColorQD && actualw <= 64 && actualh <= 64 && img->istile && img->colrdata != lispnil) {
			/* Make a color pattern. */
			ppathandle = NewPixPat();
			pmhandle = (*ppathandle)->patMap;
			SetRect(&((*pmhandle)->bounds), 0, 0, actualw, actualh);
			if (rowbytes < 1)
			  rowbytes = (actualw * pixelsize) / 8;
			(*pmhandle)->rowBytes = rowbytes;
			(*pmhandle)->pixelSize = pixelsize;
			(*pmhandle)->pmTable = interp_ctab(img->palette);
			numbytes = actualh * rowbytes;
			datahandle = NewHandle(numbytes);
			(*ppathandle)->patData = datahandle;
			mac_interp_bytes(img->colrdata, numbytes, datahandle, 0);
			(*ppathandle)->patXValid = -1;
			macimg->colrpat = ppathandle;
			colrdone = maskdone = TRUE;
		}
		if (hasColorQD && actualw <= 64 && actualh <= 64
		    && !img->istile
		    && (img->colrdata != lispnil
		        || (macimg->monosicn == nil
		            && macimg->masksicn == nil
		            && macimg->monoicon == nil
		            && macimg->maskicon == nil
		           ))) {
		    /* If no color data in evidence, prepare to use mono or mask instead. */
		    if (img->colrdata == lispnil) {
		    	pixelsize = 1;
		    }
			/* Make a full color icon. */
			bitrowbytes = ((actualw + 15) / 16) * 2;
			/* Allocate enough space for the icon and its data/mask bitmaps. */
			cicnhandle = (CIconHandle) NewHandle(sizeof(CIcon) + 2 * actualh * bitrowbytes);
			HLock((Handle) cicnhandle);
			(*cicnhandle)->iconPMap.baseAddr = 0;
			if (rowbytes < 1)
			  rowbytes = ((actualw + 15) / 16) * pixelsize * 2;
			(*cicnhandle)->iconPMap.rowBytes = rowbytes | 0x8000;
			SetRect(&((*cicnhandle)->iconPMap.bounds), 0, 0, actualw, actualh);
			(*cicnhandle)->iconPMap.pmVersion = 0;
			(*cicnhandle)->iconPMap.packType = 0;
			(*cicnhandle)->iconPMap.packSize = 0;
			(*cicnhandle)->iconPMap.hRes = 0;
			(*cicnhandle)->iconPMap.vRes = 0;
			(*cicnhandle)->iconPMap.pixelType = 0;
			(*cicnhandle)->iconPMap.pixelSize = pixelsize;
			(*cicnhandle)->iconPMap.cmpCount = 1;
			(*cicnhandle)->iconPMap.cmpSize = pixelsize;
			(*cicnhandle)->iconPMap.planeBytes = 0;
			(*cicnhandle)->iconPMap.pmTable = interp_ctab(img->palette);
			(*cicnhandle)->iconPMap.pmReserved = 0;
			/* Configure the monochrome icon. */
			SetRect(&((*cicnhandle)->iconBMap.bounds), 0, 0, actualw, actualh);
			(*cicnhandle)->iconBMap.rowBytes = 0;
			(*cicnhandle)->iconBMap.baseAddr = NULL;
			SetRect(&((*cicnhandle)->iconMask.bounds), 0, 0, actualw, actualh);
			/* Configure the mask bitmap. */
			(*cicnhandle)->iconMask.rowBytes = bitrowbytes;
			(*cicnhandle)->iconMask.baseAddr = NULL;
			numbytes = actualh * rowbytes;
			datahandle = NewHandle(numbytes);
			(*cicnhandle)->iconData = datahandle;
			/* Fill up the datahandle with the color data, or else use mono/mask data
			   if the color data is missing. */
			if (img->colrdata != lispnil) {
				mac_interp_bytes(img->colrdata, numbytes, datahandle, 0);
			} else if (img->monodata != lispnil) {
				mac_interp_bytes(img->monodata, numbytes, datahandle, (actualw <= 8 ? 1 : 0));
				/* We need an ersatz color table. */
				(*cicnhandle)->iconPMap.pmTable = synth_ctab();
			} else if (img->maskdata != lispnil) {
				mac_interp_bytes(img->maskdata, numbytes, datahandle, (actualw <= 8 ? 1 : 0));
				/* We need an ersatz color table. */
				(*cicnhandle)->iconPMap.pmTable = synth_ctab();
			}
			/* If a mono icon is already set up, use it as the icon's bitmap. */
			if (macimg->monoicon != nil) {
				HLock(macimg->monoicon);
				memcpy(((char *) (*cicnhandle)->iconMaskData) + actualh * bitrowbytes,
					   *(macimg->monoicon), actualh * bitrowbytes);
				(*cicnhandle)->iconBMap.rowBytes = bitrowbytes;
				HUnlock(macimg->monoicon);
			}
			/* If a mono sicn is already set up, use it as the icon's bitmap. */
			if (macimg->monosicn != nil) {
				HLock(macimg->monosicn);
				memcpy(((char *) (*cicnhandle)->iconMaskData) + actualh * bitrowbytes,
					   *(macimg->monosicn), actualh * bitrowbytes);
				(*cicnhandle)->iconBMap.rowBytes = bitrowbytes;
				HUnlock(macimg->monosicn);
			}
			/* ...or just use the monohandle that we built earlier. */
			if (monohandle != nil) {
				HLock(monohandle);
				memcpy(((char *) (*cicnhandle)->iconMaskData) + actualh * bitrowbytes,
					   *monohandle, actualh * bitrowbytes);
				(*cicnhandle)->iconBMap.rowBytes = bitrowbytes;
				HUnlock(monohandle);
			}
			/* If a mask icon is already set up, use it as the icon's mask. */
			if (macimg->maskicon != nil) {
				HLock(macimg->maskicon);
				memcpy((char *) (*cicnhandle)->iconMaskData,
					   *(macimg->maskicon), actualh * bitrowbytes);
				HUnlock(macimg->maskicon);
			}
			/* If a mask sicn is already set up, use it as the icon's mask. */
			if (macimg->masksicn != nil) {
				HLock(macimg->masksicn);
				memcpy((char *) (*cicnhandle)->iconMaskData,
					   *(macimg->masksicn), actualh * bitrowbytes);
				HUnlock(macimg->masksicn);
			}
			/* ...or just use the maskhandle that we built earlier. */
			if (maskhandle != nil) {
				HLock(maskhandle);
				memcpy((char *) (*cicnhandle)->iconMaskData,
					   *maskhandle, actualh * bitrowbytes);
				HUnlock(maskhandle);
			}
			macimg->colricon = (Handle) cicnhandle;
			HUnlock((Handle) cicnhandle);
			colrdone = TRUE;
		}
		if (!monodone && !colrdone) {
			run_warning("%dx%d image of \"%s\" could not be interpreted", w, h, imf->name);
		}
    }
#if 0 /* broken */
    /* If we have a color pattern of random size, and an 8x8 mono pattern, then we should
       glue the mono pattern into the color pattern's mono slot.  This requires searching
       all pairs of images in the family. */
	for (img = imf->images; img != NULL; img = img->next) {
		Image *img2;
		MacImage *macimg2;
		macimg = get_mac_image(img);
		if (macimg->colrpat != nil) {
			for (img2 = imf->images; img2 != NULL; img2 = img2->next) {
				macimg2 = get_mac_image(img2);
				if (macimg2->patdefined) {
					memcpy((char *) &(*(macimg->colrpat)->pat1Data), (char *) &(macimg2->monopat), 8);
				}
			}
		}
	}
#endif
}

void
mac_interp_bytes(datalist, numbytes, desthandle, jump)
Obj *datalist;
int numbytes, jump;
Handle desthandle;
{
    HLock(desthandle);
    interp_bytes(datalist, numbytes, *desthandle, jump);
    HUnlock(desthandle);
}

/* Given a list of color table entries, create and return a color table. */

CTabHandle
interp_ctab(palette)
Obj *palette;
{
	int len, i;
	Obj *head, *rest, *color;
	ColorSpec *ctdata;
	CTabHandle ctabhandle;

	len = length(palette);
	if (len == 0)
	  return nil;
	ctabhandle = (CTabHandle) NewHandle(8 + len * 8);
	HLock((Handle) ctabhandle);
	(*ctabhandle)->ctFlags = 0;
	(*ctabhandle)->ctSeed = GetCTSeed();
	(*ctabhandle)->ctSize = len - 1;
	ctdata = (ColorSpec *) &((*ctabhandle)->ctTable);
	for (i = 0, rest = palette; i < len; ++i, rest = cdr(rest)) {
		head = car(rest);
		ctdata[i].value = c_number(car(head));
		color = cdr(head);
		ctdata[i].rgb.red = c_number(car(color));
		ctdata[i].rgb.green = c_number(cadr(color));
		ctdata[i].rgb.blue = c_number(car(cddr(color)));
	}
	HUnlock((Handle) ctabhandle);
	return ctabhandle;
}

CTabHandle
synth_ctab()
{
	int len;
	ColorSpec *ctdata;
	CTabHandle ctabhandle;

	len = 2;
	ctabhandle = (CTabHandle) NewHandle(8 + len * 8);
	HLock((Handle) ctabhandle);
	(*ctabhandle)->ctFlags = 0;
	(*ctabhandle)->ctSeed = GetCTSeed();
	(*ctabhandle)->ctSize = len - 1;
	ctdata = (ColorSpec *) &((*ctabhandle)->ctTable);
	ctdata[0].value = 0;
	ctdata[0].rgb.red = 65535;
	ctdata[0].rgb.green = 65535;
	ctdata[0].rgb.blue = 65535;
	ctdata[1].value = 1;
	ctdata[1].rgb.red = 0;
	ctdata[1].rgb.green = 0;
	ctdata[1].rgb.blue = 0;
	HUnlock((Handle) ctabhandle);
	return ctabhandle;
}

void
make_generic_image_data(ImageFamily *imf)
{
	int i, hasmono, hasmask;
	Image *img;
	MacImage *macimg;

	for (img = imf->images; img != NULL; img = img->next) {
		hasmono = hasmask = FALSE;
		macimg = get_mac_image(img);
		if (hasColorQD && macimg->colricon != nil) {
			convert_cicn(img, (CIconHandle) macimg->colricon, &hasmono, &hasmask);
		}
		if (hasColorQD && macimg->colrpat != nil) {
			convert_ppat(img, macimg->colrpat);
		}
		if (macimg->monoicon != nil && !hasmono) {
			img->rawmonodata = xmalloc(128);
			for (i = 0; i < 128; ++i)
			  img->rawmonodata[i] = ((char *) *(macimg->monoicon))[i];
		}
		if (macimg->maskicon != nil && !hasmask) {
			img->rawmaskdata = xmalloc(128);
			for (i = 0; i < 128; ++i)
			  img->rawmaskdata[i] = ((char *) *(macimg->maskicon))[i];
		}
		if (macimg->monosicn != nil && !hasmono) {
			img->rawmonodata = xmalloc(32);
			for (i = 0; i < 32; ++i)
			  img->rawmonodata[i] = ((char *) *(macimg->monosicn))[i];
		}
		if (macimg->masksicn != nil && !hasmask) {
			img->rawmaskdata = xmalloc(32);
			for (i = 0; i < 32; ++i)
			  img->rawmaskdata[i] = ((char *) *(macimg->masksicn))[i];
		}
		if (macimg->patdefined) {
			img->rawmonodata = xmalloc(8);
			for (i = 0; i < 8; ++i)
			  img->rawmonodata[i] = ((char *) &(macimg->monopat))[i];
		}
	}
}

/* Generify a color pattern. */

void
convert_ppat(Image *img, PixPatHandle pat)
{
	int w, h, i, numbytes;
	Rect bounds;
	PixMapHandle pmhandle = (*pat)->patMap;
	CTabHandle ctabhandle = (*pmhandle)->pmTable;
	Handle patdata = (*pat)->patData;

	switch ((*pat)->patType) {
		case 0:
			/* (should put something in monodata?) */
			break;
		case 1:
			/* Compute the actual size of the pattern. */
			bounds = (*pmhandle)->bounds;
			w = bounds.right - bounds.left;  h = bounds.bottom - bounds.top;
			if (w != img->w || h != img->h) {
				img->actualw = w;  img->actualh = h;
			}
			img->rowbytes = (*pmhandle)->rowBytes & 0x3fff;
			img->pixelsize = (*pmhandle)->pixelSize;
			img->palette = convert_ctab(ctabhandle);
			numbytes = h * img->rowbytes;
			img->rawcolrdata = xmalloc(numbytes);
			for (i = 0; i < numbytes; ++i)
			  img->rawcolrdata[i] = (*patdata)[i];
			break;
		case 2:
			run_warning("Type 2 (RGB) pattern, ignoring");
			break;
	}
}

/* Generify the data in a color icon. */

void
convert_cicn(Image *img, CIconHandle cicnhandle, int *hasmono, int *hasmask)
{
	int w, h, i, rowbytes, numbytes;
	Rect bounds;
	char *baseaddr;
	PixMap pmap = (*cicnhandle)->iconPMap;
	CTabHandle ctabhandle = pmap.pmTable;
	Handle datahandle;

	*hasmono = *hasmask = FALSE;
	HLock((Handle) cicnhandle);
	bounds = pmap.bounds;
	w = bounds.right - bounds.left;  h = bounds.bottom - bounds.top;
	if (w != img->w || h != img->h) {
		img->actualw = w;  img->actualh = h;
	}
	img->rowbytes = pmap.rowBytes & 0x3fff;
	img->pixelsize = pmap.pixelSize;
	img->palette = convert_ctab(ctabhandle);
	numbytes = h * img->rowbytes;
	img->rawcolrdata = xmalloc(numbytes);
	datahandle = (*cicnhandle)->iconData;
	for (i = 0; i < numbytes; ++i)
	  img->rawcolrdata[i] = (*datahandle)[i];
	/* Convert the cicn's monochrome icon if defined. */
	rowbytes = (*cicnhandle)->iconBMap.rowBytes;
	if (rowbytes > 0) {
		baseaddr = ((char *) (*cicnhandle)->iconMaskData) + h * rowbytes;
		numbytes = h * rowbytes;
		img->rawmonodata = xmalloc(numbytes);
		for (i = 0; i < numbytes; ++i) {
			img->rawmonodata[i] = baseaddr[i];
			/* For small icons, jump over a not-strictly-necessary padding char. */
			if (w <= 8 && rowbytes == 2) ++i;
		}
		*hasmono = TRUE;
	}
	/* Write the cicn's mask if one is defined. */
	rowbytes = (*cicnhandle)->iconMask.rowBytes;
	if (rowbytes > 0) {
		baseaddr = (char *) (*cicnhandle)->iconMaskData;
		numbytes = h * rowbytes;
		img->rawmaskdata = xmalloc(numbytes);
		for (i = 0; i < numbytes; ++i) {
			img->rawmaskdata[i] = baseaddr[i];
			/* For small icons, jump over a not-strictly-necessary padding char. */
			if (w <= 8 && rowbytes == 2) ++i;
		}
		*hasmask = TRUE;
	}
	HUnlock((Handle) cicnhandle);
}

/* Write a color table. */

Obj *
convert_ctab(CTabHandle ctabhandle)
{
	int c, ctsize;
	ColorSpec cspec;
	Obj *rslt = lispnil, *tmp, *rest, *restprev, *tmp2;

	if (ctabhandle == nil)
	  return lispnil;
	ctsize = (*ctabhandle)->ctSize;
	if (ctsize >= 0) {
		HLock((Handle) ctabhandle);
		for (c = 0; c <= ctsize; ++c) {
			cspec = (*ctabhandle)->ctTable[c];
			tmp = cons(new_number(cspec.value),
					   cons(new_number(cspec.rgb.red),
					   		cons(new_number(cspec.rgb.green),
					   			 cons(new_number(cspec.rgb.blue),
					   			 	  lispnil))));
			/* Splice the new entry into the list so that all are sorted by index. */
			restprev = lispnil;
			for (rest = rslt;
				 cspec.value > (numberp(car(car(rest))) ? c_number(car(car(rest))) : 1000000);
				 rest = cdr(rest)) {
				restprev = rest;
			}
			tmp2 = cons(tmp, rest);
			if (restprev != lispnil)
			  set_cdr(restprev, tmp2);
			else
			  rslt = tmp2;
		}
		HUnlock((Handle) ctabhandle);
	}
	return rslt;
}

void
mac_load_image_color(imc)
ImageColor *imc;
{
	Handle imchandle;
    Str255 namestr;

	if (imc->name == NULL)
	  return;
	c2p(imc->name, namestr);
	imchandle = GetNamedResource('XCic', namestr);
	if (imchandle != nil) {
		imc->r = ((short *) (*imchandle))[0];
		imc->g = ((short *) (*imchandle))[1];
		imc->b = ((short *) (*imchandle))[2];
		imc->defined = 1;
	} else {
		/* Not found. */
		imc->r = imc->g = imc->b = 0;
		imc->defined = 0;
	}
}
