/* Low-level drawing for the Mac interface to Xconq.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* Routines in this file should rarely, if ever, call run_warning,
   since the exposure following the handling of the warning dialog
   can cause the same warning to appear again, and the player can
   get pretty wedged. */

#include "conq.h"
#define wind_dir(w) ((w) & 0x07)
#define wind_force(w) ((w) >> 3)
#include "macconq.h"

PolyHandle polygons[NUMPOWERS];
int lastpolyx[NUMPOWERS], lastpolyy[NUMPOWERS];

RgnHandle cellrgns[NUMPOWERS];
int lastcellrgnx[NUMPOWERS], lastcellrgny[NUMPOWERS];
RgnHandle gridcellrgns[NUMPOWERS];
int lastgridcellrgnx[NUMPOWERS], lastgridcellrgny[NUMPOWERS];

RgnHandle cellrgns30[NUMPOWERS];
int lastcellrgnx30[NUMPOWERS], lastcellrgny30[NUMPOWERS];
RgnHandle gridcellrgns30[NUMPOWERS];
int lastgridcellrgnx30[NUMPOWERS], lastgridcellrgny30[NUMPOWERS];

RgnHandle cellrgns15[NUMPOWERS];
int lastcellrgnx15[NUMPOWERS], lastcellrgny15[NUMPOWERS];
RgnHandle gridcellrgns15[NUMPOWERS];
int lastgridcellrgnx15[NUMPOWERS], lastgridcellrgny15[NUMPOWERS];

int numwindsicns = 0;

Handle windsicnhandle[5];

int numblastsicns = 0;

Handle blastsicnhandle[3];

static Image **best_terrain_images = NULL;

static Image **best_unseen_images = NULL;

/* Draw an image of the given type of unit in the given rectangle, possibly adding
   an emblem if requested. */

void
draw_unit_image(WindowPtr win, int sx, int sy, int sw, int sh, int u, int e, int mod)
{
	int ex, ey, ew, eh;
	Rect srcrect, imagerect;
	BitMap bm, *winbits;
	Image *uimg;
	MacImage *macuimg;

	/* Filter out very small images. */
	if (sw <= 1)
	  return;
	imagerect.top = sy;  imagerect.left = sx;
	imagerect.bottom = imagerect.top + sh;  imagerect.right = imagerect.left + sw;
	if (sw <= 4) {
		/* (should draw in a distinctive color if one is available) */
		FillRect(&imagerect, QDPat(black));
		return;
	}
	uimg = best_image(uimages[u], sw, sh);
	/* There should always be *some* image to display. */
	if (uimg && uimg->hook) {
		macuimg = (MacImage *) uimg->hook;
		winbits = &(((GrafPtr) win)->portBits);
		if (macuimg->monopict != nil) {
			DrawPicture(macuimg->monopict, &imagerect);
		} else if (macuimg->colricon != nil
				   && (minscreendepth > 1
				   	   || (macuimg->monoicon == nil && macuimg->monosicn == nil))) {
			PlotCIcon(&imagerect, (CIconHandle) macuimg->colricon);
		} else if (macuimg->monoicon != nil) {
			SetRect(&srcrect, 0, 0, 32, 32);
			bm.rowBytes = 4;
			bm.bounds = srcrect;
			if (macuimg->maskicon != nil) {
				bm.baseAddr = *(macuimg->maskicon);
				CopyBits(&bm, winbits, &srcrect, &imagerect, srcBic, nil);
			} else {
				/* Draw unit bbox as default mask. (maybe shrink a little??) */
				FillRect(&imagerect, QDPat(white));
			}
			bm.baseAddr = *(macuimg->monoicon);
			CopyBits(&bm, winbits, &srcrect, &imagerect, srcOr, nil);
		} else if (macuimg->monosicn != nil) {
			SetRect(&srcrect, 0, 0, 16, 16);
			if (sw > 64) {
				InsetRect(&imagerect, sw / 4, sh / 4);
			}
			bm.rowBytes = 2;
			bm.bounds = srcrect;
			if (macuimg->masksicn != nil) {
				bm.baseAddr = *(macuimg->masksicn);
				CopyBits(&bm, winbits, &srcrect, &imagerect, srcBic, nil);
			} else {
				/* Draw unit bbox as default mask. (maybe shrink a little??) */
				FillRect(&imagerect, QDPat(white));
			}
			bm.baseAddr = *(macuimg->monosicn);
			CopyBits(&bm, winbits, &srcrect, &imagerect, srcOr, nil);
		} else {
			FillRect(&imagerect, QDPat(black));
		}
	} else {
		/* Rather a crummy substitute. */
		FillRect(&imagerect, QDPat(black));
	}
	if (mod != 0) {
		gray_out_rect(&imagerect);
	}
	/* Now draw a side emblem if asked for. */
	if (between(0, e, numsides)) {
		if (uimg &&
		    uimg->embedname &&
		    side_n(e) &&
		    side_n(e)->emblemname &&
		    strcmp(uimg->embedname, side_n(e)->emblemname) == 0) {
		    /* Correct emblem is part of the unit's image, don't need to draw. */
		} else {
		    /* Get the size of the emblem, either from the image or by computing
		       a reasonable default. */
			if (uimg && uimg->embedw > 0 && uimg->embedh > 0) {
				ew = uimg->embedw;  eh = uimg->embedh;
			} else {
				ew = min(sw, max(8, sw / 4));  eh = min(sh, max(8, sh / 4));
			}
			/* Position the emblem, either explicitly, or default to UR corner
			   (note that we need the emblem's width to do this) */
			if (uimg && uimg->embedx >= 0 && uimg->embedy >= 0) {
				ex = uimg->embedx;  ey = uimg->embedy;
			} else {
				ex = sw - ew;  ey = 0;
			}
			/* Do the drawing proper. */
			draw_side_emblem(win, sx + ex, sy + ey, ew, eh, e, plain_emblem);
		}
	}
}

/* Draw a given side id's emblem. Uses the current GrafPort. */

void
draw_side_emblem(WindowPtr win, int ex, int ey, int ew, int eh, int e, int style)
{
	int actualw, actualh;
	Rect srcrect, imagerect, shadowrect;
	BitMap bm, *winbits;
	CIconHandle cicnhandle;
	Image *eimg;
	MacImage *maceimg;

	/* Filter out very small images. */
	if (ew <= 1)
	  return;
	if (ew <= 2) {
		/* (should draw in a distinctive color if one is available) */
		/* FillRect(&imagerect, QDPat(black)); */
		return;
	}
	eimg = best_image(eimages[e], ew, eh);
	imagerect.top = ey;  imagerect.left = ex;
	actualw = eimg->w;  actualh = eimg->h;
	if (ew >= actualw * 2 && eh >= actualh * 2) {
		actualw *= 2;  actualh *= 2;
	}
	if (ew >= actualw * 2 && eh >= actualh * 2) {
		actualw *= 2;  actualh *= 2;
	}
	if (actualw < ew)
	  ew = actualw;
	if (actualh < eh)
	  eh = actualh;
	imagerect.bottom = imagerect.top + eh;  imagerect.right = imagerect.left + ew;
	/* If an image is available, display it, otherwise do nothing. */
	if (eimg && eimg->hook) {
		maceimg = (MacImage *) eimg->hook;
		winbits = &(((GrafPtr) win)->portBits);
		if (maceimg->monopict != nil) {
			/* (should use a mask and shadow here somehow) */
			DrawPicture(maceimg->monopict, &imagerect);
		} else if (maceimg->colricon != nil
				   && (minscreendepth > 1
				   	   || (maceimg->monoicon == nil && maceimg->monosicn == nil))) {
			cicnhandle = (CIconHandle) maceimg->colricon;
			if (style == shadow_emblem) {
				SetRect(&srcrect, 0, 0, eimg->w, eimg->h);
				shadowrect = imagerect;
				OffsetRect(&shadowrect, (ew < 16 ? 1 : 2), (ew < 16 ? 1 : 2));
				bm.rowBytes = (*cicnhandle)->iconBMap.rowBytes;
				bm.bounds = srcrect;
				if (bm.rowBytes > 0) {
					bm.baseAddr = (char *) (*cicnhandle)->iconMaskData;
				}
				if (hasColorQD) {
					RGBColor tmpcolor;

					tmpcolor.red = tmpcolor.green = tmpcolor.blue = 49000;
					RGBForeColor(&tmpcolor);
					if (bm.rowBytes > 0) {
						CopyBits(&bm, winbits, &srcrect, &shadowrect, srcCopy, nil);
					} else {
						PaintRect(&shadowrect);
					}
					/* Restore the previous color. */
					tmpcolor.red = tmpcolor.green = tmpcolor.blue = 0;
					RGBForeColor(&tmpcolor);
				} else {
					FillRect(&shadowrect, QDPat(gray));
				}
			}
			PlotCIcon(&imagerect, cicnhandle);
		} else if (maceimg->monoicon != nil) {
			SetRect(&srcrect, 0, 0, 32, 32);
			bm.rowBytes = 4;
			bm.bounds = srcrect;
			if (maceimg->maskicon != nil) {
				bm.baseAddr = *(maceimg->maskicon);
				if (style == shadow_emblem) {
				}
				CopyBits(&bm, winbits, &srcrect, &imagerect, srcBic, nil);
			} else {
				/* Use emblem bbox as default mask. */
				if (style == shadow_emblem) {
				}
				FillRect(&imagerect, QDPat(white));
			}
			bm.baseAddr = *(maceimg->monoicon);
			CopyBits(&bm, winbits, &srcrect, &imagerect, srcOr, nil);
		} else if (maceimg->monosicn != nil) {
			SetRect(&srcrect, 0, 0, 16, 16);
			bm.rowBytes = 2;
			bm.bounds = srcrect;
			if (maceimg->masksicn != nil) {
				bm.baseAddr = *(maceimg->masksicn);
				if (style == shadow_emblem) {
				}
				CopyBits(&bm, winbits, &srcrect, &imagerect, srcBic, nil);
			} else {
				/* Use emblem bbox as default mask. */
				if (style == shadow_emblem) {
				}
				FillRect(&imagerect, QDPat(white));
			}
			bm.baseAddr = *(maceimg->monosicn);
			CopyBits(&bm, winbits, &srcrect, &imagerect, srcOr, nil);
		} else {
			FillRect(&imagerect, QDPat(black));
		}
	}
}

void
calc_best_terrain_images()
{
	int p, t;
	Image *timg;

	best_terrain_images = (Image **) xmalloc(NUMPOWERS * numttypes * sizeof(Image *));
	for_all_terrain_types(t) {
		for (p = 0; p < NUMPOWERS; ++p) {
			timg = best_image(timages[t], hws[p], hcs[p]);
			best_terrain_images[p * numttypes + t] = timg;
		}
	}
	best_unseen_images = (Image **) xmalloc(NUMPOWERS * sizeof(Image *));
	if (unseen_image != NULL) {
		for (p = 0; p < NUMPOWERS; ++p) {
			timg = best_image(unseen_image, hws[p], hcs[p]);
			best_unseen_images[p] = timg;
		}
	}
}

void
draw_cell_block(int sx, int sy, int n, int power, int t, int t2, int angle)
{
	Rect rect;
	Image *timg;
	MacImage *mactimg;
	RGBColor cellcolor, oldcolor;				

	rect.left = sx;  rect.top = sy;
	rect.right = rect.left + n * hws[power];  rect.bottom = rect.top + hcs[power];
	if (angle == 30) {
		rect.bottom = rect.top + hcs[power] / 2;
	} else if (angle == 15) {
		rect.bottom = rect.top + hcs[power] / 4;
	}
	if (best_terrain_images == NULL)
	  calc_best_terrain_images();
	if (t == NONTTYPE)
	  timg = best_unseen_images[power];
	else
	  timg = best_terrain_images[power * numttypes + t];
	if (timg && timg->hook) {
		mactimg = (MacImage *) timg->hook;
		if (hasColorQD) {
			if (mactimg->colrpat != nil && (minscreendepth > 1 || !mactimg->patdefined)) {
				FillCRect(&rect, mactimg->colrpat);
			} else if (tcolors[t] != NULL && tcolors[t]->defined && minscreendepth > 1) {
				/* A solid color is preferable to a b/w pattern. */
				cellcolor.red   = (tcolors[t]->r) << 8;
				cellcolor.green = (tcolors[t]->g) << 8;
				cellcolor.blue  = (tcolors[t]->b) << 8;
				RGBForeColor(&cellcolor);
				PaintRect(&rect);
				/* Restore the previous color. */
				oldcolor.red = oldcolor.green = oldcolor.blue = 0;
				RGBForeColor(&oldcolor);
			} else if (mactimg->patdefined) {
				FillRect(&rect, IMG_PAT(mactimg));
			}
		} else {
			FillRect(&rect, IMG_PAT(mactimg));
		}
	} else {
		/* No image; use a color instead. */
		if (hasColorQD) {
			if (tcolors[t] != NULL && tcolors[t]->defined && minscreendepth > 1) {
				cellcolor.red   = (tcolors[t]->r) << 8;
				cellcolor.green = (tcolors[t]->g) << 8;
				cellcolor.blue  = (tcolors[t]->b) << 8;
				RGBForeColor(&cellcolor);
				PaintRect(&rect);
				/* Restore the previous color. */
				oldcolor.red = oldcolor.green = oldcolor.blue = 0;
				RGBForeColor(&oldcolor);
			} else {
				/* leave it blank? sigh */
			}
		} else {
			/* leave it blank? sigh */
		}
	}
	/* If any graying/shading was requested, do it. */
	if (t2 < 0) {
		if (t2 == -1) {
			PenPat(QDPat(ltGray));
			PenMode(patOr);
		} else if (t2 == -2) {
			PenPat(QDPat(gray));
/*				PenMode(patBic);  */
			PenMode(notPatOr);
		}
		PaintRect(&rect);
		PenNormal();
	}
}

/* Draw the given terrain type into a hexagonal region of the given size and position. */

void
draw_hex_region(int sx, int sy, int power, int dogrid, int t, int t2, int angle)
{
	Image *timg;
	MacImage *mactimg;
	RGBColor hexcolor, oldcolor;
	RgnHandle rgn;

	if (best_terrain_images == NULL)
	  calc_best_terrain_images();
    if (cellrgns[power] == nil)
	  make_cell_clip(power);
	if (angle == 30) {
  		OffsetRgn(cellrgns30[power], sx - lastcellrgnx30[power], sy - lastcellrgny30[power]);
  		lastcellrgnx30[power] = sx;  lastcellrgny30[power] = sy;
  		OffsetRgn(gridcellrgns30[power], sx - lastgridcellrgnx30[power], sy - lastgridcellrgny30[power]);
  		lastgridcellrgnx30[power] = sx;  lastgridcellrgny30[power] = sy;
		rgn = (dogrid ? gridcellrgns30[power] : cellrgns30[power]);
	} else if (angle == 15) {
  		OffsetRgn(cellrgns15[power], sx - lastcellrgnx15[power], sy - lastcellrgny15[power]);
 		lastcellrgnx15[power] = sx;  lastcellrgny15[power] = sy;
  		OffsetRgn(gridcellrgns15[power], sx - lastgridcellrgnx15[power], sy - lastgridcellrgny15[power]);
  		lastgridcellrgnx15[power] = sx;  lastgridcellrgny15[power] = sy;
		rgn = (dogrid ? gridcellrgns15[power] : cellrgns15[power]);
	} else {
  		OffsetRgn(cellrgns[power], sx - lastcellrgnx[power], sy - lastcellrgny[power]);
  		lastcellrgnx[power] = sx;  lastcellrgny[power] = sy;
  		OffsetRgn(gridcellrgns[power], sx - lastgridcellrgnx[power], sy - lastgridcellrgny[power]);
 		lastgridcellrgnx[power] = sx;  lastgridcellrgny[power] = sy;
		rgn = (dogrid ? gridcellrgns[power] : cellrgns[power]);
	}
	if (t == NONTTYPE)
	  timg = best_unseen_images[power];
	else
	  timg = best_terrain_images[power * numttypes + t];
	if (timg && timg->hook) {
		mactimg = (MacImage *) timg->hook;
		if (hasColorQD) {
			if (mactimg->colrpat != nil && (minscreendepth > 1 || !mactimg->patdefined)) {
				FillCRgn(rgn, mactimg->colrpat);
			} else if (tcolors[t] != NULL && tcolors[t]->defined && minscreendepth > 1) {
				/* A solid color is preferable to a b/w pattern. */
				hexcolor.red   = (tcolors[t]->r) << 8;
				hexcolor.green = (tcolors[t]->g) << 8;
				hexcolor.blue  = (tcolors[t]->b) << 8;
				RGBForeColor(&hexcolor);
				PaintRgn(rgn);
				/* Restore the previous color. */
				oldcolor.red = oldcolor.green = oldcolor.blue = 0;
				RGBForeColor(&oldcolor);
			} else if (mactimg->patdefined) {
				/* Fall back on the b/w pattern. */
				FillRgn(rgn, IMG_PAT(mactimg));
			}
		} else {
			FillRgn(rgn, IMG_PAT(mactimg));
		}
	} else {
		if (hasColorQD) {
			if (tcolors[t] != NULL && tcolors[t]->defined && maxscreendepth > 1) {
				/* Use the solid color. */
				hexcolor.red   = (tcolors[t]->r) << 8;
				hexcolor.green = (tcolors[t]->g) << 8;
				hexcolor.blue  = (tcolors[t]->b) << 8;
				RGBForeColor(&hexcolor);
				PaintRgn(rgn);
				/* Restore the previous color. */
				oldcolor.red = oldcolor.green = oldcolor.blue = 0;
				RGBForeColor(&oldcolor);
			} else {
			}
		} else {
		}
	}
	/* Maybe overlay the cell. */
	if (t2 < 0) {
		if (t2 == -1) {
			PenPat(QDPat(ltGray));
			PenMode(patOr);
		} else if (t2 == -2) {
			PenPat(QDPat(gray));
/*				PenMode(patBic);  */
			PenMode(notPatOr);
		}
		PaintRgn(rgn);
		PenNormal();
	}
}

/* Draw a set of borders for the given position. */

void
draw_border_line_multiple(WindowPtr win, int sx, int sy, int bitmask, int power, int t, int angle)
{
	int wid = bwid[power], wid2, dir, tweakedcolor = FALSE;
	int sx1, sy1, sx2, sy2;
	Image *timg;
	MacImage *mactimg;
	RGBColor cellcolor, oldcolor;				

	if (wid == 0)
	  return;
	wid2 = wid / 2;
	if (0 /* power == 4*/) {
		Rect srcrect, destrect;
		BitMap *winbits;

		winbits = &(((GrafPtr) win)->portBits);
		SetRect(&srcrect, 0, 0, 32, 32);
		SetRect(&destrect, sx, sy, sx+32, sy+32);
		CopyBits(&(bordbitmaps[4]), winbits, &srcrect, &destrect, srcOr, nil);
		return;
	}
	PenSize(wid, wid);
	/* Decide on the line color/pattern to use. */
	timg = best_image(timages[t], wid, wid);
	if (timg && timg->hook) {
		mactimg = (MacImage *) timg->hook;
		if (hasColorQD) {
			if (mactimg->colrpat && (maxscreendepth > 1 || !mactimg->patdefined)) {
				PenPixPat(mactimg->colrpat);
			} else if (tcolors[t] != NULL && tcolors[t]->defined && minscreendepth > 1) {
				/* A solid color is preferable to a b/w pattern. */
				cellcolor.red   = (tcolors[t]->r) << 8;
				cellcolor.green = (tcolors[t]->g) << 8;
				cellcolor.blue  = (tcolors[t]->b) << 8;
				RGBForeColor(&cellcolor);
				tweakedcolor = TRUE;
			} else {
				PenPat(IMG_PAT(mactimg));
			}
		} else {
			PenPat(IMG_PAT(mactimg));
		}
	} else {
		if (hasColorQD) {
			if (tcolors[t] != NULL && tcolors[t]->defined && minscreendepth > 1) {
				/* A solid color is preferable to gray. */
				cellcolor.red   = (tcolors[t]->r) << 8;
				cellcolor.green = (tcolors[t]->g) << 8;
				cellcolor.blue  = (tcolors[t]->b) << 8;
				RGBForeColor(&cellcolor);
				tweakedcolor = TRUE;
			} else {
				PenPat(QDPat(dkGray));
			}
		} else {
			PenPat(QDPat(dkGray));
		}
	}
	for_all_directions(dir) {
		if (bitmask & (1 << dir)) {
			/* Actually draw the line. */
			sx1 = bsx[power][dir];  sy1 = bsy[power][dir];
			if (angle == 30) {
				sy1 /= 2;
			} else if (angle == 15) {
				sy1 /= 4;
			}
			MoveTo(sx + sx1 - wid2, sy + sy1 - wid2);
			sx2 = bsx[power][dir+1];  sy2 = bsy[power][dir+1];
			if (angle == 30) {
				sy2 /= 2;
			} else if (angle == 15) {
				sy2 /= 4;
			}
			LineTo(sx + sx2 - wid2, sy + sy2 - wid2);
		}
	}
	PenNormal();
	if (hasColorQD && tweakedcolor) {
		/* Restore the previous color. */
		oldcolor.red = oldcolor.green = oldcolor.blue = 0;
		RGBForeColor(&oldcolor);
	}
}

/* Draw a set of connections for the given ttype and given cell. */

void
draw_connection_line_multiple(WindowPtr win, int sx, int sy, int bitmask, int power, int t, int angle)
{
	int dir, wid = cwid[power], lh, ly, xoff, yoff, tweakedcolor = FALSE;
	Image *timg;
	MacImage *mactimg;
	RGBColor cellcolor, oldcolor;				

	if (wid == 0)
	  return;
	if (0 /*power == 4*/) {
		Rect srcrect, destrect;
		BitMap *winbits;

		winbits = &(((GrafPtr) win)->portBits);
		SetRect(&srcrect, 0, 0, 32, 32);
		SetRect(&destrect, sx, sy, sx+32, sy+32);
		CopyBits(&(connbitmaps[4]), winbits, &srcrect, &destrect, srcOr, nil);
		return;
	}
	PenSize(wid, wid);
	timg = best_image(timages[t], wid, wid);
	if (timg && timg->hook) {
		mactimg = (MacImage *) timg->hook;
		if (hasColorQD) {
			if (mactimg->colrpat && (maxscreendepth > 1 || !mactimg->patdefined)) {
				PenPixPat(mactimg->colrpat);
			} else if (tcolors[t] != NULL && tcolors[t]->defined && minscreendepth > 1) {
				/* A solid color is preferable to a b/w pattern. */
				cellcolor.red   = (tcolors[t]->r) << 8;
				cellcolor.green = (tcolors[t]->g) << 8;
				cellcolor.blue  = (tcolors[t]->b) << 8;
				RGBForeColor(&cellcolor);
				tweakedcolor = TRUE;
			} else {
				PenPat(IMG_PAT(mactimg));
			}
		} else {
			PenPat(IMG_PAT(mactimg));
		}
	} else {
		if (hasColorQD) {
			if (tcolors[t] != NULL && tcolors[t]->defined && minscreendepth > 1) {
				/* A solid color is preferable to gray. */
				cellcolor.red   = (tcolors[t]->r) << 8;
				cellcolor.green = (tcolors[t]->g) << 8;
				cellcolor.blue  = (tcolors[t]->b) << 8;
				RGBForeColor(&cellcolor);
				tweakedcolor = TRUE;
			} else {
				PenPat(QDPat(gray));
			}
		} else {
			PenPat(QDPat(gray));
		}
	}
	lh = hhs[power];
	if (angle == 30)
	  lh /= 4;
	else if (angle == 15)
	  lh /= 8;
	else
	  lh /= 2;
	xoff = hws[power] / 2 - wid / 2;
	yoff = lh - wid / 2;
	for_all_directions(dir) {
		if (bitmask & (1 << dir)) {
			MoveTo(sx + xoff, sy + yoff);
			ly = lsy[power][dir];
			if (angle == 30)
			  ly /= 2;
			else if (angle == 15)
			  ly /= 4;
			Line(lsx[power][dir], ly);
		}
	}
	PenNormal();
	if (hasColorQD && tweakedcolor) {
		/* Restore the previous color. */
		oldcolor.red = oldcolor.green = oldcolor.blue = 0;
		RGBForeColor(&oldcolor);
	}
}

/* This draws a type of terrain in a way that indicates its subtype. */

void
draw_terrain_sample(WindowPtr win, Rect tmprect, int t)
{
	switch (t_subtype(t)) {
		case cellsubtype:
			draw_hex_region(tmprect.left, tmprect.top, 4, FALSE, t, 0, 90);
			break;
		case bordersubtype:
			draw_border_line_multiple(win, tmprect.left, tmprect.top, -1, 4, t, 90);
			break;
		case connectionsubtype:
			draw_connection_line_multiple(win, tmprect.left, tmprect.top, -1, 4, t, 90);
			break;
		case coatingsubtype:
			draw_hex_region(tmprect.left, tmprect.top, 4, FALSE, t, 0, 90);
			/* Make it a 50% pattern. (should make more obvious somehow?) */
			gray_out_rect(&tmprect);
			break;
		default:
			terrain_subtype_warning("draw sample", t);
			break;
	}
}

/* Draw a set of country border at the given position. */

void
draw_country_borders(WindowPtr win, int sx, int sy, int bitmask, int power, int shade, int angle)
{
	int wid = bwid2[power], wid2, dir, dx1, dy1;

	if (wid == 0)
	  return;
	PenSize(wid, wid);
	if (shade == 0)
	  PenPat(QDPat(black));
	if (shade == 2)
	  PenPat(QDPat(gray));
	wid2 = wid / 2;
	for_all_directions(dir) {
		if (bitmask & (1 << dir)) {
			dx1 = bsx[power][dir];  dy1 = bsy[power][dir];
			if (angle == 30) { dy1 /= 2; }
			if (angle == 15) { dy1 /= 4; }
			MoveTo(sx + dx1 - wid2, sy + dy1 - wid2);
			dx1 = bsx[power][dir+1];  dy1 = bsy[power][dir+1];
			if (angle == 30) { dy1 /= 2; }
			if (angle == 15) { dy1 /= 4; }
			LineTo(sx + dx1 - wid2, sy + dy1 - wid2);
		}
	}
	PenNormal();
}

/* Draw a set of theater borders at the given position. */

void
draw_ai_region_borders(WindowPtr win, int sx, int sy, int bitmask, int power)
{
	int wid, wid2, dir;
	Rect tmprect;

	if (bwid[power] > 0) {
		wid = 2;
		wid2 = 1;
		PenSize(wid, wid);
		for_all_directions(dir) {
			if (bitmask & (1 << dir)) {
				MoveTo(sx + bsx[power][dir] - wid2 + 1, sy + bsy[power][dir] - wid2 + 1);
				LineTo(sx + bsx[power][dir+1] - wid2 + 1, sy + bsy[power][dir+1] - wid2 + 1);
			}
		}
		PenMode(notPatCopy);
		for_all_directions(dir) {
			if (bitmask & (1 << dir)) {
				MoveTo(sx + bsx[power][dir] - wid2, sy + bsy[power][dir] - wid2);
				LineTo(sx + bsx[power][dir+1] - wid2, sy + bsy[power][dir+1] - wid2);
			}
		}
		PenNormal();
	} else {
		SetRect(&tmprect, sx, sy, sx + hws[power], sy + hhs[power]);
		OffsetRect(&tmprect, 1, 1);
		FillRect(&tmprect, QDPat(black));
		OffsetRect(&tmprect, -1, -1);
		FillRect(&tmprect, QDPat(white));
	}
}

int
draw_elevation_here(int x, int y)
{
	return terrain_visible(dside, x, y);
}

/* Indicate the elevation of the given location, textually for now. */

void
draw_elevation(int sx, int sy, int power, int elev)
{
	sx += hws[power] / 2;  sy += hhs[power] / 2;
	if (elev != 0) {
		sprintf(spbuf, "%d", elev);
		draw_legend_text(sx, sy, hhs[power] / 2, spbuf, 0);
	}
}

/* Don't draw the temperature in every cell, only do ones with even coords or
   ones where the temperature in any adjacent cell is different. */

int
draw_temperature_here(int x, int y)
{
	int dir, x1, y1, temphere = temperature_view(dside, x, y);

#ifdef DESIGNERS
	/* Designers should see temperature in every cell. */
	if (dside->designer)
	  return TRUE;
#endif /* DESIGNERS */
	for_all_directions(dir) {
		if (interior_point_in_dir(x, y, dir, &x1, &y1)) {
			if (temphere != temperature_view(dside, x1, y1))
			  return TRUE;
			/* Always show temperature around edge of known area. */
			if (terrain_view(dside, x1, y1) == UNSEEN)
			  return TRUE;
		}
	}
	return (x % 2 == 0 && y % 2 == 0);
}

/* Indicate the temperature of the given location, textually for now. */

void
draw_temperature(int sx, int sy, int power, int temp)
{
	sx += hws[power] / 2;  sy += hhs[power] / 2;
	if (1 /* draw as text */) {
		sprintf(spbuf, "%d¡", temp);  /* (should do char more portably) */
		draw_legend_text(sx, sy, hhs[power] / 2, spbuf, 0);
	}
	/* (Also draw isotherms eventually) */
}

int
draw_clouds_here(int x, int y)
{
	/* Draw clouds in every cell always. */
	return TRUE;
}

void
draw_clouds(int sx, int sy, int power, int cloudtype)
{
	Rect tmprect;

	/* Don't draw clear sky. */
	if (cloudtype == 0)
	  return;
	sx += hws[power] / 2;  sy += hhs[power] / 2;
	SetRect(&tmprect, sx - hws[power] / 2, sy - hhs[power] / 2, sx + hws[power] / 2, sy + hhs[power] / 2);
	/* Should use pat# 130 patterns for this instead. */
	PenPat(cloudtype >= 3 ? QDPat(dkGray) : (cloudtype == 2 ? QDPat(gray) : QDPat(ltGray)));
	PenMode(patBic);
	PaintOval(&tmprect);
	PenNormal();
}

/* Don't draw the winds in every cell, only do ones with odd coords or
   ones where the wind in any adjacent cell is different. */
/* (generic routine?) */

int
draw_winds_here(int x, int y)
{
	int dir, x1, y1, windhere = wind_view(dside, x, y);

#ifdef DESIGNERS
	/* Designers should see wind in every cell. */
	if (dside->designer)
	  return TRUE;
#endif /* DESIGNERS */
	for_all_directions(dir) {
		if (interior_point_in_dir(x, y, dir, &x1, &y1)) {
			if (windhere != wind_view(dside, x1, y1))
			  return TRUE;
			/* Always show wind around edge of known area. */
			if (terrain_view(dside, x1, y1) == UNSEEN)
			  return TRUE;
		}
	}
	return (x % 2 == 1 && y % 2 == 1);
}

/* Indicate the given wind at the given location. */

void
draw_winds(int sx, int sy, int power, int rawwind)
{
	int i, wdir = wind_dir(rawwind), wforce = wind_force(rawwind);
	GrafPtr curport;

	sx += (hws[power] - 16) / 2;  sy += (hhs[power] - 16) / 2;
	if (wforce < 0) {
		DGprintf("negative wind force %d, substituting 0", wforce);
		wforce = 0;
	}
	/* (should use overall max/mins to compute scaling factor) */
	if (wforce > 4)
	  wforce = 4;
	if (wforce == 0)
	  wdir = 0;
	/* Collect sicns if not already in. */
	if (numwindsicns == 0) {
		for (i = 0; i <= 4; ++i) {
			windsicnhandle[i] = GetResource('SICN', sicnWinds0 + i);
		}
		numwindsicns = 5;
	}
	GetPort(&curport);
	/* Draw an offset white version, for contrast. */
	plot_sicn(curport, sx + 1, sy + 1, windsicnhandle[wforce], wdir, FALSE, srcBic);
	plot_sicn(curport, sx, sy, windsicnhandle[wforce], wdir, FALSE, srcOr);
}

/* Draw the view coverage of a cell (for debugging). */

void
draw_coverage(int sx, int sy, int power, int cov, int altcov)
{
	char buf[40];

	buf[0] = '\0';
	if (cov > 0) {
		tprintf(buf, ":%d:", cov);
	}
	if (altcov > -1) {
		tprintf(buf, ",%d:", altcov);
	}
	if (buf[0] != '\0') {
		/* Adjust to the lower left corner of the cell. */
		sx += 2;  sy += hcs[power] - 2;
		draw_legend_text(sx, sy, hhs[power] / 2, buf, -1);
	}
}

/* Draw a unit's name, if it has one. */

void
draw_unit_name(Unit *unit, int sx, int sy, int sw, int sh)
{
	if (unit->name) {
		draw_legend_text(sx + sw + 1, sy + sh/2, sh, unit->name, -1);
	}
}

void
draw_legend_text(int sx, int sy, int sh, char *legend, int just)
{
	int strwid, strleft;
	Rect maskrect;
	FontInfo fontinfo;
	
	/* Scale text sizes to fit in smaller cells if necessary. */
	TextSize(min(max(5, sh), 10));
	strwid = TextWidth(legend, 0, strlen(legend));
	if (just < 0) {
		strleft = sx;
	} else if (just > 0) {
		strleft = sx - strwid;
	} else {
		strleft = sx - strwid / 2;
	}
	MoveTo(strleft, sy);
	if (0) {
		/* Make it readable against a noisy background. */
/*		TextFace(bold|outline); */
		TextMode(srcBic);
	} else {
		/* This makes a big white box for name, less attractive but easier to read. */
		GetFontInfo(&fontinfo);
		maskrect.top = sy - fontinfo.ascent;
		/* Ensure a one pixel strip of white all along the top. */
		maskrect.top += 1;
		maskrect.left = strleft - 1;
		maskrect.bottom = sy + fontinfo.descent;
		/* Ensure a one pixel strip of white all along the bottom, even below descenders. */
		maskrect.bottom += 1;
		maskrect.right = maskrect.left + strwid + 1;
		FillRect(&maskrect, QDPat(white));
	}
	DrawText(legend, 0, strlen(legend));
}


void
draw_blast_image(WindowPtr win, int sx, int sy, int sw, int sh, int blasttype)
{
	int i;
	Rect tmprect;
	Rect srcrect, imagerect;
	BitMap bm, *winbits;
	GrafPtr curport;

	/* Collect sicns if not already in. */
	if (numblastsicns == 0) {
		for (i = 0; i <= 2; ++i) {
			blastsicnhandle[i] = GetResource('SICN', sicnMiss + i);
			++numblastsicns;
		}
	}
	SetRect(&tmprect, sx, sy, sx + sw, sy + sh);
	if (sw >= 16) {
		winbits = &(((GrafPtr) win)->portBits);
		imagerect.top = sy;  imagerect.left = sx;
		imagerect.bottom = imagerect.top + sh;  imagerect.right = imagerect.left + sw;
		/* should save image under here, then draw blast */
		GetPort(&curport);
		SetRect(&srcrect, 0, 0, 16, 16);
		bm.rowBytes = 2;
		bm.bounds = srcrect;
		bm.baseAddr = *(blastsicnhandle[blasttype]);
		if (hasColorQD) {
			RGBColor tmpcolor;

			OffsetRect(&imagerect, 1, 1);
			CopyBits(&bm, winbits, &srcrect, &imagerect, srcOr, nil);
			tmpcolor.red = 60000;  tmpcolor.green = tmpcolor.blue = 0;
			RGBForeColor(&tmpcolor);
			OffsetRect(&imagerect, -1, -1);
			CopyBits(&bm, winbits, &srcrect, &imagerect, srcOr, nil);
			/* Restore the previous color. */
			tmpcolor.red = tmpcolor.green = tmpcolor.blue = 0;
			RGBForeColor(&tmpcolor);
		} else {
			OffsetRect(&imagerect, 1, 1);
			CopyBits(&bm, winbits, &srcrect, &imagerect, srcBic, nil);
			OffsetRect(&imagerect, -1, -1);
			CopyBits(&bm, winbits, &srcrect, &imagerect, srcOr, nil);
		}
	} else {
		InvertRect(&tmprect);
	}
}

void
clear_blast_image(WindowPtr win, int sx, int sy, int sw, int sh, int blasttype)
{
	Rect tmprect;

	SetRect(&tmprect, sx, sy, sx + sw, sy + sh);
	if (sw >= 16) {
		/* should restore image under here */
	} else {
		InvertRect(&tmprect);
    }
}

int
picture_width(PicHandle pichandle)
{
	return ((*pichandle)->picFrame.right - (*pichandle)->picFrame.left);
}

int
picture_height(PicHandle pichandle)
{
	return ((*pichandle)->picFrame.bottom - (*pichandle)->picFrame.top);
}

/* Generic sicn drawer. */

void
plot_sicn(WindowPtr win, int sx, int sy, Handle sicnhandle, int n, int erase, int mode)
{
	Rect srcrect, imagerect;
	BitMap bm, *winbits;

	if (sicnhandle == nil)
	  return;
	imagerect.left = sx;  imagerect.top = sy;
	imagerect.right = imagerect.left + 16;  imagerect.bottom = imagerect.top + 16;
	winbits = &(((GrafPtr) win)->portBits);
	SetRect(&srcrect, 0, 0, 16, 16);
	bm.rowBytes = 2;
	bm.bounds = srcrect;
	if (erase)
	  EraseRect(&imagerect);
	bm.baseAddr = *(sicnhandle) + 32 * n;
	CopyBits(&bm, winbits, &srcrect, &imagerect, mode, nil);
}

/* Given a rectangle, make half of its pixels white. */

void
gray_out_rect(Rect *rectptr)
{
	PenPat(QDPat(gray));
	PenMode(patBic);
	PaintRect(rectptr);
	PenNormal();
}
