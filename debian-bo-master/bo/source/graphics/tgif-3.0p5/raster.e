/*
 * Author:      William Chia-Wei Cheng (william@cs.ucla.edu)
 *
 * Copyright (C) 1990-1996, William Chia-Wei Cheng.
 *
 * Permission limited to the use, copy, display, distribute without
 * charging for a fee, and produce derivative works of "tgif" and
 * its documentation for not-for-profit purpose is hereby granted by
 * the Author, provided that the above copyright notice appears in
 * all copies made of "tgif" and that both the copyright notice
 * and this permission notice appear in supporting documentation,
 * and that the name of the Author not be used in advertising or
 * publicity pertaining to distribution of the software without
 * specific, written prior permission.  The Author makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.  All other rights (including, but not limited to, the
 * right to sell "tgif", the right to sell derivative works of
 * "tgif", and the right to distribute "tgif" for a fee) are
 * reserved by the Author.
 *
 * THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT
 * OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/raster.e,v 3.0 1996/05/06 16:07:04 william Exp $
 */

#ifndef _RASTER_E_
#define _RASTER_E_

extern GC	rasterGC;

extern Pixmap	patPixmap[];
extern GC	patGC;

extern unsigned long	xorOne;
extern unsigned long	xorZero;

extern GC	drawGC;
extern GC	defaultGC;
extern GC	revDefaultGC;
extern GC	revGrayGC;

extern Pixmap	choicePixmap[];
extern Pixmap	patPixmap[];
extern Pixmap	* lineWidthPixmap;
extern Pixmap	lineTypePixmap[];
extern Pixmap	dashPixmap[];
extern Pixmap	lineStylePixmap[];
extern Pixmap	justPixmap[];
extern Pixmap	alignHoriPixmap[];
extern Pixmap	alignVertPixmap[];
extern Pixmap	filePixmap;
extern Pixmap	landscapePixmap;
extern Pixmap	specialPixmap;
extern Pixmap	vspacePixmap;
extern Pixmap	rcbRadiusPixmap;
extern Pixmap	moveModePixmap[];
extern Pixmap	editPixmap;
extern Pixmap	intrPixmap[];
extern Pixmap	trekPixmap;
extern Pixmap	pageLayoutPixmap[];
extern Pixmap	scrollPixmap[];
extern Pixmap	shapePixmap[];
extern Pixmap	stretchableModePixmap[];

extern Pixmap	rotatePixmap[];
extern Pixmap	whereToPrintPixmap[];
extern Pixmap	* shortLineWidthPixmap;
extern Pixmap	shortLineStylePixmap[];
extern Pixmap	shortLineTypePixmap[];
extern Pixmap	shortDashPixmap[];

extern Pixmap	statusBtnPixmap[];
extern Pixmap	msgBoxPixmap[];

extern int	maxLineWidths;

extern int	dashListLength[];
extern char	*dashList[];

extern void	InitPattern ARGS_DECL((void));
extern void	RedrawChoiceWindow ARGS_DECL((void));
extern void	CleanUpRasters ARGS_DECL((void));
extern int	UpgradePenFill ARGS_DECL((int));

#endif /*_RASTER_E_*/
