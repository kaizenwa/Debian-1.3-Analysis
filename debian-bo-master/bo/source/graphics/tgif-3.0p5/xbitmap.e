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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/xbitmap.e,v 3.0 1996/05/06 16:13:06 william Exp $
 */

#ifndef _XBITMAP_E_
#define _XBITMAP_E_

extern GC	xbmGC;
extern int	askForXBmSpec;
extern int	stripEPSComments;
extern int	saveEPSLines;
extern int	leftExportPixelTrim;
extern int	topExportPixelTrim;
extern int	rightExportPixelTrim;
extern int	bottomExportPixelTrim;
extern Pixmap	dummyBitmap;

extern void	SetExportPixelTrim ARGS_DECL((char*));
extern void	InitXBm ARGS_DECL((void));
extern void	CleanUpXBm ARGS_DECL((void));
extern void	CalcTransform ARGS_DECL((struct MtrxRec *));
extern void	MakeCachedBitmap ARGS_DECL((struct ObjRec *));
extern int	ExtractBitmap ARGS_DECL((Pixmap OrigBitmap, XImage *OrigImage,
		                         int X, int Y, int W, int H,
		                         Pixmap *Bitmap, XImage **Image));
extern void	InvertXBitmaps ARGS_DECL((void));
extern void	ParseCutSpec ARGS_DECL((char *Spec, int ImageW, int ImageH,
		                        float *Mag, int *SrcX, int *SrcY,
		                        int *SrcW, int *SrcH));
extern void	CutXBitmap ARGS_DECL((void));
extern void	BreakUpXBitmap ARGS_DECL((struct ObjRec *, int ColsAndRows,
		                          int W, int H));
extern void	GenPreviewBitmap ARGS_DECL((FILE *, int llxPage, int llyPage,
		                            int urxPage, int uryPage));
extern void	DumpXBitmapFile ARGS_DECL((void));
extern void	DumpXBmObj ARGS_DECL((FILE *, struct ObjRec *));
extern int	NeedsToCacheXBmObj ARGS_DECL((struct ObjRec *));
extern void	DrawXBmObj ARGS_DECL((Window, int XOff, int YOff,
		                      struct ObjRec *));
extern struct ObjRec	* CreateXBmObj ARGS_DECL((int ImageW, int ImageH, int W,
			                          int H, Pixmap, XImage *));
extern void	SaveXBmObj ARGS_DECL((FILE *, struct ObjRec *));
extern void	ReadXBmObj ARGS_DECL((FILE *, char *Inbuf, struct ObjRec **));
extern void	FreeXBmObj ARGS_DECL((struct ObjRec *));

#endif /*_XBITMAP_E_*/
