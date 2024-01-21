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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/auxtext.e,v 3.0 1996/05/06 16:03:51 william Exp $
 */

#ifndef _AUXTEXT_E_
#define _AUXTEXT_E_

extern GC	rotateGC;
extern Pixmap	textBackingPixmap;
extern int	textBackingPixmapW;
extern int	textBackingPixmapH;
extern int	doubleQuoteDoubleQuote;
extern int	groupedTextEditable;

extern struct SelRec	* outerSel, * innerSel;

extern void	GetStrSizeInfo ARGS_DECL((struct StrRec *, int *w,
		                          int *lbearing, int *rbearing));
extern void	GetTextObjSizeInfo ARGS_DECL((struct TextRec *, int *num_lines,
		                              int *max_w, int *min_lbearing,
		                              int *max_rbearing));
extern void	TranslateKeys ARGS_DECL((char *, KeySym *));
extern void	SetTextOBBox ARGS_DECL((struct ObjRec *, int Just,
		                        int W, int H, int LBearing,
		                        int RightExtra, int Rotate));
extern void	SetTextBBox ARGS_DECL((struct ObjRec *, int Just,
		                       int W, int H, int LBearing,
		                       int RightExtra, int Rotate));
extern void	SetTextOrigBBoxes ARGS_DECL((struct ObjRec *, int Just,
		                             int W, int H, int LBearing,
		                             int RightExtra, int Rotate));
extern int	UpdTextBBox ARGS_DECL((struct ObjRec *));
extern int	PixelOnOff ARGS_DECL((XImage *, int Col, int Row, int Scale));
extern void	MakeCachedTextBitmap ARGS_DECL((struct ObjRec *));
extern struct ObjRec	* FindTextObj ARGS_DECL((int XOff, int YOff,
			                         struct ObjRec *ObjToBeFound));
extern void	UnlinkCurTextFromInnerSel ARGS_DECL((void));
extern void	AdjAncestorsBBox ARGS_DECL((void));
extern void	DumpTextObj ARGS_DECL((FILE *, struct ObjRec *));
extern int	NeedsToCacheTextObj ARGS_DECL((struct ObjRec *));
extern void	SaveString ARGS_DECL((FILE *, char *));
extern void	SaveTextObj ARGS_DECL((FILE *, struct ObjRec *));
extern char	* ReadString ARGS_DECL((char *));
extern struct ObjRec	* FormTextObjectFromFile ARGS_DECL((FILE *, int AbsX,
			                                    int AbsY));
extern void	RepaintFirstStr ARGS_DECL((struct ObjRec *, char *));

#endif /*_AUXTEXT_E_*/
