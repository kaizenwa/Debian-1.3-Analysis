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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/drawing.e,v 3.2 1996/05/15 17:33:10 william Exp $
 */

#ifndef _DRAWING_E_
#define _DRAWING_E_

extern int	intrCheckInterval;
extern int	pasteInDrawTextMode;
extern int	pasteFromFileInDrawTextMode;
extern char	pasteFromFileName[];
extern int	copyInDrawTextMode;
extern int	numRedrawBBox;
extern int	numClipRecs;
extern int	clipOrdering;
extern XRectangle	clipRecs[];
extern int	checkBBox;

extern void	SetDefaultDrawWinClipRecs ARGS_DECL((void));
extern void	SetDefaultIconWinClipRecs ARGS_DECL((void));
extern void	ShowInterrupt ARGS_DECL((int));
extern int	HideInterrupt ARGS_DECL((void));
extern void	RedrawDummyWindow1 ARGS_DECL((void));
extern void	DummiesEventHandler ARGS_DECL((XEvent *));
extern int	ESCPressed ARGS_DECL((void));
extern int	CheckInterrupt ARGS_DECL((void));
extern void	DrawClippedPixmap ARGS_DECL((Pixmap, Window, GC, int PixmapW,
		                             int PixmapH, int LtX, int LtY));
extern void	FillClippedRectangle ARGS_DECL((Window, GC, int LtX, int LtY,
		                                int OrigW, int OrigH));
extern int	ObjInVisibleLayer ARGS_DECL((struct ObjRec *));
extern int	DrawObj ARGS_DECL((Window, struct ObjRec *));
extern void	DrawPaperBoundary ARGS_DECL((Window));
extern void	RedrawAnArea ARGS_DECL((struct ObjRec *BotObj, int LtX,
		                        int LtY, int RbX, int RbY));
extern void	RedrawAreas ARGS_DECL((struct ObjRec *BotObj, int LtX1,
		                        int LtY1, int RbX1, int RbY1,
		                        int LtX2, int LtY2, int RbX2,
		                        int RbY2));
extern Pixmap	DrawAllOnPixmap ARGS_DECL((int *LtX, int *LtY, int *W, int *H));
extern void	RedrawDrawWindow ARGS_DECL((struct ObjRec *BotObj));
extern void	ClearAndRedrawDrawWindow ARGS_DECL((void));
extern void	ClearAndRedrawDrawWindowNoCurT ARGS_DECL((void));
                /* use to be ClearAndRedrawDrawWindowDontDrawCurText */
extern int	BeginExecAnimate ARGS_DECL((void));
extern void	EndExecAnimate ARGS_DECL((void));
extern void	CleanUpDrawingWindow ARGS_DECL((void));
extern int	ShortHand ARGS_DECL((XEvent *));
extern int	CallShortCut ARGS_DECL((char *Name, int argc, char *argv[],
		                        char *Code, unsigned int State));
extern int	SomethingDirty ARGS_DECL((void));
extern int	DrawingEventHandler ARGS_DECL((XEvent *));

#endif /*_DRAWING_E_*/
