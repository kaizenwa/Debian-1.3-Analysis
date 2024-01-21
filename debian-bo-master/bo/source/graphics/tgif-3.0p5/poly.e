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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/poly.e,v 3.0 1996/05/06 16:06:48 william Exp $
 */

#ifndef _POLY_E_
#define _POLY_E_

extern int	polyDrawn;

extern short	origWidthOfLine[];
extern short	origArrowHeadH[];
extern short	origArrowHeadW[];

extern short	*curWidthOfLine;
extern short	*curArrowHeadH;
extern short	*curArrowHeadW;

extern char	**curWidthOfLineSpec;
extern char	**curArrowHeadHSpec;
extern char	**curArrowHeadWSpec;

extern int	drawPolyToEndInANode;
extern char	drawPolyFirstNodeName[];
extern char	drawPolyLastNodeName[];

extern struct ObjRec	* drawPolyHighlightedNode;

extern XPoint	* MakePolyVertex ARGS_DECL((int XOff, int YOff, int NumVs,
		                            IntPoint *));
extern void	CalcPolyBBox ARGS_DECL((struct ObjRec *));
extern void	UpdPolyBBox ARGS_DECL((struct ObjRec *, int NumPts,
		                       IntPoint *));
extern void	CreatePolyObj ARGS_DECL((int NumPts, int CreateAbsolute));
extern void	ResetCreatePoly ARGS_DECL((void));
extern void	AddPtToCreatePoly ARGS_DECL((int AbsX, int AbsY));
extern void	DrawPoly ARGS_DECL((XEvent *));
extern void	InputPolyPts ARGS_DECL((void));
extern void	JoinPoly ARGS_DECL((void));
extern void	CutPoly ARGS_DECL((void));
extern void	DumpArrow ARGS_DECL((FILE*, IntPoint *TailV, IntPoint *HeadV,
		                     int ArrowW, int ArrowH, char *AWSpec,
		                     char *AHSpec, int Pen, int ColorIndex));
extern void	DumpPolyObj ARGS_DECL((FILE *, struct ObjRec *));
extern int	NeedsToCachePolyObj ARGS_DECL((struct ObjRec *));
extern void	DrawPolyObj ARGS_DECL((Window, int XOff, int YOff,
		                       struct ObjRec *));
extern void	SaveSmoothHinge ARGS_DECL((FILE *, int Curved, int NumPts,
		                           char *Smooth));
extern void	SavePolyObj ARGS_DECL((FILE *, struct ObjRec *));
extern int	ReadSmoothHinge ARGS_DECL((FILE *, int Curved, int NumPts,
		                           char *Smooth));
extern void	ReadPolyObj ARGS_DECL((FILE *, char *Inbuf, struct ObjRec **));
extern void	FreePolyObj ARGS_DECL((struct ObjRec *));

#endif /*_POLY_E_*/
