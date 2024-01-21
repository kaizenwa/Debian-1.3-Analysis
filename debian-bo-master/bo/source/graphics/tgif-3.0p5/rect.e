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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/rect.e,v 3.0 1996/05/06 16:07:12 william Exp $
 */

#ifndef _RECT_E_
#define _RECT_E_

extern void	SetRotateVs ARGS_DECL((XPoint *, int LtX, int LtY, int RbX,
		                       int RbY));
extern void	SetBBRec ARGS_DECL((struct BBRec *, int LtX, int LtY, int RbX,
		                    int RbY));
extern void	ConcatCTM ARGS_DECL((struct XfrmMtrxRec *ctm,
		                     struct XfrmMtrxRec *orig_ctm,
		                     struct XfrmMtrxRec *new_ctm));
extern void	TransformPointThroughCTM ARGS_DECL((int X, int Y,
		                                    struct XfrmMtrxRec *,
		                                    int *NewX, int *NewY));
extern void	ReverseTransformPointThroughCTM ARGS_DECL((int X, int Y,
		                                           struct XfrmMtrxRec *,
		                                           int *NewX,
		                                           int *NewY));
extern void	TransformObjectV ARGS_DECL((struct ObjRec *,
		                            IntPoint *VIn, IntPoint *VOut));
extern void	ReverseTransformObjectV ARGS_DECL((struct ObjRec *,
		                                   IntPoint *VIn,
		                                   IntPoint *VOut));
extern void	TransformOffsetBBoxThroughCTM ARGS_DECL((struct BBRec *,
		                                         struct XfrmMtrxRec *,
		                                         IntPoint *));
extern void	GetTransformedOBBoxOffsetVs ARGS_DECL((struct ObjRec *,
		                                       XPoint *));
extern void	GetTransformedOBBoxAbsVs ARGS_DECL((struct ObjRec *,
		                                    IntPoint *));
extern void	SetCTM ARGS_DECL((struct ObjRec *, struct XfrmMtrxRec *));
extern int	IntersectRect ARGS_DECL((struct BBRec BBox1, struct BBRec BBox2,
		                         struct BBRec *BBox3));
extern int	Inside ARGS_DECL((struct BBRec Rect1, struct BBRec Rect2));
extern int	BBoxIntersect ARGS_DECL((struct BBRec BBox1,
		                         struct BBRec BBox2));
extern int	PointInBBox ARGS_DECL((int X, int Y, struct BBRec Rect));
extern int	PointInPolygon ARGS_DECL((int X, int Y, int NumPts, XPoint *));
extern int	PointInPoly ARGS_DECL((int X, int Y, int NumPts, XPoint *,
		                       int LineWidth));
extern int	PointOnPoly ARGS_DECL((int X, int Y, int NumPts, XPoint *,
		                       int LineWidth));
extern int	FindGoodText ARGS_DECL((int XOff, int YOff, struct ObjRec *));
extern int	FindGoodXBm ARGS_DECL((int XOff, int YOff, struct ObjRec *));
extern int	FindGoodXPm ARGS_DECL((int XOff, int YOff, struct ObjRec *));
extern int	FindGoodBox ARGS_DECL((int XOff, int YOff, struct ObjRec *));
extern int	FindGoodRCBox ARGS_DECL((int XOff, int YOff, struct ObjRec *));
extern int	FindGoodOval ARGS_DECL((int XOff, int YOff, struct ObjRec *));
extern int	FindGoodPoly ARGS_DECL((int XOff, int YOff, struct ObjRec *));
extern int	FindGoodPolygon ARGS_DECL((int XOff, int YOff,
		                           struct ObjRec *));
extern int	FindGoodArc ARGS_DECL((int XOff, int YOff, struct ObjRec *));
extern int	FindGoodObj ARGS_DECL((int XOff, int YOff, struct ObjRec *First,
		                       struct ObjRec **SubObj,
		                       struct ObjRec **ImmediateChildObj));

#endif /*_RECT_E_*/
