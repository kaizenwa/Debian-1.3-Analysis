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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/arc.e,v 3.0 1996/05/06 16:03:42 william Exp $
 */

#ifndef _ARC_E_
#define _ARC_E_

extern int	arcDrawn;

extern void	PointsToShearScale ARGS_DECL((int Corner,
		                              int x_pivot, int y_pivot,
		                              int x_move, int y_move,
		                              int x_current, int y_current,
		                              int * x_sheer, int * y_sheer,
		                              int * x_scale, int * y_scale));
extern void	PointsToArc ARGS_DECL((int xc, int yc, int x1, int y1,
		                       int x2, int y2, int dir, int int_degree,
		                       int *ltx, int *lty, int *w, int *h,
		                       int *angle1, int *angle2));
extern void	ArcRealX2Y2 ARGS_DECL((struct ArcRec *, int *X2, int *Y2));
extern void	GetArcArrowInfo ARGS_DECL((struct ObjRec *,
		                           IntPoint *tipvs1, IntPoint *tailvs1,
		                           IntPoint *vs1, int *a_angle1,
		                           IntPoint *tipvs2, IntPoint *tailvs2,
		                           IntPoint *vs2, int *a_angle2));
extern void	CalcArcOBBox ARGS_DECL((struct ObjRec *));
extern void	CalcArcBBox ARGS_DECL((struct ObjRec *, struct BBRec obbox,
		                       struct BBRec *bbox));
extern void	DumpArcObj ARGS_DECL((FILE *, struct ObjRec *));
extern int	NeedsToCacheArcObj ARGS_DECL((struct ObjRec *));
extern void	DrawArcObj ARGS_DECL((Window, int X, int Y, struct ObjRec *));
extern struct ObjRec	*CreateArcObj ARGS_DECL((int xc, int yc, int x1, int y1,
			                         int x2, int y2, int dir,
			                         int ltx, int lty, int w, int h,
			                         int angle1, int angle2));
extern void	DrawArc ARGS_DECL((XEvent *));
extern void	SaveArcObj ARGS_DECL((FILE *, struct ObjRec *));
extern void	ReadArcObj ARGS_DECL((FILE *, char *, struct ObjRec **));
extern void	FreeArcObj ARGS_DECL((struct ObjRec *));
extern void	MakePreciseArc ARGS_DECL((void));
extern void	PreciseRotateAnArc ARGS_DECL((void));

#endif /*_ARC_E_*/
