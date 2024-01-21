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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/stretch.e,v 3.0 1996/05/06 16:12:03 william Exp $
 */

#ifndef _STRETCH_E_
#define _STRETCH_E_

extern int	stretchableText;
extern int	rotationIncrement;

extern int	PtInPolyMark ARGS_DECL((struct ObjRec *, int XOff, int YOff,
		                        int NumPts, IntPoint *, int *Index));
extern int	RetractedArrowAttr ARGS_DECL((struct ObjRec *));
extern int	AutoRetractedArrowAttr ARGS_DECL((struct ObjRec *,
		                                  int CheckVCount));
extern struct AttrRec	* NotifyResizeAttr ARGS_DECL((struct ObjRec *));
extern int	NotifyResize ARGS_DECL((struct ObjRec *, struct AttrRec *));
extern int	AutoCenterAttr ARGS_DECL((struct ObjRec *));
extern void	CenterObjInOBBox ARGS_DECL((struct ObjRec *TextObjPtr,
                                            struct BBRec OBBox,
                                            struct BBRec *OBBoxReturn));
extern struct SelRec	* PtInSelMark ARGS_DECL((int XOff, int YOff,
			                         int * Corner));
extern void	ShearObj ARGS_DECL((struct ObjRec *, int Corner, int XShear,
		                    int YShear, int XScale, int YScale,
		                    int *RealLtX, int *RealLtY));
extern void	StretchSel ARGS_DECL((int XGridOff, int YGridOff,
		                      struct ObjRec *, int Corner));
extern void	ScaleAnEPSObj ARGS_DECL((struct ObjRec *, float * Scale));
extern void	ScaleAllSelObj ARGS_DECL((void));
extern void	SizeAllSelObj ARGS_DECL((int AbsW, int AbsH));
extern void	FlipObjHorizontal ARGS_DECL((struct ObjRec *));
extern void	FlipIconHorizontal ARGS_DECL((struct ObjRec *));
extern void	FlipObjVertical ARGS_DECL((struct ObjRec *));
extern void	FlipIconVertical ARGS_DECL((struct ObjRec *));

extern void	SetRotatePivot ARGS_DECL((void));
extern void	SetRotatePivotByObject ARGS_DECL((struct ObjRec *));
extern void	RotateObjForLayout ARGS_DECL((struct ObjRec *, double, int));
extern void	RotateObj ARGS_DECL((struct ObjRec *, int Corner,
		                     int AngleDelta, int *RealLtX,
		                     int *RealLtY));
extern void	RotateObjClockWise ARGS_DECL((struct ObjRec *));
extern void	RotateIconClockWise ARGS_DECL((struct ObjRec *));
extern void	RotateObjCounter ARGS_DECL((struct ObjRec *));
extern void	RotateIconCounter ARGS_DECL((struct ObjRec *));
extern void	FlipHorizontal ARGS_DECL((void));
extern void	FlipVertical ARGS_DECL((void));
extern void	RotateClockWise ARGS_DECL((void));
extern void	RotateCounter ARGS_DECL((void));
extern void	SetTextRotation ARGS_DECL((void));
extern void	SetRotationIncrement ARGS_DECL((void));
extern void	RotateShearSel ARGS_DECL((int XGridOff, int YGridOff,
		                          struct ObjRec *, int Corner));

#endif /*_STRETCH_E_*/
