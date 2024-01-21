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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/spline.e,v 3.0 1996/05/06 16:11:54 william Exp $
 */

#ifndef _SPLINE_E_
#define _SPLINE_E_

extern int	intSplineTension;
extern int	splineTol;
extern int	splineRubberband;

extern void	CalcAutoRetractedArrowAttrBend ARGS_DECL((int Style,
		                                          int X0, int Y0,
		                                          int X2, int Y2,
		                                          int *X1, int *Y1));
extern void	Spline ARGS_DECL((Window, int Pixel, int Func, double X1,
		                  double Y1, double X2, double Y2,
		                  double X3, double Y3, double X4, double Y4));
extern XPoint	* MakeSplinePolyVertex ARGS_DECL((int *N,
		                                  int XOff, int YOff,
		                                  int NumVs, IntPoint *));
extern XPoint	* MakeMultiSplinePolyVertex ARGS_DECL((int *N, char *Smooth,
		                                       int XOff, int YOff,
		                                       int NumVs, IntPoint *));
extern XPoint	* MakeSplinePolygonVertex ARGS_DECL((int *N,
		                                     int XOff, int YOff,
		                                     int NumVs, IntPoint *));
extern XPoint	* MakeMultiSplinePolygonVertex ARGS_DECL((int *N, char *Smooth,
		                                          int XOff, int YOff,
		                                          int NumVs,
		                                          IntPoint*));
extern XPoint	* MakeIntSplinePolyVertex ARGS_DECL((int *N, int *CntrlN,
		                                     IntPoint **CntrlVs,
		                                     int XOff, int YOff,
		                                     int NumVs, IntPoint *));
extern XPoint	* MakeIntSplinePolygonVertex ARGS_DECL((int *N, int *CntrlN,
		                                        IntPoint **CntrlVs,
		                                        int XOff, int YOff,
		                                        int NumVs, IntPoint*));
extern void	DumpCurvedPolyPoints ARGS_DECL((FILE*, int NumPts,
		                                IntPoint*, int Indent));
extern void	DumpCurvedPolygonPoints ARGS_DECL((FILE*, int NumPts,
		                                   IntPoint*, int Indent));
extern void	DumpMultiCurvedPolyPoints ARGS_DECL((FILE*, char *Smooth,
		                                     int Style, int Curved,
		                                     int NumPts, IntPoint*,
		                                     int Indent));
extern void	DumpMultiCurvedPolygonPoints ARGS_DECL((FILE*, char *Smooth,
		                                        int Curved, int NumPts,
		                                        IntPoint*, int Indent));

#endif /*_SPLINE_E_*/
