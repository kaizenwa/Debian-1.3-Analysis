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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/ruler.e,v 3.0 1996/05/06 16:07:20 william Exp $
 */

#ifndef _RULER_E_
#define _RULER_E_

extern int	showMeasurement;
extern int	showCrossHair;

extern void	GetUnitSpec ARGS_DECL((char*));
extern void	ShowUnitMsg ARGS_DECL((void));
extern int	SetUnit ARGS_DECL((char*));
extern void	InitRuler ARGS_DECL((void));
extern void	CleanUpRuler ARGS_DECL((void));
extern void	RedrawHRuler ARGS_DECL((void));
extern void	RedrawVRuler ARGS_DECL((void));
extern void	MarkRulers ARGS_DECL((int XOff, int YOff));
extern void	RedrawRulers ARGS_DECL((void));
extern void	GetCrossHairPosition ARGS_DECL((int *XOff, int *YOff,
		                                int *Shown));
extern void	RedrawCrossHair ARGS_DECL((void));
extern void	ToggleShowCrossHair ARGS_DECL((void));
extern void	RulersEventHandler ARGS_DECL((XEvent *));
extern void	FreezeMarkRulerText ARGS_DECL((void));
extern void	UnFreezeMarkRulerText ARGS_DECL((void));
extern void	BeginIntervalRulers ARGS_DECL((int LtX, int LtY, int RbX,
		                               int RbY));
extern void	DrawIntervalRulers ARGS_DECL((int LtX, int LtY, int RbX,
		                              int RbY));
extern void	EndIntervalRulers ARGS_DECL((int X, int Y));
extern void	PixelToMeasurementUnit ARGS_DECL((char *Buf, int NumPixels));
extern void	StartShowMeasureCursor ARGS_DECL((int XOff, int YOff,
                                                  char *Str, int ExtraSpace));
extern void	ShowMeasureCursor ARGS_DECL((int XOff, int YOff, char *Str,
                                             int ExtraSpace));
extern void	EndShowMeasureCursor ARGS_DECL((int XOff, int YOff, char *Str,
                                                int ExtraSpace));
extern void	ToggleShowMeasurement ARGS_DECL((void));

#endif /*_RULER_E_*/
