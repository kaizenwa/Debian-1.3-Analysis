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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/pattern.e,v 3.0 1996/05/06 16:06:41 william Exp $
 */

#ifndef _PATTERN_E_
#define _PATTERN_E_

extern int	objFill;
extern int	lineStyle;
extern int	lineWidth;
extern int	penPat;
extern int	curSpline;
extern int	curDash;
extern int	rcbRadius;
extern int	useGray;
extern char	patternStr[];

extern int	stickyMenuSelection;

extern void	ResetGrayDetection ARGS_DECL((void));
extern char	* GrayStr ARGS_DECL((int Index));
extern void	GrayCheck ARGS_DECL((int Index));
extern void	EndGrayDetection ARGS_DECL((void));

extern int	ModeMenu ARGS_DECL((int X, int Y, int TrackMenubar));
extern void	ChangeAllSelFill ARGS_DECL((int Index, int HighLight));
extern int	FillMenu ARGS_DECL((int X, int Y, int TrackMenubar));
extern void	ChangeAllSelLineStyle ARGS_DECL((int Index, int HighLight));
extern void	ChangeAllSelLineType ARGS_DECL((int Index, int HighLight));
extern void	ChangeAllSelLineWidth ARGS_DECL((int Index, int HighLight));
extern void	ChangeAllSelDashes ARGS_DECL((int Index, int HighLight));
extern int	LineWidthMenu ARGS_DECL((int X, int Y, int TrackMenubar));
extern int	LineStyleMenu ARGS_DECL((int X, int Y, int TrackMenubar));
extern int	LineTypeMenu ARGS_DECL((int X, int Y, int TrackMenubar));
extern int	LineDashMenu ARGS_DECL((int X, int Y, int TrackMenubar));
extern void	ChangeAllSelPen ARGS_DECL((int Index, int HighLight));
extern int	PenMenu ARGS_DECL((int X, int Y, int TrackMenubar));
extern void	ToggleAllSelLineType ARGS_DECL((void));
extern void	ChangeAllSelRCBRadius ARGS_DECL((int Radius));
extern void	UpdateSelObjs ARGS_DECL((void));
extern void     ChangeAllSelRealLineWidth ARGS_DECL((int Width, int AW, int AH,
		                                     char*, char*, char*,
		                                     int HighLight));
extern void     SetSelectedLineWidth ARGS_DECL((void));

#endif /*_PATTERN_E_*/
