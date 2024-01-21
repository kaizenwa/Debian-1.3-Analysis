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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/grid.e,v 3.0 1996/05/06 16:05:23 william Exp $
 */

#ifndef _GRID_E_
#define _GRID_E_

extern int	gridSystem;
extern int	gridOn;
extern int	xyEnglishGrid;
extern int	xyMetricGrid;
extern int	pageStyle;
extern int	whereToPrint;
extern int	moveMode;
extern int	gridShown;
extern int	mapShown;
extern int	usePaperSizeStoredInFile;
extern int	oneMotionSelectMove;
extern int	queryZoomInPoint;

extern char	* layoutMenuStr[];

extern void	MyHDotLine ARGS_DECL((Window, int Y, int XStart, int XEnd));
extern void	MyVDotLine ARGS_DECL((Window, int X, int YStart, int YEnd));
extern void	RedrawGridLines ARGS_DECL((Window));
extern void	DrawGridLines ARGS_DECL((Window, int LtX, int LtY, int W,
		                         int H));
extern void	RedrawPageLines ARGS_DECL((Window));
extern void	DrawPageLines ARGS_DECL((Window, int LtX, int LtY, int W,
		                         int H));
extern void	ToggleGridSystem ARGS_DECL((void));
extern void	IncGrid ARGS_DECL((void));
extern void	DecGrid ARGS_DECL((void));
extern void	ToggleGridShown ARGS_DECL((void));
extern void	ToggleSnapOn ARGS_DECL((void));
extern void	ToggleColorPostScript ARGS_DECL((void));
extern void	ToggleMoveMode ARGS_DECL((void));
extern void	ToggleMapShown ARGS_DECL((void));
extern void	ToggleUseGray ARGS_DECL((void));
extern void	SetMeasureUnit ARGS_DECL((void));
extern void	ToggleShowMenubar ARGS_DECL((void));
extern void	ToggleShowStatus ARGS_DECL((void));
extern void	ToggleWhereToPrint ARGS_DECL((void));
extern void	ToggleOneMotionSelectMove ARGS_DECL((void));
extern void	ToggleColorLayers ARGS_DECL((void));
extern void	ToggleStretchableText ARGS_DECL((void));
extern void	DefaultZoom ARGS_DECL((void));
extern void	ZoomIn ARGS_DECL((void));
extern void	ZoomInAtCursor ARGS_DECL((int AbsX, int AbsY));
extern void	ZoomWayOut ARGS_DECL((void));
extern void	ZoomOut ARGS_DECL((void));
extern void	PreciseZoom ARGS_DECL((int ZoomedIn, int ZoomScale, int Force));
extern void	SetPSPageWidthHeight ARGS_DECL((void));
extern void	ResetOnePageSize ARGS_DECL((void));
extern int	UpdPageStyle ARGS_DECL((int));
extern void	ChangePageStyle ARGS_DECL((int PageStyle, char *PageStyleStr));
extern void	LayoutSubMenu ARGS_DECL((int Index));
extern void	SetUpLayoutMenuStr ARGS_DECL((char **, char **));
extern int	LayoutMenu ARGS_DECL((int X, int Y, int TrackMenubar));
extern void	GridXY ARGS_DECL((int X, int Y, int *GridX, int *GridY));
extern void	CleanUpGrids ARGS_DECL((void));
extern void	MoveModeSubMenu ARGS_DECL((int Index));
extern int	MoveModeMenu ARGS_DECL((int X, int Y, int TrackMenubar));
extern void	StretchableTextModeSubMenu ARGS_DECL((int Index));
extern int	StretchableTextModeMenu ARGS_DECL((int X, int Y,
		                                   int TrackMenubar));

#endif /*_GRID_E_*/
