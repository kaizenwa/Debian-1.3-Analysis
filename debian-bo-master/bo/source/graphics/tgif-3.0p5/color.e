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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/color.e,v 3.0 1996/05/06 16:04:15 william Exp $
 */

#ifndef _COLOR_E_
#define _COLOR_E_

extern int	maxColors;
extern int	defaultColorIndex;
extern int	colorIndex;
extern char	myFgColorStr[];
extern char	myBgColorStr[];
extern char	* * colorMenuItems;
extern int	* colorPixels;
extern int	* xorColorPixels;
extern int	* colorLayerOn;
extern XColor	* tgifColors;
extern XColor	* tgifRequestedColors;
extern XColor	myBgColor;
extern int	maxRGB;
extern int	colorDump;
extern int	useLocalRGBTxt;
extern int	printUsingRequestedColor;
extern int	colorLayers;
extern int	needToRedrawColorWindow;
extern int	initColorDontReload;
extern int	gnUpdatePixelObjCount;

extern void	DefaultColorArrays ARGS_DECL((int Entries, int **ForePixels,
		                              int **Valid, int **InitRV,
		                              char ***StatusStr));
extern int	TgifParseColor ARGS_DECL((char*, XColor*));
extern void	InitColor ARGS_DECL((void));
extern int	OneColorObject ARGS_DECL((struct ObjRec *, int *ColorIndex));
extern int	ChangeObjColor ARGS_DECL((struct ObjRec *, int ColorIndex));
extern void	ChangeAllSelColor ARGS_DECL((int ColorIndex, int HighLight));
extern void	SetUpColorMenuPixmap ARGS_DECL((int **ForeColors,
		                                int **InitRV, Pixmap **,
		                                int *Rows, int *Cols));
extern int	ColorMenu ARGS_DECL((int X, int Y, int TrackMenubar));
extern void	CleanUpColors ARGS_DECL((void));
extern void	RedrawColorWindow ARGS_DECL((void));
extern void	ColorEventHandler ARGS_DECL((XEvent*));
extern void	RedrawColorDummyWindow ARGS_DECL((void));
extern void	ColorDummyEventHandler ARGS_DECL((XEvent*));
extern int	UpdatePixel ARGS_DECL((struct ObjRec *, char **));
extern void 	UpdateXPmObjects ARGS_DECL((struct ObjRec *));
extern int	FlushColormap ARGS_DECL((void));
extern void	AddColor ARGS_DECL((void));
extern void	DumpRGBColorLine ARGS_DECL((FILE *FP, int ColorIndex,
		                            int Indent, int EndOfLine));

#endif /*_COLOR_E_*/
