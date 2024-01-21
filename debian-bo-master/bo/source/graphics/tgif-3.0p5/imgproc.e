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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/imgproc.e,v 3.0 1996/05/06 16:05:39 william Exp $
 */

#ifndef _IMGPROC_E_
#define _IMGPROC_E_

extern int numImageProc;
extern int gnInImageProc;
extern int gnConvolving;
extern int gnNumNewColorsInPixmapFile;
extern char gszImageProcXPmFile[MAXPATHLENGTH+1];

extern char *imageProcMenuStr[];

extern void	CleanUpConvolution ARGS_DECL((void));
extern int	DoConvolution ARGS_DECL((FILE*, XImage *image,
		                         XImage *bitmap_image, int W, int H));

extern void	MakeGray ARGS_DECL((void));
extern void	InvertColor ARGS_DECL((void));
extern void	InterpolateColor ARGS_DECL((void));
extern void	BrightenDarken ARGS_DECL((void));
extern void	ChangeSaturation ARGS_DECL((void));
extern void	ChangeHue ARGS_DECL((void));
extern void	ContrastEnhance ARGS_DECL((void));
extern void	ColorBalance ARGS_DECL((void));
extern void	Gamma ARGS_DECL((void));
extern void	EdgeDetect ARGS_DECL((void));
extern void	Emboss ARGS_DECL((void));
extern void	ReduceColors ARGS_DECL((void));
extern void	ReduceToPixmapColors ARGS_DECL((void));
extern void	SetDefaultColorLevels ARGS_DECL((void));
extern void	ReduceToDefaultColors ARGS_DECL((void));
extern void	DefaultErrorDiffuse ARGS_DECL((void));
extern void	Spread ARGS_DECL((void));
extern void	Sharpen ARGS_DECL((void));
extern void	Blur3 ARGS_DECL((void));
extern void	Blur5 ARGS_DECL((void));
extern void	Blur7 ARGS_DECL((void));
extern void	RunBggen ARGS_DECL((void));
extern void	CircularBggen ARGS_DECL((void));
extern void	RegenerateImage ARGS_DECL((void));
extern void	CropImage ARGS_DECL((void));
extern void	GetColor ARGS_DECL((void));
extern void	ReplaceColor ARGS_DECL((void));
extern void	FloodFill ARGS_DECL((void));
extern void	CreateContour ARGS_DECL((void));
extern void	Subtract ARGS_DECL((void));
extern void	AlphaCombine ARGS_DECL((void));

extern void	CleanUpImageProc ARGS_DECL((void));
extern void	InitImageProc ARGS_DECL((void));
extern void	ImageProcSubMenu ARGS_DECL((int Index));
extern int	ImageProcMenu ARGS_DECL((int X, int Y, int TrackMenubar));

#endif /*_IMGPROC_E_*/
