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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/setup.e,v 3.0 1996/05/06 16:07:34 william Exp $
 */

#ifndef _SETUP_E_
#define _SETUP_E_

extern unsigned int	mainWinW;
extern unsigned int	mainWinH;
extern int	vSBarH;
extern int	hSBarW;
extern int	scrollBarW;
extern int	rulerW;
extern int	brdrW;
extern int	pageWindowW;
extern int	pageDummyWindowW;
extern int	colorWindowH;
extern int	colorDummyWindowH;
extern int	msgWindowW;
extern int	msgWindowH;
extern int	choiceImageW;
extern int	choiceImageH;
extern int	choiceWindowW;
extern int	choiceWindowH;
extern int	menuImageW;
extern int	menuImageH;
extern int	titleWindowW;
extern int	titleWindowH;
extern int	iconWindowW;
extern int	iconWindowH;
extern int	menubarWindowW;
extern int	menubarWindowH;
extern int	statusWindowW;
extern int	statusWindowH;
extern int	statusSubWindowW[];
extern int	statusSubWindowH[];

extern int	initialMenubarWindowH;

extern Display		* mainDisplay;
extern Colormap		mainColormap;
extern unsigned int	mainDepth;
extern int		mainScreen;
extern Visual		* mainVisual;

extern Window	rootWindow;
extern Window	mainWindow;
extern Window	drawWindow;
extern Window	choiceWindow;
extern Window	titleWindow;
extern Window	msgWindow;
extern Window	vSBarWindow;
extern Window	hSBarWindow;
extern Window	vRuleWindow;
extern Window	hRuleWindow;
extern Window	iconWindow;
extern Window	iconBaseWindow;
extern Window	menubarWindow;
extern Window	statusWindow;
extern Window	pageWindow;
extern Window	pageDummyWindow;
extern Window	colorWindow;
extern Window	colorDummyWindow;

extern int	paperWidth;
extern int	paperHeight;
extern int	onePageWidth;
extern int	onePageHeight;
extern int	drawOrigX;
extern int	drawOrigY;
extern int	drawWinW;
extern int	drawWinH;

extern int	zoomScale;
extern int	zoomedIn;

extern struct BBRec	drawWinBBox;

extern int	colorDisplay;
extern int	fileModified;
extern int	objId;

extern int	myBgPixel;
extern int	myFgPixel;
extern int	myBorderPixel;
extern int	reverseVideo;

extern char	drawPath[];
extern char	bootDir[];
extern char	homeDir[];

extern int	symPathNumEntries;
extern char	* * symPath;

extern int	initDrawWinW;
extern int	initDrawWinH;

extern short	handleSize;
extern int	resizeTextOnStretch;

extern Window	dummyWindow1, dummyWindow2;

extern Window	statusSubWindow[];

extern int	noMenubar;
extern int	noStatusWindow;

extern void	UpdDrawWinWH ARGS_DECL((void));
extern void	UpdDrawWinBBox ARGS_DECL((void));
extern void	ComputeMainWinXY ARGS_DECL((int *MainWinX, int *MainWinY));
extern int	mainWinEventHandler ARGS_DECL((XEvent *));
extern void	Reconfigure ARGS_DECL((int Forced));
extern void	InitPaperSize ARGS_DECL((void));
extern void	CleanUpPaperSize ARGS_DECL((void));
extern void	Setup ARGS_DECL((void));
extern void	CleanUpResiduals ARGS_DECL((void));
extern int	TieLooseEnds ARGS_DECL((void));
extern void	MakeQuiescent ARGS_DECL((void));
extern void	SetFileModified ARGS_DECL((int));

#endif /*_SETUP_E_*/
