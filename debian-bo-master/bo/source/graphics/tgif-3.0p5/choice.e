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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/choice.e,v 3.0 1996/05/06 16:04:02 william Exp $
 */

#ifndef _CHOICE_E_
#define _CHOICE_E_

extern int	curChoice;
extern int	cycleThroughChoice;

extern struct MouseStatusStrRec	choiceMouseStatus[];
extern struct MouseStatusStrRec	modeMouseStatus[];
extern struct MouseStatusStrRec	colorMouseStatus[];
extern struct MouseStatusStrRec	colorMouseStatus[];
extern struct MouseStatusStrRec	hAlignMouseStatus[];
extern struct MouseStatusStrRec	vAlignMouseStatus[];
extern struct MouseStatusStrRec	pageMouseStatus[];
extern struct MouseStatusStrRec	pageLayoutMouseStatus[];
extern struct MouseStatusStrRec	justMouseStatus[];
extern struct MouseStatusStrRec	fontMouseStatus[];
extern struct MouseStatusStrRec	vspaceMouseStatus[];
extern struct MouseStatusStrRec	textSizeMouseStatus[];
extern struct MouseStatusStrRec	rotateMouseStatus[];
extern struct MouseStatusStrRec	editMouseStatus[];
extern struct MouseStatusStrRec	specialMouseStatus[];
extern struct MouseStatusStrRec	lineWidthMouseStatus[];
extern struct MouseStatusStrRec	lineStyleMouseStatus[];
extern struct MouseStatusStrRec	lineTypeMouseStatus[];
extern struct MouseStatusStrRec	lineDashMouseStatus[];
extern struct MouseStatusStrRec	rcbRadiusMouseStatus[];
extern struct MouseStatusStrRec	zoomMouseStatus[];
extern struct MouseStatusStrRec	moveModeMouseStatus[];
extern struct MouseStatusStrRec	bwPrintMouseStatus[];
extern struct MouseStatusStrRec	colorPrintMouseStatus[];
extern struct MouseStatusStrRec	fileMouseStatus[];
extern struct MouseStatusStrRec	fillMouseStatus[];
extern struct MouseStatusStrRec	penMouseStatus[];

extern void	InitChoice ARGS_DECL((void));
extern void	CleanUpChoices ARGS_DECL((void));

extern void	ShowMode ARGS_DECL((void));
extern void	ShowColor ARGS_DECL((int PropagateWhereToPrint));
extern void	ShowHoriAlign ARGS_DECL((void));
extern void	ShowVertAlign ARGS_DECL((void));
extern void	ShowJust ARGS_DECL((void));
extern void	ShowCurFont ARGS_DECL((void));
extern void	ShowTextVSpace ARGS_DECL((void));
extern void	ShowZoom ARGS_DECL((void));
extern void	ShowTextSize ARGS_DECL((void));
extern void	ShowRotate ARGS_DECL((void));
extern void	ShowSpecial ARGS_DECL((void));
extern void	ShowLineWidth ARGS_DECL((void));
extern void	ShowLineStyle ARGS_DECL((void));
extern void	ShowLineType ARGS_DECL((void));
extern void	ShowDash ARGS_DECL((void));
extern void	ShowWhereToPrint ARGS_DECL((void));
extern void	ShowFile ARGS_DECL((void));
extern void	ShowEdit ARGS_DECL((void));
extern void	ShowRCBRadius ARGS_DECL((void));
extern void	ShowMoveMode ARGS_DECL((void));
extern void	ShowShape ARGS_DECL((void));
extern void	ShowStretchableTextMode ARGS_DECL((void));
extern void	ShowFill ARGS_DECL((void));
extern void	ShowPen ARGS_DECL((void));
extern void	ShowPage ARGS_DECL((void));
extern void	ShowPageLayout ARGS_DECL((void));

extern void	SetCurChoice ARGS_DECL((int NewChoice));
extern void	PushCurChoice ARGS_DECL((void));
extern void	FormatAngle ARGS_DECL((int DegreeTimes64, char *buf));
extern int	ChoiceEventHandler ARGS_DECL((XEvent *));

#endif /*_CHOICE_E_*/
