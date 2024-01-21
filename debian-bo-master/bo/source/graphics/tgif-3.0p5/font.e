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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/font.e,v 3.0 1996/05/06 16:05:11 william Exp $
 */

#ifndef _FONT_E_
#define _FONT_E_

extern XFontStruct	* canvasFontPtr;
extern int	canvasFontHeight;
extern int	canvasFontAsc;
extern int	canvasFontDes;
extern int	canvasFontDirection;
extern int	canvasFontDoubleByte;
extern int	canvasFontSize;
extern int	canvasFontIsFaked;

extern XFontStruct	* rulerFontPtr;
extern int	rulerFontWidth;
extern int	rulerFontHeight;
extern int	rulerFontAsc;
extern int	rulerFontDes;

extern XFontStruct	* defaultFontPtr;
extern int	defaultFontWidth;
extern int	defaultFontHeight;
extern int	defaultFontAsc;
extern int	defaultFontDes;

extern XFontStruct	* menuFontPtr;
extern int	menuFontWidth;
extern int	menuFontHeight;
extern int	menuFontAsc;
extern int	menuFontDes;

extern XFontStruct	* boldMsgFontPtr;
extern int	boldMsgFontWidth;
extern int	boldMsgFontHeight;
extern int	boldMsgFontAsc;
extern int	boldMsgFontDes;

extern XFontStruct	* msgFontPtr;
extern int	msgFontWidth;
extern int	msgFontHeight;
extern int	msgFontAsc;
extern int	msgFontDes;

extern int	curFont;
extern int	curSize;
extern int	curStyle;
extern int	curRotate; /* obsoleted, should always be ROTATE0 */
extern int	textRotation;
extern int	curUnderlineOn;

extern int	curUnderline;

extern char	* styleMenuStr[];
extern char	* * fontMenuStr;
extern char	* * sizeMenuStr;
extern int	* fontSizes;
extern int	numFonts;
extern int	numFontSizes;

extern int	changingFontSizeFromRead;
extern int	attemptingToSetFontProperty;

extern int	ValidCharCode ARGS_DECL((char *));
extern char	* CharCodeTranslate ARGS_DECL((char *));
extern void	PrepareEightBitFontInfo ARGS_DECL((void));
extern int	NeedEncode ARGS_DECL((int FontIndex, int Style));
extern void	GetPSFontStr ARGS_DECL((int FontIndex, int Style, char *));
extern int	GetFontIndex ARGS_DECL((char *FontStr, int Style,
		                        int MustFind));
extern void	CurFontMsg ARGS_DECL((void));
extern void	DumpEightBitFontInfo ARGS_DECL((FILE *));

extern int	GetCompatibleSize ARGS_DECL((int FontDPI, int FontSize));
extern void	SetCanvasFont ARGS_DECL((void));
extern void	InitFonts ARGS_DECL((void));
extern int	ChangeObjTextStyle ARGS_DECL((struct ObjRec *, int Style));
extern void	ChangeFontStyle ARGS_DECL((int Style));
extern int	ChangeObjTextJust ARGS_DECL((struct ObjRec *, int Just));
extern void	ChangeFontJust ARGS_DECL((int Just));
extern void	StyleSubMenu ARGS_DECL((int Index));
extern int	StyleMenu ARGS_DECL((int X, int Y, int TrackMenubar));
extern int	ChangeObjTextSize ARGS_DECL((struct ObjRec *, int SizeIndex));
extern void	ChangeFontSize ARGS_DECL((int SizeIndex));
extern int	GetSizeMenuIndex ARGS_DECL((void));
extern int	SizeMenu ARGS_DECL((int X, int Y, int TrackMenubar));
extern int	ChangeObjTextFont ARGS_DECL((struct ObjRec *, int FontIndex));
extern void	ChangeFont ARGS_DECL((int FontIndex));
extern int	FontMenu ARGS_DECL((int X, int Y, int TrackMenubar));
extern int	ChangeObjVSpace ARGS_DECL((struct ObjRec *, int VSpace));
extern void	ChangeVSpace ARGS_DECL((int VSpace));
extern void	SaveCurFont ARGS_DECL((void));
extern void	RestoreCurFont ARGS_DECL((void));
extern void	PushCurFont ARGS_DECL((void));
extern void	PopCurFont ARGS_DECL((void));
extern void	SetPushedFontValue ARGS_DECL((int Which, int Value));
extern void	CleanUpFonts ARGS_DECL((void));

#endif /*_FONT_E_*/
