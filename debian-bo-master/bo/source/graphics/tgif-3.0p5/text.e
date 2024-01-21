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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/text.e,v 3.0 1996/05/06 16:12:17 william Exp $
 */

#ifndef _TEXT_E_
#define _TEXT_E_

extern int		textDrawn;
extern int		curTextModified;
extern int		textVSpace;

extern int		textJust;
extern int		textCursorShown;
extern int		textCursorH;
extern int		editingText;
extern struct ObjRec	* curTextObj;

/* DynStr routines */
extern void		FreeDynStr ARGS_DECL((struct DynStrRec *));
extern void		DynStrCpy ARGS_DECL((struct DynStrRec *Dest,
			                     struct DynStrRec *Src));
extern struct DynStrRec	* NewDynStr ARGS_DECL((void));
extern struct DynStrRec	* DynStrDup ARGS_DECL((struct DynStrRec *));
extern void		DynStrSet ARGS_DECL((struct DynStrRec *, char *));

/* Str routines */
extern void		FreeStr ARGS_DECL((struct StrRec *));
extern struct StrRec	* NewStr ARGS_DECL(());

/* TextRec routines */
extern void		CopyCurInfoToTextPtr ARGS_DECL((struct TextRec *));

/* text routines */
extern void		BlurText ARGS_DECL((Window, GC, int XOff, int YOff,
			                    int W, int H));
extern void		InitText ARGS_DECL((void));
extern void		CleanUpText ARGS_DECL((void));
extern void		PutTextCursor ARGS_DECL((void));
extern void		EraseTextCursor ARGS_DECL((void));
extern void		NewCurText ARGS_DECL((void));
extern void		FreeTextObj ARGS_DECL((struct ObjRec *));
extern int		CreateTextObj ARGS_DECL((void));
extern void		HighLightJustDrawnText ARGS_DECL((void));
extern void		DrawText ARGS_DECL((XEvent *));
extern void		EditTextInAttr ARGS_DECL((struct AttrRec *));
extern void		FindText ARGS_DECL((char*));
extern void		FindTextAgain ARGS_DECL((void));
extern void		DeleteHighLightedText ARGS_DECL((void));
extern void		DumpOneStr ARGS_DECL((FILE *, int FontIndex, char *));
extern void		DrawTextObj ARGS_DECL((Window, int XOff, int YOff,
			                       struct ObjRec *));
extern void		ReadTextObj ARGS_DECL((FILE *, char *Inbuf,
			                       struct ObjRec **));
extern void		RedrawCurText ARGS_DECL((void));
extern void		UpdCurTextBBox ARGS_DECL((void));
extern void		AdjustCurText ARGS_DECL((int XOff, int YOff));
extern void		PrepareZoomCurText ARGS_DECL((int *AbsXc, int *AbsYc));
extern void		PostZoomCurText ARGS_DECL((int AbsXc, int AbsYc));

#endif /*_TEXT_E_*/
