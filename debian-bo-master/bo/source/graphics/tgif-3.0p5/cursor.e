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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/cursor.e,v 3.0 1996/05/06 16:04:21 william Exp $
 */

#ifndef _CURSOR_E_
#define _CURSOR_E_

extern Cursor	nullCursor;
extern Cursor	cornerCursor;
extern Cursor	handCursor;
extern Cursor	defaultCursor;
extern Cursor	watchCursor;
extern Cursor	drawCursor;
extern Cursor	vertexCursor;
extern Cursor	rotateCursor;
extern Cursor	rotatingCursor;
extern Cursor	horiShearCursor;
extern Cursor	vertShearCursor;
extern Cursor	moveCursor;
extern Cursor	textCursor;
extern Cursor	hyperSpaceCursor;
extern Cursor	magCursor;
extern Cursor	floodCursor;
extern Cursor	dripCursor;

extern int	watchCursorOnMainWindow;

extern void	SetTextCursor ARGS_DECL((Window));
extern void	SetNullCursor ARGS_DECL((Window));
extern void	SetWatchCursor ARGS_DECL((Window));
extern void	SetDrawCursor ARGS_DECL((Window));
extern void	SetVertexCursor ARGS_DECL((Window));
extern void	SetRotateCursor ARGS_DECL((Window));
extern void	SetRotatingCursor ARGS_DECL((Window));
extern void	SetHoriShearCursor ARGS_DECL((Window));
extern void	SetVertShearCursor ARGS_DECL((Window));
extern void	SetMoveCursor ARGS_DECL((Window));
extern void	SetHyperSpaceCursor ARGS_DECL((Window));
extern void	SetFloodFillCursor ARGS_DECL((Window));
extern void	SetDripCursor ARGS_DECL((Window));
extern void	SetDefaultCursor ARGS_DECL((Window));
extern void	ShowCursor ARGS_DECL((void));
extern void	CreateCursor ARGS_DECL((void));
extern void	PutCursor ARGS_DECL((Window, int X, int Y, int Foreground));
extern void	CleanUpCursors ARGS_DECL((void));
extern Cursor	NewFontCursor ARGS_DECL((char*));
extern void	SetWindowCursor ARGS_DECL((Window, Cursor));
extern void	DeleteFontCursor ARGS_DECL((Cursor));

#endif /*_CURSOR_E_*/
