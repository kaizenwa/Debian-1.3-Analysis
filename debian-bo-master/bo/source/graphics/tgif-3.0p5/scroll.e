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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/scroll.e,v 3.0 1996/05/06 16:07:24 william Exp $
 */

#ifndef _SCROLL_E_
#define _SCROLL_E_

extern int	autoPan;

extern void	UpdScrollWinWH ARGS_DECL((void));
extern void	InitScroll ARGS_DECL((void));
extern void	RedrawVScrollWindow ARGS_DECL((void));
extern void	RedrawHScrollWindow ARGS_DECL((void));
extern void	RedrawScrollBars ARGS_DECL((void));
extern void	ScrollTo ARGS_DECL((int XOff, int YOff));
extern void	ScrollUp ARGS_DECL((XButtonEvent *));
extern void	ForceScrollDown ARGS_DECL((int));
extern void	ScrollDown ARGS_DECL((XButtonEvent *));
extern void	ScrollLeft ARGS_DECL((XButtonEvent *));
extern void	ScrollRight ARGS_DECL((XButtonEvent *));
extern void	ScrollEventHandler ARGS_DECL((XEvent *));
extern void	ScrollToSpecifiedOrigin ARGS_DECL((int page_num, int orig_x,
		                                   int orig_y, int zoom_scale,
		                                   int zoomed_in));
extern void	ScrollToOrigin ARGS_DECL((void));
extern void	SaveOrigin ARGS_DECL((void));
extern void	CleanUpScrolls ARGS_DECL((void));

#endif /*_SCROLL_E_*/
