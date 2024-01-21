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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/page.e,v 3.0 1996/05/06 16:06:35 william Exp $
 */

#ifndef _PAGE_E_
#define _PAGE_E_

extern struct PageRec	* firstPage, * lastPage, * curPage;
extern int		curPageNum, lastPageNum;
extern int		paperRow, paperCol;
extern int		pageLayoutMode;
extern int		pageLineShownInTileMode;

extern char		* pageStackMenuStr[];
extern char		* pageTileMenuStr[];

extern void	RedrawPageWindow ARGS_DECL((void));
extern void	PageEventHandler ARGS_DECL((XEvent*));
extern void	RedrawPageDummyWindow ARGS_DECL((void));
extern void	PageDummyEventHandler ARGS_DECL((XEvent*));
extern void	InitPage ARGS_DECL((void));
extern void	GotoPageNum ARGS_DECL((int));
extern void	SetCurPage ARGS_DECL((int));
extern void	NextPage ARGS_DECL((void));
extern void	PrevPage ARGS_DECL((void));
extern void	NamePages ARGS_DECL((void));
extern void	GotoPage ARGS_DECL((void));
extern void	AddPageBefore ARGS_DECL((void));
extern void	AddPageAfter ARGS_DECL((void));
extern void	DeleteCurPage ARGS_DECL((void));
extern void	TogglePageLineShown ARGS_DECL((void));
extern void	SpecifyDrawingSize ARGS_DECL((void));
extern void	PrintOnePage ARGS_DECL((void));
extern int	SetPaperSize ARGS_DECL((char *));
extern void	SpecifyPaperSize ARGS_DECL((void));
extern void	DeletePages ARGS_DECL((void));
extern void	PrintOneFilePerPage ARGS_DECL((void));
extern void	PageSubMenu ARGS_DECL((int Index));
extern int	PageMenu ARGS_DECL((int X, int Y, int TrackMenubar));
extern void	DelAllPages ARGS_DECL((void));
extern void	PageLayoutSubMenu ARGS_DECL((int Index));
extern int	PageLayoutMenu ARGS_DECL((int X, int Y, int TrackMenubar));
extern void	CleanUpPage ARGS_DECL((void));
extern void	PushPageInfo ARGS_DECL((void));
extern void	PopPageInfo ARGS_DECL((void));

#endif /*_PAGE_E_*/
