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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/menu.e,v 3.0 1996/05/06 16:06:02 william Exp $
 */

#ifndef _MENU_E_
#define _MENU_E_

extern int	iconWindowShown;
extern int	iconWindowCreated;
extern int	importingIconFile;
extern int	showVersion;
extern int	activeMenu;
extern char	*mainMenuStr[];
extern int	mainMenuStrWidth[];
extern GC	textMenuGC;
extern GC	rvPixmapMenuGC;
extern int	multiMenu;

extern int	TextMenuLoop ARGS_DECL((int OrigX, int OrigY, char *Strings[],
		                        int Entries, int *ForeColors,
		                        int *Valid, int *InitRV,
		                        char *StatusStr[], int MultiColor,
		                        int TrackMenubar));
extern int	PxMpMenuLoop ARGS_DECL((int OrigX, int OrigY, int W, int H,
		                        int Rows, int Cols, int Entries,
		                        int *ForeColors, Pixmap PxMp[],
		                        int *InitRv, char *StatusStr[],
		                        int MultiColor, int TrackMenubar));
extern int	MainMenu ARGS_DECL((void));
extern int	IsPrefix ARGS_DECL((char *Prefix, char *Str, char **Rest));
extern void	RedrawTitleWindow ARGS_DECL((void));
extern void	RedrawIconWindow ARGS_DECL((void));
extern void	InitTitle ARGS_DECL((void));
extern void	InitMenu ARGS_DECL((void));
extern void	CleanUpMenu ARGS_DECL((void));
extern void	SaveDrawWinInfo ARGS_DECL((void));
extern void	UnIconify ARGS_DECL((void));
extern void	Iconify ARGS_DECL((void));
extern void	IconEventHandler ARGS_DECL((XEvent *));
extern void	TitleEventHandler ARGS_DECL((XEvent *));
extern void	CalcMenubarWindowHeight ARGS_DECL((void));
extern void	RedrawMenubarWindow ARGS_DECL((void));
extern int	MenubarEventHandler ARGS_DECL((XEvent *));

#endif /*_MENU_E_*/
