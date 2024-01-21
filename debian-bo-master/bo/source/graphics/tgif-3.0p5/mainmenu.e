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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/mainmenu.e,v 3.0 1996/05/06 16:05:55 william Exp $
 */

#ifndef _MAINMENU_E_
#define _MAINMENU_E_

extern int	pinnedMainMenu;
extern int	mainMenuPinDistance;
extern int	mainMenuX;
extern int	mainMenuY;
extern int	mainMenuW;
extern int	mainMenuH;
extern Window	mainMenuWindow;

extern int	numStacking;
extern Window	* stackingWins;

extern void	InitMainMenu ARGS_DECL((void));
extern void	CleanUpMainMenu ARGS_DECL((void));
extern void	SaveMainWinPosition ARGS_DECL((unsigned int X, unsigned int Y));
extern void	MoveMainMenuWindow ARGS_DECL((unsigned int X, unsigned int Y));
extern void	RedrawMainMenuWindow ARGS_DECL((void));
extern int	MainMenuEventHandler ARGS_DECL((XEvent *));
extern void	SaveStackingOrder ARGS_DECL((void));
extern void	MoveSubMenuWindow ARGS_DECL((Window));
extern void	RealizeSubMenuWindow ARGS_DECL((int X, int Y, int W, int H));
extern int	HandleSubMenuEvent ARGS_DECL((Window, int Index, XEvent*));
extern int	WindowIsSubMenu ARGS_DECL((Window, int Index));
extern void	UpdateSubMenu ARGS_DECL((int Index));
extern void	UpdateAllSubMenus ARGS_DECL((void));
extern void	DestroySubMenu ARGS_DECL((int Index));

#endif /*_MAINMENU_E_*/
