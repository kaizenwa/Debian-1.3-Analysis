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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/navigate.e,v 3.0 1996/05/06 16:06:20 william Exp $
 */

#ifndef _NAVIGATE_E_
#define _NAVIGATE_E_

extern int	navigatingBackAndForth;
extern int	inHyperSpace;
extern int	autoHyperSpaceOnRemote;
extern int	allowLaunchInHyperSpace;
extern char	* navigateMenuStr[];
extern char	* navigateMenuStrInHyperSpace[];

extern void	CleanUpNavigate ARGS_DECL((void));

extern void	UpdateLRU ARGS_DECL((struct URLCacheRec *url_cache));
extern struct URLCacheRec	*FindURLCache ARGS_DECL((char *psz_url,
				                         int update_lru));
extern void	UpdateURLCache ARGS_DECL((char *psz_url, char *psz_remote_buf,
		                          char *psz_content_type,
		                          int remote_buf_sz, int is_html));

extern void	BeforeNavigate ARGS_DECL((void));
extern void	CommitNavigate ARGS_DECL((void));

extern void	NavigateBack ARGS_DECL((void));
extern void	NavigateForward ARGS_DECL((void));
extern void	AdjustNavigate ARGS_DECL((void));
extern void	NavigateRefresh ARGS_DECL((void));
extern void	NavigateHotList ARGS_DECL((void));
extern void	NavigateAddToHotList ARGS_DECL((void));
extern void	NavigateSessionHistory ARGS_DECL((void));
extern void	ToggleHyperSpace ARGS_DECL((int KeepSelected));

extern void	NavigateSubMenu ARGS_DECL((int Index));
extern int	NavigateMenu ARGS_DECL((int X, int Y, int TrackMenubar));

#endif /*_NAVIGATE_E_*/
