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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/special.e,v 3.0 1996/05/06 16:07:46 william Exp $
 */

#ifndef _SPECIAL_E_
#define _SPECIAL_E_

extern int	placingTopObj;
extern char	* specialMenuStr[];

extern struct ObjRec	* ReadSymbol ARGS_DECL((FILE *));
extern struct ObjRec	* GetObjRepresentation ARGS_DECL((char *PathName,
			                                  char *SymName));
extern unsigned int	PlaceTopObj ARGS_DECL((struct ObjRec *));
extern void	Instantiate ARGS_DECL((void));
extern void	MakeSymbolic ARGS_DECL((void));
extern void	UnMakeSymbolic ARGS_DECL((void));
extern void	MakeIconic ARGS_DECL((void));
extern void	UnMakeIconic ARGS_DECL((void));
extern void	ImportAttrs ARGS_DECL((void));
extern void	ExportAttrs ARGS_DECL((void));
extern void	MergeWithTable ARGS_DECL((void));
extern void	ExportToTable ARGS_DECL((void));
extern void	SpecialSubMenu ARGS_DECL((int Index));
extern int	SpecialMenu ARGS_DECL((int X, int Y, int TrackMenubar));

#endif /*_SPECIAL_E_*/
