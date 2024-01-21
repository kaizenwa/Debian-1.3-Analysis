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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/rcbox.e,v 3.0 1996/05/06 16:07:07 william Exp $
 */

#ifndef _RCBOX_E_
#define _RCBOX_E_

extern int	rcBoxDrawn;

extern void	SetRCBoxVertex ARGS_DECL((int X1, int Y1, int X2, int Y2,
		                          int Radius));
extern void	MyRCBox ARGS_DECL((Window, GC, int X1, int Y1, int X2, int Y2,
		                   int Radius));
extern void	DumpRCBoxObj ARGS_DECL((FILE *, struct ObjRec *));
extern int	NeedsToCacheRCBoxObj ARGS_DECL((struct ObjRec *));
extern void	DrawRCBoxObj ARGS_DECL((Window, int XOff, int YOff,
		                        struct ObjRec *));
extern void	DrawRCBox ARGS_DECL((XEvent *));
extern void	SaveRCBoxObj ARGS_DECL((FILE *, struct ObjRec *));
extern void	ReadRCBoxObj ARGS_DECL((FILE *, char *Inbuf, struct ObjRec **));
extern void	FreeRCBoxObj ARGS_DECL((struct ObjRec *));

#endif /*_RCBOX_E_*/
