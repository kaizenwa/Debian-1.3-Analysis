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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/dup.e,v 3.0 1996/05/06 16:04:41 william Exp $
 */

#ifndef _DUP_E_
#define _DUP_E_

extern int	justDupped;
extern int	useRecentDupDistance;
extern int	dupDx;
extern int	dupDy;

extern void	CopyObjId ARGS_DECL((struct ObjRec *From, struct ObjRec *To));
extern void	CopyObjLocks ARGS_DECL((struct ObjRec *From,
		                        struct ObjRec *To));
extern void	UnlockAnObj ARGS_DECL((struct ObjRec *From));
extern void	DupObjXfrmMtrx ARGS_DECL((struct ObjRec *From,
		                          struct ObjRec *To));
extern void	DupObjBasics ARGS_DECL((struct ObjRec *From,
		                        struct ObjRec *To));
extern void	DupPolyObj ARGS_DECL((struct PolyRec *, struct ObjRec *));
extern void	DupPolygonObj ARGS_DECL((struct PolygonRec *, struct ObjRec *));
extern void	DupOvalObj ARGS_DECL((struct OvalRec *, struct ObjRec *));
extern void	DupBoxObj ARGS_DECL((struct BoxRec *, struct ObjRec *));
extern void	DupRCBoxObj ARGS_DECL((struct RCBoxRec *, struct ObjRec *));
extern void	DupArcObj ARGS_DECL((struct ArcRec *, struct ObjRec *));
extern void	DupXBmObj ARGS_DECL((struct XBmRec *, struct ObjRec *));
extern void	DupXPmObj ARGS_DECL((struct XPmRec *, struct ObjRec *));
extern void	DupTextObj ARGS_DECL((struct TextRec *, struct ObjRec *From,
		                      struct ObjRec *To));
extern void	DupGroupObj ARGS_DECL((struct GroupRec *, struct ObjRec *));
extern struct ObjRec	* DupObj ARGS_DECL((struct ObjRec *));
extern void	DupSelObj ARGS_DECL((void));
extern void	DupTheseObjects ARGS_DECL((struct SelRec *Top,
		                           struct SelRec *Bot,
		                           struct SelRec **NewTop,
		                           struct SelRec **NewBot));
extern void	JustDupSelObj ARGS_DECL((struct SelRec **NewTop,
		                         struct SelRec **NewBot));

#endif /*_DUP_E_*/
