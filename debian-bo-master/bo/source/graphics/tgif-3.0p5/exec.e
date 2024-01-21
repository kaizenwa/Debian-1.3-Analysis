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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/exec.e,v 3.0 1996/05/06 16:04:57 william Exp $
 */

#ifndef _EXEC_E_
#define _EXEC_E_

extern int		execAnimating;
extern int		execAnimateRedraw;
extern int		execCurDepth;
extern int		replaceAttrFirstValueRedraw;
extern int		execNavigateBack;

extern struct AttrRec	* warpToAttr;

extern char		* cmdToExecAfterHyperJump;

extern struct ObjRec	* FindObjWithName ARGS_DECL((struct ObjRec *BotObj,
			                             struct ObjRec *ObjPtr,
			                             char *ObjName,
			                             int InsideRootObj,
			                             int InsideThisObj,
			                             struct ObjRec **OwnerObj,
			                             struct ObjRec **TopOwner));

extern int	DoLaunch ARGS_DECL((struct AttrRec *, struct ObjRec *));
extern void	ReplaceAttrFirstValue ARGS_DECL((struct ObjRec *,
		                                 struct AttrRec *, char *));
extern int	DoExec ARGS_DECL((struct AttrRec *, struct ObjRec *));
extern void	CleanUpExec ARGS_DECL((void));

#endif /*_EXEC_E_*/
