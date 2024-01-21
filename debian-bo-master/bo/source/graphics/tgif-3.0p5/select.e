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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/select.e,v 3.0 1996/05/06 16:07:29 william Exp $
 */

#ifndef _SELECT_E_
#define _SELECT_E_

extern int		selLtX, selLtY, selRbX, selRbY;
extern int		selObjLtX, selObjLtY, selObjRbX, selObjRbY;
extern int		numObjSelected;
extern int		numObjLocked;
extern struct SelRec	* topSel, * botSel;
extern struct VSelRec	* topVSel, * botVSel;

extern int	CountSelectedVertices ARGS_DECL((void));
extern void	CalcBBox ARGS_DECL((int X1, int Y1, int X2, int Y2,
		                    int *LtX, int *LtY, int *RbX, int *RbY));
extern void	CalcVertexBBox ARGS_DECL((int *LtX, int *LtY, int *RbX,
		                          int *RbY));
extern void	UnSelNonVertexObjs ARGS_DECL((int HighLight));
extern void	JustRemoveAllVSel ARGS_DECL((void));
extern void	RemoveAllSel ARGS_DECL((void));
extern struct ObjRec	* FindAnObj ARGS_DECL((int XOff, int YOff,
			                       struct ObjRec **OwnerObj,
			                       struct ObjRec **ConnectObj,
			                       char *ObjName));
extern void	AddSel ARGS_DECL((struct SelRec *Prev, struct SelRec *Next,
		                  struct SelRec *));
extern void	AddNewSelObj ARGS_DECL((struct ObjRec *));
extern void	UpdSelBBox ARGS_DECL((void));
extern void	SelBox ARGS_DECL((Window, GC, int X1, int Y1, int X2, int Y2));
extern void	Select ARGS_DECL((XEvent *));
extern struct AttrRec	* FindFileAttrWithName ARGS_DECL((char *AttrName));
extern struct AttrRec	* FindAttrWithName ARGS_DECL((struct ObjRec *,
			                              char *AttrName,
			                              struct ObjRec **));
extern struct AttrRec	* ValidAttrArg ARGS_DECL((char *CPtr, struct ObjRec *,
			                          char **NewCPtr));
extern int	DoTeleport ARGS_DECL((struct AttrRec *));
extern int	DoPageTeleport ARGS_DECL((struct AttrRec *, int ByPageName));
extern void	DoExecLoop ARGS_DECL((struct ObjRec *, struct AttrRec *));
extern void	Teleport ARGS_DECL((XButtonEvent *));
extern void	SelAllObj ARGS_DECL((int HighLight));
extern void	JustMoveSelToTop ARGS_DECL((void));
extern void	MoveSelToTop ARGS_DECL((void));
extern void	MoveSelToBot ARGS_DECL((void));
extern void	DelAllSelObj ARGS_DECL((void));
extern void	GroupSingleObj ARGS_DECL((void));
extern void	GroupSelObj ARGS_DECL((void));
extern void	SelectTopObj ARGS_DECL((void));

#endif /*_SELECT_E_*/
