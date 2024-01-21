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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/attr.e,v 3.0 1996/05/06 16:03:47 william Exp $
 */

#ifndef _ATTR_E_
#define _ATTR_E_

extern int	dropObsIconAttrWhenUpdate;

extern void	LinkInAttr ARGS_DECL((struct AttrRec *, struct AttrRec *,
		                      struct AttrRec *));
extern void	FreeAttr ARGS_DECL((struct AttrRec *));
extern void	UnlinkAttr ARGS_DECL((struct AttrRec *));
extern void	UpdateAttr ARGS_DECL((struct TextRec *, struct AttrRec *));
extern void	DrawAttrs ARGS_DECL((Window, int X, int Y, struct AttrRec *));
extern void	MoveAttrs ARGS_DECL((struct AttrRec *, int Dx, int Dy));
extern void	DelAllAttrs ARGS_DECL((struct AttrRec *));
extern struct AttrRec	* AddAttrByNameAndValue ARGS_DECL((struct ObjRec *,
		                                           char *AttrName,
		                                           char *AttrValue));
extern void	DupAttrs ARGS_DECL((struct ObjRec *From, struct ObjRec *To));
extern void	AddAttrs ARGS_DECL((void));
extern void	SaveAttrs ARGS_DECL((FILE *, struct AttrRec *));
extern char	* ReadAttrString ARGS_DECL((char *));
extern int	ReadAttr ARGS_DECL((FILE *, struct AttrRec **));
extern void	ShowAllAttrNames ARGS_DECL((void));
extern void	ShowAllAttrs ARGS_DECL((void));
extern void	HideAllAttrs ARGS_DECL((void));
extern void	HideAllAttrNames ARGS_DECL((void));
extern void	DetachGroupAttrs ARGS_DECL((struct ObjRec *,
		                            struct SelRec **Top,
		                            struct SelRec **Bot));
extern void	DetachAllObjAttrs ARGS_DECL((struct ObjRec *,
		                             struct SelRec **Top,
		                             struct SelRec **Bot));
extern void	DetachAttrs ARGS_DECL((void));
extern void	UpdAttr ARGS_DECL((struct AttrRec *));
extern void	MoveAttr ARGS_DECL((void));
extern void	CopyAndUpdateAttrs ARGS_DECL((struct ObjRec *To,
		                              struct ObjRec *From));
extern void	ToggleNamedAttrShown ARGS_DECL((char *));
extern void	AddFileAttrs ARGS_DECL((void));
extern void	DetachFileAttrs ARGS_DECL((void));
extern void	EditFileAttrs ARGS_DECL((void));
extern void	EditAttrs ARGS_DECL((void));

#endif /*_ATTR_E_*/
