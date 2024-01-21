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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/cmd.e,v 3.0 1996/05/06 16:04:11 william Exp $
 */

#ifndef _CMD_E_
#define _CMD_E_

extern int		recordCmdIncludeTgifObj;
extern int		recordCmdUsesNewColormap;
extern int		undoingOrRedoing;
extern int		historyDepth, historyCount, defaultHistoryDepth;
extern struct CmdRec	*firstCmd, *lastCmd, *curCmd;

extern struct SelRec	*topSelBeforeInCmd, *botSelBeforeInCmd;
extern int		*stackingPosition;
extern int		stackingCount;
extern int		composingCommand;
extern Colormap		preparedColormap;

extern void	InsertCmd ARGS_DECL((struct CmdRec *PrevCmd,
		                     struct CmdRec *NextCmd,
		                     struct CmdRec *CmdPtr));
extern void	DeleteARedoRecord ARGS_DECL((struct CmdRec *, double PercStart,
		                             double PercEnd));
extern void	ClearRedoRecords ARGS_DECL((struct CmdRec *));
extern void	CleanUpCmds ARGS_DECL((void));
extern void	CopySel ARGS_DECL((struct SelRec *FromTopSel, int Count,
		                   struct SelRec **ToTopSel,
		                   struct SelRec **ToBotSel));
extern void	UndoACmd ARGS_DECL((struct CmdRec *CmdPtr, int HighLight));
extern void	UndoCmd ARGS_DECL((void));
extern void	RedoACmd ARGS_DECL((struct CmdRec *CmdPtr, int HighLight));
extern void	RedoCmd ARGS_DECL((void));
extern void	StartCompositeCmd ARGS_DECL((void));
extern void	EndCompositeCmd ARGS_DECL((void));
extern void	RestoreDefaultHistoryDepth ARGS_DECL((void));
extern void	DisableUndo ARGS_DECL((void));
extern void	EnableUndo ARGS_DECL((void));
extern void	PrepareStacking ARGS_DECL((struct SelRec *TopSel,
		                           struct SelRec *BotSel, int NumObjs));
extern void	PrepareToRecord ARGS_DECL((int, struct SelRec *Top,
		                           struct SelRec *Bot, int NumObjs));
extern void	FreeAfterSel ARGS_DECL((struct CmdRec *));
extern void	RecordCmd ARGS_DECL((int, struct SubCmdRec *,
		                     struct SelRec *Top, struct SelRec *Bot,
		                     int NumObjs));
extern void	AbortPrepareCmd ARGS_DECL((int CmdType));
extern void	RecordNewObjCmd ARGS_DECL((void));
extern void	PrepareToReplaceAnObj ARGS_DECL((struct ObjRec *));
extern void	RecordReplaceAnObj ARGS_DECL((struct ObjRec *));
extern void	ChangeReplaceOneCmdToDeleteCmd ARGS_DECL((void));

#endif /*_CMD_E_*/
