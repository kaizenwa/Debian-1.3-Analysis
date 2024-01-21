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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/msg.e,v 3.0 1996/05/06 16:06:11 william Exp $
 */

#ifndef _MSG_E_
#define _MSG_E_

extern char	scanFileName[];
extern int	scanLineNum;
extern char	progName[];

extern int	FailAllocMessage ARGS_DECL((void));
extern void	CleanUpMsg ARGS_DECL((void));
extern void	RedrawMsg ARGS_DECL((void));
extern void	Msg ARGS_DECL((char *));
extern void	Warning ARGS_DECL((char *Where, char *Message));
extern void	TwoLineMsg ARGS_DECL((char *Msg1, char *Msg2));
extern void	PrintMsgBuffer ARGS_DECL((void));
extern void	RedrawStatusWindow ARGS_DECL((void));
extern void	SetMouseStatus ARGS_DECL((char *Left, char *Middle,
		                          char *Right));
extern void	SetStringStatus ARGS_DECL((char *StatusStr));
extern void	SaveStatusStrings ARGS_DECL((void));
extern void	RestoreStatusStrings ARGS_DECL((void));
extern void	SaveStatusStringsIntoBuf ARGS_DECL((
		      char ppsz_buf[MAX_STATUS_BTNS+1][MAXSTRING+1], int*));
extern void	RestoreStatusStringsFromBuf ARGS_DECL((
		      char ppsz_buf[MAX_STATUS_BTNS+1][MAXSTRING+1], int));
extern void	MsgEventHandler ARGS_DECL((XEvent *));
extern void	StatusEventHandler ARGS_DECL((XEvent *));
extern void	InitStatus ARGS_DECL((void));
extern void	CleanUpStatus ARGS_DECL((void));
extern void	InitScan ARGS_DECL((char *Str, char *Pat));
extern int	ScanValue ARGS_DECL((char *Fmt, void *Value, char *Item,
		                     char *SType));
extern void	EmergencySave ARGS_DECL((int Sig));
extern int	EmergencySaveForX ARGS_DECL((Display *, XErrorEvent *));
extern int	IOEmergencySaveForX ARGS_DECL((Display *));
extern void	Error ARGS_DECL((char *Where, char *Message));
extern void	Usage ARGS_DECL((char *));
extern int	ProcessPrTgifOptions ARGS_DECL((int argc, char *argv[], int));
extern int	ProcessTgifOptions ARGS_DECL((int argc, char *argv[], char*));

#endif /*_MSG_E_*/
