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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/mainloop.e,v 3.0 1996/05/06 16:05:52 william Exp $
 */

#ifndef _MAINLOOP_E_
#define _MAINLOOP_E_

extern int	origArgC;
extern char	* * origArgV;
extern int	cmdLineBW;
extern int	cmdLineRV;
extern int	cmdLineCWO;
extern char	* cmdLineForeground;
extern char	* cmdLineBackground;
extern char	* cmdLineBorder;
extern int	geometrySpecified;
extern int	exitNormally;
extern char	geometrySpec[];
extern char	* displayName;
extern char	initMsg1[];
extern char	initMsg2[];
extern int	numExtraWins;

extern struct WinInfoRec	* extraWinInfo;

extern int	AddExtraWinInfo ARGS_DECL((Window, int Mapped, int Raise,
		                           void (*ExposeHandler)(),
		                           int (*EventHandler)(),
		                           void (*CleanUpRoutine)()));
extern void	DeallocStrings ARGS_DECL((char **Func,
		                char **Str1, char **Menu1,
		                char **Str2, char **Menu2,
		                char **Str3, char **Menu3));
extern void	CleanUp ARGS_DECL((void));
extern void	ExposeEventHandler ARGS_DECL((XEvent *, int Recursive));
extern void	MainLoop ARGS_DECL((char *Op, char *FileName, char **Func,
		                    char **Str1, char **Menu1,
		                    char **Str2, char **Menu2,
		                    char **Str3, char **Menu3));
extern void	Animate ARGS_DECL((char *Type, char *PolyId, char *Speed,
		                   char *Color, char **ReturnStr));
extern void	UpdAttrVal ARGS_DECL((char *ObjId, char *AttrName,
		                      char *AttrColor, char *AttrVal,
		                      char **ReturnStr));

#endif /*_MAINLOOP_E_*/
