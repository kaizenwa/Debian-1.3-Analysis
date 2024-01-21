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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/names.e,v 3.0 1996/05/06 16:06:14 william Exp $
 */

#ifndef _NAMES_E_
#define _NAMES_E_

extern char	curDomainName[];
extern char	curDomainPath[];
extern char	curDir[];
extern char	curLocalDir[];
extern char	curImportDir[];
extern char	curSymDir[];

extern int	doubleClickInterval;
extern int	importFromLibrary;
extern int	curDirIsLocal;

extern int	domainInResource;

extern int	ignoreDirectoryFlag;
		/* use to be ignoreDirectoryFlagInMakeNameDspItemArray; */

extern void	ParseSymPath ARGS_DECL((char*));
extern void	InitNames ARGS_DECL((void));
extern void	UpdateDirInfo ARGS_DECL((void));
extern void	UpdateSymInfo ARGS_DECL((void));
extern void	CleanUpNames ARGS_DECL((void));

extern char	**MakeNameDspItemArray ARGS_DECL((int Entries, DspList*));

extern int	SelectFileName ARGS_DECL((char *MsgStr, char *SelStr));
extern int	SelectFileNameToPaste ARGS_DECL((char *MsgStr, char *SelStr));
extern int	SelectFileNameToImport ARGS_DECL((char *MsgStr, char *ExtStr,
		                                  char *SelStr));
extern int	GetSymbolPath ARGS_DECL((char *SymName, char *PathName));
extern int	NameInCurDir ARGS_DECL((char *FileName));
extern int	DirInSymPath ARGS_DECL((char *DirName));
extern int	SelectDomain ARGS_DECL((char *SelStr));
extern int	SelectSymDir ARGS_DECL((char *SelStr));
extern int	SelectFromLibrary ARGS_DECL((char *MsgStr, char *ExtStr,
		                             char *SelStr, char *PathStr));
extern void	SetCurDir ARGS_DECL((char *FileName));
extern void	SetCurSymDir ARGS_DECL((char *FileName));
extern void	SetCurImportDir ARGS_DECL((char *FileName));

#endif /*_NAMES_E_*/
