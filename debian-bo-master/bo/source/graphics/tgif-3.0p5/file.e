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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/file.e,v 3.0 1996/05/06 16:05:05 william Exp $
 */

#ifndef _FILE_E_
#define _FILE_E_

extern int	PRTGIF;
extern char	curFileName[];
extern int	curFileDefined;
extern int	fileVersion;
extern int	curFileWriteVersion;
extern int	importingFile;
extern int	psDotsPerInch;
extern int	printMag;
extern int	saveTmpOnReturn;
extern int	warpToWinCenter;
extern float	tiledPageScaling;

extern char	* psXOffStr[];
extern float	psXOff[];
extern char	* * psYOffStr;
extern float	* psYOff;
extern float	* psPageWidthInInch;
extern float	* psPageHeightInInch;
extern char	printCommand[];
extern char	outputDir[];
extern char	* fileMenuStr[];

extern char	* savedComments;
extern int	savedCommentsLen;
extern int	saveCommentsInSaveNew;
extern int	usePsAdobeString;
extern char	adobeString[];
extern char	epsfString[];

extern int	readingPageNum;
extern int	loadedCurPageNum;

extern int	writeFileFailed;
extern int	foundGoodStateObject;

extern int	cmdLineHasPageNum;
extern int	cmdLinePageNum;
extern char	cmdLinePageNumStr[];

extern int	cmdLineOneFilePerPage;
extern int	cmdLineA4;

extern int	showPageInEPS;

extern void	UpdateDocumentFonts ARGS_DECL((char*));

extern void	ClearFileInfo ARGS_DECL((void));
extern void	CleanUpComments ARGS_DECL((void));
extern int	OkayToCreateFile ARGS_DECL((char *));
extern void	Save ARGS_DECL((FILE *, struct ObjRec *BotObj, int Level,
		                int PageNumber));
extern int	SaveTmpFile ARGS_DECL((char *));
extern void	SaveNewFile ARGS_DECL((int SaveSelectedOnly));
extern void	SaveSymInLibrary ARGS_DECL((void));
extern void	SaveFile ARGS_DECL((void));
extern char	* ParseStr ARGS_DECL((char *Str, int C, char *Left,
		                      int LeftSz));
extern char	* FindChar ARGS_DECL((int C, char *Str));
extern int	ReadObj ARGS_DECL((FILE *, struct ObjRec **));
extern void	ChangeDomain ARGS_DECL((void));
extern void	AdjForOldVersion ARGS_DECL((struct ObjRec *));
extern int	ImportGivenFile ARGS_DECL((char*));
extern void	ImportFile ARGS_DECL((void));
extern int	LoadFile ARGS_DECL((char *FileName, int ObjFile));
extern void	DumpPatFill ARGS_DECL((FILE *, int Fill, int CellSize,
		                       struct BBRec, char *Blanks));
extern void	DumpSymOutline ARGS_DECL((FILE *, struct ObjRec *));
extern int	DumpBBox ARGS_DECL((FILE *, int PageOnly, struct BBRec *));
extern void	ModifyOutputFileName ARGS_DECL((char *));
extern void	SetBopHook ARGS_DECL((char *));
extern void	SetEopHook ARGS_DECL((char *));
extern void	Dump ARGS_DECL((char *));
extern void	DumpOnePageInTileMode ARGS_DECL((int Row, int Col));
extern void	DumpOnePageInStackMode ARGS_DECL((void));
extern void	DumpOneFilePerPage ARGS_DECL((void));
extern void	PrintWithCommand ARGS_DECL((char *));
extern void	PrintSelectedObjs ARGS_DECL((void));
extern void	SetPrintReduction ARGS_DECL((void));
extern void	NewProc ARGS_DECL((void));
extern void	OpenProc ARGS_DECL((void));
extern void	SetTemplate ARGS_DECL((void));
extern int	QuitProc ARGS_DECL((void));
extern int	SolveProc ARGS_DECL((void));
extern int	SimulateProc ARGS_DECL((void));
extern int	ProbeProc ARGS_DECL((void));
extern int	AnimateProc ARGS_DECL((void));
extern int	EscapeProc ARGS_DECL((void));
extern int	FileSubMenu ARGS_DECL((int Index));
extern int	FileMenu ARGS_DECL((int X, int Y, int TrackMenubar));
extern void	CleanUpFiles ARGS_DECL((void));

#endif /*_FILE_E_*/
