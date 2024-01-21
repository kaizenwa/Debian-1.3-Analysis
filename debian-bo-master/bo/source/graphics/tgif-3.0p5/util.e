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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/util.e,v 3.0 1996/05/06 16:12:46 william Exp $
 */

#ifndef _TGIF_UTIL_E_
#define _TGIF_UTIL_E_

extern void	UtilFree ARGS_DECL((char *lpszStr));
extern char	*UtilStrDup ARGS_DECL((char *lpszStr));
extern int	UtilStrCpy ARGS_DECL((char *lpszDest, int nMaxDestSz,
		                      char *lpszSrc));
extern void	UtilTrimBlanks ARGS_DECL((char *lpszStr));
extern int	UtilStrNCaseCmp ARGS_DECL((char *pszStr1, char *pszStr2,
		                           int nCount));
extern int	UtilStrICmp ARGS_DECL((char *pszStr1, char *pszStr2));
extern char	*UtilStrRChr ARGS_DECL((char *pszStr, int int_ch));
#ifdef NO_STRSTR
extern char	*strstr ARGS_DECL((char *pszStr, char *pszSubStr));
#endif /* NO_STRSTR */
extern int	UtilShrinkName ARGS_DECL((char *lpszFile));
extern void	UtilRemoveQuotes ARGS_DECL((char *pszStr));
extern char	*UtilGetALine ARGS_DECL((FILE *lpFile));
extern char	*UtilGetAContinuedLine ARGS_DECL((FILE *lpFile));
extern int	UtilCopyFile ARGS_DECL((char *pszFromFile, char *pszToFile));

#endif /*_TGIF_UTIL_E_*/
