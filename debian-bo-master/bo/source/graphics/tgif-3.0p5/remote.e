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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/remote.e,v 3.0 1996/05/06 16:07:16 william Exp $
 */

#ifndef _TGIF_REMOTE_E_
#define _TGIF_REMOTE_E_

extern int	postingCGIQuery;
extern char	*fnameForPostingCGIQuery;

extern int	GetClientID ARGS_DECL((char *psz_buf, int buf_sz));
extern int	GetUserID ARGS_DECL((char *psz_buf, int buf_sz));
extern int	UserAbortComm ARGS_DECL((void));
extern int	GetPageNumFromPageSpec ARGS_DECL((char *psz_spec,
		                                  int *pn_page_num));
extern void	FreeRemoteBuf ARGS_DECL((char*));
extern int	DirIsRemote ARGS_DECL((char*));
extern int	FileIsRemote ARGS_DECL((char*));
extern int	UrlIsHtml ARGS_DECL((char*));
extern int	FormRemoteName ARGS_DECL((char *psz_file, char *psz_def_ext,
		                          char *psz_return));
extern int	FormNewFileName ARGS_DECL((char *psz_dir, char *psz_file,
		                           char *psz_def_ext, char *psz_return,
		                           char **ppsz_page_spec));
extern void	ShowRemoteStatus ARGS_DECL((char*));
extern int	LoadRemoteFileInMem ARGS_DECL((char *url, char **ppsz_buf,
		                               char **ppsz_content_type,
		                               int *pn_buf_sz, int *pn_html,
		                               int force_load));
extern char	*WriteRemoteFileIntoTemp ARGS_DECL((char *psz_buf, int buf_sz,
		                                    char *psz_ext));
extern int	UseExternalViewer ARGS_DECL((int is_html, char *psz_url,
		                             char *psz_content_type,
		                             char *tmp_fname));
extern int	LoadRemoteFileFromMem ARGS_DECL((char *psz_url, char *psz_buf,
		                                 char *psz_content_type,
		                                 int buf_sz, int is_html));
extern void	InitRemote ARGS_DECL((void));
extern void	CleanUpRemote ARGS_DECL((void));

#endif /*_TGIF_REMOTE_E_*/
