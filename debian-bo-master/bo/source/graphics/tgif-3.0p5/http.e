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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/http.e,v 3.0 1996/05/06 16:05:33 william Exp $
 */

#ifndef _TGIF_HTTP_E_
#define _TGIF_HTTP_E_

extern void	HttpFreeBuf ARGS_DECL((char *buf));
extern void	HttpDebug ARGS_DECL((int));
extern char	*Base64Encode ARGS_DECL((char*));
extern char	*FindAuthorization ARGS_DECL((char *pszHost, int nPort,
		                             char *pszScheme, char *pszRealm));
extern void	CommitAuthorization ARGS_DECL((void));
extern void	ResetAuthorization ARGS_DECL((void));
extern int	SetAuthorization ARGS_DECL((char *pszHost, int nPort,
		                            char *pszScheme, char *pszRealm,
		                            char *pszAuth));
extern void	CleanUpHttp ARGS_DECL((void));
extern void	InitHttp ARGS_DECL((void));
extern char	*HttpHeaderGetVersion ARGS_DECL((void));
extern int	HttpHeaderGetResponseCode ARGS_DECL((void));
extern char	*HttpHeaderGetResponseStatus ARGS_DECL((void));
extern char	*HttpHeaderGetDate ARGS_DECL((void));
extern char	*HttpHeaderGetLocation ARGS_DECL((void));
extern char	*HttpHeaderGetWWWAuthentication ARGS_DECL((void));
extern char	*HttpHeaderGetContentEncoding ARGS_DECL((void));
extern char	*HttpHeaderGetContentType ARGS_DECL((void));
extern long	HttpHeaderGetContentLength ARGS_DECL((void));
extern char	*HttpHeaderGetOtherField ARGS_DECL((char*));

extern int	HttpDoConnect ARGS_DECL((char *psz_host, int us_port,
		                         int *pn_socket));
extern int	HttpDoWrite ARGS_DECL((int n_socket, char *psz_path));
extern void	HttpDumpHeader ARGS_DECL((void));
extern char	*HttpExtractText ARGS_DECL((char *buf, int *pn_buf_sz,
		                            int *pn_html,
		                            char **ppsz_content_type));
extern int	HttpDoRead ARGS_DECL((int n_socket, char **ppsz_buf,
		                      int *pn_buf_sz));

#endif /*_TGIF_HTTP_E_*/
