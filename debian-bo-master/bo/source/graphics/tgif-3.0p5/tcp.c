/*
 * Code in this file is derived from the public domain code in
 *              WWW/Library/Implementation/HTTCP.c distributed with lynx-2.2,
 *              whose original author is Tim Berners-lee <timbl@info.cern.ch>.
 *
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
 */
#ifndef lint
static char RCSid[] =
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/tcp.c,v 3.0 1996/05/06 16:12:07 william Exp $";
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netdb.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#ifdef isc /* SunSoft/Interactive UNIX */
#include <net/errno.h>
#include <sys/bsdtypes.h>
#endif

#ifndef O_NONBLOCK
#include <sys/ioctl.h>
#endif /* ~O_NONBLOCK */

#if !defined(FD_SET)
#include <sys/select.h>
#endif /* !defined(FD_SET) */

#include "const.h"

#include "remote.e"
#ifndef _NO_EXTERN
#include "tcp.e"
#endif /* !_NO_EXTERN */
#include "util.e"

void SetSocketBlockingState(pn_socket, n_blocking)
   int *pn_socket, n_blocking;
{
   int rc;
#ifdef O_NONBLOCK
   int flags = fcntl(*pn_socket, F_GETFL);

   rc = fcntl(*pn_socket, F_SETFL,
            n_blocking ? (flags & (~O_NONBLOCK)) : (flags | O_NONBLOCK));
#else /* ~O_NONBLOCK */
   int val=(!n_blocking);

   rc = ioctl(*pn_socket, FIONBIO, &val);
#endif /* O_NONBLOCK */
   if (rc == (-1)) {
      fprintf(stderr, "Could not make connection %s.\n",
            (n_blocking ? "blocking" : "non-blocking"));
   }
}

void TcpFreeBuf(buf)
   char *buf;
{
   free(buf);
}

static gnPipeBroken=FALSE;

static
void BrokenPipe(nSig)
   int nSig;
{
   if (nSig == SIGPIPE) {
      gnPipeBroken = TRUE;
      signal(SIGPIPE, SIG_DFL);
   }
}

int TcpDoConnect(psz_host, us_port, pn_socket)
   char *psz_host;
   int us_port, *pn_socket;
{
   static int not_initialized=TRUE;
   struct sockaddr_in soc_address;
   struct sockaddr_in *sin=(&soc_address);
   struct hostent *p_hostent=NULL;
   int status=TG_REMOTE_STATUS_OK;

   if (not_initialized) {
      not_initialized = FALSE;
      signal(SIGPIPE, BrokenPipe);
   }
   if (*psz_host >= '0' && *psz_host <= '9') {
      sin->sin_addr.s_addr = inet_addr(psz_host);
   } else {
      p_hostent = gethostbyname(psz_host);
      if (p_hostent == NULL) {
         fprintf(stderr, "Cannot find Internet node name.\n");
         return TG_REMOTE_STATUS_HOST;
      }
      memcpy(&sin->sin_addr, p_hostent->h_addr, p_hostent->h_length);
   }
   sin->sin_family = AF_INET;
   sin->sin_port = htons((unsigned short)us_port);
   *pn_socket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

   SetSocketBlockingState(pn_socket, FALSE);

   status = connect(*pn_socket, (struct sockaddr*)&soc_address,
         sizeof(soc_address));
#ifdef SVR4
   if ((status < 0) && (errno==EINPROGRESS || errno==EAGAIN)) {
#else /* ~SVR4 */
   if ((status < 0) && (errno == EINPROGRESS)) {
#endif /* SVR4 */
      struct timeval timeout;
      int rc=0;

      timeout.tv_sec = 0;
      timeout.tv_usec = 100000;
      while (rc <= 0) {
         fd_set writefds;

         FD_ZERO(&writefds);
         FD_SET(*pn_socket, &writefds);
#ifdef __hpux
         rc = select(FD_SETSIZE, NULL, (int*)&writefds, NULL, &timeout);
#else /* ~__hpux */
         rc = select(FD_SETSIZE, NULL, &writefds, NULL, &timeout);
#endif /* __hpux */
         if ((rc < 0)&&(errno != EALREADY)) {
            status = rc;
            break;
         } else if (rc > 0) {
            gnPipeBroken = FALSE;
            status = connect(*pn_socket, (struct sockaddr*)&soc_address,
                  sizeof(soc_address));
            if (gnPipeBroken) {
               fprintf(stderr, "Broken pipe while contacting '%s'.\n",
                     psz_host);
            }
            if ((status < 0)&&(errno == EISCONN)) status = TG_REMOTE_STATUS_OK;
            if (errno == EALREADY)
               rc = 0;
            else
               break;
         } else {
            status = connect(*pn_socket, (struct sockaddr*)&soc_address,
                  sizeof(soc_address));
#ifdef SVR4
            if ((status < 0) && (errno != EALREADY) && (errno != EISCONN) &&
                  (errno != EAGAIN))
#else /* ~SVR4 */
            if ((status < 0) && (errno != EALREADY) && (errno != EISCONN))
#endif /* SVR4 */
               break;
         }
         if (UserAbortComm()) {
            status = TG_REMOTE_STATUS_INTR;
            errno = EINTR;
            break;
         }
      }
   }
   if (status >= 0) {
      SetSocketBlockingState(pn_socket, TRUE);
   } else {
      close(*pn_socket);
   }
   return status;
}

int TcpDoWrite(n_socket, buf, buf_sz)
   int n_socket, buf_sz;
   char *buf;
{
   int status=0;

   if (buf == NULL) return TG_REMOTE_STATUS_OK;

   status = write(n_socket, buf, (int)buf_sz);
   if (status <= 0) {
      if (status == 0) {
         fprintf(stderr, "Get status 0 on initial network write.\n");
      } else if ((errno == ENOTCONN || errno == ECONNRESET || errno == EPIPE)) {
         fprintf(stderr, "Unexpected network write error.\n");
         return TG_REMOTE_STATUS_WRITE;
      }
   }
   return TG_REMOTE_STATUS_OK;
}

#define MIN_READ_SIZE 0x100

int TcpDoRead(n_socket, ppsz_buf, pn_buf_sz)
   int n_socket, *pn_buf_sz;
   char **ppsz_buf;
{
   int buf_sz=0x400, len=0, end_of_file=FALSE;
   char *buf=(char*)malloc(buf_sz*sizeof(char));

   if (pn_buf_sz != NULL) *pn_buf_sz = 0;
   *ppsz_buf = NULL;
   if (buf == NULL) {
      fprintf(stderr, "Memory allocation failed.\n");
      return TG_REMOTE_STATUS_MEM;
   }
   do {
      int bytes_read;

      if (buf_sz - len < MIN_READ_SIZE) {
         buf_sz += 0x400;
         if ((buf=(char*)realloc(buf, buf_sz)) == NULL) {
            fprintf(stderr, "Memory allocation failed.\n");
            return TG_REMOTE_STATUS_MEM;
         }
      }
      bytes_read = read(n_socket, &buf[len], buf_sz-len-1);
      if (bytes_read <= 0) {
         if (bytes_read < 0 && (errno == ENOTCONN || errno == ECONNRESET ||
               errno == EPIPE)) {
            free(buf);
            fprintf(stderr, "Network read error.\n");
            return TG_REMOTE_STATUS_READ;
         } else if (bytes_read < 0) {
            free(buf);
            fprintf(stderr, "Network error.\n");
            return TG_REMOTE_STATUS_NET;
         }
         end_of_file = TRUE;
      } else {
         len += bytes_read;
      }
   } while (!end_of_file);
   buf[len] = '\0';
   *ppsz_buf = buf;
   if (pn_buf_sz != NULL) *pn_buf_sz = (len+1);
   return TG_REMOTE_STATUS_OK;
}

