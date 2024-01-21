/*
 * Code in this file is derived from the public domain code in
 *              WWW/Library/Implementation/HTFTP.c distributed with lynx-2.2,
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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/ftp.c,v 3.0 1996/05/06 16:05:17 william Exp $";
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifdef isc /* SunSoft/Interactive UNIX */
#include <net/errno.h>
#endif

#include "const.h"

#ifndef _NO_EXTERN
#include "ftp.e"
#endif /* !_NO_EXTERN */
#include "remote.e"
#include "tcp.e"
#include "util.e"

static int debugFtp=FALSE;
static int gnReadyToReceiveData=FALSE;

void FtpFreeBuf(buf)
   char *buf;
{
   free(buf);
}

void FtpDebug(val)
   int val;
{
   debugFtp = val;
}

int FtpDoConnect(psz_host, us_port, pn_socket)
   char *psz_host;
   int us_port, *pn_socket;
{
   int rc, len=strlen(psz_host)+80;
   char *msg=(char*)malloc((len+1)*sizeof(char));

   if (msg == NULL) {
      fprintf(stderr, "Memory allocation failed.\n");
      return TG_REMOTE_STATUS_MEM;
   }
   sprintf(msg, "Making an FTP connection to \"%s\"...", psz_host);
   ShowRemoteStatus(msg);

   rc = TcpDoConnect(psz_host, us_port, pn_socket);

   if (rc == TG_REMOTE_STATUS_OK) {
      sprintf(msg, "FTP: connection to \"%s\" established.", psz_host);
   } else {
      sprintf(msg, "Fail to connect to FTP server on \"%s\".", psz_host);
   }
   ShowRemoteStatus(msg);
   free(msg);

   return rc;
}

static
char *AppendSimpleString(buf, value)
   char *buf, *value;
{
   int cur_len=(buf==NULL ? 0 : strlen(buf));

   if (value == NULL) {
      int new_len=cur_len+2;

      if (buf == NULL) {
         buf = (char*)malloc((new_len+1)*sizeof(char));
      } else {
         buf = (char*)realloc(buf, new_len+1);
      }
      if (buf == NULL) return NULL;
      sprintf(&buf[cur_len], "\r\n");
   } else {
      int new_len=cur_len+strlen(value)+2;

      if (buf == NULL) {
         buf = (char*)malloc((new_len+1)*sizeof(char));
      } else {
         buf = (char*)realloc(buf, new_len+1);
      }
      if (buf == NULL) return NULL;
      sprintf(&buf[cur_len], "%s\r\n", value);
   }
   return buf;
}

int FtpDoWrite(n_socket, psz_path)
   int n_socket;
   char *psz_path;
{
   int status=TG_REMOTE_STATUS_OK;

   if (psz_path == NULL) return TG_REMOTE_STATUS_OK;

   status = TcpDoWrite(n_socket, psz_path, (int)strlen(psz_path));
   return status;
}

static
void FtpDumpResponse(func_name, buf)
   char *func_name, *buf;
{
   char *c_ptr=strchr(buf, '\n'), *line_ptr=buf;
   FILE *fp=stdout;

   if (debugFtp) fprintf(fp, "In %s:\n", func_name);
   while (c_ptr != NULL) {
      char *prev_ptr=c_ptr;

      if (prev_ptr != line_ptr && *(--prev_ptr) == '\r') {
         *prev_ptr = '\0';
      } else {
         prev_ptr = NULL;
         *c_ptr = '\0';
      }
      if (debugFtp) fprintf(fp, "  %s\n", line_ptr);
      if (prev_ptr == NULL) {
         *c_ptr = '\n';
      } else {
         *prev_ptr = '\r';
      }
      line_ptr = &c_ptr[1];
      c_ptr = strchr(line_ptr, '\n');
   }
   if (line_ptr != NULL) {
      int len=strlen(line_ptr);

      if (len > 0 && line_ptr[len-1] == '\r') {
         line_ptr[len-1] = '\0';
         if (debugFtp) fprintf(fp, "  %s\n", line_ptr);
         line_ptr[len-1] = '\r';
      } else {
         if (debugFtp) fprintf(fp, "  %s\n", line_ptr);
      }
   }
}

#define MIN_READ_SIZE 0x100

static int ftpReadData=FALSE;

int FtpDoRead(n_socket, ppsz_buf, pn_buf_sz)
   int n_socket, *pn_buf_sz;
   char **ppsz_buf;
{
   int buf_sz=0x400, len=0, end_of_file=FALSE;
   int rc=(-1);
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
      if (debugFtp >= 3) fprintf(stderr, "      read %1d bytes\n", bytes_read);
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
      if (!end_of_file && UserAbortComm()) {
         if (buf != NULL) free(buf);
         ShowRemoteStatus("FTP: aborted by the user.");
         return TG_REMOTE_STATUS_INTR;
      } else {
         char msg[40], *c_ptr;
         int cont_code=(-1);

         sprintf(msg, "FTP: %1d bytes...", len);
         ShowRemoteStatus(msg);
         buf[len] = '\0';
         if (!ftpReadData) {
            for (c_ptr=buf; *c_ptr != '\0'; c_ptr++) {
               char cont_ch;
            
               if (sscanf(c_ptr, "%d%c", &rc, &cont_ch) == 2) {
                  if (cont_code == (-1)) {
                     if (cont_ch == '-') {
                        cont_code = rc;
                     } else {
                        end_of_file = TRUE;
                        break;
                     }
                  } else if (cont_code == rc && cont_ch == ' ') {
                     cont_code = (-1);
                     end_of_file = TRUE;
                     break;
                  }
               }
               c_ptr = strchr(c_ptr, '\n');
               if (c_ptr == NULL) break;
            }
         }
      }
   } while (!end_of_file);
   if (debugFtp >= 2) fprintf(stderr, "    rc = %1d in FtpDoRead().\n", rc);
   if (rc == 421) {
      free(buf);
      fprintf(stderr, "Connection terminated by the FTP server.\n");
      return TG_REMOTE_STATUS_TERM;
   } else {
      buf[len] = '\0';
      *ppsz_buf = buf;
      if (pn_buf_sz != NULL) *pn_buf_sz = (len+1);
      return TG_REMOTE_STATUS_OK;
   }
}

static
int FtpSendSimpleCmd(n_socket, value)
   int n_socket;
   char *value;
{
   int status=TG_REMOTE_STATUS_OK;
   char *cmd;

   if ((cmd=AppendSimpleString(NULL, value)) == NULL) {
      fprintf(stderr, "Memory allocation failed.\n");
      return TG_REMOTE_STATUS_MEM;
   }
   status = FtpDoWrite(n_socket, cmd);
   free(cmd);
   return status;
}

static
int FtpSendUserName(n_socket, value)
   int n_socket;
   char *value;
{
   return FtpSendSimpleCmd(n_socket, value);
}

static
int FtpSendPassword(n_socket, password)
   int n_socket;
   char *password;
{
   return FtpSendSimpleCmd(n_socket, password);
}

static
int FtpSendRetrieveCmd(n_socket, psz_path)
   int n_socket;
   char *psz_path;
{
   int status=TG_REMOTE_STATUS_OK;
   char *cmd;

   cmd = (char*)malloc((strlen("RETR")+1+strlen(psz_path)+2+1)*sizeof(char));
   if (cmd == NULL) {
      fprintf(stderr, "Memory allocation failed.\n");
      return TG_REMOTE_STATUS_MEM;
   }
   sprintf(cmd, "RETR %s\r\n", psz_path);
   status = FtpDoWrite(n_socket, cmd);
   free(cmd);
   return status;
}

static
int FtpSendListCmd(n_socket)
   int n_socket;
{
   return FtpSendSimpleCmd(n_socket, "NLST");
}

static
int FtpSendCWDCmd(n_socket, psz_path)
   int n_socket;
   char *psz_path;
{
   int status=TG_REMOTE_STATUS_OK;
   char *cmd;

   cmd = (char*)malloc((strlen("CWD")+1+strlen(psz_path)+2+1)*sizeof(char));
   if (cmd == NULL) {
      fprintf(stderr, "Memory allocation failed.\n");
      return TG_REMOTE_STATUS_MEM;
   }
   sprintf(cmd, "CWD %s\r\n", psz_path);
   status = FtpDoWrite(n_socket, cmd);
   free(cmd);
   return status;
}

static
int FtpSendPortCmd(n_socket, pn_data_socket)
   int n_socket, *pn_data_socket;
{
   struct sockaddr_in soc_address;
   struct sockaddr_in *sin=(&soc_address);
   char port_cmd[20];

   *pn_data_socket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
   if (*pn_data_socket < 0) {
      fprintf(stderr, "Fail to open a socket for FTP data port.\n");
      return TG_REMOTE_STATUS_FILE;
   }

   sin->sin_family = AF_INET;
   sin->sin_addr.s_addr = INADDR_ANY;
   {
      int rc, addr_sz;

      addr_sz = sizeof(soc_address);
      rc = getsockname(n_socket, (struct sockaddr *)&soc_address,
            &addr_sz);
      if (rc < 0) {
         close(*pn_data_socket);
         *pn_data_socket = (-1);
         return TG_REMOTE_STATUS_HOST;
      }
      soc_address.sin_port = 0;
      rc = bind(*pn_data_socket, (struct sockaddr*)&soc_address,
            sizeof(soc_address));
      if (rc < 0) {
         close(*pn_data_socket);
         *pn_data_socket = (-1);
         return TG_REMOTE_STATUS_HOST;
      }
      addr_sz = sizeof(soc_address);
      rc = getsockname(*pn_data_socket, (struct sockaddr *)&soc_address,
            &addr_sz);
      if (rc < 0) {
         close(*pn_data_socket);
         *pn_data_socket = (-1);
         return TG_REMOTE_STATUS_HOST;
      }
   }
   sprintf(port_cmd, "PORT %d,%d,%d,%d,%d,%d",
         (int)*((unsigned char *)(&sin->sin_addr)+0),
         (int)*((unsigned char *)(&sin->sin_addr)+1),
         (int)*((unsigned char *)(&sin->sin_addr)+2),
         (int)*((unsigned char *)(&sin->sin_addr)+3),
         (int)*((unsigned char *)(&sin->sin_port)+0),
         (int)*((unsigned char *)(&sin->sin_port)+1));
   if (listen(*pn_data_socket, 1) < 0) {
      close(*pn_data_socket);
      *pn_data_socket = (-1);
      return TG_REMOTE_STATUS_HOST;
   }
   return FtpSendSimpleCmd(n_socket, port_cmd);
}

static
int FtpReadCmd(n_socket, ppsz_buf, pn_cmd)
   int n_socket, *pn_cmd;
   char **ppsz_buf;
   /* if returns TG_REMOTE_STATUS_OK, caller must call FtpFreeBuf(*ppsz_buf) */
{
   int status;

   if ((status=FtpDoRead(n_socket, ppsz_buf, NULL)) == TG_REMOTE_STATUS_OK) {
      status = TG_REMOTE_STATUS_HOST;
      if (*ppsz_buf != NULL) {
         if (sscanf(*ppsz_buf, "%d", pn_cmd) == 1) {
            *pn_cmd = (int)(*pn_cmd / 100);
            status = TG_REMOTE_STATUS_OK;
         }
      }
   }
   if (status != TG_REMOTE_STATUS_OK && *ppsz_buf != NULL) {
      FtpFreeBuf(*ppsz_buf);
      *ppsz_buf = NULL;
   }
   return status;
}

static
int FtpLogin(n_socket)
   int n_socket;
{
   char *buf=NULL;
   int status, ftp_cmd=(-1);

   if ((status=FtpReadCmd(n_socket, &buf, &ftp_cmd)) == TG_REMOTE_STATUS_OK) {
      FtpDumpResponse("FtpLogin", buf);
      status = TG_REMOTE_STATUS_FORMAT;
      if (ftp_cmd == 2) {
         status = FtpSendUserName(n_socket, "USER anonymous");
      }
      FtpFreeBuf(buf);
   }
   return status;
}

static char SZ_PASSWORD[128];

static int gnPasswordInitialized=FALSE;

static
void InitPassword()
{
   char user_name[128];

   if (gnPasswordInitialized) return;
   gnPasswordInitialized = TRUE;

   GetUserID(user_name, sizeof(user_name));

   sprintf(SZ_PASSWORD, "PASS %s", user_name);
}

static
int FtpPassword(n_socket)
   int n_socket;
{
   char *buf=NULL;
   int status, ftp_cmd=(-1);

   if ((status=FtpReadCmd(n_socket, &buf, &ftp_cmd)) == TG_REMOTE_STATUS_OK) {
      FtpDumpResponse("FtpPassword", buf);
      status = TG_REMOTE_STATUS_FORMAT;
      if (ftp_cmd == 3) {
         InitPassword();
         status = FtpSendPassword(n_socket, SZ_PASSWORD);
      }
      FtpFreeBuf(buf);
   }
   return status;
}

static
int FtpPort(n_socket, pn_data_socket)
   int n_socket, *pn_data_socket;
{
   char *buf=NULL;
   int status, ftp_cmd=(-1);

   if ((status=FtpReadCmd(n_socket, &buf, &ftp_cmd)) == TG_REMOTE_STATUS_OK) {
      FtpDumpResponse("FtpPort", buf);
      status = TG_REMOTE_STATUS_FORMAT;
      if (ftp_cmd == 2) {
         status = FtpSendPortCmd(n_socket, pn_data_socket);
      } else if (ftp_cmd == 3) {
         FtpFreeBuf(buf);
         status = FtpSendPassword(n_socket, "ACCT noaccount");
         if ((status=FtpReadCmd(n_socket, &buf, &ftp_cmd)) ==
               TG_REMOTE_STATUS_OK) {
            FtpDumpResponse("FtpPort", buf);
            status = TG_REMOTE_STATUS_FORMAT;
            if (ftp_cmd == 2) {
               status = FtpSendPortCmd(n_socket, pn_data_socket);
            }
            FtpFreeBuf(buf);
            buf = NULL;
         }
      }
      if (buf != NULL) FtpFreeBuf(buf);
   }
   return status;
}

static
int FtpRetr(n_socket, psz_path)
   int n_socket;
   char *psz_path;
{
   char *buf=NULL;
   int status, ftp_cmd=(-1);

   if ((status=FtpReadCmd(n_socket, &buf, &ftp_cmd)) == TG_REMOTE_STATUS_OK) {
      FtpDumpResponse("FtpRetr", buf);
      status = TG_REMOTE_STATUS_FORMAT;
      if (ftp_cmd == 2) {
         status = FtpSendRetrieveCmd(n_socket, psz_path);
      }
      FtpFreeBuf(buf);
   }
   return status;
}

static
int FtpCwd(n_socket, psz_path, pn_is_dir)
   int n_socket, *pn_is_dir;
   char *psz_path;
{
   char *buf=NULL;
   int status, ftp_cmd=(-1);

   *pn_is_dir = FALSE;
   if ((status=FtpReadCmd(n_socket, &buf, &ftp_cmd)) == TG_REMOTE_STATUS_OK) {
      FtpDumpResponse("FtpCwd", buf);
      status = TG_REMOTE_STATUS_FORMAT;
      if (ftp_cmd != 1) {
         FtpFreeBuf(buf);
         status = FtpSendCWDCmd(n_socket, psz_path);
         if ((status=FtpReadCmd(n_socket, &buf, &ftp_cmd)) ==
               TG_REMOTE_STATUS_OK) {
            FtpDumpResponse("FtpCwd", buf);
            if (ftp_cmd == 2) {
               *pn_is_dir = TRUE;
               gnReadyToReceiveData = TRUE;
               status = FtpSendListCmd(n_socket, psz_path);
            }
            FtpFreeBuf(buf);
            buf = NULL;
         }
      } else {
         gnReadyToReceiveData = TRUE;
         status = TG_REMOTE_STATUS_OK;
      }
      if (buf != NULL) FtpFreeBuf(buf);
   }
   return status;
}

static
int FtpGetContent(n_socket, data_socket, is_dir, ppsz_buf, pn_buf_sz)
   int n_socket, data_socket, is_dir, *pn_buf_sz;
   char **ppsz_buf;
{
   struct sockaddr_in soc_address;
   int soc_addrlen=sizeof(soc_address), accepted_socket;
   int status=TG_REMOTE_STATUS_OK;

   if (pn_buf_sz != NULL) *pn_buf_sz = 0;
   *ppsz_buf = NULL;
   accepted_socket = accept(data_socket, (struct sockaddr *)&soc_address,
         &soc_addrlen);
   if (accepted_socket < 0) return TG_REMOTE_STATUS_HOST;

   ftpReadData = TRUE;
   if (is_dir) {
      /* a directory */
      status = FtpDoRead(accepted_socket, ppsz_buf, pn_buf_sz);
   } else {
      /* a file */
      status = FtpDoRead(accepted_socket, ppsz_buf, pn_buf_sz);
   }
   ftpReadData = FALSE;
   close(accepted_socket);
   return TG_REMOTE_STATUS_OK;
}

int FtpDoTalk(n_socket, psz_path, ppsz_buf, pn_buf_sz)
   int n_socket, *pn_buf_sz;
   char *psz_path, **ppsz_buf;
{
   int status=TG_REMOTE_STATUS_OK, data_socket=(-1), is_dir=FALSE;
   char msg[80];

   *ppsz_buf = NULL;
   if ((status=FtpLogin(n_socket)) != TG_REMOTE_STATUS_OK) return status;
   if ((status=FtpPassword(n_socket)) != TG_REMOTE_STATUS_OK) return status;
   if ((status=FtpPort(n_socket, &data_socket)) != TG_REMOTE_STATUS_OK) {
      return status;
   }
   if ((status=FtpRetr(n_socket, psz_path)) != TG_REMOTE_STATUS_OK) {
      if (data_socket != (-1)) close(data_socket);
      return (FTP_LOGGED_IN|status);
   }
   gnReadyToReceiveData = FALSE;
   if ((status=FtpCwd(n_socket, psz_path, &is_dir)) != TG_REMOTE_STATUS_OK) {
      if (data_socket != (-1)) close(data_socket);
      return (FTP_LOGGED_IN|status);
   }
   if (gnReadyToReceiveData) {
      sprintf(msg, "FTP: login successful; retrieving data...");
      ShowRemoteStatus(msg);

      status = FtpGetContent(n_socket, data_socket, is_dir, ppsz_buf,
            pn_buf_sz);
   }
   if (data_socket != (-1)) close(data_socket);
   return (FTP_LOGGED_IN|status);
}

