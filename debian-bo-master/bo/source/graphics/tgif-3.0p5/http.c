/*
 * Code in this file is derived from the public domain code in
 *              WWW/Library/Implementation/HTTP.c distributed with lynx-2.2,
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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/http.c,v 3.0 1996/05/06 16:05:31 william Exp $";
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
#include "patchlvl.h"

#ifndef _NO_EXTERN
#include "http.e"
#endif /* !_NO_EXTERN */
#include "remote.e"
#include "tcp.e"
#include "util.e"
#include "version.e"

typedef struct TgifHttpLineInfo {
   char *name;
   char *value;
   struct TgifHttpLineInfo *next;
} *TgifHttpLinePtr;

typedef struct TgifHttpHeaderInfo {
   char *version;
   int resp_code;
   char *resp_status;
   char *date;
   char *location;
   char *www_authenticate;
   char *content_encoding;
   char *content_type;
   long content_length;
   struct TgifHttpLineInfo *misc;
} *TgifHttpHeaderPtr;

typedef struct AuthInfo {
   char *host;
   int port;
   char *scheme; /* e.g., Basic */
   char *realm; /* e.g., WallyWorld */
   char *authorization; /* base64 encoded */
   struct AuthInfo *next, *prev;
} *AuthInfoPtr;

static struct AuthInfo *topAuthInfo=NULL, *botAuthInfo=NULL;
static struct AuthInfo curAuthorization;

static struct TgifHttpHeaderInfo tgifHttpHeaderInfo;

static char SZ_HTTP_VERSION[]="HTTP/1.0";
static char SZ_USER_AGENT[128];
static char SZ_USER_NAME[128];

static int debugHttp=0;

void HttpFreeBuf(buf)
   char *buf;
{
   free(buf);
}

void HttpDebug(val)
   int val;
{
   debugHttp = val;
}

static
void CleanUpAuthInfo()
{
   struct AuthInfo *next_pai;

   for ( ; topAuthInfo != NULL; topAuthInfo=next_pai) {
      next_pai = topAuthInfo->next;
      if (topAuthInfo->host != NULL) free(topAuthInfo->host);
      if (topAuthInfo->scheme != NULL) free(topAuthInfo->scheme);
      if (topAuthInfo->realm != NULL) free(topAuthInfo->realm);
      if (topAuthInfo->authorization != NULL) free(topAuthInfo->authorization);
      free(topAuthInfo);
   }
   botAuthInfo = NULL;
}

static char gszEncode[] =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static int gnDecode[] = {
   -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
   -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
   -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63,
   52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1,
   -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
   15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1,
   -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
   41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1
};

static
void DoBase64Encode(buf, buf_len, return_buf)
   char *buf, *return_buf;
   int buf_len;
{
   int return_buf_index=0;

   while (buf_len > 0) {
      switch (buf_len) {
      case 1:
         return_buf[0] = gszEncode[(int)((buf[0]>>2) & 0x3f)];
         return_buf[1] = gszEncode[(int)((buf[0]<<4) & 0x3f)];
         return_buf[2] = '=';
         return_buf[3] = '=';
         return_buf[4] = '\0';
         buf++;
         buf_len--;
         return;
      case 2:
         return_buf[0] = gszEncode[(int)((buf[0]>>2) & 0x3f)];
         return_buf[1] = gszEncode[(int)(((buf[0]<<4) | (buf[1]>>4)) & 0x3f)];
         return_buf[2] = gszEncode[(int)((buf[1]<<2) & 0x3f)];
         return_buf[3] = '=';
         return_buf[4] = '\0';
         buf += 2;
         buf_len -= 2;
         return;
      default: /* convert 3 bytes */
         return_buf[0] = gszEncode[(int)((buf[0]>>2) & 0x3f)];
         return_buf[1] = gszEncode[(int)(((buf[0]<<4) | (buf[1]>>4)) & 0x3f)];
         return_buf[2] = gszEncode[(int)(((buf[1]<<2) | (buf[2]>>6)) & 0x3f)];
         return_buf[3] = gszEncode[(int)(buf[2] & 0x3f)];
         return_buf[4] = '\0';
         buf += 3;
         buf_len -= 3;
         break;
      }
      return_buf += 4;
   }
}

char *Base64Encode(buf)
   char *buf;
{
   int len=strlen(buf), new_len;
   char *return_buf;

   new_len = (int)(len/3);
   new_len += 3;
   new_len <<= 2;
   return_buf = (char*)malloc(new_len*sizeof(char));
   if (return_buf == NULL) return NULL;
   DoBase64Encode(buf, len, return_buf);
   return return_buf;
}

static
void BubbleAuthInfoToTop(pai)
   struct AuthInfo *pai;
{
   if (pai == topAuthInfo) return;
   if (pai->next != NULL) {
      pai->next->prev = pai->prev;
   } else {
      botAuthInfo = pai->prev;
   }
   pai->prev->next = pai->next;
   pai->prev = NULL;
   pai->next = topAuthInfo;
   topAuthInfo = pai;
}

char *FindAuthorization(pszHost, nPort, pszScheme, pszRealm)
   char *pszHost, *pszScheme, *pszRealm;
   int nPort;
{
   struct AuthInfo *pai;

   for (pai=topAuthInfo; pai != NULL; pai=pai->next) {
      if (pai->port == nPort &&
            pai->host != NULL && UtilStrICmp(pai->host, pszHost) == 0 &&
            pai->scheme != NULL && UtilStrICmp(pai->scheme, pszScheme) == 0 &&
            pai->realm != NULL && UtilStrICmp(pai->realm, pszRealm) == 0) {
         BubbleAuthInfoToTop(pai);
         return pai->authorization;
      }
   }
   return NULL;
}

void CommitAuthorization()
{
   struct AuthInfo *pai=(struct AuthInfo *)malloc(sizeof(struct AuthInfo));

   if (pai == NULL) {
      FailAllocMessage();
      return;
   }
   memset(pai, 0, sizeof(struct AuthInfo));
   if (curAuthorization.host != NULL) {
      pai->host = UtilStrDup(curAuthorization.host);
   }
   if (curAuthorization.scheme != NULL) {
      pai->scheme = UtilStrDup(curAuthorization.scheme);
   }
   if (curAuthorization.realm != NULL) {
      pai->realm = UtilStrDup(curAuthorization.realm);
   }
   if (curAuthorization.authorization != NULL) {
      pai->authorization = UtilStrDup(curAuthorization.authorization);
   }
   pai->port = curAuthorization.port;
   if (topAuthInfo != NULL) {
      topAuthInfo->prev = pai;
   } else {
      botAuthInfo = pai;
   }
   pai->prev = NULL;
   pai->next = topAuthInfo;
   topAuthInfo = pai;
}

void ResetAuthorization()
{
   if (curAuthorization.host != NULL) free(curAuthorization.host);
   if (curAuthorization.scheme != NULL) free(curAuthorization.scheme);
   if (curAuthorization.realm != NULL) free(curAuthorization.realm);
   if (curAuthorization.authorization != NULL) {
      free(curAuthorization.authorization);
   }
   memset(&curAuthorization, 0, sizeof(struct AuthInfo));
}

int SetAuthorization(pszHost, nPort, pszScheme, pszRealm, pszEncodedAuth)
   char *pszHost, *pszScheme, *pszRealm, *pszEncodedAuth;
   int nPort;
{
   ResetAuthorization();
   curAuthorization.host = UtilStrDup(pszHost);
   curAuthorization.scheme = UtilStrDup(pszScheme);
   curAuthorization.realm = UtilStrDup(pszRealm);
   curAuthorization.authorization = UtilStrDup(pszEncodedAuth);
   curAuthorization.port = nPort;
   if (curAuthorization.host==NULL || curAuthorization.scheme==NULL ||
         curAuthorization.realm==NULL || curAuthorization.authorization==NULL) {
      ResetAuthorization();
      return FALSE;
   }
   return TRUE;
}

static
void CleanUpHttpHeaderInfo()
{
   struct TgifHttpLineInfo *pthli, *next_thli;

   if (tgifHttpHeaderInfo.version != NULL) free(tgifHttpHeaderInfo.version);
   if (tgifHttpHeaderInfo.resp_status != NULL) {
      free(tgifHttpHeaderInfo.resp_status);
   }
   if (tgifHttpHeaderInfo.date != NULL) free(tgifHttpHeaderInfo.date);
   if (tgifHttpHeaderInfo.location != NULL) free(tgifHttpHeaderInfo.location);
   if (tgifHttpHeaderInfo.www_authenticate != NULL) {
      free(tgifHttpHeaderInfo.www_authenticate);
   }
   if (tgifHttpHeaderInfo.content_encoding != NULL) {
      free(tgifHttpHeaderInfo.content_encoding);
   }
   if (tgifHttpHeaderInfo.content_type != NULL) {
      free(tgifHttpHeaderInfo.content_type);
   }
   for (pthli=tgifHttpHeaderInfo.misc; pthli != NULL; pthli=next_thli) {
      next_thli = pthli->next;
      if (pthli->name != NULL) free(pthli->name);
      if (pthli->value != NULL) free(pthli->value);
      free(pthli);
   }
   memset(&tgifHttpHeaderInfo, 0, sizeof(struct TgifHttpHeaderInfo));
}

void CleanUpHttp()
{
   ResetAuthorization();
   CleanUpAuthInfo();
   CleanUpHttpHeaderInfo();
}

void InitHttp()
{
   topAuthInfo = botAuthInfo = NULL;
   memset(&curAuthorization, 0, sizeof(struct AuthInfo));
   memset(&tgifHttpHeaderInfo, 0, sizeof(struct TgifHttpHeaderInfo));
}

char *HttpHeaderGetVersion() { return tgifHttpHeaderInfo.version; }

int HttpHeaderGetResponseCode() { return tgifHttpHeaderInfo.resp_code; }

char *HttpHeaderGetResponseStatus() { return tgifHttpHeaderInfo.resp_status; }

char *HttpHeaderGetDate() { return tgifHttpHeaderInfo.date; }

char *HttpHeaderGetLocation() { return tgifHttpHeaderInfo.location; }

char *HttpHeaderGetWWWAuthentication()
{
   return tgifHttpHeaderInfo.www_authenticate;
}

char *HttpHeaderGetContentEncoding()
{
   return tgifHttpHeaderInfo.content_encoding;
}

char *HttpHeaderGetContentType() { return tgifHttpHeaderInfo.content_type; }

long HttpHeaderGetContentLength() { return tgifHttpHeaderInfo.content_length; }

char *HttpHeaderGetOtherField(name)
   char *name;
{
   struct TgifHttpLineInfo *pthli;

   for (pthli=tgifHttpHeaderInfo.misc; pthli != NULL; pthli=pthli->next) {
      if (pthli->name != NULL && UtilStrICmp(pthli->name, name) == 0) {
         return pthli->value;
      }
   }
   return NULL;
}

static int gnUserAgentNameInitialized=FALSE;

static
void InitUserAgentName()
{
   if (gnUserAgentNameInitialized) return;
   gnUserAgentNameInitialized = TRUE;

   GetClientID(SZ_USER_AGENT, sizeof(SZ_USER_AGENT));
   GetUserID(SZ_USER_NAME, sizeof(SZ_USER_NAME));
}

int HttpDoConnect(psz_host, us_port, pn_socket)
   char *psz_host;
   int us_port, *pn_socket;
{
   int rc, len=strlen(psz_host)+80;
   char *msg=(char*)malloc((len+1)*sizeof(char));

   if (msg == NULL) {
      fprintf(stderr, "Memory allocation failed.\n");
      return TG_REMOTE_STATUS_MEM;
   }
   sprintf(msg, "Making an HTTP connection to \"%s:%1d\"...", psz_host,
         us_port);
   ShowRemoteStatus(msg);

   rc = TcpDoConnect(psz_host, us_port, pn_socket);

   if (rc == TG_REMOTE_STATUS_OK) {
      sprintf(msg, "HTTP: connection to \"%s:%1d\" established.", psz_host,
            us_port);
   } else {
      sprintf(msg, "Fail to connect to HTTP server on \"%s:%1d\".", psz_host,
            us_port);
   }
   ShowRemoteStatus(msg);
   free(msg);

   return rc;
}

static char SZ_CONTENT_TYPE[]="Content-type:";
static char SZ_CONTENT_LENGTH[]="Content-length:";
static char SZ_DEF_GIF_NAME[]="/tmp/htclient.gif";
static char SZ_POST_CONTENT_TYPE[]="application/x-www-form-urlencoded";

static char *pszAccept[] = {
   "text/plain",
   "text/html",
   "application/x-tgif",
   "*/*",
   NULL
};

static
char *AppendAcceptStrings(buf)
   char *buf;
{
   char **s_ptr;
   int cur_len=strlen(buf);

   for (s_ptr=pszAccept; *s_ptr != NULL; s_ptr++) {
      int len=strlen(*s_ptr);
      int new_len=cur_len+len+2+8;

      if ((buf=(char*)realloc(buf, new_len+1)) == NULL) {
         return NULL;
      }
      sprintf(&buf[cur_len], "Accept: %s\r\n", *s_ptr);
      cur_len = new_len;
   }
   return buf;
}

static
char *AppendSimpleString(buf, name, value)
   char *buf, *name, *value;
{
   int cur_len=strlen(buf);

   if (name == NULL && value == NULL) {
      int new_len=cur_len+2;

      if ((buf=(char*)realloc(buf, new_len+1)) == NULL) {
         return NULL;
      }
      sprintf(&buf[cur_len], "\r\n");
   } else {
      int new_len=cur_len+strlen(name)+2+strlen(value)+2;

      if ((buf=(char*)realloc(buf, new_len+1)) == NULL) {
         return NULL;
      }
      sprintf(&buf[cur_len], "%s: %s\r\n", name, value);
   }
   return buf;
}

static
char *AppendUserAgentString(buf)
   char *buf;
{
   InitUserAgentName();
   return AppendSimpleString(buf, "User-Agent", SZ_USER_AGENT);
}

static
char *AppendFromString(buf)
   char *buf;
{
   InitUserAgentName();
   return AppendSimpleString(buf, "From", SZ_USER_NAME);
}

static
char *AppendHostString(buf)
   char *buf;
{
   char *c_ptr;

   InitUserAgentName();
   if ((c_ptr=strchr(SZ_USER_NAME, '@')) == NULL) {
      return AppendSimpleString(buf, "Host", SZ_USER_NAME);
   } else {
      return AppendSimpleString(buf, "Host", ++c_ptr);
   }
}

static
char *AppendAuthorizationString(buf)
   char *buf;
{
   char *pszReturn;
   char *pszAuth=(char*)malloc(
         (strlen(curAuthorization.scheme)+
         strlen(curAuthorization.authorization)+2)*sizeof(char));

   if (pszAuth == NULL) return NULL;
   sprintf(pszAuth, "%s %s", curAuthorization.scheme,
         curAuthorization.authorization);
   pszReturn = AppendSimpleString(buf, "Authorization", pszAuth);
   free(pszAuth);
   return pszReturn;
}

static
char *AppendPostContentTypeString(buf)
   char *buf;
{
   return AppendSimpleString(buf, "Content-type", SZ_POST_CONTENT_TYPE);
}

static
char *AppendPostContentLengthString(buf, content_length)
   char *buf;
   int content_length;
{
   char len_str[20];

   sprintf(len_str, "%1d", content_length);
   return AppendSimpleString(buf, "Content-length", len_str);
}

static
char *AppendPostContentString(buf, fp, content_length)
   char *buf;
   FILE *fp;
   int content_length;
{
   int cur_len=strlen(buf), bytes_read, total_read=0;
   int new_len=cur_len+content_length;
   char tmp_buf[512];

   if ((buf=(char*)realloc(buf, new_len+1)) == NULL) {
      return NULL;
   }
   while ((bytes_read=fread(tmp_buf, sizeof(char), sizeof(tmp_buf),
         fp)) > 0) {
      if (bytes_read+total_read > content_length) {
         bytes_read = content_length-total_read;
         fprintf(stderr, "Lines too long in AppendPostContentString().\n");
      }
      strncpy(&buf[cur_len+total_read], tmp_buf, bytes_read);
      total_read += bytes_read;
   }
   buf[cur_len+content_length] = '\0';
   return buf;
}

static
char *AppendCRLFString(buf)
   char *buf;
{
   return AppendSimpleString(buf, NULL, NULL);
}

int HttpDoWrite(n_socket, psz_path)
   int n_socket;
   char *psz_path;
{
   int status=0, total_sz=0;
   FILE *fp=NULL;
   char *buf=(char*)malloc((strlen(psz_path)+5+2+31)*sizeof(char)), msg[40];

   if (buf == NULL) {
      fprintf(stderr, "Memory allocation failed.\n");
      return TG_REMOTE_STATUS_MEM;
   }
   if (postingCGIQuery) {
      sprintf(buf, "POST %s %s\r\n", psz_path, SZ_HTTP_VERSION);
   } else {
      sprintf(buf, "GET %s %s\r\n", psz_path, SZ_HTTP_VERSION);
   }
   if ((buf=AppendAcceptStrings(buf)) == NULL) {
      fprintf(stderr, "Memory allocation failed.\n");
      return TG_REMOTE_STATUS_MEM;
   }
   if ((buf=AppendUserAgentString(buf)) == NULL) {
      fprintf(stderr, "Memory allocation failed.\n");
      return TG_REMOTE_STATUS_MEM;
   }
   if ((buf=AppendFromString(buf)) == NULL) {
      fprintf(stderr, "Memory allocation failed.\n");
      return TG_REMOTE_STATUS_MEM;
   }
   if ((buf=AppendHostString(buf)) == NULL) {
      fprintf(stderr, "Memory allocation failed.\n");
      return TG_REMOTE_STATUS_MEM;
   }
   if (curAuthorization.scheme != NULL &&
         curAuthorization.authorization != NULL) {
      if ((buf=AppendAuthorizationString(buf)) == NULL) {
         fprintf(stderr, "Memory allocation failed.\n");
         return TG_REMOTE_STATUS_MEM;
      }
   }
   if (postingCGIQuery && fnameForPostingCGIQuery != NULL) {
      int bytes_read;
      char tmp_buf[512];

      if ((fp=fopen(fnameForPostingCGIQuery, "r")) == NULL) {
         fprintf(stderr, "Fail to open '%s' for read.\n",
               fnameForPostingCGIQuery);
         return TG_REMOTE_STATUS_READ;
      }
      while ((bytes_read=fread(tmp_buf, sizeof(char), sizeof(tmp_buf),
            fp)) > 0) {
         total_sz += bytes_read;
      }
      rewind(fp);
      if ((buf=AppendPostContentTypeString(buf)) == NULL) {
         fclose(fp);
         fprintf(stderr, "Memory allocation failed.\n");
         return TG_REMOTE_STATUS_MEM;
      }
      if ((buf=AppendPostContentLengthString(buf, total_sz)) == NULL) {
         fclose(fp);
         fprintf(stderr, "Memory allocation failed.\n");
         return TG_REMOTE_STATUS_MEM;
      }
   }
   if ((buf=AppendCRLFString(buf)) == NULL) {
      fprintf(stderr, "Memory allocation failed.\n");
      return TG_REMOTE_STATUS_MEM;
   }
   if (fp != NULL) {
      buf = AppendPostContentString(buf, fp, total_sz);
      fclose(fp);
      if (buf == NULL) {
         fprintf(stderr, "Memory allocation failed.\n");
         return TG_REMOTE_STATUS_MEM;
      }
   }
   sprintf(msg, "HTTP: sending requests...");
   ShowRemoteStatus(msg);

   status = TcpDoWrite(n_socket, buf, (int)strlen(buf));
   free(buf);

   if (status != TG_REMOTE_STATUS_OK) {
      sprintf(msg, "HTTP: fail to send requests.");
      ShowRemoteStatus(msg);
   }
   return status;
}

static
void HttpDumpResponse(buf)
   char *buf;
{
   char *c_ptr=strchr(buf, '\n'), *line_ptr=buf, *content_type=NULL;
   int content_length=(-1), gif87a=FALSE;
   int len1=strlen(SZ_CONTENT_TYPE), len2=strlen(SZ_CONTENT_LENGTH);
   FILE *fp=stdout;

   while (c_ptr != NULL) {
      char *prev_ptr=c_ptr;

      if (prev_ptr != line_ptr && *(--prev_ptr) == '\r') {
         *prev_ptr = '\0';
      } else {
         prev_ptr = NULL;
         *c_ptr = '\0';
      }
      fprintf(fp, "%s\n", line_ptr);
      if (content_type == NULL &&
            UtilStrNCaseCmp(line_ptr, SZ_CONTENT_TYPE, len1) == 0) {
         content_type = UtilStrDup(&line_ptr[len1]);
         if (content_type != NULL) {
            UtilTrimBlanks(content_type);
         }
      } else if (content_length == (-1) &&
            UtilStrNCaseCmp(line_ptr, SZ_CONTENT_LENGTH, len2) == 0) {
         char *tmp_ptr=UtilStrDup(&line_ptr[len2]);

         if (tmp_ptr != NULL) {
            UtilTrimBlanks(tmp_ptr);
            content_length = atoi(tmp_ptr);
            free(tmp_ptr);
         }
      }
      if (prev_ptr == NULL) {
         *c_ptr = '\n';
      } else {
         *prev_ptr = '\r';
      }
      line_ptr = &c_ptr[1];
      if (content_type != NULL && content_length != (-1)) {
         if (strcmp(content_type, "image/gif") == 0 &&
               UtilStrNCaseCmp(line_ptr, "GIF87a", 6) == 0) {
            gif87a = TRUE;
            break;
         }
      }
      c_ptr = strchr(line_ptr, '\n');
   }
   if (gif87a) {
      FILE *gif_fp;

      if ((gif_fp=fopen(SZ_DEF_GIF_NAME, "w")) == NULL) {
         fprintf(stderr, "Fail to open '%s' for write.\n", SZ_DEF_GIF_NAME);
      } else {
         if (fwrite(line_ptr, sizeof(char), content_length, gif_fp) !=
               content_length) {
            fprintf(stderr, "Write to '%s' failed.\n", SZ_DEF_GIF_NAME);
            fprintf(stderr, "Fail system may be full.\n");
         } else {
            fprintf(stderr, "GIF87a image written to '%s'.\n", SZ_DEF_GIF_NAME);
         }
         fclose(gif_fp);
      }
   } else if (line_ptr != NULL) {
      int len=strlen(line_ptr);

      if (len > 0 && line_ptr[len-1] == '\r') {
         line_ptr[len-1] = '\0';
         fprintf(fp, "%s\n", line_ptr);
         line_ptr[len-1] = '\r';
      } else {
         fprintf(fp, "%s\n", line_ptr);
      }
   }
}

static
int ParseBadFormat(buf)
   char *buf;
{
   fprintf(stderr, "Invalid format in the HTTP header.\n");
   if (buf != NULL) free(buf);
   return TG_REMOTE_STATUS_FORMAT;
}

static
int ParseNoMem(buf)
   char *buf;
{
   fprintf(stderr, "Memory allocation failed while parsing the HTTP header.\n");
   if (buf != NULL) free(buf);
   return TG_REMOTE_STATUS_MEM;
}

static
int HttpParseHeaderLine(buf, first_line)
   char *buf;
   int first_line;
{
   char *dup_buf, *colon_ptr;

   if (*buf == '\0') return TG_REMOTE_STATUS_OK;

   if (first_line) {
      char *version_ptr, *code_ptr, *status_ptr;

      dup_buf = UtilStrDup(buf);
      if (dup_buf == NULL) return ParseNoMem(NULL);;
      version_ptr = strtok(dup_buf, " \t\n\r");
      code_ptr = (version_ptr == NULL ? NULL : strtok(NULL, " \t\n\r"));
      status_ptr = (code_ptr == NULL ? NULL : strtok(NULL, " \t\n\r"));
      if (version_ptr == NULL) return ParseBadFormat(dup_buf);
      tgifHttpHeaderInfo.version = UtilStrDup(version_ptr);
      if (code_ptr == NULL) return ParseBadFormat(dup_buf);
      tgifHttpHeaderInfo.resp_code = atoi(code_ptr);
      if (status_ptr != NULL) {
         tgifHttpHeaderInfo.resp_status = UtilStrDup(status_ptr);
         if (tgifHttpHeaderInfo.resp_status == NULL) {
            return ParseBadFormat(dup_buf);
         }
      }
      free(dup_buf);
      return TG_REMOTE_STATUS_OK;
   }
   dup_buf = UtilStrDup(buf);
   if (dup_buf == NULL) return ParseNoMem(NULL);

   colon_ptr = strchr(dup_buf, ':');
   if (colon_ptr == NULL) return ParseBadFormat(dup_buf);
   *colon_ptr = '\0';
   UtilTrimBlanks(dup_buf);
   if (UtilStrICmp(dup_buf, "Date") == 0) {
      tgifHttpHeaderInfo.date = UtilStrDup(&colon_ptr[1]);
      if (tgifHttpHeaderInfo.date == NULL) return ParseNoMem(dup_buf);;
      UtilTrimBlanks(tgifHttpHeaderInfo.date);
   } else if (UtilStrICmp(dup_buf, "Location") == 0) {
      tgifHttpHeaderInfo.location = UtilStrDup(&colon_ptr[1]);
      if (tgifHttpHeaderInfo.location == NULL) return ParseNoMem(dup_buf);;
      UtilTrimBlanks(tgifHttpHeaderInfo.location);
   } else if (UtilStrICmp(dup_buf, "WWW-Authenticate") == 0) {
      tgifHttpHeaderInfo.www_authenticate = UtilStrDup(&colon_ptr[1]);
      if (tgifHttpHeaderInfo.www_authenticate == NULL) {
         return ParseNoMem(dup_buf);;
      }
      UtilTrimBlanks(tgifHttpHeaderInfo.www_authenticate);
   } else if (UtilStrICmp(dup_buf, "Content-Encoding") == 0) {
      tgifHttpHeaderInfo.content_encoding = UtilStrDup(&colon_ptr[1]);
      if (tgifHttpHeaderInfo.content_encoding == NULL) {
         return ParseNoMem(dup_buf);;
      }
      UtilTrimBlanks(tgifHttpHeaderInfo.content_encoding);
   } else if (UtilStrICmp(dup_buf, "Content-Type") == 0) {
      tgifHttpHeaderInfo.content_type = UtilStrDup(&colon_ptr[1]);
      if (tgifHttpHeaderInfo.content_type == NULL) return ParseNoMem(dup_buf);;
      UtilTrimBlanks(tgifHttpHeaderInfo.content_type);
   } else if (UtilStrICmp(dup_buf, "Content-Length") == 0) {
      char *length_ptr=(&colon_ptr[1]);

      UtilTrimBlanks(length_ptr);
      if (sscanf(length_ptr, "%ld", &tgifHttpHeaderInfo.content_length) != 1) {
         tgifHttpHeaderInfo.content_length = 0L;
      }
   } else {
      struct TgifHttpLineInfo *pthli;

      pthli = (struct TgifHttpLineInfo*)malloc(sizeof(struct TgifHttpLineInfo));
      if (pthli == NULL) return ParseNoMem(dup_buf);;
      memset(pthli, 0, sizeof(struct TgifHttpLineInfo));
      pthli->name = UtilStrDup(dup_buf);
      if (pthli->name == NULL) return ParseNoMem(dup_buf);;
      pthli->value = UtilStrDup(&colon_ptr[1]);
      if (pthli->value == NULL) return ParseNoMem(dup_buf);;
      pthli->next = tgifHttpHeaderInfo.misc;
      tgifHttpHeaderInfo.misc = pthli;
   }
   *colon_ptr = ':';
   free(dup_buf);
   return TG_REMOTE_STATUS_OK;
}

void HttpDumpHeader()
{
   struct TgifHttpLineInfo *pthli;

   if (tgifHttpHeaderInfo.version != NULL) {
      fprintf(stderr, "%s %1d", tgifHttpHeaderInfo.version,
            tgifHttpHeaderInfo.resp_code);
      if (tgifHttpHeaderInfo.resp_status != NULL) {
         fprintf(stderr, " %s", tgifHttpHeaderInfo.resp_status);
      }
      fprintf(stderr, "\n");
   }
   if (tgifHttpHeaderInfo.date != NULL) {
      fprintf(stderr, "Date: %s\n", tgifHttpHeaderInfo.date);
   }
   if (tgifHttpHeaderInfo.location != NULL) {
      fprintf(stderr, "Location: %s\n", tgifHttpHeaderInfo.location);
   }
   if (tgifHttpHeaderInfo.www_authenticate != NULL) {
      fprintf(stderr, "WWW-Authentication: %s\n",
            tgifHttpHeaderInfo.www_authenticate);
   }
   if (tgifHttpHeaderInfo.content_encoding != NULL) {
      fprintf(stderr, "Content-Encoding: %s\n",
            tgifHttpHeaderInfo.content_encoding);
   }
   if (tgifHttpHeaderInfo.content_type != NULL) {
      fprintf(stderr, "Content-Type: %s\n", tgifHttpHeaderInfo.content_type);
   }
   if (tgifHttpHeaderInfo.content_length != 0) {
      fprintf(stderr, "Content-Length: %ld\n",
            tgifHttpHeaderInfo.content_length);
   }
   for (pthli=tgifHttpHeaderInfo.misc; pthli != NULL; pthli=pthli->next) {
      fprintf(stderr, "%s: %s\n",
            (pthli->name == NULL ? "(unknown)" : pthli->name),
            (pthli->value == NULL ? "(none)" : pthli->value));
   }
}

char *HttpExtractText(buf, pn_buf_sz, pn_html, ppsz_content_type)
   char *buf, **ppsz_content_type;
   int *pn_buf_sz, *pn_html;
{
   char *c_ptr=strchr(buf, '\n'), *line_ptr=buf, *content_type=NULL;
   int content_length=(-1), text_type=FALSE, first_line=TRUE;
   int len1=strlen(SZ_CONTENT_TYPE), len2=strlen(SZ_CONTENT_LENGTH);
   FILE *fp=stdout;

   CleanUpHttpHeaderInfo();

   if (pn_buf_sz != NULL) *pn_buf_sz = 0;
   if (pn_html != NULL) *pn_html = FALSE;
   if (ppsz_content_type != NULL) *ppsz_content_type = NULL;
   while (c_ptr != NULL) {
      char *prev_ptr=c_ptr;
      int rc;

      if (prev_ptr != line_ptr && *(--prev_ptr) == '\r') {
         *prev_ptr = '\0';
      } else {
         prev_ptr = NULL;
         *c_ptr = '\0';
      }
      if (debugHttp > 0) fprintf(fp, "%s\n", line_ptr);
      rc = HttpParseHeaderLine(line_ptr, first_line);
      if (rc != TG_REMOTE_STATUS_OK) {
         /* well, should break... */
      }
      first_line = FALSE;
      if (*line_ptr == '\0') {
         /* empty line, end of header */
         if (prev_ptr == NULL) {
            *c_ptr = '\n';
         } else {
            *prev_ptr = '\r';
         }
         line_ptr = &c_ptr[1];
         break;
      }
      if (content_type == NULL &&
            UtilStrNCaseCmp(line_ptr, SZ_CONTENT_TYPE, len1) == 0) {
         content_type = UtilStrDup(&line_ptr[len1]);
         if (content_type != NULL) {
            UtilTrimBlanks(content_type);
            if (ppsz_content_type != NULL) {
               *ppsz_content_type = UtilStrDup(content_type);
            }
            if (UtilStrNCaseCmp(content_type, "text/", 5) == 0) {
               text_type = TRUE;
               if (strcmp(&content_type[5], "html") == 0) {
                  if (pn_html != NULL) *pn_html = TRUE;
               }
            }
         }
      } else if (content_length == (-1) &&
            UtilStrNCaseCmp(line_ptr, SZ_CONTENT_LENGTH, len2) == 0) {
         char *tmp_ptr=UtilStrDup(&line_ptr[len2]);

         if (tmp_ptr != NULL) {
            UtilTrimBlanks(tmp_ptr);
            content_length = atoi(tmp_ptr);
            free(tmp_ptr);
         }
      }
      if (prev_ptr == NULL) {
         *c_ptr = '\n';
      } else {
         *prev_ptr = '\r';
      }
      line_ptr = &c_ptr[1];
      c_ptr = strchr(line_ptr, '\n');
   }
   if (content_type != NULL) free(content_type);
   if (text_type) {
      int buf_len=strlen(line_ptr);
      char *return_buf;

      if (content_length == (-1)) {
         content_length = buf_len;
         return_buf = (char*)malloc((buf_len+1)*sizeof(char));
      } else {
         return_buf = (char*)malloc((content_length+1)*sizeof(char));
      }
      if (return_buf == NULL) {
         fprintf(stderr, "Memory allocation failed.\n");
         return NULL;
      }
      if (buf_len <= content_length) {
         memcpy(return_buf, line_ptr, content_length);
      } else {
         while (buf_len > content_length) {
            if (*line_ptr == '\r' || *line_ptr == '\n') {
               line_ptr++;
               buf_len--;
            } else {
               break;
            }
         }
         memcpy(return_buf, line_ptr, content_length);
      }
      return_buf[content_length] = '\0';
      if (pn_buf_sz != NULL) *pn_buf_sz = (content_length+1);
      return return_buf;
   } else if (content_length != (-1)) {
      char *return_buf=(char*)malloc((content_length+1)*sizeof(char));

      if (return_buf == NULL) {
         fprintf(stderr, "Memory allocation failed.\n");
         return NULL;
      }
      memcpy(return_buf, line_ptr, content_length);
      return_buf[content_length] = '\0';
      if (pn_buf_sz != NULL) *pn_buf_sz = (content_length+1);
      return return_buf;
   }
   return NULL;
}

#define MIN_READ_SIZE 0x100

int HttpDoRead(n_socket, ppsz_buf, pn_buf_sz)
   int n_socket, *pn_buf_sz;
   char **ppsz_buf;
{
   int buf_sz=0x400, len=0, end_of_file=FALSE;
   int status=TG_REMOTE_STATUS_OK;
   char *buf=(char*)malloc(buf_sz*sizeof(char)), msg[40];

   if (pn_buf_sz != NULL) *pn_buf_sz = 0;
   *ppsz_buf = NULL;
   if (buf == NULL) {
      fprintf(stderr, "Memory allocation failed.\n");
      return TG_REMOTE_STATUS_MEM;
   }
   sprintf(msg, "HTTP: reading responses...");
   ShowRemoteStatus(msg);

   do {
      int bytes_read;

      if (buf_sz - len < MIN_READ_SIZE) {
         buf_sz += 0x400;
         if ((buf=(char*)realloc(buf, buf_sz)) == NULL) {
            fprintf(stderr, "Memory allocation failed.\n");
            status = TG_REMOTE_STATUS_MEM;
            break;
         }
      }
      bytes_read = read(n_socket, &buf[len], buf_sz-len-1);
      if (bytes_read <= 0) {
         if (bytes_read < 0 && (errno == ENOTCONN || errno == ECONNRESET ||
               errno == EPIPE)) {
            fprintf(stderr, "Network read error.\n");
            status = TG_REMOTE_STATUS_READ;
         } else if (bytes_read < 0) {
            fprintf(stderr, "Network error.\n");
            status = TG_REMOTE_STATUS_NET;
         }
         end_of_file = TRUE;
      } else {
         len += bytes_read;
      }
      if (status == TG_REMOTE_STATUS_OK && !end_of_file && UserAbortComm()) {
         if (buf != NULL) free(buf);
         ShowRemoteStatus("HTTP: aborted by the user.");
         return TG_REMOTE_STATUS_INTR;
      } else {
         sprintf(msg, "HTTP: %1d bytes...", len);
         ShowRemoteStatus(msg);
      }
   } while (status == TG_REMOTE_STATUS_OK && !end_of_file);
   if (status == TG_REMOTE_STATUS_OK) {
      buf[len] = '\0';
      *ppsz_buf = buf;
      if (pn_buf_sz != NULL) *pn_buf_sz = (len+1);
      sprintf(msg, "HTTP: responses received.");
      if (debugHttp == 99) {
         fprintf(stdout, "\n==========>>>\n");
         fwrite(buf, sizeof(char), len, stdout);
         fprintf(stdout, "\n<<<==========\n");
      }
   } else {
      if (buf != NULL) free(buf);
      sprintf(msg, "HTTP: error encountered in receiving responses.");
   }
   ShowRemoteStatus(msg);
   return status;
}
