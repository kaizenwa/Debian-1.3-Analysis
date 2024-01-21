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
 */
#ifndef lint
static char RCSid[] =
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/util.c,v 3.0 1996/05/06 16:12:44 william Exp $";
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>

#include "const.h"

#include "remote.e"
#ifndef _NO_EXTERN
#include "util.e"
#endif /* !_NO_EXTERN */

void UtilFree(pszStr)
   char *pszStr;
{
   if (pszStr != NULL) free(pszStr);
}

char *UtilStrDup(pszStr)
   char *pszStr;
   /* must eventually free the returned string with free() */
{
   int len=(pszStr == NULL ? 0 : strlen(pszStr));
   char *c_ptr=(char*)malloc((len+1)*sizeof(char));

   if (c_ptr == NULL) return NULL;
   strcpy(c_ptr, (pszStr == NULL ? "" : pszStr));
   return c_ptr;
}

int UtilStrCpy(pszDest, nMaxDestSz, pszSrc)
   char *pszDest, *pszSrc;
   int nMaxDestSz;
{
   int len;

   if (pszDest != NULL && nMaxDestSz > 0) *pszDest = '\0';
   if (pszDest == NULL || pszSrc == NULL || nMaxDestSz <= 0) return 0;

   len = min((int)strlen(pszSrc), nMaxDestSz-1);
   strncpy(pszDest, pszSrc, len);
   pszDest[len] = '\0';
   return len;
}

void UtilTrimBlanks(pszStr)
   char *pszStr;
   /* pszStr must be terminated by '\0' */
{
   register int len;
   register char *c_ptr;

   for (len=strlen(pszStr)-1; len >= 0; len--) {
      char ch=pszStr[len];
      
      if (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r') {
         pszStr[len] = '\0';
      } else {
         break;
      }
   }
   for (c_ptr=pszStr; *c_ptr != '\0'; c_ptr++) {
      char ch=(*c_ptr);
      
      if (!(ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r')) {
         break;
      }
   }
   if (*c_ptr != '\0' && c_ptr != pszStr) {
      while ((*pszStr++ = *c_ptr++) != '\0') ;
   }
}

int UtilStrNCaseCmp(pszStr1, pszStr2, nCount)
   char *pszStr1, *pszStr2;
   int nCount;
{
   char *c_ptr1, *c_ptr2, ch1, ch2;
   int i=0;

   for (c_ptr1=pszStr1, c_ptr2=pszStr2; i < nCount && (*c_ptr1 != '\0' ||
         *c_ptr2 != '\0'); c_ptr1++, c_ptr2++, i++) {
      ch1 = *c_ptr1;
      ch2 = *c_ptr2;
      if (ch1 >= 'A' && ch1 <= 'Z') ch1 = ch1-'A'+'a';
      if (ch2 >= 'A' && ch2 <= 'Z') ch2 = ch2-'A'+'a';
      if (ch1 != ch2) break;
   }
   if (i == nCount) return 0;
   return ((*c_ptr1)-(*c_ptr2));
}

int UtilStrICmp(pszStr1, pszStr2)
   char *pszStr1, *pszStr2;
{
   char *c_ptr1, *c_ptr2, ch1, ch2;

   for (c_ptr1=pszStr1, c_ptr2=pszStr2; *c_ptr1 != '\0' || *c_ptr2 != '\0';
         c_ptr1++, c_ptr2++) {
      ch1 = *c_ptr1;
      ch2 = *c_ptr2;
      if (ch1 >= 'A' && ch1 <= 'Z') ch1 = ch1-'A'+'a';
      if (ch2 >= 'A' && ch2 <= 'Z') ch2 = ch2-'A'+'a';
      if (ch1 != ch2) break;
   }
   return ((*c_ptr1)-(*c_ptr2));
}

char *UtilStrRChr(pszStr, int_ch)
   char *pszStr;
   int int_ch;
{
   int len=strlen(pszStr);
   char ch=(char)int_ch;

   for (len--; len >= 0; len--) {
      if (pszStr[len] == ch) {
         return (&pszStr[len]);
      }
   }
   return NULL;
}

#ifdef NO_STRSTR
char *strstr (pszStr, pszSubStr)
   char *pszStr, *pszSubStr;
{
   int len;

   for (len=strlen(pszSubStr); *pszStr != '\0'; pszStr++) {
      if (strncmp(pszStr, pszSubStr, len) == 0) {
         return(pszStr);
      }
   }
   return NULL;
}
#endif /* NO_STRSTR */

int UtilShrinkName(pszFile)
   char *pszFile;
{
   char *c_ptr, *real_ptr, *real_name;

   if (strncmp(pszFile, "./", 2) == 0) return TRUE;
   if (*pszFile != '/') return FALSE;

   real_name = UtilStrDup(pszFile);
   if (real_name == NULL) return FALSE;
   real_ptr = real_name;
   c_ptr = &real_name[1];

   while (*c_ptr != '\0') {
      if (*c_ptr == '.') {
         if (*(c_ptr+1) == '.') {
            if (*(c_ptr+2) == '/') {
               /* "../" */
               if (real_ptr != real_name) {
                  while (*(--real_ptr) != '/') ;
               }
               c_ptr += 3;
            } else {
               while (*c_ptr != '\0' && *c_ptr != '/') {
                  *(++real_ptr) = *c_ptr++;
               }
               if (*c_ptr == '/') {
                  c_ptr++;
                  *(++real_ptr) = '/';
               }
            }
         } else if (*(c_ptr+1) == '/') {
            /* "./" */
            c_ptr += 2;
         } else {
            while (*c_ptr != '\0' && *c_ptr != '/') {
               *(++real_ptr) = *c_ptr++;
            }
            if (*c_ptr == '/') {
               c_ptr++;
               *(++real_ptr) = '/';
            }
         }
#ifndef apollo
      } else if (*c_ptr == '/') {
         c_ptr++;
#endif
      } else {
         while (*c_ptr != '\0' && *c_ptr != '/') {
            *(++real_ptr) = *c_ptr++;
         }
         if (*c_ptr == '/') {
            c_ptr++;
            *(++real_ptr) = '/';
         }
      }
   }
   *(++real_ptr) = '\0';
   strcpy (pszFile, real_name);
   free(real_name);
   return TRUE;
}

void UtilRemoveQuotes(pszStr)
   char *pszStr;
{
   int nStart=0, nEnd=strlen(pszStr)-1, still_going=TRUE;

   while (still_going) {
      if (nStart >= nEnd) break;
      switch (pszStr[nStart]) {
      case '"': if (pszStr[nEnd] != '"') still_going = FALSE; break;
      case '\'': if (pszStr[nEnd] != '\'') still_going = FALSE; break;
      default: still_going = FALSE; break;
      }
      if (still_going) {
         nStart++;
         pszStr[nEnd--] = '\0';
      }
   }
   if (nStart != 0) {
      char *c_ptr=&pszStr[nStart];

      while (*pszStr != '\0') *pszStr++ = (*c_ptr++);
   }
}

static char gszMsg[512];

char *UtilGetALine(pFile)
   FILE *pFile;
{
   unsigned long len;
   
   if (fgets(gszMsg, sizeof(gszMsg), pFile) == NULL) return NULL;
   
   len = strlen(gszMsg);
   if (len == sizeof(gszMsg)-1 && gszMsg[len-1] != '\n') {
      char *buf=UtilStrDup(gszMsg);
      unsigned long cur_len;
      int still_going=TRUE;
      
      if (buf == NULL) return NULL;
      
      cur_len = strlen(buf);
      while (still_going && fgets(gszMsg, sizeof(gszMsg), pFile) != NULL) {
         len = strlen(gszMsg);
         if (len != sizeof(gszMsg)-1) {
            still_going = FALSE;
         } else if (gszMsg[len-1] == '\n') {
            gszMsg[--len] = '\0';
            still_going = FALSE;
         }
         if (buf != NULL) {
            char *new_buf=(char*)realloc(buf, (size_t)(cur_len+len+1));
            
            if (new_buf == NULL) {
               free(buf);
               return NULL;
            }
            buf = new_buf;
            strcat(&buf[cur_len], gszMsg);
            cur_len += len;
         }
      }
      return buf;
   } else {
      if (gszMsg[len-1] == '\n') gszMsg[--len] = '\0';
      return UtilStrDup(gszMsg);
   }
}

char *UtilGetAContinuedLine(pFile)
   FILE *pFile;
{
   unsigned long len;
   char *buf;

   if ((buf=UtilGetALine(pFile)) == NULL) return NULL;

   len = strlen(buf);
   while (len > 0 && buf[len-1] == '\\') {
      char *tmp_buf;
      int tmp_len;

      buf[--len] = '\0';
      tmp_buf = UtilGetALine(pFile);
      if (tmp_buf == NULL) return buf;
      tmp_len = strlen(tmp_buf);
      if (tmp_len == 0) {
         free(tmp_buf);
         return buf;
      } else if ((buf=(char*)realloc(buf, len+tmp_len+1)) == NULL) {
         free(tmp_buf);
         return NULL;
      }
      strcpy(&buf[len], tmp_buf);
      len += tmp_len;
      free(tmp_buf);
   }
   return buf;
}

int UtilCopyFile(pszFromFile, pszToFile)
   char *pszFromFile, *pszToFile;
{
   char buf[0x1000];
   int bytes_read;
   int fd1, fd2;

   if ((fd1=open(pszFromFile, O_RDONLY)) == (-1)) return TG_REMOTE_STATUS_READ;
   if ((fd2=open(pszToFile, O_WRONLY|O_CREAT|O_TRUNC)) == (-1)) {
      close(fd1);
      return TG_REMOTE_STATUS_WRITE;
   }
   while ((bytes_read=read(fd1, buf, sizeof(buf))) > 0) {
      if (write(fd2, buf, bytes_read) <= 0) {
         close(fd1);
         close(fd2);
         return TG_REMOTE_STATUS_FILE;
      }
   }
   close(fd1);
   close(fd2);
   chmod(pszToFile, 0777);
   return TG_REMOTE_STATUS_OK;
}
