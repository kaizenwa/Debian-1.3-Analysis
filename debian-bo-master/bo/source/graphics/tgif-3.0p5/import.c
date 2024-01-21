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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/import.c,v 3.1 1996/05/12 05:17:23 william Exp $";
#endif

#include <sys/types.h>
#ifdef VMS
#include "vms_comp.h"
#define DIR_ENTRY struct dirent
#else
#ifdef ibm
#include <sys/dir.h>
#define DIR_ENTRY struct direct
#else
#ifdef apollo
#include <sys/dir.h>
#define DIR_ENTRY struct direct
#else
#ifdef NeXT
#include <sys/dir.h>
#define DIR_ENTRY struct direct
#else
#ifdef luna88k
#include <sys/dir.h>
#define DIR_ENTRY struct direct
#else
#ifdef sequent
#include <sys/dir.h>
#define DIR_ENTRY struct direct
#else
#include <dirent.h>
#define DIR_ENTRY struct dirent
#endif /* sequent */
#endif /* luna88k */
#endif /* NeXT */
#endif /* apollo */
#endif /* ibm */
#endif /* VMS */
#include <sys/stat.h>
#include <sys/file.h>
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "const.h"
#include "types.h"

#include "choose.e"
#include "color.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#include "eps.e"
#include "file.e"
#ifndef _NO_EXTERN
#include "import.e"
#endif
#include "msg.e"
#include "menu.e"
#include "names.e"
#include "obj.e"
#include "select.e"
#include "setup.e"
#include "text.e"
#include "util.e"
#include "xbitmap.e"
#include "xpixmap.e"

#ifndef _NO_EXTERN
extern int unlink ARGS_DECL((char *));
#ifdef SYSV
extern unsigned sleep ARGS_DECL((unsigned));
#else
extern int sleep ARGS_DECL((unsigned));
#endif
#endif

typedef struct ImportInfoRec {
   char *name, *ext, *cmd;
   struct ImportInfoRec *next;
} *ImportInfoPtr;

static struct ImportInfoRec *topImportInfo=NULL, *botImportInfo=NULL;
static int gnMaxImportFilters=0;
static int showFileNameOnBrowse=TRUE;

int ExtensionMatch(Spec, DirName)
   char *Spec, *DirName;
{
   char *c_ptr, other_ext_str[MAXSTRING];
   int len;

   if (Spec == NULL || *Spec == '\0') return FALSE;
   len = strlen(DirName);
   strcpy(other_ext_str, Spec);
   for (c_ptr=strtok(other_ext_str,";"); c_ptr != NULL;
         c_ptr=strtok(NULL, ";")) {
      int other_ext_len=strlen(c_ptr);

      if (len > other_ext_len &&
            UtilStrICmp(c_ptr, &DirName[len-other_ext_len]) == 0) {
         return TRUE;
      }
   }
   return FALSE;
}

char *SetUpExtStr(cbBuf, pszExt, pszOtherExt)
   int cbBuf;
   char *pszExt, *pszOtherExt;
   /* pszExt="gif;GIF", pszOtherExt=".jpg;.jpeg" */
{
   char *c_ptr, *buf=(char*)malloc(cbBuf*sizeof(char)), *start_ptr, *dest_ptr;

   if (buf == NULL) {
      FailAllocMessage();
      return NULL;
   }
   *buf = '\0';
   dest_ptr = buf;

   start_ptr = pszExt;
   while (*start_ptr == ' ') start_ptr++;
   c_ptr = strchr(start_ptr, ';');
   while (start_ptr != NULL) {
      if (c_ptr != NULL) *c_ptr = '\0';
      if(dest_ptr == buf) {
         sprintf(dest_ptr, ".%s", start_ptr);
      } else {
         sprintf(dest_ptr, ";.%s", start_ptr);
      }
      dest_ptr = (&dest_ptr[strlen(dest_ptr)]);
      if (c_ptr != NULL) *c_ptr++ = ';';
      if (c_ptr == NULL) break;
      start_ptr = c_ptr;
      while (*start_ptr == ' ') start_ptr++;
      c_ptr = strchr(start_ptr, ';');
   }
   if (dest_ptr == buf) {
      strcpy(buf, pszOtherExt);
   } else if (*pszOtherExt == '\0') {
      *dest_ptr = '\0';
   } else {
      sprintf(dest_ptr, ";%s", pszOtherExt);
   }
   return buf;
}

void CleanUpImport()
{
   struct ImportInfoRec *next_import;

   while (topImportInfo != NULL) {
      next_import = topImportInfo->next;

      if(topImportInfo->name != NULL) free(topImportInfo->name);
      if(topImportInfo->ext != NULL) free(topImportInfo->ext);
      if(topImportInfo->cmd != NULL) free(topImportInfo->cmd);
      free(topImportInfo);
      topImportInfo = next_import;
   }
   topImportInfo = botImportInfo = NULL;
}

static
void InvalidFilterSpecMsg(pszEntry, pszValue, pszExplain)
   char *pszEntry, *pszValue, *pszExplain;
{
   if (pszExplain == NULL) {
      sprintf(gszMsgBox, "Invalid %s*%s: '%s'.\n\n%s, e.g.,\n\n%s %s",
            TOOL_NAME, pszEntry, pszValue,
            "Format is \"<name> <extentions> <filter spec>\"",
            "    JPEG jpg;jpeg",
            "djpeg -gif -colors 222 %s | giftopnm | ppmtoxpm");
   } else {
      sprintf(gszMsgBox, "Invalid %s*%s: '%s' (%s).\n\n%s, e.g.,\n\n%s %s",
            TOOL_NAME, pszEntry, pszValue, pszExplain,
            "Format is \"<name> <extentions> <filter spec>\"",
            "    JPEG jpg;jpeg",
            "djpeg -gif -colors 222 %s | giftopnm | ppmtoxpm");
   }
   MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
}

static
int CountPercentS(buf)
   char *buf;
{
   int count=0;
   char *c_ptr;

   for (c_ptr=strstr(buf, "%s"); c_ptr != NULL; c_ptr=strstr(++c_ptr, "%s")) {
      count++;
   }
   return count;
}

static
char *FindBlank(pszStr, ppszEnd)
   char *pszStr, **ppszEnd;
{
   char *c_ptr=pszStr, ch;

   if (!(*c_ptr == '"' || *c_ptr == '\'')) {
      *ppszEnd = strchr(pszStr, ' ');
      return pszStr;
   }
   for (ch=(*c_ptr++); *c_ptr != '\0'; c_ptr++) {
      if (*c_ptr == '\\') {
         c_ptr++;
      } else if (*c_ptr == ch) {
         *ppszEnd = c_ptr;
         return &pszStr[1];
      }
   }
   *ppszEnd = NULL;
   return pszStr;
}

void InitImport()
{
   char *c_ptr;
   int max_filters=0;

   showFileNameOnBrowse = TRUE;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"ShowFileNameOnBrowse")) !=
         NULL) {
      if (UtilStrICmp(c_ptr, "false") == 0) {
         showFileNameOnBrowse = FALSE;
      }
   }
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"MaxImportFilters")) != NULL) {
      max_filters = atoi(c_ptr);
      if (max_filters <= 0) {
         sprintf(gszMsgBox, "Invalid %s*%s: '%s'.\n\nValue must be > 0.",
               TOOL_NAME, "MaxImportFilters", c_ptr);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      }
   }
   gnMaxImportFilters = 0;
   if (max_filters > 0) {
      int i;

      for (i=0; i < max_filters; i++) {
         char buf[80];

         sprintf(buf, "ImportFilter%1d", i);
         if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,buf)) == NULL) {
            sprintf(gszMsgBox, "Cannot find %s*%s.\n\n%s?", TOOL_NAME, buf,
                  "Would you like to continue looking for filters");
            if (MsgBox(gszMsgBox, TOOL_NAME, YNC_MB) != MB_ID_YES) {
               break;
            }
         } else {
            char *name_ptr=UtilStrDup(c_ptr), *dup_buf=UtilStrDup(c_ptr);
            char *ext_ptr;

            if (name_ptr == NULL || dup_buf == NULL) FailAllocMessage();
            for (c_ptr=name_ptr; *c_ptr != '\0'; c_ptr++) {
               if (*c_ptr == '\t' || *c_ptr == '\n' || *c_ptr == '\r') {
                  *c_ptr = ' ';
               }
            }
            for (c_ptr=dup_buf; *c_ptr != '\0'; c_ptr++) {
               if (*c_ptr == '\t' || *c_ptr == '\n' || *c_ptr == '\r') {
                  *c_ptr = ' ';
               }
            }
            UtilTrimBlanks(name_ptr);
            name_ptr = FindBlank(name_ptr, &ext_ptr);
            if (ext_ptr == NULL) {
               InvalidFilterSpecMsg(buf, dup_buf, NULL);
            } else {
               char *cmd_ptr;

               *ext_ptr++ = '\0';
               UtilTrimBlanks(ext_ptr);
               ext_ptr = FindBlank(ext_ptr, &cmd_ptr);
               if (cmd_ptr == NULL) {
                  InvalidFilterSpecMsg(buf, dup_buf, NULL);
               } else {
                  int len, count;

                  *cmd_ptr++ = '\0';
                  UtilTrimBlanks(cmd_ptr);
                  len = strlen(cmd_ptr);
                  if (len >= 2 && (*cmd_ptr == '"' && cmd_ptr[len-1] == '"') ||
                        (*cmd_ptr == '\'' && cmd_ptr[len-1] == '\'')) {
                     cmd_ptr[len-1] = '\0';
                     cmd_ptr++;
                     UtilTrimBlanks(cmd_ptr);
                  }
                  count = CountPercentS(cmd_ptr);
                  if (count < 1) {
                     InvalidFilterSpecMsg(buf, dup_buf, "missing %s");
                  } else if (count > 1) {
                     InvalidFilterSpecMsg(buf, dup_buf, "too many %s");
                  } else {
                     struct ImportInfoRec *pii=(struct ImportInfoRec*)malloc(
                           sizeof(struct ImportInfoRec));

                     if (pii == NULL) FailAllocMessage();
                     memset(pii, 0, sizeof(struct ImportInfoRec));
                     pii->next = NULL;
                     pii->name = UtilStrDup(name_ptr);
                     pii->ext = UtilStrDup(ext_ptr);
                     pii->cmd = UtilStrDup(cmd_ptr);
                     if (pii->name==NULL || pii->ext==NULL || pii->cmd==NULL) {
                        FailAllocMessage();
                        if (pii->name != NULL) free(pii->name);
                        if (pii->ext != NULL) free(pii->ext);
                        if (pii->cmd != NULL) free(pii->cmd);
                     } else {
                        if (botImportInfo != NULL) {
                           botImportInfo->next = pii;
                        } else {
                           topImportInfo = pii;
                        }
                        botImportInfo = pii;
                        gnMaxImportFilters++;
                     }
                  }
               }
            }
            if (name_ptr != NULL) free(name_ptr);
            if (dup_buf != NULL) free(dup_buf);
         }
      }
   }
}

void ImportXBitmapFile()
{
   char file_name[MAXPATHLENGTH+1], s[MAXPATHLENGTH+1], * rest;
   char mag_spec[MAXSTRING+1], msg[MAXSTRING+1];
   unsigned int tmp_w, tmp_h;
   int rc, x_hot, y_hot, x, y, w, h, short_name, orig_w, orig_h;
   float mag;
   Pixmap orig_bitmap, bitmap;
   XImage *image=NULL;
   XEvent ev;
   struct ObjRec *obj_ptr;

   MakeQuiescent();

   importingFile = TRUE;
   if (importFromLibrary) {
      char name[MAXSTRING+1], path[MAXSTRING+1];

      if (SelectFromLibrary("Please select an XBitmap file to IMPORT...",
            XBM_FILE_EXT, name, path) == INVALID) {
         importingFile = FALSE;
         return;
      }
      sprintf(s, "%s/%s", path, name);
   } else if (SelectFileNameToImport(
         "Please select an XBitmap file to IMPORT...",
         XBM_FILE_EXT, s) == INVALID) {
      importingFile = FALSE;
      return;
   } else if (FileIsRemote(s)) {
      MsgBox("Importing remote XBitmap file is not supported.", TOOL_NAME,
            INFO_MB);
      importingFile = FALSE;
      return;
   }
   XSync(mainDisplay, False);
   if (XCheckMaskEvent(mainDisplay, ExposureMask, &ev)) {
      ExposeEventHandler(&ev, TRUE);
   }
   strcpy(file_name, s);

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   rc = XReadBitmapFile(mainDisplay, mainWindow, file_name, &tmp_w, &tmp_h,
         &orig_bitmap, &x_hot, &y_hot);
   orig_w = tmp_w; orig_h = tmp_h;
   SetDefaultCursor(mainWindow);
   ShowCursor();

   if ((short_name=IsPrefix(bootDir, file_name, &rest))) ++rest;
   if (rc != BitmapSuccess) {
      if (short_name) {
         sprintf(s, "Cannot import XBitmap file '%s'.", rest);
      } else {
         sprintf(s, "Cannot import XBitmap file '%s'.", file_name);
      }
      Msg(s);
      importingFile = FALSE;
      return;
   }

   x = 0;
   y = 0;
   w = orig_w;
   h = orig_h;
   mag = 1.0;
   if (askForXBmSpec) {
      sprintf(msg, "%s: [[MAG=]WxH+X+Y] (original size is %1dx%1d)",
         "Please enter geometry spec", orig_w, orig_h);
      Dialog(msg, "( <CR>: accept, <ESC>: continue )", mag_spec);
      if (*mag_spec != '\0') {
         ParseCutSpec(mag_spec, orig_w, orig_h, &mag, &x, &y, &w, &h);
      }
      if (x==0 && y==0 && w==orig_w && h==orig_h && mag==1.0) {
         bitmap = orig_bitmap;
      } else {
         orig_w = w;
         orig_h = h;
         if (!ExtractBitmap(orig_bitmap, NULL, x, y, w, h, &bitmap, &image)) {
            Msg("Cannot allocate extracted bitmap.");
            importingFile = FALSE;
            XFreePixmap(mainDisplay, orig_bitmap);
            return;
         }
         XFreePixmap(mainDisplay, orig_bitmap);
         w = (int)(((float)w) * mag);
         h = (int)(((float)h) * mag);
      }
   } else {
      bitmap = orig_bitmap;
   }

   obj_ptr = CreateXBmObj(orig_w, orig_h, w, h, bitmap, image);
   PlaceTopObj(obj_ptr);
   AddObj(NULL, topObj, obj_ptr);

   SelectTopObj();
   RecordNewObjCmd();
   SetFileModified(TRUE);
   justDupped = FALSE;

   if (!importFromLibrary) SetCurImportDir(file_name);

   if (short_name) {
      sprintf(s, "XBitmap file (%1dx%1d) '%s' imported.", orig_w, orig_h,
            rest);
   } else {
      sprintf(s, "XBitmap file (%1dx%1d) '%s' imported.", orig_w, orig_h,
            file_name);
   }
   Msg(s);
   importingFile = FALSE;
}

void ImportXPixmapFile()
{
   char file_name[MAXPATHLENGTH+1], s[MAXPATHLENGTH+1], * rest;
   int rc, ncolors, chars_per_pixel, * pixels, short_name;
   int first_pixel_is_bg, image_w, image_h, w, h;
   Pixmap pixmap=None, bitmap=None;
   XImage *image=NULL, *bitmap_image=NULL;
   char *color_char, **color_str, *xpm_data=NULL;
   XEvent ev;
   struct ObjRec *obj_ptr;

   MakeQuiescent();

   importingFile = TRUE;
   if (importFromLibrary) {
      char name[MAXSTRING+1], path[MAXSTRING+1];

      if (SelectFromLibrary("Please select an XPixmap file to IMPORT...",
            XPM_FILE_EXT, name, path) == INVALID) {
         importingFile = FALSE;
         return;
      }
      sprintf(s, "%s/%s", path, name);
   } else if (SelectFileNameToImport(
         "Please select an XPixmap file to IMPORT...",
         XPM_FILE_EXT, s) == INVALID) {
      importingFile = FALSE;
      return;
   } else if (FileIsRemote(s)) {
      MsgBox("Importing remote XPixmap file is not supported.",
            TOOL_NAME, INFO_MB);
      importingFile = FALSE;
      return;
   }

   XSync(mainDisplay, False);
   if (XCheckMaskEvent(mainDisplay, ExposureMask, &ev)) {
      ExposeEventHandler (&ev, TRUE);
   }
   strcpy(file_name, s);

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   rc = MyReadPixmapFile(file_name, &image_w, &image_h, &w, &h, &pixmap,
         &image, &bitmap, &bitmap_image, &ncolors, &chars_per_pixel,
         &first_pixel_is_bg, &color_char, &color_str, &pixels, &xpm_data);
   SetDefaultCursor(mainWindow);
   ShowCursor();

   if ((short_name=IsPrefix(bootDir, file_name, &rest))) ++rest;
   if (rc != BitmapSuccess) {
      if (short_name) {
         sprintf(s, "Cannot import XPixmap file '%s'.", rest);
      } else {
         sprintf(s, "Cannot import XPixmap file '%s'.", file_name);
      }
      Msg(s);
      importingFile = FALSE;
      return;
   }

   obj_ptr = CreateXPmObj(image_w, image_h, w, h, pixmap, image, bitmap,
         bitmap_image, ncolors, chars_per_pixel, first_pixel_is_bg, color_char,
         color_str, pixels, xpm_data);
   AddObj(NULL, topObj, obj_ptr);
   PlaceTopObj(obj_ptr);

   SelectTopObj();
   RecordNewObjCmd();
   SetFileModified(TRUE);
   justDupped = FALSE;
   if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
      RedrawColorWindow();
   }
   if (!importFromLibrary) SetCurImportDir(file_name);

   if (short_name) {
      sprintf(s, "XPixmap file (%1dx%1d) '%s' imported.", image_w, image_h,
            rest);
   } else {
      sprintf(s, "XPixmap file (%1dx%1d) '%s' imported.", image_w, image_h,
            file_name);
   }
   Msg(s);
   importingFile = FALSE;
}

void ImportEPSFile(Embed)
   int Embed;
{
   char file_name[MAXPATHLENGTH+1], s[MAXPATHLENGTH+1], *rest;
   char **lines=NULL, write_date[32];
   int rc, short_name, num_lines, epsf_level, image_w, image_h;
   float llx, lly, urx, ury;
   Pixmap bitmap;
   XImage *image=NULL;
   XEvent ev;
   struct ObjRec *obj_ptr;

   MakeQuiescent();

   importingFile = TRUE;
   if (importFromLibrary) {
      char name[MAXSTRING+1], path[MAXSTRING+1];

      if (SelectFromLibrary("Please select an EPS file to IMPORT...",
            EPSF_FILE_EXT, name, path) == INVALID) {
         importingFile = FALSE;
         return;
      }
      sprintf(s, "%s/%s", path, name);
   } else if (SelectFileNameToImport("Please select an EPS file to IMPORT...",
         EPSF_FILE_EXT, s) == INVALID) {
      importingFile = FALSE;
      return;
   } else if (FileIsRemote(s)) {
      MsgBox("Importing remote EPS file is not supported.", TOOL_NAME, INFO_MB);
      importingFile = FALSE;
      return;
   }

   XSync(mainDisplay, False);
   if (XCheckMaskEvent(mainDisplay, ExposureMask, &ev)) {
      ExposeEventHandler (&ev, TRUE);
   }
   strcpy(file_name, s);

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   rc = MyReadEPSFile(file_name, &image_w, &image_h, &bitmap, &image,
         &num_lines, &lines, &epsf_level, &llx, &lly, &urx, &ury, write_date);

   if ((short_name=IsPrefix(bootDir, file_name, &rest))) ++rest;
   if (rc != BitmapSuccess) {
      SetDefaultCursor(mainWindow);
      ShowCursor();
      if (short_name) {
         sprintf(gszMsgBox, "Cannot import EPS file '%s'.", rest);
      } else {
         sprintf(gszMsgBox, "Cannot import EPS file '%s'.", file_name);
      }
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      importingFile = FALSE;
      return;
   }

   if (Embed) saveEPSLines = TRUE;
   if (short_name) {
      obj_ptr = CreateEPSObj(rest, image_w, image_h, bitmap, image,
            num_lines, lines, epsf_level, &llx, &lly, &urx, &ury, write_date);
   } else {
      obj_ptr = CreateEPSObj(file_name, image_w, image_h, bitmap, image,
            num_lines, lines, epsf_level, &llx, &lly, &urx, &ury, write_date);
   }
   saveEPSLines = FALSE;

   if (strcmp(defaultEPSScalingStr,"1") != 0) {
      ScaleAnEPSObj(obj_ptr, &defaultEPSScaling);
   }
   SetDefaultCursor(mainWindow);
   ShowCursor();

   PlaceTopObj(obj_ptr);
   AddObj(NULL, topObj, obj_ptr);

   SelectTopObj();
   RecordNewObjCmd();
   SetFileModified(TRUE);
   justDupped = FALSE;

   if (!importFromLibrary) SetCurImportDir(file_name);

   if (short_name) {
      sprintf(s, "EPS file '%s' imported.", rest);
   } else {
      sprintf(s, "EPS file '%s' imported.", file_name);
   }
   Msg(s);
   importingFile = FALSE;
}

static char gifToXpmCmd[MAXSTRING+1];

static
void InitGifToXpm()
{
   static int nInitialized=FALSE;

   if (!nInitialized) {
      char *c_ptr;

      if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"GifToXpm")) != NULL) {
         int count=0;

         strcpy(gifToXpmCmd, c_ptr);
         for (c_ptr=strstr(gifToXpmCmd,"%s"); c_ptr!=NULL;
               c_ptr=strstr(++c_ptr,"%s")) {
            count++;
         }
         if (count != 1) {
            sprintf(gszMsgBox, "Invalid %s*%s: '%s' resource.  '%s' used.",
                  TOOL_NAME, "GifToXpm", gifToXpmCmd, "giftopnm %s | ppmtoxpm");
            MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
            strcpy(gifToXpmCmd, "giftopnm %s | ppmtoxpm");
         }
      } else {
         strcpy(gifToXpmCmd, "giftopnm %s | ppmtoxpm");
      }
   }
}

static
int ConvertGifToXpm(pszGifPath, pszXpmPath)
   char *pszGifPath, *pszXpmPath;
{
   FILE *pFile=NULL, *pPipe=NULL;
   char *pszCmd=NULL, szBuf[MAXSTRING];
   int bytes_read;

   InitGifToXpm();
   sprintf(pszXpmPath, "%sTgifXXXXXX", TMP_DIR);
   mktemp(pszXpmPath);
   unlink(pszXpmPath);
   pszCmd = (char*)malloc(
         (strlen(gifToXpmCmd)+strlen(pszGifPath)+10)*sizeof(char));
   if (pszCmd == NULL) {
      FailAllocMessage();
      return FALSE;
   }
   if ((pFile=fopen(pszXpmPath,"w")) == NULL) {
      sprintf(gszMsgBox, "Cannot create temporary file '%s'.\n\n%s.",
            pszXpmPath, "Conversion aborted");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      free(pszCmd);
      return FALSE;
   }
   sprintf(pszCmd, gifToXpmCmd, pszGifPath);
   Msg("Executing:");
   sprintf(gszMsgBox, "    %s", pszCmd);
   Msg(gszMsgBox);
   sprintf(gszMsgBox, "Executing '%s'...", pszCmd);
   SetStringStatus(gszMsgBox);
   XSync(mainDisplay, False);
   if ((pPipe=(FILE*)popen(pszCmd,"r")) == NULL) {
      sprintf(gszMsgBox, "Fail to execute '%s'.\n\n%s.",
            pszCmd, "Conversion aborted");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      free(pszCmd);
      fclose(pFile);
      unlink(pszXpmPath);
      return FALSE;
   }
   writeFileFailed = FALSE;
   while ((bytes_read=fread(szBuf, sizeof(char), sizeof(szBuf), pPipe)) > 0) {
      if ((int)fwrite(szBuf, sizeof(char), bytes_read, pFile) <= 0) {
         writeFileFailed = TRUE;
         break;
      }
   }
   pclose(pPipe);
   SetStringStatus("...Done");
   free(pszCmd);
   fclose(pFile);
   if (writeFileFailed) {
      sprintf(gszMsgBox, "Fail to write to '%s'.\n\nFile system may be full.",
            pszXpmPath);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      unlink(pszXpmPath);
      return FALSE;
   }
   return TRUE;
}

void ImportGIFFile()
{
   char file_name[MAXPATHLENGTH+1], *rest;
   char szGifPath[MAXPATHLENGTH+1];
   int rc, ncolors, chars_per_pixel, *pixels, short_name;
   int first_pixel_is_bg, image_w, image_h, w, h;
   Pixmap pixmap=None, bitmap=None;
   XImage *image=NULL, *bitmap_image=NULL;
   char *color_char, **color_str, *xpm_data=NULL;
   XEvent ev;
   struct ObjRec *obj_ptr;

   MakeQuiescent();

   importingFile = TRUE;
   if (importFromLibrary) {
      char name[MAXSTRING+1], path[MAXSTRING+1];

      if (SelectFromLibrary("Please select an GIF file to IMPORT...",
            "gif", name, path) == INVALID) {
         importingFile = FALSE;
         return;
      }
      sprintf(szGifPath, "%s/%s", path, name);
   } else if (SelectFileNameToImport("Please select an GIF file to IMPORT...",
         "gif", szGifPath) == INVALID) {
      importingFile = FALSE;
      return;
   } else if (FileIsRemote(szGifPath)) {
      MsgBox("Importing remote GIF file is not supported.", TOOL_NAME, INFO_MB);
      importingFile = FALSE;
      return;
   }
   XSync(mainDisplay, False);
   if (XCheckMaskEvent(mainDisplay, ExposureMask, &ev)) {
      ExposeEventHandler(&ev, TRUE);
   }
   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   SaveStatusStrings();
   rc = ConvertGifToXpm(szGifPath, file_name);
   RestoreStatusStrings();
   SetDefaultCursor(mainWindow);
   ShowCursor();
   if (!rc) return;

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   rc = MyReadPixmapFile(file_name, &image_w, &image_h, &w, &h, &pixmap,
         &image, &bitmap, &bitmap_image, &ncolors, &chars_per_pixel,
         &first_pixel_is_bg, &color_char, &color_str, &pixels, &xpm_data);
   SetDefaultCursor(mainWindow);
   ShowCursor();

   if ((short_name=IsPrefix(bootDir, szGifPath, &rest))) ++rest;
   if (rc != BitmapSuccess) {
      sprintf(gszMsgBox, "Cannot import GIFs file '%s'.",
            (short_name ? rest : szGifPath));
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      importingFile = FALSE;
      unlink(file_name);
      return;
   }
   unlink(file_name);
   obj_ptr = CreateXPmObj(image_w, image_h, w, h, pixmap, image, bitmap,
         bitmap_image, ncolors, chars_per_pixel, first_pixel_is_bg, color_char,
         color_str, pixels, xpm_data);
   AddObj(NULL, topObj, obj_ptr);
   PlaceTopObj(obj_ptr);

   SelectTopObj();
   RecordNewObjCmd();
   SetFileModified(TRUE);
   justDupped = FALSE;
   if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
      RedrawColorWindow();
   }
   if (!importFromLibrary) SetCurImportDir(szGifPath);

   if (short_name) {
      sprintf(gszMsgBox, "GIF file (%1dx%1d) '%s' imported.", image_w, image_h,
            rest);
   } else {
      sprintf(gszMsgBox, "GIF file (%1dx%1d) '%s' imported.", image_w, image_h,
            szGifPath);
   }
   Msg(gszMsgBox);
   importingFile = FALSE;
}

static
DspList *ImportFilterListing(pnEntries)
   int *pnEntries;
{
   int i;
   struct ImportInfoRec *pii=topImportInfo;
   DspList *pdl, *dsp_ptr;

   if (gnMaxImportFilters == 0) return NULL;
   pdl = (DspList*)malloc(gnMaxImportFilters*sizeof(DspList));
   if (pdl == NULL) {
      FailAllocMessage();
      return NULL;
   }
   for (i=0, dsp_ptr=pdl; i < gnMaxImportFilters; i++, dsp_ptr++) {
      sprintf(gszMsgBox, "%s (%s)", pii->name, pii->ext);
      UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr), gszMsgBox);
      UtilStrCpy(dsp_ptr->pathstr, sizeof(dsp_ptr->pathstr), pii->cmd);
      dsp_ptr->directory = FALSE;
      dsp_ptr->next = ((i == gnMaxImportFilters-1) ? NULL : (&dsp_ptr[1]));
      pii = pii->next;
   }
   if (pnEntries != NULL) *pnEntries = gnMaxImportFilters;
   return pdl;
}

static
int ChooseAnImportFilter(top_str, entries, num_entries)
   char *top_str, **entries;
   int num_entries;
{
   char win_name[128];
   int selected_index=INVALID;

   ResetNamesInfo();
   NamesSetTitle(top_str);
   NamesAddButton("OK", BUTTON_OK);
   NamesAddButton("CANCEL", BUTTON_CANCEL);
   NamesSetEntries(NULL, entries, num_entries, TRUE, INVALID, 0);
   NamesSetStyle(NAMES_COMPLEX_SELECT_NAME, NAMES_LOOP_ONCE);
   sprintf(win_name, "%s - %s", TOOL_NAME, top_str);
   if (Names(win_name, &selected_index, NULL, 0, NULL) == BUTTON_OK) {
      return selected_index;
   }
   return INVALID;
}

static
int SelectAnImportFilter(pszSelected)
   char *pszSelected;
{
   int num_entries=0, index=0;
   DspList *dsp_ptr=ImportFilterListing(&num_entries);
   char **entries;

   if (pszSelected != NULL) *pszSelected = '\0';
   if (dsp_ptr == NULL) {
      sprintf(gszMsgBox, "%s.\n\n%s %s.%s and %s.%s X resources.",
            "Cannot find any import filter specifications",
            "Import filters are specified with", TOOL_NAME, "MaxImportFilters",
            TOOL_NAME, "ImportFilter#");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return INVALID;
   }
   entries = MakeNameDspItemArray(num_entries, dsp_ptr);
   if (entries == NULL) {
      free(dsp_ptr);
      return INVALID;
   }
   if ((index=ChooseAnImportFilter("Please select an import filter ...",
         entries, num_entries)) == INVALID) {
      if (pszSelected != NULL) *pszSelected = '\0';
   } else {
      if (pszSelected != NULL) {
         strcpy(pszSelected, entries[index]);
      }
   }
   free(*entries);
   free(entries);
   free(dsp_ptr);
   return index;
}

static
int ConvertAnyToXpm(pii, pszAnyPath, pszXpmPath)
   struct ImportInfoRec *pii;
   char *pszAnyPath, *pszXpmPath;
{
   FILE *pFile=NULL, *pPipe=NULL;
   char *pszCmd=NULL, szBuf[MAXSTRING];
   int bytes_read;

   sprintf(pszXpmPath, "%sTgifXXXXXX", TMP_DIR);
   mktemp(pszXpmPath);
   unlink(pszXpmPath);
   pszCmd =
         (char*)malloc((strlen(pii->cmd)+strlen(pszAnyPath)+10)*sizeof(char));
   if (pszCmd == NULL) {
      FailAllocMessage();
      return FALSE;
   }
   if ((pFile=fopen(pszXpmPath,"w")) == NULL) {
      sprintf(gszMsgBox, "Cannot create temporary file '%s'.\n\n%s.",
            pszXpmPath, "Conversion aborted");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      free(pszCmd);
      return FALSE;
   }
   sprintf(pszCmd, pii->cmd, pszAnyPath);
   Msg("Executing:");
   sprintf(gszMsgBox, "    %s", pszCmd);
   Msg(gszMsgBox);
   sprintf(gszMsgBox, "Executing '%s'...", pszCmd);
   SetStringStatus(gszMsgBox);
   XSync(mainDisplay, False);
   if ((pPipe=(FILE*)popen(pszCmd,"r")) == NULL) {
      sprintf(gszMsgBox, "Fail to execute '%s'.\n\n%s.",
            pszCmd, "Conversion aborted");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      free(pszCmd);
      fclose(pFile);
      unlink(pszXpmPath);
      return FALSE;
   }
   writeFileFailed = FALSE;
   while ((bytes_read=fread(szBuf, sizeof(char), sizeof(szBuf), pPipe)) > 0) {
      if ((int)fwrite(szBuf, sizeof(char), bytes_read, pFile) <= 0) {
         writeFileFailed = TRUE;
         break;
      }
   }
   pclose(pPipe);
   SetStringStatus("...Done");
   free(pszCmd);
   fclose(pFile);
   if (writeFileFailed) {
      sprintf(gszMsgBox, "Fail to write to '%s'.\n\nFile system may be full.",
            pszXpmPath);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      unlink(pszXpmPath);
      return FALSE;
   }
   return TRUE;
}

static
void DoImportOtherFile(pii)
   struct ImportInfoRec *pii;
{
   char szTop[MAXSTRING+1];
   char szOtherPath[MAXPATHLENGTH+1], szXpmPath[MAXPATHLENGTH+1];
   int rc, ncolors, chars_per_pixel, *pixels, short_name;
   int first_pixel_is_bg, image_w, image_h, w, h;
   Pixmap pixmap=None, bitmap=None;
   XImage *image=NULL, *bitmap_image=NULL;
   char *color_char, **color_str, *xpm_data=NULL, *rest;
   struct ObjRec *obj_ptr;
   XEvent ev;

   MakeQuiescent();

   sprintf(szTop, "Please select a %s file to IMPORT...", pii->name);
   importingFile = TRUE;
   if (importFromLibrary) {
      char name[MAXSTRING+1], dir_name[MAXSTRING+1];

      if (SelectFromLibrary(szTop, pii->ext, name, dir_name) == INVALID) {
         importingFile = FALSE;
         return;
      }
      sprintf(szOtherPath, "%s/%s", dir_name, name);
   } else if (SelectFileNameToImport(szTop, pii->ext, szOtherPath) == INVALID) {
      importingFile = FALSE;
      return;
   } else if (FileIsRemote(szOtherPath)) {
      sprintf(gszMsgBox, "Importing remote %s file is not supported.",
            pii->name);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      importingFile = FALSE;
      return;
   }
   XSync(mainDisplay, False);
   if (XCheckMaskEvent(mainDisplay, ExposureMask, &ev)) {
      ExposeEventHandler(&ev, TRUE);
   }
   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   SaveStatusStrings();
   rc = ConvertAnyToXpm(pii, szOtherPath, szXpmPath);
   RestoreStatusStrings();
   SetDefaultCursor(mainWindow);
   ShowCursor();
   if (!rc) return;

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   rc = MyReadPixmapFile(szXpmPath, &image_w, &image_h, &w, &h, &pixmap,
         &image, &bitmap, &bitmap_image, &ncolors, &chars_per_pixel,
         &first_pixel_is_bg, &color_char, &color_str, &pixels, &xpm_data);
   SetDefaultCursor(mainWindow);
   ShowCursor();

   if ((short_name=IsPrefix(bootDir, szOtherPath, &rest))) ++rest;
   if (rc != BitmapSuccess) {
      sprintf(gszMsgBox, "Cannot import %s file '%s'.",
            pii->name, (short_name ? rest : szOtherPath));
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      importingFile = FALSE;
      unlink(szXpmPath);
      return;
   }
   unlink(szXpmPath);
   obj_ptr = CreateXPmObj(image_w, image_h, w, h, pixmap, image, bitmap,
         bitmap_image, ncolors, chars_per_pixel, first_pixel_is_bg, color_char,
         color_str, pixels, xpm_data);
   AddObj(NULL, topObj, obj_ptr);
   PlaceTopObj(obj_ptr);

   SelectTopObj();
   RecordNewObjCmd();
   SetFileModified(TRUE);
   justDupped = FALSE;
   if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
      RedrawColorWindow();
   }
   if (!importFromLibrary) SetCurImportDir(szOtherPath);

   sprintf(gszMsgBox, "%s file (%1dx%1d) '%s' imported.", pii->name,
         image_w, image_h, (short_name ? rest : szOtherPath));
   Msg(gszMsgBox);
   importingFile = FALSE;
}

void ImportOtherFile()
{
   int i, index;
   struct ImportInfoRec *pii;
   XEvent ev;

   if ((index=SelectAnImportFilter(NULL)) == INVALID) {
      return;
   }
   for (i=0, pii=topImportInfo; i < index && pii != NULL; i++, pii=pii->next) {
   }
   if (pii == NULL) return;

   XSync(mainDisplay, False);
   if (XCheckMaskEvent(mainDisplay, ExposureMask, &ev)) {
      ExposeEventHandler(&ev, TRUE);
   }
   DoImportOtherFile(pii);
}

void ImportOtherFileType(pszName)
   char *pszName;
{
   char *paren_ptr=(pszName == NULL ? NULL : strchr(pszName, ')'));
   struct ImportInfoRec *pii;

   if (paren_ptr == NULL) {
      sprintf(gszMsgBox, "%s ImportOtherFileType('%s'.",
            "Invalid format in shortcut specification of",
            pszName == NULL ? "" : pszName);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   *paren_ptr = '\0';
   for (pii=topImportInfo; pii != NULL; pii=pii->next) {
      if (strcmp(pii->name, pszName) == 0) {
         break;
      }
   }
   if (pii == NULL) {
      sprintf(gszMsgBox, "%s '%s'.",
            "Cannot find any import filter named", pszName);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   DoImportOtherFile(pii);
}

/* ----------------------- Browse Functions ----------------------- */

static int origBrowseX=0, origBrowseY=0, maxBrowseX=0, maxBrowseY=0;
static int curBrowseX=0, curBrowseY=0, curBrowseRowHeight=0;
static int savedDirNameLen=0;
static char savedDirName[MAXPATHLENGTH+1];

static
void InitBrowse(pszDir)
   char *pszDir;
{
   curBrowseX = origBrowseX = drawOrigX+ABS_SIZE(EIGHTH_INCH);
   maxBrowseX = drawOrigX+drawWinW;
   curBrowseY = origBrowseY = drawOrigY+ABS_SIZE(EIGHTH_INCH);
   maxBrowseY = drawOrigY+drawWinH;
   curBrowseRowHeight = 0;
   if (pszDir != NULL) {
      strcpy(savedDirName, pszDir);
      savedDirNameLen = strlen(savedDirName);
   }
}

static
int CheckExecInterrupt()
{
   XEvent ev;

   while (XCheckMaskEvent(mainDisplay, StructureNotifyMask, &ev)) {
      if (iconWindowShown) {
         if ((ev.xany.window == iconBaseWindow && ev.type == UnmapNotify) ||
               (ev.xany.window == mainWindow && ev.type == MapNotify)) {
            XPutBackEvent(mainDisplay, &ev);
            return TRUE;
         }
      } else if ((ev.xany.window == iconBaseWindow && ev.type == MapNotify) ||
            (ev.xany.window == mainWindow && ev.type == UnmapNotify)) {
         XPutBackEvent(mainDisplay, &ev);
         return TRUE;
      } else if (ev.type == ConfigureNotify) {
         Reconfigure(FALSE);
         maxBrowseX = drawOrigX+drawWinW;
      }
   }
   while (XCheckMaskEvent(mainDisplay, VisibilityChangeMask, &ev)) {
      if (iconWindowShown) {
         if (ev.xany.window == mainWindow && ev.type == VisibilityNotify &&
               ev.xvisibility.state == VisibilityUnobscured) {
            XPutBackEvent(mainDisplay, &ev);
            return TRUE;
         } else {
            ExposeEventHandler(&ev, TRUE);
         }
      } else {
         if (ev.xany.window == iconBaseWindow && ev.type == VisibilityNotify &&
               ev.xvisibility.state == VisibilityUnobscured) {
            XPutBackEvent(mainDisplay, &ev);
            return TRUE;
         } else {
            ExposeEventHandler(&ev, TRUE);
         }
      }
   }
   if (XCheckMaskEvent(mainDisplay, ExposureMask, &ev)) {
      ExposeEventHandler(&ev, TRUE);
      while (XCheckMaskEvent(mainDisplay, ExposureMask, &ev)) ;
   }
   if (ESCPressed() || CheckInterrupt()) {
      return TRUE;
   }
   while (XCheckMaskEvent(mainDisplay, ButtonPressMask|KeyPressMask, &ev)) ;
   return FALSE;
}

static
int BrowseDir(DirName, ExtStr, ExtStrLen, ObjType, pii)
   char *DirName, *ExtStr;
   int ExtStrLen, ObjType;
   struct ImportInfoRec *pii;
   /* returns TRUE if interrupted */
{
   char name[MAXPATHLENGTH+1];
   int interrupted=FALSE, len;
   DIR *dirp;
   DIR_ENTRY *d;

   if ((dirp=opendir(DirName)) == NULL) return interrupted;
   if (pii == NULL) {
      sprintf(gszMsgBox, "Browsing %s for %s files...", DirName, ExtStr);
   } else {
      sprintf(gszMsgBox, "Browsing %s for %s files...", DirName, pii->name);
   }
   Msg(gszMsgBox);
   while ((d=readdir(dirp)) != NULL) {
      int ncolors, chars_per_pixel, first_pixel_is_bg, * pixels;
      int image_w=0, image_h=0, w=0, h=0, x_hot, y_hot, rc;
      unsigned int tmp_w, tmp_h;
      char *color_char, **color_str, *xpm_data=NULL;
      Pixmap bitmap=None, pixmap=None;
      XImage *image=NULL, *bitmap_image=NULL;

      if (CheckExecInterrupt()) {
         interrupted = TRUE;
         break;
      }
      if (!ExtensionMatch(ExtStr, d->d_name)) {
         continue;
      }
      sprintf(name, "%s/%s", DirName, d->d_name);
      sprintf(gszMsgBox, "Opening %s...", &name[savedDirNameLen+1]);
      SetStringStatus(gszMsgBox);
      if (ObjType == OBJ_XBM) {
         rc = XReadBitmapFile(mainDisplay, mainWindow, name, &tmp_w, &tmp_h,
               &bitmap, &x_hot, &y_hot);
         w = (int)tmp_w; h = (int)tmp_h;
      } else {
         if (pii != NULL) {
            char szXpmPath[MAXPATHLENGTH+1];

            SaveStatusStrings();
            rc = ConvertAnyToXpm(pii, name, szXpmPath);
            RestoreStatusStrings();
            if (!rc) continue;
            rc = MyReadPixmapFile(szXpmPath, &image_w, &image_h, &w, &h,
                  &pixmap, &image, &bitmap, &bitmap_image, &ncolors,
                  &chars_per_pixel, &first_pixel_is_bg, &color_char,
                  &color_str, &pixels, &xpm_data);
            unlink(szXpmPath);
         } else {
            rc = MyReadPixmapFile(name, &image_w, &image_h, &w, &h,
                  &pixmap, &image, &bitmap, &bitmap_image, &ncolors,
                  &chars_per_pixel, &first_pixel_is_bg, &color_char,
                  &color_str, &pixels, &xpm_data);
         }
      }
      if (rc == BitmapSuccess) {
         struct ObjRec *obj_ptr;
         int short_name;
         char *rest;

         if (ObjType == OBJ_XBM) {
            obj_ptr = CreateXBmObj(w, h, w, h, bitmap, NULL);
         } else {
            obj_ptr = CreateXPmObj(image_w, image_h, w, h, pixmap, image,
                  bitmap, bitmap_image, ncolors, chars_per_pixel,
                  first_pixel_is_bg, color_char, color_str, pixels, xpm_data);
         }
         if ((short_name=IsPrefix((curDirIsLocal ? curDir : curLocalDir), name,
               &rest))) {
            ++rest;
         }
         AddAttrByNameAndValue(obj_ptr, "file_name=", short_name ? rest : name);
         if (showFileNameOnBrowse && obj_ptr->fattr != NULL &&
               obj_ptr->fattr == obj_ptr->lattr) {
            struct AttrRec *attr_ptr=obj_ptr->fattr;
            int attr_h, attr_w, attr_ltx, attr_lty;

            attr_ptr->shown = TRUE;
            attr_ptr->nameshown = FALSE;
            UpdAttr(attr_ptr);
            AdjObjBBox(attr_ptr->obj);
            attr_w = attr_ptr->obj->obbox.rbx-attr_ptr->obj->obbox.ltx;
            attr_h = attr_ptr->obj->bbox.rby-attr_ptr->obj->bbox.lty;
            attr_ltx = ((obj_ptr->obbox.ltx+obj_ptr->obbox.rbx)>>1)-(attr_w>>1);
            attr_lty = obj_ptr->bbox.rby;
            MoveObj(attr_ptr->obj, attr_ltx-attr_ptr->obj->obbox.ltx,
                  attr_lty-attr_ptr->obj->obbox.lty);
            if (attr_w > w) w = attr_w;
            h += attr_h;
         }
         AdjObjBBox(obj_ptr);
         if (curBrowseX+w > maxBrowseX) {
            curBrowseX = origBrowseX;
            curBrowseY += curBrowseRowHeight;
            while (curBrowseY > maxBrowseY) {
               ForceScrollDown(TRUE);
               maxBrowseY = drawOrigY+drawWinH;
            }
            curBrowseRowHeight = h;
         } else if (h > curBrowseRowHeight) {
            curBrowseRowHeight = h;
         }
         obj_ptr->tmp_parent = NULL;
         AddObj(NULL, topObj, obj_ptr);

         MoveObj(topObj, curBrowseX-topObj->bbox.ltx,
               curBrowseY-topObj->bbox.lty);
         numRedrawBBox = 0;
         DrawObj(drawWindow, topObj);
         RecordNewObjCmd();
         SetFileModified(TRUE);
         justDupped = FALSE;
         XSync(mainDisplay, False);
         curBrowseX += w;
      }
   }
   closedir(dirp);
   if ((dirp=opendir(DirName)) == NULL) return interrupted;
   while (!interrupted && (d=readdir(dirp)) != NULL) {
      if (CheckExecInterrupt()) {
         interrupted = TRUE;
         break;
      }
#ifdef VMS
      len = strlen(d->d_name);
      if (len > 4 && (strcmp(".dir", &d->d_name[len-4]) == 0)) {
         sprintf(name, "%s/%s", DirName, d->d_name);
         if (BrowseDir(name, ExtStr, ExtStrLen, ObjType, pii)) {
            interrupted = TRUE;
            break;
         }
      }
#endif /* VMS */
      if (strcmp(d->d_name, ".") != 0 && strcmp(d->d_name, "..") != 0) {
         struct stat stat_buf;

         sprintf(name, "%s/%s", DirName, d->d_name);
         stat(name, &stat_buf);
         if (stat_buf.st_mode & S_IFDIR) {
            int skip=FALSE;

#ifndef _NO_LSTAT
            lstat(name, &stat_buf);
            if (stat_buf.st_mode & S_IFLNK) {
               sprintf(gszMsgBox, "Skipping %s because it's a link.",
                     &name[savedDirNameLen+1]);
               Msg(gszMsgBox);
               skip = TRUE;
            }
#endif /* !_NO_LSTAT */
            if (!interrupted && !skip &&
                  BrowseDir(name, ExtStr, ExtStrLen, ObjType, pii)) {
               interrupted = TRUE;
               break;
            }
         }
      }
   }
   closedir(dirp);
   return interrupted;
}

static
void StartBrowse(DirName, ExtStr, ExtStrLen, ObjType, pii)
   char *DirName, *ExtStr;
   int ExtStrLen, ObjType;
   struct ImportInfoRec *pii;
{
   int saved_text_just=textJust;

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   MakeQuiescent();
   SaveStatusStrings();
   InitBrowse(DirName);
   StartCompositeCmd();
   ShowInterrupt(1);
   textJust = JUST_C;
   if (BrowseDir(DirName, ExtStr, ExtStrLen, ObjType, pii)) {
      Msg("Interrupted by the user.");
   }
   textJust = saved_text_just;
   if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
      RedrawColorWindow();
   }
   HideInterrupt();
   EndCompositeCmd();
   RestoreStatusStrings();
   SetDefaultCursor(mainWindow);
   ShowCursor();
   XFlush(mainDisplay);
}

void BrowseXBitmap()
{
   StartBrowse((curDirIsLocal ? curDir : curLocalDir), XBM_FILE_EXT,
         strlen(XBM_FILE_EXT), OBJ_XBM, NULL);
}

void BrowseXPixmap()
{
   StartBrowse((curDirIsLocal ? curDir : curLocalDir), XPM_FILE_EXT,
         strlen(XPM_FILE_EXT), OBJ_XPM, NULL);
}

static
void DoBrowseOther(pii)
   struct ImportInfoRec *pii;
{
   char szTop[MAXSTRING+1];
   char szOtherPath[MAXPATHLENGTH+1], szXpmPath[MAXPATHLENGTH+1];
   int rc, ncolors, chars_per_pixel, *pixels, short_name;
   int first_pixel_is_bg, image_w, image_h, w, h;
   Pixmap pixmap=None, bitmap=None;
   XImage *image=NULL, *bitmap_image=NULL;
   char *color_char, **color_str, *xpm_data=NULL, *rest;
   struct ObjRec *obj_ptr;
   XEvent ev;

   sprintf(szTop, "Please select a %s file to IMPORT...", pii->name);
   importingFile = TRUE;
   if (importFromLibrary) {
      char name[MAXSTRING+1], dir_name[MAXSTRING+1];

      if (SelectFromLibrary(szTop, pii->ext, name, dir_name) == INVALID) {
         importingFile = FALSE;
         return;
      }
      sprintf(szOtherPath, "%s/%s", dir_name, name);
   } else if (SelectFileNameToImport(szTop, pii->ext, szOtherPath) == INVALID) {
      importingFile = FALSE;
      return;
   } else if (FileIsRemote(szOtherPath)) {
      sprintf(gszMsgBox, "Importing remote %s file is not supported.",
            pii->name);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      importingFile = FALSE;
      return;
   }
   XSync(mainDisplay, False);
   if (XCheckMaskEvent(mainDisplay, ExposureMask, &ev)) {
      ExposeEventHandler(&ev, TRUE);
   }
   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   SaveStatusStrings();
   rc = ConvertAnyToXpm(pii, szOtherPath, szXpmPath);
   RestoreStatusStrings();
   SetDefaultCursor(mainWindow);
   ShowCursor();
   if (!rc) return;

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   rc = MyReadPixmapFile(szXpmPath, &image_w, &image_h, &w, &h, &pixmap,
         &image, &bitmap, &bitmap_image, &ncolors, &chars_per_pixel,
         &first_pixel_is_bg, &color_char, &color_str, &pixels, &xpm_data);
   SetDefaultCursor(mainWindow);
   ShowCursor();

   if ((short_name=IsPrefix(bootDir, szOtherPath, &rest))) ++rest;
   if (rc != BitmapSuccess) {
      sprintf(gszMsgBox, "Cannot import %s file '%s'.",
            pii->name, (short_name ? rest : szOtherPath));
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      importingFile = FALSE;
      unlink(szXpmPath);
      return;
   }
   unlink(szXpmPath);
   obj_ptr = CreateXPmObj(image_w, image_h, w, h, pixmap, image, bitmap,
         bitmap_image, ncolors, chars_per_pixel, first_pixel_is_bg, color_char,
         color_str, pixels, xpm_data);
   AddObj(NULL, topObj, obj_ptr);
   PlaceTopObj(obj_ptr);

   SelectTopObj();
   RecordNewObjCmd();
   SetFileModified(TRUE);
   justDupped = FALSE;
   if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
      RedrawColorWindow();
   }
   if (!importFromLibrary) SetCurImportDir(szOtherPath);

   sprintf(gszMsgBox, "%s file (%1dx%1d) '%s' imported.", pii->name,
         image_w, image_h, (short_name ? rest : szOtherPath));
   Msg(gszMsgBox);
   importingFile = FALSE;
}

void BrowseOther()
{
   int i, index;
   char *ext_str;
   struct ImportInfoRec *pii;
   XEvent ev;

   if ((index=SelectAnImportFilter(NULL)) == INVALID) {
      return;
   }
   for (i=0, pii=topImportInfo; i < index && pii != NULL; i++, pii=pii->next) {
   }
   if (pii == NULL) return;
   XSync(mainDisplay, False);
   if (XCheckMaskEvent(mainDisplay, ExposureMask, &ev)) {
      ExposeEventHandler(&ev, TRUE);
   }
   if ((ext_str=SetUpExtStr((strlen(pii->ext)<<1)+3, pii->ext, "")) != NULL) {
      StartBrowse((curDirIsLocal ? curDir : curLocalDir), ext_str,
            strlen(pii->ext), OBJ_XPM, pii);
      free(ext_str);
   }
}

void BrowseOtherType(pszName)
   char *pszName;
{
   char *paren_ptr=(pszName == NULL ? NULL : strchr(pszName, ')')), *ext_str;
   struct ImportInfoRec *pii;

   if (paren_ptr == NULL) {
      sprintf(gszMsgBox, "%s ImportOtherFileType('%s'.",
            "Invalid format in shortcut specification of",
            pszName == NULL ? "" : pszName);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   *paren_ptr = '\0';
   for (pii=topImportInfo; pii != NULL; pii=pii->next) {
      if (strcmp(pii->name, pszName) == 0) {
         break;
      }
   }
   if (pii == NULL) {
      sprintf(gszMsgBox, "%s '%s'.",
            "Cannot find any import filter named", pszName);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if ((ext_str=SetUpExtStr((strlen(pii->ext)<<1)+3, pii->ext, "")) != NULL) {
      StartBrowse((curDirIsLocal ? curDir : curLocalDir), ext_str,
            strlen(pii->ext), OBJ_XPM, pii);
      free(ext_str);
   }
}
