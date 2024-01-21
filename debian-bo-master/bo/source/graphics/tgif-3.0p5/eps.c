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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/eps.c,v 3.1 1996/05/11 05:40:45 william Exp $";
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "const.h"
#include "types.h"

#include "cmd.e"
#include "color.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#ifndef _NO_EXTERN
#include "eps.e"
#endif
#include "file.e"
#include "grid.e"
#include "mark.e"
#include "msg.e"
#include "obj.e"
#include "pattern.e"
#include "select.e"
#include "setup.e"
#include "util.e"
#include "xbitmap.e"

#ifndef SEEK_SET
#define SEEK_SET 0
#endif /* ~SEEK_SET */

extern char *mktemp ARGS_DECL((char*));

#ifdef __hpux
extern double	atof ARGS_DECL((const char *));
#else
extern double	atof ARGS_DECL((char *));
#endif

typedef struct LineRec {
   char			* s;
   struct LineRec	* next, * prev;
} * LineRecPtr;

float	defaultEPSScaling=1.0;
char	defaultEPSScalingStr[80];

static struct LineRec	* topLine=NULL, * botLine=NULL;
static int		numLines = 0;

static char tiffToXbmCmd[MAXSTRING+1];

void InitEPS ()
{
   char		* c_ptr;

   stripEPSComments = TRUE;
   if ((c_ptr = XGetDefault(mainDisplay,TOOL_NAME,"StripEPSComments")) != NULL)
      if (strcmp ("False", c_ptr) == 0 || strcmp ("false", c_ptr) == 0)
         stripEPSComments = FALSE;

   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"ForceClearAfterEPS")) != NULL)
      fprintf (stderr, "%s*ForceClearAfterEPS is obsolete.\n", TOOL_NAME);

   defaultEPSScaling = 1.0;
   strcpy (defaultEPSScalingStr, "1");
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"DefaultEPSScaling")) != NULL)
   {
      strcpy (defaultEPSScalingStr, c_ptr);
      defaultEPSScaling = (float) atof (c_ptr);
      if (defaultEPSScaling <= 0.0)
      {
         fprintf (stderr, "Invalid %s*DefaultEPSScaling: '%s', %s.\n",
               TOOL_NAME, c_ptr, "1.0 is used");
         defaultEPSScaling = 1.0;
         strcpy (defaultEPSScalingStr, "1");
      }
      else if (strcmp(defaultEPSScalingStr,"1")==0 ||
            strcmp(defaultEPSScalingStr,"1.0")==0)
      {
         defaultEPSScaling = 1.0;
         strcpy (defaultEPSScalingStr, "1");
      }
   }
   strcpy(tiffToXbmCmd, "tifftopnm %s | pgmtopbm | pbmtoxbm");
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"TiffToXbm")) != NULL) {
      int count=0;

      strcpy(tiffToXbmCmd, c_ptr);
      for (c_ptr=strstr(tiffToXbmCmd,"%s"); c_ptr!=NULL;
            c_ptr=strstr(++c_ptr,"%s")) {
         count++;
      }
      if (count != 1) {
         fprintf(stderr, "Invalid %s*%s: '%s'.\n\n'%s' used.",
               TOOL_NAME, "TiffToXbm", tiffToXbmCmd,
               "tifftopnm %s | pgmtopbm | pbmtoxbm");
         strcpy(tiffToXbmCmd, "tifftopnm %s | pgmtopbm | pbmtoxbm");
      }
   }
}

static
void CleanUpLines()
{
   register struct LineRec *line_ptr, *next_line;

   for (line_ptr=topLine; line_ptr != NULL; line_ptr = next_line) {
      next_line = line_ptr->next;
      if (line_ptr->s != NULL) free(line_ptr->s);
      free(line_ptr);
   }
   topLine = botLine = NULL;
   numLines = 0;
}

void CleanUpEPS ()
{
   CleanUpLines ();
   stripEPSComments = TRUE;
}

static
int ReadPreviewBitmap (fp, image_w, image_h, bps, bitmap, image)
   FILE		* fp;
   int		image_w, image_h, bps;
   Pixmap	* bitmap;
   XImage	* * image;
{
   register int	k, j;
   int		i, num_nibbles, bit_count;
   char		* line, * c_ptr;

   switch (bps)
   {
      case 1:
         num_nibbles = ((image_w & 0x3)==0) ? (int)(image_w>>2) :
               (int)(image_w>>2)+1;
         if (num_nibbles & 0x1) num_nibbles++;
         break;
      default:
         sprintf(gszMsgBox, "%1d bits per sample preview not supported.", bps);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         return FALSE;
   }
   line = (char*)malloc((num_nibbles+10)*sizeof(char));
   if (line == NULL) FailAllocMessage();
   *bitmap = XCreatePixmap(mainDisplay, dummyBitmap, image_w, image_h, 1);
   XFillRectangle(mainDisplay, *bitmap, xbmGC, 0, 0, image_w, image_h);
   *image = XGetImage(mainDisplay,*bitmap,0,0,image_w,image_h,1,ZPixmap);

   for (i=0; i < image_h; i++) {
      for (j=0, c_ptr = line; j < num_nibbles; j++, c_ptr++) {
         while (TRUE) {
            if ((*c_ptr = (char) getc (fp)) == EOF) {
               MsgBox("Invalid preview bitmap in EPS file.", TOOL_NAME,
                     INFO_MB);
               free(line);
               XFreePixmap(mainDisplay, *bitmap); *bitmap = None;
               XDestroyImage(*image); *image = NULL;
               return FALSE;
            }
            if ((*c_ptr >= '0' && *c_ptr <= '9') ||
                  (*c_ptr >= 'a' && *c_ptr <= 'f') ||
                  (*c_ptr >= 'A' && *c_ptr <= 'F')) {
               break;
            }
         }
      }
      *c_ptr = '\0';
      bit_count = 0;
      c_ptr = line;
      for (j=0; j<num_nibbles && *c_ptr!='\0'; j++, c_ptr++) {
         int data;

         if (*c_ptr >= '0' && *c_ptr <= '9') {
            data = (int)(*c_ptr) - (int)('0');
         } else if (*c_ptr >= 'a' && *c_ptr <= 'f') {
            data = (int)(*c_ptr) - (int)('a') + 10;
         } else if (*c_ptr >= 'A' && *c_ptr <= 'F') {
            data = (int)(*c_ptr) - (int)('A') + 10;
         } else {
            break;
         }

         for (k = 0; k < 4; k++) {
            if (bit_count++ == image_w) break;

            if (data & (1<<(3-k))) {
               XPutPixel(*image, j*4+k, i, 1);
            }
         }
      }
   }
   if (fgets(line, MAXSTRING, fp) == NULL) {
      MsgBox("Invalid preview bitmap in EPS file.", TOOL_NAME, INFO_MB);
      free(line);
      XFreePixmap(mainDisplay, *bitmap); *bitmap = None;
      XDestroyImage(*image); *image = NULL;
      return FALSE;
   }
   XPutImage(mainDisplay,*bitmap,xbmGC,*image,0,0,0,0,image_w,image_h);
   free(line);
   return (TRUE);
}

static
void AddLine(s)
   char *s;
{
   struct LineRec *line_ptr;

   line_ptr = (struct LineRec *)malloc(sizeof(struct LineRec));
   if (line_ptr == NULL) FailAllocMessage();
   line_ptr->s = s;

   line_ptr->next = NULL;
   line_ptr->prev = botLine;
   if (botLine == NULL)
      topLine = line_ptr;
   else
      botLine->next = line_ptr;
   botLine = line_ptr;

   numLines++;
}

static
unsigned long ReadDoubleWord(buf)
   char *buf;
{
   unsigned long lval, total=0L;
   unsigned char *c_ptr=(unsigned char *)buf;

   lval = (unsigned long)(*c_ptr++);
   total += lval;
   lval = (unsigned long)(*c_ptr++);
   total += (lval<<8);
   lval = (unsigned long)(*c_ptr++);
   total += (lval<<16);
   lval = (unsigned long)(*c_ptr++);
   total += (lval<<24);
   return total;
}

static char hexValue[]="0123456789abcdef";

static
int XbmToPreviewBitmap(FP, xbm_fname)
   FILE *FP;
   char *xbm_fname;
{
   unsigned int image_w, image_h;
   int rc, x_hot, y_hot, **data, num_image_bytes_per_row, num_lines;
   int row, col;
   Pixmap bitmap;
   XImage *image;

   if ((rc=XReadBitmapFile(mainDisplay, mainWindow, xbm_fname, &image_w,
         &image_h, &bitmap, &x_hot, &y_hot)) != BitmapSuccess) {
      return FALSE;
   }
   image = XGetImage(mainDisplay, bitmap, 0, 0, image_w, image_h, 1, ZPixmap);
   if (image == NULL) {
      XFreePixmap(mainDisplay, bitmap);
      return FALSE;
   }
   num_image_bytes_per_row = ((image_w & 0x7) ? (image_w>>3)+1 : (image_w>>3));
   num_lines = ((image_w & 0x7) ? (((image_w>>3)+1)<<1) : ((image_w>>3)<<1));
   num_lines = ((num_lines & 0x3f) ? (num_lines>>6)+1 : (num_lines>>6));

   if ((data=(int**)malloc(image_h*sizeof(int*))) == NULL) {
      XFreePixmap(mainDisplay, bitmap);
      XDestroyImage(image);
      return FailAllocMessage();
   }
   for (row=0; row < image_h; row++) {
      if ((data[row]=(int*)malloc(num_image_bytes_per_row*sizeof(int))) ==
            NULL) {
         int i;

         for (i=0; i < row; i++) free(data[i]);
         free(data);
         XFreePixmap(mainDisplay, bitmap);
         XDestroyImage(image);
         return FailAllocMessage();
      } else {
         for (col=0; col<num_image_bytes_per_row; col++) {
            data[row][col] = 0;
         }
      }
   }
   fprintf(FP, "%%!\n");
   fprintf(FP, "%%%%BeginPreview: %1d %1d 1 %1d\n", image_w, image_h,
         num_lines*image_h);
   for (row=0; row < image_h; row++) {
      for (col=0; col < image_w; col++) {
         if (XGetPixel(image, col, row) != 0) {
            data[row][col>>3] |= (1<<(7 - (col & 0x7)));
         }
      }
   }
   for (row=0; row < image_h; row++) {
      int byte_count=0;

      fprintf(FP, "%% ");
      for (col=0; col < num_image_bytes_per_row; col++) {
         if (byte_count++ == 32) {
            byte_count = 1;
            fprintf(FP, "\n%% ");
         }
         fprintf(FP, "%c", hexValue[(data[row][col]>>4) & 0xf]);
         fprintf(FP, "%c", hexValue[data[row][col] & 0xf]);
      }
      fprintf (FP, "\n");
   }
   fprintf(FP, "%%%%EndImage\n");
   fprintf(FP, "%%%%EndPreview\n");
   
   for (row=0; row < image_h; row++) free(data[row]);
   free(data);
   XDestroyImage(image);
   XFreePixmap(mainDisplay, bitmap);
   return TRUE;
}

static
int ErrorInConvertTiffToXbm(fp, tiff_fp, xbm_fp, fname)
   FILE *fp, *tiff_fp, *xbm_fp;
   char *fname;
{
   if (fp != NULL) fclose(fp);
   if (tiff_fp != NULL) fclose(tiff_fp);
   if (xbm_fp != NULL) fclose(xbm_fp);
   if (fname != NULL) {
      if (writeFileFailed) {
         sprintf(gszMsgBox,
               "Fail to write to '%s'.\n\nFile system may be full.", fname);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      }
      unlink(fname);
   }
   return FALSE;
}

static
int ConvertTiffToXbm(fp, tiff_offset, tiff_sz, xbm_fname)
   FILE *fp;
   int tiff_offset, tiff_sz;
   char *xbm_fname;
{
   FILE *tiff_fp=NULL, *xbm_fp=NULL, *pfp;
   int bytes_to_read, bytes_read;
   char tiff_fname[MAXPATHLENGTH+1];
   char cmd[(MAXSTRING<<1)+1];

   *tiff_fname = *xbm_fname = '\0';
   /* extract TIFF into a file */
   if (fseek(fp, tiff_offset, SEEK_SET) != 0) {
      return ErrorInConvertTiffToXbm(fp, NULL, NULL, NULL);
   }
   sprintf(tiff_fname, "%sTgifXXXXXX", TMP_DIR);
   mktemp(tiff_fname);
   unlink(tiff_fname);
   if ((tiff_fp=fopen(tiff_fname, "w")) == NULL) {
      return ErrorInConvertTiffToXbm(fp, NULL, NULL, NULL);
   }
   while (tiff_sz > 0) {
      bytes_to_read = min(tiff_sz, sizeof(gszMsgBox));
      if (bytes_to_read !=
            (int)fread(gszMsgBox, sizeof(char), bytes_to_read, fp)) {
         return ErrorInConvertTiffToXbm(fp, tiff_fp, NULL, tiff_fname);
      }
      if ((int)fwrite(gszMsgBox, sizeof(char), bytes_to_read, tiff_fp) <= 0) {
         writeFileFailed = TRUE;
         return ErrorInConvertTiffToXbm(fp, tiff_fp, NULL, tiff_fname);
      }
      tiff_sz -= bytes_to_read;
   }
   fclose(tiff_fp);
   tiff_fp = NULL;

   /* convert TIFF to XBM */
   sprintf(xbm_fname, "%sTgifXXXXXX", TMP_DIR);
   mktemp(xbm_fname);
   unlink(xbm_fname);
   if ((xbm_fp=fopen(xbm_fname, "w")) == NULL) {
      return ErrorInConvertTiffToXbm(fp, NULL, NULL, NULL);
   }
   sprintf(cmd, tiffToXbmCmd, tiff_fname);
   sprintf(gszMsgBox, "Executing '%s'...", cmd);
   SetStringStatus(gszMsgBox);
   XSync(mainDisplay, False);
   if ((pfp=(FILE*)popen(cmd,"r")) == NULL) {
      unlink(tiff_fname);
      return ErrorInConvertTiffToXbm(fp, NULL, xbm_fp, xbm_fname);
   }
   while ((bytes_read=(int)fread(gszMsgBox, sizeof(char), sizeof(gszMsgBox),
         pfp)) > 0) {
      if ((int)fwrite(gszMsgBox, sizeof(char), bytes_read, xbm_fp) <= 0) {
         writeFileFailed = TRUE;
         break;
      }
   }
   pclose(pfp);
   SetStringStatus("...Done");
   fclose(xbm_fp);
   xbm_fp = NULL;
   unlink(tiff_fname);
   *tiff_fname = '\0';
   if (writeFileFailed) {
      return ErrorInConvertTiffToXbm(fp, NULL, xbm_fp, xbm_fname);
   }
   return TRUE;
}

static
FILE *ErrorInGetTiffEPSIInfo(fp, eps_fp, fname)
   FILE *fp, *eps_fp;
   char *fname;
{
   if (fp != NULL) fclose(fp);
   if (eps_fp != NULL) fclose(eps_fp);
   if (fname != NULL) {
      if (writeFileFailed) {
         sprintf(gszMsgBox,
               "Fail to write to '%s'.\n\nFile system may be full.", fname);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      }
      unlink(fname);
   }
   return NULL;
}

static
FILE *GetTiffEPSIInfo(fp, pszEPS, pnPreviewOK)
   FILE *fp;
   char *pszEPS;
   int *pnPreviewOK;
{
   int bytes_to_read=0x1e, bytes_read, tiff_sz, tiff_offset, eps_sz;
   int need_to_check_ps=TRUE;
   char buf[0x20], xbm_fname[MAXPATHLENGTH+1];
   FILE *eps_fp=NULL;

   writeFileFailed = FALSE;
   *xbm_fname = '\0';
   *pszEPS = '\0';
   rewind(fp);
   if (fread(buf, sizeof(char), bytes_to_read, fp) != bytes_to_read) {
      return ErrorInGetTiffEPSIInfo(fp, eps_fp, NULL);
   }
   eps_sz = ReadDoubleWord(&buf[0x08]);
   tiff_offset = ReadDoubleWord(&buf[0x14]);
   tiff_sz = ReadDoubleWord(&buf[0x18]);

   *pnPreviewOK = ConvertTiffToXbm(fp, tiff_offset, tiff_sz, xbm_fname);

   if (fseek(fp, 0x1e, SEEK_SET) != 0) {
      return ErrorInGetTiffEPSIInfo(fp, NULL, NULL);
   }
   sprintf(pszEPS, "%sTgifXXXXXX", TMP_DIR);
   mktemp(pszEPS);
   unlink(pszEPS);
   if ((eps_fp=fopen(pszEPS, "w")) == NULL) {
      return ErrorInGetTiffEPSIInfo(fp, NULL, NULL);
   }
   if (*pnPreviewOK && !XbmToPreviewBitmap(eps_fp, xbm_fname)) {
      *pnPreviewOK = FALSE;
   }
   unlink(xbm_fname);
   *xbm_fname = '\0';

   /* copy the EPS content from the original file to the new EPS file */
   while (eps_sz > 0) {
      bytes_to_read = min(eps_sz, sizeof(gszMsgBox));
      if (bytes_to_read !=
            (int)fread(gszMsgBox, sizeof(char), bytes_to_read, fp)) {
         return ErrorInGetTiffEPSIInfo(fp, eps_fp, pszEPS);
      }
      if (need_to_check_ps) {
         need_to_check_ps = FALSE;
         if (!(bytes_to_read>=2 && gszMsgBox[0]=='%' && gszMsgBox[1]=='!')) {
            fclose(fp);
            fclose(eps_fp);
            unlink(pszEPS);
            return NULL;
         }
      }
      if ((int)fwrite(gszMsgBox, sizeof(char), bytes_to_read, eps_fp) <= 0) {
         writeFileFailed = TRUE;
         return ErrorInGetTiffEPSIInfo(fp, eps_fp, pszEPS);
      }
      eps_sz -= bytes_to_read;
   }
   fclose(fp);
   fclose(eps_fp);
   return fopen(pszEPS, "r");
}

static
int DoMyReadEPSFile (file_name, image_w, image_h, bitmap, image, num_lines,
      epsflines, epsf_level, llx, lly, urx, ury, write_date)
   char		* file_name;
   int		* image_w, * image_h, * num_lines, * epsf_level;
   Pixmap	* bitmap;
   XImage	* * image;
   char		* * * epsflines, * write_date;
   float	* llx, * lly, * urx, * ury;
{
   register struct LineRec	* line_ptr, * next_line;
   register int	i;
   char		* line=NULL, * c_ptr, loc_time[MAXSTRING+1];
   char		tiff_eps_fname[MAXPATHLENGTH+1];
   int		first_line=TRUE;
   int		boundingbox_found=FALSE, preview_found=FALSE, found;
   int		boundingbox_atend=FALSE, tiff_preview_ok=FALSE;
   FILE		* fp;
   struct stat	stat_buf;

   *image_w = *image_h = *num_lines = *epsf_level = 0;
   *bitmap = None;
   *image = NULL;
   *epsflines = NULL;

   if ((fp = fopen (file_name, "r")) == NULL) return (BitmapOpenFailed);

   CleanUpLines ();

   *tiff_eps_fname = '\0';
   while ((line=UtilGetALine(fp)) != NULL)
   {
      int need_to_free_line=TRUE;

      if (first_line)
      {
         if (line[0] == ((char)0xc5) && line[1] == ((char)0xd0) &&
               line[2] == ((char)0xd3) && line[3] == ((char)0xc6) &&
               line[4] == ((char)0x1e) && line[5] == '\0') {
            free(line);
            if ((fp=GetTiffEPSIInfo(fp, tiff_eps_fname, &tiff_preview_ok)) ==
                  NULL || (line=UtilGetALine(fp)) == NULL) {
               sprintf(gszMsgBox, "Fail to parse Windows EPS file '%s'.",
                     file_name);
               if (PRTGIF || mainWindow==None) {
                  fprintf(stderr, "%s\n");
               } else {
                  MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
               }
               if (fp != NULL) fclose(fp);
               if (*tiff_eps_fname != '\0') unlink(tiff_eps_fname);
               CleanUpLines();
               return BitmapFileInvalid;
            }
         }
         first_line = FALSE;
         if (line[0] != '%' || line[1] != '!')
         {
            free(line);
            fclose (fp);
            if (*tiff_eps_fname != '\0') unlink(tiff_eps_fname);
            CleanUpLines ();
            return (BitmapFileInvalid);
         }
         AddLine (line);
         need_to_free_line = FALSE;
      }
      else if ((!boundingbox_found || boundingbox_atend) &&
            strncmp (line, "%%BoundingBox:", 14) == 0)
      {
         if (sscanf (&(line[14]), "%f %f %f %f", llx, lly, urx, ury) == 4)
            boundingbox_found = TRUE;
         else if (!boundingbox_found)
         {
            c_ptr = FindChar ((int)'(', &(line[14]));
            if (strncmp (c_ptr, "atend)", 6) == 0)
               boundingbox_atend = TRUE;
         }
      }
      else if (!preview_found && strncmp (line, "%%BeginPreview:", 15) == 0)
      {
         int	bps;
         char	*preview_line;

         if (sscanf (&(line[15]), "%d %d %d", image_w, image_h, &bps) != 3)
         {
            MsgBox("Invalid preview box in EPS file.", TOOL_NAME, INFO_MB);
            if (need_to_free_line) free(line);
            fclose (fp);
            if (*tiff_eps_fname != '\0') unlink(tiff_eps_fname);
            CleanUpLines ();
            return (BitmapFileInvalid);
         }
         else if (bps != 1)
         {
            sprintf(gszMsgBox, "%1d bits per sample preview not supported.",
                  bps);
            MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
            if (need_to_free_line) free(line);
            fclose (fp);
            if (*tiff_eps_fname != '\0') unlink(tiff_eps_fname);
            CleanUpLines ();
            return (BitmapFileInvalid);
         }
         preview_found = TRUE;
         if (!ReadPreviewBitmap (fp, *image_w, *image_h, bps, bitmap, image))
         {
            MsgBox("Invalid preview box in EPS file.", TOOL_NAME, INFO_MB);
            if (need_to_free_line) free(line);
            fclose (fp);
            if (*tiff_eps_fname != '\0') unlink(tiff_eps_fname);
            CleanUpLines ();
            return (BitmapFileInvalid);
         }
         found = FALSE;
         while ((preview_line=UtilGetALine(fp)) != NULL)
         {
            if (strncmp (preview_line, "%%EndPreview", 12) == 0)
            {
               found = TRUE;
               free(preview_line);
               break;
            }
            free(preview_line);
         }
         if (!found)
         {
            MsgBox("Invalid preview box in EPS file.", TOOL_NAME, INFO_MB);
            if (need_to_free_line) free(line);
            if (*bitmap != None)
            {
               XFreePixmap (mainDisplay, *bitmap);
               *bitmap = None;
            }
            if (*image != NULL)
            {
               XDestroyImage (*image);
               *image = NULL;
            }
            fclose (fp);
            if (*tiff_eps_fname != '\0') unlink(tiff_eps_fname);
            CleanUpLines ();
            return (BitmapFileInvalid);
         }
      }
      else
      {
         if (line[0] == '%' && line[1] == '!') (*epsf_level)++;
         if (!stripEPSComments || line[0] != '%') {
            AddLine (line);
            need_to_free_line = FALSE;
         }
      }
      if (need_to_free_line) free(line);
   }
   fclose (fp);
   if (*tiff_eps_fname != '\0') unlink(tiff_eps_fname);
   if (!boundingbox_found)
   {
      sprintf(gszMsgBox, "%s '%s'.",
            "Can not find bounding box information in", file_name);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      if (*bitmap != None)
      {
         XFreePixmap (mainDisplay, *bitmap);
         *bitmap = None;
      }
      if (*image != NULL)
      {
         XDestroyImage (*image);
         *image = NULL;
      }
      CleanUpLines ();
      return (BitmapFileInvalid);
   }
   if (*tiff_eps_fname != '\0' && !tiff_preview_ok) {
      sprintf(gszMsgBox, "%s '%s'.",
            "Unable to convert Windows EPS preview bitmap in", file_name);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   }
   stat (file_name, &stat_buf);
   strcpy (loc_time, ctime (&(stat_buf.st_mtime)));
   loc_time[24] = '\0';
   strcpy (write_date, loc_time);

   *epsflines = (char**)malloc(numLines*sizeof(char*));
   if (*epsflines == NULL) FailAllocMessage();
   *num_lines = numLines;

   for (i=0, line_ptr=topLine; line_ptr != NULL; line_ptr = next_line, i++) {
      next_line = line_ptr->next;
      (*epsflines)[i] = UtilStrDup(line_ptr->s);
      free(line_ptr->s);
      free(line_ptr);
   }
   topLine = botLine = NULL;
   numLines = 0;
   return (BitmapSuccess);
}

int MyReadEPSFile (file_name, image_w, image_h, bitmap, image, num_lines,
      epsflines, epsf_level, llx, lly, urx, ury, write_date)
   char		* file_name;
   int		* image_w, * image_h, * num_lines, * epsf_level;
   Pixmap	* bitmap;
   XImage	* * image;
   char		* * * epsflines, * write_date;
   float	* llx, * lly, * urx, * ury;
{
   int rc;

   SaveStatusStrings();
   rc = DoMyReadEPSFile(file_name, image_w, image_h, bitmap, image,
         num_lines, epsflines, epsf_level, llx, lly, urx, ury, write_date);
   RestoreStatusStrings();
   return rc;
}

static
FILE *JustGetTiffEPSIInfo(fp, pszEPS)
   FILE *fp;
   char *pszEPS;
{
   int bytes_to_read=0x1e, eps_sz, need_to_check_ps=TRUE;
   char buf[0x20];
   FILE *eps_fp=NULL;

   writeFileFailed = FALSE;
   *pszEPS = '\0';
   rewind(fp);
   if (fread(buf, sizeof(char), bytes_to_read, fp) != bytes_to_read) {
      fclose(fp);
      fclose(eps_fp);
      return NULL;
   }
   eps_sz = ReadDoubleWord(&buf[0x08]);

   sprintf(pszEPS, "%sTgifXXXXXX", TMP_DIR);
   mktemp(pszEPS);
   unlink(pszEPS);
   if ((eps_fp=fopen(pszEPS, "w")) == NULL) {
      fclose(fp);
      unlink(pszEPS);
      return NULL;
   }

   /* copy the EPS content from the original file to the new EPS file */
   while (eps_sz > 0) {
      bytes_to_read = min(eps_sz, sizeof(gszMsgBox));
      if (bytes_to_read !=
            (int)fread(gszMsgBox, sizeof(char), bytes_to_read, fp)) {
         fclose(fp);
         fclose(eps_fp);
         unlink(pszEPS);
         return NULL;
      }
      if (need_to_check_ps) {
         need_to_check_ps = FALSE;
         if (!(bytes_to_read>=2 && gszMsgBox[0]=='%' && gszMsgBox[1]=='!')) {
            fclose(fp);
            fclose(eps_fp);
            unlink(pszEPS);
            return NULL;
         }
      }
      if ((int)fwrite(gszMsgBox, sizeof(char), bytes_to_read, eps_fp) <= 0) {
         writeFileFailed = TRUE;
         fclose(fp);
         fclose(eps_fp);
         unlink(pszEPS);
         return NULL;
      }
      eps_sz -= bytes_to_read;
   }
   fclose(fp);
   fclose(eps_fp);
   return fopen(pszEPS, "r");
}

static
int JustReadEPSLines (xbm_ptr)
   struct XBmRec	* xbm_ptr;
{
   register struct LineRec	* line_ptr, * next_line;
   register int	i;
   char		* c_ptr, * line, loc_time[MAXSTRING+1];
   char		tiff_eps_fname[MAXPATHLENGTH+1];
   int		first_line=TRUE;
   int		boundingbox_found=FALSE, preview_found=FALSE, found;
   int		boundingbox_atend=FALSE;
   float	llx, lly, urx, ury;
   char		* file_name=xbm_ptr->filename;
   FILE		* fp;
   struct stat	stat_buf;

   if ((fp=fopen(file_name, "r")) == NULL) {
      sprintf(gszMsgBox, "Can not open EPS file '%s'.", file_name);
      if (PRTGIF) {
         fprintf(stderr, "%s\n", gszMsgBox);
         fprintf(stderr, "   EPS object skipped for printing.\n");
      } else {
         TwoLineMsg(gszMsgBox, "   EPS object skipped for printing.");
      }
      return FALSE;
   }

   CleanUpLines();

   *tiff_eps_fname = '\0';
   while ((line=UtilGetALine(fp)) != NULL)
   {
      int need_to_free_line=TRUE;

      if (first_line)
      {
         if (line[0] == ((char)0xc5) && line[1] == ((char)0xd0) &&
               line[2] == ((char)0xd3) && line[3] == ((char)0xc6) &&
               line[4] == ((char)0x1e)) {
            free(line);
            if ((fp=JustGetTiffEPSIInfo(fp, tiff_eps_fname)) == NULL ||
                  (line=UtilGetALine(fp)) == NULL) {
               if (fp != NULL) fclose(fp);
               if (*tiff_eps_fname != '\0') unlink(tiff_eps_fname);
               CleanUpLines();
               return FALSE;
            }
         }
         first_line = FALSE;
         if (line[0] != '%' || line[1] != '!')
         {
            if (need_to_free_line) free(line);
            fclose (fp);
            CleanUpLines ();
            return (FALSE);
         }
         AddLine (line);
         need_to_free_line = FALSE;
      }
      else if ((!boundingbox_found || boundingbox_atend) &&
            strncmp (line, "%%BoundingBox:", 14) == 0)
      {
         if (sscanf (&(line[14]), "%f %f %f %f", &llx, &lly, &urx, &ury) == 4)
            boundingbox_found = TRUE;
         else if (!boundingbox_found)
         {
            c_ptr = FindChar ((int)'(', &(line[14]));
            if (strncmp (c_ptr, "atend)", 6) == 0)
               boundingbox_atend = TRUE;
         }
      }
      else if (!preview_found && strncmp (line, "%%BeginPreview:", 15) == 0)
      {
         char *preview_line;

         preview_found = TRUE;
         found = FALSE;
         while ((preview_line=UtilGetALine(fp)) != NULL)
         {
            if (strncmp (preview_line, "%%EndPreview", 12) == 0)
            {
               found = TRUE;
               free(preview_line);
               break;
            }
            free(preview_line);
         }
         if (!found)
         {
            if (PRTGIF) {
               fprintf(stderr, "Invalid preview box in EPS file.\n");
            } else {
               MsgBox("Invalid preview box in EPS file.", TOOL_NAME, INFO_MB);
            }
            free(line);
            if (need_to_free_line) free(line);
            fclose (fp);
            CleanUpLines ();
            return (FALSE);
         }
      }
      else
      {
         if (line[0] == '%' && line[1] == '!') (xbm_ptr->epsf_level)++;
         if (!stripEPSComments || line[0] != '%') {
            AddLine (line);
            need_to_free_line = FALSE;
         }
      }
      if (need_to_free_line) free(line);
   }
   fclose (fp);
   if (*tiff_eps_fname != '\0') unlink(tiff_eps_fname);
   if (!boundingbox_found)
   {
      if (PRTGIF) {
         fprintf(stderr, "Can not find bounding box in EPS file.\n");
      } else {
         MsgBox("Can not find bounding box in EPS file.", TOOL_NAME, INFO_MB);
      }
      CleanUpLines();
      return (FALSE);
   }
   stat (file_name, &stat_buf);
   strcpy (loc_time, ctime (&(stat_buf.st_mtime)));
   loc_time[24] = '\0';
   if (strcmp (xbm_ptr->write_date, loc_time) != 0)
   {
      sprintf(gszMsgBox,
            "Warning:  EPS file '%s' is newer than the EPS object.", file_name);
      if (PRTGIF) {
         fprintf(stderr, "%s\n", gszMsgBox);
      } else {
         Msg(gszMsgBox);
      }
   }
   xbm_ptr->epsflines = (char**)malloc(numLines*sizeof(char*));
   if (xbm_ptr->epsflines == NULL) FailAllocMessage();
   xbm_ptr->num_epsf_lines = numLines;

   for (i=0, line_ptr=topLine; line_ptr != NULL; line_ptr = next_line, i++) {
      next_line = line_ptr->next;
      (xbm_ptr->epsflines)[i] = UtilStrDup (line_ptr->s);
      free(line_ptr->s);
      free(line_ptr);
   }
   topLine = botLine = NULL;
   numLines = 0;

   return (TRUE);
}

void DumpEPSObj(FP, ObjPtr)
   FILE *FP;
   struct ObjRec *ObjPtr;
{
   int i;
   float scale=0, llx_psu, lly_psu, urx_psu, ury_psu, x_psu=0, y_psu=0;
   float w_psu, h_psu, bbox_w_psu, bbox_h_psu, dx_psu, dy_psu, ltx_psu, rby_psu;
   struct XBmRec *xbm_ptr=ObjPtr->detail.xbm;
   struct MtrxRec mtrx;

   if (!xbm_ptr->save_epsf) {
      if (xbm_ptr->bitmap == None || xbm_ptr->epsflines == NULL) {
         if (!JustReadEPSLines(xbm_ptr)) {
            return;
         }
      } else {
         char loc_time[MAXSTRING+1];
         struct stat stat_buf;

         stat(xbm_ptr->filename, &stat_buf);
         strcpy(loc_time, ctime(&(stat_buf.st_mtime)));
         loc_time[24] = '\0';
         if (strcmp(xbm_ptr->write_date, loc_time) != 0) {
            sprintf(gszMsgBox,
                  "Warning:  EPS file '%s' is newer than the EPS object.",
                  xbm_ptr->filename);
            if (PRTGIF) {
               fprintf(stderr, "%s\n", gszMsgBox);
            } else {
               Msg(gszMsgBox);
            }
         }
      }
   }
   /*
    * psu is PostScript Unit (72 psu/inch) and px be pixel.
    * scale's unit is psu/px.
    */
   scale = ((float)psDotsPerInch)/((float)PIX_PER_INCH*100)*((float)printMag);

   llx_psu = (float)(((float)xbm_ptr->llx) / 1000.0);
   lly_psu = (float)(((float)xbm_ptr->lly) / 1000.0);
   urx_psu = (float)(((float)xbm_ptr->urx) / 1000.0);
   ury_psu = (float)(((float)xbm_ptr->ury) / 1000.0);

   if (ObjPtr->ctm == NULL) {
      w_psu = (float)((float)(ObjPtr->obbox.rbx-ObjPtr->obbox.ltx) * scale);
      h_psu = (float)((float)(ObjPtr->obbox.rby-ObjPtr->obbox.lty) * scale);
   } else {
      w_psu = (float)((float)(ObjPtr->orig_obbox.rbx-ObjPtr->orig_obbox.ltx) *
            scale);
      h_psu = (float)((float)(ObjPtr->orig_obbox.rby-ObjPtr->orig_obbox.lty) *
            scale);
   }
   bbox_w_psu = urx_psu - llx_psu;
   bbox_h_psu = ury_psu - lly_psu;

   mtrx.image_w = (float)bbox_w_psu; mtrx.image_h = (float)bbox_h_psu;
   mtrx.w = w_psu; mtrx.h = h_psu;
   mtrx.rotate = xbm_ptr->rotate; mtrx.flip = xbm_ptr->flip;

   CalcTransform(&mtrx);

   dx_psu = llx_psu*mtrx.m[0][0] + lly_psu*mtrx.m[1][0];
   dy_psu = llx_psu*mtrx.m[0][1] + lly_psu*mtrx.m[1][1];

   if (ObjPtr->ctm == NULL) {
      ltx_psu = (float)((float)(ObjPtr->obbox.ltx) * scale);
      rby_psu = (float)(((float)psDotsPerInch)*psYOff[pageStyle] -
            ((float)(ObjPtr->obbox.rby) * scale));
      switch (xbm_ptr->rotate) {
      case ROTATE0:
         x_psu = (mtrx.transformed_w >= 0.0) ?
               (ltx_psu)-dx_psu : (ltx_psu+w_psu)-dx_psu;
         y_psu = (mtrx.transformed_h >= 0.0) ?
               (rby_psu)-dy_psu : (rby_psu+h_psu)-dy_psu;
         break;
      case ROTATE90:
         x_psu = (mtrx.transformed_w >= 0.0) ?
               (ltx_psu+w_psu)+dx_psu : (ltx_psu)+dx_psu;
         y_psu = (mtrx.transformed_h >= 0.0) ?
               (rby_psu+h_psu)+dy_psu : (rby_psu)+dy_psu;
         break;
      case ROTATE180:
         x_psu = (mtrx.transformed_w >= 0.0) ?
               (ltx_psu)-dx_psu : (ltx_psu+w_psu)-dx_psu;
         y_psu = (mtrx.transformed_h >= 0.0) ?
               (rby_psu)-dy_psu : (rby_psu+h_psu)-dy_psu;
         break;
      case ROTATE270:
         x_psu = (mtrx.transformed_w >= 0.0) ?
               (ltx_psu+w_psu)+dx_psu : (ltx_psu)+dx_psu;
         y_psu = (mtrx.transformed_h >= 0.0) ?
               (rby_psu+h_psu)+dy_psu : (rby_psu)+dy_psu;
         break;
      }
   } else {
      float tmp_dx, tmp_dy;
      int dx, dy, ltx, rby, x, y, tmp_x, tmp_y;

      tmp_dx = dx_psu / scale;
      tmp_dy = dy_psu / scale;
      dx = round(tmp_dx);
      dy = round(tmp_dy);
      ltx = ObjPtr->orig_obbox.ltx;
      rby = ObjPtr->orig_obbox.rby;
      x = ltx - dx;
      y = rby + dy;
      TransformPointThroughCTM(x-ObjPtr->x, y-ObjPtr->y, ObjPtr->ctm,
            &tmp_x, &tmp_y);
      tmp_x += ObjPtr->x;
      tmp_y += ObjPtr->y;
      x_psu = (float)(((float)tmp_x) * scale);
      y_psu = (float)(((float)psDotsPerInch)*psYOff[pageStyle] -
            (((float)tmp_y) * scale));
   }

   fprintf(FP, "%% EPS\n");
   fprintf(FP, "end\n");
   fprintf(FP, "/tgiflevel%1d save def\n", xbm_ptr->epsf_level);
   fprintf(FP, "/tgifdictcount%1d countdictstack def\n", xbm_ptr->epsf_level);
   fprintf(FP, "/tgifopcount%1d count 1 sub def\n", xbm_ptr->epsf_level);
   fprintf(FP, "userdict begin\n");
   fprintf(FP, "/showpage {} def\n");
   fprintf(FP, "/letter {} def\n");
   fprintf(FP, "/legal {} def\n");
   fprintf(FP, "/setpagedevice {} def\n");
   fprintf(FP, "/a4 {} def\n");
   fprintf(FP, "0 setgray 0 setlinecap 1 setlinewidth\n");
   fprintf(FP, "0 setlinejoin 10 setmiterlimit [] 0 setdash newpath\n");
   fprintf(FP, "1 %1d %1d div %1d mul 100 div div dup neg scale\n",
         psDotsPerInch, PIX_PER_INCH, printMag);
   fprintf(FP, "%1d %s mul neg %1d %s mul neg translate\n", psDotsPerInch,
         psXOffStr[pageStyle], psDotsPerInch, psYOffStr[pageStyle]);
   fprintf(FP, "\n");
   if (ObjPtr->ctm == NULL) {
      fprintf(FP, "%.3f %.3f translate %.3f %.3f scale %1d rotate\n",
            x_psu, y_psu, mtrx.dump_h_scale, mtrx.dump_v_scale, (-mtrx.degree));
   } else {
      float m[6], ftmp_x, ftmp_y;
      int abs_ltx=ABS_X(ObjPtr->rotated_obbox[0].x);
      int abs_lty=ABS_Y(ObjPtr->rotated_obbox[0].y);
      int tmp_x, tmp_y;

      ReverseTransformPointThroughCTM(abs_ltx-ObjPtr->x, abs_lty-ObjPtr->y,
            ObjPtr->ctm, &tmp_x, &tmp_y);
      tmp_x += ObjPtr->x;
      tmp_y += ObjPtr->y;
      ftmp_x = ((float)tmp_x) - llx_psu;
      ftmp_y = ((float)tmp_y) - ury_psu;
      abs_ltx = round(ftmp_x);
      abs_lty = round(ftmp_y);
      TransformPointThroughCTM(abs_ltx-ObjPtr->x, abs_lty-ObjPtr->y,
            ObjPtr->ctm, &tmp_x, &tmp_y);
      ftmp_x = ((float)tmp_x) + ObjPtr->x;
      ftmp_y = ((float)tmp_y) + ObjPtr->y;

      fprintf(FP, "%.3f %.3f translate %.3f %.3f scale %1d rotate\n",
            x_psu, y_psu, mtrx.dump_h_scale, mtrx.dump_v_scale, (-mtrx.degree));

      m[CTM_SX] = ((float)ObjPtr->ctm->m[CTM_SX])/((float)1000.0);
      m[CTM_SY] = ((float)ObjPtr->ctm->m[CTM_SY])/((float)1000.0);
      m[CTM_SIN] = ((float)ObjPtr->ctm->m[CTM_SIN])/((float)1000.0);
      m[CTM_MSIN] = ((float)ObjPtr->ctm->m[CTM_MSIN])/((float)1000.0);
      fprintf(FP, "[%.3f %.3f %.3f %.3f 0 0] concat\n",
            m[CTM_SX], -m[CTM_SIN], -m[CTM_MSIN], m[CTM_SY]);
   }
   fprintf(FP, "\n");
   fprintf(FP, "%%%%BeginDocument: %s\n", xbm_ptr->filename);

   for (i = 0; i < xbm_ptr->num_epsf_lines; i++) {
      fprintf(FP, "%s\n", xbm_ptr->epsflines[i]);
   }
   fprintf(FP, "%%%%EndDocument\n");
   fprintf(FP, "count tgifopcount%1d sub dup 0 gt %s\n",
         xbm_ptr->epsf_level, "{{pop} repeat} {pop} ifelse");
   fprintf(FP, "countdictstack tgifdictcount%1d sub dup 0 gt %s\n",
         xbm_ptr->epsf_level, "{{end} repeat} {pop} ifelse");
   fprintf(FP, "tgiflevel%1d restore\n", xbm_ptr->epsf_level);
   fprintf(FP, "tgifdict begin\n");
   fprintf(FP, "\n");
}

struct ObjRec *CreateEPSObj(FileName, ImageW, ImageH, bitmap, image,
      num_lines, lines, epsf_level, llx, lly, urx, ury, write_date)
   char *FileName, **lines, *write_date;
   int ImageW, ImageH, num_lines, epsf_level;
   Pixmap bitmap;
   XImage *image;
   float *llx, *lly, *urx, *ury;
{
   struct XBmRec *xbm_ptr;
   struct ObjRec *obj_ptr;
   int len=strlen(FileName), w, h;
   char *name;

   name = (char*)malloc((len+1)*sizeof(char));
   if (name == NULL) FailAllocMessage();
   strcpy (name, FileName);

   xbm_ptr = (struct XBmRec *)malloc(sizeof(struct XBmRec));
   if (xbm_ptr == NULL) FailAllocMessage();
   memset(xbm_ptr, 0, sizeof(struct XBmRec));

   xbm_ptr->image = image;
   xbm_ptr->image_w = ImageW;
   xbm_ptr->image_h = ImageH;
   xbm_ptr->bitmap = bitmap;
   xbm_ptr->data = NULL;

   if (bitmap == None)
   {
      xbm_ptr->eps_w = w = ((*urx) >= (*llx)) ? (int)((*urx)-(*llx)) :
            (int)((*llx)-(*urx));
      xbm_ptr->eps_h = h = ((*ury) >= (*lly)) ? (int)((*ury)-(*lly)) :
            (int)((*lly)-(*ury));
   }
   else
   {
      xbm_ptr->eps_w = w = ImageW;
      xbm_ptr->eps_h = h = ImageH;
   }

   xbm_ptr->fill = objFill;
   xbm_ptr->rotate = xbm_ptr->cached_rotate = ROTATE0;
   xbm_ptr->flip = xbm_ptr->cached_flip = 0;
   xbm_ptr->llx = (int)((*llx)*1000.0);
   xbm_ptr->lly = (int)((*lly)*1000.0);
   xbm_ptr->urx = (int)((*urx)*1000.0);
   xbm_ptr->ury = (int)((*ury)*1000.0);
   xbm_ptr->cached_zoom = 0;
   xbm_ptr->cached_bitmap = None;
   xbm_ptr->cached_w = xbm_ptr->cached_h = 0;

   xbm_ptr->real_type = XBM_EPS;
   xbm_ptr->filename = name;
   strcpy (xbm_ptr->write_date, write_date);
   xbm_ptr->epsflines = lines;
   xbm_ptr->num_epsf_lines = num_lines;
   xbm_ptr->epsf_level = epsf_level;
   xbm_ptr->save_epsf = saveEPSLines;

   obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (obj_ptr == NULL) FailAllocMessage();
   memset(obj_ptr, 0, sizeof(struct ObjRec));

   obj_ptr->bbox.ltx = obj_ptr->obbox.ltx = obj_ptr->x = drawOrigX;
   obj_ptr->bbox.lty = obj_ptr->obbox.lty = obj_ptr->y = drawOrigY;
   obj_ptr->bbox.rbx = obj_ptr->obbox.rbx = w + drawOrigX;
   obj_ptr->bbox.rby = obj_ptr->obbox.rby = h + drawOrigY;
   obj_ptr->type = OBJ_XBM;
   obj_ptr->color = colorIndex;
   obj_ptr->id = objId++;
   obj_ptr->dirty = FALSE;
   obj_ptr->rotation = 0;
   obj_ptr->locked = FALSE;
   obj_ptr->detail.xbm = xbm_ptr;
   obj_ptr->fattr = obj_ptr->lattr = NULL;
   obj_ptr->ctm = NULL;

   return (obj_ptr);
}

void UpdateEPS()
{
   struct XBmRec *xbm_ptr, *new_xbm_ptr;
   struct ObjRec *obj_ptr, *new_obj_ptr;
   char write_date[32], **lines=NULL;
   int rc, num_lines, epsf_level, image_w, image_h, save_epsf;
   int transformed;
   float llx, lly, urx, ury;
   Pixmap bitmap;
   XImage *image=NULL;

   if (topSel==NULL || topSel!=botSel || topSel->obj->type!=OBJ_XBM ||
         topSel->obj->detail.xbm->real_type!=XBM_EPS) {
      MsgBox("Please select an EPS object.", TOOL_NAME, INFO_MB);
      return;
   }
   obj_ptr = topSel->obj;
   xbm_ptr = obj_ptr->detail.xbm;
   transformed = (obj_ptr->ctm!=NULL);

   save_epsf = xbm_ptr->save_epsf;

   importingFile = TRUE;
   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);
   rc = MyReadEPSFile(xbm_ptr->filename, &image_w, &image_h, &bitmap, &image,
         &num_lines, &lines, &epsf_level, &llx, &lly, &urx, &ury, write_date);
   SetDefaultCursor(mainWindow);
   SetDefaultCursor(drawWindow);

   if (rc != BitmapSuccess) {
      sprintf(gszMsgBox, "Can not import EPS file '%s'.", xbm_ptr->filename);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      importingFile = FALSE;
      return;
   }
   importingFile = FALSE;
   HighLightReverse();
   PrepareToReplaceAnObj(obj_ptr);
   if (save_epsf) saveEPSLines = TRUE;
   new_obj_ptr = CreateEPSObj(xbm_ptr->filename, image_w, image_h, bitmap,
         image, num_lines, lines, epsf_level, &llx, &lly, &urx, &ury,
         write_date);
   saveEPSLines = FALSE;

   new_obj_ptr->x = obj_ptr->x;
   new_obj_ptr->y = obj_ptr->y;
   if (transformed) {
      new_obj_ptr->obbox.ltx = obj_ptr->orig_obbox.ltx;
      new_obj_ptr->obbox.lty = obj_ptr->orig_obbox.lty;
      new_obj_ptr->obbox.rbx = obj_ptr->orig_obbox.rbx;
      new_obj_ptr->obbox.rby = obj_ptr->orig_obbox.rby;
      new_obj_ptr->obbox.rby = obj_ptr->orig_obbox.rby;
      SetCTM(new_obj_ptr, obj_ptr->ctm);
      AdjObjBBox(new_obj_ptr);
   } else {
      new_obj_ptr->bbox.ltx = obj_ptr->bbox.ltx;
      new_obj_ptr->bbox.lty = obj_ptr->bbox.lty;
      new_obj_ptr->bbox.rbx = obj_ptr->bbox.rbx;
      new_obj_ptr->bbox.rby = obj_ptr->bbox.rby;
      new_obj_ptr->obbox.ltx = obj_ptr->obbox.ltx;
      new_obj_ptr->obbox.lty = obj_ptr->obbox.lty;
      new_obj_ptr->obbox.rbx = obj_ptr->obbox.rbx;
      new_obj_ptr->obbox.rby = obj_ptr->obbox.rby;
   }
   new_xbm_ptr = new_obj_ptr->detail.xbm;
   new_xbm_ptr->rotate = xbm_ptr->rotate;
   new_xbm_ptr->flip = xbm_ptr->flip;

   UnlinkObj(obj_ptr);
   topSel->obj = new_obj_ptr;
   AddObj(NULL, topObj, new_obj_ptr);
   RecordReplaceAnObj(new_obj_ptr);
   FreeObj(obj_ptr);

   UpdSelBBox();
   RedrawAnArea(botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   SetFileModified(TRUE);
   justDupped = FALSE;
   HighLightForward();
   Msg("EPS object updated.");
}
