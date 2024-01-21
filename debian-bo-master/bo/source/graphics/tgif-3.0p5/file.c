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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/file.c,v 3.1 1996/05/11 05:40:50 william Exp $";
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
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "const.h"
#include "patchlvl.h"
#include "types.h"

#include "align.e"
#include "arc.e"
#include "attr.e"
#include "auxtext.e"
#include "box.e"
#include "button.e"
#include "choice.e"
#include "cmd.e"
#include "color.e"
#include "cutpaste.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#include "eps.e"
#ifndef _NO_EXTERN
#include "file.e"
#endif
#include "font.e"
#include "grid.e"
#include "group.e"
#include "import.e"
#include "mainloop.e"
#include "mainmenu.e"
#include "mark.e"
#include "menu.e"
#include "move.e"
#include "msg.e"
#include "names.e"
#include "navigate.e"
#include "obj.e"
#include "oval.e"
#include "page.e"
#include "pattern.e"
#include "poly.e"
#include "polygon.e"
#include "prtgif.e"
#include "ps.e"
#include "raster.e"
#include "rcbox.e"
#include "rect.e"
#include "remote.e"
#include "ruler.e"
#include "scroll.e"
#include "select.e"
#include "setup.e"
#include "special.e"
#include "stk.e"
#include "stretch.e"
#include "text.e"
#include "util.e"
#include "version.e"
#include "xbitmap.e"
#include "xpixmap.e"

extern char	* mktemp ARGS_DECL((char *Template));
extern int	atoi ARGS_DECL((char *));
#ifndef _NO_EXTERN
extern time_t	time ARGS_DECL((time_t *));
extern int	unlink ARGS_DECL((char *));
#ifdef SYSV
extern unsigned	sleep ARGS_DECL((unsigned));
#else
extern int	sleep ARGS_DECL((unsigned));
#endif
#endif

#define CUR_VERSION 33

extern char	* getenv ARGS_DECL((char *));
#ifdef __hpux
extern double	atof ARGS_DECL((const char *));
#else
extern double	atof ARGS_DECL((char *));
#endif

int	PRTGIF=FALSE;
char	curFileName[MAXPATHLENGTH+1];
int	curFileDefined=FALSE;
int	fileVersion=INVALID;
int	curFileWriteVersion=CUR_VERSION;
int	importingFile=FALSE;
int	psDotsPerInch=72;
int	printMag=100;
int	saveTmpOnReturn=TRUE;
int	warpToWinCenter=TRUE;
float	tiledPageScaling=0.9;

char	*psXOffStr[MAXPAGESTYLES] = {"0","0"};
float	psXOff[MAXPAGESTYLES] =     { 0,  0 };
char	* * psYOffStr=NULL;
float	* psYOff=NULL;
float	* psPageWidthInInch=NULL;
float	* psPageHeightInInch=NULL;

char	printCommand[MAXSTRING+1];
char	outputDir[MAXPATHLENGTH+1];

char	* savedComments=NULL;
int	savedCommentsLen=0;
int	saveCommentsInSaveNew=TRUE;
int	usePsAdobeString=FALSE;
char	adobeString[80], epsfString[80];

int	readingPageNum=0;
int	loadedCurPageNum=0;

int	writeFileFailed=FALSE;
int	foundGoodStateObject=FALSE;

int	cmdLineHasPageNum=FALSE;
int	cmdLinePageNum=(-1);
char	cmdLinePageNumStr[80];

int	cmdLineOneFilePerPage=FALSE;
int	cmdLineA4=FALSE;

int	showPageInEPS=FALSE;

static int importingPageNum=(-1);
static char importingPageName[MAXSTRING+1];

struct DocFontRec {
   char name[80];
   int len;
   struct DocFontRec *next;
};

static struct DocFontRec *firstDocFont=NULL;

void UpdateDocumentFonts (ps_font_name)
   char	* ps_font_name;
{
   int			len=strlen(ps_font_name);
   struct DocFontRec	* df_ptr;

   for (df_ptr=firstDocFont; df_ptr!=NULL; df_ptr=df_ptr->next)
      if (df_ptr->len == len && strcmp(df_ptr->name,ps_font_name) == 0)
         return;
   df_ptr = (struct DocFontRec *)malloc(sizeof(struct DocFontRec));
   if (df_ptr == NULL) FailAllocMessage();
   memset(df_ptr, 0, sizeof(struct DocFontRec));
   df_ptr->len = len;
   strcpy (df_ptr->name, ps_font_name);
   df_ptr->next = firstDocFont;
   firstDocFont = df_ptr;
}

static
int CopyAFile (file1, file2)
   char	* file1, * file2;
{
   char	tmp_str[MAXSTRING+1], * rest;
   int	short_name;
   FILE	* fp1, * fp2;

   if ((fp1 = fopen (file1, "r")) == NULL)
   {
      sprintf (tmp_str, "Cannot open '%s' for read.", file1);
      if (PRTGIF)
         fprintf (stderr, "%s\n", tmp_str);
      else
         Msg (tmp_str);
      return (FALSE);
   }
   if ((short_name = IsPrefix (bootDir, file2, &rest))) ++rest;
   if ((fp2 = fopen (file2, "w")) == NULL)
   {
      if (PRTGIF)
         fprintf (stderr, "Cannot open '%s' for write.\n", file2);
      else
      {
         if (short_name)
            sprintf (tmp_str, "Cannot open '%s' for write.", rest);
         else
            sprintf (tmp_str, "Cannot open '%s' for write.", file2);
         Msg (tmp_str);
      }
      fclose (fp1);
      return (FALSE);
   }
   writeFileFailed = FALSE;
   while (fgets (tmp_str, MAXSTRING, fp1) != NULL)
      if (fputs (tmp_str, fp2) == EOF)
         writeFileFailed = TRUE;
   fclose (fp1);
   fclose (fp2);
   if (writeFileFailed)
   {
      writeFileFailed = FALSE;
      sprintf (tmp_str, "Fail to write to '%s'.\n\nFile system may be full.",
            file2);
      if (PRTGIF)
         fprintf (stderr, "%s\n", tmp_str);
      else
         MsgBox (tmp_str, TOOL_NAME, INFO_MB);
      return (FALSE);
   }
   return (TRUE);
}

static
int ExecuteCmd (cmd)
   char	* cmd;
{
   char	tmp_str[MAXSTRING+1];
   FILE	* fp;

   sprintf(gszMsgBox, "Executing '%s'...", cmd);
   SetStringStatus(gszMsgBox);
   XSync(mainDisplay, False);
   if ((fp=(FILE*)popen(cmd, "r")) == NULL) return (FALSE);

   while (fgets (tmp_str, MAXSTRING, fp) != NULL)
   {
      if (PRTGIF)
         fprintf (stderr, "%s", tmp_str);
      else
         Msg (tmp_str);
      sleep (5);
   }
   pclose (fp);
   SetStringStatus("...Done");
   return (TRUE);
}

void ClearFileInfo ()
{
   curFileName[0] = '\0';
   curFileDefined = FALSE;
   if (!curDirIsLocal) strcpy (curDir, curLocalDir);
   curDirIsLocal = TRUE;
   *curLocalDir = '\0';
   *curSymDir = '\0';
}

void CleanUpComments()
{
   if (savedComments != NULL) {
      free(savedComments);
      savedComments = NULL;
      savedCommentsLen = 0;
   }
}

int OkayToCreateFile(FileName)
   char *FileName;
{
   FILE *fp;

   if ((fp=fopen(FileName, "r")) == NULL) return TRUE;
   fclose(fp);
   sprintf(gszMsgBox, "File '%s' exists, okay to overwrite? [ync](y)",
         FileName);
   switch (MsgBox(gszMsgBox, TOOL_NAME, YNC_MB)) {
   case MB_ID_YES: break;
   case MB_ID_NO: return FALSE;
   case MB_ID_CANCEL: return FALSE;
   }
   unlink(FileName);
   return TRUE;
}

void Save (FP, BotObjPtr, Level, PageNumber)
   FILE			* FP;
   struct ObjRec	* BotObjPtr;
   int			Level, PageNumber;
{
   struct ObjRec	* obj_ptr;

   SetCurChoice (NOTHING);

   SetWatchCursor (drawWindow);
   SetWatchCursor (mainWindow);

   if (Level == 0 && PageNumber == 1)
   {
      char	font_str[81];

      ResetXPmErrorMessage ();
      GetPSFontStr (curFont, curStyle, font_str);
      if (TGIF_PATCHLEVEL == 0)
      {
         if (fprintf (FP, "%%TGIF %s\n", versionString) == EOF)
            writeFileFailed = TRUE;
      }
      else
      {
         if (fprintf (FP, "%%TGIF %s-p%1d\n", versionString,
               TGIF_PATCHLEVEL) == EOF)
            writeFileFailed = TRUE;
      }

      if (fprintf (FP, "state(%1d,%1d,%1d,", pageStyle, CUR_VERSION,
            printMag) == EOF)
         writeFileFailed = TRUE;
      if (fprintf (FP, "%1d,%1d,%1d,", drawOrigX, drawOrigY, zoomScale) == EOF)
         writeFileFailed = TRUE;
      if (fprintf (FP, "%1d,%1d,%1d,", xyEnglishGrid, gridOn, colorIndex) ==
            EOF)
         writeFileFailed = TRUE;
      if (fprintf (FP, "%1d,%1d,%1d,", horiAlign, vertAlign, lineWidth) == EOF)
         writeFileFailed = TRUE;
      if (fprintf (FP, "%1d,%1d,%1d,%1d,", curSpline, lineStyle, objFill,
            penPat) == EOF)
         writeFileFailed = TRUE;
      if (fprintf (FP, "%1d,'%s',%1d,%1d,", textJust,
            /* font_str starts with the '/' character */
            &font_str[1], curStyle, curSize) == EOF)
         writeFileFailed = TRUE;
      if (fprintf (FP, "%1d,%1d,%1d,", 0, curDash, gridSystem) == EOF)
         writeFileFailed = TRUE;
      if (fprintf (FP, "%1d,%1d,%1d,", xyMetricGrid, textVSpace, zoomedIn) ==
            EOF)
         writeFileFailed = TRUE;
      if (fprintf (FP, "%1d,%1d,%1d,", gridShown, moveMode, curRotate) == EOF)
         writeFileFailed = TRUE;
      if (fprintf (FP, "%1d,%1d,", rcbRadius, useGray) == EOF)
         writeFileFailed = TRUE;
      if (fprintf (FP, "%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d).\n",
            pageLayoutMode,
            (pageLayoutMode == PAGE_STACK) ? curPageNum : paperCol,
            (pageLayoutMode == PAGE_STACK) ? lastPageNum : paperRow,
            pageLineShownInTileMode, colorDump,
            round(((float)onePageWidth)*((float)printMag)/100.0),
            round(((float)onePageHeight)*((float)printMag)/100.0),
            stretchableText, textRotation, rotationIncrement) == EOF)
         writeFileFailed = TRUE;
      if (fprintf (FP, "%%\n") == EOF)
         writeFileFailed = TRUE;
      if (fprintf (FP, "%% @%s%s\n", "(#)$H", "eader$") == EOF)
         writeFileFailed = TRUE;
      if (fprintf (FP, "%% %s\n", "%W%") == EOF)
         writeFileFailed = TRUE;
      if (fprintf (FP, "%%\n") == EOF)
         writeFileFailed = TRUE;

      if (savedComments != NULL)
         if (fputs (savedComments, FP) == EOF)
            writeFileFailed = TRUE;

      if (tgifObj->lattr != NULL)
      {
         if (fprintf (FP, "file_attr(") == EOF) writeFileFailed = TRUE;
         SaveAttrs (FP, tgifObj->lattr);
         if (fprintf (FP, ").\n") == EOF) writeFileFailed = TRUE;
      }
      GetUnitSpec(gszMsgBox);
      if (fprintf(FP, "unit(\"%s\").\n", gszMsgBox) == EOF) {
         writeFileFailed = TRUE;
      }
   }
   if (Level == 0)
      if (fprintf (FP, "page(%1d,\"%s\",%1d).\n", PageNumber,
            ((pageLayoutMode==PAGE_TILE || curPage->name==NULL) ? "" :
            curPage->name), curPage->layer_on) == EOF)
         writeFileFailed = TRUE;

   for (obj_ptr = BotObjPtr; obj_ptr != NULL; obj_ptr = obj_ptr->prev)
   {
      switch (obj_ptr->type)
      {
         case OBJ_POLY: SavePolyObj (FP, obj_ptr); break;
         case OBJ_BOX: SaveBoxObj (FP, obj_ptr); break;
         case OBJ_OVAL: SaveOvalObj (FP, obj_ptr); break;
         case OBJ_TEXT: SaveTextObj (FP, obj_ptr); break;
         case OBJ_POLYGON: SavePolygonObj (FP, obj_ptr); break;
         case OBJ_ARC: SaveArcObj (FP, obj_ptr); break;
         case OBJ_RCBOX: SaveRCBoxObj (FP, obj_ptr); break;
         case OBJ_XBM: SaveXBmObj (FP, obj_ptr); break;
         case OBJ_XPM: SaveXPmObj (FP, obj_ptr); break;
         case OBJ_GROUP: SaveGroupObj (FP, obj_ptr, Level); break;
         case OBJ_SYM: SaveCompObj (FP, obj_ptr, Level); break;
         case OBJ_ICON: SaveIconObj (FP, obj_ptr, Level); break;
      }
      if (obj_ptr->prev == NULL)
      {
         if (Level == 0)
         {
            if (fprintf (FP, ".\n") == EOF) writeFileFailed = TRUE;
         }
         else
         {
            if (fprintf (FP, "\n") == EOF) writeFileFailed = TRUE;
         }
      }
      else
      {
         if (Level == 0)
         {
            if (fprintf (FP, ".\n") == EOF) writeFileFailed = TRUE;
         }
         else
         {
            if (fprintf (FP, ",\n") == EOF) writeFileFailed = TRUE;
         }
      }
   }
   SetDefaultCursor (mainWindow);
   ShowCursor ();
}

int SaveTmpFile (NewFileName)
   char	* NewFileName;
   /* return TRUE if file successfully saved */
{
   char			new_file_name[MAXPATHLENGTH+1], s[MAXPATHLENGTH+1];
   char			* rest;
   FILE			* fp;
   int			count = 0, status, short_name;
   struct PageRec	* saved_cur_page;
   struct ObjRec	* obj_ptr;

   strcpy (new_file_name, NewFileName);

   saved_cur_page = curPage;
   for (curPage=firstPage; curPage != NULL; curPage = curPage->next)
      for (obj_ptr = curPage->top; obj_ptr != NULL; obj_ptr = obj_ptr->next)
         if (obj_ptr->type == OBJ_SYM)
            count++;
   curPage = saved_cur_page;

   switch (count)
   {
      case 0:
         sprintf (new_file_name, "%s.%s", NewFileName, OBJ_FILE_EXT);
         status = OBJ_FILE_SAVED;
         break;
      case 1:
         if (lastPageNum == 1)
         {
            sprintf (new_file_name, "%s.%s", NewFileName, SYM_FILE_EXT);
            status = SYM_FILE_SAVED;
         }
         else
         {
            MsgBox ("A symbol file can only have one page.\n\nSave aborted!",
                  TOOL_NAME, INFO_MB);
            return (INVALID);
         }
         break;
      default:
         MsgBox ("Too many symbols!\n\nSymbol file not saved.",
               TOOL_NAME, INFO_MB);
         return (INVALID);
   }

   unlink (new_file_name);

   if ((short_name = IsPrefix (bootDir, new_file_name, &rest))) ++rest;
   if ((fp = fopen (new_file_name, "w")) == NULL)
   {
      if (short_name)
         sprintf (s, "Cannot open '%s', file not saved.", rest);
      else
         sprintf (s, "Cannot open '%s', file not saved.", new_file_name);
      MsgBox (s, TOOL_NAME, INFO_MB);
      return (INVALID);
   }

   if (short_name)
      sprintf (s, "Saving temporary file '%s' ...", rest);
   else
      sprintf (s, "Saving temporary file '%s' ...", new_file_name);
   Msg (s);

   writeFileFailed = FALSE;
   MakeQuiescent ();
   saved_cur_page = curPage;
   for (curPage = firstPage, count = 1; curPage != NULL;
         curPage = curPage->next, count++)
   {
      topObj = curPage->top;
      botObj = curPage->bot;
      Save (fp, botObj, 0, count);
   }
   curPage = saved_cur_page;
   topObj = curPage->top;
   botObj = curPage->bot;
   fclose (fp);

   if (writeFileFailed)
   {
      writeFileFailed = FALSE;
      sprintf (s, "Fail to write to '%s'.\n\nFile system may be full.",
            new_file_name);
      MsgBox (s, TOOL_NAME, INFO_MB);
   }
   else
   {
      if (short_name)
         sprintf (s, "Temporary file '%s' saved.", rest);
      else
         sprintf (s, "Temporary file '%s' saved.", new_file_name);
      Msg (s);
   }

   if (PSFILE_MOD != 0 && chmod (new_file_name, 0777))
   {
      if (short_name)
         sprintf (s, "Cannot chmod '%s' to 0777.", rest);
      else
         sprintf (s, "Cannot chmod '%s' to 0777.", new_file_name);
      MsgBox (s, TOOL_NAME, INFO_MB);
   }
   return (status);
}

void SaveNewFile (SaveSelectedOnly)
   int	SaveSelectedOnly;
{
   char			new_file_name[MAXPATHLENGTH+1], s[MAXPATHLENGTH+1];
   char			new_full_name[MAXPATHLENGTH+1];
   char			tmp_str[MAXPATHLENGTH+1];
   char			name_without_ext[MAXPATHLENGTH+1], * rest;
   char			saved_cur_dir[MAXPATHLENGTH+1];
   char			saved_cur_file_name[MAXPATHLENGTH+1];
   char			obj_ext_str[MAXSTRING+1], sym_ext_str[MAXSTRING+1];
   int			saved_cur_file_defined=FALSE, obj_ext_len, sym_ext_len;
   FILE			* fp;
   int			count = 0, len, short_name, ok=TRUE;
   struct ObjRec	* obj_ptr, * saved_top_obj=NULL, * saved_bot_obj=NULL;
   struct SelRec	* top_sel_ptr, * bot_sel_ptr;
   struct SelRec	* sel_ptr, * next_sel;
   struct PageRec	* saved_cur_page;

   if (SaveSelectedOnly && topSel==NULL)
   {
      MsgBox ("No objects selected!\n\nNothing saved!", TOOL_NAME, INFO_MB);
      return;
   }

   if (curDirIsLocal)
      sprintf (s, "( working directory: %s )", curDir);
   else
      sprintf (s, "( working directory: %s )", curLocalDir);
   Dialog ("Please enter new file name: ( <CR>: accept, <ESC>: cancel )",
         s, new_file_name);

   if (*new_file_name == '\0') return;
   len = strlen (new_file_name);

   if (SaveSelectedOnly)
   {
      for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
         if (sel_ptr->obj->type == OBJ_SYM)
            count++;
   }
   else
   {
      saved_cur_page = curPage;
      for (curPage = firstPage; curPage != NULL; curPage = curPage->next)
         for (obj_ptr = curPage->top; obj_ptr != NULL; obj_ptr = obj_ptr->next)
            if (obj_ptr->type == OBJ_SYM)
               count++;
      curPage = saved_cur_page;
   }

   sprintf (obj_ext_str, ".%s", OBJ_FILE_EXT);
   sprintf (sym_ext_str, ".%s", SYM_FILE_EXT);
   obj_ext_len = strlen (obj_ext_str);
   sym_ext_len = strlen (sym_ext_str);

   switch (count)
   {
      case 0:
         if (len >= obj_ext_len || len >= sym_ext_len)
         {
            if (strcmp (&new_file_name[len-sym_ext_len], sym_ext_str) == 0)
            {
               Msg ("Cannot save as a symbol file, no symbol defined.");
               return;
            }
            else if (strcmp (&new_file_name[len-obj_ext_len], obj_ext_str) != 0)
            {
               strcpy (name_without_ext, new_file_name);
               strcat (new_file_name, obj_ext_str);
            }
            else
            {
               strcpy (name_without_ext, new_file_name);
               name_without_ext[len-obj_ext_len] = '\0';
            }

            if (strlen (new_file_name) == obj_ext_len)
            {
               MsgBox ("No file name specified.\n\nFile not saved.",
                     TOOL_NAME, INFO_MB);
               return;
            }
         }
         else
         {
            strcpy (name_without_ext, new_file_name);
            strcat (new_file_name, obj_ext_str);
         }
         break;
      case 1:
         if (lastPageNum == 1)
         {
            if (len >= obj_ext_len || len >= sym_ext_len)
            {
               if (strcmp (&new_file_name[len-obj_ext_len], obj_ext_str) == 0)
               {
                  Msg ("Cannot save as an object file; one symbol defined.");
                  return;
               }
               else if (strcmp(&new_file_name[len-sym_ext_len],sym_ext_str)!=0)
               {
                  strcpy (name_without_ext, new_file_name);
                  strcat (new_file_name, sym_ext_str);
               }
               else
               {
                  strcpy (name_without_ext, new_file_name);
                  name_without_ext[len-sym_ext_len] = '\0';
               }

               if (strlen (new_file_name) == sym_ext_len)
               {
                  MsgBox ("No file name specified.\n\nFile not saved.",
                        TOOL_NAME, INFO_MB);
                  return;
               }
            }
            else
            {
               strcpy (name_without_ext, new_file_name);
               strcat (new_file_name, sym_ext_str);
            }
         }
         else
         {
            MsgBox ("A symbol file can only have one page.\n\nSave aborted!",
                  TOOL_NAME, INFO_MB);
            return;
         }
         break;
      default:
         MsgBox ("Too many symbols!\n\nSymbol file not saved.",
               TOOL_NAME, INFO_MB);
         return;
   }

   if (*new_file_name == '/')
      strcpy (new_full_name, new_file_name);
   else if (curDirIsLocal)
      sprintf (new_full_name, "%s/%s", curDir, new_file_name);
   else
      sprintf (new_full_name, "%s/%s", curLocalDir, new_file_name);

   if (!OkayToCreateFile (new_full_name)) return;

   if ((short_name = IsPrefix (bootDir, new_full_name, &rest))) ++rest;
   if ((fp = fopen (new_full_name, "w")) == NULL)
   {
      if (short_name)
         sprintf (s, "Cannot open '%s', file not saved.", rest);
      else
         sprintf (s, "Cannot open '%s', file not saved.", new_full_name);
      MsgBox (s, TOOL_NAME, INFO_MB);
      return;
   }
   if (!SaveSelectedOnly) BeforeNavigate();

   if (SaveSelectedOnly)
   {
      PushPageInfo ();
      if (pageLayoutMode == PAGE_STACK)
         paperCol = paperRow = curPageNum = lastPageNum = 1;

      if (curDirIsLocal)
         strcpy (saved_cur_dir, curDir);
      else
         strcpy (saved_cur_dir, curLocalDir);
      strcpy (saved_cur_file_name, curFileName);
      saved_cur_file_defined = curFileDefined;
      saved_top_obj = topObj;
      saved_bot_obj = botObj;

      JustDupSelObj (&top_sel_ptr, &bot_sel_ptr);
      topObj = top_sel_ptr->obj;
      botObj = bot_sel_ptr->obj;

      firstPage = lastPage = curPage =
            (struct PageRec *)malloc(sizeof(struct PageRec));
      if (firstPage == NULL) FailAllocMessage();
      memset(firstPage, 0, sizeof(struct PageRec));
      firstPage->layer_on = TRUE;
      firstPage->top = topObj;
      firstPage->bot = botObj;
      firstPage->next = firstPage->prev = NULL;
      if (pageLayoutMode == PAGE_STACK)
      {
         firstPage->draw_orig_x = drawOrigX;
         firstPage->draw_orig_y = drawOrigY;
         firstPage->zoom_scale = zoomScale;
         firstPage->zoomed_in = zoomedIn;
         curPageNum = lastPageNum = 1;
      }

      for (sel_ptr=topSel, obj_ptr=topObj; obj_ptr!=NULL;
            sel_ptr=sel_ptr->next, obj_ptr=obj_ptr->next)
      {
         CopyObjId (sel_ptr->obj, obj_ptr);
         CopyObjLocks (sel_ptr->obj, obj_ptr);
      }
   }

   if (curDirIsLocal)
      strcpy (tmp_str, curDir);
   else
      strcpy (tmp_str, curLocalDir);
   SetCurDir (new_full_name);
   curFileDefined = TRUE;

   switch (count)
   {
      case 0:
         *curSymDir = '\0';
         if ((strcmp (tmp_str, (curDirIsLocal ? curDir : curLocalDir)) != 0) ||
               (!NameInCurDir (curFileName)))
            UpdateDirInfo ();
         break;
      case 1:
         strcpy (curSymDir, (curDirIsLocal ? curDir : curLocalDir));
         if (!DirInSymPath (curDirIsLocal ? curDir : curLocalDir))
            UpdateSymInfo ();
         break;
   }

   if (short_name)
      sprintf (s, "Saving '%s' ...", rest);
   else
      sprintf (s, "Saving '%s' ...", new_full_name);
   Msg (s);

   if (!saveCommentsInSaveNew) CleanUpComments ();
   writeFileFailed = FALSE;
   saved_cur_page = curPage;
   for (curPage = firstPage, count = 1; curPage != NULL;
         curPage = curPage->next, count++)
   {
      topObj = curPage->top;
      botObj = curPage->bot;
      Save (fp, botObj, 0, count);
   }
   curPage = saved_cur_page;
   topObj = curPage->top;
   botObj = curPage->bot;
   fclose (fp);

   if (writeFileFailed)
   {
      writeFileFailed = FALSE;
      sprintf (s, "Fail to write to '%s'.\n\nFile system may be full.",
            new_full_name);
      MsgBox (s, TOOL_NAME, INFO_MB);
      ok = FALSE;
   }
   else
   {
      if (short_name)
         sprintf (s, "File '%s' saved.", rest);
      else
         sprintf (s, "File '%s' saved.", new_full_name);
      Msg (s);
   }

   if (SaveSelectedOnly)
   {
      CleanUpPage ();
      for (sel_ptr = top_sel_ptr; sel_ptr != NULL; sel_ptr = next_sel) {
         next_sel = sel_ptr->next;
         free(sel_ptr);
      }

      topObj = saved_top_obj;
      botObj = saved_bot_obj;
      if (curDirIsLocal)
         strcpy (curDir, saved_cur_dir);
      else
         strcpy (curLocalDir, saved_cur_dir);
      strcpy (curFileName, saved_cur_file_name);
      curFileDefined = saved_cur_file_defined;
      PopPageInfo ();
   }
   else if (ok)
   {
      SetFileModified (FALSE);
      RedrawTitleWindow ();
   }
   if (!SaveSelectedOnly) CommitNavigate ();
}

void SaveSymInLibrary ()
{
   char			new_file_name[MAXPATHLENGTH+1], s[MAXPATHLENGTH+1];
   char			new_full_name[MAXPATHLENGTH+1];
   char			dir_name[MAXPATHLENGTH+1];
   char			saved_dir[MAXPATHLENGTH+1], saved_file[MAXPATHLENGTH+1];
   char			saved_sym_dir[MAXPATHLENGTH+1];
   char			name_without_ext[MAXPATHLENGTH+1], * rest;
   char			sym_ext_str[MAXSTRING+1], * c_ptr;
   FILE			* fp;
   int			count=0, len, short_name, sym_ext_len;
   int			saved_cur_file_defined;
   struct ObjRec	* obj_ptr;
   struct PageRec	* saved_cur_page;

   saved_cur_page = curPage;
   for (curPage = firstPage; curPage != NULL; curPage = curPage->next)
      for (obj_ptr = curPage->top; obj_ptr != NULL; obj_ptr = obj_ptr->next)
         if (obj_ptr->type == OBJ_SYM)
            count++;
   curPage = saved_cur_page;

   switch (count)
   {
      case 0:
         MsgBox ("No symbol found!\n\nSymbol file not saved.",
               TOOL_NAME, INFO_MB);
         return;
      case 1:
         if (lastPageNum != 1)
         {
            MsgBox ("A symbol file can only have one page.\n\nSave aborted!",
                  TOOL_NAME, INFO_MB);
            return;
         }
         break;
      default:
         MsgBox ("Too many symbols!\n\nSymbol file not saved.",
               TOOL_NAME, INFO_MB);
         return;
   }
   MakeQuiescent ();

   sprintf (sym_ext_str, ".%s", SYM_FILE_EXT);
   sym_ext_len = strlen (sym_ext_str);

   if (*curFileName == '\0')
   {
      Dialog ("Please enter new file name:", "( <CR>: accept, <ESC>: cancel )",
            new_file_name);
      if (*new_file_name == '\0') return;

      len = strlen (new_file_name);
      if (len >= sym_ext_len)
      {
         if (strcmp (&new_file_name[len-sym_ext_len], sym_ext_str) != 0)
         {
            strcpy (name_without_ext, new_file_name);
            strcat (new_file_name, sym_ext_str);
         }
         else
         {
            strcpy (name_without_ext, new_file_name);
            name_without_ext[len-sym_ext_len] = '\0';
         }
   
         if (strlen (new_file_name) == sym_ext_len)
         {
            MsgBox ("No file name specified.\n\nFile not saved.",
                  TOOL_NAME, INFO_MB);
            return;
         }
      }
      else
      {
         strcpy (name_without_ext, new_file_name);
         strcat (new_file_name, sym_ext_str);
      }
   }
   else
   {
      len = strlen (curFileName);
      for (c_ptr = &curFileName[len-1]; c_ptr != curFileName; c_ptr--)
         if (*c_ptr == '/')
            break;
      if (*c_ptr == '/') c_ptr++;
      strcpy (new_file_name, c_ptr);
   }

   if (SelectSymDir (dir_name) == INVALID) { Msg (""); return; }

   if (strcmp (dir_name, ".") == 0)
      sprintf (new_full_name, "%s/%s", (curDirIsLocal ? curDir : curLocalDir),
            new_file_name);
   else
      sprintf (new_full_name, "%s/%s", dir_name, new_file_name);

   if (!OkayToCreateFile (new_full_name)) return;

   if ((short_name = IsPrefix (bootDir, new_full_name, &rest))) ++rest;
   if ((fp = fopen (new_full_name, "w")) == NULL)
   {
      if (short_name)
         sprintf (s, "Cannot open '%s', file not saved.", rest);
      else
         sprintf (s, "Cannot open '%s', file not saved.", new_full_name);
      MsgBox (s, TOOL_NAME, INFO_MB);
      return;
   }

   strcpy (saved_dir, (curDirIsLocal ? curDir : curLocalDir));
   strcpy (saved_file, curFileName);
   strcpy (saved_sym_dir, curSymDir);
   saved_cur_file_defined = curFileDefined;
   SetCurDir (new_full_name);
   curFileDefined = TRUE;

   strcpy (curSymDir, (curDirIsLocal ? curDir : curLocalDir));
   if (!DirInSymPath (curDirIsLocal ? curDir : curLocalDir)) UpdateSymInfo ();

   if (short_name)
      sprintf (s, "Saving '%s' ...", rest);
   else
      sprintf (s, "Saving '%s' ...", new_full_name);
   Msg (s);

   if (!saveCommentsInSaveNew) CleanUpComments ();
   writeFileFailed = FALSE;
   saved_cur_page = curPage;
   for (curPage = firstPage, count = 1; curPage != NULL;
         curPage = curPage->next, count++)
   {
      topObj = curPage->top;
      botObj = curPage->bot;
      Save (fp, botObj, 0, count);
   }
   curPage = saved_cur_page;
   topObj = curPage->top;
   botObj = curPage->bot;
   fclose (fp);

   if (writeFileFailed)
   {
      writeFileFailed = FALSE;
      sprintf (s, "Fail to write to '%s'.\n\nFile system may be full.",
            new_full_name);
      MsgBox (s, TOOL_NAME, INFO_MB);
   }
   else
   {
      if (short_name)
         sprintf (s, "File '%s' saved.", rest);
      else
         sprintf (s, "File '%s' saved.", new_full_name);
      Msg (s);
   }

   if (curDirIsLocal)
      strcpy (curDir, saved_dir);
   else
      strcpy (curLocalDir, saved_dir);
   strcpy (curFileName, saved_file);
   strcpy (curSymDir, saved_sym_dir);
   curFileDefined = saved_cur_file_defined;

   RedrawTitleWindow ();
}

void SaveFile ()
{
   int			i, len, count=0, short_name;
   struct ObjRec	* obj_ptr;
   FILE			* fp;
   char			ext[MAXPATHLENGTH+1], s[MAXPATHLENGTH+1];
   char			full_name[MAXPATHLENGTH+1], * rest;
   struct PageRec	* saved_cur_page;

   if (!curFileDefined || !curDirIsLocal)
   {
      SaveNewFile (FALSE);
      return;
   }

   len = strlen (curFileName);
   for (i = len-1; curFileName[i] != '.'; i--) ;
   strcpy (ext, &curFileName[i+1]);

   saved_cur_page = curPage;
   for (curPage = firstPage; curPage != NULL; curPage = curPage->next)
      for (obj_ptr = curPage->top; obj_ptr != NULL; obj_ptr = obj_ptr->next)
         if (obj_ptr->type == OBJ_SYM)
            count++;
   curPage = saved_cur_page;

   switch (count)
   {
      case 0:
         if (strcmp (ext, SYM_FILE_EXT) == 0)
         {
            sprintf (gszMsgBox, "%s.\n\n%s.",
                  "No symbol defined in a symbol file",
                  "Symbol file not saved");
            MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
            return;
         }
         break;
      case 1:
         if (strcmp (ext, OBJ_FILE_EXT) == 0)
         {
            sprintf (gszMsgBox, "%s.\n\n%s.",
                  "One symbol defined in OBJECT file",
                  "Object file not saved");
            MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
            return;
         }
         break;
      default:
         if (strcmp (ext, SYM_FILE_EXT) == 0)
         {
            sprintf (gszMsgBox, "%s!\n\n%s.",
                  "Too many symbols in a symbol file",
                  "Symbol file not saved");
            MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
            return;
         }
         break;
   }

   if (strcmp (ext, SYM_FILE_EXT) == 0)
      sprintf (full_name, "%s/%s", curSymDir, curFileName);
   else if (strcmp (ext, OBJ_FILE_EXT) == 0)
      sprintf (full_name, "%s/%s", curDir, curFileName);

   if ((short_name = IsPrefix (bootDir, full_name, &rest))) ++rest;
   if ((fp = fopen (full_name, "w")) == NULL)
   {
      if (short_name)
         sprintf (s, "Cannot open '%s', file not saved.", rest);
      else
         sprintf (s, "Cannot open '%s', file not saved.", full_name);
      MsgBox (s, TOOL_NAME, INFO_MB);
      return;
   }

   if (short_name)
      sprintf (s, "Saving '%s' ...", rest);
   else
      sprintf (s, "Saving '%s' ...", full_name);
   Msg (s);

   writeFileFailed = FALSE;
   MakeQuiescent ();
   saved_cur_page = curPage;
   for (curPage = firstPage, count = 1; curPage != NULL;
         curPage = curPage->next, count++)
   {
      topObj = curPage->top;
      botObj = curPage->bot;
      Save (fp, botObj, 0, count);
   }
   curPage = saved_cur_page;
   topObj = curPage->top;
   botObj = curPage->bot;
   fclose (fp);

   if (writeFileFailed)
   {
      writeFileFailed = FALSE;
      sprintf (s, "Fail to write to '%s'.\n\nFile system may be full.",
            full_name);
      MsgBox (s, TOOL_NAME, INFO_MB);
   }
   else
   {
      if (short_name)
         sprintf (s, "File '%s' saved.", rest);
      else
         sprintf (s, "File '%s' saved.", full_name);
      Msg (s);
      SetFileModified (FALSE);
      if (!NameInCurDir (curFileName)) UpdateDirInfo ();
   }
}

char * ParseStr (Str, C, Left, LeftSz)
   char	* Str, * Left;
   int	C, LeftSz;
{
   register char	* s = Str, * l = Left;
   register int		len = 0;
   int			max_len=LeftSz-1;
   char			the_char=(char)C;

   while (*s != '\0' && *s != the_char)
   {
      if (len < max_len)
      {
         *l++ = *s++;
         len++;
      }
      else
         break;
   }

   if (*s == the_char) s++;
   *l = '\0';

   while (len >= 2 && *Left == '\'' && *(--l) == '\'')
   {
      char	* c_ptr, * c_ptr1;

      *l-- = '\0';
      len -= 2;
      c_ptr = &Left[1];
      c_ptr1 = Left;
      while (*c_ptr != '\0') *c_ptr1++ = *c_ptr++;
      *c_ptr1 = '\0';
   }
   return (s);
}

char * FindChar (C, Str)
   int	C;
   char	* Str;
   /* returns the address of the character right after C of the string Str */
{
   register char	* s = Str, the_char=(char)C;

   while (*s != '\0' && *s != the_char) s++;

   if (*s == the_char) s++;
   return (s);
}

#define GETVALUE(val,name) \
      if (ScanValue("%d", &(val), name, "state") == INVALID) return (FALSE)
#define GETSTRNG(val,name) \
      if (ScanValue("%s", (val), name, "state") == INVALID) return (FALSE)

static
int ReadState(Inbuf)
   char *Inbuf;
{
   char *s, font_str[81];
   int page_style, forced_use_gray=FALSE, compat_dpi;

   s = FindChar ((int)'(', Inbuf);
   if (sscanf (s, "%d", &page_style) != 1) return (FALSE);
   s = FindChar ((int)',', s);
   if (*s == '\0')
      fileVersion = INVALID;
   else if (sscanf (s, "%d", &fileVersion) != 1)
      return (FALSE);

   if (fileVersion > CUR_VERSION) return (FALSE);

   if (!importingFile)
   {
      if (fileVersion <= 13)
      {
         switch (page_style)
         {
            case PORTRAIT: printMag = 100; break;
            case LANDSCAPE: printMag = 100; break;
            case HIGHPORT: printMag = 50; page_style = PORTRAIT; break;
            case HIGHLAND: printMag = 50; page_style = LANDSCAPE; break;
            case SLIDEPORT: printMag = 200; page_style = PORTRAIT; break;
            case SLIDELAND: printMag = 200; page_style = LANDSCAPE; break;
            default:
               sprintf(gszMsgBox, "Unrecognizable page style '%1d'.",
                     page_style);
               TwoLineMsg(gszMsgBox, "Portrait style assumed.");
               page_style = PORTRAIT;
               printMag = 100;
               break;
         }
      }
      else
      {
         if (page_style != PORTRAIT && page_style != LANDSCAPE)
         {
            sprintf(gszMsgBox, "Unrecognizable page style '%1d'.", page_style);
            TwoLineMsg(gszMsgBox, "Portrait style assumed.");
            page_style = PORTRAIT;
         }
         s = FindChar((int)',', s);
         sscanf (s, "%d", &printMag);
      }
      pageStyle = page_style;
   }

   if (PRTGIF && useGray) forced_use_gray = TRUE;
   if (!importingFile)
   {
      int	page_arg1=1, page_arg2=1;
      int	one_page_width=onePageWidth, one_page_height=onePageHeight;

      if (fileVersion >= 2)
      {
         compat_dpi = FONT_DPI_75;
         curDash = 0;
         gridSystem = ENGLISH_GRID;
         xyMetricGrid = DEFAULT_METRIC_GRID;
         textVSpace = 0;
         zoomedIn = FALSE;
         curRotate = ROTATE0;
         rcbRadius = DEF_RCB_RADIUS;
         pageLayoutMode = PAGE_STACK;
         paperCol = paperRow = 1;
         curPageNum = lastPageNum = 1;
         pageLineShownInTileMode = TRUE;

         if (usePaperSizeStoredInFile) ResetOnePageSize ();

         s = FindChar ((int)',', s);

         InitScan (s, "\t\n, ");

         if (fileVersion <= 3)
         {
            GETVALUE(drawOrigX,     "X Draw Origin");
            GETVALUE(drawOrigY,     "Y Draw Origin");
            GETVALUE(zoomScale,     "Zoom scale");
            GETVALUE(xyEnglishGrid, "English Grid");
            GETVALUE(gridOn,        "Grid");
            GETVALUE(colorIndex,    "Color");
            GETVALUE(horiAlign,     "Horizontal Align");
            GETVALUE(vertAlign,     "Vertical Align");
            GETVALUE(lineWidth,     "Line Width");
            GETVALUE(lineStyle,     "Line Style");
            GETVALUE(objFill,       "Fill Pattern");
            GETVALUE(penPat,        "Pen Pattern");
            GETVALUE(textJust,      "Text Justify");
            GETVALUE(curFont,       "Font Name");
            GETVALUE(curStyle,      "Font Style");
            GETVALUE(curSize,       "Font Size");

            if (lineWidth == LINE_CURVED)
            {
               lineWidth = 0;
               curSpline = LT_SPLINE;
            }
            else
               curSpline = LT_STRAIGHT;
         }
         else if (fileVersion <= 7)
         {
            GETVALUE(drawOrigX,     "X Draw Origin");
            GETVALUE(drawOrigY,     "Y Draw Origin");
            GETVALUE(zoomScale,     "Zoom scale");
            GETVALUE(xyEnglishGrid, "English Grid");
            GETVALUE(gridOn,        "Grid");
            GETVALUE(colorIndex,    "Color");
            GETVALUE(horiAlign,     "Horizontal Align");
            GETVALUE(vertAlign,     "Vertical Align");
            GETVALUE(lineWidth,     "Line Width");
            GETVALUE(curSpline,     "Spline");
            GETVALUE(lineStyle,     "Line Style");
            GETVALUE(objFill,       "Fill Pattern");
            GETVALUE(penPat,        "Pen Pattern");
            GETVALUE(textJust,      "Text Justify");
            GETVALUE(curFont,       "Font Name");
            GETVALUE(curStyle,      "Font Style");
            GETVALUE(curSize,       "Font Size");
         }
         else if (fileVersion <= 8)
         {
            GETVALUE(drawOrigX,     "X Draw Origin");
            GETVALUE(drawOrigY,     "Y Draw Origin");
            GETVALUE(zoomScale,     "Zoom scale");
            GETVALUE(xyEnglishGrid, "English Grid");
            GETVALUE(gridOn,        "Grid");
            GETVALUE(colorIndex,    "Color");
            GETVALUE(horiAlign,     "Horizontal Align");
            GETVALUE(vertAlign,     "Vertical Align");
            GETVALUE(lineWidth,     "Line Width");
            GETVALUE(curSpline,     "Spline");
            GETVALUE(lineStyle,     "Line Style");
            GETVALUE(objFill,       "Fill Pattern");
            GETVALUE(penPat,        "Pen Pattern");
            GETVALUE(textJust,      "Text Justify");
            GETVALUE(curFont,       "Font Name");
            GETVALUE(curStyle,      "Font Style");
            GETVALUE(curSize,       "Font Size");
            GETVALUE(compat_dpi,    "Font DPI");
         }
         else if (fileVersion <= 11)
         {
            GETVALUE(drawOrigX,     "X Draw Origin");
            GETVALUE(drawOrigY,     "Y Draw Origin");
            GETVALUE(zoomScale,     "Zoom scale");
            GETVALUE(xyEnglishGrid, "English Grid");
            GETVALUE(gridOn,        "Grid");
            GETVALUE(colorIndex,    "Color");
            GETVALUE(horiAlign,     "Horizontal Align");
            GETVALUE(vertAlign,     "Vertical Align");
            GETVALUE(lineWidth,     "Line Width");
            GETVALUE(curSpline,     "Spline");
            GETVALUE(lineStyle,     "Line Style");
            GETVALUE(objFill,       "Fill Pattern");
            GETVALUE(penPat,        "Pen Pattern");
            GETVALUE(textJust,      "Text Justify");
            GETVALUE(curFont,       "Font Name");
            GETVALUE(curStyle,      "Font Style");
            GETVALUE(curSize,       "Font Size");
            GETVALUE(compat_dpi,    "Font DPI");
            GETVALUE(curDash,       "Dash Style");
         }
         else if (fileVersion <= 12)
         {
            GETVALUE(drawOrigX,     "X Draw Origin");
            GETVALUE(drawOrigY,     "Y Draw Origin");
            GETVALUE(zoomScale,     "Zoom scale");
            GETVALUE(xyEnglishGrid, "English Grid");
            GETVALUE(gridOn,        "Grid");
            GETVALUE(colorIndex,    "Color");
            GETVALUE(horiAlign,     "Horizontal Align");
            GETVALUE(vertAlign,     "Vertical Align");
            GETVALUE(lineWidth,     "Line Width");
            GETVALUE(curSpline,     "Spline");
            GETVALUE(lineStyle,     "Line Style");
            GETVALUE(objFill,       "Fill Pattern");
            GETVALUE(penPat,        "Pen Pattern");
            GETVALUE(textJust,      "Text Justify");
            GETVALUE(curFont,       "Font Name");
            GETVALUE(curStyle,      "Font Style");
            GETVALUE(curSize,       "Font Size");
            GETVALUE(compat_dpi,    "Font DPI");
            GETVALUE(curDash,       "Dash Style");
            GETVALUE(gridSystem,    "Grid System");
            GETVALUE(xyMetricGrid,  "Metric Grid");
         }
         else if (fileVersion <= 18)
         {
            GETVALUE(drawOrigX,     "X Draw Origin");
            GETVALUE(drawOrigY,     "Y Draw Origin");
            GETVALUE(zoomScale,     "Zoom scale");
            GETVALUE(xyEnglishGrid, "English Grid");
            GETVALUE(gridOn,        "Grid");
            GETVALUE(colorIndex,    "Color");
            GETVALUE(horiAlign,     "Horizontal Align");
            GETVALUE(vertAlign,     "Vertical Align");
            GETVALUE(lineWidth,     "Line Width");
            GETVALUE(curSpline,     "Spline");
            GETVALUE(lineStyle,     "Line Style");
            GETVALUE(objFill,       "Fill Pattern");
            GETVALUE(penPat,        "Pen Pattern");
            GETVALUE(textJust,      "Text Justify");
            GETVALUE(curFont,       "Font Name");
            GETVALUE(curStyle,      "Font Style");
            GETVALUE(curSize,       "Font Size");
            GETVALUE(compat_dpi,    "Font DPI");
            GETVALUE(curDash,       "Dash Style");
            GETVALUE(gridSystem,    "Grid System");
            GETVALUE(xyMetricGrid,  "Metric Grid");
            GETVALUE(textVSpace,    "Text Vertical Spacing");
         }
         else if (fileVersion <= 19)
         {
            GETVALUE(drawOrigX,     "X Draw Origin");
            GETVALUE(drawOrigY,     "Y Draw Origin");
            GETVALUE(zoomScale,     "Zoom scale");
            GETVALUE(xyEnglishGrid, "English Grid");
            GETVALUE(gridOn,        "Grid");
            GETVALUE(colorIndex,    "Color");
            GETVALUE(horiAlign,     "Horizontal Align");
            GETVALUE(vertAlign,     "Vertical Align");
            GETVALUE(lineWidth,     "Line Width");
            GETVALUE(curSpline,     "Spline");
            GETVALUE(lineStyle,     "Line Style");
            GETVALUE(objFill,       "Fill Pattern");
            GETVALUE(penPat,        "Pen Pattern");
            GETVALUE(textJust,      "Text Justify");
            GETVALUE(curFont,       "Font Name");
            GETVALUE(curStyle,      "Font Style");
            GETVALUE(curSize,       "Font Size");
            GETVALUE(compat_dpi,    "Font DPI");
            GETVALUE(curDash,       "Dash Style");
            GETVALUE(gridSystem,    "Grid System");
            GETVALUE(xyMetricGrid,  "Metric Grid");
            GETVALUE(textVSpace,    "Text Vertical Spacing");
            GETVALUE(zoomedIn,      "Zoomed In");
         }
         else if (fileVersion <= 21)
         {
            GETVALUE(drawOrigX,     "X Draw Origin");
            GETVALUE(drawOrigY,     "Y Draw Origin");
            GETVALUE(zoomScale,     "Zoom scale");
            GETVALUE(xyEnglishGrid, "English Grid");
            GETVALUE(gridOn,        "Grid");
            GETVALUE(colorIndex,    "Color");
            GETVALUE(horiAlign,     "Horizontal Align");
            GETVALUE(vertAlign,     "Vertical Align");
            GETVALUE(lineWidth,     "Line Width");
            GETVALUE(curSpline,     "Spline");
            GETVALUE(lineStyle,     "Line Style");
            GETVALUE(objFill,       "Fill Pattern");
            GETVALUE(penPat,        "Pen Pattern");
            GETVALUE(textJust,      "Text Justify");
            GETVALUE(curFont,       "Font Name");
            GETVALUE(curStyle,      "Font Style");
            GETVALUE(curSize,       "Font Size");
            GETVALUE(compat_dpi,    "Font DPI");
            GETVALUE(curDash,       "Dash Style");
            GETVALUE(gridSystem,    "Grid System");
            GETVALUE(xyMetricGrid,  "Metric Grid");
            GETVALUE(textVSpace,    "Text Vertical Spacing");
            GETVALUE(zoomedIn,      "Zoomed In");
            GETVALUE(gridShown,     "Grid Shown");
            GETVALUE(moveMode,      "Move Mode");
         }
         else if (fileVersion <= 26)
         {
            GETVALUE(drawOrigX,     "X Draw Origin");
            GETVALUE(drawOrigY,     "Y Draw Origin");
            GETVALUE(zoomScale,     "Zoom scale");
            GETVALUE(xyEnglishGrid, "English Grid");
            GETVALUE(gridOn,        "Grid");
            GETVALUE(colorIndex,    "Color");
            GETVALUE(horiAlign,     "Horizontal Align");
            GETVALUE(vertAlign,     "Vertical Align");
            GETVALUE(lineWidth,     "Line Width");
            GETVALUE(curSpline,     "Spline");
            GETVALUE(lineStyle,     "Line Style");
            GETVALUE(objFill,       "Fill Pattern");
            GETVALUE(penPat,        "Pen Pattern");
            GETVALUE(textJust,      "Text Justify");
            GETVALUE(curFont,       "Font Name");
            GETVALUE(curStyle,      "Font Style");
            GETVALUE(curSize,       "Font Size");
            GETVALUE(compat_dpi,    "Font DPI");
            GETVALUE(curDash,       "Dash Style");
            GETVALUE(gridSystem,    "Grid System");
            GETVALUE(xyMetricGrid,  "Metric Grid");
            GETVALUE(textVSpace,    "Text Vertical Spacing");
            GETVALUE(zoomedIn,      "Zoomed In");
            GETVALUE(gridShown,     "Grid Shown");
            GETVALUE(moveMode,      "Move Mode");
            GETVALUE(curRotate,     "Text Rotation");
            GETVALUE(rcbRadius,     "RCBox Radius");
         }
         else if (fileVersion <= 27)
         {
            GETVALUE(drawOrigX,     "X Draw Origin");
            GETVALUE(drawOrigY,     "Y Draw Origin");
            GETVALUE(zoomScale,     "Zoom scale");
            GETVALUE(xyEnglishGrid, "English Grid");
            GETVALUE(gridOn,        "Grid");
            GETVALUE(colorIndex,    "Color");
            GETVALUE(horiAlign,     "Horizontal Align");
            GETVALUE(vertAlign,     "Vertical Align");
            GETVALUE(lineWidth,     "Line Width");
            GETVALUE(curSpline,     "Spline");
            GETVALUE(lineStyle,     "Line Style");
            GETVALUE(objFill,       "Fill Pattern");
            GETVALUE(penPat,        "Pen Pattern");
            GETVALUE(textJust,      "Text Justify");
            GETVALUE(curFont,       "Font Name");
            GETVALUE(curStyle,      "Font Style");
            GETVALUE(curSize,       "Font Size");
            GETVALUE(compat_dpi,    "Font DPI");
            GETVALUE(curDash,       "Dash Style");
            GETVALUE(gridSystem,    "Grid System");
            GETVALUE(xyMetricGrid,  "Metric Grid");
            GETVALUE(textVSpace,    "Text Vertical Spacing");
            GETVALUE(zoomedIn,      "Zoomed In");
            GETVALUE(gridShown,     "Grid Shown");
            GETVALUE(moveMode,      "Move Mode");
            GETVALUE(curRotate,     "Text Rotation");
            GETVALUE(rcbRadius,     "RCBox Radius");
            GETVALUE(useGray,       "Use Gray Scale");
         }
         else if (fileVersion <= 28)
         {  /* Matsuda's Version */
            GETVALUE(drawOrigX,     "X Draw Origin");
            GETVALUE(drawOrigY,     "Y Draw Origin");
            GETVALUE(zoomScale,     "Zoom scale");
            GETVALUE(xyEnglishGrid, "English Grid");
            GETVALUE(gridOn,        "Grid");
            GETVALUE(colorIndex,    "Color");
            GETVALUE(horiAlign,     "Horizontal Align");
            GETVALUE(vertAlign,     "Vertical Align");
            GETVALUE(lineWidth,     "Line Width");
            GETVALUE(curSpline,     "Spline");
            GETVALUE(lineStyle,     "Line Style");
            GETVALUE(objFill,       "Fill Pattern");
            GETVALUE(penPat,        "Pen Pattern");
            GETVALUE(textJust,      "Text Justify");
            GETVALUE(curFont,       "Font Name");
            GETVALUE(curStyle,      "Font Style");
            GETVALUE(curSize,       "Font Size");
            GETVALUE(compat_dpi,    "Font DPI");
            GETVALUE(curDash,       "Dash Style");
            GETVALUE(gridSystem,    "Grid System");
            GETVALUE(xyMetricGrid,  "Metric Grid");
            GETVALUE(textVSpace,    "Text Vertical Spacing");
            GETVALUE(zoomedIn,      "Zoomed In");
            GETVALUE(gridShown,     "Grid Shown");
            GETVALUE(moveMode,      "Move Mode");
            GETVALUE(curRotate,     "Text Rotation");
            GETVALUE(rcbRadius,     "RCBox Radius");
            GETVALUE(useGray,       "Use Gray Scale");
            GETVALUE(paperCol,      "Num Columns");
            GETVALUE(paperRow,      "Num Rows");
            GETVALUE(curPageNum,    "Current Page Number");
            GETVALUE(lastPageNum,   "Last Page Number");
         }
         else if (fileVersion <= 29)
         {
            GETVALUE(drawOrigX,     "X Draw Origin");
            GETVALUE(drawOrigY,     "Y Draw Origin");
            GETVALUE(zoomScale,     "Zoom scale");
            GETVALUE(xyEnglishGrid, "English Grid");
            GETVALUE(gridOn,        "Grid");
            GETVALUE(colorIndex,    "Color");
            GETVALUE(horiAlign,     "Horizontal Align");
            GETVALUE(vertAlign,     "Vertical Align");
            GETVALUE(lineWidth,     "Line Width");
            GETVALUE(curSpline,     "Spline");
            GETVALUE(lineStyle,     "Line Style");
            GETVALUE(objFill,       "Fill Pattern");
            GETVALUE(penPat,        "Pen Pattern");
            GETVALUE(textJust,      "Text Justify");
            GETVALUE(curFont,       "Font Name");
            GETVALUE(curStyle,      "Font Style");
            GETVALUE(curSize,       "Font Size");
            GETVALUE(compat_dpi,    "Font DPI");
            GETVALUE(curDash,       "Dash Style");
            GETVALUE(gridSystem,    "Grid System");
            GETVALUE(xyMetricGrid,  "Metric Grid");
            GETVALUE(textVSpace,    "Text Vertical Spacing");
            GETVALUE(zoomedIn,      "Zoomed In");
            GETVALUE(gridShown,     "Grid Shown");
            GETVALUE(moveMode,      "Move Mode");
            GETVALUE(curRotate,     "Text Rotation");
            GETVALUE(rcbRadius,     "RCBox Radius");
            GETVALUE(useGray,       "Use Gray Scale");
            GETVALUE(pageLayoutMode,"Page Layout Mode");
            GETVALUE(page_arg1,     "Page Layout Subarg 1");
            GETVALUE(page_arg2,     "Page Layout Subarg 2");
            GETVALUE(pageLineShownInTileMode,"Page Lines Shown");
         }
         else if (fileVersion <= 30)
         {
            GETVALUE(drawOrigX,     "X Draw Origin");
            GETVALUE(drawOrigY,     "Y Draw Origin");
            GETVALUE(zoomScale,     "Zoom scale");
            GETVALUE(xyEnglishGrid, "English Grid");
            GETVALUE(gridOn,        "Grid");
            GETVALUE(colorIndex,    "Color");
            GETVALUE(horiAlign,     "Horizontal Align");
            GETVALUE(vertAlign,     "Vertical Align");
            GETVALUE(lineWidth,     "Line Width");
            GETVALUE(curSpline,     "Spline");
            GETVALUE(lineStyle,     "Line Style");
            GETVALUE(objFill,       "Fill Pattern");
            GETVALUE(penPat,        "Pen Pattern");
            GETVALUE(textJust,      "Text Justify");
            GETSTRNG(font_str,      "Font Name String");
            GETVALUE(curStyle,      "Font Style");
            GETVALUE(curSize,       "Font Size");
            GETVALUE(compat_dpi,    "Font DPI");
            GETVALUE(curDash,       "Dash Style");
            GETVALUE(gridSystem,    "Grid System");
            GETVALUE(xyMetricGrid,  "Metric Grid");
            GETVALUE(textVSpace,    "Text Vertical Spacing");
            GETVALUE(zoomedIn,      "Zoomed In");
            GETVALUE(gridShown,     "Grid Shown");
            GETVALUE(moveMode,      "Move Mode");
            GETVALUE(curRotate,     "Text Rotation");
            GETVALUE(rcbRadius,     "RCBox Radius");
            GETVALUE(useGray,       "Use Gray Scale");
            GETVALUE(pageLayoutMode,"Page Layout Mode");
            GETVALUE(page_arg1,     "Page Layout Subarg 1");
            GETVALUE(page_arg2,     "Page Layout Subarg 2");
            GETVALUE(pageLineShownInTileMode,"Page Lines Shown");
         }
         else if (fileVersion <= 31)
         {
            GETVALUE(drawOrigX,     "X Draw Origin");
            GETVALUE(drawOrigY,     "Y Draw Origin");
            GETVALUE(zoomScale,     "Zoom scale");
            GETVALUE(xyEnglishGrid, "English Grid");
            GETVALUE(gridOn,        "Grid");
            GETVALUE(colorIndex,    "Color");
            GETVALUE(horiAlign,     "Horizontal Align");
            GETVALUE(vertAlign,     "Vertical Align");
            GETVALUE(lineWidth,     "Line Width");
            GETVALUE(curSpline,     "Spline");
            GETVALUE(lineStyle,     "Line Style");
            GETVALUE(objFill,       "Fill Pattern");
            GETVALUE(penPat,        "Pen Pattern");
            GETVALUE(textJust,      "Text Justify");
            GETSTRNG(font_str,      "Font Name String");
            GETVALUE(curStyle,      "Font Style");
            GETVALUE(curSize,       "Font Size");
            GETVALUE(compat_dpi,    "Font DPI");
            GETVALUE(curDash,       "Dash Style");
            GETVALUE(gridSystem,    "Grid System");
            GETVALUE(xyMetricGrid,  "Metric Grid");
            GETVALUE(textVSpace,    "Text Vertical Spacing");
            GETVALUE(zoomedIn,      "Zoomed In");
            GETVALUE(gridShown,     "Grid Shown");
            GETVALUE(moveMode,      "Move Mode");
            GETVALUE(curRotate,     "Text Rotation");
            GETVALUE(rcbRadius,     "RCBox Radius");
            GETVALUE(useGray,       "Use Gray Scale");
            GETVALUE(pageLayoutMode,"Page Layout Mode");
            GETVALUE(page_arg1,     "Page Layout Subarg 1");
            GETVALUE(page_arg2,     "Page Layout Subarg 2");
            GETVALUE(pageLineShownInTileMode,"Page Lines Shown");
            GETVALUE(colorDump,     "Print In Color");
         }
         else if (fileVersion <= 32)
         {
            GETVALUE(drawOrigX,      "X Draw Origin");
            GETVALUE(drawOrigY,      "Y Draw Origin");
            GETVALUE(zoomScale,      "Zoom scale");
            GETVALUE(xyEnglishGrid,  "English Grid");
            GETVALUE(gridOn,         "Grid");
            GETVALUE(colorIndex,     "Color");
            GETVALUE(horiAlign,      "Horizontal Align");
            GETVALUE(vertAlign,      "Vertical Align");
            GETVALUE(lineWidth,      "Line Width");
            GETVALUE(curSpline,      "Spline");
            GETVALUE(lineStyle,      "Line Style");
            GETVALUE(objFill,        "Fill Pattern");
            GETVALUE(penPat,         "Pen Pattern");
            GETVALUE(textJust,       "Text Justify");
            GETSTRNG(font_str,       "Font Name String");
            GETVALUE(curStyle,       "Font Style");
            GETVALUE(curSize,        "Font Size");
            GETVALUE(compat_dpi,     "Font DPI");
            GETVALUE(curDash,        "Dash Style");
            GETVALUE(gridSystem,     "Grid System");
            GETVALUE(xyMetricGrid,   "Metric Grid");
            GETVALUE(textVSpace,     "Text Vertical Spacing");
            GETVALUE(zoomedIn,       "Zoomed In");
            GETVALUE(gridShown,      "Grid Shown");
            GETVALUE(moveMode,       "Move Mode");
            GETVALUE(curRotate,      "Text Rotation");
            GETVALUE(rcbRadius,      "RCBox Radius");
            GETVALUE(useGray,        "Use Gray Scale");
            GETVALUE(pageLayoutMode, "Page Layout Mode");
            GETVALUE(page_arg1,      "Page Layout Subarg 1");
            GETVALUE(page_arg2,      "Page Layout Subarg 2");
            GETVALUE(pageLineShownInTileMode,"Page Lines Shown");
            GETVALUE(colorDump,      "Print In Color");
            GETVALUE(one_page_width, "One Page Width");
            GETVALUE(one_page_height,"One Page Height");
         }
         else
         {
            GETVALUE(drawOrigX,      "X Draw Origin");
            GETVALUE(drawOrigY,      "Y Draw Origin");
            GETVALUE(zoomScale,      "Zoom scale");
            GETVALUE(xyEnglishGrid,  "English Grid");
            GETVALUE(gridOn,         "Grid");
            GETVALUE(colorIndex,     "Color");
            GETVALUE(horiAlign,      "Horizontal Align");
            GETVALUE(vertAlign,      "Vertical Align");
            GETVALUE(lineWidth,      "Line Width");
            GETVALUE(curSpline,      "Spline");
            GETVALUE(lineStyle,      "Line Style");
            GETVALUE(objFill,        "Fill Pattern");
            GETVALUE(penPat,         "Pen Pattern");
            GETVALUE(textJust,       "Text Justify");
            GETSTRNG(font_str,       "Font Name String");
            GETVALUE(curStyle,       "Font Style");
            GETVALUE(curSize,        "Font Size");
            GETVALUE(compat_dpi,     "Font DPI");
            GETVALUE(curDash,        "Dash Style");
            GETVALUE(gridSystem,     "Grid System");
            GETVALUE(xyMetricGrid,   "Metric Grid");
            GETVALUE(textVSpace,     "Text Vertical Spacing");
            GETVALUE(zoomedIn,       "Zoomed In");
            GETVALUE(gridShown,      "Grid Shown");
            GETVALUE(moveMode,       "Move Mode");
            GETVALUE(curRotate,      "Text Rotation");
            GETVALUE(rcbRadius,      "RCBox Radius");
            GETVALUE(useGray,        "Use Gray Scale");
            GETVALUE(pageLayoutMode, "Page Layout Mode");
            GETVALUE(page_arg1,      "Page Layout Subarg 1");
            GETVALUE(page_arg2,      "Page Layout Subarg 2");
            GETVALUE(pageLineShownInTileMode,"Page Lines Shown");
            GETVALUE(colorDump,      "Print In Color");
            GETVALUE(one_page_width, "One Page Width");
            GETVALUE(one_page_height,"One Page Height");
            GETVALUE(stretchableText,"Stretchable Text");
            GETVALUE(textRotation,   "Text Rotation");
            GETVALUE(rotationIncrement,"Rotation Increment");
         }
         if (fileVersion <= 28) readingPageNum++;
         if (fileVersion <= 29) {
            curSize = GetCompatibleSize(compat_dpi,curSize);
         } else {
            int len=strlen(font_str);
            char *s;

            if (*font_str == '\'' && font_str[len-1] == '\'')
            {
               font_str[len-1] = '\0';
               s = &font_str[1];
            }
            else
               s = font_str;
            curFont = GetFontIndex (s, curStyle, TRUE);
            if (curFont == INVALID && !PRTGIF) {
               sprintf(gszMsgBox, "Cannot find screen font for '%s'.", s);
               TwoLineMsg(gszMsgBox, "    Use Times instead.");
               curFont = FONT_TIM;
            }
         }
         switch (pageLayoutMode) {
         case PAGE_STACK:
            curPageNum = page_arg1;
            lastPageNum = page_arg2;
            paperCol = paperRow = 1;
            pageLineShownInTileMode = TRUE;
            break;
         case PAGE_TILE:
            paperCol = page_arg1;
            paperRow = page_arg2;
            curPageNum = lastPageNum = 1;
            break;
         }
         loadedCurPageNum = curPageNum;
         InitPage ();
         curPageNum = loadedCurPageNum;

         if (PRTGIF)
         {
            if (forced_use_gray) useGray = TRUE;
            return (TRUE);
         }
         if (colorIndex >= maxColors)
         {
            fprintf (stderr, "%s #%1d, use '%s' %s.\n",
                  "In reading state, can not find color", colorIndex,
                  colorMenuItems[defaultColorIndex], "as the current color");
            colorIndex = defaultColorIndex;
         }
         SetCanvasFont ();
         if (fileVersion <= 13)
         {
            switch (gridSystem)
            {
               case ENGLISH_GRID:
                  drawOrigX += HALF_INCH;
                  drawOrigY += HALF_INCH;
                  break;
               case METRIC_GRID:
                  drawOrigX += 2.5*ONE_CM;
                  drawOrigY += 2.5*ONE_CM;
                  break;
            }
         }
         if (usePaperSizeStoredInFile && fileVersion >= 32)
         {
            if (pageStyle == LANDSCAPE)
            {
               onePageWidth = one_page_height;
               onePageHeight = one_page_width;
            }
            else
            {
               onePageWidth = one_page_width;
               onePageHeight = one_page_height;
            }
            SetPSPageWidthHeight ();
         }
      }
      UpdPageStyle (pageStyle);
      if (PRTGIF) return (TRUE);

      if (lineWidth >= maxLineWidths)
      {
         fprintf (stderr, "%s '%1d' is out of range!  Set to 0.\n",
               "File's linewidth index", lineWidth);
         lineWidth = 0;
      }

      UpdDrawWinWH ();
      RedrawScrollBars ();
      ShowPage ();
      ShowPageLayout ();
      UpdDrawWinBBox ();

      SetDefaultDrawWinClipRecs ();

      DrawPaperBoundary (drawWindow);
      RedrawGridLines (drawWindow);
      RedrawPageLines (drawWindow);
      RedrawRulers ();
      RedrawChoiceWindow ();
   }
   return (TRUE);
}

static
void ReadObjAttrs (MinFileVersion, FP, ObjPtr)
   int			MinFileVersion;
   FILE			* FP;
   struct ObjRec	* * ObjPtr;
{
   struct AttrRec	* top_attr = NULL, * bot_attr = NULL, * attr_ptr;

   if (fileVersion <= MinFileVersion) return;

   while (ReadAttr (FP, &attr_ptr))
   {
      attr_ptr->owner = *ObjPtr;
      attr_ptr->prev = NULL;
      attr_ptr->next = top_attr;
      if (top_attr == NULL)
         bot_attr = attr_ptr;
      else
         top_attr->prev = attr_ptr;
      top_attr = attr_ptr;
   }
   if (bot_attr != NULL) bot_attr->next = NULL;
   if (*ObjPtr == NULL) {
      DelAllAttrs(top_attr);
   } else {
      (*ObjPtr)->fattr = top_attr;
      (*ObjPtr)->lattr = bot_attr;
   }
}

static
int ReadPageObj (Inbuf, ppsz_page_name)
   char	* Inbuf, **ppsz_page_name;
{
   int	page_num;
   char	* s, * c_ptr;

   if (ppsz_page_name != NULL) *ppsz_page_name = NULL;
   s = FindChar ((int)'(', Inbuf);
   if (sscanf (s, "%d", &page_num) != 1)
   {
      (void) sprintf(gszMsgBox, "%s, %d:  Missing %s in %s.",
            scanFileName, scanLineNum, "page_num", "page");
      if (PRTGIF) {
         fprintf(stderr, "%s\n", gszMsgBox);
      } else {
         Msg(gszMsgBox);
      }
      return FALSE;
   }
   if (!importingFile)
   {
      for (curPage = firstPage; page_num != 1 && curPage != NULL;
            curPage = curPage->next, page_num--) ;
      if (curPage == NULL)
      {
         sprintf (gszMsgBox, "Malformed input file (%s %1d).  Abort!",
               "apparently reading page", page_num);
         if (PRTGIF) {
            fprintf (stderr, "%s\n", gszMsgBox);
         } else {
            MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
         }
         return FALSE;
      }
      s = FindChar ((int)',', s);
      c_ptr = FindChar ((int)'"', s);
      s = ReadString (c_ptr);
      *(--s) = '\0';
      if (*c_ptr != '\0') {
         curPage->name = (char*)malloc((strlen(c_ptr)+1)*sizeof(char));
         if (curPage->name == NULL) FailAllocMessage();
         strcpy(curPage->name, c_ptr);
      }
      topObj = curPage->top;
      botObj = curPage->bot;

      curPage->layer_on = TRUE;
      if (fileVersion <= 32) {
      } else {
         int layer_on=TRUE;

         s = FindChar((int)',', ++s);
         if (s == NULL || sscanf(s, "%d", &layer_on) != 1) {
            (void)sprintf(gszMsgBox, "%s, %d:  Missing %s in %s.",
                  scanFileName, scanLineNum, "layer_on", "page");
            if (PRTGIF) {
               fprintf (stderr, "%s\n", gszMsgBox);
            } else {
               MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
            }
            return FALSE;
         }
         curPage->layer_on = layer_on;
      }
   }
   else
   {
      s = FindChar ((int)',', s);
      c_ptr = FindChar ((int)'"', s);
      s = ReadString (c_ptr);
      *(--s) = '\0';
      if (*c_ptr != '\0' && ppsz_page_name != NULL) {
         *ppsz_page_name = UtilStrDup(c_ptr);
      }
   }
   readingPageNum++;
   return TRUE;
}

static
int ReadUnitObj(Inbuf)
   char *Inbuf;
{
   if (!importingFile) {
      char *s=FindChar((int)'(', Inbuf), *c_ptr;

      c_ptr = FindChar((int)'"', s);
      s = ReadString(c_ptr);
      *(--s) = '\0';
      if (SetUnit(c_ptr)) {
         return TRUE;
      } else {
         return FALSE;
      }
   }
   return TRUE;
}

static
int FreeBufAndReturn(buf, rc)
   char *buf;
   int rc;
{
   if (buf != NULL) free(buf);
   return rc;
}

int ReadObj (FP, ObjPtr)
   FILE			* FP;
   struct ObjRec	* * ObjPtr;
{
   char *line, obj_name[80];
   int read_state_ok;

   *ObjPtr = NULL;
   while ((line=UtilGetALine(FP)) != NULL) {
      char *c_ptr;

      scanLineNum++;
      if (*line == ']') return FreeBufAndReturn(line, FALSE);

      if (*line == '%') {
         if (!importingFile && line[1]=='%') {
            int	line_len=strlen(line);

            if (savedComments == NULL) {
               if ((savedComments=(char*)malloc((line_len+2)*sizeof(char)))
                     == NULL) {
                  FailAllocMessage();
               }
               *savedComments = '\0';
            } else {
               if ((savedComments=(char*)realloc(savedComments,
                     savedCommentsLen+line_len+2)) == NULL) {
                  FailAllocMessage();
               }
               savedComments[savedCommentsLen] = '\0';
            }
            strcat(savedComments, line);
            savedCommentsLen += line_len;
            savedComments[savedCommentsLen++] = '\n';
            savedComments[savedCommentsLen] = '\0';
         }
         free(line);
         continue;
      }

      if (ParseStr (line, (int)'(', obj_name, sizeof(obj_name)) == NULL)
      {
      }
      else if (strcmp (obj_name, "poly") == 0)
      {
         ReadPolyObj (FP, line, ObjPtr);
         if (*ObjPtr == NULL) return FreeBufAndReturn(line, FALSE);
         ReadObjAttrs (INVALID, FP, ObjPtr);
         if (RetractedArrowAttr(*ObjPtr) ||
               AutoRetractedArrowAttr(*ObjPtr, TRUE)) {
            /* fake the undoingOrRedoing so that no */
            /*		actual auto-adjusting is done */
            undoingOrRedoing = TRUE;
            AdjObjSplineVs(*ObjPtr);
            undoingOrRedoing = FALSE;
         }
         AdjObjBBox (*ObjPtr);
         return FreeBufAndReturn(line, TRUE);
      }
      else if (strcmp (obj_name, "box") == 0)
      {
         ReadBoxObj (FP, line, ObjPtr);
         if (*ObjPtr == NULL) return FreeBufAndReturn(line, FALSE);
         ReadObjAttrs (START_HAVING_ATTRS-1, FP, ObjPtr);
         AdjObjBBox (*ObjPtr);
         return FreeBufAndReturn(line, TRUE);
      }
      else if (strcmp (obj_name, "oval") == 0)
      {
         ReadOvalObj (FP, line, ObjPtr);
         if (*ObjPtr == NULL) return FreeBufAndReturn(line, FALSE);
         ReadObjAttrs (START_HAVING_ATTRS-1, FP, ObjPtr);
         AdjObjBBox (*ObjPtr);
         return FreeBufAndReturn(line, TRUE);
      }
      else if (strcmp (obj_name, "text") == 0)
      {
         ReadTextObj (FP, line, ObjPtr);
         if (*ObjPtr == NULL) return FreeBufAndReturn(line, FALSE);
         return FreeBufAndReturn(line, TRUE);
      }
      else if (strcmp (obj_name, "polygon") == 0)
      {
         ReadPolygonObj (FP, line, ObjPtr);
         if (*ObjPtr == NULL) return FreeBufAndReturn(line, FALSE);
         ReadObjAttrs (START_HAVING_ATTRS-1, FP, ObjPtr);
         AdjObjBBox (*ObjPtr);
         return FreeBufAndReturn(line, TRUE);
      }
      else if (strcmp (obj_name, "arc") == 0)
      {
         ReadArcObj (FP, line, ObjPtr);
         if (*ObjPtr == NULL) return FreeBufAndReturn(line, FALSE);
         ReadObjAttrs (START_HAVING_ATTRS-1, FP, ObjPtr);
         AdjObjBBox (*ObjPtr);
         return FreeBufAndReturn(line, TRUE);
      }
      else if (strcmp (obj_name, "rcbox") == 0)
      {
         ReadRCBoxObj (FP, line, ObjPtr);
         if (*ObjPtr == NULL) return FreeBufAndReturn(line, FALSE);
         ReadObjAttrs (START_HAVING_ATTRS-1, FP, ObjPtr);
         AdjObjBBox (*ObjPtr);
         return FreeBufAndReturn(line, TRUE);
      }
      else if (strcmp (obj_name, "xbm") == 0)
      {
         ReadXBmObj (FP, line, ObjPtr);
         if (*ObjPtr == NULL) return FreeBufAndReturn(line, FALSE);
         ReadObjAttrs (START_HAVING_ATTRS-1, FP, ObjPtr);
         AdjObjBBox (*ObjPtr);
         return FreeBufAndReturn(line, TRUE);
      }
      else if (strcmp (obj_name, "xpm") == 0)
      {
         ReadXPmObj (FP, line, ObjPtr);
         if (*ObjPtr == NULL) return FreeBufAndReturn(line, FALSE);
         ReadObjAttrs (START_HAVING_ATTRS-1, FP, ObjPtr);
         AdjObjBBox (*ObjPtr);
         return FreeBufAndReturn(line, TRUE);
      }
      else if (strcmp (obj_name, "group") == 0)
      {
         ReadGroupObj (FP, OBJ_GROUP, ObjPtr);
         if (*ObjPtr == NULL) return FreeBufAndReturn(line, FALSE);
         ReadObjAttrs (INVALID, FP, ObjPtr);
         AdjObjBBox (*ObjPtr);
         return FreeBufAndReturn(line, TRUE);
      }
      else if (strcmp (obj_name, "sym") == 0)
      {
         ReadGroupObj (FP, OBJ_SYM, ObjPtr);
         if (*ObjPtr == NULL) return FreeBufAndReturn(line, FALSE);
         ReadObjAttrs (INVALID, FP, ObjPtr);
         AdjObjBBox (*ObjPtr);
         return FreeBufAndReturn(line, TRUE);
      }
      else if (strcmp (obj_name, "icon") == 0)
      {
         ReadGroupObj (FP, OBJ_ICON, ObjPtr);
         if (*ObjPtr == NULL) return FreeBufAndReturn(line, FALSE);
         ReadObjAttrs (INVALID, FP, ObjPtr);
         AdjObjBBox (*ObjPtr);
         return FreeBufAndReturn(line, TRUE);
      }
      else if (strcmp (obj_name, "page") == 0)
      {
         int ok;
         char *page_name=NULL;

         *ObjPtr = NULL;
         if (importingFile && !importingIconFile) {
            if (importingPageNum == (-1) && *importingPageName == '\0') {
               if (readingPageNum == 1) {
                  /* as if read to the end of file */
                  if (!PRTGIF && !pastingFile) {
                     Msg ("Only page 1 is imported from a multipage file.");
                  }
                  return FreeBufAndReturn(line, FALSE);
               }
            } else {
               if (readingPageNum == importingPageNum) {
                  /* as if read to the end of file */
                  if (!PRTGIF && !pastingFile) {
                     sprintf(gszMsgBox, "%s %1d %s.",
                           "Only page", importingPageNum,
                           "is imported from a multipage file");
                     Msg(gszMsgBox);
                  }
                  return FreeBufAndReturn(line, FALSE);
               }
            }
         }
         ok = ReadPageObj (line, &page_name);
         if (importingFile && !importingIconFile &&
               importingPageNum == (-1) && *importingPageName != '\0' &&
               page_name != NULL && strcmp(page_name,importingPageName)==0) {
            *importingPageName = '\0';
            importingPageNum = readingPageNum;
         }
         if (page_name != NULL) free(page_name);
         return FreeBufAndReturn(line, ok);
      }
      else if (strcmp (obj_name, "state") == 0)
      {
         if ((read_state_ok=ReadState(line)) == TRUE)
            foundGoodStateObject = TRUE;
         *ObjPtr = NULL;
         return FreeBufAndReturn(line, (read_state_ok) ? TRUE : INVALID);
      }
      else if (strcmp (obj_name, "file_attr") == 0)
      {
         if (importingFile && !importingIconFile)
         {
            struct AttrRec	* saved_first_attr, * saved_last_attr;

            saved_first_attr = tgifObj->fattr;
            saved_last_attr = tgifObj->lattr;
            tgifObj->fattr = NULL;
            tgifObj->lattr = NULL;

            ReadObjAttrs (START_HAVING_ATTRS-1, FP, &tgifObj);
            DelAllAttrs (tgifObj->fattr);

            tgifObj->fattr = saved_first_attr;
            tgifObj->lattr = saved_last_attr;
         }
         else
            ReadObjAttrs (START_HAVING_ATTRS-1, FP, &tgifObj);
         return FreeBufAndReturn(line, TRUE);
      }
      else if (strcmp(obj_name, "unit") == 0)
      {
         int ok=ReadUnitObj(line);

         *ObjPtr = NULL;
         return FreeBufAndReturn(line, ok);
      }
      free(line);
   }
   return FALSE;
}

static
void ConvertToUpperCase (InStr, OutStr)
   register char	* InStr, * OutStr;
{
   for ( ; *InStr != '\0'; InStr++, OutStr++)
      *OutStr = (*InStr>='a' && *InStr<='z') ? *InStr-'a'+'A' : *InStr;
   *OutStr = '\0';
}

void ChangeDomain ()
{
   char 	domain_name[MAXPATHLENGTH+1], env_str[MAXPATHLENGTH+1];
   char 	s[MAXSTRING+1], s1[MAXSTRING+1], * c_ptr;
   char 	cap_tool_name[MAXSTRING+1];
   int		index;
   XEvent	ev;

   index = SelectDomain (domain_name);
   if (index == INVALID) return;

   XSync (mainDisplay, False);
   if (XCheckMaskEvent (mainDisplay, ExposureMask, &ev))
      ExposeEventHandler (&ev, TRUE);

   if (domainInResource)
   {
      sprintf (s, "DomainPath%1d", index);
      if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,s)) != NULL)
      {
         char	* c_ptr1;

         while (*c_ptr==' ' || *c_ptr=='\t' || *c_ptr=='\n') c_ptr++;
         if (*c_ptr != '\0' && (c_ptr1=strchr (c_ptr, ':')) != NULL)
            c_ptr = &c_ptr1[1];
         ParseSymPath (c_ptr);
      }
      else if (strcmp (domain_name, "Examples") == 0)
         ParseSymPath (TGIF_PATH);
      else
         ParseSymPath (".");
   }
   else
   {
      ConvertToUpperCase (TOOL_NAME, cap_tool_name);
      sprintf (env_str, "%s_%s", cap_tool_name, domain_name);
      if ((c_ptr = getenv (env_str)) == NULL)
      {
         if (strcmp (domain_name, "Examples") == 0)
            ParseSymPath (TGIF_PATH);
         else
            ParseSymPath (".");
      }
      else
         ParseSymPath (c_ptr);
   }

   UpdateSymInfo ();

   strcpy (curDomainName, domain_name);
   sprintf (s, "Current domain is '%s'.", curDomainName);
   sprintf (s1, "Symbol path set to '%s'.", curDomainPath);
   TwoLineMsg (s, s1);
   RedrawTitleWindow ();
}

void AdjForOldVersion (obj_ptr)
   struct ObjRec	* obj_ptr;
{
   if (fileVersion <= 13)
   {
      switch (gridSystem)
      {
         case ENGLISH_GRID:
            MoveObj (obj_ptr, (int)(HALF_INCH), (int)(HALF_INCH));
            break;
         case METRIC_GRID:
            MoveObj (obj_ptr, (int)(2.5*ONE_CM), (int)(2.5*ONE_CM));
            break;
      }
   }
}

int ImportGivenFile(file_name)
   char *file_name;
   /* returns TRUE if ok */
   /* returns FALSE if file_name looks ok, only have temporary problems */
   /* returns BAD if file_name is bad */
{
   struct ObjRec *obj_ptr, *saved_top_obj, *saved_bot_obj;
   char *rest, remote_fname[MAXPATHLENGTH+1], *remote_buf=NULL;
   char tmp_filename[MAXPATHLENGTH+1], *tmp_remote_fname=NULL, *page_spec=NULL;
   int short_name, read_status, remote_buf_sz=0;
   int tmp_linenum, file_is_remote=FALSE, interrupted;
   FILE *fp=NULL;
   XEvent ev;

   if (FileIsRemote(file_name)) {
      int rc=TRUE;

      if (!FormNewFileName(curDir, file_name, NULL, remote_fname, &page_spec)) {
         sprintf(gszMsgBox, "Invalid remote file name '%s'.", file_name);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         rc = BAD;
      } else {
         int is_html=FALSE;;

         SaveStatusStrings();
         rc = LoadRemoteFileInMem(remote_fname, &remote_buf, NULL,
               &remote_buf_sz, &is_html, TRUE);
         RestoreStatusStrings();
         if (rc && remote_buf != NULL) {
            if ((tmp_remote_fname=WriteRemoteFileIntoTemp(remote_buf,
                  remote_buf_sz, NULL)) != NULL) {
               file_is_remote = TRUE;
            } else {
               rc = FALSE;
            }
         }
      }
      if (!file_is_remote) {
         if (remote_buf != NULL) FreeRemoteBuf(remote_buf);
         return rc;
      }
   }

   XSync(mainDisplay, False);
   if (XCheckMaskEvent(mainDisplay, ExposureMask, &ev)) {
      ExposeEventHandler(&ev, TRUE);
   }
   if (file_is_remote) {
      if ((fp=fopen(tmp_remote_fname, "r")) == NULL) {
         sprintf(gszMsgBox, "Cannot read tmp file '%s'.", tmp_remote_fname);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         FreeRemoteBuf(remote_buf);
         FreeRemoteBuf(tmp_remote_fname);
         unlink(tmp_remote_fname);
         /* temporary problem */
         return FALSE;
      }
   } else {
      if ((short_name=IsPrefix(bootDir, file_name, &rest))) ++rest;
      if ((fp=fopen(file_name, "r")) == NULL) {
         if (short_name) {
            sprintf(gszMsgBox, "Cannot import '%s'.", rest);
         } else {
            sprintf(gszMsgBox, "Cannot import '%s'.", file_name);
         }
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         /* temporary problem -- may be the file does not exist yet */
         return FALSE;
      }
   }

   strcpy(tmp_filename, scanFileName);
   tmp_linenum = scanLineNum;
   if (file_is_remote) {
      strcpy(scanFileName, tmp_remote_fname);
   } else {
      strcpy(scanFileName, (short_name ? rest : file_name));
   }
   scanLineNum = 0;

   saved_top_obj = topObj;
   saved_bot_obj = botObj;
   curPage->top = curPage->bot = topObj = botObj = NULL;

   if (file_is_remote) {
      sprintf(gszMsgBox, "Importing '%s' ...", remote_fname);
   } else {
      if (short_name) {
         sprintf(gszMsgBox, "Importing '%s' ...", rest);
      } else {
         sprintf(gszMsgBox, "Importing '%s' ...", file_name);
      }
   }
   Msg(gszMsgBox);

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);

   numRedrawBBox = 0;
   readingPageNum = 0;
   ShowInterrupt(1);
   interrupted = FALSE;
   foundGoodStateObject = FALSE;
   importingPageNum = (-1);
   *importingPageName = '\0';
   if (page_spec != NULL) {
      if (*page_spec == '#') {
         importingPageNum = atoi(&page_spec[1]);
         if (importingPageNum < 1) {
            importingPageNum = (-1);
            sprintf(gszMsgBox, "Invalid page number '%s' specified.",
                  page_spec);
            Msg(gszMsgBox);
         }
      } else {
         strcpy(importingPageName, page_spec);
      }
   }
   if (importingPageNum == (-1) && *importingPageName == '\0') {
      while (!interrupted && (read_status=ReadObj(fp, &obj_ptr)) == TRUE) {
         if (obj_ptr != NULL) {
            obj_ptr->tmp_parent = NULL;
            AdjForOldVersion(obj_ptr);
            UnlockAnObj(obj_ptr);
            AddObj(NULL, topObj, obj_ptr);
            if (!interrupted &&
                  (PointInBBox(obj_ptr->x, obj_ptr->y, drawWinBBox) ||
                  BBoxIntersect(obj_ptr->bbox, drawWinBBox))) {
               if (!DrawObj(drawWindow, obj_ptr)) interrupted = TRUE;
               if (CheckInterrupt()) interrupted = TRUE;
            }
         }
      }
   } else if (importingPageNum == (-1)) {
      while (!interrupted && (read_status=ReadObj(fp, &obj_ptr)) == TRUE) {
         if (obj_ptr != NULL) {
            obj_ptr->tmp_parent = NULL;
            if (importingPageNum == (-1)) {
               FreeObj(obj_ptr);
               if (!interrupted && CheckInterrupt()) interrupted = TRUE;
            } else {
               AdjForOldVersion(obj_ptr);
               UnlockAnObj(obj_ptr);
               AddObj(NULL, topObj, obj_ptr);
               if (!interrupted &&
                     (PointInBBox(obj_ptr->x, obj_ptr->y, drawWinBBox) ||
                     BBoxIntersect(obj_ptr->bbox, drawWinBBox))) {
                  if (!DrawObj(drawWindow, obj_ptr)) interrupted = TRUE;
                  if (CheckInterrupt()) interrupted = TRUE;
               }
            }
         }
      }
   } else {
      while (!interrupted && (read_status=ReadObj(fp, &obj_ptr)) == TRUE) {
         if (obj_ptr != NULL) {
            obj_ptr->tmp_parent = NULL;
            if (importingPageNum != readingPageNum) {
               FreeObj(obj_ptr);
               if (!interrupted && CheckInterrupt()) interrupted = TRUE;
            } else {
               AdjForOldVersion(obj_ptr);
               UnlockAnObj(obj_ptr);
               AddObj(NULL, topObj, obj_ptr);
               if (!interrupted &&
                     (PointInBBox(obj_ptr->x, obj_ptr->y, drawWinBBox) ||
                     BBoxIntersect(obj_ptr->bbox, drawWinBBox))) {
                  if (!DrawObj(drawWindow, obj_ptr)) interrupted = TRUE;
                  if (CheckInterrupt()) interrupted = TRUE;
               }
            }
         }
      }
   }
   if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
      RedrawColorWindow();
   }
   if (interrupted) {
      MsgBox("User interrupt.\n\nDrawing aborted.", TOOL_NAME, INFO_MB);
   }
   HideInterrupt();

   if (fp != NULL) fclose(fp);

   strcpy(scanFileName, tmp_filename);
   scanLineNum = tmp_linenum;

   if (read_status == INVALID) {
      if (fileVersion > CUR_VERSION) {
         sprintf(gszMsgBox, "%s (=%1d) %s.\n\n%s!\n\n%s %s.  %s <URL:%s>.",
               "File version", fileVersion, "too large",
               "Import aborted", "You may need a more recent version of",
               TOOL_NAME, "Please check", homePageURL);
      } else {
         sprintf(gszMsgBox, "%s.\n\n%s!", "File corrupted", "Import aborted");
      }
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      SetDefaultCursor(mainWindow);
      ShowCursor();
      if (file_is_remote) {
         FreeRemoteBuf(remote_buf);
         FreeRemoteBuf(tmp_remote_fname);
         unlink(tmp_remote_fname);
      }
      /* temporary problem -- may be the file will be fixed later */
      return FALSE;
   }
   if (file_is_remote) {
      if (!foundGoodStateObject) PasteString(remote_buf);

      FreeRemoteBuf(remote_buf);
      FreeRemoteBuf(tmp_remote_fname);
      unlink(tmp_remote_fname);
   }

   if (topObj != NULL) SetFileModified(TRUE);
   justDupped = FALSE;

   SelAllObj(FALSE);

   if (botObj != NULL) {
      botObj->next = saved_top_obj;
   } else {
      topObj = saved_top_obj;
   }
   if (saved_top_obj != NULL) {
      saved_top_obj->prev = botObj;
      botObj = saved_bot_obj;
   }
   curPage->top = topObj;
   curPage->bot = botObj;

   if (!(file_is_remote && !foundGoodStateObject) && topSel != NULL) {
      PrepareToRecord(CMD_NEW, NULL, NULL, 0);
      RecordCmd(CMD_NEW, NULL, topSel, botSel, numObjSelected);
   }
   HighLightForward();

   if (!importFromLibrary && !file_is_remote) SetCurImportDir(file_name);

   if (file_is_remote) {
      sprintf(gszMsgBox, "'%s' imported.", remote_fname);
   } else {
      if (short_name) {
         sprintf(gszMsgBox, "'%s' imported.", rest);
      } else {
         sprintf(gszMsgBox, "'%s' imported.", file_name);
      }
   }
   Msg(gszMsgBox);
   SetDefaultCursor(mainWindow);
   ShowCursor();
   if (page_spec != NULL) free(page_spec);
   return TRUE;
}

void ImportFile()
{
   char file_name[MAXPATHLENGTH+1];

   MakeQuiescent();

   importingFile = TRUE;
   if (importFromLibrary) {
      char name[MAXSTRING+1], path[MAXSTRING+1];

      if (SelectFromLibrary("Please select an object file to IMPORT...",
            OBJ_FILE_EXT, name, path) == INVALID) {
         importingFile = FALSE;
         return;
      }
      sprintf(file_name, "%s/%s", path, name);
   } else if (SelectFileNameToImport(
         "Please select an object file to IMPORT...",
         OBJ_FILE_EXT, file_name) == INVALID) {
      importingFile = FALSE;
      return;
   }
   ImportGivenFile(file_name);
   importingFile = FALSE;
}

int LoadFile (FullName, ObjFile)
   char	* FullName;
   int	ObjFile; /* equals TRUE if the file is an OBJ file */
                 /* equals FALSE if the file is an SYM file */
                 /* equals -1 if the file is an temporary OBJ file */
{
   struct ObjRec	* obj_ptr;
   char 		file_name[MAXPATHLENGTH+1];
   char 		saved_cur_dir[MAXPATHLENGTH+1], * rest;
   int			read_status, short_name;
   FILE			* fp;
   char 		tmp_filename[MAXPATHLENGTH+1];
   int 			tmp_linenum, interrupted;
   XEvent		ev;

   if (ObjFile != FALSE)
      strcpy (saved_cur_dir, curDir);
   else
      strcpy (saved_cur_dir, curSymDir);
   strcpy (file_name, FullName);

   if ((short_name = IsPrefix (bootDir, file_name, &rest))) ++rest;
   if ((fp = fopen (file_name, "r")) == NULL)
   {
      if (short_name)
         sprintf (gszMsgBox, "Cannot open '%s'.", rest);
      else
         sprintf (gszMsgBox, "Cannot open '%s'.", file_name);
      MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
      return (FALSE);
   }
   BeforeNavigate();

   CleanUpComments ();
   if (usePaperSizeStoredInFile) ResetOnePageSize ();

   strcpy (tmp_filename, scanFileName);
   tmp_linenum = scanLineNum;
   strcpy (scanFileName, (short_name ? rest : file_name));
   scanLineNum = 0;

   TieLooseEnds ();
   CleanUpDrawingWindow ();
   SetFileModified (FALSE);

   XSync (mainDisplay, False);
   while (XCheckWindowEvent (mainDisplay, drawWindow, ExposureMask, &ev)) ;

   if (short_name)
      sprintf (gszMsgBox, "Loading '%s' ...", rest);
   else
      sprintf (gszMsgBox, "Loading '%s' ...", file_name);
   Msg (gszMsgBox);
   SetWatchCursor (drawWindow);
   SetWatchCursor (mainWindow);

   XClearWindow (mainDisplay, drawWindow);
   somethingHighLighted = FALSE;

   numRedrawBBox = 0;
   ShowInterrupt (1);
   interrupted = FALSE;
   readingPageNum = 0;
   loadedCurPageNum = 0;
   foundGoodStateObject = FALSE;
   while ((read_status=ReadObj(fp, &obj_ptr)) == TRUE)
   {
      if (obj_ptr != NULL)
      {
         obj_ptr->tmp_parent = NULL;
         AdjForOldVersion (obj_ptr);
         AddObj (NULL, topObj, obj_ptr);
         if (!interrupted && readingPageNum == loadedCurPageNum &&
               (PointInBBox (obj_ptr->x, obj_ptr->y, drawWinBBox) ||
               BBoxIntersect (obj_ptr->bbox, drawWinBBox)))
         {
            if (!DrawObj (drawWindow, obj_ptr)) interrupted = TRUE;
            if (CheckInterrupt ()) interrupted = TRUE;
         }
      }
   }
   if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
      RedrawColorWindow();
   }
   if (interrupted) {
      MsgBox("User interrupt.\n\nDrawing aborted.", TOOL_NAME, INFO_MB);
   }
   HideInterrupt();

   strcpy(scanFileName, tmp_filename);
   scanLineNum = tmp_linenum;

   if (read_status == INVALID) {
      if (fileVersion > CUR_VERSION) {
         sprintf(gszMsgBox, "%s (=%1d) %s.\n\n%s!\n\n%s %s.  %s <URL:%s>.",
               "File version", fileVersion, "too large",
               "Import aborted", "You may need a more recent version of",
               TOOL_NAME, "Please check", homePageURL);
      } else {
         sprintf(gszMsgBox, "%s.\n\n%s!", "File corrupted", "Import aborted");
      }
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      DrawPaperBoundary(drawWindow);
      RedrawGridLines(drawWindow);
      SetDefaultCursor(mainWindow);
      ShowCursor();
      return FALSE;
   }

   fclose (fp);
   if (ObjFile == TRUE)
   {
      SetCurDir (file_name);
      *curSymDir = '\0';
   }
   else if (ObjFile == FALSE)
      SetCurSymDir (file_name);

   curFileDefined = TRUE;

   if (loadedCurPageNum <= 0 || curPage == NULL)
   {
      DelAllPages ();
      loadedCurPageNum = curPageNum = lastPageNum = 1;
      InitPage ();
      DrawPaperBoundary (drawWindow);
      RedrawGridLines (drawWindow);
      RedrawPageLines (drawWindow);
      RedrawChoiceWindow ();
   }
   GotoPageNum (loadedCurPageNum);

   if (ObjFile != FALSE)
   {
      if (strcmp (saved_cur_dir, curDir) != 0 && DirInSymPath ("."))
         UpdateSymInfo ();
   }
   else
   {
      if (strcmp (saved_cur_dir, curSymDir) != 0 && DirInSymPath ("."))
         UpdateSymInfo ();
   }

   if (short_name)
      sprintf (gszMsgBox, "Current file is '%s'.", rest);
   else
      sprintf (gszMsgBox, "Current file is '%s'.", file_name);
   Msg (gszMsgBox);

   RedrawTitleWindow ();
   justDupped = FALSE;
   SetDefaultCursor (mainWindow);
   ShowCursor ();

   CleanUpCmds ();

   UpdateAllSubMenus ();

   if (foundGoodStateObject && !navigatingBackAndForth) CommitNavigate ();
   return (foundGoodStateObject);
}

void DumpPatFill (FP, Fill, CellSize, BBox, Blanks)
   FILE		* FP;
   int		Fill, CellSize;
   struct BBRec	BBox;
   char		* Blanks;
{
   int	ltx, lty, rbx, rby;

   ltx = ((BBox.ltx % CellSize) == 0) ? BBox.ltx :
         ((BBox.ltx > 0) ? ((int)(BBox.ltx / CellSize))*CellSize :
         ((int)(BBox.ltx / CellSize)-1)*CellSize);
   lty = ((BBox.lty % CellSize) == 0) ? BBox.lty :
         ((BBox.lty > 0) ? ((int)(BBox.lty / CellSize))*CellSize :
         ((int)(BBox.lty / CellSize)-1)*CellSize);
   rbx = ((BBox.rbx % CellSize) == 0) ? BBox.rbx :
         ((BBox.rbx > 0) ? ((int)(BBox.rbx / CellSize)+1)*CellSize :
         ((int)(BBox.rbx / CellSize))*CellSize);
   rby = ((BBox.rby % CellSize) == 0) ? BBox.rby :
         ((BBox.rby > 0) ? ((int)(BBox.rby / CellSize)+1)*CellSize :
         ((int)(BBox.rby / CellSize))*CellSize);

   if (fprintf (FP, "%spat%1d %1d %1d %1d %1d %1d tgifpatfill\n",
         Blanks, Fill, CellSize, ltx, lty, rbx-ltx, rby-lty) == EOF)
      writeFileFailed = TRUE;
}

void DumpSymOutline (FP, ObjPtr)
   FILE				* FP;
   register struct ObjRec	* ObjPtr;
{
   int  ltx, lty, rbx, rby;

   ltx = ObjPtr->obbox.ltx - QUARTER_INCH + 1;
   lty = ObjPtr->obbox.lty - QUARTER_INCH + 1;
   rbx = ObjPtr->obbox.rbx + QUARTER_INCH - 1;
   rby = ObjPtr->obbox.rby + QUARTER_INCH - 1;

   fprintf (FP, "gsave\n");
   fprintf (FP, "   0 setgray\n");
   fprintf (FP, "   [4 4] 0 setdash\n");
   fprintf (FP, "   newpath\n   %1d %1d moveto ", ltx, lty);
   fprintf (FP, "%1d %1d lineto ", rbx, lty);
   fprintf (FP, "%1d %1d lineto ", rbx, rby);
   fprintf (FP, "%1d %1d lineto\n", ltx, rby);
   fprintf (FP, "   closepath stroke\n");
   fprintf (FP, "grestore\n");
}

static int	printingFirstPageNum=1;
static int	printingPageNum=1, printingPageRow=1, printingPageCol=1;
static int	printingLastPageNum=0;
static int	dumpOnePageInTileMode=FALSE;
static int	dumpOnePageInStackMode=FALSE;
static int	dumpOneFilePerPage=FALSE;

static FILE	* dumpFP=NULL;
static char	tmpFile[MAXSTRING+1];
static int	llxTotal=0, llyTotal=0, urxTotal=0, uryTotal=0;
static int	totalBBoxValid=FALSE;

static int	msgAboutTiledPageScalingSeen=FALSE;

static
void DumpAttrs (FP, AttrPtr)
   FILE				* FP;
   register struct AttrRec	* AttrPtr;
{
   for ( ; AttrPtr != NULL; AttrPtr = AttrPtr->prev)
   {
      if (AttrPtr->shown)
      {
         if (!AttrPtr->nameshown && *AttrPtr->attr_name.s == '!' &&
               strcmp(AttrPtr->attr_name.s, "!PAGE_NUM=") == 0)
         {
            if (pageLayoutMode == PAGE_STACK)
            {
               struct StrRec	* s_ptr=AttrPtr->obj->detail.t->first;
               char		* c_ptr;

               if (s_ptr != NULL && (c_ptr=strstr(s_ptr->dyn_str.s,
                     "!(STACKED_PAGE_NUM)")) != NULL)
               {
                  char	saved_str[MAXSTRING+1], * c_ptr1;
                  int	len;

                  strcpy (saved_str, s_ptr->dyn_str.s);
                  sprintf (c_ptr, "%1d", printingPageNum);
                  len = s_ptr->dyn_str.sz-1;
                  c_ptr1 = &c_ptr[19];
                  c_ptr = &s_ptr->dyn_str.s[len];
                  while (*c_ptr1 != '\0') *c_ptr++ = *c_ptr1++;
                  *c_ptr = '\0';
                  DumpTextObj (FP, AttrPtr->obj);
                  DynStrSet (&s_ptr->dyn_str, saved_str);
               }
               else
                  DumpTextObj (FP, AttrPtr->obj);
            }
            else
            {
               struct StrRec	* s_ptr=AttrPtr->obj->detail.t->first;
               char		* c_ptr;

               if (s_ptr != NULL)
               {
                  char	saved_str[MAXSTRING+1], * c_ptr1, * c_ptr2;
                  int	len;

                  strcpy (saved_str, s_ptr->dyn_str.s);
                  for (c_ptr=s_ptr->dyn_str.s; *c_ptr != '\0'; c_ptr++)
                  {
                     if (*c_ptr == '!' &&
                           (strncmp (c_ptr, "!(TILED_PAGE_ROW)", 17) == 0 ||
                           strncmp (c_ptr, "!(TILED_PAGE_COL)", 17) == 0))
                     {
                        if (strncmp (c_ptr, "!(TILED_PAGE_ROW)", 17) == 0)
                           sprintf (c_ptr, "%1d", printingPageRow);
                        else
                           sprintf (c_ptr, "%1d", printingPageCol);
                        len = strlen(c_ptr);
                        c_ptr1 = &c_ptr[17];
                        c_ptr = c_ptr2 = &c_ptr[len];
                        while (*c_ptr1 != '\0') *c_ptr2++ = *c_ptr1++;
                        *c_ptr2 = '\0';
                        c_ptr--;
                     }
                  }
                  DumpTextObj (FP, AttrPtr->obj);
                  DynStrSet (&s_ptr->dyn_str, saved_str);
               }
               else
                  DumpTextObj (FP, AttrPtr->obj);
            }
         }
         else
            DumpTextObj (FP, AttrPtr->obj);
      }
   }
}

static
void DumpAnObj(FP, ObjPtr)
   FILE *FP;
   register struct ObjRec *ObjPtr;
{
   switch (ObjPtr->type) {
   case OBJ_POLY:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DumpPolyObj(FP, ObjPtr);
         DumpAttrs(FP, ObjPtr->lattr);
      }
      break;
   case OBJ_BOX:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DumpBoxObj(FP, ObjPtr);
         DumpAttrs(FP, ObjPtr->lattr);
      }
      break;
   case OBJ_OVAL:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DumpOvalObj(FP, ObjPtr);
         DumpAttrs(FP, ObjPtr->lattr);
      }
      break;
   case OBJ_TEXT:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DumpTextObj(FP, ObjPtr);
      }
      break;
   case OBJ_POLYGON:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DumpPolygonObj(FP, ObjPtr);
         DumpAttrs(FP, ObjPtr->lattr);
      }
      break;
   case OBJ_ARC:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DumpArcObj(FP, ObjPtr);
         DumpAttrs(FP, ObjPtr->lattr);
      }
      break;
   case OBJ_RCBOX:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DumpRCBoxObj(FP, ObjPtr);
         DumpAttrs(FP, ObjPtr->lattr);
      }
      break;
   case OBJ_XBM:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DumpXBmObj(FP, ObjPtr);
         DumpAttrs(FP, ObjPtr->lattr);
      }
      break;
   case OBJ_XPM:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DumpXPmObj(FP, ObjPtr);
         DumpAttrs(FP, ObjPtr->lattr);
      }
      break;
   case OBJ_SYM:
   case OBJ_ICON:
   case OBJ_GROUP:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         struct ObjRec *obj_ptr=ObjPtr->detail.r->last;

         for ( ; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
            obj_ptr->tmp_parent = ObjPtr;
            DumpAnObj(FP, obj_ptr);
         }
         DumpAttrs(FP, ObjPtr->lattr);
         if (ObjPtr->type == OBJ_SYM) DumpSymOutline(FP, ObjPtr);
      }
      break;
   }
}

static int	llxPage=0, llyPage=0, urxPage=0, uryPage=0;

#define ALL_BBOX 0
#define PAGE_BBOX 1

int DumpBBox (fp, page_only, page_bbox)
   FILE		* fp;
   int		page_only;
   struct BBRec	* page_bbox;
{
   register struct ObjRec	* obj_ptr;
   int				ltx=0, lty=0, rbx=0, rby=0, rc=TRUE;
   double			llx1=0, lly1=0, urx1=0, ury1=0;

   if ((obj_ptr = topObj) == NULL)
   {
      ltx = lty = rbx = rby = 0;
      if (!(pageLayoutMode == PAGE_STACK &&
            printingFirstPageNum != printingLastPageNum))
      {
         strcpy(gszMsgBox, "Warning:  The PostScript bounding box is empty!");
         if (PRTGIF) {
            fprintf(stderr, "%s\n", gszMsgBox);
         } else {
            Msg(gszMsgBox);
         }
      }
      rc = FALSE;
   }
   else if (pageLayoutMode == PAGE_STACK || page_bbox == NULL)
   {
      ltx = obj_ptr->bbox.ltx; lty = obj_ptr->bbox.lty;
      rbx = obj_ptr->bbox.rbx; rby = obj_ptr->bbox.rby;

      for (obj_ptr = topObj->next; obj_ptr != NULL; obj_ptr = obj_ptr->next)
      {
         if (obj_ptr->bbox.ltx < ltx) ltx = obj_ptr->bbox.ltx;
         if (obj_ptr->bbox.lty < lty) lty = obj_ptr->bbox.lty;
         if (obj_ptr->bbox.rbx > rbx) rbx = obj_ptr->bbox.rbx;
         if (obj_ptr->bbox.rby > rby) rby = obj_ptr->bbox.rby;
      }
   }
   else
   {
      int found=FALSE;

      for (obj_ptr = botObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev)
         if (Inside (obj_ptr->bbox, *page_bbox) ||
               BBoxIntersect (obj_ptr->bbox, *page_bbox))
         {
            if (found)
            {
               if (obj_ptr->bbox.ltx < ltx) ltx = obj_ptr->bbox.ltx;
               if (obj_ptr->bbox.lty < lty) lty = obj_ptr->bbox.lty;
               if (obj_ptr->bbox.rbx > rbx) rbx = obj_ptr->bbox.rbx;
               if (obj_ptr->bbox.rby > rby) rby = obj_ptr->bbox.rby;
            }
            else
            {
               found = TRUE;
               ltx = obj_ptr->bbox.ltx; lty = obj_ptr->bbox.lty;
               rbx = obj_ptr->bbox.rbx; rby = obj_ptr->bbox.rby;
            }
         }
      if (!found)
      {
         rc = FALSE;
         ltx = lty = rbx = rby = 0;
      }
      else if (page_bbox != NULL)
      {
         struct BBRec	bbox, bbox1;

         bbox.ltx = ltx; bbox.lty = lty; bbox.rbx = rbx; bbox.rby = rby;
         if (IntersectRect (bbox, *page_bbox, &bbox1))
         {
            ltx = bbox1.ltx-page_bbox->ltx; lty = bbox1.lty-page_bbox->lty;
            rbx = bbox1.rbx-page_bbox->ltx; rby = bbox1.rby-page_bbox->lty;
         }
      }
   }
   switch (pageStyle)
   {
      case PORTRAIT:
         llx1 = (double) (1.0*ltx*psDotsPerInch/PIX_PER_INCH*printMag/100 +
               psXOff[pageStyle]*psDotsPerInch);
         lly1 = (double) (-1.0*rby*psDotsPerInch/PIX_PER_INCH*printMag/100 +
               psYOff[pageStyle]*psDotsPerInch);
         urx1 = (double) (1.0*rbx*psDotsPerInch/PIX_PER_INCH*printMag/100 +
               psXOff[pageStyle]*psDotsPerInch);
         ury1 = (double) (-1.0*lty*psDotsPerInch/PIX_PER_INCH*printMag/100 +
               psYOff[pageStyle]*psDotsPerInch);
         break;
      case LANDSCAPE:
         llx1 = (double) (1.0*lty*psDotsPerInch/PIX_PER_INCH*printMag/100 -
               psYOff[pageStyle]*psDotsPerInch);
         lly1 = (double) (1.0*ltx*psDotsPerInch/PIX_PER_INCH*printMag/100 +
               psXOff[pageStyle]*psDotsPerInch);
         urx1 = (double) (1.0*rby*psDotsPerInch/PIX_PER_INCH*printMag/100 -
               psYOff[pageStyle]*psDotsPerInch);
         ury1 = (double) (1.0*rbx*psDotsPerInch/PIX_PER_INCH*printMag/100 +
               psXOff[pageStyle]*psDotsPerInch);
         break;
      default:
         fprintf (stderr, "Unrecognizable page style '%1d'\n", pageStyle);
         break;
   }

   if (rc)
   {
      llxPage = (llx1 >= 0.0) ? ((int)llx1)-1 : (-((int)(-llx1)))-1;
      llyPage = (lly1 >= 0.0) ? ((int)lly1)-1 : (-((int)(-lly1)))-1;
      urxPage = (urx1 >= 0.0) ? ((int)urx1)+1 : (-((int)(0.999-urx1)))+1;
      uryPage = (ury1 >= 0.0) ? ((int)ury1)+1 : (-((int)(0.999-ury1)))+1;
   }
   else
      llxPage = llyPage = urxPage = uryPage = 0;

   fprintf (fp,"%%%%%sBoundingBox: %1d %1d %1d %1d\n",
         (page_only ? "Page" : ""), llxPage, llyPage, urxPage, uryPage);
   return (rc);
}

void ModifyOutputFileName (FileName)
   char	* FileName;
{
   register int	i;
   int		len;
   char		s[MAXPATHLENGTH+1];

   if (*outputDir == '\0') return;

   strcpy (s, FileName);
   len = strlen (s);
   for (i = len-1; i >= 0 && s[i] != '/'; i--) ;
   if (i >= 0)
      sprintf (FileName, "%s/%s", outputDir, &s[i+1]);
   else
      sprintf (FileName, "%s/%s", outputDir, s);
}

static
void DumpTextObjInAscii (FP, ObjPtr)
   FILE				* FP;
   register struct ObjRec	* ObjPtr;
{
   struct TextRec	* text_ptr=ObjPtr->detail.t;
   struct StrRec	* s_ptr;

   if (text_ptr->pen == NONEPAT) return;
   for (s_ptr=text_ptr->first; s_ptr != NULL; s_ptr=s_ptr->next)
   {
      if (fprintf (FP, "%s\n", s_ptr->dyn_str.s) == EOF)
         writeFileFailed = TRUE;
      totalBBoxValid = TRUE;
   }
}

static
void DumpAttrsInAscii (FP, AttrPtr)
   FILE				* FP;
   register struct AttrRec	* AttrPtr;
{
   for ( ; AttrPtr != NULL; AttrPtr = AttrPtr->prev)
   {
      if (AttrPtr->shown)
      {
         if (!AttrPtr->nameshown && *AttrPtr->attr_name.s == '!' &&
               strcmp(AttrPtr->attr_name.s, "!PAGE_NUM=") == 0)
         {
            if (pageLayoutMode == PAGE_STACK)
            {
               struct StrRec	* s_ptr=AttrPtr->obj->detail.t->first;
               char		* c_ptr;

               if (s_ptr != NULL && (c_ptr=strstr(s_ptr->dyn_str.s,
                     "!(STACKED_PAGE_NUM)")) != NULL)
               {
                  char	saved_str[MAXSTRING+1], * c_ptr1;
                  int	len;

                  strcpy (saved_str, s_ptr->dyn_str.s);
                  sprintf (c_ptr, "%1d", printingPageNum);
                  len = s_ptr->dyn_str.sz-1;
                  c_ptr1 = &c_ptr[19];
                  c_ptr = &s_ptr->dyn_str.s[len];
                  while (*c_ptr1 != '\0') *c_ptr++ = *c_ptr1++;
                  *c_ptr = '\0';
                  DumpTextObjInAscii (FP, AttrPtr->obj);
                  DynStrSet (&s_ptr->dyn_str, saved_str);
               }
               else
                  DumpTextObjInAscii (FP, AttrPtr->obj);
            }
            else
            {
               struct StrRec	* s_ptr=AttrPtr->obj->detail.t->first;
               char		* c_ptr;

               if (s_ptr != NULL)
               {
                  char	saved_str[MAXSTRING+1], * c_ptr1, * c_ptr2;
                  int	len;

                  strcpy(saved_str, s_ptr->dyn_str.s);
                  for (c_ptr=s_ptr->dyn_str.s; *c_ptr != '\0'; c_ptr++)
                  {
                     if (*c_ptr == '!' &&
                           (strncmp(c_ptr, "!(TILED_PAGE_ROW)", 17) == 0 ||
                           strncmp(c_ptr, "!(TILED_PAGE_COL)", 17) == 0))
                     {
                        if (strncmp(c_ptr, "!(TILED_PAGE_ROW)", 17) == 0)
                           sprintf(c_ptr, "%1d", printingPageRow);
                        else
                           sprintf(c_ptr, "%1d", printingPageCol);
                        len = strlen(c_ptr);
                        c_ptr1 = &c_ptr[17];
                        c_ptr = c_ptr2 = &c_ptr[len];
                        while (*c_ptr1 != '\0') *c_ptr2++ = *c_ptr1++;
                        *c_ptr2 = '\0';
                        c_ptr--;
                     }
                  }
                  DumpTextObjInAscii (FP, AttrPtr->obj);
                  DynStrSet (&s_ptr->dyn_str, saved_str);
               }
               else
                  DumpTextObjInAscii (FP, AttrPtr->obj);
            }
         }
         else
            DumpTextObjInAscii (FP, AttrPtr->obj);
      }
   }
}

static
void DumpAnObjInAscii(FP, ObjPtr)
   FILE *FP;
   register struct ObjRec *ObjPtr;
{
   switch (ObjPtr->type) {
   case OBJ_POLY:
   case OBJ_BOX:
   case OBJ_OVAL:
   case OBJ_POLYGON:
   case OBJ_ARC:
   case OBJ_RCBOX:
   case OBJ_XBM:
   case OBJ_XPM:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DumpAttrsInAscii(FP, ObjPtr->lattr);
      }
      break;
   case OBJ_TEXT:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         DumpTextObjInAscii(FP, ObjPtr);
      }
      break;
   case OBJ_SYM:
   case OBJ_ICON:
   case OBJ_GROUP:
      if (!colorLayers ||
            ObjPtr->tmp_parent!=NULL || ObjInVisibleLayer(ObjPtr)) {
         struct ObjRec *obj_ptr=ObjPtr->detail.r->last;

         for ( ; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
            obj_ptr->tmp_parent = ObjPtr;
            DumpAnObjInAscii(FP, obj_ptr);
         }
         DumpAttrsInAscii(FP, ObjPtr->lattr);
      }
      break;
   }
}

#define WRITEBYTE(fp,byte) fputc((byte),(fp))

static
void WriteWord(fp, word)
   FILE	* fp;
   unsigned short word;
{
   WRITEBYTE(fp, word&0xff);
   WRITEBYTE(fp, ((unsigned short)(word&0xff00))>>8);
}

static
void WriteDoubleWord(fp, dword)
   FILE	* fp;
   unsigned long dword;
{
   WRITEBYTE(fp, dword&0xff);
   WRITEBYTE(fp, (dword&0xff00)>>8);
   WRITEBYTE(fp, (dword&0xff0000)>>16);
   WRITEBYTE(fp, (dword&0xff000000)>>24);
}

static char	psBopHook[MAXSTRING], psEopHook[MAXSTRING];
static int	psBopHookStatus=INVALID, psEopHookStatus=INVALID;
static int	minimalEPS=INVALID;
static int	colorBgInPrintingColorPS=INVALID;
static int	generateTiffEPSI=INVALID;
static char	xbmToTiffCmd[MAXSTRING+1];
static char	epsiExportExtension[MAXSTRING];
static int	overrideEPSIExportExtension=INVALID;
static int	numberFileInPrintOnePage=INVALID;

void SetBopHook(buf)
   char *buf;
{
   strcpy(psBopHook, buf);
   psBopHookStatus = TRUE;
}

void SetEopHook(buf)
   char *buf;
{
   strcpy(psEopHook, buf);
   psEopHookStatus = TRUE;
}

static
void GenDumpInitDefaults ()
{
   char	* c_ptr;

   if (PRTGIF)
      minimalEPS = TRUE;
   else if (minimalEPS == INVALID)
   {
      minimalEPS = TRUE;
      if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"MinimalEPS")) != NULL &&
            (strcmp (c_ptr, "false") == 0 || strcmp (c_ptr, "False") == 0))
         minimalEPS = FALSE;
   }
   if (colorBgInPrintingColorPS == INVALID)
   {
      colorBgInPrintingColorPS = FALSE;
      if (!PRTGIF)
      {
         if ((c_ptr=XGetDefault (mainDisplay, TOOL_NAME,
               "ColorBgInPrintingColorPS")) != NULL &&
               (strcmp (c_ptr, "True") == 0 || strcmp (c_ptr, "true") == 0))
            colorBgInPrintingColorPS = TRUE;
      }
   }
   if (psBopHookStatus == INVALID)
   {
      *psBopHook = '\0';
      if (!PRTGIF)
      {
         if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"PSBopHook")) != NULL)
         {
            strcpy (psBopHook, c_ptr);
            psBopHookStatus = TRUE;
         }
         else
            psBopHookStatus = FALSE;
      }
   }
   if (psEopHookStatus == INVALID)
   {
      *psEopHook = '\0';
      if (!PRTGIF)
      {
         if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"PSEopHook")) != NULL)
         {
            strcpy (psEopHook, c_ptr);
            psEopHookStatus = TRUE;
         }
         else
            psEopHookStatus = FALSE;
      }
   }
   if (generateTiffEPSI == INVALID)
   {
      generateTiffEPSI = FALSE;
      if (!PRTGIF)
      {
         if ((c_ptr=XGetDefault (mainDisplay, TOOL_NAME, "TiffEPSI")) != NULL &&
               (strcmp (c_ptr, "True") == 0 || strcmp (c_ptr, "true") == 0))
            generateTiffEPSI = TRUE;

         if ((c_ptr=XGetDefault (mainDisplay, TOOL_NAME, "XbmToTiff")) != NULL)
         {
            int	count=0;

            strcpy(xbmToTiffCmd, c_ptr);
            for (c_ptr=strstr(xbmToTiffCmd,"%s"); c_ptr!=NULL;
                  c_ptr=strstr(++c_ptr,"%s"))
               count++;
            if (count != 1 && count != 2)
            {
               sprintf (gszMsgBox, "Invalid %s*%s: %s resource.\n\n'%s' used.",
                     TOOL_NAME, "XbmToTiff", xbmToTiffCmd,
                     "xbmtopbm %s | pnmtotiff -none > %s");
               MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
               strcpy (xbmToTiffCmd, "xbmtopbm %s | pnmtotiff -none > %s");
            }
         }
         else
            strcpy (xbmToTiffCmd, "xbmtopbm %s | pnmtotiff -none > %s");
      }
   }
   if (overrideEPSIExportExtension == INVALID)
   {
      *epsiExportExtension = '\0';
      overrideEPSIExportExtension = FALSE;
      if (!PRTGIF)
      {
         if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"EPSIExportExtension")) !=
               NULL)
         {
            strcpy (epsiExportExtension, c_ptr);
            if (strchr (epsiExportExtension, '.') != NULL ||
                  strchr (epsiExportExtension, ' ') != NULL)
            {
               fprintf (stderr, "Invalid %s*EPSIExportExtension: '%s'.\n",
                     TOOL_NAME, epsiExportExtension);
               *epsiExportExtension = '\0';
            }
         }
      }
   }
   if (numberFileInPrintOnePage == INVALID)
   {
      numberFileInPrintOnePage = FALSE;
      if (PRTGIF)
      {
         if (cmdLineOneFilePerPage)
            numberFileInPrintOnePage = TRUE;
      }
      else
      {
         if ((c_ptr=XGetDefault (mainDisplay, TOOL_NAME,
               "NumberFileInPrintOnePage")) != NULL &&
               (strcmp (c_ptr, "True") == 0 || strcmp (c_ptr, "true") == 0))
            numberFileInPrintOnePage = TRUE;
      }
   }
}

static
void GenTiffFile (tmp_fname, epsi_fname)
   char	* tmp_fname, * epsi_fname;
{
   register int	i;
   char		tiff_fname[MAXPATHLENGTH+1], xbm_fname[MAXPATHLENGTH+1];
   char		cmd[MAXPATHLENGTH+1], buf[1024], * c_ptr;
   int		bytes_read, len, count, tiff_fd, epsi_fd;
   FILE		* tmp_fp, * epsi_fp, * pfp;
   struct stat	stat_buf;
   long		tmp_bytes, tiff_bytes;

   sprintf (xbm_fname, "%s/%s", curDir, curFileName);
   len = strlen (xbm_fname);
   for (i = len-1; xbm_fname[i] != '.'; i--) ;
   sprintf (&xbm_fname[i], ".%s", XBM_FILE_EXT);
   ModifyOutputFileName (xbm_fname);

   sprintf (tiff_fname, "%s/%s", curDir, curFileName);
   len = strlen (tiff_fname);
   for (i = len-1; tiff_fname[i] != '.'; i--) ;
   sprintf (&tiff_fname[i], ".tif");
   ModifyOutputFileName (tiff_fname);

   count = 0;
   for (c_ptr=strstr(xbmToTiffCmd,"%s"); c_ptr!=NULL;
         c_ptr=strstr(++c_ptr,"%s"))
      count++;
   if (count == 1)
      sprintf (cmd, xbmToTiffCmd, xbm_fname);
   else
      sprintf (cmd, xbmToTiffCmd, xbm_fname, tiff_fname);
   unlink (tiff_fname);

   sprintf(gszMsgBox, "Executing '%s'...", cmd);
   SetStringStatus(gszMsgBox);
   XSync(mainDisplay, False);
   if ((pfp=(FILE*)popen(cmd, "r")) == NULL) {
      sprintf(gszMsgBox, "Fail to execute '%s'.\n\nEPSI file not generated!",
            cmd);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   while (fgets (buf, sizeof(buf), pfp) != NULL)
   {
      Msg (buf);
      sleep (1);
   }
   pclose (pfp);
   SetStringStatus("...Done");

   stat (tmp_fname, &stat_buf);
   tmp_bytes = (long)stat_buf.st_size;
   stat (tiff_fname, &stat_buf);
   tiff_bytes = (long)stat_buf.st_size;

   if ((tmp_fp=fopen(tmp_fname, "r")) == NULL)
   {
      sprintf (gszMsgBox, "Cannot open '%s' for read.", tmp_fname);
      MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if ((tiff_fd=open(tiff_fname, O_RDONLY)) == (-1))
   {
      sprintf (gszMsgBox, "Cannot open '%s' for read.", tiff_fname);
      MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
      fclose (tmp_fp);
      return;
   }
   if ((epsi_fp=fopen(epsi_fname, "w")) == NULL)
   {
      sprintf (gszMsgBox, "Cannot open '%s' for read.", tiff_fname);
      MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
      fclose (tmp_fp);
      close (tiff_fd);
      unlink (tiff_fname);
      return;
   }
   WRITEBYTE(epsi_fp, 0xc5);
   WRITEBYTE(epsi_fp, 0xd0);
   WRITEBYTE(epsi_fp, 0xd3);
   WRITEBYTE(epsi_fp, 0xc6);
   WriteDoubleWord(epsi_fp, 0x0000001e);
   WriteDoubleWord(epsi_fp, tmp_bytes);
   WriteDoubleWord(epsi_fp, 0L);
   WriteDoubleWord(epsi_fp, 0L);
   WriteDoubleWord(epsi_fp, tmp_bytes+0x1e);
   WriteDoubleWord(epsi_fp, tiff_bytes);
   WriteWord(epsi_fp, 0xffff);

   while (!writeFileFailed && fgets(buf, sizeof(buf), tmp_fp) != NULL)
      if (fputs (buf, epsi_fp) == EOF)
         writeFileFailed = TRUE;

   fclose (tmp_fp);
   fclose (epsi_fp);
   if (writeFileFailed)
   {
      writeFileFailed = FALSE;
      sprintf (gszMsgBox, "Fail to write to '%s'.\n\nFile system may be full.",
            epsi_fname);
      MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
      close (tiff_fd);
      unlink (tiff_fname);
      return;
   }
   if ((epsi_fd=open(epsi_fname, O_WRONLY|O_APPEND)) == (-1))
   {
      sprintf (gszMsgBox, "Cannot open '%s' for append.", epsi_fname);
      MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
      close (tiff_fd);
      unlink (tiff_fname);
      return;
   }
   while ((bytes_read=read(tiff_fd, buf, sizeof(buf))) > 0) {
      if (write(epsi_fd, buf, bytes_read) <= 0) {
         writeFileFailed = TRUE;
         break;
      }
   }
   if (writeFileFailed) {
      writeFileFailed = FALSE;
      sprintf(gszMsgBox, "Fail to write to '%s'.\n\nFile system may be full.",
            epsi_fname);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   }
   close (epsi_fd);
   close (tiff_fd);
   fclose (epsi_fp);
   unlink (tiff_fname);
}

static int psA4PaperSize=FALSE;

static
void InitNeedA4()
{
   static int nInitialized=FALSE;

   if (nInitialized) return;
   nInitialized = TRUE;
   if (cmdLineA4) {
      psA4PaperSize = TRUE;
   } else if (!PRTGIF) {
      char *c_ptr;

      if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"PSA4PaperSize")) != NULL &&
            UtilStrICmp(c_ptr, "true") == 0) {
         psA4PaperSize = TRUE;
      }
   }
}

static
int NeedA4()
{
   int width_to_match, height_to_match;

   InitNeedA4();
   if (!psA4PaperSize) return FALSE;
   width_to_match = (int)(825*PIX_PER_INCH/printMag);
   height_to_match = (int)(117*PIX_PER_INCH*10/printMag);
   return (onePageWidth == width_to_match && onePageHeight == height_to_match);
}

static
int DoGenDump(FileName)
   char *FileName;
{
   register struct ObjRec *obj_ptr;
   char cmd[MAXSTRING+1], tmp_str[MAXSTRING+1], ps_file[MAXSTRING+1];
   char *rest, loc_time[MAXSTRING+1];
   int i, len, short_name = FALSE;
   time_t tloc;
   struct DocFontRec *next_df;

   if (!curDirIsLocal) {
      MsgBox("Cannot print a remote file!", TOOL_NAME, INFO_MB);
      return FALSE;
   }
   GenDumpInitDefaults();

   if (botObj==NULL && ((pageLayoutMode==PAGE_STACK &&
         dumpOnePageInStackMode) || pageLayoutMode==PAGE_TILE)) {
      sprintf(gszMsgBox, "No objects to print.");
      if (PRTGIF) {
         fprintf(stderr, "%s\n", gszMsgBox);
      } else {
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      }
      return FALSE;
   }
   switch (whereToPrint) {
   case PRINTER: break;

   case LATEX_FIG:
   case PS_FILE:
   case XBM_FILE:
   case TEXT_FILE:
   case EPSI_FILE:
   case GIF_FILE:
   case HTML_FILE:
      if (!PRTGIF && !curFileDefined) {
         switch (whereToPrint) {
         case LATEX_FIG:
            sprintf(gszMsgBox, "%s.\n\n%s!", "No current file",
                  "Cannot generate LaTeX (EPS) output");
            break;
         case PS_FILE:
            sprintf(gszMsgBox, "%s.\n\n%s!", "No current file",
                  "Cannot generate PostScript output");
            break;
         case XBM_FILE:
            sprintf(gszMsgBox, "%s.\n\nCannot generate X11 %s output!",
                  "No current file", (colorDump ? "pixmap" : "bitmap"));
            break;
         case TEXT_FILE:
            sprintf(gszMsgBox, "%s.\n\n%s!", "No current file",
                  "Cannot generate ASCII text output");
            break;
         case EPSI_FILE:
            sprintf(gszMsgBox, "%s.\n\n%s!", "No current file",
                  "Cannot generate EPSI output");
            break;
         case GIF_FILE:
            sprintf(gszMsgBox, "%s.\n\n%s!", "No current file",
                  "Cannot generate GIF/ISMAP output");
            break;
         case HTML_FILE:
            sprintf(gszMsgBox, "%s.\n\n%s!", "No current file",
                  "Cannot generate HTML output");
            break;
         }
         if (PRTGIF) {
            fprintf(stderr, "%s\n", gszMsgBox);
         } else {
            MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         }
         return FALSE;
      }
      break;
   default:
      sprintf(gszMsgBox, "Don't know where to print this!");
      if (PRTGIF) {
         fprintf(stderr, "%s\n", gszMsgBox);
      } else {
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      }
      return FALSE;
   }
   if (!PRTGIF && printingPageNum==printingFirstPageNum) {
      Msg("Generating print file ...");
   }
   if (whereToPrint == XBM_FILE || whereToPrint == GIF_FILE ||
         whereToPrint == HTML_FILE) {
      if (topObj == NULL) {
         sprintf(gszMsgBox, "No objects to print.");
         if (PRTGIF) {
            fprintf(stderr, "%s\n", gszMsgBox);
         } else {
            MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         }
      } else {
         int saved_colordump;

         SetWatchCursor(drawWindow);
         SetWatchCursor(mainWindow);
         saved_colordump = colorDump;
         if (whereToPrint == GIF_FILE || whereToPrint == HTML_FILE) {
            colorDump = TRUE;
         }
         DumpXBitmapFile();
         colorDump = saved_colordump;
         SetDefaultCursor(mainWindow);
         ShowCursor();
      }
      return TRUE;
   }

   if (printingPageNum == printingFirstPageNum) {
      sprintf(tmpFile, "%sTgifXXXXXX", TMP_DIR);
      mktemp(tmpFile);
      unlink(tmpFile);

      if ((dumpFP=fopen(tmpFile, "w")) == NULL) {
         sprintf(tmp_str, "Cannot create '%s', print aborted.", tmpFile);
         if (PRTGIF) {
            fprintf(stderr, "%s\n", tmp_str);
         } else {
            Msg(tmp_str);
         }
         return FALSE;
      }

      if (PRTGIF) fprintf(stderr, "Writing to '%s' ...\n", tmpFile);

      writeFileFailed = FALSE;
      ResetGrayDetection();
   }
   if (printingPageNum == printingFirstPageNum && whereToPrint != TEXT_FILE) {
      if (usePsAdobeString) {
         switch (whereToPrint) {
         case PRINTER:
         case PS_FILE:
            if (*adobeString == '\0') {
               if (fprintf(dumpFP, "%%!PS-Adobe-2.0\n") == EOF) {
                  writeFileFailed = TRUE;
               }
            } else if (fprintf(dumpFP,"%%!PS-Adobe-%s\n",adobeString) == EOF) {
               writeFileFailed = TRUE;
            }
            break;
         case LATEX_FIG:
         case EPSI_FILE:
            if (*adobeString == '\0') {
               if (fprintf(dumpFP, "%%!PS-Adobe-2.0 EPSF-1.2\n") == EOF) {
                  writeFileFailed = TRUE;
               }
            } else if (*epsfString == '\0') {
               if (fprintf(dumpFP, "%%!PS-Adobe-%s\n", adobeString) == EOF) {
                  writeFileFailed = TRUE;
               }
            } else {
               if (fprintf(dumpFP, "%%!PS-Adobe-%s EPSF-%s\n",
                     adobeString, epsfString) == EOF) {
                  writeFileFailed = TRUE;
               }
            }
            break;
         }
      } else {
         if (preDumpSetup) PSUsePSAdobe();
         if (fprintf(dumpFP, "%%!\n") == EOF) {
            writeFileFailed = TRUE;
         }
      }
      if (pageLayoutMode == PAGE_STACK &&
            printingFirstPageNum == printingLastPageNum) {
         DumpBBox(dumpFP, ALL_BBOX, NULL);
      } else if (fprintf(dumpFP,"%%%%BoundingBox: (atend)\n") == EOF) {
         writeFileFailed = TRUE;
      }
      if (PRTGIF) {
         if (fprintf(dumpFP, "%%%%Title: %s\n", FileName) == EOF) {
            writeFileFailed = TRUE;
         }
      } else if (curFileDefined) {
         strcpy(tmp_str, curFileName);
         len = strlen(tmp_str);
         for (i = len-1; tmp_str[i] != '.'; i--) ;
         tmp_str[i] = '\0';
         if (fprintf(dumpFP, "%%%%Title: %s\n", tmp_str) == EOF) {
            writeFileFailed = TRUE;
         }
      } else if (fprintf(dumpFP, "%%%%Title: [Unnamed]\n") == EOF) {
         writeFileFailed = TRUE;
      }
      time(&tloc);
      strcpy(loc_time, ctime(&tloc));
      loc_time[24] = '\0';
      if (fprintf(dumpFP, "%%%%CreationDate: %s\n", loc_time) == EOF) {
         writeFileFailed = TRUE;
      }
      if (TGIF_PATCHLEVEL == 0) {
         if (fprintf(dumpFP, "%%%%Creator: %s-%s by %s\n",
               TOOL_NAME, versionString,
               "William Chia-Wei Cheng (william@cs.UCLA.edu)") == EOF) {
            writeFileFailed = TRUE;
         }
      } else {
         if (fprintf(dumpFP, "%%%%Creator: %s-%s-p%1d by %s\n",
               TOOL_NAME, versionString, TGIF_PATCHLEVEL,
               "William Chia-Wei Cheng (william@cs.UCLA.edu)") == EOF) {
            writeFileFailed = TRUE;
         }
      }
      if (whereToPrint != LATEX_FIG && whereToPrint != EPSI_FILE) {
         if (fprintf(dumpFP, "%%%%Orientation: %s\n",
               (pageStyle==PORTRAIT ? "Portrait" : "Landscape")) == EOF) {
            writeFileFailed = TRUE;
         }
      }
      if ((whereToPrint != LATEX_FIG && whereToPrint != EPSI_FILE) ||
            !minimalEPS) {
         if (fprintf(dumpFP, "%%%%Pages: %1d\n", 
               pageLayoutMode == PAGE_STACK ? lastPageNum :
               paperCol * paperRow) == EOF) {
            writeFileFailed = TRUE;
         }
         if (fprintf(dumpFP, "%%%%DocumentFonts: (atend)\n") == EOF) {
            writeFileFailed = TRUE;
         }
         if (NeedA4()) {
            if (fprintf(dumpFP, "%%%%DocumentPaperSizes: a4\n") == EOF) {
               writeFileFailed = TRUE;
            }
         }
         if (fprintf(dumpFP, "%%%%EndComments\n") == EOF) {
            writeFileFailed = TRUE;
         }
      }
      if (!PRTGIF && whereToPrint == EPSI_FILE && !generateTiffEPSI) {
         SaveStatusStrings();
         SetStringStatus("Generating preview bitmap...");
         GenPreviewBitmap(dumpFP, llxPage, llyPage, urxPage, uryPage);
         RestoreStatusStrings();
      }

      if ((whereToPrint != LATEX_FIG && whereToPrint != EPSI_FILE) ||
            !minimalEPS) {
         if (fprintf(dumpFP, "%%%%BeginProlog\n") == EOF) {
            writeFileFailed = TRUE;
         }
      }

      DumpPSMacro(dumpFP);

      PrepareEightBitFontInfo();
      DumpEightBitFontInfo(dumpFP);

      if (fprintf(dumpFP, "end\n\n") == EOF) writeFileFailed = TRUE;
      if ((whereToPrint != LATEX_FIG && whereToPrint != EPSI_FILE) ||
            !minimalEPS) {
         if (fprintf(dumpFP, "%%%%EndProlog\n") == EOF) writeFileFailed = TRUE;
         if (NeedA4()) {
            if (fprintf(dumpFP, "%%%%BeginSetup\n") == EOF ||
                  fprintf(dumpFP, "%%%%PaperSize: a4\n") == EOF ||
                  fprintf(dumpFP, "%%%%BeginPaperSize: a4\n") == EOF ||
                  fprintf(dumpFP, "a4\n") == EOF ||
                  fprintf(dumpFP, "%%%%EndPaperSize\n") == EOF ||
                  fprintf(dumpFP, "%%%%EndSetup\n") == EOF) {
               writeFileFailed = TRUE;
            }
         }
      }
   }
   if (pageLayoutMode == PAGE_STACK || (pageLayoutMode == PAGE_TILE &&
         ((paperCol == 1 && paperRow == 1) ||
         ((whereToPrint == LATEX_FIG || whereToPrint == EPSI_FILE) &&
         !dumpOnePageInTileMode)))) {
      printingPageRow = printingPageCol = 1;
      if (whereToPrint == TEXT_FILE) {
         if (printingPageNum != printingFirstPageNum) {
            fprintf(dumpFP, "\014\n");
         }
         for (obj_ptr = botObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
            obj_ptr->tmp_parent = NULL;
            DumpAnObjInAscii(dumpFP, obj_ptr);
         }
      } else {
         if ((whereToPrint != LATEX_FIG && whereToPrint != EPSI_FILE) ||
               !minimalEPS) {
            fprintf(dumpFP, "%%%%Page: %1d %1d\n\n",
                  printingPageNum-printingFirstPageNum+1,
                  printingPageNum-printingFirstPageNum+1);
         }
         DumpBBox(dumpFP, PAGE_BBOX, NULL);
         if (totalBBoxValid) {
            if (topObj != NULL) {
               if (llxPage < llxTotal) llxTotal = llxPage;
               if (llyPage < llyTotal) llyTotal = llyPage;
               if (urxPage > urxTotal) urxTotal = urxPage;
               if (uryPage > uryTotal) uryTotal = uryPage;
            }
         } else if (topObj != NULL) {
            totalBBoxValid = TRUE;
            llxTotal = llxPage; llyTotal = llyPage;
            urxTotal = urxPage; uryTotal = uryPage;
         }
         if (psBopHookStatus == TRUE && (whereToPrint == PRINTER ||
               whereToPrint == PS_FILE)) {
            fprintf(dumpFP, "userdict /%s known { %s } if\n\n",
                  psBopHook, psBopHook);
         }
         fprintf(dumpFP, "tgifdict begin\n");
         fprintf(dumpFP, "/tgifsavedpage save def\n\n");
         fprintf(dumpFP, "1 setmiterlimit\n");
         fprintf(dumpFP, "1 setlinewidth\n\n");
         fprintf(dumpFP, "0 setgray\n\n");

         if (pageStyle == LANDSCAPE) fprintf(dumpFP, "90 rotate\n");

         fprintf(dumpFP, "%1d %s mul %1d %s mul translate\n", psDotsPerInch,
               psXOffStr[pageStyle], psDotsPerInch, psYOffStr[pageStyle]);

         fprintf(dumpFP, "%1d %1d div %1d mul 100 div dup neg scale\n\n",
               psDotsPerInch, PIX_PER_INCH, printMag);

         fprintf(dumpFP, "gsave\n\n");

         if (needsTgifOrigCTM) {
            fprintf(dumpFP, "/tgiforigctm matrix currentmatrix def\n\n");
         }
         if (colorDump && colorBgInPrintingColorPS &&
               (whereToPrint == PS_FILE || whereToPrint == PRINTER)) {
            fprintf(dumpFP, "%% Background\n");
            if (myBgColor.red==0 && myBgColor.green==0 && myBgColor.blue==0) {
               fprintf(dumpFP, "0 setgray\n");
            } else if (myBgColor.red==maxRGB && myBgColor.green==maxRGB &&
                  myBgColor.blue==maxRGB) {
               fprintf(dumpFP, "1 setgray\n");
            } else {
               fprintf(dumpFP, "%.3f %.3f %.3f setrgbcolor\n",
                     ((float)myBgColor.red/maxRGB),
                     ((float)myBgColor.green/maxRGB),
                     ((float)myBgColor.blue/maxRGB));
            }
            fprintf(dumpFP, "newpath\n");
            fprintf(dumpFP,
                  "   0 0 moveto 0 %d lineto %d %d lineto %d 0 lineto\n",
                  onePageHeight, onePageWidth, onePageHeight, onePageWidth);
            fprintf(dumpFP, "closepath fill\n\n");
         }

         for (obj_ptr = botObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
            obj_ptr->tmp_parent = NULL;
            DumpAnObj(dumpFP, obj_ptr);
         }
         fprintf(dumpFP, "grestore\n");
         fprintf(dumpFP, "tgifsavedpage restore\n");
         fprintf(dumpFP, "end\n");

         switch (whereToPrint) {
         case PRINTER:
         case PS_FILE:
            if (psEopHookStatus == TRUE) {
               fprintf(dumpFP,"userdict /%s known { %s } if\n",
                     psEopHook, psEopHook);
            }
            fprintf(dumpFP, "showpage\n\n");
            break;
         case EPSI_FILE:
         case LATEX_FIG:
            if (showPageInEPS) {
               fprintf(dumpFP, "showpage\n\n");
            }
            break;
         }
      }
   } else if (whereToPrint == TEXT_FILE) {
      for (obj_ptr = botObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
         obj_ptr->tmp_parent = NULL;
         DumpAnObjInAscii(dumpFP, obj_ptr);
      }
   } else {
      int row, col;
      float one=(float)atof("1.0"), f_rbx, f_rby, x_dist, y_dist;

      if (tiledPageScaling == one && !msgAboutTiledPageScalingSeen) {
         char	msg1[MAXSTRING+1];

         msgAboutTiledPageScalingSeen = TRUE;
         sprintf(gszMsgBox, "%s %s*TiledPageScaling is set to 1.",
               "Output may get truncated because", TOOL_NAME);
         sprintf(msg1, "    Reducing the above setting is recommended.");
         if (PRTGIF) {
            fprintf(stderr, "%s\n%s\n", gszMsgBox, msg1);
         } else {
            TwoLineMsg(gszMsgBox, msg1);
         }
      }
      f_rbx = (float)(((float)PIX_PER_INCH) * 100.0 / ((float)printMag) *
            psPageWidthInInch[pageStyle]);
      f_rby = (float)(((float)PIX_PER_INCH) * 100.0 / ((float)printMag) *
            psPageHeightInInch[pageStyle]);
      x_dist = (float)(((float)PIX_PER_INCH) * psPageWidthInInch[pageStyle] *
            (1.0-tiledPageScaling) * 50.0 / ((float)printMag));
      y_dist = (float)(((float)PIX_PER_INCH) * psPageHeightInInch[pageStyle] *
            (1.0-tiledPageScaling) * 50.0 / ((float)printMag));

      for (row = 0; row < paperRow; row++) {
         printingPageRow = row+1;
         for (col = 0; col < paperCol; col++) {
            struct BBRec page_bbox;
            float real_ltx, real_lty;

            if (dumpOnePageInTileMode &&
                  row*paperCol+col+1 != printingPageNum) {
               continue;
            }
            printingPageCol = col+1;
            page_bbox.ltx = col * onePageWidth;
            page_bbox.lty = row * onePageHeight;
            page_bbox.rbx = page_bbox.ltx + onePageWidth;
            page_bbox.rby = page_bbox.lty + onePageHeight;

            if ((whereToPrint != LATEX_FIG && whereToPrint != EPSI_FILE) ||
                  !minimalEPS) {
               if (dumpOnePageInTileMode) {
                  fprintf(dumpFP, "%%%%Page: 1 1\n\n");
               } else {
                  fprintf(dumpFP, "%%%%Page: %1d %1d\n\n",
                        col+1+row*paperCol, col+1+row*paperCol);
               }
            }
            if (DumpBBox(dumpFP, PAGE_BBOX, &page_bbox)) {
               if (totalBBoxValid) {
                  if (topObj != NULL) {
                     if (llxPage < llxTotal) llxTotal = llxPage;
                     if (llyPage < llyTotal) llyTotal = llyPage;
                     if (urxPage > urxTotal) urxTotal = urxPage;
                     if (uryPage > uryTotal) uryTotal = uryPage;
                  }
               } else if (topObj != NULL) {
                  totalBBoxValid = TRUE;
                  llxTotal = llxPage; llyTotal = llyPage;
                  urxTotal = urxPage; uryTotal = uryPage;
               }
            }
            if (psBopHookStatus == TRUE && (whereToPrint == PRINTER ||
                  whereToPrint == PS_FILE)) {
               fprintf(dumpFP, "userdict /%s known { %s } if\n\n",
                     psBopHook, psBopHook);
            }
            fprintf(dumpFP, "tgifdict begin\n");
            fprintf(dumpFP, "/tgifsavedpage save def\n\n");
            fprintf(dumpFP, "1 setmiterlimit\n");
            fprintf(dumpFP, "1 setlinewidth\n\n");
            fprintf(dumpFP, "0 setgray\n\n");

            if (pageStyle == LANDSCAPE) fprintf(dumpFP, "90 rotate\n");

            fprintf(dumpFP, "%1d %s mul %1d %s mul translate\n",
                  psDotsPerInch, psXOffStr[pageStyle], psDotsPerInch,
                  psYOffStr[pageStyle]);

            fprintf(dumpFP, "%1d %1d div %1d mul 100 div dup neg scale\n\n",
                  psDotsPerInch, PIX_PER_INCH, printMag);

            fprintf(dumpFP, "gsave\n\n");

            fprintf(dumpFP, "%% SETUP CLIP FOR PAGE\n\n");
            fprintf(dumpFP, "%.3f %.3f translate\n", x_dist, y_dist);
            fprintf(dumpFP, "%.3f %.3f scale\n", tiledPageScaling,
                  tiledPageScaling);

            fprintf(dumpFP, "-%1d -%1d translate\n", page_bbox.ltx,
                  page_bbox.lty);
            real_ltx = (float)(((float)col) *
                  ((float)psPageWidthInInch[pageStyle]) *
                  ((float)PIX_PER_INCH) * 100.0 / ((float)printMag));
            real_lty = (float)(((float)row) *
                  ((float)psPageHeightInInch[pageStyle]) *
                  ((float)PIX_PER_INCH) * 100.0 / ((float)printMag));
            fprintf(dumpFP, "newpath\n");
            fprintf(dumpFP, "   %.3f %.3f moveto\n", real_ltx, real_lty);
            fprintf(dumpFP, "   %.3f 0 rlineto\n", f_rbx);
            fprintf(dumpFP, "   0 %.3f rlineto\n", f_rby);
            fprintf(dumpFP, "   -%.3f 0 rlineto\n", f_rbx);
            fprintf(dumpFP, "closepath clip newpath\n\n");

            if (needsTgifOrigCTM) {
               fprintf(dumpFP, "/tgiforigctm matrix currentmatrix def\n\n");
            }
            if (colorDump && colorBgInPrintingColorPS &&
                  (whereToPrint == PS_FILE || whereToPrint == PRINTER)) {
               fprintf(dumpFP, "%% Background\n");
               if (myBgColor.red==0 && myBgColor.green==0 &&
                     myBgColor.blue==0) {
                  fprintf(dumpFP, "0 setgray\n");
               } else if (myBgColor.red==maxRGB && myBgColor.green==maxRGB &&
                     myBgColor.blue==maxRGB) {
                  fprintf(dumpFP, "1 setgray\n");
               } else {
                  fprintf(dumpFP, "%.3f %.3f %.3f setrgbcolor\n",
                        ((float)myBgColor.red/maxRGB),
                        ((float)myBgColor.green/maxRGB),
                        ((float)myBgColor.blue/maxRGB));
               }
               fprintf(dumpFP, "newpath\n");
               fprintf(dumpFP, "   %.3f %.3f moveto\n", real_ltx, real_lty);
               fprintf(dumpFP, "   %.3f 0 rlineto\n", f_rbx);
               fprintf(dumpFP, "   0 %.3f rlineto\n", f_rby);
               fprintf(dumpFP, "   -%.3f 0 rlineto\n", f_rbx);
               fprintf(dumpFP, "closepath fill newpath\n\n");
            }
            for (obj_ptr = botObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
               if (Inside(obj_ptr->bbox, page_bbox) ||
                     BBoxIntersect(obj_ptr->bbox, page_bbox)) {
                  obj_ptr->tmp_parent = NULL;
                  DumpAnObj(dumpFP, obj_ptr);
               }
            }
            fprintf(dumpFP, "grestore\n");
            fprintf(dumpFP, "tgifsavedpage restore\n");
            fprintf(dumpFP, "end\n");

            switch (whereToPrint) {
            case PRINTER:
            case PS_FILE:
               if (psEopHookStatus == TRUE) {
                  fprintf(dumpFP, "userdict /%s known { %s } if\n",
                        psEopHook, psEopHook);
               }
               fprintf(dumpFP, "showpage\n\n");
               break;
            case EPSI_FILE:
            case LATEX_FIG:
               if (showPageInEPS) {
                  fprintf(dumpFP, "showpage\n\n");
               }
               break;
            }
         }
      }
   }
   if (printingPageNum != printingLastPageNum) return TRUE;

   if (whereToPrint != TEXT_FILE) {
      if ((whereToPrint != LATEX_FIG && whereToPrint != EPSI_FILE) ||
            !minimalEPS) {
         if (fprintf(dumpFP, "%%%%Trailer\n") == EOF) {
            writeFileFailed = TRUE;
         }
      }
      if (pageLayoutMode!=PAGE_STACK ||
            printingFirstPageNum!=printingLastPageNum) {
         if (fprintf(dumpFP,"%%%%BoundingBox: %1d %1d %1d %1d\n",
               llxTotal, llyTotal, urxTotal, uryTotal) == EOF) {
            writeFileFailed = TRUE;
         }
      }
      if (fprintf(dumpFP, "%%MatchingCreationDate: %s\n", loc_time) == EOF) {
         writeFileFailed = TRUE;
      }
      if ((whereToPrint != LATEX_FIG && whereToPrint != EPSI_FILE) ||
            !minimalEPS) {
         if (fprintf(dumpFP, "%%%%DocumentFonts: ") == EOF) {
            writeFileFailed = TRUE;
         }
         for ( ; firstDocFont != NULL; firstDocFont = next_df) {
            next_df = firstDocFont->next;
            if (fprintf(dumpFP, "%s", firstDocFont->name) == EOF) {
               writeFileFailed = TRUE;
            }
            if (next_df != NULL) {
               if (fprintf(dumpFP, "\n%%%%+ ") == EOF) {
                  writeFileFailed = TRUE;
               }
            }
            free(firstDocFont);
         }
         if (fprintf(dumpFP, "\n") == EOF) writeFileFailed = TRUE;
         if (fprintf(dumpFP, "%%%%EOF\n") == EOF) writeFileFailed = TRUE;
      }
   }
   fclose(dumpFP);

   EndGrayDetection();

   if (!totalBBoxValid) {
      sprintf(gszMsgBox, "No objects to print.");
      if (PRTGIF) {
         fprintf(stderr, "%s\n", gszMsgBox);
      } else {
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      }
      return FALSE;
   }
   if (writeFileFailed) {
      writeFileFailed = FALSE;
      sprintf(gszMsgBox, "Fail to write to '%s'.\n\nFile system may be full.",
            tmpFile);
      if (PRTGIF) {
         fprintf(stderr, "%s\n", gszMsgBox);
      } else {
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      }
#ifdef KEEP_WHEN_PRINT
      if (whereToPrint != PRINTER) unlink(tmpFile);
#else
      unlink(tmpFile);
#endif
      return FALSE;
   }

   switch (whereToPrint) {
   case PRINTER:
#ifdef VMS
#define PRINT_TRAILER ""
#else
#define PRINT_TRAILER " 2>&1"
#endif
      if (preDumpSetup) break;
      if (PRTGIF) {
         if (strstr(printCommand, "%s") == NULL) {
            if (lastFile) {
               sprintf(cmd,"%s %s%s",printCommand,tmpFile,PRINT_TRAILER);
            } else {
               sprintf(cmd,"%s -h %s%s",printCommand,tmpFile,PRINT_TRAILER);
            }
         } else {
            sprintf(cmd, printCommand, tmpFile);
            strcat(cmd, PRINT_TRAILER);
         }
         fprintf(stderr, "%s\n", cmd);
      } else {
         if (strstr(printCommand, "%s") == NULL) {
            sprintf(cmd,"%s %s%s",printCommand,tmpFile,PRINT_TRAILER);
            sprintf(gszMsgBox, "Printing with '%s' command.", printCommand);
         } else {
            sprintf(cmd, printCommand, tmpFile);
            strcat(cmd, PRINT_TRAILER);
            sprintf(gszMsgBox, "Printing with '%s' command.", printCommand);
         }
         Msg(gszMsgBox);
      }
      if (!ExecuteCmd(cmd)) {
         if (PRTGIF) {
            fprintf(stderr, "Cannot execute '%s', print aborted.\n", cmd);
         } else {
            sprintf(gszMsgBox, "Cannot execute '%s', print aborted.", cmd);
            Msg(gszMsgBox);
         }
         unlink(tmpFile);
         return FALSE;
      }
      if (PRTGIF) {
         fprintf(stderr, "'%s' printed.\n\n", tmpFile);
      } else {
         Msg("Print completed.");
      }
      break;
   case EPSI_FILE:
   case LATEX_FIG:
      if (preDumpSetup) break;
      if (PRTGIF) {
         if (cmdLineOneFilePerPage) {
            sprintf(ps_file, "%s-%1d.%s", FileName, cmdLinePageNum,
                  EPSF_FILE_EXT);
         } else {
            sprintf(ps_file, "%s.%s", FileName, EPSF_FILE_EXT);
         }
         ModifyOutputFileName(ps_file);
      } else {
         if (!curFileDefined) {
            MsgBox("No current file.\n\nCannot generate LaTeX output!",
                  TOOL_NAME, INFO_MB);
            unlink(tmpFile);
            return FALSE;
         }
         sprintf(ps_file, "%s/%s", curDir, curFileName);
         len = strlen(ps_file);
         for (i = len-1; ps_file[i] != '.'; i--) ;
         if (dumpOneFilePerPage ||
               (dumpOnePageInStackMode && numberFileInPrintOnePage)) {
            if (whereToPrint != EPSI_FILE || *epsiExportExtension == '\0') {
               sprintf(&ps_file[i], "-%1d.%s", curPageNum, EPSF_FILE_EXT);
            } else {
               sprintf(&ps_file[i], "-%1d.%s", curPageNum,
                     epsiExportExtension);
            }
         } else {
            if (whereToPrint != EPSI_FILE || *epsiExportExtension == '\0') {
               sprintf(&ps_file[i], ".%s", EPSF_FILE_EXT);
            } else {
               sprintf(&ps_file[i], ".%s", epsiExportExtension);
            }
         }
         ModifyOutputFileName(ps_file);
         if ((short_name=IsPrefix(bootDir, ps_file, &rest))) ++rest;
         if (short_name && *outputDir=='\0') {
            sprintf(cmd, "Printing into '%s' ...", rest);
         } else {
            sprintf(cmd, "Printing into '%s' ...", ps_file);
         }
         Msg(cmd);
      }
      if (!PRTGIF && whereToPrint == EPSI_FILE && generateTiffEPSI) {
         int saved_colordump=colorDump;

         SaveStatusStrings();
         SetStringStatus("Generating TIFF preview bitmap...");

         colorDump = FALSE;
         DumpXBitmapFile();
         colorDump = saved_colordump;

         GenTiffFile(tmpFile, ps_file);
         RestoreStatusStrings();
         unlink(tmpFile);
      } else {
         if (!CopyAFile(tmpFile, ps_file)) {
            if (PRTGIF) {
               fprintf(stderr, "LaTeX output not generated.\n");
            } else {
               Msg("LaTeX output not generated.");
            }
            unlink(tmpFile);
            return FALSE;
         }
      }
      if (PSFILE_MOD != 0 && chmod (ps_file, PSFILE_MOD)) {
         if (PRTGIF) {
            fprintf(stderr, "Cannot chmod '%s' to 0%1o.\n", ps_file,
                  PSFILE_MOD);
         } else {
            if (short_name && *outputDir=='\0') {
               sprintf(gszMsgBox, "Cannot chmod '%s' to 0%1o.", rest,
                     PSFILE_MOD);
            } else {
               sprintf(gszMsgBox, "Cannot chmod '%s' to 0%1o.", ps_file,
                     PSFILE_MOD);
            }
            Msg(gszMsgBox);
         }
      }
      if (PRTGIF) {
         fprintf(stderr, "LaTeX figure printed into '%s'.\n\n", ps_file);
      } else {
         if (short_name && *outputDir=='\0') {
            sprintf(gszMsgBox, "LaTeX figure printed into '%s'.", rest);
         } else {
            sprintf(gszMsgBox, "LaTeX figure printed into '%s'.", ps_file);
         }
         Msg(gszMsgBox);
      }
      break;
   case PS_FILE:
      if (preDumpSetup) break;
      if (PRTGIF) {
         if (cmdLineOneFilePerPage) {
            sprintf(ps_file, "%s-%1d.%s", FileName, cmdLinePageNum,
                  PS_FILE_EXT);
         } else {
            sprintf(ps_file, "%s.%s", FileName, PS_FILE_EXT);
         }
         ModifyOutputFileName(ps_file);
      } else {
         if (!curFileDefined) {
            sprintf(gszMsgBox, "%s.\n\n%s!",
                  "No current file", "Cannot generate PostScript output");
            MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
            unlink(tmpFile);
            return FALSE;
         }
         sprintf(ps_file, "%s/%s", curDir, curFileName);
         len = strlen(ps_file);
         for (i = len-1; ps_file[i] != '.'; i--) ;
         if (dumpOneFilePerPage ||
               (dumpOnePageInStackMode && numberFileInPrintOnePage)) {
            sprintf(&ps_file[i], "-%1d.%s", curPageNum, PS_FILE_EXT);
         } else {
            sprintf(&ps_file[i], ".%s", PS_FILE_EXT);
         }
         ModifyOutputFileName(ps_file);
         if ((short_name=IsPrefix(bootDir, ps_file, &rest))) ++rest;
         if (short_name && *outputDir=='\0') {
            sprintf(gszMsgBox, "Printing into '%s' ...", rest);
         } else {
            sprintf(gszMsgBox, "Printing into '%s' ...", ps_file);
         }
         Msg(gszMsgBox);
      }
      if (!CopyAFile(tmpFile, ps_file)) {
         if (PRTGIF) {
            fprintf(stderr, "PostScript output not generated.\n");
         } else {
            Msg("PostScript output not generated.");
         }
         unlink(tmpFile);
         return FALSE;
      }
      if (PSFILE_MOD != 0 && chmod(ps_file, PSFILE_MOD)) {
         if (PRTGIF) {
            fprintf(stderr, "Cannot chmod '%s' to 0%1o.\n", ps_file,
                  PSFILE_MOD);
         } else {
            if (short_name && *outputDir=='\0') {
               sprintf(gszMsgBox, "Cannot chmod '%s' to 0%1o.", rest,
                     PSFILE_MOD);
            } else {
               sprintf(gszMsgBox, "Cannot chmod '%s' to 0%1o.", ps_file,
                     PSFILE_MOD);
            }
            Msg(gszMsgBox);
         }
      }
      if (PRTGIF) {
         fprintf(stderr, "PostScript file printed into '%s'.\n\n", ps_file);
      } else {
         if (short_name && *outputDir=='\0') {
            sprintf(gszMsgBox, "PostScript file printed into '%s'.", rest);
         } else {
            sprintf(gszMsgBox, "PostScript file printed into '%s'.", ps_file);
         }
         Msg(gszMsgBox);
      }
      break;
   case TEXT_FILE:
      if (preDumpSetup) break;
      if (PRTGIF) {
         if (cmdLineOneFilePerPage ||
               (dumpOnePageInStackMode && numberFileInPrintOnePage)) {
            sprintf(ps_file, "%s-%1d.%s", FileName, cmdLinePageNum,
                  TEXT_FILE_EXT);
         } else {
            sprintf(ps_file, "%s.%s", FileName, TEXT_FILE_EXT);
         }
         ModifyOutputFileName(ps_file);
      } else {
         if (!curFileDefined) {
            sprintf(gszMsgBox, "%s.\n\n%s!",
                  "No current file", "Cannot generate ASCII text output");
            MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
            unlink(tmpFile);
            return FALSE;
         }
         sprintf(ps_file, "%s/%s", curDir, curFileName);
         len = strlen(ps_file);
         for (i = len-1; ps_file[i] != '.'; i--) ;
         if (dumpOneFilePerPage ||
               (dumpOnePageInStackMode && numberFileInPrintOnePage)) {
            sprintf(&ps_file[i], "-%1d.%s", curPageNum, TEXT_FILE_EXT);
         } else {
            sprintf(&ps_file[i], ".%s", TEXT_FILE_EXT);
         }
         ModifyOutputFileName(ps_file);
         if ((short_name=IsPrefix(bootDir, ps_file, &rest))) ++rest;
         if (short_name && *outputDir=='\0') {
            sprintf(gszMsgBox, "Printing into '%s' ...", rest);
         } else {
            sprintf(gszMsgBox, "Printing into '%s' ...", ps_file);
         }
         Msg(gszMsgBox);
      }
      if (!CopyAFile(tmpFile, ps_file)) {
         if (PRTGIF) {
            fprintf(stderr, "ASCII text output not generated.\n");
         } else {
            Msg("ASCII text output not generated.");
         }
         unlink(tmpFile);
         return FALSE;
      }
      if (PSFILE_MOD != 0 && chmod (ps_file, PSFILE_MOD)) {
         if (PRTGIF) {
            fprintf(stderr, "Cannot chmod '%s' to 0%1o.\n", ps_file,
                  PSFILE_MOD);
         } else {
            if (short_name && *outputDir=='\0') {
               sprintf(gszMsgBox, "Cannot chmod '%s' to 0%1o.", rest,
                     PSFILE_MOD);
            } else {
               sprintf(gszMsgBox, "Cannot chmod '%s' to 0%1o.", ps_file,
                     PSFILE_MOD);
            }
            Msg(gszMsgBox);
         }
      }
      if (PRTGIF) {
         fprintf(stderr, "ASCII text file printed into '%s'.\n\n", ps_file);
      } else {
         if (short_name && *outputDir=='\0') {
            sprintf(gszMsgBox, "ASCII text file printed into '%s'.", rest);
         } else {
            sprintf(gszMsgBox, "ASCII text file printed into '%s'.", ps_file);
         }
         Msg(gszMsgBox);
      }
      break;
   }
#ifdef KEEP_WHEN_PRINT
   if (preDumpSetup || whereToPrint != PRINTER) {
      unlink(tmpFile);
   }
#else
   unlink(tmpFile);
#endif
   return TRUE;
}

static
int GenDump(FileName)
   char *FileName;
{
   int rc;

   SaveStatusStrings();
   rc = DoGenDump(FileName);
   RestoreStatusStrings();
   return rc;
}

void Dump(FileName)
   char *FileName;
{
   int len, obj_ext_len, sym_ext_len, ok=TRUE;
   char obj_ext_str[MAXSTRING+1], sym_ext_str[MAXSTRING+1];
   char dummy_str[MAXSTRING+1];
   struct PageRec *saved_cur_page;

   sprintf(obj_ext_str, ".%s", OBJ_FILE_EXT);
   obj_ext_len = strlen(obj_ext_str);
   sprintf(sym_ext_str, ".%s", SYM_FILE_EXT);
   sym_ext_len = strlen(sym_ext_str);

   if ((whereToPrint == LATEX_FIG || whereToPrint == EPSI_FILE) &&
         pageLayoutMode == PAGE_TILE && (paperCol != 1 || paperRow != 1)) {
      strcpy(dummy_str, "Cannot print in EPS format in Tiled page mode.");
      if (PRTGIF) {
         fprintf(stderr, "%s\n", dummy_str);
      } else {
         MsgBox("Cannot print in EPS format in Tiled page mode.",
               TOOL_NAME, INFO_MB);
      }
      return;
   }
   if (PRTGIF) {
      len = strlen(FileName);
      if ((len >= obj_ext_len &&
            strcmp(&FileName[len-obj_ext_len], obj_ext_str) == 0) ||
            (len >= sym_ext_len &&
            strcmp(&FileName[len-sym_ext_len], sym_ext_str) == 0)) {
         FileName[len-obj_ext_len] = '\0';
      }
      if (cmdLineOneFilePerPage ||
            (cmdLineHasPageNum && pageLayoutMode==PAGE_STACK &&
            (whereToPrint==PS_FILE || whereToPrint==PRINTER))) {
         totalBBoxValid = FALSE;
         printingFirstPageNum = printingPageNum = printingLastPageNum =
               curPageNum;
         dumpOnePageInStackMode = TRUE;
      }
   } else {
      HighLightReverse();
      SetWatchCursor(drawWindow);
      SetWatchCursor(mainWindow);
   }
   totalBBoxValid = FALSE;
   if (whereToPrint == PRINTER || whereToPrint == PS_FILE ||
         whereToPrint == TEXT_FILE) {
      if (PRTGIF && (cmdLineOneFilePerPage || (cmdLineHasPageNum &&
            pageLayoutMode==PAGE_STACK && (whereToPrint==PS_FILE ||
            whereToPrint==PRINTER)))) {
         if (whereToPrint==PRINTER || whereToPrint==LATEX_FIG ||
               whereToPrint==PS_FILE || whereToPrint==EPSI_FILE) {
            ResetPSInfo();

            ok = GenDump(FileName);

            DoneResetPSInfo();
         }
         if (ok) GenDump(FileName);
      } else {
         XColor *saved_tgif_colors=tgifColors;

         if (printUsingRequestedColor) tgifColors = tgifRequestedColors;

         if (whereToPrint==PRINTER || whereToPrint==LATEX_FIG ||
               whereToPrint==PS_FILE || whereToPrint==EPSI_FILE) {
            ResetPSInfo();

            printingFirstPageNum = 1;
            printingPageNum = 1;
            printingLastPageNum = lastPageNum;
            saved_cur_page = curPage;
            SaveStatusStrings();
            for (curPage=firstPage; ok && curPage != NULL;
                  curPage=curPage->next, printingPageNum++) {
               topObj = curPage->top;
               botObj = curPage->bot;
               sprintf(dummy_str, "Preprocess page %1d of %1d...",
                     printingPageNum, lastPageNum);
               SetStringStatus(dummy_str);
               ok = GenDump(FileName);
            }
            RestoreStatusStrings();
            curPage = saved_cur_page;
            topObj = curPage->top;
            botObj = curPage->bot;

            DoneResetPSInfo();
         }
         printingFirstPageNum = 1;
         printingPageNum = 1;
         printingLastPageNum = lastPageNum;
         saved_cur_page = curPage;
         SaveStatusStrings();
         for (curPage=firstPage; ok && curPage != NULL;
               curPage=curPage->next, printingPageNum++) {
            topObj = curPage->top;
            botObj = curPage->bot;
            sprintf(dummy_str, "Generating page %1d of %1d...",
                  printingPageNum, lastPageNum);
            SetStringStatus(dummy_str);
            ok = GenDump(FileName);
         }
         RestoreStatusStrings();
         curPage = saved_cur_page;
         topObj = curPage->top;
         botObj = curPage->bot;

         if (printUsingRequestedColor) tgifColors = saved_tgif_colors;
      }
   } else {
      XColor *saved_tgif_colors=tgifColors;

      if (printUsingRequestedColor) tgifColors = tgifRequestedColors;

      if (whereToPrint==PRINTER || whereToPrint==LATEX_FIG ||
            whereToPrint==PS_FILE || whereToPrint==EPSI_FILE) {
         ResetPSInfo();

         printingFirstPageNum = 1;
         printingPageNum = 1;
         printingLastPageNum = 1;
         ok = GenDump(FileName);

         DoneResetPSInfo();
      }
      printingFirstPageNum = 1;
      printingPageNum = 1;
      printingLastPageNum = 1;
      if (ok) GenDump(FileName);

      if (printUsingRequestedColor) tgifColors = saved_tgif_colors;
   }
   if (PRTGIF) {
      if (cmdLineOneFilePerPage || (cmdLineHasPageNum &&
            pageLayoutMode==PAGE_STACK && (whereToPrint==PS_FILE ||
            whereToPrint==PRINTER))) {
         dumpOnePageInStackMode = FALSE;
      }
   } else {
      SetDefaultCursor(mainWindow);
      ShowCursor();
      HighLightForward();
   }
}

void DumpOnePageInTileMode (row, col)
   int	row, col;
{
   int ok=TRUE;
   XColor *saved_tgif_colors=tgifColors;

   if (printUsingRequestedColor) tgifColors = tgifRequestedColors;

   SetWatchCursor (drawWindow);
   SetWatchCursor (mainWindow);

   totalBBoxValid = FALSE;
   printingFirstPageNum = printingPageNum = printingLastPageNum =
         row*paperCol+col+1;
   dumpOnePageInTileMode = TRUE;
   if (whereToPrint==PRINTER || whereToPrint==LATEX_FIG ||
         whereToPrint==PS_FILE || whereToPrint==EPSI_FILE) {
      ResetPSInfo();
      ok = GenDump ("");
      DoneResetPSInfo();
   }
   if (ok) GenDump ("");
   dumpOnePageInTileMode = FALSE;

   SetDefaultCursor (mainWindow);
   ShowCursor ();

   if (printUsingRequestedColor) tgifColors = saved_tgif_colors;
}

void DumpOnePageInStackMode ()
{
   int ok=TRUE;
   XColor *saved_tgif_colors=tgifColors;

   if (printUsingRequestedColor) tgifColors = tgifRequestedColors;

   SetWatchCursor (drawWindow);
   SetWatchCursor (mainWindow);

   totalBBoxValid = FALSE;
   printingFirstPageNum = printingPageNum = printingLastPageNum = curPageNum;
   dumpOnePageInStackMode = TRUE;
   if (whereToPrint==PRINTER || whereToPrint==LATEX_FIG ||
         whereToPrint==PS_FILE || whereToPrint==EPSI_FILE) {
      ResetPSInfo();
      ok = GenDump ("");
      DoneResetPSInfo();
   }
   if (ok) GenDump ("");
   dumpOnePageInStackMode = FALSE;

   SetDefaultCursor (mainWindow);
   ShowCursor ();

   if (printUsingRequestedColor) tgifColors = saved_tgif_colors;
}

void DumpOneFilePerPage()
{
   int ok=TRUE, saved_cur_page_num=curPageNum;
   XColor *saved_tgif_colors=tgifColors;

   if (printUsingRequestedColor) tgifColors = tgifRequestedColors;

   SetWatchCursor (drawWindow);
   SetWatchCursor (mainWindow);

   if (lastPageNum != 1) dumpOneFilePerPage = TRUE;
   dumpOnePageInStackMode = TRUE;
   for (curPageNum=1; ok && curPageNum <= lastPageNum; curPageNum++) {
      totalBBoxValid = FALSE;
      GotoPageNum(curPageNum);
      printingFirstPageNum = printingPageNum = printingLastPageNum = curPageNum;
      ResetPSInfo();
      ok = GenDump ("");
      DoneResetPSInfo();
      if (ok) GenDump ("");
   }
   dumpOnePageInStackMode = FALSE;
   dumpOneFilePerPage = FALSE;

   SetDefaultCursor (mainWindow);
   ShowCursor ();

   if (printUsingRequestedColor) tgifColors = saved_tgif_colors;

   if (!ok && curPageNum <= lastPageNum) {
      if (curPageNum == lastPageNum) {
         sprintf(gszMsgBox, "Page %1d has not been generated.", lastPageNum);
      } else {
         sprintf(gszMsgBox, "Pages %1d through %1d have not been generated.",
               curPageNum, lastPageNum);
      }
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   }
   if (saved_cur_page_num != curPageNum) GotoPageNum(saved_cur_page_num);
}

void PrintWithCommand (FileName)
   char	* FileName;
{
   struct PageRec	* saved_cur_page;
   int ok=TRUE;
   XColor *saved_tgif_colors=tgifColors;

   if (whereToPrint != PRINTER)
   {
      Msg ("PrintWithCmd only works when output device is the printer.");
      return;
   }
   Dialog ("Please enter print command name:",
         "( <CR>: accept, <ESC>: cancel )", printCommand);
   if (*printCommand == '\0') return;

   if (printUsingRequestedColor) tgifColors = tgifRequestedColors;

   SetWatchCursor (drawWindow);
   SetWatchCursor (mainWindow);

   if (whereToPrint==PRINTER || whereToPrint==LATEX_FIG ||
         whereToPrint==PS_FILE || whereToPrint==EPSI_FILE) {
      ResetPSInfo();

      printingPageNum = 1;
      printingLastPageNum = lastPageNum;
      saved_cur_page = curPage;
      totalBBoxValid = FALSE;
      SaveStatusStrings ();
      for (curPage = firstPage; ok && curPage != NULL; curPage = curPage->next,
            printingPageNum++)
      {
         char	dummy_str[MAXSTRING+1];

         topObj = curPage->top;
         botObj = curPage->bot;
         sprintf (dummy_str, "Preprocess page %1d of %1d...", printingPageNum,
               lastPageNum);
         SetStringStatus (dummy_str);
         ok = GenDump (FileName);
      }
      RestoreStatusStrings ();
      curPage = saved_cur_page;
      topObj = curPage->top;
      botObj = curPage->bot;

      DoneResetPSInfo();
   }
   printingPageNum = 1;
   printingLastPageNum = lastPageNum;
   saved_cur_page = curPage;
   totalBBoxValid = FALSE;
   SaveStatusStrings ();
   for (curPage = firstPage; ok && curPage != NULL; curPage = curPage->next,
         printingPageNum++)
   {
      char	dummy_str[MAXSTRING+1];

      topObj = curPage->top;
      botObj = curPage->bot;
      sprintf (dummy_str, "Generating page %1d of %1d...", printingPageNum,
            lastPageNum);
      SetStringStatus (dummy_str);
      ok = GenDump (FileName);
   }
   RestoreStatusStrings ();
   curPage = saved_cur_page;
   topObj = curPage->top;
   botObj = curPage->bot;

   SetDefaultCursor (mainWindow);
   ShowCursor ();

   if (printUsingRequestedColor) tgifColors = saved_tgif_colors;
}

void PrintSelectedObjs()
{
   struct SelRec *top_sel_ptr, *bot_sel_ptr, *sel_ptr, *next_sel;
   struct ObjRec *saved_top_obj, *saved_bot_obj, *obj_ptr;

   if (topSel == NULL) {
      MsgBox("No objects selected!\n\nNothing printed!", TOOL_NAME, INFO_MB);
      return;
   }
   HighLightReverse();
   PushPageInfo();
   saved_top_obj = topObj;
   saved_bot_obj = botObj;

   JustDupSelObj(&top_sel_ptr, &bot_sel_ptr);
   curPage->top = topObj = top_sel_ptr->obj;
   curPage->bot = botObj = bot_sel_ptr->obj;
   for (sel_ptr=topSel, obj_ptr=topObj; obj_ptr!=NULL;
         sel_ptr=sel_ptr->next, obj_ptr=obj_ptr->next) {
      CopyObjId(sel_ptr->obj, obj_ptr);
      CopyObjLocks(sel_ptr->obj, obj_ptr);
   }

   if (pageLayoutMode == PAGE_STACK) {
      DumpOnePageInStackMode();
   } else {
      int ok=TRUE;
      XColor *saved_tgif_colors=tgifColors;

      if (printUsingRequestedColor) tgifColors = tgifRequestedColors;

      printingFirstPageNum = 1;
      printingPageNum = 1;
      printingLastPageNum = 1;
      totalBBoxValid = FALSE;
      if (whereToPrint==PRINTER || whereToPrint==LATEX_FIG ||
            whereToPrint==PS_FILE || whereToPrint==EPSI_FILE) {
         ResetPSInfo();
         ok = GenDump("");
         DoneResetPSInfo();
      }
      if (ok) GenDump("");

      if (printUsingRequestedColor) tgifColors = saved_tgif_colors;
   }
   DelAllObj();
   for (sel_ptr = top_sel_ptr; sel_ptr != NULL; sel_ptr = next_sel) {
      next_sel = sel_ptr->next;
      free(sel_ptr);
   }
   PopPageInfo();
   curPage->top = topObj = saved_top_obj;
   curPage->bot = botObj = saved_bot_obj;
   RedrawAnArea(botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward();
}

void SetPrintReduction ()
{
   int value;
   char buf[MAXSTRING+1];

   Dialog("Please specify percent reduction (<100) or enlargement (>100):",
         "( <CR>: accept, <ESC>: cancel )", buf);
   if (*buf == '\0') return;

   value = atoi(buf);
   if (value <= 0) {
      sprintf(gszMsgBox, "Invalid reduction '%s'.", buf);
      Msg(gszMsgBox);
      return;
   }
   printMag = value;
   if (UpdPageStyle(pageStyle)) {
      UpdDrawWinBBox();
      AdjSplineVs();
      ClearAndRedrawDrawWindow();
   }
   RedrawScrollBars();
   RedrawRulers();
   RedrawTitleWindow();
   SetFileModified(TRUE);
   if (printMag <= 100) {
      sprintf(gszMsgBox, "New reduction is %1d%%.", printMag);
   } else {
      sprintf(gszMsgBox, "New enlargement is %1d%%.", printMag);
   }
   Msg(gszMsgBox);
}

void NewProc ()
{
   while (!DirIsRemote(curDir) && fileModified)
   {
      switch (MsgBox ("File modified, save file before clear? [ync](y)",
            TOOL_NAME, YNC_MB))
      {
         case MB_ID_YES: SaveFile (); break;
         case MB_ID_NO: TieLooseEnds (); SetFileModified (FALSE); break;
         case MB_ID_CANCEL: return;
      }
   }
   if (inHyperSpace) ToggleHyperSpace (FALSE);

   CleanUpComments ();
   CleanUpCmds ();
   CleanUpDrawingWindow ();
   ClearFileInfo ();
   ClearAndRedrawDrawWindow ();
   Msg ("Editing no file.");
   objId = 0;
   RedrawTitleWindow ();
   DelAllPages ();
   lastPageNum = 1;
   InitPage ();
   ShowPage ();
}

void OpenProc ()
{
   char	file_name[MAXPATHLENGTH+1];
   int	do_not_save=FALSE, need_to_check_auto_exec=FALSE;

   while (!DirIsRemote(curDir) && fileModified)
   {
      switch (MsgBox ("File modified, save file before open? [ync](y)",
            TOOL_NAME, YNC_MB))
      {
         case MB_ID_YES: SaveFile (); break;
         case MB_ID_NO: do_not_save = TRUE; SetFileModified (FALSE); break;
         case MB_ID_CANCEL: return;
      }
   }
   if (SelectFileName ("Please select a file to OPEN ...",file_name) != INVALID)
   {
      SetWatchCursor (drawWindow);
      SetWatchCursor (mainWindow);

      if (FileIsRemote (file_name))
      {
         char remote_fname[MAXPATHLENGTH+1];
         char *page_spec=NULL;

         if (!FormNewFileName (curDir, file_name, NULL, remote_fname,
               &page_spec))
         {
            sprintf(gszMsgBox, "Invalid remote file name '%s'.", file_name);
            MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
            if (do_not_save) SetFileModified (TRUE);
         }
         else
         {
            char	* buf=NULL, * content_type=NULL;
            int		rc, buf_sz=0, is_html=FALSE;

            SaveStatusStrings ();
            rc = LoadRemoteFileInMem(remote_fname, &buf, &content_type,
                  &buf_sz, &is_html, TRUE);
            RestoreStatusStrings ();
            if (rc && buf != NULL) {
               if (LoadRemoteFileFromMem(remote_fname, buf, content_type,
                     buf_sz, is_html)) {
                  need_to_check_auto_exec = TRUE;
               }
            } else if (do_not_save) {
               SetFileModified (TRUE);
            }
            if (content_type != NULL) FreeRemoteBuf (content_type);
            if (buf != NULL) FreeRemoteBuf (buf);
            if (page_spec != NULL) {
               int new_page_num=(-1);

               need_to_check_auto_exec = FALSE;
               if (!GetPageNumFromPageSpec(page_spec, &new_page_num)) {
                  sprintf(gszMsgBox, "Invalid page specified for '%s'.",
                        file_name);
                  MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
               } else if (new_page_num != curPageNum) {
                  GotoPageNum(new_page_num);
                  ShowPage();
                  ClearAndRedrawDrawWindow();
                  RedrawTitleWindow();
                  RedrawRulers();
                  RedrawScrollBars();
                  justDupped = FALSE;
               }
            }
         }
         if (page_spec != NULL) free(page_spec);
      }
      else
      {
         char	ext_str[80];
         int	ext_len, len, obj_file=TRUE;

         sprintf (ext_str, ".%s", OBJ_FILE_EXT);
         ext_len = strlen (ext_str);
         len = strlen (file_name);

         if (len < ext_len ||
               strcmp (&file_name[len-ext_len], ext_str) != 0)
         {
            sprintf (ext_str, ".%s", SYM_FILE_EXT);
            ext_len = strlen (ext_str);
   
            if (len >= ext_len &&
                  strcmp (&file_name[len-ext_len], ext_str) == 0)
               obj_file = FALSE;
         }
         if (!LoadFile(file_name, obj_file)) {
            if (do_not_save) {
               SetFileModified (TRUE);
            }
         } else {
            need_to_check_auto_exec = TRUE;
         }
      }
      SetDefaultCursor (mainWindow);
      ShowCursor ();
   }
   else if (do_not_save)
      SetFileModified (TRUE);

   if (need_to_check_auto_exec) {
      struct AttrRec *exec_attr=FindFileAttrWithName("auto_exec=");

      if (exec_attr != NULL) {
         DoExecLoop(NULL, exec_attr);
      }
   }
}

void SetTemplate()
{
   char file_name[MAXPATHLENGTH+1], *rest=NULL;
   int short_name, rc;
   FILE *fp;
   struct AttrRec *attr_ptr;

   MakeQuiescent ();

   if (importFromLibrary) {
      char name[MAXSTRING+1], path[MAXSTRING+1];

      if (SelectFromLibrary("Please select an object file to IMPORT...",
            OBJ_FILE_EXT, name, path) == INVALID) {
         return;
      }
      sprintf(file_name, "%s/%s", path, name);
   } else {
      sprintf(gszMsgBox, "Please select a file to be used as template...");
      if (SelectFileNameToImport(gszMsgBox, OBJ_FILE_EXT, file_name) ==
            INVALID) {
         return;
      } else if (FileIsRemote(file_name)) {
         MsgBox("Using a remote template file is not supported.", TOOL_NAME,
               INFO_MB);
         return;
      }
   }
   if (FileIsRemote(file_name)) {
      sprintf(gszMsgBox, "Using a remote template file is not supported.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if ((short_name=IsPrefix(bootDir, file_name, &rest))) ++rest;
   if ((fp=fopen(file_name, "r")) == NULL) {
      sprintf(gszMsgBox, "Cannot open '%s' for reading.\n\n%s?",
            short_name ? rest : file_name,
            "Would you still like to use it as a template file");
      if (MsgBox(gszMsgBox, TOOL_NAME, YNC_MB) != MB_ID_YES) {
         return;
      }
   } else {
      fclose(fp);
   }
   StartCompositeCmd();
   importingFile = TRUE;
   rc = ImportGivenFile(file_name);
   importingFile = FALSE;
   if (rc == BAD) {
      EndCompositeCmd();
      return;
   } else if (rc == FALSE) {
      sprintf(gszMsgBox, "%s '%s' as a template file?",
            "Would you still like to use",
            short_name ? rest : file_name);
      if (MsgBox(gszMsgBox, TOOL_NAME, YNC_MB) != MB_ID_YES) {
         EndCompositeCmd();
         return;
      }
   }
   if ((attr_ptr=FindAttrWithName(tgifObj, "template=", NULL)) != NULL) {
      ReplaceAttrFirstValue(tgifObj, attr_ptr, short_name ? rest : file_name);
   } else {
      int x=tgifObj->obbox.ltx, y=tgifObj->obbox.rby;

      AddObj(NULL, topObj, tgifObj);
      UpdSelBBox();

      PrepareToReplaceAnObj(tgifObj);
      attr_ptr = AddAttrByNameAndValue(tgifObj, "template=",
            short_name ? rest : file_name);
      attr_ptr->shown = TRUE;
      attr_ptr->obj->color = colorIndex;
      MoveObj(attr_ptr->obj, x-attr_ptr->obj->x, y-attr_ptr->obj->y);
      UpdTextBBox(attr_ptr->obj);
      AdjObjBBox(tgifObj);

      recordCmdIncludeTgifObj = TRUE;
      RecordReplaceAnObj(tgifObj);
      recordCmdIncludeTgifObj = FALSE;

      RemoveAllSel();
      UnlinkObj(topObj);
      UpdSelBBox();

      SetFileModified(TRUE);
      justDupped = FALSE;
   }
   EndCompositeCmd();
   sprintf(gszMsgBox, "Template set to '%s'.", short_name ? rest : file_name);
   Msg(gszMsgBox);
}

char * fileMenuStr[] =
      { "New             ^n",
        "Open            ^o",
        "Save            ^s",
        "SaveNew        ^#s",
        "Import          #p",
        "ImportXBitmap  ^#.",
        "ImportXPixmap  ^#,",
        "ImportEPSFile   #(",
        "ImportGIFFile     ",
        "ImportOtherFile   ",
        "EmbedEPSFile      ",
        "BrowseXBitmap     ",
        "BrowseXPixmap     ",
        "BrowseOther       ",
        "ChangeDomain    ^c",
        "Print           ^p",
        "PrintWithCmd    ^-",
        "PrintSelectedObjs ",
        "PrintOnePage      ",
        "SetExportPixelTrim",
        "InputPolyPts      ",
        "InputPolygonPts   ",
        "SetTemplate       ",
        "Solve           #s",
        "Simulate        #y",
        "Probe           #b",
        "Animate         ^z",
        "Escape          #x",
        "SaveSelectedAs  #~",
        "SaveSymInLibrary  ",
        "Quit            ^q",
        NULL
      };
static char * fileMenuDescription[] =
      { "Start with a blank/new drawing",
        "Open an existing drawing",
        "Save current drawing",
        "Save current drawing in a different file",
        "Embed/Import another drawing",
        "Embed/Import an X11 bitmap file",
        "Embed/Import an X11 pixmap file",
        "Link to an PS/EPS file",
        "Embed/Import an GIF file",
        "Embed/Import another type of file",
        "Embed/Import an PS/EPS file",
        "Recursively read in all X11 bitmap files",
        "Recursively read in all X11 pixmap files",
        "Recursively read in all another type of files",
        "Change to a different domain",
        "Print/export current drawing",
        "Print/export current drawing with a specific command",
        "Print/export only selected objects",
        "Print a page in a multipage drawing",
        "Specify the number of pixels to trim when exporting",
        "Read points from terminal and create a poly",
        "Read points from terminal and create a polygon",
        "Select a template file be used in STACKED page mode",
        "Escape to driver",
        "Escape to driver",
        "Escape to driver",
        "Escape to driver",
        "Escape to driver",
        "Save selected objects in a different file",
        "Save symbol file in a library path within the current domain",
        "Exit <<_______________________PROGRAM_NAME_______________________>>",
        NULL
      };

int QuitProc ()
{
   int	do_not_save = FALSE;

   while (!DirIsRemote(curDir) && fileModified)
   {
      switch (MsgBox ("File modified, save file before quit? [ync](y)",
            TOOL_NAME, YNC_MB))
      {
         case MB_ID_YES: SaveFile (); break;
         case MB_ID_NO: do_not_save = TRUE; SetFileModified (FALSE); break;
         case MB_ID_CANCEL: return (INVALID);
      }
   }
   if (AncesterModified ())
   {
      switch (MsgBox ("Ancester file modified, still quitting? [ync](y)",
            TOOL_NAME, YNC_MB))
      {
         case MB_ID_YES: return (FILE_QUIT);
         case MB_ID_NO:
            if (do_not_save) SetFileModified (TRUE);
            return (INVALID);
         case MB_ID_CANCEL:
            if (do_not_save) SetFileModified (TRUE);
            return (INVALID);
      }
   }
   return (FILE_QUIT);
}

int SolveProc ()
{
   if (!saveTmpOnReturn) return (FILE_SOLVE);

   switch (SaveTmpFile ("tmpmodel"))
   {
      case OBJ_FILE_SAVED: return (FILE_SOLVE);
      case SYM_FILE_SAVED: return (INVALID);
      case INVALID: return (INVALID);
   }
   return (INVALID);
}

int SimulateProc ()
{
   if (!saveTmpOnReturn) return (FILE_SIMULATE);

   switch (SaveTmpFile ("tmpmodel"))
   {
      case OBJ_FILE_SAVED: return (FILE_SIMULATE);
      case SYM_FILE_SAVED: return (INVALID);
      case INVALID: return (INVALID);
   }
   return (INVALID);
}

int ProbeProc ()
{
   if (!saveTmpOnReturn) return (FILE_PROBE);

   switch (SaveTmpFile ("tmpmodel"))
   {
      case OBJ_FILE_SAVED: return (FILE_PROBE);
      case SYM_FILE_SAVED: return (INVALID);
      case INVALID: return (INVALID);
   }
   return (INVALID);
}

int AnimateProc ()
{
   if (!saveTmpOnReturn) return (FILE_ANIMATE);

   switch (SaveTmpFile ("tmpmodel"))
   {
      case OBJ_FILE_SAVED: return (FILE_ANIMATE);
      case SYM_FILE_SAVED: return (INVALID);
      case INVALID: return (INVALID);
   }
   return (INVALID);
}

int EscapeProc ()
{
   return (FILE_ESCAPE);
}

int FileSubMenu (index)
   int	index;
{
   switch (index)
   {
      case FILE_NEW: NewProc (); break;
      case FILE_OPEN: OpenProc (); break;
      case FILE_SAVE: SaveFile (); break;
      case FILE_SAVENEW: SaveNewFile (FALSE); break;
      case FILE_IMPORT: ImportFile (); break;
      case FILE_IMPORTXBM: ImportXBitmapFile (); break;
      case FILE_IMPORTXPM: ImportXPixmapFile (); break;
      case FILE_IMPORTEPS: ImportEPSFile (FALSE); break;
      case FILE_IMPORTGIF: ImportGIFFile (); break;
      case FILE_IMPORTOTHERS: ImportOtherFile (); break;
      case FILE_EMBEDEPS: ImportEPSFile (TRUE); break;
      case FILE_BROWSEXBM: BrowseXBitmap (); break;
      case FILE_BROWSEXPM: BrowseXPixmap (); break;
      case FILE_BROWSEOTHERS: BrowseOther (); break;
      case FILE_DOMAIN: ChangeDomain (); break;
      case FILE_DUMP: Dump (""); break;
      case FILE_USR_DUMP: PrintWithCommand (""); break;
      case FILE_DUMPSELECTED: PrintSelectedObjs (); break;
      case FILE_PRINTONE: PrintOnePage (); break;
      case FILE_SETEXPORTTRIM: SetExportPixelTrim (NULL); break;
      case FILE_INPUT_POLY: InputPolyPts (); break;
      case FILE_INPUT_POLYGON: InputPolygonPts (); break;
      case FILE_SET_TEMPLATE: SetTemplate (); break;
      case FILE_SOLVE: return (SolveProc ());
      case FILE_SIMULATE: return (SimulateProc ());
      case FILE_PROBE: return (ProbeProc ());
      case FILE_ANIMATE: return (AnimateProc ());
      case FILE_ESCAPE: return (EscapeProc ());
      case FILE_SAVESELAS: SaveNewFile (TRUE); break;
      case FILE_SAVESYMINLIB: SaveSymInLibrary (); break;
      case FILE_QUIT: return (QuitProc ());
   }
   return (INVALID);
}

int FileMenu (X, Y, TrackMenuBar)
   int	X, Y, TrackMenuBar;
{
   register int i;
   int		index, * fore_colors, * valid, * init_rv;
   char		**desc=(char **)malloc((FILEMENUENTRIES+1)*sizeof(char*));

   if (desc == NULL) FailAllocMessage();
   for (i=0; i < FILEMENUENTRIES; i++) {
      desc[i] = (char*)malloc((strlen(fileMenuDescription[i])+1)*sizeof(char));
      if (desc[i] == NULL) FailAllocMessage();
      strcpy(desc[i], fileMenuDescription[i]);
   }
   sprintf (desc[i-1], "Exit %s", TOOL_NAME);
   desc[i] = NULL;
   DefaultColorArrays (FILEMENUENTRIES, &fore_colors, &valid, &init_rv, NULL);
   activeMenu = MENU_FILE;
   index = TextMenuLoop (X, Y, fileMenuStr, FILEMENUENTRIES, fore_colors,
         valid, init_rv, desc, SINGLECOLOR, TrackMenuBar);

   if (index >= 0) return FileSubMenu(index);
   if (desc != NULL) {
      for (i=0; i < FILEMENUENTRIES; i++) {
         if (desc[i] != NULL) {
            free(desc[i]);
         }
      }
      free(desc);
   }
   return (index);
}

void CleanUpFiles ()
{
   CleanUpComments ();
   ClearFileInfo ();
   if (usePaperSizeStoredInFile) ResetOnePageSize ();
   fileModified = FALSE;
}
