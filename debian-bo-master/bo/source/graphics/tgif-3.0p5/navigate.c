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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/navigate.c,v 3.0 1996/05/06 16:06:18 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "const.h"
#include "types.h"

#include "auxtext.e"
#include "button.e"
#include "choice.e"
#include "choose.e"
#include "cmd.e"
#include "color.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "file.e"
#include "font.e"
#include "mainloop.e"
#include "mainmenu.e"
#include "menu.e"
#include "msg.e"
#include "names.e"
#ifndef _NO_EXTERN
#include "navigate.e"
#endif
#include "obj.e"
#include "page.e"
#include "rect.e"
#include "remote.e"
#include "scroll.e"
#include "select.e"
#include "setup.e"
#include "stk.e"
#include "util.e"

#ifndef XK_KP_Left
#define XK_KP_Left	0xFF96
#define XK_KP_Up	0xFF97
#define XK_KP_Right	0xFF98
#define XK_KP_Down	0xFF99
#endif /* ~XK_KP_LEFT */

extern int atoi ARGS_DECL((char*));
extern char *getenv ARGS_DECL((char*));

int navigatingBackAndForth=FALSE;
int inHyperSpace=FALSE;
int autoHyperSpaceOnRemote=TRUE;
int allowLaunchInHyperSpace=FALSE;

char * navigateMenuStr[] = {
      "GoBack             ",
      "GoForward          ",
      "RefreshCurrent     ",
      "HotList            ",
      "AddCurrentToHotList",
      "SessionHistory     ",
      "GoHyperSpace       ",
      NULL
};
char * navigateMenuStrInHyperSpace[] = {
      "GoBack             ",
      "GoForward          ",
      "RefreshCurrent     ",
      "HotList            ",
      "AddCurrentToHotList",
      "SessionHistory     ",
      "LeaveHyperSpace    ",
      NULL
};
static char * navigateMenuDescription[] = {
      "Go back one file",
      "Go forward one file",
      "Reload the current file",
      "Navigate using the hot-list",
      "Add the current file to the hot-list",
      "Go to a file visited during this session",
      "Go into HyperSpace -- cursor will indicate hot spots",
      NULL
};
static char * navigateMenuDescriptionInHyperSpace[] = {
      "Go back one file",
      "Go forward one file",
      "Reload the current file",
      "Navigate using the hot-list",
      "Add the current file to the hot-list",
      "Go to a file visited during this session",
      "Get out of HyperSpace",
      NULL
};

static int validHotListFileName=FALSE;
static char *hotListFileName=NULL;

struct NavigateRec {
   struct StkRec *stk;
   struct NavigateRec *next, *prev;
   char *full_fname;
   char *doc_name;
   int cur_page_num, orig_x, orig_y, zoom_scale, zoomed_in;
};

static struct NavigateRec *firstNavigate=NULL, *lastNavigate=NULL;
static struct NavigateRec *curNavigate=NULL;

static struct NavigateRec *firstSessionHistory=NULL, *lastSessionHistory=NULL;

static struct URLCacheRec *firstURLCache=NULL, *lastURLCache=NULL;
static int maxURLCache=(-1), curURLCache=0;

static
void InsertNavigate(pnr_prev, pnr_next, pnr)
   struct NavigateRec *pnr_prev, *pnr_next, *pnr;
{
   pnr->prev = pnr_prev;
   pnr->next = pnr_next;

   if (pnr_prev == NULL) {
      firstNavigate = pnr;
   } else {
      pnr_prev->next = pnr;
   }

   if (pnr_next == NULL) {
      lastNavigate = pnr;
   } else {
      pnr_next->prev = pnr;
   }
}

static
void DeleteNavigate(pnr)
   struct NavigateRec *pnr;
{
   if (pnr->full_fname != NULL) free(pnr->full_fname);
   if (pnr->doc_name != NULL) free(pnr->doc_name);
   free(pnr);
}

static
void ClearNavigateRecords(pnr)
   struct NavigateRec *pnr;
{
   struct NavigateRec *pnr_next;

   for (; pnr != NULL; pnr=pnr_next) {
      pnr_next = pnr->next;
      DeleteNavigate(pnr);
   }
   lastNavigate = curNavigate;
   if (lastNavigate == NULL) firstNavigate = NULL;
}

static
void ClearSessionHistory()
{
   struct NavigateRec *pnr, *pnr_prev;

   for (pnr=lastSessionHistory; pnr != NULL; pnr=pnr_prev) {
      pnr_prev = pnr->prev;
      if (pnr->full_fname != NULL) free(pnr->full_fname);
      if (pnr->doc_name != NULL) free(pnr->doc_name);
      free(pnr);
   }
   lastSessionHistory = firstSessionHistory = NULL;
}

static
void FreeAnURLCache(url_cache)
   struct URLCacheRec *url_cache;
{
   if (url_cache == NULL) return;
   if (url_cache->remote_buf != NULL) free(url_cache->remote_buf);
   if (url_cache->content_type != NULL) free(url_cache->content_type);
   if (url_cache->simple_url_name != NULL) free(url_cache->simple_url_name);
   free(url_cache);
}

static
void CleanUpURLCache()
{
   struct URLCacheRec *next_cache;

   for ( ; firstURLCache != NULL; firstURLCache=next_cache) {
      next_cache = firstURLCache->next;
      FreeAnURLCache(firstURLCache);
   }
   curURLCache = 0;
   firstURLCache = lastURLCache = NULL;
}

void CleanUpNavigate()
{
   curNavigate = NULL;
   ClearNavigateRecords(firstNavigate);
   if (hotListFileName != NULL) free(hotListFileName);
   hotListFileName = NULL;
   validHotListFileName = FALSE;
   ClearSessionHistory();

   CleanUpURLCache();
}

static
void InitURLCache()
{
   if (maxURLCache == (-1)) {
      char *c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"MaxNavigateCacheBuffers");

      maxURLCache = 40;
      if (c_ptr != NULL) {
         maxURLCache = atoi(c_ptr);
         if (maxURLCache < 0) {
            fprintf(stderr, "Invalid %s*%s: '%s', 40 is used.\n",
                  TOOL_NAME, "MaxNavigateCacheBuffers", c_ptr);
            maxURLCache = 40;
         }
      }
      curURLCache = 0;
      firstURLCache = lastURLCache = NULL;
   }
}

static
char *SimpleURLName(psz_url)
   char *psz_url;
{
   char *c_ptr=strchr(psz_url, '#');

   if (c_ptr != NULL) {
      char *return_buf;

      *c_ptr = '\0';
      return_buf = UtilStrDup(psz_url);
      *c_ptr = '#';
      return return_buf;
   }
   return UtilStrDup(psz_url);
}

static
void UnlinkURLCache(url_cache)
   struct URLCacheRec *url_cache;
{
   if (url_cache == NULL) return;
   if (url_cache->prev == NULL) {
      firstURLCache = url_cache->next;
   } else {
      url_cache->prev->next = url_cache->next;
   }
   if (url_cache->next == NULL) {
      lastURLCache = url_cache->prev;
   } else {
      url_cache->next->prev = url_cache->prev;
   }
   url_cache->prev = url_cache->next = NULL;
   curURLCache--;
}

static
void InsertURLCache(prev_url, next_url, url_cache)
   struct URLCacheRec *prev_url, *next_url, *url_cache;
{
   url_cache->prev = prev_url;
   url_cache->next = next_url;
   if (prev_url == NULL) {
      firstURLCache = url_cache;
   } else {
      prev_url->next = url_cache;
   }
   if (next_url == NULL) {
      lastURLCache = url_cache;
   } else {
      next_url->prev = url_cache;
   }
   curURLCache++;
}

void UpdateLRU(url_cache)
   struct URLCacheRec *url_cache;
{
   UnlinkURLCache(url_cache);
   InsertURLCache(lastURLCache, NULL, url_cache);
}

struct URLCacheRec *FindURLCache(psz_url, update_lru)
   char *psz_url;
   int update_lru;
{
   char *simple_url_name=SimpleURLName(psz_url);
   struct URLCacheRec *url_cache;

   InitURLCache();
   if (simple_url_name == NULL) return NULL;
   for (url_cache=lastURLCache; url_cache != NULL; url_cache=url_cache->prev) {
      if (strcmp(simple_url_name, url_cache->simple_url_name) == 0) {
         free(simple_url_name);
         if (update_lru) UpdateLRU(url_cache);
         return url_cache;
      }
   }
   free(simple_url_name);
   return NULL;
}

void UpdateURLCache(psz_url, psz_remote_buf, psz_content_type, remote_buf_sz,
      is_html)
   char *psz_url, *psz_remote_buf, *psz_content_type;
   int remote_buf_sz, is_html;
{
   char *simple_url_name=SimpleURLName(psz_url);
   struct URLCacheRec *url_cache;

   InitURLCache();
   if (simple_url_name == NULL) return;
   for (url_cache=lastURLCache; url_cache != NULL; url_cache=url_cache->prev) {
      if (strcmp(simple_url_name, url_cache->simple_url_name) == 0) {
         break;
      }
   }
   if (url_cache != NULL) {
      UnlinkURLCache(url_cache);
      FreeAnURLCache(url_cache);
   } else {
      if (curURLCache >= maxURLCache) {
         url_cache = firstURLCache;
         UnlinkURLCache(url_cache);
         FreeAnURLCache(url_cache);
      }
   }
   url_cache = (struct URLCacheRec*)malloc(sizeof(struct URLCacheRec));
   if (url_cache == NULL) {
      FailAllocMessage();
      free(simple_url_name);
      return;
   }
   memset(url_cache, 0, sizeof(struct URLCacheRec));
   url_cache->remote_buf_sz = remote_buf_sz;
   url_cache->is_html = is_html;
   url_cache->remote_buf = UtilStrDup(psz_remote_buf);
   url_cache->content_type = UtilStrDup(psz_content_type);
   url_cache->simple_url_name = simple_url_name;
   InsertURLCache(lastURLCache, NULL, url_cache);
}

static
int InitHotListFileName()
{
   if (!validHotListFileName)
   {
      char *c_ptr;

      hotListFileName = NULL;
      if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"HotListFileName")) !=
            NULL) {
         hotListFileName = UtilStrDup(c_ptr);
      } else {
         int len=strlen(homeDir)+strlen(TOOL_NAME)+20;

         hotListFileName = (char*)malloc((len+1)*sizeof(char));
         if (hotListFileName == NULL) FailAllocMessage();
         sprintf(hotListFileName, "%s/.%s_hotlist", homeDir, TOOL_NAME);
      }
   }
   validHotListFileName = TRUE;
   return TRUE;
}

void BeforeNavigate()
{
   if (curFileDefined && curNavigate != NULL) {
      curNavigate->cur_page_num = curPageNum;
      curNavigate->orig_x = drawOrigX;
      curNavigate->orig_y = drawOrigY;
      curNavigate->zoom_scale = zoomScale;
      curNavigate->zoomed_in = zoomedIn;
   }
}

static
void AddToSessionHistory(cur_pnr)
   struct NavigateRec *cur_pnr;
{
   struct NavigateRec *pnr;

   pnr = (struct NavigateRec *)malloc(sizeof(struct NavigateRec));
   if (pnr == NULL) FailAllocMessage();
   memset(pnr, 0, sizeof(struct NavigateRec));

   pnr->prev = lastSessionHistory;
   pnr->next = NULL;
   pnr->stk = NULL;
   pnr->full_fname = (cur_pnr->full_fname==NULL ? NULL :
         UtilStrDup(cur_pnr->full_fname));
   pnr->doc_name = (cur_pnr->doc_name==NULL ? NULL :
         UtilStrDup(cur_pnr->doc_name));

   if (lastSessionHistory == NULL) {
      firstSessionHistory = pnr;
   } else {
      lastSessionHistory->next = pnr;
   }
   lastSessionHistory = pnr;
}

void CommitNavigate()
   /* add a navigation record at the end */
{
   int len;

   if (curNavigate == NULL) {
      ClearNavigateRecords(firstNavigate);
   } else if (curNavigate != lastNavigate) {
      ClearNavigateRecords(curNavigate->next);
   }

   if (!curFileDefined) return;

   curNavigate = (struct NavigateRec *)malloc(sizeof(struct NavigateRec));
   if (curNavigate == NULL) FailAllocMessage();
   memset(curNavigate, 0, sizeof(struct NavigateRec));
   curNavigate->stk = NULL;
   curNavigate->next = curNavigate->prev = NULL;
   curNavigate->full_fname = curNavigate->doc_name = NULL;
   curNavigate->cur_page_num = 1;
   curNavigate->orig_x = curNavigate->orig_y = 0;
   curNavigate->zoom_scale = 0;
   curNavigate->zoomed_in = FALSE;

   len = strlen(curDir)+1+strlen(curFileName);

   curNavigate->full_fname = (char*)malloc((len+1)*sizeof(char));
   if (curNavigate->full_fname == NULL) FailAllocMessage();
   sprintf(curNavigate->full_fname, "%s/%s", curDir, curFileName);
   if (firstPage != NULL && firstPage->name != NULL) {
      curNavigate->doc_name = UtilStrDup(firstPage->name);
   } else {
      curNavigate->doc_name = NULL;
   }
   curNavigate->stk = topStk;

   InsertNavigate(lastNavigate, NULL, curNavigate);
   curNavigate = lastNavigate;
   AddToSessionHistory(curNavigate);
}

static
void PostNavigate(pnr)
   struct NavigateRec *pnr;
{
   struct AttrRec *exec_attr=FindFileAttrWithName("auto_exec=");

   if (exec_attr != NULL) {
      DoExecLoop(NULL, exec_attr);
   } else if (pnr != NULL) {
      ScrollToSpecifiedOrigin(pnr->cur_page_num, pnr->orig_x, pnr->orig_y,
            pnr->zoom_scale, pnr->zoomed_in);
   }
}

static
void NavigateTo(full_fname, do_not_save, force_load)
   char *full_fname;
   int do_not_save, force_load;
{
   if (FileIsRemote(full_fname)) {
      char *buf=NULL, *content_type=NULL;
      int rc, buf_sz=0, is_html=FALSE;

      SetWatchCursor(drawWindow);
      SetWatchCursor(mainWindow);
      SaveStatusStrings();
      rc = LoadRemoteFileInMem(full_fname, &buf, &content_type, &buf_sz,
            &is_html, force_load);
      RestoreStatusStrings();
      SetDefaultCursor(mainWindow);
      ShowCursor();
      if (rc && buf != NULL) {
         navigatingBackAndForth = TRUE;
         LoadRemoteFileFromMem(full_fname, buf, content_type, buf_sz, is_html);
         navigatingBackAndForth = FALSE;
      } else if (do_not_save) {
         SetFileModified(TRUE);
      }
      if (content_type != NULL) FreeRemoteBuf(content_type);
      if (buf != NULL) FreeRemoteBuf(buf);
   } else {
      navigatingBackAndForth = TRUE;
      if (!LoadFile(full_fname, TRUE)) {
         if (do_not_save) {
            SetFileModified(TRUE);
         }
      }
      navigatingBackAndForth = FALSE;
   }
}

void NavigateBack()
{
   int do_not_save=FALSE;
   struct NavigateRec nr;

   if (curNavigate != NULL && curNavigate->stk != NULL &&
         curNavigate->stk == topStk && (curNavigate->prev == NULL ||
         (curNavigate->prev != NULL && curNavigate->prev->stk != topStk))) {
      /* curNavigate->stk = NULL; */
      /* if (curFileDefined) curNavigate = curNavigate->prev; */
      PopIcon();
      return;
   }
   while (!DirIsRemote(curDir) && fileModified) {
      switch (MsgBox(
            "File modified, save file before going back? [ync](y)",
            TOOL_NAME, YNC_MB)) {
      case MB_ID_YES: SaveFile(); break;
      case MB_ID_NO: do_not_save=TRUE; SetFileModified(FALSE); break;
      case MB_ID_CANCEL: return;
      }
   }
   MakeQuiescent();
   if (curNavigate == NULL || (curFileDefined && curNavigate->prev == NULL)) {
      MsgBox("Nothing to go back to!", TOOL_NAME, INFO_MB);
      if (do_not_save) SetFileModified(TRUE);
      return;
   }
   BeforeNavigate();
   if (curFileDefined) curNavigate = curNavigate->prev;
   memcpy(&nr, curNavigate, sizeof(struct NavigateRec));
   NavigateTo(curNavigate->full_fname, do_not_save, FALSE);
   PostNavigate(&nr);
}

void NavigateForward()
{
   int do_not_save=FALSE;
   struct NavigateRec nr;

   while (!DirIsRemote(curDir) && fileModified) {
      switch (MsgBox(
            "File modified, save file before going forward? [ync](y)",
            TOOL_NAME, YNC_MB)) {
      case MB_ID_YES: SaveFile(); break;
      case MB_ID_NO: do_not_save=TRUE; SetFileModified(FALSE); break;
      case MB_ID_CANCEL: return;
      }
   }
   MakeQuiescent();
   if (curNavigate == lastNavigate) {
      MsgBox("Nothing to go forward to!", TOOL_NAME, INFO_MB);
      if (do_not_save) SetFileModified(TRUE);
      return;
   }
   BeforeNavigate();
   if (curFileDefined) curNavigate = curNavigate->next;
   memcpy(&nr, curNavigate, sizeof(struct NavigateRec));
   NavigateTo(curNavigate->full_fname, do_not_save, FALSE);
   PostNavigate(&nr);
}

void AdjustNavigate()
{
   while (curNavigate != NULL && curNavigate->stk != NULL &&
         curNavigate->stk == topStk) {
      curNavigate->stk = NULL;
      if (curNavigate->prev == NULL || (curNavigate->prev != NULL &&
            curNavigate->prev->stk != topStk)) {
         if (curFileDefined && curNavigate->prev != NULL) {
            curNavigate = curNavigate->prev;
         }
         return;
      } else {
         curNavigate = curNavigate->prev;
      }
   }
   Msg("Possible fatal error.  Please read the terminal error output.");
   fprintf(stderr, "Warning:  fail to find record in AdjustNavigate().\n");
   fprintf(stderr, "\nNavigating may crash %s.\n", TOOL_NAME);
   fprintf(stderr, "Safest thing to do is to save and exit.\n");
   fprintf(stderr, "Please try to reproduce this error and\n");
   fprintf(stderr, "\tsend bug report to william@cs.ucla.edu.\n");
   MsgBox("Possible fatal error.  Please read the terminal error output.",
         TOOL_NAME, STOP_MB);
   XFlush(mainDisplay);
   XSync(mainDisplay, False);
}

void NavigateRefresh()
{
   if (!curFileDefined) {
      MsgBox("Can not refresh an undefined file.", TOOL_NAME, INFO_MB);
      return;
   }
   while (!DirIsRemote(curDir) && fileModified) {
      switch (MsgBox("File modified, save file before reloading? [ync](y)",
         TOOL_NAME, YNC_MB)) {
      case MB_ID_YES: SaveFile(); break;
      case MB_ID_NO: SetFileModified(FALSE); break;
      case MB_ID_CANCEL: return;
      }
   }
   NavigateTo(curNavigate->full_fname, TRUE, TRUE);
   PostNavigate(NULL);
}

static
char **ReadHotListFile(pn_count)
   int *pn_count;
{
   FILE *fp;
   char **ppsz_buf=NULL, *buf;
   int num_lines=0;

   if (pn_count != NULL) *pn_count = 0;
   if (!InitHotListFileName() || hotListFileName==NULL) {
      sprintf(gszMsgBox, "%s.  %s %s*%s.",
            "Hot list file undefined",
            "Please specify it in", TOOL_NAME, "HotListFileName");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return NULL;
   }
   if ((ppsz_buf=(char**)malloc((num_lines+1)*sizeof(char*))) == NULL) {
      FailAllocMessage();
      return NULL;
   }
   if ((fp=fopen(hotListFileName, "r")) == NULL) {
      ppsz_buf[num_lines] = NULL;
      return ppsz_buf;
   }
   while ((buf=UtilGetALine(fp)) != NULL) {
      if ((ppsz_buf=(char**)realloc(ppsz_buf,
            ((++num_lines)+1)*sizeof(char*))) == NULL) {
         FailAllocMessage();
         return NULL;
      }
      ppsz_buf[num_lines-1] = buf;
   }
   ppsz_buf[num_lines] = NULL;
   if ((num_lines & 0x1) != 0) {
      ppsz_buf[num_lines-1] = NULL;
      sprintf(gszMsgBox, "Malformed hot list file '%s'.", hotListFileName);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   }
   fclose(fp);
   if (pn_count != NULL) *pn_count = (num_lines>>1);
   return ppsz_buf;
}

static
DspList *HotListListing(ppsz_buf, pn_count)
   char **ppsz_buf;
   int *pn_count;
{
   int i;
   char **s_ptr;
   DspList *pdl, *dsp_ptr;

   *pn_count = 0;
   for (s_ptr=ppsz_buf; *s_ptr != NULL; s_ptr=(&s_ptr[2])) {
      (*pn_count)++;
   }
   pdl = (DspList*)malloc((*pn_count)*sizeof(DspList));
   if (pdl == NULL) FailAllocMessage();
   memset(pdl, 0, (*pn_count)*sizeof(DspList));
   for (i=(*pn_count)-1, dsp_ptr=pdl, s_ptr=ppsz_buf; *s_ptr != NULL;
         i--, dsp_ptr++, s_ptr=(&s_ptr[2])) {
      if (**s_ptr == '\0') {
         UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr), s_ptr[1]);
         dsp_ptr->directory = FALSE; /* use file name as title */
      } else {
         UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr), s_ptr[0]);
         dsp_ptr->directory = TRUE; /* file has a title */
      }
      UtilStrCpy(dsp_ptr->pathstr, sizeof(dsp_ptr->pathstr), s_ptr[1]);
      dsp_ptr->next = (i==0 ? NULL : &dsp_ptr[1]);
   }
   return pdl;
}

static
DspList *HistoryListing(pn_count)
   int *pn_count;
{
   int i;
   struct NavigateRec *pnr;
   DspList *pdl, *dsp_ptr;

   *pn_count = 0;
   for (pnr=lastSessionHistory; pnr != NULL; pnr=pnr->prev) {
      (*pn_count)++;
   }
   if (*pn_count == 0) return NULL;
   pdl = (DspList*)malloc((*pn_count)*sizeof(DspList));
   if (pdl == NULL) FailAllocMessage();
   memset(pdl, 0, (*pn_count)*sizeof(DspList));
   for (i=0, dsp_ptr=pdl, pnr=firstSessionHistory; pnr != NULL;
         i++, dsp_ptr++, pnr=pnr->next) {
      if (pnr->doc_name == NULL) {
         UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr),
               pnr->full_fname);
         dsp_ptr->directory = FALSE; /* use file name as title */
      } else {
         UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr), pnr->doc_name);
         dsp_ptr->directory = TRUE; /* file has a title */
      }
      UtilStrCpy(dsp_ptr->pathstr, sizeof(dsp_ptr->pathstr), pnr->full_fname);
      dsp_ptr->next = (i==(*pn_count)-1 ? NULL : &dsp_ptr[1]);
   }
   return pdl;
}

struct HotListInfoRec {
   char **ppsz_buf;
};

#define GOTO_BUTTON 101
#define DELETE_BUTTON 102
#define CLOSE_BUTTON 103

static
int GetHotListEntries(p_dsp_ptr, p_entries, pn_num_entries, pn_marked_index,
      cur_buf, p_void)
   DspList **p_dsp_ptr;
   char ***p_entries, *cur_buf;
   int *pn_num_entries, *pn_marked_index;
   void *p_void;
{
   struct HotListInfoRec *p_hlinfo=(struct HotListInfoRec *)p_void;

   p_hlinfo->ppsz_buf = ReadHotListFile(NULL);

   if (p_hlinfo->ppsz_buf == NULL) {
      return FALSE;
   } else if (*p_hlinfo->ppsz_buf == NULL) {
      MsgBox("Hot list file empty!", TOOL_NAME, INFO_MB);
      free(p_hlinfo->ppsz_buf);
      p_hlinfo->ppsz_buf = NULL;
      return FALSE;
   } else {
      *p_dsp_ptr = HotListListing(p_hlinfo->ppsz_buf, pn_num_entries);

      ignoreDirectoryFlag = TRUE;
      *p_entries = MakeNameDspItemArray(*pn_num_entries, *p_dsp_ptr);
      ignoreDirectoryFlag = FALSE;
   }
   return TRUE;
}

static
int HotListAfterLoop(p_dsp_ptr, p_entries, pn_num_entries, pn_marked_index,
      cur_buf, btn_id, selected_index, p_void)
   DspList **p_dsp_ptr;
   char ***p_entries, *cur_buf;
   int *pn_num_entries, *pn_marked_index, btn_id, selected_index;
   void *p_void;
{
   int i, modified=FALSE, something_deleted=FALSE, goto_something=FALSE;
   int navigated_to=FALSE;
   struct HotListInfoRec *p_hlinfo=(struct HotListInfoRec *)p_void;
   char **s_ptr;

   switch (btn_id) {
   case GOTO_BUTTON: goto_something=TRUE; break;
   case DELETE_BUTTON: something_deleted=TRUE; break;
   }
   for (s_ptr=p_hlinfo->ppsz_buf, i=0; *s_ptr != NULL; s_ptr=(&s_ptr[2]), i++) {
      if (something_deleted && i == (*pn_marked_index)) {
         sprintf(gszMsgBox, "Delete '%s' from the hot list? [ync](y)",
               (**s_ptr == '\0' ? s_ptr[1] : *s_ptr));
         if (MsgBox(gszMsgBox, TOOL_NAME, YNC_MB)==MB_ID_YES) {
            *s_ptr[0] = *s_ptr[1] = '\0';
            modified = TRUE;
            continue;
         }
      }
      if (*s_ptr[0] == '\0') {
         if (strcmp((*p_entries)[i], s_ptr[1]) != 0) {
            free(*s_ptr);
            if (((*s_ptr)=UtilStrDup((*p_entries)[i])) == NULL) {
               FailAllocMessage();
            }
            modified = TRUE;
         }
      } else {
         if (strcmp((*p_entries)[i], *s_ptr) != 0) {
            free(*s_ptr);
            if (((*s_ptr)=UtilStrDup((*p_entries)[i])) == NULL) {
               FailAllocMessage();
            }
            modified = TRUE;
         }
      }
   }
   if (modified || something_deleted) {
      FILE *fp;

      if ((fp=fopen(hotListFileName, "w")) == NULL) {
         sprintf(gszMsgBox, "Can not open '%s' for write.",
               hotListFileName);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      } else {
         for (s_ptr=p_hlinfo->ppsz_buf; *s_ptr != NULL; s_ptr=(&s_ptr[2])) {
            if (!(**s_ptr == '\0' && *s_ptr[1] == '\0')) {
               fprintf(fp, "%s\n%s\n", *s_ptr, s_ptr[1]);
            }
         }
         fclose(fp);
      }
   }
   free(*p_dsp_ptr);
   free(**p_entries);
   free(*p_entries);
   *p_entries = NULL;

   if (goto_something) {
      int do_not_save=FALSE, canceled=FALSE;

      while (!canceled && !DirIsRemote(curDir) && fileModified)
      {
         sprintf(gszMsgBox, "%s, %s? [ync](y)",
               "File modified", "save file before opening another file");
         switch (MsgBox(gszMsgBox, TOOL_NAME, YNC_MB)) {
         case MB_ID_YES: SaveFile(); break;
         case MB_ID_NO: do_not_save=TRUE; SetFileModified(FALSE); break;
         case MB_ID_CANCEL: canceled=TRUE; break;
         }
      }
      if (!canceled) {
         for (i=0, s_ptr=p_hlinfo->ppsz_buf; *s_ptr != NULL;
               s_ptr=(&s_ptr[2]), i++) {
            if (i == (*pn_marked_index)) {
               NavigateTo(s_ptr[1], do_not_save, FALSE);
               CommitNavigate();
               navigated_to = TRUE;
               break;
            }
         }
      }
   }
   for (s_ptr=p_hlinfo->ppsz_buf; *s_ptr != NULL; s_ptr++) free(*s_ptr);
   free(p_hlinfo->ppsz_buf);
   p_hlinfo->ppsz_buf = NULL;

   if (something_deleted) {
      if (modified) {
         (*pn_marked_index)--;
      }
   } else if (!goto_something) {
      (*pn_marked_index) = (*pn_num_entries)-1;
   }
   if ((*pn_marked_index) < 0) (*pn_marked_index) = INVALID;

   if (navigated_to) {
      PostNavigate(NULL);
   }
   return TRUE;
}

static
int GetHistoryEntries(p_dsp_ptr, p_entries, pn_num_entries, pn_marked_index,
      cur_buf, p_void)
   DspList **p_dsp_ptr;
   char ***p_entries, *cur_buf;
   int *pn_num_entries, *pn_marked_index;
   void *p_void;
{
   *p_dsp_ptr = HistoryListing(pn_num_entries);
   if (*p_dsp_ptr == NULL) {
      MsgBox("Sesion history has not been established yet!",
            TOOL_NAME, INFO_MB);
      return FALSE;
   } else {
      ignoreDirectoryFlag = TRUE;
      *p_entries = MakeNameDspItemArray(*pn_num_entries, *p_dsp_ptr);
      ignoreDirectoryFlag = FALSE;
      if (*pn_marked_index == INVALID) {
         *pn_marked_index = (*pn_num_entries)-1;
      }
   }
   return TRUE;
}

static
int HistoryAfterLoop(p_dsp_ptr, p_entries, pn_num_entries, pn_marked_index,
      cur_buf, btn_id, selected_index, p_void)
   DspList **p_dsp_ptr;
   char ***p_entries, *cur_buf;
   int *pn_num_entries, *pn_marked_index, btn_id, selected_index;
   void *p_void;
{
   int goto_something=(btn_id == GOTO_BUTTON), navigated_to=FALSE;

   free(*p_dsp_ptr);
   free(**p_entries);
   free(*p_entries);
   *p_entries = NULL;

   if (goto_something) {
      int do_not_save=FALSE, canceled=FALSE;

      while (!canceled && !DirIsRemote(curDir) && fileModified) {
         sprintf(gszMsgBox, "%s, %s? [ync](y)",
               "File modified", "save file before opening another file");
         switch (MsgBox(gszMsgBox, TOOL_NAME, YNC_MB)) {
         case MB_ID_YES: SaveFile(); break;
         case MB_ID_NO: do_not_save=TRUE; SetFileModified(FALSE); break;
         case MB_ID_CANCEL: canceled=TRUE; break;
         }
      }
      if (!canceled) {
         int i;
         struct NavigateRec *pnr;

         pnr = firstSessionHistory;
         for (i=0; pnr != NULL; pnr=pnr->next, i++) {
            if (i == (*pn_marked_index)) {
               NavigateTo(pnr->full_fname, do_not_save, FALSE);
               CommitNavigate();
               navigated_to = TRUE;
               break;
            }
         }
      }
   }
   if (navigated_to) {
      PostNavigate(NULL);
   }
   return TRUE;
}

static XComposeStatus c_stat;

static
void SelectForNavigate(TopStr, Which)
   char *TopStr;
   int Which; /* either NAVIGATE_HOTLIST or NAVIGATE_HISTORY */
{
   char win_name[128];
   struct HotListInfoRec hlinfo;

   memset(&hlinfo, 0, sizeof(struct HotListInfoRec));

   ResetNamesInfo();
   NamesSetTitle(TopStr);
   NamesSetDefaultBtnId(GOTO_BUTTON, GOTO_BUTTON);
   NamesSetEntries(NULL, NULL, 0, TRUE, INVALID, 0);
   switch (Which) {
   case NAVIGATE_HOTLIST:
      NamesAddButton("GOTO", GOTO_BUTTON);
      NamesAddButton("DELETE", DELETE_BUTTON);
      NamesAddButton("CLOSE", BUTTON_CANCEL);
      NamesSetStyle(NAMES_EDIT_NAME, NAMES_LOOP_MANY);
      NamesSetCallback((GetEntriesFunc*)GetHotListEntries,
            (AfterLoopFunc*)HotListAfterLoop);
      sprintf(win_name, "%s - Hot List", TOOL_NAME);
      break;
   case NAVIGATE_HISTORY:
      NamesAddButton("GOTO", GOTO_BUTTON);
      NamesAddButton("CLOSE", BUTTON_CANCEL);
      NamesSetStyle(NAMES_COMPLEX_SELECT_NAME, NAMES_LOOP_MANY);
      NamesSetCallback((GetEntriesFunc*)GetHistoryEntries,
            (AfterLoopFunc*)HistoryAfterLoop);
      sprintf(win_name, "%s - History", TOOL_NAME);
      break;
   default: return;
   }
   Names(win_name, NULL, NULL, 0, &hlinfo);
}

void NavigateHotList()
{
   MakeQuiescent ();
   if (!InitHotListFileName() || hotListFileName==NULL) {
      sprintf(gszMsgBox, "%s.  %s %s*%s.",
            "Hot list file undefined",
            "Please specify it in", TOOL_NAME, "HotListFileName");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   SelectForNavigate("Hot list...", NAVIGATE_HOTLIST);
}

void NavigateAddToHotList()
{
   int len=strlen(curDir)+1+strlen(curFileName), num_entries=0;
   char **s_ptr, **ppsz_buf, *full_fname;
   FILE *fp;

   if (!curFileDefined) {
      MsgBox("Can not add an undefined file to hot list.", TOOL_NAME, INFO_MB);
      return;
   }
   if ((full_fname=(char*)malloc((len+1)*sizeof(char))) == NULL) {
      FailAllocMessage();
      return;
   }
   sprintf(full_fname, "%s/%s", curDir, curFileName);
   if ((ppsz_buf=ReadHotListFile(&num_entries)) == NULL) return;
   if ((fp=fopen(hotListFileName, "w")) == NULL) {
      sprintf(gszMsgBox, "Can not open '%s' for write.", hotListFileName);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      free(full_fname);
      return;
   }
   for (s_ptr=ppsz_buf; *s_ptr != NULL; s_ptr=(&s_ptr[2])) {
      if (strcmp(s_ptr[1], full_fname) == 0) {
         char **s_ptr1;

         if (s_ptr[2] != NULL) {
            for (s_ptr1=ppsz_buf; s_ptr1 != s_ptr; s_ptr1++) {
               fprintf(fp, "%s\n", *s_ptr1);
            }
            for (s_ptr1=(&s_ptr[2]); *s_ptr1 != NULL; s_ptr1++) {
               fprintf(fp, "%s\n", *s_ptr1);
            }
         } else {
            for (s_ptr1=ppsz_buf; s_ptr1 != s_ptr; s_ptr1++) {
               fprintf(fp, "%s\n", *s_ptr1);
            }
         }
         fprintf(fp, "%s\n", (firstPage->name==NULL ? "" : firstPage->name));
         fprintf(fp, "%s\n", full_fname);
         break;
      }
   }
   if (*s_ptr == NULL) {
      for (s_ptr=ppsz_buf; *s_ptr != NULL; s_ptr++) {
         fprintf(fp, "%s\n", *s_ptr);
      }
      fprintf(fp, "%s\n", (firstPage->name==NULL ? "" : firstPage->name));
      fprintf(fp, "%s\n", full_fname);
      num_entries++;
   }
   for (s_ptr=ppsz_buf; *s_ptr != NULL; s_ptr++) free(*s_ptr);
   free(ppsz_buf);
   fclose(fp);
   free(full_fname);
   sprintf(gszMsgBox, "Hot list file updated (%1d entrie%s).", num_entries,
         (num_entries>1 ? "s" : ""));
   Msg(gszMsgBox);
}

void NavigateSessionHistory()
{
   MakeQuiescent ();
   SelectForNavigate("Session history...", NAVIGATE_HISTORY);
}

void ToggleHyperSpace(KeepSelected)
   int KeepSelected;
{
   inHyperSpace = !inHyperSpace;
   if (inHyperSpace) {
      if (!KeepSelected) MakeQuiescent();
      Msg("Entering hyperspace...");
   } else {
      ShowCursor();
      Msg("Leaving hyperspace...");
   }
   UpdateSubMenu(MENU_NAVIGATE);
   RedrawDummyWindow1();
}

void NavigateSubMenu(index)
   int index;
{
   switch (index) {
   case NAVIGATE_BACK: NavigateBack(); break;
   case NAVIGATE_FORWARD: NavigateForward(); break;
   case NAVIGATE_REFRESH: NavigateRefresh(); break;
   case NAVIGATE_HOTLIST: NavigateHotList(); break;
   case NAVIGATE_ADD: NavigateAddToHotList(); break;
   case NAVIGATE_HISTORY: NavigateSessionHistory(); break;
   case NAVIGATE_HYPERSPACE: ToggleHyperSpace(FALSE); break;
   }
}

int NavigateMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   int index;
   int *fore_colors, *valid, *init_rv;

   DefaultColorArrays(MAXNAVIGATEMENUS, &fore_colors, &valid, &init_rv, NULL);
   activeMenu = MENU_NAVIGATE;
   if (inHyperSpace) {
      index = TextMenuLoop(X, Y, navigateMenuStrInHyperSpace, MAXNAVIGATEMENUS,
            fore_colors, valid, init_rv, navigateMenuDescriptionInHyperSpace,
            SINGLECOLOR, TrackMenubar);
   } else {
      index = TextMenuLoop(X, Y, navigateMenuStr, MAXNAVIGATEMENUS, fore_colors,
            valid, init_rv, navigateMenuDescription, SINGLECOLOR, TrackMenubar);
   }

   if (index >= 0) NavigateSubMenu(index);
   return index;
}
