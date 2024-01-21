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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/page.c,v 3.0 1996/05/06 16:06:33 william Exp $";
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
#include "cmd.e"
#include "choice.e"
#include "choose.e"
#include "color.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "file.e"
#include "font.e"
#include "grid.e"
#include "mark.e"
#include "mainloop.e"
#include "mainmenu.e"
#include "menu.e"
#include "move.e"
#include "msg.e"
#include "names.e"
#include "obj.e"
#ifndef _NO_EXTERN
#include "page.e"
#endif
#include "raster.e"
#include "rect.e"
#include "scroll.e"
#include "select.e"
#include "setup.e"
#include "stk.e"
#include "util.e"

extern int	atoi ARGS_DECL((char *));

struct PageRec	* firstPage=NULL, * lastPage=NULL, * curPage=NULL;
int		curPageNum=1, lastPageNum=1;
int		paperRow=1, paperCol=1;
int		pageLayoutMode=PAGE_STACK;
int		pageLineShownInTileMode=TRUE;

static int pageWindowFirstIndex=1;

char * pageStackMenuStr[] =
      { "NextPage           ",
        "PrevPage           ",
        "NamePages          ",
        "GotoPage           ",
        "AddPageBefore      ",
        "AddPageAfter       ",
        "DeleteCurPage      ",
        "PrintOnePage       ",
        "SpecifyPaperSize   ",
        "DeletePages        ",
        "PrintOneFilePerPage",
        NULL
      };
static char * pageStackMenuDescription[] =
      { "Go to next page",
        "Go to previous page",
        "Give names to pages",
        "Go to a specified page",
        "Add a page before the current page",
        "Add a page after the current page",
        "Delete the current page",
        "Print the curret page",
        "Set the physical size of the paper",
        "Delete user-specified pages",
        "Print into a separate file for each page",
        NULL
      };

char * pageTileMenuStr[] =
      { "TogglePageLineShown",
        "SpecifyDrawingSize ",
        "PrintOnePage       ",
        "SpecifyPaperSize   "
      };
static char * pageTileMenuDescription[] =
      { "Toggle the showing of page boundaries",
        "Change the drawing size",
        "Select and print one of the pages",
        "Set the physical size of the paper",
        NULL
      };

static int gnPageNumOnTab=TRUE;

static
int DrawAHorizontalTab(page_num, page_ptr, x, y, on_top, skip)
   int page_num, x, y, on_top, skip;
   struct PageRec *page_ptr;
{
   char s[20];
   XPoint v[5];
   int w=0, num_digits=0, x_offset, y_offset;

   if (gnPageNumOnTab) {
      sprintf(s, "%1d", page_num);
      num_digits = strlen(s);
      w = (num_digits+2)*rulerFontWidth;
   } else {
      /* not implemented yet */
      if (page_ptr->name == NULL) {
      } else {
      }
      w = (num_digits+2)*rulerFontWidth;
   }
   if (skip) return w;

   v[0].x = v[4].x = x;         v[0].y = v[4].y = y-1;
   v[1].x = x+rulerFontWidth;   v[1].y = y+2+rulerFontHeight;
   v[2].x = x+w;                v[2].y = y+2+rulerFontHeight;
   v[3].x = x+w+rulerFontWidth; v[3].y = y-1;

   XSetForeground(mainDisplay, defaultGC, myBgPixel);
   XFillPolygon(mainDisplay, pageWindow, defaultGC, v, 5, Convex,
         CoordModeOrigin);
   XSetForeground(mainDisplay, defaultGC, myFgPixel);
   if (on_top) {
      XDrawLines(mainDisplay, pageWindow, defaultGC, v, 4, CoordModeOrigin);
      XSetForeground(mainDisplay, defaultGC, myBgPixel);
      XDrawLines(mainDisplay, pageWindow, defaultGC, &v[3], 2, CoordModeOrigin);
      XSetForeground(mainDisplay, defaultGC, myFgPixel);
   } else {
      XDrawLines(mainDisplay, pageWindow, defaultGC, v, 5, CoordModeOrigin);
   }
   y_offset = ((rulerFontHeight-7)>>1)+1;
   x_offset = (rulerFontWidth>>1)+rulerFontWidth;
   XDrawString(mainDisplay, pageWindow, defaultGC, x+x_offset,
         y+1+rulerFontAsc, s, num_digits);
   XSetForeground(mainDisplay, defaultGC, myFgPixel);
   return w;
}

void RedrawPageWindow()
{
   int i, x, y=1;
   char s[20];
   struct PageRec *page_ptr;

   XClearArea(mainDisplay, pageWindow, 0, 0, pageWindowW,
         scrollBarW+(brdrW<<1), FALSE);
   XDrawRectangle(mainDisplay, pageWindow, defaultGC, 0, 0, pageWindowW-1,
         scrollBarW+(brdrW<<1)-1);
   if (pageLayoutMode == PAGE_TILE) return;

   x = 1 + (scrollBarW<<2);
   XSetFont(mainDisplay, defaultGC, rulerFontPtr->fid);
   for (i=1, page_ptr=firstPage; page_ptr != NULL;
         page_ptr=page_ptr->next, i++) {
      if (i >= pageWindowFirstIndex) {
         int w=0;

         if (page_ptr == curPage) {
            w = DrawAHorizontalTab(i, page_ptr, x, y, TRUE, TRUE);
         } else {
            w = DrawAHorizontalTab(i, page_ptr, x, y, FALSE, FALSE);
         }
         x += w;
      }
   }
   x = 1 + (scrollBarW<<2);
   for (i=1, page_ptr=firstPage; page_ptr != NULL;
         page_ptr=page_ptr->next, i++) {
      if (i >= pageWindowFirstIndex) {
         int w=0;

         if (page_ptr == curPage) {
            w = DrawAHorizontalTab(i, page_ptr, x, y, TRUE, FALSE);
            break;
         } else {
            w = DrawAHorizontalTab(i, page_ptr, x, y, FALSE, TRUE);
         }
         x += w;
      }
   }
   XSetFont(mainDisplay, defaultGC, defaultFontPtr->fid);

   for (i=0, x=brdrW; i < 4; i++, x += scrollBarW) {
      XSetTSOrigin(mainDisplay, rasterGC, x, brdrW);
      XSetStipple(mainDisplay, rasterGC, scrollPixmap[i]);
      XFillRectangle(mainDisplay, pageWindow, rasterGC,
            x, brdrW, scrollBarW, scrollBarW);
   }
   XSetTSOrigin(mainDisplay, rasterGC, 0, 0);
}

static
void HandleClickInPageWindow(button_ev)
   XButtonEvent *button_ev;
{
   int i, index=(int)(button_ev->x / scrollBarW), total, x, offset;
   struct PageRec *page_ptr;

   if (button_ev->x <= 0) {
      index = 0;
   } else {
      index = (int)((button_ev->x-1) / scrollBarW);
   }
   switch (index) {
   case SCROLL_LEFTEND:
      pageWindowFirstIndex = 1;
      RedrawPageWindow();
      break;
   case SCROLL_LEFT:
      if (pageWindowFirstIndex > 1) {
         pageWindowFirstIndex--;
         RedrawPageWindow();
      }
      break;
   case SCROLL_RIGHT:
      if (pageWindowFirstIndex < lastPageNum) {
         pageWindowFirstIndex++;
         RedrawPageWindow();
      }
      break;
   case SCROLL_RIGHTEND:
      total = pageWindowW-(scrollBarW<<2)-1;
      pageWindowFirstIndex = lastPageNum;
      for (i=lastPageNum, page_ptr=lastPage; page_ptr != NULL;
            page_ptr=page_ptr->prev, i--) {
         int w=DrawAHorizontalTab(i, page_ptr, 0, 0, FALSE, TRUE);

         total -= w;
         if (total > 0) {
            pageWindowFirstIndex = i;
         } else {
            break;
         }
      }
      RedrawPageWindow();
      break;
   default:
      offset = button_ev->x-(scrollBarW<<2)-1;
      for (i=1, page_ptr=firstPage; page_ptr != NULL;
            page_ptr=page_ptr->next, i++) {
         if (i >= pageWindowFirstIndex) {
            int w=DrawAHorizontalTab(i, page_ptr, 0, 0, FALSE, TRUE);

            if (w >= offset) {
               int start=(((rulerFontWidth<<1)-7)>>1)+rulerFontWidth;

               if (page_ptr != curPage) {
                  SetCurPage(i);
               }
               break;
            }
            offset -= w;
         }
      }
      break;
   }
}

static struct MouseStatusStrRec pageMouseStatus[] = {
   { "Shift All Tabs Right", "Page menu", "(none)" },
   { "Shift Tabs Right", "Page menu", "(none)" },
   { "Shift Tabs Left", "Page menu", "(none)" },
   { "Shift All Tabs Left", "Page menu", "(none)" },
   { NULL, NULL, NULL }
};

static
void HandleMotionInPageWindow(motion_ev)
   XMotionEvent *motion_ev;
{
   int i, index, total, x, offset;
   struct PageRec *page_ptr;

   if (motion_ev->x <= 0) {
      index = 0;
   } else {
      index = (int)((motion_ev->x-1) / scrollBarW);
   }
   switch (index) {
   case SCROLL_LEFTEND:
   case SCROLL_LEFT:
   case SCROLL_RIGHT:
   case SCROLL_RIGHTEND:
      SetMouseStatus(pageMouseStatus[index].l, pageMouseStatus[index].m,
            pageMouseStatus[index].r);
      break;
   default:
      offset = motion_ev->x-(scrollBarW<<2)-1;
      for (i=1, page_ptr=firstPage; page_ptr != NULL;
            page_ptr=page_ptr->next, i++) {
         if (i >= pageWindowFirstIndex) {
            int w=DrawAHorizontalTab(i, page_ptr, 0, 0, FALSE, TRUE);

            if (w >= offset) {
               int start=(((rulerFontWidth<<1)-7)>>1)+rulerFontWidth;

               if (page_ptr->name == NULL) {
                  sprintf(gszMsgBox, "Goto page %1d", i);
               } else {
                  sprintf(gszMsgBox, "Goto page %1d: \"%s\"", i,
                        page_ptr->name);
               }
               SetStringStatus(gszMsgBox);
               break;
            }
            offset -= w;
         }
      }
      break;
   }
}

void PageEventHandler(input)
   XEvent *input;
{
   XEvent ev;

   if (input->type == Expose) {
      while (XCheckWindowEvent(mainDisplay, pageWindow, ExposureMask, &ev)) ;
      RedrawPageWindow();
   } else if (input->type == EnterNotify) {
      SetMouseStatus("", "", "");
   } else if (input->type == MotionNotify) {
      while (XCheckWindowEvent(mainDisplay,pageWindow,PointerMotionMask,&ev)) ;
      if (pageLayoutMode == PAGE_TILE) {
         SetMouseStatus("(none)", "Page Menu", "(none)");
      } else {
         HandleMotionInPageWindow(&input->xmotion);
      }
   } else if (input->type == ButtonPress) {
      XButtonEvent *button_ev=(&(input->xbutton));

      if (pageLayoutMode == PAGE_TILE) {
         if (button_ev->button == Button2) {
            PageMenu(button_ev->x_root, button_ev->y_root, FALSE);
         }
      } else {
         if (button_ev->button == Button1) {
            HandleClickInPageWindow(button_ev);
         } else if (button_ev->button == Button2) {
            PageMenu(button_ev->x_root, button_ev->y_root, FALSE);
         }
      }
   }
}

void RedrawPageDummyWindow()
{
}

void PageDummyEventHandler(input)
   XEvent *input;
{
   if (input->type == Expose) {
      XEvent ev;

      while (XCheckWindowEvent(mainDisplay,pageDummyWindow,ExposureMask,&ev)) ;
      RedrawPageDummyWindow();
   } else if (input->type == EnterNotify) {
      SetMouseStatus("(none)", "(none)", "(none)");
   }
}

static
void FreePage(page_ptr)
   struct PageRec *page_ptr;
{
   topObj = page_ptr->top;
   botObj = page_ptr->bot;
   DelAllObj();
   if (page_ptr->name != NULL) free(page_ptr->name);
   free(page_ptr);
   topObj = NULL;
   botObj = NULL;
}

static
void InitPageOnlyOnce()
{
   static int initialized=FALSE;
   char *c_ptr;

   if (initialized) return;
   initialized = TRUE;

   if (PRTGIF) return;
}

void InitPage ()
   /* given lastPageNum, allocate enough pages */
{
   int	i;

   InitPageOnlyOnce();
   while (firstPage != NULL) {
      curPage = firstPage->next;
      FreePage(firstPage);
      firstPage = curPage;
   }
   firstPage = lastPage = curPage = NULL;
   for (i=1; i <= lastPageNum; i++) {
      curPage = (struct PageRec *)malloc(sizeof(struct PageRec));
      if (curPage == NULL) FailAllocMessage();
      memset(curPage, 0, sizeof(struct PageRec));
      curPage->layer_on = TRUE;
      curPage->name = NULL;
      curPage->top = curPage->bot = topObj = botObj = NULL;
      curPage->next = NULL;
      curPage->prev = lastPage;
      curPage->draw_orig_x = drawOrigX;
      curPage->draw_orig_y = drawOrigY;
      curPage->zoom_scale = zoomScale;
      curPage->zoomed_in = zoomedIn;
      if (firstPage == NULL)
         firstPage = curPage;
      else
         lastPage->next = curPage;
      lastPage = curPage;
   }
   curPageNum = (lastPageNum > 0 ? 1 : 0);
   curPage = (lastPageNum > 0 ? firstPage : NULL);
}

void GotoPageNum (new_page_num)
   int	new_page_num;
{
   int			i=1;
   struct PageRec	* page_ptr;

   curPage->draw_orig_x = drawOrigX;
   curPage->draw_orig_y = drawOrigY;
   curPage->zoom_scale = zoomScale;
   curPage->zoomed_in = zoomedIn;

   for (page_ptr = firstPage; page_ptr != NULL; page_ptr = page_ptr->next, i++)
      if (i == new_page_num)
         break;
   curPageNum = new_page_num;
   curPage = page_ptr;
   topObj = curPage->top;
   botObj = curPage->bot;
   if (curPage->draw_orig_x != drawOrigX || curPage->draw_orig_y != drawOrigY ||
         curPage->zoom_scale != zoomScale || curPage->zoomed_in != zoomedIn)
   {
      if (!PRTGIF) AdjSplineVs();
      if (!PRTGIF) AdjCaches();
      curPage->draw_orig_x = drawOrigX;
      curPage->draw_orig_y = drawOrigY;
      curPage->zoom_scale = zoomScale;
      curPage->zoomed_in = zoomedIn;
   }
}

void SetCurPage (page_number)
   int	page_number;
{
   if (pageLayoutMode == PAGE_TILE) return;

   MakeQuiescent ();
   PrepareToRecord (CMD_GOTO_PAGE, NULL, NULL, curPageNum);
   GotoPageNum (page_number);
   RecordCmd (CMD_GOTO_PAGE, NULL, NULL, NULL, curPageNum);
   ClearAndRedrawDrawWindow ();
   RedrawTitleWindow ();
   ShowPage ();
}

void NextPage ()
{
   if (pageLayoutMode == PAGE_TILE)
   {
      MsgBox ("Can not do NextPage() in TILED page mode.", TOOL_NAME, INFO_MB);
      return;
   }
   if (curPageNum == lastPageNum)
   {
      MsgBox ("Already at last page.", TOOL_NAME, INFO_MB);
      return;
   }
   SetCurPage (curPageNum+1);
}

void PrevPage ()
{
   if (pageLayoutMode == PAGE_TILE)
   {
      MsgBox ("Can not do PrevPage() in TILED page mode.", TOOL_NAME, INFO_MB);
      return;
   }
   if (curPageNum == 1)
   {
      MsgBox ("Already at first page.", TOOL_NAME, INFO_MB);
      return;
   }
   SetCurPage (curPageNum-1);
}

static XComposeStatus	c_stat;
static int		leadingChars=(-1);
static char		formatStr[20];
static DspList		* pageNameDspPtr=NULL;

static
DspList *PageNameListing(pn_entries)
   int *pn_entries;
{
   register int		i;
   DspList		* dsp_ptr;
   struct PageRec	* page_ptr;

   for (leadingChars=1, i=lastPageNum; ;leadingChars++, i /= 10)
      if (i < 10)
         break;

   sprintf (formatStr, "%%%1dd %%s", leadingChars++);
   pageNameDspPtr = (DspList*)malloc(lastPageNum*sizeof(DspList));
   if (pageNameDspPtr == NULL) FailAllocMessage();
   memset(pageNameDspPtr, 0, lastPageNum*sizeof(DspList));
   for (i = 1, dsp_ptr = pageNameDspPtr, page_ptr = firstPage; i <= lastPageNum;
         i++, dsp_ptr++, page_ptr = page_ptr->next)
   {
      sprintf (gszMsgBox, formatStr, i,
            ((page_ptr->name == NULL) ? "" : page_ptr->name));
      UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr), gszMsgBox);
      dsp_ptr->directory = FALSE;
      dsp_ptr->next = ((i == lastPageNum) ? NULL : &dsp_ptr[1]);
   }
   *pn_entries = lastPageNum;
   return (pageNameDspPtr);
}

static
int EditOrSelectPageNames(TopStr, Which, entries, num_entries)
   char *TopStr, **entries;
   int Which; /* either PAGE_GOTO or PAGE_NAME */
   int num_entries;
{
   int marked_index=curPageNum-1, rc=INVALID;
   char win_name[128];

   ResetNamesInfo();
   NamesSetTitle(TopStr);
   NamesAddButton("OK", BUTTON_OK);
   NamesAddButton("CANCEL", BUTTON_CANCEL);
   NamesSetEntries(NULL, entries, num_entries, TRUE, marked_index,
         leadingChars);
   if (Which==PAGE_NAME) {
      /* ignore double-click and <CR> */
      NamesSetDefaultBtnId(INVALID, INVALID);
      sprintf(win_name, "%s - Name Pages", TOOL_NAME);
      NamesSetStyle(NAMES_EDIT_NAME, NAMES_LOOP_ONCE);
      if (Names(win_name, NULL, NULL, 0, NULL) == BUTTON_OK) {
         rc = TRUE;
      }
   } else {
      int selected_index=INVALID;

      sprintf(win_name, "%s - Goto Page", TOOL_NAME);
      NamesSetStyle(NAMES_COMPLEX_SELECT_NAME, NAMES_LOOP_ONCE);
      if (Names(win_name, &selected_index, NULL, 0, NULL) != INVALID) {
         return selected_index;
      }
   }
   return rc;
}

static
int BlankStr (s)
   register char	* s;
{
   while (*s == ' ') s++;
   return (*s == '\0');
}

void NamePages()
{
   int i, num_entries=0;
   DspList *dsp_ptr;
   char **entries=NULL;

   if (pageLayoutMode == PAGE_TILE) {
      MsgBox("Can not do NamePages() in TILED page mode.", TOOL_NAME, INFO_MB);
      return;
   }
   MakeQuiescent();

   dsp_ptr = PageNameListing(&num_entries);
   ignoreDirectoryFlag = TRUE;
   entries = MakeNameDspItemArray(num_entries, dsp_ptr);
   ignoreDirectoryFlag = FALSE;
   if (EditOrSelectPageNames("Edit page names...", PAGE_NAME, entries,
         num_entries)) {
      int modified=FALSE;
      struct PageRec *page_ptr;

      for (page_ptr=firstPage, i=0; page_ptr!=NULL;
            page_ptr=page_ptr->next, i++) {
         int blank_str=BlankStr(&(entries[i])[leadingChars]);

         if (page_ptr->name == NULL) {
            if (!blank_str) {
               modified = TRUE;
               page_ptr->name = (char*)malloc(
                     (strlen(&(entries[i])[leadingChars])+1)*sizeof(char));
               if (page_ptr->name == NULL) FailAllocMessage();
               strcpy(page_ptr->name, &(entries[i])[leadingChars]);
            }
         } else {
            if (blank_str || strcmp(page_ptr->name,
                  &(entries[i])[leadingChars]) != 0) {
               modified = TRUE;
               free(page_ptr->name);
               if (blank_str) {
                  page_ptr->name = NULL;
               } else {
                  page_ptr->name = (char*)malloc(
                        (strlen(&(entries[i])[leadingChars])+1)*sizeof(char));
                  if (page_ptr->name == NULL) FailAllocMessage();
                  strcpy(page_ptr->name, &(entries[i])[leadingChars]);
               }
            }
         }
      }
      if (modified) {
         SetFileModified(TRUE);
         RedrawTitleWindow();
      }
   }
   free(dsp_ptr);
   free(*entries);
   free(entries);
   Msg("");
}

void GotoPage()
{
   int num_entries=0;
   DspList *dsp_ptr;
   char **entries=NULL;
   int index;

   if (pageLayoutMode == PAGE_TILE) {
      MsgBox("Can not do GotoPage() in TILED page mode.", TOOL_NAME, INFO_MB);
      return;
   }
   MakeQuiescent();

   dsp_ptr = PageNameListing(&num_entries);
   ignoreDirectoryFlag = TRUE;
   entries = MakeNameDspItemArray(num_entries, dsp_ptr);
   ignoreDirectoryFlag = FALSE;

   index = EditOrSelectPageNames("Goto page...", PAGE_GOTO, entries,
         num_entries);

   free(dsp_ptr);
   free(*entries);
   free(entries);

   if (index++ == (-1)) {
      Msg("No page selected.");
      return;
   }
   if (index < 1 || index > lastPageNum) {
      sprintf(gszMsgBox, "Invalid page number '%1d'.", index);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (index == curPageNum) {
      sprintf(gszMsgBox, "Already at page %1d.", curPageNum);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   SetCurPage(index);

   sprintf(gszMsgBox, "Current page is page %1d.", curPageNum);
   Msg(gszMsgBox);
}

#define BEFORE_CUR_PAGE 0
#define AFTER_CUR_PAGE 1

static
void AddPage(AfterCurPage)
   int AfterCurPage;
{
   int n;
   struct PageRec *page_ptr;
   struct AttrRec *attr_ptr;

   if (pageLayoutMode == PAGE_TILE) {
      MsgBox("Can not add a page in TILED page mode.", TOOL_NAME, INFO_MB);
      return;
   }
   if (firstCmd != NULL) {
      sprintf(gszMsgBox, "Adding a page %s page %1d can not be undone.  %s",
            (AfterCurPage ? "after" : "before"), curPageNum,
            "Ok to proceed? [ync](y)");
      if (MsgBox(gszMsgBox, TOOL_NAME, YNC_MB) != MB_ID_YES) return;
      CleanUpCmds();
   }
   n = (AfterCurPage ? curPageNum : curPageNum-1);

   MakeQuiescent();

   for (curPageNum=1, curPage=firstPage; curPageNum <= n; curPageNum++) {
      curPage=curPage->next;
   }
   page_ptr = (struct PageRec *)malloc(sizeof(struct PageRec));
   if (page_ptr == NULL) FailAllocMessage();
   memset(page_ptr, 0, sizeof(struct PageRec));
   page_ptr->layer_on = TRUE;
   page_ptr->top = page_ptr->bot = topObj = botObj = NULL;
   page_ptr->next = curPage;
   page_ptr->name = NULL;
   page_ptr->draw_orig_x = drawOrigX;
   page_ptr->draw_orig_y = drawOrigY;
   page_ptr->zoom_scale = zoomScale;
   page_ptr->zoomed_in = zoomedIn;
   if (curPage == NULL) {
      page_ptr->prev = lastPage;
      lastPage->next = page_ptr;
      lastPage = page_ptr;
   } else {
      page_ptr->prev = curPage->prev;
      if (curPage->prev == NULL) {
         firstPage = page_ptr;
      } else {
         curPage->prev->next = page_ptr;
      }
      curPage->prev = page_ptr;
   }
   curPage = page_ptr;
   lastPageNum++;
   topObj = curPage->top;
   botObj = curPage->bot;

   ClearAndRedrawDrawWindow();
   RedrawTitleWindow();
   SetFileModified(TRUE);
   ShowPage();
   if ((attr_ptr=FindAttrWithName(tgifObj, "template=", NULL)) != NULL) {
      importingFile = TRUE;
      if (ImportGivenFile(attr_ptr->attr_value.s)) {
         sprintf(gszMsgBox, "Template loaded: '%s'.", attr_ptr->attr_value.s);
         Msg(gszMsgBox);
      } else {
         sprintf(gszMsgBox, "Problem loading template file '%s'.",
               attr_ptr->attr_value.s);
         Msg(gszMsgBox);
      }
      importingFile = FALSE;
   }
   sprintf(gszMsgBox, "Page %1d added.", curPageNum);
   Msg(gszMsgBox);
}

void AddPageBefore ()
{
   AddPage (BEFORE_CUR_PAGE);
}

void AddPageAfter ()
{
   AddPage (AFTER_CUR_PAGE);
}

void DeleteCurPage ()
{
   int			n;
   struct PageRec	* page_ptr;

   if (pageLayoutMode == PAGE_TILE)
   {
      MsgBox ("Can not delete a page in TILED page mode.", TOOL_NAME, INFO_MB);
      return;
   }
   if (lastPageNum == 1)
   {
      MsgBox ("Can not delete (the only) page.", TOOL_NAME, INFO_MB);
      return;
   }
   if (firstCmd != NULL || topObj != NULL)
   {
      sprintf (gszMsgBox, "Deleting page %1d can not be undone.  %s",
            curPageNum, "Ok to proceed? [ync](y)");
      if (MsgBox (gszMsgBox, TOOL_NAME, YNC_MB) != MB_ID_YES) return;
      CleanUpCmds ();
   }
   n = curPageNum;
   SetFileModified (TRUE);

   MakeQuiescent ();

   if (curPage == firstPage)
   {
      page_ptr = firstPage = curPage->next;
      firstPage->prev = NULL;
   }
   else if (curPage == lastPage)
   {
      page_ptr = lastPage = curPage->prev;
      lastPage->next = NULL;
      curPageNum--;
   }
   else
   {
      curPage->next->prev = curPage->prev;
      curPage->prev->next = curPage->next;
      page_ptr = curPage->next;
   }
   FreePage(curPage);
   curPage = page_ptr;
   lastPageNum--;
   topObj = curPage->top;
   botObj = curPage->bot;

   ClearAndRedrawDrawWindow ();
   RedrawTitleWindow ();
   ShowPage ();

   sprintf (gszMsgBox, "Page %1d deleted.", n);
   Msg (gszMsgBox);
}

void TogglePageLineShown ()
{
   if (pageLayoutMode == PAGE_STACK)
   {
      MsgBox ("Can not toggle page line shown in STACKED page mode.",
            TOOL_NAME, INFO_MB);
      return;
   }
   pageLineShownInTileMode = !pageLineShownInTileMode;
   ClearAndRedrawDrawWindow ();
}

static
int JustSpecifyDrawingSize ()
{
   char	spec[MAXSTRING+1], * c_ptr, * part_1;
   int	cols=0, rows=0;

   if (Dialog ("Please enter drawing size specification: [W x H]",
         "( <CR>: accept, <ESC>: cancel )", spec) == INVALID)
      return (FALSE);
   if ((part_1=strtok(spec, " ,xX")) == NULL) return (FALSE);
   if ((c_ptr=strtok(NULL, " ,xX")) == NULL)
   {
      sprintf (gszMsgBox, "Invalid drawing size specification.\n\n%s.",
            "Must specify both Width and Height");
      MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
      return (FALSE);
   }
   else
   {
      cols = atoi (part_1);
      rows = atoi (c_ptr);
   }
   if (cols > 0 && rows > 0 && (cols*rows >= lastPageNum))
   {
      if (cols*rows < lastPageNum)
      {
         sprintf (gszMsgBox,
               "Invalid drawing size:  W times H should be >= %1d",
               lastPageNum);
         MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
      }
      else
      {
         paperCol = cols;
         paperRow = rows;
         return (TRUE);
      }
   }
   else if (cols == 0 || rows == 0)
   {
      sprintf (gszMsgBox, "Invalid drawing size specification.\n\n%s.",
            "Must specify both Width and Height");
      MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
      return (FALSE);
   }
   else if (cols*rows < lastPageNum)
   {
      sprintf (gszMsgBox, "Invalid drawing size:  W times H should be >= %1d",
            lastPageNum);
      MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
   }
   else
      MsgBox ("Invalid drawing size specified.", TOOL_NAME, INFO_MB);
   return (FALSE);
}

void SpecifyDrawingSize ()
{
   if (pageLayoutMode != PAGE_TILE)
   {
      MsgBox ("Can only do SpecifyDrawingSize() in TILED page mode.",
            TOOL_NAME, INFO_MB);
      return;
   }
   if (JustSpecifyDrawingSize ())
   {
      UpdPageStyle (pageStyle);
      RedrawScrollBars ();
      UpdDrawWinBBox ();
      AdjSplineVs ();
      ClearAndRedrawDrawWindow ();

      ShowPage ();
   }
}

void PrintOnePage ()
{
   int	x, y;

   if (whereToPrint == XBM_FILE)
   {
      sprintf (gszMsgBox, "Can not do PrintOnePage() in %s format.",
            colorDump ? "XPM" : "XBM");
      MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   MakeQuiescent ();

   switch (pageLayoutMode)
   {
      case PAGE_TILE:
         TwoLineMsg ("Left button selects a page to print,",
               "    other buttons cancel printing one page.");
         SetMouseStatus ("Select a page to print", "Cancel", "Cancel");
         if (PickAPoint (&x, &y, handCursor) != Button1)
            Msg ("Operation canceled by the user.");
         else if (x>=0 && x<ZOOMED_SIZE(drawWinW) && y>=0 &&
               y<ZOOMED_SIZE(drawWinH))
         {
            int	abs_x=ABS_X(x), abs_y=ABS_Y(y);
            int	col = (int)(abs_x / onePageWidth);
            int	row = (int)(abs_y / onePageHeight);

            if (col >= paperCol || row >= paperRow)
               MsgBox ("Must select a point on the paper.", TOOL_NAME, INFO_MB);
            else
               DumpOnePageInTileMode (row, col);
         }
         else
            MsgBox ("Must select a point from the drawing canvas.",
                  TOOL_NAME, INFO_MB);
         break;
      case PAGE_STACK:
         DumpOnePageInStackMode ();
         break;
   }
}

#ifdef __hpux
extern double   atof ARGS_DECL((const char *));
#else /* !__hpux */
extern double   atof ARGS_DECL((char *));
#endif /* __hpux */

static
int GetDimension (Spec, NumPixels)
   char	* Spec;
   int	* NumPixels;
{
   char		* c_ptr;
   double	val;

   if ((c_ptr=strstr(Spec, "in")) != NULL ||
         (c_ptr=strstr(Spec, "In")) != NULL ||
         (c_ptr=strstr(Spec, "IN")) != NULL)
   {
      *c_ptr = '\0';
      val = atof (Spec) * ((double)PIX_PER_INCH);
      *NumPixels = round(val);
      if (*NumPixels <= 0) return FALSE;
   }
   else if ((c_ptr=strstr(Spec, "cm")) != NULL ||
         (c_ptr=strstr(Spec, "Cm")) != NULL ||
         (c_ptr=strstr(Spec, "CM")) != NULL)
   {
      *c_ptr = '\0';
      val = atof (Spec) * ((double)ONE_CM);
      *NumPixels = round(val);
      if (*NumPixels <= 0) return FALSE;
   }
   else if (sscanf(Spec, "%d", NumPixels) != 1)
   {
      return FALSE;
   }
   return TRUE;
}

int SetPaperSize(Spec)
   char *Spec;
{
   int ok=TRUE;
   char *c_ptr;

   UtilTrimBlanks(Spec);
   if (UtilStrICmp(Spec, "letter") == 0) {
      onePageWidth = (85*PIX_PER_INCH)/10;
      onePageHeight = 11*PIX_PER_INCH;
      SetPSPageWidthHeight();
   } else if (UtilStrICmp(Spec, "legal") == 0) {
      onePageWidth = (85*PIX_PER_INCH)/10;
      onePageHeight = 14*PIX_PER_INCH;
      SetPSPageWidthHeight();
   } else if (UtilStrICmp(Spec, "a4") == 0) {
      onePageWidth = (825*PIX_PER_INCH)/100;
      onePageHeight = (117*PIX_PER_INCH)/10;
      SetPSPageWidthHeight();
   } else if ((c_ptr=strstr(Spec, " x ")) == NULL &&
         (c_ptr=strstr(Spec, " X ")) == NULL) {
      ok = FALSE;
   } else {
      int w_in_pixel, h_in_pixel;

      *c_ptr = '\0';
      UtilTrimBlanks(&c_ptr[3]);
      if (GetDimension(Spec, &w_in_pixel) &&
            GetDimension(&c_ptr[3], &h_in_pixel)) {
         onePageWidth = w_in_pixel;
         onePageHeight = h_in_pixel;
         SetPSPageWidthHeight();
      } else {
         ok = FALSE;
      }
   }
   if (ok) {
      if (mainWindow != None) {
         sprintf(gszMsgBox,
               "New paper size: %.2fin x %.2fin (%.2fcm x %.2fcm).",
               (float)(((float)onePageWidth)/((float)PIX_PER_INCH)),
               (float)(((float)onePageHeight)/((float)PIX_PER_INCH)),
               (float)(((float)onePageWidth)/((float)ONE_CM)),
               (float)(((float)onePageHeight)/((float)ONE_CM)));
         Msg(gszMsgBox);
      }
   } else {
      if (mainWindow == None) {
         sprintf(gszMsgBox, "Invalid %s*InitialPaperSize: '%s'.", TOOL_NAME,
               Spec);
         fprintf(stderr, "%s\n", gszMsgBox);
      } else {
         sprintf(gszMsgBox, "Invalid paper size specification: '%s'.", Spec);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      }
   }
   return ok;
}

void SpecifyPaperSize ()
{
   char	spec[MAXSTRING+1];

   MakeQuiescent ();
   if (Dialog ("Please enter physical paper size: [W x H] (e.g., 8.5in x 11in)",
         "( <CR>: accept, <ESC>: cancel )", spec) == INVALID) {
      return;
   }
   if (SetPaperSize(spec)) {
      UpdPageStyle (pageStyle);
      SetFileModified (TRUE);
      ClearAndRedrawDrawWindow ();
   }
}

void DeletePages()
{
   char spec[MAXSTRING+1], *dup_spec, *c_ptr;
   int i, *mark_to_delete, ok=TRUE, num_to_delete=0;

   if (pageLayoutMode == PAGE_TILE) {
      MsgBox("Can not delete pages in TILED page mode.", TOOL_NAME, INFO_MB);
      return;
   }
   if (lastPageNum == 1) {
      MsgBox("Can not delete (the only) page.", TOOL_NAME, INFO_MB);
      return;
   }
   MakeQuiescent();

   *spec = '\0';
   sprintf(gszMsgBox, "%s: (%s)",
         "Please specify pages to delete", "e.g., 2,3,7-9,10");
   Dialog(gszMsgBox, NULL, spec);
   UtilTrimBlanks(spec);
   if (*spec == '\0') return;
   if ((mark_to_delete=(int*)malloc(lastPageNum*sizeof(int))) == NULL) {
      FailAllocMessage();
      return;
   }
   if ((dup_spec=UtilStrDup(spec)) == NULL) {
      free(mark_to_delete);
      FailAllocMessage();
      return;
   }
   for (i=0; i < lastPageNum; i++) mark_to_delete[i] = FALSE;
   for (c_ptr=strtok(dup_spec, " ,\t"); ok && c_ptr != NULL;
         c_ptr=strtok(NULL, " ,\t")) {
      char *dash_ptr=strchr(c_ptr, '-');

      if (dash_ptr == NULL) {
         int page_num=atoi(c_ptr);

         if (page_num > 0 && page_num <= lastPageNum) {
            if (!mark_to_delete[page_num-1]) num_to_delete++;
            mark_to_delete[page_num-1] = TRUE;
         } else {
            sprintf(gszMsgBox, "Page %1d is out of range.", page_num);
            MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
            ok = FALSE;
         }
      } else {
         int start_page_num, end_page_num;

         *dash_ptr = '\0';
         start_page_num = atoi(c_ptr);
         end_page_num = atoi(&dash_ptr[1]);
         *dash_ptr = '-';
         if (start_page_num <= 0 || start_page_num > lastPageNum) {
            sprintf(gszMsgBox, "Page %1d is out of range.", start_page_num);
            MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
            ok = FALSE;
         } else if (end_page_num <= 0 || end_page_num > lastPageNum) {
            sprintf(gszMsgBox, "Page %1d is out of range.", end_page_num);
            MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
            ok = FALSE;
         } else if (start_page_num > end_page_num) {
            sprintf(gszMsgBox, "Malformed specification: '%s'.", spec);
            MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
            ok = FALSE;
         } else {
            for (i=start_page_num; i <= end_page_num; i++) {
               if (!mark_to_delete[i-1]) num_to_delete++;
               mark_to_delete[i-1] = TRUE;
            }
         }
      }
   }
   if (ok && num_to_delete == lastPageNum) {
      MsgBox("Can not delete all pages.", TOOL_NAME, INFO_MB);
   } else if (ok && num_to_delete > 0) {
      sprintf(gszMsgBox, "Deleting page%s can not be undone.  %s",
            (num_to_delete > 1 ? "s" : ""), "Ok to proceed? [ync](y)");
      if (firstCmd == NULL ||
            MsgBox(gszMsgBox, TOOL_NAME, YNC_MB) == MB_ID_YES) {
         struct PageRec *page_ptr;
         int page_num;

         for (page_num=lastPageNum; page_num > 0; page_num--) {
            if (mark_to_delete[page_num-1]) {
               GotoPageNum(page_num);
               if (curPage == firstPage) {
                  page_ptr = firstPage = curPage->next;
                  firstPage->prev = NULL;
               } else if (curPage == lastPage) {
                  page_ptr = lastPage = curPage->prev;
                  lastPage->next = NULL;
                  curPageNum--;
               } else {
                  curPage->next->prev = curPage->prev;
                  curPage->prev->next = curPage->next;
                  page_ptr = curPage->next;
               }
               FreePage(curPage);
               curPage = page_ptr;
               lastPageNum--;
               topObj = curPage->top;
               botObj = curPage->bot;
            }
         }
         CleanUpCmds();
         ClearAndRedrawDrawWindow();
         RedrawTitleWindow();
         ShowPage();

         sprintf(gszMsgBox, "%1d page%s deleted.", num_to_delete,
               (num_to_delete > 1 ? "s" : ""));
         Msg(gszMsgBox);
         SetFileModified(TRUE);
      }
   }
   free(dup_spec);
   free(mark_to_delete);
}

void PrintOneFilePerPage ()
{
   if (whereToPrint == XBM_FILE || whereToPrint == PRINTER) {
      if (whereToPrint == XBM_FILE) {
         sprintf(gszMsgBox, "Can not do PrintOneFilePerPage() in %s format.",
               colorDump ? "XPM" : "XBM");
      } else {
         sprintf(gszMsgBox, "Can not do PrintOneFilePerPage() to printer.");
      }
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   MakeQuiescent();
   DumpOneFilePerPage();
}

void PageSubMenu (index)
   int	index;
{
   switch (pageLayoutMode)
   {
      case PAGE_STACK:
         switch (index)
         {
            case PAGE_NEXT: NextPage (); break;
            case PAGE_PREV: PrevPage (); break;
            case PAGE_NAME: NamePages (); break;
            case PAGE_GOTO: GotoPage (); break;
            case PAGE_ADDBEFORE: AddPageBefore (); break;
            case PAGE_ADDAFTER: AddPageAfter (); break;
            case PAGE_DEL: DeleteCurPage (); break;
            case PAGE_PRINT_ONE_IN_STACK: PrintOnePage (); break;
            case PAGE_PAPER_SIZE_IN_STACK: SpecifyPaperSize (); break;
            case PAGE_DEL_MANY: DeletePages (); break;
            case PAGE_PRINT_ONE_FILE_PER_PAGE: PrintOneFilePerPage (); break;
         }
         break;
      case PAGE_TILE:
         switch (index)
         {
            case PAGE_TOGGLELINE: TogglePageLineShown (); break;
            case PAGE_SIZING: SpecifyDrawingSize (); break;
            case PAGE_PRINT_ONE: PrintOnePage (); break;
            case PAGE_PAPER_SIZE: SpecifyPaperSize (); break;
         }
         break;
   }
   UpdateSubMenu (MENU_PAGE);
}

int PageMenu (X, Y, TrackMenubar)
   int	X, Y, TrackMenubar;
{
   int	index=INVALID, * fore_colors, * valid, * init_rv;

   switch (pageLayoutMode)
   {
      case PAGE_STACK:
         DefaultColorArrays (MAXPAGESTACKMENUS, &fore_colors, &valid, &init_rv,
               NULL);
         activeMenu = MENU_PAGE;
         index = TextMenuLoop (X, Y, pageStackMenuStr, MAXPAGESTACKMENUS,
               fore_colors, valid, init_rv, pageStackMenuDescription,
               SINGLECOLOR, TrackMenubar);
         break;
      case PAGE_TILE:
         DefaultColorArrays (MAXPAGETILEMENUS, &fore_colors, &valid, &init_rv,
               NULL);
         activeMenu = MENU_PAGE;
         index = TextMenuLoop (X, Y, pageTileMenuStr, MAXPAGETILEMENUS,
               fore_colors, valid, init_rv, pageTileMenuDescription,
               SINGLECOLOR, TrackMenubar);
         break;
   }

   if (index >= 0)
      if (!multiMenu)
         PageSubMenu (index);
   return (index);
}

void DelAllPages ()
{
   register struct PageRec	* next_page=NULL;

   for (curPage = firstPage; curPage != NULL; curPage = next_page)
   {
      next_page = curPage->next;
      FreePage(curPage);
   }
   lastPageNum = 0;
   firstPage = lastPage = curPage = NULL;
   InitPage ();
}

static
int AnyObjOnPageBoundary ()
{
   register struct ObjRec	* obj_ptr;
   int				ltx, lty, rbx, rby, next_page_x, next_page_y;

   for (obj_ptr = topObj; obj_ptr != NULL; obj_ptr = obj_ptr->next)
   {
      ltx = obj_ptr->obbox.ltx; lty = obj_ptr->obbox.lty;
      rbx = obj_ptr->obbox.rbx; rby = obj_ptr->obbox.rby;
      if (ltx < 0 || lty < 0 || rbx >= onePageWidth*paperCol ||
            rby >= onePageHeight*paperRow)
      {
         sprintf (gszMsgBox, "%s.\n\n%s.",
               "There are objects outside the page boundary",
               "Can not switch to TILED mode");
         MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
         return (TRUE);
      }
      next_page_x = ((ltx % onePageWidth) == 0) ?
            onePageWidth * (round(ltx / onePageWidth) + 1) :
            onePageWidth * (((int)(ltx / onePageWidth)) + 1);
      next_page_y = ((lty % onePageHeight) == 0) ?
            onePageHeight * (round(lty / onePageHeight) + 1) :
            onePageHeight * (((int)(lty / onePageHeight)) + 1);
      if (rbx >= next_page_x || rby >= next_page_y)
      {
         sprintf (gszMsgBox, "%s.\n\n%s.",
               "There are objects on the page boundary",
               "Can not switch to TILED mode");
         MsgBox (gszMsgBox, TOOL_NAME, INFO_MB);
         return (TRUE);
      }
   }
   return (FALSE);
}

static
int CalcStackPageNum (obj_ptr)
   struct ObjRec	* obj_ptr;
{
   int	ltx, lty, col, row;

   ltx = obj_ptr->obbox.ltx; lty = obj_ptr->obbox.lty;
   col = ((ltx % onePageWidth) == 0) ? round(ltx / onePageWidth) + 1 :
         ((int)(ltx / onePageWidth)) + 1;
   row = ((lty % onePageHeight) == 0) ? round(lty / onePageHeight) + 1 :
         ((int)(lty / onePageHeight)) + 1;
   return ((row-1) * paperCol + col);
}

static
void TileToStack ()
{
   register int			i;
   register struct ObjRec	* obj_ptr, * prev_obj;
   int				* dx=NULL, * dy=NULL, index;
   struct PageRec		* * page_handle=NULL;
   struct ObjRec		* saved_top_obj, * saved_bot_obj;

   saved_top_obj=curPage->top;
   saved_bot_obj=curPage->bot;
   curPage->top = curPage->bot = NULL;
   DelAllPages ();

   firstPage = lastPage = curPage = NULL;
   lastPageNum = paperCol * paperRow;
   InitPage ();

   page_handle = (struct PageRec **)malloc(lastPageNum*sizeof(struct PageRec*));
   if (page_handle == NULL) FailAllocMessage();
   dx = (int*)malloc(lastPageNum*sizeof(int));
   dy = (int*)malloc(lastPageNum*sizeof(int));
   if (dx == NULL || dy == NULL) FailAllocMessage();
   for (curPage=firstPage, i=0; curPage != NULL; curPage=curPage->next, i++) {
      dx[i] = (i % paperCol) * onePageWidth;
      dy[i] = (((i % paperCol) == 0) ? round(i / paperCol):
            ((int)(i / paperCol))) * onePageHeight;
      page_handle[i] = curPage;
   }

   for (obj_ptr=saved_bot_obj; obj_ptr != NULL; obj_ptr = prev_obj)
   {
      prev_obj = obj_ptr->prev;
      index = CalcStackPageNum(obj_ptr)-1;
      curPage = page_handle[index];
      AddObj (NULL, curPage->top, obj_ptr);
      MoveObj (obj_ptr, -(dx[index]), -(dy[index]));
   }
   curPage = firstPage;
   topObj = curPage->top;
   botObj = curPage->bot;
   curPageNum = 1;
   paperCol = paperRow = 1;
   free(dx);
   free(dy);
   free(page_handle);
}

static
void StackToTile ()
{
   register struct ObjRec	* obj_ptr, * prev_obj;
   int				i, dx, dy;
   struct PageRec		* saved_first_page, * saved_last_page;
   struct PageRec		* tmp_page, * page_ptr;

   if (paperCol * paperRow < lastPageNum && !JustSpecifyDrawingSize ()) return;

   saved_first_page = firstPage;
   saved_last_page = lastPage;
   firstPage = lastPage = NULL;
   lastPageNum = 1;
   InitPage ();
   tmp_page = firstPage;

   i = 0;
   for (page_ptr = saved_first_page; page_ptr != NULL;
         page_ptr = page_ptr->next, i++)
   {
      dx = (i % paperCol) * onePageWidth;
      dy = (((i % paperCol) == 0) ? round(i / paperCol):
            ((int)(i / paperCol))) * onePageHeight;
      for (obj_ptr = page_ptr->bot; obj_ptr != NULL; obj_ptr = prev_obj)
      {
         prev_obj = obj_ptr->prev;
         AddObj (NULL, topObj, obj_ptr);
         MoveObj (obj_ptr, dx, dy);
      }
      page_ptr->top = page_ptr->bot = NULL;
   }
   firstPage = saved_first_page;
   lastPage = saved_last_page;
   DelAllPages ();
   firstPage = lastPage = curPage = tmp_page;
   curPageNum = lastPageNum = 1;
   pageLineShownInTileMode = TRUE;
   topObj = curPage->top;
   botObj = curPage->bot;
}

static
int OkToFlushUndoBuffer ()
{
   if (firstCmd != NULL)
   {
      sprintf (gszMsgBox, "%s.  %s? [ync](y)",
            "Changing the page layout mode can not be undone", "Ok to proceed");
      if (MsgBox (gszMsgBox, TOOL_NAME, YNC_MB) == MB_ID_YES)
      {
         CleanUpCmds ();
         return (TRUE);
      }
      return (FALSE);
   }
   return (TRUE);
}

static
int PreservePageNames ()
{
   struct PageRec	* page_ptr;

   for (page_ptr = firstPage; page_ptr != NULL; page_ptr = page_ptr->next)
      if (page_ptr->name != NULL && *page_ptr->name != '\0')
         break;
   if (page_ptr != NULL)
   {
      sprintf (gszMsgBox, "%s  %s [ync](y)",
            "Switching from TILED to STACKED mode loses all page names.",
            "Ok to proceed?" );
      if (MsgBox (gszMsgBox, TOOL_NAME, YNC_MB) != MB_ID_YES)
         return (FALSE);
   }
   return (TRUE);
}

void PageLayoutSubMenu (index)
   int	index;
{
   MakeQuiescent ();
   if (pageLayoutMode != index)
   {
      switch (index)
      {
         case PAGE_STACK:
            if (AnyObjOnPageBoundary ()) return;
            if (!OkToFlushUndoBuffer ()) return;

            pageLayoutMode = index;
            TileToStack ();
            Msg ("Page mode is STACKED.");
            break;
         case PAGE_TILE:
            if (!PreservePageNames ()) return;
            if (!JustSpecifyDrawingSize ()) return;
            if (!OkToFlushUndoBuffer ()) return;

            pageLayoutMode = index;
            StackToTile ();
            Msg ("Page mode is TILED.");
            break;
      }
      ShowPage ();
      ShowPageLayout ();
      UpdateSubMenu (MENU_PAGELAYOUT);
      DestroySubMenu (MENU_PAGE);

      UpdPageStyle (pageStyle);
      RedrawScrollBars ();
      UpdDrawWinBBox ();
      AdjSplineVs ();
      ClearAndRedrawDrawWindow ();
      RedrawTitleWindow ();
      SetFileModified (TRUE);
   }
}

static char * pageLayoutMenuDescription[] =
{
   "Stacked page mode",
   "Tiled page mode",
   NULL
};

int PageLayoutMenu (X, Y, TrackMenubar)
   int	X, Y, TrackMenubar;
{
   int	index, * fore_colors, * valid, * init_rv;

   DefaultColorArrays (MAXPAGELAYOUTMODES, &fore_colors, &valid, &init_rv,
         NULL);
   free(valid);
   init_rv[pageLayoutMode] = TRUE;
   activeMenu = MENU_PAGELAYOUT;
   index = PxMpMenuLoop (X, Y, choiceImageW, choiceImageH, MAXPAGELAYOUTMODES,
         1, MAXPAGELAYOUTMODES, fore_colors, pageLayoutPixmap, init_rv,
         pageLayoutMenuDescription, SINGLECOLOR, TrackMenubar);

   if (index >= 0)
      if (!multiMenu)
         PageLayoutSubMenu (index);
   return (index);
}

void CleanUpPage ()
{
   DelAllPages ();
}

static int savedPageLayoutMode, savedPaperCol, savedPaperRow;
static int savedCurPageNum, savedLastPageNum;
static struct PageRec *savedFirstPage, *savedLastPage, *savedCurPage;

void PushPageInfo ()
{
   savedPageLayoutMode = pageLayoutMode;
   savedPaperCol = paperCol;
   savedPaperRow = paperRow;
   savedFirstPage = firstPage;
   savedLastPage = lastPage;
   savedCurPage = curPage;
   savedCurPageNum = curPageNum;
   savedLastPageNum = lastPageNum;
}

void PopPageInfo ()
{
   pageLayoutMode = savedPageLayoutMode;
   paperCol = savedPaperCol;
   paperRow = savedPaperRow;
   firstPage = savedFirstPage;
   lastPage = savedLastPage;
   curPage = savedCurPage;
   curPageNum = savedCurPageNum;
   lastPageNum = savedLastPageNum;
}
