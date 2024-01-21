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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/choose.c,v 3.0 1996/05/06 16:04:05 william Exp $";
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
#endif
#endif
#endif
#endif
#endif
#endif
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "const.h"
#include "types.h"

#include "button.e"
#ifndef _NO_EXTERN
#include "choose.e"
#endif
#include "cutpaste.e"
#include "cursor.e"
#include "dialog.e"
#include "file.e"
#include "font.e"
#include "mainloop.e"
#include "mainmenu.e"
#include "msg.e"
#include "names.e"
#include "raster.e"
#include "setup.e"
#include "util.e"

#ifndef XK_KP_Left
#define XK_KP_Left	0xFF96
#define XK_KP_Up	0xFF97
#define XK_KP_Right	0xFF98
#define XK_KP_Down	0xFF99
#endif /* ~XK_KP_LEFT */

typedef struct NamesRec {
   Window base_win, dsp_win, scroll_win;
   int base_win_w, base_win_h, dsp_win_w, dsp_win_h;
   int dsp_area_w, dsp_area_h, scroll_area_h;

   char **entries;
   int num_entries;
   int first_index;
   int marked_index;
   int num_btns;
   struct BBRec btn_bbox[MAXBUTTONS], path_bbox;
   char btn_str[MAXBUTTONS][8];
   int btn_id[MAXBUTTONS];

   char inbuf[512];
   int buf_index, just_clicked;

   DspList *dsp_ptr;
   char *title_str;

   int a_btn_w, title_w, graph_w, btn_start, title_start, graph_start;
   int btn_w, btn_gap, btn_selected, exposed, screen_w, screen_h;
   int edit_style, loop_once, dont_free_entries, leading;

   int change_to_root, pop_from_root, faking_dot_dot;
   int tabbed_from_root, just_tabbed_from_root;
   int def_btn_id, double_click_btn_id;

   GetEntriesFunc *pf_before_loop;
   AfterLoopFunc *pf_after_loop;

   /* used only when edit_style== NAMES_SELECT_FILE */
   char dir_name[MAXPATHLENGTH+1], saved_dir_name[MAXPATHLENGTH+1];
   char name[MAXPATHLENGTH+1], saved_name[MAXPATHLENGTH+1];
   int name_index, remote_file;
} *NamesRecPtr;

static struct NamesRec namesInfo;

static GC nameGC=(GC)0, revNameGC=(GC)0;

static XComposeStatus c_stat;

static
void CalcBaseWinWidth(pni)
   struct NamesRec *pni;
{
   int max_w=max(max(pni->title_w,pni->graph_w),pni->btn_w);

   pni->base_win_w = max_w + (defaultFontWidth<<2);
   pni->title_start = ((pni->base_win_w-pni->title_w)>>1);
   pni->graph_start = ((pni->base_win_w-pni->graph_w)>>1);
   pni->btn_start = ((pni->base_win_w-pni->btn_w)>>1);

   pni->path_bbox.ltx = pni->graph_start;
   if (boldMsgFontPtr == NULL) {
      pni->path_bbox.lty = 3*ROW_HEIGHT;
   } else {
      pni->path_bbox.lty = ((ROW_HEIGHT<<1)+boldMsgFontHeight+1);
   }
   pni->path_bbox.lty -= 2;
   pni->path_bbox.rbx = pni->path_bbox.ltx +
         ITEM_LEN*defaultFontWidth+6;
   pni->path_bbox.rby = pni->path_bbox.lty +
         (msgFontPtr==NULL ? defaultFontHeight : msgFontHeight)+1+4;
}

void NamesSetTitle(title_str)
   char *title_str;
{
   struct NamesRec *pni=(&namesInfo);
   int len;

   if (pni->title_str != NULL) free(pni->title_str);
   pni->title_str = UtilStrDup(title_str);
   if (pni->title_str == NULL) FailAllocMessage();
   len = strlen(pni->title_str);

   if (boldMsgFontPtr == NULL) {
      pni->title_w = defaultFontWidth * len;
   } else {
      pni->title_w = XTextWidth(boldMsgFontPtr, pni->title_str, len);
   }
   CalcBaseWinWidth(pni);
}

void ResetNamesInfo()
{
   struct NamesRec *pni=(&namesInfo);

   if (pni->title_str != NULL) free(pni->title_str);
   if (!pni->dont_free_entries) {
      if (pni->dsp_ptr != NULL) free(pni->dsp_ptr);
      if (pni->entries != NULL) {
         free(*(pni->entries));
         free(pni->entries);
      }
   }

   memset(&namesInfo, 0, sizeof(struct NamesRec));

   pni->num_entries = INVALID;
   pni->first_index = INVALID;
   pni->marked_index = INVALID;
   pni->a_btn_w = ButtonWidth("OK",8);
   pni->btn_gap = defaultFontWidth;
   pni->screen_w = DisplayWidth(mainDisplay, mainScreen);
   pni->screen_h = DisplayHeight(mainDisplay, mainScreen);
   pni->dsp_area_w = ITEM_LEN * defaultFontWidth;
   pni->dsp_area_h = ITEM_DSPED * ROW_HEIGHT;
   pni->dsp_win_w = pni->dsp_area_w + (brdrW<<1);
   pni->dsp_win_h = pni->dsp_area_h + (brdrW<<1);
   pni->scroll_area_h = pni->dsp_area_h;
   pni->graph_w = pni->dsp_area_w + scrollBarW + (brdrW<<2);
   NamesSetTitle("");
   if (boldMsgFontPtr == NULL) {
      pni->base_win_h = (8+ITEM_DSPED)*ROW_HEIGHT;
   } else {
      pni->base_win_h = (6+ITEM_DSPED)*ROW_HEIGHT + 2*(boldMsgFontHeight+1);
   }
   pni->def_btn_id = BUTTON_OK;
   pni->double_click_btn_id = BUTTON_OK;
   pni->tabbed_from_root = TRUE;
}

void InitNamesInfo()
{
   XGCValues values;

   memset(&namesInfo, 0, sizeof(struct NamesRec));

   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.fill_style = FillSolid;
   if (msgFontPtr == NULL) {
      values.font = defaultFontPtr->fid;
   } else {
      values.font = msgFontPtr->fid;
   }
   nameGC = XCreateGC(mainDisplay, rootWindow,
         GCForeground | GCBackground | GCFillStyle | GCFont, &values);

   values.foreground = myBgPixel;
   values.background = myFgPixel;
   revNameGC = XCreateGC(mainDisplay, rootWindow,
         GCForeground | GCBackground | GCFillStyle | GCFont, &values);
}

void CleanUpNamesInfo()
{
   struct NamesRec *pni=(&namesInfo);

   ResetNamesInfo();
   if (pni->title_str != NULL) free(pni->title_str);
   pni->title_str = NULL;
   XFreeGC(mainDisplay, nameGC);
   XFreeGC(mainDisplay, revNameGC);
   nameGC = revNameGC = (GC)0;
}

void NamesAddButton(btn_str, btn_id)
   char *btn_str;
   int btn_id;
{
   struct NamesRec *pni=(&namesInfo);

   pni->btn_id[pni->num_btns] = btn_id;
   strcpy(pni->btn_str[pni->num_btns], btn_str);
   pni->btn_w += pni->a_btn_w;
   if (pni->num_btns != 0) {
      pni->btn_w += pni->btn_gap;
   }
   pni->num_btns++;
   CalcBaseWinWidth(pni);
}

static
void RedrawNameScrollWin()
{
   struct NamesRec *pni=(&namesInfo);
   double frac, start_frac;
   int block_h, block_start;
   XGCValues values;

   start_frac = (pni->num_entries > 0) ?
         (double)((double)(pni->first_index) /
         (double)(pni->num_entries)) : ((double)0.0);
   /* starting pixel */
   block_start = (int)(pni->scroll_area_h * start_frac);

   if (pni->num_entries > ITEM_DSPED) {
      frac = (double)((double)ITEM_DSPED / (double)(pni->num_entries));
   } else {
      frac = 1.0;
   }
   if (pni->first_index+ITEM_DSPED >= pni->num_entries) {
      block_h = pni->scroll_area_h - block_start;
   } else {
      block_h = (int)(pni->scroll_area_h * frac);
   }
   values.foreground = myBgPixel;
   values.background = myFgPixel;
   values.function = GXcopy;
   values.fill_style = FillSolid;
   XChangeGC(mainDisplay, patGC,
         GCForeground | GCBackground | GCFunction | GCFillStyle, &values);
   XFillRectangle(mainDisplay, pni->scroll_win, patGC, 0, 0,
         scrollBarW, pni->scroll_area_h);

   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.fill_style = FillOpaqueStippled;
   values.stipple = patPixmap[SCROLLPAT];
   XChangeGC(mainDisplay, patGC,
         GCForeground | GCBackground | GCFillStyle | GCStipple, &values);
   XFillRectangle(mainDisplay, pni->scroll_win, patGC, 0, block_start,
         scrollBarW, block_h);
}

static
void RedrawDspWindow()
{
   struct NamesRec *pni=(&namesInfo);
   int i, top, end;

   if (msgFontPtr == NULL) {
      top = defaultFontAsc+1;
   } else {
      top = msgFontAsc+1;
   }
   if (pni->first_index+ITEM_DSPED > pni->num_entries) {
      end = pni->num_entries;
   } else {
      end = pni->first_index + ITEM_DSPED;
   }
   XFillRectangle(mainDisplay, pni->dsp_win, revNameGC, 0, 0,
         ITEM_LEN*defaultFontWidth, ITEM_DSPED*ROW_HEIGHT);

   for (i=pni->first_index; i < end; i++) {
      int len=strlen(pni->entries[i]);

      if (i == pni->marked_index) {
         XFillRectangle(mainDisplay, pni->dsp_win, nameGC, 0,
               (i-pni->first_index)*ROW_HEIGHT, ITEM_LEN*defaultFontWidth,
               ROW_HEIGHT);
         XDrawString(mainDisplay, pni->dsp_win, revNameGC, 0,
               (i-pni->first_index)*ROW_HEIGHT+top, pni->entries[i],
               len);
      } else {
         XFillRectangle(mainDisplay, pni->dsp_win, revNameGC, 0,
               (i-pni->first_index)*ROW_HEIGHT, ITEM_LEN*defaultFontWidth,
               ROW_HEIGHT);
         XDrawString(mainDisplay, pni->dsp_win, nameGC, 0,
               (i-pni->first_index)*ROW_HEIGHT+top, pni->entries[i],
               len);
      }
   }
}

static
void RedrawNamePath()
{
   struct NamesRec *pni=(&namesInfo);
   int len=strlen(pni->inbuf), cursor_x=0, cursor_y, box_w, box_h, x, y;
   char *c_ptr;

   if (msgFontPtr == NULL) {
      XSetFont(mainDisplay, nameGC, defaultFontPtr->fid);
   } else {
      XSetFont(mainDisplay, nameGC, msgFontPtr->fid);
   }
   x = pni->graph_start;
   if (boldMsgFontPtr == NULL) {
      y = 3*ROW_HEIGHT;
   } else {
      y = ((ROW_HEIGHT<<1)+boldMsgFontHeight+1);
   }
   box_w = ITEM_LEN*defaultFontWidth+6;
   cursor_y = y;
   if (msgFontPtr == NULL) {
      y += defaultFontAsc;
   } else {
      y += msgFontAsc;
   }
   box_h = (ROW_HEIGHT+3);
   XClearArea(mainDisplay, pni->base_win, x, cursor_y-2, box_w, box_h, False);
   XDrawRectangle(mainDisplay, pni->base_win, nameGC, x, cursor_y-2,
         box_w, box_h);

   if (msgFontPtr == NULL) {
      if (len > ITEM_LEN) {
         c_ptr = &(pni->inbuf[len-ITEM_LEN]);
         len = ITEM_LEN;
      } else {
         c_ptr = pni->inbuf;
      }
      cursor_x = x+2+len*defaultFontWidth;
   } else {
      int index=0;

      cursor_x = XTextWidth(msgFontPtr, pni->inbuf, len);
      while (cursor_x > box_w-3) {
         index++;
         cursor_x = XTextWidth(msgFontPtr, &pni->inbuf[index], len-index);
      }
      c_ptr = &pni->inbuf[index];
      len -= index;
      cursor_x += x+2+1;
   }
   XDrawString(mainDisplay, pni->base_win, nameGC, x+2, y+1, c_ptr, len);
   XDrawLine(mainDisplay, pni->base_win, nameGC, cursor_x, cursor_y+1,
         cursor_x, cursor_y+box_h-5);
}

static
void RedrawNameBaseWindow()
{
   struct NamesRec *pni=(&namesInfo);
   int i, left, base_line;

   /* draw a frame around the window */
   XDrawRectangle(mainDisplay, pni->base_win, nameGC, 0, 0,
         pni->base_win_w-1, pni->base_win_h-1);
   if (boldMsgFontPtr == NULL) {
      base_line = defaultFontAsc+ROW_HEIGHT;
      XSetFont(mainDisplay, nameGC, defaultFontPtr->fid);
   } else {
      base_line = boldMsgFontAsc+ROW_HEIGHT;
      XSetFont(mainDisplay, nameGC, boldMsgFontPtr->fid);
   }
   /* draw the title */
   if (pni->tabbed_from_root) {
      XDrawString(mainDisplay, pni->base_win, nameGC,
            pni->title_start, base_line, pni->title_str,
            strlen(pni->title_str));
   } else {
      int tmp_w, len, tmp_start;

      strcpy(gszMsgBox, "Type <TAB> for file completion..");
      len = strlen(gszMsgBox);
      if (boldMsgFontPtr == NULL) {
         tmp_w = defaultFontWidth * len;
      } else {
         tmp_w = XTextWidth(boldMsgFontPtr, gszMsgBox, len);
      }
      tmp_start = ((pni->base_win_w-tmp_w)>>1);
      XDrawString(mainDisplay, pni->base_win, nameGC,
            tmp_start, base_line, gszMsgBox, len);
   }
   if (boldMsgFontPtr != NULL) {
      if (msgFontPtr == NULL) {
         XSetFont(mainDisplay, nameGC, defaultFontPtr->fid);
      } else {
         XSetFont(mainDisplay, nameGC, msgFontPtr->fid);
      }
   }
   RedrawNamePath();

   /* draw the buttons */
   left = pni->btn_start;
   for (i=0; i < pni->num_btns; i++) {
      if (boldMsgFontPtr == NULL) {
         pni->btn_bbox[i].lty = (6+ITEM_DSPED)*ROW_HEIGHT;
      } else {
         pni->btn_bbox[i].lty = (5+ITEM_DSPED)*ROW_HEIGHT +
               (boldMsgFontHeight+1);
      }
      pni->btn_bbox[i].lty -= 2;
      pni->btn_bbox[i].ltx = left;
      DisplayButton(pni->base_win, pni->btn_str[i], 8,
            &(pni->btn_bbox[i]), BUTTON_NORMAL);
      left = pni->btn_bbox[i].rbx + 1 + defaultFontWidth;
   }
}

static
void RedrawNamesWindow()
{
   RedrawNameBaseWindow();
   RedrawNameScrollWin();
   RedrawDspWindow();
}

static
int ExposeOrMapNames(input)
   XEvent *input;
{
   struct NamesRec *pni=(&namesInfo);
   XEvent ev;

   if ((input->type==MapNotify && input->xany.window==pni->base_win) ||
         (input->type==Expose && (input->xany.window==pni->base_win ||
         input->xany.window==pni->scroll_win ||
         input->xany.window==pni->dsp_win)) || (!pni->exposed &&
         (XCheckWindowEvent(mainDisplay, pni->base_win, ExposureMask,
         &ev) || XCheckWindowEvent(mainDisplay, pni->scroll_win,
         ExposureMask, &ev) || XCheckWindowEvent(mainDisplay, pni->dsp_win,
         ExposureMask, &ev) || XCheckWindowEvent(mainDisplay,
         pni->base_win, StructureNotifyMask, &ev)))) {
      while (XCheckWindowEvent(mainDisplay, pni->base_win,
            ExposureMask, &ev)) ;
      while (XCheckWindowEvent(mainDisplay, pni->scroll_win,
            ExposureMask, &ev)) ;
      while (XCheckWindowEvent(mainDisplay, pni->dsp_win,
            ExposureMask, &ev)) ;
      while (XCheckWindowEvent(mainDisplay, pni->base_win,
            StructureNotifyMask, &ev)) ;

      RedrawNamesWindow();

      pni->exposed = TRUE;
      XSync(mainDisplay, False);

      if ((input->type==MapNotify &&
            input->xany.window==pni->base_win) ||
            (input->type==Expose &&
            (input->xany.window==pni->base_win ||
            input->xany.window==pni->scroll_win ||
            input->xany.window==pni->dsp_win))) {
         return TRUE;
      }
   }
   return FALSE;
}

static
int CreateNamesWindows(win_name)
   char *win_name;
{
   struct NamesRec *pni=(&namesInfo);
   int x, y, base_win_x, base_win_y;
   XWMHints wmhints;
   XSizeHints sizehints;
   XSetWindowAttributes win_attrs;

   base_win_x = (pni->base_win_w > pni->screen_w) ? 0 :
         ((pni->screen_w-pni->base_win_w)>>1);
   base_win_y = (pni->base_win_h > pni->screen_h) ? 0 :
         ((pni->screen_h-pni->base_win_h)/3);

   if ((pni->base_win=XCreateSimpleWindow(mainDisplay, rootWindow,
         base_win_x, base_win_y, pni->base_win_w, pni->base_win_h,
         brdrW, myBorderPixel, myBgPixel)) == 0) {
      fprintf(stderr, "XCreateSimpleWindow(): fail to create a window.\n");
      return FALSE;
   }
   XDefineCursor(mainDisplay, pni->base_win, defaultCursor);

   if (boldMsgFontPtr == NULL) {
      y = 5*ROW_HEIGHT;
   } else {
      y = ((ROW_HEIGHT<<2)+boldMsgFontHeight+1);
   }
   if ((pni->dsp_win=XCreateSimpleWindow(mainDisplay, pni->base_win,
         pni->graph_start, y, pni->dsp_area_w, pni->dsp_area_h,
         brdrW, myBorderPixel, myBgPixel)) == 0) {
      XDestroyWindow(mainDisplay, pni->base_win);
      fprintf(stderr, "XCreateSimpleWindow(): fail to create a window.\n");
      return FALSE;
   }
   if ((pni->scroll_win=XCreateSimpleWindow(mainDisplay,
         pni->base_win, pni->graph_start+pni->dsp_win_w,
         y, scrollBarW, pni->dsp_area_h,
         brdrW, myBorderPixel, myBgPixel)) == 0) {
      XDestroyWindow(mainDisplay, pni->base_win);
      fprintf(stderr, "XCreateSimpleWindow(): fail to create a window.\n");
      return FALSE;
   }
   win_attrs.save_under = True;
   win_attrs.colormap = mainColormap;
   XChangeWindowAttributes(mainDisplay, pni->base_win,
         CWSaveUnder | CWColormap, &win_attrs);

   wmhints.flags = InputHint | StateHint;
   wmhints.input = True;
   wmhints.initial_state = NormalState;
   XSetWMHints(mainDisplay, pni->base_win, &wmhints);

   sizehints.flags = PPosition | PSize | USPosition | PMinSize | PMaxSize;
   sizehints.x = base_win_x;
   sizehints.y = base_win_y;
   sizehints.width = sizehints.min_width = sizehints.max_width =
         pni->base_win_w;
   sizehints.height = sizehints.min_height = sizehints.max_height =
         pni->base_win_h;
#ifdef NOTR4MODE
   XSetNormalHints(mainDisplay, pni->base_win, &sizehints);
#else
   XSetWMNormalHints(mainDisplay, pni->base_win, &sizehints);
#endif
   XStoreName(mainDisplay, pni->base_win, win_name);

   XSetTransientForHint(mainDisplay, pni->base_win, mainWindow);

#ifdef MAPBEFORESELECT
   XMapWindow(mainDisplay, pni->base_win);
   XSelectInput(mainDisplay, pni->base_win,
         KeyPressMask | ButtonPressMask | ExposureMask | StructureNotifyMask);
   XMapWindow(mainDisplay, pni->dsp_win);
   XSelectInput(mainDisplay, pni->dsp_win,
         KeyPressMask | ButtonPressMask | ExposureMask);
   XMapWindow(mainDisplay, pni->scroll_win);
   XSelectInput(mainDisplay, pni->scroll_win,
         KeyPressMask | ButtonPressMask | ExposureMask);
#else
   XSelectInput(mainDisplay, pni->base_win,
         KeyPressMask | ButtonPressMask | ExposureMask | StructureNotifyMask);
   XMapWindow(mainDisplay, pni->base_win);
   XSelectInput(mainDisplay, pni->dsp_win,
         KeyPressMask | ButtonPressMask | ExposureMask);
   XMapWindow(mainDisplay, pni->dsp_win);
   XSelectInput(mainDisplay, pni->scroll_win,
         KeyPressMask | ButtonPressMask | ExposureMask);
   XMapWindow(mainDisplay, pni->scroll_win);
#endif

   if (warpToWinCenter) {
      XWarpPointer(mainDisplay, None, pni->base_win, 0, 0, 0, 0,
            (pni->base_win_w>>1), (pni->base_win_h>>1));
   }
   XSync(mainDisplay, False);

   Msg("");

   return TRUE;
}

static
void NamesUpdateIndices()
{
   struct NamesRec *pni=(&namesInfo);

   pni->first_index = 0;
   if (pni->marked_index == INVALID) {
      if (pni->edit_style != NAMES_SELECT_FILE) {
         *pni->inbuf = '\0';
         pni->buf_index = 0;
      }
   } else {
      if (pni->marked_index >= ITEM_DSPED) {
         if (pni->marked_index < pni->num_entries-ITEM_DSPED) {
            pni->first_index = pni->marked_index;
         } else {
            pni->first_index = pni->num_entries-ITEM_DSPED;
         }
      }
      if (pni->edit_style != NAMES_SELECT_FILE) {
         strcpy(pni->inbuf, &(pni->entries[pni->marked_index])[pni->leading]);
         pni->buf_index = strlen(pni->inbuf);;
      }
   }
}

static
int IsFirstEqChar(s, index)
   char *s;
   int index;
{
   s = (&s[index]);
   if (*s != '=') return FALSE;
   for (--index, --s; index >= 0; index--, s--) {
      if (*s == '=') {
         return FALSE;
      }
   }
   return TRUE;
}

static
int SetMarkedIndex()
{
   struct NamesRec *pni=(&namesInfo);
   int i;

   for (i=0; i < pni->num_entries; i++) {
      if (strncmp(&(pni->entries[i])[pni->leading],
            pni->name, pni->name_index) == 0) {
         break;
      }
   }
   if (i < pni->num_entries) {
      if (i < pni->first_index) {
         pni->first_index = i;
      } else if (i >= pni->first_index+ITEM_DSPED) {
         if (i < pni->num_entries-ITEM_DSPED) {
            pni->first_index = i;
         } else {
            pni->first_index = pni->num_entries-ITEM_DSPED;
         }
      }
      pni->marked_index = i;
      return TRUE;
   } else {
      return FALSE;
   }
}

static
void BackSpaceInNames(pn_changing, pn_selected_btn_index)
   int *pn_changing, *pn_selected_btn_index;
{
   struct NamesRec *pni=(&namesInfo);
   int i;

   switch (pni->edit_style) {
   case NAMES_SIMPLE_SELECT_NAME: return;
   case NAMES_COMPLEX_SELECT_NAME:
      if (pni->marked_index == INVALID || pni->buf_index <= 0) return;
      pni->inbuf[--pni->buf_index] = '\0';
      if (pni->buf_index != 0) {
         int i;

         for (i=0; i < pni->num_entries; i++) {
            if (strncmp(&(pni->entries[i])[pni->leading],
                  pni->inbuf, pni->buf_index) == 0) {
               break;
            }
         }
         if (i < pni->num_entries) {
            if (i < pni->first_index) {
               pni->first_index = i;
            } else if (i >= pni->first_index+ITEM_DSPED) {
               if (i < pni->num_entries-ITEM_DSPED) {
                  pni->first_index = i;
               } else {
                  pni->first_index = pni->num_entries-ITEM_DSPED;
               }
            }
            pni->marked_index = i;
         }
      } else {
         pni->first_index = 0;
         pni->marked_index = INVALID;
      }
      break;
   case NAMES_SELECT_FILE:
      if (pni->name_index > 0) {
         pni->name[--pni->name_index] = '\0';
         sprintf(pni->inbuf, "%s/%s", pni->dir_name, pni->name);
         pni->buf_index = strlen(pni->inbuf);
         if (pni->tabbed_from_root && pni->name_index != 0) {
            for (i=0; i < pni->num_entries; i++) {
               if (strncmp(&(pni->entries[i])[pni->leading],
                     pni->name, pni->name_index) == 0) {
                  pni->marked_index = i;
                  break;
               }
            }
         } else {
            pni->marked_index = INVALID;
         }
      } else if (pni->change_to_root && *pni->dir_name == '\0' &&
            *pni->saved_dir_name != '\0') {
         pni->pop_from_root = TRUE;
         strcpy(pni->dir_name, pni->saved_dir_name);
         *pni->saved_dir_name = '\0';
         pni->tabbed_from_root = TRUE;
         pni->change_to_root = FALSE;
         sprintf(pni->inbuf, "%s/%s", pni->dir_name, pni->name);
         pni->buf_index = strlen(pni->inbuf);
         *pn_changing = FALSE;
         *pn_selected_btn_index = INVALID;
      } else {
         /* fake as if "../" is selected */
         strcpy(pni->name, "../");
         pni->name_index = strlen(pni->name);
         for (i=0; i < pni->num_entries; i++) {
            if (strncmp(&(pni->entries[i])[pni->leading],
                  pni->name, pni->name_index) == 0) {
               pni->marked_index = i;
               pni->faking_dot_dot = TRUE;
               *pn_changing = FALSE;
               *pn_selected_btn_index = i;
               break;
            }
         }
      }
      break;
   case NAMES_EDIT_ATTR:
      if (pni->marked_index == INVALID || pni->buf_index <= 0) return;
      if (pni->dsp_ptr == NULL ||
            !pni->dsp_ptr[pni->marked_index].directory ||
            !IsFirstEqChar(pni->inbuf, pni->buf_index-1)) {
         pni->inbuf[--pni->buf_index] = '\0';
         strcpy(&(pni->entries[pni->marked_index])[pni->leading],
               pni->inbuf);
      }
      break;
   case NAMES_EDIT_NAME:
      if (pni->marked_index == INVALID || pni->buf_index <= 0) return;
      pni->inbuf[--pni->buf_index] = '\0';
      strcpy(&(pni->entries[pni->marked_index])[pni->leading],
            pni->inbuf);
      break;
   default: break;
   }
   if (pni->exposed) {
      RedrawNamePath();
      RedrawDspWindow();
   }
}

static
void ParseFileName(FullName, DirName, Name)
   char *FullName, *DirName, *Name;
{
   int i, len;

   len = strlen(FullName);
   strcpy(DirName, FullName);
   for (i=len-1; i>=0; i--) {
      if (DirName[i] == '/') {
         strcpy(Name, &(DirName[i+1]));
         DirName[i] = '\0';
         return;
      }
   }
   *DirName = *Name = '\0';
}

static
void TabInNames()
{
   struct NamesRec *pni=(&namesInfo);

   if (pni->edit_style == NAMES_SELECT_FILE) {
      pni->tabbed_from_root = TRUE;
      pni->just_tabbed_from_root = TRUE;
      if (!FileIsRemote(pni->name)) {
         /* saved_name is used for just_tabbed_from_root */
         sprintf(pni->inbuf, "%s/%s", pni->dir_name, pni->name);
         pni->buf_index = strlen(pni->inbuf);
         strcpy(pni->saved_name, pni->name);
         ParseFileName(pni->inbuf, pni->dir_name, pni->name);
      } else {
         pni->remote_file = TRUE;
      }
   }
}

static
void CharInNames(buf, pn_changing, pn_selected_btn_index)
   char *buf;
   int *pn_changing, *pn_selected_btn_index;
{
   struct NamesRec *pni=(&namesInfo);
   int i=INVALID;

   switch (pni->edit_style) {
   case NAMES_SIMPLE_SELECT_NAME: return;

   case NAMES_COMPLEX_SELECT_NAME:
      if (buf[0] == '$') {
         i = pni->num_entries-1;
         strcpy(pni->inbuf, &(pni->entries[i])[pni->leading]);
      } else if (buf[0] == '^') {
         i = 0;
         strcpy(pni->inbuf, &(pni->entries[i])[pni->leading]);
      } else {
         pni->inbuf[pni->buf_index++] = buf[0];
         pni->inbuf[pni->buf_index] = '\0';
         for (i=0; i < pni->num_entries; i++) {
            if (strncmp(&(pni->entries[i])[pni->leading],
                  pni->inbuf, pni->buf_index) == 0) {
               break;
            }
         }
      }
      if (i < pni->num_entries) {
         if (i < pni->first_index) {
            pni->first_index = i;
         } else if (i >= pni->first_index+ITEM_DSPED) {
            if (i < pni->num_entries-ITEM_DSPED) {
               pni->first_index = i;
            } else {
               pni->first_index = pni->num_entries-ITEM_DSPED;
            }
         }
         pni->marked_index = i;
      } else {
         pni->inbuf[--pni->buf_index] = '\0';
      }
      break;
   case NAMES_SELECT_FILE:
      if (!pni->tabbed_from_root) {
         pni->name[pni->name_index++] = buf[0];
         pni->name[pni->name_index] = '\0';
         sprintf(pni->inbuf, "%s/%s", pni->dir_name, pni->name);
         pni->buf_index = strlen(pni->inbuf);
      } else if (buf[0] == '$') {
         i = pni->num_entries-1;
         strcpy(pni->name, &(pni->entries[i])[pni->leading]);
         pni->name_index = strlen(pni->name);
      } else if (buf[0] == '^') {
         i = 0;
         strcpy(pni->name, &(pni->entries[i])[pni->leading]);
         pni->name_index = strlen(pni->name);
#ifdef apollo
      } else if (pni->name_index == 0 && buf[0] == '/' &&
            *pni->dir_name == '\0') {
         strcpy(pni->dir_name, "/");
         pni->name[pni->name_index++] = buf[0];
         i = pni->num_entries;
         strcpy(pni->inbuf, "//");
#endif /* apollo */
      } else if (pni->name_index == 0 && buf[0] == '/') {
         if (*pni->saved_dir_name == '\0') {
            strcpy(pni->saved_dir_name, pni->dir_name);
            pni->num_entries = 0;
         }
         *pn_changing = FALSE;
         pni->change_to_root = TRUE;
         pni->tabbed_from_root = FALSE;
         *pn_selected_btn_index = INVALID;
         *pni->dir_name = *pni->name = '\0';
         pni->name_index = 0;
         sprintf(pni->inbuf, "%s/%s", pni->dir_name, pni->name);
         pni->buf_index = strlen(pni->inbuf);
         break;
      } else {
         pni->name[pni->name_index++] = buf[0];
         pni->name[pni->name_index] = '\0';
         for (i=0; i < pni->num_entries; i++) {
            if (strncmp(&(pni->entries[i])[pni->leading],
                  pni->name, pni->name_index) == 0) {
               break;
            }
         }
      }
      if (pni->num_entries > 0) {
         if (i < pni->num_entries) {
            if (i < pni->first_index) {
               pni->first_index = i;
            } else if (i >= pni->first_index+ITEM_DSPED) {
               if (i < pni->num_entries-ITEM_DSPED) {
                  pni->first_index = i;
               } else {
                  pni->first_index = pni->num_entries-ITEM_DSPED;
               }
            }
            pni->marked_index = i;
            sprintf(pni->inbuf, "%s/%s", pni->dir_name, pni->name);
            pni->buf_index = strlen(pni->inbuf);
            if (buf[0] == '/') {
               *pn_changing = FALSE;
               *pn_selected_btn_index = INVALID;
            }
         } else {
            pni->name[--pni->name_index] = '\0';
         }
      }
      break;
   case NAMES_EDIT_ATTR:
      if (pni->marked_index == INVALID) return;
      pni->inbuf[pni->buf_index++] = buf[0];
      pni->inbuf[pni->buf_index] = '\0';
      strcpy(&(pni->entries[pni->marked_index])[pni->leading],
            pni->inbuf);
      break;
   case NAMES_EDIT_NAME:
      if (pni->marked_index == INVALID) return;
      pni->inbuf[pni->buf_index++] = buf[0];
      pni->inbuf[pni->buf_index] = '\0';
      strcpy(&(pni->entries[pni->marked_index])[pni->leading],
            pni->inbuf);
      break;
   default: break;
   }
   if (pni->exposed) {
      RedrawNamePath();
      RedrawDspWindow();
   }
}

static
int GetNameEntryNum(RowOffset)
   int RowOffset;
{
   struct NamesRec *pni=(&namesInfo);
   int index;

   index = pni->first_index + RowOffset;
   if (index >= pni->num_entries) return INVALID;
   return index;
}

static
void NameScrollHandler(button_ev)
   XButtonEvent *button_ev;
{
   struct NamesRec *pni=(&namesInfo);
   double frac, start_frac;
   int block_h, block_start;

   if (button_ev->button == Button3) {
      if (pni->first_index == 0) return;

      if (button_ev->state & (ShiftMask | ControlMask)) {
         pni->first_index -= ITEM_DSPED;
         if (pni->first_index < 0) pni->first_index = 0;
      } else {
         pni->first_index--;
      }
   } else if (button_ev->button == Button1) {
      if (pni->num_entries <= ITEM_DSPED ||
            pni->first_index+ITEM_DSPED == pni->num_entries) {
         return;
      }
      if (button_ev->state & (ShiftMask | ControlMask)) {
         pni->first_index += ITEM_DSPED;
         if (pni->first_index+ITEM_DSPED >= pni->num_entries)
            pni->first_index = pni->num_entries-ITEM_DSPED;
      } else {
         pni->first_index++;
      }
   } else if (button_ev->button == Button2) {
      int done=FALSE;
      XEvent ev;

      if (pni->num_entries <= ITEM_DSPED) return;

      frac = (double)((double)ITEM_DSPED / (double)(pni->num_entries));
      block_h = (int)(pni->scroll_area_h * frac);
      block_start = button_ev->y;
      start_frac = (double)((double)(block_start) /
            (double)(pni->scroll_area_h));
      if (block_start+block_h >= pni->scroll_area_h) {
         pni->first_index = pni->num_entries - ITEM_DSPED;
      } else {
         pni->first_index = (int)(pni->num_entries * start_frac);
      }
      RedrawNameScrollWin();
      RedrawDspWindow();

      XGrabPointer(mainDisplay, pni->scroll_win, False,
            PointerMotionMask | ButtonReleaseMask, GrabModeAsync,
            GrabModeAsync, None, handCursor, CurrentTime);

      while (!done) {
         XNextEvent(mainDisplay, &ev);

         if (ev.type == Expose || ev.type == VisibilityNotify) {
            ExposeEventHandler (&ev, TRUE);
         } else if (ev.type == ButtonRelease) {
            XUngrabPointer(mainDisplay, CurrentTime);
            done = TRUE;
         } else if (ev.type == MotionNotify) {
            int	new_name_first, y=ev.xmotion.y;

            start_frac = (double)(((double)y) /
                 ((double)pni->scroll_area_h));

            if (y <= 0) {
               new_name_first = 0;
            } else if (y+block_h >= pni->scroll_area_h) {
               new_name_first = pni->num_entries - ITEM_DSPED;
            } else {
               new_name_first = (int)(pni->num_entries * start_frac);
            }
            if (pni->first_index != new_name_first) {
               pni->first_index = new_name_first;
               RedrawNameScrollWin();
               RedrawDspWindow();
            }
            while (XCheckMaskEvent(mainDisplay, PointerMotionMask, &ev)) ;
         }
      }
      return;
   }
   RedrawNameScrollWin();
   RedrawDspWindow();
}

static
int NameDspHandler(button_ev)
   XButtonEvent *button_ev;
{
   struct NamesRec *pni=(&namesInfo);
   static Time last_click_time;
   static int last_name_marked;
   int row_offset, len, top;
   Time click_time;

   if (msgFontPtr == NULL) {
      top = defaultFontAsc+1;
   } else {
      top = msgFontAsc+1;
   }

   row_offset = (int)(button_ev->y / ROW_HEIGHT);

   if (pni->marked_index != INVALID &&
         pni->marked_index >= pni->first_index &&
         pni->marked_index < pni->first_index+ITEM_DSPED) {
      len = strlen(pni->entries[pni->marked_index]);
      XFillRectangle(mainDisplay, pni->dsp_win, revNameGC, 0,
            (pni->marked_index - pni->first_index)*ROW_HEIGHT,
            ITEM_LEN*defaultFontWidth, ROW_HEIGHT);
      XDrawString(mainDisplay, pni->dsp_win, nameGC, 0,
            (pni->marked_index - pni->first_index)*ROW_HEIGHT+top,
            pni->entries[pni->marked_index], len);
   }
   pni->marked_index = GetNameEntryNum(row_offset);
   if (pni->marked_index != INVALID) {
      len = strlen(pni->entries[pni->marked_index]);
      XFillRectangle(mainDisplay, pni->dsp_win, nameGC, 0,
            (pni->marked_index - pni->first_index)*ROW_HEIGHT,
            ITEM_LEN*defaultFontWidth, ROW_HEIGHT);
      XDrawString(mainDisplay, pni->dsp_win, revNameGC, 0,
            (pni->marked_index - pni->first_index)*ROW_HEIGHT+top,
            pni->entries[pni->marked_index], len);
   }
   click_time = button_ev->time;
   if (pni->just_clicked && pni->marked_index != INVALID &&
         last_name_marked == pni->marked_index &&
         (click_time-last_click_time) < doubleClickInterval) {
      return TRUE;
   }
   pni->just_clicked = TRUE;
   last_click_time = click_time;
   last_name_marked = pni->marked_index;

   if (pni->dsp_ptr != NULL && pni->marked_index != INVALID) {
      SetStringStatus(pni->dsp_ptr[pni->marked_index].pathstr);
   } else {
      SetStringStatus("");
   }
   return INVALID;
}

static
int ControlChar(key_ev, key_sym)
   XKeyEvent *key_ev;
   KeySym key_sym;
{
   register int i;
   struct NamesRec *pni=(&namesInfo);

   if (key_ev->state & ControlMask) {
      switch (key_sym) {
      case XK_Left: return (BAD);
      case XK_KP_Left: return (BAD);
      case XK_Up: key_sym = ((unsigned long)'b')&0xff; break;
      case XK_KP_Up: key_sym = ((unsigned long)'b')&0xff; break;
      case XK_Right: return (BAD);
      case XK_KP_Right: return (BAD);
      case XK_Down: key_sym = ((unsigned long)'f')&0xff; break;
      case XK_KP_Down: key_sym = ((unsigned long)'f')&0xff; break;
      }
   } else {
      switch (key_sym) {
      case XK_Left: return BAD;
      case XK_KP_Left: return BAD;
      case XK_Up: key_sym = ((unsigned long)'k')&0xff; break;
      case XK_KP_Up: key_sym = ((unsigned long)'k')&0xff; break;
      case XK_Right: return BAD;
      case XK_KP_Right: return BAD;
      case XK_Down: key_sym = ((unsigned long)'j')&0xff; break;
      case XK_KP_Down: key_sym = ((unsigned long)'j')&0xff; break;
      }
   }
   switch (key_sym&0xff) {
   case 'w': /* erase */
   case 'y':
      return INVALID;
   case 'n': /* down one */
   case 'j':
      i = (pni->marked_index < pni->num_entries-1) ?
            pni->marked_index+1 : pni->num_entries-1;
      break;
   case 'p': /* up one */
   case 'k':
      i = (pni->marked_index>0) ? pni->marked_index-1 : 0;
      break;
   case 'd': /* down one page */
   case 'f':
      if (pni->marked_index==INVALID) {
         i = (pni->num_entries <= ITEM_DSPED) ?
               pni->num_entries-1 : ITEM_DSPED;
      } else if (pni->marked_index < pni->num_entries-ITEM_DSPED) {
         i = pni->marked_index+ITEM_DSPED;
      } else {
         i = pni->num_entries-1;
      }
      break;
   case 'u': /* up one page */
   case 'b':
      i = (pni->marked_index > (ITEM_DSPED-1)) ?
            pni->marked_index-ITEM_DSPED : 0;
      break;
   default: return BAD;
   }
   return i;
}

static
void BackUpOneWord(pn_changing, pn_selected_btn_index)
   int *pn_changing, *pn_selected_btn_index;
{
   struct NamesRec *pni=(&namesInfo);
   int new_dir=FALSE;

   if (pni->buf_index == 0) return;

   if (pni->inbuf[pni->buf_index-1] == '/') {
      pni->inbuf[--pni->buf_index] = '\0';
      new_dir = TRUE;
   } else {
      while (pni->buf_index > 0) {
         if (pni->inbuf[(pni->buf_index)-1] == '/') {
            pni->inbuf[pni->buf_index] = '\0';
            break;
         } else {
            pni->buf_index--;
         }
      }
      pni->inbuf[pni->buf_index] = '\0';
   }
   ParseFileName(pni->inbuf, pni->dir_name, pni->name);
   if (new_dir) {
      int i;

      strcpy(pni->name, "../");
      pni->name_index = strlen(pni->name);
      for (i=0; i < pni->num_entries; i++) {
         if (strncmp(&(pni->entries[i])[pni->leading],
               pni->name, pni->name_index) == 0) {
            pni->marked_index = i;
            pni->faking_dot_dot = TRUE;
            *pn_changing = FALSE;
            *pn_selected_btn_index = i;
            break;
         }
      }
   } else {
      pni->first_index = 0;
      pni->marked_index = INVALID;
   }
}

static
void SpecialKeyInNames(key_ev, key_sym, pn_changing, pn_selected_btn_index)
   XKeyEvent *key_ev;
   KeySym key_sym;
   int *pn_changing, *pn_selected_btn_index;
{
   struct NamesRec *pni=(&namesInfo);
   int i=ControlChar(key_ev, key_sym);

   if (i == BAD) return;

   if (i == INVALID) {
      if (pni->edit_style == NAMES_SELECT_FILE) {
         /* back-up one word */
         BackUpOneWord(pn_changing, pn_selected_btn_index);
         sprintf(pni->inbuf, "%s/%s", pni->dir_name, pni->name);
         pni->buf_index = strlen(pni->inbuf);
      } else {
         *pni->inbuf = '\0';
         pni->buf_index = 0;
         pni->first_index = 0;
         pni->marked_index = INVALID;
      }
   } else if (i < pni->num_entries) {
      if (pni->edit_style == NAMES_SELECT_FILE) {
         strcpy(pni->name, &(pni->entries[i])[pni->leading]);
         pni->name_index = strlen(pni->name);
         sprintf(pni->inbuf, "%s/%s", pni->dir_name, pni->name);
         pni->buf_index = strlen(pni->inbuf);
      } else {
         strcpy(pni->inbuf, &(pni->entries[i])[pni->leading]);
         pni->buf_index = strlen(pni->inbuf);
      }
      if (i < pni->first_index) {
         pni->first_index = i;
      } else if (i >= pni->first_index+ITEM_DSPED) {
         if (i < pni->num_entries-ITEM_DSPED) {
            pni->first_index = i;
         } else {
            pni->first_index = pni->num_entries-ITEM_DSPED;
         }
      }
      pni->marked_index = i;
   }
   if (pni->exposed) {
      RedrawNamePath();
      RedrawNameScrollWin();
      RedrawDspWindow();
   }
   if (pni->dsp_ptr != NULL && pni->marked_index != INVALID) {
      SetStringStatus(pni->dsp_ptr[pni->marked_index].pathstr);
   } else {
      SetStringStatus("");
   }
}

static
int GetBtnIndexFromBtnId(btn_id)
   int btn_id;
{
   struct NamesRec *pni=(&namesInfo);
   int i;

   for (i=0; i < pni->num_btns; i++) {
      if (pni->btn_id[i] == btn_id) {
         return i;
      }
   }
   sprintf(gszMsgBox, "Programing error!\n\nCannot find a button with id=%1d.",
         btn_id);
   MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   return INVALID;
}

static
int KeyPressInNames(key_ev, pn_changing, pn_selected_btn_index)
   XKeyEvent *key_ev;
   int *pn_changing, *pn_selected_btn_index;
{
   struct NamesRec *pni=(&namesInfo);
   char buf[80];
   KeySym key_sym;

   XLookupString(key_ev, buf, sizeof(buf)-1, &key_sym, &c_stat);
   TranslateKeys(buf, &key_sym);

   if ((buf[0]=='\r' && (key_sym & 0xff)=='\r') ||
         (buf[0]=='\n' && (key_sym & 0xff)=='\n')) {
      if (pni->edit_style == NAMES_SELECT_FILE) {
         if (!pni->tabbed_from_root) {
            TabInNames();
         }
         *pn_changing = FALSE;
         *pn_selected_btn_index = GetBtnIndexFromBtnId(pni->def_btn_id);
      } else {
         if (pni->def_btn_id != INVALID) {
            *pn_changing = FALSE;
            *pn_selected_btn_index = GetBtnIndexFromBtnId(pni->def_btn_id);
         }
      }
   } else if (buf[0]=='\033' && (key_sym & 0xff)=='\033') {
      *pn_changing = FALSE;
      *pn_selected_btn_index = GetBtnIndexFromBtnId(BUTTON_CANCEL);
   } else if ((buf[0] == '\b'  && (key_sym & 0xff)=='\b') ||
         (buf[0] == '\b' && (key_sym & 0xff)=='h') ||
         (buf[0] == '\177' && (key_sym & 0x7f)=='\177') ||
         key_sym==XK_Left || key_sym==XK_KP_Left) {
      if (pni->edit_style == NAMES_SELECT_FILE &&
            (pni->faking_dot_dot || pni->pop_from_root)) {
         *pn_changing = FALSE;
         *pn_selected_btn_index = INVALID;
      } else {
         BackSpaceInNames(pn_changing, pn_selected_btn_index);
      }
   } else if (buf[0] == '\t') {
      TabInNames();
      *pn_changing = FALSE;
      *pn_selected_btn_index = INVALID;
   } else if ((!pni->tabbed_from_root || pni->num_entries != 0) &&
         ((key_sym>'\040' && key_sym<='\177' &&
         (key_ev->state & ControlMask)) || key_sym==XK_Up ||
         key_sym==XK_Down || key_sym==XK_KP_Up || key_sym==XK_KP_Down)) {
      SpecialKeyInNames(key_ev, key_sym, pn_changing, pn_selected_btn_index);
   } else if (key_sym>='\040' && key_sym<='\177' &&
         (!pni->tabbed_from_root || pni->num_entries != 0)) {
      CharInNames(buf, pn_changing, pn_selected_btn_index);
   }
}

static
void ButtonPressInPath(button_ev)
   XButtonEvent *button_ev;
{
   struct NamesRec *pni=(&namesInfo);

   if (button_ev->button == Button1) {
   } else if (button_ev->button == Button2) {
      int buf_len=0;
      char *cut_buffer;

      switch (pni->edit_style) {
      case NAMES_SIMPLE_SELECT_NAME: return;
      case NAMES_COMPLEX_SELECT_NAME: return;
      case NAMES_SELECT_FILE:
         if (pni->tabbed_from_root) return;
         break;
      case NAMES_EDIT_ATTR: break;
      case NAMES_EDIT_NAME: break;
      }
      cut_buffer = FetchCutBuffer(&buf_len);
      if (cut_buffer != NULL) {
         char *c_ptr=cut_buffer;
         int max_len;

         if (pni->edit_style == NAMES_SELECT_FILE) {
            max_len = sizeof(pni->inbuf)-strlen(pni->dir_name)-3;
            while (pni->name_index < max_len &&
                  *c_ptr >= '\040' && *c_ptr < '\177') {
               pni->name[pni->name_index++] = (*c_ptr++);
            }
            pni->name[pni->name_index] = '\0';
            sprintf(pni->inbuf, "%s/%s", pni->dir_name, pni->name);
            pni->buf_index = strlen(pni->inbuf);
         } else {
            max_len = sizeof(pni->inbuf)-strlen(pni->inbuf)-3;
            while (pni->buf_index < max_len &&
                  *c_ptr >= '\040' && *c_ptr < '\177') {
               pni->inbuf[pni->buf_index++] = (*c_ptr++);
            }
            pni->inbuf[pni->buf_index] = '\0';
            strcpy(&(pni->entries[pni->marked_index])[pni->leading],
                  pni->inbuf);
         }
         if (pni->exposed) {
            RedrawNamePath();
            RedrawDspWindow();
         }
         XFree(cut_buffer);
      }
   } else if (button_ev->button == Button3) {
   }
}

static
void ButtonPressInNames(button_ev, pn_changing, pn_selected_btn_index)
   XButtonEvent *button_ev;
   int *pn_changing, *pn_selected_btn_index;
{
   struct NamesRec *pni=(&namesInfo);

   if (button_ev->window == pni->base_win) {
      int i;

      if (PointInBBox(button_ev->x, button_ev->y, pni->path_bbox)) {
         ButtonPressInPath(button_ev);
      } else {
         for (i=0; i < pni->num_btns; i++) {
            if (PointInBBox(button_ev->x, button_ev->y,
                  pni->btn_bbox[i])) {
               *pn_changing = FALSE;
               *pn_selected_btn_index = i;
               break;
            }
         }
      }
   } else if (button_ev->window == pni->scroll_win) {
      NameScrollHandler(button_ev);
   } else if (button_ev->window == pni->dsp_win) {
      int double_clicked=(NameDspHandler(button_ev)!=INVALID);

      if (pni->marked_index != INVALID) {
         if (pni->edit_style == NAMES_SELECT_FILE) {
            strcpy(pni->name,
                  &(pni->entries[pni->marked_index])[pni->leading]);
            pni->name_index = strlen(pni->name);
            sprintf(pni->inbuf, "%s/%s", pni->dir_name, pni->name);
            pni->buf_index = strlen(pni->inbuf);
         } else {
            strcpy(pni->inbuf,
                  &(pni->entries[pni->marked_index])[pni->leading]);
            pni->buf_index = strlen(pni->inbuf);
         }
         RedrawNamePath();
         if (double_clicked && pni->double_click_btn_id != INVALID) {
            *pn_changing = FALSE;
            *pn_selected_btn_index =
                  GetBtnIndexFromBtnId(pni->double_click_btn_id);
         }
      }
   }
}

void NamesSetDefaultBtnId(def_btn_id, double_click_btn_id)
   int def_btn_id;
{
   struct NamesRec *pni=(&namesInfo);

   pni->def_btn_id = def_btn_id;
   pni->double_click_btn_id = double_click_btn_id;
}

void NamesSetStyle(edit_style, loop_once)
   int edit_style, loop_once;
{
   struct NamesRec *pni=(&namesInfo);

   pni->edit_style = edit_style;
   pni->loop_once = loop_once;
}

void NamesSetEntries(dsp_ptr, entries, num_entries, dont_free_entries,
      marked_index, leading)
   DspList *dsp_ptr;
   char **entries;
   int num_entries, dont_free_entries, marked_index, leading;
{
   struct NamesRec *pni=(&namesInfo);

   pni->dsp_ptr = dsp_ptr;
   pni->entries = entries;
   pni->num_entries = num_entries;
   pni->dont_free_entries = dont_free_entries;
   pni->marked_index = marked_index;
   pni->leading = leading;
}

void NamesSetCallback(pf_before_loop, pf_after_loop)
   GetEntriesFunc *pf_before_loop;
   AfterLoopFunc *pf_after_loop;
{
   struct NamesRec *pni=(&namesInfo);

   if (pni->dsp_ptr != NULL || pni->entries != NULL) {
      sprintf(gszMsgBox, "%s.",
            "dsp_ptr != NULL || entries != NULL in NamesSetCallback()");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      if (pni->dsp_ptr != NULL) free(pni->dsp_ptr);
      if (pni->entries != NULL) {
         free(*(pni->entries));
         free(pni->entries);
      }
   }
   pni->dsp_ptr = NULL;
   pni->entries = NULL;
   pni->num_entries = 0;
   pni->pf_before_loop = pf_before_loop;
   pni->pf_after_loop = pf_after_loop;
}

void NamesSetDir(dir_name)
   char *dir_name;
{
   struct NamesRec *pni=(&namesInfo);

   if (pni->edit_style != NAMES_SELECT_FILE) {
      sprintf(gszMsgBox, "%s!\n\nNamesSetDir() called with %s.",
            "Programing error", "pni->edit_style != NAMES_SELECT_FILE.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   }
   UtilStrCpy(pni->dir_name, sizeof(pni->dir_name), dir_name);
}

static
void BeforeLoopForSelectFile()
{
   struct NamesRec *pni=(&namesInfo);
   int i;

   if (pni->faking_dot_dot) {
      *pni->name = '\0';
      pni->name_index = 0;
   } else if (pni->tabbed_from_root &&
         !pni->just_tabbed_from_root) {
      pni->name[0] = '\0';
      pni->name_index = 0;
   } else if (pni->tabbed_from_root && pni->just_tabbed_from_root) {
      pni->name_index = strlen(pni->name);
      if (!SetMarkedIndex()) {
         pni->name[0] = '\0';
         pni->name_index = 0;
      }
   }
   sprintf(pni->inbuf, "%s/%s", pni->dir_name, pni->name);
   pni->buf_index = strlen(pni->inbuf);

   if (pni->exposed) {
      XClearWindow(mainDisplay, pni->base_win);
      RedrawNameBaseWindow();
   }
   pni->pop_from_root = FALSE;
}

static
int BreakForSelectFileAfterLoop()
{
   struct NamesRec *pni=(&namesInfo);

   if (pni->marked_index == INVALID && !pni->change_to_root &&
         !pni->pop_from_root && !pni->just_tabbed_from_root) {
      return TRUE;
   } else if (FileIsRemote(pni->name)) {
      pni->remote_file = TRUE;
      return TRUE;
   }
   if (pni->inbuf[pni->buf_index-1] != '/') {
      if (!pni->just_tabbed_from_root) {
         return TRUE;
      }
   } else {
      pni->inbuf[--(pni->buf_index)] = '\0';
      if (strcmp(pni->name, "../") == 0) {
         /* saved_name is used for faking_dot_dot */
         int i;

         for (i=strlen(pni->dir_name)-1;
               i >= 0 && pni->dir_name[i] != '/'; i--) {
         }
         if (i < 0) {
            strcpy(pni->saved_name, pni->dir_name);
            *pni->dir_name = '\0';
         } else {
            strcpy(pni->saved_name, &(pni->dir_name[i+1]));
            pni->dir_name[i] = '\0';
         }
      } else {
         strcpy(&pni->inbuf[pni->buf_index++], "/");
         ParseFileName(pni->inbuf, pni->dir_name, pni->name);
      }
   }
   return FALSE;
}

int Names(win_name, pn_selected_index, selected_str, str_sz, p_void)
   char *win_name, *selected_str;
   int *pn_selected_index, str_sz;
   void *p_void;
{
   struct NamesRec *pni=(&namesInfo);
   int index=INVALID, looping=TRUE, selected_btn_index=INVALID;

   if (selected_str != NULL) *selected_str = '\0';
   if (pn_selected_index != NULL) *pn_selected_index = INVALID;

   if (!CreateNamesWindows(win_name)) return INVALID;

   if (pni->edit_style == NAMES_SELECT_FILE) {
      pni->faking_dot_dot = FALSE;
      pni->change_to_root = FALSE;
      pni->just_tabbed_from_root = FALSE;
      *pni->saved_dir_name = '\0';
   }
   SaveStatusStrings();
   while (looping) {
      int changing=TRUE;

      if (pni->edit_style == NAMES_SELECT_FILE) {
         BeforeLoopForSelectFile();
      }
      if (pni->pf_before_loop != NULL) {
         int rc;
         char saved_ch='\0';

         SetWatchCursor(drawWindow);
         SetWatchCursor(mainWindow);
         SetWatchCursor(pni->base_win);
         if (pni->edit_style == NAMES_SELECT_FILE && !pni->tabbed_from_root &&
               !pni->just_tabbed_from_root) {
            saved_ch = *pni->inbuf;
            *pni->inbuf = '\0';
         }
         rc = (pni->pf_before_loop)(&pni->dsp_ptr,
               &pni->entries, &pni->num_entries,
               &pni->marked_index, pni->inbuf, p_void);
         if (pni->edit_style == NAMES_SELECT_FILE && !pni->tabbed_from_root &&
               !pni->just_tabbed_from_root) {
            *pni->inbuf = saved_ch;
         }
         if (pni->edit_style == NAMES_SELECT_FILE &&
               pni->just_tabbed_from_root) {
            /* saved_name is used for just_tabbed_from_root */
            *pni->dir_name = '\0';
            strcpy(pni->name, pni->saved_name);
            pni->name_index = strlen(pni->name);
            sprintf(pni->inbuf, "%s/%s", pni->dir_name, pni->name);
            pni->buf_index = strlen(pni->inbuf);
            *pni->saved_name = '\0';
            ParseFileName(pni->inbuf, pni->dir_name, pni->name);
            pni->name_index = strlen(pni->name);
            if (!SetMarkedIndex()) {
               pni->name[0] = '\0';
               pni->name_index = 0;
            }
         }
         if (pni->edit_style == NAMES_SELECT_FILE && pni->faking_dot_dot) {
            /* saved_name is used for faking_dot_dot */
            strcpy(pni->name, pni->saved_name);
            pni->name_index = strlen(pni->name);
            sprintf(pni->inbuf, "%s/%s", pni->dir_name, pni->name);
            pni->buf_index = strlen(pni->inbuf);
            *pni->saved_name = '\0';
            if (!SetMarkedIndex()) {
               pni->name[0] = '\0';
               pni->name_index = 0;
            }
         }
         SetDefaultCursor(mainWindow);
         SetDefaultCursor(drawWindow);
         SetDrawCursor(pni->base_win);
         if (!rc) break;
      }
      if (pni->edit_style == NAMES_SELECT_FILE) {
         pni->just_tabbed_from_root = FALSE;
         pni->faking_dot_dot = FALSE;
      }
      NamesUpdateIndices();

      if (pni->pf_before_loop != NULL && pni->exposed) {
         RedrawNamePath();
         RedrawNameScrollWin();
         RedrawDspWindow();
      }
      if (pni->dsp_ptr != NULL && pni->marked_index != INVALID) {
         SetStringStatus(pni->dsp_ptr[pni->marked_index].pathstr);
      } else {
         SetStringStatus("");
      }
      XSync(mainDisplay, False);

      selected_btn_index = INVALID;
      while (changing) {
         XEvent input, ev;

         XNextEvent(mainDisplay, &input);

         if (ExposeOrMapNames(&input)) {
            continue;
         }
         if (input.type == Expose) {
            ExposeEventHandler(&input, FALSE);
         } else if (input.type==VisibilityNotify &&
               input.xany.window==mainWindow &&
               input.xvisibility.state==VisibilityUnobscured) {
            int i;

            while (XCheckWindowEvent(mainDisplay, mainWindow,
                  VisibilityChangeMask, &ev)) ;
            if (pinnedMainMenu) XMapRaised(mainDisplay, mainMenuWindow);
            for (i=0; i < numExtraWins; i++) {
               if (extraWinInfo[i].mapped && extraWinInfo[i].raise &&
                     extraWinInfo[i].window != None) {
                  XMapRaised(mainDisplay, extraWinInfo[i].window);
               }
            }
            XMapRaised(mainDisplay, pni->base_win);
         } else if (input.type == KeyPress) {
            KeyPressInNames(&input.xkey, &changing, &selected_btn_index);
         } else if (input.type == ButtonPress) {
            ButtonPressInNames(&input.xbutton, &changing, &selected_btn_index);
         }
      }
      if (pni->exposed && selected_btn_index != INVALID) {
         DisplayButton(pni->base_win, pni->btn_str[selected_btn_index],
               8, &pni->btn_bbox[selected_btn_index], BUTTON_INVERT);
         XSync(mainDisplay, False);
      }
      if (pni->edit_style == NAMES_SELECT_FILE) {
         if (pni->marked_index != INVALID) {
            strcpy(pni->name,
                  &(pni->entries[pni->marked_index])[pni->leading]);
            pni->name_index = strlen(pni->name);
            sprintf(pni->inbuf, "%s/%s", pni->dir_name, pni->name);
            pni->buf_index = strlen(pni->inbuf);
         }
      }
      if (pni->pf_after_loop != NULL) {
         int btn_id=INVALID;

         if (selected_btn_index != INVALID) {
            btn_id = pni->btn_id[selected_btn_index];
         }
         if (!(pni->pf_after_loop)(&pni->dsp_ptr,
               &pni->entries, &pni->num_entries,
               &pni->marked_index, pni->inbuf, btn_id,
               pni->marked_index, p_void)) {
            looping = FALSE;
         }
      }
      if (selected_btn_index != INVALID) {
         if (pni->btn_id[selected_btn_index] == BUTTON_CANCEL) {
            looping = FALSE;
         }
      }
      if (pni->edit_style == NAMES_SELECT_FILE) {
         if (BreakForSelectFileAfterLoop()) {
            looping = FALSE;
         }
      }
      if (pni->loop_once == NAMES_LOOP_ONCE) {
         looping = FALSE;
      }
      if (pni->exposed && selected_btn_index != INVALID) {
         DisplayButton(pni->base_win, pni->btn_str[selected_btn_index],
               8, &pni->btn_bbox[selected_btn_index], BUTTON_NORMAL);
         XSync(mainDisplay, False);
      }
   }
   RestoreStatusStrings();
   XDestroyWindow(mainDisplay, pni->base_win);
   if (warpToWinCenter) {
      XWarpPointer(mainDisplay, None, drawWindow, 0, 0, 0, 0,
            (int)(ZOOMED_SIZE(drawWinW)>>1), (int)(ZOOMED_SIZE(drawWinH)>>1));
   }
   if (selected_str != NULL) {
      if (pni->remote_file) {
         UtilStrCpy(selected_str, str_sz, pni->name);
      } else {
         UtilStrCpy(selected_str, str_sz, pni->inbuf);
      }
   }
   if (pn_selected_index != NULL) *pn_selected_index = pni->marked_index;
   if (selected_btn_index != INVALID) {
      return pni->btn_id[selected_btn_index];
   }
   return INVALID;
}
