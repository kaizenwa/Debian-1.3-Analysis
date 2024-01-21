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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/dialog.c,v 3.0 1996/05/06 16:04:31 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "const.h"
#include "types.h"

#include "auxtext.e"
#include "box.e"
#include "button.e"
#include "cutpaste.e"
#include "cursor.e"
#ifndef _NO_EXTERN
#include "dialog.e"
#endif
#include "drawing.e"
#include "file.e"
#include "font.e"
#include "mainloop.e"
#include "mainmenu.e"
#include "msg.e"
#include "raster.e"
#include "setup.e"
#include "text.e"
#include "util.e"

int	doPassword=FALSE;
char	gszMsgBox[2048];

unsigned int CornerLoop(OrigX, OrigY)
   int *OrigX, *OrigY;
{
   XEvent input;

   XGrabPointer(mainDisplay, rootWindow, False, ButtonPressMask,
         GrabModeAsync, GrabModeAsync, None, cornerCursor, CurrentTime);

   for (;;) {
      XNextEvent(mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify) {
         ExposeEventHandler(&input, TRUE);
      } else if (input.type == ButtonPress) {
         XUngrabPointer(mainDisplay, CurrentTime);
         XSync(mainDisplay, False);
         *OrigX = input.xbutton.x;
         *OrigY = input.xbutton.y;
         return input.xbutton.button;
      }
   }
}

static XComposeStatus c_stat;

unsigned int DrawWindowLoop(OrigX, OrigY, cursor, snap_to_grid)
   int *OrigX, *OrigY, snap_to_grid;
   Cursor cursor;
{
   XGrabPointer(mainDisplay, drawWindow, False,
         PointerMotionMask | ButtonPressMask,
         GrabModeAsync, GrabModeAsync, None, cursor, CurrentTime);

   for (;;) {
      XEvent input;

      XNextEvent(mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify) {
         ExposeEventHandler(&input, TRUE);
      } else if (input.type == ButtonPress) {
         XUngrabPointer(mainDisplay, CurrentTime);
         XSync(mainDisplay, False);
         *OrigX = input.xbutton.x;
         *OrigY = input.xbutton.y;
         return input.xbutton.button;
      } else if (input.type == KeyPress) {
         XKeyEvent *key_ev=(&(input.xkey));
         KeySym key_sym;
         char s[80];
         int has_ch=XLookupString(key_ev, s, sizeof(s)-1, &key_sym, &c_stat);

         TranslateKeys(s, &key_sym);
         if (key_sym== XK_Escape || (has_ch && s[0]=='\033')) {
            XUngrabPointer(mainDisplay, CurrentTime);
            XSync(mainDisplay, False);
            return (unsigned int)(-1);
         }
      } else if (input.type == MotionNotify) {
         if (snap_to_grid) {
            int grid_x, grid_y;

            GridXY(input.xmotion.x, input.xmotion.y, &grid_x, &grid_y);
            MarkRulers(grid_x, grid_y);
         } else {
            MarkRulers(input.xmotion.x, input.xmotion.y);
         }
      }
   }
}

unsigned int PickAPoint(OrigX, OrigY, cursor)
   int *OrigX, *OrigY;
   Cursor cursor;
{
   XEvent input;

   XGrabPointer(mainDisplay, drawWindow, False, ButtonPressMask,
         GrabModeAsync, GrabModeAsync, None, cursor, CurrentTime);

   for (;;) {
      XNextEvent(mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify) {
         ExposeEventHandler(&input, TRUE);
      } else if (input.type == ButtonPress) {
         XUngrabPointer(mainDisplay, CurrentTime);
         XSync(mainDisplay, False);
         *OrigX = input.xbutton.x;
         *OrigY = input.xbutton.y;
         return input.xbutton.button;
      } else if (input.type == KeyPress) {
         XKeyEvent *key_ev=(&(input.xkey));
         KeySym key_sym;
         char s[80];
         int has_ch=XLookupString(key_ev, s, sizeof(s)-1, &key_sym, &c_stat);

         TranslateKeys(s, &key_sym);
         if (key_sym== XK_Escape || (has_ch && s[0]=='\033')) {
            XUngrabPointer(mainDisplay, CurrentTime);
            XSync(mainDisplay, False);
            return (unsigned int)(-1);
         }
      }
   }
}

#include "xbm/info.xbm"

#define ICON_W (info_width)
#define ICON_H (info_height)

#define X_MARGIN 20
#define Y_MARGIN 8
#define X_GAP 20
#define Y_GAP 20

#define SPACING 0

#define BTN_X_MARGIN 8
#define BTN_Y_MARGIN 2
#define BTN_XY_EXTRA 2
#define BTN_MIN_X_GAP 8

#define MAX_KEYSYMS 10

#define LF_BUTTON  0x01
#define MD_BUTTON  0x02
#define RT_BUTTON  0x03
#define XT_BUTTON  0x04

#define DEF_BUTTON 0x10

#define MAX_BUTTONS 3
#define MAX_BTN_STR_LEN 10

typedef struct BtnInfoRec {
   char		* str;
   struct BBRec	bbox;
   int		id, highlight;
   KeySym	key_sym[MAX_KEYSYMS];
} * BtnInfoRecPtr;

typedef struct MBRec {
   Window main_win, root_win, icon_win, msg_win, btn_win;

   int main_win_x, main_win_y, main_win_w, main_win_h;
   int icon_win_w, icon_win_h, msg_win_w, msg_win_h;
   int btn_win_w, btn_win_h, max_msg_win_w;
   int max_msg_str_len, max_msg_str_total;
   int exposed;

   char * msg_copy;

   Pixmap cur_bitmap;

   struct BtnInfoRec btn_info[MAX_BUTTONS+1];

   /* dialog specific */
   int is_dialog;
   int cur_x, cur_y, cursor_x, cursor_y, index, str_w;
   char *return_str;
} * MBRecPtr;

static struct MBRec	mbInfo;

static int	numButtons=MAX_BUTTONS;
static char	extraBtnChar='q';
static char	*btnStr[] = {
      "INVALID", "OK", "CANCEL", "YES", "NO", "Extra", NULL
};

static
void SetupMBButton(MBInfoPtr, BtnDesc, BtnCh, BtnID)
   struct MBRec *MBInfoPtr;
   int BtnDesc, BtnID;
   char BtnCh;
{
   int btn_index=(BtnDesc & 0x0f)-1;
   int i=0;

   if (BtnID == MB_ID_FAILED) {
      MBInfoPtr->btn_info[btn_index].str = NULL;
   } else {
      MBInfoPtr->btn_info[btn_index].str = btnStr[BtnID];
   }

   MBInfoPtr->btn_info[btn_index].id = BtnID;
   switch (BtnCh) {
   case 'o':
      MBInfoPtr->btn_info[btn_index].key_sym[i++] = XK_o;
      MBInfoPtr->btn_info[btn_index].key_sym[i++] = XK_O;
      break;
   case 'y':
      MBInfoPtr->btn_info[btn_index].key_sym[i++] = XK_y;
      MBInfoPtr->btn_info[btn_index].key_sym[i++] = XK_Y;
      break;
   case 'n':
      MBInfoPtr->btn_info[btn_index].key_sym[i++] = XK_n;
      MBInfoPtr->btn_info[btn_index].key_sym[i++] = XK_N;
      break;
   case 'c':
      MBInfoPtr->btn_info[btn_index].key_sym[i++] = XK_c;
      MBInfoPtr->btn_info[btn_index].key_sym[i++] = XK_C;
      break;
   }
   if (BtnDesc & DEF_BUTTON) {
      MBInfoPtr->btn_info[btn_index].key_sym[i++] = XK_Return;
      MBInfoPtr->btn_info[btn_index].key_sym[i++] = XK_Linefeed;
      MBInfoPtr->btn_info[btn_index].highlight = TRUE;
   } else {
      MBInfoPtr->btn_info[btn_index].highlight = FALSE;
   }
   MBInfoPtr->btn_info[btn_index].key_sym[i] = (KeySym)0;
}

static
void CalcSimpleGeometry(MBInfoPtr, Message)
   struct MBRec *MBInfoPtr;
   char *Message;
{
   int len=strlen(Message), max_len=0, max_h=0, left, inc, i, a_btn_w, a_btn_h;
   char *dest_ptr, *c_ptr;
   int font_height=0, font_asc=0, font_des=0;

   font_height = defaultFontHeight;
   font_asc = defaultFontAsc;
   font_des = defaultFontDes;

   MBInfoPtr->msg_copy = (char*)malloc(((len<<1)+1)*sizeof(char));
   if (MBInfoPtr->msg_copy == NULL) FailAllocMessage();

   dest_ptr = MBInfoPtr->msg_copy;
   *dest_ptr = '\0';
   c_ptr = Message;
   while (c_ptr != NULL) {
      /* a line at a time */
      char *lf_ptr=strchr(c_ptr, '\n');
      int full_str_len;

      if (lf_ptr != NULL) *lf_ptr = '\0';
      full_str_len = strlen(c_ptr);
      if (full_str_len > max_len) {
         if (full_str_len > MBInfoPtr->max_msg_str_len) {
            /* line too long for the message box */
            char *line=c_ptr;

            max_len = MBInfoPtr->max_msg_str_len;
            while (line != NULL && *line != '\0') {
               int line_len;

               while (*line == ' ') line++;
               line_len = strlen(line);
               if (line_len > MBInfoPtr->max_msg_str_len) {
                  int index=MBInfoPtr->max_msg_str_len;

                  if (line[index] == ' ') {
                     int saved_index=index--;

                     while (index >=0 && line[index] == ' ') index--;
                     line[++index] = '\0';
                     sprintf(dest_ptr, "%s\n", line);
                     dest_ptr = &dest_ptr[strlen(dest_ptr)];
                     max_h += font_height+SPACING;
                     line = &line[saved_index+1];
                  } else {
                     int saved_index=index;

                     while (index >=0 && line[index] != ' ') index--;
                     if (index < 0) {
                        /* very long word */
                        char saved_ch=line[saved_index];

                        line[saved_index] = '\0';
                        sprintf(dest_ptr, "%s\n", line);
                        dest_ptr = &dest_ptr[strlen(dest_ptr)];
                        max_h += font_height+SPACING;
                        line[saved_index] = saved_ch;
                        line = &line[saved_index];
                     } else {
                        /* find a good space */
                        saved_index = index--;
                        while (index >=0 && line[index] == ' ') index--;
                        line[++index] = '\0';
                        sprintf(dest_ptr, "%s\n", line);
                        dest_ptr = &dest_ptr[strlen(dest_ptr)];
                        max_h += font_height+SPACING;
                        line = &line[saved_index+1];
                     }
                  }
               } else if (line_len > 0) {
                  sprintf(dest_ptr, "%s\n", line);
                  dest_ptr = &dest_ptr[strlen(dest_ptr)];
                  max_h += font_height+SPACING;
                  break;
               }
            }
         } else {
            max_len = full_str_len;
            sprintf(dest_ptr, "%s\n", c_ptr);
            dest_ptr = &dest_ptr[strlen(dest_ptr)];
            max_h += font_height+SPACING;
         }
      } else {
         sprintf(dest_ptr, "%s\n", c_ptr);
         dest_ptr = &dest_ptr[strlen(dest_ptr)];
         max_h += font_height+SPACING;
      }
      max_h -= SPACING;
      if (lf_ptr != NULL) {
         *lf_ptr = '\n';
         c_ptr = &lf_ptr[1];
      } else {
         break;
      }
   }
   if (MBInfoPtr->is_dialog) {
      MBInfoPtr->msg_win_w = MBInfoPtr->max_msg_str_total;
   } else {
      MBInfoPtr->msg_win_w = max_len*defaultFontWidth;
   }
   MBInfoPtr->msg_win_h = max_h;
   MBInfoPtr->icon_win_w = ICON_W;
   MBInfoPtr->icon_win_h = ICON_H;
   if (MBInfoPtr->msg_win_h > MBInfoPtr->icon_win_h) {
      MBInfoPtr->icon_win_h = MBInfoPtr->msg_win_h;
   } else {
      MBInfoPtr->msg_win_h = MBInfoPtr->icon_win_h;
   }
   a_btn_w = MAX_BTN_STR_LEN*defaultFontWidth + (BTN_XY_EXTRA<<1);
   if (MBInfoPtr->is_dialog) {
      if (msgFontPtr == NULL) {
         a_btn_h = defaultFontHeight + (BTN_Y_MARGIN<<1) + (BTN_XY_EXTRA<<1);
      } else {
         a_btn_h = msgFontHeight + (BTN_Y_MARGIN<<1) + (BTN_XY_EXTRA<<1);
      }
   } else {
      if (boldMsgFontPtr == NULL) {
         a_btn_h = defaultFontHeight + (BTN_Y_MARGIN<<1) + (BTN_XY_EXTRA<<1);
      } else {
         a_btn_h = boldMsgFontHeight + (BTN_Y_MARGIN<<1) + (BTN_XY_EXTRA<<1);
      }
   }

   MBInfoPtr->btn_win_w = numButtons*a_btn_w+BTN_MIN_X_GAP*(numButtons-1)+2;
   MBInfoPtr->btn_win_h = a_btn_h+2;

   if (MBInfoPtr->btn_win_w >
         MBInfoPtr->msg_win_w+MBInfoPtr->icon_win_w+X_GAP) {
      MBInfoPtr->msg_win_w =
            MBInfoPtr->btn_win_w-MBInfoPtr->icon_win_w-X_GAP;
   } else {
      MBInfoPtr->btn_win_w =
            MBInfoPtr->msg_win_w+MBInfoPtr->icon_win_w+X_GAP;
   }
   MBInfoPtr->main_win_w = MBInfoPtr->btn_win_w + (X_MARGIN<<1) +
         (brdrW<<1);
   MBInfoPtr->main_win_h = MBInfoPtr->icon_win_h + MBInfoPtr->btn_win_h +
         (Y_MARGIN<<1) + Y_GAP + (brdrW<<1);
   left = ((MBInfoPtr->btn_win_w - numButtons*a_btn_w -
         BTN_MIN_X_GAP*(numButtons-1))>>1);
   inc = a_btn_w + ((MBInfoPtr->btn_win_w-(left<<1)-numButtons*a_btn_w) /
         (numButtons-1));
   for (i=0; i < numButtons; i++) {
      MBInfoPtr->btn_info[i].bbox.ltx = left+BTN_XY_EXTRA;
      MBInfoPtr->btn_info[i].bbox.lty = BTN_XY_EXTRA;
      MBInfoPtr->btn_info[i].bbox.rbx = left +
            MAX_BTN_STR_LEN*defaultFontWidth + BTN_XY_EXTRA;
      MBInfoPtr->btn_info[i].bbox.rby = BTN_XY_EXTRA + defaultFontHeight +
            (BTN_Y_MARGIN<<1);
      left += inc;
   }
   MBInfoPtr->main_win_x = ((DisplayWidth(mainDisplay, mainScreen) -
         MBInfoPtr->main_win_w)>>1);
   MBInfoPtr->main_win_y = ((DisplayHeight(mainDisplay, mainScreen) -
         MBInfoPtr->main_win_h)/3);
   if (MBInfoPtr->main_win_x < 0) MBInfoPtr->main_win_x = 0;
   if (MBInfoPtr->main_win_y < 0) MBInfoPtr->main_win_y = 0;
}

static
void CalcGeometry(MBInfoPtr, Message)
   struct MBRec *MBInfoPtr;
   char *Message;
{
   int len=strlen(Message), max_h=0, left, inc, i, a_btn_w, a_btn_h;
   int max_len=0, max_total=0;
   char *dest_ptr, *c_ptr;
   int font_height, font_asc, font_des, total;

   font_height = boldMsgFontHeight;
   font_asc = boldMsgFontAsc;
   font_des = boldMsgFontDes;
   total = XTextWidth(boldMsgFontPtr, Message, len);

   MBInfoPtr->msg_copy = (char*)malloc(((len<<1)+1)*sizeof(char));
   if (MBInfoPtr->msg_copy == NULL) FailAllocMessage();

   dest_ptr = MBInfoPtr->msg_copy;
   *dest_ptr = '\0';
   c_ptr = Message;
   while (c_ptr != NULL) {
      /* a line at a time */
      char *lf_ptr=strchr(c_ptr, '\n');
      int full_str_len, full_str_total;

      if (lf_ptr != NULL) *lf_ptr = '\0';
      full_str_len = strlen(c_ptr);
      full_str_total = XTextWidth(boldMsgFontPtr, c_ptr, full_str_len);
      if (full_str_total > max_total) {
         if (full_str_total > MBInfoPtr->max_msg_str_total) {
            /* line too long for the message box */
            char *line=c_ptr;

            max_total = MBInfoPtr->max_msg_str_total;
            while (line != NULL && *line != '\0') {
               int line_len, line_total;

               while (*line == ' ') line++;
               line_len = strlen(line);
               line_total = XTextWidth(boldMsgFontPtr, line, line_len);
               if (line_total > MBInfoPtr->max_msg_str_total) {
                  char *lead_ptr=line, *last_ptr=NULL;
                  int lead_index=0, last_index=0, still_going=TRUE;

                  while (still_going && *lead_ptr != '\0') {
                     char saved_ch;
                     int w;

                     while (*lead_ptr != ' ' && *lead_ptr != '\0') {
                        lead_ptr++;
                        lead_index++;
                     }
                     saved_ch = (*lead_ptr);
                     *lead_ptr = '\0';
                     w = XTextWidth(boldMsgFontPtr, line, lead_index);
                     if (w > MBInfoPtr->max_msg_str_total) {
                        if (last_ptr == NULL) {
                           /* very long word */
                           sprintf(dest_ptr, "%s\n", line);
                           dest_ptr = &dest_ptr[strlen(dest_ptr)];
                           max_h += font_height+SPACING;
                           line = &line[lead_index];
                        } else {
                           /* find a good space */
                           *last_ptr = '\0';
                           sprintf(dest_ptr, "%s\n", line);
                           *last_ptr = ' ';
                           dest_ptr = &dest_ptr[strlen(dest_ptr)];
                           max_h += font_height+SPACING;
                           line = &line[last_index+1];
                        }
                        still_going = FALSE;
                     } else {
                        last_ptr = lead_ptr;
                        last_index = lead_index;
                     }
                     *lead_ptr++ = saved_ch;
                     lead_index++;
                  }
               } else if (line_len > 0) {
                  sprintf(dest_ptr, "%s\n", line);
                  dest_ptr = &dest_ptr[strlen(dest_ptr)];
                  max_h += font_height+SPACING;
                  break;
               }
            }
         } else {
            max_len = full_str_len;
            max_total = full_str_total;
            sprintf(dest_ptr, "%s\n", c_ptr);
            dest_ptr = &dest_ptr[strlen(dest_ptr)];
            max_h += font_height+SPACING;
         }
      } else {
         sprintf(dest_ptr, "%s\n", c_ptr);
         dest_ptr = &dest_ptr[strlen(dest_ptr)];
         max_h += font_height+SPACING;
      }
      max_h -= SPACING;
      if (lf_ptr != NULL) {
         *lf_ptr = '\n';
         c_ptr = &lf_ptr[1];
      } else {
         break;
      }
   }
   if (MBInfoPtr->is_dialog) {
      MBInfoPtr->msg_win_w = MBInfoPtr->max_msg_str_total;
   } else {
      MBInfoPtr->msg_win_w = max_total;
   }
   MBInfoPtr->msg_win_h = max_h;
   MBInfoPtr->icon_win_w = ICON_W;
   MBInfoPtr->icon_win_h = ICON_H;
   if (MBInfoPtr->msg_win_h > MBInfoPtr->icon_win_h) {
      MBInfoPtr->icon_win_h = MBInfoPtr->msg_win_h;
   } else {
      MBInfoPtr->msg_win_h = MBInfoPtr->icon_win_h;
   }
   a_btn_w = XTextWidth(boldMsgFontPtr, "  CANCEL  ", 10) + (BTN_XY_EXTRA<<1);
   if (MBInfoPtr->is_dialog) {
      if (msgFontPtr == NULL) {
         a_btn_h = defaultFontHeight + (BTN_Y_MARGIN<<1) + (BTN_XY_EXTRA<<1);
      } else {
         a_btn_h = msgFontHeight + (BTN_Y_MARGIN<<1) + (BTN_XY_EXTRA<<1);
      }
   } else {
      if (boldMsgFontPtr == NULL) {
         a_btn_h = defaultFontHeight + (BTN_Y_MARGIN<<1) + (BTN_XY_EXTRA<<1);
      } else {
         a_btn_h = boldMsgFontHeight + (BTN_Y_MARGIN<<1) + (BTN_XY_EXTRA<<1);
      }
   }

   MBInfoPtr->btn_win_w = numButtons*a_btn_w+BTN_MIN_X_GAP*(numButtons-1)+2;
   MBInfoPtr->btn_win_h = a_btn_h+2;

   if (MBInfoPtr->btn_win_w >
         MBInfoPtr->msg_win_w+MBInfoPtr->icon_win_w+X_GAP) {
      MBInfoPtr->msg_win_w =
            MBInfoPtr->btn_win_w-MBInfoPtr->icon_win_w-X_GAP;
   } else {
      MBInfoPtr->btn_win_w =
            MBInfoPtr->msg_win_w+MBInfoPtr->icon_win_w+X_GAP;
   }
   MBInfoPtr->main_win_w = MBInfoPtr->btn_win_w + (X_MARGIN<<1) +
         (brdrW<<1);
   MBInfoPtr->main_win_h = MBInfoPtr->icon_win_h + MBInfoPtr->btn_win_h +
         (Y_MARGIN<<1) + Y_GAP + (brdrW<<1);
   left = ((MBInfoPtr->btn_win_w - numButtons*a_btn_w -
         BTN_MIN_X_GAP*(numButtons-1))>>1);
   inc = a_btn_w + ((MBInfoPtr->btn_win_w-(left<<1)-numButtons*a_btn_w) /
         (numButtons-1));
   for (i=0; i < numButtons; i++) {
      MBInfoPtr->btn_info[i].bbox.ltx = left+BTN_XY_EXTRA;
      MBInfoPtr->btn_info[i].bbox.lty = BTN_XY_EXTRA;
      MBInfoPtr->btn_info[i].bbox.rbx = left +
            (a_btn_w-(BTN_XY_EXTRA<<1)) + BTN_XY_EXTRA;
      MBInfoPtr->btn_info[i].bbox.rby = BTN_XY_EXTRA + font_height +
            (BTN_Y_MARGIN<<1);
      left += inc;
   }
   MBInfoPtr->main_win_x = ((DisplayWidth(mainDisplay, mainScreen) -
         MBInfoPtr->main_win_w)>>1);
   MBInfoPtr->main_win_y = ((DisplayHeight(mainDisplay, mainScreen) -
         MBInfoPtr->main_win_h)/3);
   if (MBInfoPtr->main_win_x < 0) MBInfoPtr->main_win_x = 0;
   if (MBInfoPtr->main_win_y < 0) MBInfoPtr->main_win_y = 0;
}

static
int SetupMBWindow(MBInfoPtr, Message, Title, IconAndBtns, IsDialog)
   struct MBRec *MBInfoPtr;
   char *Message, *Title;
   int IconAndBtns, IsDialog;
{
   XWMHints wmhints;
   XSizeHints sizehints;
   XSetWindowAttributes win_attrs;

   memset(MBInfoPtr, 0, sizeof(struct MBRec));
   MBInfoPtr->is_dialog = IsDialog;
   MBInfoPtr->msg_copy = NULL;
   MBInfoPtr->max_msg_win_w = (DisplayWidth(mainDisplay,mainScreen)>>1);
   MBInfoPtr->max_msg_str_len = MBInfoPtr->max_msg_win_w / defaultFontWidth;
   MBInfoPtr->max_msg_str_total = MBInfoPtr->max_msg_win_w;

   numButtons = MAX_BUTTONS;
   if (IconAndBtns & MB_BTN_EXTRA) numButtons++;
   if (boldMsgFontPtr == NULL) {
      CalcSimpleGeometry(MBInfoPtr, Message);
   } else {
      CalcGeometry(MBInfoPtr, Message);
   }
   switch (IconAndBtns & MB_BTNMASK) {
   case MB_BTN_NONE:
      break;
   case MB_BTN_OK:
      SetupMBButton(MBInfoPtr, LF_BUTTON, '\0', MB_ID_FAILED);
      SetupMBButton(MBInfoPtr, MD_BUTTON | DEF_BUTTON, 'o', MB_ID_OK);
      SetupMBButton(MBInfoPtr, RT_BUTTON, '\0', MB_ID_FAILED);
      break;
   case MB_BTN_YESNOCANCEL:
      SetupMBButton(MBInfoPtr, LF_BUTTON | DEF_BUTTON, 'y', MB_ID_YES);
      SetupMBButton(MBInfoPtr, MD_BUTTON, 'n', MB_ID_NO);
      SetupMBButton(MBInfoPtr, RT_BUTTON, 'c', MB_ID_CANCEL);
      break;
   case MB_BTN_OKCANCEL:
      SetupMBButton(MBInfoPtr, LF_BUTTON | DEF_BUTTON, 'o', MB_ID_OK);
      SetupMBButton(MBInfoPtr, MD_BUTTON, '\0', MB_ID_FAILED);
      SetupMBButton(MBInfoPtr, RT_BUTTON, 'c', MB_ID_CANCEL);
      break;
   case MB_BTN_YESNO:
      SetupMBButton(MBInfoPtr, LF_BUTTON | DEF_BUTTON, 'y', MB_ID_YES);
      SetupMBButton(MBInfoPtr, MD_BUTTON, '\0', MB_ID_FAILED);
      SetupMBButton(MBInfoPtr, RT_BUTTON, 'n', MB_ID_NO);
      break;
   default:
      SetupMBButton(MBInfoPtr, LF_BUTTON, '\0', MB_ID_FAILED);
      SetupMBButton(MBInfoPtr, MD_BUTTON, '\0', MB_ID_FAILED);
      SetupMBButton(MBInfoPtr, RT_BUTTON, '\0', MB_ID_FAILED);
      break;
   }
   if (IconAndBtns & MB_BTN_EXTRA) {
      if ((IconAndBtns & MB_BTNMASK) != 0) {
         SetupMBButton(MBInfoPtr, XT_BUTTON, extraBtnChar, MB_ID_EXTRA);
      } else {
         SetupMBButton(MBInfoPtr, XT_BUTTON | DEF_BUTTON, extraBtnChar,
               MB_ID_EXTRA);
      }
   } else {
      SetupMBButton(MBInfoPtr, XT_BUTTON, '\0', MB_ID_FAILED);
   }

   switch (IconAndBtns & MB_ICONMASK) {
   case MB_ICON_STOP:
      MBInfoPtr->cur_bitmap = msgBoxPixmap[MB_PIXMAP_STOP];
      break;
   case MB_ICON_QUESTION:
      MBInfoPtr->cur_bitmap = msgBoxPixmap[MB_PIXMAP_QUESTION];
      break;
   case MB_ICON_INFORMATION:
      MBInfoPtr->cur_bitmap = msgBoxPixmap[MB_PIXMAP_INFORMATION];
      break;
   case MB_ICON_DIALOG:
      MBInfoPtr->cur_bitmap = msgBoxPixmap[MB_PIXMAP_DIALOG];
      break;
   default: MBInfoPtr->cur_bitmap = None; break;
   }

   if ((MBInfoPtr->main_win=XCreateSimpleWindow(mainDisplay, rootWindow,
         MBInfoPtr->main_win_x, MBInfoPtr->main_win_y,
         MBInfoPtr->main_win_w, MBInfoPtr->main_win_h, brdrW,
         myBorderPixel, myBgPixel)) == 0) {
      fprintf(stderr, "Fatal error!  Can not create MsgBox window!\n");
      return FALSE;
   }
   if ((MBInfoPtr->icon_win=XCreateSimpleWindow(mainDisplay,
         MBInfoPtr->main_win, X_MARGIN, Y_MARGIN,
         MBInfoPtr->icon_win_w, MBInfoPtr->icon_win_h, 0,
         myBorderPixel, myBgPixel)) == 0) {
      fprintf(stderr, "Fatal error!  Can not create MsgBox window!\n");
      return FALSE;
   }
   if ((MBInfoPtr->msg_win=XCreateSimpleWindow(mainDisplay,
         MBInfoPtr->main_win, X_MARGIN+MBInfoPtr->icon_win_w+X_GAP, Y_MARGIN,
         MBInfoPtr->msg_win_w, MBInfoPtr->msg_win_h, 0,
         myBorderPixel, myBgPixel)) == 0) {
      fprintf(stderr, "Fatal error!  Can not create MsgBox window!\n");
      return FALSE;
   }
   if ((MBInfoPtr->btn_win=XCreateSimpleWindow(mainDisplay,
         MBInfoPtr->main_win, X_MARGIN, Y_MARGIN+Y_GAP+MBInfoPtr->icon_win_h,
         MBInfoPtr->btn_win_w, MBInfoPtr->btn_win_h, 0,
         myBorderPixel, myBgPixel)) == 0) {
      fprintf(stderr, "Fatal error!  Can not create MsgBox window!\n");
      return FALSE;
   }
   win_attrs.save_under = True;
   win_attrs.colormap = mainColormap;
   XChangeWindowAttributes(mainDisplay, MBInfoPtr->main_win,
         CWSaveUnder | CWColormap, &win_attrs);

   wmhints.flags = InputHint | StateHint;
   wmhints.input = True;
   wmhints.initial_state = NormalState;
   XSetWMHints(mainDisplay, MBInfoPtr->main_win, &wmhints);
   wmhints.flags = InputHint;
   XSetWMHints(mainDisplay, MBInfoPtr->icon_win, &wmhints);
   XSetWMHints(mainDisplay, MBInfoPtr->msg_win, &wmhints);
   XSetWMHints(mainDisplay, MBInfoPtr->btn_win, &wmhints);

   sizehints.flags = PPosition | PSize | USPosition | PMinSize | PMaxSize;
   sizehints.x = MBInfoPtr->main_win_x;
   sizehints.y = MBInfoPtr->main_win_y;
   sizehints.width = sizehints.max_width = sizehints.min_width =
         MBInfoPtr->main_win_w;
   sizehints.height = sizehints.max_height = sizehints.min_height =
         MBInfoPtr->main_win_h;
#ifdef NOTR4MODE
   XSetNormalHints(mainDisplay, MBInfoPtr->main_win, &sizehints);
#else
   XSetWMNormalHints(mainDisplay, MBInfoPtr->main_win, &sizehints);
#endif /* NOTR4MODE */
   XStoreName(mainDisplay, MBInfoPtr->main_win, Title);

#ifdef MAPBEFORESELECT
   XMapWindow(mainDisplay, MBInfoPtr->main_win);
   XMapWindow(mainDisplay, MBInfoPtr->icon_win);
   XMapWindow(mainDisplay, MBInfoPtr->msg_win);
   XMapWindow(mainDisplay, MBInfoPtr->btn_win);
   XSelectInput(mainDisplay, MBInfoPtr->main_win,
         ButtonPressMask | KeyPressMask |
         KeyPressMask | StructureNotifyMask | VisibilityChangeMask);
   XSelectInput(mainDisplay, MBInfoPtr->icon_win,
         ButtonPressMask | KeyPressMask | ExposureMask);
   XSelectInput(mainDisplay, MBInfoPtr->msg_win,
         ButtonPressMask | KeyPressMask | ExposureMask);
   XSelectInput(mainDisplay, MBInfoPtr->btn_win, ButtonReleaseMask |
         ButtonPressMask | KeyPressMask |
         ButtonPressMask | KeyPressMask | ExposureMask);
#else /* !MAPBEFORESELECT */
   XSelectInput(mainDisplay, MBInfoPtr->main_win,
         ButtonPressMask | KeyPressMask |
         KeyPressMask | StructureNotifyMask | VisibilityChangeMask);
   XSelectInput(mainDisplay, MBInfoPtr->icon_win,
         ButtonPressMask | KeyPressMask | ExposureMask);
   XSelectInput(mainDisplay, MBInfoPtr->msg_win,
         ButtonPressMask | KeyPressMask | ExposureMask);
   XSelectInput(mainDisplay, MBInfoPtr->btn_win, ButtonReleaseMask |
         ButtonPressMask | KeyPressMask |
         ButtonPressMask | KeyPressMask | ExposureMask);
   XMapWindow(mainDisplay, MBInfoPtr->main_win);
   XMapWindow(mainDisplay, MBInfoPtr->icon_win);
   XMapWindow(mainDisplay, MBInfoPtr->msg_win);
   XMapWindow(mainDisplay, MBInfoPtr->btn_win);
#endif /* !MAPBEFORESELECT */
   if (warpToWinCenter) {
      XWarpPointer(mainDisplay, None, MBInfoPtr->main_win, 0, 0, 0, 0,
            (MBInfoPtr->main_win_w>>1), (MBInfoPtr->main_win_h>>1));
   }
   XSync(mainDisplay, False);
   return TRUE;
}

static
void DisplayInput(MBInfoPtr)
   struct MBRec *MBInfoPtr;
{
   Window win=MBInfoPtr->btn_win;
   char *buf=MBInfoPtr->return_str, *dup_buf=NULL, *msg=NULL;
   int buf_len;

   MBInfoPtr->str_w = 0;
   MBInfoPtr->cur_x = (MBInfoPtr->btn_win_w>>1);
   if (buf == NULL) return;

   buf_len = strlen(buf);
   if (msgFontPtr == NULL) {
      MBInfoPtr->str_w = defaultFontWidth*strlen(buf);
   } else {
      MBInfoPtr->str_w = XTextWidth(msgFontPtr, buf, buf_len);
   }
   MBInfoPtr->cur_x = ((MBInfoPtr->btn_win_w-MBInfoPtr->str_w)>>1);
   MBInfoPtr->cursor_x = MBInfoPtr->cur_x+MBInfoPtr->str_w+1;
   if (doPassword) {
      int i;

      dup_buf = (char*)malloc((buf_len+1)*sizeof(char));
      if (dup_buf == NULL) FailAllocMessage();
      for (i=0; i < buf_len; i++) dup_buf[i] = '*';
      msg = dup_buf;
   } else {
      msg = buf;
   }
   if (msgFontPtr != NULL) {
      XSetFont(mainDisplay, defaultGC, msgFontPtr->fid);
   }
   XDrawString(mainDisplay, win, defaultGC, MBInfoPtr->cur_x, MBInfoPtr->cur_y,
         msg, buf_len);
   if (msgFontPtr != NULL) {
      XSetFont(mainDisplay, defaultGC, defaultFontPtr->fid);
   }
   if (dup_buf != NULL) free(dup_buf);
}

static
int HandleMsgBoxKeyEvent(MBInfoPtr, input)
   struct MBRec *MBInfoPtr;
   XEvent *input;
{
   XKeyEvent *key_ev=(&(input->xkey));
   KeySym key_sym;
   char buf[80];
   int i, j;

   XLookupString(key_ev, buf, 80-1, &key_sym, &c_stat);
   TranslateKeys(buf, &key_sym);
   if (MBInfoPtr->is_dialog) {
      /* for Dialog(), return INVALID for <ESC> */
      /*    return FALSE for a normal character, including <BS> */
      /*    return TRUE for <CR> or <LF> */
      if (MBInfoPtr->exposed) {
         /* erase the old cursor */
         PutCursor(MBInfoPtr->btn_win, MBInfoPtr->cursor_x, MBInfoPtr->cursor_y,
               myBgPixel);
      }
      if ((buf[0]=='\033' && (key_sym & 0xff)=='\033') ||
            (buf[0]=='\r' && (key_sym & 0xff)=='\r') ||
            (buf[0]=='\n' && (key_sym & 0xff)=='\n') ||
            (buf[0]=='\b' && (key_sym & 0xff)=='\b') ||
            (buf[0]=='\b' && (key_sym & 0xff)=='h' &&
            (key_ev->state & ControlMask)) ||
            (buf[0]=='\177' && (key_sym & 0x7f)=='\177') ||
            ((key_ev->state & ControlMask)==0 &&
            key_sym>='\040' && key_sym<='\177')) {
         if (MBInfoPtr->return_str == NULL) {
            return INVALID;
         }
         switch (buf[0]) {
         case '\033':
            if (MBInfoPtr->return_str != NULL) *MBInfoPtr->return_str = '\0';
            return INVALID;
         case '\r':
         case '\n':
            return TRUE;
            break;
         case '\177': /* <DEL> */
         case '\b': /* <BS> */
            if (MBInfoPtr->index > 0) {
               if (MBInfoPtr->exposed) {
                  XClearWindow(mainDisplay, MBInfoPtr->btn_win);
               }
               MBInfoPtr->return_str[--MBInfoPtr->index] = '\0';
               DisplayInput(MBInfoPtr);
            }
            break;
         default:
            if (buf[0] >= '\040' && MBInfoPtr->index < 80) {
               if (MBInfoPtr->exposed) {
                  XClearWindow(mainDisplay, MBInfoPtr->btn_win);
               }
               MBInfoPtr->return_str[MBInfoPtr->index++] = buf[0];
               MBInfoPtr->return_str[MBInfoPtr->index] = '\0';
               DisplayInput(MBInfoPtr);
            }
            break;
         }
      }
      if (MBInfoPtr->exposed) {
         PutCursor(MBInfoPtr->btn_win, MBInfoPtr->cursor_x, MBInfoPtr->cursor_y,
               myFgPixel);
      }
      return FALSE;
   } else {
      /* for MsgBox(), return a button id */
      if (buf[0]=='\033' && (key_sym & 0xff)=='\033') {
         return (MB_ID_CANCEL);
      }
      for (i=0; i < numButtons; i++) {
         if (MBInfoPtr->btn_info[i].str != NULL) {
            for (j=0; MBInfoPtr->btn_info[i].key_sym[j] != (KeySym)0; j++) {
               if (MBInfoPtr->btn_info[i].key_sym[j] == key_sym) {
                  DisplayButtonInBBox(MBInfoPtr->btn_win,
                        MBInfoPtr->btn_info[i].str,
                        strlen(MBInfoPtr->btn_info[i].str),
                        &MBInfoPtr->btn_info[i].bbox, BUTTON_INVERT,
                        MBInfoPtr->btn_info[i].highlight, BTN_XY_EXTRA);
                  XSync(mainDisplay, False);
                  return MBInfoPtr->btn_info[i].id;
               }
            }
         }
      }
   }
   return INVALID;
}

static
void HandlePasteInDialog(MBInfoPtr, input)
   struct MBRec *MBInfoPtr;
   XEvent *input;
{
   XButtonEvent *button_ev=(&(input->xbutton));
   int buf_len=0;
   char *cut_buffer;

   if (button_ev->button != Button2) return;

   cut_buffer = FetchCutBuffer(&buf_len);
   if (cut_buffer == NULL) return;

   if ((unsigned char)(*cut_buffer) != TGIF_HEADER &&
         MBInfoPtr->return_str != NULL) {
      unsigned char *c_ptr=(unsigned char *)cut_buffer;

      if (MBInfoPtr->exposed) {
         XClearWindow(mainDisplay, MBInfoPtr->btn_win);
      }
      for ( ; MBInfoPtr->index < 80 && *c_ptr != '\0'; c_ptr++) {
         if (*c_ptr >= (unsigned char)('\040') &&
               *c_ptr < (unsigned char)('\377')) {
            MBInfoPtr->return_str[MBInfoPtr->index++] = (char)(*c_ptr);
         } else {
            break;
         }
      }
      if (MBInfoPtr->exposed) {
         DisplayInput(MBInfoPtr);
         PutCursor(MBInfoPtr->btn_win, MBInfoPtr->cursor_x, MBInfoPtr->cursor_y,
               myFgPixel);
      }
   }
   XFree(cut_buffer);
}

static
int HandleMsgBoxBtnEvent(MBInfoPtr, input)
   struct MBRec *MBInfoPtr;
   XEvent *input;
{
   XButtonEvent *button_ev=(&(input->xbutton));
   int i, x=button_ev->x, y=button_ev->y;

   for (i=0; i < numButtons; i++) {
      if (MBInfoPtr->btn_info[i].str != NULL) {
         if (x >= MBInfoPtr->btn_info[i].bbox.ltx &&
               x < MBInfoPtr->btn_info[i].bbox.rbx &&
               y >= MBInfoPtr->btn_info[i].bbox.lty &&
               y < MBInfoPtr->btn_info[i].bbox.rby) {
            int inside=TRUE;

            DisplayButtonInBBox(MBInfoPtr->btn_win, MBInfoPtr->btn_info[i].str,
                  strlen(MBInfoPtr->btn_info[i].str),
                  &MBInfoPtr->btn_info[i].bbox, BUTTON_INVERT,
                  MBInfoPtr->btn_info[i].highlight, BTN_XY_EXTRA);

            XGrabPointer(mainDisplay, MBInfoPtr->btn_win, FALSE,
                  PointerMotionMask | ButtonReleaseMask,
                  GrabModeAsync, GrabModeAsync, None, defaultCursor,
                  CurrentTime);
            while (TRUE) {
               XEvent ev;

               XNextEvent(mainDisplay, &ev);
               if (ev.type == ButtonRelease) {
                  XUngrabPointer(mainDisplay, CurrentTime);
                  XSync(mainDisplay, False);
                  button_ev = &(ev.xbutton);
                  x = button_ev->x;
                  y = button_ev->y;
                  if (x >= MBInfoPtr->btn_info[i].bbox.ltx &&
                        x < MBInfoPtr->btn_info[i].bbox.rbx &&
                        y >= MBInfoPtr->btn_info[i].bbox.lty &&
                        y < MBInfoPtr->btn_info[i].bbox.rby) {
                     DisplayButtonInBBox(MBInfoPtr->btn_win,
                           MBInfoPtr->btn_info[i].str,
                           strlen(MBInfoPtr->btn_info[i].str),
                           &MBInfoPtr->btn_info[i].bbox,
                           BUTTON_NORMAL,
                           MBInfoPtr->btn_info[i].highlight, BTN_XY_EXTRA);
                     return MBInfoPtr->btn_info[i].id;
                  }
                  break;
               } else if (ev.type == MotionNotify) {
                  XEvent tmp_ev;
                  XMotionEvent *motion_ev=(&(ev.xmotion));

                  while (XCheckMaskEvent(mainDisplay, PointerMotionMask,
                        &tmp_ev)) ;
                  x = motion_ev->x;
                  y = motion_ev->y;
                  if (inside) {
                     if (!(x >= MBInfoPtr->btn_info[i].bbox.ltx &&
                           x < MBInfoPtr->btn_info[i].bbox.rbx &&
                           y >= MBInfoPtr->btn_info[i].bbox.lty &&
                           y < MBInfoPtr->btn_info[i].bbox.rby)) {
                        DisplayButtonInBBox(MBInfoPtr->btn_win,
                              MBInfoPtr->btn_info[i].str,
                              strlen(MBInfoPtr->btn_info[i].str),
                              &MBInfoPtr->btn_info[i].bbox, BUTTON_NORMAL,
                              MBInfoPtr->btn_info[i].highlight, BTN_XY_EXTRA);
                        inside = FALSE;
                     }
                  } else {
                     if (x >= MBInfoPtr->btn_info[i].bbox.ltx &&
                           x < MBInfoPtr->btn_info[i].bbox.rbx &&
                           y >= MBInfoPtr->btn_info[i].bbox.lty &&
                           y < MBInfoPtr->btn_info[i].bbox.rby) {
                        DisplayButtonInBBox(MBInfoPtr->btn_win,
                              MBInfoPtr->btn_info[i].str,
                              strlen(MBInfoPtr->btn_info[i].str),
                              &MBInfoPtr->btn_info[i].bbox, BUTTON_INVERT,
                              MBInfoPtr->btn_info[i].highlight, BTN_XY_EXTRA);
                        inside = TRUE;
                     }
                  }
               }
            }
            return INVALID;
         }
      }
   }
   return INVALID;
}

static
void RefreshMsgBox(MBInfoPtr)
   struct MBRec *MBInfoPtr;
{
   int i;
   XEvent ev;

   if (MBInfoPtr->msg_copy != NULL && *MBInfoPtr->msg_copy != '\0') {
      int y=0;
      char *c_ptr=MBInfoPtr->msg_copy;

      if (boldMsgFontPtr != NULL) {
         XSetFont(mainDisplay, defaultGC, boldMsgFontPtr->fid);
      }
      while (c_ptr != NULL) {
         char *c_ptr1=strchr(c_ptr, '\n');
         int len, w;

         if (c_ptr1 != NULL) *c_ptr1 = '\0';
         len = strlen(c_ptr);
         if (boldMsgFontPtr == NULL) {
            if (MBInfoPtr->is_dialog) {
               w = len * defaultFontWidth;
               XDrawString(mainDisplay, MBInfoPtr->msg_win, defaultGC,
                     (MBInfoPtr->msg_win_w-w)>>1, y+defaultFontAsc, c_ptr, len);
            } else {
               XDrawString(mainDisplay, MBInfoPtr->msg_win, defaultGC,
                     0, y+defaultFontAsc, c_ptr, len);
            }
            y += defaultFontHeight+SPACING;
         } else {
            if (MBInfoPtr->is_dialog) {
               w = XTextWidth(boldMsgFontPtr, c_ptr, len);
               XDrawString(mainDisplay, MBInfoPtr->msg_win, defaultGC,
                     (MBInfoPtr->msg_win_w-w)>>1, y+boldMsgFontAsc, c_ptr, len);
            } else {
               XDrawString(mainDisplay, MBInfoPtr->msg_win, defaultGC,
                     0, y+boldMsgFontAsc, c_ptr, len);
            }
            y += boldMsgFontHeight+SPACING;
         }
         if (c_ptr1 != NULL) {
            *c_ptr1 = '\n';
            c_ptr = &c_ptr1[1];
         } else {
            break;
         }
      }
      if (boldMsgFontPtr != NULL) {
         XSetFont(mainDisplay, defaultGC, defaultFontPtr->fid);
      }
   }
   if (MBInfoPtr->is_dialog) {
      DisplayInput(MBInfoPtr);
      PutCursor(MBInfoPtr->btn_win, MBInfoPtr->cursor_x, MBInfoPtr->cursor_y,
            myFgPixel);
   } else {
      for (i=0; i < numButtons; i++) {
         if (MBInfoPtr->btn_info[i].str != NULL) {
            DisplayButtonInBBox(MBInfoPtr->btn_win, MBInfoPtr->btn_info[i].str,
                  strlen(MBInfoPtr->btn_info[i].str),
                  &MBInfoPtr->btn_info[i].bbox, BUTTON_NORMAL,
                  MBInfoPtr->btn_info[i].highlight, BTN_XY_EXTRA);
         }
      }
   }
   if (MBInfoPtr->cur_bitmap != None) {
      int y=((MBInfoPtr->icon_win_h-ICON_H)>>1);

      XSetTSOrigin(mainDisplay, defaultGC, 0, y);
      XSetFillStyle(mainDisplay, defaultGC, FillOpaqueStippled);
      XSetStipple(mainDisplay, defaultGC, MBInfoPtr->cur_bitmap);
      XFillRectangle(mainDisplay, MBInfoPtr->icon_win, defaultGC, 0, y,
            ICON_W, ICON_H);
      XSetFillStyle(mainDisplay, defaultGC, FillSolid);
      XSetTSOrigin(mainDisplay, defaultGC, 0, 0);
   }
   while (XCheckWindowEvent(mainDisplay,MBInfoPtr->main_win,ExposureMask,&ev)) ;
   while (XCheckWindowEvent(mainDisplay,MBInfoPtr->icon_win,ExposureMask,&ev)) ;
   while (XCheckWindowEvent(mainDisplay,MBInfoPtr->msg_win,ExposureMask,&ev)) ;
   while (XCheckWindowEvent(mainDisplay,MBInfoPtr->btn_win,ExposureMask,&ev)) ;
}

int MsgBox(Message, Title, IconAndBtns)
   char *Message, *Title;
   int IconAndBtns;
{
   char *dup_msg=UtilStrDup(Message);
   int rc=MB_ID_FAILED, looping=TRUE;

   if (dup_msg == NULL) {
      FailAllocMessage();
      return rc;
   }
   if (!SetupMBWindow(&mbInfo, dup_msg, Title, IconAndBtns, FALSE)) {
      fprintf(stderr, "Invalid parameters passed to MsgBox().\n");
      Msg("Invalid parameters passed to MsgBox().\n");
      if (mbInfo.msg_copy != NULL) free(mbInfo.msg_copy);
      free(dup_msg);
      return rc;
   }
   while (looping) {
      XEvent input, ev;

      XNextEvent(mainDisplay, &input);
      if ((input.type==MapNotify && input.xany.window==mbInfo.main_win) ||
            (input.type==Expose && (input.xany.window==mbInfo.main_win ||
            input.xany.window==mbInfo.icon_win ||
            input.xany.window==mbInfo.msg_win ||
            input.xany.window==mbInfo.btn_win)) ||
            (!mbInfo.exposed &&
            (XCheckWindowEvent(mainDisplay,mbInfo.main_win,ExposureMask,&ev) ||
            XCheckWindowEvent(mainDisplay,mbInfo.main_win,StructureNotifyMask,
            &ev)))) {
         RefreshMsgBox(&mbInfo);
         mbInfo.exposed = TRUE;
         XSync(mainDisplay, False);
         if (input.xany.window==mbInfo.main_win ||
               input.xany.window==mbInfo.icon_win ||
               input.xany.window==mbInfo.msg_win ||
               input.xany.window==mbInfo.btn_win) {
            continue;
         }
      }
      if (input.type == Expose) {
         ExposeEventHandler(&input, FALSE);
      } else if (input.type == VisibilityNotify &&
            input.xany.window==mainWindow &&
            input.xvisibility.state==VisibilityUnobscured) {
         int i;

         while (XCheckWindowEvent(mainDisplay, mainWindow,
               VisibilityChangeMask, &ev)) ;
         if (pinnedMainMenu) XMapRaised(mainDisplay, mainMenuWindow);
         for (i = 0; i < numExtraWins; i++) {
            if (extraWinInfo[i].mapped && extraWinInfo[i].raise &&
                  extraWinInfo[i].window != None) {
               XMapRaised(mainDisplay, extraWinInfo[i].window);
            }
         }
         XMapRaised(mainDisplay, mbInfo.main_win);
      } else if (input.type == KeyPress) {
         if ((rc=HandleMsgBoxKeyEvent(&mbInfo, &input)) != INVALID) {
            break;
         }
      } else if (input.type==ButtonPress && input.xany.window==mbInfo.btn_win) {
         if ((rc=HandleMsgBoxBtnEvent(&mbInfo, &input)) != INVALID) {
            break;
         }
      }
   }
   if (mbInfo.msg_copy != NULL) free(mbInfo.msg_copy);
   free(dup_msg);

   XDestroyWindow(mainDisplay, mbInfo.main_win);
   if (warpToWinCenter) {
      XWarpPointer(mainDisplay, None, drawWindow, 0, 0, 0, 0,
            (int)(ZOOMED_SIZE(drawWinW)>>1), (int)(ZOOMED_SIZE(drawWinH)>>1));
   }
   return rc;
}

static
int DoDialog(Message, ReturnStr)
   char *Message, *ReturnStr;
{
   char *dup_msg=UtilStrDup(Message), szTitle[80];
   int rc=FALSE, looping=TRUE;

   if (dup_msg == NULL) {
      FailAllocMessage();
      return INVALID;
   }
   sprintf(szTitle, "%s - Input", TOOL_NAME);
   if (!SetupMBWindow(&mbInfo, dup_msg, szTitle, MB_ICON_DIALOG, TRUE)) {
      fprintf(stderr, "Invalid parameters passed to MsgBox().\n");
      Msg("Invalid parameters passed to MsgBox().\n");
      if (mbInfo.msg_copy != NULL) free(mbInfo.msg_copy);
      free(dup_msg);
      return INVALID;
   }
   if (ReturnStr != NULL) *ReturnStr = '\0';
   mbInfo.cur_x = ((mbInfo.btn_win_w)>>1);
   mbInfo.cursor_x = mbInfo.cur_x + 1;
   if (msgFontPtr == NULL) {
      mbInfo.cur_y = ((mbInfo.btn_win_h-defaultFontHeight)>>1)+defaultFontAsc;
      mbInfo.cursor_y = mbInfo.cur_y - defaultFontAsc +
            ((defaultFontAsc-16)>>1);
   } else {
      mbInfo.cur_y = ((mbInfo.btn_win_h-msgFontHeight)>>1)+msgFontAsc;
      mbInfo.cursor_y = mbInfo.cur_y - msgFontAsc + 
            ((msgFontAsc-16)>>1);
   }
   mbInfo.index = 0;
   mbInfo.return_str = ReturnStr;

   while (looping) {
      XEvent input, ev;

      XNextEvent(mainDisplay, &input);
      if ((input.type==MapNotify && input.xany.window==mbInfo.main_win) ||
            (input.type==Expose && (input.xany.window==mbInfo.main_win ||
            input.xany.window==mbInfo.icon_win ||
            input.xany.window==mbInfo.msg_win ||
            input.xany.window==mbInfo.btn_win)) ||
            (!mbInfo.exposed &&
            (XCheckWindowEvent(mainDisplay,mbInfo.main_win,ExposureMask,&ev) ||
            XCheckWindowEvent(mainDisplay,mbInfo.main_win,StructureNotifyMask,
            &ev)))) {
         RefreshMsgBox(&mbInfo);
         mbInfo.exposed = TRUE;
         XSync(mainDisplay, False);
         if (input.xany.window==mbInfo.main_win ||
               input.xany.window==mbInfo.icon_win ||
               input.xany.window==mbInfo.msg_win ||
               input.xany.window==mbInfo.btn_win) {
            continue;
         }
      }
      if (input.type == Expose) {
         ExposeEventHandler(&input, FALSE);
      } else if (input.type == VisibilityNotify &&
            input.xany.window==mainWindow &&
            input.xvisibility.state==VisibilityUnobscured) {
         int i;

         while (XCheckWindowEvent(mainDisplay, mainWindow,
               VisibilityChangeMask, &ev)) ;
         if (pinnedMainMenu) XMapRaised(mainDisplay, mainMenuWindow);
         for (i = 0; i < numExtraWins; i++) {
            if (extraWinInfo[i].mapped && extraWinInfo[i].raise &&
                  extraWinInfo[i].window != None) {
               XMapRaised(mainDisplay, extraWinInfo[i].window);
            }
         }
         XMapRaised(mainDisplay, mbInfo.main_win);
      } else if (input.type == KeyPress) {
         switch (HandleMsgBoxKeyEvent(&mbInfo, &input)) {
         case INVALID: looping = FALSE; rc = INVALID; break;
         case TRUE: looping = FALSE; rc = TRUE; break;
         case FALSE: break;
         }
      } else if (input.type==ButtonPress && input.xany.window==mbInfo.btn_win) {
         HandlePasteInDialog(&mbInfo, &input);
      }
   }
   if (mbInfo.msg_copy != NULL) free(mbInfo.msg_copy);
   free(dup_msg);

   XDestroyWindow(mainDisplay, mbInfo.main_win);
   if (warpToWinCenter) {
      XWarpPointer(mainDisplay, None, drawWindow, 0, 0, 0, 0,
            (int)(ZOOMED_SIZE(drawWinW)>>1), (int)(ZOOMED_SIZE(drawWinH)>>1));
   }
   return rc;
}

int Dialog(Message, Comment, ReturnStr)
   char *Message, *Comment, *ReturnStr;
   /* returns INVALID if <ESC> is types */
   /* returns FALSE otherwise */
   /* if Comment is NULL, "( <CR> or <ESC> to continue )" is assumed */
   /* if ReturnStr is NULL, hitting any key will return INVALID */
{
   char *real_msg=NULL, def_comment[MAXSTRING+1];
   int real_len=strlen(Message), rc;

   if (Comment == NULL) {
      strcpy(def_comment, "( <CR> or <ESC> to continue )");
      real_len += strlen(def_comment)+2;
   } else {
      real_len += strlen(Comment)+2;
   }
   real_msg = (char*)malloc((real_len+1)*sizeof(char));
   if (real_msg == NULL) {
      FailAllocMessage();
      return INVALID;
   }
   if (Comment == NULL) {
      sprintf(real_msg, "%s\n\n%s", Message, def_comment);
   } else {
      sprintf(real_msg, "%s\n\n%s", Message, Comment);
   }
   rc = DoDialog(real_msg, ReturnStr);
   free(real_msg);
   return rc;
}
