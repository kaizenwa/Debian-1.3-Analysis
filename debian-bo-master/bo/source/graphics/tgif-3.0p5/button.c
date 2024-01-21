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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/button.c,v 3.0 1996/05/06 16:03:56 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "const.h"
#include "types.h"

#include "auxtext.e"
#include "box.e"
#ifndef _NO_EXTERN
#include "button.e"
#endif
#include "cursor.e"
#include "file.e"
#include "font.e"
#include "mainloop.e"
#include "mainmenu.e"
#include "msg.e"
#include "raster.e"
#include "rect.e"
#include "setup.e"

static int twoSpaceWidth=0;
static int twoSpaceWidthInitialized=FALSE;

static
void SetTwoSpaceWidth()
{
   if (boldMsgFontPtr == NULL) return;
   if (!twoSpaceWidthInitialized) {
      twoSpaceWidthInitialized = TRUE;
      twoSpaceWidth = XTextWidth(boldMsgFontPtr, "  ", 2);
   }
}

int ButtonWidth(Str, Len)
   char *Str;
   int Len;
{
   int len=strlen(Str);

   if (boldMsgFontPtr == NULL) {
      return (defaultFontWidth * max(len+2, Len));
   } else {
      SetTwoSpaceWidth();
      return 4+XTextWidth(boldMsgFontPtr, "  CANCEL  ", 10);
   }
}

void DisplayButton(Win, Str, Len, BBox, Normal)
   Window Win;
   char *Str;
   int Len;
   struct BBRec *BBox;
   int Normal;
   /* Display a button in Win at location ((*BBox).ltx,(*BBox).lty), Str is */
   /*    centered in the button.  The width of the button is the width of */
   /*    Str + 2 character widths or the width of Len number of characters, */
   /*    whichever is bigger.  *BBox will be set with the bounding */
   /*    box of the button. */
{
   int button_w, button_h, len_of_str, left;

   len_of_str = strlen(Str);
   button_w = ButtonWidth(Str, Len);
   if (boldMsgFontPtr == NULL) {
      left = ((button_w - defaultFontWidth * len_of_str)>>1);
      button_h = defaultFontHeight + 4;
   } else {
      left = ((button_w - XTextWidth(boldMsgFontPtr, Str, len_of_str))>>1);
      button_h = boldMsgFontHeight + 4;
   }
   BBox->rbx = BBox->ltx + button_w - 1;
   BBox->rby = BBox->lty + button_h - 1;
   if (Normal) {
      XSetForeground(mainDisplay, defaultGC, myBgPixel);
      XFillRectangle(mainDisplay, Win, defaultGC, BBox->ltx, BBox->lty,
            button_w, button_h);
      XSetForeground(mainDisplay, defaultGC, myFgPixel);

      XDrawRectangle(mainDisplay, Win, defaultGC, BBox->ltx, BBox->lty,
            button_w, button_h);
      if (boldMsgFontPtr == NULL) {
         XDrawString(mainDisplay, Win, defaultGC, BBox->ltx+left,
               BBox->lty+defaultFontAsc+2, Str, len_of_str);
      } else {
         XSetFont(mainDisplay, defaultGC, boldMsgFontPtr->fid);
         XDrawString(mainDisplay, Win, defaultGC, BBox->ltx+left,
               BBox->lty+boldMsgFontAsc+2, Str, len_of_str);
         XSetFont(mainDisplay, defaultGC, defaultFontPtr->fid);
      }
   } else {
      XFillRectangle(mainDisplay, Win, revDefaultGC,
            BBox->ltx, BBox->lty, button_w, button_h);
   }
}

void DisplayButtonInBBox(Win, Str, Len, BBox, Normal, HighLight, Width)
   Window Win;
   char *Str;
   int Len, HighLight, Width;
   struct BBRec *BBox;
   int Normal;
   /* Display a button in Win at location ((*BBox).ltx,(*BBox).lty), Str is */
   /*    centered in the button.  The width of the button is given in BBox. */
   /* Len must be strlen(Str) */
   /* If HighLight is TRUE, Width is used to draw an outline around the box. */
{
   int button_w, button_h, left, top, text_w;

   button_w = BBox->rbx - BBox->ltx;
   button_h = BBox->rby - BBox->lty;
   if (boldMsgFontPtr == NULL) {
      text_w = defaultFontWidth * Len;
      left = ((button_w - text_w)>>1);
      top = ((button_h - defaultFontHeight)>>1);
   } else {
      text_w = XTextWidth(boldMsgFontPtr, Str, Len);
      left = ((button_w - text_w)>>1);
      top = ((button_h - boldMsgFontHeight)>>1);
   }
   if (Normal) {
      XSetForeground(mainDisplay, defaultGC, myBgPixel);
      XFillRectangle(mainDisplay, Win, defaultGC, BBox->ltx, BBox->lty,
            button_w, button_h);
      XSetForeground(mainDisplay, defaultGC, myFgPixel);

      XDrawRectangle(mainDisplay, Win, defaultGC, BBox->ltx, BBox->lty,
            button_w, button_h);
      if (boldMsgFontPtr == NULL) {
         XDrawString(mainDisplay, Win, defaultGC, BBox->ltx+left,
               BBox->lty+defaultFontAsc+2, Str, Len);
      } else {
         XSetFont(mainDisplay, defaultGC, boldMsgFontPtr->fid);
         XDrawString(mainDisplay, Win, defaultGC, BBox->ltx+left,
               BBox->lty+boldMsgFontAsc+2, Str, Len);
         XSetFont(mainDisplay, defaultGC, defaultFontPtr->fid);
      }
   } else {
      XFillRectangle(mainDisplay, Win, revDefaultGC, BBox->ltx, BBox->lty,
            button_w, button_h);
   }
   if (HighLight) {
      XDrawRectangle(mainDisplay, Win, defaultGC, BBox->ltx-Width,
            BBox->lty-Width, button_w+(Width<<1), button_h+(Width<<1));
   }
}
