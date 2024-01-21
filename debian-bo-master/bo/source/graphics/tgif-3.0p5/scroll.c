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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/scroll.c,v 3.0 1996/05/06 16:07:22 william Exp $";
#endif

#include <stdio.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "choice.e"
#include "cursor.e"
#include "drawing.e"
#include "dup.e"
#include "grid.e"
#include "mainloop.e"
#include "msg.e"
#include "obj.e"
#include "page.e"
#include "raster.e"
#include "ruler.e"
#ifndef _NO_EXTERN
#include "scroll.e"
#endif
#include "setup.e"
#include "text.e"

#define FAKE_CM 80

int	autoPan=TRUE;

static int	scrollAreaH=0, scrollAreaW=0;

void UpdScrollWinWH()
{
   scrollAreaH = vSBarH;
   scrollAreaW = hSBarW;
}

void InitScroll()
{
   UpdScrollWinWH();
}

void RedrawVScrollWindow()
{
   double frac, start_frac;
   int block_h, block_start, rby;
   XGCValues values;
   XEvent ev;

   XSync(mainDisplay, False);
   while (XCheckWindowEvent(mainDisplay, vSBarWindow, ExposureMask, &ev)) ;

   rby = drawOrigY+drawWinH;
   start_frac = (double)((double)(drawOrigY)/(double)(max(paperHeight,rby)));
   block_start = (int)(scrollAreaH * start_frac);

   frac = (double)((double)drawWinH / (double)(max(paperHeight,rby)));
   if (frac > 1.0) frac = 1.0;

   if (start_frac + frac >= 1.0) {
      block_h = scrollAreaH - block_start;
   } else {
      block_h = (int)(scrollAreaH * frac);
   }

   if (block_h <= 0) block_h = 1;

   values.foreground = myBgPixel;
   values.background = myFgPixel;
   values.function = GXcopy;
   values.fill_style = FillSolid;
   XChangeGC(mainDisplay, patGC,
         GCForeground | GCBackground | GCFunction | GCFillStyle, &values);
   XFillRectangle(mainDisplay, vSBarWindow, patGC, 0, 0,
         scrollBarW, scrollAreaH);

   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.fill_style = FillOpaqueStippled;
   values.stipple = patPixmap[SCROLLPAT];
   XChangeGC(mainDisplay, patGC,
         GCForeground | GCBackground | GCFillStyle | GCStipple, &values);
   XFillRectangle(mainDisplay, vSBarWindow, patGC, 0, block_start,
         scrollBarW, block_h);
}

void RedrawHScrollWindow()
{
   double frac, start_frac;
   int block_w, block_start, rbx;
   XGCValues values;
   XEvent ev;

   XSync(mainDisplay, False);
   while (XCheckWindowEvent(mainDisplay, hSBarWindow, ExposureMask, &ev)) ;

   rbx = drawOrigX+drawWinW;
   start_frac = (double)((double)(drawOrigX)/(double)(max(paperWidth,rbx)));
   block_start = (int)(scrollAreaW * start_frac);

   frac = (double)((double)drawWinW / (double)(max(paperWidth,rbx)));
   if (frac > 1.0) frac = 1.0;

   if (start_frac + frac >= 1.0) {
      block_w = scrollAreaW - block_start;
   } else {
      block_w = (int)(scrollAreaW * frac);
   }

   if (block_w <= 0) block_w = 1;

   values.foreground = myBgPixel;
   values.background = myFgPixel;
   values.function = GXcopy;
   values.fill_style = FillSolid;
   XChangeGC(mainDisplay, patGC,
         GCForeground | GCBackground | GCFunction | GCFillStyle, &values);
   XFillRectangle(mainDisplay, hSBarWindow, patGC, 0, 0,
         scrollAreaW, scrollBarW);

   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.fill_style = FillOpaqueStippled;
   values.stipple = patPixmap[SCROLLPAT];
   XChangeGC(mainDisplay, patGC,
         GCForeground | GCBackground | GCFillStyle | GCStipple, &values);
   XFillRectangle(mainDisplay, hSBarWindow, patGC, block_start, 0,
         block_w, scrollBarW);
}

void RedrawScrollBars()
{
   RedrawVScrollWindow();
   RedrawHScrollWindow();
}

void ScrollTo(XOff, YOff)
   int XOff, YOff;
{
   int h_adjust=0, v_adjust=0;
   static int first_auto_pan_msg=TRUE;

   if (!autoPan || (XOff >= 0 && ABS_SIZE(XOff) < drawWinW &&
         YOff >= 0 && ABS_SIZE(YOff) < drawWinH)) {
      return;
   }

   if (XOff < 0) {
      if (ABS_SIZE(-XOff) > drawOrigX) {
         h_adjust = (-ZOOMED_SIZE(drawOrigX));
      } else {
         switch (gridSystem) {
         case ENGLISH_GRID:
            h_adjust = ((-XOff) % HALF_INCH == 0) ? (XOff) :
                  (((int)((-XOff)/HALF_INCH))+1) * (-HALF_INCH);
            break;
         case METRIC_GRID:
            if (zoomedIn && zoomScale > 1) {
               h_adjust = ((-XOff) % FAKE_CM == 0) ? (XOff) :
                     (((int)((-XOff)/FAKE_CM))+1) * (-FAKE_CM);
            } else {
               h_adjust = ((-XOff) % ONE_CM == 0) ? (XOff) :
                     (((int)((-XOff)/ONE_CM))+1) * (-ONE_CM);
            }
            break;
         }
      }
   } else if (ABS_SIZE(XOff) >= drawWinW) {
      switch (gridSystem) {
      case ENGLISH_GRID:
         h_adjust = (((int)((XOff-ZOOMED_SIZE(drawWinW))/HALF_INCH))+1) *
               HALF_INCH;
         break;
      case METRIC_GRID:
         if (zoomedIn && zoomScale > 1) {
            h_adjust = (((int)((XOff-ZOOMED_SIZE(drawWinW))/FAKE_CM))+1) *
                  FAKE_CM;
         } else {
            h_adjust = (((int)((XOff-ZOOMED_SIZE(drawWinW))/ONE_CM))+1) *
                  ONE_CM;
         }
         break;
      }
   }

   if (YOff < 0) {
      if (ABS_SIZE(-YOff) > drawOrigY) {
         v_adjust = (-ZOOMED_SIZE(drawOrigY));
      } else {
         switch (gridSystem) {
         case ENGLISH_GRID:
            v_adjust = ((-YOff) % HALF_INCH == 0) ? (YOff) :
                  (((int)((-YOff)/HALF_INCH))+1) * (-HALF_INCH);
            break;
         case METRIC_GRID:
            if (zoomedIn && zoomScale > 1) {
               v_adjust = ((-YOff) % FAKE_CM == 0) ? (YOff) :
                     (((int)((-YOff)/FAKE_CM))+1) * (-FAKE_CM);
            } else {
               v_adjust = ((-YOff) % ONE_CM == 0) ? (YOff) :
                     (((int)((-YOff)/ONE_CM))+1) * (-ONE_CM);
            }
            break;
         }
      }
   } else if (ABS_SIZE(YOff+(textCursorH>>1)) >= drawWinH) {
      switch (gridSystem) {
      case ENGLISH_GRID:
         v_adjust = (((int)((YOff+(textCursorH>>1) -
               ZOOMED_SIZE(drawWinH))/HALF_INCH))+1) * HALF_INCH;
         break;
      case METRIC_GRID:
         if (zoomedIn && zoomScale > 1) {
            v_adjust = (((int)((YOff+(textCursorH>>1) -
                  ZOOMED_SIZE(drawWinH))/FAKE_CM))+1) * FAKE_CM;
         } else {
            v_adjust = (((int)((YOff+(textCursorH>>1) -
                  ZOOMED_SIZE(drawWinH))/ONE_CM))+1) * ONE_CM;
         }
         break;
      }
   }
   if (h_adjust == 0 && v_adjust == 0) return;

   if (first_auto_pan_msg) {
      first_auto_pan_msg = FALSE;
      TwoLineMsg("To turn off auto-panning, set",
            "    Tgif*AutoPanInEditText to false in X defautls.");
   }

   drawOrigX += ABS_SIZE(h_adjust);
   drawOrigY += ABS_SIZE(v_adjust);
   RedrawScrollBars();
   UpdDrawWinBBox();
   AdjSplineVs();
   AdjustCurText((-h_adjust), (-v_adjust));
   RedrawRulers();
   ClearAndRedrawDrawWindow();
}

void ScrollUp(button_ev)
   XButtonEvent *button_ev;
{
   int adjustment=0;

   if (drawOrigY != 0) {
      switch (gridSystem) {
      case ENGLISH_GRID:
         if (button_ev!=NULL &&
               (button_ev->state & (ShiftMask|ControlMask))) {
            adjustment = ((int)(ZOOMED_SIZE(drawWinH)/HALF_INCH))*HALF_INCH;
            if (drawOrigY-ABS_SIZE(adjustment) < 0) {
               adjustment = ZOOMED_SIZE(drawOrigY);
            }
         } else {
            adjustment = HALF_INCH;
         }
         break;
      case METRIC_GRID:
         if (zoomedIn && zoomScale > 1) {
            if (button_ev!=NULL &&
                  (button_ev->state & (ShiftMask|ControlMask))) {
               adjustment = ((int)(ZOOMED_SIZE(drawWinH)/FAKE_CM))*FAKE_CM;
               if (drawOrigY-ABS_SIZE(adjustment) < 0)
                  adjustment = ZOOMED_SIZE(drawOrigY);
            } else {
               adjustment = FAKE_CM;
            }
         } else {
            if (button_ev!=NULL &&
                  (button_ev->state & (ShiftMask|ControlMask))) {
               adjustment = ((int)(ZOOMED_SIZE(drawWinH)/ONE_CM))*ONE_CM;
               if (drawOrigY-ABS_SIZE(adjustment) < 0)
                  adjustment = ZOOMED_SIZE(drawOrigY);
            } else {
               adjustment = ONE_CM;
            }
         }
         break;
      }
      drawOrigY -= ABS_SIZE(adjustment);
      RedrawVScrollWindow();
      UpdDrawWinBBox();
      AdjSplineVs();
      AdjustCurText(0, adjustment);
      RedrawRulers();
      ClearAndRedrawDrawWindow();
   }
}

void ForceScrollDown(ScrollAFullWindow)
   int ScrollAFullWindow;
{
   int adjustment=0;

   switch (gridSystem) {
   case ENGLISH_GRID:
      if (ScrollAFullWindow) {
         adjustment = ((int)(ZOOMED_SIZE(drawWinH)/HALF_INCH))*HALF_INCH;
      } else {
         adjustment = HALF_INCH;
      }
      break;
   case METRIC_GRID:
      if (zoomedIn && zoomScale > 1) {
         if (ScrollAFullWindow) {
            adjustment = ((int)(ZOOMED_SIZE(drawWinH)/FAKE_CM))*FAKE_CM;
         } else {
            adjustment = FAKE_CM;
         }
      } else {
         if (ScrollAFullWindow) {
            adjustment = ((int)(ZOOMED_SIZE(drawWinH)/ONE_CM))*ONE_CM;
         } else {
            adjustment = ONE_CM;
         }
      }
      break;
   }
   drawOrigY += ABS_SIZE(adjustment);
   RedrawVScrollWindow();
   UpdDrawWinBBox();
   AdjSplineVs();
   AdjustCurText(0, -adjustment);
   RedrawRulers();
   ClearAndRedrawDrawWindow();
}

void ScrollDown(button_ev)
   XButtonEvent *button_ev;
{
   if (paperHeight <= drawWinH) return;

   if (drawOrigY+drawWinH < paperHeight) {
      ForceScrollDown(button_ev != NULL &&
            (button_ev->state & (ShiftMask|ControlMask)));
   }
}

static
void VSBarHandler(button_ev)
   XButtonEvent *button_ev;
{
   double frac, start_frac;
   int block_h, block_start, adjustment, new_start=0, saved_y;

   if (button_ev->button == Button3) {
      ScrollUp (button_ev);
   } else if (button_ev->button == Button1) {
      ScrollDown (button_ev);
   } else if (button_ev->button == Button2) {
      int done=FALSE, lty;
      XGCValues values;
      XEvent ev;

      block_start = button_ev->y;
      start_frac = (double)((double)(block_start)/(double)(scrollAreaH));

      frac = (double)((double)drawWinH / (double)(paperHeight));

      block_h = (frac >= 1.0) ? scrollAreaH : (int)(scrollAreaH * frac);
      if (block_h <= 0) block_h = 1;
      lty = (start_frac + frac >= 1.0) ? scrollAreaH-block_h : block_start;

      values.foreground = myFgPixel;
      values.background = myBgPixel;
      values.fill_style = FillOpaqueStippled;
      values.stipple = patPixmap[SCROLLPAT];
      XChangeGC(mainDisplay, patGC,
            GCForeground | GCBackground | GCFillStyle | GCStipple, &values);

      XClearWindow(mainDisplay, vSBarWindow);
      XFillRectangle(mainDisplay, vSBarWindow, patGC, 0, lty, scrollBarW,
            block_h);
      XGrabPointer(mainDisplay, vSBarWindow, False,
            PointerMotionMask | ButtonReleaseMask, GrabModeAsync,
            GrabModeAsync, None, handCursor, CurrentTime);

      while (!done) {
         XNextEvent(mainDisplay, &ev);

         if (ev.type == Expose || ev.type == VisibilityNotify) {
            ExposeEventHandler(&ev, TRUE);
         } else if (ev.type == ButtonRelease) {
            XUngrabPointer(mainDisplay, CurrentTime);
            done = TRUE;
            XClearWindow(mainDisplay, vSBarWindow);
            XFillRectangle(mainDisplay, vSBarWindow, patGC, 0, lty, scrollBarW,
                  block_h);
         } else if (ev.type == MotionNotify) {
            int new_y;

            block_start = ev.xmotion.y;

            if (block_start <= 0) {
               new_y = 0;
            } else if (block_start+block_h >= scrollAreaH) {
               new_y = scrollAreaH-block_h;
            } else {
               new_y = block_start;
            }

            if (new_y != lty) {
               XClearWindow(mainDisplay, vSBarWindow);
               XFillRectangle(mainDisplay, vSBarWindow, patGC, 0, lty,
                  scrollBarW, block_h);
               lty = new_y;
               XClearWindow(mainDisplay, vSBarWindow);
               XFillRectangle(mainDisplay, vSBarWindow, patGC, 0, lty,
                  scrollBarW, block_h);
            }
            while (XCheckMaskEvent(mainDisplay, PointerMotionMask, &ev)) ;
         }
      }

      start_frac = (double)((double)(block_start)/(double)(scrollAreaH));
      if (start_frac <= 0.0) start_frac = 0.0;
      if (block_start+block_h >= scrollAreaH) {
         saved_y = drawOrigY;

         if (paperHeight <= drawWinH) {
            drawOrigY = 0;
         } else {
            switch (gridSystem) {
            case ENGLISH_GRID:
               if ((paperHeight-drawWinH) % ABS_SIZE(HALF_INCH) == 0) {
                  drawOrigY = paperHeight-drawWinH;
               } else {
                  drawOrigY = max(0, ((int)((paperHeight-drawWinH)/
                        ABS_SIZE(HALF_INCH)) + 1) * ABS_SIZE(HALF_INCH));
               }
               break;
            case METRIC_GRID:
               if (zoomedIn && zoomScale > 1) {
                  if ((paperHeight-drawWinH) % ABS_SIZE(FAKE_CM) == 0) {
                     drawOrigY = paperHeight-drawWinH;
                  } else {
                     drawOrigY = max(0, ((int)((paperHeight-drawWinH)/
                           ABS_SIZE(FAKE_CM)) + 1) * ABS_SIZE(FAKE_CM));
                  }
               } else {
                  if ((paperHeight-drawWinH) % ABS_SIZE(ONE_CM) == 0) {
                     drawOrigY = paperHeight-drawWinH;
                  } else {
                     drawOrigY = max(0, ((int)((paperHeight-drawWinH)/
                           ABS_SIZE(ONE_CM)) + 1) * ABS_SIZE(ONE_CM));
                  }
               }
               break;
            }
         }

         adjustment = saved_y - drawOrigY;
         if (adjustment != 0) {
            RedrawVScrollWindow();
            UpdDrawWinBBox();
            AdjSplineVs();
            AdjustCurText(0, ZOOMED_SIZE(adjustment));
            RedrawRulers();
            ClearAndRedrawDrawWindow();
         } else {
            RedrawVScrollWindow();
         }
         return;
      } else {
         switch (gridSystem) {
         case ENGLISH_GRID:
            new_start = ((int)(paperHeight*start_frac/
                  ABS_SIZE(HALF_INCH)))*ABS_SIZE(HALF_INCH);
            break;
         case METRIC_GRID:
            if (zoomedIn && zoomScale > 1) {
               new_start = ((int)(paperHeight*start_frac/
                     ABS_SIZE(FAKE_CM)))*ABS_SIZE(FAKE_CM);
            } else {
               new_start = ((int)(paperHeight*start_frac/
                     ABS_SIZE(ONE_CM)))*ABS_SIZE(ONE_CM);
            }
            break;
         }
         adjustment = drawOrigY - new_start;
         if (adjustment != 0) {
            drawOrigY = new_start;
            RedrawVScrollWindow();
            UpdDrawWinBBox();
            AdjSplineVs();
            AdjustCurText(0, ZOOMED_SIZE(adjustment));
            RedrawRulers();
            ClearAndRedrawDrawWindow();
         } else {
            RedrawVScrollWindow();
         }
         return;
      }
   }
}

void ScrollLeft(button_ev)
   XButtonEvent *button_ev;
{
   int adjustment=0;

   if (drawOrigX != 0) {
      switch (gridSystem) {
      case ENGLISH_GRID:
         if (button_ev!=NULL &&
               (button_ev->state & (ShiftMask|ControlMask))) {
            adjustment = ((int)(ZOOMED_SIZE(drawWinW)/HALF_INCH))*HALF_INCH;
            if (drawOrigX-ABS_SIZE(adjustment) < 0) {
               adjustment = ZOOMED_SIZE(drawOrigX);
            }
         } else {
            adjustment = HALF_INCH;
         }
         break;
      case METRIC_GRID:
         if (zoomedIn && zoomScale > 1) {
            if (button_ev!=NULL &&
                  (button_ev->state & (ShiftMask|ControlMask))) {
               adjustment = ((int)(ZOOMED_SIZE(drawWinW)/FAKE_CM))*FAKE_CM;
               if (drawOrigX-ABS_SIZE(adjustment) < 0) {
                  adjustment = ZOOMED_SIZE(drawOrigX);
               }
            } else {
               adjustment = FAKE_CM;
            }
         } else {
            if (button_ev!=NULL &&
                  (button_ev->state & (ShiftMask|ControlMask))) {
               adjustment = ((int)(ZOOMED_SIZE(drawWinW)/ONE_CM))*ONE_CM;
               if (drawOrigX-ABS_SIZE(adjustment) < 0) {
                  adjustment = ZOOMED_SIZE(drawOrigX);
               }
            } else {
               adjustment = ONE_CM;
            }
         }
         break;
      }
      drawOrigX -= ABS_SIZE(adjustment);
      RedrawHScrollWindow();
      UpdDrawWinBBox();
      AdjSplineVs();
      AdjustCurText(adjustment, 0);
      RedrawHRuler();
      ClearAndRedrawDrawWindow();
   }
}

void ScrollRight(button_ev)
   XButtonEvent *button_ev;
{
   int adjustment=0;

   if (paperWidth <= drawWinW) return;

   if (drawOrigX+drawWinW < paperWidth) {
      switch (gridSystem) {
      case ENGLISH_GRID:
         if (button_ev!=NULL && (button_ev->state & (ShiftMask|ControlMask))) {
            adjustment = ((int)(ZOOMED_SIZE(drawWinW)/HALF_INCH))*HALF_INCH;
         } else {
            adjustment = HALF_INCH;
         }
         break;
      case METRIC_GRID:
         if (zoomedIn && zoomScale > 1) {
            if (button_ev!=NULL &&
                  (button_ev->state & (ShiftMask|ControlMask))) {
               adjustment = ((int)(ZOOMED_SIZE(drawWinW)/FAKE_CM))*FAKE_CM;
            } else {
               adjustment = FAKE_CM;
            }
         } else {
            if (button_ev!=NULL &&
                  (button_ev->state & (ShiftMask|ControlMask))) {
               adjustment = ((int)(ZOOMED_SIZE(drawWinW)/ONE_CM))*ONE_CM;
            } else {
               adjustment = ONE_CM;
            }
         }
         break;
      }
      drawOrigX += ABS_SIZE(adjustment);
      RedrawHScrollWindow();
      UpdDrawWinBBox();
      AdjSplineVs();
      AdjustCurText(-adjustment, 0);
      RedrawHRuler();
      ClearAndRedrawDrawWindow();
   }
}

static
void HSBarHandler(button_ev)
   XButtonEvent *button_ev;
{
   double frac, start_frac;
   int block_w, block_start, adjustment, new_start=0, saved_x;

   if (button_ev->button == Button3) {
      ScrollLeft(button_ev);
   } else if (button_ev->button == Button1) {
      ScrollRight(button_ev);
   } else if (button_ev->button == Button2) {
      int done=FALSE, ltx;
      XGCValues values;
      XEvent ev;

      block_start = button_ev->x - scrollBarW;
      start_frac = (double)((double)(block_start)/(double)(scrollAreaW));

      frac = (double)((double)drawWinW / (double)(paperWidth));

      block_w = (frac >= 1.0) ? scrollAreaW : (int)(scrollAreaW * frac);
      if (block_w <= 0) block_w = 1;
      ltx = (start_frac + frac >= 1.0) ? scrollAreaW-block_w : block_start;

      values.foreground = myFgPixel;
      values.background = myBgPixel;
      values.fill_style = FillOpaqueStippled;
      values.stipple = patPixmap[SCROLLPAT];
      XChangeGC(mainDisplay, patGC,
            GCForeground | GCBackground | GCFillStyle | GCStipple, &values);

      XClearWindow(mainDisplay, hSBarWindow);
      XFillRectangle(mainDisplay, hSBarWindow, patGC, ltx, 0, block_w,
            scrollBarW);
      XGrabPointer(mainDisplay, hSBarWindow, False,
            PointerMotionMask | ButtonReleaseMask, GrabModeAsync,
            GrabModeAsync, None, handCursor, CurrentTime);

      while (!done) {
         XNextEvent(mainDisplay, &ev);

         if (ev.type == Expose || ev.type == VisibilityNotify) {
            ExposeEventHandler(&ev, TRUE);
         } else if (ev.type == ButtonRelease) {
            XUngrabPointer(mainDisplay, CurrentTime);
            done = TRUE;
            XClearWindow(mainDisplay, hSBarWindow);
            XFillRectangle(mainDisplay, hSBarWindow, patGC, ltx, 0, block_w,
                  scrollBarW);
         } else if (ev.type == MotionNotify) {
            int new_x;

            block_start = ev.xmotion.x;

            if (block_start <= 0) {
               new_x = 0;
            } else if (block_start+block_w >= scrollAreaW) {
               new_x = scrollAreaW-block_w;
            } else {
               new_x = block_start;
            }

            if (new_x != ltx) {
               XClearWindow(mainDisplay, hSBarWindow);
               XFillRectangle(mainDisplay, hSBarWindow, patGC, ltx, 0,
                  block_w, scrollBarW);
               ltx = new_x;
               XClearWindow(mainDisplay, hSBarWindow);
               XFillRectangle(mainDisplay, hSBarWindow, patGC, ltx, 0,
                  block_w, scrollBarW);
            }
            while (XCheckMaskEvent(mainDisplay, PointerMotionMask, &ev)) ;
         }
      }

      start_frac = (double)((double)(block_start)/(double)(scrollAreaW));
      if (start_frac <= 0.0) start_frac = 0.0;
      if (block_start+block_w >= scrollAreaW) {
         saved_x = drawOrigX;

         if (paperWidth <= drawWinW) {
             drawOrigX = 0;
         } else {
            switch (gridSystem) {
            case ENGLISH_GRID:
               if ((paperWidth-drawWinW) % ABS_SIZE(HALF_INCH) == 0) {
                  drawOrigX = paperWidth-drawWinW;
               } else {
                  drawOrigX = max(0, ((int)((paperWidth-drawWinW)/
                        ABS_SIZE(HALF_INCH)) + 1) * ABS_SIZE(HALF_INCH));
               }
               break;
            case METRIC_GRID:
               if (zoomedIn && zoomScale > 1) {
                  if ((paperWidth-drawWinW) % ABS_SIZE(FAKE_CM) == 0) {
                     drawOrigX = paperWidth-drawWinW;
                  } else {
                     drawOrigX = max(0, ((int)((paperWidth-drawWinW)/
                           ABS_SIZE(FAKE_CM)) + 1) * ABS_SIZE(FAKE_CM));
                  }
               } else {
                  if ((paperWidth-drawWinW) % ABS_SIZE(ONE_CM) == 0) {
                     drawOrigX = paperWidth-drawWinW;
                  } else {
                     drawOrigX = max(0, ((int)((paperWidth-drawWinW)/
                           ABS_SIZE(ONE_CM)) + 1) * ABS_SIZE(ONE_CM));
                  }
               }
               break;
            }
         }

         adjustment = saved_x - drawOrigX;
         if (adjustment != 0) {
            RedrawHScrollWindow();
            UpdDrawWinBBox();
            AdjSplineVs();
            AdjustCurText(ZOOMED_SIZE(adjustment), 0);
            RedrawHRuler();
            ClearAndRedrawDrawWindow();
         } else {
            RedrawHScrollWindow();
         }
         return;
      } else {
         switch (gridSystem) {
         case ENGLISH_GRID:
            new_start = ((int)(paperWidth*start_frac/
                  ABS_SIZE(HALF_INCH)))*ABS_SIZE(HALF_INCH);
            break;
         case METRIC_GRID:
            if (zoomedIn && zoomScale > 1) {
               new_start = ((int)(paperWidth*start_frac/
                     ABS_SIZE(FAKE_CM)))*ABS_SIZE(FAKE_CM);
            } else {
               new_start = ((int)(paperWidth*start_frac/
                     ABS_SIZE(ONE_CM)))*ABS_SIZE(ONE_CM);
            }
            break;
         }
         adjustment = drawOrigX - new_start;
         if (adjustment != 0) {
            drawOrigX = new_start;
            RedrawHScrollWindow();
            UpdDrawWinBBox();
            AdjSplineVs();
            AdjustCurText(ZOOMED_SIZE(adjustment), 0);
            RedrawHRuler();
            ClearAndRedrawDrawWindow();
         } else {
            RedrawHScrollWindow();
         }
         return;
      }
   }
}

void ScrollEventHandler(input)
   XEvent *input;
{
   if (input->xany.window == vSBarWindow) {
      if (input->type == Expose) {
         XSync(mainDisplay, False);
         RedrawVScrollWindow();
         return;
      } else if (input->type == EnterNotify) {
         SetMouseStatus("scroll down", "scroll vertically", "scroll up");
         return;
      }
      Msg(""); 
      VSBarHandler(&(input->xbutton));
      return;
   } else if (input->xany.window == hSBarWindow) {
      if (input->type == Expose) {
         XSync(mainDisplay, False);
         RedrawHScrollWindow();
         return;
      } else if (input->type == EnterNotify) {
         SetMouseStatus("scroll right", "scroll horizontally", "scroll left");
         return;
      }
      Msg(""); 
      HSBarHandler(&(input->xbutton));
      return;
   }
}

void ScrollToSpecifiedOrigin(cur_page_num, orig_x, orig_y, zoom_scale,
      zoomed_in)
   int cur_page_num, orig_x, orig_y, zoom_scale, zoomed_in;
{
   int adj_caches=(zoomScale != zoom_scale || zoomedIn != zoomed_in);

   if (cur_page_num <= 0 || cur_page_num > lastPageNum) return;
   if (drawOrigX == orig_x && drawOrigY == orig_y && !adj_caches) return;

   drawOrigX = orig_x;
   drawOrigY = orig_y;
   zoomScale = zoom_scale;
   zoomedIn = zoomed_in;
   UpdDrawWinWH();

   UpdDrawWinBBox();
   AdjSplineVs();
   if (adj_caches) { AdjCaches(); ShowZoom(); }

   if (cur_page_num != curPageNum) {
      GotoPageNum(cur_page_num);
      ShowPage();
   }
   ClearAndRedrawDrawWindow();
   RedrawRulers();
   RedrawScrollBars();
   justDupped = FALSE;
}

static int xOrigin=0, yOrigin=0, zoomScaleOrigin, zoomedInOrigin;

void ScrollToOrigin()
{
   int adj_caches=(zoomScale!=zoomScaleOrigin || zoomedIn!=zoomedInOrigin);

   if (drawOrigX==xOrigin && drawOrigY==yOrigin && !adj_caches) {
      return;
   }
   TieLooseEnds();
   SetCurChoice(NOTHING);

   drawOrigX = xOrigin;
   drawOrigY = yOrigin;
   zoomScale = zoomScaleOrigin;
   zoomedIn = zoomedInOrigin;
   UpdDrawWinWH();

   UpdDrawWinBBox();
   AdjSplineVs();
   if (adj_caches) { AdjCaches(); ShowZoom(); }
   ClearAndRedrawDrawWindow();
   RedrawRulers();
   RedrawScrollBars();
   justDupped = FALSE;
}

void SaveOrigin()
{
   xOrigin = drawOrigX;
   yOrigin = drawOrigY;
   zoomScaleOrigin = zoomScale;
   zoomedInOrigin = zoomedIn;
}

void CleanUpScrolls()
{
}
