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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/ruler.c,v 3.0 1996/05/06 16:07:18 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "cursor.e"
#include "dialog.e"
#include "font.e"
#include "grid.e"
#include "mainmenu.e"
#include "msg.e"
#include "raster.e"
#ifndef _NO_EXTERN
#include "ruler.e"
#endif
#include "setup.e"
#include "util.e"

#define INCH_H 8
#define HALF_INCH_H 6
#define QUARTER_INCH_H 4
#define MIN_INCH_H 2

int	showMeasurement=FALSE;
int	showCrossHair=FALSE;

static float	gfPixelsPerUnit=(float)1.0;
static float	gfNumUnits=(float)1.0;
static char	numUnitStr[80], baseUnitStr[80], unitStr[80];

static GC	rulerGC;

static int	oldXOff=(-1), oldYOff=(-1);
static int	rulerLen=0;
static int	hRulerJustRedrawn=TRUE, justUnFrozen=FALSE;
static int	freezeMarkRulerText=FALSE, frozenXOff=0, frozenYOff=0;

void GetUnitSpec(buf)
   char *buf;
{
   sprintf(buf, "%s %s/%s",
         (*numUnitStr)=='\0' ? "1" : numUnitStr,
         (*baseUnitStr)=='\0' ? "pixel" : baseUnitStr,
         (*unitStr)=='\0' ? "pixel" : unitStr);
}

void ShowUnitMsg()
{
   sprintf(gszMsgBox, "Measurement will be shown in '%s' (%s %s).",
         (*unitStr)=='\0' ? "pixel" : unitStr,
         (*numUnitStr)=='\0' ? "1" : numUnitStr,
         (*baseUnitStr)=='\0' ? "pixel" : baseUnitStr);
   Msg(gszMsgBox);
}

#define INT_TOL (1.0e-5)

static
int BadUnit(spec)
   char *spec;
{
   if (msgWindow != None) {
      sprintf(gszMsgBox, "Bad measurement unit specification '%s'.", spec);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   }
   return FALSE;
}

int SetUnit(spec)
   char *spec;
{
   char *spec_copy=UtilStrDup(spec);
   char *num_ptr=NULL, *base_ptr=NULL, *unit_ptr=NULL;
   int ok=TRUE;

   if (spec_copy == NULL) return FALSE;
   if ((num_ptr=strtok(spec_copy, " \t\n\r")) != NULL &&
         (base_ptr=strtok(NULL, "/ \t\n\r")) != NULL &&
         (unit_ptr=strtok(NULL, "/ \t\n\r")) != NULL) {
      float fval;

      if (sscanf(num_ptr, "%f", &fval) == 1 && fval > INT_TOL &&
            *base_ptr != '\0' && *unit_ptr != '\0') {
         gfNumUnits = fval;
         strcpy(numUnitStr, num_ptr);
         if (UtilStrICmp("pixel", unit_ptr) == 0) {
            *unitStr = '\0';
         } else {
            strcpy(unitStr, unit_ptr);
         }
         switch (*base_ptr) {
         case 'i':
         case 'I':
            gfPixelsPerUnit = gfNumUnits * ((float)ONE_INCH);
            strcpy(baseUnitStr, "in");
            break;
         case 'c':
         case 'C':
            gfPixelsPerUnit = gfNumUnits * ((float)ONE_CM);
            strcpy(baseUnitStr, "cm");
            break;
         case 'p':
         case 'P':
            gfPixelsPerUnit = gfNumUnits;
            *baseUnitStr = '\0';
            break;
         default: ok = BadUnit(spec); break;
         }
      } else {
         ok = BadUnit(spec);
      }
   } else {
      ok = BadUnit(spec);
   }
   if (!ok) {
      gfPixelsPerUnit = gfNumUnits = (float)1.0;
      *numUnitStr = *baseUnitStr = *unitStr = '\0';
   }
   free(spec_copy);
   return ok;
}

void InitRuler ()
{
   XGCValues	values;
   char		* c_ptr;

   values.foreground = myFgPixel;
   values.background = myBgPixel;
   values.fill_style = FillSolid;
   values.font = rulerFontPtr->fid;

   rulerGC = XCreateGC (mainDisplay, mainWindow,
         GCForeground | GCBackground | GCFillStyle | GCFont, &values);

   showMeasurement = FALSE;
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "ShowMeasurement")) != NULL &&
         UtilStrICmp(c_ptr, "true") == 0) {
      showMeasurement = TRUE;
   }
   showCrossHair = FALSE;
/* if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "ShowCrossHair")) != NULL &&
         UtilStrICmp(c_ptr, "true") == 0) {
      showCrossHair = TRUE;
   } */

   gfPixelsPerUnit = gfNumUnits = (float)1.0;
   *numUnitStr = *baseUnitStr = *unitStr = '\0';
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "ShowMeasurementUnit")) !=
         NULL) {
      char spec[80];

      if (strcmp("pixel", c_ptr) == 0) {
         strcpy(spec, "1 pixel/pixel");
      } else if (strcmp("inch", c_ptr) == 0) {
         sprintf(spec, "%1d pixel/in", ONE_INCH);
      } else if (strcmp("cm", c_ptr) == 0) {
         sprintf(spec, "%1d pixel/cm", ONE_CM);
      } else {
         strcpy(spec, c_ptr);
      }
      if (!SetUnit(spec)) {
         fprintf (stderr, "Invalid %s*%s: '%s', pixel is used.\n",
               TOOL_NAME, "ShowMeasurementUnit", spec);
      }
   }
   rulerLen = rulerW-1;
}

void CleanUpRuler ()
{
   XFreeGC (mainDisplay, rulerGC);
}

void RedrawHRuler ()
{
   register int	i, pos, len, index;
   int		inc, abs_inc;
   char		s[5];
   XEvent	ev;

   XClearWindow (mainDisplay, hRuleWindow);

   XSync (mainDisplay, False);
   while (XCheckWindowEvent (mainDisplay, hRuleWindow, ExposureMask, &ev)) ;

   switch (gridSystem)
   {
      case ENGLISH_GRID:
         inc = (zoomedIn ? (xyEnglishGrid<<zoomScale) : xyEnglishGrid);
         abs_inc = ABS_SIZE(inc);

         if (drawOrigX % abs_inc == 0)
         {
            i = 0;
            pos = 0;
         }
         else
         {
            i = ((int)(drawOrigX / abs_inc) + 1) * abs_inc - drawOrigX;
            pos = ZOOMED_SIZE(i);
         }
         for ( ; i<drawWinW; i+=abs_inc, pos+=inc)
         {
            if ((GRID_ZOOMED_SIZE(i+drawOrigX)) % ONE_INCH == 0)
            {
               index = (int)((GRID_ZOOMED_SIZE(i+drawOrigX)) / ONE_INCH);
               sprintf (s, "%1d", GRID_ABS_SIZE(index));
               len = strlen (s);
               XDrawString (mainDisplay, hRuleWindow, rulerGC,
                     pos-((len*rulerFontWidth)>>1),
                     rulerW-INCH_H-rulerFontDes, s, len);
               XDrawLine (mainDisplay, hRuleWindow, defaultGC, pos, rulerW,
                     pos, rulerW-INCH_H);
            }
            else if ((GRID_ZOOMED_SIZE(i+drawOrigX)) % HALF_INCH == 0)
               XDrawLine (mainDisplay, hRuleWindow, defaultGC, pos, rulerW,
                     pos, rulerW-HALF_INCH_H);
            else if ((GRID_ZOOMED_SIZE(i+drawOrigX)) % QUARTER_INCH == 0)
               XDrawLine (mainDisplay, hRuleWindow, defaultGC, pos, rulerW,
                     pos, rulerW-QUARTER_INCH_H);
            else
               XDrawLine (mainDisplay, hRuleWindow, defaultGC, pos, rulerW,
                     pos, rulerW-MIN_INCH_H);
         }
         break;
      case METRIC_GRID:
         inc = (zoomedIn ? (xyMetricGrid<<zoomScale) : xyMetricGrid);
         abs_inc = ABS_SIZE(inc);

         if (drawOrigX % abs_inc == 0)
         {
            i = 0;
            pos = 0;
         }
         else
         {
            i = ((int)(drawOrigX / abs_inc) + 1) * abs_inc - drawOrigX;
            pos = ZOOMED_SIZE(i);
         }
         for ( ; i<drawWinW; i+=abs_inc, pos+=inc)
         {
            if ((GRID_ZOOMED_SIZE(i+drawOrigX)) % ONE_CM == 0)
            {
               index = (int)((GRID_ZOOMED_SIZE(i+drawOrigX)) / ONE_CM);
               sprintf (s, "%1d", GRID_ABS_SIZE(index));
               len = strlen (s);
               XDrawString (mainDisplay, hRuleWindow, rulerGC,
                     pos-rulerFontWidth/2, rulerW-HALF_INCH_H-rulerFontDes,
                     s, len);
               XDrawLine (mainDisplay, hRuleWindow, defaultGC, pos, rulerW,
                     pos, rulerW-HALF_INCH_H);
            }
            else if ((GRID_ZOOMED_SIZE(i+drawOrigX)) % FIVE_MM == 0)
               XDrawLine (mainDisplay, hRuleWindow, defaultGC, pos, rulerW,
                     pos, rulerW-QUARTER_INCH_H);
            else
               XDrawLine (mainDisplay, hRuleWindow, defaultGC, pos, rulerW,
                     pos, rulerW-MIN_INCH_H);
         }
         break;
   }
   oldXOff = (-1);
   XDrawLine (mainDisplay, hRuleWindow, revDefaultGC, oldXOff, 0,
         oldXOff, rulerLen);
   hRulerJustRedrawn = TRUE;
   justUnFrozen = FALSE;
}

void RedrawVRuler ()
{
   register int	i, pos, len, index;
   int		inc, abs_inc;
   char		s[5];
   XEvent	ev;

   XClearWindow (mainDisplay, vRuleWindow);

   XSync (mainDisplay, False);
   while (XCheckWindowEvent (mainDisplay, vRuleWindow, ExposureMask, &ev)) ;

   switch (gridSystem)
   {
      case ENGLISH_GRID:
         inc = (zoomedIn ? (xyEnglishGrid<<zoomScale) : xyEnglishGrid);
         abs_inc = ABS_SIZE(inc);

         if (drawOrigY % abs_inc == 0)
         {
            i = 0;
            pos = 0;
         }
         else
         {
            i = ((int)(drawOrigY / abs_inc) + 1) * abs_inc - drawOrigY;
            pos = ZOOMED_SIZE(i);
         }
         for ( ; i<drawWinH; i+=abs_inc, pos+=inc)
         {
            if ((GRID_ZOOMED_SIZE(i+drawOrigY)) % ONE_INCH == 0)
            {
               index = (int)((GRID_ZOOMED_SIZE(i+drawOrigY)) / ONE_INCH);
               sprintf (s, "%1d", GRID_ABS_SIZE(index));
               len = strlen (s);
               XDrawString (mainDisplay, vRuleWindow, rulerGC, 1,
                     pos-rulerFontHeight/2+rulerFontAsc, s, len);
               XDrawLine (mainDisplay, vRuleWindow, defaultGC, rulerW, pos,
                     rulerW-INCH_H, pos);
            }
            else if ((GRID_ZOOMED_SIZE(i+drawOrigY)) % HALF_INCH == 0)
               XDrawLine (mainDisplay, vRuleWindow, defaultGC, rulerW, pos,
                     rulerW-HALF_INCH_H, pos);
            else if ((GRID_ZOOMED_SIZE(i+drawOrigY)) % QUARTER_INCH == 0)
               XDrawLine (mainDisplay, vRuleWindow, defaultGC, rulerW, pos,
                     rulerW-QUARTER_INCH_H, pos);
            else
               XDrawLine (mainDisplay, vRuleWindow, defaultGC, rulerW, pos,
                     rulerW-MIN_INCH_H, pos);
         }
         break;
      case METRIC_GRID:
         inc = (zoomedIn ? (xyMetricGrid<<zoomScale) : xyMetricGrid);
         abs_inc = ABS_SIZE(inc);

         if (drawOrigY % abs_inc == 0)
         {
            i = 0;
            pos = 0;
         }
         else
         {
            i = ((int)(drawOrigY / abs_inc) + 1) * abs_inc - drawOrigY;
            pos = ZOOMED_SIZE(i);
         }
         for ( ; i<drawWinH; i+=abs_inc, pos+=inc)
         {
            if ((GRID_ZOOMED_SIZE(i+drawOrigY)) % ONE_CM == 0)
            {
               index = (int)((GRID_ZOOMED_SIZE(i+drawOrigY)) / ONE_CM);
               sprintf (s, "%1d", GRID_ABS_SIZE(index));
               len = strlen (s);
               XDrawString (mainDisplay, vRuleWindow, rulerGC, 1,
                     pos-rulerFontHeight/2+rulerFontAsc, s, len);
               XDrawLine (mainDisplay, vRuleWindow, defaultGC, rulerW, pos,
                     rulerW-HALF_INCH_H, pos);
            }
            else if ((GRID_ZOOMED_SIZE(i+drawOrigY)) % FIVE_MM == 0)
               XDrawLine (mainDisplay, vRuleWindow, defaultGC, rulerW, pos,
                     rulerW-QUARTER_INCH_H, pos);
            else
               XDrawLine (mainDisplay, vRuleWindow, defaultGC, rulerW, pos,
                     rulerW-MIN_INCH_H, pos);
         }
         break;
   }
   if (!freezeMarkRulerText && showMeasurement && !hRulerJustRedrawn &&
         oldYOff != 0)
   {
      char	buf[80];

      sprintf (buf, "[%1d,%1d]", ABS_X(oldXOff), ABS_Y(oldYOff));
      XDrawString (mainDisplay, hRuleWindow, revDefaultGC, 8, 2+defaultFontAsc,
            buf, strlen(buf));
      hRulerJustRedrawn = TRUE;
   }
   oldYOff = (-1);
   XDrawLine (mainDisplay, vRuleWindow, revDefaultGC, 0, oldYOff,
         rulerLen, oldYOff);
}

void MarkRulers(XOff, YOff)
   int XOff, YOff;
{
   char buf[80];

   XDrawLine(mainDisplay,hRuleWindow,revDefaultGC,oldXOff,0,oldXOff,rulerLen);
   XDrawLine(mainDisplay,vRuleWindow,revDefaultGC,0,oldYOff,rulerLen,oldYOff);
   if (showCrossHair) {
      XDrawLine(mainDisplay, drawWindow, revDefaultGC, oldXOff, 0,
            oldXOff, ZOOMED_SIZE(drawWinH));
      XDrawLine(mainDisplay, drawWindow, revDefaultGC, 0, oldYOff,
            ZOOMED_SIZE(drawWinW), oldYOff);
   }

   if (hRulerJustRedrawn) {
      hRulerJustRedrawn= FALSE;
   } else if (!freezeMarkRulerText && showMeasurement) {
      if (justUnFrozen) {
         justUnFrozen = FALSE;
         sprintf(buf, "[%1d,%1d]", ABS_X(frozenXOff), ABS_Y(frozenYOff));
      } else {
         sprintf(buf, "[%1d,%1d]", ABS_X(oldXOff), ABS_Y(oldYOff));
      }
      XDrawString(mainDisplay, hRuleWindow, revDefaultGC, 8, 2+defaultFontAsc,
            buf, strlen(buf));
   }
   XDrawLine(mainDisplay,hRuleWindow,revDefaultGC,XOff,0,XOff,rulerLen);
   XDrawLine(mainDisplay,vRuleWindow,revDefaultGC,0,YOff,rulerLen,YOff);
   if (showCrossHair) {
      XDrawLine(mainDisplay, drawWindow, revDefaultGC, XOff, 0,
            XOff, ZOOMED_SIZE(drawWinH));
      XDrawLine(mainDisplay, drawWindow, revDefaultGC, 0, YOff,
            ZOOMED_SIZE(drawWinW), YOff);
   }

   if (!freezeMarkRulerText && showMeasurement) {
      sprintf(buf, "[%1d,%1d]", ABS_X(XOff), ABS_Y(YOff));
      XDrawString(mainDisplay, hRuleWindow, revDefaultGC, 8, 2+defaultFontAsc,
            buf, strlen(buf));
   }
   oldXOff = XOff;
   oldYOff = YOff;
}

void RedrawRulers()
{
   RedrawHRuler();
   RedrawVRuler();
}

void GetCrossHairPosition(pnXOff, pnYOff, pnShown)
   int *pnXOff, *pnYOff, *pnShown;
{
   if (pnXOff != NULL) *pnXOff = oldXOff;
   if (pnYOff != NULL) *pnYOff = oldYOff;
   if (pnShown != NULL) *pnShown = showCrossHair;
}

void RedrawCrossHair()
{
   if (showCrossHair) {
      XDrawLine(mainDisplay, drawWindow, revDefaultGC, oldXOff, 0,
            oldXOff, ZOOMED_SIZE(drawWinH));
      XDrawLine(mainDisplay, drawWindow, revDefaultGC, 0, oldYOff,
            ZOOMED_SIZE(drawWinW), oldYOff);
   }
}

void ToggleShowCrossHair()
{
   /* the cross-hair stuff is disabled -- cannot get it to work right */
   if (!showCrossHair) return;

   RedrawCrossHair();

   showCrossHair = !showCrossHair;
   RedrawRulers();
   if (showCrossHair) {
      Msg("Showing cross-hair enabled.");
      SetNullCursor(drawWindow);
   } else {
      SetDefaultCursor(drawWindow);
      ShowCursor();
      Msg("Showing cross-hair disabled.");
   }
}

void RulersEventHandler(input)
   XEvent *input;
{
   if (input->xany.window == vRuleWindow) {
      if (input->type == Expose) {
         RedrawVRuler();
      } else if (input->type == EnterNotify) {
         SetMouseStatus("(none)", "(none)", "(none)");
      } else if (input->type == ButtonPress) {
         if (input->xbutton.button == Button3) {
            ToggleShowCrossHair();
         }
      }
   } else if (input->xany.window == hRuleWindow) {
      if (input->type == Expose) {
         RedrawHRuler();
      } else if (input->type == EnterNotify) {
         SetMouseStatus("(none)", "(none)", "(none)");
      } else if (input->type == ButtonPress) {
         if (input->xbutton.button == Button3) {
            ToggleShowCrossHair();
         }
      }
   }
}

void FreezeMarkRulerText ()
{
   freezeMarkRulerText = TRUE;
   frozenXOff = oldXOff;
   frozenYOff = oldYOff;
}

void UnFreezeMarkRulerText ()
{
   freezeMarkRulerText = FALSE;
   justUnFrozen = TRUE;
}

static int oldLtX, oldLtY, oldRbX, oldRbY, oldMdX, oldMdY;

static
void DoIntervalRulers ()
{
   if (oldLtX == oldRbX) {
      XDrawLine(mainDisplay,hRuleWindow,revDefaultGC,oldLtX,0,oldLtX,rulerLen);
   } else if (oldLtX==oldMdX || oldRbX==oldMdX) {
      XDrawLine(mainDisplay,hRuleWindow,revDefaultGC,oldLtX,0,oldLtX,rulerLen);
      XDrawLine(mainDisplay,hRuleWindow,revDefaultGC,oldRbX,0,oldRbX,rulerLen);
   } else {
      XDrawLine(mainDisplay,hRuleWindow,revDefaultGC,oldLtX,0,oldLtX,rulerLen);
      XDrawLine(mainDisplay,hRuleWindow,revDefaultGC,oldMdX,0,oldMdX,rulerLen);
      XDrawLine(mainDisplay,hRuleWindow,revDefaultGC,oldRbX,0,oldRbX,rulerLen);
   }
   if (oldLtY == oldRbY) {
      XDrawLine(mainDisplay,vRuleWindow,revDefaultGC,0,oldLtY,rulerLen,oldLtY);
   } else if (oldLtY==oldMdY || oldRbY==oldMdY) {
      XDrawLine(mainDisplay,vRuleWindow,revDefaultGC,0,oldLtY,rulerLen,oldLtY);
      XDrawLine(mainDisplay,vRuleWindow,revDefaultGC,0,oldRbY,rulerLen,oldRbY);
   } else {
      XDrawLine(mainDisplay,vRuleWindow,revDefaultGC,0,oldLtY,rulerLen,oldLtY);
      XDrawLine(mainDisplay,vRuleWindow,revDefaultGC,0,oldMdY,rulerLen,oldMdY);
      XDrawLine(mainDisplay,vRuleWindow,revDefaultGC,0,oldRbY,rulerLen,oldRbY);
   }
   if (showCrossHair) {
      XDrawLine(mainDisplay, drawWindow, revDefaultGC, oldRbX, 0,
            oldRbX, ZOOMED_SIZE(drawWinH));
      XDrawLine(mainDisplay, drawWindow, revDefaultGC, 0, oldRbY,
            ZOOMED_SIZE(drawWinW), oldRbY);
   }
}

void BeginIntervalRulers (ltx, lty, rbx, rby)
   int	ltx, lty, rbx, rby;
{
   XDrawLine(mainDisplay,hRuleWindow,revDefaultGC,oldXOff,0,oldXOff,rulerLen);
   XDrawLine(mainDisplay,vRuleWindow,revDefaultGC,0,oldYOff,rulerLen,oldYOff);
   if (showCrossHair) {
      XDrawLine(mainDisplay, drawWindow, revDefaultGC, oldXOff, 0,
            oldXOff, ZOOMED_SIZE(drawWinH));
      XDrawLine(mainDisplay, drawWindow, revDefaultGC, 0, oldYOff,
            ZOOMED_SIZE(drawWinW), oldYOff);
   }
   oldLtX = ltx; oldLtY = lty;
   oldRbX = rbx; oldRbY = rby;
   oldMdX = (oldLtX+oldRbX)>>1;
   oldMdY = (oldLtY+oldRbY)>>1;
   if (showMeasurement) FreezeMarkRulerText ();
   DoIntervalRulers ();
}

void DrawIntervalRulers (ltx, lty, rbx, rby)
   int	ltx, lty, rbx, rby;
{
   DoIntervalRulers ();
   oldLtX = ltx; oldLtY = lty;
   oldRbX = rbx; oldRbY = rby;
   oldMdX = (oldLtX+oldRbX)>>1; oldMdY = (oldLtY+oldRbY)>>1;
   DoIntervalRulers ();
   if (showMeasurement) UnFreezeMarkRulerText ();
}

void EndIntervalRulers (x, y)
   int	x, y;
{
   DoIntervalRulers ();
   oldXOff = x;
   oldYOff = y;
   XDrawLine(mainDisplay,hRuleWindow,revDefaultGC,oldXOff,0,oldXOff,rulerLen);
   XDrawLine(mainDisplay,vRuleWindow,revDefaultGC,0,oldYOff,rulerLen,oldYOff);
   if (showCrossHair) {
      XDrawLine(mainDisplay, drawWindow, revDefaultGC, oldXOff, 0,
            oldXOff, ZOOMED_SIZE(drawWinH));
      XDrawLine(mainDisplay, drawWindow, revDefaultGC, 0, oldYOff,
            ZOOMED_SIZE(drawWinW), oldYOff);
   }
   MarkRulers (x, y);
   frozenXOff = oldXOff;
   frozenYOff = oldYOff;
}

void PixelToMeasurementUnit(Buf, NumPixels)
   char *Buf;
   int NumPixels;
{
   float fval;
   int ival;

   if (*unitStr == '\0') {
      sprintf(Buf, "%+1d", NumPixels);
   } else {
      fval = (float)((float)NumPixels*1000.0)/(gfPixelsPerUnit);
      ival = ((int)round(fval));
      fval = ((float)ival)/((float)1000.0);
      sprintf(Buf, "%+.3f%s", fval, unitStr);
   }
}

void StartShowMeasureCursor(XOff, YOff, Str, ExtraSpace)
   int XOff, YOff, ExtraSpace;
   char *Str;
{
   if (!showMeasurement) return;
   MarkRulers(XOff, YOff);
   FreezeMarkRulerText();
   if (Str != NULL && *Str != '\0') {
      if (ExtraSpace) {
         XDrawString(mainDisplay, drawWindow, revDefaultGC, XOff+18,
               YOff+defaultFontAsc, Str, strlen(Str));
      } else {
         XDrawString(mainDisplay, drawWindow, revDefaultGC, XOff+4,
               YOff+defaultFontAsc, Str, strlen(Str));
      }
   }
}

void ShowMeasureCursor(XOff, YOff, Str, ExtraSpace)
   int XOff, YOff, ExtraSpace;
   char *Str;
{
   if (!showMeasurement) return;
   if (Str != NULL && *Str != '\0') {
      if (ExtraSpace) {
         XDrawString(mainDisplay, drawWindow, revDefaultGC, XOff+18,
               YOff+defaultFontAsc, Str, strlen(Str));
      } else {
         XDrawString(mainDisplay, drawWindow, revDefaultGC, XOff+4,
               YOff+defaultFontAsc, Str, strlen(Str));
      }
   }
}

void EndShowMeasureCursor(XOff, YOff, Str, ExtraSpace)
   int XOff, YOff, ExtraSpace;
   char *Str;
{
   if (!showMeasurement) return;
   if (Str != NULL && *Str != '\0') {
      if (ExtraSpace) {
         XDrawString(mainDisplay, drawWindow, revDefaultGC, XOff+18,
               YOff+defaultFontAsc, Str, strlen(Str));
      } else {
         XDrawString(mainDisplay, drawWindow, revDefaultGC, XOff+4,
               YOff+defaultFontAsc, Str, strlen(Str));
      }
   }
   UnFreezeMarkRulerText();
}

void ToggleShowMeasurement()
{
   showMeasurement = !showMeasurement;
   RedrawRulers();
   if (showMeasurement) {
      Msg("Showing measurement enabled.");
   } else {
      Msg("Showing measurement disabled.");
   }
   UpdateSubMenu(MENU_LAYOUT);
}

