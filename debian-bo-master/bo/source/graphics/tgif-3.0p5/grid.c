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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/grid.c,v 3.0 1996/05/06 16:05:21 william Exp $";
#endif

#include <stdio.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "choice.e"
#include "color.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#include "file.e"
#ifndef _NO_EXTERN
#include "grid.e"
#endif
#include "mainmenu.e"
#include "menu.e"
#include "msg.e"
#include "obj.e"
#include "page.e"
#include "pattern.e"
#include "raster.e"
#include "ruler.e"
#include "scroll.e"
#include "select.e"
#include "setup.e"
#include "stretch.e"
#include "text.e"

#define ENGLISH_GRID_STEP 8
#define METRIC_GRID_STEP 5

int	gridSystem = ENGLISH_GRID;
int	gridOn = TRUE;
int	xyEnglishGrid = DEFAULT_ENGLISH_GRID;
int	xyMetricGrid = DEFAULT_METRIC_GRID;
int	pageStyle = PORTRAIT;
int	whereToPrint = LATEX_FIG;
int	moveMode = UNCONST_MOVE;
int	gridShown = TRUE;
int	mapShown = TRUE;
int	usePaperSizeStoredInFile = FALSE;
int	oneMotionSelectMove = FALSE;
int	queryZoomInPoint=FALSE;

char * layoutMenuStr[] =
      {  "+Grid          #i",
         "-Grid          #d",
         "HideGrid       #g",
         "SnapOff       ^#g",
         "ScrollToOrig   #^",
         "SaveOrigin       ",
         "ZoomIn         #z",
         "ZoomOut        #o",
         "DefaultZoom    #:",
         "ZoomWayOut     #`",
         "Landscape        ",
         "Portrait         ",
         "SetReduction   #%",
         "RawPSFile     ^#x",
         "ColorPS       ^#k",
         "ConstMove      #@",
         "MetricGrid    ^#-",
         "HideBit/Pixmap   ",
         "UseGrayScale     ",
         "ShowMeasurement  ",
         "SetMeasureUnit   ",
         "ShowMenubar      ",
         "ShowStatus       ",
         "ClickSelClickMove",
         "UseColorLayers   ",
         NULL
      };
#define MAX_LAYOUT_STR 20 /* longest string length in layoutMenuStr[] */
static char * layoutMenuDescription[] =
      {  "Increment grid size",
         "Decrement grid size",
         "Hide/show grid lines",
         "Toggle the snapping to grid points effect",
         "Scroll to the 'origin' of the drawing",
         "Save current location as the 'origin' of the drawing",
         "Zoom in",
         "Zoom out",
         "No zoom",
         "Zoom way out to see the whole drawing",
         "Use landscape page style",
         "Use portrait page style",
         "Specify reduction/magnification of the whole drawing",
         "Set printing mode/format",
         "Toggle between color and black & white printing modes",
         "Toggle between constrained and unconstrained move modes",
         "Toggle between English and Metric grid systems",
         "Toggle between hiding/showing detailed bitmap/pixmap objects",
         "Use gray scales for tiling patterns to speed up printing",
         "Show cursor measurements",
         "Set the unit of measurements",
         "Show menubar window",
         "Show status window",
         "Toggle between click-select-move and click-select-click-move modes",
         "Use color layers",
         NULL
      };
#define MAX_LAYOUT_DESC 80 /* longest string length in layoutMenuStr[] */

void MyHDotLine (Win, Y, XStart, XEnd)
   Window	Win;
   int		Y, XStart, XEnd;
{
   register int	x, step = 0;

   switch (gridSystem)
   {
      case ENGLISH_GRID: step = ENGLISH_GRID_STEP; break;
      case METRIC_GRID: step = METRIC_GRID_STEP; break;
   }

   for (x = XStart; x < XEnd; x += step)
      PUT_A_POINT (mainDisplay, Win, defaultGC, x, Y);
}

void MyVDotLine (Win, X, YStart, YEnd)
   Window	Win;
   int		X, YStart, YEnd;
{
   register int	y, step = 0;

   switch (gridSystem)
   {
      case ENGLISH_GRID: step = ENGLISH_GRID_STEP; break;
      case METRIC_GRID: step = METRIC_GRID_STEP; break;
   }

   for (y = YStart; y < YEnd; y += step)
      PUT_A_POINT (mainDisplay, Win, defaultGC, X, y);
}

#define FAKE_CM 80

void RedrawGridLines (Win)
   Window	Win;
{
   register int	i, inc = 0, abs_grid = 0;
   int		x_start, y_start, x_end, y_end, x_grid_start, y_grid_start;
   int		step = ENGLISH_GRID_STEP;

   if (!gridShown) return;

   switch (gridSystem)
   {
      case ENGLISH_GRID:
         inc = HALF_INCH;
         abs_grid = ABS_SIZE(HALF_INCH);
         step = ENGLISH_GRID_STEP;
         break;
      case METRIC_GRID:
         if (zoomedIn && zoomScale > 1)
         {
            inc = FAKE_CM>>1;
            abs_grid = ABS_SIZE(FAKE_CM>>1);
         }
         else
         {
            inc = ONE_CM;
            abs_grid = ABS_SIZE(ONE_CM);
         }
         step = METRIC_GRID_STEP;
         break;
   }

   if (drawOrigX % abs_grid == 0)
      x_start = 0;
   else
      x_start = OFFSET_X(((int)(drawOrigX / abs_grid) + 1) * abs_grid);
   if (drawOrigY % abs_grid == 0)
      y_start = 0;
   else
      y_start = OFFSET_Y(((int)(drawOrigY / abs_grid) + 1) * abs_grid);

   x_end = min(OFFSET_X(paperWidth), OFFSET_X(drawOrigX+drawWinW));
   y_end = min(OFFSET_Y(paperHeight), OFFSET_Y(drawOrigY+drawWinH));

   if (drawOrigX % step == 0)
      x_grid_start = 0;
   else
      x_grid_start = ((int)(drawOrigX / step) + 1) * step - drawOrigX;
   if (drawOrigY % step == 0)
      y_grid_start = 0;
   else
      y_grid_start = ((int)(drawOrigY / step) + 1) * step - drawOrigY;

   for (i = x_start; i < x_end; i += inc)
      MyVDotLine (Win, i, y_grid_start, y_end);
   for (i = y_start; i < y_end; i += inc)
      MyHDotLine (Win, i, x_grid_start, x_end);
}

void DrawGridLines (Win, LtX, LtY, W, H)
   Window	Win;
   int 		LtX, LtY, W, H; /* screen offsets */
{
   register int	i, inc = 0;
   int		x_start, y_start, x_end, y_end, rbx, rby;
   int		x_grid_start, y_grid_start, x_grid_end, y_grid_end;
   int		step = ENGLISH_GRID_STEP;

   if (!gridShown) return;

   switch (gridSystem)
   {
      case ENGLISH_GRID:
         inc = HALF_INCH;
         step = ENGLISH_GRID_STEP;
         break;
      case METRIC_GRID:
         inc = (zoomedIn && zoomScale > 1) ? (FAKE_CM>>1) : ONE_CM;
         step = METRIC_GRID_STEP;
         break;
   }

   if (LtX > OFFSET_X(paperWidth) || LtY > OFFSET_Y(paperHeight)) return;

   rbx = LtX+W;
   rby = LtY+H;

   if (LtX % inc == 0)
      x_start = LtX;
   else
      x_start = ((int)(LtX/inc) + 1) * inc;
   if (LtY % inc == 0)
      y_start = LtY;
   else
      y_start = ((int)(LtY/inc) + 1) * inc;

   if (rbx % inc == 0)
      x_end = min(OFFSET_X(paperWidth),rbx);
   else
      x_end = min(OFFSET_X(paperWidth), ((int)(rbx/inc) + 1) * inc);
   if (rby % inc == 0)
      y_end = min(OFFSET_Y(paperHeight),rby);
   else
      y_end = min(OFFSET_Y(paperHeight), ((int)(rby/inc) + 1) * inc);

   if (LtX % step == 0)
      x_grid_start = LtX;
   else
      x_grid_start = ((int)(LtX/step) + 1) * step;
   if (LtY % step == 0)
      y_grid_start = LtY;
   else
      y_grid_start = ((int)(LtY/step) + 1) * step;

   if (rbx % step == 0)
      x_grid_end = min(OFFSET_X(paperWidth), rbx);
   else
      x_grid_end = min(OFFSET_X(paperWidth), ((int)(rbx/step)+1)*step);
   if (rby % step == 0)
      y_grid_end = min(OFFSET_Y(paperHeight), rby);
   else
      y_grid_end = min(OFFSET_Y(paperHeight), ((int)(rby/step)+1)*step);

   for (i = x_start; i < x_end; i += inc)
      MyVDotLine (Win, i, y_grid_start, y_grid_end);
   for (i = y_start; i < y_end; i += inc)
      MyHDotLine (Win, i, x_grid_start, x_grid_end);
}

void RedrawPageLines (Win)
   Window	Win;
{
   register int	i, x, y;
   int		end;

   if (pageLayoutMode == PAGE_STACK || !pageLineShownInTileMode) return;

   end = OFFSET_Y(min(drawOrigY+drawWinH,paperHeight));
   for (i = 0; i < paperWidth; i += onePageWidth)
   {
      x = OFFSET_X(i);
      if (i >= drawOrigX && i < drawOrigX+drawWinW)
         XDrawLine (mainDisplay, Win, defaultGC, x, 0, x, end);
      if (i > drawOrigX+drawWinW)
         break;
   }
   end = OFFSET_X(min(drawOrigX+drawWinW,paperWidth));
   for (i = 0; i < paperHeight; i += onePageHeight)
   {
      y = OFFSET_Y(i);
      if (i >= drawOrigY && i < drawOrigY+drawWinH)
         XDrawLine (mainDisplay, Win, defaultGC, 0, y, end, y);
      if (i > drawOrigY+drawWinH)
         break;
   }
}

void DrawPageLines (Win, LtX, LtY, W, H)
   Window	Win;
   int 		LtX, LtY, W, H; /* screen offsets */
{
   register int	i, x, y;
   int		start, end;

   if (pageLayoutMode == PAGE_STACK || !pageLineShownInTileMode) return;

   start = max(0,LtY);
   end = min(LtY+H,OFFSET_Y(min(drawOrigY+drawWinH,paperHeight)));
   for (i = 0; i < paperWidth; i += onePageWidth)
   {
      x = OFFSET_X(i);
      if (x >= LtX && x < LtX+W)
         XDrawLine (mainDisplay, Win, defaultGC, x, start, x, end);
      if (i > drawOrigX+drawWinW)
         break;
   }
   start = max(0,LtX);
   end = min(LtX+W,OFFSET_X(min(drawOrigX+drawWinW,paperWidth)));
   for (i = 0; i < paperHeight; i += onePageHeight)
   {
      y = OFFSET_Y(i);
      if (y >= LtY && y < LtY+H)
         XDrawLine (mainDisplay, Win, defaultGC, start, y, end, y);
      if (i > drawOrigY+drawWinH)
         break;
   }
}

static char showGridStr[]        = "ShowGrid       #g";
static char hideGridStr[]        = "HideGrid       #g";
static char showGridDesc[]       = "Show grid lines";
static char hideGridDesc[]       = "Hide grid lines";

static char snapOnStr[]          = "SnapOn        ^#g";
static char snapOffStr[]         = "SnapOff       ^#g";
static char snapOnDesc[]         = "Turn on snapping to grid points";
static char snapOffDesc[]        = "Turn off snapping to grid points";

static char printerStr[]   = "Printer       ^#x";
static char latexFigStr[]  = "LaTeXFig      ^#x";
static char psFileStr[]    = "RawPSFile     ^#x";
static char xbmStr[]       = "XBitmap       ^#x";
static char xpmStr[]       = "XPixmap       ^#x";
static char textFileStr[]  = "TextFile      ^#x";
static char epsiFileStr[]  = "EPSI          ^#x";
static char gifStr[]       = "GIF/ISMAP     ^#x";
static char htmlStr[]      = "HTML/USEMAP   ^#x";
static char printerDesc[]  = "Set printing mode to print to a printer";
static char latexFigDesc[] = "Set printing mode to generate EPS files";
static char psFileDesc[]   = "Set printing mode to generate PS files";
static char xbmDesc[]      = "Set printing mode to generate bitmap files";
static char xpmDesc[]      = "Set printing mode to generate pixmap files";
static char textFileDesc[] = "Set printing mode to generate ASCII files";
static char epsiFileDesc[] = "Set printing mode to generate EPSI files";
static char gifDesc[]      = "Set printing mode to generate GIF/ISMAP files";
static char htmlDesc[]     = "Set printing mode to generate HTML files";

static char bwPostScriptStr[]    = "BlkWhtPS      ^#k";
static char colorPostScriptStr[] = "ColorPS       ^#k";
static char bwPostScriptDesc[]   = "Set black & white printing mode";
static char colorPostScriptDesc[]= "Set color printing mode";

static char constMoveStr[]       = "ConstMove      #@";
static char unConstMoveStr[]     = "UnConstMove    #@";
static char constMoveDesc[]      = "Constrained move mode";
static char unConstMoveDesc[]    = "Unconstrained move mode";

static char englishGridSysStr[]  = "EnglishGrid   ^#-";
static char metricGridSysStr[]   = "MetricGrid    ^#-";
static char englishGridSysDesc[] = "Use English grid system";
static char metricGridSysDesc[]  = "Use Metric grid system";

static char showMapStr[]         = "ShowBit/Pixmap";
static char hideMapStr[]         = "HideBit/Pixmap";
static char showMapDesc[]        = "Show detailed bitmap/pixmap objects";
static char hideMapDesc[]        = "Show outlines for bitmap/pixmap objects";

static char useGrayStr[]         = "UseGrayScale";
static char noGrayStr[]          = "NoGrayScale";
static char useGrayDesc[]        =
      "Use gray scales for tiling patterns to speed up printing";
static char noGrayDesc[]         =
      "Use tiling patterns as is (may be slow for printing";

static char hideMeasureStr[]      = "HideMeasurement";
static char showMeasureStr[]      = "ShowMeasurement";
static char hideMeasureDesc[]     = "Hide cursor measurements";
static char showMeasureDesc[]     = "Show cursor measurements";

static char hideMenubarStr[]      = "HideMenubar";
static char showMenubarStr[]      = "ShowMenubar";
static char hideMenubarDesc[]     = "Hide menubar window";
static char showMenubarDesc[]     = "Show menubar window";

static char hideStatusStr[]       = "HideStatus";
static char showStatusStr[]       = "ShowStatus";
static char hideStatusDesc[]      = "Hide status window";
static char showStatusDesc[]      = "Show status window";

static char oneMotionSelMoveStr[] = "OneMotionSelMove";
static char clickSelClickMoveStr[]= "ClickSelClickMove";

static char noColorLayersStr[]    = "NoColorLayers";
static char colorLayersStr[]      = "UseColorLayers";
static char noColorLayersDesc[]   = "Do not use color layers";
static char colorLayersDesc[]     = "Use color layers";

void ToggleGridSystem ()
{
   int	saved_orig_x, saved_orig_y;

   switch (gridSystem)
   {
      case ENGLISH_GRID: Msg ("Using Metric system."); break;
      case METRIC_GRID: Msg ("Using English system."); break;
   }
   gridSystem = !gridSystem;
   if (drawOrigX != 0 || drawOrigY != 0)
   {
      saved_orig_x = drawOrigX;
      saved_orig_y = drawOrigY;
      drawOrigX = drawOrigY = 0;
      RedrawScrollBars ();
      UpdDrawWinBBox ();
      AdjSplineVs ();
      AdjustCurText (-(saved_orig_x), -(saved_orig_y));
   }
   ClearAndRedrawDrawWindow ();
   RedrawRulers ();
   UpdateSubMenu (MENU_LAYOUT);
}

void IncGrid ()
{
   if (!gridOn)
      Msg ("Snap is not on, grid size not changed.");
   else if (gridSystem == ENGLISH_GRID && xyEnglishGrid < HALF_INCH)
   {
      xyEnglishGrid <<= 1;
      RedrawRulers ();
   }
   else if (gridSystem == METRIC_GRID && xyMetricGrid < ONE_CM)
   {
      switch (xyMetricGrid)
      {
         case ONE_MM: xyMetricGrid = TWO_MM; break;
         case TWO_MM: xyMetricGrid = FIVE_MM; break;
         case FIVE_MM: xyMetricGrid = ONE_CM; break;
      }
      RedrawRulers ();
   }
   else
      Msg ("Already at maximun grid, grid size not changed.");
}

void DecGrid ()
{
   if (!gridOn)
      Msg ("Snap is not on, grid size not changed.");
   else if (gridSystem == ENGLISH_GRID && xyEnglishGrid > 4)
   {
      xyEnglishGrid >>= 1;
      RedrawRulers ();
   }
   else if (gridSystem == METRIC_GRID && xyMetricGrid > ONE_MM)
   {
      switch (xyMetricGrid)
      {
         case TWO_MM: xyMetricGrid = ONE_MM; break;
         case FIVE_MM: xyMetricGrid = TWO_MM; break;
         case ONE_CM: xyMetricGrid = FIVE_MM; break;
      }
      RedrawRulers ();
   }
   else
      Msg ("Already at minimum grid, grid size not changed.");
}

void ToggleGridShown ()
{
   gridShown = !gridShown;
   ClearAndRedrawDrawWindow ();
   UpdateSubMenu (MENU_LAYOUT);
}

void ToggleSnapOn ()
{
   gridOn = !gridOn;
   if (gridOn)
      Msg ("Snapping to grid point activated.");
   else
      Msg ("Snapping to grid point disabled.");
   RedrawRulers ();
   UpdateSubMenu (MENU_LAYOUT);
}

void ToggleColorPostScript ()
{
   colorDump = !colorDump;
   if (colorDump)
      Msg ("Will print in color (PS and pixmap).");
   else
      Msg ("Will print in black-and-white (PS and bitmap).");
   ShowWhereToPrint ();
   SetFileModified (TRUE);
   UpdateSubMenu (MENU_LAYOUT);
}

void ToggleMoveMode ()
{
   moveMode = !moveMode;
   switch (moveMode)
   {
      case CONST_MOVE: Msg ("Constrained move."); break;
      case UNCONST_MOVE: Msg ("Unconstrained move."); break;
   }
   ShowMoveMode ();
   UpdateSubMenu (MENU_LAYOUT);
   UpdateSubMenu (MENU_MOVEMODE);
}

void ToggleMapShown ()
{
   mapShown = !mapShown;
   if (mapShown)
      Msg ("Will display bitmap/pixmap.");
   else
      Msg ("Will not display bitmap/pixmap.");
   ClearAndRedrawDrawWindow ();
   UpdateSubMenu (MENU_LAYOUT);
}

void ToggleUseGray ()
{
   useGray = !useGray;
   if (useGray)
      Msg ("Gray scale enabled in black&white printing.");
   else
      Msg ("Gray scale disabled in black&white printing.");
   UpdateSubMenu (MENU_LAYOUT);
}

void SetMeasureUnit()
{
   char spec[MAXSTRING+1];

   sprintf(gszMsgBox, "Please enter '%s' (%s):",
         "<num> [pixel|cm|in]/<unit>", "e.g., 36 in/yd or 0.1 in/mil");
   Dialog(gszMsgBox, "( <CR>: accept, <ESC>: cancel )", spec);
   UtilTrimBlanks(spec);
   if (*spec == '\0') return;
   if (SetUnit(spec)) {
      ShowUnitMsg();
      SetFileModified(TRUE);
   }
}

void ToggleShowMenubar ()
{
   noMenubar = !noMenubar;
   if (noMenubar)
      XUnmapWindow (mainDisplay, menubarWindow);
   else
      XMapWindow (mainDisplay, menubarWindow);
   Reconfigure (TRUE);
   UpdateSubMenu (MENU_LAYOUT);
}

void ToggleShowStatus ()
{
   noStatusWindow = !noStatusWindow;
   if (noStatusWindow)
      XUnmapWindow (mainDisplay, statusWindow);
   else
      XMapWindow (mainDisplay, statusWindow);
   Reconfigure (TRUE);
   UpdateSubMenu (MENU_LAYOUT);
}

void ToggleWhereToPrint ()
{
   whereToPrint = (whereToPrint+1) % MAXWHERETOPRINT;
   switch (whereToPrint) {
   case PRINTER: Msg("Print device set to printer."); break;
   case LATEX_FIG:
      Msg("Will print Encapsulated PostScript (LaTeX-Figure) file.");
      break;
   case PS_FILE: Msg("Will print raw PostScript file."); break;
   case XBM_FILE:
      if (colorDump) {
         Msg("Will print X11 Pixmap file.");
      } else {
         Msg("Will print X11 Bitmap file.");
      }
      break;
   case TEXT_FILE: Msg("Will print ASCII text file."); break;
   case EPSI_FILE:
      Msg("Will print EPSI file (EPS with preview bitmap).");
      break;
   case GIF_FILE: Msg("Will print GIF/ISMAP file."); break;
   case HTML_FILE:
      Msg("Will print HTML (with Client-side imagemap) file.");
      break;
   }
   ShowWhereToPrint();
   UpdateSubMenu(MENU_LAYOUT);
   if (colorDump) {
      SetMouseStatus(colorPrintMouseStatus[whereToPrint].l,
            colorPrintMouseStatus[whereToPrint].m,
            colorPrintMouseStatus[whereToPrint].r);
   } else {
      SetMouseStatus(bwPrintMouseStatus[whereToPrint].l,
            bwPrintMouseStatus[whereToPrint].m,
            bwPrintMouseStatus[whereToPrint].r);
   }
}

void ToggleOneMotionSelectMove ()
{
   oneMotionSelectMove = !oneMotionSelectMove;
   if (oneMotionSelectMove)
      Msg ("Click-select-move in one motion mode selected.");
   else
      Msg ("Click-select-click-move mode selected.");
   UpdateSubMenu (MENU_LAYOUT);
}

void ToggleColorLayers ()
{
   colorLayers = !colorLayers;
   if (colorLayers) {
      XMapWindow(mainDisplay, colorWindow);
      XMapWindow(mainDisplay, colorDummyWindow);
      Msg("Color layers enabled.");
   } else {
      XUnmapWindow(mainDisplay, colorWindow);
      XUnmapWindow(mainDisplay, colorDummyWindow);
      Msg("Color layers disabled.");
   }
   Reconfigure(TRUE);
   UpdateSubMenu(MENU_LAYOUT);
   ClearAndRedrawDrawWindow();
}

void ToggleStretchableText ()
{
   stretchableText = !stretchableText;
   if (stretchableText) {
      Msg("Text objects are stretchable.");
   } else {
      Msg("Text objects are not stretchable.");
   }
   ShowStretchableTextMode();
   UpdateSubMenu(MENU_STRETCHTEXT);
}

static
void PostZoom (xc, yc)
   int	xc, yc;
{
   int	new_orig_x, new_orig_y, resolution=0;

   UpdDrawWinWH ();

   new_orig_x = ((xc<<1)-drawWinW)>>1;
   new_orig_y = ((yc<<1)-drawWinH)>>1;
   switch (gridSystem)
   {
      case ENGLISH_GRID: resolution = ABS_SIZE(HALF_INCH); break;
      case METRIC_GRID:
         resolution = (zoomedIn && zoomScale > 1) ? ABS_SIZE(FAKE_CM) :
               ABS_SIZE(ONE_CM);
         break;
   }
   drawOrigX = max(0,((int)(new_orig_x/resolution))*resolution);
   drawOrigY = max(0,((int)(new_orig_y/resolution))*resolution);

   if (drawOrigX>=0 && drawWinW<paperWidth && drawOrigX+drawWinW>=paperWidth)
   {
      switch (gridSystem)
      {
         case ENGLISH_GRID:
            if ((paperWidth-drawWinW) % ABS_SIZE(HALF_INCH) == 0)
               drawOrigX = paperWidth-drawWinW;
            else
               drawOrigX = max(0, ((int)((paperWidth-drawWinW)/
                     ABS_SIZE(HALF_INCH)) + 1) * ABS_SIZE(HALF_INCH));
            break;
         case METRIC_GRID:
            if (zoomedIn && zoomScale > 1)
            {
               if ((paperWidth-drawWinW) % ABS_SIZE(FAKE_CM) == 0)
                  drawOrigX = paperWidth-drawWinW;
               else
                  drawOrigX = max(0, ((int)((paperWidth-drawWinW)/
                        ABS_SIZE(FAKE_CM)) + 1) * ABS_SIZE(FAKE_CM));
            }
            else
            {
               if ((paperWidth-drawWinW) % ABS_SIZE(ONE_CM) == 0)
                  drawOrigX = paperWidth-drawWinW;
               else
                  drawOrigX = max(0, ((int)((paperWidth-drawWinW)/
                        ABS_SIZE(ONE_CM)) + 1) * ABS_SIZE(ONE_CM));
            }
            break;
      }
   }
   else if (drawOrigX < 0 || drawWinW >= paperWidth)
      drawOrigX = 0;

   if (drawOrigY>=0 && drawWinH<paperHeight && drawOrigY+drawWinH>=paperHeight)
   {
      switch (gridSystem)
      {
         case ENGLISH_GRID:
            if ((paperHeight-drawWinH) % ABS_SIZE(HALF_INCH) == 0)
               drawOrigY = paperHeight-drawWinH;
            else
               drawOrigY = max(0, ((int)((paperHeight-drawWinH)/
                     ABS_SIZE(HALF_INCH)) + 1) * ABS_SIZE(HALF_INCH));
            break;
         case METRIC_GRID:
            if (zoomedIn && zoomScale > 1)
            {
               if ((paperHeight-drawWinH) % ABS_SIZE(FAKE_CM) == 0)
                  drawOrigY = paperHeight-drawWinH;
               else
                  drawOrigY = max(0, ((int)((paperHeight-drawWinH)/
                        ABS_SIZE(FAKE_CM)) + 1) * ABS_SIZE(FAKE_CM));
            }
            else
            {
               if ((paperHeight-drawWinH) % ABS_SIZE(ONE_CM) == 0)
                  drawOrigY = paperHeight-drawWinH;
               else
                  drawOrigY = max(0, ((int)((paperHeight-drawWinH)/
                        ABS_SIZE(ONE_CM)) + 1) * ABS_SIZE(ONE_CM));
            }
            break;
      }
   }
   else if (drawOrigY < 0 || drawWinH >= paperHeight)
      drawOrigY = 0;

   UpdDrawWinBBox ();
   SetWatchCursor (drawWindow);
   SetWatchCursor (mainWindow);
   RedrawRulers ();
   RedrawScrollBars ();
   AdjSplineVs ();
   AdjCaches ();
   ShowZoom ();
   /* use to be ClearAndRedrawDrawWindowDontDrawCurText() */
   ClearAndRedrawDrawWindowNoCurT ();
   SetDefaultCursor (mainWindow);
   ShowCursor ();
   justDupped = FALSE;
}

void DefaultZoom ()
{
   int	xc, yc;

   if (zoomScale == 0) { Msg ("Already at the default zoom."); return; }

   if (curChoice==DRAWTEXT && textCursorShown)
      PrepareZoomCurText (&xc, &yc);
   else
   {
      xc = (topSel==NULL) ? ((drawOrigX<<1)+drawWinW)>>1 : (selLtX+selRbX)>>1;
      yc = (topSel==NULL) ? ((drawOrigY<<1)+drawWinH)>>1 : (selLtY+selRbY)>>1;
   }

   if (curChoice == VERTEXMODE)
   {
      TieLooseEnds ();
      SetCurChoice (NOTHING);
   }

   zoomScale = 0;
   zoomedIn = FALSE;

   PostZoom (xc, yc);
   if (curChoice==DRAWTEXT && textCursorShown) PostZoomCurText (xc, yc);
}

static int zoomInAbsX=0, zoomInAbsY=0;
static int useZoomInAtCursor=FALSE;

void ZoomIn ()
{
   int	xc, yc;

   if (zoomedIn && zoomScale == MAX_ZOOMED_IN)
   {
      Msg ("Already at highest magnification, can no longer zoom in.");
      return;
   }

   if (curChoice==DRAWTEXT && textCursorShown) {
      if (useZoomInAtCursor) {
         TwoLineMsg("Zooming in around text being edited...", gszMsgBox);
         sprintf(gszMsgBox, "    %s %s",
               "(the cursor position is ignored even",
               "though <Cntrl><Shift>Btn1 is used)");
      }
      PrepareZoomCurText(&xc, &yc);
   } else if (useZoomInAtCursor) {
      xc = zoomInAbsX;
      yc = zoomInAbsY;
   } else if (queryZoomInPoint==TRUE || (queryZoomInPoint==INVALID &&
         topSel==NULL)) {
      MakeQuiescent();
      SaveStatusStrings();
      TwoLineMsg ("Left button selects a zoom center,",
            "    other buttons cancel zoom.");
      SetMouseStatus("Select zoom center", "Cancel", "Cancel");
      if (DrawWindowLoop(&xc, &yc, magCursor, FALSE) != Button1) {
         RestoreStatusStrings();
         Msg("ZoomIn canceled by the user.");
         return;
      }
      RestoreStatusStrings();
      xc = ABS_X(xc);
      yc = ABS_Y(yc);
   } else if (queryZoomInPoint==BAD) {
      Window root_win, child_win;
      int root_x, root_y;
      unsigned int status;

      XQueryPointer(mainDisplay, drawWindow, &root_win, &child_win,
            &root_x, &root_y, &xc, &yc, &status);
      xc = ABS_X(xc);
      yc = ABS_Y(yc);
   } else {
      xc = (topSel==NULL) ? ((drawOrigX<<1)+drawWinW)>>1 : (selLtX+selRbX)>>1;
      yc = (topSel==NULL) ? ((drawOrigY<<1)+drawWinH)>>1 : (selLtY+selRbY)>>1;
   }

   if (curChoice == VERTEXMODE)
   {
      TieLooseEnds ();
      SetCurChoice (NOTHING);
   }

   if (zoomedIn)
      zoomScale++;
   else if (zoomScale == 0)
   {
      zoomedIn = TRUE;
      zoomScale++;
   }
   else
      zoomScale--;

   PostZoom (xc, yc);
   if (curChoice==DRAWTEXT && textCursorShown) PostZoomCurText (xc, yc);
}

void ZoomInAtCursor(abs_x, abs_y)
   int abs_x, abs_y;
{
   zoomInAbsX = abs_x;
   zoomInAbsY = abs_y;
   useZoomInAtCursor = TRUE;
   ZoomIn();
   useZoomInAtCursor = FALSE;
}

void ZoomWayOut ()
{
   int	saved_zoom_scale = zoomScale, saved_zoomed_in = zoomedIn;

   while ((drawWinW>>1) >= paperWidth && (drawWinH>>1) >= paperHeight)
   {
      if (zoomedIn)
         zoomScale++;
      else if (zoomScale == 0)
      {
         zoomedIn = TRUE;
         zoomScale++;
      }
      else
         zoomScale--;
      drawWinW >>= 1;
      drawWinH >>= 1;
   }
   while (drawWinW < paperWidth || drawWinH < paperHeight)
   {
      if (!zoomedIn)
         zoomScale++;
      else if (zoomScale == 1)
      {
         zoomedIn = FALSE;
         zoomScale--;
      }
      else
         zoomScale--;
      drawWinW <<= 1;
      drawWinH <<= 1;
   }

   if (saved_zoom_scale==zoomScale && saved_zoomed_in==zoomedIn &&
         drawOrigX==0 && drawOrigY==0)
      return;

   TieLooseEnds ();
   SetCurChoice (NOTHING);

   PostZoom (0, 0);
}

void ZoomOut ()
{
   int	xc, yc;

   if (paperWidth <= drawWinW && paperHeight <= drawWinH)
   {
      Msg ("Already at paper boundaries, can no longer zoom out.");
      return;
   }

   if (curChoice==DRAWTEXT && textCursorShown)
      PrepareZoomCurText (&xc, &yc);
   else
   {
      xc = (topSel==NULL) ? ((drawOrigX<<1)+drawWinW)>>1 : (selLtX+selRbX)>>1;
      yc = (topSel==NULL) ? ((drawOrigY<<1)+drawWinH)>>1 : (selLtY+selRbY)>>1;
   }

   if (curChoice == VERTEXMODE)
   {
      TieLooseEnds ();
      SetCurChoice (NOTHING);
   }

   if (!zoomedIn)
      zoomScale++;
   else if (zoomScale == 1)
   {
      zoomedIn = FALSE;
      zoomScale--;
   }
   else
      zoomScale--;

   PostZoom (xc, yc);
   if (curChoice==DRAWTEXT && textCursorShown) PostZoomCurText (xc, yc);
}

void PreciseZoom (zoomed_in, zoom_scale, force_zoom)
   int	zoomed_in, zoom_scale, force_zoom;
{
   int	xc, yc;

   if (!force_zoom && zoomedIn == zoomed_in && zoomScale == zoom_scale) return;

   if (curChoice==DRAWTEXT && textCursorShown)
      PrepareZoomCurText (&xc, &yc);
   else
   {
      xc = (topSel==NULL) ? ((drawOrigX<<1)+drawWinW)>>1 : (selLtX+selRbX)>>1;
      yc = (topSel==NULL) ? ((drawOrigY<<1)+drawWinH)>>1 : (selLtY+selRbY)>>1;
   }

   if (curChoice == VERTEXMODE)
   {
      TieLooseEnds ();
      SetCurChoice (NOTHING);
   }

   zoomedIn = zoomed_in;
   zoomScale = zoom_scale;

   PostZoom (xc, yc);
   if (curChoice==DRAWTEXT && textCursorShown) PostZoomCurText (xc, yc);
}

void SetPSPageWidthHeight ()
{
   if (mainWindow != None)
   {
      sprintf (psYOffStr[0], "%.2f",
            (float)(((float)onePageHeight)/((float)PIX_PER_INCH)));
      psYOff[0] = ((float)onePageHeight)/((float)PIX_PER_INCH);
      psPageWidthInInch[0]=((float)onePageWidth)/((float)PIX_PER_INCH);
      psPageWidthInInch[1]=((float)onePageHeight)/((float)PIX_PER_INCH);
      psPageHeightInInch[0]=((float)onePageHeight)/((float)PIX_PER_INCH);
      psPageHeightInInch[1]=((float)onePageWidth)/((float)PIX_PER_INCH);
   }
}

void ResetOnePageSize ()
{  /* reset 8.5in x 11in */
#ifndef A4PAPER
   onePageWidth = (85*PIX_PER_INCH)/10;
   onePageHeight = 11*PIX_PER_INCH;
#else /* A4PAPER */
   onePageWidth = (825*PIX_PER_INCH)/100;
   onePageHeight = (117*PIX_PER_INCH)/10;
#endif /* A4PAPER */
   SetPSPageWidthHeight ();
}

int UpdPageStyle (NewPageStyle)
   int	NewPageStyle;
{
   int	changed = FALSE, old_w = paperWidth, old_h = paperHeight;

   pageStyle = NewPageStyle;
   onePageWidth =
         round(psPageWidthInInch[NewPageStyle]*PIX_PER_INCH*100/printMag);
   onePageHeight =
         round(psPageHeightInInch[NewPageStyle]*PIX_PER_INCH*100/printMag);
   paperWidth = onePageWidth * paperCol;
   paperHeight = onePageHeight * paperRow;
   if (drawOrigX + drawWinW > paperWidth || drawOrigX + drawWinW > old_w)
   {
      if (drawOrigX + drawWinW > paperWidth)
      {
         int	tmp_x = paperWidth-drawWinW;

         switch (gridSystem)
         {
            case ENGLISH_GRID:
               if (zoomedIn)
               {
                  if ((ZOOMED_SIZE(tmp_x) % HALF_INCH) == 0)
                     drawOrigX = max(0,tmp_x);
                  else
                     drawOrigX = max(0,((int)(ZOOMED_SIZE(tmp_x)/HALF_INCH)) *
                           HALF_INCH);
               }
               else
               {
                  if ((tmp_x % (ABS_SIZE(HALF_INCH))) == 0)
                     drawOrigX = max(0,tmp_x);
                  else
                     drawOrigX = max(0,((int)(tmp_x/ABS_SIZE(HALF_INCH))) *
                           ABS_SIZE(HALF_INCH));
               }
               break;
            case METRIC_GRID:
               if (zoomedIn)
               {
                  if (zoomScale > 1)
                  {
                     if ((ZOOMED_SIZE(tmp_x) % ONE_CM) == 0)
                        drawOrigX = max(0,tmp_x);
                     else
                        drawOrigX = max(0,((int)(ZOOMED_SIZE(tmp_x)/ONE_CM)) *
                              ONE_CM);
                  }
                  else
                  {
                     if ((ZOOMED_SIZE(tmp_x) % FAKE_CM) == 0)
                        drawOrigX = max(0,tmp_x);
                     else
                        drawOrigX = max(0,((int)(ZOOMED_SIZE(tmp_x)/FAKE_CM)) *
                              FAKE_CM);
                  }
               }
               else
               {
                  if ((tmp_x % (ABS_SIZE(ONE_CM))) == 0)
                     drawOrigX = max(0,tmp_x);
                  else
                     drawOrigX = max(0,((int)(tmp_x/ABS_SIZE(ONE_CM))) *
                           ABS_SIZE(ONE_CM));
               }
               break;
         }
      }
      changed = TRUE;
   }
   if (drawOrigY + drawWinH > paperHeight || drawOrigY + drawWinH > old_h)
   {
      if (drawOrigY + drawWinH > paperHeight)
      {
         int	tmp_y = paperHeight-drawWinH;

         switch (gridSystem)
         {
            case ENGLISH_GRID:
               if (zoomedIn)
               {
                  if ((ZOOMED_SIZE(tmp_y) % HALF_INCH) == 0)
                     drawOrigY = max(0,tmp_y);
                  else
                     drawOrigY = max(0,((int)(ZOOMED_SIZE(tmp_y)/HALF_INCH)) *
                           HALF_INCH);
               }
               else
               {
                  if ((tmp_y % (ABS_SIZE(HALF_INCH))) == 0)
                     drawOrigY = max(0,tmp_y);
                  else
                     drawOrigY = max(0,((int)(tmp_y/ABS_SIZE(HALF_INCH))) *
                           ABS_SIZE(HALF_INCH));
               }
               break;
            case METRIC_GRID:
               if (zoomedIn)
               {
                  if (zoomScale > 1)
                  {
                     if ((ZOOMED_SIZE(tmp_y) % FAKE_CM) == 0)
                        drawOrigY = max(0,tmp_y);
                     else
                        drawOrigY = max(0,((int)(ZOOMED_SIZE(tmp_y)/FAKE_CM)) *
                              FAKE_CM);
                  }
                  else
                  {
                     if ((ZOOMED_SIZE(tmp_y) % ONE_CM) == 0)
                        drawOrigY = max(0,tmp_y);
                     else
                        drawOrigY = max(0,((int)(ZOOMED_SIZE(tmp_y)/ONE_CM)) *
                              ONE_CM);
                  }
               }
               else
               {
                  if ((tmp_y % (ABS_SIZE(ONE_CM))) == 0)
                     drawOrigY = max(0,tmp_y);
                  else
                     drawOrigY = max(0,((int)(tmp_y/ABS_SIZE(ONE_CM))) *
                           ABS_SIZE(ONE_CM));
               }
               break;
         }
      }
      changed = TRUE;
   }
   return (changed);
}

void ChangePageStyle (NewPageStyle, PageStyleStr)
   int	NewPageStyle;
   char	* PageStyleStr;
{
   char msg[MAXSTRING];

   if (pageStyle != NewPageStyle)
   {
      if (UpdPageStyle (NewPageStyle))
      {
         UpdDrawWinBBox ();
         AdjSplineVs ();
         ClearAndRedrawDrawWindow ();
      }
      ShowFile ();
      UpdateSubMenu (MENU_LAYOUT);
      RedrawScrollBars ();
      RedrawRulers ();
      SetFileModified (TRUE);
      sprintf (msg, "Page style changed to %s.", PageStyleStr);
      Msg (msg);
   }
}

void LayoutSubMenu (index)
   int	index;
{
   switch (index)
   {
      case LAYOUT_INCGRID: IncGrid (); break;
      case LAYOUT_DECGRID: DecGrid (); break;
      case LAYOUT_GRID: ToggleGridShown (); break;
      case LAYOUT_SNAP: ToggleSnapOn (); break;
      case LAYOUT_GOHOME: ScrollToOrigin (); break;
      case LAYOUT_SETHOME: SaveOrigin (); break;
      case LAYOUT_ZOOMIN: ZoomIn (); break;
      case LAYOUT_ZOOMOUT: ZoomOut (); break;
      case LAYOUT_DEFZOOM: DefaultZoom (); break;
      case LAYOUT_ZOOMWAYOUT: ZoomWayOut (); break;
      case LAYOUT_LAND: ChangePageStyle (LANDSCAPE, "LandScape"); break;
      case LAYOUT_PORT: ChangePageStyle (PORTRAIT, "Portrait"); break;
      case LAYOUT_PRINTMAG: SetPrintReduction (); break;
      case LAYOUT_TOGGLE_WHERE_TO_PRINT: ToggleWhereToPrint (); break;
      case LAYOUT_TOGGLE_COLOR_PS: ToggleColorPostScript (); break;
      case LAYOUT_TOGGLE_MOVE_MODE: ToggleMoveMode (); break;
      case LAYOUT_TOGGLE_GRID_SYSTEM: ToggleGridSystem (); break;
      case LAYOUT_TOGGLE_MAP_SHOWN: ToggleMapShown (); break;
      case LAYOUT_TOGGLE_USE_GRAY: ToggleUseGray (); break;
      case LAYOUT_TOGGLE_SHOW_MEASURE: ToggleShowMeasurement (); break;
      case LAYOUT_TOGGLE_SET_MEASURE_UNIT: SetMeasureUnit (); break;
      case LAYOUT_TOGGLE_SHOW_MENUBAR: ToggleShowMenubar (); break;
      case LAYOUT_TOGGLE_SHOW_STATUS: ToggleShowStatus (); break;
      case LAYOUT_TOGGLE_ONEMOTION_SELMOVE: ToggleOneMotionSelectMove (); break;
      case LAYOUT_TOGGLE_COLOR_LAYERS: ToggleColorLayers (); break;
   }
}

void SetUpLayoutMenuStr (menu_strings, menu_description)
   char	* * menu_strings, * * menu_description;
{
   register int	i;
   char		* str_ptr, * desc_ptr;

   str_ptr = (char*)malloc(MAX_LAYOUT_STR*MAXLAYOUTMENUS*sizeof(char));
   desc_ptr = (char*)malloc(MAX_LAYOUT_DESC*MAXLAYOUTMENUS*sizeof(char));
   if (str_ptr == NULL || desc_ptr == NULL) FailAllocMessage();
   for (i = 0; i < MAXLAYOUTMENUS; i++, str_ptr += MAX_LAYOUT_STR,
         desc_ptr += MAX_LAYOUT_DESC) {
      menu_strings[i] = str_ptr;
      strcpy (menu_strings[i], layoutMenuStr[i]);
      menu_description[i] = desc_ptr;
      strcpy (menu_description[i], layoutMenuDescription[i]);
   }
   if (gridShown) {
      strcpy(menu_strings[LAYOUT_GRID], hideGridStr);
      strcpy(menu_description[LAYOUT_GRID], hideGridDesc);
   } else {
      strcpy(menu_strings[LAYOUT_GRID], showGridStr);
      strcpy(menu_description[LAYOUT_GRID], showGridDesc);
   }
   if (gridOn)
   {
      strcpy (menu_strings[LAYOUT_SNAP], snapOffStr);
      strcpy (menu_description[LAYOUT_SNAP], snapOffDesc);
   }
   else
   {
      strcpy (menu_strings[LAYOUT_SNAP], snapOnStr);
      strcpy (menu_description[LAYOUT_SNAP], snapOnDesc);
   }
   switch (whereToPrint)
   {
      case PRINTER:
         strcpy (menu_strings[LAYOUT_TOGGLE_WHERE_TO_PRINT], latexFigStr);
         strcpy (menu_description[LAYOUT_TOGGLE_WHERE_TO_PRINT], latexFigDesc);
         break;
      case LATEX_FIG:
         strcpy (menu_strings[LAYOUT_TOGGLE_WHERE_TO_PRINT], psFileStr);
         strcpy (menu_description[LAYOUT_TOGGLE_WHERE_TO_PRINT], psFileDesc);
         break;
      case PS_FILE:
         if (colorDump)
         {
            strcpy (menu_strings[LAYOUT_TOGGLE_WHERE_TO_PRINT], xpmStr);
            strcpy (menu_description[LAYOUT_TOGGLE_WHERE_TO_PRINT], xpmDesc);
         }
         else
         {
            strcpy (menu_strings[LAYOUT_TOGGLE_WHERE_TO_PRINT], xbmStr);
            strcpy (menu_description[LAYOUT_TOGGLE_WHERE_TO_PRINT], xbmDesc);
         }
         break;
      case XBM_FILE:
         strcpy (menu_strings[LAYOUT_TOGGLE_WHERE_TO_PRINT], textFileStr);
         strcpy (menu_description[LAYOUT_TOGGLE_WHERE_TO_PRINT], textFileDesc);
         break;
      case TEXT_FILE:
         strcpy (menu_strings[LAYOUT_TOGGLE_WHERE_TO_PRINT], epsiFileStr);
         strcpy (menu_description[LAYOUT_TOGGLE_WHERE_TO_PRINT], epsiFileDesc);
         break;
      case EPSI_FILE:
         strcpy (menu_strings[LAYOUT_TOGGLE_WHERE_TO_PRINT], gifStr);
         strcpy (menu_description[LAYOUT_TOGGLE_WHERE_TO_PRINT], gifDesc);
         break;
      case GIF_FILE:
         strcpy (menu_strings[LAYOUT_TOGGLE_WHERE_TO_PRINT], htmlStr);
         strcpy (menu_description[LAYOUT_TOGGLE_WHERE_TO_PRINT], htmlDesc);
         break;
      case HTML_FILE:
         strcpy (menu_strings[LAYOUT_TOGGLE_WHERE_TO_PRINT], printerStr);
         strcpy (menu_description[LAYOUT_TOGGLE_WHERE_TO_PRINT], printerDesc);
         break;
   }
   if (colorDump)
   {
      strcpy (menu_strings[LAYOUT_TOGGLE_COLOR_PS], bwPostScriptStr);
      strcpy (menu_description[LAYOUT_TOGGLE_COLOR_PS], bwPostScriptDesc);
   }
   else
   {
      strcpy (menu_strings[LAYOUT_TOGGLE_COLOR_PS], colorPostScriptStr);
      strcpy (menu_description[LAYOUT_TOGGLE_COLOR_PS], colorPostScriptDesc);
   }
   switch (moveMode)
   {
      case CONST_MOVE:
         strcpy (menu_strings[LAYOUT_TOGGLE_MOVE_MODE], unConstMoveStr);
         strcpy (menu_description[LAYOUT_TOGGLE_MOVE_MODE], unConstMoveDesc);
         break;
      case UNCONST_MOVE:
         strcpy (menu_strings[LAYOUT_TOGGLE_MOVE_MODE], constMoveStr);
         strcpy (menu_description[LAYOUT_TOGGLE_MOVE_MODE], constMoveDesc);
         break;
   }
   switch (gridSystem)
   {
      case ENGLISH_GRID:
         strcpy (menu_strings[LAYOUT_TOGGLE_GRID_SYSTEM], metricGridSysStr);
         strcpy (menu_description[LAYOUT_TOGGLE_GRID_SYSTEM],
               metricGridSysDesc);
         break;
      case METRIC_GRID:
         strcpy (menu_strings[LAYOUT_TOGGLE_GRID_SYSTEM], englishGridSysStr);
         strcpy (menu_description[LAYOUT_TOGGLE_GRID_SYSTEM],
               englishGridSysDesc);
         break;
   }
   if (mapShown)
   {
      strcpy (menu_strings[LAYOUT_TOGGLE_MAP_SHOWN], hideMapStr);
      strcpy (menu_description[LAYOUT_TOGGLE_MAP_SHOWN], hideMapDesc);
   }
   else
   {
      strcpy (menu_strings[LAYOUT_TOGGLE_MAP_SHOWN], showMapStr);
      strcpy (menu_description[LAYOUT_TOGGLE_MAP_SHOWN], showMapDesc);
   }
   if (useGray)
   {
      strcpy (menu_strings[LAYOUT_TOGGLE_USE_GRAY], noGrayStr);
      strcpy (menu_description[LAYOUT_TOGGLE_USE_GRAY], noGrayDesc);
   }
   else
   {
      strcpy (menu_strings[LAYOUT_TOGGLE_USE_GRAY], useGrayStr);
      strcpy (menu_description[LAYOUT_TOGGLE_USE_GRAY], useGrayDesc);
   }
   if (showMeasurement)
   {
      strcpy (menu_strings[LAYOUT_TOGGLE_SHOW_MEASURE], hideMeasureStr);
      strcpy (menu_description[LAYOUT_TOGGLE_SHOW_MEASURE], hideMeasureDesc);
   }
   else
   {
      strcpy (menu_strings[LAYOUT_TOGGLE_SHOW_MEASURE], showMeasureStr);
      strcpy (menu_description[LAYOUT_TOGGLE_SHOW_MEASURE], showMeasureDesc);
   }
   if (noMenubar)
   {
      strcpy (menu_strings[LAYOUT_TOGGLE_SHOW_MENUBAR], showMenubarStr);
      strcpy (menu_description[LAYOUT_TOGGLE_SHOW_MENUBAR], showMenubarDesc);
   }
   else
   {
      strcpy (menu_strings[LAYOUT_TOGGLE_SHOW_MENUBAR], hideMenubarStr);
      strcpy (menu_description[LAYOUT_TOGGLE_SHOW_MENUBAR], hideMenubarDesc);
   }
   if (noStatusWindow)
   {
      strcpy (menu_strings[LAYOUT_TOGGLE_SHOW_STATUS], showStatusStr);
      strcpy (menu_description[LAYOUT_TOGGLE_SHOW_STATUS], showStatusDesc);
   }
   else
   {
      strcpy (menu_strings[LAYOUT_TOGGLE_SHOW_STATUS], hideStatusStr);
      strcpy (menu_description[LAYOUT_TOGGLE_SHOW_STATUS], hideStatusDesc);
   }
   if (oneMotionSelectMove) {
      strcpy (menu_strings[LAYOUT_TOGGLE_ONEMOTION_SELMOVE],
            clickSelClickMoveStr);
   } else {
      strcpy (menu_strings[LAYOUT_TOGGLE_ONEMOTION_SELMOVE],
            oneMotionSelMoveStr);
   }
   if (colorLayers) {
      strcpy(menu_strings[LAYOUT_TOGGLE_COLOR_LAYERS], noColorLayersStr);
      strcpy (menu_description[LAYOUT_TOGGLE_SHOW_STATUS], noColorLayersDesc);
   } else {
      strcpy(menu_strings[LAYOUT_TOGGLE_COLOR_LAYERS], colorLayersStr);
      strcpy (menu_description[LAYOUT_TOGGLE_SHOW_STATUS], colorLayersDesc);
   }
}

int LayoutMenu (X, Y, TrackMenubar)
   int	X, Y, TrackMenubar;
{
   int		index, * fore_colors, * valid, * init_rv;
   char		* menu_strings[MAXLAYOUTMENUS];
   char		* menu_description[MAXLAYOUTMENUS];

   SetUpLayoutMenuStr (menu_strings, menu_description);

   DefaultColorArrays (MAXLAYOUTMENUS, &fore_colors, &valid, &init_rv, NULL);

   switch (pageStyle)
   {
      case PORTRAIT: init_rv[LAYOUT_PORT] = TRUE; break;
      case LANDSCAPE: init_rv[LAYOUT_LAND] = TRUE; break;
   }
   activeMenu = MENU_LAYOUT;
   index = TextMenuLoop (X, Y, menu_strings, MAXLAYOUTMENUS,
         fore_colors, valid, init_rv, menu_description, SINGLECOLOR,
         TrackMenubar);

   free(menu_strings[0]);
   free(menu_description[0]);

   if (index >= 0) LayoutSubMenu (index);
   return (index);
}

void GridXY (X, Y, GridX, GridY)
   int	X, Y, * GridX, * GridY;
{
   register int	dx, dy, grid = ENGLISH_GRID;

   if (gridOn)
   {
      switch (gridSystem)
      {
         case ENGLISH_GRID: grid = xyEnglishGrid; break;
         case METRIC_GRID: grid = xyMetricGrid; break;
      }
      if (zoomedIn)
      {
         grid <<= zoomScale;
         dx = (X+(drawOrigX<<zoomScale)) % grid;
         dy = (Y+(drawOrigY<<zoomScale)) % grid;
      }
      else
      {
         dx = (X+drawOrigX) % grid;
         dy = (Y+drawOrigY) % grid;
      }
      *GridX = (dx < grid/2) ? X-dx : X-dx+grid;
      *GridY = (dy < grid/2) ? Y-dy : Y-dy+grid;
   }
   else
   {
      if (zoomedIn)
      {
         grid = 1 << zoomScale;
         dx = (X+(drawOrigX<<zoomScale)) % grid;
         dy = (Y+(drawOrigY<<zoomScale)) % grid;
         *GridX = (dx < grid/2) ? X-dx : X-dx+grid;
         *GridY = (dy < grid/2) ? Y-dy : Y-dy+grid;
      }
      else
      {
         *GridX = X;
         *GridY = Y;
      }
   }
}

void CleanUpGrids ()
{
}

void MoveModeSubMenu (index)
   int	index;
{
   moveMode = index;
   switch (moveMode)
   {
      case CONST_MOVE: Msg ("Constrained move."); break;
      case UNCONST_MOVE: Msg ("Unconstrained move."); break;
   }
   ShowMoveMode ();
   UpdateSubMenu (MENU_LAYOUT);
   UpdateSubMenu (MENU_MOVEMODE);
}

static char * moveModeMenuDescription[] =
{
   "Constrained move mode",
   "Unconstrained move mode",
   NULL
};

int MoveModeMenu (X, Y, TrackMenubar)
   int	X, Y, TrackMenubar;
{
   int	index, * fore_colors, * valid, * init_rv;

   DefaultColorArrays (MAXMOVEMODES, &fore_colors, &valid, &init_rv, NULL);
   free(valid);
   init_rv[moveMode] = TRUE;
   activeMenu = MENU_MOVEMODE;
   index = PxMpMenuLoop (X, Y, choiceImageW, choiceImageH, MAXMOVEMODES, 1,
         MAXMOVEMODES, fore_colors, moveModePixmap, init_rv,
         moveModeMenuDescription, SINGLECOLOR, TrackMenubar);

   if (index >= 0) MoveModeSubMenu (index);
   return (index);
}

void StretchableTextModeSubMenu(index)
   int index;
{
   stretchableText = index;
   switch (stretchableText) {
   case NO_STRETCHABLE_TEXT: Msg("Text objects are not stretchable."); break;
   case STRETCHABLE_TEXT: Msg("Text objects are stretchable."); break;
   }
   ShowStretchableTextMode();
   UpdateSubMenu(MENU_STRETCHTEXT);
}

static char * stretchableTextModeMenuDescription[] =
{
   "Make text objects stretchable",
   "Make text objects not stretchable",
   NULL
};

int StretchableTextModeMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   int index, *fore_colors, *valid, *init_rv;

   DefaultColorArrays(MAXSTRETCHABLEMODES, &fore_colors, &valid, &init_rv,
         NULL);
   free(valid);
   init_rv[stretchableText] = TRUE;
   activeMenu = MENU_STRETCHTEXT;
   index = PxMpMenuLoop(X, Y, choiceImageW, choiceImageH, MAXSTRETCHABLEMODES,
         1, MAXSTRETCHABLEMODES, fore_colors, stretchableModePixmap, init_rv,
         stretchableTextModeMenuDescription, SINGLECOLOR, TrackMenubar);

   if (index >= 0) StretchableTextModeSubMenu(index);
   return (index);
}
