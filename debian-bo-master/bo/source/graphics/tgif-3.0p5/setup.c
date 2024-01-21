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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/setup.c,v 3.0 1996/05/06 16:07:31 william Exp $";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "const.h"
#include "types.h"

#include "attr.e"
#include "auxtext.e"
#include "choice.e"
#include "cmd.e"
#include "color.e"
#include "cursor.e"
#include "drawing.e"
#include "dup.e"
#include "file.e"
#include "font.e"
#include "grid.e"
#include "help.e"
#include "imgproc.e"
#include "import.e"
#include "mainloop.e"
#include "mainmenu.e"
#include "mark.e"
#include "menu.e"
#include "move.e"
#include "msg.e"
#include "names.e"
#include "page.e"
#include "pattern.e"
#include "ps.e"
#include "raster.e"
#include "remote.e"
#include "ruler.e"
#include "scroll.e"
#include "select.e"
#ifndef _NO_EXTERN
#include "setup.e"
#endif
#include "shape.e"
#include "shortcut.e"
#include "spline.e"
#include "stk.e"
#include "stretch.e"
#include "text.e"
#include "xbitmap.e"
#include "xpixmap.e"

#include "tgificon.xbm"


extern char	* getenv ARGS_DECL((char *));
#ifdef __hpux
extern double	atof ARGS_DECL((const char *));
#else
extern double	atof ARGS_DECL((char *));
#endif
extern int	atoi ARGS_DECL((char *));

#define MENU_IMAGE_W 64
#define MENU_IMAGE_H 20
#define CHOICE_IMAGE_W 32
#define CHOICE_IMAGE_H 20
#define SCROLLBAR_W 16
#define RULER_W 20
#define BRDR_W 1

#define DEF_VSBAR_H 64
#define DEF_HSBAR_W 128

#define DRAW_WINDOW_W (5*PIX_PER_INCH)
#define DRAW_WINDOW_H (5*PIX_PER_INCH)
#define CHOICE_WINDOW_W (MAXCHOICEWINDOWCOLS*CHOICE_IMAGE_W)
#define CHOICE_WINDOW_H (CHOICE_IMAGE_H<<1)
#define VSBAR_H (DRAW_WINDOW_H+RULER_W+(BRDR_W<<1))
#define HSBAR_W 128
#define PAGE_DUMMY_WINDOW_W 3
#define PAGE_WINDOW_W (DRAW_WINDOW_W+RULER_W-HSBAR_W-PAGE_DUMMY_WINDOW_W)
#define COLOR_DUMMY_WINDOW_H 3
#define COLOR_WINDOW_H (DRAW_WINDOW_H+RULER_W-VSBAR_H-COLOR_DUMMY_WINDOW_H)
#define TITLE_WINDOW_W (DRAW_WINDOW_W+RULER_W+BRDR_W+SCROLLBAR_W+(BRDR_W<<1))
#define TITLE_WINDOW_H (MENU_IMAGE_H<<1)
#define MSG_WINDOW_W (TITLE_WINDOW_W-CHOICE_WINDOW_W-(BRDR_W<<1))
#define MSG_WINDOW_H (CHOICE_WINDOW_H)
#define ICON_WINDOW_W 64
#define ICON_WINDOW_H 64
#define MENUBAR_WINDOW_W (TITLE_WINDOW_W)
#define MENUBAR_WINDOW_H 20
#define STATUS_WINDOW_W (TITLE_WINDOW_W)
#define STATUS_WINDOW_H 20

unsigned int	mainWinW = 0;
unsigned int	mainWinH = 0;
int	vSBarH = VSBAR_H;
int	hSBarW = HSBAR_W;
int	scrollBarW = SCROLLBAR_W;
int	rulerW = RULER_W;
int	brdrW = BRDR_W;
int	pageWindowW = PAGE_WINDOW_W;
int	pageDummyWindowW = PAGE_DUMMY_WINDOW_W;
int	colorWindowH = COLOR_WINDOW_H;
int	colorDummyWindowH = COLOR_DUMMY_WINDOW_H;
int	msgWindowW = MSG_WINDOW_W;
int	msgWindowH = MSG_WINDOW_H;
int	choiceImageW = CHOICE_IMAGE_W;
int	choiceImageH = CHOICE_IMAGE_H;
int	choiceWindowW = CHOICE_WINDOW_W;
int	choiceWindowH = CHOICE_WINDOW_H;
int	menuImageW = MENU_IMAGE_W;
int	menuImageH = MENU_IMAGE_H;
int	titleWindowW = TITLE_WINDOW_W;
int	titleWindowH = TITLE_WINDOW_H;
int	iconWindowW = ICON_WINDOW_W;
int	iconWindowH = ICON_WINDOW_H;
int	menubarWindowW = MENUBAR_WINDOW_W;
int	menubarWindowH = MENUBAR_WINDOW_H;
int	statusWindowW = STATUS_WINDOW_W;
int	statusWindowH = STATUS_WINDOW_H;
int	statusSubWindowW[MAX_STATUS_BTNS];
int	statusSubWindowH[MAX_STATUS_BTNS];

int	initialMenubarWindowH=MENUBAR_WINDOW_H;

static int	statusSubWindowX[MAX_STATUS_BTNS];
static int	statusSubWindowY[MAX_STATUS_BTNS];

Display		* mainDisplay=NULL;
Colormap	mainColormap=(Colormap)0;
unsigned int	mainDepth;
int		mainScreen;
Visual		* mainVisual=NULL;

Window	rootWindow=None;
Window	mainWindow=None;
Window	drawWindow=None;
Window	choiceWindow=None;
Window	titleWindow=None;
Window	msgWindow=None;
Window	vSBarWindow=None;
Window	hSBarWindow=None;
Window	vRuleWindow=None;
Window	hRuleWindow=None;
Window	iconWindow=None;
Window	iconBaseWindow=None;
Window	menubarWindow=None;
Window	statusWindow=None;
Window	pageWindow=None;
Window	pageDummyWindow=None;
Window	colorWindow=None;
Window	colorDummyWindow=None;

#ifndef A4PAPER
int	onePageWidth = (85*PIX_PER_INCH)/10;
int	onePageHeight = 11*PIX_PER_INCH;
int	paperWidth = (85*PIX_PER_INCH)/10;
int	paperHeight = 11*PIX_PER_INCH;
#else /* A4PAPER */
int	onePageWidth = (825*PIX_PER_INCH)/100;
int	onePageHeight = (117*PIX_PER_INCH)/10;
int	paperWidth = (825*PIX_PER_INCH)/100;
int	paperHeight = (117*PIX_PER_INCH)/10;
#endif /* A4PAPER */
int	drawOrigX = 0;
int	drawOrigY = 0;
int	drawWinW = DRAW_WINDOW_W;
int	drawWinH = DRAW_WINDOW_H;

int	zoomScale = 0;
int	zoomedIn = FALSE;

struct BBRec	drawWinBBox;

int	colorDisplay = FALSE;
int	fileModified = FALSE;
int	objId = 0;

int	myBgPixel;
int	myFgPixel;
int	myBorderPixel;
int	reverseVideo = FALSE;

char	drawPath[MAXPATHLENGTH];
char	bootDir[MAXPATHLENGTH+2];
char	homeDir[MAXPATHLENGTH];

int	symPathNumEntries = INVALID;
char	* * symPath=NULL;

int	initDrawWinW, initDrawWinH;

short	handleSize=3;
int	resizeTextOnStretch=FALSE;

Window	dummyWindow1=None, dummyWindow2=None;

Window	statusSubWindow[MAX_STATUS_BTNS];

int	noMenubar=FALSE;
int	noStatusWindow=FALSE;

static Atom	protocolAtom=(Atom)0;

static int	canvasWindowOnly=FALSE;

void UpdDrawWinWH ()
{
   drawWinW = ABS_SIZE(initDrawWinW);
   drawWinH = ABS_SIZE(initDrawWinH);
}

void UpdDrawWinBBox ()
{
   drawWinBBox.ltx = drawOrigX;
   drawWinBBox.lty = drawOrigY;
   drawWinBBox.rbx = drawOrigX + drawWinW-1;
   drawWinBBox.rby = drawOrigY + drawWinH-1;
}

#include "xbm/btn1.xbm"

static
void CalcStatusSubWinGeom ()
{
   register int	i;
   int		left=0, w=(int)(statusWindowW/3), win_y, right=0, win_h;

   if (defaultFontHeight+(brdrW<<1)+2 > btn1_height)
      win_y = 1;
   else
      win_y = (statusWindowH-defaultFontHeight-(brdrW<<1)-2)>>1;
   win_h = statusWindowH-(win_y<<1)-2;
   for (i=0; i < MAX_STATUS_BTNS; i++)
   {
      int	win_x, win_w;

      right += w;
      if (right >= statusWindowW) right = statusWindowW-1;
      win_x = left+(brdrW<<2)+btn1_width+2;
      win_w = right-left-(brdrW*6)-btn1_width-2;
      statusSubWindowX[i] = win_x;
      statusSubWindowY[i] = win_y;
      statusSubWindowW[i] = win_w;
      statusSubWindowH[i] = win_h;
      left += w;
   }
}

static
void InitWinSizes ()
{
   initDrawWinW = drawWinW;
   initDrawWinH = drawWinH;
   titleWindowH = (showVersion) ? (MENU_IMAGE_H<<1) : MENU_IMAGE_H;
   pageWindowW = drawWinW+rulerW-hSBarW-pageDummyWindowW;
   colorWindowH = drawWinH+rulerW-DEF_VSBAR_H-colorDummyWindowH;
   vSBarH = drawWinH+rulerW+(brdrW<<1);
   titleWindowW = drawWinW+rulerW+brdrW+scrollBarW+(brdrW<<1);
   menubarWindowW = statusWindowW = titleWindowW;
   CalcMenubarWindowHeight ();
   msgWindowW = titleWindowW-choiceWindowW-(brdrW<<1);
   mainWinW = titleWindowW+(brdrW<<1);
   mainWinH =
         (noMenubar ? 0 : menubarWindowH+(brdrW<<1)) +
         (noStatusWindow ? 0 : statusWindowH+(brdrW<<1)) +
         titleWindowH+choiceWindowH+vSBarH+scrollBarW+8*brdrW;
   if (colorLayers) vSBarH = DEF_VSBAR_H;
   CalcStatusSubWinGeom ();
}

static
void InverseInitWinSizes ()
   /* derive other win sizes from mainWinW and mainWinH */
{
   titleWindowW = mainWinW-(brdrW<<1);
   menubarWindowW = statusWindowW = titleWindowW;
   CalcMenubarWindowHeight ();
   msgWindowW = titleWindowW-choiceWindowW-(brdrW<<1);
   vSBarH = mainWinH - titleWindowH - choiceWindowH - scrollBarW - 8*brdrW -
         (noMenubar ? 0 : menubarWindowH+(brdrW<<1)) -
         (noStatusWindow ? 0 : statusWindowH+(brdrW<<1));
   drawWinH = initDrawWinH = vSBarH-rulerW-(brdrW<<1);
   drawWinW = initDrawWinW = titleWindowW-scrollBarW-rulerW-(brdrW<<2);
   pageWindowW = titleWindowW-scrollBarW-hSBarW-pageDummyWindowW-(brdrW<<2);
   colorWindowH = drawWinH+rulerW-DEF_VSBAR_H-colorDummyWindowH;
   if (colorLayers) vSBarH = DEF_VSBAR_H;
   CalcMenubarWindowHeight ();
   CalcStatusSubWinGeom ();
}

void ComputeMainWinXY (MainWinX, MainWinY)
   int	* MainWinX, * MainWinY;
{
   int		win_x, win_y, done = FALSE;
   unsigned int	win_w, win_h, win_brdr_w, win_d, num_child;
   Window	win = mainWindow, root_win, parent_win, * child_wins;

   *MainWinX = *MainWinY = 0;
   while (!done)
   {
      XGetGeometry (mainDisplay, win, &root_win, &win_x, &win_y, &win_w,
            &win_h, &win_brdr_w, &win_d);
      *MainWinX += win_x;
      *MainWinY += win_y;
      if (XQueryTree (mainDisplay, win, &root_win, &parent_win, &child_wins,
            &num_child) == 0)
         return;
      if (child_wins != NULL) XFree ((void *)child_wins);
      if (parent_win == rootWindow)
         done = TRUE;
      else
         win = parent_win;
   }
}

void Reconfigure (Forced)
   int	Forced;
{
   register int	i;
   Window	root_win;
   int		win_x, win_y, main_win_x, main_win_y;
   unsigned int	win_w, win_h, win_brdr_w, win_d;
   XEvent	ev;

   ComputeMainWinXY (&main_win_x, &main_win_y);
   XGetGeometry (mainDisplay, mainWindow, &root_win, &win_x, &win_y, &win_w,
         &win_h, &win_brdr_w, &win_d);
   if (!Forced && win_w == mainWinW && win_h == mainWinH)
   {
      SaveStackingOrder ();
      if (pinnedMainMenu)
         MoveMainMenuWindow (main_win_x, main_win_y);
      else
         SaveMainWinPosition (main_win_x, main_win_y);
      for (i = 0; i < numExtraWins; i++)
         if (extraWinInfo[i].mapped && extraWinInfo[i].raise &&
               extraWinInfo[i].window != None)
            MoveSubMenuWindow (extraWinInfo[i].window);
      for (i = 0; i < numStacking; i++)
         XMapRaised (mainDisplay, stackingWins[i]);

      while (XCheckWindowEvent (mainDisplay, mainWindow,
            VisibilityChangeMask | StructureNotifyMask, &ev)) ;

      XFlush (mainDisplay);
      return;
   }
   if (!Forced && !iconWindowShown)
   {
      SaveStackingOrder ();
      for (i = 0; i < numStacking; i++)
         XMapRaised (mainDisplay, stackingWins[i]);
   }

   mainWinW = win_w;
   mainWinH = win_h;

   menubarWindowW = mainWinW - (brdrW<<1);
   CalcMenubarWindowHeight ();
   if (canvasWindowOnly)
   {
      initDrawWinW = mainWinW-(brdrW<<1);
      initDrawWinH = mainWinH-(brdrW<<1);
   }
   else
   {
      initDrawWinW = mainWinW - rulerW - scrollBarW - 6*brdrW;
      initDrawWinH = mainWinH - titleWindowH -
            (noMenubar ? 0 : menubarWindowH+(brdrW<<1)) -
            (noStatusWindow ? 0 : statusWindowH+(brdrW<<1)) -
            choiceWindowH - rulerW - scrollBarW - 10*brdrW;
   }
   drawWinW = initDrawWinW;
   drawWinH = initDrawWinH;
   pageWindowW = initDrawWinW+rulerW-hSBarW-pageDummyWindowW;
   colorWindowH = initDrawWinH+rulerW-DEF_VSBAR_H-colorDummyWindowH-(brdrW<<1);
   vSBarH = (colorLayers ? DEF_VSBAR_H : initDrawWinH + rulerW + (brdrW<<1));
   titleWindowW = initDrawWinW + rulerW + scrollBarW + (brdrW<<2);
   menubarWindowW = statusWindowW = titleWindowW;
   CalcMenubarWindowHeight ();
   msgWindowW = titleWindowW - choiceWindowW - (brdrW<<1);
   CalcStatusSubWinGeom ();
   if (canvasWindowOnly)
      XResizeWindow (mainDisplay, drawWindow, drawWinW, drawWinH);
   else
   {
      int	cur_y=0;

      XResizeWindow (mainDisplay, titleWindow, titleWindowW, titleWindowH);
      cur_y += titleWindowH + (brdrW<<1);
      if (!noMenubar)
      {
         XResizeWindow (mainDisplay, menubarWindow, menubarWindowW,
               menubarWindowH);
         cur_y += menubarWindowH + (brdrW<<1);
      }
      XMoveResizeWindow (mainDisplay, msgWindow, 0, cur_y,
            msgWindowW, msgWindowH);
      XMoveWindow (mainDisplay, choiceWindow, msgWindowW+(brdrW<<1), cur_y);
      cur_y += choiceWindowH + (brdrW<<1);
      XMoveResizeWindow (mainDisplay, dummyWindow1, 0, cur_y,
            drawWinW, rulerW);
      XMoveResizeWindow (mainDisplay, hRuleWindow, rulerW+(brdrW<<1), cur_y,
            drawWinW, rulerW);
      if (colorLayers) {
         XMoveResizeWindow (mainDisplay, colorWindow,
               drawWinW+rulerW+(brdrW<<2), cur_y, scrollBarW, colorWindowH);
         XMoveWindow (mainDisplay, colorDummyWindow,
               drawWinW+rulerW+(brdrW<<2), cur_y+colorWindowH+(brdrW<<1));
         XMoveResizeWindow (mainDisplay, vSBarWindow,
               drawWinW+rulerW+(brdrW<<2),
               cur_y+colorWindowH+colorDummyWindowH+(brdrW<<2), scrollBarW,
               DEF_VSBAR_H);
      } else {
         XMoveResizeWindow (mainDisplay, vSBarWindow,
               drawWinW+rulerW+(brdrW<<2), cur_y, scrollBarW, vSBarH);
      }
      cur_y += rulerW + (brdrW<<1);
      XMoveResizeWindow (mainDisplay, vRuleWindow, 0, cur_y,
            rulerW, drawWinH);
      XMoveResizeWindow (mainDisplay, drawWindow, rulerW+(brdrW<<1), cur_y,
            drawWinW, drawWinH);
      cur_y += drawWinH + (brdrW<<1);
      XMoveResizeWindow (mainDisplay, pageWindow, 0, cur_y, pageWindowW,
            scrollBarW+(brdrW<<1));
      XMoveWindow (mainDisplay, pageDummyWindow, pageWindowW, cur_y);
      XMoveWindow (mainDisplay, hSBarWindow,
            pageWindowW+pageDummyWindowW+(brdrW<<1), cur_y);
      XMoveWindow (mainDisplay, dummyWindow2, drawWinW+rulerW+(brdrW<<2),
            cur_y);
      cur_y += scrollBarW + (brdrW<<1);
      if (!noStatusWindow)
      {
         XMoveResizeWindow (mainDisplay, statusWindow, 0, cur_y,
               statusWindowW, statusWindowH);
         for (i=0; i < MAX_STATUS_BTNS; i++)
            XMoveResizeWindow (mainDisplay, statusSubWindow[i],
                  statusSubWindowX[i], statusSubWindowY[i],
                  statusSubWindowW[i], statusSubWindowH[i]);
      }
   }
   UpdDrawWinWH ();
   UpdDrawWinBBox ();
   UpdScrollWinWH ();
   SetDefaultDrawWinClipRecs ();
   SaveMainWinPosition (main_win_x, main_win_y);
}

int mainWinEventHandler (input)
   XEvent	* input;
{
   register int	i;
   int		configure = FALSE;
   char		* c_ptr, msg[MAXSTRING];

   if (input->type == UnmapNotify)
      Iconify ();
   else if (input->type == MapNotify)
      UnIconify ();
   else if (input->type == ConfigureNotify)
      configure = TRUE;
   else if (input->type == VisibilityNotify &&
         input->xvisibility.state == VisibilityUnobscured)
   {
      if (iconWindowShown)
         UnIconify ();
      else
      {
         SaveStackingOrder ();
         for (i = 0; i < numStacking; i++)
            XMapRaised (mainDisplay, stackingWins[i]);
      }
   }
   else if (input->type == ClientMessage)
   {
      c_ptr = XGetAtomName (mainDisplay, input->xclient.message_type);
      if (strcmp ("WM_MOVED", c_ptr) == 0)
         configure = TRUE;
      else if (strcmp ("WM_PROTOCOLS", c_ptr) == 0 &&
            input->xclient.data.l[0] == protocolAtom)
         return (QuitProc ());
      else
      {
         sprintf (msg, "mainWindow ClientMessage: '%s'.", c_ptr);
         Msg (msg);
      }
      XFree ((void *)c_ptr);
   }

   if (configure) Reconfigure (FALSE);
   if (showCrossHair) {
      SetNullCursor(drawWindow);
   }
   return (INVALID);
}

void InitPaperSize ()
{
   psYOffStr = (char**)malloc(MAXPAGESTYLES*sizeof(char *));
   if (psYOffStr == NULL) FailAllocMessage();
   psYOffStr[0] = (char*)malloc(20*sizeof(char));
   psYOffStr[1] = (char*)malloc(2*sizeof(char));
   if (psYOffStr[0] == NULL || psYOffStr[1] == NULL) FailAllocMessage();
   sprintf (psYOffStr[0], "%.2f",
         (float)(((float)onePageHeight)/((float)PIX_PER_INCH)));
   sprintf (psYOffStr[1], "0");
   psYOff = (float*)malloc(MAXPAGESTYLES*sizeof(float));
   if (psYOff == NULL) FailAllocMessage();
   psYOff[0] = ((float)onePageHeight)/((float)PIX_PER_INCH);
   psYOff[1] = (float)0.0;
   psPageWidthInInch = (float*)malloc(MAXPAGESTYLES*sizeof(float));
   if (psPageWidthInInch == NULL) FailAllocMessage();
   psPageWidthInInch[0] = ((float)onePageWidth)/((float)PIX_PER_INCH);
   psPageWidthInInch[1] = ((float)onePageHeight)/((float)PIX_PER_INCH);
   psPageHeightInInch = (float*)malloc(MAXPAGESTYLES*sizeof(float));
   if (psPageHeightInInch == NULL) FailAllocMessage();
   psPageHeightInInch[0] = ((float)onePageHeight)/((float)PIX_PER_INCH);
   psPageHeightInInch[1] = ((float)onePageWidth)/((float)PIX_PER_INCH);
}

void CleanUpPaperSize ()
{
   free(psPageHeightInInch);
   free(psPageWidthInInch);
   free(psYOff);
   free(psYOffStr[0]);
   free(psYOffStr[1]);
   free(psYOffStr);
   psYOffStr = NULL;
   psYOff = psPageWidthInInch = psPageHeightInInch = NULL;
}

static Window	savedMainWindow=None, savedDrawWindow=None;

void Setup ()
{
   int		bitmask = 0, x_neg_in_def = FALSE, y_neg_in_def = FALSE;
   int		x_in_cmdline=FALSE, y_in_cmdline=FALSE, cur_y=0, i;
   char		* c_ptr;
   int		def_x_neg = 0, def_y_neg = 0, paper_size_set;
   XWMHints	wmhints;
   XSizeHints	sizehints;

   *homeDir = *bootDir = '\0';
   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME, "Synchronize")) != NULL)
      if ((strcmp (c_ptr, "on") == 0) || (strcmp (c_ptr, "On") == 0))
         XSynchronize (mainDisplay, True);

   showVersion = FALSE;
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "DontShowVersion")) != NULL &&
         UtilStrICmp(c_ptr, "false") == 0) {
      showVersion = TRUE;
   }

   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME, "PrintCommand")) != NULL)
      strcpy (printCommand, c_ptr);
   else
#ifdef PRINT_CMD
      strcpy (printCommand, PRINT_CMD);
#else
#ifdef VMS
      strcpy (printCommand, "print");
#else
#ifdef SYSV
      strcpy (printCommand, "lp -dpostscript");
#else
      strcpy (printCommand, "lpr");
#endif /* SYSV */
#endif /* VMS */
#endif /* PRINT_CMD */

   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME, "PrintDirectory")) != NULL)
      strcpy (outputDir, c_ptr);
   else
      *outputDir = '\0';

   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME, "WhereToPrint")) != NULL)
   {
      if (strcmp (c_ptr, "Printer") == 0)
         whereToPrint = PRINTER;
      else if (strcmp (c_ptr, "EPS") == 0)
         whereToPrint = LATEX_FIG;
      else if (strcmp (c_ptr, "PS") == 0)
         whereToPrint = PS_FILE;
      else if (strcmp (c_ptr, "Bitmap") == 0)
         whereToPrint = XBM_FILE;
      else if (strcmp (c_ptr, "Text") == 0)
         whereToPrint = TEXT_FILE;
      else if (strcmp (c_ptr, "EPSI") == 0)
         whereToPrint = EPSI_FILE;
      else if (strcmp (c_ptr, "GIF") == 0)
         whereToPrint = GIF_FILE;
      else if (strcmp (c_ptr, "HTML") == 0)
         whereToPrint = HTML_FILE;
      else
         fprintf (stderr, "Unrecognized WhereToPrint:  %s\n", c_ptr);
   }

   useGray = FALSE;
   if ((c_ptr = XGetDefault (mainDisplay,TOOL_NAME,"UseGrayScale")) != NULL)
   {
      if (strcmp ("true", c_ptr) == 0 || strcmp ("True", c_ptr) == 0)
         useGray = TRUE;
   }

   autoPan = TRUE;
   if ((c_ptr = XGetDefault (mainDisplay,TOOL_NAME,"AutoPanInEditText")) !=
         NULL)
   {
      if (strcmp ("false", c_ptr) == 0 || strcmp ("False", c_ptr) == 0)
         autoPan = FALSE;
   }

   moveMode = UNCONST_MOVE;
   if ((c_ptr = XGetDefault (mainDisplay,TOOL_NAME,"ConstrainedMove")) != NULL)
   {
      if (strcmp ("true", c_ptr) == 0 || strcmp ("True", c_ptr) == 0)
         moveMode = CONST_MOVE;
   }

   doubleQuoteDoubleQuote = FALSE;
   if ((c_ptr = XGetDefault (mainDisplay,TOOL_NAME,"DoubleQuoteDoubleQuote"))
         != NULL)
   {
      if (strcmp ("true", c_ptr) == 0 || strcmp ("True", c_ptr) == 0)
         doubleQuoteDoubleQuote = TRUE;
   }

   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME, "GridSystem")) != NULL)
   {
      if (strcmp ("English", c_ptr) == 0 || strcmp ("english", c_ptr) == 0)
         gridSystem = ENGLISH_GRID;
      else if (strcmp ("Metric", c_ptr) == 0 || strcmp ("metric", c_ptr) == 0)
         gridSystem = METRIC_GRID;
   }

   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME, "InitialGrid")) != NULL)
   {
      int	grid_size = atoi (c_ptr);

      switch (gridSystem)
      {
         case ENGLISH_GRID:
            if (grid_size < -2 || grid_size > 2)
               fprintf (stderr, "Invalid %s*InitialGrid: '%s', 0 is used.\n",
                     TOOL_NAME, c_ptr);
            else
               xyEnglishGrid = HALF_INCH>>(2-grid_size);
            break;
         case METRIC_GRID:
            if (grid_size < -1 || grid_size > 2)
               fprintf (stderr, "Invalid %s*InitialGrid: '%s', 0 is used.\n",
                     TOOL_NAME, c_ptr);
            else
               switch (grid_size)
               {
                  case -1: xyMetricGrid = ONE_MM; break;
                  case 0: xyMetricGrid = TWO_MM; break;
                  case 1: xyMetricGrid = FIVE_MM; break;
                  case 2: xyMetricGrid = ONE_CM; break;
               }
            break;
      }
   }

   splineTol = 9;
   if ((c_ptr = XGetDefault (mainDisplay,TOOL_NAME,"SplineTolerance")) != NULL)
   {
      splineTol = atoi (c_ptr);
      if (splineTol < 3 || splineTol > 13)
      {
         fprintf (stderr, "Invalid %s*SplineTolerance: '%s', 9 is used.\n",
               TOOL_NAME, c_ptr);
         splineTol = 9;
      }
   }

   splineRubberband = TRUE;
   if ((c_ptr = XGetDefault (mainDisplay,TOOL_NAME,"SplineRubberband")) != NULL)
      if ((strcmp (c_ptr, "false") == 0) || (strcmp (c_ptr, "False") == 0))
         splineRubberband = FALSE;

   saveTmpOnReturn = TRUE;
   if ((c_ptr = XGetDefault (mainDisplay,TOOL_NAME,"SaveTmpOnReturn")) != NULL)
      if ((strcmp (c_ptr, "false") == 0) || (strcmp (c_ptr, "False") == 0))
         saveTmpOnReturn = FALSE;

   dropObsIconAttrWhenUpdate = FALSE;
   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME,
         "DropObsIconAttrWhenUpdate")) != NULL)
   {
      if (strcmp ("true", c_ptr) == 0 || strcmp ("True", c_ptr) == 0)
         dropObsIconAttrWhenUpdate = TRUE;
   }

   useRecentDupDistance = TRUE;
   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME,
         "UseRecentDupDistance")) != NULL)
   {
      if (strcmp ("false", c_ptr) == 0 || strcmp ("False", c_ptr) == 0)
         useRecentDupDistance = FALSE;
   }

   handleSize = 3;
   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME, "HandleSize")) != NULL)
   {
      handleSize = atoi (c_ptr);
      if (handleSize < 2 || handleSize > 6)
      {
         fprintf (stderr, "Invalid %s*HandleSize:  '%s', 3 is used.\n",
               TOOL_NAME, c_ptr);
         handleSize = 3;
      }
   }

   historyDepth = -1;
   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME, "HistoryDepth")) != NULL)
      historyDepth = atoi (c_ptr);

   defaultHistoryDepth = historyDepth;

   canvasWindowOnly = FALSE;
   if (cmdLineCWO)
      canvasWindowOnly = TRUE;
   else if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"CanvasWindowOnly")) !=
         NULL)
   {
      if (strcmp ("true", c_ptr) == 0 || strcmp ("True", c_ptr) == 0)
         canvasWindowOnly = TRUE;
   }

   saveCommentsInSaveNew = TRUE;
   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME,
         "SaveCommentsInSaveNew")) != NULL)
   {
      if (strcmp ("false", c_ptr) == 0 || strcmp ("False", c_ptr) == 0)
         saveCommentsInSaveNew = FALSE;
   }

   usePsAdobeString = FALSE;
   *adobeString = '\0';
   *epsfString = '\0';
   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME,
         "UsePsAdobeString")) != NULL)
   {
      if (strcmp ("true", c_ptr) == 0 || strcmp ("True", c_ptr) == 0)
         usePsAdobeString = TRUE;
      else
      {
         char	tmp_str[80];

         strcpy (tmp_str, c_ptr);
         if (!ParsePsAdobeString(tmp_str, &usePsAdobeString, adobeString,
               epsfString))
         {
            fprintf (stderr, "%s %s*UsePsAdobeString:  '%s', %s.\n",
                  "Invalid", TOOL_NAME, tmp_str, "'false' is used");
            usePsAdobeString = FALSE;
         }
      }
   }

   groupedTextEditable = FALSE;
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "GroupedTextEditable")) !=
         NULL && UtilStrICmp(c_ptr, "true") == 0) {
      groupedTextEditable = TRUE;
   }

   intrCheckInterval = 10;
   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME,
         "IntrCheckInterval")) != NULL)
      intrCheckInterval = atoi (c_ptr);

   tiledPageScaling = 0.9;
   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME,
         "TiledPageScaling")) != NULL)
   {
      tiledPageScaling = (float) atof (c_ptr);
      if (tiledPageScaling <= 0.0 || tiledPageScaling > 1.0)
      {
         fprintf (stderr, "Invalid %s*TiledPageScaling: '%s', %s.\n",
               TOOL_NAME, c_ptr, "0.9 used");
         tiledPageScaling = 0.9;
      }
   }

   stickyMenuSelection = FALSE;
   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME,
         "StickyMenuSelection")) != NULL)
   {
      if (strcmp ("true", c_ptr) == 0 || strcmp ("True", c_ptr) == 0)
         stickyMenuSelection = TRUE;
   }

   gridShown = TRUE;
   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME,
         "InitialShowGrid")) != NULL)
   {
      if (strcmp ("false", c_ptr) == 0 || strcmp ("False", c_ptr) == 0)
         gridShown = FALSE;
   }

   gridOn = TRUE;
   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME, "InitialSnapOn")) != NULL)
   {
      if (strcmp ("false", c_ptr) == 0 || strcmp ("False", c_ptr) == 0)
         gridOn = FALSE;
   }

   noMenubar = FALSE;
   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME, "NoMenubar")) != NULL)
   {
      if (strcmp ("true", c_ptr) == 0 || strcmp ("True", c_ptr) == 0)
         noMenubar = TRUE;
   }

   noStatusWindow = FALSE;
   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME, "NoStatusWindow")) != NULL)
   {
      if (strcmp ("true", c_ptr) == 0 || strcmp ("True", c_ptr) == 0)
         noStatusWindow = TRUE;
   }

   scrollBarW = SCROLLBAR_W;
   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME, "ScrollBarWidth")) != NULL)
   {
      scrollBarW = atoi (c_ptr);
      if (scrollBarW < 2 || scrollBarW > 16)
      {
         fprintf (stderr, "Invalid %s*ScrollBarWidth: '%s', %s, %1d is used.\n",
               TOOL_NAME, c_ptr, "(must be between 2 and 16)", SCROLLBAR_W);
         splineTol = SCROLLBAR_W;
      }
   }

   paper_size_set = FALSE;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"InitialPaperSize")) != NULL)
      paper_size_set = SetPaperSize (c_ptr);

   pageStyle = PORTRAIT;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"PageStyleLandscape")) !=
         NULL && (strcmp ("true", c_ptr) == 0 || strcmp ("True", c_ptr) == 0)) {
      pageStyle = LANDSCAPE;
      paper_size_set = TRUE;
   }

   InitPaperSize ();

   printMag = 100;
   if ((c_ptr = XGetDefault (mainDisplay,TOOL_NAME,"PercentPrintReduction")) !=
         NULL)
   {
      int	print_reduction = atoi (c_ptr);

      if (print_reduction <= 0)
      {
         fprintf (stderr, "Invalid %s*PercentPrintReduction: '%s', %s.\n",
               TOOL_NAME, c_ptr, "100 is used");
         print_reduction = 100;
      }
      if (printMag != print_reduction)
      {
         printMag = print_reduction;
         paper_size_set = TRUE;
      }
   }
   if (paper_size_set) UpdPageStyle (pageStyle);

   usePaperSizeStoredInFile = FALSE;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"UsePaperSizeStoredInFile")) !=
         NULL)
   {
      if (strcmp ("true", c_ptr) == 0 || strcmp ("True", c_ptr) == 0)
         usePaperSizeStoredInFile = TRUE;
   }

   oneMotionSelectMove = FALSE;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"OneMotionSelMove")) != NULL)
   {
      if (strcmp ("true", c_ptr) == 0 || strcmp ("True", c_ptr) == 0)
         oneMotionSelectMove = TRUE;
   }

   queryZoomInPoint = FALSE;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"QueryZoomInPoint")) != NULL) {
      if (strcmp("true",c_ptr)==0 || strcmp("True",c_ptr)==0 ||
            strcmp("always",c_ptr)==0 || strcmp("Always",c_ptr)==0) {
         queryZoomInPoint = TRUE;
      } else if (strcmp("no_select",c_ptr)==0 || strcmp("No_select",c_ptr)==0) {
         queryZoomInPoint = INVALID;
      } else if (strcmp("no_query",c_ptr)==0 || strcmp("No_query",c_ptr)==0) {
         queryZoomInPoint = BAD;
      }
   }

   lineStyle = LS_RIGHT;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"InitialArrowStyle")) != NULL)
   {
      if (strcmp("NONE",c_ptr) == 0) {
         lineStyle = LS_PLAIN;
      } else if (strcmp("RIGHT",c_ptr) == 0) {
         lineStyle = LS_RIGHT;
      } else if (strcmp("LEFT",c_ptr) == 0) {
         lineStyle = LS_LEFT;
      } else if (strcmp("DOUBLE",c_ptr) == 0) {
         lineStyle = LS_DOUBLE;
      } else {
         fprintf (stderr, "Invalid %s*InitialArrowStyle: '%s', %s.\n",
               TOOL_NAME, c_ptr, "RIGHT is used");
      }
   }

   showPageInEPS = FALSE;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"ShowPageInEPS")) !=
         NULL && (strcmp ("true", c_ptr) == 0 || strcmp ("True", c_ptr) == 0))
      showPageInEPS = TRUE;

   oneMotionTimeout = 200;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"OneMotionTimeout")) != NULL) {
      oneMotionTimeout = atoi(c_ptr);
      if (oneMotionTimeout < 0 || oneMotionTimeout >= 1000) {
         fprintf (stderr, "Invalid %s*%s: '%s', %s, %1d is used.\n",
               TOOL_NAME, "OneMotionTimeout", c_ptr,
               "(must be between 0 and 1000)", 200);
         oneMotionTimeout = 200;
      }
   }

   minMoveInterval = 0;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"MinMoveInterval")) != NULL) {
      minMoveInterval = atoi(c_ptr);
      if (minMoveInterval < 0 || minMoveInterval >= 1000) {
         fprintf (stderr, "Invalid %s*%s: '%s', %s, %1d is used.\n",
               TOOL_NAME, "OneMotionTimeout", c_ptr,
               "(must be between 0 and 1000)", 0);
         minMoveInterval = 0;
      }
   }

   rotationIncrement = (45<<6); /* initially 45 degrees */
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"RotationIncrement")) != NULL) {
      float fval;

      if (sscanf(c_ptr, "%f", &fval) != 1 || fval < (float)0.0) {
         fprintf (stderr, "Invalid %s*%s: '%s', 45 is used.\n",
               TOOL_NAME, "RotationIncrement", c_ptr);
      } else {
         while (fval < (float)360.0) fval += (float)360.0;
         while (fval >= (float)360.0) fval -= (float)360.0;
         fval *= 64.0;
         rotationIncrement = round(fval);
      }
   }

   initialMenubarWindowH = MENUBAR_WINDOW_H;

   strcpy (scanFileName, "No File");
   scanLineNum = 0;

   colorDisplay = (cmdLineBW || DisplayPlanes(mainDisplay,mainScreen)==1 ||
         mainVisual->class==StaticGray) ? FALSE : TRUE;

   InitColor ();

   if ((c_ptr = getenv ("TGIFPATH")) == NULL)
   {
      if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME, "TGIFPATH")) != NULL)
         strcpy (drawPath, c_ptr);
      else
         strcpy (drawPath, TGIF_PATH);
   }
   else if (strlen (c_ptr) >= 255)
      strcpy (drawPath, TGIF_PATH);
   else
      strcpy (drawPath, c_ptr);

   if ((c_ptr = getenv ("HOME")) == NULL)
      strcpy (homeDir, "/");
   else if (strlen (c_ptr) >= MAXPATHLENGTH-1)
      strcpy (homeDir, "/");
   else
      strcpy (homeDir, c_ptr);

   if (*bootDir == '\0')
   {
#ifdef VMS
      extern char * getcwd ARGS_DECL((char *, int, int));

      if (getcwd (bootDir, MAXPATHLENGTH+2, 0) == NULL) strcpy (bootDir, ".");
#else
#ifdef ibm
      extern char * getwd ARGS_DECL((char *, int));

      if (getwd (bootDir, MAXPATHLENGTH+2) == NULL) strcpy (bootDir, ".");
#else
#ifdef NeXT
      extern char * getwd ARGS_DECL((char *, int));

      if (getwd (bootDir, MAXPATHLENGTH+2) == NULL) strcpy (bootDir, ".");
#else
#ifdef luna88k
      extern char * getwd ARGS_DECL((char *, int));

      if (getwd (bootDir) == NULL) strcpy (bootDir, ".");
#else
#ifdef sequent
      extern char * getwd ARGS_DECL((char *, int));

      if (getwd (bootDir) == NULL) strcpy (bootDir, ".");
#else
      extern char * getcwd ARGS_DECL((char *, int));

      if (getcwd (bootDir, MAXPATHLENGTH+2) == NULL) strcpy (bootDir, ".");
#endif
#endif
#endif
#endif
#endif
   }

   sizehints.flags = PPosition | PSize | PMinSize;
   sizehints.x = 0;
   sizehints.y = 0;
   sizehints.width = mainWinW;
   sizehints.height = mainWinH;

   if ((c_ptr = XGetDefault (mainDisplay, TOOL_NAME, "Geometry")) != NULL)
   {
      bitmask = XParseGeometry (c_ptr, &(sizehints.x), &(sizehints.y),
            (unsigned int *)&(sizehints.width),
            (unsigned int *)&(sizehints.height));
      if (bitmask & (XValue | YValue)) sizehints.flags |= USPosition;
      if (bitmask & (WidthValue | HeightValue))
      {
         sizehints.flags |= USSize;
         drawWinW = sizehints.width;
         drawWinH = sizehints.height;
      }
      if (bitmask & XNegative)
      {
         x_neg_in_def = TRUE;
         def_x_neg = sizehints.x;
      }
      if (bitmask & YNegative)
      {
         y_neg_in_def = TRUE;
         def_y_neg = sizehints.y;
      }
   }

   if (geometrySpecified && *geometrySpec != '\0')
   {
      bitmask = XParseGeometry (geometrySpec, &(sizehints.x), &(sizehints.y),
            (unsigned int *)&(sizehints.width),
            (unsigned int *)&(sizehints.height));
      if (bitmask & (XValue | YValue)) sizehints.flags |= USPosition;
      if (bitmask & (WidthValue | HeightValue))
      {
         sizehints.flags |= USSize;
         drawWinW = sizehints.width;
         drawWinH = sizehints.height;
      }
      if (bitmask & XValue) x_in_cmdline = TRUE;
      if (bitmask & YValue) y_in_cmdline = TRUE;
   }

   InitFonts ();
   initialMenubarWindowH = defaultFontHeight;
   if (msgFontPtr == NULL) {
      statusWindowH = max(defaultFontHeight+(brdrW<<1)+4, btn1_height+2);
   } else {
      statusWindowH = max(msgFontHeight+(brdrW<<1)+4, btn1_height+2);
   }
   InitWinSizes ();

   if (canvasWindowOnly)
   {
      noMenubar = TRUE;
      noStatusWindow = TRUE;

      mainWinW = initDrawWinW;
      mainWinH = initDrawWinH;

      switch (gridSystem)
      {
         case ENGLISH_GRID:
            sizehints.min_width = HALF_INCH+3*brdrW;
            sizehints.min_height = HALF_INCH+3*brdrW;
            break;
         case METRIC_GRID:
            sizehints.min_width = ONE_CM+3*brdrW;
            sizehints.min_height = ONE_CM+3*brdrW;
            break;
      }
      if (mainWinW > sizehints.min_width)
         sizehints.width = mainWinW;
      else
         sizehints.width = sizehints.min_width = mainWinW;
      if (mainWinH > sizehints.min_height)
         sizehints.height = mainWinH;
      else
         sizehints.height = sizehints.min_height = mainWinH;

      drawWinW = initDrawWinW = mainWinW-(brdrW<<1);
      drawWinH = initDrawWinH = mainWinH-(brdrW<<1);
      if (msgWindowW <= 0) msgWindowW = 2;
   }
   else
   {
      sizehints.min_width = choiceWindowW+(brdrW<<2)+defaultFontWidth;
      sizehints.min_height =
            (noMenubar ? 0 : menubarWindowH+(brdrW<<1))+
            (noStatusWindow ? 0 : statusWindowH+(brdrW<<1))+
            titleWindowH+choiceWindowH+PIX_PER_INCH+rulerW+scrollBarW+10*brdrW;
      if (mainWinW > sizehints.min_width)
         sizehints.width = mainWinW;
      else
         mainWinW = sizehints.width = sizehints.min_width;
      if (mainWinH > sizehints.min_height)
         sizehints.height = mainWinH;
      else
         mainWinH = sizehints.height = sizehints.min_height;

      InverseInitWinSizes ();
   }

   if (bitmask & XNegative)
      sizehints.x += DisplayWidth(mainDisplay,mainScreen)-mainWinW-1;
   else if (!x_in_cmdline && x_neg_in_def)
      sizehints.x = def_x_neg+DisplayWidth(mainDisplay,mainScreen)-mainWinW-1;

   if (bitmask & YNegative)
      sizehints.y += DisplayHeight(mainDisplay,mainScreen)-mainWinH-1;
   else if (!y_in_cmdline && y_neg_in_def)
      sizehints.y = def_y_neg+DisplayHeight(mainDisplay,mainScreen)-mainWinH-1;

   if ((mainWindow = XCreateSimpleWindow (mainDisplay, rootWindow,
         sizehints.x, sizehints.y, sizehints.width, sizehints.height,
         brdrW, myBorderPixel, myBgPixel)) == 0)
   { fprintf (stderr, "Could not create main window!\n"); exit(1); }

   if (newColormapUsed)
      XSetWindowColormap (mainDisplay, mainWindow, mainColormap);

   InitShortCut ();
   InitScroll ();
   InitPattern ();
   InitRuler ();
   InitMenu ();
   InitNames ();
   InitStk ();
   InitXBm ();
   InitXPm ();
   CreateCursor ();
   lastPageNum = 1;
   InitPage ();

   if ((titleWindow = XCreateSimpleWindow (mainDisplay, mainWindow, 0, 0,
         titleWindowW, titleWindowH, brdrW, myBorderPixel, myBgPixel)) == 0)
      Error ("setup", "Could not create the title window");
   cur_y += titleWindowH + (brdrW<<1);

   if ((menubarWindow = XCreateSimpleWindow (mainDisplay, mainWindow, 0,
         cur_y, menubarWindowW, menubarWindowH, brdrW, myBorderPixel,
         myBgPixel)) == 0)
      Error ("setup", "Could not create the menubar window");

   if (!noMenubar)
      cur_y += menubarWindowH + (brdrW<<1);

   if ((msgWindow = XCreateSimpleWindow (mainDisplay, mainWindow, 0, cur_y,
         msgWindowW, msgWindowH, brdrW, myBorderPixel, myBgPixel)) == 0)
      Error ("setup", "Could not create the message window");

   if ((choiceWindow = XCreateSimpleWindow (mainDisplay, mainWindow,
         msgWindowW+(brdrW<<1), cur_y, choiceWindowW, choiceWindowH,
         brdrW, myBorderPixel, myBgPixel)) == 0)
      Error ("setup", "Could not create the choice window");
   cur_y += msgWindowH + (brdrW<<1);

   InitChoice ();

   if ((dummyWindow1 = XCreateSimpleWindow (mainDisplay, mainWindow, 0, cur_y,
         rulerW, rulerW, brdrW, myBorderPixel, myBgPixel)) == 0) 
      Error ("setup", "Could not create the left top dummy window");

   if ((hRuleWindow = XCreateSimpleWindow (mainDisplay, mainWindow,
         rulerW+(brdrW<<1), cur_y, drawWinW, rulerW,
         brdrW, myBorderPixel, myBgPixel)) == 0)
      Error ("setup", "Could not create the horizontal ruler window");

   if ((colorWindow = XCreateSimpleWindow (mainDisplay, mainWindow,
         (brdrW<<2)+rulerW+drawWinW, cur_y, scrollBarW, colorWindowH,
         brdrW, myBorderPixel, myBgPixel)) == 0)
      Error ("setup", "Could not create the color window");

   if ((colorDummyWindow = XCreateSimpleWindow (mainDisplay, mainWindow,
         (brdrW<<2)+rulerW+drawWinW, cur_y+colorWindowH+(brdrW<<1), scrollBarW,
         colorDummyWindowH, brdrW, myBorderPixel, myBgPixel)) == 0)
      Error ("setup", "Could not create the color dummy window");

   if ((vSBarWindow = XCreateSimpleWindow (mainDisplay, mainWindow,
         (brdrW<<2)+rulerW+drawWinW,
         colorLayers ? cur_y+colorWindowH+colorDummyWindowH+(brdrW<<2) : cur_y,
         scrollBarW, vSBarH, brdrW, myBorderPixel, myBgPixel)) == 0) 
      Error ("setup", "Could not create the vertical scrollbar window");
   cur_y += rulerW + (brdrW<<1);

   if ((vRuleWindow = XCreateSimpleWindow (mainDisplay, mainWindow, 0, cur_y,
         rulerW, drawWinH, brdrW, myBorderPixel, myBgPixel)) == 0)
      Error ("setup", "Could not create the vertical ruler window");

   if ((drawWindow = XCreateSimpleWindow (mainDisplay, mainWindow,
         rulerW+(brdrW<<1), cur_y, drawWinW, drawWinH,
         0, myBorderPixel, myBgPixel)) == 0) 
      Error ("setup", "Could not create the drawing window");
   cur_y += drawWinH + (brdrW<<1);

   if ((pageWindow = XCreateSimpleWindow (mainDisplay, mainWindow, 0, cur_y,
         pageWindowW, scrollBarW+(brdrW<<1), 0, myBorderPixel, myBgPixel)) == 0)
      Error ("setup", "Could not create the page window");

   if ((pageDummyWindow = XCreateSimpleWindow (mainDisplay, mainWindow,
         pageWindowW, cur_y, pageDummyWindowW, scrollBarW, brdrW,
         myBorderPixel, myBgPixel)) == 0) 
      Error ("setup", "Could not create the page dummy window");

   if ((hSBarWindow = XCreateSimpleWindow (mainDisplay, mainWindow,
         pageWindowW+pageDummyWindowW+(brdrW<<1), cur_y,
         hSBarW, scrollBarW, brdrW, myBorderPixel, myBgPixel)) == 0) 
      Error ("setup", "Could not create the horizontal scrollbar window");

   if ((dummyWindow2 = XCreateSimpleWindow (mainDisplay, mainWindow,
         rulerW+drawWinW+(brdrW<<2), cur_y,
         scrollBarW, scrollBarW, brdrW, myBorderPixel, myBgPixel)) == 0) 
      Error ("setup", "Could not create the right bottom dummy window");
   cur_y += scrollBarW + (brdrW<<1);

   if ((statusWindow = XCreateSimpleWindow (mainDisplay, mainWindow, 0,
         cur_y, statusWindowW, statusWindowH, brdrW, myBorderPixel,
         myBgPixel)) == 0)
      Error ("setup", "Could not create the status window");
   CalcStatusSubWinGeom ();
   for (i=0; i < MAX_STATUS_BTNS; i++)
      if ((statusSubWindow[i] = XCreateSimpleWindow (mainDisplay,
            statusWindow, statusSubWindowX[i], statusSubWindowY[i],
            statusSubWindowW[i], statusSubWindowH[i], brdrW,
            myBorderPixel, myBgPixel)) == 0)
         Error ("setup", "Could not create the status sub windows");

   if (!noStatusWindow)
      cur_y += statusWindowH + (brdrW<<1);

   if (canvasWindowOnly)
   {
      savedMainWindow = mainWindow;
      savedDrawWindow = drawWindow;

      if ((mainWindow = XCreateSimpleWindow (mainDisplay, rootWindow,
            sizehints.x, sizehints.y, mainWinW, mainWinH,
            brdrW, myBorderPixel, myBgPixel)) == 0)
      { fprintf (stderr, "Could not create main window!\n"); exit(1); }

      if (newColormapUsed)
         XSetWindowColormap (mainDisplay, mainWindow, mainColormap);

      if ((drawWindow = XCreateSimpleWindow (mainDisplay, mainWindow,
            0, 0, drawWinW, drawWinH, brdrW, myBorderPixel, myBgPixel)) == 0) 
      { fprintf (stderr, "Could not create draw window!\n"); exit(1); }
   }

#ifdef NOTR4MODE
   XSetNormalHints (mainDisplay, mainWindow, &sizehints);
#else
   XSetWMNormalHints (mainDisplay, mainWindow, &sizehints);
#endif
   XStoreName (mainDisplay, mainWindow, TOOL_NAME);

   UpdDrawWinBBox ();
   InitTitle ();
   InitStatus ();
   SetCanvasFont ();

#ifdef MAPBEFORESELECT
   XMapWindow (mainDisplay, mainWindow);
   XSelectInput (mainDisplay, mainWindow, KeyPressMask | StructureNotifyMask
         | VisibilityChangeMask);
#else
   XSelectInput (mainDisplay, mainWindow, KeyPressMask | StructureNotifyMask
         | VisibilityChangeMask);
   XMapWindow (mainDisplay, mainWindow);
#endif
   XDefineCursor (mainDisplay, mainWindow, defaultCursor);

   protocolAtom = XInternAtom (mainDisplay, "WM_DELETE_WINDOW", False);
#ifndef NOTR4MODE
   XSetWMProtocols (mainDisplay, mainWindow, &protocolAtom, 1);
#endif

#ifdef MAPBEFORESELECT
   if (canvasWindowOnly)
   {
      XMapWindow (mainDisplay, drawWindow); 
      XSelectInput (mainDisplay, drawWindow, ButtonReleaseMask |
            ButtonPressMask | PointerMotionMask | KeyPressMask |
            KeyReleaseMask | ExposureMask | EnterWindowMask);
   }
   else
   {
      XMapWindow (mainDisplay, titleWindow);
      XSelectInput (mainDisplay, titleWindow, ExposureMask | EnterWindowMask);

      if (menubarWindow != None)
      {
         XMapWindow (mainDisplay, menubarWindow);
         XSelectInput (mainDisplay, menubarWindow, ExposureMask |
               EnterWindowMask | ButtonPressMask | PointerMotionMask);
      }

      XMapWindow (mainDisplay, msgWindow);
      XSelectInput (mainDisplay, msgWindow, ButtonPressMask |
            ButtonReleaseMask | PointerMotionMask | ExposureMask |
            EnterWindowMask);

      XMapWindow (mainDisplay, choiceWindow);
      XSelectInput (mainDisplay, choiceWindow,
            ButtonReleaseMask | ButtonPressMask | ExposureMask |
            PointerMotionMask | EnterWindowMask);

      XMapWindow (mainDisplay, hRuleWindow);
      XSelectInput (mainDisplay, hRuleWindow,
            ButtonPressMask | ExposureMask | EnterWindowMask);
      XMapWindow (mainDisplay, vRuleWindow);
      XSelectInput (mainDisplay, vRuleWindow,
            ButtonPressMask | ExposureMask | EnterWindowMask);

      XMapWindow (mainDisplay, drawWindow); 
      XSelectInput (mainDisplay, drawWindow, ButtonReleaseMask |
            ButtonPressMask | PointerMotionMask | KeyPressMask |
            KeyReleaseMask | ExposureMask | EnterWindowMask);

      if (colorLayers) {
         XMapWindow(mainDisplay, colorWindow);
         XSelectInput(mainDisplay, colorWindow, ButtonPressMask |
               PointerMotionMask | ExposureMask | EnterWindowMask);
         XMapWindow(mainDisplay, colorDummyWindow);
         XSelectInput(mainDisplay, colorDummyWindow, ButtonPressMask |
               ExposureMask | EnterWindowMask);
      } else {
         XSelectInput(mainDisplay, colorWindow, ButtonPressMask |
               PointerMotionMask | ExposureMask | EnterWindowMask);
         XSelectInput(mainDisplay, colorDummyWindow, ButtonPressMask |
               ExposureMask | EnterWindowMask);
      }
      XMapWindow (mainDisplay, vSBarWindow);
      XSelectInput (mainDisplay, vSBarWindow, ButtonPressMask | ExposureMask |
            EnterWindowMask);

      XMapWindow (mainDisplay, pageWindow);
      XSelectInput (mainDisplay, pageWindow, ButtonPressMask |
            PointerMotionMask | ExposureMask | EnterWindowMask);
      XMapWindow (mainDisplay, pageDummyWindow);
      XSelectInput (mainDisplay, pageDummyWindow, ButtonPressMask |
            ExposureMask | EnterWindowMask);
      XMapWindow (mainDisplay, hSBarWindow);
      XSelectInput (mainDisplay, hSBarWindow, ButtonPressMask | ExposureMask |
            EnterWindowMask);
      XMapWindow (mainDisplay, dummyWindow1);
      XSelectInput (mainDisplay, dummyWindow1, ButtonPressMask | ExposureMask |
            EnterWindowMask);
      XMapWindow (mainDisplay, dummyWindow2);
      XSelectInput (mainDisplay, dummyWindow2, EnterWindowMask);

      if (!noStatusWindow)
      {
         XMapWindow (mainDisplay, statusWindow);
         XSelectInput (mainDisplay, statusWindow, ExposureMask |
               EnterWindowMask);
         for (i=0; i < MAX_STATUS_BTNS; i++)
            XMapWindow (mainDisplay, statusSubWindow[i]);
      }
      else
         XSelectInput (mainDisplay, statusWindow, ExposureMask |
               EnterWindowMask);
   }
#else /* ~MAPBEFORESELECT */
   if (canvasWindowOnly)
   {
      XSelectInput (mainDisplay, drawWindow, ButtonReleaseMask |
            ButtonPressMask | PointerMotionMask | KeyPressMask |
            KeyReleaseMask | ExposureMask | EnterWindowMask);
      XMapWindow (mainDisplay, drawWindow); 
   }
   else
   {
      XSelectInput (mainDisplay, titleWindow, ExposureMask | EnterWindowMask);
      XMapWindow (mainDisplay, titleWindow);

      if (menubarWindow != None)
      {
         XSelectInput (mainDisplay, menubarWindow, ExposureMask |
               EnterWindowMask | ButtonPressMask | PointerMotionMask);
         XMapWindow (mainDisplay, menubarWindow);
      }

      XSelectInput (mainDisplay, msgWindow, ButtonPressMask |
            ButtonReleaseMask | PointerMotionMask | ExposureMask |
            EnterWindowMask);
      XMapWindow (mainDisplay, msgWindow);

      XSelectInput (mainDisplay, choiceWindow,
            ButtonReleaseMask | ButtonPressMask | ExposureMask |
            PointerMotionMask | EnterWindowMask);
      XMapWindow (mainDisplay, choiceWindow);

      XSelectInput (mainDisplay, hRuleWindow,
            ButtonPressMask | ExposureMask | EnterWindowMask);
      XMapWindow (mainDisplay, hRuleWindow);
      XSelectInput (mainDisplay, vRuleWindow,
            ButtonPressMask | ExposureMask | EnterWindowMask);
      XMapWindow (mainDisplay, vRuleWindow);

      XSelectInput (mainDisplay, drawWindow, ButtonReleaseMask |
            ButtonPressMask | PointerMotionMask | KeyPressMask |
            KeyReleaseMask | ExposureMask | EnterWindowMask);
      XMapWindow (mainDisplay, drawWindow); 

      if (colorLayers) {
         XSelectInput(mainDisplay, colorWindow, ButtonPressMask |
               PointerMotionMask | ExposureMask | EnterWindowMask);
         XMapWindow(mainDisplay, colorWindow);
         XSelectInput(mainDisplay, colorDummyWindow, ButtonPressMask |
               ExposureMask | EnterWindowMask);
         XMapWindow(mainDisplay, colorDummyWindow);
      } else {
         XSelectInput(mainDisplay, colorWindow, ButtonPressMask |
               PointerMotionMask | ExposureMask | EnterWindowMask);
         XSelectInput(mainDisplay, colorDummyWindow, ButtonPressMask |
               ExposureMask | EnterWindowMask);
      }
      XSelectInput (mainDisplay, vSBarWindow, ButtonPressMask | ExposureMask |
            EnterWindowMask);
      XMapWindow (mainDisplay, vSBarWindow);

      XSelectInput (mainDisplay, pageWindow, ButtonPressMask |
            PointerMotionMask | ExposureMask | EnterWindowMask);
      XMapWindow (mainDisplay, pageWindow);
      XSelectInput (mainDisplay, pageDummyWindow, ButtonPressMask |
            ExposureMask | EnterWindowMask);
      XMapWindow (mainDisplay, pageDummyWindow);
      XSelectInput (mainDisplay, hSBarWindow, ButtonPressMask | ExposureMask |
            EnterWindowMask);
      XMapWindow (mainDisplay, hSBarWindow);
      XSelectInput (mainDisplay, dummyWindow1, ButtonPressMask | ExposureMask |
            EnterWindowMask);
      XMapWindow (mainDisplay, dummyWindow1);
      XSelectInput (mainDisplay, dummyWindow2, EnterWindowMask);
      XMapWindow (mainDisplay, dummyWindow2);

      if (!noStatusWindow)
      {
         XSelectInput (mainDisplay, statusWindow, ExposureMask |
               EnterWindowMask);
         XMapWindow (mainDisplay, statusWindow);
         for (i=0; i < MAX_STATUS_BTNS; i++)
            XMapWindow (mainDisplay, statusSubWindow[i]);
      }
      else
         XSelectInput (mainDisplay, statusWindow, ExposureMask |
               EnterWindowMask);
   }
#endif

   wmhints.flags = InputHint;
   wmhints.input = True;
   wmhints.icon_pixmap = None;

   if (iconWindowCreated)
   {
      unsigned int	dummy_icon_w, dummy_icon_h;

      sizehints.flags = PSize | PMinSize | PMaxSize;
      sizehints.width = sizehints.min_width = sizehints.max_width =
            iconWindowW+(brdrW<<1);
      sizehints.height = sizehints.min_height = sizehints.max_height =
            iconWindowH+(brdrW<<1);

      wmhints.flags |= IconWindowHint;
      wmhints.icon_window = iconBaseWindow;

      if ((c_ptr = XGetDefault (mainDisplay,TOOL_NAME,"IconGeometry")) != NULL)
      {
         bitmask = XParseGeometry (c_ptr, &(sizehints.x), &(sizehints.y),
               &dummy_icon_w, &dummy_icon_h);
         if ((bitmask & XValue) && (bitmask & YValue))
         {
            if (bitmask & XNegative) sizehints.x += DisplayWidth (mainDisplay,
                  mainScreen) - iconWindowW - (brdrW<<1) - 1;
            if (bitmask & YNegative) sizehints.y += DisplayHeight (mainDisplay,
                  mainScreen) - iconWindowH - (brdrW<<1) - 1;

            sizehints.flags |= USPosition;

            wmhints.flags |= IconPositionHint;
            wmhints.icon_x = sizehints.x;
            wmhints.icon_y = sizehints.y;
         }
         else
            fprintf (stderr, "%s %s*IconGeometry:  %s.  Ignored!\n",
                  "Invalid spec -- ", TOOL_NAME, c_ptr);
      }
#ifdef NOTR4MODE
      XSetNormalHints (mainDisplay, iconBaseWindow, &sizehints);
#else
      XSetWMNormalHints (mainDisplay, iconBaseWindow, &sizehints);
#endif
   }
   else
   {
      if (((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"UseWMIconPixmap")) != NULL)
            && (strcmp(c_ptr,"True") == 0 || strcmp(c_ptr,"true") == 0))
      {
         wmhints.initial_state = NormalState;
         wmhints.icon_pixmap = XCreateBitmapFromData (mainDisplay, mainWindow,
               tgificon_bits, tgificon_width, tgificon_height);
         wmhints.flags |= IconPixmapHint | StateHint;
      }
   }
   XSetWMHints (mainDisplay, mainWindow, &wmhints);
   if ((wmhints.flags & IconPixmapHint) && wmhints.icon_pixmap != None) {
      XFreePixmap(mainDisplay, wmhints.icon_pixmap);
   }

   wmhints.flags = InputHint;
   wmhints.input = True;
   XSetWMHints (mainDisplay, drawWindow, &wmhints);

   InitText ();
   InitRemote ();
   InitImageProc ();
   InitImport ();
   InitHelp ();
   InitShape ();
}

void CleanUpResiduals ()
{
   if (savedMainWindow != None) XDestroyWindow (mainDisplay, savedMainWindow);
   CleanUpPaperSize ();
}

int TieLooseEnds ()
   /* returns TRUE if something got created */
   /* returns FALSE otherwise */
{
   if (curChoice == DRAWTEXT) return (CreateTextObj ());
   return (FALSE);
}

void MakeQuiescent ()
{
   TieLooseEnds ();
   SetCurChoice (NOTHING);
   if (topSel != NULL) { HighLightReverse (); RemoveAllSel (); }
}

void SetFileModified (modified)
   int	modified;
{
   if (modified != fileModified)
   {
      fileModified = modified;
      RedrawTitleWindow ();
   }
}
