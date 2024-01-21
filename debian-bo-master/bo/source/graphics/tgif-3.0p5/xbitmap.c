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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/xbitmap.c,v 3.2 1996/05/12 08:08:26 william Exp $";
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
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
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#include "eps.e"
#include "file.e"
#include "font.e"
#include "grid.e"
#include "imgproc.e"
#include "mark.e"
#include "menu.e"
#include "msg.e"
#include "names.e"
#include "obj.e"
#include "page.e"
#include "pattern.e"
#include "ps.e"
#include "raster.e"
#include "rect.e"
#include "select.e"
#include "setup.e"
#include "util.e"
#ifndef _NO_EXTERN
#include "xbitmap.e"
#endif
#include "xpixmap.e"

#ifndef _NO_EXTERN
#ifdef _AIX
extern void	srand ARGS_DECL((unsigned int));
#else /* !_AIX */
#ifdef __alpha
extern void	srand ARGS_DECL((unsigned int));
#else /* !__alpha */
#ifdef sgi
extern void	srand ARGS_DECL((unsigned int));
#else
extern int	srand ARGS_DECL((int));
#endif /* sgi */
#endif /* __alpha */
#endif /* _AIX */
extern int	unlink ARGS_DECL((char *));
#endif /* !_NO_EXTERN */

extern int	rand ARGS_DECL((void));

#ifdef __hpux
extern double	atof ARGS_DECL((const char *));
#else /* !__hpux */
#if __STDC__ || defined(__cplusplus) || defined(c_plusplus)
extern double	atof ARGS_DECL((const char *));
#else
extern double	atof ARGS_DECL((char *));
#endif /* __STDC__ || defined(__cplusplus) || defined(c_plusplus) */
#endif /* __hpux */

extern int	atoi ARGS_DECL((char *));

GC	xbmGC = NULL;

int	askForXBmSpec = FALSE;
int	stripEPSComments = TRUE;
int	saveEPSLines = FALSE;

int leftExportPixelTrim=0;
int topExportPixelTrim=0;
int rightExportPixelTrim=0;
int bottomExportPixelTrim=0;

Pixmap	dummyBitmap;

static char	hexValue[] = "0123456789abcdef";

static int	importXBmRV = FALSE;
static int	numColorsToDump = 0;
static int	charsPerPixel = 1;
static int	* pixelValue = NULL;
static int	* colorIndexToDumpIndex = NULL;
static char	* colorChar = NULL;
static char	* * colorStr = NULL;

static int	xpmOutputVersion = 1;
static int	xpmInXGrabSCFormat = FALSE;

static int	halfToneBitmap = FALSE;
static int	thresholdBitmap = FALSE;
static float	bitmapThreshold = 0.5;
static char	bitmapThresholdStr[MAXSTRING+1];

#define IMF_FORMAT_NCSA 0
#define IMF_FORMAT_CERN 1
#define IMF_FORMAT_SPYG 2

static int	generateImageMap=INVALID;
static char	xpmToGifCmd[MAXSTRING+1];
static char	imageMapFileExtension[MAXSTRING+1];
static char	htmlFileExtension[MAXSTRING+1];
static char	gifFileExtension[MAXSTRING+1];
static int	imageMapFileFormat=IMF_FORMAT_NCSA;
static int	useXPmVersion1ForImageMap=TRUE;
static int	generateHtmlHref=TRUE;

typedef struct LineRec {
   char			* s;
   struct LineRec	* next, * prev;
} * LineRecPtr;

static
void ExpandTmpStorage ()
{
   /* the size before are all maxColors+2 */
   pixelValue = (int*)realloc(pixelValue, (maxColors+3)*sizeof(int));
   colorIndexToDumpIndex = (int*)realloc(colorIndexToDumpIndex,
         (maxColors+3)*sizeof(int));
   if (maxColors > 20) {
      charsPerPixel = 2;
      colorChar = (char*)realloc(colorChar, ((maxColors<<1)+4)*sizeof(char));
   } else {
      charsPerPixel = 1;
      colorChar = (char*)realloc(colorChar, (maxColors+3)*sizeof(char));
   }
   colorStr = (char**)realloc(colorStr, (maxColors+3)*sizeof(char*));
   if (colorStr == NULL) FailAllocMessage();
   colorStr[maxColors+2] = NULL;
   colorIndexToDumpIndex[maxColors+2] = INVALID;
}

static
void ParseExportPixelTrim(pszSpec, nInit)
   char *pszSpec;
   int nInit;
{
   char	*pszLeft, *pszTop=NULL, *pszRight=NULL, *pszBottom=NULL;
   int left, top, right, bottom;

   pszLeft = strtok(pszSpec, " ,\t");
   if (pszLeft == NULL) return;

   pszTop = strtok(NULL, " ,\t");
   if (pszTop != NULL) {
      pszRight = strtok(NULL, " ,\t");
      if (pszRight != NULL) {
         pszBottom = strtok(NULL, " ,\t");
      }
   }
   left = (pszLeft==NULL ? 0 : atoi(pszLeft));
   top = (pszTop==NULL ? 0 : atoi(pszTop));
   right = (pszRight==NULL ? 0 : atoi(pszRight));
   bottom = (pszBottom==NULL ? 0 : atoi(pszBottom));
   if (left < 0 || top < 0 || right < 0 || bottom < 0) {
      sprintf(gszMsgBox, "%s:  %s.\n\n[%1d,%1d,%1d,%1d] used.",
            "Error in parsing ExportPixelTrim values",
            "negative values are not allowed", leftExportPixelTrim,
            topExportPixelTrim, rightExportPixelTrim, bottomExportPixelTrim);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   } else {
      leftExportPixelTrim = left;
      topExportPixelTrim = top;
      rightExportPixelTrim = right;
      bottomExportPixelTrim = bottom;
      sprintf(gszMsgBox, "%s [%1d,%1d,%1d,%1d].",
            "ExportPixelTrim values set to", leftExportPixelTrim,
            topExportPixelTrim, rightExportPixelTrim, bottomExportPixelTrim);
      if (!nInit) Msg(gszMsgBox);
   }
}

void SetExportPixelTrim(pszError)
   char *pszError;
{
   char	szSpec[MAXSTRING+1], szCurrent[MAXSTRING+1];

   *szSpec = '\0';
   sprintf(gszMsgBox, "%s %s: [current: %1d,%1d,%1d,%1d]",
         "Please specify the number of pixels to trim when exporting",
         "(or printing)", leftExportPixelTrim, topExportPixelTrim,
         rightExportPixelTrim, bottomExportPixelTrim);
   if (pszError) {
      sprintf(szCurrent, "( %s %s )",
            "Enter 4 numbers or type <CR> or <ESC> to use the current values",
            pszError);
   } else {
      sprintf(szCurrent, "( %s )",
            "Enter 4 numbers or type <CR> or <ESC> to use the current values");
   }
   Dialog(gszMsgBox, szCurrent, szSpec);
   ParseExportPixelTrim(szSpec, FALSE);
}

void InitXBm ()
{
   XGCValues	values;
   char		* c_ptr;

   dummyBitmap = XCreatePixmap (mainDisplay, mainWindow, 1, 1, 1);

   values.foreground = 0;
   values.background = 0;
   values.function = GXcopy;
   values.fill_style = FillSolid;
   xbmGC = XCreateGC (mainDisplay, dummyBitmap,
         GCForeground | GCBackground | GCFunction | GCFillStyle, &values);

   importXBmRV = FALSE;
   if ((c_ptr = XGetDefault(mainDisplay,TOOL_NAME,"XBmReverseVideo")) != NULL)
      if (strcmp ("True", c_ptr) == 0 || strcmp ("true", c_ptr) == 0)
         importXBmRV = TRUE;

   askForXBmSpec = FALSE;
   if ((c_ptr = XGetDefault(mainDisplay,TOOL_NAME,"AskForXBmSpec")) != NULL)
      if (strcmp ("True", c_ptr) == 0 || strcmp ("true", c_ptr) == 0)
         askForXBmSpec = TRUE;

   stripEPSComments = TRUE;
   if ((c_ptr = XGetDefault(mainDisplay,TOOL_NAME,"StripEPSComments")) != NULL)
      if (strcmp ("False", c_ptr) == 0 || strcmp ("false", c_ptr) == 0)
         stripEPSComments = FALSE;

   xpmOutputVersion = 1;
   if ((c_ptr = XGetDefault(mainDisplay,TOOL_NAME,"XPmOutputVersion")) != NULL)
   {
      xpmOutputVersion = atoi (c_ptr);
      if (xpmOutputVersion != 1 && xpmOutputVersion != 3)
      {
         fprintf (stderr, "Invalid XPmOutputVersion (%s) in X Defaults.  %s\n",
               c_ptr, "1 is used.");
         xpmOutputVersion = 1;
      }
   }

   xpmInXGrabSCFormat = FALSE;
   if ((c_ptr = XGetDefault(mainDisplay,TOOL_NAME,"XPmInXGrabSCFormat"))!=NULL)
      if (strcmp ("True", c_ptr) == 0 || strcmp ("true", c_ptr) == 0)
         xpmInXGrabSCFormat = TRUE;

   halfToneBitmap = FALSE;
   if ((c_ptr = XGetDefault(mainDisplay,TOOL_NAME,"HalfToneBitmap"))!=NULL)
      if (strcmp ("True", c_ptr) == 0 || strcmp ("true", c_ptr) == 0)
         halfToneBitmap = TRUE;

   thresholdBitmap = FALSE;
   if ((c_ptr = XGetDefault(mainDisplay,TOOL_NAME,"ThresholdBitmap"))!=NULL)
      if (!halfToneBitmap && (strcmp ("True", c_ptr) == 0 ||
            strcmp ("true", c_ptr) == 0))
         thresholdBitmap = TRUE;

   bitmapThreshold = (halfToneBitmap) ? 0.5 : 1.0;
   strcpy (bitmapThresholdStr, ((halfToneBitmap) ? "0.5" : "1.0"));
   if ((c_ptr = XGetDefault(mainDisplay,TOOL_NAME,"BitmapThreshold"))!=NULL)
   {
      strcpy (bitmapThresholdStr, c_ptr);
      bitmapThreshold = (float) atof (c_ptr);
      if (bitmapThreshold < 0 || bitmapThreshold > 1)
      {
         fprintf (stderr, "%s (%s) in X Defaults.  %s is used.\n",
               "Invalid BitmapThreshold", c_ptr,
               ((halfToneBitmap) ? "0.5" : "1.0"));
         bitmapThreshold = (halfToneBitmap) ? 0.5 : 1.0;
         strcpy (bitmapThresholdStr, ((halfToneBitmap) ? "0.5" : "1.0"));
      }
   }

   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"InitExportPixelTrim")) !=
         NULL) {
      ParseExportPixelTrim(c_ptr, TRUE);
   }
   InitEPS ();
}

static int transparentIndex=INVALID;

static
void FreeCachedStrings()
{
   register int	i;

   if (colorChar != NULL) {
      for (i = 0; i < numColorsToDump+3; i++) {
         if (colorStr[i] != NULL) {
            free(colorStr[i]);
         } else {
            break;
         }
      }
      free(colorStr);
      free(colorChar);
      free(pixelValue);
      free(colorIndexToDumpIndex);
   }
   colorStr = NULL;
   colorChar = NULL;
   pixelValue = colorIndexToDumpIndex = NULL;
   transparentIndex = INVALID;
}

void CleanUpXBm ()
{
   FreeCachedStrings();
   CleanUpEPS ();

   if (xbmGC != NULL) XFreeGC (mainDisplay, xbmGC);
   XFreePixmap (mainDisplay, dummyBitmap);

   importXBmRV = FALSE;
   askForXBmSpec = FALSE;
}

static
void MultiplyTwoByTwo (m1, m2)
   float	m1[2][2], m2[2][2];
{
   float	m3[2][2];

   m3[0][0] = m1[0][0]*m2[0][0] + m1[0][1]*m2[1][0];
   m3[0][1] = m1[0][0]*m2[0][1] + m1[0][1]*m2[1][1];
   m3[1][0] = m1[1][0]*m2[0][0] + m1[1][1]*m2[1][0];
   m3[1][1] = m1[1][0]*m2[0][1] + m1[1][1]*m2[1][1];

   m1[0][0]=m3[0][0]; m1[0][1]=m3[0][1]; m1[1][0]=m3[1][0]; m1[1][1]=m3[1][1];
}

void CalcTransform (mtrx)
   register struct MtrxRec	* mtrx;
{
   float	m1[2][2], m2[2][2], tmp_val;

   switch (mtrx->rotate)
   {
      case ROTATE0:
      case ROTATE180:
         mtrx->h_scale = mtrx->w / mtrx->image_w;
         mtrx->v_scale = mtrx->h / mtrx->image_h;
         break;
      case ROTATE90:
      case ROTATE270:
         mtrx->h_scale = mtrx->h / mtrx->image_w;
         mtrx->v_scale = mtrx->w / mtrx->image_h;
         break;
   }

   m1[0][0]=1.0; m1[0][1]=0.0; m1[1][0]=0.0; m1[1][1]=1.0;
   if (mtrx->flip & HORI_EVEN)
   {
      m2[0][0]=(-1.0); m2[0][1]=0.0; m2[1][0]=0.0; m2[1][1]=1.0;
      MultiplyTwoByTwo (m1, m2);
   }
   if (mtrx->flip & VERT_EVEN)
   {
      m2[0][0]=1.0; m2[0][1]=0.0; m2[1][0]=0.0; m2[1][1]=(-1.0);
      MultiplyTwoByTwo (m1, m2);
   }
   switch (mtrx->rotate)
   {
      case ROTATE0:
         if (mtrx->flip & (HORI_ODD | VERT_ODD))
         {
            m2[0][0]=rotatedCosine[ROTATE90]; m2[0][1]=rotatedSine[ROTATE90];
            m2[1][0]=(-rotatedSine[ROTATE90]); m2[1][1]=rotatedCosine[ROTATE90];
            MultiplyTwoByTwo (m1, m2);
            if (mtrx->flip & HORI_ODD)
            {
               m2[0][0]=(-1.0); m2[0][1]=0.0; m2[1][0]=0.0; m2[1][1]=1.0;
               MultiplyTwoByTwo (m1, m2);
            }
            if (mtrx->flip & VERT_ODD)
            {
               m2[0][0]=1.0; m2[0][1]=0.0; m2[1][0]=0.0; m2[1][1]=(-1.0);
               MultiplyTwoByTwo (m1, m2);
            }
            m2[0][0]=rotatedCosine[ROTATE270];
            m2[0][1]=rotatedSine[ROTATE270];
            m2[1][0]=(-rotatedSine[ROTATE270]);
            m2[1][1]=rotatedCosine[ROTATE270];
            MultiplyTwoByTwo (m1, m2);
         }
         break;
      case ROTATE90:
         m2[0][0]=rotatedCosine[ROTATE90]; m2[0][1]=rotatedSine[ROTATE90];
         m2[1][0]=(-rotatedSine[ROTATE90]); m2[1][1]=rotatedCosine[ROTATE90];
         MultiplyTwoByTwo (m1, m2);
         if (mtrx->flip & HORI_ODD)
         {
            m2[0][0]=(-1.0); m2[0][1]=0.0; m2[1][0]=0.0; m2[1][1]=1.0;
            MultiplyTwoByTwo (m1, m2);
         }
         if (mtrx->flip & VERT_ODD)
         {
            m2[0][0]=1.0; m2[0][1]=0.0; m2[1][0]=0.0; m2[1][1]=(-1.0);
            MultiplyTwoByTwo (m1, m2);
         }
         break;
      case ROTATE180:
         m2[0][0]=rotatedCosine[ROTATE90]; m2[0][1]=rotatedSine[ROTATE90];
         m2[1][0]=(-rotatedSine[ROTATE90]); m2[1][1]=rotatedCosine[ROTATE90];
         MultiplyTwoByTwo (m1, m2);
         if (mtrx->flip & HORI_ODD)
         {
            m2[0][0]=(-1.0); m2[0][1]=0.0; m2[1][0]=0.0; m2[1][1]=1.0;
            MultiplyTwoByTwo (m1, m2);
         }
         if (mtrx->flip & VERT_ODD)
         {
            m2[0][0]=1.0; m2[0][1]=0.0; m2[1][0]=0.0; m2[1][1]=(-1.0);
            MultiplyTwoByTwo (m1, m2);
         }
         m2[0][0]=rotatedCosine[ROTATE90]; m2[0][1]=rotatedSine[ROTATE90];
         m2[1][0]=(-rotatedSine[ROTATE90]); m2[1][1]=rotatedCosine[ROTATE90];
         MultiplyTwoByTwo (m1, m2);
         break;
      case ROTATE270:
         m2[0][0]=rotatedCosine[ROTATE270]; m2[0][1]=rotatedSine[ROTATE270];
         m2[1][0]=(-rotatedSine[ROTATE270]); m2[1][1]=rotatedCosine[ROTATE270];
         MultiplyTwoByTwo (m1, m2);
         if (mtrx->flip & HORI_ODD)
         {
            m2[0][0]=(-1.0); m2[0][1]=0.0; m2[1][0]=0.0; m2[1][1]=1.0;
            MultiplyTwoByTwo (m1, m2);
         }
         if (mtrx->flip & VERT_ODD)
         {
            m2[0][0]=1.0; m2[0][1]=0.0; m2[1][0]=0.0; m2[1][1]=(-1.0);
            MultiplyTwoByTwo (m1, m2);
         }
         break;
   }
   m2[0][0]=mtrx->h_scale; m2[0][1]=0.0; m2[1][0]=0.0; m2[1][1]=mtrx->v_scale;
   MultiplyTwoByTwo (m2, m1);

   mtrx->m[0][0]=m2[0][0]; mtrx->m[0][1]=m2[0][1];
   mtrx->m[1][0]=m2[1][0]; mtrx->m[1][1]=m2[1][1];
   tmp_val = m2[0][0]*m2[1][1] - m2[0][1]*m2[1][0];
   if (fabs((double) tmp_val) < 1.0e-6)
      tmp_val = ((float)1000000);
   else
      tmp_val = ((float)1.0)/tmp_val;
   mtrx->rev_m[0][0] = tmp_val*m2[1][1];
   mtrx->rev_m[0][1] = (-tmp_val)*m2[0][1];
   mtrx->rev_m[1][0] = (-tmp_val)*m2[1][0];
   mtrx->rev_m[1][1] = tmp_val*m2[0][0];

   mtrx->transformed_w = mtrx->image_w*m2[0][0] + mtrx->image_h*m2[1][0];
   mtrx->transformed_h = mtrx->image_w*m2[0][1] + mtrx->image_h*m2[1][1];

   switch (mtrx->rotate)
   {
      case ROTATE0:
         mtrx->dump_h_scale=mtrx->m[0][0]; mtrx->dump_v_scale=mtrx->m[1][1];
         mtrx->degree=0;
         break;
      case ROTATE90:
         mtrx->dump_h_scale=mtrx->m[1][0]; mtrx->dump_v_scale=(-mtrx->m[0][1]);
         mtrx->degree=(-90);
         break;
      case ROTATE180:
         mtrx->dump_h_scale=(-mtrx->m[0][0]);
         mtrx->dump_v_scale=(-mtrx->m[1][1]);
         mtrx->degree=(-180);
         break;
      case ROTATE270:
         mtrx->dump_h_scale=(-mtrx->m[1][0]); mtrx->dump_v_scale=mtrx->m[0][1];
         mtrx->degree=(-270);
         break;
   }
}

void MakeCachedBitmap (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   register int		r, c;
   int			w, h, rotate, flip, target_percent;
   int			num_cols, num_rows, image_w, image_h, watch_cursor;
   int			start_col, start_row, do_msg;
   struct XBmRec	* xbm_ptr = ObjPtr->detail.xbm;
   struct MtrxRec	mtrx;
   Pixmap		dest_bitmap;
   XImage		* src_image, * dest_image;

   if (xbm_ptr->real_type==XBM_EPS && xbm_ptr->bitmap==None) return;

   w = ObjPtr->obbox.rbx - ObjPtr->obbox.ltx;
   h = ObjPtr->obbox.rby - ObjPtr->obbox.lty;
   num_cols = (zoomedIn) ? (w<<zoomScale) : (w>>zoomScale);
   num_rows = (zoomedIn) ? (h<<zoomScale) : (h>>zoomScale);

   if (ObjPtr->ctm==NULL && xbm_ptr->cached_bitmap!=None &&
         xbm_ptr->cached_zoomed==zoomedIn && xbm_ptr->cached_zoom==zoomScale &&
         xbm_ptr->cached_w==num_cols && xbm_ptr->cached_h==num_rows &&
         xbm_ptr->cached_rotate==xbm_ptr->rotate &&
         xbm_ptr->cached_flip==xbm_ptr->flip)
      return;

   if ((w>>zoomScale)==0 || (h>>zoomScale)==0)
   {
      if (xbm_ptr->cached_bitmap != None)
         XFreePixmap (mainDisplay, xbm_ptr->cached_bitmap);
      xbm_ptr->cached_bitmap = None;
      return;
   }

   watch_cursor = watchCursorOnMainWindow;
   if (!watch_cursor)
   {
      SetWatchCursor (drawWindow);
      SetWatchCursor (mainWindow);
   }

   src_image = xbm_ptr->image;
   flip = xbm_ptr->flip;
   rotate = xbm_ptr->rotate;
   image_w = xbm_ptr->image_w;
   image_h = xbm_ptr->image_h;
   if (xbm_ptr->cached_bitmap != None)
      XFreePixmap (mainDisplay, xbm_ptr->cached_bitmap);
   xbm_ptr->cached_bitmap = None;

   if (src_image == NULL)
      src_image = xbm_ptr->image = XGetImage (mainDisplay, xbm_ptr->bitmap,
            0, 0, image_w, image_h, 1, ZPixmap);

   do_msg = ((num_rows*num_cols)>=0x4000);
   if (do_msg) {
      SaveStatusStrings();
      SetStringStatus("Caching bitmap...");
      XSync(mainDisplay, False);
   }

   dest_bitmap = XCreatePixmap (mainDisplay,dummyBitmap,num_cols,num_rows,1);
   XFillRectangle (mainDisplay,dest_bitmap,xbmGC,0,0,num_cols,num_rows);
   dest_image = XGetImage (mainDisplay, dest_bitmap, 0, 0, num_cols,
         num_rows, 1, ZPixmap);

   if (ObjPtr->ctm == NULL)
   {
      mtrx.image_w = (float)image_w; mtrx.image_h = (float)image_h;
      mtrx.w = (float)num_cols; mtrx.h = (float)num_rows;
      mtrx.rotate = rotate; mtrx.flip = flip;

      CalcTransform (&mtrx);

      start_col = (mtrx.transformed_w >= 0.0) ? 0 : (-num_cols)+1;
      start_row = (mtrx.transformed_h >= 0.0) ? 0 : (-num_rows)+1;

      target_percent = 5;
      for (r = 0; r < num_rows; r++)
      {
         float	part_x, part_y;

         if (do_msg && ((r & 0xf) == 0)) {
            int	percent=(r*10000/num_rows)/100;

            if (percent >= target_percent) {
               sprintf(gszMsgBox, "Progress: %1d%%", percent);
               SetStringStatus(gszMsgBox);
               XSync(mainDisplay, False);
               while (target_percent <= percent) target_percent += 5;
            }
         }
         part_x = (r+start_row)*mtrx.rev_m[1][0];
         part_y = (r+start_row)*mtrx.rev_m[1][1];
         for (c = 0; c < num_cols; c++)
         {
            int	x, y;

            x = (int)((c+start_col)*mtrx.rev_m[0][0]+part_x);
            y = (int)((c+start_col)*mtrx.rev_m[0][1]+part_y);
            if (x>=0 && x<image_w && y>=0 && y<image_h &&
                  XGetPixel(src_image,x,y)==1)
               XPutPixel (dest_image, c, r, 1);
         }
      }
   }
   else
   {
      int	abs_offset_x=ObjPtr->obbox.ltx-ObjPtr->x;
      int	abs_offset_y=ObjPtr->obbox.lty-ObjPtr->y;

      target_percent = 5;
      for (r = 0; r < num_rows; r++)
      {
         int	y=abs_offset_y+ABS_SIZE(r);

         if (do_msg && ((r & 0xf) == 0)) {
            int	percent=(r*10000/num_rows)/100;

            if (percent >= target_percent) {
               sprintf(gszMsgBox, "Progress: %1d%%", percent);
               SetStringStatus(gszMsgBox);
               XSync(mainDisplay, False);
               while (target_percent <= percent) target_percent += 5;
            }
         }
         for (c = 0; c < num_cols; c++)
         {
            int	x=abs_offset_x+ABS_SIZE(c);
            int	new_x, new_y;

            ReverseTransformPointThroughCTM (x, y, ObjPtr->ctm, &new_x, &new_y);
            new_x += ObjPtr->x-ObjPtr->orig_obbox.ltx;
            new_y += ObjPtr->y-ObjPtr->orig_obbox.lty;
            if (new_x>=0 && new_x<image_w && new_y>=0 && new_y<image_h &&
                  XGetPixel(src_image,new_x,new_y)==1)
               XPutPixel (dest_image, c, r, 1);
         }
      }
      memcpy (&xbm_ptr->cached_ctm, ObjPtr->ctm, sizeof(struct XfrmMtrxRec));
   }
   if (do_msg) {
      SetStringStatus("Finishing caching bitmap...");
      XSync(mainDisplay, False);
   }
   XPutImage (mainDisplay, dest_bitmap, xbmGC, dest_image, 0, 0, 0, 0,
         num_cols, num_rows);
   if (do_msg) RestoreStatusStrings ();

   xbm_ptr->cached_bitmap = dest_bitmap;
   xbm_ptr->cached_zoomed = zoomedIn;
   xbm_ptr->cached_zoom = zoomScale;
   xbm_ptr->cached_rotate = xbm_ptr->rotate;
   xbm_ptr->cached_flip = xbm_ptr->flip;
   xbm_ptr->cached_w = num_cols;
   xbm_ptr->cached_h = num_rows;

   XDestroyImage (dest_image);

   if (!watch_cursor)
   {
      SetDefaultCursor (mainWindow);
      ShowCursor ();
   }
}

int ExtractBitmap(OrigBitmap, OrigImage, X, Y, W, H, Bitmap, Image)
   Pixmap OrigBitmap, *Bitmap;
   XImage *OrigImage, **Image;
   int X, Y, W, H;
{
   register int j, i;
   XImage *src_image;

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);

   if ((*Bitmap=XCreatePixmap(mainDisplay,mainWindow,W,H,1)) == None) {
      sprintf(gszMsgBox, "Can not allocate bitmap of size %1dx%1d.", W, H);
      Msg(gszMsgBox);
      SetDefaultCursor(mainWindow);
      SetDefaultCursor(drawWindow);
      return FALSE;
   }

   if ((*Image=XGetImage(mainDisplay, *Bitmap, 0, 0, W, H, 1,
         ZPixmap)) == NULL) {
      Msg("XGetImage() failed!  May have run out of memory!");
      XFreePixmap(mainDisplay, *Bitmap); *Bitmap = None;
      SetDefaultCursor(mainWindow);
      SetDefaultCursor(drawWindow);
      return FALSE;
   }
   if (OrigImage != NULL && X == 0 && Y == 0) {
      src_image = OrigImage;
   } else {
      if ((src_image=XGetImage(mainDisplay, OrigBitmap, X, Y, W, H, 1,
            ZPixmap)) == NULL) {
         Msg("XGetImage() failed!  May have run out of memory!");
         XFreePixmap(mainDisplay, *Bitmap);
         *Bitmap = None;
         XDestroyImage(*Image); *Image = NULL;
         SetDefaultCursor(mainWindow);
         SetDefaultCursor(drawWindow);
         return FALSE;
      }
   }
   for (i = 0; i < H; i++) {
      for (j = 0; j < W; j++) {
         XPutPixel(*Image, j, i, XGetPixel(src_image, j, i));
      }
   }
   XPutImage(mainDisplay, *Bitmap, xbmGC, *Image, 0, 0, 0, 0, W, H);
   SetDefaultCursor(mainWindow);
   SetDefaultCursor(drawWindow);

   if (!(OrigImage != NULL && X == 0 && Y == 0)) XDestroyImage(src_image);
   return TRUE;
}

static
void InvertXBmObject(ObjPtr)
   struct ObjRec *ObjPtr;
{
   int i, j, image_w, image_h, pixel;
   Pixmap bitmap;
   XImage *image;

   SetWatchCursor(drawWindow);
   SetWatchCursor(mainWindow);

   bitmap = ObjPtr->detail.xbm->bitmap;

   image_w = ObjPtr->detail.xbm->image_w;
   image_h = ObjPtr->detail.xbm->image_h;
   if (ObjPtr->detail.xbm->image == NULL) {
      if ((image=ObjPtr->detail.xbm->image = XGetImage(mainDisplay, bitmap,
            0, 0, image_w, image_h, 1, ZPixmap)) == NULL) {
         fprintf(stderr, "XGetImage() failed!  May have run out of memory!\n");
         fflush(stderr);
         SetDefaultCursor(mainWindow);
         SetDefaultCursor(drawWindow);
         return;
      }
   } else {
      image = ObjPtr->detail.xbm->image;
   }
   for (i = 0; i < image_h; i++) {
      for (j = 0; j < image_w; j++) {
         pixel = XGetPixel(image, j, i);
         XPutPixel(image, j, i, ((pixel==1) ? 0 : 1));
      }
   }
   XPutImage(mainDisplay, bitmap, xbmGC, image, 0, 0, 0, 0, image_w, image_h);

   if (ObjPtr->detail.xbm->cached_bitmap != None) {
      XFreePixmap(mainDisplay, ObjPtr->detail.xbm->cached_bitmap);
   }
   ObjPtr->detail.xbm->cached_bitmap = None;
   if (zoomScale != 0) {
      ObjPtr->detail.xbm->cached_zoom = 0;
   } else {
      ObjPtr->detail.xbm->cached_rotate = INVALID;
   }
   SetDefaultCursor(mainWindow);
   SetDefaultCursor(drawWindow);
}

static
int ObjListInvertable(LastObjPtr)
   struct ObjRec *LastObjPtr;
{
   struct ObjRec *obj_ptr;

   for (obj_ptr=LastObjPtr; obj_ptr != NULL; obj_ptr=obj_ptr->prev) {
      switch (obj_ptr->type) {
      case OBJ_XBM: return (TRUE);

      case OBJ_ICON:
      case OBJ_SYM:
      case OBJ_GROUP:
         if (ObjListInvertable(obj_ptr->detail.r->last)) return TRUE;
         break;
      }
   }
   return FALSE;
}

static
void InvertObjListXBitmap(LastObjPtr)
   struct ObjRec *LastObjPtr;
{
   struct ObjRec *obj_ptr;

   for (obj_ptr=LastObjPtr; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
      switch (obj_ptr->type) {
      case OBJ_XBM: InvertXBmObject(obj_ptr); break;

      case OBJ_ICON:
      case OBJ_SYM:
      case OBJ_GROUP:
         InvertObjListXBitmap(obj_ptr->detail.r->last);
         break;
      }
   }
}

void InvertXBitmaps()
{
   struct SelRec *sel_ptr;
   int changed=FALSE;

   if (topSel == NULL) return;

   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      switch (sel_ptr->obj->type) {
      case OBJ_XBM:
         changed = TRUE;
         PrepareToReplaceAnObj(sel_ptr->obj);
         InvertXBmObject(sel_ptr->obj);
         RecordReplaceAnObj(sel_ptr->obj);
         break;

      case OBJ_ICON:
      case OBJ_SYM:
      case OBJ_GROUP:
         if (ObjListInvertable(sel_ptr->obj->detail.r->last)) {
            changed = TRUE;
            PrepareToReplaceAnObj(sel_ptr->obj);
            InvertObjListXBitmap(sel_ptr->obj->detail.r->last);
            RecordReplaceAnObj(sel_ptr->obj);
         }
         break;
      }
   }
   EndCompositeCmd();

   if (changed) {
      SetFileModified(TRUE);
      HighLightReverse();
      RedrawAnArea(botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
            selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
      HighLightForward();
   } else {
      sprintf(gszMsgBox, "No X Bitmap objects selected.");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   }
}

static
void ParseGeomSpec (geom_spec, image_w, image_h, src_x, src_y, src_w, src_h)
   char		* geom_spec;
   int		image_w, image_h, * src_x, * src_y, * src_w, * src_h;
{
   int		bitmask;
   XSizeHints	sizehints;

   *src_x = *src_y = 0;
   *src_w = image_w;
   *src_h = image_h;

   bitmask = XParseGeometry (geom_spec, &(sizehints.x), &(sizehints.y),
         (unsigned int *)&(sizehints.width),
         (unsigned int *)&(sizehints.height));
   if (bitmask & WidthValue) *src_w = sizehints.width;
   if (bitmask & HeightValue) *src_h = sizehints.height;
   if (bitmask & XValue)
   {
      *src_x = (bitmask & XNegative) ? image_w+sizehints.x : sizehints.x;
      if (bitmask & XNegative)
      {
         if (*src_w > image_w) *src_w = image_w;
         if (*src_x >= image_w || *src_x < 0) *src_x = image_w;
         if (*src_x-*src_w < 0)
         {
            *src_w = *src_x;
            *src_x = 0;
         }
         else
            *src_x -= *src_w;
      }
      else
      {
         if (*src_w > image_w) *src_w = image_w;
         if (*src_x >= image_w || *src_x < 0) *src_x = 0;
         if (*src_x+*src_w > image_w) *src_w = image_w-*src_x;
      }
   }
   if (bitmask & YValue)
   {
      *src_y = (bitmask & YNegative) ? image_h+sizehints.y : sizehints.y;
      if (bitmask & YNegative)
      {
         if (*src_h > image_h) *src_h = image_h;
         if (*src_y >= image_h || *src_y < 0) *src_y = image_h;
         if (*src_y-*src_h < 0)
         {
            *src_h = *src_y;
            *src_y = 0;
         }
         else
            *src_y -= *src_h;
      }
      else
      {
         if (*src_h > image_h) *src_h = image_h;
         if (*src_y >= image_h || *src_y < 0) *src_y = 0;
         if (*src_y+*src_h > image_h) *src_h = image_h-*src_y;
      }
   }
   if ((bitmask&WidthValue) && *src_x+*src_w>image_w) *src_w = image_w-*src_x;
   if ((bitmask&HeightValue) && *src_y+*src_h>image_h) *src_h = image_h-*src_y;
}

static
char * FindEqual(s)
   register char	* s;
{
   while (*s != '=' && *s != '\0') s++;
   return ((*s == '=') ? (s) : (char *)NULL);
}

void ParseCutSpec (spec, image_w, image_h, mag, src_x, src_y, src_w, src_h)
   char		* spec;
   int		image_w, image_h, * src_x, * src_y, * src_w, * src_h;
   float	* mag;
{
   char	* geom_spec;

   *mag = 1.0;

   if ((geom_spec = FindEqual (spec)) == NULL)
      ParseGeomSpec (spec,image_w,image_h,src_x,src_y,src_w,src_h);
   else
   {
      *geom_spec = '\0';
      geom_spec++;
      ParseGeomSpec (geom_spec,image_w,image_h,src_x,src_y,src_w,src_h);

      sscanf (spec, "%f", mag);
      if (*mag <= 0.0) *mag = 1.0;
   }
}

void CutXBitmap ()
{
   int			w, h;
   int			ltx, lty, rbx, rby, new_w, new_h;
   int			src_x, src_y, src_w, src_h, image_w, image_h;
   char			mag_spec[MAXSTRING];
   float		h_scale=1.0, v_scale=1.0, mag;
   Pixmap		dest_bitmap=None;
   XImage		* dest_image=NULL;
   struct ObjRec	* obj_ptr = topSel->obj, * new_obj_ptr;
   struct XBmRec	* new_xbm_ptr;

   if (obj_ptr->detail.xbm->real_type==XBM_EPS)
   {
      MsgBox("Can not cut an EPS object.", TOOL_NAME, INFO_MB);
      return;
   }

   src_x = 0;
   src_y = 0;
   src_w = image_w = obj_ptr->detail.xbm->image_w;
   src_h = image_h = obj_ptr->detail.xbm->image_h;
   mag = 1.0;

   switch (obj_ptr->detail.xbm->rotate)
   {
      case ROTATE0:
      case ROTATE180:
         h_scale = ((float)((float)(obj_ptr->obbox.rbx-obj_ptr->obbox.ltx)) /
               ((float)image_w));
         v_scale = ((float)((float)(obj_ptr->obbox.rby-obj_ptr->obbox.lty)) /
               ((float)image_h));
         break;
      case ROTATE90:
      case ROTATE270:
         h_scale = ((float)((float)(obj_ptr->obbox.rby-obj_ptr->obbox.lty)) /
               ((float)image_w));
         v_scale = ((float)((float)(obj_ptr->obbox.rbx-obj_ptr->obbox.ltx)) /
               ((float)image_h));
         break;
   }

   sprintf (gszMsgBox, "%s: [[MAG=]WxH+X+Y] (original size is %1dx%1d)",
         "Please enter geometry spec", image_w, image_h);
   Dialog (gszMsgBox, "( <CR>: accept, <ESC>: cancel )", mag_spec);
   if (*mag_spec == '\0') return;

   ParseCutSpec (mag_spec,image_w,image_h,&mag,&src_x,&src_y,&src_w,&src_h);
   if (src_x==0 && src_y==0 && src_w==image_w && src_h==image_h && mag==1.0)
      return;

   if (src_w==0 || src_h==0)
   {
      Msg ("Bitmap can not have 0 width or height.");
      return;
   }

   PrepareToRecord (CMD_REPLACE, topSel, botSel, numObjSelected);
   if (!ExtractBitmap (obj_ptr->detail.xbm->bitmap,
         obj_ptr->detail.xbm->image, src_x, src_y, src_w, src_h,
         &dest_bitmap, &dest_image))
   {
      AbortPrepareCmd (CMD_REPLACE);
      return;
   }

   sprintf (gszMsgBox, "New bitmap size is %1dx%1d.", src_w, src_h);
   Msg (gszMsgBox);

   UnlinkObj (obj_ptr);

   ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
   HighLightReverse ();

   w = new_w = (int)(((float)src_w) * mag);
   h = new_h = (int)(((float)src_h) * mag);

   new_obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (new_obj_ptr == NULL) FailAllocMessage();
   memset(new_obj_ptr, 0, sizeof(struct ObjRec));
   DupObjBasics (obj_ptr, new_obj_ptr);

   new_xbm_ptr = (struct XBmRec *)malloc(sizeof(struct XBmRec));
   if (new_xbm_ptr == NULL) FailAllocMessage();
   memset(new_xbm_ptr, 0, sizeof(struct XBmRec));
   new_obj_ptr->detail.xbm = new_xbm_ptr;

   new_xbm_ptr->image = dest_image;
   new_xbm_ptr->image_w = src_w;
   new_xbm_ptr->image_h = src_h;
   new_xbm_ptr->bitmap = dest_bitmap;
   new_xbm_ptr->data = NULL;
   new_xbm_ptr->fill = obj_ptr->detail.xbm->fill;
   new_xbm_ptr->rotate = obj_ptr->detail.xbm->rotate;
   new_xbm_ptr->flip = obj_ptr->detail.xbm->flip;
   new_xbm_ptr->cached_zoom = 0;
   new_xbm_ptr->cached_bitmap = None;
   new_xbm_ptr->cached_rotate = INVALID;
   new_xbm_ptr->cached_flip = 0;
   new_xbm_ptr->cached_w = 0;
   new_xbm_ptr->cached_h = 0;

   switch (obj_ptr->detail.xbm->rotate)
   {
      case ROTATE0:
      case ROTATE180:
         new_w = round(h_scale * ((float)w));
         new_h = round(v_scale * ((float)h));
         break;
      case ROTATE90:
      case ROTATE270:
         new_h = round(h_scale * ((float)w));
         new_w = round(v_scale * ((float)h));
         break;
   }

   new_obj_ptr->obbox.ltx = obj_ptr->obbox.ltx;
   new_obj_ptr->obbox.lty = obj_ptr->obbox.lty;
   new_obj_ptr->obbox.rbx = new_obj_ptr->bbox.rbx = obj_ptr->obbox.ltx+new_w;
   new_obj_ptr->obbox.rby = new_obj_ptr->bbox.rby = obj_ptr->obbox.lty+new_h;

   AdjObjBBox (new_obj_ptr);

   topSel->obj = botSel->obj = new_obj_ptr;
   AddObj (NULL, topObj, new_obj_ptr);
   RecordCmd (CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
   FreeObj (obj_ptr);

   UpdSelBBox ();
   RedrawAreas (botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward ();
   SetFileModified (TRUE);
   justDupped = FALSE;
}

static
void CopyXBmProperties(DestXbmObj, SrcXbmObj)
   struct ObjRec *DestXbmObj, *SrcXbmObj;
{
   struct XBmRec *dest_xbm_ptr=DestXbmObj->detail.xbm;
   struct XBmRec *src_xbm_ptr=SrcXbmObj->detail.xbm;

   dest_xbm_ptr->fill = src_xbm_ptr->fill;
   dest_xbm_ptr->rotate = src_xbm_ptr->rotate;
   DestXbmObj->color = SrcXbmObj->color;
}

static
int FinishBreakUpXBitmap(obj_ptr, cols_and_rows, cols, rows)
   struct ObjRec *obj_ptr;
   int cols_and_rows, cols, rows;
{
   struct XBmRec *xbm_ptr=obj_ptr->detail.xbm;
   int y, image_w=xbm_ptr->image_w, image_h=xbm_ptr->image_h;
   int chunk_w=0, chunk_h=0, num_cols=0, num_rows=0, total_chunks=0;
   int orig_x=obj_ptr->x, orig_y=obj_ptr->y;

   if (cols_and_rows) {
      chunk_w = (int)(image_w / cols);
      chunk_h = (int)(image_h / rows);
   } else {
      chunk_w = cols;
      chunk_h = rows;
   }
   for (y=0; y < image_h; y += chunk_h) {
      int h=min(image_h-y,chunk_h), x;

      for (x=0; x < image_w; x += chunk_w) {
         int w=min(image_w-x,chunk_w);
         struct ObjRec *new_obj_ptr;
         Pixmap dest_bitmap=None;
         XImage *dest_image=NULL;

         if (w <= 0 || h <= 0 ||
               !ExtractBitmap(xbm_ptr->bitmap, xbm_ptr->image, x, y, w, h,
               &dest_bitmap, &dest_image)) {
            continue;
         }
         total_chunks++;
         new_obj_ptr = CreateXBmObj(w, h, w, h, dest_bitmap, dest_image);
         CopyXBmProperties(new_obj_ptr, obj_ptr);
         AdjObjBBox(new_obj_ptr);
         MoveObj(new_obj_ptr, orig_x+x, orig_y+y);
         AddObj(NULL, topObj, new_obj_ptr);
      }
   }
   if (total_chunks > 0) {
      int i;

      RemoveAllSel();
      UnlinkObj(obj_ptr);
      FreeObj(obj_ptr);

      for (i=0, obj_ptr=topObj; obj_ptr != NULL && i < total_chunks; i++,
            obj_ptr=obj_ptr->next) {
         struct SelRec *sel_ptr=(struct SelRec *)malloc(sizeof(struct SelRec));

         if (sel_ptr == NULL) {
            FailAllocMessage();
            return FALSE;
         }
         sel_ptr->obj = obj_ptr;
         if (botSel != NULL) {
            botSel->next = sel_ptr;
         } else {
            topSel = sel_ptr;
         }
         sel_ptr->prev = botSel;
         sel_ptr->next = NULL;
         botSel = sel_ptr;
      }
   }
   return (total_chunks > 0);
}

void BreakUpXBitmap(obj_ptr, cols_and_rows, cols, rows)
   struct ObjRec *obj_ptr;
   int cols_and_rows, cols, rows;
{
   struct XBmRec *xbm_ptr=obj_ptr->detail.xbm;

   if (xbm_ptr->real_type == XBM_EPS) {
      MsgBox("Cannot break up an EPS object.", TOOL_NAME, INFO_MB);
      return;
   }
   HighLightReverse ();
   PrepareToReplaceAnObj(obj_ptr);
   if (FinishBreakUpXBitmap(obj_ptr, cols_and_rows, cols, rows)) {
      UpdSelBBox();
      RecordCmd(CMD_ONE_TO_MANY, NULL, topSel, botSel, numObjSelected);
      SetFileModified(TRUE);
      justDupped = FALSE;
   } else {
      AbortPrepareCmd(CMD_REPLACE);
   }
   HighLightForward();
}

static
void BuildObjXPmColors (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   register int			i, color_index;
   register struct ObjRec	* obj_ptr;
   register struct AttrRec	* attr_ptr;
   struct XPmRec		* xpm_ptr;
   int				start_index, new_alloc;

   switch (ObjPtr->type)
   {
      case OBJ_POLY:
      case OBJ_BOX:
      case OBJ_OVAL:
      case OBJ_TEXT:
      case OBJ_POLYGON:
      case OBJ_ARC:
      case OBJ_RCBOX:
      case OBJ_XBM:
         color_index = ObjPtr->color;
         if (colorIndexToDumpIndex[color_index] == INVALID)
         {
            if (colorStr[numColorsToDump] != NULL) {
               free(colorStr[numColorsToDump]);
            }
            pixelValue[numColorsToDump] = colorPixels[color_index];
            colorIndexToDumpIndex[color_index] = numColorsToDump;
            colorStr[numColorsToDump] = (char*)malloc(
                  (strlen(colorMenuItems[color_index])+1)*sizeof(char));
            if (colorStr[numColorsToDump] == NULL) FailAllocMessage();
            strcpy(colorStr[numColorsToDump], colorMenuItems[color_index]);
            numColorsToDump++;
         }
         break;

      case OBJ_XPM:
         xpm_ptr = ObjPtr->detail.xpm;
         start_index = (xpm_ptr->first_pixel_is_bg) ? 1 : 0;
         for (i = start_index; i < xpm_ptr->ncolors; i++) {
            if (UtilStrICmp(xpm_ptr->color_str[i],
                  "None") == 0) {
               if (transparentIndex == (-1)) {
                  if (colorStr[numColorsToDump] != NULL) {
                     free(colorStr[numColorsToDump]);
                  }
                  pixelValue[numColorsToDump] = (-1);
                  colorStr[numColorsToDump] = UtilStrDup("None");
                  transparentIndex = numColorsToDump++;
               }
            } else if ((color_index =
                  QuickFindColorIndex(NULL, xpm_ptr->color_str[i], &new_alloc,
                  TRUE)) != INVALID) {
               if (new_alloc) ExpandTmpStorage();
               if (colorIndexToDumpIndex[color_index] == INVALID) {
                  if (colorStr[numColorsToDump] != NULL) {
                     free(colorStr[numColorsToDump]);
                  }
                  pixelValue[numColorsToDump] = colorPixels[color_index];
                  colorIndexToDumpIndex[color_index] = numColorsToDump;
                  colorStr[numColorsToDump] = (char*)malloc(
                        (strlen(colorMenuItems[color_index])+1)*sizeof(char));
                  if (colorStr[numColorsToDump] == NULL) FailAllocMessage();
                  strcpy(colorStr[numColorsToDump],
                        colorMenuItems[color_index]);
                  numColorsToDump++;
               }
            }
         }
         if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
            RedrawColorWindow();
         }
         break;

      case OBJ_GROUP:
      case OBJ_SYM:
      case OBJ_ICON:
         for (obj_ptr = ObjPtr->detail.r->first; obj_ptr != NULL;
               obj_ptr = obj_ptr->next)
            BuildObjXPmColors (obj_ptr);
         break;
   }
   for (attr_ptr=ObjPtr->fattr; attr_ptr!=NULL; attr_ptr=attr_ptr->next)
   {
      color_index = attr_ptr->obj->color;
      if (colorIndexToDumpIndex[color_index] == INVALID)
      {
         if (colorStr[numColorsToDump] != NULL) {
            free(colorStr[numColorsToDump]);
         }
         pixelValue[numColorsToDump] = colorPixels[color_index];
         colorIndexToDumpIndex[color_index] = numColorsToDump;
         colorStr[numColorsToDump] = (char *) malloc(
               (strlen(colorMenuItems[color_index])+1)*sizeof(char));
         if (colorStr[numColorsToDump] == NULL) FailAllocMessage();
         strcpy (colorStr[numColorsToDump], colorMenuItems[color_index]);
         numColorsToDump++;
      }
   }
}

static
void BuildXPmColors ()
{
   register int			i;
   register struct ObjRec	* obj_ptr;

   FreeCachedStrings();
   if (colorChar == NULL)
   {
      pixelValue = (int*)malloc((maxColors+3)*sizeof(int));
      if (pixelValue == NULL) FailAllocMessage();
      colorIndexToDumpIndex = (int*)malloc((maxColors+3)*sizeof(int));
      if (colorIndexToDumpIndex == NULL) FailAllocMessage();
      if (maxColors > 20) {
         charsPerPixel = 2;
         colorChar = (char*)malloc(((maxColors<<1)+6)*sizeof(char));
      } else {
         charsPerPixel = 1;
         colorChar = (char*)malloc((maxColors+3)*sizeof(char));
      }
      if (colorChar == NULL) FailAllocMessage();
      colorStr = (char**)malloc((maxColors+3)*sizeof(char*));
      if (colorStr == NULL) FailAllocMessage();

      for (i = 0; i < maxColors+3; i++) colorStr[i] = NULL;
   }

   for (i = 0; i < maxColors; i++) colorIndexToDumpIndex[i] = INVALID;

   pixelValue[0] = myBgPixel;
   colorStr[0] = (char*)malloc((strlen(myBgColorStr)+1)*sizeof(char));
   if (colorStr[0] == NULL) FailAllocMessage();
   strcpy (colorStr[0], myBgColorStr);
   numColorsToDump = 1;

   for (obj_ptr = botObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev)
      BuildObjXPmColors (obj_ptr);

   colorChar[0] = '`';
   if (charsPerPixel > 1) colorChar[1] = '`';
   for (i = 1; i < numColorsToDump; i++)
   {
      if (charsPerPixel == 1)
         colorChar[i] = (char)(((int)('a'))+i-1);
      else
      {
         colorChar[i*2] = (char)(((int)('a'))+(int)(i/10));
         colorChar[i*2+1] = (char)(((int)('0'))+(i%10));
      }
   }
}

static
void ColorStrToXPmStr (index, color_str)
   int	index;
   char	* color_str;
{
   char	s[3];
   int	i, value;

   if (*(colorStr[index]) == '#')
      strcpy (color_str, colorStr[index]);
   else
   {
      for (i = 0; i < maxColors; i++)
         if (colorPixels[i] ==  pixelValue[index])
            break;

      if (i == maxColors)
         strcpy (color_str, colorStr[index]);
      else
      {
         strcpy (color_str, "#");
         value = (int) (((float)tgifColors[i].red/maxRGB) * 0x100);
         if (value > 255)
            value = 255;
         else if (value < 255)
            value = 0;
         sprintf (s, "%c%c", hexValue[(value>>4)&0x0f], hexValue[value&0x0f]);
         strcat (color_str, s);
         strcat (color_str, s);

         value = (int) (((float)tgifColors[i].green/maxRGB) * 0x100);
         if (value > 255)
            value = 255;
         else if (value < 255)
            value = 0;
         sprintf (s, "%c%c", hexValue[(value>>4)&0x0f], hexValue[value&0x0f]);
         strcat (color_str, s);
         strcat (color_str, s);

         value = (int) (((float)tgifColors[i].blue/maxRGB) * 0x100);
         if (value > 255)
            value = 255;
         else if (value < 255)
            value = 0;
         sprintf (s, "%c%c", hexValue[(value>>4)&0x0f], hexValue[value&0x0f]);
         strcat (color_str, s);
         strcat (color_str, s);
      }
   }
}

static
void DumpXPmColors(FP)
   FILE *FP;
{
   int i, j;
   char s[MAXSTRING];

   if (xpmOutputVersion == 1) {
      if (xpmInXGrabSCFormat) {
         for (i=0; i < numColorsToDump-1; i++) {
            if (fprintf (FP, "\"") == EOF) writeFileFailed = TRUE;
            for (j = 0; j < charsPerPixel; j++) {
               if (fprintf (FP, "%c", colorChar[i*charsPerPixel+j]) == EOF) {
                  writeFileFailed = TRUE;
               }
            }
            ColorStrToXPmStr (i, s);
            if (fprintf (FP, "\", \"%s\"\n", s) == EOF) writeFileFailed = TRUE;
         }
         if (fprintf (FP, "\"") == EOF) writeFileFailed = TRUE;
         for (j = 0; j < charsPerPixel; j++) {
            if (fprintf (FP, "%c", colorChar[i*charsPerPixel+j]) == EOF) {
               writeFileFailed = TRUE;
            }
         }
         ColorStrToXPmStr (i, s);
         if (fprintf (FP, "\", \"%s\"\n} ;\n", s) == EOF) {
            writeFileFailed = TRUE;
         }
      } else {
         for (i=0; i < numColorsToDump-1; i++) {
            if (fprintf (FP, "   \"") == EOF) writeFileFailed = TRUE;
            for (j = 0; j < charsPerPixel; j++) {
               if (fprintf (FP, "%c", colorChar[i*charsPerPixel+j]) == EOF) {
                  writeFileFailed = TRUE;
               }
            }
            if (fprintf (FP, "\", \"%s\",\n", colorStr[i]) == EOF)
               writeFileFailed = TRUE;
         }
         if (fprintf (FP, "   \"") == EOF) writeFileFailed = TRUE;
         for (j = 0; j < charsPerPixel; j++) {
            if (fprintf (FP, "%c", colorChar[i*charsPerPixel+j]) == EOF) {
               writeFileFailed = TRUE;
            }
         }
         if (fprintf (FP, "\", \"%s\"\n};\n", colorStr[i]) == EOF) {
            writeFileFailed = TRUE;
         }
      }
   } else {
      /* xpmOutputVersion is 3 */
      for (i=0; i < numColorsToDump; i++) {
         if (fprintf (FP, "\"") == EOF) writeFileFailed = TRUE;
         for (j = 0; j < charsPerPixel; j++) {
            if (fprintf (FP, "%c", colorChar[i*charsPerPixel+j]) == EOF) {
               writeFileFailed = TRUE;
            }
         }
         if (fprintf (FP, " c %s\",\n", colorStr[i]) == EOF) {
            writeFileFailed = TRUE;
         }
      }
   }
}

/*
 * static
 * void MapPixToEPSI(SrcX, SrcY, SrcW, SrcH, DestW, DestH, DestX, DestY)
 *    int	SrcX, SrcY, SrcW, SrcH, DestW, DestH, * DestX, * DestY;
 * {
 *    if (pageStyle == LANDSCAPE)
 *    {
 *       *DestY = (int)round(((float)DestH) -
 *             ((((float)DestH)*((float)SrcX))/((float)SrcW)));
 *       *DestX = (int)round((((float)DestW)*((float)SrcY))/((float)SrcH));
 *    }
 *    else
 *    {
 *       *DestX = (int)round((((float)DestW)*((float)SrcX))/((float)SrcW));
 *       *DestY = (int)round((((float)DestH)*((float)SrcY))/((float)SrcH));
 *    }
 *    if (*DestX >= DestW) *DestX = DestW-1; if (*DestX < 0) *DestX = 0;
 *    if (*DestY >= DestH) *DestY = DestH-1; if (*DestY < 0) *DestY = 0;
 * }
 */

static
void MapEPSIToPix(DestX, DestY, DestW, DestH, SrcW, SrcH, SrcX, SrcY)
   int	DestX, DestY, DestW, DestH, SrcW, SrcH, * SrcX, * SrcY;
{
   if (pageStyle == LANDSCAPE)
   {
      *SrcX = (int)round(((float)(DestH-DestY))*((float)SrcW)/((float)DestH));
      *SrcY = (int)round((((float)SrcH)*((float)DestX))/((float)DestW));
   }
   else
   {
      *SrcX = (int)round((((float)SrcW)*((float)DestX))/((float)DestW));
      *SrcY = (int)round((((float)SrcH)*((float)DestY))/((float)DestH));
   }
   if (*SrcX >= SrcW) *SrcX = SrcW-1; if (*SrcX < 0) *SrcX = 0;
   if (*SrcY >= SrcH) *SrcY = SrcH-1; if (*SrcY < 0) *SrcY = 0;
}

void GenPreviewBitmap (FP, llxPage, llyPage, urxPage, uryPage)
   FILE	* FP;
   int	llxPage, llyPage, urxPage, uryPage;
{
   register int	col, row;
   int		ltx, lty, w, h, * * data, num_image_bytes_per_row, x, y;
   int		image_w=urxPage-llxPage, image_h=uryPage-llyPage, num_lines;
   Pixmap	pixmap;
   XImage	* image;

   if (image_w == 0 || image_h == 0) return;
   if ((pixmap = DrawAllOnPixmap (&ltx, &lty, &w, &h)) == None) return;

   image = XGetImage (mainDisplay, pixmap, 0, 0, w, h, AllPlanes, ZPixmap);
   if (image == NULL)
   {
      MsgBox ("Can not generate image.\n\nPrint aborted!", TOOL_NAME, INFO_MB);
      XFreePixmap (mainDisplay, pixmap);
      return;
   }
   if (image_w < 0) image_w = (-image_w);
   if (image_h < 0) image_h = (-image_h);
   num_image_bytes_per_row = ((image_w & 0x7) ? (image_w>>3)+1 : (image_w>>3));
   num_lines = ((image_w & 0x7) ? (((image_w>>3)+1)<<1) : ((image_w>>3)<<1));
   num_lines = ((num_lines & 0x3f) ? (num_lines>>6)+1 : (num_lines>>6));

   if ((data=(int**)malloc(image_h*sizeof(int*))) == NULL) {
      FailAllocMessage();
      return;
   }
   for (row=0; row < image_h; row++)
      if ((data[row]=(int*)malloc(num_image_bytes_per_row*sizeof(int))) ==
            NULL) {
         FailAllocMessage();
         return;
      } else {
         for (col=0; col<num_image_bytes_per_row; col++) data[row][col] = 0;
      }

   fprintf (FP, "%%%%BeginPreview: %1d %1d 1 %1d\n", image_w, image_h,
         num_lines*image_h);
   for (row=0; row < image_h; row++)
   {
      for (col=0; col < image_w; col++)
      {
         MapEPSIToPix(col, row, image_w, image_h, w, h, &x, &y);
         if (XGetPixel (image, x, y) != myBgPixel)
         {
            data[row][col>>3] |= (1<<(7 - (col & 0x7)));
         }
      }
   }
   for (row=0; row < image_h; row++)
   {
      int	byte_count=0;

      fprintf (FP, "%% ");
      for (col=0; col < num_image_bytes_per_row; col++)
      {
         if (byte_count++ == 32)
         {
            byte_count = 1;
            fprintf (FP, "\n%% ");
         }
         fprintf (FP, "%c", hexValue[(data[row][col]>>4) & 0xf]);
         fprintf (FP, "%c", hexValue[data[row][col] & 0xf]);
      }
      fprintf (FP, "\n");
   }
   fprintf (FP, "%%%%EndImage\n");
   fprintf (FP, "%%%%EndPreview\n");

   for (row=0; row < image_h; row++) free(data[row]);
   free(data);
   XDestroyImage (image);
   XFreePixmap (mainDisplay, pixmap);
}

static
void InitImageMap ()
{
   char *c_ptr;

   if (generateImageMap != INVALID) return;

   generateImageMap = FALSE;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"GenerateImageMap")) != NULL &&
         UtilStrICmp(c_ptr, "true") == 0) {
      generateImageMap = TRUE;
   }
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"XpmToGif")) != NULL) {
      int count=0;

      strcpy(xpmToGifCmd, c_ptr);
      for (c_ptr=strstr(xpmToGifCmd,"%s"); c_ptr!=NULL;
            c_ptr=strstr(++c_ptr,"%s")) {
         count++;
      }
      if (count != 1) {
         sprintf(gszMsgBox, "Invalid %s*%s: %s resource.\n\n'%s' used.",
               TOOL_NAME, "XpmToGif", xpmToGifCmd,
               "xpmtoppm %s | ppmtogif");
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         strcpy(xpmToGifCmd, "xpmtoppm %s | ppmtogif");
      }
   } else {
      strcpy(xpmToGifCmd, "xpmtoppm %s | ppmtogif");
   }
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"GifFileExtension")) != NULL) {
      strcpy(gifFileExtension, c_ptr);
   } else {
      strcpy(gifFileExtension, "gif");
   }
   useXPmVersion1ForImageMap = TRUE;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,
         "UseXPmVersion1ForImageMap")) != NULL &&
         UtilStrICmp("false",c_ptr) == 0) {
      useXPmVersion1ForImageMap = FALSE;
   }
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME,
         "ImageMapFileExtension")) != NULL) {
      strcpy(imageMapFileExtension, c_ptr);
   } else {
      strcpy(imageMapFileExtension, "map");
   }
   imageMapFileFormat = IMF_FORMAT_NCSA;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"ImageMapFileFormat")) !=
         NULL) {
      if (strcmp(c_ptr, "NCSA") == 0) {
         imageMapFileFormat = IMF_FORMAT_NCSA;
      } else if (strcmp(c_ptr, "CERN") == 0) {
         imageMapFileFormat = IMF_FORMAT_CERN;
      } else {
         sprintf(gszMsgBox, "Invalid %s*%s: %s resource.\n\nNCSA used.",
               TOOL_NAME, "ImageMapFileFormat", c_ptr);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      }
   }
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME,
         "HtmlFileExtension")) != NULL) {
      strcpy(htmlFileExtension, c_ptr);
   } else {
      strcpy(htmlFileExtension, "html");
   }
   generateHtmlHref = TRUE;
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "GenerateHtmlHref")) != NULL) {
      if (UtilStrICmp(c_ptr, "false") == 0) {
         generateHtmlHref = FALSE;
      }
   }
}

static
char *ModifyToGenerateHtmlHRef(value_str)
   char *value_str;
   /* data in value_str is not supposed to be touched on return */
{
   char *href=NULL;

   if (generateHtmlHref && imageMapFileFormat==IMF_FORMAT_SPYG) {
      char *name=UtilStrRChr(value_str, '/'), *dot, *pound;

      if (name == NULL) {
         name = value_str;
      } else {
         name++;
      }
      pound = strchr(name, '#');
      if (pound != NULL) *pound = '\0';
      dot = UtilStrRChr(name, '.');
      if (dot != NULL && (UtilStrICmp(&dot[1], "obj")==0 ||
            UtilStrICmp(&dot[1], OBJ_FILE_EXT) == 0)) {
         int len=strlen(value_str)+strlen(htmlFileExtension)+2;

         *dot = '\0';
         if (pound != NULL) len += strlen(&pound[1])+1;
         href = (char*)malloc((len+1)*sizeof(char));
         if (href == NULL) FailAllocMessage();
         if (pound != NULL) {
            sprintf(href, "%s.%s#%s", value_str, htmlFileExtension, &pound[1]);
         } else {
            sprintf(href, "%s.%s", value_str, htmlFileExtension);
         }
         *dot = '.';
      }
      if (pound != NULL) *pound = '#';
   }
   return href;
}

static
int GenerateObjImageMap(FP, ObjPtr, LtX, LtY)
   FILE *FP;
   struct ObjRec *ObjPtr;
   int LtX, LtY;
{
   register int i;
   int n, something_generated=FALSE;
   struct AttrRec *attr_ptr;
   struct ObjRec *obj_ptr;
   XPoint *v;

   if ((attr_ptr=FindAttrWithName(ObjPtr, "href=", NULL)) != NULL) {
      char *href=ModifyToGenerateHtmlHRef(attr_ptr->attr_value.s);

      switch (ObjPtr->type) {
      case OBJ_POLY:
      case OBJ_POLYGON:
         if (ObjPtr->type == OBJ_POLY) {
            n = ObjPtr->detail.p->sn;
            v = ObjPtr->detail.p->svlist;
         } else {
            n = ObjPtr->detail.g->sn;
            v = ObjPtr->detail.g->svlist;
         }
         switch (imageMapFileFormat) {
         case IMF_FORMAT_NCSA:
            fprintf(FP, "\npoly %s", attr_ptr->attr_value.s);
            for (i=0; i < n; i++) {
               fprintf(FP, " %1d,%1d", v[i].x-LtX, v[i].y-LtY);
            }
            fprintf(FP, "\n");
            break;
         case IMF_FORMAT_CERN:
            fprintf(FP, "\npoly");
            for (i=0; i < n; i++) {
               fprintf(FP, " (%1d,%1d)", v[i].x-LtX, v[i].y-LtY);
            }
            fprintf(FP, " %s\n", attr_ptr->attr_value.s);
            break;
         case IMF_FORMAT_SPYG:
            fprintf(FP, "<AREA SHAPE=\"POLY\" COORDS=\"");
            for (i=0; i < n; i++) {
               fprintf(FP, "%s%1d,%1d", (i==0 ? "" : ","),
                     v[i].x-LtX, v[i].y-LtY);
            }
            fprintf(FP, "\" HREF=\"%s\">\n",
                  (href != NULL ? href : attr_ptr->attr_value.s));
            break;
         default: break;
         }
         break;
      case OBJ_OVAL:
         if (ObjPtr->obbox.rbx-ObjPtr->obbox.ltx ==
               ObjPtr->obbox.rby-ObjPtr->obbox.lty) {
            switch (imageMapFileFormat) {
            case IMF_FORMAT_NCSA:
               fprintf(FP, "\ncircle %s %1d,%1d %1d,%1d\n",
                     attr_ptr->attr_value.s,
                     ((ObjPtr->obbox.ltx+ObjPtr->obbox.rbx)>>1)-LtX,
                     ((ObjPtr->obbox.lty+ObjPtr->obbox.rby)>>1)-LtY,
                     ObjPtr->obbox.rbx-LtX,
                     ((ObjPtr->obbox.lty+ObjPtr->obbox.rby)>>1)-LtY);
               break;
            case IMF_FORMAT_CERN:
               fprintf(FP, "\ncircle (%1d,%1d) %1d %s\n",
                     ((ObjPtr->obbox.ltx+ObjPtr->obbox.rbx)>>1)-LtX,
                     ((ObjPtr->obbox.lty+ObjPtr->obbox.rby)>>1)-LtY,
                     (ObjPtr->obbox.rbx-ObjPtr->obbox.ltx)>>1,
                     attr_ptr->attr_value.s);
               break;
            case IMF_FORMAT_SPYG:
               fprintf(FP, "<AREA SHAPE=\"CIRCLE\" COORDS=\"");
               fprintf(FP, "%1d,%1d,%1d",
                     ((ObjPtr->obbox.ltx+ObjPtr->obbox.rbx)>>1)-LtX,
                     ((ObjPtr->obbox.lty+ObjPtr->obbox.rby)>>1)-LtY,
                     (ObjPtr->obbox.rbx-ObjPtr->obbox.ltx)>>1);
               fprintf(FP, "\" HREF=\"%s\">\n",
                  (href != NULL ? href : attr_ptr->attr_value.s));
               break;
            default: break;
            }
         } else {
            switch (imageMapFileFormat) {
            case IMF_FORMAT_NCSA:
               fprintf(FP, "\nrect %s %1d,%1d %1d,%1d\n",
                     attr_ptr->attr_value.s,
                     ObjPtr->obbox.ltx-LtX, ObjPtr->obbox.lty-LtY,
                     ObjPtr->obbox.rbx-LtX, ObjPtr->obbox.rby-LtY);
               break;
            case IMF_FORMAT_CERN:
               fprintf(FP, "\nrect (%1d,%1d) (%1d,%1d) %s\n",
                     ObjPtr->obbox.ltx-LtX, ObjPtr->obbox.lty-LtY,
                     ObjPtr->obbox.rbx-LtX, ObjPtr->obbox.rby-LtY,
                     attr_ptr->attr_value.s);
               break;
            case IMF_FORMAT_SPYG:
               fprintf(FP, "<AREA SHAPE=\"RECT\" COORDS=\"");
               fprintf(FP, "%1d,%1d,%1d,%1d",
                     ObjPtr->obbox.ltx-LtX, ObjPtr->obbox.lty-LtY,
                     ObjPtr->obbox.rbx-LtX, ObjPtr->obbox.rby-LtY);
               fprintf(FP, "\" HREF=\"%s\">\n",
                  (href != NULL ? href : attr_ptr->attr_value.s));
               break;
            default: break;
            }
         }
         break;
      default:
         switch (imageMapFileFormat) {
         case IMF_FORMAT_NCSA:
            fprintf(FP, "\nrect %s %1d,%1d %1d,%1d\n",
                  attr_ptr->attr_value.s,
                  ObjPtr->obbox.ltx-LtX, ObjPtr->obbox.lty-LtY,
                  ObjPtr->obbox.rbx-LtX, ObjPtr->obbox.rby-LtY);
            break;
         case IMF_FORMAT_CERN:
            fprintf(FP, "\nrect (%1d,%1d) (%1d,%1d) %s\n",
                  ObjPtr->obbox.ltx-LtX, ObjPtr->obbox.lty-LtY,
                  ObjPtr->obbox.rbx-LtX, ObjPtr->obbox.rby-LtY,
                  attr_ptr->attr_value.s);
            break;
         case IMF_FORMAT_SPYG:
            fprintf(FP, "<AREA SHAPE=\"RECT\" COORDS=\"");
            fprintf(FP, "%1d,%1d,%1d,%1d",
                  ObjPtr->obbox.ltx-LtX, ObjPtr->obbox.lty-LtY,
                  ObjPtr->obbox.rbx-LtX, ObjPtr->obbox.rby-LtY);
            fprintf(FP, "\" HREF=\"%s\">\n",
                  (href != NULL ? href : attr_ptr->attr_value.s));
            break;
         default: break;
         }
         break;
      }
      if (href != NULL) free(href);
      return TRUE;
   }
   switch (ObjPtr->type) {
   case OBJ_ICON:
   case OBJ_GROUP:
   case OBJ_SYM:
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr=obj_ptr->prev) {
         if (GenerateObjImageMap (FP, obj_ptr, LtX, LtY)) {
            something_generated = TRUE;;
         }
      }
      break;
   }
   return something_generated;
}

static
void GenerateHtmlHeader(map_fp)
   FILE *map_fp;
{
   struct AttrRec *attr_ptr;

   fprintf(map_fp, "<HTML>\n<HEAD>\n");
   if (curPage->name != NULL && *curPage->name != '\0') {
      fprintf(map_fp, "<TITLE>\n%s\n</TITLE>\n", curPage->name);
   } else if ((attr_ptr=FindFileAttrWithName("title=")) != NULL) {
      fprintf(map_fp, "<TITLE>\n%s\n</TITLE>\n",
            attr_ptr->attr_value.s);
   }
   if ((attr_ptr=FindFileAttrWithName("base=")) != NULL) {
      fprintf(map_fp, "<BASE HREF=\"%s\">\n", attr_ptr->attr_value.s);
   }
   if ((attr_ptr=FindFileAttrWithName("made=")) != NULL) {
      fprintf(map_fp, "<LINK REV=\"made\" HREF=\"%s\">\n",
            attr_ptr->attr_value.s);
   }
   if ((attr_ptr=FindFileAttrWithName("keywords=")) != NULL) {
      fprintf(map_fp, "<META HTTP-EQUIV=\"Keywords\" CONTENT=\"%s\">\n",
            attr_ptr->attr_value.s);
   }
   fprintf(map_fp, "</HEAD>\n<BODY>\n");
}

static
void GenerateUseMapHeader(map_fp, gif_fname, map_fname)
   FILE *map_fp;
   char *gif_fname, *map_fname;
{
   char *gif_fname_ptr=UtilStrRChr(gif_fname, '/');
   char *map_fname_ptr=UtilStrRChr(map_fname, '/');
   int page_num=0;
   struct AttrRec *attr_ptr;

   if (gif_fname_ptr != NULL) gif_fname_ptr++;
   if (map_fname_ptr != NULL) map_fname_ptr++;
   fprintf(map_fp, "<H1>\n");
   attr_ptr = FindFileAttrWithName("alt=");
   if (attr_ptr != NULL) {
      fprintf(map_fp, "<IMG ALT=\"%s\" SRC=\"%s\" USEMAP=\"%s#p%1d\">\n",
            attr_ptr->attr_value.s,
            (gif_fname_ptr==NULL ? "unknown" : gif_fname_ptr),
            (map_fname_ptr==NULL ? "unknown" : map_fname_ptr), page_num);
   } else {
      fprintf(map_fp, "<IMG SRC=\"%s\" USEMAP=\"%s#p%1d\">\n",
            (gif_fname_ptr==NULL ? "unknown" : gif_fname_ptr),
            (map_fname_ptr==NULL ? "unknown" : map_fname_ptr), page_num);
   }
   fprintf(map_fp, "</H1>\n");
   fprintf(map_fp, "<MAP NAME=\"p%1d\">\n", page_num);
}

static
void GenerateUseMapTrailer(map_fp, W, H)
   FILE *map_fp;
   int W, H;
{
   struct AttrRec *attr_ptr=FindFileAttrWithName("href=");

   if (attr_ptr != NULL) {
      char *href=ModifyToGenerateHtmlHRef(attr_ptr->attr_value.s);

      fprintf(map_fp, "<AREA SHAPE=\"RECT\" COORDS=\"");
      fprintf(map_fp, "0,0,%1d,%1d", W, H);
      fprintf(map_fp, "\" HREF=\"%s\">\n",
            (href != NULL ? href : attr_ptr->attr_value.s));
      if (href != NULL) free(href);
   }
   fprintf(map_fp, "</MAP>\n");
}

static
void GenerateHtmlTrailer(map_fp)
   FILE *map_fp;
{
   fprintf(map_fp, "</BODY>\n</HTML>\n");
}

static
FILE *OpenMapFile(pszMapFile)
   char *pszMapFile;
{
   FILE *pFile=fopen(pszMapFile, "w");

   if (pFile == NULL) {
      sprintf(gszMsgBox, "Fail to open '%s' for write.\n\n%s!", pszMapFile,
            "Imagemap not generated");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return NULL;
   }
   return pFile;
}

static
void GenerateImageMap(XpmFileName, LtX, LtY, RbX, RbY)
   char *XpmFileName;
   int LtX, LtY, RbX, RbY;
{
   char cmd[MAXSTRING+1];
   char map_fname[MAXPATHLENGTH+1], gif_fname[MAXPATHLENGTH+1], buf[1024];
   struct ObjRec *obj_ptr;
   FILE *gif_fp, *pfp, *map_fp;
   int xpm_ext_len, len, bytes_read, something_generated=FALSE;
   struct AttrRec *attr_ptr;

   *map_fname = '\0';
   xpm_ext_len = strlen(XPM_FILE_EXT)+1;
   len = strlen(XpmFileName);
   if (XpmFileName[len-xpm_ext_len] != '.') {
      sprintf(gszMsgBox, "%s.\n\n%s!",
            "Fatal error in generating GIF/imagemap", "Imagemap not generated");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   XpmFileName[len-xpm_ext_len] = '\0';
   switch (whereToPrint) {
   case GIF_FILE:
      sprintf(map_fname, "%s%s%s", XpmFileName,
            *imageMapFileExtension == '\0' ? "" : ".", imageMapFileExtension);
      break;
   case HTML_FILE:
      sprintf(map_fname, "%s%s%s", XpmFileName,
            *htmlFileExtension == '\0' ? "" : ".", htmlFileExtension);
      break;
   }
   sprintf(gif_fname, "%s%s%s", XpmFileName,
         *gifFileExtension == '\0' ? "" : ".", gifFileExtension);
   XpmFileName[len-xpm_ext_len] = '.';
   if ((gif_fp=fopen(gif_fname, "w")) == NULL) {
      sprintf(gszMsgBox, "Fail to open '%s' for write.\n\n%s!", gif_fname,
            "Imagemap not generated");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   sprintf(cmd, xpmToGifCmd, XpmFileName);

   Msg("Executing:");
   sprintf(gszMsgBox, "    %s", cmd);
   Msg(gszMsgBox);
   sprintf(gszMsgBox, "Executing '%s'...", cmd);
   SetStringStatus(gszMsgBox);
   XSync(mainDisplay, False);
   if ((pfp=(FILE*)popen(cmd, "r")) == NULL) {
      fclose(gif_fp);
      unlink(gif_fname);
      sprintf(gszMsgBox, "Fail to execute '%s'.\n\nImagemap not generated!",
            cmd);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   while ((bytes_read=fread(buf, sizeof(char), sizeof(buf), pfp)) > 0) {
      if ((int)fwrite(buf, sizeof(char), bytes_read, gif_fp) <= 0) {
         writeFileFailed = TRUE;
         break;
      }
   }
   pclose(pfp);
   SetStringStatus("...Done");
   fclose(gif_fp);
   if (writeFileFailed) {
      sprintf(gszMsgBox, "Fail to write to '%s'.\n\nFile system may be full.",
            map_fname);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   }
   switch (whereToPrint) {
   case GIF_FILE:
      if ((attr_ptr=FindFileAttrWithName("href=")) != NULL) {
         map_fp = OpenMapFile(map_fname);
         if (map_fp != NULL) {
            Msg("Generating Imagemap file...");
            fprintf(map_fp, "default %s\n", attr_ptr->attr_value.s);
            for (obj_ptr=botObj; obj_ptr != NULL; obj_ptr=obj_ptr->prev) {
               if (GenerateObjImageMap(map_fp, obj_ptr, LtX, LtY)) {
                  something_generated = TRUE;
               }
            }
            fclose(map_fp);
            Msg("");
            sprintf(gszMsgBox, "Imagemap file '%s' generated.", map_fname);
            Msg(gszMsgBox);
         }
      } else if (generateImageMap) {
         if (something_generated) {
            sprintf(gszMsgBox, "%s.\n\n%s.",
                  "Cannot find a 'href=' file attribute",
                  "Imagemap not generated");
         } else {
            sprintf(gszMsgBox, "%s.\n\n%s.  %s.",
                  "Cannot find any 'href=' attributes",
                  "Imagemap not generated", "Only a GIF file is generated");
         }
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      }
      break;
   case HTML_FILE:
      map_fp = OpenMapFile(map_fname);
      if (map_fp != NULL) {
         int saved_image_map_file_format=imageMapFileFormat;

         Msg("Generating HTML file...");
         GenerateHtmlHeader(map_fp);
         GenerateUseMapHeader(map_fp, gif_fname, map_fname);
         imageMapFileFormat = IMF_FORMAT_SPYG;
         for (obj_ptr=botObj; obj_ptr != NULL; obj_ptr=obj_ptr->prev) {
            if (GenerateObjImageMap(map_fp, obj_ptr, LtX, LtY)) {
               something_generated = TRUE;
            }
         }
         GenerateUseMapTrailer(map_fp, RbX-LtX, RbY-LtY);
         GenerateHtmlTrailer(map_fp);
         Msg("");
         fclose(map_fp);
         imageMapFileFormat = saved_image_map_file_format;
         sprintf(gszMsgBox, "HTML file '%s' generated.", map_fname);
         Msg(gszMsgBox);
      }
      break;
   }
}

extern char *mktemp ARGS_DECL((char *));

#define FS_SCALE 0x400
#define HALF_FS_SCALE 0x200

void DumpXBitmapFile ()
{  /* called when printing in the xbm or xpm format */
   register int j, k, bit_count, index, data;
   int i, len, ltx, lty, w, h, byte_count, short_name, pixel;
   int left, top, right, bottom, target_percent;
   char xbm_file_name[MAXPATHLENGTH+1], *rest, name[MAXPATHLENGTH+1];
   FILE *fp;
   Pixmap pixmap=None;
   XImage *image=NULL, *bitmap_image=NULL;

   if (gnInImageProc) {
      *gszImageProcXPmFile = '\0';
      sprintf(xbm_file_name, "%sTgifXXXXXX", TMP_DIR);
      mktemp(xbm_file_name);
      unlink(xbm_file_name);
      strcpy(name, "unnamed");
   } else {
      strcpy (name, curFileName);
      len = strlen (name);
      for (j = len-1; name[j] != '.'; j--) ;
      name[j] = '\0';

      sprintf (xbm_file_name, "%s/%s", curDir, curFileName);
      len = strlen (xbm_file_name);
      for (j = len-1; xbm_file_name[j] != '.'; j--) ;
      if (colorDump)
         sprintf (&xbm_file_name[j], ".%s", XPM_FILE_EXT);
      else
         sprintf (&xbm_file_name[j], ".%s", XBM_FILE_EXT);

      ModifyOutputFileName (xbm_file_name);
   }
   if ((short_name=IsPrefix(bootDir, xbm_file_name, &rest))) ++rest;
   if ((fp=fopen(xbm_file_name, "w")) == NULL) {
      if (short_name) {
         sprintf(gszMsgBox, "Can not open '%s', print aborted.", rest);
      } else {
         sprintf(gszMsgBox, "Can not open '%s', print aborted.",
               xbm_file_name);
      }
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return;
   }
   if (colorDump) InitImageMap();
   Msg("Generating image...");

   if ((pixmap=DrawAllOnPixmap(&ltx, &lty, &w, &h)) == None) {
      fclose(fp);
      unlink(xbm_file_name);
      return;
   }

   if (short_name) {
      sprintf(gszMsgBox, "Printing into '%s'...", rest);
   } else {
      sprintf(gszMsgBox, "Printing into '%s'...", xbm_file_name);
   }
   Msg(gszMsgBox);

   left = leftExportPixelTrim;
   top = topExportPixelTrim;
   right = rightExportPixelTrim;
   bottom = bottomExportPixelTrim;
   if (leftExportPixelTrim+rightExportPixelTrim >= w ||
         topExportPixelTrim+bottomExportPixelTrim >= h) {
      SetExportPixelTrim("-- which are too large now");
      if (leftExportPixelTrim+rightExportPixelTrim >= w ||
            topExportPixelTrim+bottomExportPixelTrim >= h) {
         sprintf(gszMsgBox, "%s [%1d,%1d,%1d,%1d] %s.\n\n%s.",
               "The ExportPixelTrim values", leftExportPixelTrim,
               topExportPixelTrim, rightExportPixelTrim,
               bottomExportPixelTrim, "are still too large",
               "Zeroes are used.");
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         left = top = right = bottom = 0;
      } else {
         left = leftExportPixelTrim;
         top = topExportPixelTrim;
         right = rightExportPixelTrim;
         bottom = bottomExportPixelTrim;
      }
   }
   if ((image=XGetImage(mainDisplay, pixmap, left, top,
         w-left-right, h-top-bottom, AllPlanes, ZPixmap)) == NULL) {
      Msg("Can not generate image, print aborted!");
      fclose(fp);
      unlink(xbm_file_name);
      XFreePixmap(mainDisplay, pixmap);
      return;
   }
   if (gnInImageProc && topSel != NULL && topSel == botSel &&
         topSel->obj->type == OBJ_XPM) {
/*    if ((bitmap_image=XGetImage(mainDisplay,
            topSel->obj->detail.xpm->bitmap, left, top,
            w-left-right, h-top-bottom, 1, ZPixmap)) == NULL) {
         Msg("Can not generate image, print aborted!");
         fclose(fp);
         unlink(xbm_file_name);
         XDestroyImage(image);
         XFreePixmap(mainDisplay, pixmap);
         return;
      } */
   }
   if (left != 0 || top != 0 || right != 0 || bottom != 0) {
      sprintf(gszMsgBox, "Applying ExportPixelTrim of [%1d,%1d,%1d,%1d]...",
            left, top, right, bottom);
      Msg(gszMsgBox);
   }
   writeFileFailed = FALSE;
   if (colorDump && gnInImageProc && gnConvolving) {
      if (!DoConvolution(fp, image, bitmap_image, w, h)) {
         fclose(fp);
         unlink(xbm_file_name);
         if (bitmap_image != NULL) XDestroyImage(bitmap_image);
         XDestroyImage(image);
         XFreePixmap(mainDisplay, pixmap);
         return;
      }
   } else if (colorDump) {
      int saved_xpm_output_version=xpmOutputVersion;

      if (whereToPrint==GIF_FILE) {
         if (useXPmVersion1ForImageMap) xpmOutputVersion = 1;
      }
      BuildXPmColors();
      if (xpmOutputVersion == 1) {
         fprintf(fp, "#define %s_format 1\n", name);
         fprintf(fp, "#define %s_width %1d\n", name, w-left-right);
         fprintf(fp, "#define %s_height %1d\n", name, h-top-bottom);
         fprintf(fp, "#define %s_ncolors %1d\n", name, numColorsToDump);
         fprintf(fp, "#define %s_chars_per_pixel %1d\n", name, charsPerPixel);
         if (xpmInXGrabSCFormat) {
            fprintf(fp, "static char * %s_colors[] = {\n", name);
         } else {
            fprintf(fp, "static char *%s_colors[] = {\n", name);
         }
         DumpXPmColors(fp);
         if (xpmInXGrabSCFormat) {
            fprintf(fp, "static char * %s_pixels[] = {\n", name);
         } else {
            fprintf(fp, "static char *%s_pixels[] = {\n", name);
         }
      } else {
         /* xpmOutputVersion is 3 */
         fprintf(fp, "/* XPM */\n");
         fprintf(fp, "static char * %s[] = {\n", name);
         fprintf(fp, "\"%1d %1d %1d %1d\",\n",
               w-left-right, h-top-bottom, numColorsToDump, charsPerPixel);
         DumpXPmColors(fp);
      }
      xpmOutputVersion = saved_xpm_output_version;
   } else {
      fprintf(fp, "#define %s_width %1d\n", name, w-left-right);
      fprintf(fp, "#define %s_height %1d\n", name, h-top-bottom);
      fprintf(fp, "#define %s_x_hot 0\n", name);
      fprintf(fp, "#define %s_y_hot 0\n", name);
      fprintf(fp, "static char %s_bits[] = {\n   ", name);
   }
   SaveStatusStrings();
   if (colorDump && gnInImageProc && gnConvolving) {
   } else if (colorDump) {
      BuildXPmBuckets(numColorsToDump, pixelValue, INVALID, NULL);
      target_percent = 5;
      for (i = top; i < h-bottom; i++) {
         int percent=((i-top)*10000/(h-top-bottom))/100;

         if (percent >= target_percent) {
            sprintf(gszMsgBox, "Progress: %1d%%", percent);
            SetStringStatus(gszMsgBox);
            XSync(mainDisplay, False);
            while (target_percent <= percent) target_percent += 5;
         }
         if (fprintf(fp, "\"") == EOF) writeFileFailed = TRUE;
         for (j = left; j < w-right; j++) {
            int transparent=FALSE;

            if (gnInImageProc && bitmap_image != NULL) {
               if (XGetPixel(bitmap_image,j-left,i-top) != 0) {
                  data = XGetPixel(image,j-left,i-top);
               } else {
                  transparent = TRUE;
               }
            } else {
               data = XGetPixel(image,j-left,i-top);
            }
            if (!gnInImageProc && data == myBgPixel) {
               switch (charsPerPixel) {
               case 1:
                  if (fprintf(fp, "`") == EOF) writeFileFailed = TRUE;
                  break;
               case 2:
                  if (fprintf(fp, "``") == EOF) writeFileFailed = TRUE;
                  break;
               }
            } else {
               if (transparent) {
                  index = transparentIndex;
               } else {
                  index = XPmLookUp(data, INVALID, NULL);
               }
               if (index == INVALID) {
                  fclose(fp);
                  sprintf(gszMsgBox, "Unrecognized pixel value %1d!\n\n%s!",
                        data, "Print aborted");
                  MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);

                  unlink(xbm_file_name);
                  if (bitmap_image != NULL) XDestroyImage(bitmap_image);
                  XDestroyImage(image);
                  XFreePixmap(mainDisplay, pixmap);
                  return;
               }
               for (k = 0; k < charsPerPixel; k++) {
                  if (fprintf(fp, "%c", colorChar[index*charsPerPixel+k]) ==
                        EOF) {
                     writeFileFailed = TRUE;
                  }
               }
            }
         }
         if (i == h-bottom-1) {
            if (xpmInXGrabSCFormat) {
               if (fprintf(fp, "\",\n} ;\n") == EOF) {
                  writeFileFailed = TRUE;
               }
            } else {
               if (fprintf(fp, "\"\n};\n") == EOF) {
                  writeFileFailed = TRUE;
               }
            }
         } else if (fprintf(fp, "\",\n") == EOF) {
            writeFileFailed = TRUE;
         }
      }
   } else {
      if (halfToneBitmap) {
         /*--------------------------------------------------*/
         /* the halftoning code is adapted from 'pgmopbm.c', */
         /*       which is part of the pbmplus package.      */
         /*                                                  */
         /*       Copyright (C) 1989 by Jef Poskanzer        */
         /*--------------------------------------------------*/
         long	* thiserr, * nexterr, * tmperr, threshval, sum;
         int	fs_forward=TRUE, col, limitcol, found_index, * bits;
         float	gray;

         sprintf(gszMsgBox, "    FS halftone bitmap (threshold=%s) ...",
               bitmapThresholdStr);
         Msg(gszMsgBox);

         srand (0);
         thiserr = (long*)malloc((w-left-right+2)*sizeof(long));
         nexterr = (long*)malloc((w-left-right+2)*sizeof(long));
         bits = (int*)malloc((w-left-right+2)*sizeof(int));
         if (thiserr==NULL || nexterr==NULL || bits==NULL) FailAllocMessage();
         for (j = 0; j < w-left-right+2; j++) {
            thiserr[j] = (rand() % FS_SCALE - HALF_FS_SCALE) >> 2;
         }
         threshval = (long)(bitmapThreshold * (float)FS_SCALE);

         byte_count = 0;
         target_percent = 5;
         for (i = top; i < h-bottom; i++) {
            int	percent=((i-top)*10000/(h-top-bottom))/100;

            if (percent >= target_percent) {
               sprintf(gszMsgBox, "Progress: %1d%%", percent);
               SetStringStatus(gszMsgBox);
               XSync(mainDisplay, False);
               while (target_percent <= percent) target_percent += 5;
            }
            for (col = 0; col < w-left-right+2; col++) {
               nexterr[col] = bits[col] = 0;
            }
            bit_count = 0;
            col = (fs_forward) ? 0 : w-left-right-1;
            limitcol = (fs_forward) ? w-left-right : -1;
            do {
               found_index = INVALID;
               pixel = XGetPixel(image, col, i);

               for (k = 0; k < maxColors; k++) {
                  if (colorPixels[k] == pixel) {
                     found_index = k;
                     break;
                  }
               }
               if (found_index == INVALID) {
                  if (pixel == myBgPixel) {
                     gray = (float)1.0;
                  } else {
                     sprintf(gszMsgBox,
                           "Unrecognized pixel value %1d!  1 used.", pixel);
                     Msg(gszMsgBox);
                     gray = (float)0.0;
                  }
               } else {
                  gray = 0.299*((float)tgifColors[found_index].red/maxRGB) +
                        0.587*((float)tgifColors[found_index].green/maxRGB) +
                        0.114*((float)tgifColors[found_index].blue/maxRGB);
               }
               sum = ((long)(gray * FS_SCALE)) + thiserr[col+1];
               if (sum >= threshval) {
                  sum = sum-threshval-HALF_FS_SCALE;
               } else {
                  bits[col] = 1; /* black bit */
               }
               if (fs_forward) {
                  thiserr[col+2] += (sum*7)>>4;
                  nexterr[col  ] += (sum*3)>>4;
                  nexterr[col+1] += (sum*5)>>4;
                  nexterr[col+2] += (sum  )>>4;
                  col++;
               } else {
                  thiserr[col  ] += (sum*7)>>4;
                  nexterr[col+2] += (sum*3)>>4;
                  nexterr[col+1] += (sum*5)>>4;
                  nexterr[col  ] += (sum  )>>4;
                  col--;
               }
            } while (col != limitcol);

            tmperr = thiserr;
            thiserr = nexterr;
            nexterr = tmperr;
            fs_forward = !fs_forward;

            bit_count = 0;
            data = 0;

            for (j = left; j < w-right; j++) {
               if (bits[j]) data |= (1<<bit_count);

               if (++bit_count == 8) {
                  if (byte_count++ == 12) {
                     byte_count = 1;
                     if (fprintf(fp, "\n   ") == EOF) writeFileFailed = TRUE;
                  }
                  if (fprintf(fp, "0x%c", hexValue[(data>>4) & 0xf]) == EOF) {
                     writeFileFailed = TRUE;
                  }
                  if (i == h-bottom-1 && j == w-right-1) {
                     if (fprintf(fp, "%c};\n", hexValue[data & 0xf]) == EOF) {
                        writeFileFailed = TRUE;
                     }
                  } else {
                     if (fprintf(fp, "%c, ", hexValue[data & 0xf]) == EOF) {
                        writeFileFailed = TRUE;
                     }
                  }
                  bit_count = 0;
                  data = 0;
               }
            }
            if (((w-left-right) % 8) != 0) {
               if (byte_count++ == 12) {
                  byte_count = 1;
                  if (fprintf(fp, "\n   ") == EOF) writeFileFailed = TRUE;
               }
               for (j=((w-left-right)%8); j < 8; j++) data |= (1<<j);

               if (fprintf(fp, "0x%c", hexValue[(data>>4) & 0xf]) == EOF) {
                  writeFileFailed = TRUE;
               }
               if (i == h-bottom-1) {
                  if (fprintf(fp, "%c};\n", hexValue[data & 0xf]) == EOF) {
                     writeFileFailed = TRUE;
                  }
               } else {
                  if (fprintf(fp, "%c, ", hexValue[data & 0xf]) == EOF) {
                     writeFileFailed = TRUE;
                  }
               }
            }
         }
      } else {
         if (thresholdBitmap) {
            sprintf(gszMsgBox, "    Threshold bitmap (threshold=%s) ...",
                  bitmapThresholdStr);
            Msg(gszMsgBox);
         }
         byte_count = 0;
         target_percent = 5;
         for (i = top; i < h-bottom; i++) {
            int percent=((i-top)*10000/(h-top-bottom))/100;

            if (percent >= target_percent) {
               sprintf(gszMsgBox, "Progress: %1d%%", percent);
               SetStringStatus(gszMsgBox);
               XSync(mainDisplay, False);
               while (target_percent <= percent) target_percent += 5;
            }
            bit_count = 0;
            data = 0;

            for (j = left; j < w-right; j++) {
               if (thresholdBitmap) {
                  int found_index=INVALID;
                  float gray;

                  pixel = XGetPixel(image, j, i);
                  for (k = 0; k < maxColors; k++) {
                     if (colorPixels[k] == pixel) {
                        found_index = k;
                        break;
                     }
                  }
                  if (found_index == INVALID) {
                     if (pixel != myBgPixel) {
                        sprintf(gszMsgBox,
                              "Unrecognized pixel value %1d!  1 used.", pixel);
                        Msg(gszMsgBox);
                        data |= (1<<bit_count);
                     }
                  } else {
                     gray = 0.299*((float)tgifColors[found_index].red/maxRGB) +
                           0.587*((float)tgifColors[found_index].green/maxRGB) +
                           0.114*((float)tgifColors[found_index].blue/maxRGB);
                     if (gray < bitmapThreshold) {
                        data |= (1<<bit_count);
                     }
                  }
               } else if (XGetPixel (image,j,i) != myBgPixel) {
                  data |= (1<<bit_count);
               }
               if (++bit_count == 8) {
                  if (byte_count++ == 12) {
                     byte_count = 1;
                     if (fprintf(fp, "\n   ") == EOF) writeFileFailed = TRUE;
                  }
                  if (fprintf(fp, "0x%c", hexValue[(data>>4) & 0xf]) == EOF) {
                     writeFileFailed = TRUE;
                  }
                  if (i == h-bottom-1 && j == w-right-1) {
                     if (fprintf(fp, "%c};\n", hexValue[data & 0xf]) == EOF) {
                        writeFileFailed = TRUE;
                     }
                  } else {
                     if (fprintf(fp, "%c, ", hexValue[data & 0xf]) == EOF) {
                        writeFileFailed = TRUE;
                     }
                  }
                  bit_count = 0;
                  data = 0;
               }
            }
            if (((w-left-right) % 8) != 0) {
               if (byte_count++ == 12) {
                  byte_count = 1;
                  if (fprintf(fp, "\n   ") == EOF) writeFileFailed = TRUE;
               }
               for (j=((w-left-right)%8); j < 8; j++) data |= (1<<j);

               if (fprintf(fp, "0x%c", hexValue[(data>>4) & 0xf]) == EOF) {
                  writeFileFailed = TRUE;
               }
               if (i == h-bottom-1) {
                  if (fprintf(fp, "%c};\n", hexValue[data & 0xf]) == EOF) {
                     writeFileFailed = TRUE;
                  }
               } else {
                  if (fprintf(fp, "%c, ", hexValue[data & 0xf]) == EOF) {
                     writeFileFailed = TRUE;
                  }
               }
            }
         }
      }
   }
   RestoreStatusStrings();
   fclose(fp);

   XDestroyImage(image);
   XFreePixmap(mainDisplay, pixmap);

   if (writeFileFailed) {
      writeFileFailed = FALSE;
      sprintf(gszMsgBox, "Fail to write to '%s'.\n\nFile system may be full.",
            xbm_file_name);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   } else {
      if (short_name) {
         sprintf(gszMsgBox, "%s file [%1dx%1d] printed into '%s'.",
               colorDump ? "XPM" : "XBM", w-left-right, h-top-bottom, rest);
      } else {
         sprintf(gszMsgBox, "%s file [%1dx%1d] printed into '%s'.",
               colorDump ? "XPM" : "XBM", w-left-right, h-top-bottom,
               xbm_file_name);
      }
      Msg(gszMsgBox);
      if (colorDump && (whereToPrint==GIF_FILE || whereToPrint==HTML_FILE)) {
         Msg("Generating imagemap...");
         SaveStatusStrings();
         GenerateImageMap(xbm_file_name, ltx+left, lty+top,
               ltx+w-right, lty+h-bottom);
         RestoreStatusStrings();
         unlink(xbm_file_name);
      }
   }
   if (gnInImageProc) {
      strcpy(gszImageProcXPmFile, xbm_file_name);
      if (gnConvolving) {
         CleanUpConvolution();
      }
   }
   if (bitmap_image != NULL) XDestroyImage(bitmap_image);
}

void DumpXBmObj (FP, ObjPtr)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
{  /* called when printing in the PostScript format */
   register int		j, data, bit_count;
   int			i, ltx, lty, rbx, rby, w, h, block_w, block_h;
   int			num_nibbles, nibble_count, fill, h_blocks, v_blocks;
   int			row, col, x, y, nibbles_per_row, image_w, image_h;
   int			color_index, rotate, flip;
   int			orig_x, orig_y;
   Pixmap		bitmap;
   char			* xbm_data;
   XImage		* image=NULL;
   struct XBmRec	* xbm_ptr=ObjPtr->detail.xbm;
   struct MtrxRec	mtrx;

   if (xbm_ptr->real_type == XBM_EPS)
   {
      DumpEPSObj (FP, ObjPtr);
      return;
   }

   if (ObjPtr->ctm == NULL) {
      ltx = ObjPtr->obbox.ltx;
      lty = ObjPtr->obbox.lty;
      rbx = ObjPtr->obbox.rbx;
      rby = ObjPtr->obbox.rby;
   } else {
      ltx = ObjPtr->orig_obbox.ltx;
      lty = ObjPtr->orig_obbox.lty;
      rbx = ObjPtr->orig_obbox.rbx;
      rby = ObjPtr->orig_obbox.rby;
   }

   bitmap = xbm_ptr->bitmap;
   fill = xbm_ptr->fill;
   rotate = xbm_ptr->rotate;
   flip = xbm_ptr->flip;
   image_w = xbm_ptr->image_w;
   image_h = xbm_ptr->image_h;

   w = rbx - ltx;
   h = rby - lty;

   if (!PRTGIF)
   {
      if ((image = xbm_ptr->image) == NULL)
         if ((image = xbm_ptr->image = XGetImage (mainDisplay, bitmap, 0, 0,
               image_w, image_h, 1, ZPixmap)) == NULL)
         {
            Msg ("XGetImage() failed!  May have run out of memory!");
            Msg ("X bitmap object skipped for printing.");
            return;
         }
   }

   color_index = ObjPtr->color;

   if (ObjPtr->ctm == NULL) {
      mtrx.image_w = (float)image_w; mtrx.image_h = (float)image_h;
      mtrx.w = (float)w; mtrx.h = (float)h;
      mtrx.rotate = rotate; mtrx.flip = flip;

      CalcTransform (&mtrx);

      orig_x = (mtrx.transformed_w >= 0.0) ? ltx : ltx+w;
      orig_y = (mtrx.transformed_h >= 0.0) ? lty : lty+h;
   }
   fprintf (FP, "%% XBM\n");
   if (ObjPtr->ctm != NULL) {
      float m[6];

      fprintf(FP, "gsave\n");
      m[CTM_SX] = ((float)ObjPtr->ctm->m[CTM_SX])/((float)1000.0);
      m[CTM_SY] = ((float)ObjPtr->ctm->m[CTM_SY])/((float)1000.0);
      m[CTM_SIN] = ((float)ObjPtr->ctm->m[CTM_SIN])/((float)1000.0);
      m[CTM_MSIN] = ((float)ObjPtr->ctm->m[CTM_MSIN])/((float)1000.0);
      fprintf (FP, "   %1d %1d translate\n", ObjPtr->x, ObjPtr->y);
      fprintf (FP, "   [%.3f %.3f %.3f %.3f %1d %1d] concat\n",
            m[CTM_SX], m[CTM_SIN], m[CTM_MSIN], m[CTM_SY],
            ObjPtr->ctm->m[CTM_TX], ObjPtr->ctm->m[CTM_TY]);
      fprintf (FP, "   %1d neg %1d neg translate\n", ObjPtr->x, ObjPtr->y);
   }
   DumpRGBColorLine(FP, color_index, 0, TRUE);

   switch (fill)
   {
      case NONEPAT: break;
      case SOLIDPAT:
         fprintf (FP, "newpath\n");
         fprintf (FP, "   %1d %1d moveto ", ltx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, rby);
         fprintf (FP, "%1d %1d lineto\n", ltx, rby);
         fprintf (FP, "closepath fill\n");
         break;
      case BACKPAT:
         fprintf (FP, "newpath\n");
         fprintf (FP, "   %1d %1d moveto ", ltx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, rby);
         fprintf (FP, "%1d %1d lineto\n", ltx, rby);
         fprintf (FP, "closepath 1 setgray fill\n\n");
         DumpRGBColorLine(FP, color_index, 0, TRUE);
         break;
      default:
         /* patterned */
         fprintf (FP, "gsave\n");
         if (colorDump || !useGray)
         {
            fprintf (FP, "   newpath\n");
            fprintf (FP, "      %1d %1d moveto ", ltx, lty);
            fprintf (FP, "%1d %1d lineto ", rbx, lty);
            fprintf (FP, "%1d %1d lineto ", rbx, rby);
            fprintf (FP, "%1d %1d lineto\n", ltx, rby);
            fprintf (FP, "   closepath 1 setgray fill\n\n");
            DumpRGBColorLine(FP, color_index, 3, TRUE);
         }
         else
         {
            GrayCheck (fill);
            fprintf (FP, "   %s setgray\n", GrayStr(fill));
         }
         fprintf (FP, "   newpath\n");
         fprintf (FP, "      %1d %1d moveto ", ltx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, lty);
         fprintf (FP, "%1d %1d lineto ", rbx, rby);
         fprintf (FP, "%1d %1d lineto\n", ltx, rby);
         if (colorDump || !useGray)
         {
            if (preDumpSetup) PSUseColorPattern();
            fprintf (FP, "   closepath eoclip newpath\n");
            DumpPatFill (FP, fill, 8, ObjPtr->bbox, "   ");
         }
         else
            fprintf (FP, "   closepath fill\n");
         fprintf (FP, "grestore\n");
         break;
   }

   h_blocks = ((image_w&0xff) == 0) ? (image_w>>8) : ((image_w>>8)+1);
   v_blocks = ((image_h&0xff) == 0) ? (image_h>>8) : ((image_h>>8)+1);
   nibbles_per_row = ((image_w%4)==0) ? (int)(image_w>>2) : (int)(image_w>>2)+1;

   fprintf (FP, "gsave\n");
   if (ObjPtr->ctm == NULL) {
      fprintf (FP, "   %1d %1d translate %.3f %.3f scale %1d rotate\n\n",
            orig_x, orig_y, mtrx.dump_h_scale, mtrx.dump_v_scale, mtrx.degree);
   } else {
      fprintf (FP, "   %1d %1d translate\n", ObjPtr->x, ObjPtr->y);
   }

   for (row = 0; row < v_blocks; row++)
   {
      y = row<<8;
      block_h = (row == v_blocks-1) ? image_h-y : 0x100;

      for (col = 0; col < h_blocks; col++)
      {
         x = col<<8;
         block_w = (col == h_blocks-1) ? image_w-x : 0x100;

         num_nibbles = ((block_w%4) == 0) ? (int)(block_w>>2) :
               (int)(block_w>>2)+1;

         fprintf (FP, "   gsave\n");
         fprintf (FP, "   %1d %1d translate\n", x, y);
         fprintf (FP, "   %1d %1d true [1 0 0 1 0 0]\n   {<", block_w, block_h);
         if (PRTGIF)
         {
            xbm_data = ObjPtr->detail.xbm->data;

            nibble_count = 0;
            for (i = 0; i < block_h; i++)
            {
               for (j = 0; j < num_nibbles; j++)
               {
                  if (nibble_count++ == 64)
                  {
                     nibble_count = 1;
                     fprintf (FP, "\n     ");
                  }
                  fprintf (FP, "%c",
                        xbm_data[(i+y)*nibbles_per_row+j+(x>>2)]);
               }
               if ((num_nibbles & 0x1) == 1)
               {
                  if (nibble_count++ == 64)
                  {
                     nibble_count = 1;
                     fprintf (FP, "\n     ");
                  }
                  fprintf (FP, "0");
               }
            }
         }
         else
         {
            nibble_count = 0;
            for (i = 0; i < block_h; i++)
            {
               bit_count = 0;
               data = 0;
               for (j = 0; j < block_w; j++)
               {
                  data = (XGetPixel (image, j+x, i+y) == 1) ? (data<<1) | 1 :
                        (data<<1);
                  if (++bit_count == 4)
                  {
                     if (nibble_count++ == 64)
                     {
                        nibble_count = 1;
                        fprintf (FP, "\n     ");
                     }
                     fprintf (FP, "%c", hexValue[data]);
                     bit_count = 0;
                     data = 0;
                  }
               }
               if ((block_w % 4) != 0)
               {
                  data <<= (4 - (block_w % 4));
                  if (nibble_count++ == 64)
                  {
                     nibble_count = 1;
                     fprintf (FP, "\n     ");
                  }
                  fprintf (FP, "%c", hexValue[data]);
               }
               if ((num_nibbles & 0x1) == 1)
               {
                  if (nibble_count++ == 64)
                  {
                     nibble_count = 1;
                     fprintf (FP, "\n     ");
                  }
                  fprintf (FP, "0");
               }
            }
         }
         fprintf (FP, ">}\n");
         fprintf (FP, "   imagemask\n");
         fprintf (FP, "   grestore\n");
         if (row!=v_blocks-1 || col!=h_blocks-1) fprintf (FP, "\n");
      }
   }
   fprintf (FP, "grestore\n");
   if (ObjPtr->ctm != NULL) fprintf(FP, "grestore\n");
   fprintf (FP, "\n");
}

int NeedsToCacheXBmObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   register struct XBmRec	* xbm_ptr = ObjPtr->detail.xbm;
   int				w, h;

   w = ObjPtr->obbox.rbx - ObjPtr->obbox.ltx;
   h = ObjPtr->obbox.rby - ObjPtr->obbox.lty;

   return (ObjPtr->ctm != NULL || zoomScale != 0 ||
         xbm_ptr->image_w != w || xbm_ptr->image_h != h ||
         xbm_ptr->rotate != ROTATE0 || xbm_ptr->flip != 0
   );
}

static
void DrawHiddenXBm (win, ctm, vs, x, y, w, h, s)
   Window		win;
   struct XfrmMtrxRec	* ctm;
   XPoint		* vs;
   int			x, y, w, h;
   char			* s;
{
   int		str_w, len, sx, sy;
   XGCValues	values;

   values.foreground = myFgPixel;
   values.function = GXcopy;;
   values.fill_style = FillSolid;
#ifdef NO_THIN_LINE
   values.line_width = 1;
#else
   values.line_width = 0;
#endif
   values.font = rulerFontPtr->fid;
   values.line_style = LineSolid;

   XChangeGC (mainDisplay, drawGC,
         GCForeground | GCFunction | GCFillStyle | GCLineWidth | GCFont |
         GCLineStyle, &values);
   if (ctm != NULL)
      XDrawLines (mainDisplay, win, drawGC, vs, 5, CoordModeOrigin);
   else
      XDrawRectangle (mainDisplay, win, drawGC, x, y, w, h);

   len = strlen (s);
   str_w = rulerFontWidth*len;
   if (str_w < w && rulerFontHeight < h)
   {
      sx = x + ((w-str_w)>>1);
      sy = y + ((h-rulerFontHeight)>>1);
      XDrawString (mainDisplay, win, drawGC, sx, sy+rulerFontAsc, s, len);
   }
   XSetFont (mainDisplay, drawGC, canvasFontPtr->fid);
}

void DrawXBmObj (win, XOff, YOff, ObjPtr)
   Window		win;
   int			XOff, YOff;
   struct ObjRec	* ObjPtr;
{
   int			ltx, lty, rbx, rby, w, h, scr_w, scr_h;
   int			real_x_off, real_y_off;
   int			fill;
   char			s[80];
   struct XBmRec	* xbm_ptr = ObjPtr->detail.xbm;
   XGCValues		values;

   if (ObjPtr->prev != NULL && ObjPtr->prev->type == OBJ_XBM &&
         ObjPtr->prev->detail.xbm->real_type == XBM_XBM &&
         ObjPtr->prev->detail.xbm->fill != NONEPAT &&
         Inside(ObjPtr->obbox, ObjPtr->prev->obbox))
   {
      return;
   }

   fill = xbm_ptr->fill;

   w = ObjPtr->obbox.rbx - ObjPtr->obbox.ltx;
   h = ObjPtr->obbox.rby - ObjPtr->obbox.lty;

   real_x_off = (zoomedIn ? XOff : (XOff>>zoomScale)<<zoomScale);
   real_y_off = (zoomedIn ? YOff : (YOff>>zoomScale)<<zoomScale);
   ltx = ZOOMED_SIZE(ObjPtr->obbox.ltx - real_x_off);
   lty = ZOOMED_SIZE(ObjPtr->obbox.lty - real_y_off);
   rbx = ZOOMED_SIZE(ObjPtr->obbox.rbx - real_x_off);
   rby = ZOOMED_SIZE(ObjPtr->obbox.rby - real_y_off);
   scr_w = rbx - ltx;
   scr_h = rby - lty;

   if (xbm_ptr->real_type==XBM_XBM && fill != NONEPAT)
   {
      values.foreground = (fill == 2) ? myBgPixel : colorPixels[ObjPtr->color];
      values.function = GXcopy;
      values.fill_style = FillOpaqueStippled;
      values.stipple = patPixmap[fill];
      XChangeGC (mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle | GCStipple, &values);
      if (ObjPtr->ctm != NULL)
         XFillPolygon (mainDisplay, win, drawGC,  ObjPtr->rotated_obbox, 5,
               Convex, CoordModeOrigin);
      else
         XFillRectangle (mainDisplay, win, drawGC, ltx, lty, scr_w, scr_h);
   }

   if (!mapShown)
   {
      if (xbm_ptr->real_type == XBM_EPS)
         DrawHiddenXBm (win, ObjPtr->ctm, ObjPtr->rotated_obbox,
               ltx, lty, scr_w, scr_h, xbm_ptr->filename);
      else
      {
         sprintf (s, "(%1dx%1d)", xbm_ptr->image_w, xbm_ptr->image_h);
         DrawHiddenXBm (win, ObjPtr->ctm, ObjPtr->rotated_obbox,
               ltx, lty, scr_w, scr_h, s);
      }
      return;
   }

   if (NeedsToCacheXBmObj (ObjPtr) && xbm_ptr->cached_bitmap == None)
      MakeCachedBitmap (ObjPtr);

   if (xbm_ptr->real_type==XBM_XBM || (xbm_ptr->real_type==XBM_EPS &&
         xbm_ptr->bitmap!=None))
   {
      values.foreground = colorPixels[ObjPtr->color];
      values.function = GXcopy;
      values.fill_style = FillStippled;
      values.ts_x_origin = ltx;
      values.ts_y_origin = lty;

      if (ObjPtr->ctm==NULL && zoomScale==0 && xbm_ptr->rotate==ROTATE0 &&
            xbm_ptr->flip==0 && xbm_ptr->image_w==w && xbm_ptr->image_h==h)
         values.stipple = xbm_ptr->bitmap;
      else
      {
         if (xbm_ptr->cached_bitmap == None) return;
         values.stipple = xbm_ptr->cached_bitmap;
      }

      XChangeGC (mainDisplay, drawGC,
            GCForeground | GCFunction | GCFillStyle | GCStipple |
            GCTileStipXOrigin | GCTileStipYOrigin, &values);
      XFillRectangle (mainDisplay, win, drawGC, ltx, lty, scr_w, scr_h);
      XSetTSOrigin (mainDisplay, drawGC, 0, 0);
   }
   else if (xbm_ptr->real_type==XBM_EPS && xbm_ptr->bitmap==None)
      DrawHiddenXBm (win, ObjPtr->ctm, ObjPtr->rotated_obbox,
               ltx, lty, rbx-ltx, rby-lty, xbm_ptr->filename);
}

struct ObjRec * CreateXBmObj (ImageW, ImageH, W, H, bitmap, image)
   int		ImageW, ImageH, W, H;
   Pixmap	bitmap;
   XImage	* image;
{
   struct XBmRec	* xbm_ptr;
   struct ObjRec	* obj_ptr;

   xbm_ptr = (struct XBmRec *)malloc(sizeof(struct XBmRec));
   if (xbm_ptr == NULL) FailAllocMessage();
   memset(xbm_ptr, 0, sizeof(struct XBmRec));

   xbm_ptr->image = image;
   xbm_ptr->image_w = ImageW;
   xbm_ptr->image_h = ImageH;
   xbm_ptr->bitmap = bitmap;
   xbm_ptr->data = NULL;

   xbm_ptr->eps_w = xbm_ptr->eps_h = 0;

   xbm_ptr->fill = objFill;
   xbm_ptr->rotate = xbm_ptr->cached_rotate = ROTATE0;
   xbm_ptr->flip = xbm_ptr->cached_flip = 0;
   xbm_ptr->cached_zoom = 0;
   xbm_ptr->cached_bitmap = None;
   xbm_ptr->cached_w = xbm_ptr->cached_h = 0;

   xbm_ptr->real_type = XBM_XBM;
   xbm_ptr->filename = NULL;
   xbm_ptr->epsflines = NULL;
   xbm_ptr->num_epsf_lines = 0;

   obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (obj_ptr == NULL) FailAllocMessage();
   memset(obj_ptr, 0, sizeof(struct ObjRec));

   obj_ptr->bbox.ltx = obj_ptr->obbox.ltx = obj_ptr->x = drawOrigX;
   obj_ptr->bbox.lty = obj_ptr->obbox.lty = obj_ptr->y = drawOrigY;
   obj_ptr->bbox.rbx = obj_ptr->obbox.rbx = W + drawOrigX;
   obj_ptr->bbox.rby = obj_ptr->obbox.rby = H + drawOrigY;
   obj_ptr->type = OBJ_XBM;
   obj_ptr->color = colorIndex;
   obj_ptr->id = objId++;
   obj_ptr->dirty = FALSE;
   obj_ptr->rotation = 0;
   obj_ptr->locked = FALSE;
   obj_ptr->detail.xbm = xbm_ptr;
   obj_ptr->fattr = obj_ptr->lattr = NULL;
   obj_ptr->ctm = NULL;
   obj_ptr->invisible = FALSE;

   if (importXBmRV) InvertXBmObject (obj_ptr);
   return (obj_ptr);
}

static
void DumpXBmData (FP, bitmap, image, W, H)
   FILE		* FP;
   Pixmap	bitmap;
   XImage	* * image;
   int		W, H;
{
   register int	nibble_count, bit_count, data, i, j;

   if (*image == NULL)
      if ((*image = XGetImage (mainDisplay,bitmap,0,0,W,H,1,ZPixmap)) == NULL)
      {
         Msg ("XGetImage() failed!  May have run out of memory!");
         Msg ("Output file maybe corrupted.");
         return;
      }

   nibble_count = 0;

   for (i = 0; i < H; i++)
   {
      bit_count = 0;
      data = 0;

      for (j = 0; j < W; j++)
      {
         data = (XGetPixel (*image, j, i) == 1) ? (data<<1) | 1 : (data<<1);

         if (++bit_count == 4)
         {
            if (nibble_count++ == 64)
            {
               nibble_count = 1;
               if (fprintf (FP, "\n     ") == EOF) writeFileFailed = TRUE;
            }
            if (fprintf (FP, "%c", hexValue[data]) == EOF) writeFileFailed = TRUE;
            bit_count = 0;
            data = 0;
         }
      }
      if ((W % 4) != 0)
      {
         data <<= (4 - (W % 4));
         if (nibble_count++ == 64)
         {
            nibble_count = 1;
            if (fprintf (FP, "\n     ") == EOF) writeFileFailed = TRUE;
         }
         if (fprintf (FP, "%c", hexValue[data]) == EOF) writeFileFailed = TRUE;
      }
   }
}

void SaveXBmObj (FP, ObjPtr)
   FILE			* FP;
   struct ObjRec	* ObjPtr;
{
   register int		i;
   int			ltx, lty, rbx, rby, image_w, image_h;
   int			no_bitmap, compressed=FALSE;
   struct XBmRec	* xbm_ptr=ObjPtr->detail.xbm;

   no_bitmap = (xbm_ptr->real_type==XBM_EPS &&
         !(xbm_ptr->save_epsf && xbm_ptr->bitmap != None));

   ltx = ObjPtr->obbox.ltx; lty = ObjPtr->obbox.lty;
   rbx = ObjPtr->obbox.rbx; rby = ObjPtr->obbox.rby;
   if (no_bitmap)
   {
      image_w = xbm_ptr->eps_w;
      image_h = xbm_ptr->eps_h;
   }
   else
   {
      image_w = xbm_ptr->image_w;
      image_h = xbm_ptr->image_h;
   }
   if (fprintf (FP, "xbm('%s',", colorMenuItems[ObjPtr->color]) == EOF)
      writeFileFailed = TRUE;
   if (fprintf (FP,
         "%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,\n    ",
         ltx, lty, rbx, rby, xbm_ptr->fill, ObjPtr->id, ObjPtr->rotation,
         image_w, image_h, xbm_ptr->rotate, xbm_ptr->flip, xbm_ptr->real_type,
         xbm_ptr->llx, xbm_ptr->lly, xbm_ptr->urx, xbm_ptr->ury,
         no_bitmap, ObjPtr->locked, xbm_ptr->save_epsf, compressed,
         ObjPtr->ctm!=NULL, ObjPtr->invisible) == EOF) {
      writeFileFailed = TRUE;
   }
   switch (xbm_ptr->real_type)
   {
      case XBM_XBM:
         if (fprintf (FP, "\"\",\"\",") == EOF) writeFileFailed = TRUE;
         break;
      case XBM_EPS:
         if (fprintf (FP, "\"%s\",\"%s\",", xbm_ptr->write_date,
               xbm_ptr->filename) == EOF)
            writeFileFailed = TRUE;
         break;
   }
   if (xbm_ptr->save_epsf)
   {
      if (fprintf (FP, "%1d,[", xbm_ptr->num_epsf_lines) == EOF)
         writeFileFailed = TRUE;
      for (i = 0; i < xbm_ptr->num_epsf_lines; i++)
         if (fprintf (FP, "\n    \"%s\"%s,", xbm_ptr->epsflines[i],
               (i==xbm_ptr->num_epsf_lines-1 ? "]" : "")) == EOF)
            writeFileFailed = TRUE;
   }
   if (!no_bitmap)
   {
      if (fprintf (FP, "\n    \"") == EOF) writeFileFailed = TRUE;
      DumpXBmData (FP, xbm_ptr->bitmap, &(xbm_ptr->image), image_w, image_h);
      if (fprintf (FP, "\",") == EOF) writeFileFailed = TRUE;
   }
   if (ObjPtr->ctm != NULL && fprintf(FP,
         "[\n    %1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d],",
         ObjPtr->x, ObjPtr->y,
         ObjPtr->orig_obbox.ltx, ObjPtr->orig_obbox.lty,
         ObjPtr->orig_obbox.rbx, ObjPtr->orig_obbox.rby,
         ObjPtr->ctm->m[CTM_SX], ObjPtr->ctm->m[CTM_SIN],
         ObjPtr->ctm->m[CTM_MSIN], ObjPtr->ctm->m[CTM_SY],
         ObjPtr->ctm->m[CTM_TX], ObjPtr->ctm->m[CTM_TY]) == EOF) {
      writeFileFailed = TRUE;
   }
   SaveAttrs (FP, ObjPtr->lattr);
   if (fprintf (FP, ")") == EOF) writeFileFailed = TRUE;
}

#define GETVALUE(val,name) ScanValue("%d", &(val), name, "xbitmap")

static
int ReadTransformAndAdjustForXBm(FP, ObjPtr, xbm_ptr, transformed, rotate, flip)
   FILE *FP;
   struct ObjRec **ObjPtr;
   struct XBmRec *xbm_ptr;
   int transformed, rotate, flip;
{
   if (fileVersion >= 33 && transformed)
   {
      char inbuf[MAXSTRING];
      struct XfrmMtrxRec *ctm=NULL;
      int real_x=0, real_y=0;
      struct BBRec orig_obbox;

      fgets(inbuf, MAXSTRING, FP);
      scanLineNum++;
      InitScan(inbuf, "\t\n, ");

      ctm = (struct XfrmMtrxRec *)malloc(sizeof(struct XfrmMtrxRec));
      if (ctm == NULL) FailAllocMessage();
      if (GETVALUE(real_x,           "real_x") == INVALID ||
          GETVALUE(real_y,           "real_y") == INVALID ||
          GETVALUE(orig_obbox.ltx,   "orig_obbox.ltx") == INVALID ||
          GETVALUE(orig_obbox.lty,   "orig_obbox.lty") == INVALID ||
          GETVALUE(orig_obbox.rbx,   "orig_obbox.rbx") == INVALID ||
          GETVALUE(orig_obbox.rby,   "orig_obbox.rby") == INVALID ||
          GETVALUE(ctm->m[CTM_SX],   "CTM_SX") == INVALID ||
          GETVALUE(ctm->m[CTM_SIN],  "CTM_SIN") == INVALID ||
          GETVALUE(ctm->m[CTM_MSIN], "CTM_MSIN") == INVALID ||
          GETVALUE(ctm->m[CTM_SY],   "CTM_SY") == INVALID ||
          GETVALUE(ctm->m[CTM_TX],   "CTM_TX") == INVALID ||
          GETVALUE(ctm->m[CTM_TY],   "CTM_TY") == INVALID) {
         return;
      }
      (*ObjPtr)->ctm = ctm;
      if (ctm != NULL) {
         memcpy(&(*ObjPtr)->orig_obbox, &orig_obbox, sizeof(struct BBRec));
         (*ObjPtr)->x = real_x;
         (*ObjPtr)->y = real_y;
         GetTransformedOBBoxOffsetVs(*ObjPtr, (*ObjPtr)->rotated_obbox);
      }
   }
   if (fileVersion < 33 && (rotate != ROTATE0 || flip != NO_FLIP)) {
      int ltx, lty;

      if (rotate == ROTATE90 || rotate == ROTATE270) {
         int h=(*ObjPtr)->obbox.rbx-(*ObjPtr)->obbox.ltx;
         int w=(*ObjPtr)->obbox.rby-(*ObjPtr)->obbox.lty;

         (*ObjPtr)->obbox.rby = (*ObjPtr)->obbox.lty + h;
         (*ObjPtr)->obbox.rbx = (*ObjPtr)->obbox.ltx + w;
      }
      ltx = ((*ObjPtr)->obbox.ltx);
      lty = ((*ObjPtr)->obbox.lty);
      SetRotatePivotByObject(*ObjPtr);

      if (flip & HORI_EVEN) {
         ShearObj(*ObjPtr, CORNER_LEFT, 0, 0, -1000, 1000, &ltx, &lty);
      }
      if (flip & VERT_EVEN) {
         ShearObj(*ObjPtr, CORNER_TOP, 0, 0, 1000, -1000, &ltx, &lty);
      }
      if (rotate == ROTATE0) {
         if (flip & (HORI_ODD | VERT_ODD)) {
            RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
            if (flip & HORI_ODD) {
               ShearObj(*ObjPtr, CORNER_LEFT, 0, 0, -1000, 1000, &ltx, &lty);
            }
            if (flip & VERT_ODD) {
               ShearObj(*ObjPtr, CORNER_TOP, 0, 0, 1000, -1000, &ltx, &lty);
            }
            RotateObj(*ObjPtr, CORNER_LT, COUNTER90, &ltx, &lty);
         }
      } else {
         switch (rotate) {
         case ROTATE90:
            RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
            if (flip & HORI_ODD) {
               ShearObj(*ObjPtr, CORNER_LEFT, 0, 0, -1000, 1000, &ltx, &lty);
            }
            if (flip & VERT_ODD) {
               ShearObj(*ObjPtr, CORNER_TOP, 0, 0, 1000, -1000, &ltx, &lty);
            }
            break;
         case ROTATE180:
            RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
            if (flip & HORI_ODD) {
               ShearObj(*ObjPtr, CORNER_LEFT, 0, 0, -1000, 1000, &ltx, &lty);
            }
            if (flip & VERT_ODD) {
               ShearObj(*ObjPtr, CORNER_TOP, 0, 0, 1000, -1000, &ltx, &lty);
            }
            RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
            break;
         case ROTATE270:
            RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
            if (flip & HORI_ODD) {
               ShearObj(*ObjPtr, CORNER_LEFT, 0, 0, -1000, 1000, &ltx, &lty);
            }
            if (flip & VERT_ODD) {
               ShearObj(*ObjPtr, CORNER_TOP, 0, 0, 1000, -1000, &ltx, &lty);
            }
            RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
            RotateObj(*ObjPtr, CORNER_LT, CLOCKWISE90, &ltx, &lty);
            break;
         }
      }
      xbm_ptr->rotate = ROTATE0;
      xbm_ptr->flip = NO_FLIP;
   }
   return TRUE;
}

void ReadXBmObj (FP, Inbuf, ObjPtr)
   FILE			* FP;
   char			* Inbuf;
   struct ObjRec	* * ObjPtr;
{
   struct XBmRec	* xbm_ptr;
   char			color_str[40], * s, inbuf[MAXSTRING], * c_ptr;
   int			ltx, lty, rbx, rby, i, j, k, data=0, color_index;
   int			nibble_count, bit_count, num_nibbles, fill;
   int			rotation=0, new_alloc, id=0, image_w, image_h;
   int			rotate=ROTATE0, flip=NO_FLIP, real_type=XBM_XBM, len;
   int			no_bitmap=FALSE, llx=0, lly=0, urx=0, ury=0;
   int			locked=FALSE, save_epsf=FALSE, compressed=FALSE;
   int			num_epsf_lines=0, transform_read=FALSE;
   int			transformed=FALSE, invisible=FALSE;
   Pixmap		bitmap;
   char			* * epsflines=NULL;
   char			* xbm_data, write_date[32], * filename;
   XImage		* image;

   *ObjPtr = NULL;

   s = FindChar ((int)'(', Inbuf);
   s = ParseStr (s, (int)',', color_str, sizeof(color_str));

   InitScan (s, "\t\n, ");

   if (fileVersion <= 8)
   {
      sprintf (inbuf, "Invalid X Bitmap version (%1d).", fileVersion);
      if (PRTGIF)
         fprintf (stderr, "%s\n", inbuf);
      else
         Msg (inbuf);
      return;
   }
   else if (fileVersion <= 13)
   {
      if (GETVALUE (ltx,      "ltx") == INVALID ||
          GETVALUE (lty,      "lty") == INVALID ||
          GETVALUE (rbx,      "rbx") == INVALID ||
          GETVALUE (rby,      "rby") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (id,       "id") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 22)
   {
      if (GETVALUE (ltx,      "ltx") == INVALID ||
          GETVALUE (lty,      "lty") == INVALID ||
          GETVALUE (rbx,      "rbx") == INVALID ||
          GETVALUE (rby,      "rby") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 23)
   {
      if (GETVALUE (ltx,      "ltx") == INVALID ||
          GETVALUE (lty,      "lty") == INVALID ||
          GETVALUE (rbx,      "rbx") == INVALID ||
          GETVALUE (rby,      "rby") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID ||
          GETVALUE (image_w,  "image_w") == INVALID ||
          GETVALUE (image_h,  "image_h") == INVALID ||
          GETVALUE (rotate,   "rotate") == INVALID ||
          GETVALUE (flip,     "flip") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 25)
   {
      if (GETVALUE (ltx,       "ltx") == INVALID ||
          GETVALUE (lty,       "lty") == INVALID ||
          GETVALUE (rbx,       "rbx") == INVALID ||
          GETVALUE (rby,       "rby") == INVALID ||
          GETVALUE (fill,      "fill") == INVALID ||
          GETVALUE (id,        "id") == INVALID ||
          GETVALUE (rotation,  "rotation") == INVALID ||
          GETVALUE (image_w,   "image_w") == INVALID ||
          GETVALUE (image_h,   "image_h") == INVALID ||
          GETVALUE (rotate,    "rotate") == INVALID ||
          GETVALUE (flip,      "flip") == INVALID ||
          GETVALUE (real_type, "real_type") == INVALID ||
          GETVALUE (llx,       "llx") == INVALID ||
          GETVALUE (lly,       "lly") == INVALID ||
          GETVALUE (urx,       "urx") == INVALID ||
          GETVALUE (ury,       "ury") == INVALID ||
          GETVALUE (no_bitmap, "no_bitmap") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 28)
   {
      if (GETVALUE (ltx,       "ltx") == INVALID ||
          GETVALUE (lty,       "lty") == INVALID ||
          GETVALUE (rbx,       "rbx") == INVALID ||
          GETVALUE (rby,       "rby") == INVALID ||
          GETVALUE (fill,      "fill") == INVALID ||
          GETVALUE (id,        "id") == INVALID ||
          GETVALUE (rotation,  "rotation") == INVALID ||
          GETVALUE (image_w,   "image_w") == INVALID ||
          GETVALUE (image_h,   "image_h") == INVALID ||
          GETVALUE (rotate,    "rotate") == INVALID ||
          GETVALUE (flip,      "flip") == INVALID ||
          GETVALUE (real_type, "real_type") == INVALID ||
          GETVALUE (llx,       "llx") == INVALID ||
          GETVALUE (lly,       "lly") == INVALID ||
          GETVALUE (urx,       "urx") == INVALID ||
          GETVALUE (ury,       "ury") == INVALID ||
          GETVALUE (no_bitmap, "no_bitmap") == INVALID ||
          GETVALUE (locked,    "locked") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 32)
   {
      if (GETVALUE (ltx,       "ltx") == INVALID ||
          GETVALUE (lty,       "lty") == INVALID ||
          GETVALUE (rbx,       "rbx") == INVALID ||
          GETVALUE (rby,       "rby") == INVALID ||
          GETVALUE (fill,      "fill") == INVALID ||
          GETVALUE (id,        "id") == INVALID ||
          GETVALUE (rotation,  "rotation") == INVALID ||
          GETVALUE (image_w,   "image_w") == INVALID ||
          GETVALUE (image_h,   "image_h") == INVALID ||
          GETVALUE (rotate,    "rotate") == INVALID ||
          GETVALUE (flip,      "flip") == INVALID ||
          GETVALUE (real_type, "real_type") == INVALID ||
          GETVALUE (llx,       "llx") == INVALID ||
          GETVALUE (lly,       "lly") == INVALID ||
          GETVALUE (urx,       "urx") == INVALID ||
          GETVALUE (ury,       "ury") == INVALID ||
          GETVALUE (no_bitmap, "no_bitmap") == INVALID ||
          GETVALUE (locked,    "locked") == INVALID ||
          GETVALUE (save_epsf, "save_epsf") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else
   {
      if (GETVALUE (ltx,         "ltx") == INVALID ||
          GETVALUE (lty,         "lty") == INVALID ||
          GETVALUE (rbx,         "rbx") == INVALID ||
          GETVALUE (rby,         "rby") == INVALID ||
          GETVALUE (fill,        "fill") == INVALID ||
          GETVALUE (id,          "id") == INVALID ||
          GETVALUE (rotation,    "rotation") == INVALID ||
          GETVALUE (image_w,     "image_w") == INVALID ||
          GETVALUE (image_h,     "image_h") == INVALID ||
          GETVALUE (rotate,      "rotate") == INVALID ||
          GETVALUE (flip,        "flip") == INVALID ||
          GETVALUE (real_type,   "real_type") == INVALID ||
          GETVALUE (llx,         "llx") == INVALID ||
          GETVALUE (lly,         "lly") == INVALID ||
          GETVALUE (urx,         "urx") == INVALID ||
          GETVALUE (ury,         "ury") == INVALID ||
          GETVALUE (no_bitmap,   "no_bitmap") == INVALID ||
          GETVALUE (locked,      "locked") == INVALID ||
          GETVALUE (save_epsf,   "save_epsf") == INVALID ||
          GETVALUE (compressed,  "compressed") == INVALID ||
          GETVALUE (transformed, "transformed") == INVALID ||
          GETVALUE (invisible,   "invisible") == INVALID)
      {
         return;
      }
      if (id >= objId) objId = id+1;
   }
   if (fileVersion <= 22)
   {
      image_w = rbx-ltx;
      image_h = rby-lty;
      rotate=ROTATE0;
      flip=0;
   }
   if (fileVersion >= 24)
   {
      char	* tmp_str, * s, * s1;

      fgets (inbuf, MAXSTRING, FP);
      scanLineNum++;

      tmp_str = FindChar ((int)'"', inbuf);
      s = s1 = ReadString (tmp_str);
      s1++;
      *(--s) = '\0';
      len = strlen (tmp_str);
      if (len > 24)
      {
         (void) sprintf (gszMsgBox, "%s, %d:  %s too long in %s",
               scanFileName, scanLineNum, "write_date", "xbm");
         if (PRTGIF)
            fprintf (stderr, "%s\n", gszMsgBox);
         else
            Msg (gszMsgBox);
      }
      strcpy (write_date, tmp_str);

      tmp_str = FindChar ((int)'"', s1);
      s = ReadString (tmp_str);
      *(--s) = '\0';
      len = strlen (tmp_str);
      filename = (char*)malloc((len+1)*sizeof(char));
      if (filename == NULL) FailAllocMessage();
      strcpy (filename, tmp_str);
      if (save_epsf)
      {
         if (sscanf (&s[2], "%d", &num_epsf_lines) != 1)
         {
            sprintf (gszMsgBox, "%s, %d:  Bad num_epsf_lines in X Bitmap [%s]",
                  scanFileName, scanLineNum, &s[2]);
            if (PRTGIF)
               fprintf (stderr, "%s\n", gszMsgBox);
            else
               Msg (gszMsgBox);
            return;
         }
         epsflines = (char**)malloc(num_epsf_lines*sizeof(char*));
         if (epsflines == NULL) FailAllocMessage();
         for (i = 0; i < num_epsf_lines; i++) {
            if (fgets(inbuf, MAXSTRING, FP) == NULL) {
               sprintf(gszMsgBox, "Unexpected EOF at line %d of %s.",
                     scanLineNum, scanFileName);
               if (PRTGIF) {
                  fprintf(stderr, "%s\n", gszMsgBox);
               } else {
                  MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
               }
               return;
            }
            scanLineNum++;
            len = strlen (inbuf);
            if ((c_ptr = strchr (inbuf, '"')) == NULL)
            {
               sprintf (gszMsgBox, "Malformed input at line %d of %s.",
                     scanLineNum, scanFileName);
               if (PRTGIF)
                  fprintf (stderr, "%s\n", gszMsgBox);
               else
                  Msg (gszMsgBox);
               return;
            }
            c_ptr++;
            len -= (c_ptr-inbuf);
            if (c_ptr[len-1] != '\n')
            {
               int	cur_size=2*MAXSTRING-1, done=FALSE;
               char	*line=(char*)malloc((cur_size+1)*sizeof(char));

               if (line == NULL) FailAllocMessage();
               strcpy (line, c_ptr);
               c_ptr = &(line[len]);

               while (!done && fgets(inbuf, MAXSTRING, FP) != NULL) {
                  len = strlen(inbuf);
                  if (inbuf[len-1] == '\r' || inbuf[len-1] == '\n') {
                     done = TRUE;
                     inbuf[len-1] = '\0';
                     strcpy(c_ptr, inbuf);
                  } else {
                     int n=c_ptr-line;

                     cur_size += MAXSTRING-1;
                     line = (char*)realloc(line, cur_size+1);
                     c_ptr = line + n;
                     strcpy(c_ptr, inbuf);
                     c_ptr += MAXSTRING-1;
                  }
               }
               if (!done) {
                  sprintf(gszMsgBox, "Unexpected EOF at line %d of %s.",
                        scanLineNum, scanFileName);
                  if (PRTGIF) {
                     fprintf(stderr, "%s\n", gszMsgBox);
                  } else {
                     MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
                  }
                  return;
               }
               epsflines[i] = line;
               len = strlen(line);
            } else {
               c_ptr[--len] = '\0';
               epsflines[i] = (char*)malloc((len+1)*sizeof(char));
               if (epsflines[i] == NULL) FailAllocMessage();
               strncpy(epsflines[i], c_ptr, len);
               epsflines[i][len] = '\0';
            }
            for (c_ptr = (&(epsflines[i])[len]); c_ptr != epsflines[i] &&
                  *c_ptr != '"'; c_ptr--) ;
            if (*c_ptr != '"') {
               sprintf(gszMsgBox, "Malformed input at line %d of %s.",
                     scanLineNum, scanFileName);
               if (PRTGIF) {
                  fprintf(stderr, "%s\n", gszMsgBox);
               } else {
                  MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
               }
               return;
            }
            *c_ptr = '\0';
         }
      }
   } else {
      *write_date = '\0';
      filename = NULL;
   }
   fill = UpgradePenFill (fill);

   *ObjPtr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (*ObjPtr == NULL) FailAllocMessage();
   memset(*ObjPtr, 0, sizeof(struct ObjRec));
   xbm_ptr = (struct XBmRec *)malloc(sizeof(struct XBmRec));
   if (xbm_ptr == NULL) FailAllocMessage();
   memset(xbm_ptr, 0, sizeof(struct XBmRec));

   color_index = QuickFindColorIndex(*ObjPtr, color_str, &new_alloc, TRUE);

   (*ObjPtr)->color = color_index;
   (*ObjPtr)->dirty = FALSE;
   (*ObjPtr)->id = id;
   (*ObjPtr)->rotation = rotation;
   (*ObjPtr)->locked = locked;
   (*ObjPtr)->type = OBJ_XBM;
   (*ObjPtr)->obbox.ltx = (*ObjPtr)->bbox.ltx = (*ObjPtr)->x = ltx;
   (*ObjPtr)->obbox.lty = (*ObjPtr)->bbox.lty = (*ObjPtr)->y = lty;
   (*ObjPtr)->obbox.rbx = (*ObjPtr)->bbox.rbx = rbx;
   (*ObjPtr)->obbox.rby = (*ObjPtr)->bbox.rby = rby;
   (*ObjPtr)->detail.xbm = xbm_ptr;
   (*ObjPtr)->ctm = NULL;
   (*ObjPtr)->invisible = invisible;

   xbm_ptr->bitmap = None;
   xbm_ptr->image = NULL;
   xbm_ptr->cached_bitmap = None;
   xbm_ptr->cached_zoom = 0;
   xbm_ptr->data = NULL;
   xbm_ptr->fill = fill;
   xbm_ptr->rotate = rotate;
   xbm_ptr->flip = flip;
   xbm_ptr->cached_rotate = INVALID;
   xbm_ptr->cached_flip = 0;
   xbm_ptr->cached_w = xbm_ptr->cached_h = 0;
   xbm_ptr->image_w = image_w;
   xbm_ptr->image_h = image_h;
   xbm_ptr->llx = llx; xbm_ptr->lly = lly;
   xbm_ptr->urx = urx; xbm_ptr->ury = ury;

   xbm_ptr->real_type = real_type;
   strcpy (xbm_ptr->write_date, write_date);
   xbm_ptr->filename = filename;
   xbm_ptr->epsflines = epsflines;
   xbm_ptr->num_epsf_lines = num_epsf_lines;
   xbm_ptr->save_epsf = save_epsf;

   if (no_bitmap && xbm_ptr->real_type==XBM_EPS)
   {
      float	file_llx=0.0, file_lly=0.0, file_urx=0.0, file_ury=0.0;

      xbm_ptr->eps_w = xbm_ptr->image_w;
      xbm_ptr->eps_h = xbm_ptr->image_h;
      xbm_ptr->image_w = 0;
      xbm_ptr->image_h = 0;

      if (!PRTGIF) {
         transform_read = TRUE;
         if (!ReadTransformAndAdjustForXBm(FP, ObjPtr, xbm_ptr, transformed,
               rotate, flip)) {
            return;
         }
         if (MyReadEPSFile(xbm_ptr->filename, &image_w,  &image_h,
               &(xbm_ptr->bitmap), &(xbm_ptr->image),
               &(xbm_ptr->num_epsf_lines), &(xbm_ptr->epsflines),
               &(xbm_ptr->epsf_level), &file_llx, &file_lly, &file_urx,
               &file_ury, xbm_ptr->write_date) != BitmapSuccess) {
            return;
         } else if (xbm_ptr->bitmap == None) {
            xbm_ptr->eps_w = (file_urx >= file_llx) ?
                  (int)(file_urx-file_llx) : (int)(file_llx-file_urx);
            xbm_ptr->eps_h = (file_ury >= file_lly) ?
                  (int)(file_ury-file_lly) : (int)(file_lly-file_ury);
         } else {
            xbm_ptr->eps_w = xbm_ptr->image_w = image_w;
            xbm_ptr->eps_h = xbm_ptr->image_h = image_h;
         }
         xbm_ptr->llx = (int)(file_llx*1000.0);
         xbm_ptr->lly = (int)(file_lly*1000.0);
         xbm_ptr->urx = (int)(file_urx*1000.0);
         xbm_ptr->ury = (int)(file_ury*1000.0);
      }
   } else {
      if (xbm_ptr->real_type == XBM_EPS) {
         xbm_ptr->eps_w = xbm_ptr->image_w;
         xbm_ptr->eps_h = xbm_ptr->image_h;
      } else {
         xbm_ptr->eps_w = xbm_ptr->eps_h = 0;
      }
      num_nibbles = ((image_w % 4) == 0) ? (int)(image_w>>2) :
            (int)(image_w>>2)+1;

      fgets(inbuf, MAXSTRING, FP);
      scanLineNum++;
      c_ptr = &inbuf[5];
      nibble_count = 0;

      if (PRTGIF) {
         xbm_data = (char*)malloc((image_h*num_nibbles)*sizeof(char));
         if (xbm_data == NULL) FailAllocMessage();
         for (i = 0; i < image_h; i++) {
            for (j = 0; j < num_nibbles; j++) {
               if (nibble_count++ == 64) {
                  fgets(inbuf, MAXSTRING, FP);
                  scanLineNum++;
                  c_ptr = &inbuf[5];
                  nibble_count = 1;
               }
               xbm_data[i*num_nibbles+j] = *c_ptr++;
            }
         }
         xbm_ptr->data = xbm_data;
      } else {
         bitmap = XCreatePixmap(mainDisplay, dummyBitmap, image_w, image_h, 1);
         XFillRectangle(mainDisplay, bitmap, xbmGC, 0, 0, image_w, image_h);
         image = XGetImage(mainDisplay,bitmap,0,0,image_w,image_h,1,ZPixmap);

         for (i = 0; i < image_h; i++) {
            bit_count = 0;
            for (j = 0; j < num_nibbles; j++) {
               if (nibble_count++ == 64) {
                  fgets(inbuf, MAXSTRING, FP);
                  scanLineNum++;
                  c_ptr = &inbuf[5];
                  nibble_count = 1;
               }
               if (*c_ptr >= '0' && *c_ptr <= '9') {
                  data = (int)(*c_ptr++) - (int)('0');
               } else if (*c_ptr >= 'a' && *c_ptr <= 'f') {
                  data = (int)(*c_ptr++) - (int)('a') + 10;
               }
               for (k = 0; k < 4; k++) {
                  if (bit_count++ == image_w) break;

                  if (data & (1<<(3-k))) {
                     XPutPixel(image, j*4+k, i, 1);
                  }
               }
            }
         }
         XPutImage(mainDisplay,bitmap,xbmGC,image,0,0,0,0,image_w,image_h);
         xbm_ptr->bitmap = bitmap;
         xbm_ptr->image = image;
      }
   }
   if (!transform_read && !ReadTransformAndAdjustForXBm(FP, ObjPtr, xbm_ptr,
         transformed, rotate, flip)) {
      return;
   }
   if (xbm_ptr->rotate != ROTATE0) {
      fprintf(stderr, "XBm object rotation is not 0!\n");
   }
}

void FreeXBmObj (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   register struct XBmRec	* xbm_ptr = ObjPtr->detail.xbm;
   register int			i;

   if (xbm_ptr->bitmap != None) XFreePixmap (mainDisplay, xbm_ptr->bitmap);
   if (xbm_ptr->image != NULL) XDestroyImage (xbm_ptr->image);
   if (xbm_ptr->cached_bitmap != None) {
      XFreePixmap (mainDisplay, xbm_ptr->cached_bitmap);
   }
   xbm_ptr->bitmap = None;
   xbm_ptr->image = NULL;
   xbm_ptr->cached_bitmap = None;
   xbm_ptr->cached_zoom = 0;
   if (xbm_ptr->data != NULL) free(xbm_ptr->data);
   if (xbm_ptr->filename != NULL) free(xbm_ptr->filename);
   if (xbm_ptr->real_type == XBM_EPS) {
      for (i = 0; i < xbm_ptr->num_epsf_lines; i++) {
         if (xbm_ptr->epsflines[i] != NULL) {
            free(xbm_ptr->epsflines[i]);
         }
      }
      if (xbm_ptr->epsflines != NULL) free(xbm_ptr->epsflines);
   }
   free(xbm_ptr);
   free(ObjPtr);
}
