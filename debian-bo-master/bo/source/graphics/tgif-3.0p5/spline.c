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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/spline.c,v 3.0 1996/05/06 16:11:53 william Exp $";
#endif

#include <math.h>
#include <stdio.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "const.h"
#include "types.h"

#include "dialog.e"
#include "poly.e"
#include "polygon.e"
#include "raster.e"
#include "rect.e"
#include "setup.e"
#ifndef _NO_EXTERN
#include "spline.e"
#endif

#define SUM_MINUS_2 (theSum-((double)2.0))

int	intSplineTension = 3;
int	splineTol = 9;
int	splineRubberband = TRUE;

static double theSum=(double)6.0;

void CalcAutoRetractedArrowAttrBend(Style, X0, Y0, X2, Y2, X1, Y1)
   int Style, X0, Y0, X2, Y2, *X1, *Y1;
{
   double dx, dy, len, new_x, new_y;

   dx = (double)(X2 - X0);
   dy = (double)(Y2 - Y0);
   len = (double)(sqrt(((double)dx)*((double)dx)+((double)dy)*((double)dy)));
   if (Style == LS_RIGHT) {
      new_x = ((double)((X0+X2)>>1)) + dy/((double)8.0);
      new_y = ((double)((Y0+Y2)>>1)) - dx/((double)8.0);
   } else {
      new_x = ((double)((X0+X2)>>1)) - dy/((double)8.0);
      new_y = ((double)((Y0+Y2)>>1)) + dx/((double)8.0);
   }
   *X1 = (int)round(new_x);
   *Y1 = (int)round(new_y);
}

void Spline (Win, Pixel, Func, X1, Y1, X2, Y2, X3, Y3, X4, Y4)
   Window	Win;
   int		Pixel, Func;
   double	X1, Y1, X2, Y2, X3, Y3, X4, Y4;
   /* X1, Y1, X2, Y2, X3, Y3, X4, Y4 are screen offsets */
{
   double	x, y;

   x = (X2 + X3) / 2.0;
   y = (Y2 + Y3) / 2.0;
   if (fabs (X1 - x) < splineTol && fabs (Y1 - y) < splineTol)
      XDrawLine (mainDisplay, Win, drawGC, round(X1), round(Y1), round(x),
            round(y));
   else
      Spline (Win, Pixel, Func, X1, Y1, ((X1+X2)/2.0), ((Y1+Y2)/2.0),
            ((3.0*X2+X3)/4.0), ((3.0*Y2+Y3)/4.0), x, y);

   if (fabs (x - X4) < splineTol && fabs (y - Y4) < splineTol)
      XDrawLine (mainDisplay, Win, drawGC, round(x), round(y), round(X4),
            round(Y4));
   else
      Spline (Win, Pixel, Func, x, y, ((X2+3.0*X3)/4.0), ((Y2+3.0*Y3)/4.0),
            ((X3+X4)/2.0), ((Y3+Y4)/2.0), X4, Y4);
}

static XPoint	* splineVs;

static
int AddSplinePt(N, MaxN, X, Y)
   int		* N, * MaxN, X, Y;
{
   if (*N == *MaxN)
   {
      splineVs = (XPoint *) realloc (splineVs, (*MaxN)*2*sizeof(XPoint)+1);
      if (splineVs == NULL)
      {
         fprintf (stderr, "Can not realloc() in AddSplinePt ().\n");
         return (FALSE);
      }
      *MaxN = (*MaxN) * 2;
   }
   splineVs[*N].x = X;
   splineVs[*N].y = Y;
   (*N)++;
   return (TRUE);
}

static
void SetSplineVs (N, MaxN, X1, Y1, X2, Y2, X3, Y3, X4, Y4)
   int		* N, * MaxN;
   double	X1, Y1, X2, Y2, X3, Y3, X4, Y4;
   /* X1, Y1, X2, Y2, X3, Y3, X4, Y4 are screen offsets */
{
   double	x, y;

   x = (X2 + X3) / 2.0;
   y = (Y2 + Y3) / 2.0;
   if (fabs (X1 - x) < splineTol && fabs (Y1 - y) < splineTol)
      AddSplinePt (N, MaxN, round(x), round(y));
   else
      SetSplineVs (N, MaxN, X1, Y1, ((X1+X2)/2.0), ((Y1+Y2)/2.0),
            ((3.0*X2+X3)/4.0), ((3.0*Y2+Y3)/4.0), x, y);

   if (fabs (x - X4) < splineTol && fabs (y - Y4) < splineTol)
      AddSplinePt (N, MaxN, round(X4), round(Y4));
   else
      SetSplineVs (N, MaxN, x, y, ((X2+3.0*X3)/4.0), ((Y2+3.0*Y3)/4.0),
            ((X3+X4)/2.0), ((Y3+Y4)/2.0), X4, Y4);
}

XPoint * MakeSplinePolyVertex (N, XOff, YOff, NumVs, Vs)
   int		* N, XOff, YOff, NumVs;
   IntPoint	* Vs;
{
   double	mx1, my1, mx2, my2, mx3, my3, mx4, my4, x1, y1, x2, y2;
   int		i, x_off, y_off, max_n;

   x_off = (zoomedIn ? XOff : (XOff>>zoomScale)<<zoomScale);
   y_off = (zoomedIn ? YOff : (YOff>>zoomScale)<<zoomScale);

   splineVs = NULL;

   switch (NumVs)
   {
      case 0:
      case 1:
         break;
      case 2:
         splineVs = (XPoint*)malloc((NumVs+1)*sizeof(XPoint));
         if (splineVs == NULL) {
            FailAllocMessage();
            *N = 0;
            return (splineVs);
         }
         splineVs[0].x = ZOOMED_SIZE(Vs[0].x-x_off);
         splineVs[0].y = ZOOMED_SIZE(Vs[0].y-y_off);
         splineVs[1].x = ZOOMED_SIZE(Vs[1].x-x_off);
         splineVs[1].y = ZOOMED_SIZE(Vs[1].y-y_off);
         *N = 2;
         break;
      case 3:
         mx1 = ZOOMED_SIZE(Vs->x-x_off); my1 = ZOOMED_SIZE((Vs++)->y-y_off);
         x1 = ZOOMED_SIZE(Vs->x-x_off);  y1 = ZOOMED_SIZE((Vs++)->y-y_off);
         mx2 = (mx1+x1)/2.0;             my2 = (my1+y1)/2.0;
         mx4 = ZOOMED_SIZE(Vs->x-x_off); my4 = ZOOMED_SIZE(Vs->y-y_off);
         mx3 = (x1+mx4)/2.0;             my3 = (y1+my4)/2.0;
         max_n = 100;
         splineVs = (XPoint*)malloc((max_n+1)*sizeof(XPoint));
         if (splineVs == NULL) {
            FailAllocMessage();
            *N = 0;
            return (splineVs);
         }
         splineVs[0].x = mx1;
         splineVs[0].y = my1;
         *N = 1;
         SetSplineVs (N, &max_n, mx1, my1, mx2, my2, mx3, my3, mx4, my4);
         break;
      default:
         mx1 = ZOOMED_SIZE(Vs->x-x_off); my1 = ZOOMED_SIZE((Vs++)->y-y_off);
         x1 = ZOOMED_SIZE(Vs->x-x_off);  y1 = ZOOMED_SIZE((Vs++)->y-y_off);
         x2 = ZOOMED_SIZE(Vs->x-x_off);  y2 = ZOOMED_SIZE((Vs++)->y-y_off);
         mx2 = (mx1+x1)/2.0;             my2 = (my1+y1)/2.0;
         mx3 = (3.0*x1+x2)/4.0;          my3 = (3.0*y1+y2)/4.0;
         mx4 = (x1+x2)/2.0;              my4 = (y1+y2)/2.0;
         max_n = 100;
         splineVs = (XPoint *)malloc((max_n+1)*sizeof(XPoint));
         if (splineVs == NULL) {
            FailAllocMessage();
            *N = 0;
            return (splineVs);
         }
         splineVs[0].x = mx1;
         splineVs[0].y = my1;
         *N = 1;
         SetSplineVs (N, &max_n, mx1, my1, mx2, my2, mx3, my3, mx4, my4);
      
         for (i = 2; i < NumVs-2; i++, Vs++)
         {
            mx1 = mx4;                     my1 = my4;
            mx2 = (x1 + 3.0*x2) / 4.0;     my2 = (y1 + 3.0*y2) / 4.0;
            x1 = x2;                       y1 = y2;
            x2 = ZOOMED_SIZE(Vs->x-x_off); y2 = ZOOMED_SIZE(Vs->y-y_off);
            mx3 = (3.0*x1 + x2) / 4.0;     my3 = (3.0*y1 + y2) / 4.0;
            mx4 = (x1 + x2) / 2.0;         my4 = (y1 + y2) / 2.0;
            SetSplineVs (N, &max_n, mx1, my1, mx2, my2, mx3, my3, mx4, my4);
         }
         mx1 = mx4;                      my1 = my4;
         mx2 = (x1 + 3.0*x2) / 4.0;      my2 = (y1 + 3.0*y2) / 4.0;
         x1 = x2;                        y1 = y2;
         mx4 = ZOOMED_SIZE(Vs->x-x_off); my4 = ZOOMED_SIZE(Vs->y-y_off);
         mx3 = (x1 + mx4) / 2.0;         my3 = (y1 + my4) / 2.0;
         SetSplineVs (N, &max_n, mx1, my1, mx2, my2, mx3, my3, mx4, my4);
         break;
   }
   return (splineVs);
}

typedef struct MultiSplineRec {
   XPoint	* vlist;
   int		n;
} *MultiSplineRecPtr;

XPoint * MakeMultiSplinePolyVertex (N, Smooth, XOff, YOff, NumVs, Vs)
   int		* N, XOff, YOff, NumVs;
   char		* Smooth;
   IntPoint	* Vs;
{
   register int	i, j;
   int		segments=1, has_smooth_point=FALSE, start_index, seg_index;
   int		total=0;
   XPoint	* xpptr;
   struct MultiSplineRec	* msptr;

   if (Smooth == NULL) return MakeSplinePolyVertex (N, XOff, YOff, NumVs, Vs);
   if (Smooth[0] || Smooth[NumVs-1])
   {
      fprintf (stderr, "Bad poly in MakeMultiSplinePolyVertex().\n");
      fprintf (stderr, "Safest thing to do is to save file and exit.\n");
      fprintf (stderr, "Please try to reproduce this error and\n");
      fprintf (stderr, "\tsend bug report to william@cs.ucla.edu.\n");
      fflush (stderr);
      sprintf (gszMsgBox, "%s.\n\n%s.\n\n%s %s.",
            "Bad poly in MakeMultiSplinePolyVertex()",
            "Safest thing to do is to save file and exit",
            "Please try to reproduce this error and",
            "send bug report to william@cs.ucla.edu");
      MsgBox (gszMsgBox, TOOL_NAME, STOP_MB);
      return (NULL);
   }
   for (i=1; i < NumVs-1; i++)
      if (Smooth[i])
         has_smooth_point = TRUE;
      else
         segments++;

   if (!has_smooth_point)
   {
      *N = NumVs;
      return MakePolyVertex (XOff, YOff, NumVs, Vs);
   }
   if (segments == 1) return MakeSplinePolyVertex (N, XOff, YOff, NumVs, Vs);
   msptr = (struct MultiSplineRec *)malloc(segments *
         sizeof(struct MultiSplineRec));
   if (msptr == NULL) {
      FailAllocMessage();
      return NULL;
   }
   for (i=0; i < segments; i++) msptr[i].vlist = NULL;
   start_index = 0;
   seg_index = 0;
   for (i=1; i <= NumVs-1; i++)
   {
      if (!Smooth[i])
      {
         msptr[seg_index].vlist = MakeSplinePolyVertex (&msptr[seg_index].n,
               XOff, YOff, i-start_index+1, &Vs[start_index]);
         total += msptr[seg_index].n-1;
         seg_index++;
         start_index = i;
      }
   }
   if (total > 0) total++;
   splineVs = (XPoint *)malloc((total+2)*sizeof(XPoint));
   if (splineVs == NULL) FailAllocMessage();
   xpptr = splineVs;
   for (i=0; i < segments; i++) {
      if (msptr[i].vlist != NULL) {
         for (j=0; j < msptr[i].n; j++) {
            xpptr->x = msptr[i].vlist[j].x;
            xpptr->y = msptr[i].vlist[j].y;
            xpptr++;
         }
         xpptr--;
         free(msptr[i].vlist);
      }
   }
   free(msptr);
   *N = total;
   return (splineVs);
}

XPoint * MakeSplinePolygonVertex (N, XOff, YOff, NumVs, Vs)
   int		* N, XOff, YOff, NumVs;
   IntPoint	* Vs;
{
   double	mx1, my1, mx2, my2, mx3, my3, mx4, my4, x1, y1, x2, y2;
   int		i, max_n, x_off, y_off;

   x_off = (zoomedIn ? XOff : (XOff>>zoomScale)<<zoomScale);
   y_off = (zoomedIn ? YOff : (YOff>>zoomScale)<<zoomScale);

   splineVs = NULL;

   if (NumVs <= 3)
   {
      splineVs = (XPoint *)malloc(5*sizeof(XPoint));
      if (splineVs == NULL) {
         FailAllocMessage();
         *N = 0;
         return (splineVs);
      }
      splineVs[0].x = ZOOMED_SIZE(Vs[0].x-x_off);
      splineVs[0].y = ZOOMED_SIZE(Vs[0].y-y_off);
      splineVs[1].x = ZOOMED_SIZE(Vs[1].x-x_off);
      splineVs[1].y = ZOOMED_SIZE(Vs[1].y-y_off);
      *N = 2;
      return (splineVs);
   }

   Vs[NumVs].x = Vs[1].x; Vs[NumVs].y = Vs[1].y;
   x1 = ZOOMED_SIZE(Vs->x-x_off); y1 = ZOOMED_SIZE((Vs++)->y-y_off);
   x2 = ZOOMED_SIZE(Vs->x-x_off); y2 = ZOOMED_SIZE((Vs++)->y-y_off);
   mx4 = (x1 + x2) / 2.0;         my4 = (y1 + y2) / 2.0;

   max_n = 100;
   splineVs = (XPoint*)malloc((max_n+1)*sizeof(XPoint));
   if (splineVs == NULL) {
      FailAllocMessage();
      *N = 0;
      return (splineVs);
   }
   splineVs[0].x = mx4;
   splineVs[0].y = my4;
   *N = 1;

   for (i = 1; i < NumVs; i++, Vs++)
   {
      mx1 = mx4;                     my1 = my4;
      mx2 = (x1+3.0*x2)/4.0;         my2 = (y1+3.0*y2)/4.0;
      x1 = x2;                       y1 = y2;
      x2 = ZOOMED_SIZE(Vs->x-x_off); y2 = ZOOMED_SIZE(Vs->y-y_off);
      mx3 = (3.0*x1+x2)/4.0;         my3 = (3.0*y1+y2)/4.0;
      mx4 = (x1+x2)/2.0;             my4 = (y1+y2)/2.0;
      SetSplineVs (N, &max_n, mx1, my1, mx2, my2, mx3, my3, mx4, my4);
   }
   return (splineVs);
}

XPoint * MakeMultiSplinePolygonVertex (N, Smooth, XOff, YOff, NumVs, Vs)
   int		* N, XOff, YOff, NumVs;
   char		* Smooth;
   IntPoint	* Vs;
{
   register int	i, j;
   int		num_smooth_points=0, num_hinge_points=0;
   int		start_index, seg_index, tmp_index;
   int		total=0, once_around=FALSE;
   XPoint	*xpptr;
   IntPoint	*tmp_vs;
   struct MultiSplineRec	* msptr;

   if (Smooth == NULL)
      return MakeSplinePolygonVertex (N, XOff, YOff, NumVs, Vs);
   for (i=1; i < NumVs; i++)
      if (Smooth[i])
         num_smooth_points++;
      else
         num_hinge_points++;

   if (num_smooth_points == 0)
   {
      *N = NumVs;
      return MakePolygonVertex (XOff, YOff, NumVs, Vs);
   }
   if (num_hinge_points == 0)
   {
      return MakeSplinePolygonVertex (N, XOff, YOff, NumVs, Vs);
   }
   msptr = (struct MultiSplineRec *)malloc(num_hinge_points *
         sizeof(struct MultiSplineRec));
   if (msptr == NULL) FailAllocMessage();
   for (i=0; i < num_hinge_points; i++) msptr[i].vlist = NULL;

   for (i=0; i < NumVs; i++)
      if (!Smooth[i])
         break;
   tmp_vs = (IntPoint*)malloc((NumVs+1)*sizeof(IntPoint));
   if (tmp_vs == NULL) FailAllocMessage();
   start_index = i;
   seg_index = 0;

   tmp_vs[0].x = Vs[start_index].x;
   tmp_vs[0].y = Vs[start_index].y;
   tmp_index = 1;
   for (i=start_index+1; !(once_around && i==start_index+1); i++, tmp_index++)
   {
      tmp_vs[tmp_index].x = Vs[i].x;
      tmp_vs[tmp_index].y = Vs[i].y;
      if (!Smooth[i])
      {
         msptr[seg_index].vlist = MakeSplinePolyVertex (&msptr[seg_index].n,
               XOff, YOff, tmp_index+1, tmp_vs);
         total += msptr[seg_index].n-1;
         seg_index++;
         start_index = (i==NumVs-1 ? 0 : i);
         tmp_vs[0].x = Vs[start_index].x;
         tmp_vs[0].y = Vs[start_index].y;
         tmp_index = 0;
      }
      if (i == NumVs-1)
      {
         i = 0;
         once_around = TRUE;
      }
   }
   if (tmp_vs != NULL) free(tmp_vs);
   if (total > 0) total++;
   splineVs = (XPoint*)malloc((total+2)*sizeof(XPoint));
   if (splineVs == NULL) FailAllocMessage();
   xpptr = splineVs;
   for (i=0; i < num_hinge_points; i++) {
      if (msptr[i].vlist != NULL) {
         for (j=0; j < msptr[i].n; j++) {
            xpptr->x = msptr[i].vlist[j].x;
            xpptr->y = msptr[i].vlist[j].y;
            xpptr++;
         }
         xpptr--;
         free(msptr[i].vlist);
      }
   }
   free(msptr);
   *N = total;
   return (splineVs);
}

struct MtxRec {
   double	* x, * y, * dx, * dy;
   double	* * mtx;
} mtxInfo;

static
void OpenSetupMatrix (NumPts, Vs)
   int			NumPts;
   IntPoint		* Vs;
{
   register int		i;

   mtxInfo.x = (double*)malloc(NumPts*sizeof(double));
   mtxInfo.y = (double*)malloc(NumPts*sizeof(double));
   mtxInfo.dx = (double*)malloc(NumPts*sizeof(double));
   mtxInfo.dy = (double*)malloc(NumPts*sizeof(double));
   if (mtxInfo.x == NULL || mtxInfo.y == NULL || mtxInfo.dx == NULL ||
         mtxInfo.dy == NULL) {
      FailAllocMessage();
   }
   for (i=0; i < NumPts; i++) {
      mtxInfo.x[i] = mtxInfo.dx[i] = ((double)(Vs[i].x))*((double)theSum);
      mtxInfo.y[i] = mtxInfo.dy[i] = ((double)(Vs[i].y))*((double)theSum);
   }
   mtxInfo.mtx = (double**)malloc(NumPts*sizeof(double*));
   if (mtxInfo.mtx == NULL) FailAllocMessage();
   for (i=0; i < NumPts; i++) {
      mtxInfo.mtx[i] = (double*)malloc(3*sizeof(double));
      if (mtxInfo.mtx[i] == NULL) FailAllocMessage();
   }
   mtxInfo.mtx[0][0] = mtxInfo.mtx[NumPts-1][2] = (double)0.0;
   mtxInfo.mtx[0][1] = mtxInfo.mtx[NumPts-1][1] = (double)theSum;
   mtxInfo.mtx[0][2] = mtxInfo.mtx[NumPts-1][0] = (double)0.0;
   for (i=1; i < NumPts-1; i++)
   {
      mtxInfo.mtx[i][0] = (double)1.0;
      mtxInfo.mtx[i][1] = (double)SUM_MINUS_2;
      mtxInfo.mtx[i][2] = (double)1.0;
   }
}

static
void TriGaussian (NumPts)
   int	NumPts;
{
   register int		i;
   register double	val;

   for (i=1; i<NumPts-1; i++)
   {
      val = (-mtxInfo.mtx[i-1][1]);
      mtxInfo.mtx[i][0] = (double)0.0;
      mtxInfo.mtx[i][1] = (mtxInfo.mtx[i][1]*val+mtxInfo.mtx[i-1][2])/theSum;
      mtxInfo.mtx[i][2] = val/theSum;
      mtxInfo.x[i] = (mtxInfo.x[i]*val+mtxInfo.x[i-1])/theSum;
      mtxInfo.y[i] = (mtxInfo.y[i]*val+mtxInfo.y[i-1])/theSum;
   }
   mtxInfo.x[i] = mtxInfo.x[i]/mtxInfo.mtx[i][1];
   mtxInfo.y[i] = mtxInfo.y[i]/mtxInfo.mtx[i][1];
   for (i--; i>=0; i--)
   {
      mtxInfo.x[i] = (mtxInfo.x[i]-mtxInfo.x[i+1]*mtxInfo.mtx[i][2]) /
            mtxInfo.mtx[i][1];
      mtxInfo.y[i] = (mtxInfo.y[i]-mtxInfo.y[i+1]*mtxInfo.mtx[i][2]) /
            mtxInfo.mtx[i][1];
   }
}

static
void FreeMtxInfo (NumPts)
   int	NumPts;
{
   register int	i;

   free(mtxInfo.x); mtxInfo.x = NULL;
   free(mtxInfo.y); mtxInfo.y = NULL;
   free(mtxInfo.dx); mtxInfo.dx = NULL;
   free(mtxInfo.dy); mtxInfo.dy = NULL;
   for (i=0; i < NumPts; i++) free(mtxInfo.mtx[i]);
   free(mtxInfo.mtx); mtxInfo.mtx = NULL;
}

static
IntPoint * OpenControlPts (NumPts, N)
   int	NumPts, * N;
{
   register int	i;
   int		index=0;
   double	half=theSum/((double)2.0);
   double	weight=half-((double)1.0);
   IntPoint	*v;

   v = (IntPoint*)malloc((((NumPts-2)<<1)+2)*sizeof(IntPoint));
   if (v == NULL) FailAllocMessage();
   v[index].x = (int)(mtxInfo.x[0]);
   v[index].y = (int)(mtxInfo.y[0]);
   index++;
   v[index].x = (int)((mtxInfo.x[0]+weight*mtxInfo.x[1])/half);
   v[index].y = (int)((mtxInfo.y[0]+weight*mtxInfo.y[1])/half);
   index++;
   for (i=1; i<NumPts-2; i++) {
      v[index].x = (int)((weight*mtxInfo.x[i]+mtxInfo.x[i+1])/half);
      v[index].y = (int)((weight*mtxInfo.y[i]+mtxInfo.y[i+1])/half);
      index++;
      v[index].x = (int)((mtxInfo.x[i]+weight*mtxInfo.x[i+1])/half);
      v[index].y = (int)((mtxInfo.y[i]+weight*mtxInfo.y[i+1])/half);
      index++;
   }
   v[index].x = (int)((weight*mtxInfo.x[i]+mtxInfo.x[i+1])/half);
   v[index].y = (int)((weight*mtxInfo.y[i]+mtxInfo.y[i+1])/half);
   index++;
   v[index].x = (int)(mtxInfo.x[NumPts-1]);
   v[index].y = (int)(mtxInfo.y[NumPts-1]);
   index++;

   FreeMtxInfo (NumPts);

   *N = index;
   return (v);
}

XPoint * MakeIntSplinePolyVertex (N, CntrlN, CntrlVs, XOff, YOff, NumVs, Vs)
   int		* N, * CntrlN, XOff, YOff, NumVs;
   IntPoint	* * CntrlVs, * Vs;
{
   int x_off, y_off;

   x_off = (zoomedIn ? XOff : (XOff>>zoomScale)<<zoomScale);
   y_off = (zoomedIn ? YOff : (YOff>>zoomScale)<<zoomScale);

   splineVs = NULL;

   switch (NumVs)
   {
      case 0:
      case 1:
         break;
      case 2:
         *CntrlVs = (IntPoint*)malloc((NumVs+1)*sizeof(IntPoint));
         if (*CntrlVs == NULL) FailAllocMessage();
         splineVs = (XPoint*)malloc((NumVs+1)*sizeof(XPoint));
         if (splineVs == NULL) {
            FailAllocMessage();
            *N = 0;
            return (splineVs);
         }
         splineVs[0].x = ZOOMED_SIZE(Vs[0].x-x_off);
         splineVs[0].y = ZOOMED_SIZE(Vs[0].y-y_off);
         splineVs[1].x = ZOOMED_SIZE(Vs[1].x-x_off);
         splineVs[1].y = ZOOMED_SIZE(Vs[1].y-y_off);
         (*CntrlVs)[0].x = Vs[0].x;
         (*CntrlVs)[0].y = Vs[0].y;
         (*CntrlVs)[1].x = Vs[1].x;
         (*CntrlVs)[1].y = Vs[1].y;
         *N = *CntrlN = 2;
         break;
      default:
         OpenSetupMatrix (NumVs, Vs);
         TriGaussian (NumVs);
         *CntrlVs = OpenControlPts (NumVs, CntrlN);
         return (MakeSplinePolyVertex (N, XOff, YOff, *CntrlN, *CntrlVs));
         break;
   }
   return (splineVs);
}

static
void ClosedSetupMatrix (NumPts, Vs)
   int			NumPts;
   IntPoint		* Vs;
{
   register int		i;
   register double	val;

   mtxInfo.x = (double*)malloc(NumPts*sizeof(double));
   mtxInfo.y = (double*)malloc(NumPts*sizeof(double));
   mtxInfo.dx = (double*)malloc(NumPts*sizeof(double));
   mtxInfo.dy = (double*)malloc(NumPts*sizeof(double));
   if (mtxInfo.x == NULL || mtxInfo.y == NULL || mtxInfo.dx == NULL ||
         mtxInfo.dy == NULL) {
      FailAllocMessage();
   }
   for (i=0; i < NumPts; i++) {
      mtxInfo.x[i] = mtxInfo.dx[i] = ((double)(Vs[i].x))*((double)theSum);
      mtxInfo.y[i] = mtxInfo.dy[i] = ((double)(Vs[i].y))*((double)theSum);
   }
   /* the first NumPts-2 rows have an extra column */
   mtxInfo.mtx = (double**)malloc(NumPts*sizeof(double*));
   if (mtxInfo.mtx == NULL) FailAllocMessage();
   for (i=0; i < NumPts; i++) {
      mtxInfo.mtx[i] = (double*)malloc(4*sizeof(double));
      if (mtxInfo.mtx[i] == NULL) FailAllocMessage();
   }
   mtxInfo.mtx[0][0] = mtxInfo.mtx[NumPts-1][2] = (double)1.0;
   mtxInfo.mtx[0][1] = mtxInfo.mtx[NumPts-1][1] = (double)SUM_MINUS_2;
   mtxInfo.mtx[0][2] = mtxInfo.mtx[NumPts-1][0] = (double)1.0;
   mtxInfo.mtx[0][3] = (double)1.0;
   /* use mtx[NumPts-1][3] as mtx[NumPts-1][i] where i is moving to right */
   mtxInfo.mtx[NumPts-1][3] = (double)0.0;
   for (i=1; i < NumPts-1; i++)
   {
      mtxInfo.mtx[i][0] = (double)1.0;
      mtxInfo.mtx[i][1] = (double)SUM_MINUS_2;
      mtxInfo.mtx[i][2] = (double)1.0;
      mtxInfo.mtx[i][3] = (double)0.0;
   }
   val = (-mtxInfo.mtx[0][1]);
   if (NumPts == 3)
      mtxInfo.mtx[NumPts-1][0] =
            (mtxInfo.mtx[NumPts-1][0]*val+mtxInfo.mtx[0][2])/theSum;
   else
      mtxInfo.mtx[NumPts-1][0] = mtxInfo.mtx[NumPts-1][0]*val/theSum;
   mtxInfo.mtx[NumPts-1][1] =
         (mtxInfo.mtx[NumPts-1][1]*val+mtxInfo.mtx[0][3])/theSum;
   /* use mtx[NumPts-1][3] as mtx[NumPts-1][i] where i moves to right */
   mtxInfo.mtx[NumPts-1][3] = mtxInfo.mtx[0][2]/theSum;
   mtxInfo.x[NumPts-1] = (mtxInfo.x[NumPts-1]*val+mtxInfo.x[0])/theSum;
   mtxInfo.y[NumPts-1] = (mtxInfo.y[NumPts-1]*val+mtxInfo.y[0])/theSum;
}

static int gaussIteration=0;

static
void DebugClosedMatrix (NumPts)
   int	NumPts;
{
   int	j, i;

   /* mtxInfo.mtx[0][3] is actually mtxInfo.mtx[0][NumPts-1]           */
   /* mtxInfo.mtx[NumPts-1][3] is actually mtxInfo.mtx[NumPts-1][i+1]  */
   /*       where i is the current iteration in gaussian elimincation; */
   /*       before gaussian() is called, i is 0                        */
   printf("|  %+8.2f  %+8.2f  ", mtxInfo.mtx[0][1], mtxInfo.mtx[0][2]);
   for (j=2; j<NumPts-1; j++) printf("          ");
   printf("%+8.2f  |  (%+8.2f,%+8.2f)\n", mtxInfo.mtx[0][3],
         mtxInfo.x[0], mtxInfo.y[0]);
   for (i=1; i<NumPts-1; i++) {
      if (i == NumPts-2) {
         printf("|  ");
         for (j=1; j<i; j++) printf("          ");
         printf("%+8.2f  %+8.2f  %+8.2f  ", mtxInfo.mtx[i][0],
               mtxInfo.mtx[i][1], mtxInfo.mtx[i][2]);
         for (j=i+2; j<NumPts; j++) printf("          ");
         printf("|  (%+8.2f,%+8.2f)\n", mtxInfo.x[i], mtxInfo.y[i]);
      } else {
         printf("|  ");
         for (j=1; j<i; j++) printf("          ");
         printf("%+8.2f  %+8.2f  %+8.2f  ", mtxInfo.mtx[i][0],
               mtxInfo.mtx[i][1], mtxInfo.mtx[i][2]);
         for (j=i+2; j<NumPts-1; j++) printf("          ");
         printf("%+8.2f  |  (%+8.2f,%+8.2f)\n", mtxInfo.mtx[i][3],
               mtxInfo.x[i], mtxInfo.y[i]);
      }
   }
   printf("|  ");
   if (gaussIteration+2 < i) {
      for (j=1; j<gaussIteration+2; j++) printf("          ");
      printf("%+8.2f  ", mtxInfo.mtx[i][3]);
      for (j=gaussIteration+3; j<i; j++) printf("          ");
   } else {
      for (j=1; j<i; j++) printf("          ");
   }
   printf("%+8.2f  %+8.2f  ", mtxInfo.mtx[i][0], mtxInfo.mtx[i][1]);
   printf("|  (%+8.2f,%+8.2f)\n", mtxInfo.x[i], mtxInfo.y[i]);
}

static
void Gaussian (NumPts)
   int	NumPts;
{
   register int		i;
   register double	val;

   gaussIteration = 0;
   /* DebugClosedMatrix (NumPts); */
   for (i=1; i<NumPts-1; i++)
   {
      val = (-mtxInfo.mtx[i-1][1]);
      mtxInfo.mtx[i][0] = (double)0.0;
      mtxInfo.mtx[i][1] = (mtxInfo.mtx[i][1]*val+mtxInfo.mtx[i-1][2])/theSum;
      if (i == NumPts-2) {
         mtxInfo.mtx[i][2] = (mtxInfo.mtx[i][2]*val+mtxInfo.mtx[i-1][3])/theSum;
      } else {
         mtxInfo.mtx[i][2] = val/theSum;
      }
      mtxInfo.x[i] = (mtxInfo.x[i]*val+mtxInfo.x[i-1])/theSum;
      mtxInfo.y[i] = (mtxInfo.y[i]*val+mtxInfo.y[i-1])/theSum;
      if (i != NumPts-2) {
         mtxInfo.mtx[i][3] = mtxInfo.mtx[i-1][3]/theSum;;
      }
      val = (-mtxInfo.mtx[i][1])/mtxInfo.mtx[NumPts-1][3];
      if (i < NumPts-2) {
         if (i < NumPts-3) {
            mtxInfo.mtx[NumPts-1][0] = mtxInfo.mtx[NumPts-1][0]*val/theSum;
         } else {
            mtxInfo.mtx[NumPts-1][0] =
                  (mtxInfo.mtx[NumPts-1][0]*val+mtxInfo.mtx[i][2])/theSum;
         }
         mtxInfo.mtx[NumPts-1][1] =
            (mtxInfo.mtx[NumPts-1][1]*val+mtxInfo.mtx[i][3])/theSum;
         mtxInfo.mtx[NumPts-1][3] = mtxInfo.mtx[i][2]/theSum;
         mtxInfo.x[NumPts-1] = (mtxInfo.x[NumPts-1]*val+mtxInfo.x[i])/theSum;
         mtxInfo.y[NumPts-1] = (mtxInfo.y[NumPts-1]*val+mtxInfo.y[i])/theSum;
      }
      gaussIteration++;
      /* DebugClosedMatrix (NumPts); */
   }
   val = (-mtxInfo.mtx[i-1][1])/mtxInfo.mtx[i][0];
   mtxInfo.mtx[i][0] = (double)0.0;
   mtxInfo.mtx[i][1] = (mtxInfo.mtx[i][1]*val+mtxInfo.mtx[i-1][2])/theSum;
   mtxInfo.x[i] = (mtxInfo.x[i]*val+mtxInfo.x[i-1])/theSum;
   mtxInfo.y[i] = (mtxInfo.y[i]*val+mtxInfo.y[i-1])/theSum;

   mtxInfo.x[i] = mtxInfo.x[i]/mtxInfo.mtx[i][1];
   mtxInfo.y[i] = mtxInfo.y[i]/mtxInfo.mtx[i][1];
   for (i--; i>=0; i--)
   {
      if (i == NumPts-2) {
         mtxInfo.x[i] = (mtxInfo.x[i]-mtxInfo.x[i+1]*mtxInfo.mtx[i][2]) /
               mtxInfo.mtx[i][1];
         mtxInfo.y[i] = (mtxInfo.y[i]-mtxInfo.y[i+1]*mtxInfo.mtx[i][2]) /
               mtxInfo.mtx[i][1];
      } else {
         mtxInfo.x[i] = (mtxInfo.x[i]-mtxInfo.x[i+1]*mtxInfo.mtx[i][2] -
               mtxInfo.x[NumPts-1]*mtxInfo.mtx[i][3])/mtxInfo.mtx[i][1];
         mtxInfo.y[i] = (mtxInfo.y[i]-mtxInfo.y[i+1]*mtxInfo.mtx[i][2] -
               mtxInfo.y[NumPts-1]*mtxInfo.mtx[i][3])/mtxInfo.mtx[i][1];
      }
   }
   /* DebugClosedMatrix (NumPts); */
}

static
IntPoint * ClosedControlPts (NumPts, N)
   int	NumPts, * N;
{
   register int	i;
   int		index=0;
   double	half=theSum/((double)2.0);
   double	weight=half-((double)1.0);
   IntPoint	*v;

   v = (IntPoint*)malloc(((NumPts<<1)+2)*sizeof(IntPoint));
   if (v == NULL) FailAllocMessage();
   for (i=0; i<NumPts; i++) {
      v[index].x = (int)((weight*mtxInfo.x[i]+mtxInfo.x[(i+1) % NumPts])/half);
      v[index].y = (int)((weight*mtxInfo.y[i]+mtxInfo.y[(i+1) % NumPts])/half);
      index++;
      v[index].x = (int)((mtxInfo.x[i]+weight*mtxInfo.x[(i+1) % NumPts])/half);
      v[index].y = (int)((mtxInfo.y[i]+weight*mtxInfo.y[(i+1) % NumPts])/half);
      index++;
   }
   v[index].x = (int)((weight*mtxInfo.x[0]+mtxInfo.x[1])/half);
   v[index].y = (int)((weight*mtxInfo.y[0]+mtxInfo.y[1])/half);
   index++;

   FreeMtxInfo (NumPts);

   *N = index;
   return (v);
}

XPoint * MakeIntSplinePolygonVertex (N, CntrlN, CntrlVs, XOff, YOff, NumVs, Vs)
   int		* N, * CntrlN, XOff, YOff, NumVs;
   IntPoint	* * CntrlVs, * Vs;
{
   int	x_off, y_off;

   x_off = (zoomedIn ? XOff : (XOff>>zoomScale)<<zoomScale);
   y_off = (zoomedIn ? YOff : (YOff>>zoomScale)<<zoomScale);

   splineVs = NULL;

   if (NumVs <= 3)
   {
      splineVs = (XPoint *)malloc(5*sizeof(XPoint));
      if (splineVs == NULL) {
         FailAllocMessage();
         *N = 0;
         return (splineVs);
      }
      splineVs[0].x = ZOOMED_SIZE(Vs[0].x-x_off);
      splineVs[0].y = ZOOMED_SIZE(Vs[0].y-y_off);
      splineVs[1].x = ZOOMED_SIZE(Vs[1].x-x_off);
      splineVs[1].y = ZOOMED_SIZE(Vs[1].y-y_off);
      *N = *CntrlN = 2;
      return (splineVs);
   }
   gaussIteration = 0;
   NumVs--; /* drop the duplicated end point */
   ClosedSetupMatrix (NumVs, Vs);
   Gaussian (NumVs);
   *CntrlVs = ClosedControlPts (NumVs, CntrlN);
   return (MakeSplinePolygonVertex (N, XOff, YOff, *CntrlN, *CntrlVs));
}

void DumpCurvedPolyPoints (FP, NumPts, V, Indent)
   FILE			* FP;
   int			NumPts, Indent;
   register IntPoint	* V;
{
   register int	j, i;
   double	x1, y1, x2, y2;
   double	mx1, my1, mx2, my2, mx3, my3, mx4, my4;

   switch (NumPts)
   {
      case 0:
      case 1:
      case 2:
         break;
      case 3:
         mx1 = V->x; my1 = (V++)->y;
         x1 = V->x; y1 = (V++)->y;
         x2 = V->x; y2 = (V++)->y;
         mx2 = (mx1 + 2.0*x1) / 3.0; my2 = (my1 + 2.0*y1) / 3.0;
         mx3 = (2.0*x1 + x2) / 3.0; my3 = (2.0*y1 + y2) / 3.0;
         for (j = 0; j < Indent; j++) fprintf (FP, " ");
         fprintf (FP, "%.2f %.2f %.2f %.2f\n", mx2, my2, mx3, my3);
         break;
      default:
         mx1 = V->x; my1 = (V++)->y;
         x1 = V->x; y1 = (V++)->y;
         x2 = V->x; y2 = (V++)->y;
         mx2 = (mx1 + 2.0*x1) / 3.0; my2 = (my1 + 2.0*y1) / 3.0;
         mx3 = (5.0*x1 + x2) / 6.0; my3 = (5.0*y1 + y2) / 6.0;
         mx4 = (x1 + x2) / 2.0; my4 = (y1 + y2) / 2.0;
         for (j = 0; j < Indent; j++) fprintf (FP, " ");
         fprintf (FP, "%.2f %.2f %.2f %.2f %.2f %.2f curveto\n",
               mx2, my2, mx3, my3, mx4, my4);
      
         for (i = 2; i < NumPts-2; i++, V++)
         {
            mx2 = (x1 + 5.0*x2) / 6.0; my2 = (y1 + 5.0*y2) / 6.0;
            x1 = x2; y1 = y2;
#ifdef stellar
            mx3 = (5.0*x1 + V->x) / 6.0; my3 = (5.0*y1 + V->y) / 6.0;
            mx4 = (x1 + V->x) / 2.0; my4 = (y1 + V->y) / 2.0;
#else
            x2 = V->x; y2 = V->y;
            mx3 = (5.0*x1 + x2) / 6.0; my3 = (5.0*y1 + y2) / 6.0;
            mx4 = (x1 + x2) / 2.0; my4 = (y1 + y2) / 2.0;
#endif
            for (j = 0; j < Indent; j++) fprintf (FP, " ");
            fprintf (FP, "%.2f %.2f %.2f %.2f %.2f %.2f curveto\n",
                  mx2, my2, mx3, my3, mx4, my4);
#ifdef stellar
            x2 = V->x; y2 = V->y;
#endif
         }
         mx2 = (x1 + 5.0*x2) / 6.0; my2 = (y1 + 5.0*y2) / 6.0;
         x1 = x2; y1 = y2;
         mx3 = (2.0*x1 + V->x) / 3.0; my3 = (2.0*y1 + V->y) / 3.0;
         for (j = 0; j < Indent; j++) fprintf (FP, " ");
         fprintf (FP, "%.2f %.2f %.2f %.2f\n", mx2, my2, mx3, my3);
         break;
   }
}

void DumpCurvedPolygonPoints (FP, NumPts, V, Indent)
   FILE			* FP;
   int			NumPts, Indent;
   register IntPoint	* V;
{
   register int	j;
   double	mx2, my2, mx3, my3, mx4, my4, x1, y1, x2, y2;
   int		i;

   V[NumPts].x = V[1].x; V[NumPts].y = V[1].y;
   x1 = V->x;             y1 = (V++)->y;
   x2 = V->x;             y2 = (V++)->y;
   mx4 = (x1 + x2) / 2.0; my4 = (y1 + y2) / 2.0;
   for (j = 0; j < Indent; j++) fprintf (FP, " ");
   fprintf (FP, "%.2f %.2f moveto\n", mx4, my4);

   for (i = 1; i < NumPts; i++, V++)
   {
      mx2 = (x1+5.0*x2)/6.0; my2 = (y1+5.0*y2)/6.0;
      x1 = x2;               y1 = y2;
#ifdef stellar
      mx3 = (5.0*x1+V->x)/6.0; my3 = (5.0*y1+V->y)/6.0;
      mx4 = (x1+V->x)/2.0;     my4 = (y1+V->y)/2.0;
#else
      x2 = V->x;             y2 = V->y;
      mx3 = (5.0*x1+x2)/6.0; my3 = (5.0*y1+y2)/6.0;
      mx4 = (x1+x2)/2.0;     my4 = (y1+y2)/2.0;
#endif
      for (j = 0; j < Indent; j++) fprintf (FP, " ");
      fprintf (FP, "%.2f %.2f %.2f %.2f %.2f %.2f curveto\n",
            mx2, my2, mx3, my3, mx4, my4);
#ifdef stellar
      x2 = V->x;             y2 = V->y;
#endif
   }
}

void DumpMultiCurvedPolyPoints (FP, Smooth, Style, Curved, NumPts, V, Indent)
   FILE			* FP;
   char			* Smooth;
   int			Style, Curved, NumPts, Indent;
   register IntPoint	* V;
{
   register int	i, j;
   int		segments=1, has_smooth_point=FALSE, start_index;

   if (Curved == LT_INTSPLINE || Smooth == NULL)
   {
      DumpCurvedPolyPoints (FP, NumPts, V, Indent);
      return;
   }
   if (Smooth[0] || Smooth[NumPts-1])
   {
      fprintf (stderr, "Bad poly in DumpMultiCurvedPolyPoints().\n");
      fprintf (stderr, "Safest thing to do is to save file and exit.\n");
      fprintf (stderr, "Please try to reproduce this error and\n");
      fprintf (stderr, "\tsend bug report to william@cs.ucla.edu.\n");
      fflush (stderr);
      sprintf (gszMsgBox, "%s.\n\n%s.\n\n%s %s.",
            "Bad poly in DumpMultiCurvedPolyPoints()",
            "Safest thing to do is to save file and exit",
            "Please try to reproduce this error and",
            "send bug report to william@cs.ucla.edu");
      MsgBox (gszMsgBox, TOOL_NAME, STOP_MB);
      return;
   }
   for (i=1; i < NumPts-1; i++)
      if (Smooth[i])
         has_smooth_point = TRUE;
      else
         segments++;

   if (!has_smooth_point)
   {  /* simple polyline */
      if (Style & LS_RIGHT)
         DumpPoints (FP, NumPts-1, V, Indent);
      else
         DumpPoints (FP, NumPts, V, Indent);
      return;
   }
   if (segments == 1)
   {  /* simple spline */
      if (Style & LS_RIGHT)
      {
         if (NumPts != 2)
            DumpCurvedPolyPoints (FP, NumPts, V, Indent);
         else
            DumpPoints (FP, NumPts-1, V, Indent);
      }
      else if (NumPts != 2)
      {
         DumpCurvedPolyPoints (FP, NumPts, V, Indent);
         for (i=0; i < Indent; i++) fprintf (FP, " ");
         fprintf (FP, "%1d %1d curveto\n", V[NumPts-1].x, V[NumPts-1].y);
      }
      else
         DumpPoints (FP, NumPts, V, Indent);
      return;
   }
   start_index = 0;
   for (i=1; i <= NumPts-1; i++)
   {
      if (!Smooth[i])
      {
         int	num_tmp_vs=i-start_index+1;

         if (num_tmp_vs == 2)
            DumpPoints (FP, num_tmp_vs, &V[start_index], Indent);
         else
         {
            DumpCurvedPolyPoints (FP, num_tmp_vs, &V[start_index], Indent);
            if (!(i == NumPts-1 && (Style & LS_RIGHT)))
            {
               for (j=0; j < Indent; j++) fprintf (FP, " ");
               fprintf (FP, "%1d %1d curveto\n", V[i].x, V[i].y);
            }
         }
         start_index = i;
      }
   }
}

void DumpMultiCurvedPolygonPoints (FP, Smooth, Curved, NumPts, V, Indent)
   FILE			* FP;
   char			* Smooth;
   int			Curved, NumPts, Indent;
   register IntPoint	* V;
{
   register int	i, j;
   int		num_smooth_points=0, num_hinge_points=0, tmp_index;
   int		start_index;
   int		once_around=FALSE;
   IntPoint	*tmp_vs;

   if (Curved == LT_INTSPLINE || Smooth == NULL)
   {
      DumpCurvedPolygonPoints (FP, NumPts, V, Indent);
      return;
   }
   for (i=1; i < NumPts; i++)
      if (Smooth[i])
         num_smooth_points++;
      else
         num_hinge_points++;

   if (num_smooth_points == 0)
   {  /* simple polygon */
      for (j=0; j < Indent; j++) fprintf (FP, " ");
      fprintf (FP, "%1d %1d moveto\n", V[0].x, V[0].y);
      DumpPoints (FP, NumPts-1, V, Indent);
      return;
   }
   if (num_hinge_points == 0)
   {
      DumpCurvedPolygonPoints (FP, NumPts, V, Indent);
      return;
   }
   tmp_vs = (IntPoint*)malloc((NumPts+1)*sizeof(IntPoint));
   if (tmp_vs == NULL) FailAllocMessage();

   for (i=0; i < NumPts; i++) {
      if (!Smooth[i]) {
         break;
      }
   }
   for (j=0; j < Indent; j++) fprintf (FP, " ");
   fprintf (FP, "%1d %1d moveto\n", V[i].x, V[i].y);
   start_index = i;
   tmp_vs[0].x = V[start_index].x;
   tmp_vs[0].y = V[start_index].y;
   tmp_index = 1;
   for (i=start_index+1; !(once_around && i==start_index+1); i++, tmp_index++)
   {
      tmp_vs[tmp_index].x = V[i].x;
      tmp_vs[tmp_index].y = V[i].y;
      if (!Smooth[i])
      {
         if (tmp_index == 1)
            DumpPoints (FP, tmp_index+1, tmp_vs, Indent);
         else
         {
            DumpCurvedPolyPoints (FP, tmp_index+1, tmp_vs, Indent);
            for (j=0; j < Indent; j++) fprintf (FP, " ");
            fprintf (FP, "%1d %1d curveto\n", V[i].x, V[i].y);
         }
         start_index = (i==NumPts-1 ? 0 : i);
         tmp_vs[0].x = V[start_index].x;
         tmp_vs[0].y = V[start_index].y;
         tmp_index = 0;
      }
      if (i == NumPts-1)
      {
         i = 0;
         once_around = TRUE;
      }
   }
   if (tmp_vs != NULL) free(tmp_vs);
}

