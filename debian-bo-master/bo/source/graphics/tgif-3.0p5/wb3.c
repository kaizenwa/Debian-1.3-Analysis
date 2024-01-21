/*
 * Author:      Renato Santana, <renato@nce.ufrj.br> in January, 1996.
 *
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/wb3.c,v 3.0 1996/05/06 16:13:00 william Exp $
 */

#ifdef _TGIF_WB

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netdb.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

#include "const.h"
#include "types.h"
#include "patchlvl.h"

#include "align.e"
#include "auxtext.e"
#include "color.e"
#include "file.e"
#include "font.e"
#include "grid.e"
#include "msg.e"
#include "page.e"
#include "pattern.e"
#include "poly.e"
#include "prtgif.e"
#include "setup.e"
#include "stk.e"
#include "text.e"
#include "version.e"
#include "wb2.e"
#ifndef _NO_EXTERN
#include "wb3.e"
#endif
#include "xpixmap.e"

#define GETVALUE(val,name) ScanValue("%d", &(val), name, "poly")
#define CUR_VERSION 32
 
extern int      sock;

static struct StrRec    * firstStrRec = NULL, * lastStrRec = NULL;

/* prototypes definitions */
void ReadPoly ARGS_DECL((char*,struct ObjRec**));
void ReadPolygon ARGS_DECL((char*,struct ObjRec**));
int ReadSmoothHingeFromBuff ARGS_DECL((char*,int,int,char*));
static int OldFontIndex ARGS_DECL((register int,register int,register int,register int));
void ReadText     ARGS_DECL((char*,struct ObjRec**));
void AddStrRec    ARGS_DECL((struct StrRec*,struct StrRec*,struct StrRec*));
void SaveToBuffer ARGS_DECL((char*, struct ObjRec*, int, int));
void ReadGroup    ARGS_DECL((char *, int, struct ObjRec * *));

/* prototypes defined*/


void ReadPoly (Inbuf, ObjPtr)
   char                 * Inbuf;
   struct ObjRec        * * ObjPtr;
{
   register int         i;
   struct PolyRec       * poly_ptr;
   IntPoint             * v;
   char                 color_str[40], * s, inbuf[MAXSTRING+1], msg[MAXSTRING];
   int                  num_pts, ltx=0, lty=0, rbx=0, rby=0, x, y, id=0;
   int                  initialized, rotation, count, new_alloc, j=0;
   int                  style, width=0, pen, curved, fill, dash, locked=FALSE;
   int                  aw=origArrowHeadW[6], ah=origArrowHeadH[6];
   char                 * smooth=NULL, aux[10000], aux2[10000];
 
   *ObjPtr = NULL;
   
   strcpy(aux,Inbuf); 

   s = FindChar ((int)'(', Inbuf);
   s = ParseStr (s, (int)',', color_str, sizeof(color_str));
 
   InitScan (s, "\t\n, []");
   if (GETVALUE (num_pts, "number of points") == INVALID)
      return;
 
   if (num_pts <= 0)
   {
      (void) sprintf (msg, "%s, %d:  Invalid number of points in poly",
            scanFileName, scanLineNum);
      if (PRTGIF)
         fprintf (stderr, "%s\n", msg);
      else
         Msg (msg);
      return;
   }
 
   *ObjPtr = (struct ObjRec *) malloc (sizeof(struct ObjRec));
   if (*ObjPtr == NULL) FailAllocMessage();
   memset(*ObjPtr, 0, sizeof(struct ObjRec));
   poly_ptr = (struct PolyRec *) malloc (sizeof(struct PolyRec));
   if (poly_ptr == NULL) FailAllocMessage();
   memset(poly_ptr, 0, sizeof(struct PolyRec));
 
   if (num_pts == 1)
   {
      v = (IntPoint *) malloc (4*sizeof(IntPoint));
      if (v == NULL) FailAllocMessage ();
      smooth = (char *) malloc (4*sizeof(char));
      if (smooth == NULL) FailAllocMessage ();
   }
   else
   {
      v = (IntPoint *) malloc ((num_pts+1)*sizeof(IntPoint));
      if (v == NULL) FailAllocMessage ();
      smooth = (char *) malloc ((num_pts+1)*sizeof(char));
      if (smooth == NULL) FailAllocMessage ();
   }
 
   initialized = FALSE;
 
   if (fileVersion <= 13)
   {
      for (i = 0; i < num_pts; i++)
      {
         if (GETVALUE (x, "x") == INVALID || GETVALUE (y, "y") == INVALID)
         {
            free(*ObjPtr);
            free(poly_ptr);
            free(v);
            *ObjPtr = NULL;
            return;
         }
         v[i].x = x; v[i].y = y;
         if (!initialized)
         {
            initialized = TRUE;
            ltx = rbx = x; lty = rby = y;
         }
         else
         {
            if (x < ltx) ltx = x; if (y < lty) lty = y;
            if (x > rbx) rbx = x; if (y > rby) rby = y;
         }
      }
   }
   else
   {
      scanLineNum++;
      strcpy(aux2,aux);
      s = FindChar ((int)'[', aux);
      s++;


      InitScan (s, "\t\n, []");

      for (i = 0, count = 0; i < num_pts; ++i, ++count)
      {
         if (count == 8 && i != num_pts-1)
         {
            count = 0;
            scanLineNum++;
            while (*s != '\n') ++s;
            ++s;
            strcpy(aux,s);
	    s = aux;
            InitScan (s, "\t\n, []");
         }
         if (GETVALUE (x, "x") == INVALID || GETVALUE (y, "y") == INVALID)
         {
            free(*ObjPtr);
            free(poly_ptr);
            free(v);
            *ObjPtr = NULL;
            return;
         }
         v[i].x = x; v[i].y = y;
         if (!initialized)
         {
            initialized = TRUE;
            ltx = rbx = x; lty = rby = y;
         }
         else
         {
            if (x < ltx) ltx = x; if (y < lty) lty = y;
            if (x > rbx) rbx = x; if (y > rby) rby = y;
         }
      }
   }

   s = FindChar ((int)']', aux2);
   s++;
   strcpy(aux, s);
   InitScan (s, "\t\n, []");

   if (num_pts == 1)
   {
      sprintf (msg, "%s (%1d,%1d) converted to double point poly.",
            "Single point poly", v[0].x, v[0].y);
      if (PRTGIF)
         fprintf (stderr, "%s\n", msg);
      else
         Msg (msg);
      v[1].x = v[0].x;
      v[1].y = v[0].y;
      num_pts = 2;
   }

   poly_ptr->n = num_pts;
 
   dash = 0;
   rotation = 0;
   if (fileVersion == INVALID)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      id = objId++;
      fill = NONEPAT;
      if (width == LINE_CURVED)
      {
         width = 0;
         curved = TRUE;
      }
      else
         curved = FALSE;
      switch (width)
      {
         case 1: width = 3; break;
         case 2: width = 6; break;
      }
   }
   else if (fileVersion <= 3)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      /*if (id >= objId) objId = id+1;*/
      objId = id+1;
      fill = NONEPAT;
      if (width == LINE_CURVED)
      {
         width = 0;
         curved = TRUE;
      }
      else
         curved = FALSE;
      switch (width)
      {
         case 1: width = 3; break;
         case 2: width = 6; break;
      }
   }
   else if (fileVersion <= 4)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
      fill = NONEPAT;
      switch (width)
      {
         case 1: width = 3; break;
         case 2: width = 6; break;
      }
   }
   else if (fileVersion <= 5)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
      switch (width)
      {
         case 1: width = 3; break;
         case 2: width = 6; break;
      }
   }
   else if (fileVersion <= 8)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 13)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 16)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 25)
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID ||
          GETVALUE (aw,       "arrow head width") == INVALID ||
          GETVALUE (ah,       "arrow head height") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else
   {
      if (GETVALUE (style,    "style") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID ||
          GETVALUE (aw,       "arrow head width") == INVALID ||
          GETVALUE (ah,       "arrow head height") == INVALID ||
          GETVALUE (locked,   "locked") == INVALID)
      {
         free(*ObjPtr);
         free(poly_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      objId = id+1;
   }
 
   if (fileVersion <= 16 && width <= 6)
   {
      aw = origArrowHeadW[width];
      ah = origArrowHeadH[width];
      width = origWidthOfLine[width];
   }
   if (curved == LT_INTSPLINE && smooth != NULL)
   {
      free(smooth);
      smooth = NULL;
   }
   if (fileVersion <= 30)
   {
      switch (curved)
      {
         case LT_STRAIGHT:
            for (i=0; i < num_pts; i++) smooth[i] = FALSE;
            break;
         case LT_SPLINE:
            smooth[0] = smooth[num_pts-1] = FALSE;
            for (i=1; i < num_pts-1; i++) smooth[i] = TRUE;
            break;
      }
   }
   else if (!ReadSmoothHingeFromBuff (aux, curved, num_pts, smooth))
      return; 
 
   fill = UpgradePenFill (fill);
   pen = UpgradePenFill (pen);
 
   poly_ptr->style = style;
   poly_ptr->width = width;
   poly_ptr->aw = aw;
   poly_ptr->ah = ah;
   poly_ptr->pen = pen;
   poly_ptr->curved = curved;
   poly_ptr->fill = fill;
   poly_ptr->dash = dash;
 
   poly_ptr->vlist = v;
   poly_ptr->smooth = smooth;
   poly_ptr->svlist = poly_ptr->asvlist = NULL;
   poly_ptr->intvlist = NULL;
 
   (*ObjPtr)->x = ltx;
   (*ObjPtr)->y = lty;
   (*ObjPtr)->color = QuickFindColorIndex(*ObjPtr, color_str, &new_alloc, TRUE);
   (*ObjPtr)->dirty = FALSE;
   (*ObjPtr)->id = id;
   (*ObjPtr)->rotation = rotation;
   (*ObjPtr)->locked = locked;
   (*ObjPtr)->type = OBJ_POLY;
   (*ObjPtr)->obbox.ltx = ltx;
   (*ObjPtr)->obbox.lty = lty;
   (*ObjPtr)->obbox.rbx = rbx;
   (*ObjPtr)->obbox.rby = rby;
   (*ObjPtr)->detail.p = poly_ptr;

   AdjObjSplineVs (*ObjPtr);
   if (poly_ptr->curved != LT_INTSPLINE)
      UpdPolyBBox (*ObjPtr, poly_ptr->n, poly_ptr->vlist);
   else
      UpdPolyBBox (*ObjPtr, poly_ptr->intn, poly_ptr->intvlist);
}

void ReadPolygon (Inbuf, ObjPtr)
   char                 * Inbuf;
   struct ObjRec        * * ObjPtr;
{
   register int         i;
   struct PolygonRec    * polygon_ptr;
   IntPoint             * v;
   char                 color_str[40], * s, inbuf[MAXSTRING], msg[MAXSTRING];
   int                  num_pts, ltx=0, lty=0, rbx=0, rby=0, x, y, id=0;
   int                  fill, width, pen, w, new_alloc, locked=FALSE;
   int                  curved, dash, initialized, rotation, count;
   char                 * smooth=NULL, aux[10000], aux2[10000];
 
   *ObjPtr = NULL;
 
   strcpy(aux,Inbuf);

   s = FindChar ((int)'(', Inbuf);
   s = ParseStr (s, (int)',', color_str, sizeof(color_str));
 
   InitScan (s, "\t\n, []");
 
   if (GETVALUE (num_pts, "number of points") == INVALID)
      return;
 
   if (num_pts <= 0)
   {
      (void) sprintf (msg, "%s, %d:  Invalid number of points in polygon",
            scanFileName, scanLineNum);
      if (PRTGIF)
         fprintf (stderr, "%s\n", msg);
      else
         Msg (msg);
      return;
   }
 
   *ObjPtr = (struct ObjRec *) malloc (sizeof(struct ObjRec));
   if (*ObjPtr == NULL) FailAllocMessage();
   memset(*ObjPtr, 0, sizeof(struct ObjRec));
   polygon_ptr = (struct PolygonRec *) malloc (sizeof(struct PolygonRec));
   if (polygon_ptr == NULL) FailAllocMessage();
   memset(polygon_ptr, 0, sizeof(struct PolygonRec));
 
   if (num_pts < 3)
   {
      v = (IntPoint *) malloc (5*sizeof(IntPoint));
      if (v == NULL) FailAllocMessage ();
      smooth = (char *) malloc (5*sizeof(char));
      if (smooth == NULL) FailAllocMessage ();
   }
   else
   {
      v = (IntPoint *) malloc ((num_pts+1)*sizeof(IntPoint));
      if (v == NULL) FailAllocMessage ();
      smooth = (char *) malloc ((num_pts+1)*sizeof(char));
      if (smooth == NULL) FailAllocMessage ();
   }
 
   initialized = FALSE;
 
   if (fileVersion <= 13)
   {
      for (i = 0; i < num_pts; i++)
      {
         if (GETVALUE (x, "x") == INVALID || GETVALUE (y, "y") == INVALID)
         {
            free(*ObjPtr);
            free(polygon_ptr);
            free(v);
            *ObjPtr = NULL;
            return;
         }
         v[i].x = x; v[i].y = y;
         if (!initialized)
         {
            initialized = TRUE;
            ltx = rbx = x; lty = rby = y;
         }
         else
         {
            if (x < ltx) ltx = x; if (y < lty) lty = y;
            if (x > rbx) rbx = x; if (y > rby) rby = y;
         }
      }
   }
   else
   {
      /*fgets (inbuf, MAXSTRING, FP);
      scanLineNum++;
      s = inbuf; */

      scanLineNum++;
      strcpy(aux2,aux);
      s = FindChar ((int)'[', aux);
      s++;

      InitScan (s, "\t\n, []");

      for (i = 0, count = 0; i < num_pts; i++)
      {
         if (GETVALUE (x, "x") == INVALID || GETVALUE (y, "y") == INVALID)
         {
            free(*ObjPtr);
            free(polygon_ptr);
            free(v);
            *ObjPtr = NULL;
            return;
         }
         v[i].x = x; v[i].y = y;
         if (!initialized)
         {
            initialized = TRUE;
            ltx = rbx = x; lty = rby = y;
         }
         else
         {
            if (x < ltx) ltx = x; if (y < lty) lty = y;
            if (x > rbx) rbx = x; if (y > rby) rby = y;
         }
         if (++count == 8 && i != num_pts-1)
         {
            /*count = 0;
            fgets (inbuf, MAXSTRING, FP);
            scanLineNum++;
            s = inbuf;
            InitScan (s, "\t\n, []");*/

            count = 0;
            scanLineNum++;
            while (*s != '\n') ++s;
            ++s;
            strcpy(aux,s);
            s = aux;
            InitScan (s, "\t\n, []");

         }
      }
   }

   s = FindChar ((int)']', aux2);
   s++;
   strcpy(aux, s);
   InitScan (s, "\t\n, []");

 
   switch (num_pts)
   {
      case 1:
         sprintf (msg, "%s (%1d,%1d) converted to 3-point polygon.",
               "1-point polygon", v[0].x, v[0].y);
         if (PRTGIF)
            fprintf (stderr, "%s\n", msg);
         else
            Msg (msg);
         v[3].x = v[2].x = v[1].x = v[0].x;
         v[3].y = v[2].y = v[1].y = v[0].y;
         num_pts = 4;
         break;
      case 2:
         sprintf (msg, "%s [%1d,%1d,%1d,%1d] converted to 3-point polygon.",
               "2-point polygon", v[0].x, v[0].y, v[1].x, v[1].y);
         if (PRTGIF)
            fprintf (stderr, "%s\n", msg);
         else
            Msg (msg);
         v[3].x = v[2].x = v[0].x;
         v[3].y = v[2].y = v[0].y;
         num_pts = 4;
         break;
      case 3:
         sprintf (msg, "%s [%1d,%1d,%1d,%1d] converted to 3-point polygon.",
               "2-point polygon", v[0].x, v[0].y, v[1].x, v[1].y);
         if (PRTGIF)
            fprintf (stderr, "%s\n", msg);
         else
            Msg (msg);
         v[3].x = v[2].x = v[0].x;
         v[3].y = v[2].y = v[0].y;
         num_pts = 4;
         break;
   }
 
   polygon_ptr->n = num_pts;
 
   dash = 0;
   rotation = 0;
   if (fileVersion <= 3)
   {
      if (GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID)
      {
         free(*ObjPtr);
         free(polygon_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (width == LINE_CURVED)
      {
         width = 0;
         curved = TRUE;
      }
      else
         curved = FALSE;
      switch (width)
      {
         case 1: width = 3; break;
         case 2: width = 6; break;
      }
      id = objId++;
   }
   else if (fileVersion <= 5)
   {
      if (GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID)
      {
         free(*ObjPtr);
         free(polygon_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      switch (width)
      {
         case 1: width = 3; break;
         case 2: width = 6; break;
      }
      id = objId++;
   }
   else if (fileVersion <= 7)
   {
      if (GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID)
      {
         free(*ObjPtr);
         free(polygon_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      id = objId++;
   }
   else if (fileVersion <= 8)
   {
      if (GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (id,       "id") == INVALID)
      {
         free(*ObjPtr);
         free(polygon_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 13)
   {
      if (GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID)
      {
         free(*ObjPtr);
         free(polygon_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else if (fileVersion <= 25)
   {
      if (GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID)
      {
         free(*ObjPtr);
         free(polygon_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }
   else
   {
      if (GETVALUE (fill,     "fill") == INVALID ||
          GETVALUE (width,    "width") == INVALID ||
          GETVALUE (pen,      "pen") == INVALID ||
          GETVALUE (curved,   "curved") == INVALID ||
          GETVALUE (id,       "id") == INVALID ||
          GETVALUE (dash,     "dash") == INVALID ||
          GETVALUE (rotation, "rotation") == INVALID ||
          GETVALUE (locked,   "locked") == INVALID)
      {
         free(*ObjPtr);
         free(polygon_ptr);
         free(v);
         *ObjPtr = NULL;
         return;
      }
      if (id >= objId) objId = id+1;
   }

   if (fileVersion <= 16 && width <= 6) width = origWidthOfLine[width];
 
   if (fileVersion <= 25 && curved > 1) curved = 0;
   if (curved == LT_INTSPLINE && smooth != NULL)
   {
      free(smooth);
      smooth = NULL;
   }
   if (fileVersion <= 30)
   {
      switch (curved)
      {
         case LT_STRAIGHT:
            for (i=0; i < num_pts; i++) smooth[i] = FALSE;
            break;
         case LT_SPLINE:
            for (i=0; i < num_pts; i++) smooth[i] = TRUE;
            break;
      }
   }
   else if (!ReadSmoothHingeFromBuff (aux, curved, num_pts, smooth))
      return;
 
   fill = UpgradePenFill (fill);
   pen = UpgradePenFill (pen);
 
   polygon_ptr->fill = fill;
   polygon_ptr->width = width;
   polygon_ptr->pen = pen;
   polygon_ptr->curved = curved;
   polygon_ptr->dash = dash;
 
   polygon_ptr->vlist = v;
   polygon_ptr->smooth = smooth;
   polygon_ptr->svlist = NULL;
   polygon_ptr->intvlist = NULL;
 
   (*ObjPtr)->x = ltx;
   (*ObjPtr)->y = lty;
   (*ObjPtr)->color = QuickFindColorIndex(*ObjPtr, color_str, &new_alloc, TRUE);
   (*ObjPtr)->dirty = FALSE;
   (*ObjPtr)->id = id;
   (*ObjPtr)->rotation = rotation;
   (*ObjPtr)->locked = locked;
   (*ObjPtr)->type = OBJ_POLYGON;
   (*ObjPtr)->obbox.ltx = ltx;
   (*ObjPtr)->obbox.lty = lty;
   (*ObjPtr)->obbox.rbx = rbx;
   (*ObjPtr)->obbox.rby = rby;
   w = HALF_W(width);
   (*ObjPtr)->bbox.ltx = ltx - w;
   (*ObjPtr)->bbox.lty = lty - w;
   (*ObjPtr)->bbox.rbx = rbx + w;
   (*ObjPtr)->bbox.rby = rby + w;
   (*ObjPtr)->detail.g = polygon_ptr;
   AdjObjSplineVs (*ObjPtr);
   if (polygon_ptr->curved != LT_INTSPLINE)
      UpdPolyBBox (*ObjPtr, polygon_ptr->n, polygon_ptr->vlist);
   else
      UpdPolyBBox (*ObjPtr, polygon_ptr->intn, polygon_ptr->intvlist);
}
 


int ReadSmoothHingeFromBuff (buff, Curved, NumPts, Smooth)
   char * buff;
   int  Curved, NumPts;
   char * Smooth;
{
   int  num_nibbles=NumPts>>2, nibble_count=0, bit_count=0, j, k;
   char * c_ptr, inbuf[MAXSTRING+1], msg[MAXSTRING];
 
   while (*buff != '\n') ++buff;
   buff++;
   if ((NumPts & 0x3) != 0) num_nibbles++;
   scanLineNum++;
   if (Curved == LT_INTSPLINE || Smooth == NULL) return (TRUE);
   if ((c_ptr = strchr (buff, '"')) == NULL)
   {
      (void) sprintf (msg, "%s, %d:  Invalid smooth/hinge spec for a poly.",
            scanFileName, scanLineNum);
      if (PRTGIF)
         fprintf (stderr, "%s\n", msg);
      else
         Msg (msg);
      return (FALSE);
   }
   c_ptr++;
   for (j = 0; j < num_nibbles; j++)
   {
      int       data=0;
 
      if (nibble_count++ == 64)
      {
         while (*buff != '\n') ++buff;
         buff++;
         scanLineNum++;
         for (c_ptr=buff; *c_ptr == ' '; c_ptr++) ;
         nibble_count = 1;
      }
      if (*c_ptr >= '0' && *c_ptr <= '9')
         data = (int)(*c_ptr++) - (int)('0');
      else if (*c_ptr >= 'a' && *c_ptr <= 'f')
         data = (int)(*c_ptr++) - (int)('a') + 10;
      for (k = 0; k < 4; k++)
      {
         if (bit_count++ == NumPts) break;
 
         Smooth[(j<<2)+k] = (data & (1<<(3-k)) ? TRUE : FALSE);
      }
   }
   return (TRUE);
}


#define FONTS_PER_DPI (((MAXFONTS-1)*MAXFONTSTYLES+1)*MAXFONTSIZES)
 
static
int OldFontIndex (dpi_index, font_index, size_index, style_index)
   register int dpi_index, font_index, size_index, style_index;
   /* obsoleted procedure, kept to remain compatible with old versions */
{
   if (font_index == FONT_SYM)
      return (size_index+MAXFONTSIZES*(MAXFONTSTYLES*font_index) +
            dpi_index*FONTS_PER_DPI);
   else
      return (size_index+MAXFONTSIZES*(style_index+MAXFONTSTYLES*font_index) +
            dpi_index*FONTS_PER_DPI);
}
 
#define GETVAL(val,name) ScanValue("%d", &(val), name, "text")
#define GETSTRNG(val,name) ScanValue("%s", (val), name, "text")
 

void ReadText (Inbuf, ObjPtr)
   char                 * Inbuf;
   struct ObjRec        * * ObjPtr;
{

   register int         i, len, max_len;
   struct StrRec        * s_ptr;
   struct TextRec       * text_ptr;
   char                 color_str[80], * s, * c_ptr, font_str[80];
   char                 * tmp_str, inbuf[MAXSTRING+1], * line;
   int                  num_lines, x, y, font, style, size, new_alloc, id=0;
   int                  text_just, rotate, pen, cur_size, done, rotation;
   int                  bbox_w, bbox_h, dpi, asc, des, fill, allocated, v_space;
   int                  locked=FALSE, min_lbearing=0, max_rextra=0;
   char                 aux[10000], aux2[10000], *t;
 
   dpi = FONT_DPI_75;
   fill = NONEPAT;
   v_space = 0;
 
   *ObjPtr = NULL;
 
   t = FindChar ((int)'[', Inbuf);
   strcpy(aux, ++t); 

   s = FindChar ((int)'(', Inbuf);
   s = ParseStr (s, (int)',', color_str, sizeof(color_str));
   InitScan (s, ", \t\n");
 
   rotate = 0;
   pen = 1;
   rotation = 0;

   if (GETVAL (x,         "x") == INVALID ||
       GETVAL (y,         "y") == INVALID ||
       GETSTRNG (font_str,  "font_str") == INVALID ||
       GETVAL (style,     "style") == INVALID ||
       GETVAL (size,      "size") == INVALID ||
       GETVAL (num_lines, "num_lines") == INVALID ||
       GETVAL (text_just, "text_just") == INVALID ||
       GETVAL (rotate,    "rotate") == INVALID ||
       GETVAL (pen,       "pen") == INVALID ||
       GETVAL (bbox_w,    "bbox w") == INVALID ||
       GETVAL (bbox_h,    "bbox h") == INVALID ||
       GETVAL (id,        "id") == INVALID ||
       GETVAL (dpi,       "dpi") == INVALID ||
       GETVAL (asc,       "ascent") == INVALID ||
       GETVAL (des,       "descent") == INVALID ||
       GETVAL (fill,      "fill") == INVALID ||
       GETVAL (v_space,   "vertical spacing") == INVALID ||
       GETVAL (rotation,  "rotation") == INVALID ||
       GETVAL (locked,    "locked") == INVALID)
   {
      return;
   }


   /*if (id >= objId) objId = id+1; */
   objId++;
   fill = UpgradePenFill (fill);
   pen = UpgradePenFill (pen);
 
   *ObjPtr = (struct ObjRec *) malloc (sizeof(struct ObjRec));
   if (*ObjPtr == NULL) FailAllocMessage();
   memset(*ObjPtr, 0, sizeof(struct ObjRec));
 
   text_ptr = (struct TextRec *) malloc (sizeof(struct TextRec));
   if (text_ptr == NULL) FailAllocMessage();
   memset(text_ptr, 0, sizeof(struct TextRec));
   text_ptr->lines = num_lines;
   text_ptr->cached_bitmap = None;
   text_ptr->cached_zoom = 0;
   text_ptr->cached_rotate = ROTATE0;
 
   if (!PRTGIF) SaveCurFont ();

   if (PRTGIF)
   {
      if (fileVersion <= 29)
      {
         curFont = text_ptr->font = font;
         text_ptr->font_name = NULL;
      }
      else
      {
         len = strlen (font_str);
         text_ptr->font_name = (char *) malloc ((len+1)*sizeof(char));
         if (text_ptr->font_name == NULL) FailAllocMessage();
         if (*font_str == '\'' && font_str[len-1] == '\'')
         {
            font_str[len-1] = '\0';
            strcpy (text_ptr->font_name, &font_str[1]);
         }
         else
{
            strcpy (text_ptr->font_name, font_str);
}
         curFont = text_ptr->font =
               GetFontIndex (text_ptr->font_name, style, FALSE);
      }
   }
   else
   {
      text_ptr->font_name = NULL;
      if (fileVersion <= 29)
         curFont = text_ptr->font = font;
      else
      {
         char   *s;
 
         len = strlen (font_str);
         if (*font_str == '\'' && font_str[len-1] == '\'')
         {
            font_str[len-1] = '\0';
            s = &font_str[1];
         }
            s = font_str;
         curFont = text_ptr->font = GetFontIndex (s, style, TRUE);
         if (curFont == INVALID)
         {
            char        msg[MAXSTRING];
 
            sprintf (msg, "Can not find screen font for '%s'.", s);
            TwoLineMsg (msg, "    Use Times instead.");
            SetFileModified (TRUE);
            curFont = text_ptr->font = FONT_TIM;
         }
      }
   }

   curStyle = text_ptr->style = style;
   curSize = text_ptr->size =
         (fileVersion<=29 ? GetCompatibleSize (dpi, size) : size);
   textJust = text_ptr->just = text_just;
   textVSpace = text_ptr->v_space = v_space;
   curRotate = text_ptr->rotate = rotate;
   penPat = text_ptr->pen = pen;
   objFill = text_ptr->fill = fill;
 
   if (PRTGIF)
   {
      if (fileVersion < 10)
      {
         canvasFontAsc =
               pDrawFontAsc[OldFontIndex(dpi,curFont,size,curStyle)];
         canvasFontDes =
               pDrawFontDes[OldFontIndex(dpi,curFont,size,curStyle)];
      }
      else
      {
         canvasFontAsc = asc;
         canvasFontDes = des;
      }
      textCursorH = canvasFontAsc + canvasFontDes;
      text_ptr->read_only = FALSE;
   }
   else
   {
      SetCanvasFont ();
      text_ptr->read_only = (curSize != canvasFontSize);
   }
   if (text_ptr->read_only)
   {
      text_ptr->asc = asc;
      text_ptr->des = des;
      switch (rotate)
      {
         case ROTATE0:
         case ROTATE180:
            text_ptr->orig_w = bbox_w;
            text_ptr->orig_h = bbox_h;
            break;
         case ROTATE90:
         case ROTATE270:
            text_ptr->orig_w = bbox_h;
            text_ptr->orig_h = bbox_w;
            break;
      }
   }
   else
   {
      text_ptr->asc = canvasFontAsc;
      text_ptr->des = canvasFontDes;
      text_ptr->orig_w = text_ptr->orig_h = 0;
   }
   s = aux;
   for (i = 0; i < num_lines; i++)
   {
      /*fgets (inbuf, MAXSTRING, FP); */
      scanLineNum++;
      allocated = FALSE;
 
      strcpy(aux2,aux);
      tmp_str = FindChar ((int)'"', aux2);
      t = tmp_str;
      while ((*t != '\n') && (*t != '\r'))  t++;
      *t = '\0';
      t++;
      strcpy(aux,t);
      s = ReadString (tmp_str);
      *(--s) = '\0';
      s_ptr = (struct StrRec *) malloc (sizeof(struct StrRec));
      if (s_ptr == NULL) FailAllocMessage();
      memset(s_ptr, 0, sizeof(struct StrRec));
      strcpy (s_ptr->dyn_str.s, tmp_str);
      AddStrRec (lastStrRec, (struct StrRec *)NULL, s_ptr);
      if (PRTGIF)
         len = strlen (tmp_str); /* assume string width = 1 pixel per char */
      else
         len = XTextWidth (canvasFontPtr, tmp_str, strlen (tmp_str));
      if (len > max_len) max_len = len;
 
      /* if (allocated) free (line); */
   }
   text_ptr->first = firstStrRec;
   text_ptr->last = lastStrRec;
 
   firstStrRec = lastStrRec = NULL;
 
   (*ObjPtr)->x = x;
   (*ObjPtr)->y = y;
 
   if (PRTGIF && fileVersion > 6)
   {
      switch (rotate)
      {
         case ROTATE0:
         case ROTATE180:
            SetTextBBox (*ObjPtr, text_just, bbox_w, bbox_h,
                  min_lbearing, max_rextra, rotate);
            break;
         case ROTATE90:
         case ROTATE270:
            SetTextBBox (*ObjPtr, text_just, bbox_h, bbox_w,
                  min_lbearing, max_rextra, rotate);
            break;
      }
   }
   else if (!PRTGIF && text_ptr->read_only)
      SetTextBBox (*ObjPtr, text_just, text_ptr->orig_w, text_ptr->orig_h,
            min_lbearing, max_rextra, rotate);
   else
      SetTextBBox (*ObjPtr, text_just, max_len,
            num_lines*textCursorH+(num_lines-1)*textVSpace,
            min_lbearing, max_rextra, rotate);
 
   (*ObjPtr)->type = OBJ_TEXT;
   (*ObjPtr)->color = QuickFindColorIndex(*ObjPtr, color_str, &new_alloc, TRUE);
   (*ObjPtr)->dirty = FALSE;
   (*ObjPtr)->id = id;
   (*ObjPtr)->rotation = rotation;
   (*ObjPtr)->locked = locked;
   (*ObjPtr)->detail.t = text_ptr;
 
   if (!PRTGIF) RestoreCurFont ();
}

void AddStrRec (PrevPtr, NextPtr, StrPtr)
   struct StrRec        * PrevPtr, * NextPtr, * StrPtr;
{
   StrPtr->prev = PrevPtr;
   StrPtr->next = NextPtr;
 
   if (PrevPtr == NULL)
      firstStrRec = StrPtr;
   else
      PrevPtr->next = StrPtr;
 
   if (NextPtr == NULL)
      lastStrRec = StrPtr;
   else
      NextPtr->prev = StrPtr;
}


void SaveToBuffer(buff, BotObjPtr, Level, PageNumber)
   char           *buff;
   struct ObjRec  *BotObjPtr;
   int             Level;
   int             PageNumber;
{
   struct ObjRec        * obj_ptr;
   char      font_str[81];
 
   if (Level == 0 && PageNumber == 1)
   {
      ResetXPmErrorMessage ();
      GetPSFontStr (curFont, curStyle, font_str);

      if (TGIF_PATCHLEVEL == 0)
      {
         sprintf (buff, "%s%%TGIF %s\n", buff, versionString);
      }
      else
      {
         sprintf (buff, "%s%%TGIF %s-p%1d\n", buff, versionString, TGIF_PATCHLEVEL);
      }

      sprintf (buff, "%sstate(%1d,%1d,%1d,", buff, pageStyle, CUR_VERSION,
                                             printMag);
      sprintf (buff, "%s%1d,%1d,%1d,%1d,%1d,%1d,", buff, drawOrigX, drawOrigY, 
                     zoomScale, xyEnglishGrid, gridOn, colorIndex);
      sprintf (buff, "%s%1d,%1d,%1d,", buff, horiAlign, vertAlign, lineWidth);
      sprintf (buff, "%s%1d,%1d,%1d,%1d,%1d,'%s',%1d,%1d,", buff, curSpline, 
                     lineStyle, objFill, penPat,textJust, &font_str[1], 
                     curStyle, curSize);

      sprintf (buff, "%s%1d,%1d,%1d,%1d,%1d,%1d,", buff, 0, curDash, 
                     gridSystem, xyMetricGrid, textVSpace, zoomedIn);
      sprintf (buff, "%s%1d,%1d,%1d,%1d,%1d,", buff, gridShown, moveMode, 
                     curRotate, rcbRadius, useGray);
      sprintf (buff, "%s%1d,%1d,%1d,%1d,%1d,%1d,%1d).\n", buff, pageLayoutMode,
            (pageLayoutMode == PAGE_STACK) ? curPageNum : paperCol,
            (pageLayoutMode == PAGE_STACK) ? lastPageNum : paperRow, 
            pageLineShownInTileMode, colorDump, 
            round(((float)onePageWidth)*((float)printMag)/100.0), 
            round(((float)onePageHeight)*((float)printMag)/100.0)); 
      sprintf (buff, "%s%%\n%% @%s%s\n", buff, "(#)$H", "eader$");
      sprintf (buff, "%s%% %s\n%%\n", buff, "%W%");
 
      if (savedComments != NULL)
         sprintf(buff, "%s%s", buff, savedComments);
 
      if (tgifObj->lattr != NULL)
      {
         sprintf (buff, "%sfile_attr(", buff);

         SaveAttrsToBuff (buff, tgifObj->lattr);
         sprintf (buff, "%s).\n", buff);
      }
   }
   if (Level == 0)
   {
      sprintf (buff, "%spage(%1d,\"%s\").\n", buff, PageNumber, ((pageLayoutMode==PAGE_TILE || curPage->name==NULL) ? "" : curPage->name) );
   }
   for (obj_ptr = BotObjPtr; obj_ptr != NULL; obj_ptr = obj_ptr->prev)
   {
      switch (obj_ptr->type)
      {
         case OBJ_POLY:  RecordPolyToBuffer (obj_ptr, buff); break;
         case OBJ_BOX:   RecordBoxToBuffer (obj_ptr,buff); break;
         case OBJ_OVAL:  RecordOvalToBuffer (obj_ptr, buff); break;
         case OBJ_TEXT:  RecordTextToBuffer (obj_ptr, buff); break;
         case OBJ_POLYGON: break;
         case OBJ_ARC:   RecordArcToBuffer (obj_ptr, buff); break;
         case OBJ_RCBOX: RecordRCBoxToBuffer (obj_ptr, buff); break;
         case OBJ_XBM:   break;
         case OBJ_XPM:   break;
         case OBJ_GROUP: RecordGroupToBuffer (obj_ptr, buff, Level); break;
         case OBJ_SYM:   break;
         case OBJ_ICON:  break;
      }
      if (obj_ptr->prev == NULL)
      {
         if (Level == 0)
         {
            sprintf (buff, "%s.\n", buff);
         }
         else
         {
            sprintf (buff, "%s\n", buff);
         }
      }
      else
      {
         if (Level == 0)
         {
            sprintf (buff , "%s.\n", buff);
         }
         else
         {
            sprintf (buff, "%s,\n", buff);
         }
      }
   }
/*   SetCurChoice (NOTHING);
   SetDefaultCursor (mainWindow);
   ShowCursor ();*/
}


void ReadGroup( Inbuf, ObjType, ObjPtr)
   char                 * Inbuf;
   int                  ObjType;
   struct ObjRec        * * ObjPtr;
{

/*
   struct GroupRec      * group_ptr;
   struct ObjRec        * top_obj = NULL, * bot_obj = NULL, * obj_ptr;
   int                  ltx, lty, rbx, rby, id=0, locked=FALSE;
   int                  obj_ltx, obj_lty, obj_rbx, obj_rby, rotate=0, flip=0;
   char                 line[MAXSTRING+1], * s, * s1, tmp_str[MAXSTRING+1];
*/
 
   char *t, aux[10000], aux2[10000];

   *ObjPtr = NULL;


   t = FindChar ((int)'[', Inbuf);
   strcpy(aux, ++t);
   t = ParseStr (t, (int)')', aux2, sizeof(aux2));
   t = t+3;

 
/*
   s = FindChar ((int)'(', Inbuf);
   s = ParseStr (s, (int)',', color_str, sizeof(color_str));
   InitScan (s, ", \t\n");
 
   while (ReadObj (FP, &obj_ptr))
   {
      buff = ParseStr (buff,(int) '#', aux, strlen(buff));
      ReadObjFromBuff(aux, &obj_ptr);

      if (obj_ptr == NULL) return;
 
      obj_ptr->next = top_obj;
      if (top_obj == NULL)
         bot_obj = obj_ptr;
      else
         top_obj->prev = obj_ptr;
      top_obj = obj_ptr;
   }
  if (top_obj == NULL) return;
 
   if (fileVersion <= 20 && (ObjType==OBJ_GROUP || ObjType==OBJ_SYM))
      id = objId++;
   else
   {
      if (fgets (line, MAXSTRING, FP) == NULL) return;
      scanLineNum++;
 
      switch (ObjType)
      {
         case OBJ_GROUP:
            InitScan (line, "\t\n, []");
            if (fileVersion <= 25)
            {
               if (ScanValue ("%d", &id, "id", "group") == INVALID) return;
 
               if (id >= objId) objId = id+1;
            }
            else
            {
               if (ScanValue ("%d", &id, "id", "group") == INVALID ||
                   ScanValue ("%d", &locked, "locked", "group") == INVALID)
                  return;
 
               if (id >= objId) objId = id+1;
            }
            break;
         case OBJ_SYM:
            InitScan (line, "\t\n, []");
            if (fileVersion <= 25)
            {
               if (ScanValue ("%d", &id, "id", "sym") == INVALID) return;
 
               if (id >= objId) objId = id+1;
            }
            else
            {
               if (ScanValue ("%d", &id, "id", "sym") == INVALID ||
                   ScanValue ("%d", &locked, "locked", "sym") == INVALID)
                  return;
 
               if (id >= objId) objId = id+1;
            }
            break;
         case OBJ_ICON:
            strcpy(tmp_str, FindChar ((int)'"', line));
            s = FindChar ((int)'"', tmp_str);
            if (fileVersion == INVALID) return;
 
            if (fileVersion <= 12)
            {
               s1 = FindChar ((int)',', s);
               InitScan (s1, "\t\n, ");
               if (ScanValue ("%d", &id, "id", "icon") == INVALID) return;
 
               if (id >= objId) objId = id+1;
            }
            else if (fileVersion <= 25)
            {
               s1 = FindChar ((int)',', s);
               InitScan (s1, "\t\n, ");
               if (ScanValue ("%d", &id, "id", "icon") == INVALID ||
                   ScanValue ("%d", &rotate, "rotation", "icon") == INVALID ||
                   ScanValue ("%d", &flip, "flip", "icon") == INVALID)
               {
                  return;
               }
               if (id >= objId) objId = id+1;
            }
            else
            {
               s1 = FindChar ((int)',', s);
               InitScan (s1, "\t\n, ");
               if (ScanValue ("%d", &id, "id", "icon") == INVALID ||
                   ScanValue ("%d", &rotate, "rotation", "icon") == INVALID ||
                   ScanValue ("%d", &flip, "flip", "icon") == INVALID ||
                   ScanValue ("%d", &locked, "locked", "icon") == INVALID)
               {
                  return;
               }
               if (id >= objId) objId = id+1;
            }
            *(--s) = '\0';
            break;
      }
   }
 
   *ObjPtr = (struct ObjRec *) malloc (sizeof(struct ObjRec));
   if (*ObjPtr == NULL) FailAllocMessage();
   memset(*ObjPtr, 0, sizeof(struct ObjRec));
 
   top_obj->prev = NULL;
 
   group_ptr = (struct GroupRec *) malloc (sizeof(struct GroupRec));
   if (group_ptr == NULL) FailAllocMessage();
   memset(group_ptr, 0, sizeof(struct GroupRec));
   group_ptr->first = top_obj;
   group_ptr->last = bot_obj;
   group_ptr->rotate = rotate;
   group_ptr->flip = flip;
   group_ptr->deck_index = (-1);
   if (ObjType == OBJ_ICON) strcpy (group_ptr->s, tmp_str);
 
   ltx = top_obj->bbox.ltx;
   lty = top_obj->bbox.lty;
   rbx = top_obj->bbox.rbx;
   rby = top_obj->bbox.rby;
   obj_ltx = top_obj->obbox.ltx;
   obj_lty = top_obj->obbox.lty;
   obj_rbx = top_obj->obbox.rbx;
   obj_rby = top_obj->obbox.rby;
   for (obj_ptr = top_obj->next; obj_ptr != NULL; obj_ptr = obj_ptr->next)
   {
      if (obj_ptr->bbox.ltx < ltx) ltx = obj_ptr->bbox.ltx;
      if (obj_ptr->bbox.lty < lty) lty = obj_ptr->bbox.lty;
      if (obj_ptr->bbox.rbx > rbx) rbx = obj_ptr->bbox.rbx;
      if (obj_ptr->bbox.rby > rby) rby = obj_ptr->bbox.rby;
      if (obj_ptr->obbox.ltx < obj_ltx) obj_ltx = obj_ptr->obbox.ltx;
      if (obj_ptr->obbox.lty < obj_lty) obj_lty = obj_ptr->obbox.lty;
      if (obj_ptr->obbox.rbx > obj_rbx) obj_rbx = obj_ptr->obbox.rbx;
      if (obj_ptr->obbox.rby > obj_rby) obj_rby = obj_ptr->obbox.rby;
   }
 
   (*ObjPtr)->x = obj_ltx;
   (*ObjPtr)->y = obj_lty;
   (*ObjPtr)->dirty = FALSE;
   (*ObjPtr)->id = id;
   (*ObjPtr)->locked = locked;
   (*ObjPtr)->type = ObjType;
   (*ObjPtr)->bbox.ltx = ltx;
   (*ObjPtr)->bbox.lty = lty;
   (*ObjPtr)->bbox.rbx = rbx;
   (*ObjPtr)->bbox.rby = rby;
   (*ObjPtr)->obbox.ltx = obj_ltx;
   (*ObjPtr)->obbox.lty = obj_lty;
   (*ObjPtr)->obbox.rbx = obj_rbx;
   (*ObjPtr)->obbox.rby = obj_rby;
   (*ObjPtr)->detail.r = group_ptr;

*/
}


#endif    /* _TGIF_WB */
