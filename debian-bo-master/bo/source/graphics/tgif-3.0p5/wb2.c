/*
 * Author:      Renato Santana, <renato@nce.ufrj.br> in January, 1996.
 *
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/wb2.c,v 3.0 1996/05/06 16:12:57 william Exp $
 */

#ifdef _TGIF_WB

#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <sys/types.h>
#include <sys/socket.h>

#include <sys/time.h>
#include <netinet/in.h>
#include <netdb.h>

#include "const.h"
#include "types.h"

#include "attr.e"
#include "auxtext.e"
#include "color.e"
#include "dup.e"
#include "file.e"
#ifndef _NO_EXTERN
#include "wb2.e"
#endif
#include "wb3.e"

/* prototypes definitions */
void RecordPolyToBuffer ARGS_DECL((struct ObjRec*,char*));
void SaveSmoothHingeToBuff ARGS_DECL((char*,int,int,char*));
void RecordBoxToBuffer ARGS_DECL((struct ObjRec*,char*));
void RecordOvalToBuffer ARGS_DECL((struct ObjRec*,char*));
void RecordTextToBuffer ARGS_DECL((struct ObjRec*,char*));
void RecordArcToBuffer ARGS_DECL((struct ObjRec*,char*));
void RecordPolygonToBuffer ARGS_DECL((struct ObjRec*,char*));
void RecordGroupToBuffer ARGS_DECL((struct ObjRec*,char*, int));
void RecordRCBoxToBuffer ARGS_DECL((struct ObjRec*,char*));
void SaveAttrsToBuff ARGS_DECL((char*,struct AttrRec*));
void SaveAttrToBuff ARGS_DECL((char*,struct AttrRec*));
void SaveStringToBuff ARGS_DECL((char*,char*));
void SaveTextObjToBuff ARGS_DECL((char*,struct ObjRec*));
int ReadAttrFromBuff ARGS_DECL((char*,struct AttrRec**));
void CopyObj ARGS_DECL((register struct ObjRec*,register struct ObjRec*));
/* prototypes defined */


void RecordPolyToBuffer (ObjPtr, buff)
   struct ObjRec        * ObjPtr;
   char                 * buff;
{
   register int         i, n;
   int                  count;
   struct PolyRec       * poly_ptr = ObjPtr->detail.p;
 
   n = poly_ptr->n;
   sprintf (buff, "%spoly('%s',%1d,[\n\t", buff,
                   colorMenuItems[ObjPtr->color], poly_ptr->n);
   for (i = 0, count = 0; i < n-1; i++)
   {
      sprintf (buff, "%s%1d,%1d,", buff, poly_ptr->vlist[i].x,
                                   poly_ptr->vlist[i].y);
      if (++count == 8)
      {
         count = 0;
         sprintf (buff, "%s\n\t", buff);
      }
   }
   sprintf (buff, "%s%1d,%1d],", buff, poly_ptr->vlist[n-1].x,
                                 poly_ptr->vlist[n-1].y);
 
   sprintf (buff, "%s%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,", buff,
         poly_ptr->style, poly_ptr->width, poly_ptr->pen, ObjPtr->id,
         poly_ptr->curved, poly_ptr->fill, poly_ptr->dash, ObjPtr->rotation,
         poly_ptr->aw, poly_ptr->ah, ObjPtr->locked);
   sprintf (buff, "%s\n    \"", buff);
   SaveSmoothHingeToBuff (buff,poly_ptr->curved, poly_ptr->n, poly_ptr->smooth);
   sprintf (buff, "%s\",", buff);
   SaveAttrsToBuff (buff, ObjPtr->lattr);
   sprintf (buff, "%s)", buff);

}

static char     hexValue[] = "0123456789abcdef";

void SaveSmoothHingeToBuff (buff, Curved, NumPts, Smooth)
   char * buff;
   int  Curved, NumPts;
   char * Smooth;
{
   register int nibble_count=0, bit_count=0, data=0, j;
 
   if (Curved == LT_INTSPLINE || Smooth == NULL) return;
 
   for (j = 0; j < NumPts; j++)
   {
      data = (Smooth[j] ? (data<<1) | 1 : (data<<1));
 
      if (++bit_count == 4)
      {
         if (nibble_count++ == 64)
         {
            nibble_count = 1;
            sprintf (buff, "%s\n     ", buff);
         }
         sprintf (buff, "%s%c", buff, hexValue[data]);
         bit_count = 0;
         data = 0;
      }
   }
   if ((NumPts & 0x3) != 0)
   {
      data <<= (4 - (NumPts & 0x3));
      if (nibble_count++ == 64)
      {
         nibble_count = 1;
         sprintf (buff, "%s\n     ", buff);
      }
      sprintf (buff, "%s%c", buff, hexValue[data]);
   }
}


void RecordBoxToBuffer (ObjPtr, buff)
   struct ObjRec        * ObjPtr;
   char                 * buff;
{
 
   sprintf (buff, "%sbox('%s',", buff,colorMenuItems[ObjPtr->color]);
   sprintf (buff, "%s%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,", buff,
         ObjPtr->obbox.ltx, ObjPtr->obbox.lty, ObjPtr->obbox.rbx,
         ObjPtr->obbox.rby, ObjPtr->detail.b->fill, ObjPtr->detail.b->width,
         ObjPtr->detail.b->pen, ObjPtr->id, ObjPtr->detail.b->dash,
         ObjPtr->rotation, ObjPtr->locked);
   SaveAttrsToBuff (buff, ObjPtr->lattr);
   sprintf (buff, "%s)",buff);
}
 
void RecordOvalToBuffer (ObjPtr, buff)
   struct ObjRec        * ObjPtr;
   char                 * buff;
{
   sprintf (buff, "%soval('%s',", buff, colorMenuItems[ObjPtr->color]);
   sprintf (buff, "%s%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,",buff,
         ObjPtr->obbox.ltx, ObjPtr->obbox.lty, ObjPtr->obbox.rbx,
         ObjPtr->obbox.rby, ObjPtr->detail.o->fill, ObjPtr->detail.o->width,
         ObjPtr->detail.o->pen, ObjPtr->id, ObjPtr->detail.o->dash,
         ObjPtr->rotation, ObjPtr->locked);
   SaveAttrsToBuff (buff, ObjPtr->lattr);
   sprintf (buff, "%s)", buff);
}

void RecordTextToBuffer (ObjPtr, buff)
   struct ObjRec        * ObjPtr;
   char                 * buff;
{

   register struct TextRec      * text_ptr = ObjPtr->detail.t;
   register struct StrRec       * s_ptr;
   char                         font_str[MAXSTRING];
 
   GetPSFontStr (text_ptr->font, text_ptr->style, font_str);
   sprintf (buff, "%stext('%s',", buff, colorMenuItems[ObjPtr->color]);
   sprintf (buff, "%s%1d,%1d,'%s',%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,[\n",buff,
         ObjPtr->x, ObjPtr->y, &font_str[1], text_ptr->style, text_ptr->size,
         text_ptr->lines, text_ptr->just, text_ptr->rotate, text_ptr->pen,
         ObjPtr->obbox.rbx-ObjPtr->obbox.ltx,
         ObjPtr->obbox.rby-ObjPtr->obbox.lty, ObjPtr->id, 0, /* dummy dpi */
         text_ptr->asc, text_ptr->des, text_ptr->fill, text_ptr->v_space,
         ObjPtr->rotation, ObjPtr->locked);

   for (s_ptr = text_ptr->first; s_ptr->next != NULL; s_ptr = s_ptr->next)
   {
      sprintf (buff, "%s\t\"", buff);
      SaveStringToBuff (buff, s_ptr->dyn_str.s);
      sprintf (buff, "%s\",\n", buff);
   }
 
   sprintf (buff, "%s\t\"", buff);
   SaveStringToBuff (buff, s_ptr->dyn_str.s);

   sprintf (buff, "%s\"])", buff);
}


void RecordArcToBuffer (ObjPtr, buff)
   struct ObjRec        * ObjPtr;
   char                 * buff;
{
   register struct ArcRec       * arc_ptr = ObjPtr->detail.a;
 
   sprintf (buff, "%sarc('%s',", buff, colorMenuItems[ObjPtr->color]);
   sprintf (buff, "%s%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,\
%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,", buff,
         arc_ptr->fill, arc_ptr->width, arc_ptr->pen, arc_ptr->dash,
         arc_ptr->ltx, arc_ptr->lty, arc_ptr->xc, arc_ptr->yc,
         arc_ptr->x1, arc_ptr->y1, arc_ptr->x2, arc_ptr->y2,
         arc_ptr->dir, arc_ptr->w, arc_ptr->h, arc_ptr->angle1, arc_ptr->angle2,
         ObjPtr->id, ObjPtr->rotation, arc_ptr->style, arc_ptr->aw,
         arc_ptr->ah, ObjPtr->locked);

   SaveAttrsToBuff (buff, ObjPtr->lattr);
   sprintf (buff, "%s)", buff);
}

void RecordPolygonToBuffer (ObjPtr, buff)
   struct ObjRec        * ObjPtr;
   char                 * buff;
{
   register int         i, n;
   int                  count;
   struct PolygonRec    * polygon_ptr = ObjPtr->detail.g;
 
   n = polygon_ptr->n;
   sprintf (buff , "%spolygon('%s',%1d,[\n\t", buff, 
                   colorMenuItems[ObjPtr->color], polygon_ptr->n);
   for (i = 0, count = 0; i < n-1; i++)
   {
      sprintf (buff , "%s%1d,%1d,", buff, polygon_ptr->vlist[i].x, 
                      polygon_ptr->vlist[i].y);
      if (++count == 8)
      {
         count = 0;
         sprintf (buff , "%s\n\t", buff);
      }
   }
   sprintf (buff , "%s%1d,%1d],", buff, polygon_ptr->vlist[n-1].x, 
                   polygon_ptr->vlist[n-1].y);
   sprintf (buff , "%s%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,", buff, 
                   polygon_ptr->fill, polygon_ptr->width, 
                   polygon_ptr->pen, polygon_ptr->curved, 
                   ObjPtr->id, polygon_ptr->dash, ObjPtr->rotation, 
                   ObjPtr->locked);
   sprintf (buff , "%s\n    \"", buff);
   SaveSmoothHingeToBuff (buff, polygon_ptr->curved, polygon_ptr->n, 
                                polygon_ptr->smooth);
   sprintf (buff , "%s\",", buff);
   SaveAttrsToBuff (buff , ObjPtr->lattr);
   sprintf (buff , "%s)", buff);
}
 
void RecordGroupToBuffer (ObjPtr, buff, Level)
   struct ObjRec        * ObjPtr;
   char                 * buff;
   int                    Level;
{
   sprintf (buff, "%sgroup([\n", buff);
   SaveToBuffer (buff, ObjPtr->detail.r->last, Level+1, INVALID);

   sprintf (buff, "%s],\n", buff);
   sprintf (buff, "%s%1d,%1d,", buff, ObjPtr->id, ObjPtr->locked);
   SaveAttrsToBuff (buff, ObjPtr->lattr);
   sprintf (buff, "%s)", buff);

}

void RecordRCBoxToBuffer (ObjPtr, buff)
   struct ObjRec        * ObjPtr;
   char                 * buff;
{

   sprintf (buff, "%srcbox('%s',", buff, colorMenuItems[ObjPtr->color]);
   sprintf (buff, "%s%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,",buff,
         ObjPtr->obbox.ltx,
         ObjPtr->obbox.lty, ObjPtr->obbox.rbx, ObjPtr->obbox.rby,
         ObjPtr->detail.rcb->fill, ObjPtr->detail.rcb->width,
         ObjPtr->detail.rcb->pen, ObjPtr->detail.rcb->dash,
         ObjPtr->detail.rcb->radius, ObjPtr->id, ObjPtr->rotation,
         ObjPtr->locked);
   SaveAttrsToBuff (buff, ObjPtr->lattr);
   sprintf (buff, "%s)", buff);
}



void SaveAttrsToBuff (buff, AttrPtr)
   char                 * buff;
   struct AttrRec       * AttrPtr;
{
 
 
   struct AttrRec       * ptr;
 
   sprintf (buff, "%s[\n", buff);
 
   for (ptr = AttrPtr; ptr != NULL; ptr = ptr->prev)
   {
      SaveAttrToBuff (buff, ptr);
      if (ptr->prev != NULL)
         sprintf (buff, "%s,\n", buff);
   }
 
   if (AttrPtr == NULL)
   {
      strcat(buff,"]");
   }
   else
   {
      strcat(buff, "\n]");
   }
 
}
 

void SaveAttrToBuff (buff, AttrPtr)
   char                 * buff;
   struct AttrRec       * AttrPtr;
{
 
   sprintf (buff, "%sattr(\"", buff);
   SaveStringToBuff (buff, AttrPtr->attr_name.s);
   sprintf (buff, "%s\", \"", buff);
   SaveStringToBuff (buff, AttrPtr->attr_value.s);
   sprintf (buff, "%s\", %1d, %1d, %1d,\n", buff,
         AttrPtr->shown, AttrPtr->nameshown, AttrPtr->inherited);
 
   SaveTextObjToBuff (buff, AttrPtr->obj);
   sprintf (buff, "%s)",buff);
 
}
 
void SaveStringToBuff (buff, S)
   char                 * buff;
   char                 * S;
{
   for ( ; *S != '\0'; S++)
   {
      if (*S == '\\')
      {
           strcat(buff,"\\\\");
      }
      else if (*S == '"')
      {
         if (doubleQuoteDoubleQuote)
         {
            sprintf (buff, "%s%s", buff,  "\"\"");
         }
         else
         {
            sprintf (buff, "%s%s", buff,  "\\\"");
         }
      }
      else if ((*S) & 0x80)
      {
         sprintf (buff, "%s\\%o", buff, (*S)&0xff);
      }
      else
      {
         sprintf(buff, "%s%c", buff, *S);
         /*strcat(*buff,*S);*/
      }
 
   }
 
}
 

void SaveTextObjToBuff (buff, ObjPtr)
   char                 * buff;
   struct ObjRec        * ObjPtr;
{
 
   register struct TextRec      * text_ptr = ObjPtr->detail.t;
   register struct StrRec       * s_ptr;
   char                         font_str[MAXSTRING];
 
   GetPSFontStr (text_ptr->font, text_ptr->style, font_str);
   sprintf (buff, "%stext('%s',", buff, colorMenuItems[ObjPtr->color]);
   sprintf (buff,
       "%s%1d,%1d,'%s',%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,%1d,[\n", buff,
         ObjPtr->x, ObjPtr->y, &font_str[1], text_ptr->style, text_ptr->size,
         text_ptr->lines, text_ptr->just, text_ptr->rotate, text_ptr->pen,
         ObjPtr->obbox.rbx-ObjPtr->obbox.ltx,
         ObjPtr->obbox.rby-ObjPtr->obbox.lty, ObjPtr->id, 0,
         text_ptr->asc, text_ptr->des, text_ptr->fill, text_ptr->v_space,
         ObjPtr->rotation, ObjPtr->locked);
 
   for (s_ptr = text_ptr->first; s_ptr->next != NULL; s_ptr = s_ptr->next)
   {
      sprintf (buff, "%s\t\"", buff);
      sprintf (buff, "%s\",\n", buff);
   }
 
   sprintf (buff, "%s\t\"", buff);
   sprintf (buff, "%s\"])", buff);
 
}
 
 
#define GETVALUE(val,name) ScanValue("%d", &(val), name, "attribute")

int ReadAttrFromBuff (buff, AttrPtr)
   char                 * buff;
   struct AttrRec       * * AttrPtr;
{
 
   struct ObjRec        * TextObj;
   char                 inbuf[MAXSTRING+1], * s, * line, * c_ptr;
   char                 * name, * value;
   int                  len, shown, nameshown, inherited, cur_size;
   int                  done, allocated = FALSE;
 
 
   len = strlen(buff);
   if (buff[len-1] != '\r' && buff[len-1] != '\n')
   {  /* line longer than MAXSTRING characters */
      cur_size = 2*MAXSTRING-1;
      line = (char *) malloc ((cur_size+1)*sizeof(char));
      if (line == NULL) FailAllocMessage();
      allocated = TRUE;
      strcpy (line, buff);
      c_ptr = &(line[MAXSTRING-1]);
 
      done = FALSE;
   }
   else
   {
      line = buff;
      line[len-1] = '\0';
   }
 
   if (line[0] == ']')
   {
      if (allocated) free(line);
      return (FALSE);
   }
 
   *AttrPtr = NULL;
 
   len = strlen(line) - 1;
   line[len] = '\0';
 
   name = (char *) malloc ((len+1)*sizeof(char));
   if (name == NULL) FailAllocMessage();
   s = FindChar ('"', line);
   strcpy(name, s);
   s = ReadAttrString (s);
   s = FindChar (',', s);
   value = (char *) malloc ((len+1)*sizeof(char));
   if (value == NULL) FailAllocMessage();
   strcpy(value, FindChar ('"', s));
   s = ReadAttrString (value);
   s = FindChar (',', s);
 
   InitScan (s, "\t\n ,");
   if (GETVALUE (shown, "shown") == INVALID || 
       GETVALUE (nameshown, "name shown") == INVALID || 
       GETVALUE (inherited, "inherited") == INVALID) 
   {
      if (allocated) free(line);
      free(name);
      free(value);
      return (FALSE);
   }
 
   *AttrPtr = (struct AttrRec *) malloc (sizeof(struct AttrRec));
   if (*AttrPtr == NULL) FailAllocMessage();
   memset(*AttrPtr, 0, sizeof(struct AttrRec));
 
   s = ReadString (name);
   *(--s) = '\0';
   strcpy ((*AttrPtr)->attr_name.s, name);
   s = ReadString (value);
   *(--s) = '\0';
   strcpy ((*AttrPtr)->attr_value.s, value);
 
   free(name);
   free(value);
 
   (*AttrPtr)->shown = shown;
   (*AttrPtr)->nameshown = nameshown;
   (*AttrPtr)->inherited = inherited;
 
   if (allocated) free(line);
   return (TRUE);
}


void CopyObj(obj_from, obj_to)
   register struct ObjRec       * obj_from, * obj_to;
{
   DupObjBasics (obj_from, obj_to);

   switch (obj_to->type)
   {
      case OBJ_POLY:
         DupPolyObj (obj_from->detail.p, obj_to);
         break;
      case OBJ_BOX:
         DupBoxObj (obj_from->detail.b, obj_to);
         break;
      case OBJ_OVAL:
         DupOvalObj (obj_from->detail.o, obj_to);
         break;
      case OBJ_TEXT: DupTextObj (obj_from->detail.t, obj_from, obj_to); 
         break;
      case OBJ_POLYGON:
         DupPolygonObj (obj_from->detail.g, obj_to);
         break;
      case OBJ_ARC:
         DupArcObj (obj_from->detail.a, obj_to);
         break;
      case OBJ_RCBOX:
         DupRCBoxObj (obj_from->detail.rcb, obj_to);
         break;
      case OBJ_XBM:
         DupXBmObj (obj_from->detail.xbm, obj_to);
         break;
      case OBJ_XPM:
         DupXPmObj (obj_from->detail.xpm, obj_to);
         break;
      case OBJ_SYM:
      case OBJ_GROUP:
      case OBJ_ICON:
         DupGroupObj (obj_from->detail.r, obj_to);
         if (obj_from->type == OBJ_ICON)
         {
            strcpy (obj_to->detail.r->s, obj_from->detail.r->s);
            obj_to->detail.r->rotate = obj_from->detail.r->rotate;
            obj_to->detail.r->flip = obj_from->detail.r->flip;
            obj_to->detail.r->deck_index = (-1);
         }
         break;
   }
   DupAttrs (obj_from, obj_to);

}


#endif    /* _TGIF_WB */
