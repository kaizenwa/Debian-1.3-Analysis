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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/attr.c,v 3.0 1996/05/06 16:03:44 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "const.h"
#include "types.h"

#ifndef _NO_EXTERN
#include "attr.e"
#endif
#include "auxtext.e"
#include "button.e"
#include "choice.e"
#include "choose.e"
#include "cmd.e"
#include "color.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#include "file.e"
#include "font.e"
#include "grid.e"
#include "mainloop.e"
#include "mainmenu.e"
#include "mark.e"
#include "menu.e"
#include "move.e"
#include "msg.e"
#include "names.e"
#include "obj.e"
#include "pattern.e"
#include "raster.e"
#include "rect.e"
#include "ruler.e"
#include "select.e"
#include "setup.e"
#include "stk.e"
#include "text.e"
#include "util.e"

#define PAINT 0
#define ERASE 1

#ifndef XK_KP_Left
#define XK_KP_Left	0xFF96
#define XK_KP_Up	0xFF97
#define XK_KP_Right	0xFF98
#define XK_KP_Down	0xFF99
#endif /* ~XK_KP_LEFT */

int		dropObsIconAttrWhenUpdate = FALSE;

static struct AttrRec	* topAttr=NULL, * botAttr=NULL;

void LinkInAttr (PrevPtr, NextPtr, AttrPtr)
   struct AttrRec	* PrevPtr, * NextPtr, * AttrPtr;
   /* add AttrPtr between PrevPtr and NextPtr */
{
   AttrPtr->prev = PrevPtr;
   AttrPtr->next = NextPtr;

   if (PrevPtr == NULL)
      topAttr = AttrPtr;
   else
      PrevPtr->next = AttrPtr;

   if (NextPtr == NULL)
      botAttr = AttrPtr;
   else
      NextPtr->prev = AttrPtr;
}

void FreeAttr (AttrPtr)
   struct AttrRec	* AttrPtr;
   /* This routine only frees the attribute record, not   */
   /*    the text record, which must be freed explicitly. */
{
   if (AttrPtr->attr_name.s != NULL) free(AttrPtr->attr_name.s);
   if (AttrPtr->attr_value.s != NULL) free(AttrPtr->attr_value.s);
   free(AttrPtr);
}

void UnlinkAttr (AttrPtr)
   struct AttrRec	* AttrPtr;
{
   struct ObjRec	* own_ptr;
   struct AttrRec	* * top_attr_ad;
   struct AttrRec	* * bot_attr_ad;

   own_ptr = AttrPtr->owner;
 
   top_attr_ad = &(own_ptr->fattr);
   bot_attr_ad = &(own_ptr->lattr);
     
   if (*top_attr_ad == AttrPtr)
      *top_attr_ad = AttrPtr->next;
   else
      AttrPtr->prev->next = AttrPtr->next;

   if (*bot_attr_ad == AttrPtr)
      *bot_attr_ad = AttrPtr->prev;
   else
      AttrPtr->next->prev = AttrPtr->prev;
}

static
char * FindEqual(s)
   register char	* s;
{
   while (*s != '=' && *s != '\0') s++;
   return ((*s == '=') ? (s) : (char *)NULL);
}

static
void ParseAttrStr(Str, name, s)
   char	* Str, * name, * s;
{
   char	* eq_ptr, * str_ptr, * ptr;

   if ((eq_ptr = FindEqual(Str)) != NULL)
   {
      eq_ptr++;
      ptr = name;
      str_ptr = Str;
      do
      {
         *ptr = *str_ptr; 
         ptr++;
         str_ptr++;
      } while (str_ptr != eq_ptr);
 
      *ptr = '\0';

      ptr = s;
      do 
      {
         *ptr = *str_ptr; 
         ptr++;
         str_ptr++;
      } while (*str_ptr != '\0');

      *ptr = '\0';
   }
   else
   {
      *name = '\0';
      strcpy (s, Str);
   }
} 

void UpdateAttr (TextPtr, AttrPtr)
   struct TextRec	* TextPtr;
   struct AttrRec	* AttrPtr;
   /* This routine updates the name and value in the AttrRec */
   /*   and its ObjRec after an attribute was edited.        */
{
   char	value[MAXSTRING+1], name[MAXSTRING+1];

   if (AttrPtr->nameshown)
   {
      ParseAttrStr (TextPtr->first->dyn_str.s, name, value);
      DynStrSet (&AttrPtr->attr_value, value);
      DynStrSet (&AttrPtr->attr_name, name);
      strcat (name, value);
      DynStrSet (&TextPtr->first->dyn_str, name);
   }
   else
   {
      strcpy (value, TextPtr->first->dyn_str.s);
      DynStrSet (&AttrPtr->attr_value, value);
      DynStrSet (&TextPtr->first->dyn_str, value);
   }
   UpdTextBBox (AttrPtr->obj);
}

void DrawAttrs (Win, XOff, YOff, AttrPtr)
   Window		Win;
   int			XOff, YOff;
   struct AttrRec	* AttrPtr;
{
   struct AttrRec	* ptr;

   for (ptr = AttrPtr; ptr != NULL; ptr = ptr->next)
      if (ptr->shown)
         DrawTextObj(Win, XOff, YOff, ptr->obj);
}

void MoveAttrs(AttrPtr, Dx, Dy)
   int Dx, Dy;
   struct AttrRec *AttrPtr;
{
   register struct AttrRec *attr_ptr;

   for (attr_ptr=AttrPtr; attr_ptr != NULL; attr_ptr=attr_ptr->next) {
      MoveObj(attr_ptr->obj, Dx, Dy);
   }
}

void DelAllAttrs (AttrPtr)
   struct AttrRec	* AttrPtr;
{
   struct AttrRec	* ptr, * next_attr;

   for (ptr = AttrPtr; ptr != NULL; ptr = next_attr)
   {
      next_attr = ptr->next;
      FreeTextObj(ptr->obj);
      FreeAttr(ptr);
   }
}

struct AttrRec * AddAttrByNameAndValue (ObjPtr, AttrName, AttrValue)
   struct ObjRec	* ObjPtr;
   char			* AttrName, * AttrValue;
{
   struct ObjRec * text_obj_ptr;
   struct TextRec * text_ptr;
   struct StrRec * s_ptr;
   struct AttrRec * attr_ptr;
   char s[MAXSTRING+1];

   sprintf (s, "%s%s", AttrName, AttrValue);

   text_obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   text_ptr = (struct TextRec *)malloc(sizeof(struct TextRec));
   attr_ptr = (struct AttrRec *)malloc(sizeof(struct AttrRec));
   if (text_obj_ptr == NULL || text_ptr == NULL || attr_ptr == NULL) {
      FailAllocMessage();
   }
   memset(text_obj_ptr, 0, sizeof(struct ObjRec));
   memset(text_ptr, 0, sizeof(struct TextRec));
   memset(attr_ptr, 0, sizeof(struct AttrRec));

   CopyCurInfoToTextPtr (text_ptr);
   text_ptr->lines = 1;
   text_ptr->cached_zoom = 0;
   text_ptr->cached_zoomed = FALSE;
   text_ptr->cached_rotate = ROTATE0;
   text_ptr->cached_bitmap = None;
   text_ptr->image = NULL;

   text_ptr->first = text_ptr->last = s_ptr = NewStr ();
   if (text_ptr->first == NULL) FailAllocMessage ();

   DynStrSet (&s_ptr->dyn_str, s);
   s_ptr->next = s_ptr->prev = NULL;

   text_obj_ptr->x = ObjPtr->obbox.ltx;
   text_obj_ptr->y = ObjPtr->obbox.rby;
   text_obj_ptr->type = OBJ_TEXT;
   text_obj_ptr->color = ObjPtr->color;
   text_obj_ptr->dirty = FALSE;
   text_obj_ptr->id = objId++;
   text_obj_ptr->rotation = 0;
   text_obj_ptr->locked = FALSE;
   text_obj_ptr->ctm = NULL;
   text_obj_ptr->detail.t = text_ptr;

   UpdTextBBox (text_obj_ptr);

   attr_ptr->shown = FALSE;
   attr_ptr->nameshown = TRUE;
   attr_ptr->inherited = FALSE;
   attr_ptr->obj = text_obj_ptr;
   attr_ptr->owner = ObjPtr;
   DynStrSet (&attr_ptr->attr_name, AttrName);
   DynStrSet (&attr_ptr->attr_value, AttrValue);

   text_ptr->attr = attr_ptr;

   attr_ptr->prev = NULL;
   attr_ptr->next = ObjPtr->fattr;
   if (ObjPtr->fattr == NULL)
      ObjPtr->lattr = attr_ptr;
   else
      ObjPtr->fattr->prev = attr_ptr;
   ObjPtr->fattr = attr_ptr;
   AdjObjBBox (ObjPtr);
   return (attr_ptr);
}

static
struct AttrRec	* NewAttr (OwnerObjPtr, ObjPtr, Inherited)
   struct ObjRec	* ObjPtr;
   struct ObjRec	* OwnerObjPtr;
   short		Inherited;
{
   struct AttrRec	* attr_ptr;
    
   attr_ptr = (struct AttrRec *)malloc(sizeof(struct AttrRec));
   if (attr_ptr == NULL) FailAllocMessage();
   memset(attr_ptr, 0, sizeof(struct AttrRec));
   attr_ptr->shown = TRUE;
   attr_ptr->nameshown = TRUE;
   attr_ptr->inherited = Inherited;
   attr_ptr->obj = ObjPtr;
   attr_ptr->next= attr_ptr->prev = NULL;
   attr_ptr->owner = OwnerObjPtr;
   DynStrSet (&attr_ptr->attr_name, "");
   DynStrSet (&attr_ptr->attr_value, "");
   ObjPtr->detail.t->attr = attr_ptr;

   return attr_ptr;
}

static
void DupAnAttr (FromAttrPtr, ToAttrPtr)
   register struct AttrRec	* FromAttrPtr, * ToAttrPtr;
{
   struct ObjRec	* text_obj_ptr;

   text_obj_ptr = (struct ObjRec *)malloc(sizeof(struct ObjRec));
   if (text_obj_ptr == NULL) FailAllocMessage ();
   memset(text_obj_ptr, 0, sizeof(struct ObjRec));
   DupObjBasics (FromAttrPtr->obj, text_obj_ptr);
   DupTextObj (FromAttrPtr->obj->detail.t, FromAttrPtr->obj, text_obj_ptr);

   DynStrCpy (&ToAttrPtr->attr_name, &FromAttrPtr->attr_name);
   DynStrCpy (&ToAttrPtr->attr_value, &FromAttrPtr->attr_value);
   ToAttrPtr->shown = FromAttrPtr->shown;
   ToAttrPtr->nameshown = FromAttrPtr->nameshown;
   ToAttrPtr->inherited = FromAttrPtr->inherited;
   ToAttrPtr->obj = text_obj_ptr;
   ToAttrPtr->next= ToAttrPtr->prev = NULL;
   text_obj_ptr->detail.t->attr = ToAttrPtr;
}

void DupAttrs (FromObjPtr, ToObjPtr)
   register struct ObjRec	* FromObjPtr, * ToObjPtr;
{
   register struct AttrRec	* to_attr_ptr, * from_attr_ptr;

   topAttr = botAttr = NULL;
   from_attr_ptr = FromObjPtr->lattr;
   for ( ; from_attr_ptr != NULL; from_attr_ptr = from_attr_ptr->prev)
   {
      to_attr_ptr = (struct AttrRec *)malloc(sizeof(struct AttrRec));
      if (to_attr_ptr == NULL) FailAllocMessage ();
      memset(to_attr_ptr, 0, sizeof(struct AttrRec));
      to_attr_ptr->owner = ToObjPtr;
      DupAnAttr (from_attr_ptr, to_attr_ptr);
      LinkInAttr ((struct AttrRec *)NULL, topAttr, to_attr_ptr);
   }
   ToObjPtr->fattr = topAttr;
   ToObjPtr->lattr = botAttr;
}

static
void AddAttr (ObjPtr, TextObjPtr)
   struct ObjRec	* ObjPtr, * TextObjPtr;
{
   struct AttrRec	* attr_ptr;
   struct TextRec	* text_ptr;
   char			name[MAXSTRING+1], value[MAXSTRING+1];

   text_ptr = TextObjPtr->detail.t; 

   ParseAttrStr(text_ptr->first->dyn_str.s, name, value);
   topAttr = ObjPtr->fattr;
   botAttr = ObjPtr->lattr;

   UnlinkObj (TextObjPtr);
   TextObjPtr->next = TextObjPtr->prev = NULL;
   attr_ptr = NewAttr (ObjPtr, TextObjPtr, FALSE); 
   UpdateAttr (text_ptr, attr_ptr); 
   LinkInAttr ((struct AttrRec *)NULL, topAttr, attr_ptr);

   ObjPtr->fattr = topAttr;
   ObjPtr->lattr = botAttr;
}

void AddAttrs ()
{
   struct ObjRec	* owner_ptr = NULL;
   struct SelRec	* sel_ptr;
   int			text_count=0, sel_ltx, sel_lty, sel_rbx, sel_rby;
   int			locked_text_count=0;

   if (topSel == NULL)
   {
      MsgBox ("Must select at least one text object.", TOOL_NAME, INFO_MB);
      return;
   }
   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
      switch (sel_ptr->obj->type)
      {
         case OBJ_TEXT: text_count++; break;

         case OBJ_BOX:
         case OBJ_OVAL:
         case OBJ_POLYGON:
         case OBJ_POLY:
         case OBJ_SYM:
         case OBJ_GROUP:
         case OBJ_ICON:
         case OBJ_ARC:
         case OBJ_RCBOX:
         case OBJ_XBM:
         case OBJ_XPM:
            if (owner_ptr != NULL)
            {
               Msg("Two non-text objects selected.");
               return;
            }
            owner_ptr = sel_ptr->obj;
            break; 
      }
 
   if (text_count == 0)
   {
      MsgBox ("No text objects selected to add as attributes.",
            TOOL_NAME, INFO_MB);
      return;
   }
   if (owner_ptr == NULL)
   {
      MsgBox ("No objects (other than TEXT objects) selected.",
            TOOL_NAME, INFO_MB);
      return;
   }
   HighLightReverse ();
   PrepareToRecord (CMD_REPLACE, topSel, botSel, numObjSelected);
   sel_ltx = selLtX; sel_lty = selLtY;
   sel_rbx = selRbX; sel_rby = selRbY;

   for (sel_ptr = botSel;  sel_ptr != NULL; sel_ptr = sel_ptr->prev)
      if (sel_ptr->obj->type == OBJ_TEXT)
      {
         if (sel_ptr->obj->locked)
         {
            locked_text_count++;
            sel_ptr->obj->locked = FALSE;
         }
         AddAttr(owner_ptr, sel_ptr->obj);
      }

   RemoveAllSel ();
   UnlinkObj (owner_ptr);
   AddObj (NULL, topObj, owner_ptr);
   topSel = botSel = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (topSel == NULL) FailAllocMessage ();
   topSel->obj = owner_ptr;
   topSel->prev = NULL;
   botSel->next = NULL;
   AdjObjBBox (owner_ptr);
   UpdSelBBox ();
   RecordCmd (CMD_MANY_TO_ONE, NULL, topSel, botSel, 1);
   RedrawAreas (botObj, sel_ltx-GRID_ABS_SIZE(1), sel_lty-GRID_ABS_SIZE(1),
         sel_rbx+GRID_ABS_SIZE(1), sel_rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward ();
   SetFileModified (TRUE);
   justDupped = FALSE;
   if (locked_text_count==0)
      Msg ("Text object(s) attached.");
   else
      Msg ("Text object(s) unlocked and attached.");
}

static
void SaveAttr (FP, AttrPtr)
   FILE			* FP;
   struct AttrRec	* AttrPtr;
{
   if (fprintf (FP, "attr(\"") == EOF) writeFileFailed = TRUE;
   SaveString (FP, AttrPtr->attr_name.s);
   if (fprintf (FP, "\", \"") == EOF) writeFileFailed = TRUE;
   SaveString (FP, AttrPtr->attr_value.s);
   if (fprintf (FP, "\", %1d, %1d, %1d,\n",
         AttrPtr->shown, AttrPtr->nameshown, AttrPtr->inherited) == EOF)
      writeFileFailed = TRUE;
  
   SaveTextObj (FP, AttrPtr->obj);
   if (fprintf (FP, ")") == EOF) writeFileFailed = TRUE;
}

void SaveAttrs (FP, BotAttrPtr)
   FILE			* FP;
   struct AttrRec	* BotAttrPtr;
{
 
   struct AttrRec       * ptr;

   if (fprintf (FP, "[\n") == EOF) writeFileFailed = TRUE;

   for (ptr = BotAttrPtr; ptr != NULL; ptr = ptr->prev)
   {
      SaveAttr (FP, ptr);
      if (ptr->prev != NULL)
         if (fprintf (FP, ",\n") == EOF)
            writeFileFailed = TRUE;
   }

   if (BotAttrPtr == NULL)
   {
      if (fprintf (FP, "]") == EOF) writeFileFailed = TRUE;
   }
   else
   {
      if (fprintf (FP, "\n]") == EOF) writeFileFailed = TRUE;
   }
}

char * ReadAttrString (Str)
   char	* Str;
{
   register char	* s;

   for (s = Str; *s != '\0'; s++)
      if (*s == '"')
      {
         if (s[1] == '"')
            s++;
         else
            break;
      }
      else if (*s == '\\')
         s++;

   if (*s == '"') s++;
   return (s);
}

static
int FreeBufAndReturn(line, rc)
   char *line;
   int rc;
{
   if (line != NULL) free(line);
   return rc;
}

#define GETVALUE(val,name) ScanValue("%d", &(val), name, "attribute")

int ReadAttr (FP, AttrPtr)
   FILE			* FP;
   struct AttrRec	* * AttrPtr;
{
   struct ObjRec	* TextObj;
   char			* s, * line, * c_ptr;
   char			* name, * value;
   int			len, shown, nameshown, inherited;
 
   if ((line=UtilGetALine(FP)) == NULL) {
      sprintf(gszMsgBox, "%s, %d:  EOF in ReadAttr ().  Read aborted!",
            scanFileName, scanLineNum);
      if (PRTGIF) {
         fprintf(stderr, "%s\n", gszMsgBox);
      } else {
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      }
      return FALSE;
   }
   scanLineNum++;

   if (*line == ']') return FreeBufAndReturn(line, FALSE);

   *AttrPtr = NULL;
   len = strlen(line);

   name = (char *)malloc((len+1)*sizeof(char));
   if (name == NULL) FailAllocMessage ();
   s = FindChar ((int)'"', line);
   strcpy(name, s);
   s = ReadAttrString (s);
   s = FindChar ((int)',', s);
   value = (char *)malloc((len+1)*sizeof(char));
   if (value == NULL) FailAllocMessage ();
   strcpy(value, FindChar ((int)'"', s));
   s = ReadAttrString (value);
   s = FindChar ((int)',', s);

   InitScan (s, "\t\n ,");
   if (GETVALUE (shown, "shown") == INVALID ||
       GETVALUE (nameshown, "name shown") == INVALID ||
       GETVALUE (inherited, "inherited") == INVALID)
   {
      free(name);
      free(value);
      return FreeBufAndReturn(line, FALSE);
   }

   *AttrPtr = (struct AttrRec *)malloc(sizeof(struct AttrRec)); 
   if (*AttrPtr == NULL) FailAllocMessage();
   memset(*AttrPtr, 0, sizeof(struct AttrRec));

   s = ReadString (name);
   *(--s) = '\0';
   DynStrSet (&((*AttrPtr)->attr_name), name);
   s = ReadString (value);
   *(--s) = '\0';
   DynStrSet (&((*AttrPtr)->attr_value), value);

   free(name);
   free(value);

   (*AttrPtr)->shown = shown;
   (*AttrPtr)->nameshown = nameshown;
   (*AttrPtr)->inherited = inherited;

   if (ReadObj(FP, &TextObj) == FALSE) {
     free(*AttrPtr);
     *AttrPtr = NULL;
     return FreeBufAndReturn(line, FALSE);
   }
   TextObj->detail.t->attr = *AttrPtr;
   (*AttrPtr)->obj = TextObj;

   return FreeBufAndReturn(line, TRUE);
}

static
int ObjMightChangeForShowAndUpdAttrNames (ObjPtr)
   struct ObjRec	* ObjPtr;
{
   register struct AttrRec	* attr_ptr;

   if ((attr_ptr = ObjPtr->fattr) != NULL)
      for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
         if (!(attr_ptr->nameshown))
            return (TRUE);

   return (FALSE);
}

static
void UpdateFirstTextStringWithAttrNameAndValue (ObjPtr, AttrPtr)
   struct ObjRec	* ObjPtr;
   struct AttrRec	* AttrPtr;
{
   int			len1, len2;
   char			s[MAXSTRING+1], msg[80];
   struct DynStrRec	* dest_dyn_str=(&ObjPtr->detail.t->first->dyn_str);

   DynStrCpy (dest_dyn_str, &AttrPtr->attr_name);

   len1 = AttrPtr->attr_name.sz-1;
   len2 = AttrPtr->attr_value.sz-1;
   if (len1+len2 >= MAXSTRING)
   {
      sprintf (msg, "String length exceeds %1d.  %s.",
            MAXSTRING, "String truncated");
      Msg (msg);
      AttrPtr->attr_value.s[MAXSTRING-len1] = '\0';
      AttrPtr->attr_value.sz = MAXSTRING-len1+1;
   }
   sprintf (s, "%s%s", dest_dyn_str->s, AttrPtr->attr_value.s);
   DynStrSet (dest_dyn_str, s);
}

static
int DoShowAndUpdAttrNames (ObjPtr, Force)
   struct ObjRec	* ObjPtr;
   int			Force;
   /* Force will force attribute name to be shown whether the attribute */
   /*    is inherited or not.                                           */
{
   register struct AttrRec	* attr_ptr=ObjPtr->fattr;
   int				picture_changed = FALSE;

   for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
   {
      if (!(attr_ptr->nameshown) && (Force || !(attr_ptr->inherited)))
      {
         UpdateFirstTextStringWithAttrNameAndValue (attr_ptr->obj, attr_ptr);

         attr_ptr->nameshown = TRUE;
         UpdTextBBox (attr_ptr->obj);
         if (attr_ptr->shown)
         {
            picture_changed = TRUE;

            if (attr_ptr->obj->detail.t->cached_bitmap != None)
               XFreePixmap (mainDisplay,
                     attr_ptr->obj->detail.t->cached_bitmap);
            attr_ptr->obj->detail.t->cached_zoom = 0;
            attr_ptr->obj->detail.t->cached_bitmap = None;
         }
      }
   }
   AdjObjBBox (ObjPtr);

   return (picture_changed);
}

static
int ShowAndUpdAttrNames ()
   /* returns TRUE if any attribute names are updated                   */
   /* This routine concatinate the 'name' and 's' first of every        */
   /*    attribute of the selected object and assign that to the        */
   /*    first line of the text object the attribute pointer points to. */
{
   struct SelRec	* sel_ptr;
   int			picture_changed = FALSE;

   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
      if (ObjMightChangeForShowAndUpdAttrNames (sel_ptr->obj))
      {
         PrepareToReplaceAnObj (sel_ptr->obj);
         picture_changed = DoShowAndUpdAttrNames (sel_ptr->obj, TRUE);
         RecordReplaceAnObj (sel_ptr->obj);
      }

   return (picture_changed);
}

void ShowAllAttrNames ()
{
   HighLightReverse ();
   StartCompositeCmd ();
   if (ShowAndUpdAttrNames ())
   {
      UpdSelBBox ();
      RedrawAnArea (botObj, selLtX-GRID_ABS_SIZE(1),
            selLtY-GRID_ABS_SIZE(1), selRbX+GRID_ABS_SIZE(1),
            selRbY+GRID_ABS_SIZE(1));
      SetFileModified (TRUE);
   }
   EndCompositeCmd ();
   HighLightForward ();
}

static
int ShowAndUpdAttrs ()
   /* returns TRUE if any attribute was not shown before */
{
   struct SelRec	* sel_ptr;
   struct ObjRec	* obj_ptr;
   struct AttrRec	* attr_ptr;
   int			picture_changed = FALSE;

   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
   {
      obj_ptr = sel_ptr->obj;
      attr_ptr = obj_ptr->fattr;
      if (attr_ptr != NULL)
      {
         int	obj_changed = FALSE;

         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
            if (!attr_ptr->shown)
            {
               obj_changed = TRUE;
               break;
            }

         if (obj_changed)
         {
            PrepareToReplaceAnObj (obj_ptr);
            for (attr_ptr=obj_ptr->fattr; attr_ptr!=NULL;
                  attr_ptr=attr_ptr->next)
               if (!attr_ptr->shown)
                  attr_ptr->shown = TRUE;

            picture_changed = TRUE;
            AdjObjBBox (obj_ptr);
            RecordReplaceAnObj (obj_ptr);
         }
      }
   }
   return (picture_changed);
}

void ShowAllAttrs ()
{
   HighLightReverse ();
   StartCompositeCmd ();
   if (ShowAndUpdAttrs ())
   {
      UpdSelBBox ();
      RedrawAnArea (botObj, selLtX-GRID_ABS_SIZE(1),
            selLtY-GRID_ABS_SIZE(1), selRbX+GRID_ABS_SIZE(1),
            selRbY+GRID_ABS_SIZE(1));
      SetFileModified (TRUE);
   }
   EndCompositeCmd ();
   HighLightForward ();
}

static
int HideAndUpdAttrs ()
   /* returns TRUE if any attribute was shown */
{
   struct SelRec	* sel_ptr;
   struct ObjRec	* obj_ptr;
   struct AttrRec	* attr_ptr;
   int			picture_changed = FALSE;

   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
   {
      obj_ptr = sel_ptr->obj;
      attr_ptr = obj_ptr->fattr;
      if (attr_ptr != NULL)
      {
         int	obj_changed = FALSE;

         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
            if (attr_ptr->shown)
            {
               obj_changed = TRUE;
               break;
            }

         if (obj_changed)
         {
            PrepareToReplaceAnObj (obj_ptr);
            for (attr_ptr=obj_ptr->fattr; attr_ptr!=NULL;
                  attr_ptr=attr_ptr->next)
               if (attr_ptr->shown)
                  attr_ptr->shown = FALSE;

            picture_changed = TRUE;
            AdjObjBBox (obj_ptr);
            RecordReplaceAnObj (obj_ptr);
         }
      }
   }
   return (picture_changed);
}

void HideAllAttrs ()
{
   int	sel_ltx, sel_lty, sel_rbx, sel_rby;

   sel_ltx = selLtX; sel_lty = selLtY; sel_rbx = selRbX; sel_rby = selRbY;

   HighLightReverse ();
   StartCompositeCmd ();
   if (HideAndUpdAttrs ())
   {
      UpdSelBBox ();
      RedrawAnArea (botObj, sel_ltx-GRID_ABS_SIZE(1),
            sel_lty-GRID_ABS_SIZE(1), sel_rbx+GRID_ABS_SIZE(1),
            sel_rby+GRID_ABS_SIZE(1));
      SetFileModified (TRUE);
   }
   EndCompositeCmd ();
   HighLightForward ();
}

static
int HideAndUpdAttrNames ()
   /* returns TRUE if any attribute names are updated */
   /* For all the first line of the selected object's attributes,    */
   /*    this routine change them to the 's' field of the attribute. */
{
   struct SelRec	* sel_ptr;
   struct ObjRec	* obj_ptr;
   struct AttrRec	* attr_ptr;
   int			picture_changed = FALSE;

   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
   {
      obj_ptr = sel_ptr->obj;
      attr_ptr = obj_ptr->fattr;
      if (attr_ptr != NULL)
      {
         int	obj_change = FALSE;

         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
            if (attr_ptr->nameshown && *(attr_ptr->attr_name.s) != '\0')
            {
               obj_change = TRUE;
               break;
            }

         if (obj_change)
         {
            PrepareToReplaceAnObj (obj_ptr);
            for (attr_ptr=obj_ptr->fattr; attr_ptr!=NULL;
                  attr_ptr=attr_ptr->next)
            {
               if (attr_ptr->nameshown && *(attr_ptr->attr_name.s) != '\0')
               {
                  attr_ptr->nameshown = FALSE;
                  DynStrCpy (&attr_ptr->obj->detail.t->first->dyn_str,
                        &attr_ptr->attr_value);
                  UpdTextBBox (attr_ptr->obj);
                  if (attr_ptr->shown)
                  {
                     picture_changed = TRUE;

                     if (attr_ptr->obj->detail.t->cached_bitmap != None)
                        XFreePixmap (mainDisplay,
                              attr_ptr->obj->detail.t->cached_bitmap);
                     attr_ptr->obj->detail.t->cached_zoom = 0;
                     attr_ptr->obj->detail.t->cached_bitmap = None;
                  }
               }
            }
            AdjObjBBox (obj_ptr);
            RecordReplaceAnObj (obj_ptr);
         }
      }
   }
   return (picture_changed);
}

void HideAllAttrNames ()
{
   int	sel_ltx, sel_lty, sel_rbx, sel_rby;

   sel_ltx = selLtX; sel_lty = selLtY; sel_rbx = selRbX; sel_rby = selRbY;

   HighLightReverse ();
   StartCompositeCmd ();
   if (HideAndUpdAttrNames ())
   {
      UpdSelBBox ();
      RedrawAnArea (botObj, sel_ltx-GRID_ABS_SIZE(1),
            sel_lty-GRID_ABS_SIZE(1), sel_rbx+GRID_ABS_SIZE(1),
            sel_rby+GRID_ABS_SIZE(1));
      SetFileModified (TRUE);
   }
   EndCompositeCmd ();
   HighLightForward ();
}

void DetachGroupAttrs (ObjPtr, TopSelPtr, BotSelPtr)
   struct ObjRec	* ObjPtr;
   struct SelRec	* * TopSelPtr, * * BotSelPtr;
{
   struct AttrRec	* attr_ptr, * next_attr;
   struct SelRec	* new_sel_ptr;

   for (attr_ptr=ObjPtr->fattr; attr_ptr!=NULL; attr_ptr=next_attr)
   {
      next_attr = attr_ptr->next;
      if (!(attr_ptr->nameshown))
      {
         UpdateFirstTextStringWithAttrNameAndValue (attr_ptr->obj, attr_ptr);

         UpdTextBBox (attr_ptr->obj);
      }

      attr_ptr->obj->detail.t->attr = NULL;

      attr_ptr->obj->prev = NULL;
      attr_ptr->obj->next = ObjPtr->detail.r->first;

      if (attr_ptr->obj->next == NULL)
         ObjPtr->detail.r->last = attr_ptr->obj;
      else
         attr_ptr->obj->next->prev = attr_ptr->obj;
      ObjPtr->detail.r->first = attr_ptr->obj;

      new_sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
      if (new_sel_ptr == NULL) FailAllocMessage();
      new_sel_ptr->obj = attr_ptr->obj;

      new_sel_ptr->prev = NULL;
      new_sel_ptr->next = *TopSelPtr;

      if (new_sel_ptr->next == NULL)
         *BotSelPtr = new_sel_ptr;
      else
         (*TopSelPtr)->prev = new_sel_ptr;
      *TopSelPtr = new_sel_ptr;

      free(attr_ptr);
   }
}

void DetachAllObjAttrs(obj_ptr, TopSelPtr, BotSelPtr)
   struct ObjRec *obj_ptr;
   struct SelRec **TopSelPtr, **BotSelPtr;
{
   struct AttrRec *attr_ptr, *prev_attr;

   DoShowAndUpdAttrNames(obj_ptr, FALSE);

   (*TopSelPtr) = (*BotSelPtr) = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (*TopSelPtr == NULL) FailAllocMessage();
   (*TopSelPtr)->next = (*TopSelPtr)->prev = NULL;
   (*TopSelPtr)->obj = obj_ptr;

   topAttr = botAttr = NULL;
   for (attr_ptr=obj_ptr->lattr; attr_ptr!=NULL; attr_ptr=prev_attr) {
      struct SelRec *tmp_sel_ptr;

      prev_attr = attr_ptr->prev;

      tmp_sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
      if (tmp_sel_ptr == NULL) FailAllocMessage();

      tmp_sel_ptr->prev = NULL;
      tmp_sel_ptr->next = (*TopSelPtr);
      (*TopSelPtr)->prev = tmp_sel_ptr;
      (*TopSelPtr) = tmp_sel_ptr;
      tmp_sel_ptr->obj = attr_ptr->obj;

      attr_ptr->obj->detail.t->attr = NULL;
      AddObj(obj_ptr->prev, obj_ptr, attr_ptr->obj);
      free(attr_ptr);
   }
   obj_ptr->fattr = obj_ptr->lattr = NULL;
}

void DetachAttrs ()
{
   struct SelRec	* sel_ptr, * new_sel_ptr;
   struct ObjRec	* obj_ptr;
   struct AttrRec	* attr_ptr, * next_attr;
   int			picture_changed=FALSE, name_changed=FALSE;

   HighLightReverse ();
   StartCompositeCmd ();

   for (sel_ptr = topSel; sel_ptr != NULL; sel_ptr = sel_ptr->next)
   {
      obj_ptr = sel_ptr->obj;
      attr_ptr = obj_ptr->fattr;
      if (attr_ptr != NULL)
      {
         int	obj_change = FALSE;

         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next)
            if (!attr_ptr->inherited)
            {
               obj_change = TRUE;
               break;
            }

         if (obj_change)
         {
            struct SelRec	* tmp_top_sel, * tmp_bot_sel;
            struct SelRec	* tmp_sel_ptr, * next_sel;
            int			count;

            PrepareToReplaceAnObj (obj_ptr);
            if (DoShowAndUpdAttrNames (obj_ptr, FALSE)) name_changed = TRUE;

            tmp_top_sel = tmp_bot_sel = (struct SelRec *)malloc(
                  sizeof(struct SelRec));
            if (tmp_top_sel == NULL) FailAllocMessage();
            tmp_top_sel->next = tmp_top_sel->prev = NULL;
            tmp_top_sel->obj = obj_ptr;
            count = 1;

            topAttr = botAttr = NULL;
            for (attr_ptr=obj_ptr->fattr; attr_ptr!=NULL; attr_ptr=next_attr)
            {
               next_attr = attr_ptr->next;
               if (obj_ptr->type == OBJ_ICON && attr_ptr->inherited)
               {
                  LinkInAttr ((struct AttrRec *)NULL, topAttr, attr_ptr);
                  continue;
               }
               picture_changed = TRUE;

               tmp_sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
               if (tmp_sel_ptr == NULL) FailAllocMessage ();
               tmp_sel_ptr->next = tmp_bot_sel;
               tmp_sel_ptr->obj = attr_ptr->obj;
               if (tmp_top_sel == tmp_bot_sel)
               {
                  tmp_sel_ptr->prev = NULL;
                  tmp_top_sel->prev = tmp_sel_ptr;
                  tmp_top_sel = tmp_sel_ptr;
               }
               else
               {
                  tmp_sel_ptr->prev = tmp_bot_sel->prev;
                  tmp_bot_sel->prev->next = tmp_sel_ptr;
                  tmp_bot_sel->prev = tmp_sel_ptr;
               }
               count++;

               attr_ptr->obj->detail.t->attr = NULL;
               AddObj (obj_ptr->prev, obj_ptr, attr_ptr->obj);
               new_sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
               if (new_sel_ptr == NULL) FailAllocMessage ();
               new_sel_ptr->obj = obj_ptr->prev;
               AddSel(sel_ptr->prev, sel_ptr, new_sel_ptr);
               free(attr_ptr);
            }
            obj_ptr->fattr = topAttr;
            obj_ptr->lattr = botAttr;
            AdjObjBBox (obj_ptr);
            RecordCmd (CMD_ONE_TO_MANY, NULL, tmp_top_sel, tmp_bot_sel, count);

            for (tmp_sel_ptr=tmp_top_sel; tmp_sel_ptr!=NULL;
                  tmp_sel_ptr=next_sel) {
               next_sel = tmp_sel_ptr->next;
               free(tmp_sel_ptr);
            }
         }
      }
   }
   EndCompositeCmd ();
   UpdSelBBox ();
   if (picture_changed || name_changed)
   {
      RedrawAnArea (botObj, selLtX-GRID_ABS_SIZE(1),
            selLtY-GRID_ABS_SIZE(1), selRbX+GRID_ABS_SIZE(1),
            selRbY+GRID_ABS_SIZE(1));
      SetFileModified (TRUE);
   }
   HighLightForward ();
}

void UpdAttr (AttrPtr)
   struct AttrRec	* AttrPtr;
   /* Update the text object's string value associated with AttrPtr */
{
   if (AttrPtr->nameshown)
      UpdateFirstTextStringWithAttrNameAndValue (AttrPtr->obj, AttrPtr);
   else
      DynStrCpy (&AttrPtr->obj->detail.t->first->dyn_str, &AttrPtr->attr_value);
   UpdTextBBox(AttrPtr->obj);
}

static
int MoveOneAttr (ObjPtr, AttrPtr)
   struct ObjRec	* ObjPtr;
   struct AttrRec	* AttrPtr;
{
   struct ObjRec	* text_obj_ptr;
   int          	x, y, grid_x, grid_y, dx, dy, placing = TRUE;
   int          	ltx, lty, rbx, rby;
   int			orig_x, orig_y, grid_orig_x, grid_orig_y;
   char			buf[80], x_buf[80], y_buf[80];
   XEvent		input, ev;

   text_obj_ptr = AttrPtr->obj;
   Msg ("LEFT--show and move.  MIDDLE--toggle name shown.  RIGHT--hide attr.");
   SetMouseStatus ("Show/move an attribute", "Toggle name shown",
         "Hide an attribute");

   orig_x = OFFSET_X(text_obj_ptr->x);
   orig_y = OFFSET_Y(text_obj_ptr->y);
   GridXY (orig_x, orig_y, &grid_orig_x, &grid_orig_y);
   ltx = OFFSET_X(text_obj_ptr->bbox.ltx);
   lty = OFFSET_Y(text_obj_ptr->bbox.lty);
   rbx = OFFSET_X(text_obj_ptr->bbox.rbx)+1;
   rby = OFFSET_Y(text_obj_ptr->bbox.rby)+1;

   XGrabPointer (mainDisplay, drawWindow, FALSE,
         PointerMotionMask | ButtonPressMask,
         GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);
   XWarpPointer (mainDisplay, None, drawWindow, 0, 0, 0, 0, orig_x, orig_y);

   dx = dy = 0;
   grid_x = grid_orig_x; grid_y = grid_orig_y;

   SelBox (drawWindow, revDefaultGC, ltx+dx, lty+dy, rbx+dx, rby+dy);
   PixelToMeasurementUnit(x_buf, 0);
   PixelToMeasurementUnit(y_buf, 0);
   sprintf (buf, "dx=%s,dy=%s", x_buf, y_buf);
   StartShowMeasureCursor (ltx, lty-defaultFontHeight, buf, TRUE);
   while (placing)
   {
      XNextEvent (mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify)
      {
         PixelToMeasurementUnit(x_buf, ABS_SIZE(dx));
         PixelToMeasurementUnit(y_buf, ABS_SIZE(dy));
         sprintf (buf, "dx=%s,dy=%s", x_buf, y_buf);
         ShowMeasureCursor (ltx+dx, lty-defaultFontHeight+dy, buf, TRUE);
         SelBox (drawWindow, revDefaultGC, ltx+dx, lty+dy, rbx+dx, rby+dy);
         ExposeEventHandler (&input, TRUE);
         SelBox (drawWindow, revDefaultGC, ltx+dx, lty+dy, rbx+dx, rby+dy);
         PixelToMeasurementUnit(x_buf, ABS_SIZE(dx));
         PixelToMeasurementUnit(y_buf, ABS_SIZE(dy));
         sprintf (buf, "dx=%s,dy=%s", x_buf, y_buf);
         ShowMeasureCursor (ltx+dx, lty-defaultFontHeight+dy, buf, TRUE);
      }
      else if (input.type == MotionNotify)
      {
         x = input.xmotion.x;
         y = input.xmotion.y;
         GridXY (x, y, &grid_x, &grid_y);

         if (grid_x != grid_orig_x+dx || grid_y != grid_orig_y+dy)
         {
            PixelToMeasurementUnit(x_buf, ABS_SIZE(dx));
            PixelToMeasurementUnit(y_buf, ABS_SIZE(dy));
            sprintf (buf, "dx=%s,dy=%s", x_buf, y_buf);
            ShowMeasureCursor (ltx+dx, lty-defaultFontHeight+dy, buf, TRUE);
            SelBox (drawWindow, revDefaultGC, ltx+dx, lty+dy, rbx+dx, rby+dy);
            dx = grid_x - grid_orig_x;
            dy = grid_y - grid_orig_y;
            SelBox (drawWindow, revDefaultGC, ltx+dx, lty+dy, rbx+dx, rby+dy);
            PixelToMeasurementUnit(x_buf, ABS_SIZE(dx));
            PixelToMeasurementUnit(y_buf, ABS_SIZE(dy));
            sprintf (buf, "dx=%s,dy=%s", x_buf, y_buf);
            ShowMeasureCursor (ltx+dx, lty-defaultFontHeight+dy, buf, TRUE);
            MarkRulers (grid_x, grid_y);
         }
         while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
      }
      else if (input.type == ButtonPress)
      {
         XUngrabPointer (mainDisplay, CurrentTime);
         placing = FALSE;
         SelBox (drawWindow, revDefaultGC, ltx+dx, lty+dy, rbx+dx, rby+dy);
         PixelToMeasurementUnit(x_buf, ABS_SIZE(dx));
         PixelToMeasurementUnit(y_buf, ABS_SIZE(dy));
         sprintf (buf, "dx=%s,dy=%s", x_buf, y_buf);
         EndShowMeasureCursor (ltx+dx, lty-defaultFontHeight+dy, buf, TRUE);
         Msg ("");
         SetMouseStatus ("", "", "");
         switch (input.xbutton.button)
         {
            case Button1:
               if (dx != 0 || dy != 0)
               {
                  if (ObjPtr->locked)
                  {
                     Msg ("Can not move attributes of locked object.");
                     return (FALSE);
                  }
                  HighLightReverse ();
                  PrepareToReplaceAnObj (ObjPtr);
                  AttrPtr->shown = TRUE;
                  MoveObj (text_obj_ptr, ABS_SIZE(dx), ABS_SIZE(dy));
                  AdjObjBBox (ObjPtr);
                  RecordReplaceAnObj (ObjPtr);
                  return (TRUE);
               }
               else if (!(AttrPtr->shown))
               {
                  HighLightReverse ();
                  PrepareToReplaceAnObj (ObjPtr);
                  AttrPtr->shown = TRUE;
                  AdjObjBBox (ObjPtr);
                  RecordReplaceAnObj (ObjPtr);
                  return (TRUE);
               }
               return (FALSE);
            case Button2:
               if (*(AttrPtr->attr_name.s) != '\0')
                  PrepareToReplaceAnObj (ObjPtr);
               if (!(AttrPtr->nameshown && *(AttrPtr->attr_name.s)=='\0'))
                  AttrPtr->nameshown = !AttrPtr->nameshown;
               UpdAttr (AttrPtr);
               if (AttrPtr->shown)
               {
                  HighLightReverse ();
                  AdjObjCache (ObjPtr);
                  AdjObjBBox (ObjPtr);
                  if (*(AttrPtr->attr_name.s) != '\0')
                     RecordReplaceAnObj (ObjPtr);
                  return (TRUE);
               }
               if (*(AttrPtr->attr_name.s) != '\0') RecordReplaceAnObj (ObjPtr);
               return (FALSE);
            case Button3:
               if (AttrPtr->shown)
               {
                  HighLightReverse ();
                  PrepareToReplaceAnObj (ObjPtr);
                  AttrPtr->shown = FALSE;
                  AdjObjBBox (ObjPtr);
                  RecordReplaceAnObj (ObjPtr);
                  return (TRUE);
               }
               return (FALSE);
         }
      }
   }
   return (FALSE);
}

static
int ChangeAttrJust (ObjPtr, AttrPtr)
   struct ObjRec	* ObjPtr;
   struct AttrRec	* AttrPtr;
{
   struct ObjRec	* text_obj_ptr;
   int          	x, y, grid_x, grid_y, dx, dy, placing = TRUE;
   int          	ltx, lty, rbx, rby;
   int			orig_x, orig_y, grid_orig_x, grid_orig_y;
   int			old_just, new_just = 0;
   XEvent		input, ev;

   text_obj_ptr = AttrPtr->obj;
   SetMouseStatus ("Left justify an attribute", "Center justify an attribute",
         "Right justify an attribute");

   orig_x = OFFSET_X(text_obj_ptr->x);
   orig_y = OFFSET_Y(text_obj_ptr->y);
   GridXY (orig_x, orig_y, &grid_orig_x, &grid_orig_y);
   ltx = OFFSET_X(text_obj_ptr->bbox.ltx);
   lty = OFFSET_Y(text_obj_ptr->bbox.lty);
   rbx = OFFSET_X(text_obj_ptr->bbox.rbx)+1;
   rby = OFFSET_Y(text_obj_ptr->bbox.rby)+1;

   XGrabPointer (mainDisplay, drawWindow, FALSE,
         PointerMotionMask | ButtonPressMask,
         GrabModeAsync, GrabModeAsync, None, handCursor, CurrentTime);
   XWarpPointer (mainDisplay, None, drawWindow, 0, 0, 0, 0, orig_x, orig_y);

   dx = dy = 0;
   grid_x = grid_orig_x; grid_y = grid_orig_y;

   SelBox (drawWindow, revDefaultGC, ltx+dx, lty+dy, rbx+dx, rby+dy);
   StartShowMeasureCursor (ltx, lty, NULL, TRUE);
   while (placing)
   {
      XNextEvent (mainDisplay, &input);

      if (input.type == Expose || input.type == VisibilityNotify)
      {
         SelBox (drawWindow, revDefaultGC, ltx+dx, lty+dy, rbx+dx, rby+dy);
         ExposeEventHandler (&input, TRUE);
         SelBox (drawWindow, revDefaultGC, ltx+dx, lty+dy, rbx+dx, rby+dy);
      }
      else if (input.type == MotionNotify)
      {
         x = input.xmotion.x;
         y = input.xmotion.y;
         GridXY (x, y, &grid_x, &grid_y);

         if (grid_x != grid_orig_x+dx || grid_y != grid_orig_y+dy)
         {
            SelBox (drawWindow, revDefaultGC, ltx+dx, lty+dy, rbx+dx, rby+dy);
            dx = grid_x - grid_orig_x;
            dy = grid_y - grid_orig_y;
            SelBox (drawWindow, revDefaultGC, ltx+dx, lty+dy, rbx+dx, rby+dy);
            MarkRulers (grid_x, grid_y);
         }
         while (XCheckMaskEvent (mainDisplay, PointerMotionMask, &ev)) ;
      }
      else if (input.type == ButtonPress)
      {
         XUngrabPointer (mainDisplay, CurrentTime);
         placing = FALSE;
         SelBox (drawWindow, revDefaultGC, ltx+dx, lty+dy, rbx+dx, rby+dy);
         EndShowMeasureCursor (ltx+dx, lty+dy, NULL, TRUE);
         Msg ("");
         SetMouseStatus ("", "", "");
         old_just = text_obj_ptr->detail.t->just;
         switch (input.xbutton.button)
         {
            case Button1: new_just = JUST_L; break;
            case Button2: new_just = JUST_C; break;
            case Button3: new_just = JUST_R; break;
         }
         if (old_just != new_just)
         {
            HighLightReverse ();
            PrepareToReplaceAnObj (ObjPtr);
            text_obj_ptr->detail.t->just = new_just;
            AdjObjCache (ObjPtr);
            UpdTextBBox (text_obj_ptr);
            AdjObjBBox (ObjPtr);
            RecordReplaceAnObj (ObjPtr);
            return (TRUE);
         }
      }
   }
   return (FALSE);
}

void MoveAttr ()
{
   struct ObjRec	* obj_ptr;
   struct AttrRec	* attr_ptr, * attr_ptr1;
   int			num_attrs = 0, i, index=INVALID, x, y;
   int			sel_ltx, sel_lty, sel_rbx, sel_rby;
   int			* fore_colors, * pixel_ptr, * valid, * flag_ptr;
   int			* init_rv;
   char			* * attrStrs, * s, buf[MAXSTRING];
   unsigned int		button;

   if (topSel != botSel || topSel == NULL)
   { Msg ("Please select only ONE object."); return; }

   obj_ptr = topSel->obj;
   attr_ptr1 = attr_ptr = obj_ptr->fattr;
   for ( ; attr_ptr1 != NULL; attr_ptr1 = attr_ptr1->next, num_attrs++) ;

   if (num_attrs == 0)
   { Msg ("Selected object currently has NO attributes."); return; }

   attrStrs = (char * *)malloc(num_attrs*sizeof(char *));
   fore_colors = pixel_ptr = (int*)malloc(num_attrs*sizeof(int));
   valid = flag_ptr = (int*)malloc(num_attrs*sizeof(int));
   init_rv = (int*)malloc(num_attrs*sizeof(int));
   if (attrStrs==NULL || fore_colors==NULL || valid==NULL || init_rv==NULL) {
      FailAllocMessage();
   }

   attr_ptr1 = attr_ptr;
   for (i = 0; i < num_attrs; i++, attr_ptr1 = attr_ptr1->next)
   {
      s = (char*)malloc((MAXSTRING+1)*sizeof(char));
      if (s == NULL) FailAllocMessage();
      attrStrs[i] = s;

      UpdateFirstTextStringWithAttrNameAndValue (attr_ptr1->obj, attr_ptr1);
      strcpy (s, attr_ptr1->obj->detail.t->first->dyn_str.s);

      *pixel_ptr++ = colorPixels[attr_ptr1->obj->color];
      *flag_ptr++ = TRUE;
      init_rv[i] = FALSE;
   }

   Msg ("Hold down left button to see attributes.");
   strcpy (buf, "Left button show/move an attribute.  ");
   strcat (buf, "Middle button change an attribute justification.");
   Msg (buf);
   SetMouseStatus ("Show/move an attribute",
         "Change an attribute justification", "(none)");
   button = CornerLoop (&x, &y);
   activeMenu = INVALID;
   if (button == Button1 || button == Button2)
      index = TextMenuLoop (x, y, attrStrs, num_attrs, fore_colors, valid,
            init_rv, NULL, MULTICOLOR, FALSE);
   if (index != INVALID)
   {
      attr_ptr1 = attr_ptr;
      for (i = 0; i < index; i++, attr_ptr1 = attr_ptr1->next) ;
      sel_ltx = selLtX; sel_lty = selLtY; sel_rbx = selRbX; sel_rby = selRbY;
      if (button == Button1)
      {
         if (MoveOneAttr (obj_ptr, attr_ptr1))
         {  /* HighLightReverse () is expected to be called */
            UpdSelBBox ();
            RedrawAreas (botObj, sel_ltx-GRID_ABS_SIZE(1),
                  sel_lty-GRID_ABS_SIZE(1),
                  sel_rbx+GRID_ABS_SIZE(1), sel_rby+GRID_ABS_SIZE(1),
                  selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
                  selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
            SetFileModified (TRUE);
            HighLightForward ();
         }
      }
      else if (button == Button2)
      {
         if (ChangeAttrJust (obj_ptr, attr_ptr1))
         {  /* HighLightReverse () is expected to be called */
            UpdSelBBox ();
            RedrawAreas (botObj, sel_ltx-GRID_ABS_SIZE(1),
                  sel_lty-GRID_ABS_SIZE(1), sel_rbx+GRID_ABS_SIZE(1),
                  sel_rby+GRID_ABS_SIZE(1), selLtX-GRID_ABS_SIZE(1),
                  selLtY-GRID_ABS_SIZE(1), selRbX+GRID_ABS_SIZE(1),
                  selRbY+GRID_ABS_SIZE(1));
            SetFileModified (TRUE);
            HighLightForward ();
         }
      }
   }

   for (i = 0; i < num_attrs; i++) free(attrStrs[i]);
   free(attrStrs);
   justDupped = FALSE;
}

void CopyAndUpdateAttrs (ToObjPtr, FromObjPtr)
   struct ObjRec	* ToObjPtr, * FromObjPtr;
{
   register struct AttrRec	* to_attr_ptr, * from_attr_ptr;
   char				msg[MAXSTRING];
   int				confirm_status=MB_ID_CANCEL;

   topAttr = botAttr = NULL;
   from_attr_ptr = FromObjPtr->fattr;
   for ( ; from_attr_ptr != NULL; from_attr_ptr = from_attr_ptr->next)
   {
      to_attr_ptr = ToObjPtr->fattr;
      for ( ; to_attr_ptr != NULL; to_attr_ptr = to_attr_ptr->next)
      {
         if (from_attr_ptr->obj->color == to_attr_ptr->obj->color &&
               strcmp (from_attr_ptr->attr_name.s, to_attr_ptr->attr_name.s)==0)
         {
            if (*(from_attr_ptr->attr_value.s) != '\0')
            {
               DynStrCpy (&to_attr_ptr->attr_value, &from_attr_ptr->attr_value);
               UpdAttr (to_attr_ptr);
            }
            break;
         }
      }
      if (to_attr_ptr == NULL)
      {  /* can not find the same attribute in the new one */
         confirm_status = MB_ID_NO;

         if (from_attr_ptr->inherited)
         {  /* obsolete attribute? */
            confirm_status = (dropObsIconAttrWhenUpdate) ? MB_ID_YES :
                  MB_ID_CANCEL;
            if (*from_attr_ptr->attr_name.s == '\0')
               sprintf (msg, "Is the attribute '%s' %s for icon '%s'?  [yn](y)",
                     from_attr_ptr->attr_value.s, "obsolete",
                     FromObjPtr->detail.r->s);
            else
               sprintf (msg, "Is the attribute '%s' %s for icon '%s'?  [yn](y)",
                     from_attr_ptr->attr_name.s, "obsolete",
                     FromObjPtr->detail.r->s);
            while (confirm_status == MB_ID_CANCEL)
            {
               confirm_status = MsgBox (msg, TOOL_NAME, YN_MB);
               if (confirm_status == MB_ID_CANCEL)
                  MsgBox ("CANCEL is not a valid option.", TOOL_NAME, INFO_MB);
            }
         }

         if (confirm_status == MB_ID_NO)
         {  /* new attribute */
            to_attr_ptr = (struct AttrRec *)malloc(sizeof(struct AttrRec));
            if (to_attr_ptr == NULL) FailAllocMessage ();
            memset(to_attr_ptr, 0, sizeof(struct AttrRec));
            to_attr_ptr->owner = ToObjPtr;
            DupAnAttr (from_attr_ptr, to_attr_ptr);
            LinkInAttr ((struct AttrRec *)NULL, topAttr, to_attr_ptr);
         }
      }
      else
      {
         to_attr_ptr->shown = from_attr_ptr->shown;
         to_attr_ptr->nameshown = from_attr_ptr->nameshown;
      }
   }
   if (topAttr != NULL)
   {
      topAttr->prev = NULL;
      botAttr->next = ToObjPtr->fattr;

      if (ToObjPtr->fattr != NULL) ToObjPtr->fattr->prev = botAttr;
      ToObjPtr->fattr = topAttr;
      if (ToObjPtr->lattr == NULL) ToObjPtr->lattr = botAttr;
   }
   AdjObjBBox (ToObjPtr);
}

void ToggleNamedAttrShown (attr_name)
   char	* attr_name;
{
   char			* paren_ptr, msg[MAXSTRING+1];
   struct AttrRec	* attr_ptr;
   int			ltx, lty, rbx, rby;

   if (topSel != botSel || topSel == NULL)
   { Msg ("Please select only ONE object."); return; }

   if ((paren_ptr = strchr (attr_name, ')')) == NULL)
   { Msg ("Invalid format in shortcut specification."); return; }

   *paren_ptr = '\0';
   if ((attr_ptr = FindAttrWithName (topSel->obj, attr_name, NULL)) == NULL)
   {
      sprintf (msg, "Can not find a '%s' attribute for the selected object.",
            attr_name);
      Msg (msg);
      return;
   }
   ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
   HighLightReverse ();
   PrepareToReplaceAnObj (topSel->obj);
   attr_ptr->shown = !attr_ptr->shown;
   AdjObjBBox (topSel->obj);
   RecordReplaceAnObj (topSel->obj);
   UpdSelBBox ();
   RedrawAreas (botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   SetFileModified (TRUE);
   HighLightForward ();
}

void AddFileAttrs ()
{
   struct SelRec	* sel_ptr;
   int			ltx, lty, rbx, rby;

   if (topSel == NULL)
   {
      MsgBox ("Must select at least one text object.", TOOL_NAME, INFO_MB);
      return;
   }
   for (sel_ptr=topSel; sel_ptr!=NULL; sel_ptr=sel_ptr->next)
   {
      if (sel_ptr->obj->type != OBJ_TEXT)
      {
         MsgBox ("Can not attach non-text objects.", TOOL_NAME, INFO_MB);
         return;
      }
/*    else if (sel_ptr->obj->detail.t->first != sel_ptr->obj->detail.t->last)
      {
         MsgBox ("A multiline text can NOT be a file attribute.",
               TOOL_NAME, INFO_MB);
         return;
      } */
   }

   ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;

   HighLightReverse ();
   AddObj (NULL, topObj, tgifObj);
   AddNewSelObj (topObj);
   PrepareToRecord (CMD_REPLACE, topSel, botSel, numObjSelected);

   for (sel_ptr=topSel->next; sel_ptr!=NULL; sel_ptr=sel_ptr->next)
      AddAttr (topObj, sel_ptr->obj);
   RemoveAllSel ();
   AddNewSelObj (topObj);
   UpdSelBBox ();

   recordCmdIncludeTgifObj = TRUE;
   RecordCmd (CMD_MANY_TO_ONE, NULL, topSel, botSel, 1);
   recordCmdIncludeTgifObj = FALSE;
   RemoveAllSel ();
   UnlinkObj (topObj);
   RedrawAnArea (botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1));
   SetFileModified (TRUE);
   justDupped = FALSE;
}

void DetachFileAttrs ()
{
   struct AttrRec	* attr_ptr, * next_attr;
   struct SelRec	* sel_ptr;

   MakeQuiescent ();

   if (tgifObj->fattr == NULL)
   {
      MsgBox ("Current file contains no attributes.", TOOL_NAME, INFO_MB);
      return;
   }

   AddObj (NULL, topObj, tgifObj);
   AddNewSelObj (topObj);
   PrepareToReplaceAnObj (topObj);

   for (attr_ptr=topObj->fattr; attr_ptr!=NULL; attr_ptr=next_attr) {
      next_attr = attr_ptr->next;

      sel_ptr = (struct SelRec *)malloc(sizeof(struct SelRec));
      if (sel_ptr == NULL) FailAllocMessage();
      sel_ptr->obj = attr_ptr->obj;
      AddSel (topSel, topSel->next, sel_ptr);

      attr_ptr->obj->detail.t->attr = NULL;
      AddObj (topObj, topObj->next, attr_ptr->obj);
      free(attr_ptr);
   }
   topObj->fattr = topObj->lattr = NULL;
   recordCmdIncludeTgifObj = TRUE;
   RecordCmd (CMD_ONE_TO_MANY, NULL, topSel, botSel, numObjSelected);
   recordCmdIncludeTgifObj = FALSE;

   UnlinkObj (topObj);

   sel_ptr = topSel;
   topSel = topSel->next;
   topSel->prev = NULL;
   free(sel_ptr);
   numObjSelected--;
   UpdSelBBox ();

   RedrawAnArea (botObj, selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
         selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
   HighLightForward ();
   SetFileModified (TRUE);
   justDupped = FALSE;
}

static XComposeStatus	c_stat;
static DspList		* fileAttrNameDspPtr=NULL;

static
DspList *FileAttrNameListing(ObjPtr, pn_entries)
   struct ObjRec *ObjPtr;
   int *pn_entries;
{
   int i;
   DspList *dsp_ptr;
   struct AttrRec *attr_ptr;

   for (*pn_entries=0, attr_ptr=ObjPtr->fattr; attr_ptr!=NULL;
         attr_ptr=attr_ptr->next) {
      (*pn_entries)++;
   }
   fileAttrNameDspPtr = (DspList*)malloc((*pn_entries)*sizeof(DspList));
   if (fileAttrNameDspPtr == NULL) FailAllocMessage();
   memset(fileAttrNameDspPtr, 0, (*pn_entries)*sizeof(DspList));
   for (i=1, dsp_ptr=fileAttrNameDspPtr, attr_ptr=ObjPtr->fattr;
         i<=(*pn_entries); i++, dsp_ptr++, attr_ptr=attr_ptr->next)
   {
      if (*attr_ptr->attr_name.s == '\0') {
         UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr),
               attr_ptr->attr_value.s);
      } else {
         sprintf(gszMsgBox, "%s%s", attr_ptr->attr_name.s,
               attr_ptr->attr_value.s);
         UtilStrCpy(dsp_ptr->itemstr, sizeof(dsp_ptr->itemstr), gszMsgBox);
      }
      /* use the directory field for inherited */
      dsp_ptr->directory = attr_ptr->inherited;
      dsp_ptr->next = ((i == (*pn_entries)) ? NULL : &dsp_ptr[1]);
   }
   return (fileAttrNameDspPtr);
}

static
int EditAttrNames(TopStr, ObjAttrDspList, entries, num_entries)
   char *TopStr, **entries;
   DspList *ObjAttrDspList;
   int num_entries;
{
   char win_name[128];

   sprintf(win_name, "%s - Edit Attribute Names", TOOL_NAME);
   ResetNamesInfo();
   NamesSetTitle(TopStr);
   NamesAddButton("OK", BUTTON_OK);
   NamesAddButton("CANCEL", BUTTON_CANCEL);
   /* ignore double-click and <CR> */
   NamesSetDefaultBtnId(BUTTON_OK, INVALID);
   NamesSetStyle(NAMES_EDIT_ATTR, NAMES_LOOP_ONCE);
   NamesSetEntries(ObjAttrDspList, entries, num_entries, TRUE, INVALID, 0);
   return (Names(win_name, NULL, NULL, 0, NULL)==BUTTON_OK);
}

static
int BlankStr(s)
   register char *s;
{
   while (*s == ' ') s++;
   return (*s == '\0');
}

void EditFileAttrs()
{
   int i, num_entries=0;
   DspList *dsp_ptr;
   char **entries=NULL;

   MakeQuiescent();

   if (tgifObj->fattr == NULL) {
      MsgBox("Current file contains no attributes.", TOOL_NAME, INFO_MB);
      return;
   }
   dsp_ptr = FileAttrNameListing(tgifObj, &num_entries);
   ignoreDirectoryFlag = TRUE;
   entries = MakeNameDspItemArray(num_entries, dsp_ptr);
   ignoreDirectoryFlag = FALSE;
   if (EditAttrNames("Edit File Attributes...", NULL, entries, num_entries)) {
      int modified=FALSE;
      struct AttrRec *attr_ptr, *next_attr;

      AddObj(NULL, topObj, tgifObj);
      AddNewSelObj(topObj);
      PrepareToReplaceAnObj(topObj);

      for (attr_ptr=tgifObj->fattr, i=0; attr_ptr!=NULL;
            attr_ptr=next_attr, i++) {
         int blank_str=BlankStr(entries[i]);
         struct TextRec *text_ptr=attr_ptr->obj->detail.t;

         next_attr = attr_ptr->next;
         if ((blank_str &&
               text_ptr->first==text_ptr->last) ||
               strcmp(text_ptr->first->dyn_str.s,
               entries[i]) != 0) {
            modified = TRUE;
            if (blank_str && text_ptr->first==text_ptr->last) {
               UnlinkAttr(attr_ptr);
               FreeTextObj(attr_ptr->obj);
               FreeAttr(attr_ptr);
            } else {
               DynStrSet(&text_ptr->first->dyn_str, entries[i]);
               UpdateAttr(text_ptr, attr_ptr);
            }
         }
      }
      if (modified) {
         recordCmdIncludeTgifObj = TRUE;
         RecordReplaceAnObj(topObj);
         recordCmdIncludeTgifObj = FALSE;

         RemoveAllSel();
         UnlinkObj(topObj);

         SetFileModified(TRUE);
         Msg("File attribute updated.");
      } else {
         AbortPrepareCmd(CMD_REPLACE);

         RemoveAllSel();
         UnlinkObj(topObj);
      }
   }
   free(dsp_ptr);
   free(*entries);
   free(entries);
   Msg("");
}

void EditAttrs ()
{
   int i, num_entries=0;
   DspList *dsp_ptr;
   char **entries=NULL;

   if (topSel == NULL || topSel != botSel) {
      MsgBox("Must select one object for EditAttrs().", TOOL_NAME, INFO_MB);
      return;
   }
   if (topSel->obj->fattr == NULL) {
      MsgBox("Selected object contains no attributes.", TOOL_NAME, INFO_MB);
      return;
   }
   dsp_ptr = FileAttrNameListing(topSel->obj, &num_entries);
   ignoreDirectoryFlag = TRUE;
   entries = MakeNameDspItemArray(num_entries, dsp_ptr);
   ignoreDirectoryFlag = FALSE;
   if (EditAttrNames("Edit Object Attributes...", dsp_ptr, entries,
         num_entries)) {
      int modified=FALSE, sel_ltx, sel_lty, sel_rbx, sel_rby;
      struct AttrRec *attr_ptr, *next_attr;

      sel_ltx = selLtX; sel_lty = selLtY; sel_rbx = selRbX; sel_rby = selRbY;
      HighLightReverse();
      PrepareToReplaceAnObj(topSel->obj);

      for (attr_ptr=topSel->obj->fattr, i=0; attr_ptr!=NULL;
            attr_ptr=next_attr, i++) {
         int blank_str=BlankStr(entries[i]);
         char orig_str[MAXPATHLENGTH+1];
         struct TextRec *text_ptr=attr_ptr->obj->detail.t;

         next_attr = attr_ptr->next;
         if (*attr_ptr->attr_name.s == '\0') {
            strcpy(orig_str, attr_ptr->attr_value.s);
         } else {
            sprintf(orig_str, "%s%s", attr_ptr->attr_name.s,
                  attr_ptr->attr_value.s);
         }
         if ((blank_str && text_ptr->first==text_ptr->last) ||
               strcmp(orig_str, entries[i]) != 0) {
            modified = TRUE;
            if (blank_str && text_ptr->first==text_ptr->last) {
               UnlinkAttr(attr_ptr);
               FreeTextObj(attr_ptr->obj);
               FreeAttr(attr_ptr);
            } else {
               int saved_nameshown=attr_ptr->nameshown;

               DynStrSet(&text_ptr->first->dyn_str,
                     entries[i]);
               attr_ptr->nameshown = TRUE;
               UpdateAttr(text_ptr, attr_ptr);
               if (!saved_nameshown) {
                  if (*attr_ptr->attr_name.s != '\0') {
                     attr_ptr->nameshown = saved_nameshown;
                  }
                  UpdAttr(attr_ptr);
                  UpdateAttr(text_ptr, attr_ptr);
               }
            }
         }
      }
      if (modified) {
         AdjObjBBox(topSel->obj);
         RecordReplaceAnObj(topSel->obj);
         UpdSelBBox();
         RedrawAreas(botObj, sel_ltx-GRID_ABS_SIZE(1),
                  sel_lty-GRID_ABS_SIZE(1),
                  sel_rbx+GRID_ABS_SIZE(1), sel_rby+GRID_ABS_SIZE(1),
                  selLtX-GRID_ABS_SIZE(1), selLtY-GRID_ABS_SIZE(1),
                  selRbX+GRID_ABS_SIZE(1), selRbY+GRID_ABS_SIZE(1));
         SetFileModified(TRUE);
         justDupped = FALSE;
         Msg("Object attribute updated.");
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
      HighLightForward();
   }
   free(dsp_ptr);
   free(*entries);
   free(entries);
   Msg("");
}
