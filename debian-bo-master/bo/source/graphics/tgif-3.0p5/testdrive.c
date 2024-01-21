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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/testdrive.c,v 3.0 1996/05/06 16:12:11 william Exp $";
#endif

#include <stdio.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "mainloop.e"
#include "msg.e"
#include "obj.e"
#include "setup.e"

/*
 * extern int	malloc_debug ARGS_DECL((int));
 */

int	lastFile;
short	*pDrawFontAsc;
short	*pDrawFontDes;

/*
 * static
 * void Prompt2 (PromptStr, OpName, FileName)
 *    char	* PromptStr, * OpName, * FileName;
 * {
 *    char	inbuf[80];
 * 
 *    printf (PromptStr);
 *    fgets (inbuf, 80, stdin);
 *    sscanf (inbuf, "%s%s", OpName, FileName);
 * }
 *
 * static
 * void Prompt3 (PromptStr, AttrName, ColorName, ValName)
 *    char	* PromptStr, * AttrName, * ColorName, * ValName;
 * {
 *    char	inbuf[80];
 * 
 *    printf (PromptStr);
 *    fgets (inbuf, 80, stdin);
 *    sscanf (inbuf, "%s%s%s", AttrName, ColorName, ValName);
 * }
 */

static
void PrintObjId(ObjPtr, Level)
   struct ObjRec *ObjPtr;
   int Level;
{
   register int i;
   int id=ObjPtr->id;
   struct ObjRec *obj_ptr;
   struct AttrRec *attr_ptr;

   for (i = 0; i < Level; i++) printf("   ");
   switch (ObjPtr->type) {
   /* these are all the tgif object types */
   case OBJ_POLY: printf("poly: %1d\n", id); break;
   case OBJ_BOX: printf("box: %1d\n", id); break;
   case OBJ_OVAL: printf("oval: %1d\n", id); break;
   case OBJ_TEXT: printf("text: %1d\n", id); break;
   case OBJ_ARC: printf("arc: %1d\n", id); break;
   case OBJ_RCBOX: printf("rcbox: %1d\n", id); break;
   case OBJ_XBM: printf("xbm: %1d\n", id); break;
   case OBJ_XPM: printf("xpm: %1d\n", id); break;
   case OBJ_POLYGON: printf("polygon: %1d\n", id); break;
   case OBJ_GROUP: printf("group: %1d\n", id); break;
   case OBJ_SYM: printf("sym: %1d\n", id); break;
   case OBJ_ICON: printf("icon: %1d\n", id); break;
   }
   if (ObjPtr->type == OBJ_GROUP || ObjPtr->type == OBJ_SYM ||
         ObjPtr->type == OBJ_ICON) {
      /* for these composite objects, each on maintains a LIST of objects */
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr=obj_ptr->prev) {
         PrintObjId(obj_ptr, Level+1);
      }
   }
   /* all object can have a list of attributes; the head    */
   /*     of the list is pointed to by the fattr and the    */
   /*     tail of the list is pointed to by the lattr field */
   if ((attr_ptr=ObjPtr->lattr) != NULL) {
      for (i = 0; i < Level+1; i++) printf("   ");
      printf("attrs:\n");

      /* each attribute entry contains a TEXT object */
      for ( ; attr_ptr != NULL; attr_ptr=attr_ptr->prev) {
         PrintObjId(attr_ptr->obj, Level+2);
      }
   }
}

int main(argc, argv)
   int argc;
   char *argv[];
   /* All these strangeness with strings are related to */
   /*    Prolog's foreign function interface. */
{
   register int i;
   char op_name[80], file_name[80];
   char *sp[6], *func_strp;
/*
 * char	color_name[80], val_name[80];
 * char	attr_name[80], speed_name[80], id_name[80];
 */

/*
 * malloc_debug (1);
 */

   if (!ProcessTgifOptions(argc, argv, file_name)) return 1;

   if (file_name[0] == '\0') {
      MainLoop("init", "", &func_strp,
            &sp[0], &sp[1], &sp[2], &sp[3], &sp[4], &sp[5]);
   } else {
      MainLoop("init", file_name, &func_strp,
            &sp[0], &sp[1], &sp[2], &sp[3], &sp[4], &sp[5]);
   }
/*
 * for (i = 0; i < 6; i++)
 *    if (strcmp (sp[i], "") != 0)
 *       printf ("%s ", sp[i]);
 *    else
 *       break;
 * printf ("\n");
 */

   while (TRUE) {
      char s[80];

      strcpy(s, func_strp);
      s[4] = '\0';

      DeallocStrings(&func_strp,&sp[0],&sp[1],&sp[2],&sp[3],&sp[4],&sp[5]);

      if (strcmp(s, "Quit") == 0) {
         *file_name = '\0';
         MainLoop("quit", file_name, &func_strp,
               &sp[0], &sp[1], &sp[2], &sp[3], &sp[4], &sp[5]);
         DeallocStrings(&func_strp,&sp[0],&sp[1],&sp[2],&sp[3],&sp[4],&sp[5]);
         break;
      } else if (strcmp(s, "Solv") == 0) {
         struct ObjRec *obj_ptr;

         printf("==============\n");
         printf("Listing IDs...\n");
         printf("==============\n");
         /* botObj points to the last top-level object, and */
         /*     topObj points to the first top-level object */
         for (obj_ptr = botObj; obj_ptr != NULL; obj_ptr = obj_ptr->prev) {
            PrintObjId(obj_ptr, 0);
         }
         printf("\n");
      }

      Msg("Returned from basic driver.");
/*
 *    Prompt2 ("Input an operation and a sub command.\n",op_name,file_name);
 *
 *    if (strcmp (op_name, "animate") == 0)
 *    {
 *       Prompt3 ("Input poly_id, speed, color.\n", id_name, speed_name,
 *             color_name);
 *       Animate (file_name, id_name, speed_name, color_name, &func_strp);
 *       printf ("Animate RETURNs --> %s %s %s\n", func_strp, sp[0], sp[1]);
 *    }
 *    if (strcmp (op_name, "upd_attr_val") == 0)
 *    {
 *       Prompt3 ("Input attrname, color and value.\n", attr_name, color_name,
 *             val_name);
 *       UpdAttrVal (file_name, attr_name, color_name, val_name, &func_strp);
 *       printf ("UpdAttrVal RETURNs --> %s %s %s\n", func_strp, sp[0], sp[1]);
 *    }
 */
      *op_name = *file_name = '\0';
      MainLoop(op_name, file_name, &func_strp,
            &sp[0], &sp[1], &sp[2], &sp[3], &sp[4], &sp[5]);
/*
 *    printf ("RETURN --> %s ", func_strp);
 *    for (i = 0; i < 6; i++)
 *       if (strcmp (sp[i], "") != 0)
 *          printf ("%s ", sp[i]);
 *       else
 *          break;
 *    printf ("\n");
 */
   }
   return 0;
}
