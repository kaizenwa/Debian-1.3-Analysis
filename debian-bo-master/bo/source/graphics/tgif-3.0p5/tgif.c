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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/tgif.c,v 3.0 1996/05/06 16:12:21 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "color.e"
#include "file.e"
#include "mainloop.e"
#include "msg.e"
#include "obj.e"
#include "page.e"
#include "setup.e"
#include "util.e"

/*
 * extern int	malloc_debug ARGS_DECL((int));
 */

int	lastFile=TRUE;

short pDrawFontAsc[] = {
    8, 10, 12, 14, 17, 22, 
    8, 10, 12, 14, 17, 22, 
    8, 10, 12, 14, 17, 23, 
    8, 10, 12, 14, 17, 22, 
    8,  9, 11, 13, 15, 19, 
    7,  9, 11, 12, 15, 20, 
    7,  9, 11, 13, 15, 19, 
    7,  9, 11, 12, 15, 20, 
    9, 11, 12, 14, 18, 24, 
    9, 11, 12, 14, 17, 24, 
    9, 11, 12, 14, 17, 24, 
    9, 11, 12, 14, 17, 24, 
    8, 11, 12, 14, 18, 23, 
    8, 11, 12, 15, 18, 24, 
    8, 11, 12, 14, 16, 23, 
    8, 11, 12, 14, 16, 24, 
    8, 10, 12, 14, 18, 24, 
    9, 14, 15, 18, 23, 30, 
   10, 14, 16, 17, 23, 30, 
    9, 13, 15, 17, 22, 30, 
    9, 13, 15, 18, 22, 30, 
    9, 12, 14, 15, 19, 26, 
    9, 12, 14, 15, 21, 26, 
    9, 12, 14, 15, 19, 26, 
    9, 12, 14, 15, 20, 26, 
   11, 14, 16, 18, 24, 31, 
   11, 14, 16, 18, 24, 31, 
   11, 14, 16, 18, 24, 31, 
   11, 14, 16, 18, 24, 31, 
   11, 14, 16, 19, 24, 32, 
   11, 15, 16, 19, 24, 33, 
   11, 14, 16, 18, 23, 32, 
   11, 15, 16, 19, 24, 32, 
   11, 12, 13, 14, 19, 27 
};
short pDrawFontDes[] = {
    2,  3,  3,  4,  4,  6, 
    2,  3,  3,  4,  4,  6, 
    2,  3,  3,  4,  5,  6, 
    2,  3,  3,  3,  4,  6, 
    2,  2,  3,  3,  4,  5, 
    2,  2,  3,  4,  5,  5, 
    2,  2,  3,  3,  4,  5, 
    2,  2,  3,  4,  5,  5, 
    2,  2,  3,  3,  4,  5, 
    2,  2,  3,  3,  5,  5, 
    2,  2,  3,  3,  5,  5, 
    2,  2,  3,  3,  5,  5, 
    2,  2,  3,  3,  4,  5, 
    2,  2,  3,  3,  4,  5, 
    2,  2,  3,  3,  6,  5, 
    2,  2,  3,  3,  6,  5, 
    3,  4,  4,  6,  7,  8, 
    3,  3,  4,  4,  6,  7, 
    3,  3,  4,  4,  6,  7, 
    3,  4,  4,  5,  6,  7, 
    3,  3,  4,  4,  6,  7, 
    2,  3,  3,  4,  5,  6, 
    2,  4,  3,  5,  5,  7, 
    2,  3,  4,  4,  5,  6, 
    2,  4,  4,  5,  5,  7, 
    2,  3,  4,  4,  5,  7, 
    2,  3,  4,  5,  5,  7, 
    2,  3,  4,  5,  5,  7, 
    2,  3,  4,  5,  5,  7, 
    2,  3,  3,  4,  5,  7, 
    2,  3,  3,  4,  5,  7, 
    2,  3,  3,  4,  5,  7, 
    2,  3,  3,  4,  5,  7, 
    4,  3,  4,  5,  5,  7 
};

static
void SetProgramName(FileName)
   char *FileName;
{
   char *c_ptr=UtilStrRChr(FileName, '/');

   if (c_ptr != NULL && *(++c_ptr) != '\0') {
      strcpy(progName, c_ptr);
   } else {
      strcpy(progName, TOOL_NAME);
   }
}

static
int PrTgifLoad(FileName)
   char *FileName;
{
   struct ObjRec *obj_ptr;
   char full_name[MAXPATHLENGTH+1], tmp_filename[MAXPATHLENGTH+1];
   int len, read_status;
   FILE *fp;
   int tmp_linenum, obj_ext_len, sym_ext_len;
   char obj_ext_str[MAXSTRING+1], sym_ext_str[MAXSTRING+1];

   sprintf(obj_ext_str, ".%s", OBJ_FILE_EXT);
   obj_ext_len = strlen(obj_ext_str);
   sprintf(sym_ext_str, ".%s", SYM_FILE_EXT);
   sym_ext_len = strlen(sym_ext_str);

   len = strlen(FileName);

   if ((len>=obj_ext_len &&
         strcmp(&FileName[len-obj_ext_len], obj_ext_str) == 0) ||
         (len>=sym_ext_len &&
         strcmp(&FileName[len-sym_ext_len],sym_ext_str)==0)) {
      strcpy(full_name, FileName);
   } else {
      sprintf(full_name, "%s.%s", FileName, OBJ_FILE_EXT);
   }
   if ((fp=fopen(full_name, "r")) == NULL) {
      fprintf(stderr, "Can not open '%s'.\n", full_name);
      return FALSE;
   }

   strcpy(tmp_filename, scanFileName);
   tmp_linenum = scanLineNum;
   strcpy(scanFileName, full_name);
   scanLineNum = 0;

   fprintf(stderr, "Reading '%s' ...\n", full_name);

   while ((read_status = ReadObj(fp, &obj_ptr)) == TRUE) {
      if (obj_ptr != NULL) {
         AdjForOldVersion(obj_ptr);
         AddObj(NULL, topObj, obj_ptr);
      }
   }

   strcpy(scanFileName, tmp_filename);
   scanLineNum = tmp_linenum;

   fclose(fp);

   if (read_status == INVALID) {
      fprintf(stderr, "File version too large (=%1d).  Abort processing!\n",
            fileVersion);
      return FALSE;
   }
   if (cmdLineHasPageNum) {
      if (cmdLinePageNum > lastPageNum) {
         fprintf(stderr, "%s is skipped because it only contains %1d pages!\n",
               FileName, lastPageNum);
         return FALSE;
      }
      GotoPageNum(cmdLinePageNum);
   }
   return TRUE;
}

static
void DoPrTgif(argc, argv, from_prtgif)
   int argc, from_prtgif;
   char *argv[];
{
   char inbuf[MAXSTRING+1];
   char *c_ptr;
   int len, argc_to_be;

   if ((argc_to_be=ProcessPrTgifOptions(argc, argv, from_prtgif)) == INVALID) {
      return;
   }
   while (argc > argc_to_be) {
      argc--; argv++;
   }
   InitPaperSize();
   if (argc <= 0) {
      fprintf(stderr, "\nTgif File Name to Print> ");
      fflush(stderr);
      while (fgets(inbuf, MAXSTRING, stdin) != NULL) {
         len = strlen(inbuf);
         if (len > 0) {
            if (inbuf[--len] == '\n') inbuf[len] = '\0';
            if (PrTgifLoad(inbuf)) {
               colorDump = FALSE;
               if (cmdLineOneFilePerPage) {
                  if (pageLayoutMode == PAGE_TILE) {
                     fprintf(stderr, "%s %s.\n",
                           "Cannot use -one_file_per_page in",
                           "TILED page mode");
                  } else {
                     if (cmdLineHasPageNum) {
                        sprintf(cmdLinePageNumStr, "%1d", cmdLinePageNum);
                        GotoPageNum(cmdLinePageNum);
                        Dump(*argv);
                     } else {
                        cmdLineHasPageNum = TRUE;
                        for (cmdLinePageNum=1; cmdLinePageNum <= lastPageNum;
                              cmdLinePageNum++) {
                           sprintf(cmdLinePageNumStr, "%1d", cmdLinePageNum);
                           GotoPageNum(cmdLinePageNum);
                           Dump(inbuf);
                        }
                        cmdLineHasPageNum = FALSE;
                     }
                  }
               } else {
                  Dump(inbuf);
               }
               DelAllObj();
            }
         }
         fprintf(stderr, "\nTgif File Name to Print> ");
         fflush(stderr);
      }
   } else {
      for ( ; argc > 0; argc--, argv++) {
         lastFile = (argc == 1);
         if (PrTgifLoad(*argv)) {
            colorDump = FALSE;
            if (cmdLineOneFilePerPage) {
               if (pageLayoutMode == PAGE_TILE) {
                  fprintf(stderr, "%s %s.\n",
                        "Cannot use -one_file_per_page in",
                        "TILED page mode");
               } else {
                  if (cmdLineHasPageNum) {
                     sprintf(cmdLinePageNumStr, "%1d", cmdLinePageNum);
                     GotoPageNum(cmdLinePageNum);
                     Dump(*argv);
                  } else {
                     cmdLineHasPageNum = TRUE;
                     for (cmdLinePageNum=1; cmdLinePageNum <= lastPageNum;
                           cmdLinePageNum++) {
                        sprintf(cmdLinePageNumStr, "%1d", cmdLinePageNum);
                        GotoPageNum(cmdLinePageNum);
                        Dump(*argv);
                     }
                     cmdLineHasPageNum = FALSE;
                  }
               }
            } else {
               Dump(*argv);
            }
            DelAllObj();
         }
      }
   }
   CleanUpPaperSize();
}

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
void DoTgif(argc, argv)
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

   if (!ProcessTgifOptions(argc, argv, file_name)) return;

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
      }

      Msg("Returned from basic driver.");
/*
 *    Prompt2("Input an operation and a sub command.\n",op_name,file_name);
 *
 *    if (strcmp(op_name, "animate") == 0)
 *    {
 *       Prompt3("Input poly_id, speed, color.\n", id_name, speed_name,
 *             color_name);
 *       Animate(file_name, id_name, speed_name, color_name, &func_strp);
 *       printf("Animate RETURNs --> %s %s %s\n", func_strp, sp[0], sp[1]);
 *    }
 *    if (strcmp(op_name, "upd_attr_val") == 0)
 *    {
 *       Prompt3("Input attrname, color and value.\n", attr_name, color_name,
 *             val_name);
 *       UpdAttrVal(file_name, attr_name, color_name, val_name, &func_strp);
 *       printf("UpdAttrVal RETURNs --> %s %s %s\n", func_strp, sp[0], sp[1]);
 *    }
 */
      *op_name = *file_name = '\0';
      MainLoop(op_name, file_name, &func_strp,
            &sp[0], &sp[1], &sp[2], &sp[3], &sp[4], &sp[5]);
/*
 *    printf("RETURN --> %s ", func_strp);
 *    for (i = 0; i < 6; i++)
 *       if (strcmp(sp[i], "") != 0)
 *          printf("%s ", sp[i]);
 *       else
 *          break;
 *    printf("\n");
 */
   }
}

int main(argc, argv)
   int argc;
   char *argv[];
{
   register int i, from_prtgif;

   origArgC = argc;
   origArgV = argv;

   PRTGIF = FALSE;
   from_prtgif = FALSE;
   SetProgramName(*argv);
   for (i = 1; i < argc; i++) {
      if (strcmp(argv[i],"-prtgif")==0) {
         PRTGIF = TRUE;
         from_prtgif = TRUE;
         break;
      } else if ((strcmp(argv[i],"-print")==0) ||
            (strcmp(argv[i],"-PRINT")==0)) {
         PRTGIF = TRUE;
         break;
      }
   }
/*
 * malloc_debug (1);
 */
   if (PRTGIF) {
      DoPrTgif(argc, argv, from_prtgif);
   } else {
      DoTgif(argc, argv);
   }
   return 0;
}
