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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/exec.c,v 3.3 1996/05/15 17:33:15 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <sys/time.h>
#ifdef _NO_GETTIMEOFDAY
#include <sys/timeb.h>
#endif /* _NO_GETTIMEOFDAY */
#ifdef isc /* SunSoft/Interactive UNIX */
#include <sys/bsdtypes.h>
#endif

#if !defined(FD_SET)
#include <sys/select.h>
#endif /* !defined(FD_SET) */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "const.h"
#include "types.h"

#include "align.e"
#include "attr.e"
#include "auxtext.e"
#include "cmd.e"
#include "color.e"
#include "choice.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "dup.e"
#include "edit.e"
#include "eps.e"
#ifndef _NO_EXTERN
#include "exec.e"
#endif
#include "expr.e"
#include "file.e"
#include "font.e"
#include "mainloop.e"
#include "menu.e"
#include "move.e"
#include "msg.e"
#include "names.e"
#include "navigate.e"
#include "obj.e"
#include "pattern.e"
#include "poly.e"
#include "raster.e"
#include "remote.e"
#include "select.e"
#include "setup.e"
#include "shortcut.e"
#include "stk.e"
#include "stretch.e"
#include "text.e"
#include "util.e"
#include "xbitmap.e"
#include "xpixmap.e"

int execAnimating=FALSE;
int execAnimateRedraw=FALSE;
int execCurDepth=0;
int replaceAttrFirstValueRedraw=TRUE;
int execNavigateBack=FALSE;

struct AttrRec *warpToAttr=NULL;

char *cmdToExecAfterHyperJump=NULL;

extern char	* mktemp ARGS_DECL((char *Template));
extern char	* getenv ARGS_DECL((char *));
#ifndef _NO_EXTERN
extern int	fork ARGS_DECL((void));
#if defined(IRIX) || defined(linux)
extern int	system ARGS_DECL((const char *));
#else /* ~(defined(IRIX) || defined(linux)) */
extern int	system ARGS_DECL((char *));
#endif /* defined(IRIX) || defined(linux) */
extern int	unlink ARGS_DECL((char *));
#endif
extern int	atoi ARGS_DECL((char *));

#define TOK_INVALID	(INVALID)
#define TOK_EMPTY	0
#define TOK_STR		1
#define TOK_LEFT_P	2
#define TOK_RIGHT_P	3
#define TOK_LEFT_B	4
#define TOK_RIGHT_B	5
#define TOK_LEFT_CB	6
#define TOK_RIGHT_CB	7
#define TOK_COMMA	8
#define TOK_SEMI	9

static char execDummyStr[MAXSTRING+1];
static int gnAbortExec=FALSE;

typedef int (ExecFunc)ARGS_DECL((char **argv, struct ObjRec *obj_ptr,
                                 char *orig_cmd));
typedef int (RawExecFunc)ARGS_DECL((char **argv, char **raw_argv,
                                    struct ObjRec *obj_ptr, char *orig_cmd));
typedef void (SimpleExecFunc)ARGS_DECL((void));

typedef struct ExecInfoRec {
   void *pfunc;
   char *func_name;
   int func_argc;
         /*
          * if (func_argc == 0) {
          *    pfunc is (SimpleExecFunc)();
          * } else if (func_argc < 0) {
          *    pfunc is (RawExecFunc)();
          * } else {
          *    pfunc is (ExecFunc)();
          * }
          */
   int double_quotes_for_null;
         /*
          * If TRUE, then if $(foo) evaluates to the empty string, "" is
          *       passed.  This is mainly used by commands with expression
          *       evaluations.
          */
} * ExecInfoPtr;

int ExecLaunch ARGS_DECL((char**, struct ObjRec *, char*));
int ExecExec ARGS_DECL((char**, struct ObjRec *, char*));
int ExecMktemp ARGS_DECL((char**, struct ObjRec *, char*));
int ExecUseTemplate ARGS_DECL((char**, struct ObjRec *, char*));
int ExecUpdEPSChild ARGS_DECL((char**, struct ObjRec *, char*));
int ExecUpdXBMChild ARGS_DECL((char**, struct ObjRec *, char*));
int ExecUpdXPMChild ARGS_DECL((char**, struct ObjRec *, char*));
int ExecFlipDeck ARGS_DECL((char**, struct ObjRec *, char*));
int ExecReadFileIntoAttr ARGS_DECL((char**, struct ObjRec *, char*));
int ExecWriteAttrIntoFile ARGS_DECL((char**, struct ObjRec *, char*));
int ExecAppendAttrIntoFile ARGS_DECL((char**, struct ObjRec *, char*));
int ExecSelectObjByName ARGS_DECL((char**, struct ObjRec *, char*));
void ExecUnSelectAllObj ARGS_DECL((void));
int ExecMoveSelObjRel ARGS_DECL((char**, struct ObjRec *, char*));
int ExecRepeat ARGS_DECL((char**, struct ObjRec *, char*));
int ExecHyperJump ARGS_DECL((char**, struct ObjRec *, char*));
int ExecMakeCGIQuery ARGS_DECL((char**, struct ObjRec *, char*));
int ExecWaitClick ARGS_DECL((char**, struct ObjRec *, char*));
int ExecSleep ARGS_DECL((char**, struct ObjRec *, char*));
void ExecBeginAnimate ARGS_DECL((void));
void ExecEndAnimate ARGS_DECL((void));
int ExecSetRedraw ARGS_DECL((char**, struct ObjRec *, char*));
int ExecSetSelObjColor ARGS_DECL((char**, struct ObjRec *, char*));
int ExecSetSelObjFill ARGS_DECL((char**, struct ObjRec *, char*));
int ExecSetSelObjPen ARGS_DECL((char**, struct ObjRec *, char*));
int ExecSetSelObjLineWidth ARGS_DECL((char**, struct ObjRec *, char*));
int ExecSetSelObjSpline ARGS_DECL((char**, struct ObjRec *, char*));
int ExecSetSelObjArrow ARGS_DECL((char**, struct ObjRec *, char*));
int ExecSetSelObjDash ARGS_DECL((char**, struct ObjRec *, char*));
int ExecInc ARGS_DECL((char**, struct ObjRec *, char*));
int ExecDec ARGS_DECL((char**, struct ObjRec *, char*));
int ExecShuffleObjToTop ARGS_DECL((char**, struct ObjRec *, char*));
void ExecDisableUndo ARGS_DECL((void));
void ExecEnableUndo ARGS_DECL((void));
int ExecGetDrawingArea ARGS_DECL((char**, struct ObjRec *, char*));
int ExecGetSelObjBBox ARGS_DECL((char**, struct ObjRec *, char*));
int ExecMoveSelObjAbs ARGS_DECL((char**, struct ObjRec *, char*));
int ExecAssign ARGS_DECL((char**, struct ObjRec *, char*));
int ExecStrCpy ARGS_DECL((char**, struct ObjRec *, char*));
int ExecWhile ARGS_DECL((char**, char**, struct ObjRec *, char*));
int ExecIf ARGS_DECL((char**, struct ObjRec *, char*));
int ExecGetCurrentFile ARGS_DECL((char**, struct ObjRec *, char*));
int ExecGetEnv ARGS_DECL((char**, struct ObjRec *, char*));
int ExecStrLen ARGS_DECL((char**, struct ObjRec *, char*));
int ExecSubStr ARGS_DECL((char**, struct ObjRec *, char*));
int ExecStrStr ARGS_DECL((char**, struct ObjRec *, char*));
int ExecStrRStr ARGS_DECL((char**, struct ObjRec *, char*));
void ExecUnMakeSelObjIconic ARGS_DECL((void));
int ExecHyperJumpThenExec ARGS_DECL((char**, struct ObjRec *, char*));
int ExecShowAttr ARGS_DECL((char**, struct ObjRec *, char*));
int ExecHideAttr ARGS_DECL((char**, struct ObjRec *, char*));
int ExecShowAttrName ARGS_DECL((char**, struct ObjRec *, char*));
int ExecHideAttrName ARGS_DECL((char**, struct ObjRec *, char*));
int ExecShowValue ARGS_DECL((char**, struct ObjRec *, char*));
int ExecHideValue ARGS_DECL((char**, struct ObjRec *, char*));
int ExecGetAttrBBox ARGS_DECL((char**, struct ObjRec *, char*));
int ExecSizeSelObjAbs ARGS_DECL((char**, struct ObjRec *, char*));
int ExecMessageBox ARGS_DECL((char**, struct ObjRec *, char*));
int ExecGetUserInput ARGS_DECL((char**, struct ObjRec *, char*));
int ExecAddAttrToSelObj ARGS_DECL((char**, struct ObjRec *, char*));
int ExecUserEndAnEdge ARGS_DECL((char**, struct ObjRec *, char*));
int ExecUserDrawAnEdge ARGS_DECL((char**, struct ObjRec *, char*));
int ExecGetAPolyVertexAbs ARGS_DECL((char**, struct ObjRec *, char*));
int ExecMoveAPolyVertexAbs ARGS_DECL((char**, struct ObjRec *, char*));
int ExecPostAttrAndGetCGI ARGS_DECL((char**, struct ObjRec *, char*));
void ExecNavigateBack ARGS_DECL((void));
void ExecStop ARGS_DECL((void));
int ExecSqrt ARGS_DECL((char**, struct ObjRec *, char*));
int ExecRandom ARGS_DECL((char**, struct ObjRec *, char*));
int ExecRound ARGS_DECL((char**, struct ObjRec *, char*));
int ExecRedrawObj ARGS_DECL((char**, struct ObjRec *, char*));
void ExecRedrawDrawingArea ARGS_DECL((void));
int ExecIntToHex ARGS_DECL((char**, struct ObjRec *, char*));
int ExecForI ARGS_DECL((char**, struct ObjRec *, char*));
void ExecSetFileNotModified ARGS_DECL((void));
int ExecNewId ARGS_DECL((char**, struct ObjRec *, char*));
int ExecRotateSelObj ARGS_DECL((char**, struct ObjRec *, char*));
int ExecCallSimpleShortCut ARGS_DECL((char**, struct ObjRec *, char*));
int ExecCallOneArgShortCut ARGS_DECL((char**, struct ObjRec *, char*));

static
struct ExecInfoRec gExecInfo[] = {
   { (void*)ExecLaunch,             "launch",                            1, 0},
   { (void*)ExecExec,               "exec",                              1, 0},
   { (void*)ExecMktemp,             "mktemp",                            2, 0},
   { (void*)ExecUseTemplate,        "create_file_using_simple_template", 4, 0},
   { (void*)ExecUpdEPSChild,        "update_eps_child",                  1, 0},
   { (void*)ExecUpdXBMChild,        "update_xbm_child",                  1, 0},
   { (void*)ExecUpdXPMChild,        "update_xpm_child",                  1, 0},
   { (void*)ExecFlipDeck,           "flip_deck",                         3, 0},
   { (void*)ExecReadFileIntoAttr,   "read_file_into_attr",               2, 0},
   { (void*)ExecWriteAttrIntoFile,  "write_attr_into_file",              2, 0},
   { (void*)ExecAppendAttrIntoFile, "append_attr_into_file",             2, 0},
   { (void*)ExecSelectObjByName,    "select_obj_by_name",                1, 0},
   { (void*)ExecUnSelectAllObj,     "unselect_all_obj",                  0, 0},
   { (void*)ExecMoveSelObjRel,      "move_selected_obj_relative",        2, 0},
   { (void*)ExecRepeat,             "repeat",                            2, 0},
   { (void*)ExecHyperJump,          "hyperjump",                         1, 0},
   { (void*)ExecMakeCGIQuery,       "make_cgi_query",                    3, 0},
   { (void*)ExecWaitClick,          "wait_click",                        3, 0},
   { (void*)ExecSleep,              "sleep",                             2, 0},
   { (void*)ExecBeginAnimate,       "begin_animate",                     0, 0},
   { (void*)ExecEndAnimate,         "end_animate",                       0, 0},
   { (void*)ExecSetRedraw,          "set_redraw",                        1, 0},
   { (void*)ExecSetSelObjColor,     "set_selected_obj_color",            1, 0},
   { (void*)ExecSetSelObjFill,      "set_selected_obj_fill",             1, 0},
   { (void*)ExecSetSelObjPen,       "set_selected_obj_pen",              1, 0},
   { (void*)ExecSetSelObjLineWidth, "set_selected_obj_line_width",       3, 0},
   { (void*)ExecSetSelObjSpline,    "set_selected_obj_spline",           1, 0},
   { (void*)ExecSetSelObjArrow,     "set_selected_obj_arrow",            1, 0},
   { (void*)ExecSetSelObjDash,      "set_selected_obj_dash",             1, 0},
   { (void*)ExecInc,                "inc",                               2, 0},
   { (void*)ExecDec,                "dec",                               2, 0},
   { (void*)ExecShuffleObjToTop,    "shuffle_obj_to_top",                1, 0},
   { (void*)ExecDisableUndo,        "disable_undo",                      0, 0},
   { (void*)ExecEnableUndo,         "enable_undo",                       0, 0},
   { (void*)ExecGetDrawingArea,     "get_drawing_area",                  4, 0},
   { (void*)ExecGetSelObjBBox,      "get_selected_obj_bbox",             4, 0},
   { (void*)ExecMoveSelObjAbs,      "move_selected_obj_absolute",        2, 0},
   { (void*)ExecAssign,             "assign",                            2, 0},
   { (void*)ExecStrCpy,             "strcpy",                            2, 0},
   { (void*)ExecWhile,              "while",                            -2, 1},
   { (void*)ExecIf,                 "if",                                3, 1},
   { (void*)ExecGetCurrentFile,     "get_current_file",                  1, 0},
   { (void*)ExecGetEnv,             "getenv",                            2, 0},
   { (void*)ExecStrLen,             "strlen",                            2, 1},
   { (void*)ExecSubStr,             "substr",                            4, 1},
   { (void*)ExecStrStr,             "strstr",                            3, 1},
   { (void*)ExecStrRStr,            "strrstr",                           3, 1},
   { (void*)ExecUnMakeSelObjIconic, "unmake_selected_obj_iconic",        0, 0},
   { (void*)ExecHyperJumpThenExec,  "hyperjump_then_exec",               2, 0},
   { (void*)ExecShowAttr,           "show_attr",                         1, 0},
   { (void*)ExecHideAttr,           "hide_attr",                         1, 0},
   { (void*)ExecShowAttrName,       "show_attr_name",                    1, 0},
   { (void*)ExecHideAttrName,       "hide_attr_name",                    1, 0},
   { (void*)ExecShowValue,          "show_value",                        1, 0},
   { (void*)ExecHideValue,          "hide_value",                        1, 0},
   { (void*)ExecGetAttrBBox,        "get_attr_bbox",                     5, 0},
   { (void*)ExecSizeSelObjAbs,      "size_selected_obj_absolute",        2, 0},
   { (void*)ExecMessageBox,         "message_box",                       4, 0},
   { (void*)ExecGetUserInput,       "get_user_input",                    3, 0},
   { (void*)ExecAddAttrToSelObj,    "add_attr_to_selected_obj",          4, 1},
   { (void*)ExecUserEndAnEdge,      "user_end_an_edge",                  3, 1},
   { (void*)ExecUserDrawAnEdge,     "user_draw_an_edge",                 2, 0},
   { (void*)ExecGetAPolyVertexAbs,  "get_a_poly_vertex_absolute",        4, 0},
   { (void*)ExecMoveAPolyVertexAbs, "move_a_poly_vertex_absolute",       4, 0},
   { (void*)ExecPostAttrAndGetCGI,  "post_attr_and_get_cgi_result",      3, 0},
   { (void*)ExecNavigateBack,       "navigate_back",                     0, 0},
   { (void*)ExecStop,               "stop",                              0, 0},
   { (void*)ExecSqrt,               "sqrt",                              2, 0},
   { (void*)ExecRandom,             "random",                            1, 0},
   { (void*)ExecRound,              "round",                             2, 0},
   { (void*)ExecRedrawObj,          "redraw_obj",                        1, 0},
   { (void*)ExecRedrawDrawingArea,  "redraw_drawing_area",               0, 0},
   { (void*)ExecIntToHex,           "itox",                              3, 0},
   { (void*)ExecForI,               "for_i",                             5, 0},
   { (void*)ExecSetFileNotModified, "set_file_not_modified",             0, 0},
   { (void*)ExecNewId,              "new_id",                            1, 0},
   { (void*)ExecRotateSelObj,       "rotate_selected_obj",               1, 0},
   { (void*)ExecCallSimpleShortCut, "call_simple_shortcut",              1, 0},
   { (void*)ExecCallOneArgShortCut, "call_one_arg_shortcut",             2, 0},
   { NULL, NULL, 0, 0 }
};

struct ObjRec *FindObjWithName(BotObj, OrigObj, obj_name, inside_root_obj,
      inside_this_obj, pp_owner_obj, pp_top_owner)
   struct ObjRec *BotObj, *OrigObj, **pp_owner_obj, **pp_top_owner;
   char *obj_name;
   int inside_root_obj, inside_this_obj;
{
   register struct AttrRec *attr_ptr;
   register struct ObjRec *obj_ptr;
   struct ObjRec *bot_obj=BotObj;
   char *bang_ptr;

   if (obj_name == NULL || *obj_name == '\0') return NULL;
   if (*obj_name == '!') {
      if (inside_root_obj || inside_this_obj) return NULL;
      obj_name++;
      inside_root_obj = TRUE;
      inside_this_obj = FALSE;
      if (pp_owner_obj != NULL) *pp_owner_obj = NULL;
      if (pp_top_owner != NULL) *pp_top_owner = NULL;
      bot_obj = botObj;
   }
   if (*obj_name == '!') return NULL;

   if ((bang_ptr=strchr(obj_name, '!')) != NULL) {
      if (OrigObj != NULL) {
         if (!(OrigObj->type == OBJ_GROUP || OrigObj->type == OBJ_ICON ||
               OrigObj->type == OBJ_SYM)) {
            return NULL;
         }
      }
      *bang_ptr = '\0';
      if (OrigObj != NULL && strcmp(obj_name, "THIS") == 0) {
         int this_ok=FALSE;

         *bang_ptr++ = '!';
         if (inside_root_obj || inside_this_obj) return NULL;
         if (!(OrigObj->type == OBJ_GROUP || OrigObj->type == OBJ_ICON ||
               OrigObj->type == OBJ_SYM)) {
            return NULL;
         }
         for (obj_ptr=botObj; obj_ptr!=NULL; obj_ptr=obj_ptr->prev) {
            if (obj_ptr == OrigObj) {
               this_ok = TRUE;
               break;
            }
         }
         if (!this_ok) return NULL;
         if (pp_top_owner != NULL) *pp_top_owner = OrigObj;
         if (pp_owner_obj != NULL) *pp_owner_obj = OrigObj;

         return FindObjWithName(OrigObj->detail.r->last, OrigObj,
               bang_ptr, FALSE, TRUE, pp_owner_obj, pp_top_owner);
      } else {
         for (obj_ptr=bot_obj; obj_ptr!=NULL; obj_ptr=obj_ptr->prev) {
            if (obj_ptr->fattr != NULL &&
                  (attr_ptr=FindAttrWithName(obj_ptr,"name=",NULL)) != NULL &&
                  strcmp (attr_ptr->attr_value.s, obj_name) == 0) {
               *bang_ptr++ = '!';
               if (!(obj_ptr->type == OBJ_GROUP || obj_ptr->type == OBJ_ICON ||
                     obj_ptr->type == OBJ_SYM)) {
                  return NULL;
               }
               if (pp_owner_obj != NULL) *pp_owner_obj = obj_ptr;
               if (pp_top_owner != NULL && bot_obj == botObj) {
                  *pp_top_owner = OrigObj;
               }
               return FindObjWithName(obj_ptr->detail.r->last, obj_ptr,
                     bang_ptr, inside_root_obj, inside_this_obj, pp_owner_obj,
                     pp_top_owner);
            }
         }
         *bang_ptr = '!';
         return NULL;
      }
   } else if (strcmp(obj_name, "THIS") == 0) {
      int this_ok=FALSE;

      if (inside_root_obj || inside_this_obj) return NULL;
      for (obj_ptr=botObj; obj_ptr!=NULL; obj_ptr=obj_ptr->prev) {
         if (obj_ptr == OrigObj) {
            this_ok = TRUE;
            break;
         }
      }
      if (!this_ok) return NULL;
      if (pp_top_owner != NULL) *pp_top_owner = NULL;
      if (pp_owner_obj != NULL) *pp_owner_obj = NULL;

      return OrigObj;
   }
   for (obj_ptr=bot_obj; obj_ptr!=NULL; obj_ptr=obj_ptr->prev) {
      if (obj_ptr->fattr != NULL &&
            (attr_ptr=FindAttrWithName(obj_ptr,"name=",NULL)) != NULL &&
            strcmp (attr_ptr->attr_value.s, obj_name) == 0) {
         if (pp_top_owner != NULL && bot_obj == botObj) {
            *pp_top_owner = obj_ptr;
         }
         return (obj_ptr);
      }
   }
   return NULL;
}

static int gnSeenLeftParan=FALSE;

static
int EndingRightParan(inbuf)
   char *inbuf;
{
   for ( ; *inbuf != '\0'; inbuf++) {
      if (*inbuf != ' ' && *inbuf != '\t') {
         return (*inbuf == ';');
      }
   }
   return TRUE;
}

static
int OnlyBlanksLeft(inbuf)
   char *inbuf;
{
   for ( ; *inbuf != '\0'; inbuf++) {
      if (*inbuf != ' ' && *inbuf != '\t') {
         return FALSE;
      }
   }
   return TRUE;
}

static
char *GetToken(inbuf, outbuf, tok_type)
   char *inbuf, *outbuf;
   int *tok_type;
   /* returns NULL of the input string is invalid */
   /* otherwise, the return value points to the character */
   /*        immediately following the end of the token */
   /* *tok_type contains the token type */
{
   register char *c_ptr, *obuf_ptr=outbuf;

   *tok_type = TOK_INVALID;
   while (*inbuf == ' ' || *inbuf == '\t') inbuf++;
   switch (*inbuf) {
   case '\0':
      *tok_type = TOK_EMPTY;
      return (inbuf);
/* case '\'':
   case '"':
      *tok_type = TOK_STR;
      for (c_ptr=(&inbuf[1]); *c_ptr!=(*inbuf) && *c_ptr!='\0';
            c_ptr++, obuf_ptr++) {
         switch (*c_ptr) {
         case '\\':
            switch (*(++c_ptr)) {
            case 'n': *obuf_ptr = '\n'; break;
            case 'r': *obuf_ptr = '\r'; break;
            case 't': *obuf_ptr = '\t'; break;
            default: *obuf_ptr = *c_ptr; break;
            }
            break;
         default: *obuf_ptr = *c_ptr; break;
         }
      }
      if (*c_ptr == '\0') return (NULL);
      *obuf_ptr++ = '\0';
      return (++c_ptr);
 */
   case '(':
      if (gnSeenLeftParan) {
         break;
      } else {
         strcpy(obuf_ptr,"(");
         *tok_type=TOK_LEFT_P;
         gnSeenLeftParan = TRUE;
         return(&inbuf[1]);
      }
   case ')':
      if (gnSeenLeftParan && !EndingRightParan(&inbuf[1])) {
         break;
      } else {
         strcpy(obuf_ptr,")");
         *tok_type=TOK_RIGHT_P;
         return(&inbuf[1]);
      }
/* case '[': strcpy(obuf_ptr,"["); *tok_type=TOK_LEFT_B; return(&inbuf[1]); */
/* case ']': strcpy(obuf_ptr,"]"); *tok_type=TOK_RIGHT_B; return(&inbuf[1]); */
/* case '{': strcpy(obuf_ptr,"{"); *tok_type=TOK_LEFT_CB; return(&inbuf[1]); */
/* case '}': strcpy(obuf_ptr,"}"); *tok_type=TOK_RIGHT_CB; return(&inbuf[1]); */
   case ',': strcpy(obuf_ptr,","); *tok_type=TOK_COMMA; return(&inbuf[1]);
   case ';': strcpy(obuf_ptr,";"); *tok_type=TOK_SEMI; return(&inbuf[1]);
   }
   *tok_type = TOK_STR;
   c_ptr = inbuf;
/* while (*c_ptr != '\0' && strchr (" \t()[]{},;", *c_ptr) == NULL) {
      if (*c_ptr == '$' && c_ptr[1] == '(') {
         *obuf_ptr++ = *c_ptr++;
         *obuf_ptr++ = *c_ptr++;
         while (*c_ptr != '\0' && *c_ptr != ')') *obuf_ptr++ = *c_ptr++;
         if (*c_ptr == '\0') return NULL;
      } else {
         *obuf_ptr++ = *c_ptr++;
      }
   }
 */
   while (*c_ptr != '\0') {
      if (*c_ptr == '$' && c_ptr[1] == '(') {
         *obuf_ptr++ = *c_ptr++;
         *obuf_ptr++ = *c_ptr++;
         while (*c_ptr != '\0' && *c_ptr != ')') *obuf_ptr++ = *c_ptr++;
         if (*c_ptr == '\0') return NULL;
      } else if (*c_ptr == '"' || *c_ptr == '\'') {
         char *tmp_c_ptr;

         *obuf_ptr++ = *c_ptr;
         for (tmp_c_ptr=(&c_ptr[1]); *tmp_c_ptr!=(*c_ptr) && *tmp_c_ptr!='\0';
               tmp_c_ptr++) {
            switch (*tmp_c_ptr) {
            case '\\':
               switch (*(++tmp_c_ptr)) {
               case 'n': *obuf_ptr++ = '\n'; break;
               case 'r': *obuf_ptr++ = '\r'; break;
               case 't': *obuf_ptr++ = '\t'; break;
               default: *obuf_ptr++ = *tmp_c_ptr; break;
               }
               break;
            default: *obuf_ptr++ = *tmp_c_ptr; break;
            }
         }
         if (*tmp_c_ptr == '\0') return NULL;
         c_ptr = tmp_c_ptr;
         *obuf_ptr++ = *c_ptr++;
      } else {
         *obuf_ptr++ = *c_ptr++;
      }
      if (*c_ptr == ')') {
         if (!gnSeenLeftParan || EndingRightParan(&c_ptr[1])) {
            break;
         }
      } else if (*c_ptr == '(') {
         if (!gnSeenLeftParan) {
            break;
         }
      } else if (*c_ptr == ',' || *c_ptr == ';') {
         break;
      }
   }
   *obuf_ptr = '\0';
   return (c_ptr);
}

static
char *convert_str(inbuf, obj_ptr, double_quotes_for_null)
   char *inbuf;
   struct ObjRec *obj_ptr;
   int double_quotes_for_null;
{
   register char *buf_ptr;
   char *return_str, *return_ptr, quote_char='\0';
   int cur_size=(MAXSTRING<<1), count=0, return_len=0, inside_quote=FALSE;

   return_ptr = return_str = (char *)malloc((cur_size+2)*sizeof(char));
   if (return_str == NULL) return (char*)FailAllocMessage();
   *return_str = '\0';
   buf_ptr = inbuf;
   while (*inbuf != '\0') {
      struct AttrRec *attr_ptr;
      int null_string=FALSE, found_var=FALSE, n;
      char *c_ptr=inbuf, *new_c_ptr=NULL, *cp, *cp1;

      while (new_c_ptr == NULL) {
         if (inside_quote) {
            for ( ; *c_ptr != '\0'; c_ptr++) {
               if (*c_ptr == quote_char) {
                  inside_quote = FALSE;
                  c_ptr++;
                  break;
               } else if (*c_ptr == '$' && c_ptr[1] == '(') {
                  new_c_ptr = c_ptr;
                  break;
               }
            }
         } else {
            for ( ; *c_ptr != '\0'; c_ptr++) {
               if (*c_ptr == '"' || *c_ptr == '\'') {
                  quote_char = *c_ptr;
                  inside_quote = TRUE;
                  c_ptr++;
                  break;
               } else if (*c_ptr == '$' && c_ptr[1] == '(') {
                  new_c_ptr = c_ptr;
                  break;
               }
            }
         }
         if (*c_ptr == '\0') break;
      }
      if (new_c_ptr == NULL) {
         count = strlen(inbuf);
         if (count != 0) {
            if (count+return_len >= cur_size) {
               n = return_ptr-return_str;
               cur_size += count+MAXSTRING;
               return_str = (char*)realloc(return_str,
                     (cur_size+2)*sizeof(char));
               return_ptr = &return_str[n];
            }
            strncpy(return_ptr, inbuf, count);
            return_ptr += count;
            *return_ptr = '\0';
         }
         return return_str;
      }
      buf_ptr = new_c_ptr;
      count = buf_ptr-inbuf;
      return_len += count;
      if (count != 0) {
         if (count+return_len >= cur_size) {
            n = return_ptr-return_str;
            cur_size += count+MAXSTRING;
            return_str = (char*)realloc(return_str, (cur_size+2)*sizeof(char));
            return_ptr = &return_str[n];
         }
         strncpy(return_ptr, inbuf, count);
         return_ptr += count;
         *return_ptr = '\0';
      }
      if ((attr_ptr=ValidAttrArg(buf_ptr, obj_ptr, &new_c_ptr)) == NULL) {
         free(return_str);
         return NULL;
      }
      count = attr_ptr->attr_value.sz-1;
      if (count == 0 && double_quotes_for_null && !inside_quote) {
         null_string = TRUE;
         count += 2;
      }
      if (count+return_len >= cur_size) {
         n = return_ptr-return_str;
         cur_size += count+MAXSTRING;
         return_str = (char*)realloc(return_str, (cur_size+2)*sizeof(char));
         return_ptr = &return_str[n];
      }
      if (null_string) {
         return_ptr[0] = return_ptr[1] = '"';
         return_ptr[2] = '\0';
      } else {
         strcpy(return_ptr, attr_ptr->attr_value.s);
      }
      if ((cp=strstr(return_ptr, "//")) != NULL) {
         *cp = '\0';
         count = cp-return_ptr;
      }
      if (!double_quotes_for_null && *return_ptr == '"' &&
            return_ptr[count-1] == '"') {
         int n;

         for (n=1, cp1=return_ptr, cp=(&return_ptr[1]); n < count-1; n++) {
            *cp1++ = *cp++;
         }
         *cp1 = '\0';
         count -= 2;
      }
      return_ptr += count;
      inbuf = buf_ptr = ++new_c_ptr;
   }
   return return_str;
}

static
void LaunchIt(cmd)
   char *cmd;
{
   int len=strlen(cmd);

   while (len > 0 && (cmd[len-1] == ' ')) cmd[--len] = '\0';
   if (cmd[0] != '\0') {
      SetWatchCursor(drawWindow);
      SetWatchCursor(mainWindow);
      if (cmd[len-1] == '&') {
         cmd[--len] = '\0';
         while (len > 0 && (cmd[len-1] == ' ')) cmd[--len] = '\0';
         if (cmd[0] != '\0') {
#ifdef _BACKGROUND_DONT_FORK
            fprintf(stderr, "Backgrounding:  '%s'.\n", cmd);
            strcat(cmd, " &");
            system(cmd);
#else /* ~_BACKGROUND_DONT_FORK */
            int pid;

            fprintf(stderr, "Backgrounding:  '%s'.\n", cmd);
            pid = fork();
            if (pid == 0) { system(cmd); exit(0); }
#endif /* _BACKGROUND_DONT_FORK */
         }
      } else {
         FILE *fp;

         sprintf(gszMsgBox, "Executing '%s'...", cmd);
         SetStringStatus(gszMsgBox);
         XSync(mainDisplay, False);
         if ((fp=(FILE*)popen(cmd, "r")) == NULL) {
            sprintf(gszMsgBox, "Fail to popen(%s).", cmd);
            MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         } else {
            char tmp_str[MAXSTRING+1];
            XEvent ev;

            while (fgets(tmp_str, MAXSTRING, fp) != NULL) {
               fprintf(stderr, "%s", tmp_str);
               if (XCheckMaskEvent(mainDisplay,
                     ExposureMask | VisibilityChangeMask, &ev)) {
                  ExposeEventHandler(&ev, TRUE);
                  while (XCheckMaskEvent(mainDisplay,
                        ExposureMask | VisibilityChangeMask, &ev)) ;
               }
            }
            pclose(fp);
         }
         SetStringStatus("...Done");
      }
      SetDefaultCursor(mainWindow);
      SetDefaultCursor(drawWindow);
   }
}

int DoLaunch(launch_attr, obj_ptr)
   struct AttrRec *launch_attr;
   struct ObjRec *obj_ptr;
{
   register char *c_ptr, *buf_ptr;
   char *new_c_ptr, buf[MAXSTRING+1], *cmd, *cmd_ptr;
   int cur_size=2*MAXSTRING, count=0, cmd_len=0, n, first_time=TRUE;
   struct StrRec *str_ptr;

   cmd = (char *)malloc((cur_size+1)*sizeof(char));
   if (cmd == NULL) return FailAllocMessage();
   cmd_ptr = cmd;
   buf_ptr = buf;
   for (str_ptr=launch_attr->obj->detail.t->first; str_ptr != NULL;
         str_ptr=str_ptr->next) {
      struct AttrRec *attr_ptr;

      if (first_time) {
         first_time = FALSE;
         c_ptr = launch_attr->attr_value.s;
      } else {
         c_ptr = str_ptr->dyn_str.s;
      }
      for ( ; *c_ptr != '\0'; c_ptr++, count++) {
         switch (*c_ptr) {
         case '\\': c_ptr++; *buf_ptr++ = *c_ptr; break;
         case '$':
            if (count != 0) {
               *buf_ptr = '\0';
               if (count+cmd_len >= cur_size) {
                  n = cmd_ptr-cmd;
                  cur_size += MAXSTRING;
                  cmd = (char*)realloc(cmd, (cur_size+2)*sizeof(char));
                  cmd_ptr = cmd+n;
               }
               strcpy(cmd_ptr, buf);
               cmd_ptr += count;
               count = 0;
               buf_ptr = buf;
            }
            if ((attr_ptr=ValidAttrArg(c_ptr,obj_ptr,&new_c_ptr)) == NULL) {
               free(cmd);
               sprintf(gszMsgBox, "Invalid attribute specification: '%s'.",
                     c_ptr);
               MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
               return FALSE;
            }
            count = attr_ptr->attr_value.sz-1;
            if (count+cmd_len >= cur_size) {
               n = cmd_ptr-cmd;
               cur_size += MAXSTRING;
               cmd = (char*)realloc(cmd, (cur_size+2)*sizeof(char));
               cmd_ptr = cmd+n;
            }
            strcpy(cmd_ptr, attr_ptr->attr_value.s);
            cmd_ptr += count;
            count = -1;
            c_ptr = new_c_ptr;
            break;
         default: *buf_ptr++ = *c_ptr; break;
         }
      }
      if (count != 0) {
         *buf_ptr = '\0';
         if (count+cmd_len >= cur_size) {
            n = cmd_ptr-cmd;
            cur_size += MAXSTRING;
            cmd = (char*)realloc(cmd, (cur_size+2)*sizeof(char));
            cmd_ptr = cmd+n;
         }
         strcpy(cmd_ptr, buf);
         cmd_ptr += count;
         count = 0;
         buf_ptr = buf;
      }
      if (str_ptr->next != NULL) { *cmd_ptr++ = ' '; *cmd_ptr = '\0'; }
   }
   if (inHyperSpace && !allowLaunchInHyperSpace) {
      XFlush(mainDisplay);
      XSync(mainDisplay, False);
      sprintf(gszMsgBox, "%s.  %s:\n\n    %s\n\n(%s.)",
            "Launching is not automatic in hyperspace",
            "Do you want to execute to following command?", cmd,
            "If you are not sure about this, just click on \"NO\"");
      if (MsgBox(gszMsgBox, TOOL_NAME, YN_MB) != MB_ID_YES) {
         sprintf(gszMsgBox, "%s.",
               "Launching in hyperspace aborted at user's request");
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         return FALSE;
      }
   }
   SaveStatusStrings();
   LaunchIt(cmd);
   RestoreStatusStrings();
   free(cmd);
   return TRUE;
}

static
int BadCmd(cmd_name)
   char *cmd_name;
{
   sprintf(gszMsgBox, "Malformed '%s' command.\n\nCommand execution aborted.",
         cmd_name);
   MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   return FALSE;
}

static
int BadSelectedObj(cmd_name)
   char *cmd_name;
{
   sprintf(gszMsgBox, "%s '%s' %s.",
         "No object is selected when executing the", cmd_name, "command");
   MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   return FALSE;
}

static
int BadAttr(attr_name, cmd_name)
   char *attr_name, *cmd_name;
{
   char msg[MAXSTRING+1];

   sprintf(msg, "Can not find the '%s' %s '%s' command.",
         attr_name, "attribute while executing the", cmd_name);
   MsgBox(msg, TOOL_NAME, INFO_MB);
   return FALSE;
}

static
int FileAttrNotAllowed(attr_name, cmd_name)
   char *attr_name, *cmd_name;
{
   char msg[MAXSTRING+1];

   sprintf(msg, "File attribute '%s' %s '%s' command.",
         attr_name, "is not appropriate for the", cmd_name);
   MsgBox(msg, TOOL_NAME, INFO_MB);
   return FALSE;
}

static
int BadObjName(obj_name, cmd_name)
   char *obj_name, *cmd_name;
{
   sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
         "Can not find object named", obj_name, "while executing the",
         cmd_name, "command");
   MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   return FALSE;
}

static
int BadArg(arg_name, cmd_name)
   char *arg_name, *cmd_name;
{
   sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
         "Invalid", arg_name, "while executing the", cmd_name, "command");
   MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   return FALSE;
}

void ReplaceAttrFirstValue(obj_ptr, attr_ptr, new_value)
   struct ObjRec *obj_ptr;
   struct AttrRec *attr_ptr;
   char *new_value;
   /* obj_ptr better be a top-level object */
{
   int value_len, same=FALSE;

   value_len = strlen(new_value);
   if (value_len >= 2 && (*new_value == '\'' || *new_value == '"') &&
         new_value[value_len-1] == *new_value) {
      new_value[value_len-1] = '\0';
      if (strcmp(attr_ptr->attr_value.s, &new_value[1]) == 0) {
         same = TRUE;
      }
      new_value[value_len-1] = *new_value;
   } else {
      if (strcmp(attr_ptr->attr_value.s, new_value) == 0) {
         same = TRUE;
      }
   }
   if (!same) {
      int ltx, lty, rbx, rby, switch_selected=FALSE;
      struct SelRec *saved_top_sel=topSel, *saved_bot_sel=botSel;

      if (topSel == NULL || topSel != botSel || topSel->obj != obj_ptr) {
         switch_selected = TRUE;
         topSel = botSel = NULL;
         if (obj_ptr == tgifObj) AddObj(NULL, topObj, tgifObj);
         UpdSelBBox();
      }
      ltx = obj_ptr->bbox.ltx; lty = obj_ptr->bbox.lty;
      rbx = obj_ptr->bbox.rbx; rby = obj_ptr->bbox.rby;
      PrepareToReplaceAnObj(obj_ptr);
      if (value_len >= 2 && (*new_value == '\'' || *new_value == '"') &&
            new_value[value_len-1] == *new_value) {
         new_value[value_len-1] = '\0';
         DynStrSet(&attr_ptr->attr_value, &new_value[1]);
         new_value[value_len-1] = *new_value;
      } else {
         DynStrSet(&attr_ptr->attr_value, new_value);
      }
      UpdAttr(attr_ptr);

      attr_ptr->obj->detail.t->cached_zoom = 0;
      if (attr_ptr->obj->detail.t->cached_bitmap != None) {
         XFreePixmap(mainDisplay, attr_ptr->obj->detail.t->cached_bitmap);
         attr_ptr->obj->detail.t->cached_bitmap = None;
      }
      if (attr_ptr->shown) {
         AdjObjCache(obj_ptr);
         AdjObjBBox(obj_ptr);
      }
      if (obj_ptr == tgifObj) recordCmdIncludeTgifObj = TRUE;
      RecordReplaceAnObj(obj_ptr);
      if (obj_ptr == tgifObj) recordCmdIncludeTgifObj = FALSE;
      if (switch_selected) {
         RemoveAllSel();
         if (obj_ptr == tgifObj) UnlinkObj(topObj);
         topSel = saved_top_sel;
         botSel = saved_bot_sel;
         UpdSelBBox();
      }
      if (replaceAttrFirstValueRedraw && obj_ptr != tgifObj &&
            attr_ptr->shown) {
         RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
               rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
               obj_ptr->bbox.ltx-GRID_ABS_SIZE(1),
               obj_ptr->bbox.lty-GRID_ABS_SIZE(1),
               obj_ptr->bbox.rbx+GRID_ABS_SIZE(1),
               obj_ptr->bbox.rby+GRID_ABS_SIZE(1));
      }
      SetFileModified(TRUE);
   }
}

static
int CheckExecInterrupt(check_any_button, orig_cmd)
   int check_any_button;
   char *orig_cmd;
{
   XEvent ev;

   while (XCheckMaskEvent(mainDisplay, StructureNotifyMask, &ev)) {
      if (iconWindowShown) {
         if ((ev.xany.window == iconBaseWindow && ev.type == UnmapNotify) ||
               (ev.xany.window == mainWindow && ev.type == MapNotify)) {
            XPutBackEvent(mainDisplay, &ev);
            return TRUE;
         }
      } else if ((ev.xany.window == iconBaseWindow && ev.type == MapNotify) ||
            (ev.xany.window == mainWindow && ev.type == UnmapNotify)) {
         XPutBackEvent(mainDisplay, &ev);
         return TRUE;
      } else if (ev.type == ConfigureNotify) {
         Reconfigure(FALSE);
      }
   }
   while (XCheckMaskEvent(mainDisplay, VisibilityChangeMask, &ev)) {
      if (iconWindowShown) {
         if (ev.xany.window == mainWindow && ev.type == VisibilityNotify &&
               ev.xvisibility.state == VisibilityUnobscured) {
            XPutBackEvent(mainDisplay, &ev);
            return TRUE;
         } else {
            ExposeEventHandler(&ev, TRUE);
         }
      } else {
         if (ev.xany.window == iconBaseWindow && ev.type == VisibilityNotify &&
               ev.xvisibility.state == VisibilityUnobscured) {
            XPutBackEvent(mainDisplay, &ev);
            return TRUE;
         } else {
            ExposeEventHandler(&ev, TRUE);
         }
      }
   }
   if (XCheckMaskEvent(mainDisplay, ExposureMask, &ev)) {
      ExposeEventHandler(&ev, TRUE);
      while (XCheckMaskEvent(mainDisplay, ExposureMask, &ev)) ;
   }
   if (ESCPressed() || (check_any_button && XCheckMaskEvent(mainDisplay,
         ButtonPressMask | KeyPressMask, &ev)) || CheckInterrupt()) {
      if (orig_cmd == NULL) {
         sprintf(gszMsgBox, "User interrupt.");
      } else {
         sprintf(gszMsgBox, "%s():  User interrupt.", orig_cmd);
      }
      Msg(gszMsgBox);
      return TRUE;
   }
   if (!check_any_button) {
      while (XCheckMaskEvent(mainDisplay, ButtonPressMask|KeyPressMask, &ev)) ;
   }
   return FALSE;
}

static
int IntExpression(expr, p_ival, orig_cmd)
   char *expr, *orig_cmd;
   int *p_ival;
{
   struct VRec v;

   if (EvalExpr(expr, &v)) {
      switch (v.vtype) {
      case NULL_VAL: break;
      case INT_VAL: if (p_ival != NULL) *p_ival = v.val.i; return TRUE;
      case DBL_VAL: break;
      case STR_VAL: break;
      default: break;
      }
   }
   sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
         "Invalid evaluation", expr, "(integer expected) when executing the",
         orig_cmd, "command");
   MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
   return FALSE;
}

static
void SelectAnObj(sub_obj, owner_obj, top_owner, pp_saved_top_sel,
      pp_saved_bot_sel)
   struct ObjRec *sub_obj, *owner_obj, *top_owner;
   struct SelRec **pp_saved_top_sel, **pp_saved_bot_sel;
{
   if (pp_saved_top_sel != NULL && pp_saved_bot_sel != NULL) {
      *pp_saved_top_sel = topSel;
      *pp_saved_bot_sel = botSel;
   } else {
      if (topSel != NULL) RemoveAllSel();
   }
   topSel = botSel = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (topSel == NULL) FailAllocMessage();

   topSel->next = topSel->prev = NULL;
   topSel->obj = (owner_obj==NULL ? sub_obj : top_owner);
   UpdSelBBox ();
}

static
void RecursivelyAdjAnObjBBox(target_obj, owner_obj)
   struct ObjRec *target_obj, *owner_obj;
{
   if (target_obj == owner_obj) {
      AdjObjBBox(target_obj);
   } else {
      struct ObjRec *obj_ptr;

      switch (owner_obj->type) {
      case OBJ_GROUP:
      case OBJ_SYM:
      case OBJ_ICON:
         for (obj_ptr=owner_obj->detail.r->first; obj_ptr!=NULL;
               obj_ptr=obj_ptr->next) {
            RecursivelyAdjAnObjBBox(target_obj, obj_ptr);
         }
         AdjObjBBox(owner_obj);
         break;
      default: break;
      }
   }
}

static
void RecursivelyAdjObjBBox(sub_obj, owner_obj, top_owner)
   struct ObjRec *sub_obj, *owner_obj, *top_owner;
{
   if (owner_obj == NULL) {
      RecursivelyAdjAnObjBBox(sub_obj, sub_obj);
   } else {
      RecursivelyAdjAnObjBBox(sub_obj, top_owner);
   }
}

/* --------------------- Exec Routines --------------------- */

int ExecLaunch(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* launch(attribute_to_launch); */
{
   char *attr_name=argv[0];
   struct AttrRec *attr_ptr;

   UtilRemoveQuotes(attr_name);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, NULL);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   return (DoLaunch(attr_ptr, obj_ptr));
}

#ifdef _NO_EXTERN
extern int DoExec ARGS_DECL((struct AttrRec *exec_attr,
      struct ObjRec *obj_ptr));
#endif /* _NO_EXTERN */

int ExecExec(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* exec(attribute_to_exec); */
{
   char *attr_name=argv[0];
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;

   UtilRemoveQuotes(attr_name);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   return (DoExec(attr_ptr, attr_owner_obj));
}

int ExecMktemp(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* mktemp(tmp_file_template_string,result_attribute); */
{
   char *file_name=argv[0], *attr_name=argv[1];
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;
   int rc=TRUE;

   UtilRemoveQuotes(file_name);
   UtilRemoveQuotes(attr_name);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return (BadAttr (execDummyStr, orig_cmd));

   if (mktemp(file_name) == NULL) {
      MsgBox ("Fail to mktemp().  Command execution aborted.",
            TOOL_NAME, INFO_MB);
      rc = FALSE;
   } else {
      unlink(file_name);
      ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, file_name);
   }
   return rc;
}

int ExecUseTemplate(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* create_file_using_simple_template(template_file, */
   /*      output_file,string_to_match,substitution_attribute); */
{
   char *template_name=argv[0], *file_name=argv[1];
   char *match_str=argv[2], *attr_name=argv[3], msg[MAXSTRING+1];
   struct AttrRec *attr_ptr;
   FILE *in_fp=NULL, *out_fp=NULL;

   UtilRemoveQuotes(template_name);
   UtilRemoveQuotes(file_name);
   UtilRemoveQuotes(match_str);
   UtilRemoveQuotes(attr_name);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, NULL);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   if ((out_fp=fopen(file_name, "w")) != NULL &&
         (in_fp=fopen(template_name, "r")) != NULL) {
      int len_to_match=strlen(match_str), len, rc=TRUE;
      char tmp_buf[MAXSTRING+1];
      struct StrRec *str_ptr;

      writeFileFailed = FALSE;
      while (fgets(tmp_buf, MAXSTRING, in_fp) != NULL) {
         len = strlen (tmp_buf);
         if (tmp_buf[len-1] == '\n') {
            tmp_buf[--len] = '\0';
            if (len==len_to_match && strcmp(tmp_buf, match_str)==0) {
               if (*attr_ptr->attr_value.s != '\0') {
                  if (fprintf(out_fp, "%s\n", attr_ptr->attr_value.s) == EOF) {
                     writeFileFailed = TRUE;
                  }
               }
               if (attr_ptr->obj->detail.t->first != NULL) {
                  for (str_ptr = attr_ptr->obj->detail.t->first->next;
                        str_ptr != NULL; str_ptr = str_ptr->next) {
                     if (fprintf(out_fp,"%s\n", str_ptr->dyn_str.s) == EOF) {
                        writeFileFailed = TRUE;
                     }
                  }
               }
            } else if (fprintf(out_fp, "%s\n", tmp_buf) == EOF) {
               writeFileFailed = TRUE;
            }
         } else if (len==len_to_match && strcmp(tmp_buf, match_str)==0) {
            if (*attr_ptr->attr_value.s != '\0') {
               if (fprintf(out_fp, "%s\n", attr_ptr->attr_value.s) == EOF) {
                  writeFileFailed = TRUE;
               }
            }
            if (attr_ptr->obj->detail.t->first != NULL) {
               for (str_ptr = attr_ptr->obj->detail.t->first->next;
                     str_ptr != NULL; str_ptr = str_ptr->next) {
                  if (fprintf(out_fp, "%s\n", str_ptr->dyn_str.s) == EOF) {
                     writeFileFailed = TRUE;
                  }
               }
            }
         } else if (fprintf(out_fp, "%s\n", tmp_buf) == EOF) {
            writeFileFailed = TRUE;
         }
      }
      if (writeFileFailed) {
         sprintf(msg, "Fail to write to '%s'.\n\nFile system may be full.",
               file_name);
         MsgBox(msg, TOOL_NAME, INFO_MB);
         rc = FALSE;
      }
      fclose(in_fp);
      fclose(out_fp);
      return rc;
   } else if (out_fp == NULL) {
      sprintf(msg, "Can not open '%s' for write.", file_name);
      MsgBox(msg, TOOL_NAME, INFO_MB);
   } else if (in_fp == NULL) {
      sprintf(msg, "Can not open '%s' for read.", template_name);
      MsgBox(msg, TOOL_NAME, INFO_MB);
   }
   if (out_fp != NULL) fclose(out_fp);
   if (in_fp != NULL) fclose(in_fp);
   return FALSE;
}

#define UPDATE_EPS_CHILD 0
#define UPDATE_XBM_CHILD 1
#define UPDATE_XPM_CHILD 2

#define UPDATE_CHILD_NO_ALIGNMENT 0
#define UPDATE_CHILD_NORMAL_ALIGNMENT 1
#define UPDATE_CHILD_NO_OVERLAP_ALIGNMENT 2

static int updateChildUsingAlignment=INVALID;

static
int ExecUpdChild(argv, obj_ptr, orig_cmd, upd_type)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   int upd_type;
   /* update_eps_child(eps_file_name); */
   /* update_xbm_child(xbm_file_name); */
   /* update_xpm_child(xpm_file_name); */
{
   char			*file_name=argv[0];
   char			msg[MAXPATHLENGTH+1];
   char			**lines=NULL, write_date[32];
   struct ObjRec	*sub_obj_ptr=NULL, *del_obj=NULL, *tmp_obj;
   int			rc=BitmapOpenFailed;
   int			num_lines=0, epsf_level=0, image_w=0, image_h=0;
   int			del_obj_ltx=0, del_obj_lty=0, ltx=0, lty=0;
   int			rbx=0, rby=0, x_hot, y_hot, w=0, h=0, ncolors=0;
   int			chars_per_pixel=0, first_pixel_is_bg=0, *pixels=NULL;
   struct BBRec		parent_obbox;
   unsigned int		ui_image_w=0, ui_image_h=0;
   char			*color_char=NULL, **color_str=NULL, *xpm_data=NULL;
   float		llx=0, lly=0, urx=0, ury=0;
   Pixmap		bitmap=None, pixmap=None;
   XImage		*image=NULL, *bitmap_image=NULL;

   UtilRemoveQuotes(file_name);
   if (updateChildUsingAlignment == INVALID) {
      char *c_ptr;

      updateChildUsingAlignment = UPDATE_CHILD_NO_ALIGNMENT;
      if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME,
            "UpdateChildUsingAlignment")) != NULL) {
         if (UtilStrICmp(c_ptr, "true") == 0) {
            updateChildUsingAlignment = UPDATE_CHILD_NORMAL_ALIGNMENT;
         } else if (UtilStrICmp(c_ptr, "no_overlap") == 0) {
            updateChildUsingAlignment = UPDATE_CHILD_NO_OVERLAP_ALIGNMENT;
         }
      }
   }
   if (obj_ptr->type != OBJ_GROUP && obj_ptr->type != OBJ_ICON &&
         obj_ptr->type != OBJ_SYM) {
      sprintf(msg, "%s() only works with composite objects.", orig_cmd);
      MsgBox(msg, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   parent_obbox.ltx = obj_ptr->obbox.rbx;
   parent_obbox.lty = obj_ptr->obbox.rby;
   parent_obbox.rbx = obj_ptr->obbox.ltx;
   parent_obbox.rby = obj_ptr->obbox.lty;
   switch (upd_type) {
   case UPDATE_EPS_CHILD:
      for (tmp_obj=obj_ptr->detail.r->last; tmp_obj!=NULL;
            tmp_obj=tmp_obj->prev) {
         if (tmp_obj->type==OBJ_XBM &&
               tmp_obj->detail.xbm->real_type!=XBM_XBM) {
            del_obj_ltx = tmp_obj->obbox.ltx;
            del_obj_lty = tmp_obj->obbox.lty;
            del_obj = tmp_obj;
            if (!updateChildUsingAlignment) break;
         } else if (updateChildUsingAlignment) {
            if (tmp_obj->obbox.ltx < parent_obbox.ltx)
               parent_obbox.ltx = tmp_obj->obbox.ltx;
            if (tmp_obj->obbox.lty < parent_obbox.lty)
               parent_obbox.lty = tmp_obj->obbox.lty;
            if (tmp_obj->obbox.rbx > parent_obbox.rbx)
               parent_obbox.rbx = tmp_obj->obbox.rbx;
            if (tmp_obj->obbox.rby > parent_obbox.rby)
               parent_obbox.rby = tmp_obj->obbox.rby;
         }
      }
      break;
   case UPDATE_XBM_CHILD:
      for (tmp_obj=obj_ptr->detail.r->last; tmp_obj!=NULL;
            tmp_obj=tmp_obj->prev) {
         if (tmp_obj->type==OBJ_XBM &&
               tmp_obj->detail.xbm->real_type==XBM_XBM) {
            del_obj_ltx = tmp_obj->obbox.ltx;
            del_obj_lty = tmp_obj->obbox.lty;
            del_obj = tmp_obj;
            if (!updateChildUsingAlignment) break;
         } else if (updateChildUsingAlignment) {
            if (tmp_obj->obbox.ltx < parent_obbox.ltx)
               parent_obbox.ltx = tmp_obj->obbox.ltx;
            if (tmp_obj->obbox.lty < parent_obbox.lty)
               parent_obbox.lty = tmp_obj->obbox.lty;
            if (tmp_obj->obbox.rbx > parent_obbox.rbx)
               parent_obbox.rbx = tmp_obj->obbox.rbx;
            if (tmp_obj->obbox.rby > parent_obbox.rby)
               parent_obbox.rby = tmp_obj->obbox.rby;
         }
      }
      break;
   case UPDATE_XPM_CHILD:
      for (tmp_obj=obj_ptr->detail.r->last; tmp_obj!=NULL;
            tmp_obj=tmp_obj->prev) {
         if (tmp_obj->type==OBJ_XPM) {
            del_obj_ltx = tmp_obj->obbox.ltx;
            del_obj_lty = tmp_obj->obbox.lty;
            del_obj = tmp_obj;
            if (!updateChildUsingAlignment) break;
         } else if (updateChildUsingAlignment) {
            if (tmp_obj->obbox.ltx < parent_obbox.ltx)
               parent_obbox.ltx = tmp_obj->obbox.ltx;
            if (tmp_obj->obbox.lty < parent_obbox.lty)
               parent_obbox.lty = tmp_obj->obbox.lty;
            if (tmp_obj->obbox.rbx > parent_obbox.rbx)
               parent_obbox.rbx = tmp_obj->obbox.rbx;
            if (tmp_obj->obbox.rby > parent_obbox.rby)
               parent_obbox.rby = tmp_obj->obbox.rby;
         }
      }
      break;
   default: return BadCmd(orig_cmd);
   }
   if (del_obj == NULL) {
      del_obj_ltx = obj_ptr->obbox.ltx;
      del_obj_lty = obj_ptr->obbox.rby;
   }
   importingFile = TRUE;
   switch (upd_type) {
   case UPDATE_EPS_CHILD:
      rc = MyReadEPSFile(file_name, &image_w, &image_h, &bitmap,
            &image, &num_lines, &lines, &epsf_level, &llx, &lly, &urx, &ury,
            write_date);
      break;
   case UPDATE_XBM_CHILD:
      rc = XReadBitmapFile(mainDisplay, mainWindow, file_name,
            &ui_image_w, &ui_image_h, &bitmap, &x_hot, &y_hot);
      image_w = ui_image_w; image_h = ui_image_h;
      break;
   case UPDATE_XPM_CHILD:
      rc = MyReadPixmapFile(file_name, &image_w, &image_h, &w, &h,
            &pixmap, &image, &bitmap, &bitmap_image, &ncolors, &chars_per_pixel,
            &first_pixel_is_bg, &color_char, &color_str, &pixels, &xpm_data);
      break;
   }
   if (rc != BitmapSuccess) {
      importingFile = FALSE;
      sprintf(msg, "%s():  Fail to import '%s'.", orig_cmd, file_name);
      MsgBox(msg, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   ltx = obj_ptr->bbox.ltx; lty = obj_ptr->bbox.lty;
   rbx = obj_ptr->bbox.rbx; rby = obj_ptr->bbox.rby;

   PrepareToReplaceAnObj(obj_ptr);

   switch (upd_type) {
   case UPDATE_EPS_CHILD:
      saveEPSLines = TRUE;
      sub_obj_ptr = CreateEPSObj(file_name, image_w, image_h, bitmap,
            image, num_lines, lines, epsf_level, &llx, &lly, &urx, &ury,
            write_date);
      saveEPSLines = FALSE;
      if (strcmp(defaultEPSScalingStr,"1") != 0) {
         ScaleAnEPSObj(sub_obj_ptr, &defaultEPSScaling);
      }
      break;
   case UPDATE_XBM_CHILD:
      sub_obj_ptr = CreateXBmObj(image_w, image_h, image_w, image_h,
            bitmap, image);
      break;
   case UPDATE_XPM_CHILD:
      sub_obj_ptr = CreateXPmObj(image_w, image_h, w, h, pixmap, image, bitmap,
            bitmap_image, ncolors, chars_per_pixel, first_pixel_is_bg,
            color_char, color_str, pixels, xpm_data);
      break;
   }
   if (!PRTGIF && colorLayers && needToRedrawColorWindow) {
      RedrawColorWindow();
   }
   if (updateChildUsingAlignment != UPDATE_CHILD_NO_ALIGNMENT) {
      switch (horiAlign) {
      case ALIGN_N:
      case ALIGN_L:
         del_obj_ltx = parent_obbox.ltx;
         break;
      case ALIGN_C:
      case ALIGN_S:
         del_obj_ltx = ((parent_obbox.ltx+parent_obbox.rbx)>>1) -
               ((sub_obj_ptr->obbox.rbx-sub_obj_ptr->obbox.ltx)>>1);
         break;
      case ALIGN_R:
         del_obj_ltx = parent_obbox.rbx -
               (sub_obj_ptr->obbox.rbx-sub_obj_ptr->obbox.ltx);
         break;
      }
      switch (updateChildUsingAlignment) {
      case UPDATE_CHILD_NORMAL_ALIGNMENT:
         switch (vertAlign) {
         case ALIGN_T:
            del_obj_lty = parent_obbox.lty;
            break;
         case ALIGN_M:
         case ALIGN_S:
            del_obj_lty = ((parent_obbox.lty+parent_obbox.rby)>>1) -
                  ((sub_obj_ptr->obbox.rby-sub_obj_ptr->obbox.lty)>>1);
            break;
         case ALIGN_B:
            del_obj_lty = parent_obbox.rby -
                  (sub_obj_ptr->obbox.rby-sub_obj_ptr->obbox.lty);
            break;
         case ALIGN_N:
            del_obj_lty = parent_obbox.rby;
            break;
         }
         break;
      case UPDATE_CHILD_NO_OVERLAP_ALIGNMENT:
         switch (vertAlign) {
         case ALIGN_T:
            del_obj_lty = parent_obbox.lty -
                  (sub_obj_ptr->obbox.rby-sub_obj_ptr->obbox.lty);
            break;
         case ALIGN_M:
         case ALIGN_S:
            del_obj_lty = ((parent_obbox.lty+parent_obbox.rby)>>1) -
                  ((sub_obj_ptr->obbox.rby-sub_obj_ptr->obbox.lty)>>1);
            break;
         case ALIGN_B:
         case ALIGN_N:
            del_obj_lty = parent_obbox.rby;
            break;
         }
         break;
      }
   }
   MoveObj(sub_obj_ptr, del_obj_ltx-sub_obj_ptr->obbox.ltx,
         del_obj_lty-sub_obj_ptr->obbox.lty);
   if (del_obj != NULL) {
      if (del_obj == obj_ptr->detail.r->first) {
         obj_ptr->detail.r->first = del_obj->next;
      } else {
         del_obj->prev->next = del_obj->next;
      }
      if (del_obj == obj_ptr->detail.r->last) {
         obj_ptr->detail.r->last = del_obj->prev;
      } else {
         del_obj->next->prev = del_obj->prev;
      }
      FreeObj(del_obj);
   }
   sub_obj_ptr->prev = NULL;
   sub_obj_ptr->next = obj_ptr->detail.r->first;
   if (obj_ptr->detail.r->first == NULL) {
      obj_ptr->detail.r->last = sub_obj_ptr;
   } else {
      obj_ptr->detail.r->first->prev = sub_obj_ptr;
   }
   obj_ptr->detail.r->first = sub_obj_ptr;
   AdjObjBBox(obj_ptr);

   RecordReplaceAnObj(obj_ptr);
   RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
      rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
      obj_ptr->bbox.ltx-GRID_ABS_SIZE(1), obj_ptr->bbox.lty-GRID_ABS_SIZE(1),
      obj_ptr->bbox.rbx+GRID_ABS_SIZE(1), obj_ptr->bbox.rby+GRID_ABS_SIZE(1));
   SetFileModified(TRUE);

   importingFile = FALSE;

   return TRUE;
}

int ExecUpdEPSChild(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
{
   return ExecUpdChild(argv, obj_ptr, orig_cmd, UPDATE_EPS_CHILD);
}

int ExecUpdXBMChild(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
{
   return ExecUpdChild(argv, obj_ptr, orig_cmd, UPDATE_XBM_CHILD);
}

int ExecUpdXPMChild(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
{
   return ExecUpdChild(argv, obj_ptr, orig_cmd, UPDATE_XPM_CHILD);
}

static
long ms_time(tv)
   struct timeval *tv;
{
   return ((long)(tv->tv_usec / 1000.0)) + ((long)(tv->tv_sec * 1000));
}

#define FD_STYLE_LINEAR 0
#define FD_STYLE_PINGPONG 1

#define FD_FORWARD 0
#define FD_REVERSE 1

int ExecFlipDeck(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* flip_deck(times,frames_per_second,style); */
   /*		style=[linear | pingpong] */
{
   char			*times_str=argv[0];
   char			*fps_str=argv[1], *style_str=argv[2];
   int			num_bm, iteration, times, fps, style;
   int			rc=TRUE;
   struct ObjRec	*bm_obj, *cur_obj, *prev_obj, *next_obj;
   struct BBRec		obbox;
   int			select_width=XConnectionNumber(mainDisplay)+1;
#ifdef _NO_GETTIMEOFDAY
   struct timeb		start;
#else /* ~_NO_GETTIMEOFDAY */
   struct timeval	start;
   struct timezone	zone;
#endif /* _NO_GETTIMEOFDAY */
   long			ms_start_time, ms_frame_interval;
   fd_set		fdset;

   UtilRemoveQuotes(times_str);
   UtilRemoveQuotes(fps_str);
   UtilRemoveQuotes(style_str);
   if (obj_ptr->type != OBJ_GROUP && obj_ptr->type != OBJ_ICON &&
         obj_ptr->type != OBJ_SYM) {
      sprintf(execDummyStr, "%s() only works with composite objects.",
            orig_cmd);
      MsgBox(execDummyStr, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   num_bm = 0;
   for (bm_obj=obj_ptr->detail.r->last; bm_obj != NULL; bm_obj=bm_obj->prev) {
      if (bm_obj->type != OBJ_XBM && bm_obj->type != OBJ_XPM) {
         MsgBox ("flip_deck():  Object contains non-Bitmap/Pixmap sub-objects.",
               TOOL_NAME, INFO_MB);
         return (FALSE);
      } else if (num_bm++ == 0) {
         obbox.ltx = bm_obj->obbox.ltx; obbox.lty = bm_obj->obbox.lty;
         obbox.rbx = bm_obj->obbox.rbx; obbox.rby = bm_obj->obbox.rby;
      } else {
         if (obbox.ltx!=bm_obj->obbox.ltx || obbox.lty!=bm_obj->obbox.lty ||
               obbox.rbx!=bm_obj->obbox.rbx || obbox.rby!=bm_obj->obbox.rby) {
            MsgBox("flip_deck():  Different sizes Bitmap/Pixmap sub-objects.",
                  TOOL_NAME, INFO_MB);
            return FALSE;
         }
      }
   }
   if (num_bm < 2) {
      MsgBox("flip_deck():  Must have > 1 Bitmap/Pixmap sub-objects.",
            TOOL_NAME, INFO_MB);
      return FALSE;
   }
   if (!IntExpression(fps_str, &fps, orig_cmd)) {
      return FALSE;
   }
   if (fps < 1 || fps > 60) {
      MsgBox("flip_deck():  frames_per_second must be >= 1 and <= 60.",
            TOOL_NAME, INFO_MB);
      return FALSE;
   }
   ms_frame_interval = round(1000 / fps) - 1;
   if (strcmp(style_str, "linear") == 0) {
      style = FD_STYLE_LINEAR;
   } else if (strcmp(style_str, "ping_pong") == 0) {
      style = FD_STYLE_PINGPONG;
   } else {
      MsgBox("flip_deck():  undefined style.", TOOL_NAME, INFO_MB);
      return FALSE;
   }
   if (strcmp(times_str, "infinite") == 0) {
      times = (-1);
   } else {
      if (!IntExpression(times_str, &times, orig_cmd)) {
         return FALSE;
      }
   }
   iteration = 0;
#ifdef _NO_GETTIMEOFDAY
   ftime(&start);
   ms_start_time = ((long)start.time * 1000) + ((long)start.millitm);
#else /* ~_NO_GETTIMEOFDAY */
   gettimeofday(&start, &zone);
   ms_start_time = ms_time(&start);
#endif /* _NO_GETTIMEOFDAY */
   while (rc == TRUE && (times < 0 || iteration < times)) {
      int looping=TRUE, direction=FD_FORWARD;

      if (times >= 0) iteration++;
      cur_obj = obj_ptr->detail.r->first;
      while (looping) {
         struct timeval timeout;
#ifdef _NO_GETTIMEOFDAY
         struct timeb now;
#else /* ~_NO_GETTIMEOFDAY */
         struct timeval now;
#endif /* _NO_GETTIMEOFDAY */
         long ms_cur_time, ms_interval;
         int status;

         prev_obj = cur_obj->prev;
         next_obj = cur_obj->next;
         cur_obj->prev = cur_obj->next = NULL;
         switch (cur_obj->type) {
         case OBJ_XBM:
            if (iconWindowShown) {
               DrawXBmObj(iconWindow, 0, 0, cur_obj);
            } else {
               DrawXBmObj(drawWindow, drawOrigX, drawOrigY, cur_obj);
            }
            break;
         case OBJ_XPM:
            if (iconWindowShown) {
               DrawXPmObj(iconWindow, 0, 0, cur_obj);
            } else {
               DrawXPmObj(drawWindow, drawOrigX, drawOrigY, cur_obj);
            }
            break;
         }
         XSync(mainDisplay, FALSE);
         cur_obj->prev = prev_obj;
         cur_obj->next = next_obj;
         switch (style) {
         case FD_STYLE_LINEAR:
            if ((cur_obj=cur_obj->next) == NULL)
               looping = FALSE;
            break;
         case FD_STYLE_PINGPONG:
            switch (direction) {
            case FD_FORWARD:
               if (cur_obj->next == NULL) {
                  if ((cur_obj=cur_obj->prev) == obj_ptr->detail.r->first) {
                     looping = FALSE;
                  } else {
                     direction = FD_REVERSE;
                  }
               } else {
                  cur_obj = cur_obj->next;
               }
               break;
            case FD_REVERSE:
               if ((cur_obj=cur_obj->prev) == obj_ptr->detail.r->first) {
                  looping = FALSE;
               }
               break;
            }
            break;
         }
         do
         {
            FD_ZERO(&fdset);
            FD_SET(select_width-1, &fdset);

#ifdef _NO_GETTIMEOFDAY
            ftime(&now);
            ms_cur_time = ((long)now.time * 1000) + ((long)now.millitm);
#else /* ~_NO_GETTIMEOFDAY */
            gettimeofday(&now, &zone);
            ms_cur_time = ms_time(&now);
#endif /* _NO_GETTIMEOFDAY */
            ms_interval = ms_start_time + ms_frame_interval - ms_cur_time;
            while (ms_interval <= 0) {
               ms_start_time = ms_cur_time;
               ms_interval = ms_start_time + ms_frame_interval - ms_cur_time;
            }
            timeout.tv_sec = 0;
            timeout.tv_usec = 1000 * ms_interval;

#ifdef __hpux
            status = select(select_width, (int*)&fdset, NULL, NULL, &timeout);
#else /* !__hpux */
            status = select(select_width, &fdset, NULL, NULL, &timeout);
#endif /* __hpux */

            if (status < 0)
            {
               Msg ("flip_deck():  select() system call failed.");
               looping = FALSE;
               rc = FALSE;
               break;
            } else if (status == 0) {
               break;
            } else {
               if (CheckExecInterrupt(FALSE, orig_cmd)) {
                  looping = FALSE;
                  rc = FALSE;
                  break;
               }
            }
         } while (looping && ms_interval > 0);
      }
   }

/* RedrawAnArea (botObj, obj_ptr->bbox.ltx-GRID_ABS_SIZE(1),
      obj_ptr->bbox.lty-GRID_ABS_SIZE(1), obj_ptr->bbox.rbx+GRID_ABS_SIZE(1),
      obj_ptr->bbox.rby+GRID_ABS_SIZE(1)); */

   cur_obj = obj_ptr->detail.r->first;
   prev_obj = cur_obj->prev;
   next_obj = cur_obj->next;
   cur_obj->prev = cur_obj->next = NULL;
   switch (cur_obj->type) {
   case OBJ_XBM:
      if (iconWindowShown) {
         DrawXBmObj(iconWindow, 0, 0, cur_obj);
      } else {
         DrawXBmObj(drawWindow, drawOrigX, drawOrigY, cur_obj);
      }
      break;
   case OBJ_XPM:
      if (iconWindowShown) {
         DrawXPmObj(iconWindow, 0, 0, cur_obj);
      } else {
         DrawXPmObj(drawWindow, drawOrigX, drawOrigY, cur_obj);
      }
      break;
   }
   XSync(mainDisplay, FALSE);
   cur_obj->prev = prev_obj;
   cur_obj->next = next_obj;

   return rc;
}

static
int JustReadFileIntoAttr(attr_ptr, attr_owner_obj, file_name, orig_cmd)
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj;
   char *file_name, *orig_cmd;
{
   int ltx, lty, rbx, rby, saved_color_index, x, y;
   struct ObjRec *text_obj_ptr;
   FILE *fp;

   if (*file_name == '|') {
      sprintf(gszMsgBox, "Executing '%s'...", &file_name[1]);
      SetStringStatus(gszMsgBox);
      XSync(mainDisplay, False);
      fp = (FILE*)popen(&file_name[1], "r");
   } else {
      fp = fopen(file_name, "r");
   }
   if (fp == NULL) {
      sprintf(gszMsgBox, "Fail to open '%s' for read %s '%s' command.",
            file_name, "when executing the", orig_cmd);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   ltx = attr_owner_obj->bbox.ltx; lty = attr_owner_obj->bbox.lty;
   rbx = attr_owner_obj->bbox.rbx; rby = attr_owner_obj->bbox.rby;

   PrepareToReplaceAnObj(attr_owner_obj);

   saved_color_index = colorIndex;

   colorIndex = attr_ptr->obj->color;
   SaveCurFont();

   curFont = attr_ptr->obj->detail.t->font;
   curStyle = attr_ptr->obj->detail.t->style;
   curSize = attr_ptr->obj->detail.t->size;
   textJust = attr_ptr->obj->detail.t->just;
   textVSpace = attr_ptr->obj->detail.t->v_space;
   curRotate = attr_ptr->obj->detail.t->rotate;
   penPat = attr_ptr->obj->detail.t->pen;
   objFill = attr_ptr->obj->detail.t->fill;

   SetCanvasFont();

   x = attr_ptr->obj->x;
   y = attr_ptr->obj->y;
   FreeTextObj(attr_ptr->obj);
   attr_ptr->obj = NULL;
   text_obj_ptr = FormTextObjectFromFile(fp, x, y);

   RestoreCurFont();

   colorIndex = saved_color_index;

   if (fp != NULL) {
      if (*file_name == '|') {
         pclose(fp);
         SetStringStatus("...Done");
      } else {
         fclose(fp);
      }
   }
   if (text_obj_ptr == NULL) {
      sprintf(gszMsgBox, "Unexpected error %s '%s' command.\n\n%s!",
            "when executing the", orig_cmd,
            "Command execution aborted");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      DynStrSet(&attr_ptr->attr_value, "");
   } else {
      if (text_obj_ptr->detail.t->first != NULL) {
         DynStrCpy(&attr_ptr->attr_value,
               &text_obj_ptr->detail.t->first->dyn_str);
      } else {
         DynStrSet(&attr_ptr->attr_value, "");
      }
   }
   attr_ptr->obj = text_obj_ptr;
   text_obj_ptr->detail.t->attr = attr_ptr;
   UpdAttr(attr_ptr);
   if (attr_ptr->shown) {
      AdjObjCache(attr_owner_obj);
      AdjObjBBox(attr_owner_obj);
   }
   RecordReplaceAnObj(attr_owner_obj);
   RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
         attr_owner_obj->bbox.ltx-GRID_ABS_SIZE(1),
         attr_owner_obj->bbox.lty-GRID_ABS_SIZE(1),
         attr_owner_obj->bbox.rbx+GRID_ABS_SIZE(1),
         attr_owner_obj->bbox.rby+GRID_ABS_SIZE(1));
   SetFileModified(TRUE);

   return (text_obj_ptr != NULL);
}

int ExecReadFileIntoAttr(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* read_file_into_attr(file_name,result_attribute); */
{
   char *file_name=argv[0], *attr_name=argv[1];
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj;
   int rc;

   UtilRemoveQuotes(file_name);
   UtilRemoveQuotes(attr_name);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   SaveStatusStrings();
   rc = JustReadFileIntoAttr(attr_ptr, attr_owner_obj, file_name, orig_cmd);
   RestoreStatusStrings();
   return rc;
}

static
int ExecWriteOrAppendAttrIntoFile(argv, obj_ptr, orig_cmd, open_mode)
   char **argv, *orig_cmd, *open_mode;
   struct ObjRec *obj_ptr;
   /* write_attr_into_file(attribute,file_name); */
   /* append_attr_into_file(attribute,file_name); */
{
   char *attr_name=argv[0], *file_name=argv[1];
   struct AttrRec *attr_ptr;
   struct StrRec *str_ptr;
   FILE *fp;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(file_name);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, NULL);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   if ((fp=fopen(file_name, open_mode)) == NULL) {
      sprintf(execDummyStr, "Fail to open '%s' while executing %s().",
            file_name, orig_cmd);
      MsgBox(execDummyStr, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   fprintf(fp, "%s\n", attr_ptr->attr_value.s);
   if (attr_ptr->obj->detail.t->first != NULL) {
      for (str_ptr=attr_ptr->obj->detail.t->first->next; str_ptr != NULL;
            str_ptr = str_ptr->next) {
         fprintf(fp, "%s\n", str_ptr->dyn_str.s);
      }
   }
   fclose(fp);

   return TRUE;
}

int ExecWriteAttrIntoFile(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* write_attr_into_file(attribute,file_name); */
{
   return ExecWriteOrAppendAttrIntoFile(argv, obj_ptr, orig_cmd, "w");
}

int ExecAppendAttrIntoFile(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* append_attr_into_file(attribute,file_name); */
{
   return ExecWriteOrAppendAttrIntoFile(argv, obj_ptr, orig_cmd, "a");
}

int ExecSelectObjByName(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* select_obj_by_name(obj_name); */
{
   char *obj_name=argv[0];
   struct ObjRec *owner_obj=NULL, *named_obj;

   UtilRemoveQuotes(obj_name);
   named_obj = FindObjWithName(botObj, obj_ptr, obj_name, FALSE,
         FALSE, &owner_obj, NULL);
   if (topSel != NULL) RemoveAllSel();
   if (named_obj == NULL) {
      sprintf(execDummyStr, "%s '%s' %s '%s' %s.",
            "Can not find object named", obj_name, "while executing the",
            orig_cmd, "command");
      MsgBox(execDummyStr, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   if (owner_obj != NULL) {
      sprintf(execDummyStr, "%s: '%s'.\n\n%s '%s' %s.",
            "Bad object name", obj_name,
            "Only top level object can be selected when executing the",
            orig_cmd, "command");
      MsgBox(execDummyStr, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   AddNewSelObj(named_obj);
   UpdSelBBox();

   return TRUE;
}

void ExecUnSelectAllObj()
   /* unselect_all_obj(); */
{
   RemoveAllSel();
}

int ExecMoveSelObjRel(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* move_selected_obj_relative(dx,dy); */
{
   char *dx_str=argv[0], *dy_str=argv[1];
   int dx, dy;

   if (topSel == NULL) return BadSelectedObj(orig_cmd);

   UtilRemoveQuotes(dx_str);
   UtilRemoveQuotes(dy_str);
   if (!IntExpression(dx_str, &dx, orig_cmd) ||
         !IntExpression(dy_str, &dy, orig_cmd)) {
      return FALSE;
   }
   if (dx != 0 || dy != 0) {
      MoveAllSel(dx, dy);
      UpdSelBBox();
      SetFileModified(TRUE);
   }
   return TRUE;
}

int ExecRepeat(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* repeat(attr_name,times); */
{
   char *attr_name=argv[0], *times_str=argv[1];
   int i, times=(-1), rc=TRUE;
   struct AttrRec *attr_ptr;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(times_str);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, NULL);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   if (strcmp(times_str, "infinite") != 0) {
      if (!IntExpression(times_str, &times, orig_cmd)) {
         return FALSE;
      }
   }
   for (i=0; times < 0 || i < times; i++) {
      rc = DoExec(attr_ptr, obj_ptr);
      if (!rc) break;
      if (times < 0) i--;
   }
   return rc;
}

int ExecHyperJump(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* hyperjump(attr_name); */
{
   char *attr_name=argv[0];
   struct AttrRec *attr_ptr;

   UtilRemoveQuotes(attr_name);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, NULL);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   warpToAttr = attr_ptr;
   return TRUE;
}

static char *CGICharMap[] = {
   "                                ",
   "+         *  -. 0123456789      ",
   "@ABCDEFGHIJKLMNOPQRSTUVWXYZ    _",
   " abcdefghijklmnopqrstuvwxyz     ",
   NULL
};

static
int MapCGIChars(buf, buf_sz, orig_str)
   char *buf, *orig_str;
   int buf_sz;
{
   int count=0;

   for ( ; *orig_str != '\0'; orig_str++) {
      unsigned char orig_code=(unsigned char)(*orig_str);
      char code;

      if (orig_code < 0x20) {
         code = ' ';
      } else if (orig_code < 0x40) {
         code = CGICharMap[1][orig_code-0x20];
      } else if (orig_code < 0x60) {
         code = CGICharMap[2][orig_code-0x40];
      } else if (orig_code < 0x80) {
         code = CGICharMap[3][orig_code-0x60];
      } else {
         code = ' ';
      }
      if (code == ' ') {
         if (count+3 >= buf_sz) break;
         sprintf(&buf[count], "%%%02X", (int)orig_code);
         count += 3;
      } else {
         if (count+1 >= buf_sz) break;
         buf[count++] = code;
      }
   }
   buf[count] = '\0';
   return count;
}

int ExecMakeCGIQuery(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* make_cgi_query(dest_attr_name,url_name,list_attr_name); */
{
   char *dest_attr_name=argv[0], *url_name=argv[1], *list_attr_name=argv[2];
   struct AttrRec *dest_attr, *list_attr;
   struct ObjRec *dest_attr_owner_obj=NULL;
   struct StrRec *str_ptr;
   char *buf=NULL, *c_ptr, *tmp_buf=NULL;
   int buf_sz, buf_len, tmp_sz, num_attrs;
   int found_name, seen_first_attr=FALSE;

   UtilRemoveQuotes(dest_attr_name);
   UtilRemoveQuotes(url_name);
   UtilRemoveQuotes(list_attr_name);

   buf_sz = strlen(url_name);
   buf = (char*)malloc((buf_sz+1)*sizeof(char));
   if (buf == NULL) return FailAllocMessage();

   sprintf(buf, "%s", url_name);
   buf_len = buf_sz;

   sprintf(execDummyStr, "%s=", dest_attr_name);
   dest_attr = FindAttrWithName(obj_ptr, execDummyStr, &dest_attr_owner_obj);
   if (dest_attr == NULL) return BadAttr(execDummyStr, orig_cmd);

   sprintf(execDummyStr, "%s=", list_attr_name);
   list_attr = FindAttrWithName(obj_ptr, execDummyStr, NULL);
   if (list_attr == NULL) return BadAttr(execDummyStr, orig_cmd);

   tmp_sz = 0;
   for (str_ptr=list_attr->obj->detail.t->first; str_ptr != NULL;
         str_ptr=str_ptr->next) {
      tmp_sz += strlen(str_ptr->dyn_str.s)+1;
   }
   tmp_buf = (char*)malloc((tmp_sz+2)*sizeof(char));
   if (tmp_buf == NULL) {
      free(buf);
      return FailAllocMessage();
   }
   tmp_sz = 0;
   found_name = FALSE;
   for (str_ptr=list_attr->obj->detail.t->first; str_ptr != NULL;
         str_ptr=str_ptr->next) {
      if (found_name) {
         strcpy(&tmp_buf[tmp_sz], str_ptr->dyn_str.s);
         tmp_sz += strlen(str_ptr->dyn_str.s);
      } else if ((c_ptr=strchr(str_ptr->dyn_str.s, '=')) != NULL) {
         found_name = TRUE;
         strcpy(&tmp_buf[tmp_sz], ++c_ptr);
         tmp_sz += strlen(c_ptr);
      } else {
         continue;
      }
      tmp_buf[tmp_sz++] = '\n';
   }
   tmp_buf[tmp_sz] = '\0';
   for (num_attrs=0, c_ptr=strtok(tmp_buf, " ,\t\n\r"); c_ptr != NULL;
         num_attrs++, c_ptr=strtok(NULL, " ,\t\n\r")) {
      struct AttrRec *attr_ptr;
      int len, buf1_index;
      char *buf1=NULL;

      sprintf(execDummyStr, "%s=", c_ptr);
      attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, NULL);
      if (attr_ptr == NULL) {
         free(buf);
         free(tmp_buf);
         return BadAttr(execDummyStr, orig_cmd);
      }
      len = strlen(execDummyStr);
      for (str_ptr=attr_ptr->obj->detail.t->first; str_ptr != NULL;
            str_ptr=str_ptr->next) {
         if (str_ptr == attr_ptr->obj->detail.t->first) {
            len += strlen(attr_ptr->attr_value.s)+1;
         } else {
            len += strlen(str_ptr->dyn_str.s)+1;
         }
      }
      len = len*3;
      buf1 = (char*)malloc((len+1)*sizeof(char));
      if (buf1 == NULL) {
         free(buf);
         free(tmp_buf);
         return FailAllocMessage();
      }
      if (seen_first_attr) {
         *buf1 = '&';
      } else {
         *buf1 = '?';
         seen_first_attr = TRUE;
      }
      buf1_index = 1+MapCGIChars(&buf1[1], len-1, c_ptr);
      buf1[buf1_index++] = '=';
      for (str_ptr=attr_ptr->obj->detail.t->first; str_ptr != NULL;
            str_ptr=str_ptr->next) {
         if (str_ptr == attr_ptr->obj->detail.t->first) {
            buf1_index += MapCGIChars(&buf1[buf1_index], len-buf1_index,
                  attr_ptr->attr_value.s);
         } else {
            buf1_index += MapCGIChars(&buf1[buf1_index], len-buf1_index,
                  str_ptr->dyn_str.s);
         }
         if (str_ptr->next != NULL && buf1_index < len-1) {
            buf1_index += MapCGIChars(&buf1[buf1_index], len-buf1_index, "\n");
         }
      }
      buf1[buf1_index] = '\0';
      if ((buf=(char*)realloc(buf, buf1_index+buf_sz+1)) == NULL) {
         free(buf);
         free(tmp_buf);
         free(buf1);
         return FailAllocMessage();
      }
      strcpy(&buf[buf_sz], buf1);
      buf_sz += buf1_index;
      free(buf1);
   }
   ReplaceAttrFirstValue(dest_attr_owner_obj, dest_attr, buf);
   free(buf);
   free(tmp_buf);
   return TRUE;
}

int ExecWaitClick(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* wait_click(cursor_name,grab,attr_name); */
{
   char *cursor_name=argv[0], *grab_str=argv[1], *attr_name=argv[2];
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;
   Cursor cursor=(Cursor)0;
   int grab;

   UtilRemoveQuotes(cursor_name);
   UtilRemoveQuotes(grab_str);
   UtilRemoveQuotes(attr_name);
   grab = (strcmp(grab_str, "TRUE") == 0);
   if (strcmp(cursor_name, "NULL") != 0) {
      cursor = NewFontCursor(cursor_name);
      if (cursor == (Cursor)0) {
         sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
               "Can not create the", cursor_name, "cursor while executing the",
               orig_cmd, "command");
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         return FALSE;
      }
   }
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) {
      if (cursor != (Cursor)0) DeleteFontCursor(cursor);
      return BadAttr(execDummyStr, orig_cmd);
   }
   if (cursor != (Cursor)0) {
      SetWindowCursor(drawWindow, cursor);
      SetWindowCursor(mainWindow, cursor);
   }
   if (grab) {
      XGrabPointer(mainDisplay, rootWindow, False, ButtonPressMask,
            GrabModeAsync, GrabModeAsync, None,
            cursor==(Cursor)0 ? defaultCursor : cursor, CurrentTime);
   }
   for (;;) {
      XEvent input;

      XNextEvent(mainDisplay, &input);
      if (input.type == Expose || input.type == VisibilityNotify) {
         ExposeEventHandler(&input, TRUE);
      } else if (input.type == ButtonPress) {
         char buf[80];
         int button_num=0;

         if (grab) {
            XUngrabPointer(mainDisplay, CurrentTime);
         }
         switch (input.xbutton.button) {
         case Button1: button_num = 1; break;
         case Button2: button_num = 2; break;
         case Button3: button_num = 3; break;
         default: button_num = (input.xbutton.button-Button1)+1; break;
         }
         sprintf(buf, "%1d", button_num);
         ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, buf);
         break;
      }
   }
   if (cursor != (Cursor)0) {
      SetDefaultCursor(mainWindow);
      ShowCursor();
      DeleteFontCursor(cursor);
   }
   return TRUE;
}

int ExecSleep(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* sleep(cursor_name,ms_interval); */
{
   char *cursor_name=argv[0], *ms_interval_str=argv[1];
   Cursor cursor=(Cursor)0;
   int select_width=XConnectionNumber(mainDisplay)+1, rc=TRUE, val;
   long ms_start_time, ms_interval, ms_togo;
   struct timeval timeout;
#ifdef _NO_GETTIMEOFDAY
   struct timeb now;
#else /* ~_NO_GETTIMEOFDAY */
   struct timeval now;
   struct timezone zone;
#endif /* _NO_GETTIMEOFDAY */
   fd_set fdset;

   UtilRemoveQuotes(cursor_name);
   UtilRemoveQuotes(ms_interval_str);
   if (!IntExpression(ms_interval_str, &val, orig_cmd)) {
      return FALSE;
   }
   ms_interval = (long)val;
   if (strcmp(cursor_name, "NULL") != 0) {
      cursor = NewFontCursor(cursor_name);
      if (cursor == (Cursor)0) {
         sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
               "Can not create the", cursor_name, "cursor while executing the",
               orig_cmd, "command");
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         return FALSE;
      }
   }
   if (cursor != (Cursor)0) {
      SetWindowCursor(drawWindow, cursor);
      SetWindowCursor(mainWindow, cursor);
      XSync(mainDisplay, False);
   }
#ifdef _NO_GETTIMEOFDAY
   ftime(&now);
   ms_start_time = ((long)now.time * 1000) + ((long)now.millitm);
#else /* ~_NO_GETTIMEOFDAY */
   gettimeofday(&now, &zone);
   ms_start_time = ms_time(&now);
#endif /* _NO_GETTIMEOFDAY */
   ms_togo = ms_interval;
   while (rc && ms_togo > 0) {
      int status;
      long ms_cur_time;

      FD_ZERO(&fdset);
      FD_SET(select_width-1, &fdset);
      timeout.tv_sec = (long)(ms_togo/1000);
      timeout.tv_usec = 1000 * (ms_togo % 1000);
#ifdef __hpux
      status = select(select_width, (int*)&fdset, NULL, NULL, &timeout);
#else /* !__hpux */
      status = select(select_width, &fdset, NULL, NULL, &timeout);
#endif /* __hpux */
      if (status < 0) {
         sprintf(gszMsgBox, "%s():  select() system call failed.", orig_cmd);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         rc = FALSE;
      } else if (status == 0) {
         /* good timeout */
         break;
      } else {
         rc = (!CheckExecInterrupt(TRUE, orig_cmd));
      }
      if (rc) {
#ifdef _NO_GETTIMEOFDAY
         ftime(&now);
         ms_cur_time = ((long)now.time * 1000) + ((long)now.millitm);
#else /* ~_NO_GETTIMEOFDAY */
         gettimeofday(&now, &zone);
         ms_cur_time = ms_time(&now);
#endif /* _NO_GETTIMEOFDAY */
         ms_togo = ms_start_time+ms_interval-ms_cur_time;
      }
   }

   if (cursor != (Cursor)0) {
      SetDefaultCursor(mainWindow);
      ShowCursor();
      DeleteFontCursor(cursor);
   }
   return rc;
}

void ExecBeginAnimate()
   /* begin_animate(); */
{
   if (!BeginExecAnimate()) {
      gnAbortExec = TRUE;
   } else {
      ShowInterrupt(1);
   }
}

void ExecEndAnimate()
   /* end_animate(); */
{
   EndExecAnimate();
   while (HideInterrupt() > 0) ;
}

int ExecSetRedraw(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* set_redraw(true_or_false); */
{
   char *redraw_str=argv[0];

   UtilRemoveQuotes(redraw_str);
   execAnimateRedraw = (strcmp(redraw_str, "TRUE") == 0);
   return TRUE;
}

int ExecSetSelObjColor(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* set_selected_obj_color(color_str); */
{
   char *color_str=argv[0];
   int index, new_alloc;

   UtilRemoveQuotes(color_str);
   if (topSel == NULL) return BadSelectedObj(orig_cmd);

   if ((index=QuickFindColorIndex(NULL, color_str, &new_alloc, FALSE)) ==
         INVALID) {
      sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
            "Fail to allocate the", color_str, "color when executing the",
            orig_cmd, "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   if (obj_ptr != NULL) {
      UtilStrCpy(obj_ptr->color_str, sizeof(obj_ptr->color_str),
            colorMenuItems[index]);
   }
   ChangeAllSelColor(index, FALSE);
   return TRUE;
}

int ExecSetSelObjFill(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* set_selected_obj_fill(fill_index); */
{
   char *index_str=argv[0];
   int index;

   UtilRemoveQuotes(index_str);
   if (topSel == NULL) return BadSelectedObj(orig_cmd);

   if (!IntExpression(index_str, &index, orig_cmd)) {
      return FALSE;
   }
   if (index < 0 || index >= MAXPATTERNS) {
      sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
            "Fill index of", index_str, "is out of range when executing the",
            orig_cmd, "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   ChangeAllSelFill(index, FALSE);
   return TRUE;
}

int ExecSetSelObjPen(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* set_selected_obj_pen(pen_index); */
{
   char *index_str=argv[0];
   int index;

   UtilRemoveQuotes(index_str);
   if (topSel == NULL) return BadSelectedObj(orig_cmd);

   if (!IntExpression(index_str, &index, orig_cmd)) {
      return FALSE;
   }
   if (index < 0 || index >= MAXPATTERNS) {
      sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
            "Pen index of", index_str, "is out of range when executing the",
            orig_cmd, "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   ChangeAllSelPen(index, FALSE);
   return TRUE;
}

int ExecSetSelObjLineWidth(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* set_selected_obj_line_width(width,aw,ah); */
{
   char *width_str=argv[0], *aw_str=argv[1], *ah_str=argv[2];
   char width_spec[40], aw_spec[40], ah_spec[40];
   int width, aw, ah;

   UtilRemoveQuotes(width_str);
   UtilRemoveQuotes(aw_str);
   UtilRemoveQuotes(ah_str);
   if (topSel == NULL) return BadSelectedObj(orig_cmd);

   if (!IntExpression(width_str, &width, orig_cmd) ||
         !IntExpression(aw_str, &aw, orig_cmd) ||
         !IntExpression(ah_str, &ah, orig_cmd)) {
      return FALSE;
   }
   if (width < 0) {
      sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
            "Line width of", width_str, "is out of range when executing the",
            orig_cmd, "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   sprintf(width_spec, "%1d", width);
   sprintf(aw_spec, "%1d", aw);
   sprintf(ah_spec, "%1d", ah);
   ChangeAllSelRealLineWidth(width, aw, ah, width_spec, aw_spec, ah_spec,
         FALSE);
   return TRUE;
}

int ExecSetSelObjSpline(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* set_selected_obj_spline(spline_type); */
{
   char *spline_spec=argv[0];
   int index;

   UtilRemoveQuotes(spline_spec);
   if (topSel == NULL) return BadSelectedObj(orig_cmd);

   if (strcmp(spline_spec,"straight") == 0) {
      index = LT_STRAIGHT;
   } else if (strcmp(spline_spec,"spline") == 0) {
      index = LT_SPLINE;
   } else if (strcmp(spline_spec,"interpolated") == 0) {
      index = LT_INTSPLINE;
   } else {
      sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
            "Spline type of", spline_spec, "is unknown when executing the",
            orig_cmd, "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   ChangeAllSelLineType(index, FALSE);
   return TRUE;
}

int ExecSetSelObjArrow(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* set_selected_obj_arrow(arrow_type); */
{
   char *arrow_spec=argv[0];
   int index;

   UtilRemoveQuotes(arrow_spec);
   if (topSel == NULL) return BadSelectedObj(orig_cmd);

   if (strcmp(arrow_spec,"none") == 0) {
      index = LS_PLAIN;
   } else if (strcmp(arrow_spec,"right") == 0) {
      index = LS_RIGHT;
   } else if (strcmp(arrow_spec,"left") == 0) {
      index = LS_LEFT;
   } else if (strcmp(arrow_spec,"double") == 0) {
      index = LS_DOUBLE;
   } else {
      sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
            "Arrow style of", arrow_spec, "is unknown when executing the",
            orig_cmd, "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   ChangeAllSelLineStyle(index, FALSE);
   return TRUE;
}

int ExecSetSelObjDash(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* set_selected_obj_dash(dash_index); */
{
   char *index_str=argv[0];
   int index;

   UtilRemoveQuotes(index_str);
   if (topSel == NULL) return BadSelectedObj(orig_cmd);

   if (!IntExpression(index_str, &index, orig_cmd)) {
      return FALSE;
   }
   if (index < 0 || index >= MAXDASHES) {
      sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
            "Dash index of", index_str, "is out of range when executing the",
            orig_cmd, "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   ChangeAllSelDashes(index, FALSE);
   return TRUE;
}

int ExecInc(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* inc(attr_name,expr); */
{
   char *attr_name=argv[0], *expr=argv[1], buf[40];
   int orig_ival, val;
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(expr);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   orig_ival = atoi(attr_ptr->attr_value.s);
   if (!IntExpression(expr, &val, orig_cmd)) return FALSE;
   if (val != 0) {
      sprintf(buf, "%1d", orig_ival+val);
      ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, buf);
   }
   return TRUE;
}

int ExecDec(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* dec(attr_name,expr); */
{
   char *attr_name=argv[0], *expr=argv[1], buf[40];
   int orig_ival, val;
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(expr);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   orig_ival = atoi(attr_ptr->attr_value.s);
   if (!IntExpression(expr, &val, orig_cmd)) return FALSE;
   if (val != 0) {
      sprintf(buf, "%1d", orig_ival-val);
      ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, buf);
   }
   return TRUE;
}

int ExecShuffleObjToTop(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* shuffle_obj_to_top(obj_name); */
{
   char *obj_name=argv[0];
   struct ObjRec *owner_obj=NULL, *top_owner=NULL, *named_obj;
   int ltx, lty, rbx, rby;
   struct SelRec *saved_top_sel=topSel, *saved_bot_sel=botSel;

   UtilRemoveQuotes(obj_name);
   named_obj = FindObjWithName(botObj, obj_ptr, obj_name, FALSE,
         FALSE, &owner_obj, &top_owner);
   if (named_obj == NULL) return BadObjName(obj_name, orig_cmd);

   if (owner_obj == NULL) {
      if (named_obj == topObj) return TRUE;

      ltx = named_obj->bbox.ltx; lty = named_obj->bbox.lty;
      rbx = named_obj->bbox.rbx; rby = named_obj->bbox.rby;
   } else {
      if (named_obj == owner_obj->detail.r->first) return TRUE;

      ltx = top_owner->bbox.ltx; lty = top_owner->bbox.lty;
      rbx = top_owner->bbox.rbx; rby = top_owner->bbox.rby;
   }
   topSel = botSel = (struct SelRec *)malloc(sizeof(struct SelRec));
   if (topSel == NULL) {
      FailAllocMessage();
      topSel = saved_top_sel;
      botSel = saved_bot_sel;
      return FALSE;
   }
   topSel->next = topSel->prev = NULL;
   topSel->obj = (owner_obj==NULL ? named_obj : top_owner);
   UpdSelBBox ();

   if (owner_obj == NULL) {
      MoveSelToTop();
   } else {
      PrepareToRecord (CMD_REPLACE, topSel, botSel, numObjSelected);

      named_obj->prev->next = named_obj->next;
      if (named_obj == owner_obj->detail.r->last) {
         owner_obj->detail.r->last = named_obj->prev;
      } else {
         named_obj->next->prev = named_obj->prev;
      }
      named_obj->prev = NULL;
      named_obj->next = owner_obj->detail.r->first;
      owner_obj->detail.r->first->prev = named_obj;
      owner_obj->detail.r->first = named_obj;

      RecordCmd (CMD_REPLACE, NULL, topSel, botSel, numObjSelected);
   }
   free(topSel);
   topSel = saved_top_sel;
   botSel = saved_bot_sel;
   UpdSelBBox ();

   RedrawAnArea(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1));
   SetFileModified(TRUE);

   return TRUE;
}

void ExecDisableUndo()
   /* disable_undo(); */
{
   if (historyDepth != 0) {
      Msg("Undo buffer flushed by disable_undo().");
   }
   DisableUndo();
}

void ExecEnableUndo()
   /* enable_undo(); */
{
   EnableUndo();
}

int ExecGetDrawingArea(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* get_drawing_area(ltx_attr,lty_attr,rbx_attr,rby_attr); */
{
   char *ltx_attr_name=argv[0], *lty_attr_name=argv[1];
   char *rbx_attr_name=argv[2], *rby_attr_name=argv[3], buf[40];
   struct AttrRec *ltx_attr_ptr, *lty_attr_ptr, *rbx_attr_ptr, *rby_attr_ptr;
   struct ObjRec *ltx_attr_owner_obj=NULL, *lty_attr_owner_obj=NULL;
   struct ObjRec *rbx_attr_owner_obj=NULL, *rby_attr_owner_obj=NULL;

   UtilRemoveQuotes(ltx_attr_name); UtilRemoveQuotes(lty_attr_name);
   UtilRemoveQuotes(rbx_attr_name); UtilRemoveQuotes(rby_attr_name);
   sprintf(execDummyStr, "%s=", ltx_attr_name);
   ltx_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &ltx_attr_owner_obj);
   if (ltx_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   sprintf(execDummyStr, "%s=", lty_attr_name);
   lty_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &lty_attr_owner_obj);
   if (lty_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   sprintf(execDummyStr, "%s=", rbx_attr_name);
   rbx_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &rbx_attr_owner_obj);
   if (rbx_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   sprintf(execDummyStr, "%s=", rby_attr_name);
   rby_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &rby_attr_owner_obj);
   if (rby_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   sprintf(buf, "%1d", drawOrigX);
   ReplaceAttrFirstValue(ltx_attr_owner_obj, ltx_attr_ptr, buf);
   sprintf(buf, "%1d", drawOrigY);
   ReplaceAttrFirstValue(lty_attr_owner_obj, lty_attr_ptr, buf);
   sprintf(buf, "%1d", drawOrigX+drawWinW);
   ReplaceAttrFirstValue(rbx_attr_owner_obj, rbx_attr_ptr, buf);
   sprintf(buf, "%1d", drawOrigY+drawWinH);
   ReplaceAttrFirstValue(rby_attr_owner_obj, rby_attr_ptr, buf);

   SetFileModified(TRUE);
   return TRUE;
}

int ExecGetSelObjBBox(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* get_selected_obj_bbox(ltx_attr,lty_attr,rbx_attr,rby_attr); */
{
   char *ltx_attr_name=argv[0], *lty_attr_name=argv[1];
   char *rbx_attr_name=argv[2], *rby_attr_name=argv[3], buf[40];
   struct AttrRec *ltx_attr_ptr, *lty_attr_ptr, *rbx_attr_ptr, *rby_attr_ptr;
   struct ObjRec *ltx_attr_owner_obj=NULL, *lty_attr_owner_obj=NULL;
   struct ObjRec *rbx_attr_owner_obj=NULL, *rby_attr_owner_obj=NULL;

   UtilRemoveQuotes(ltx_attr_name); UtilRemoveQuotes(lty_attr_name);
   UtilRemoveQuotes(rbx_attr_name); UtilRemoveQuotes(rby_attr_name);
   if (topSel == NULL) return BadSelectedObj(orig_cmd);

   sprintf(execDummyStr, "%s=", ltx_attr_name);
   ltx_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &ltx_attr_owner_obj);
   if (ltx_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   sprintf(execDummyStr, "%s=", lty_attr_name);
   lty_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &lty_attr_owner_obj);
   if (lty_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   sprintf(execDummyStr, "%s=", rbx_attr_name);
   rbx_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &rbx_attr_owner_obj);
   if (rbx_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   sprintf(execDummyStr, "%s=", rby_attr_name);
   rby_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &rby_attr_owner_obj);
   if (rby_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   sprintf(buf, "%1d", selObjLtX);
   ReplaceAttrFirstValue(ltx_attr_owner_obj, ltx_attr_ptr, buf);
   sprintf(buf, "%1d", selObjLtY);
   ReplaceAttrFirstValue(lty_attr_owner_obj, lty_attr_ptr, buf);
   sprintf(buf, "%1d", selObjRbX);
   ReplaceAttrFirstValue(rbx_attr_owner_obj, rbx_attr_ptr, buf);
   sprintf(buf, "%1d", selObjRbY);
   ReplaceAttrFirstValue(rby_attr_owner_obj, rby_attr_ptr, buf);

   SetFileModified(TRUE);
   return TRUE;
}

int ExecMoveSelObjAbs(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* move_selected_obj_absolute(ltx,lty); */
{
   char *ltx_str=argv[0], *lty_str=argv[1];
   int ltx, lty;

   UtilRemoveQuotes(ltx_str);
   UtilRemoveQuotes(lty_str);
   if (topSel == NULL) return BadSelectedObj(orig_cmd);

   if (!IntExpression(ltx_str, &ltx, orig_cmd) ||
         !IntExpression(lty_str, &lty, orig_cmd)) {
      return FALSE;
   }
   if (ltx != selObjLtX || lty != selObjLtY) {
      MoveAllSel(ltx-selObjLtX, lty-selObjLtY);
      UpdSelBBox();
      SetFileModified(TRUE);
   }
   return TRUE;
}

int ExecAssign(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* assign(attr_name,expr); */
{
   char *attr_name=argv[0], *expr=argv[1], buf[40];
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;
   struct VRec v;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(expr);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   if (!EvalExpr(expr, &v)) return FALSE;

   switch (v.vtype) {
   case INT_VAL:
      sprintf(buf, "%1d", v.val.i);
      ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, buf);
      break;
   case DBL_VAL:
      sprintf(buf, "%.12lf", v.val.d);
      ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, buf);
      break;
   case NULL_VAL:
   case STR_VAL:
      sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
            "Invalid evaluation", expr,
            "(numeric value expected) when executing the",
            orig_cmd, "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      if (v.vtype == STR_VAL && v.val.s != NULL) free(v.val.s);
      return FALSE;
   }
   return TRUE;
}

int ExecStrCpy(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* strcpy(attr_name,str); */
{
   char *attr_name=argv[0], *the_str=argv[1];
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;

   UtilRemoveQuotes(attr_name);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, the_str);
   return TRUE;
}

int ExecWhile(argv, raw_argv, obj_ptr, orig_cmd)
   char **argv, **raw_argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* while(expr,attr_to_exec); */
{
   char *raw_expr=raw_argv[0], *attr_name=argv[1];
   struct AttrRec *attr_ptr;
   int rc=TRUE;

   UtilRemoveQuotes(attr_name);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, NULL);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   while (rc) {
      int val;
      char *expr=convert_str(raw_expr, obj_ptr, TRUE);

      if (expr == NULL) {
         BadAttr(raw_expr, orig_cmd);
         return FALSE;
      } else if (!IntExpression(expr, &val, orig_cmd)) {
         free(expr);
         return FALSE;
      }
      free(expr);
      if (val == 0) break;
      if (!DoExec(attr_ptr, obj_ptr)) return FALSE;
      rc = (!CheckExecInterrupt(FALSE, orig_cmd));
   }
   return rc;
}

int ExecIf(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* if(expr,then_attr_to_exec,else_attr_to_exec); */
{
   char *expr=argv[0], *then_attr_name=argv[1], *else_attr_name=argv[2];
   struct AttrRec *then_attr_ptr=NULL, *else_attr_ptr=NULL;
   int val;

   UtilRemoveQuotes(then_attr_name);
   UtilRemoveQuotes(else_attr_name);
   if (strcmp(then_attr_name, "NULL") != 0) {
      sprintf(execDummyStr, "%s=", then_attr_name);
      then_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, NULL);
      if (then_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   }
   if (strcmp(else_attr_name, "NULL") != 0) {
      sprintf(execDummyStr, "%s=", else_attr_name);
      else_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, NULL);
      if (else_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   }

   if (!IntExpression(expr, &val, orig_cmd)) return FALSE;
   if (val) {
      if (then_attr_ptr != NULL && !DoExec(then_attr_ptr, obj_ptr)) {
         return FALSE;
      }
   } else {
      if (else_attr_ptr != NULL && !DoExec(else_attr_ptr, obj_ptr)) {
         return FALSE;
      }
   }
   return TRUE;
}

int ExecGetCurrentFile(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* get_current_file(attr_name); */
{
   char *attr_name=argv[0], *full_name=NULL;
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;

   UtilRemoveQuotes(attr_name);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   if (curFileDefined) {
      if (*curSymDir == '\0') {
         sprintf(gszMsgBox, "%s/%s", curDir, curFileName);
      } else {
         sprintf(gszMsgBox, "%s/%s", curSymDir, curFileName);
      }
      full_name = UtilStrDup(gszMsgBox);
   }
   ReplaceAttrFirstValue(attr_owner_obj, attr_ptr,
         (full_name==NULL ? "" : full_name));
   if (full_name != NULL) free(full_name);
   return TRUE;

}

int ExecGetEnv(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* getenv(attr_name,env_var_name); */
{
   char *attr_name=argv[0], *env_var_name=argv[1], *c_ptr, *env_var_value=NULL;
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(env_var_name);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   if ((c_ptr=getenv(env_var_name)) != NULL) {
      env_var_value = UtilStrDup(c_ptr);
   }
   ReplaceAttrFirstValue(attr_owner_obj, attr_ptr,
         (env_var_value==NULL ? "" : env_var_value));
   if (env_var_value != NULL) free(env_var_value);
   return TRUE;
}

int ExecStrLen(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* strlen(attr_name,str); */
{
   char *attr_name=argv[0], *the_str=argv[1], val_str[40];
   int len;
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(the_str);
   len = strlen(the_str);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   sprintf(val_str, "%1d", len);
   ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, val_str);
   return TRUE;
}

int ExecSubStr(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* substr(attr_name,str,start_index,length); */
{
   char *attr_name=argv[0], *the_str=argv[1];
   char *start_index_str=argv[2], *length_str=argv[3], *buf;
   int len, start_index, length;
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(the_str);
   UtilRemoveQuotes(start_index_str);
   len = strlen(the_str);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   if (sscanf(start_index_str, "%d", &start_index) != 1) {
      return BadArg("start_index", orig_cmd);
   } else if (sscanf(length_str, "%d", &length) != 1) {
      return BadArg("length", orig_cmd);
   }
   if (start_index < 0) start_index = 0;
   if (length+start_index > len) {
      length = len-start_index;
   }
   if (length < 0) length = 0;
   if ((buf=UtilStrDup(the_str)) == NULL) {
      FailAllocMessage();
   } else {
      strncpy(buf, &the_str[start_index], length);
      buf[start_index+length] = '\0';
   }
   ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, (buf==NULL ? "" : buf));
   if (buf != NULL) free(buf);
   return TRUE;
}

int ExecStrStr(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* strstr(attr_name,str,substr); */
{
   char *attr_name=argv[0], *the_str=argv[1], *sub_str=argv[2], *c_ptr;
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(the_str);
   UtilRemoveQuotes(sub_str);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   c_ptr = strstr(the_str, sub_str);
   ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, (c_ptr==NULL ? "" : c_ptr));
   return TRUE;
}

int ExecStrRStr(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* strrstr(attr_name,str,substr); */
{
   char *attr_name=argv[0], *the_str=argv[1], *sub_str=argv[2];
   char *c_ptr, *last_match=NULL;
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(the_str);
   UtilRemoveQuotes(sub_str);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   for (c_ptr=strstr(the_str, sub_str); c_ptr != NULL;
         c_ptr=strstr(&last_match[1], sub_str)) {
      last_match = c_ptr;
   }
   ReplaceAttrFirstValue(attr_owner_obj, attr_ptr,
         (last_match==NULL ? "" : last_match));
   return TRUE;
}

void ExecUnMakeSelObjIconic()
   /* unmake_selected_obj_iconic(); */
{
   struct SelRec *sel_ptr;
   int modified=FALSE;

   if (topSel == NULL) { BadSelectedObj("unmake_selected_obj_iconic"); return; }

   StartCompositeCmd();
   for (sel_ptr=topSel; sel_ptr != NULL; sel_ptr=sel_ptr->next) {
      struct ObjRec *obj_ptr=sel_ptr->obj;

      if (obj_ptr->type == OBJ_ICON) {
         struct AttrRec *attr_ptr;

         modified = TRUE;
         PrepareToReplaceAnObj(obj_ptr);
         obj_ptr->type = OBJ_GROUP;
         attr_ptr = obj_ptr->fattr;
         for ( ; attr_ptr != NULL; attr_ptr = attr_ptr->next) {
            attr_ptr->inherited = FALSE;
         }
         AdjObjBBox(obj_ptr);
         RecordReplaceAnObj(obj_ptr);
      }
   }
   EndCompositeCmd();
   if (modified) {
      SetFileModified(TRUE);
   }
}

int ExecHyperJumpThenExec(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* hyperjump_then_exec(attr_name,attr_name_to_exec); */
{
   char *attr_name=argv[0], *attr_name_to_exec=argv[1];
   struct AttrRec *attr_ptr;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(attr_name_to_exec);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, NULL);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   warpToAttr = attr_ptr;
   if (cmdToExecAfterHyperJump != NULL) free(cmdToExecAfterHyperJump);
   sprintf(execDummyStr, "%s=", attr_name_to_exec);
   cmdToExecAfterHyperJump = UtilStrDup(execDummyStr);

   return TRUE;
}

#define SHOW_ATTR 0
#define HIDE_ATTR 1
#define SHOW_ATTR_NAME 2
#define HIDE_ATTR_NAME 3
#define SHOW_VALUE 4
#define HIDE_VALUE 5

static
void ReplaceAttrShown(obj_ptr, attr_ptr, show_type)
   struct ObjRec *obj_ptr;
   struct AttrRec *attr_ptr;
   int show_type;
   /* obj_ptr better be a top-level object */
{
   int ltx, lty, rbx, rby, needs_redraw=FALSE;

   ltx = obj_ptr->bbox.ltx; lty = obj_ptr->bbox.lty;
   rbx = obj_ptr->bbox.rbx; rby = obj_ptr->bbox.rby;
   PrepareToReplaceAnObj(obj_ptr);
   switch (show_type) {
   case SHOW_ATTR:
   case SHOW_VALUE:
      attr_ptr->shown = TRUE;
      needs_redraw = TRUE;
      break;
   case HIDE_ATTR:
   case HIDE_VALUE:
      attr_ptr->shown = FALSE;
      needs_redraw = TRUE;
      break;
   case SHOW_ATTR_NAME:
      attr_ptr->nameshown = TRUE;
      if (attr_ptr->shown) needs_redraw = TRUE;
      UpdAttr(attr_ptr);
      break;
   case HIDE_ATTR_NAME:
      attr_ptr->nameshown = FALSE;
      if (attr_ptr->shown) needs_redraw = TRUE;
      UpdAttr(attr_ptr);
      break;
   }
   attr_ptr->obj->detail.t->cached_zoom = 0;
   if (attr_ptr->obj->detail.t->cached_bitmap != None) {
      XFreePixmap(mainDisplay, attr_ptr->obj->detail.t->cached_bitmap);
      attr_ptr->obj->detail.t->cached_bitmap = None;
   }
   if (attr_ptr->shown) {
      AdjObjCache(obj_ptr);
      AdjObjBBox(obj_ptr);
   }
   RecordReplaceAnObj(obj_ptr);
   if (needs_redraw) {
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
            obj_ptr->bbox.ltx-GRID_ABS_SIZE(1),
            obj_ptr->bbox.lty-GRID_ABS_SIZE(1),
            obj_ptr->bbox.rbx+GRID_ABS_SIZE(1),
            obj_ptr->bbox.rby+GRID_ABS_SIZE(1));
   }
   SetFileModified(TRUE);
}

static
int ExecShowHideAttr(argv, obj_ptr, orig_cmd, show_type)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   int show_type;
   /* show_attr(attr_name); */
   /* hide_attr(attr_name); */
   /* show_attr_name(attr_name); */
   /* hide_attr_name(attr_name); */
{
   char *attr_name=argv[0];
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;

   UtilRemoveQuotes(attr_name);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   if (attr_owner_obj == tgifObj) {
      return FileAttrNotAllowed(execDummyStr, orig_cmd);
   }
   switch (show_type) {
   case SHOW_ATTR:
      if (attr_ptr->shown) return TRUE;
      break;
   case HIDE_ATTR:
      if (!attr_ptr->shown) return TRUE;
      break;
   case SHOW_ATTR_NAME:
      if (*attr_ptr->attr_name.s == '\0' || attr_ptr->nameshown) return TRUE;
      break;
   case HIDE_ATTR_NAME:
      if (*attr_ptr->attr_name.s == '\0' || !attr_ptr->nameshown) return TRUE;
      break;
   }
   ReplaceAttrShown(attr_owner_obj, attr_ptr, show_type);

   return TRUE;
}

int ExecShowAttr(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* show_attr(attr_name); */
{
   return ExecShowHideAttr(argv, obj_ptr, orig_cmd, SHOW_ATTR);
}

int ExecHideAttr(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* hide_attr(attr_name); */
{
   return ExecShowHideAttr(argv, obj_ptr, orig_cmd, HIDE_ATTR);
}

int ExecShowAttrName(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* show_attr_name(attr_name); */
{
   return ExecShowHideAttr(argv, obj_ptr, orig_cmd, SHOW_ATTR_NAME);
}

int ExecHideAttrName(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* hide_attr_name(attr_name); */
{
   return ExecShowHideAttr(argv, obj_ptr, orig_cmd, HIDE_ATTR_NAME);
}

static
int ExecShowHideValue(argv, obj_ptr, orig_cmd, show_type)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   int show_type;
   /* show_value(attr_value); */
   /* hide_value(attr_value); */
{
   char *attr_value=argv[0];
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;

   UtilRemoveQuotes(attr_value);
   sprintf(execDummyStr, "%s", attr_value);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   if (attr_owner_obj == tgifObj) {
      return FileAttrNotAllowed(execDummyStr, orig_cmd);
   }
   switch (show_type) {
   case SHOW_VALUE:
      if (attr_ptr->shown) return TRUE;
      break;
   case HIDE_VALUE:
      if (!attr_ptr->shown) return TRUE;
      break;
   }
   ReplaceAttrShown(attr_owner_obj, attr_ptr, show_type);

   return TRUE;
}

int ExecShowValue(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* show_value(attr_value); */
{
   return ExecShowHideValue(argv, obj_ptr, orig_cmd, SHOW_VALUE);
}

int ExecHideValue(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* hide_value(attr_value); */
{
   return ExecShowHideValue(argv, obj_ptr, orig_cmd, HIDE_VALUE);
}

int ExecGetAttrBBox(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* get_attr_bbox(ltx_attr,lty_attr,rbx_attr,rby_attr,attr_name); */
{
   char *ltx_attr_name=argv[0], *lty_attr_name=argv[1];
   char *rbx_attr_name=argv[2], *rby_attr_name=argv[3], buf[40];
   char *attr_name=argv[4];
   struct AttrRec *ltx_attr_ptr, *lty_attr_ptr, *rbx_attr_ptr, *rby_attr_ptr;
   struct AttrRec *attr_ptr;
   struct ObjRec *ltx_attr_owner_obj=NULL, *lty_attr_owner_obj=NULL;
   struct ObjRec *rbx_attr_owner_obj=NULL, *rby_attr_owner_obj=NULL;
   struct ObjRec *attr_owner_obj=NULL;

   UtilRemoveQuotes(ltx_attr_name); UtilRemoveQuotes(lty_attr_name);
   UtilRemoveQuotes(rbx_attr_name); UtilRemoveQuotes(rby_attr_name);
   UtilRemoveQuotes(attr_name);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   if (attr_owner_obj == tgifObj) {
      return FileAttrNotAllowed(execDummyStr, orig_cmd);
   }
   sprintf(execDummyStr, "%s=", ltx_attr_name);
   ltx_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &ltx_attr_owner_obj);
   if (ltx_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   sprintf(execDummyStr, "%s=", lty_attr_name);
   lty_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &lty_attr_owner_obj);
   if (lty_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   sprintf(execDummyStr, "%s=", rbx_attr_name);
   rbx_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &rbx_attr_owner_obj);
   if (rbx_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   sprintf(execDummyStr, "%s=", rby_attr_name);
   rby_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &rby_attr_owner_obj);
   if (rby_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   sprintf(buf, "%1d", attr_ptr->obj->obbox.ltx);
   ReplaceAttrFirstValue(ltx_attr_owner_obj, ltx_attr_ptr, buf);
   sprintf(buf, "%1d", attr_ptr->obj->obbox.lty);
   ReplaceAttrFirstValue(lty_attr_owner_obj, lty_attr_ptr, buf);
   sprintf(buf, "%1d", attr_ptr->obj->obbox.rbx);
   ReplaceAttrFirstValue(rbx_attr_owner_obj, rbx_attr_ptr, buf);
   sprintf(buf, "%1d", attr_ptr->obj->obbox.rby);
   ReplaceAttrFirstValue(rby_attr_owner_obj, rby_attr_ptr, buf);

   SetFileModified(TRUE);
   return TRUE;
}

int ExecSizeSelObjAbs(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* size_selected_obj_absolute(abs_w,abs_h); */
{
   char *abs_w_str=argv[0], *abs_h_str=argv[1];
   int abs_w, abs_h;

   UtilRemoveQuotes(abs_w_str);
   UtilRemoveQuotes(abs_h_str);
   if (topSel == NULL) return BadSelectedObj(orig_cmd);

   if (!IntExpression(abs_w_str, &abs_w, orig_cmd) ||
         !IntExpression(abs_h_str, &abs_h, orig_cmd)) {
      return FALSE;
   }
   if (abs_w != selObjRbX-selObjLtX || abs_h != selObjRbY-selObjLtY) {
      SizeAllSelObj(abs_w, abs_h);
   }
   return TRUE;
}

int ExecMessageBox(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* message_box(attr_name,msg,title,style); */
{
   char *attr_name=argv[0], *msg=argv[1], *title=argv[2], *style_str=argv[3];
   char buf[10];
   struct AttrRec *attr_ptr=NULL;
   struct ObjRec *attr_owner_obj=NULL;
   int style=INFO_MB;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(msg);
   UtilRemoveQuotes(title);
   UtilRemoveQuotes(style_str);
   if (strcmp(attr_name, "NULL") != 0) {
      sprintf(execDummyStr, "%s=", attr_name);
      attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
      if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   }
   if (strcmp(title, "NULL") == 0) {
      title = TOOL_NAME;
   }
   if (strcmp(style_str, "NULL") == 0) {
      style = INFO_MB;
   } else if (strcmp(style_str, "info") == 0) {
      style = INFO_MB;
   } else if (strcmp(style_str, "ync") == 0) {
      style = YNC_MB;
   } else if (strcmp(style_str, "yn") == 0) {
      style = YN_MB;
   } else if (strcmp(style_str, "stop") == 0) {
      style = STOP_MB;
   } else {
      sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
            "Style of", style_str, "is invalid for the", orig_cmd, "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   switch (MsgBox(msg, title, style)) {
   case MB_ID_FAILED: strcpy(buf, "FAILED"); break;
   case MB_ID_OK: strcpy(buf, "OK"); break;
   case MB_ID_CANCEL: strcpy(buf, "CANCEL"); break;
   case MB_ID_YES: strcpy(buf, "YES"); break;
   case MB_ID_NO: strcpy(buf, "NO"); break;
   case MB_ID_EXTRA: strcpy(buf, "EXTRA"); break;
   default: strcpy(buf, "(unknown)"); break;
   }
   if (attr_ptr != NULL) {
      ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, buf);
   }
   return TRUE;
}

int ExecGetUserInput(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* get_user_input(attr_name,msg1,msg2); */
{
   char *attr_name=argv[0], *msg1=argv[1], *msg2=argv[2];
   char buf[MAXPATHLENGTH+1];
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;
   int rc=INVALID;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(msg1);
   UtilRemoveQuotes(msg2);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   *buf = '\0';
   if (strcmp(msg2, "USE_CURRENT_DIR") == 0) {
      if (curDirIsLocal) {
         sprintf(gszMsgBox, "( working directory: %s )", curDir);
      } else {
         sprintf(gszMsgBox, "( working directory: %s )", curLocalDir);
      }
      rc = Dialog(msg1, gszMsgBox, buf);
   } else if (strcmp(msg2, "NULL") == 0) {
      rc = Dialog(msg1, NULL, buf);
   } else {
      rc = Dialog(msg1, msg2, buf);
   }
   if (rc == INVALID) *buf = '\0';
   ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, buf);
   return TRUE;
}

int ExecAddAttrToSelObj(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* add_attr_to_selected_obj(attr_name,attr_value,abs_x,abs_y); */
{
   char *attr_name=argv[0], *attr_value=argv[1];
   char *abs_x_str=argv[2], *abs_y_str=argv[3];
   struct AttrRec *attr_ptr;
   struct ObjRec *selected_obj;
   int abs_x=0, abs_y=0, ignore_x=FALSE, ignore_y=FALSE;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(attr_value);
   UtilRemoveQuotes(abs_x_str);
   UtilRemoveQuotes(abs_y_str);
   if (topSel == NULL) return BadSelectedObj(orig_cmd);
   if (topSel != botSel) {
      sprintf(gszMsgBox, "%s '%s' %s.",
            "Too many objects selected when executing the", orig_cmd,
            "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   } else if (strchr(attr_name, '.') != NULL) {
      sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
            "Illegal attribute name", attr_name, "when executing the", orig_cmd,
            "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   if (strcmp(abs_x_str, "NULL") == 0) {
      ignore_x = TRUE;
   } else if (!IntExpression(abs_x_str, &abs_x, orig_cmd)) {
      return FALSE;
   }
   if (strcmp(abs_y_str, "NULL") == 0) {
      ignore_y = TRUE;
   } else if (!IntExpression(abs_y_str, &abs_y, orig_cmd)) {
      return FALSE;
   }
   selected_obj = topSel->obj;

   if (strcmp(attr_name, "NULL") == 0 || *attr_name == '\0') {
      *execDummyStr = '\0';
   } else {
      sprintf(execDummyStr, "%s=", attr_name);
   }
   attr_ptr = FindAttrWithName(selected_obj, execDummyStr, NULL);
   if (attr_ptr == NULL) {
      int ltx, lty, rbx, rby, x, y;

      ltx = selected_obj->bbox.ltx; lty = selected_obj->bbox.lty;
      rbx = selected_obj->bbox.rbx; rby = selected_obj->bbox.rby;
      x = (ignore_x ? selected_obj->obbox.ltx : abs_x);
      y = (ignore_y ? selected_obj->obbox.rby : abs_y);
      PrepareToReplaceAnObj(selected_obj);
      attr_ptr = AddAttrByNameAndValue(selected_obj, execDummyStr, attr_value);
      attr_ptr->shown = TRUE;
      attr_ptr->obj->color = colorIndex;
      MoveObj(attr_ptr->obj, x-attr_ptr->obj->x, y-attr_ptr->obj->y);
      UpdTextBBox(attr_ptr->obj);
      AdjObjBBox(selected_obj);
      RecordReplaceAnObj(selected_obj);
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
               rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1),
               selected_obj->bbox.ltx-GRID_ABS_SIZE(1),
               selected_obj->bbox.lty-GRID_ABS_SIZE(1),
               selected_obj->bbox.rbx+GRID_ABS_SIZE(1),
               selected_obj->bbox.rby+GRID_ABS_SIZE(1));
      SetFileModified(TRUE);
   } else {
      ReplaceAttrFirstValue(selected_obj, attr_ptr, attr_value);
   }
   return TRUE;
}

int ExecUserEndAnEdge(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* user_end_an_edge(attr_name,abs_x,abs_y); */
{
   char *attr_name=argv[0], *abs_x_str=argv[1], *abs_y_str=argv[2];
   struct AttrRec *attr_ptr=NULL;
   struct ObjRec *attr_owner_obj;
   int abs_x, abs_y, poly_created=FALSE, already_in_hyperspace=inHyperSpace;
   XEvent ev;
   XButtonEvent *button_ev_ptr;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(abs_x_str);
   UtilRemoveQuotes(abs_y_str);
   if (strcmp(attr_name, "NULL") != 0) {
      sprintf(execDummyStr, "%s=", attr_name);
      attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
      if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   }
   if (topSel != NULL) {
      RemoveAllSel();
   }
   if (!IntExpression(abs_x_str, &abs_x, orig_cmd) ||
         !IntExpression(abs_y_str, &abs_y, orig_cmd)) {
      return FALSE;
   }
   SetCurChoice(DRAWPOLY);
   SetStringStatus("Please end an edge at a port...");

   ev.type = ButtonPress;
   button_ev_ptr = &(ev.xbutton);
   button_ev_ptr->button = Button1;
   button_ev_ptr->x = OFFSET_X(abs_x);
   button_ev_ptr->y = OFFSET_Y(abs_y);

   polyDrawn = FALSE;
   drawPolyToEndInANode = 1;
   DrawPoly(&ev);
   drawPolyToEndInANode = 0;
   poly_created = polyDrawn;
   SetCurChoice(NOTHING);
   if (already_in_hyperspace && !inHyperSpace) ToggleHyperSpace(TRUE);

   if (attr_ptr != NULL) {
      ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, poly_created ?
            (strncmp(drawPolyLastNodeName,"NodeName: ",10) == 0 ?
            &drawPolyLastNodeName[10] : drawPolyLastNodeName) : "");
   }
   return TRUE;
}

int ExecUserDrawAnEdge(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* user_draw_an_edge(start_attr_name,end_attr_name); */
{
   char *start_attr_name=argv[0], *end_attr_name=argv[1];
   struct AttrRec *start_attr_ptr=NULL, *end_attr_ptr=NULL;
   struct ObjRec *start_attr_owner_obj, *end_attr_owner_obj;
   int poly_created=FALSE, already_in_hyperspace=inHyperSpace;
   XGCValues values;

   UtilRemoveQuotes(start_attr_name);
   UtilRemoveQuotes(end_attr_name);
   if (strcmp(start_attr_name, "NULL") != 0) {
      sprintf(execDummyStr, "%s=", start_attr_name);
      start_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr,
            &start_attr_owner_obj);
      if (start_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   }
   if (strcmp(end_attr_name, "NULL") != 0) {
      sprintf(execDummyStr, "%s=", end_attr_name);
      end_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr,
            &end_attr_owner_obj);
      if (end_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   }
   if (topSel != NULL) {
      RemoveAllSel();
   }
   values.line_width = 3;
   XChangeGC(mainDisplay, revGrayGC, GCLineWidth, &values);

   SetCurChoice(DRAWPOLY);
   drawPolyToEndInANode = 2;
   drawPolyHighlightedNode = NULL;
   SetStringStatus("Please draw an edge between ports...");
   for (;;) {
      XEvent input;

      XNextEvent(mainDisplay, &input);
      if (input.type == Expose) {
         ExposeEventHandler(&input, TRUE);
      } else if (input.xany.window == drawWindow) {
         polyDrawn = FALSE;
         DrawingEventHandler(&input);
         if (curChoice == DRAWPOLY) {
            if (polyDrawn) {
               break;
            } else if (drawPolyToEndInANode == (-1)) {
               break;
            }
         } else {
            polyDrawn = FALSE;
            break;
         }
      }
   }
   drawPolyToEndInANode = 0;
   poly_created = polyDrawn;
   SetCurChoice(NOTHING);
   values.line_width = 1;
   XChangeGC(mainDisplay, revGrayGC, GCLineWidth, &values);
   if (already_in_hyperspace && !inHyperSpace) ToggleHyperSpace(TRUE);

   if (start_attr_ptr != NULL) {
      ReplaceAttrFirstValue(start_attr_owner_obj, start_attr_ptr, poly_created ?
            (strncmp(drawPolyFirstNodeName,"NodeName: ",10) == 0 ?
            &drawPolyFirstNodeName[10] : drawPolyFirstNodeName) : "");
   }
   if (end_attr_ptr != NULL) {
      ReplaceAttrFirstValue(end_attr_owner_obj, end_attr_ptr, poly_created ?
            (strncmp(drawPolyLastNodeName,"NodeName: ",10) == 0 ?
            &drawPolyLastNodeName[10] : drawPolyLastNodeName) : "");
   }
   return TRUE;
}

int ExecGetAPolyVertexAbs(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* get_a_poly_vertex_absolute(x_attr,y_attr,obj_name,v_index); */
{
   char *x_attr_name=argv[0], *y_attr_name=argv[1], *obj_name=argv[2];
   char *v_index_str=argv[3], buf[40];
   int v_index=0, abs_x=0, abs_y=0;
   struct AttrRec *x_attr_ptr, *y_attr_ptr;
   struct ObjRec *x_attr_owner_obj=NULL, *y_attr_owner_obj=NULL;
   struct ObjRec *owner_obj=NULL, *named_obj;

   UtilRemoveQuotes(x_attr_name);
   UtilRemoveQuotes(y_attr_name);
   UtilRemoveQuotes(obj_name);
   UtilRemoveQuotes(v_index_str);
   named_obj = FindObjWithName(botObj, obj_ptr, obj_name, FALSE,
         FALSE, &owner_obj, NULL);
   if (named_obj == NULL) {
      sprintf(execDummyStr, "%s '%s' %s '%s' %s.",
            "Can not find object named", obj_name, "while executing the",
            orig_cmd, "command");
      MsgBox(execDummyStr, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   if (!IntExpression(v_index_str, &v_index, orig_cmd)) return FALSE;

   switch (named_obj->type) {
   case OBJ_POLY:
      if (v_index < 0 || v_index >= named_obj->detail.p->n) {
         sprintf(execDummyStr, "%s %1d %s '%s' %s '%s' %s.",
               "Cannot find vertex", v_index, "for", obj_name,
               "when executing the", orig_cmd, "command");
         MsgBox(execDummyStr, TOOL_NAME, INFO_MB);
         return FALSE;
      }
      abs_x = (int)named_obj->detail.p->vlist[v_index].x;
      abs_y = (int)named_obj->detail.p->vlist[v_index].y;
      break;
   case OBJ_POLYGON:
      if (v_index < 0 || v_index >= named_obj->detail.g->n) {
         sprintf(execDummyStr, "%s %1d %s '%s' %s '%s' %s.",
               "Cannot find vertex", v_index, "for", obj_name,
               "when executing the", orig_cmd, "command");
         MsgBox(execDummyStr, TOOL_NAME, INFO_MB);
         return FALSE;
      }
      abs_x = (int)named_obj->detail.g->vlist[v_index].x;
      abs_y = (int)named_obj->detail.g->vlist[v_index].y;
      break;
   default:
      sprintf(execDummyStr, "'%s' %s '%s' %s.",
            obj_name,
            "is neither a poly nor a polygon object when executing the",
            orig_cmd, "command");
      MsgBox(execDummyStr, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   sprintf(execDummyStr, "%s=", x_attr_name);
   x_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &x_attr_owner_obj);
   if (x_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   sprintf(execDummyStr, "%s=", y_attr_name);
   y_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &y_attr_owner_obj);
   if (y_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   sprintf(buf, "%1d", abs_x);
   ReplaceAttrFirstValue(x_attr_owner_obj, x_attr_ptr, buf);
   sprintf(buf, "%1d", abs_y);
   ReplaceAttrFirstValue(y_attr_owner_obj, y_attr_ptr, buf);

   SetFileModified(TRUE);
   return TRUE;
}

int ExecMoveAPolyVertexAbs(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* move_a_poly_vertex_absolute(obj_name,v_index,abs_x,abs_y); */
{
   char *obj_name=argv[0], *v_index_str=argv[1];
   char *abs_x_str=argv[2], *abs_y_str=argv[3];
   int v_index=0, abs_x=0, abs_y=0, cur_x=0, cur_y=0, ltx, lty, rbx, rby;
   int auto_center_attr;
   struct ObjRec *owner_obj=NULL, *top_owner=NULL, *named_obj;
   struct SelRec *saved_top_sel=topSel, *saved_bot_sel=botSel;

   UtilRemoveQuotes(obj_name);
   UtilRemoveQuotes(v_index_str);
   UtilRemoveQuotes(abs_x_str);
   UtilRemoveQuotes(abs_y_str);
   named_obj = FindObjWithName(botObj, obj_ptr, obj_name, FALSE,
         FALSE, &owner_obj, &top_owner);
   if (named_obj == NULL) {
      sprintf(execDummyStr, "%s '%s' %s '%s' %s.",
            "Can not find object named", obj_name, "while executing the",
            orig_cmd, "command");
      MsgBox(execDummyStr, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   if (!IntExpression(v_index_str, &v_index, orig_cmd)) return FALSE;
   if (!IntExpression(abs_x_str, &abs_x, orig_cmd)) return FALSE;
   if (!IntExpression(abs_y_str, &abs_y, orig_cmd)) return FALSE;

   switch (named_obj->type) {
   case OBJ_POLY:
      if (v_index < 0 || v_index >= named_obj->detail.p->n) {
         sprintf(execDummyStr, "%s %1d %s '%s' %s '%s' %s.",
               "Cannot find vertex", v_index, "for", obj_name,
               "when executing the", orig_cmd, "command");
         MsgBox(execDummyStr, TOOL_NAME, INFO_MB);
         return FALSE;
      }
      cur_x = named_obj->detail.p->vlist[v_index].x;
      cur_y = named_obj->detail.p->vlist[v_index].y;
      break;
   case OBJ_POLYGON:
      if (v_index < 0 || v_index >= named_obj->detail.g->n) {
         sprintf(execDummyStr, "%s %1d %s '%s' %s '%s' %s.",
               "Cannot find vertex", v_index, "for", obj_name,
               "when executing the", orig_cmd, "command");
         MsgBox(execDummyStr, TOOL_NAME, INFO_MB);
         return FALSE;
      }
      cur_x = named_obj->detail.g->vlist[v_index].x;
      cur_y = named_obj->detail.g->vlist[v_index].y;
      break;
   default:
      sprintf(execDummyStr, "'%s' %s '%s' %s.",
            obj_name,
            "is neither a poly nor a polygon object when executing the",
            orig_cmd, "command");
      MsgBox(execDummyStr, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   if (cur_x == abs_x && cur_y == abs_y) return TRUE;
   if (owner_obj == NULL) {
      ltx = named_obj->bbox.ltx; lty = named_obj->bbox.lty;
      rbx = named_obj->bbox.rbx; rby = named_obj->bbox.rby;
   } else {
      ltx = top_owner->bbox.ltx; lty = top_owner->bbox.lty;
      rbx = top_owner->bbox.rbx; rby = top_owner->bbox.rby;
   }
   SelectAnObj(named_obj, owner_obj, top_owner, &saved_top_sel, &saved_bot_sel);

   PrepareToRecord(CMD_REPLACE, topSel, botSel, numObjSelected);
   switch (named_obj->type) {
   case OBJ_POLY:
      named_obj->detail.p->vlist[v_index].x = abs_x;
      named_obj->detail.p->vlist[v_index].y = abs_y;
      AdjObjSplineVs(named_obj);
      if (named_obj->detail.p->curved != LT_INTSPLINE) {
         UpdPolyBBox (named_obj, named_obj->detail.p->n,
               named_obj->detail.p->vlist);
      } else {
         UpdPolyBBox (named_obj, named_obj->detail.p->intn,
               named_obj->detail.p->intvlist);
      }
      break;
   case OBJ_POLYGON:
      named_obj->detail.g->vlist[v_index].x = abs_x;
      named_obj->detail.g->vlist[v_index].y = abs_y;
      if (v_index == 0) {
         named_obj->detail.g->vlist[named_obj->detail.g->n-1].x = abs_x;
         named_obj->detail.g->vlist[named_obj->detail.g->n-1].y = abs_y;
      } else if (v_index == named_obj->detail.g->n-1) {
         named_obj->detail.g->vlist[0].x = abs_x;
         named_obj->detail.g->vlist[0].y = abs_y;
      }
      AdjObjSplineVs(named_obj);
      if (named_obj->detail.g->curved != LT_INTSPLINE) {
         UpdPolyBBox (named_obj, named_obj->detail.g->n,
               named_obj->detail.g->vlist);
      } else {
         UpdPolyBBox (named_obj, named_obj->detail.g->intn,
               named_obj->detail.g->intvlist);
      }
      break;
   }
   auto_center_attr = AutoCenterAttr(named_obj);
   if (auto_center_attr) {
      struct AttrRec *attr_ptr;
      int modified=FALSE;

      for (attr_ptr=named_obj->fattr; attr_ptr != NULL;
            attr_ptr=attr_ptr->next) {
         if (attr_ptr->shown) {
            struct BBRec bbox;

            CenterObjInOBBox(attr_ptr->obj, named_obj->obbox, &bbox);
            if (bbox.ltx < ltx) ltx = bbox.ltx;
            if (bbox.lty < lty) lty = bbox.lty;
            if (bbox.rbx > rbx) rbx = bbox.rbx;
            if (bbox.rby > rby) rby = bbox.rby;
            modified = TRUE;
         }
      }
      if (modified) AdjObjBBox(named_obj);
   }
   RecursivelyAdjObjBBox(named_obj, owner_obj, top_owner);
   RecordCmd(CMD_REPLACE, NULL, topSel, botSel, numObjSelected);

   UpdSelBBox();
   if (owner_obj == NULL) {
      if (named_obj->bbox.ltx < ltx) ltx = named_obj->bbox.ltx;
      if (named_obj->bbox.lty < lty) lty = named_obj->bbox.lty;
      if (named_obj->bbox.rbx > rbx) rbx = named_obj->bbox.rbx;
      if (named_obj->bbox.rby > rby) rby = named_obj->bbox.rby;
   } else {
      if (top_owner->bbox.ltx < ltx) ltx = top_owner->bbox.ltx;
      if (top_owner->bbox.lty < lty) lty = top_owner->bbox.lty;
      if (top_owner->bbox.rbx > rbx) rbx = top_owner->bbox.rbx;
      if (top_owner->bbox.rby > rby) rby = top_owner->bbox.rby;
   }
   free(topSel);
   topSel = saved_top_sel;
   botSel = saved_bot_sel;
   UpdSelBBox();

   RedrawAnArea(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1));
   SetFileModified(TRUE);
   return TRUE;
}

int ExecPostAttrAndGetCGI(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* post_attr_and_get_cgi_result(url_attr,query_attr,result_attr); */
{
   char *url_attr_name=argv[0], *query_attr_name=argv[1];
   char *result_attr_name=argv[2];
   struct AttrRec *url_attr, *query_attr, *result_attr;
   struct ObjRec *result_attr_owner_obj=NULL;
   char *remote_buf=NULL, *tmp_remote_fname=NULL;
   int is_html=FALSE, remote_buf_sz=0, rc;
   FILE *fp;

   UtilRemoveQuotes(url_attr_name);
   UtilRemoveQuotes(query_attr_name);
   UtilRemoveQuotes(result_attr_name);
   sprintf(execDummyStr, "%s=", url_attr_name);
   url_attr = FindAttrWithName(obj_ptr, execDummyStr, NULL);
   if (url_attr == NULL) return BadAttr(execDummyStr, orig_cmd);

   sprintf(execDummyStr, "%s=", query_attr_name);
   query_attr = FindAttrWithName(obj_ptr, execDummyStr, NULL);
   if (query_attr == NULL) return BadAttr(execDummyStr, orig_cmd);

   sprintf(execDummyStr, "%s=", result_attr_name);
   result_attr = FindAttrWithName(obj_ptr, execDummyStr,
         &result_attr_owner_obj);
   if (result_attr == NULL) return BadAttr(execDummyStr, orig_cmd);

   if (!FileIsRemote(url_attr->attr_value.s)) {
      sprintf(gszMsgBox, "'%s' is not a remote file name %s '%s' command.",
            url_attr->attr_value.s, "while executing the", orig_cmd);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   fnameForPostingCGIQuery = (char*)malloc((strlen(TMP_DIR)+20)*sizeof(char));
   if (fnameForPostingCGIQuery == NULL) {
      FailAllocMessage();
      return FALSE;
   }
   sprintf(fnameForPostingCGIQuery, "%sTgifXXXXXX", TMP_DIR);
   mktemp(fnameForPostingCGIQuery);
   unlink(fnameForPostingCGIQuery);
   if ((fp=fopen(fnameForPostingCGIQuery, "w")) == NULL) {
      sprintf(gszMsgBox, "Fail to open '%s' for write %s '%s' command.",
            fnameForPostingCGIQuery, "while executing the", orig_cmd);
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);

      free(fnameForPostingCGIQuery);
      fnameForPostingCGIQuery = NULL;
      return FALSE;
   }
   writeFileFailed = FALSE;
   fprintf(fp, "%s", query_attr->attr_value.s);
   if (query_attr->obj->detail.t->first != NULL) {
      struct StrRec *str_ptr;

      for (str_ptr=query_attr->obj->detail.t->first->next; str_ptr != NULL;
            str_ptr = str_ptr->next) {
         if (fprintf(fp, "\r\n%s", str_ptr->dyn_str.s) == EOF) {
            writeFileFailed = TRUE;
         }
      }
   }
   fclose(fp);
   if (writeFileFailed) {
      sprintf(gszMsgBox, "Fail to write to '%s' %s '%s' command.\n\n%s.",
            fnameForPostingCGIQuery, "while executing the", orig_cmd,
            "File system may be full");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);

      unlink(fnameForPostingCGIQuery);
      free(fnameForPostingCGIQuery);
      fnameForPostingCGIQuery = NULL;
      return FALSE;
   }
   postingCGIQuery = TRUE;
   SaveStatusStrings();
   rc = LoadRemoteFileInMem(url_attr->attr_value.s, &remote_buf, NULL,
         &remote_buf_sz, &is_html, TRUE);
   RestoreStatusStrings();
   postingCGIQuery = FALSE;
   unlink(fnameForPostingCGIQuery);
   free(fnameForPostingCGIQuery);
   fnameForPostingCGIQuery = NULL;

   if (!rc) {
      sprintf(gszMsgBox, "Unexpected error %s '%s' command.\n\n%s!",
            "when executing the", orig_cmd,
            "Command execution aborted");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   tmp_remote_fname = WriteRemoteFileIntoTemp(remote_buf, remote_buf_sz, NULL);
   if (tmp_remote_fname == NULL) {
      return FALSE;
   }
   SaveStatusStrings();
   rc = JustReadFileIntoAttr(result_attr, result_attr_owner_obj,
         tmp_remote_fname, orig_cmd);
   RestoreStatusStrings();

   if (remote_buf != NULL) FreeRemoteBuf(remote_buf);
   if (tmp_remote_fname != NULL) {
      unlink(tmp_remote_fname);
      FreeRemoteBuf(tmp_remote_fname);
   }
   return rc;
}

void ExecNavigateBack()
   /* navigate_back(); */
{
   execNavigateBack = TRUE;
}

void ExecStop()
   /* stop(); */
{
   gnAbortExec = TRUE;
   sprintf(gszMsgBox, "stop() executed.");
   Msg(gszMsgBox);
}

int ExecSqrt(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* sqrt(attr_name,expr); */
{
   char *attr_name=argv[0], *expr=argv[1], buf[40];
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;
   struct VRec v;
   double d_val=(double)0.0;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(expr);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   if (!EvalExpr(expr, &v)) return FALSE;

   switch (v.vtype) {
   case INT_VAL: d_val = (double)v.val.i; break;
   case DBL_VAL: d_val = (double)v.val.d; break;
   case NULL_VAL:
   case STR_VAL:
      sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
            "Invalid evaluation", expr,
            "(numeric value expected) when executing the",
            orig_cmd, "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      if (v.vtype == STR_VAL && v.val.s != NULL) free(v.val.s);
      return FALSE;
   }
   if (d_val < (double)0.0) {
      sprintf(gszMsgBox, "%s %s: %s.",
            orig_cmd, "domain error",
            "cannot take square-root of a negative number");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   d_val = (double)sqrt((double)d_val);
   sprintf(buf, "%.12lf", d_val);
   ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, buf);
   return TRUE;
}

#ifndef _NO_EXTERN
#ifdef _AIX
extern void srand ARGS_DECL((unsigned int));
#else /* !_AIX */
#ifdef __alpha
extern void srand ARGS_DECL((unsigned int));
#else /* !__alpha */
#ifdef sgi
extern void srand ARGS_DECL((unsigned int));
#else
extern int srand ARGS_DECL((int));
#endif /* sgi */
#endif /* __alpha */
#endif /* _AIX */
#endif /* !_NO_EXTERN */
extern int rand ARGS_DECL((void));

static int gnSeeded=FALSE;

int ExecRandom(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* random(attr_name); */
{
   char *attr_name=argv[0], buf[40];
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;

   UtilRemoveQuotes(attr_name);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   if (!gnSeeded) {
      gnSeeded = TRUE;
      srand(0);
      rand();
   }
   sprintf(buf, "%1d", rand());
   ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, buf);
   return TRUE;
}

int ExecRound(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* round(attr_name,expr); */
{
   char *attr_name=argv[0], *expr=argv[1], buf[40];
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;
   struct VRec v;
   double d_val=(double)0.0;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(expr);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   if (!EvalExpr(expr, &v)) return FALSE;

   switch (v.vtype) {
   case INT_VAL: d_val = (double)v.val.i; break;
   case DBL_VAL: d_val = (double)v.val.d; break;
   case NULL_VAL:
   case STR_VAL:
      sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
            "Invalid evaluation", expr,
            "(numeric value expected) when executing the",
            orig_cmd, "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      if (v.vtype == STR_VAL && v.val.s != NULL) free(v.val.s);
      return FALSE;
   }
   sprintf(buf, "%1d", round(d_val));
   ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, buf);
   return TRUE;
}

int ExecRedrawObj(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* redraw_obj(obj_name); */
{
   char *obj_name=argv[0];
   struct ObjRec *owner_obj=NULL, *top_owner=NULL, *named_obj;
   int ltx, lty, rbx, rby;

   UtilRemoveQuotes(obj_name);
   named_obj = FindObjWithName(botObj, obj_ptr, obj_name, FALSE,
         FALSE, &owner_obj, &top_owner);
   if (named_obj == NULL) return BadObjName(obj_name, orig_cmd);

   ltx = named_obj->bbox.ltx; lty = named_obj->bbox.lty;
   rbx = named_obj->bbox.rbx; rby = named_obj->bbox.rby;
   RedrawAnArea(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
         rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1));

   return TRUE;
}

void ExecRedrawDrawingArea()
   /* redraw_drawing_area(); */
{
   RedrawDrawWindow(botObj);
}

int ExecIntToHex(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* itox(attr_name,digits,expr); */
{
   char *attr_name=argv[0], *digits_str=argv[1], *expr=argv[2];
   char buf[40], buf1[40];
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;
   struct VRec v;
   int digits=0, i_val=0;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(digits_str);
   UtilRemoveQuotes(expr);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   if (!IntExpression(digits_str, &digits, orig_cmd)) {
      return FALSE;
   }
   if (digits <= 0 || digits > 8) {
      sprintf(gszMsgBox, "%s: '%s' %s '%s' %s.\n\n%s.",
            "Out of range <digits> argument", digits_str,
            "encountered while executing the", orig_cmd, "command",
            "The value should be between 1 and 8 (inclusive)");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }

   if (!EvalExpr(expr, &v)) return FALSE;

   switch (v.vtype) {
   case INT_VAL: i_val = v.val.i; break;
   case DBL_VAL: i_val = round(v.val.d); break;
   case NULL_VAL:
   case STR_VAL:
      sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
            "Invalid evaluation", expr,
            "(numeric value expected) when executing the",
            orig_cmd, "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      if (v.vtype == STR_VAL && v.val.s != NULL) free(v.val.s);
      return FALSE;
   }
   sprintf(buf1, "%%0%1dx", digits);
   sprintf(buf, buf1, i_val);
   ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, buf);
   return TRUE;
}

int ExecForI(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* for_i(attr_name,min_val,max_val,inc,attr_to_exec); */
{
   char *attr_name=argv[0], *min_str=argv[1], *max_str=argv[2];
   char *inc_str=argv[3], *exec_attr_name=argv[4], num_buf[40];
   char *compare_buf=NULL, *assign_buf=NULL;
   struct AttrRec *attr_ptr=NULL, *exec_attr_ptr=NULL;
   struct ObjRec *attr_owner_obj=NULL;
   int min_val=0, max_val=0, inc_val=1, ok=TRUE;

   UtilRemoveQuotes(attr_name);
   UtilRemoveQuotes(min_str);
   UtilRemoveQuotes(max_str);
   UtilRemoveQuotes(inc_str);
   UtilRemoveQuotes(exec_attr_name);
   if (strcmp(attr_name, "NULL") != 0) {
      sprintf(execDummyStr, "%s=", attr_name);
      attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
      if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);
   }
   sprintf(execDummyStr, "%s=", exec_attr_name);
   exec_attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, NULL);
   if (exec_attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   if (!IntExpression(min_str, &min_val, orig_cmd) ||
         !IntExpression(max_str, &max_val, orig_cmd) ||
         !IntExpression(inc_str, &inc_val, orig_cmd)) {
      return FALSE;
   }
   if (attr_ptr != NULL) {
      compare_buf = (char*)malloc((strlen(attr_name)+40)*sizeof(char));
      assign_buf = (char*)malloc((strlen(attr_name)+40)*sizeof(char));
      if (compare_buf == NULL || assign_buf == NULL) {
         FailAllocMessage();
         if (compare_buf != NULL) free(compare_buf);
         if (assign_buf != NULL) free(assign_buf);
         return FALSE;
      }
      sprintf(compare_buf, "$(%s) <= %1d", attr_name, max_val);
      sprintf(assign_buf, "$(%s)", attr_name);

      sprintf(num_buf, "%1d", min_val);
      ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, num_buf);
   }
   while (ok) {
      if (attr_ptr != NULL) {
         int val;
         char *expr=convert_str(compare_buf, obj_ptr, TRUE);

         if (expr == NULL) {
            BadAttr(compare_buf, orig_cmd);
            free(compare_buf);
            return FALSE;
         } else if (!IntExpression(expr, &val, orig_cmd)) {
            free(expr);
            free(compare_buf);
            return FALSE;
         }
         free(expr);
         if (val == 0) break;
      } else {
         if (min_val > max_val) break;
      }
      if (!DoExec(exec_attr_ptr, obj_ptr)) return FALSE;
      if (attr_ptr != NULL) {
         int val;
         char *expr=convert_str(assign_buf, obj_ptr, TRUE);

         if (expr == NULL) {
            BadAttr(assign_buf, orig_cmd);
            free(assign_buf);
            return FALSE;
         } else if (!IntExpression(expr, &val, orig_cmd)) {
            free(expr);
            free(assign_buf);
            return FALSE;
         }
         free(expr);
         min_val = val + inc_val;

         sprintf(num_buf, "%1d", min_val);
         ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, num_buf);
      } else {
         min_val += inc_val;
      }
      ok = (!CheckExecInterrupt(FALSE, orig_cmd));
   }
   if (compare_buf != NULL) free(compare_buf);
   if (assign_buf != NULL) free(assign_buf);
   return ok;
}

void ExecSetFileNotModified()
   /* set_file_not_modified(); */
{
   justDupped = FALSE;
   SetFileModified(FALSE);
   RedrawTitleWindow();
}

int ExecNewId(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* new_id(attr_name); */
{
   char *attr_name=argv[0], buf[40];
   struct AttrRec *attr_ptr;
   struct ObjRec *attr_owner_obj=NULL;

   UtilRemoveQuotes(attr_name);
   sprintf(execDummyStr, "%s=", attr_name);
   attr_ptr = FindAttrWithName(obj_ptr, execDummyStr, &attr_owner_obj);
   if (attr_ptr == NULL) return BadAttr(execDummyStr, orig_cmd);

   sprintf(buf, "%1d", objId++);
   ReplaceAttrFirstValue(attr_owner_obj, attr_ptr, buf);
   return TRUE;
}

int ExecRotateSelObj(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* rotate_selected_obj(angle); */
{
   char *expr=argv[0];
   struct VRec v;
   double d_val=(double)0.0;

   UtilRemoveQuotes(expr);
   if (topSel == NULL) return BadSelectedObj(orig_cmd);

   if (!EvalExpr(expr, &v)) return FALSE;

   switch (v.vtype) {
   case INT_VAL: d_val = (double)v.val.i; break;
   case DBL_VAL: d_val = (double)v.val.d; break;
   case NULL_VAL:
   case STR_VAL:
      sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
            "Invalid evaluation", expr,
            "(numeric value expected) when executing the",
            orig_cmd, "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      if (v.vtype == STR_VAL && v.val.s != NULL) free(v.val.s);
      return FALSE;
   }
   RotateAllSelObj(d_val);
   return TRUE;
}

int ExecCallSimpleShortCut(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* call_simple_shortcut(shortcut_name); */
{
   char *shortcut_name=argv[0], *func_name=NULL, *buf=NULL, code='\0';
   unsigned int state;
   int len;

   UtilRemoveQuotes(shortcut_name);

   if (strcmp(shortcut_name, "Quit") == 0 ||
         !ValidShortCut(shortcut_name, 0, &code, &state)) {
      sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
            "Invalid shortcut name", shortcut_name,
            "when executing the", orig_cmd, "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   len = strlen(shortcut_name);
   if ((func_name=(char*)malloc((len+3)*sizeof(char))) == NULL) {
      FailAllocMessage();
      return FALSE;
   }
   sprintf(func_name, "%s()", shortcut_name);
   if ((buf=UtilStrDup(func_name)) == NULL) {
      free(func_name);
      FailAllocMessage();
      return FALSE;
   }
   CallShortCut(func_name, 1, &buf, &code, state);
   free(buf);
   free(func_name);
   return TRUE;
}

int ExecCallOneArgShortCut(argv, obj_ptr, orig_cmd)
   char **argv, *orig_cmd;
   struct ObjRec *obj_ptr;
   /* call_one_arg_shortcut(shortcut_name); */
{
   char *shortcut_name=argv[0], *arg=argv[1], **ppsz_buf=NULL, code='\0';
   char *func_name=NULL;
   unsigned int state;
   int len;

   UtilRemoveQuotes(shortcut_name);
   UtilRemoveQuotes(arg);

   if (strcmp(shortcut_name, "Quit") == 0 ||
         !ValidShortCut(shortcut_name, 1, &code, &state)) {
      sprintf(gszMsgBox, "%s '%s' %s '%s' %s.",
            "Invalid shortcut name", shortcut_name,
            "when executing the", orig_cmd, "command");
      MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
      return FALSE;
   }
   len = strlen(shortcut_name);
   if ((func_name=(char*)malloc((len+3)*sizeof(char))) == NULL) {
      FailAllocMessage();
      return FALSE;
   }
   sprintf(func_name, "%s()", shortcut_name);
   if ((ppsz_buf=(char**)malloc(2*sizeof(char*))) == NULL) {
      free(func_name);
      FailAllocMessage();
      return FALSE;
   }
   if ((ppsz_buf[0]=UtilStrDup(func_name)) == NULL) {
      free(ppsz_buf);
      free(func_name);
      FailAllocMessage();
      return FALSE;
   }
   len = strlen(arg);
   if ((ppsz_buf[1]=(char*)malloc((len+2)*sizeof(char))) == NULL) {
      free(ppsz_buf[0]);
      free(ppsz_buf);
      free(func_name);
      FailAllocMessage();
      return FALSE;
   }
   sprintf(ppsz_buf[1], "%s)", arg);
   CallShortCut(func_name, 2, ppsz_buf, &code, state);
   free(ppsz_buf[1]);
   free(ppsz_buf[0]);
   free(ppsz_buf);
   free(func_name);
   return TRUE;
}

/* --------------------- End of Exec Routines --------------------- */

static
char *ExecFreeArgv(need_raw_argv, argc, p_argv, p_raw_argv)
   int need_raw_argv, argc;
   char ***p_argv, ***p_raw_argv;
{
   register int i;

   for (i=0; i < argc; i++) {
      if ((*p_argv)[i] == NULL) break;
      free((*p_argv)[i]);
   }
   if (*p_argv != NULL) free(*p_argv);
   if (need_raw_argv) {
      for (i=0; i < argc; i++) {
         if ((*p_raw_argv)[i] == NULL) break;
         free((*p_raw_argv)[i]);
      }
      if (*p_raw_argv != NULL) free(*p_raw_argv);
   }
   return NULL;
}

static
void ShowExecStatus(need_raw_argv, argc, argv, raw_argv, func_name)
   int need_raw_argv, argc;
   char **argv, **raw_argv, *func_name;
{
   register int i, index;

   sprintf(execDummyStr, "EXEC: %s", func_name);
   index = strlen(execDummyStr);
   for (i=0; index < sizeof(execDummyStr)-7 && i < argc; i++) {
      int len=strlen(need_raw_argv ? raw_argv[i] : argv[i]);

      if (index+len+1 >= sizeof(execDummyStr)-7) {
         break;
      }
      if (i == 0) {
         execDummyStr[index++] = '(';
      } else {
         execDummyStr[index++] = ',';
      }
      strcpy(&execDummyStr[index], need_raw_argv ? raw_argv[i] : argv[i]);
      index += len;
   }
   if (i == argc) {
      execDummyStr[index++] = ')';
      execDummyStr[index] = '\0';
   } else {
      strcpy(&execDummyStr[index], ",...)");
   }
   SetStringStatus(execDummyStr);
   XSync(mainDisplay, False);
}

static
char *ExecuteACommand(cmd_ptr, obj_ptr)
   char * cmd_ptr;
   struct ObjRec * obj_ptr;
{
   struct ExecInfoRec *pei;
   int i, tok_type, rc=TRUE, func_argc=0, need_raw_argv=FALSE;
   char buf[MAXSTRING+1], **argv=NULL, **raw_argv=NULL;
   char *c_ptr=GetToken(cmd_ptr, buf, &tok_type);

   if (c_ptr == NULL) return NULL;
   if (*c_ptr == '\0') return c_ptr;
   for (pei=gExecInfo; pei->pfunc != NULL; pei++) {
      if (strcmp(pei->func_name, buf) == 0) {
         break;
      }
   }
   if (pei->pfunc == NULL) {
      int len=strlen(buf);

      if (len > 0 && buf[len-1] == ' ') {
         sprintf(execDummyStr, "%s: '%s' (%s).\n\n%s.",
              "Unrecognized command", buf,
              "may be caused by illegal trailing blanks in command name",
              "Command execution aborted");
      } else {
         sprintf(execDummyStr, "%s: '%s'.\n\n%s.\n\n%s %s %s.\n%s %s %s.",
              "Unrecognized command", buf, "Command execution aborted",
              "It is possible that the command is supported",
              "in a newer version of", TOOL_NAME, "Please check out",
              "http://bourbon.cs.ucla.edu:8001/tgif/current.html",
              "for the current release");
      }
      MsgBox(execDummyStr, TOOL_NAME, INFO_MB);
      return NULL;
   }
   func_argc = pei->func_argc;
   if (func_argc < 0) {
      func_argc = (-func_argc);
      need_raw_argv = TRUE;
   }
   if (func_argc > 0) {
      if (need_raw_argv) {
         raw_argv = (char**)malloc(func_argc*sizeof(char*));
         if (raw_argv == NULL) return (char*)FailAllocMessage();
         for (i=0; i < func_argc; i++) raw_argv[i] = NULL;
      }
      argv = (char**)malloc(func_argc*sizeof(char*));
      if (argv == NULL) return (char*)FailAllocMessage();
      for (i=0; i < func_argc; i++) argv[i] = NULL;
   }
   for (i=0; i < func_argc; i++) {
      if ((c_ptr=GetToken(c_ptr, buf, &tok_type)) == NULL ||
            tok_type == TOK_EMPTY) {
         BadCmd(pei->func_name);
         return ExecFreeArgv(need_raw_argv, func_argc, &argv, &raw_argv);
      }
      if (i == 0) {
         if (tok_type != TOK_LEFT_P) {
            BadCmd(pei->func_name);
            return ExecFreeArgv(need_raw_argv, func_argc, &argv, &raw_argv);
         }
      } else {
         if (tok_type != TOK_COMMA) {
            BadCmd(pei->func_name);
            return ExecFreeArgv(need_raw_argv, func_argc, &argv, &raw_argv);
         }
      }
      if ((c_ptr=GetToken(c_ptr, buf, &tok_type)) == NULL ||
            tok_type == TOK_EMPTY) {
         BadCmd(pei->func_name);
         return ExecFreeArgv(need_raw_argv, func_argc, &argv, &raw_argv);
      }
      if (tok_type != TOK_STR) {
         BadCmd(pei->func_name);
         return ExecFreeArgv(need_raw_argv, func_argc, &argv, &raw_argv);
      }
      if (need_raw_argv) {
         raw_argv[i] = UtilStrDup(buf);
         if (raw_argv[i] == NULL) {
            FailAllocMessage();
            return ExecFreeArgv(need_raw_argv, func_argc, &argv, &raw_argv);
         }
      }
      argv[i] = convert_str(buf, obj_ptr, pei->double_quotes_for_null);

      if (argv[i] == NULL) {
         BadAttr(buf, pei->func_name);
         return ExecFreeArgv(need_raw_argv, func_argc, &argv, &raw_argv);
      }
   }
   if (func_argc == 0) {
      if ((c_ptr=GetToken(c_ptr, buf, &tok_type)) == NULL ||
            tok_type == TOK_EMPTY) {
         BadCmd(pei->func_name);
         return ExecFreeArgv(need_raw_argv, func_argc, &argv, &raw_argv);
      }
      if (tok_type != TOK_LEFT_P) {
         BadCmd(pei->func_name);
         return ExecFreeArgv(need_raw_argv, func_argc, &argv, &raw_argv);
      }
   }
   if ((c_ptr=GetToken(c_ptr, buf, &tok_type)) == NULL ||
         tok_type == TOK_EMPTY) {
      BadCmd(pei->func_name);
      return ExecFreeArgv(need_raw_argv, func_argc, &argv, &raw_argv);
   }
   if (tok_type != TOK_RIGHT_P) {
      BadCmd(pei->func_name);
      return ExecFreeArgv(need_raw_argv, func_argc, &argv, &raw_argv);
   }

   ShowExecStatus(need_raw_argv, func_argc, argv, raw_argv, pei->func_name);
   if (func_argc > 0) {
      if (need_raw_argv) {
         rc = ((RawExecFunc*)(pei->pfunc))(argv, raw_argv, obj_ptr,
               pei->func_name);
      } else {
         rc = ((ExecFunc*)(pei->pfunc))(argv, obj_ptr, pei->func_name);
      }
   } else if (func_argc == 0) {
      gnAbortExec = FALSE;
      ((SimpleExecFunc*)(pei->pfunc))();
      if (gnAbortExec) rc = FALSE;
   }
   ExecFreeArgv(need_raw_argv, func_argc, &argv, &raw_argv);
   return (rc ? c_ptr : NULL);
}

int DoExec(exec_attr, obj_ptr)
   struct AttrRec *exec_attr;
   struct ObjRec *obj_ptr;
{
   char *c_ptr, *cmd, *cmd_ptr, buf[MAXSTRING+1];
   int cur_size=2*MAXSTRING, count=0, cmd_len=0, n;
   int first_time=TRUE, tok_type, rc=TRUE;
   struct StrRec *str_ptr;

   cmd = (char*)malloc((cur_size+4)*sizeof(char));
   if (cmd == NULL) FailAllocMessage();
   cmd_ptr = cmd;
   for (str_ptr = exec_attr->obj->detail.t->first; str_ptr != NULL;
         str_ptr = str_ptr->next) {
      if (first_time) {
         first_time = FALSE;
         c_ptr = exec_attr->attr_value.s;
      } else {
         c_ptr = str_ptr->dyn_str.s;
      }
      if ((count=strlen(c_ptr)) != 0) {
         if (count+cmd_len >= cur_size) {
            n = cmd_ptr-cmd;
            cur_size += count+MAXSTRING;
            cmd = (char*)realloc(cmd, (cur_size+4)*sizeof(char));
            cmd_ptr = &cmd[n];
         }
         strncpy(cmd_ptr, c_ptr, count);
         cmd_ptr += count;
         cmd_len += count;
         if (str_ptr->next != NULL) {
            *cmd_ptr++ = ' ';
            *cmd_ptr = '\0';
            cmd_len++;
         }
      }
   }
   *cmd_ptr = '\0';
   justDupped = FALSE;
   execCurDepth++;
   StartCompositeCmd();

   cmd_ptr = cmd;
   while (rc && *cmd_ptr != '\0') {
      gnSeenLeftParan = FALSE;
      if ((cmd_ptr=ExecuteACommand(cmd_ptr, obj_ptr)) == NULL) {
         rc = FALSE;
      } else if (*cmd_ptr != '\0') {
         if (execNavigateBack) {
            break;
         } else if (warpToAttr != NULL) {
            break;
         } else if (OnlyBlanksLeft(cmd_ptr)) {
            break;
         } else if ((cmd_ptr=GetToken(cmd_ptr, buf, &tok_type)) == NULL) {
            rc = FALSE;
         } else if (tok_type != TOK_SEMI) {
            MsgBox("Unrecognized command.  Command execution aborted.",
                  TOOL_NAME, INFO_MB);
            rc = FALSE;
         }
      }
      if (rc && CheckExecInterrupt(FALSE, NULL)) {
         Msg("User interrupt.");
         rc = FALSE;
      }
   }
   EndCompositeCmd();
   execCurDepth--;
   justDupped = FALSE;
   free(cmd);

   return rc;
}

void CleanUpExec()
{
   if (cmdToExecAfterHyperJump != NULL) {
      free(cmdToExecAfterHyperJump);
      cmdToExecAfterHyperJump = NULL;
   }
}

