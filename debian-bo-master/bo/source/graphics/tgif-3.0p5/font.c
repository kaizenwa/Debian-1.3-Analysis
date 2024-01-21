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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/font.c,v 3.1 1996/05/12 08:08:26 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "auxtext.e"
#include "choice.e"
#include "cmd.e"
#include "color.e"
#include "cursor.e"
#include "dialog.e"
#include "drawing.e"
#include "exec.e"
#include "file.e"
#ifndef _NO_EXTERN
#include "font.e"
#endif
#include "mainmenu.e"
#include "mark.e"
#include "menu.e"
#include "msg.e"
#include "obj.e"
#include "pattern.e"
#include "page.e"
#include "ps.e"
#include "raster.e"
#include "select.e"
#include "setup.e"
#include "text.e"
#include "util.e"
#include "xpixmap.e"

extern int	atoi ARGS_DECL((char *));

#define DEFAULT_FONT_SIZE 14

struct FontSizeRec {
   XFontStruct		* xfs;
   int			size, faked;
   struct FontSizeRec	* next;
};

struct FontFmlyRec {
   struct FontSizeRec	fr[MAXFONTSTYLES];
         /* fr[i] points to the default size used for the given font */
         /* this font is used if the requested size is not found */
   char			* name_faked;
};

XFontStruct	* canvasFontPtr=NULL;
int	canvasFontHeight=0;
int	canvasFontAsc=0;
int	canvasFontDes=0;
int	canvasFontDirection=0;
int	canvasFontDoubleByte=FALSE;
int	canvasFontSize=0;
int	canvasFontIsFaked=FALSE;

XFontStruct	* rulerFontPtr=NULL;
int	rulerFontWidth=0;
int	rulerFontHeight=0;
int	rulerFontAsc=0;
int	rulerFontDes=0;
static char *rulerFontName=NULL;

XFontStruct	* defaultFontPtr=NULL;
int	defaultFontWidth=0;
int	defaultFontHeight=0;
int	defaultFontAsc=0;
int	defaultFontDes=0;
static char *defaultFontName=NULL;

XFontStruct	* menuFontPtr=NULL;
int	menuFontWidth=0;
int	menuFontHeight=0;
int	menuFontAsc=0;
int	menuFontDes=0;
static char *menuFontName=NULL;

XFontStruct	* boldMsgFontPtr=NULL;
int	boldMsgFontWidth=0;
int	boldMsgFontHeight=0;
int	boldMsgFontAsc=0;
int	boldMsgFontDes=0;
static char *boldMsgFontName=NULL;

XFontStruct	* msgFontPtr=NULL;
int	msgFontWidth=0;
int	msgFontHeight=0;
int	msgFontAsc=0;
int	msgFontDes=0;
static char *msgFontName=NULL;

int	curFont=FONT_COU;
int	curSize=20;
int	curStyle=STYLE_NR;
int	curRotate=ROTATE0; /* obsoleted, should always be ROTATE0 */
int	textRotation=0; /* degrees*64 */
int	curUnderlineOn=FALSE;

int	curUnderline = 2;

static int	defaultCurFont=(-1);
static int	defaultCurSize=(-1);
static int	defaultCurStyle=(-1);

static char	* initSizeMenuStr[] = {
      "8 ", "10", "11", "12", "14", "17", "18", "20", "24", "25", "34" };

char	* styleMenuStr[] = {
      "Roman      ^#o",
      "Bold       ^#b",
      "Italic     ^#t",
      "BoldItalic ^#p",
      "--------------",
      "Left       ^#l",
      "Center     ^#c",
      "Right      ^#r" };
char	* * fontMenuStr=NULL;
char	* * sizeMenuStr=NULL;
int	* fontSizes=NULL;
int	numFonts=MAXFONTS;
int	numFontSizes=MAXFONTS;

int	changingFontSizeFromRead=TRUE;
int	attemptingToSetFontProperty=FALSE;

static int	defaultFontSize=DEFAULT_FONT_SIZE;
static int	numFakedFonts=0;
static char	fontNamePrefix[MAXSTRING+1];
static int	hasAlternateDefaultFonts=FALSE;

static struct FontFmlyRec	* fontFamilies=NULL;

static char	* initFontInfoStr[]={
  "times-medium-r-normal", "iso8859-1", "Times-Roman",
  "times-bold-r-normal", "iso8859-1", "Times-Bold",
  "times-medium-i-normal", "iso8859-1", "Times-Italic",
  "times-bold-i-normal", "iso8859-1", "Times-BoldItalic",
  "courier-medium-r-normal", "iso8859-1", "Courier",
  "courier-bold-r-normal", "iso8859-1", "Courier-Bold",
  "courier-medium-o-normal", "iso8859-1", "Courier-Oblique",
  "courier-bold-o-normal", "iso8859-1", "Courier-BoldOblique",
  "helvetica-medium-r-normal", "iso8859-1", "Helvetica",
  "helvetica-bold-r-normal", "iso8859-1", "Helvetica-Bold",
  "helvetica-medium-o-normal", "iso8859-1", "Helvetica-Oblique",
  "helvetica-bold-o-normal", "iso8859-1", "Helvetica-BoldOblique",
  "new century schoolbook-medium-r-normal", "iso8859-1",
        "NewCenturySchlbk-Roman",
  "new century schoolbook-bold-r-normal", "iso8859-1",
        "NewCenturySchlbk-Bold",
  "new century schoolbook-medium-i-normal", "iso8859-1",
        "NewCenturySchlbk-Italic",
  "new century schoolbook-bold-i-normal", "iso8859-1",
        "NewCenturySchlbk-BoldItalic",
  "symbol-medium-r-normal", "adobe-fontspecific", "Symbol",
  "symbol-medium-r-normal", "adobe-fontspecific", "Symbol",
  "symbol-medium-r-normal", "adobe-fontspecific", "Symbol",
  "symbol-medium-r-normal", "adobe-fontspecific", "Symbol",
  NULL, NULL, NULL
};

static char	**altFontInfoStr=NULL;

static char	* * fontInfoStr=NULL;

static char	* charCodeToName[] =
{
/* \200 */ "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
/* \220 */ "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
/* \240 */ "x",
           "8#241 /exclamdown",
           "8#242 /cent",
           "8#243 /sterling",
           "8#244 /currency",
           "8#245 /yen",
           "8#246 /bar",
           "8#247 /section",
           "8#250 /dieresis",
           "8#251 /copyright",
           "8#252 /ordfeminine",
           "8#253 /guillemotleft",
           "8#254 /logicalnot",
           "8#255 /emdash",
           "8#256 /registered",
           "8#257 /macron",
/* \260 */ "8#260 /degree",
           "8#261 /plusminus",
           "8#262 /twosuperior",
           "8#263 /threesuperior",
           "8#264 /acute",
           "8#265 /mu",
           "8#266 /paragraph",
           "8#267 /periodcentered",
           "8#270 /cedilla",
           "x",
           "8#272 /ordmasculine",
           "8#273 /guillemotright",
           "x",
           "x",
           "x",
           "8#277 /questiondown",
/* \300 */ "8#300 /Agrave",
           "8#301 /Aacute",
           "8#302 /Acircumflex",
           "8#303 /Atilde",
           "8#304 /Adieresis",
           "8#305 /Aring",
           "8#306 /AE",
           "8#307 /Ccedilla",
           "8#310 /Egrave",
           "8#311 /Eacute",
           "8#312 /Ecircumflex",
           "8#313 /Edieresis",
           "8#314 /Igrave",
           "8#315 /Iacute",
           "8#316 /Icircumflex",
           "8#317 /Idieresis",
/* \320 */ "8#320 /Eth",
           "8#321 /Ntilde",
           "8#322 /Ograve",
           "8#323 /Oacute",
           "8#324 /Ocircumflex",
           "8#325 /Otilde",
           "8#326 /Odieresis",
           "8#327 /multiply",
           "8#330 /Oslash",
           "8#331 /Ugrave",
           "8#332 /Uacute",
           "8#333 /Ucircumflex",
           "8#334 /Udieresis",
           "8#335 /Yacute",
           "8#336 /Thorn",
           "8#337 /germandbls",
/* \340 */ "8#340 /agrave",
           "8#341 /aacute",
           "8#342 /acircumflex",
           "8#343 /atilde",
           "8#344 /adieresis",
           "8#345 /aring",
           "8#346 /ae",
           "8#347 /ccedilla",
           "8#350 /egrave",
           "8#351 /eacute",
           "8#352 /ecircumflex",
           "8#353 /edieresis",
           "8#354 /igrave",
           "8#355 /iacute",
           "8#356 /icircumflex",
           "8#357 /idieresis",
/* \360 */ "8#360 /eth",
           "8#361 /ntilde",
           "8#362 /ograve",
           "8#363 /oacute",
           "8#364 /ocircumflex",
           "8#365 /otilde",
           "8#366 /odieresis",
           "8#367 /divide",
           "8#370 /oslash",
           "8#371 /ugrave",
           "8#372 /uacute",
           "8#373 /ucircumflex",
           "8#374 /udieresis",
           "8#375 /yacute",
           "8#376 /thorn",
           "8#377 /ydieresis"
};

#define InfoIndex(FontIdx,StyleIdx) (((FontIdx)*MAXFONTSTYLES+(StyleIdx))*3)

static int	debugScalableFonts=FALSE; /* debug scalable font */
static char	gszAttemptedFontName[MAXSTRING+1];

static
struct FontSizeRec *FindFontInfo(font_index, style_index, size)
   int font_index, style_index, size;
{
   struct FontSizeRec *fs_ptr, *prev_fs=NULL, *new_fs_ptr;
   int info_index, watch_cursor;
   XFontStruct *xfs=NULL;

   canvasFontIsFaked = FALSE;
   *gszAttemptedFontName = '\0';
   for (fs_ptr=fontFamilies[font_index].fr[style_index].next; fs_ptr != NULL;
         fs_ptr = fs_ptr->next) {
      if (fs_ptr->size == size) {
         canvasFontIsFaked = fs_ptr->faked;
         return (canvasFontIsFaked ?
               &fontFamilies[font_index].fr[style_index] : fs_ptr);
      }
      if (fs_ptr->size > size) break;
      prev_fs = fs_ptr;
   }
   info_index = InfoIndex(font_index, style_index);
   if (strstr(fontInfoStr[info_index], "%d") != NULL) {
      sprintf(gszAttemptedFontName, fontInfoStr[info_index], size);
   } else {
      sprintf(gszAttemptedFontName, "%s-%s-*-%1d-*-*-*-*-*-%s", fontNamePrefix,
            fontInfoStr[info_index], size, fontInfoStr[info_index+1]);
   }

   watch_cursor = watchCursorOnMainWindow;
   if (!watch_cursor && mainWindow!=None) {
      SetWatchCursor(drawWindow);
      SetWatchCursor(mainWindow);
   }
   xfs = XLoadQueryFont(mainDisplay, gszAttemptedFontName);
   if (xfs != NULL && debugScalableFonts) {
      int i;

      for (i=0; i < (MAXFONTSIZES<<1)-1; i++) {
         if (atoi(initSizeMenuStr[i]) == size) {
            break;
         }
      }
      if (i >= (MAXFONTSIZES<<1)-1) {
         XFreeFont(mainDisplay, xfs);
         xfs = NULL;
      }
   }
   if (xfs == NULL) {
      canvasFontIsFaked = TRUE;
      xfs = fontFamilies[font_index].fr[style_index].xfs;
      if (xfs == NULL) {
         char font_name[MAXSTRING+1];

         if (strstr(fontInfoStr[info_index], "%d") != NULL) {
            sprintf(font_name, fontInfoStr[info_index],
                  defaultFontSize);
         } else {
            sprintf(font_name, "%s-%s-*-%1d-*-*-*-*-*-%s",
                  fontNamePrefix, fontInfoStr[info_index], defaultFontSize,
                  fontInfoStr[info_index+1]);
         }
         xfs = XLoadQueryFont(mainDisplay, font_name);
         fontFamilies[font_index].fr[style_index].xfs = xfs;
         fontFamilies[font_index].fr[style_index].size = defaultFontSize;
         if (defaultCurFont == (-1)) {
            defaultCurFont = font_index;
            defaultCurStyle = style_index;
            defaultCurSize = defaultFontSize;
         }
         if (xfs == NULL) {
            if (!attemptingToSetFontProperty) {
               int default_info_index=InfoIndex(defaultCurFont,defaultCurStyle);

               if (strstr(fontInfoStr[default_info_index], "%d") != NULL) {
                  sprintf(font_name, fontInfoStr[default_info_index],
                        defaultCurSize);
               } else {
                  sprintf(font_name, "%s-%s-*-%1d-*-*-*-*-*-%s",
                        fontNamePrefix, fontInfoStr[default_info_index],
                        defaultCurSize, fontInfoStr[default_info_index+1]);
               }
               xfs = XLoadQueryFont(mainDisplay, font_name);
               fontFamilies[font_index].fr[style_index].xfs = xfs;
               fontFamilies[font_index].fr[style_index].size = defaultCurSize;
               if (xfs == NULL) {
                  if (!watch_cursor && mainWindow!=None) {
                     SetDefaultCursor(mainWindow);
                     ShowCursor();
                  }
                  return NULL;
               }
               sprintf(gszMsgBox, "%s %1d substituted for %s %1d.\n\n%s",
                     fontInfoStr[default_info_index+2], defaultCurSize,
                     fontInfoStr[info_index+2], size,
                     "Such text can not be edited and most likely look ugly.");
               MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
            }
         } else if (changingFontSizeFromRead) {
            sprintf(gszMsgBox, "%s %1d substituted for %s %1d...",
                  fontInfoStr[info_index+2], defaultFontSize,
                  fontInfoStr[info_index+2], size);
            TwoLineMsg(gszMsgBox, "    such text can not be edited.");
         }
      }
   } else if (defaultCurFont == (-1)) {
      defaultCurFont = font_index;
      defaultCurStyle = style_index;
      defaultCurSize = size;
   }
   if (!watch_cursor && mainWindow!=None) {
      SetDefaultCursor(mainWindow);
      ShowCursor();
   }
   if (xfs == NULL) return NULL;
   new_fs_ptr = (struct FontSizeRec *)malloc(sizeof(struct FontSizeRec));
   if (new_fs_ptr == NULL) FailAllocMessage();
   memset(new_fs_ptr, 0, sizeof(struct FontSizeRec));
   new_fs_ptr->next = fs_ptr;
   new_fs_ptr->faked = canvasFontIsFaked;
   new_fs_ptr->xfs = xfs;
   new_fs_ptr->size = size;
   if (prev_fs == NULL) {
      fontFamilies[font_index].fr[style_index].next = new_fs_ptr;
   } else {
      prev_fs->next = new_fs_ptr;
   }
   return (canvasFontIsFaked ? &fontFamilies[font_index].fr[style_index]:
         new_fs_ptr);
}

int ValidCharCode (c_ptr)
   char	* c_ptr;
{
   register int	index = (int)(*c_ptr - '\200');

   if (*charCodeToName[index] == '\0' || *charCodeToName[index] == '8' ||
         *charCodeToName[index] == '\\')
      return (TRUE);
   else
   {
      sprintf (gszMsgBox,
         "Unrecognized character code \\%o.  Character discarded!",
         (*c_ptr)&0xff);
      Msg (gszMsgBox);
      return (FALSE);
   }
}

char * CharCodeTranslate (c_ptr)
   char	* c_ptr;
{
   register int	index = (int)(*c_ptr - '\200');

   if (*charCodeToName[index] == '\\' || *charCodeToName[index] == '8')
      return (charCodeToName[index]);
   else
      return (NULL);
}

static
void RemoveIllegalChars (TextPtr)
   struct TextRec	* TextPtr;
{
   register char	* c_ptr, * dest_c_ptr;
   struct StrRec	* str_ptr;

   for (str_ptr = TextPtr->last; str_ptr != NULL; str_ptr = str_ptr->prev)
   {
      for (c_ptr = dest_c_ptr = str_ptr->dyn_str.s; *c_ptr != '\0'; c_ptr++)
         if (!(((*c_ptr) & 0x80) && !ValidCharCode (c_ptr)))
            *dest_c_ptr++ = *c_ptr;
      *dest_c_ptr = '\0';
   }
}

static int	* encodeFont=NULL;
static short	* * encodeCharFlags=NULL;
static int	encodeCharFlagsAllocated=FALSE;
static int	numEncodeCharFonts=0;

static
void CleanUpEncodeCharFonts ()
{
   register int	i;

   if (encodeCharFlagsAllocated)
   {
      for (i = 0; i < numEncodeCharFonts*MAXFONTSTYLES; i++) {
         free(encodeCharFlags[i]);
      }
      free(encodeFont);
      free(encodeCharFlags);
      encodeCharFlagsAllocated = FALSE;
      numEncodeCharFonts = 0;
      encodeFont = NULL;
      encodeCharFlags = NULL;
   }
}

static
void PrepareText (TextPtr)
   register struct TextRec	* TextPtr;
{
   register char	* c_ptr;
   struct StrRec	* str_ptr;
   int			index, byte_index;
   short		* flag_ptr;

   if (TextPtr->font_name == NULL)
   {
      if (TextPtr->font == FONT_SYM) return;
   }
   else if (strcmp (TextPtr->font_name, "Symbol") == 0)
      return;

   index = TextPtr->font*MAXFONTSTYLES + TextPtr->style;
   for (str_ptr = TextPtr->last; str_ptr != NULL; str_ptr = str_ptr->prev)
   {
      for (c_ptr = str_ptr->dyn_str.s; *c_ptr != '\0'; c_ptr++)
         if (((*c_ptr)&0x80) && *charCodeToName[(int)(*c_ptr-'\200')]=='8')
         {
            encodeFont[index] = TRUE;
            flag_ptr = encodeCharFlags[index];
            byte_index = (int)(((*c_ptr) & 0x7f) / 8);
            flag_ptr[byte_index] |= (1<<(((*c_ptr) & 0x7f) % 8));
         }
   }
}

static
void PrepareObjFontInfo (ObjPtr)
   register struct ObjRec	* ObjPtr;
{
   register struct ObjRec	* obj_ptr;
   register struct AttrRec	* attr_ptr;

   for (obj_ptr = ObjPtr; obj_ptr != NULL; obj_ptr = obj_ptr->prev)
   {
      switch (obj_ptr->type)
      {
         case OBJ_TEXT: PrepareText (obj_ptr->detail.t); break;

         case OBJ_GROUP:
         case OBJ_SYM:
         case OBJ_ICON: PrepareObjFontInfo (obj_ptr->detail.r->last); break;
      }
      for (attr_ptr=obj_ptr->lattr; attr_ptr!=NULL; attr_ptr=attr_ptr->prev)
         PrepareText (attr_ptr->obj->detail.t);
   }
}

void PrepareEightBitFontInfo ()
{
   register struct ObjRec	* obj_ptr;
   register struct AttrRec	* attr_ptr;
   register int			j, i;
   short			* flag_ptr;
   struct PageRec		* page_ptr;
   if (encodeCharFlagsAllocated && numEncodeCharFonts < numFonts)
      CleanUpEncodeCharFonts ();

   if (!encodeCharFlagsAllocated)
   {
      int font_count=0;

      font_count = (PRTGIF ? MAXFONTS+numFakedFonts : numFonts+numFakedFonts);

      encodeCharFlags =
            (short**)malloc(font_count*MAXFONTSTYLES*sizeof(short*));
      if (encodeCharFlags == NULL) FailAllocMessage();
      encodeFont = (int*)malloc(font_count*MAXFONTSTYLES*sizeof(int));
      if (encodeFont == NULL) FailAllocMessage();
      for (i = 0; i < font_count*MAXFONTSTYLES; i++) {
         encodeCharFlags[i] = (short*)malloc(16*sizeof(short));
         if (encodeCharFlags[i] == NULL) FailAllocMessage();
      }
      encodeCharFlagsAllocated = TRUE;
      numEncodeCharFonts = font_count;
   }

   for (i = 0; i < numEncodeCharFonts*MAXFONTSTYLES; i++)
   {
      encodeFont[i] = FALSE;
      flag_ptr = encodeCharFlags[i];
      for (j = 0; j < 16; j++) flag_ptr[j] = 0;
   }

   for (page_ptr = firstPage; page_ptr != NULL; page_ptr = page_ptr->next)
      for (obj_ptr = page_ptr->bot; obj_ptr != NULL; obj_ptr = obj_ptr->prev)
      {
         switch (obj_ptr->type)
         {
            case OBJ_TEXT: PrepareText (obj_ptr->detail.t); break;

            case OBJ_GROUP:
            case OBJ_SYM:
            case OBJ_ICON: PrepareObjFontInfo (obj_ptr->detail.r->last); break;
         }
         for (attr_ptr=obj_ptr->lattr; attr_ptr!=NULL; attr_ptr=attr_ptr->prev)
            PrepareText (attr_ptr->obj->detail.t);
      }
}

int NeedEncode (FontIndex, StyleIndex)
   int	FontIndex, StyleIndex;
{
   if (FontIndex == FONT_SYM)
      return (FALSE);
   else
      return (encodeFont[FontIndex*MAXFONTSTYLES+StyleIndex]);
}

void GetPSFontStr (FontIndex, StyleIndex, FontStr)
   int	FontIndex, StyleIndex;
   char	* FontStr;
{
   if (PRTGIF)
   {
      if (FontIndex < MAXFONTS)
         sprintf (FontStr, "/%s",
               initFontInfoStr[InfoIndex(FontIndex,StyleIndex)+2]);
      else
         sprintf (FontStr, "/%s", fontFamilies[FontIndex].name_faked);
   }
   else
   {
      if (FontIndex < numFonts)
         sprintf (FontStr, "/%s",
               fontInfoStr[InfoIndex(FontIndex,StyleIndex)+2]);
      else
         sprintf (FontStr, "/%s", fontFamilies[FontIndex].name_faked);
   }
}

int GetFontIndex (FontStr, StyleIndex, MustFind)
   char	* FontStr;
   int	StyleIndex;
   int	MustFind;
{
   register int	i;

   if (PRTGIF)
   {
      for (i=0; i < MAXFONTS; i++)
         if (strcmp (initFontInfoStr[(i*MAXFONTSTYLES+StyleIndex)*3+2],
               FontStr) == 0)
            return (i);

      for ( ; i < MAXFONTS+numFakedFonts; i++)
         if (strcmp (fontFamilies[i].name_faked, FontStr) == 0)
            return (i);
   }
   else
   {
      for (i=0; i < numFonts; i++)
         if (strcmp (fontInfoStr[(i*MAXFONTSTYLES+StyleIndex)*3+2],
               FontStr) == 0)
            return (i);

      for ( ; i < numFonts+numFakedFonts; i++)
         if (strcmp (fontFamilies[i].name_faked, FontStr) == 0)
            return (i);
   }
   if (MustFind) return (INVALID);

   if (PRTGIF)
   {
      numFakedFonts++;
      if (fontFamilies == NULL) {
         fontFamilies = (struct FontFmlyRec *)malloc(
               (MAXFONTS+numFakedFonts)*sizeof(struct FontFmlyRec));
      } else {
         fontFamilies = (struct FontFmlyRec *) realloc (fontFamilies,
               (MAXFONTS+numFakedFonts)*sizeof(struct FontFmlyRec));
      }
      if (fontFamilies == NULL) FailAllocMessage();
      fontFamilies[MAXFONTS+numFakedFonts-1].name_faked =
            (char*)malloc((strlen(FontStr)+1)*sizeof(char));
      if (fontFamilies[MAXFONTS+numFakedFonts-1].name_faked == NULL) {
         FailAllocMessage();
      }
      strcpy(fontFamilies[MAXFONTS+numFakedFonts-1].name_faked, FontStr);
      for (i=0; i<MAXFONTSTYLES; i++) {
         fontFamilies[MAXFONTS+numFakedFonts-1].fr[i].next = NULL;
         fontFamilies[MAXFONTS+numFakedFonts-1].fr[i].xfs = NULL;
      }
      return (MAXFONTS+numFakedFonts-1);
   }
   else
   {
      numFakedFonts++;
      if (fontFamilies == NULL) {
         fontFamilies = (struct FontFmlyRec *)malloc(
               (numFonts+numFakedFonts)*sizeof(struct FontFmlyRec));
      } else {
         fontFamilies = (struct FontFmlyRec *) realloc (fontFamilies,
               (numFonts+numFakedFonts)*sizeof(struct FontFmlyRec));
      }
      if (fontFamilies == NULL) FailAllocMessage();
      fontFamilies[numFonts+numFakedFonts-1].name_faked =
            (char*)malloc((strlen(FontStr)+1)*sizeof(char));
      if (fontFamilies[numFonts+numFakedFonts-1].name_faked == NULL) {
         FailAllocMessage();
      }
      strcpy(fontFamilies[numFonts+numFakedFonts-1].name_faked, FontStr);
      for (i=0; i<MAXFONTSTYLES; i++) {
         fontFamilies[numFonts+numFakedFonts-1].fr[i].next = NULL;
         fontFamilies[numFonts+numFakedFonts-1].fr[i].xfs = NULL;
      }
      return (numFonts+numFakedFonts-1);
   }
}

static int gnCurFontMsgNoDisplay=FALSE;

void CurFontMsg()
{
   char	style[20];

   switch (curStyle) {
   case STYLE_BI: strcpy(style, "BoldItalic"); break;
   case STYLE_BR: strcpy(style, "Bold"); break;
   case STYLE_NI: strcpy(style, "Italic"); break;
   case STYLE_NR: strcpy(style, "Roman"); break;
   }
   sprintf(gszMsgBox, "%s-%s-%1dpt", fontMenuStr[curFont], style, curSize);
   if (gnCurFontMsgNoDisplay) return;
   Msg(gszMsgBox);
}

void DumpEightBitFontInfo (FP)
   FILE	* FP;
{
   register int		k, j;
   register short	* flag_ptr, flag;
   int			font_index, style_index;
   char			font_str[MAXSTRING];
   int			font_count;

   font_count = (PRTGIF ? MAXFONTS+numFakedFonts : numFonts+numFakedFonts);

   for (font_index = 0; font_index < font_count; font_index++)
   {
      for (style_index = 0; style_index < MAXFONTSTYLES; style_index++)
      {
         if (NeedEncode (font_index, style_index))
         {
            GetPSFontStr (font_index, style_index, font_str);
            fprintf (FP, "%s-vec [\n", font_str);
            flag_ptr = encodeCharFlags[font_index*MAXFONTSTYLES+style_index];
            for (j = 0; j < 16; j++, flag_ptr++)
            {
               flag = *flag_ptr;
               if ((flag & 0xff) != 0)
               {
                  for (k = 0; k < 8; k++)
                     if (flag & (1<<k))
                        fprintf (FP, " %s\n", charCodeToName[(j<<3)+k]);
               }
            }
            fprintf (FP, " ] def\n");
            fprintf (FP, "%s %s-8 %s-vec tgifReEncodeSmall\n\n", font_str,
                  font_str, &font_str[1]);
            if (preDumpSetup) PSUseReencode(font_str);
         }
      }
   }
}

static int	pointSize75[] = { 8, 10, 12, 14, 18, 24 };
static int	pointSize100[] = { 11, 14, 17, 20, 25, 34 };

int GetCompatibleSize (font_dpi, font_size)
   int	font_dpi, font_size;
{
   switch (font_dpi)
   {
      case FONT_DPI_75: return (pointSize75[font_size]);
      case FONT_DPI_100: return (pointSize100[font_size]);
   }
   return (INVALID);
}

void SetCanvasFont()
{
   struct FontSizeRec *fs_ptr;

   fs_ptr = FindFontInfo(curFont, curStyle, curSize);

   if (!attemptingToSetFontProperty && fs_ptr == NULL) {
      fs_ptr = FindFontInfo(defaultCurFont, defaultCurStyle, defaultCurSize);
   }
   if (attemptingToSetFontProperty && fs_ptr == NULL) {
      canvasFontSize = INVALID;
      return;
   }
   if (fs_ptr == NULL) {
      gnCurFontMsgNoDisplay = TRUE;
      CurFontMsg();
      gnCurFontMsgNoDisplay = FALSE;
      fprintf(stderr,
            "Severe error: Cannot find the %s font,\n\tuse '%s' instead.\n",
            gszMsgBox, defaultFontName);
      canvasFontPtr = defaultFontPtr;
      canvasFontSize = INVALID;
   } else {
      canvasFontPtr = fs_ptr->xfs;
      canvasFontSize = fs_ptr->size;
   }
   canvasFontAsc = canvasFontPtr->max_bounds.ascent;
   canvasFontDes = canvasFontPtr->max_bounds.descent;
   canvasFontDirection = canvasFontPtr->direction;
   canvasFontDoubleByte = (canvasFontPtr->min_byte1 != 0 ||
         canvasFontPtr->max_byte1 != 0);
   canvasFontHeight = canvasFontAsc + canvasFontDes;
   XSetFont(mainDisplay, drawGC, canvasFontPtr->fid);

   textCursorH = canvasFontHeight;
}

struct TmpFontFmlyRec {
   char				* * font_strings;
   struct TmpFontFmlyRec	* next, * prev;
};

void InitFonts ()
{
   register int		i, j;
   int			fmly_index, style_index;
   int			len, ruler_font_size;
   char			* c_ptr, * buf;
   struct FontSizeRec	* fs_ptr;

   debugScalableFonts = FALSE;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"DebugScalableFonts")) != NULL)
      if (strcmp ("True", c_ptr) == 0 || strcmp ("true", c_ptr) == 0)
         debugScalableFonts = TRUE;

   defaultFontSize = DEFAULT_FONT_SIZE;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"DefaultFontSize")) != NULL)
   {
      if ((defaultFontSize = atoi (c_ptr)) <= 0)
      {
         defaultFontSize = DEFAULT_FONT_SIZE;
         fprintf (stderr, "%s '%s*DefaultFontSize'.  %1d used.\n",
               "Warning:  Error in processing", TOOL_NAME, DEFAULT_FONT_SIZE);
      }
   }

   strcpy (fontNamePrefix, "-*");
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"FontNamePrefix")) != NULL)
      strcpy (fontNamePrefix, c_ptr);

   fontSizes = NULL;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"FontSizes")) != NULL)
   {
      char	* size_ptr;
      int	allocated=50, ok=TRUE;

      numFontSizes = 0;
      fontSizes = (int*)malloc(allocated*sizeof(int));
      if (fontSizes == NULL) FailAllocMessage();
      sizeMenuStr = (char**)malloc(allocated*sizeof(char*));
      if (sizeMenuStr == NULL) FailAllocMessage();
      len = strlen(c_ptr);
      buf = (char*)malloc((len+1)*sizeof(char));
      if (buf == NULL) FailAllocMessage();
      strcpy(buf, c_ptr);
      if ((size_ptr=strtok(buf," ,\t\n\r")) != NULL)
      {
         while (ok && size_ptr != NULL)
         {
            int	size=atoi(size_ptr);

            if (size <= 0 || size>=1000)
            {
               ok = FALSE;
               break;
            }
            if (numFontSizes >= allocated)
            {
               allocated += 50;
               fontSizes = (int *) realloc (fontSizes, allocated*sizeof(int));
               sizeMenuStr = (char **) realloc (sizeMenuStr,
                     allocated*sizeof(char*));
            }
            sizeMenuStr[numFontSizes] = (char*)malloc(4*sizeof(char));
            if (sizeMenuStr[numFontSizes] == NULL) FailAllocMessage();
            sprintf(sizeMenuStr[numFontSizes], "%-3d", size);
            fontSizes[numFontSizes++] = size;
            size_ptr = strtok (NULL, " ,\t\n\r");
         }
      }
      free(buf);
      if (!ok || numFontSizes <= 0) {
         for (i=0; i < numFontSizes; i++) free(sizeMenuStr[i]);
         free(sizeMenuStr);
         sizeMenuStr = NULL;
         free(fontSizes);
         fontSizes = NULL;
         numFontSizes = 0;
         fprintf (stderr, "%s '%s*FontSizes'.  %s.\n",
               "Warning:  Error in processing", TOOL_NAME,
               "Default font sizes used");
      }
   }
   if (fontSizes == NULL)
   {
      numFontSizes = (MAXFONTSIZES<<1)-1;
      fontSizes = (int*)malloc(numFontSizes*sizeof(int));
      if (fontSizes == NULL) FailAllocMessage();
      sizeMenuStr = (char**)malloc(numFontSizes*sizeof(char*));
      if (sizeMenuStr == NULL) FailAllocMessage();
      for (i=0; i<numFontSizes; i++) {
         fontSizes[i] = atoi(initSizeMenuStr[i]);
         sizeMenuStr[i] = (char*)malloc(4*sizeof(char));
         if (sizeMenuStr[i] == NULL) FailAllocMessage();
         strcpy(sizeMenuStr[i], initSizeMenuStr[i]);
      }
   }

   hasAlternateDefaultFonts = FALSE;
   if ((c_ptr=XGetDefault (mainDisplay, TOOL_NAME,
         "HasAlternateDefaultFonts")) != NULL)
      if ((strcmp (c_ptr, "true") == 0) || (strcmp (c_ptr, "True") == 0))
         hasAlternateDefaultFonts = TRUE;

   fontFamilies = NULL;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"AdditionalFonts")) != NULL)
   {
      char	* font_ptr;

      len = strlen (c_ptr);
      buf = (char*)malloc((len+1)*sizeof(char));
      if (buf == NULL) FailAllocMessage();
      strcpy(buf, c_ptr);
      for (font_ptr=buf; *font_ptr != '\0' &&
            strchr(" ,\t\n\r",*font_ptr) != NULL; font_ptr++) ;
      if (font_ptr != NULL && *font_ptr != '\0')
      {
         int	ok=TRUE, num_new_fonts=0, index;
         char	* * font_strings;
         struct TmpFontFmlyRec	* first_fmly=NULL, * last_fmly=NULL, * fmly_ptr;

         while (ok && font_ptr != NULL && *font_ptr != '\0') {
            font_strings = (char**)malloc((MAXFONTSTYLES*3)*sizeof(char *));
            if (font_strings == NULL) FailAllocMessage();
            for (i=0, index=0; i<MAXFONTSTYLES; i++)
            {
               char *rgstry=NULL, *ps_name=NULL, *dup_ptr;

               dup_ptr = (font_ptr == NULL ? NULL : UtilStrDup(font_ptr));
               if ((font_ptr=strtok(font_ptr,",\t\n\r")) != NULL &&
                     (rgstry=strtok(NULL,",\t\n\r")) != NULL &&
                     (ps_name=strtok(NULL," ,\t\n\r")) != NULL) {
                  font_strings[index] = (char*)malloc(81*sizeof(char));
                  if (font_strings[index] == NULL) FailAllocMessage();
                  strcpy (font_strings[index], font_ptr);
                  UtilTrimBlanks (font_strings[index++]);
                  font_strings[index] = (char*)malloc(81*sizeof(char));
                  if (font_strings[index] == NULL) FailAllocMessage();
                  strcpy (font_strings[index], rgstry);
                  UtilTrimBlanks (font_strings[index++]);
                  font_strings[index] = (char*)malloc(81*sizeof(char));
                  if (font_strings[index] == NULL) FailAllocMessage();
                  strcpy (font_strings[index], ps_name);
                  UtilTrimBlanks (font_strings[index++]);
                  for (font_ptr=(&ps_name[strlen(ps_name)+1]);
                        *font_ptr != '\0' && 
                        strchr(" ,\t\n\r",*font_ptr) != NULL; font_ptr++) ;
               }
               else
               {
                  int dup_len=(dup_ptr == NULL ? 0 : strlen(dup_ptr));

                  if (dup_len > 128) {
                     strcpy(&dup_ptr[128-3], "...");
                  }
                  ok = FALSE;
                  *gszMsgBox = '\0';
                  if (dup_ptr == NULL) {
                     sprintf(gszMsgBox, "Expecting font specification");
                  } else if (font_ptr == NULL) {
                     sprintf(gszMsgBox,
                           "Invalid X font information in '%s'", dup_ptr);
                  } else if (rgstry == NULL) {
                     sprintf(gszMsgBox,
                           "Invalid X registry information in '%s'", dup_ptr);
                  } else if (ps_name == NULL) {
                     sprintf(gszMsgBox,
                           "Invalid PS font information in '%s'", dup_ptr);
                  }
                  fprintf (stderr, "In '%s*AdditionalFonts':\n\t%s.\n",
                        TOOL_NAME, gszMsgBox);
               }
               if (dup_ptr != NULL) free(dup_ptr);
               if (!ok) break;
            }
            if (ok)
            {
               num_new_fonts++;
               fmly_ptr = (struct TmpFontFmlyRec *)malloc(
                     sizeof(struct TmpFontFmlyRec));
               if (fmly_ptr == NULL) FailAllocMessage();
               memset(fmly_ptr, 0, sizeof(struct TmpFontFmlyRec));
               fmly_ptr->next = NULL;
               fmly_ptr->prev = last_fmly;
               fmly_ptr->font_strings = font_strings;
               if (last_fmly == NULL)
                  first_fmly = fmly_ptr;
               else
                  last_fmly->next = fmly_ptr;
               last_fmly = fmly_ptr;
            }
            if (font_ptr-buf >= len) break;
         }
         if (ok)
         {
            struct TmpFontFmlyRec	* next_fmly;

            numFonts = MAXFONTS+num_new_fonts;
            fontFamilies = (struct FontFmlyRec *)malloc(
                  numFonts*sizeof(struct FontFmlyRec));
            if (fontFamilies == NULL) FailAllocMessage();
            fontInfoStr =
                  (char**)malloc(numFonts*MAXFONTSTYLES*3*sizeof(char*));
            if (fontInfoStr == NULL) FailAllocMessage();
            fontMenuStr = (char**)malloc(numFonts*sizeof(char*));
            if (fontMenuStr == NULL) FailAllocMessage();
            index = MAXFONTS*MAXFONTSTYLES*3;
            for (fmly_ptr=first_fmly, fmly_index=MAXFONTS; fmly_ptr != NULL;
                  fmly_ptr=next_fmly, fmly_index++) {
               char s[81];

               next_fmly = fmly_ptr->next;
               strcpy (s, fmly_ptr->font_strings[2]);
               if ((c_ptr=strchr(s,'-')) != NULL) *c_ptr='\0';
               fontMenuStr[fmly_index] =
                     (char*)malloc((strlen(s)+1)*sizeof(char));
               if (fontMenuStr[fmly_index] == NULL) FailAllocMessage();
               strcpy(fontMenuStr[fmly_index], s );
               for (i=0, j=0; i<MAXFONTSTYLES; i++) {
                  fontInfoStr[index++] = fmly_ptr->font_strings[j++];
                  fontInfoStr[index++] = fmly_ptr->font_strings[j++];
                  fontInfoStr[index++] = fmly_ptr->font_strings[j++];
               }
               free(fmly_ptr->font_strings);
               free(fmly_ptr);
               fontFamilies[fmly_index].fr[STYLE_NR].next =
                     fontFamilies[fmly_index].fr[STYLE_BR].next =
                     fontFamilies[fmly_index].fr[STYLE_NI].next =
                     fontFamilies[fmly_index].fr[STYLE_BI].next = NULL;
               fontFamilies[fmly_index].fr[STYLE_NR].xfs =
                     fontFamilies[fmly_index].fr[STYLE_BR].xfs =
                     fontFamilies[fmly_index].fr[STYLE_NI].xfs =
                     fontFamilies[fmly_index].fr[STYLE_BI].xfs = NULL;
            }
         }
      }
      free(buf);
   }
   if (fontFamilies == NULL)
   {
      fontFamilies =
            (struct FontFmlyRec *)malloc(numFonts*sizeof(struct FontFmlyRec));
      if (fontFamilies == NULL) FailAllocMessage();
      fontInfoStr = (char**)malloc(numFonts*MAXFONTSTYLES*3*sizeof(char*));
      fontMenuStr = (char**)malloc(numFonts*sizeof(char*));
      if (fontInfoStr == NULL || fontMenuStr == NULL) FailAllocMessage();
   }
   altFontInfoStr = (char**)malloc(MAXFONTS*MAXFONTSTYLES*3*sizeof(char*));
   if (altFontInfoStr == NULL) FailAllocMessage();
   for (j=0; j<MAXFONTS*MAXFONTSTYLES*3; j++) altFontInfoStr[j] = NULL;
   i = 0;
   fmly_index = 0;
   for (fmly_index=0; fmly_index < MAXFONTS; fmly_index++)
   {
      char	s[81];

      strcpy (s, initFontInfoStr[i+2]);
      if ((c_ptr=strchr(s,'-')) != NULL) *c_ptr='\0';
      fontMenuStr[fmly_index] = (char*)malloc((strlen(s)+1)*sizeof(char));
      if (fontMenuStr[fmly_index] == NULL) FailAllocMessage();
      strcpy(fontMenuStr[fmly_index], s );
      for (style_index=0; style_index < MAXFONTSTYLES; style_index++) {
         if (hasAlternateDefaultFonts) {
            for (j=0; j<3; j++, i++) {
               if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME,
                     initFontInfoStr[i+2])) != NULL) {
                  if (strstr(c_ptr, "%d") != NULL) {
                     len = strlen (c_ptr);
                     altFontInfoStr[i] = (char*)malloc((len+1)*sizeof(char));
                     if (altFontInfoStr[i] == NULL) FailAllocMessage();
                     strcpy(altFontInfoStr[i], c_ptr);
                     fontInfoStr[i] = altFontInfoStr[i];
                  }
                  else
                  {
                     fprintf (stderr,
                           "%s '%s*%s'.\n\t'%s-%s-*-%%d-*-*-*-*-*-%s' used.\n",
                           "Warning:  Error in processing",
                           TOOL_NAME, initFontInfoStr[i+2],
                           fontNamePrefix, initFontInfoStr[i],
                           initFontInfoStr[i+1]);
                     fontInfoStr[i] = initFontInfoStr[i];
                  }
               }
               else
                  fontInfoStr[i] = initFontInfoStr[i];
            }
         }
         else
         {
            fontInfoStr[i] = initFontInfoStr[i]; i++;
            fontInfoStr[i] = initFontInfoStr[i]; i++;
            fontInfoStr[i] = initFontInfoStr[i]; i++;
         }
      }
      fontFamilies[fmly_index].fr[STYLE_NR].next =
            fontFamilies[fmly_index].fr[STYLE_BR].next =
            fontFamilies[fmly_index].fr[STYLE_NI].next =
            fontFamilies[fmly_index].fr[STYLE_BI].next = NULL;
      fontFamilies[fmly_index].fr[STYLE_NR].xfs =
            fontFamilies[fmly_index].fr[STYLE_BR].xfs =
            fontFamilies[fmly_index].fr[STYLE_NI].xfs =
            fontFamilies[fmly_index].fr[STYLE_BI].xfs = NULL;
   }

   curFont = FONT_COU;
   curStyle = STYLE_NR;
   curSize = 17;

   ruler_font_size = 10;
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "RulerFontSize")) != NULL) {
      ruler_font_size = atoi(c_ptr);
      if (ruler_font_size <= 0) {
         ruler_font_size = defaultFontSize;
         fprintf(stderr, "%s '%s', '%1d' is used.\n",
               "Warning:  can not set RulerFontSize to", c_ptr,
               ruler_font_size);
      }
   }
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "InitialFontDPI")) != NULL) {
      fprintf(stderr, "Obsoleted %s*InitialFontDPI used.\n%s%s%s.\n",
            TOOL_NAME, "\tPlease use ", TOOL_NAME, "*InitialFontSize instead");
   }
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"MsgFontSizeIndex")) != NULL) {
      fprintf(stderr, "Obsoleted %s*MsgFontSizeIndex used.\n%s%s%s.\n",
            TOOL_NAME, "\tPlease use ", TOOL_NAME, "*MsgFontSize instead");
   }
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "MsgFontSize")) != NULL) {
      curSize = atoi(c_ptr);
      if (curSize <= 0) {
         curSize = defaultFontSize;
         fprintf(stderr, "%s '%s', '%1d' is used.\n",
               "Warning:  can not set MsgFontSize to", c_ptr, curSize);
      }
   }
   defaultFontPtr = NULL;
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "DefFixedWidthFont")) !=
         NULL) {
      XFontStruct *xfs=NULL;

      defaultFontName = UtilStrDup(c_ptr);
      if (defaultFontName == NULL) FailAllocMessage();
      xfs = XLoadQueryFont(mainDisplay, defaultFontName);
      if (xfs == NULL) {
         fprintf(stderr, "%s %s*DefFixedWidthFont: '%s'.\n",
               "Cannot load font specified by", TOOL_NAME, defaultFontName);
         if (defaultFontName != NULL) free(defaultFontName);
         defaultFontName = NULL;
         defaultFontPtr = NULL;
      } else {
         defaultFontPtr = xfs;
         defaultFontWidth = defaultFontPtr->max_bounds.width;
         defaultFontAsc = defaultFontPtr->max_bounds.ascent;
         defaultFontDes = defaultFontPtr->max_bounds.descent;
         defaultFontHeight = defaultFontAsc + defaultFontDes;
      }
   }
   if (defaultFontPtr == NULL) {
      fs_ptr = FindFontInfo(curFont, curStyle, curSize);
      if (fs_ptr == NULL) {
         sprintf(gszMsgBox, "Can not open the Default(Msg)Font '%s'!  Abort!",
               gszAttemptedFontName);
         Error("OpenFont()", gszMsgBox);
      }
      defaultFontPtr = fs_ptr->xfs;
      defaultFontWidth = defaultFontPtr->max_bounds.width;
      defaultFontAsc = defaultFontPtr->max_bounds.ascent;
      defaultFontDes = defaultFontPtr->max_bounds.descent;
      defaultFontHeight = defaultFontAsc + defaultFontDes;
   }
   if (defaultFontWidth < 1 || defaultFontHeight < 1) {
      fprintf(stderr, "%s (%1d/%1d).\n\t%s.\n\t%s.\n",
            "Warning:  Very small default font width/height",
            defaultFontWidth, defaultFontHeight,
            "Possibly a problem with the font path",
            "Set default font width to 9 and height to 14 as a temporary fix");
      defaultFontWidth = 9;
      defaultFontHeight = 14;
   }

   rulerFontPtr = NULL;
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "DefFixedWidthRulerFont")) !=
         NULL) {
      XFontStruct *xfs=NULL;

      rulerFontName = UtilStrDup(c_ptr);
      if (rulerFontName == NULL) FailAllocMessage();
      xfs = XLoadQueryFont(mainDisplay, rulerFontName);
      if (xfs == NULL) {
         fprintf(stderr, "%s %s*DefFixedWidthRulerFont: '%s'.\n",
               "Cannot load font specified by", TOOL_NAME, rulerFontName);
         if (rulerFontName != NULL) free(rulerFontName);
         rulerFontName = NULL;
         rulerFontPtr = NULL;
      } else {
         rulerFontPtr = xfs;
         rulerFontWidth = rulerFontPtr->max_bounds.width;
         rulerFontAsc = rulerFontPtr->max_bounds.ascent;
         rulerFontDes = rulerFontPtr->max_bounds.descent;
         rulerFontHeight = rulerFontAsc + rulerFontDes;
      }
   }
   if (rulerFontPtr == NULL) {
      fs_ptr = FindFontInfo(curFont, curStyle, ruler_font_size);
      if (fs_ptr == NULL) {
         sprintf(gszMsgBox, "Can not open the RulerFont '%s'!  Abort!",
               gszAttemptedFontName);
         Error("OpenFont()", gszMsgBox);
      }
      rulerFontPtr = fs_ptr->xfs;
      rulerFontWidth = rulerFontPtr->max_bounds.width;
      rulerFontAsc = rulerFontPtr->max_bounds.ascent;
      rulerFontDes = rulerFontPtr->max_bounds.descent;
      rulerFontHeight = rulerFontAsc + rulerFontDes;
   }
   if (rulerFontWidth < 1 || rulerFontHeight < 1) {
      fprintf(stderr, "%s (%1d/%1d).\n\t%s.\n\t%s.\n",
            "Warning:  Very small ruler font width/height",
            rulerFontWidth, rulerFontHeight,
            "Possibly a problem with the font path",
            "Set ruler font width to 7 and height to 12 as a temporary fix");
      rulerFontWidth = 7;
      rulerFontHeight = 12;
   }

   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "MenuFont")) != NULL) {
      XFontStruct *xfs=NULL;

      menuFontName = UtilStrDup(c_ptr);
      if (menuFontName == NULL) FailAllocMessage();
      xfs = XLoadQueryFont(mainDisplay, menuFontName);
      if (xfs == NULL) {
         fprintf(stderr, "Cannot load font specified by %s*MenuFont: '%s'.\n",
               TOOL_NAME, menuFontName);
         if (menuFontName != NULL) free(menuFontName);
         menuFontName = NULL;
         menuFontPtr = NULL;
      } else {
         menuFontPtr = xfs;
         menuFontWidth = menuFontPtr->max_bounds.width;
         menuFontAsc = menuFontPtr->max_bounds.ascent;
         menuFontDes = menuFontPtr->max_bounds.descent;
         menuFontHeight = menuFontAsc + menuFontDes;
      }
   }
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "BoldMsgFont")) != NULL) {
      XFontStruct *xfs=NULL;

      boldMsgFontName = UtilStrDup(c_ptr);
      if (boldMsgFontName == NULL) FailAllocMessage();
      xfs = XLoadQueryFont(mainDisplay, boldMsgFontName);
      if (xfs == NULL) {
         fprintf(stderr, "%s %s*BoldMsgFont: '%s'.\n",
               "Cannot load font specified by", TOOL_NAME, boldMsgFontName);
         if (boldMsgFontName != NULL) free(boldMsgFontName);
         boldMsgFontName = NULL;
         boldMsgFontPtr = NULL;
      } else {
         boldMsgFontPtr = xfs;
         boldMsgFontWidth = boldMsgFontPtr->max_bounds.width;
         boldMsgFontAsc = boldMsgFontPtr->max_bounds.ascent;
         boldMsgFontDes = boldMsgFontPtr->max_bounds.descent;
         boldMsgFontHeight = boldMsgFontAsc + boldMsgFontDes;
      }
   }
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "MsgFont")) != NULL) {
      XFontStruct *xfs=NULL;

      msgFontName = UtilStrDup(c_ptr);
      if (msgFontName == NULL) FailAllocMessage();
      xfs = XLoadQueryFont(mainDisplay, msgFontName);
      if (xfs == NULL) {
         fprintf(stderr, "%s %s*MsgFont: '%s'.\n",
               "Cannot load font specified by", TOOL_NAME, msgFontName);
         if (msgFontName != NULL) free(msgFontName);
         msgFontName = NULL;
         msgFontPtr = NULL;
      } else {
         msgFontPtr = xfs;
         msgFontWidth = msgFontPtr->max_bounds.width;
         msgFontAsc = msgFontPtr->max_bounds.ascent;
         msgFontDes = msgFontPtr->max_bounds.descent;
         msgFontHeight = msgFontAsc + msgFontDes;
      }
   }
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "InitialFont")) != NULL) {
      for (i = 0; i < numFonts; i++) {
         if (strcmp(c_ptr, fontMenuStr[i]) == 0) {
            break;
         }
      }
      if (i != numFonts) {
         curFont = i;
      } else {
         fprintf(stderr, "Warning:  can not set InitialFont to '%s'\n", c_ptr);
      }
   }
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"InitialFontStyle")) != NULL) {
      len = strlen(c_ptr);
      if (len < ((int) strlen(styleMenuStr[0]))) {
         for (i = 0; i < MAXFONTSTYLES; i++) {
            if (strncmp(c_ptr, styleMenuStr[i], len) == 0) {
               break;
            }
         }
         if (i != MAXFONTSTYLES) {
            curStyle = i;
         } else {
            fprintf(stderr, "%s '%s'\n",
                  "Warning:  can not set InitialFontStyle to", c_ptr);
         }
      } else {
         fprintf(stderr, "Warning:  can not set InitialFontStyle to '%s'\n",
               c_ptr);
      }
   }
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "InitialFontJust")) != NULL) {
      len = strlen(c_ptr);
      if (len < ((int) strlen(styleMenuStr[0]))) {
         for (i = MAXFONTSTYLES+1; i < MAXFONTSTYLES+1+MAXJUSTS; i++) {
            if (strncmp(c_ptr, styleMenuStr[i], len) == 0) {
               break;
            }
         }
         if (i != MAXFONTSTYLES+1+MAXJUSTS) {
            textJust = i-MAXFONTSTYLES-1;
         } else {
            fprintf(stderr, "%s '%s'\n",
                  "Warning:  can not set InitialFontJust to", c_ptr);
         }
      } else {
         fprintf(stderr, "Warning:  can not set InitialFontJust to '%s'\n",
               c_ptr);
      }
   }
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"InitialFontSizeIndex")) !=
         NULL) {
      fprintf(stderr, "Obsoleted %s*InitialFontSizeIndex used.\n%s%s%s.\n",
            TOOL_NAME, "\tPlease use ", TOOL_NAME, "*InitialFontSize instead");
   }
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"InitialFontSize")) != NULL) {
      curSize = atoi(c_ptr);
      if (curSize <= 0) {
         curSize = defaultFontSize;
         fprintf(stderr, "%s '%s'\n",
               "Warning:  can not set InitialFontSize to", c_ptr);
      }
   }
   curUnderlineOn = FALSE;
   if ((c_ptr=XGetDefault(mainDisplay,TOOL_NAME,"InitialUnderlineOn"))!=NULL &&
         UtilStrICmp(c_ptr, "true") == 0) {
      curUnderlineOn = TRUE;
   }
   curUnderline = 2;
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME,
         "InitialUnderlineAmount")) != NULL) {
      curUnderline = atoi (c_ptr);
   }
}

static
int AdjTransformedTextBBox(ObjPtr)
   struct ObjRec *ObjPtr;
   /* a transformed text object has just changed it's font */
   /*      property, so its bounding box needs to be adjusted */
   /* a transformed text object has just changed it's font */
{
   struct XfrmMtrxRec *ctm=ObjPtr->ctm;

   if (ctm == NULL) return UpdTextBBox(ObjPtr);
   ObjPtr->ctm = NULL;
   if (!UpdTextBBox(ObjPtr)) {
      ObjPtr->ctm = ctm;
      return FALSE;
   }
   SetCTM(ObjPtr, ctm);
   UpdTextBBox(ObjPtr);
   AdjObjSplineVs(ObjPtr);
   AdjObjBBox(ObjPtr);
   return TRUE;
}

int ChangeObjTextStyle(ObjPtr, StyleIndex)
   register struct ObjRec *ObjPtr;
   register int StyleIndex;
{
   register struct ObjRec *obj_ptr;
   register int changed=FALSE, obj_changed;

   switch (ObjPtr->type) {
   case OBJ_TEXT:
      if (ObjPtr->detail.t->style != StyleIndex) {
         ObjPtr->detail.t->style = StyleIndex;
         if (AdjTransformedTextBBox(ObjPtr) == FALSE) {
            Msg("Invalid vertical spacing for a text object.");
            Msg("    That object's vertical spacing reset to 0.");
            ObjPtr->detail.t->v_space = 0;
            UpdTextBBox(ObjPtr);
         }
         if (ObjPtr->detail.t->cached_bitmap != None) {
            XFreePixmap(mainDisplay,ObjPtr->detail.t->cached_bitmap);
         }
         ObjPtr->detail.t->cached_zoom = 0;
         ObjPtr->detail.t->cached_bitmap = None;

         changed = TRUE;
      }
      break;

   case OBJ_GROUP:
   case OBJ_SYM:
      obj_changed = FALSE;
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr=obj_ptr->prev) {
         if (ChangeObjTextStyle(obj_ptr, StyleIndex)) {
            obj_changed = TRUE;
         }
      }
      if (obj_changed) {
         changed = TRUE;
         AdjObjBBox(ObjPtr);
      }
      break;
   }
   return changed;
}

void ChangeFontStyle(StyleIndex)
   int StyleIndex;
{
   register struct SelRec *sel_ptr;
   int ltx, lty, rbx, rby, changed=FALSE;

   if (StyleIndex == INVALID) return;

   if (topSel == NULL || stickyMenuSelection) {
      if (!(curChoice == DRAWTEXT && textCursorShown)) {
         TieLooseEnds();
      }
      curStyle = StyleIndex;
      SetCanvasFont();
      if (curChoice == DRAWTEXT && textCursorShown) {
         if (ChangeObjTextStyle(curTextObj, StyleIndex)) {
            curTextModified = TRUE;
            UpdCurTextBBox();
            RedrawCurText();
            if (cycleThroughChoice) {
               SetPushedFontValue(PUSH_STYLE, curStyle);
            }
         }
      } else {
         textCursorShown = FALSE;
      }
      ShowCurFont();
      ShowTextSize();
      UpdateSubMenu(MENU_STYLE);
      if (topSel == NULL) return;
   }

   HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      PrepareToReplaceAnObj(sel_ptr->obj);
      if (ChangeObjTextStyle(sel_ptr->obj, StyleIndex)) {
         changed = TRUE;
         RecordReplaceAnObj(sel_ptr->obj);
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
   }
   EndCompositeCmd();

   if (changed) {
      ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
      UpdSelBBox();
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1), selLtX-GRID_ABS_SIZE(1),
            selLtY-GRID_ABS_SIZE(1), selRbX+GRID_ABS_SIZE(1),
            selRbY+GRID_ABS_SIZE(1));
      SetFileModified(TRUE);
   }
   HighLightForward();
}

int ChangeObjTextJust(ObjPtr, JustIndex)
   register struct ObjRec *ObjPtr;
   register int JustIndex;
{
   register struct ObjRec *obj_ptr;
   register int changed=FALSE, obj_changed;

   switch (ObjPtr->type) {
   case OBJ_TEXT:
      if (ObjPtr->detail.t->just != JustIndex) {
         ObjPtr->detail.t->just = JustIndex;
         UpdTextBBox(ObjPtr);

         if (ObjPtr->detail.t->cached_bitmap != None) {
            XFreePixmap(mainDisplay,ObjPtr->detail.t->cached_bitmap);
         }
         ObjPtr->detail.t->cached_zoom = 0;
         ObjPtr->detail.t->cached_bitmap = None;

         changed = TRUE;
      }
      break;

   case OBJ_GROUP:
   case OBJ_SYM:
      obj_changed = FALSE;
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr=obj_ptr->prev) {
         if (ChangeObjTextJust(obj_ptr, JustIndex)) {
            obj_changed = TRUE;
         }
      }
      if (obj_changed) {
         changed = TRUE;
         AdjObjBBox(ObjPtr);
      }
      break;
   }
   return changed;
}

void ChangeFontJust(JustIndex)
   int JustIndex;
{
   register struct SelRec *sel_ptr;
   int ltx, lty, rbx, rby, changed=FALSE;

   if (JustIndex == INVALID) return;

   if (topSel == NULL || stickyMenuSelection) {
      if (!(curChoice == DRAWTEXT && textCursorShown)) {
         TieLooseEnds();
      }
      textJust = JustIndex;
      ShowJust();
      UpdateSubMenu(MENU_STYLE);
      if (curChoice == DRAWTEXT && textCursorShown) {
         if (ChangeObjTextJust(curTextObj, JustIndex)) {
            curTextModified = TRUE;
            UpdCurTextBBox();
            ClearAndRedrawDrawWindow();
            if (cycleThroughChoice) {
               SetPushedFontValue(PUSH_JUST, textJust);
            }
         }
      } else {
         textCursorShown = FALSE;
      }
      if (topSel == NULL) return;
   }

   HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      PrepareToReplaceAnObj(sel_ptr->obj);
      if (ChangeObjTextJust(sel_ptr->obj, JustIndex)) {
         changed = TRUE;
         RecordReplaceAnObj(sel_ptr->obj);
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
   }
   EndCompositeCmd();

   if (changed) {
      ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
      UpdSelBBox();
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1), selLtX-GRID_ABS_SIZE(1),
            selLtY-GRID_ABS_SIZE(1), selRbX+GRID_ABS_SIZE(1),
            selRbY+GRID_ABS_SIZE(1));
      SetFileModified(TRUE);
   }
   HighLightForward();
}

void StyleSubMenu(index)
   int index;
{
   if (index < MAXFONTSTYLES) {
      ChangeFontStyle(index);
   } else if (index > MAXFONTSTYLES) {
      ChangeFontJust(index - MAXFONTSTYLES - 1);
   }
}

static char *styleMenuDescription[] = {
   "Roman text style",
   "Bold text style",
   "Italic text style",
   "Bold-italic text style",
   "",
   "Left justified text",
   "Center justified text",
   "Right justified text",
   NULL
};

int StyleMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   register int index;
   int *fore_colors, *valid, *init_rv;

   DefaultColorArrays (MAXFONTSTYLES+MAXJUSTS+1, &fore_colors, &valid,
         &init_rv, NULL);

   init_rv[curStyle] = TRUE;
   init_rv[MAXFONTSTYLES+1+textJust] = TRUE;
   activeMenu = MENU_STYLE;
   index = TextMenuLoop (X, Y, styleMenuStr, MAXFONTSTYLES+MAXJUSTS+1,
         fore_colors, valid, init_rv, styleMenuDescription, SINGLECOLOR,
         TrackMenubar);

   if (index >= 0) StyleSubMenu (index);
   return (index);
}

int ChangeObjTextSize(ObjPtr, SizeIndex)
   register struct ObjRec *ObjPtr;
   register int SizeIndex;
{
   register struct ObjRec *obj_ptr;
   register int changed=FALSE, obj_changed;
   int size=fontSizes[SizeIndex];

   switch (ObjPtr->type) {
   case OBJ_TEXT:
      if (ObjPtr->detail.t->size != size) {
         int saved_size=ObjPtr->detail.t->size;
         int saved_read_only=ObjPtr->detail.t->read_only, rc;

         ObjPtr->detail.t->size = size;
         ObjPtr->detail.t->read_only = FALSE;
         changingFontSizeFromRead = FALSE;
         rc = AdjTransformedTextBBox(ObjPtr);
         changingFontSizeFromRead = TRUE;
         switch (rc) {
         case INVALID:
            ObjPtr->detail.t->size = saved_size;
            ObjPtr->detail.t->read_only = saved_read_only;
            UpdTextBBox (ObjPtr);
            break;
         case FALSE:
            Msg("Invalid vertical spacing for a text object.");
            Msg("    That object's vertical spacing reset to 0.");
            ObjPtr->detail.t->v_space = 0;
            UpdTextBBox(ObjPtr);
            break;
         }
         if (rc != INVALID) {
            if (ObjPtr->detail.t->cached_bitmap != None) {
               XFreePixmap(mainDisplay,ObjPtr->detail.t->cached_bitmap);
            }
            ObjPtr->detail.t->cached_zoom = 0;
            ObjPtr->detail.t->cached_bitmap = None;

            changed = TRUE;
         }
      }
      break;

   case OBJ_GROUP:
   case OBJ_SYM:
      obj_changed = FALSE;
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr=obj_ptr->prev) {
         if (ChangeObjTextSize(obj_ptr, SizeIndex)) {
            obj_changed = TRUE;
         }
      }
      if (obj_changed) {
         changed = TRUE;
         AdjObjBBox(ObjPtr);
      }
      break;
   }
   return changed;
}

void ChangeFontSize(SizeIndex)
   int SizeIndex;
{
   register struct SelRec *sel_ptr;
   int ltx, lty, rbx, rby;
   int changed=FALSE;
   int saved_size=curSize;

   if (SizeIndex == INVALID) return;

   if (topSel == NULL || stickyMenuSelection) {
      if (!(curChoice == DRAWTEXT && textCursorShown)) {
         TieLooseEnds();
      }
      curSize = fontSizes[SizeIndex];
      changingFontSizeFromRead = FALSE;
      attemptingToSetFontProperty = TRUE;
      SetCanvasFont();
      attemptingToSetFontProperty = FALSE;
      changingFontSizeFromRead = TRUE;
      if (canvasFontSize == INVALID) {
         sprintf(gszMsgBox, "%s-%1d not available.", fontMenuStr[curFont],
               curSize);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         curSize = saved_size;
         SizeIndex = GetSizeMenuIndex();
         SetCanvasFont();
      } else if (curSize != canvasFontSize) {
         sprintf(gszMsgBox, "Can not change size to %1d.  %1d used.",
               curSize, canvasFontSize);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         curSize = canvasFontSize;
         SizeIndex = GetSizeMenuIndex();
      }
      if (curChoice == DRAWTEXT && textCursorShown) {
         if (ChangeObjTextSize(curTextObj, SizeIndex)) {
            curTextModified = TRUE;
            UpdCurTextBBox();
            RedrawCurText();
            if (cycleThroughChoice) {
               SetPushedFontValue(PUSH_SIZE, curSize);
            }
         }
      } else {
         textCursorShown = FALSE;
      }
      ShowCurFont();
      ShowTextSize();
      UpdateSubMenu(MENU_SIZE);
      if (topSel == NULL) return;
   }

   HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      PrepareToReplaceAnObj(sel_ptr->obj);
      if (ChangeObjTextSize(sel_ptr->obj, SizeIndex)) {
         changed = TRUE;
         RecordReplaceAnObj(sel_ptr->obj);
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
   }
   EndCompositeCmd();

   if (changed) {
      ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
      UpdSelBBox();
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1), selLtX-GRID_ABS_SIZE(1),
            selLtY-GRID_ABS_SIZE(1), selRbX+GRID_ABS_SIZE(1),
            selRbY+GRID_ABS_SIZE(1));
      SetFileModified(TRUE);
   }
   HighLightForward();
}

int GetSizeMenuIndex()
{
   register int i;

   for (i=0; i < numFontSizes; i++) {
      if (fontSizes[i] == curSize) {
         return i;
      }
   }
   return INVALID;
}

int SizeMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   register int i;
   int index, *fore_colors, *valid, *init_rv;
   char **desc=(char**)malloc((numFontSizes+1)*sizeof(char*));

   if (desc == NULL) FailAllocMessage();
   for (i=0; i < numFontSizes; i++) {
      desc[i] = (char*)malloc(80*sizeof(char));
      if (desc[i] == NULL) FailAllocMessage();
      sprintf(desc[i], "Set font size to %1d", fontSizes[i]);
   }
   desc[i] = NULL;
   DefaultColorArrays(numFontSizes, &fore_colors, &valid, &init_rv, NULL);

   index = GetSizeMenuIndex();
   if (index != INVALID) init_rv[index] = TRUE;
   activeMenu = MENU_SIZE;
   index = TextMenuLoop(X, Y, sizeMenuStr, numFontSizes, fore_colors,
         valid, init_rv, desc, SINGLECOLOR, TrackMenubar);

   if (index >= 0) ChangeFontSize(index);
   if (desc != NULL) {
      for (i=0; i < numFontSizes; i++) {
         if (desc[i] != NULL) {
            free(desc[i]);
         }
      }
      free(desc);
   }
   return index;
}

int ChangeObjTextFont(ObjPtr, FontIndex)
   register struct ObjRec *ObjPtr;
   register int FontIndex;
{
   register struct ObjRec *obj_ptr;
   register int changed=FALSE, obj_changed;

   switch (ObjPtr->type) {
   case OBJ_TEXT:
      if (ObjPtr->detail.t->font != FontIndex) {
         int rc, saved_font=ObjPtr->detail.t->font;

         if (ObjPtr->detail.t->font == FONT_SYM) {
            RemoveIllegalChars(ObjPtr->detail.t);
         }
         ObjPtr->detail.t->font = FontIndex;
         changingFontSizeFromRead = FALSE;
         rc = AdjTransformedTextBBox(ObjPtr);
         changingFontSizeFromRead = TRUE;
         switch (rc) {
         case INVALID:
            ObjPtr->detail.t->font = saved_font;
            UpdTextBBox(ObjPtr);
            break;
         case FALSE:
            Msg("Invalid vertical spacing for a text object.");
            Msg("    That object's vertical spacing reset to 0.");
            ObjPtr->detail.t->v_space = 0;
            UpdTextBBox(ObjPtr);
            break;
         }
         if (ObjPtr->detail.t->cached_bitmap != None) {
            XFreePixmap(mainDisplay,ObjPtr->detail.t->cached_bitmap);
         }
         ObjPtr->detail.t->cached_zoom = 0;
         ObjPtr->detail.t->cached_bitmap = None;

         changed = TRUE;
      }
      break;

   case OBJ_GROUP:
   case OBJ_SYM:
      obj_changed = FALSE;
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr=obj_ptr->prev) {
         if (ChangeObjTextFont(obj_ptr, FontIndex)) {
            obj_changed = TRUE;
         }
      }
      if (obj_changed) {
         changed = TRUE;
         AdjObjBBox(ObjPtr);
      }
      break;
   }
   return changed;
}

void ChangeFont(FontIndex)
   int FontIndex;
{
   register struct SelRec *sel_ptr;
   int ltx, lty, rbx, rby, changed=FALSE, saved_font=curFont;

   if (FontIndex == INVALID) return;

   if (topSel == NULL || stickyMenuSelection) {
      if (!(curChoice == DRAWTEXT && textCursorShown)) {
         TieLooseEnds();
      }
      curFont = FontIndex;
      attemptingToSetFontProperty = TRUE;
      SetCanvasFont();
      attemptingToSetFontProperty = FALSE;
      if (canvasFontSize == INVALID) {
         sprintf(gszMsgBox, "%s-%1d not available.", fontMenuStr[curFont], curSize);
         MsgBox(gszMsgBox, TOOL_NAME, INFO_MB);
         curFont = FontIndex = saved_font;
         SetCanvasFont();
      }
      if (curChoice == DRAWTEXT && textCursorShown) {
         if (ChangeObjTextFont(curTextObj, FontIndex)) {
            curTextModified = TRUE;
            UpdCurTextBBox();
            RedrawCurText();
            if (cycleThroughChoice) {
               SetPushedFontValue(PUSH_FONT, curFont);
            }
         }
      } else {
         textCursorShown = FALSE;
      }
      ShowCurFont();
      ShowTextSize();
      UpdateSubMenu(MENU_FONT);
      if (topSel == NULL) return;
   }

   HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      PrepareToReplaceAnObj(sel_ptr->obj);
      if (ChangeObjTextFont(sel_ptr->obj, FontIndex)) {
         changed = TRUE;
         RecordReplaceAnObj(sel_ptr->obj);
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
   }
   EndCompositeCmd();

   if (changed) {
      ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
      UpdSelBBox();
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1), selLtX-GRID_ABS_SIZE(1),
            selLtY-GRID_ABS_SIZE(1), selRbX+GRID_ABS_SIZE(1),
            selRbY+GRID_ABS_SIZE(1));
      SetFileModified(TRUE);
   }
   HighLightForward();
}

int FontMenu(X, Y, TrackMenubar)
   int X, Y, TrackMenubar;
{
   register int i;
   int index, *fore_colors, *valid, *init_rv;
   char **desc=(char**)malloc((numFonts+1)*sizeof(char*));

   if (desc == NULL) FailAllocMessage();
   for (i=0; i < numFonts; i++) {
      desc[i] = (char*)malloc(80*sizeof(char));
      if (desc[i] == NULL) FailAllocMessage();
      sprintf(desc[i], "Set font to '%s'", fontMenuStr[i]);
   }
   desc[i] = NULL;
   DefaultColorArrays(numFonts, &fore_colors, &valid, &init_rv, NULL);

   init_rv[curFont] = TRUE;
   activeMenu = MENU_FONT;
   index = TextMenuLoop(X, Y, fontMenuStr, numFonts, fore_colors, valid,
         init_rv, desc, SINGLECOLOR, TrackMenubar);

   if (index >= 0) ChangeFont(index);
   if (desc != NULL) {
      for (i=0; i < numFonts; i++) {
         if (desc[i] != NULL) {
            free(desc[i]);
         }
      }
      free(desc);
   }
   return index;
}

int ChangeObjVSpace(ObjPtr, VSpace)
   register struct ObjRec *ObjPtr;
   register int VSpace;
{
   register struct ObjRec *obj_ptr;
   register int changed=FALSE, obj_changed;
   int saved_v_space;

   switch (ObjPtr->type) {
   case OBJ_TEXT:
      if (ObjPtr->detail.t->v_space != VSpace) {
         saved_v_space = ObjPtr->detail.t->v_space;
         ObjPtr->detail.t->v_space = VSpace;
         if (AdjTransformedTextBBox(ObjPtr) == FALSE) {
            Msg("Invalid vertical spacing for a text object.");
            Msg("Vertical spacing for that object not changed.");
            ObjPtr->detail.t->v_space = saved_v_space;
            UpdTextBBox(ObjPtr);
         } else {
            if (ObjPtr->detail.t->cached_bitmap != None) {
               XFreePixmap(mainDisplay,ObjPtr->detail.t->cached_bitmap);
            }
            ObjPtr->detail.t->cached_zoom = 0;
            ObjPtr->detail.t->cached_bitmap = None;

            changed = TRUE;
         }
      }
      break;

   case OBJ_GROUP:
   case OBJ_SYM:
      obj_changed = FALSE;
      for (obj_ptr=ObjPtr->detail.r->last; obj_ptr != NULL;
            obj_ptr=obj_ptr->prev) {
         if (ChangeObjVSpace(obj_ptr, VSpace)) {
            obj_changed = TRUE;
         }
      }
      if (obj_changed) {
         changed = TRUE;
         AdjObjBBox(ObjPtr);
      }
      break;
   }
   return changed;
}

void ChangeVSpace(VSpace)
   int VSpace;
{
   register struct SelRec *sel_ptr;
   int ltx, lty, rbx, rby, changed=FALSE;

   if (topSel == NULL || stickyMenuSelection) {
      if (!(curChoice == DRAWTEXT && textCursorShown)) {
         TieLooseEnds();
      }
      if (textCursorH+textVSpace <= 0) {
         Msg("Text vertical spacing too small.  No change.");
      } else {
         textVSpace = VSpace;
         SetCanvasFont();
         ShowTextVSpace();
      }
      if (curChoice == DRAWTEXT && textCursorShown) {
         if (ChangeObjVSpace(curTextObj, VSpace)) {
            curTextModified = TRUE;
            UpdCurTextBBox();
            RedrawCurText();
            if (cycleThroughChoice) {
               SetPushedFontValue(PUSH_VSPACE, textVSpace);
            }
         }
      } else {
         textCursorShown = FALSE;
      }
      if (topSel == NULL) return;
   }

   HighLightReverse();
   StartCompositeCmd();
   for (sel_ptr=botSel; sel_ptr != NULL; sel_ptr=sel_ptr->prev) {
      PrepareToReplaceAnObj(sel_ptr->obj);
      if (ChangeObjVSpace(sel_ptr->obj, VSpace)) {
         changed = TRUE;
         RecordReplaceAnObj(sel_ptr->obj);
      } else {
         AbortPrepareCmd(CMD_REPLACE);
      }
   }
   EndCompositeCmd();

   if (changed) {
      ltx = selLtX; lty = selLtY; rbx = selRbX; rby = selRbY;
      UpdSelBBox();
      RedrawAreas(botObj, ltx-GRID_ABS_SIZE(1), lty-GRID_ABS_SIZE(1),
            rbx+GRID_ABS_SIZE(1), rby+GRID_ABS_SIZE(1), selLtX-GRID_ABS_SIZE(1),
            selLtY-GRID_ABS_SIZE(1), selRbX+GRID_ABS_SIZE(1),
            selRbY+GRID_ABS_SIZE(1));
      SetFileModified(TRUE);
   }
   HighLightForward();
}

static int	savedFont, savedSize, savedStyle;
static int	savedJust, savedRotate, savedPen, savedFill, savedVSpace;
static int	savedUnderlineOn, savedUnderline;

void SaveCurFont()
{
   savedFont = curFont;
   savedSize = curSize;
   savedStyle = curStyle;
   savedJust = textJust;
   savedRotate = curRotate;
   savedPen = penPat;
   savedFill = objFill;
   savedVSpace = textVSpace;
   savedUnderlineOn = curUnderlineOn;
   savedUnderline = curUnderline;
}

void RestoreCurFont()
{
   curFont = savedFont;
   curSize = savedSize;
   curStyle = savedStyle;
   textJust = savedJust;
   curRotate = savedRotate;
   penPat = savedPen;
   objFill = savedFill;
   textVSpace = savedVSpace;
   curUnderlineOn = savedUnderlineOn;
   curUnderline = savedUnderline;

   SetCanvasFont();
}

static int	pushedFont, pushedSize, pushedStyle, pushedJust;
static int	pushedColorIndex, pushedRotate, pushedPen, pushedVSpace;
static int	pushedFill, pushedUnderlineOn, pushedUnderline;

void PushCurFont()
{
   pushedFont = curFont;
   pushedSize = curSize;
   pushedStyle = curStyle;
   pushedJust = textJust;
   pushedRotate = curRotate;
   pushedPen = penPat;
   pushedFill = objFill;
   pushedVSpace = textVSpace;
   pushedColorIndex = colorIndex;
   pushedUnderlineOn = curUnderlineOn;
   pushedUnderline = curUnderline;
}

void PopCurFont()
{
   curFont = pushedFont;
   curSize = pushedSize;
   curStyle = pushedStyle;
   textJust = pushedJust;
   curRotate = pushedRotate;
   penPat = pushedPen;
   objFill = pushedFill;
   textVSpace = pushedVSpace;
   colorIndex = pushedColorIndex;
   curUnderlineOn = pushedUnderlineOn;
   curUnderline = pushedUnderline;

   SetCanvasFont();
}

void SetPushedFontValue(which, value)
   int which, value;
{
   switch(which) {
   case PUSH_FONT: pushedFont = value; break;
   case PUSH_SIZE: pushedSize = value; break;
   case PUSH_STYLE: pushedStyle = value; break;
   case PUSH_JUST: pushedJust = value; break;
   case PUSH_ROTATE: pushedRotate = value; break;
   case PUSH_PEN: pushedPen = value; break;
   case PUSH_FILL: pushedFill = value; break;
   case PUSH_VSPACE: pushedVSpace = value; break;
   case PUSH_COLORINDEX: pushedColorIndex = value; break;
   case PUSH_UNDERLINEON: pushedUnderlineOn = value; break;
   case PUSH_UNDERLINE: pushedUnderline = value; break;
   }
}

void CleanUpFonts()
{
   register int i;
   int fmly_index, style_index;
   struct FontSizeRec *fs_ptr, *next_fs;

   for (i=0; i<numFontSizes; i++) free(sizeMenuStr[i]);
   free(sizeMenuStr);
   sizeMenuStr = NULL;

   free(fontSizes);
   fontSizes = NULL;
   numFontSizes = 0;

   CleanUpEncodeCharFonts();

   for (fmly_index=0; fmly_index<numFonts; fmly_index++) {
      for (style_index=0; style_index<MAXFONTSTYLES; style_index++) {
         if (fontFamilies[fmly_index].fr[style_index].xfs != NULL) {
            XFreeFont(mainDisplay,
                  fontFamilies[fmly_index].fr[style_index].xfs);
         }
         for (fs_ptr=fontFamilies[fmly_index].fr[style_index].next;
               fs_ptr != NULL; fs_ptr = next_fs) {
            next_fs = fs_ptr->next;
            if (fs_ptr->xfs != NULL && !fs_ptr->faked) {
               XFreeFont(mainDisplay, fs_ptr->xfs);
            }
            free(fs_ptr);
         }
      }
   }
   for (fmly_index=0; fmly_index<numFakedFonts; fmly_index++) {
      free(fontFamilies[fmly_index+numFonts].name_faked);
   }
   free(fontFamilies);
   fontFamilies = NULL;

   for (i=MAXFONTS*MAXFONTSTYLES*3; i<numFonts*MAXFONTSTYLES*3; i++) {
      free(fontInfoStr[i]);
   }
   if (altFontInfoStr) {
      for (i=0; i<MAXFONTS*MAXFONTSTYLES*3; i++) {
         if (altFontInfoStr[i] != NULL) {
            free(altFontInfoStr[i]);
         }
      }
      free(altFontInfoStr);
      altFontInfoStr = NULL;
   }
   free(fontInfoStr);
   fontInfoStr = NULL;

   for (fmly_index=0; fmly_index<numFonts; fmly_index++) {
      free(fontMenuStr[fmly_index]);
   }
   free(fontMenuStr);
   fontMenuStr = NULL;

   numFonts = MAXFONTS;
   numFakedFonts = 0;
   if (menuFontPtr != NULL) XFreeFont(mainDisplay, menuFontPtr);
   if (boldMsgFontPtr != NULL) XFreeFont(mainDisplay, boldMsgFontPtr);
   if (msgFontPtr != NULL) XFreeFont(mainDisplay, msgFontPtr);
   menuFontPtr = boldMsgFontPtr = msgFontPtr = NULL;
   if (rulerFontName != NULL) free(rulerFontName);
   if (defaultFontName != NULL) free(defaultFontName);
   if (menuFontName != NULL) free(menuFontName);
   if (boldMsgFontName != NULL) free(boldMsgFontName);
   if (msgFontName != NULL) free(msgFontName);
   rulerFontName = defaultFontName = NULL;
   menuFontName = boldMsgFontName = msgFontName = NULL;
}

/* The following procedure is used to generate pdrawFontAsc[] and */
/*    pDrawFontDes[], to be used in ``prtgif.c''.  It is supposed */
/*    to be called within dbx and not tgif.                       */

/*
 * static
 * void GenerateFontInfo ()
 * {
 *    register int	i, j, num_rows;
 * 
 *    for (i = 0; i < FONTTABLESIZE; i++)
 *       if (!myFontInfo[i].valid)
 *       {
 *          if ((myFontInfo[i].xfs =
 *                XLoadQueryFont (mainDisplay, fontNameStr[i])) == NULL)
 *          {
 *             printf ("Can not open %s.  Abort.\n\n", fontNameStr[i]);
 *             exit (-1);
 *          }
 *          myFontInfo[i].valid = TRUE;
 *       }
 * 
 *    num_rows = FONTTABLESIZE / MAXFONTSIZES;
 *    printf ("short\tpDrawFontAsc[] =\n{\n");
 *    for (i = 0; i < num_rows; i++)
 *    {
 *       printf ("   ");
 *       for (j = 0; j < MAXFONTSIZES; j++)
 *          if (i == num_rows-1 && j == MAXFONTSIZES-1)
 *             printf ("%2d ",
 *                   (myFontInfo[i*MAXFONTSIZES+j].xfs)->max_bounds.ascent);
 *          else
 *             printf ("%2d, ",
 *                   (myFontInfo[i*MAXFONTSIZES+j].xfs)->max_bounds.ascent);
 *       printf ("\n");
 *    }
 *    printf ("};\n\n");
 * 
 *    printf ("short\tpDrawFontDes[] =\n{\n");
 *    for (i = 0; i < num_rows; i++)
 *    {
 *       printf ("   ");
 *       for (j = 0; j < MAXFONTSIZES; j++)
 *          if (i == num_rows-1 && j == MAXFONTSIZES-1)
 *             printf ("%2d ",
 *                   (myFontInfo[i*MAXFONTSIZES+j].xfs)->max_bounds.descent);
 *          else
 *             printf ("%2d, ",
 *                   (myFontInfo[i*MAXFONTSIZES+j].xfs)->max_bounds.descent);
 *       printf ("\n");
 *    }
 *    printf ("};\n");
 * }
 */
