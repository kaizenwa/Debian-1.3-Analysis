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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/shortcut.c,v 3.2 1996/05/15 17:33:12 william Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include "const.h"
#include "types.h"

#include "msg.e"
#include "setup.e"
#ifndef _NO_EXTERN
#include "shortcut.e"
#endif

#define CNTRL_ONLY (ControlMask)
#define META_ONLY (Mod1Mask)
#define CNTRL_META (ControlMask|Mod1Mask)

struct ShortCutRec {
   char		code;
   unsigned int	state;
   char		name[80];
   short	num_args;
};

static struct ShortCutRec	shortCutXlateTbl[] =
{
/* Control Keys */
   { 'a', CNTRL_ONLY, "SelectAll()", 0, },
   { 'b', CNTRL_ONLY, "Back()", 0, },
   { 'c', CNTRL_ONLY, "ChangeDomain()", 0, },
   { 'd', CNTRL_ONLY, "Duplicate()", 0, },
   { 'e', CNTRL_ONLY, "PushCurChoice()", 0, },
   { 'f', CNTRL_ONLY, "Front()", 0, },
   { 'g', CNTRL_ONLY, "Group()", 0, },
   { 'i', CNTRL_ONLY, "Instantiate()", 0, },
   { 'k', CNTRL_ONLY, "Pop()", 0, },
   { 'l', CNTRL_ONLY, "AlignObjs()", 0, },
   { 'n', CNTRL_ONLY, "New()", 0, },
   { 'o', CNTRL_ONLY, "Open()", 0, },
   { 'p', CNTRL_ONLY, "Print()", 0, },
   { 'q', CNTRL_ONLY, "Quit()", 0, },
   { 'r', CNTRL_ONLY, "Redraw()", 0, },
   { 's', CNTRL_ONLY, "Save()", 0, },
   { 't', CNTRL_ONLY, "AlignToGrid()", 0, },
   { 'u', CNTRL_ONLY, "UnGroup()", 0, },
   { 'v', CNTRL_ONLY, "Push()", 0, },
   { 'w', CNTRL_ONLY, "DrawText()", 0, },
   { 'x', CNTRL_ONLY, "Delete()", 0, },
   { 'y', CNTRL_ONLY, "Copy()", 0, },
   { 'z', CNTRL_ONLY, "Animate()", 0, },
   { ',', CNTRL_ONLY, "ScrollLeft()", 0, },
   { '.', CNTRL_ONLY, "ScrollRight()", 0, },
   { '-', CNTRL_ONLY, "PrintWithCmd()", 0, },
/* Meta Keys */
   { 'a', META_ONLY,  "AttachAttrs()", 0, },
   { 'b', META_ONLY,  "Probe()", 0, },
   { 'c', META_ONLY,  "RotateCounter()", 0, },
   { 'd', META_ONLY,  "-Grid()", 0, },
   { 'e', META_ONLY,  "AnimateSend()", 0, },
   { 'f', META_ONLY,  "AnimateFlash()", 0, },
   { 'g', META_ONLY,  "ToggleGrid()", 0, },
   { 'h', META_ONLY,  "FlipHorizontal()", 0, },
   { 'i', META_ONLY,  "+Grid()", 0, },
   { 'j', META_ONLY,  "HideAttrName()", 0, },
   { 'k', META_ONLY,  "SelectMode()", 0, },
   { 'l', META_ONLY,  "DistributeObjs()", 0, },
   { 'm', META_ONLY,  "Move/JustfyAttr()", 0, },
   { 'n', META_ONLY,  "ShowAttrName()", 0, },
   { 'o', META_ONLY,  "ZoomOut()", 0, },
   { 'p', META_ONLY,  "Import()", 0, },
   { 'q', META_ONLY,  "DrawPoly()", 0, },
   { 'r', META_ONLY,  "DrawBox()", 0, },
   { 's', META_ONLY,  "Solve()", 0, },
   { 't', META_ONLY,  "DetachAttrs()", 0, },
   { 'u', META_ONLY,  "Undo()", 0, },
   { 'v', META_ONLY,  "FlipVertical()", 0, },
   { 'w', META_ONLY,  "RotateClockWise()", 0, },
   { 'x', META_ONLY,  "Escape()", 0, },
   { 'y', META_ONLY,  "Simulate()", 0, },
   { 'z', META_ONLY,  "ZoomIn()", 0, },
   { '9', META_ONLY,  "SpecifyAnArc()", 0, },
   { '0', META_ONLY,  "Update()", 0, },
   { ',', META_ONLY,  "ScrollUp()", 0, },
   { '.', META_ONLY,  "ScrollDown()", 0, },
   { '-', META_ONLY,  "ShowAttr()", 0, },
   { '{', META_ONLY,  "AlignObjsTop()", 0, },
   { '+', META_ONLY,  "AlignObjsMiddle()", 0, },
   { '}', META_ONLY,  "AlignObjsBottom()", 0, },
   { '[', META_ONLY,  "AlignObjsLeft()", 0, },
   { '=', META_ONLY,  "AlignObjsCenter()", 0, },
   { ']', META_ONLY,  "AlignObjsRight()", 0, },
   { '"', META_ONLY,  "MakeRegPolygon()", 0, },
   { '%', META_ONLY,  "SetReduction()", 0, },
   { ':', META_ONLY,  "DefaultZoom()", 0, },
   { '`', META_ONLY,  "ZoomWayOut()", 0, },
   { '~', META_ONLY,  "SaveNew()", 0, },
   { ';', META_ONLY,  "CutBit/Pixmap()", 0, },
   { '_', META_ONLY,  "AbutHorizontal()", 0, },
   { '|', META_ONLY,  "AbutVertical()", 0, },
   { '#', META_ONLY,  "BreakUpText()", 0, },
   { '^', META_ONLY,  "ScrollToOrig()", 0, },
   { '@', META_ONLY,  "ToggleMoveMode()", 0, },
   { '$', META_ONLY,  "SelectVertexMode()", 0, },
   { '&', META_ONLY,  "AlignToPage()", 0, },
   { '*', META_ONLY,  "Redo()", 0, },
   { '(', META_ONLY,  "ImportEPSFile()", 0, },
   { ')', META_ONLY,  "PreciseScale()", 0, },
   { '<', META_ONLY,  "Lock()", 0, },
   { '>', META_ONLY,  "UnLock()", 0, },
/* Control Meta Keys */
   { 'a', CNTRL_META, "AddPoint()", 0, },
   { 'b', CNTRL_META, "Bold()", 0, },
   { 'c', CNTRL_META, "Center()", 0, },
   { 'd', CNTRL_META, "DeletePoint()", 0, },
   { 'e', CNTRL_META, "DrawRCBox()", 0, },
   { 'f', CNTRL_META, "InvertXBitmap()", 0, },
   { 'g', CNTRL_META, "ToggleSnap()", 0, },
   { 'h', CNTRL_META, "HideAttr()", 0, },
   { 'i', CNTRL_META, "MakeIconic()", 0, },
   { 'j', CNTRL_META, "UnMakeIconic()", 0, },
   { 'k', CNTRL_META, "ToggleBW/ColorPS()", 0, },
   { 'l', CNTRL_META, "Left()", 0, },
   { 'm', CNTRL_META, "MakeSymbolic()", 0, },
   { 'n', CNTRL_META, "UnMakeSymbolic()", 0, },
   { 'o', CNTRL_META, "Roman()", 0, },
   { 'p', CNTRL_META, "BoldItalic()", 0, },
   { 'q', CNTRL_META, "DrawPolygon()", 0, },
   { 'r', CNTRL_META, "Right()", 0, },
   { 's', CNTRL_META, "SaveSelectedAs()", 0, },
   { 't', CNTRL_META, "Italic()", 0, },
   { 'u', CNTRL_META, "UpdateSymbols()", 0, },
   { 'v', CNTRL_META, "DrawOval()", 0, },
   { 'w', CNTRL_META, "ToggleLineType()", 0, },
   { 'x', CNTRL_META, "CyclePrintFormat()", 0, },
   { 'y', CNTRL_META, "Paste()", 0, },
   { 'z', CNTRL_META, "DrawArc()", 0, },
   { '.', CNTRL_META, "ImportXBitmap()", 0, },
   { ',', CNTRL_META, "ImportXPixmap()", 0, },
   { '-', CNTRL_META, "ToggleGridSystem()", 0, },
/* Miscellaneous */
   { '\0', 0, "ScrollPageUp()", 0, },
   { '\0', 0, "ScrollPageDown()", 0, },
   { '\0', 0, "ScrollPageLeft()", 0, },
   { '\0', 0, "ScrollPageRight()", 0, },
   { '\0', 0, "FlushUndoBuffer()", 0, },
   { '\0', 0, "PrintMsgBuffer()", 0, },
   { '\0', 0, "SaveOrigin()", 0, },
   { '\0', 0, "RestoreImageWH()", 0, },
   { '\0', 0, "UpdateEPS()", 0, },
   { '\0', 0, "ToggleMapShown()", 0, },
   { '\0', 0, "ToggleUseGrayScale()", 0, },
   { '\0', 0, "FreeHandMode()", 0, },
   { '\0', 0, "SaveSymInLibrary()", 0, },
   { '\0', 0, "CenterAnEndPoint()", 0, },
   { '\0', 0, "CenterAVertex()", 0, },
   { '\0', 0, "NextPage()", 0, },
   { '\0', 0, "PrevPage()", 0, },
   { '\0', 0, "NamePages()", 0, },
   { '\0', 0, "GotoPage()", 0, },
   { '\0', 0, "AddPageBefore()", 0, },
   { '\0', 0, "AddPageAfter()", 0, },
   { '\0', 0, "DeleteCurPage()", 0, },
   { '\0', 0, "TogglePageLineShown()", 0, },
   { '\0', 0, "SpecifyDrawingSize()", 0, },
   { '\0', 0, "PrintOnePage()", 0, },
   { '\0', 0, "ToggleNamedAttrShown()", 1, },
   { '\0', 0, "AttachFileAttrs()", 0, },
   { '\0', 0, "DetachFileAttrs()", 0, },
   { '\0', 0, "EditFileAttrs()", 0, },
   { '\0', 0, "PrintSelectedObjs()", 0, },
   { '\0', 0, "InputPolyPts()", 0, },
   { '\0', 0, "InputPolygonPts()", 0, },
   { '\0', 0, "EditAttrs()", 0, },
   { '\0', 0, "ConvertIntSpline()", 0, },
   { '\0', 0, "PasteFromFile()", 0, },
   { '\0', 0, "ToggleShowMeasurement()", 0, },
   { '\0', 0, "SetMeasureUnit()", 0, },
   { '\0', 0, "Cut()", 0, },
   { '\0', 0, "ToggleSmoothHinge()", 0, },
   { '\0', 0, "ToggleShowMenubar()", 0, },
   { '\0', 0, "ToggleShowStatus()", 0, },
   { '\0', 0, "BrowseXBitmap()", 0, },
   { '\0', 0, "BrowseXPixmap()", 0, },
   { '\0', 0, "SpecifyPaperSize()", 0, },
   { '\0', 0, "ToggleOneMotionSelMove()", 0, },
   { '\0', 0, "GoBack()", 0, },
   { '\0', 0, "GoForward()", 0, },
   { '\0', 0, "RefreshCurrent()", 0, },
   { '\0', 0, "HotList()", 0, },
   { '\0', 0, "AddCurrentToHotList()", 0, },
   { '\0', 0, "SessionHistory()", 0, },
   { '\0', 0, "ToggleHyperSpace()", 0, },
   { '\0', 0, "EmbedEPSFile()", 0, },
   { '\0', 0, "SetSelLineWidth()", 0, },
   { '\0', 0, "AddColor()", 0, },
   { '\0', 0, "ImportAttrs()", 0, },
   { '\0', 0, "ExportAttrs()", 0, },
   { '\0', 0, "MergeWithTable()", 0, },
   { '\0', 0, "ExportToTable()", 0, },
   { '\0', 0, "DeletePages()", 0, },
   { '\0', 0, "PrintOneFilePerPage()", 0, },
   { '\0', 0, "ImportGIFFile()", 0, },
   { '\0', 0, "SetExportPixelTrim()", 0, },
#ifdef _TGIF_WB
   { '\0', 0, "WhiteBoard()", 0, },
#endif /* _TGIF_WB */
   { '\0', 0, "ToggleColorLayers()", 0, },
   { '\0', 0, "ToggleStretchableText()", 0, },
   { '\0', 0, "BreakUpBit/Pixmap()", 0, },
   { '\0', 0, "LayoutOnArc()", 0, },
   { '\0', 0, "PreciseRotate()", 0, },
   { '\0', 0, "JoinPoly()", 0, },
   { '\0', 0, "CutPoly()", 0, },
   { '\0', 0, "GetBoundingBox()", 0, },
   { '\0', 0, "SetTemplate()", 0, },
   { '\0', 0, "MakeGray()", 0, },
   { '\0', 0, "InvertColor()", 0, },
   { '\0', 0, "InterpolateColor()", 0, },
   { '\0', 0, "BrightenDarken()", 0, },
   { '\0', 0, "ChangeSaturation()", 0, },
   { '\0', 0, "ChangeHue()", 0, },
   { '\0', 0, "ContrastEnhance()", 0, },
   { '\0', 0, "ColorBalance()", 0, },
   { '\0', 0, "Gamma()", 0, },
   { '\0', 0, "EdgeDetect()", 0, },
   { '\0', 0, "Emboss()", 0, },
   { '\0', 0, "ReduceColors()", 0, },
   { '\0', 0, "ReduceToPixmapColors()", 0, },
   { '\0', 0, "SetDefaultColorLevels()", 0, },
   { '\0', 0, "ReduceToDefaultColors()", 0, },
   { '\0', 0, "DefaultErrorDiffuse()", 0, },
   { '\0', 0, "Spread()", 0, },
   { '\0', 0, "Sharpen()", 0, },
   { '\0', 0, "Blur3()", 0, },
   { '\0', 0, "Blur5()", 0, },
   { '\0', 0, "Blur7()", 0, },
   { '\0', 0, "RunBggen()", 0, },
   { '\0', 0, "CircularBggen()", 0, },
   { '\0', 0, "RegenerateImage()", 0, },
   { '\0', 0, "CropImage()", 0, },
   { '\0', 0, "GetColor()", 0, },
   { '\0', 0, "ReplaceColor()", 0, },
   { '\0', 0, "FloodFill()", 0, },
   { '\0', 0, "CreateContour()", 0, },
   { '\0', 0, "Subtract()", 0, },
   { '\0', 0, "AlphaCombine()", 0, },
   { '\0', 0, "ImportOtherFile()", 0, },
   { '\0', 0, "ImportOtherFileType()", 1, },
   { '\0', 0, "BrowseOther()", 0, },
   { '\0', 0, "BrowseOtherType()", 1, },
   { '\0', 0, "ToggleShowCrossHair()", 0, },
   { '\0', 0, "", 0 }
};

static int	maxShortCuts = 0;
static int	shortCutIndex[256];
static char	*shortCutArgs[256];

void InitShortCut ()
{
   int i, j;
   unsigned int code;
   int precise_match, ok, len;
   char *c_ptr, *saved_c_ptr, *tmp_c_ptr, *buf;

   for (i = 0; i < 256; i++) {
      shortCutIndex[i] = INVALID;
      shortCutArgs[i] = NULL;
   }

   maxShortCuts = 0;
   if ((c_ptr=XGetDefault(mainDisplay, TOOL_NAME, "ShortCuts")) == NULL) {
      return;
   }
   len = strlen(c_ptr);
   if ((buf=(char *)malloc((len+1)*sizeof(char))) == NULL) {
      FailAllocMessage();
      return;
   }
   while (*c_ptr!=':' && *c_ptr!='!' && *c_ptr!='<' && *c_ptr!='\0') c_ptr++;
   while (*c_ptr != '\0') {
      saved_c_ptr = c_ptr;
      precise_match = FALSE;
      ok = TRUE;
      if ((*c_ptr==':' || *c_ptr=='!') && strncmp(&c_ptr[1],"<Key>",5)==0)
      {
         precise_match = TRUE;
         strcpy (buf, &c_ptr[6]);
      } else if (strncmp(c_ptr,"<Key>",5)==0) {
         strcpy (buf, &c_ptr[5]);
      } else {
         ok = FALSE;
      }
      if (ok) {
         tmp_c_ptr = buf;
         while (*tmp_c_ptr != ')' && *tmp_c_ptr != '\0') tmp_c_ptr++;
         if (*tmp_c_ptr == '\0') {
            ok = FALSE;
         } else {
            *(++tmp_c_ptr) = '\0';
            code = buf[0]&0xff;
            if (buf[0] != '\0' && buf[1] == ':' &&
                  ((code>0x20 && code<=0x7f) || (code>0xa0 && code<=0xff))) {
               char *paren_ptr;

               if ((paren_ptr=strchr(&buf[2], '(')) == NULL) {
                  fprintf(stderr, "Invalid shortcut entry:  '%s'; skipped.\n",
                        buf);
                  continue;
               }
               len = paren_ptr-(&buf[2]);
               for (j = 0; *(shortCutXlateTbl[j].name) != '\0'; j++) {
                  if (strncmp(shortCutXlateTbl[j].name, &buf[2], len) == 0) {
                     if (shortCutXlateTbl[j].num_args != 0) {
                        shortCutArgs[code] = (char*)malloc(
                              (strlen(&buf[len+2])+1)*sizeof(char));
                        if (shortCutArgs[code] == NULL) FailAllocMessage();
                        strcpy(shortCutArgs[code], ++paren_ptr);
                     }
                     if (shortCutIndex[code] != INVALID)
                        fprintf(stderr, "Warning:  %s for '%c'.\n",
                              "duplicate shortcut entry", code);
                     shortCutIndex[code] = j;
                     if (!precise_match) {
                        if (*buf>='a' && *buf<='z') {
                           code = *buf-'a'+'A';
                           if (shortCutIndex[code] != INVALID)
                              fprintf(stderr, "Warning:  %s for '%c'.\n",
                                    "duplicate shortcut entry", code);
                           shortCutIndex[code] = j;
                        } else if (*buf>='A' && *buf<='Z') {
                           code = *buf-'A'+'a';
                           if (shortCutIndex[code] != INVALID)
                              fprintf(stderr, "Warning:  %s for '%c'.\n",
                                    "duplicate shortcut entry", code);
                           shortCutIndex[code] = j;
                        }
                     }
                     maxShortCuts++;
                     break;
                  }
               }
               if (*(shortCutXlateTbl[j].name) == '\0') {
                  fprintf(stderr, "Invalid shortcut entry:  '%s'; skipped.\n",
                        buf);
               }
            } else {
               fprintf(stderr, "Invalid shortcut entry:  '%s'; skipped.\n",
                     buf);
            }
         }
      }
      if (ok) {
         while (*c_ptr!=')' && *c_ptr!='\0') c_ptr++;
         while (*c_ptr!=':' && *c_ptr!='!' && *c_ptr!='<' && *c_ptr!='\0') {
            c_ptr++;
         }
      } else {
         fprintf(stderr, "Invalid shortcut:  '%s'.\n", saved_c_ptr);
         break;
      }
   }
   free(buf);
}

void CleanUpShortCut()
{
   register int i;

   for (i=0; i < 256; i++) {
      if (shortCutArgs[i] != NULL) {
         free(shortCutArgs[i]);
      }
   }
}

int FetchShortCut (c, code, state, name, args)
   int		c;
   char		* code, * * name, * args;
   unsigned int	* state;
{
   if (shortCutIndex[c] == INVALID) return (FALSE);

   *code = shortCutXlateTbl[shortCutIndex[c]].code;
   *state = shortCutXlateTbl[shortCutIndex[c]].state;
   *name = shortCutXlateTbl[shortCutIndex[c]].name;
   if (shortCutXlateTbl[shortCutIndex[c]].num_args == 0) {
      *args = '\0';
   } else {
      strcpy(args, shortCutArgs[c]);
   }
   return (TRUE);
}

int ValidShortCut(name, num_args, code, state)
   char *name, *code;
   int num_args;
   unsigned int *state;
{
   int j, len=strlen(name);

   if (len <= 2) return FALSE;
   for (j=0; *(shortCutXlateTbl[j].name) != '\0'; j++) {
      if (num_args == ((int)shortCutXlateTbl[j].num_args) &&
            strncmp(shortCutXlateTbl[j].name, name, len-2) == 0 &&
            shortCutXlateTbl[j].name[len] == '(' &&
            shortCutXlateTbl[j].name[len+1] == ')') {
         *code = shortCutXlateTbl[j].code;
         *state = shortCutXlateTbl[j].state;
         return TRUE;
      }
   }
   return FALSE;
}
