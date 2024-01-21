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
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/ps.c,v 3.0 1996/05/06 16:06:59 william Exp $";
#endif

#include <stdio.h>
#include <X11/Xlib.h>

#include "const.h"
#include "types.h"

#include "msg.e"
#ifndef _NO_EXTERN
#include "ps.e"
#endif
#include "util.e"

#define DEF_PS_DICT_COUNT 2

int preDumpSetup=FALSE;
int needsTgifOrigCTM=TRUE;

static int psDictCount=DEF_PS_DICT_COUNT;

static int psUsePSAdobe=TRUE;
static int psUseMinRadius=FALSE;
static int psUseEllipse=FALSE;
static int psUseArrow=FALSE;
static int psUseArc=FALSE;
static int psUsePattern=FALSE;
static int psUseBWPattern=FALSE;
static int psUseColorImage=FALSE;
static int psUseColorPattern=FALSE;
static int psUseCenterText=FALSE;
static int psUseRightText=FALSE;
static int psUseReencode=FALSE;

void PSUsePSAdobe()
{
   if (preDumpSetup && !psUsePSAdobe) {
      psUsePSAdobe = FALSE;
   }
}

void PSUseMinRadius()
{
   if (preDumpSetup && !psUseMinRadius) {
      psUseMinRadius = TRUE;
      psDictCount++;
   }
}

void PSUseEllipse()
{
   if (preDumpSetup && !psUseEllipse) {
      psUseEllipse = TRUE;
      psDictCount += 2;
   }
}

void PSUseArrow()
{
   if (preDumpSetup && !psUseArrow) {
      psUseArrow = TRUE;
      psDictCount += 2;
   }
}

void PSUseArc()
{
   if (preDumpSetup && !psUseArc) {
      psUseArc = TRUE;
      psDictCount += 3;
   }
}

static
void PSUsePattern()
{
   if (preDumpSetup && !psUsePattern) {
      psUsePattern = TRUE;
      psDictCount += 29;
   }
}

void PSUseBWPattern()
{
   if (preDumpSetup && !psUseBWPattern) {
      PSUsePattern();
      psUseBWPattern = TRUE;
      psDictCount += 5;
   }
}

void PSUseColorImage()
{
   if (preDumpSetup && !psUseColorImage) {
      psUseColorImage = TRUE;
      psDictCount += 11;
   }
}

void PSUseColorPattern()
{
   if (preDumpSetup && !psUseColorPattern) {
      PSUsePattern();
      psUseColorPattern = TRUE;
      psDictCount += 4;
   }
}

void PSUseCenterText()
{
   if (preDumpSetup && !psUseCenterText) {
      psUseCenterText = TRUE;
      psDictCount += 1;
   }
}

void PSUseRightText()
{
   if (preDumpSetup && !psUseRightText) {
      psUseRightText = TRUE;
      psDictCount += 1;
   }
}

typedef struct ReEncodeRec {
   char *font_name;
   struct ReEncodeRec *next;
} *ReEncodePtr;

static struct ReEncodeRec *topReEncodeInfo=NULL;

void PSUseReencode(font_name)
   char *font_name;
{
   if (preDumpSetup && !psUseReencode) {
      psUseReencode = TRUE;
      psDictCount += 2;
   }
   if (preDumpSetup && psUseReencode) {
      struct ReEncodeRec *p_reencode;

      for (p_reencode=topReEncodeInfo; p_reencode != NULL;
            p_reencode=p_reencode->next) {
         if (strcmp(p_reencode->font_name, font_name) == 0) {
            return;
         }
      }
      p_reencode = (struct ReEncodeRec *)malloc(sizeof(struct ReEncodeRec));
      if (p_reencode == NULL) FailAllocMessage();
      p_reencode->font_name = UtilStrDup(font_name);
      p_reencode->next = topReEncodeInfo;
      topReEncodeInfo = p_reencode;
      psDictCount += 1;
   }
}

static
void CleanUpReEncodeInfo()
{
   struct ReEncodeRec *p_next_reencode;

   for ( ; topReEncodeInfo != NULL; topReEncodeInfo=p_next_reencode) {
      p_next_reencode = topReEncodeInfo->next;
      free(topReEncodeInfo->font_name);
      free(topReEncodeInfo);
   }
}

void ResetPSInfo()
{
   psDictCount = DEF_PS_DICT_COUNT;
   preDumpSetup = TRUE;

   needsTgifOrigCTM = TRUE;

   psUsePSAdobe=TRUE;
#ifdef INVERT_CTM_BUG
   UseMinRadius();
#else /* ~INVERT_CTM_BUG */
   psUseMinRadius = FALSE;
#endif /* INVERT_CTM_BUG */
   psUseEllipse=FALSE;
   psUseArrow=FALSE;
   psUseArc=FALSE;
   psUsePattern=FALSE;
   psUseBWPattern=FALSE;
   psUseColorImage=FALSE;
   psUseColorPattern=FALSE;
   psUseCenterText=FALSE;
   psUseRightText=FALSE;
   psUseReencode=FALSE;
   CleanUpReEncodeInfo();
}

void DoneResetPSInfo()
{
   preDumpSetup = FALSE;
   CleanUpReEncodeInfo();
}

static char *psAdobeMacro[] =
{ "%",
  "%\tDue to bugs in Transcript, the 'PS-Adobe-' stuff is omitted from line 1",
  "%",
  "",
  NULL
};

static char *psMinRadiusMacro[] = {
  "%",
  "%\tUsing a zero value radius for an ellipse or an arc would result",
  "%\t\tin a non-invertible CTM matrix which causes problem when this",
  "%\t\twhen this PostScript is wrapped inside other routines, such as",
  "%\t\tthe multi.ps package from",
  "%\t\tftp.ucc.su.oz.au:/pub/ps_printing/multi.  You can overcome such",
  "%\t\terror by uncommenting the sole line of the procedure below:",
  "%",
  "/tgif_min_radius",
  " {",
  "%    dup 0.01 lt { pop 0.01 } if",
  " } bind def",
  "",
  NULL
};

static char *psEllipseMacro[] = {
  "/tgifellipsedict 6 dict def",
  "tgifellipsedict /mtrx matrix put",
  "",
  "/tgifellipse",
  " { tgifellipsedict begin",
  "      /yrad exch def",
  "      /xrad exch def",
  "      /y exch def",
  "      /x exch def",
  "      /savematrix mtrx currentmatrix def",
  "      x y translate",
  "      xrad yrad scale",
  "      0 0 1 0 360 arc",
  "      savematrix setmatrix",
  "   end",
  " } def",
  "",
  NULL
};

static char *psArrowMacro[] = {
  "/tgifarrowtipdict 8 dict def",
  "tgifarrowtipdict /mtrx matrix put",
  "",
  "/tgifarrowtip",
  " { tgifarrowtipdict begin",
  "      /dy exch def",
  "      /dx exch def",
  "      /h exch def",
  "      /w exch def",
  "      /y exch def",
  "      /x exch def",
  "      /savematrix mtrx currentmatrix def",
  "      x y translate",
  "      dy dx atan rotate",
  "      0 0 moveto",
  "      w neg h lineto",
  "      w neg h neg lineto",
  "      savematrix setmatrix",
  "   end",
  " } def",
  "",
  NULL
};

static char *psArcMacro[] = {
  "/tgifarcdict 8 dict def",
  "tgifarcdict /mtrx matrix put",
  "",
  "/tgifarcn",
  " { tgifarcdict begin",
  "      /endangle exch def",
  "      /startangle exch def",
  "      /yrad exch def",
  "      /xrad exch def",
  "      /y exch def",
  "      /x exch def",
  "      /savematrix mtrx currentmatrix def",
  "      x y translate",
  "      xrad yrad scale",
  "      0 0 1 startangle endangle arc",
  "      savematrix setmatrix",
  "   end",
  " } def",
  "",
  "/tgifarc",
  " { tgifarcdict begin",
  "      /endangle exch def",
  "      /startangle exch def",
  "      /yrad exch def",
  "      /xrad exch def",
  "      /y exch def",
  "      /x exch def",
  "      /savematrix mtrx currentmatrix def",
  "      x y translate",
  "      xrad yrad scale",
  "      0 0 1 startangle endangle arcn",
  "      savematrix setmatrix",
  "   end",
  " } def",
  "",
  NULL
};

static char *psBWPatternMacro[] = {
  "/tgifsetuserscreendict 22 dict def",
  "tgifsetuserscreendict begin",
  "   /tempctm matrix def",
  "   /temprot matrix def",
  "   /tempscale matrix def",
  "",
  "   /concatprocs",
  "    { /proc2 exch cvlit def",
  "      /proc1 exch cvlit def",
  "      /newproc proc1 length proc2 length add array def",
  "      newproc 0 proc1 putinterval",
  "      newproc proc1 length proc2 putinterval",
  "      newproc cvx",
  "    } def",
  "   /resmatrix matrix def",
  "   /findresolution",
  "    { 72 0 resmatrix defaultmatrix dtransform",
  "      /yres exch def /xres exch def",
  "      xres dup mul yres dup mul add sqrt",
  "    } def",
  "end",
  "",
  "/tgifsetuserscreen",
  " { tgifsetuserscreendict begin",
  "      /spotfunction exch def",
  "      /screenangle exch def",
  "      /cellsize exch def",
  "",
  "      /m tempctm currentmatrix def",
  "      /rm screenangle temprot rotate def",
  "      /sm cellsize dup tempscale scale def",
  "",
  "      sm rm m m concatmatrix m concatmatrix pop",
  "",
  "      1 0 m dtransform /y1 exch def /x1 exch def",
  "",
  "      /veclength x1 dup mul y1 dup mul add sqrt def",
  "      /frequency findresolution veclength div def",
  "",
  "      /newscreenangle y1 x1 atan def",
  "",
  "      m 2 get m 1 get mul m 0 get m 3 get mul sub 0 gt",
  "",
  "      {{neg} /spotfunction load concatprocs",
  "         /spotfunction exch def",
  "      } if",
  "",
  "      frequency newscreenangle /spotfunction load setscreen",
  "   end",
  " } def",
  "",
  "/tgifsetpatterndict 18 dict def",
  "tgifsetpatterndict begin",
  "   /bitison",
  "    { /ybit exch def /xbit exch def",
  "      /bytevalue bstring ybit bwidth mul xbit 8 idiv add get def",
  "",
  "      /mask 1 7 xbit 8 mod sub bitshift def",
  "      bytevalue mask and 0 ne",
  "    } def",
  "end",
  "",
  "/tgifbitpatternspotfunction",
  " { tgifsetpatterndict begin",
  "      /y exch def /x exch def",
  "",
  "      /xindex x 1 add 2 div bpside mul cvi def",
  "      /yindex y 1 add 2 div bpside mul cvi def",
  "",
  "      xindex yindex bitison",
  "       { /onbits onbits 1 add def 1 }",
  "       { /offbits offbits 1 add def 0 }",
  "       ifelse",
  "   end",
  " } def",
  "",
  "/tgifsetpattern",
  " { tgifsetpatterndict begin",
  "      /cellsz exch def",
  "      /angle exch def",
  "      /bwidth exch def",
  "      /bpside exch def",
  "      /bstring exch def",
  "",
  "      /onbits 0 def /offbits 0 def",
  "      cellsz angle /tgifbitpatternspotfunction load tgifsetuserscreen",
  "      {} settransfer",
  "      offbits offbits onbits add div setgray",
  "   end",
  " } def",
  "",
  NULL
};

static char *psColorImageMacro[] = {
  "/tgifxpmdict 4 dict def",
  "/tgifbwpicstr 1 string def",
  "/tgifcolorpicstr 3 string def",
  "",
  "/tgifsetpixels { tgifxpmdict begin /pixels exch def end } def",
  "",
  "/tgifsetpix { tgifxpmdict begin pixels 3 1 roll putinterval end } def",
  "",
  "/tgifcolorspot",
  " { tgifxpmdict begin",
  "      /index exch def",
  "      pixels index 3 mul 3 getinterval aload pop",
  "      255 mul cvi tgifcolorpicstr 2 3 -1 roll put",
  "      255 mul cvi tgifcolorpicstr 1 3 -1 roll put",
  "      255 mul cvi tgifcolorpicstr 0 3 -1 roll put",
  "      tgifcolorpicstr",
  "   end",
  " } def",
  "",
  "/tgifnewcolorspot",
  " { tgifxpmdict begin",
  "      /index exch def",
  "      pixels index 3 mul 3 getinterval aload pop setrgbcolor",
  "   end",
  " } def",
  "",
  "/tgifcolordict 4 dict def",
  "",
  "/colorimage where",
  " { pop }",
  " { /colorimage",
  "   { tgifcolordict begin",
  "        pop pop pop pop pop",
  "        /ih exch def",
  "        /iw exch def",
  "        /x 0 def",
  "        /y 0 def",
  "        1 1 ih",
  "         { pop 1 1 iw",
  "            { pop currentfile",
  "              tgifbwpicstr readhexstring pop 0 get tgifnewcolorspot",
  "              x y moveto 1 0 rlineto 0 1 rlineto -1 0 rlineto",
  "              closepath fill",
  "              /x x 1 add def",
  "            } for",
  "           /y y 1 add def",
  "           /x 0 def",
  "         } for",
  "     end",
  "   } def",
  " } ifelse",
  "",
  "/tgiftranscolorspot",
  " { tgifxpmdict begin",
  "      /index exch def",
  "      pixels index 3 mul 3 getinterval aload pop",
  "      dup 0 lt { pop pop pop false } { setrgbcolor true } ifelse",
  "   end",
  " } def",
  "",
  "/tgiftranscolorimage",
  " { tgifcolordict begin",
  "      pop pop pop pop pop",
  "      /ih exch def",
  "      /iw exch def",
  "      /x 0 def",
  "      /y 0 def",
  "      1 1 ih",
  "       { pop 1 1 iw",
  "          { pop currentfile",
  "            tgifbwpicstr readhexstring pop 0 get tgiftranscolorspot",
  "            { x y moveto 1 0 rlineto 0 1 rlineto -1 0 rlineto",
  "              closepath fill",
  "            }",
  "            if",
  "            /x x 1 add def",
  "          } for",
  "         /y y 1 add def",
  "         /x 0 def",
  "       } for",
  "   end",
  " } def",
  "",
  NULL
};

static char *psColorPatternMacro[] = {
  "/tgifpatdict 10 dict def",
  "",
  "/tgifpatbyte",
  " { currentdict /retstr get exch",
  "   pat i cellsz mod get put",
  " } def",
  "",
  "/tgifpatproc",
  " { 0 1 widthlim {tgifpatbyte} for retstr",
  "   /i i 1 add def",
  " } def",
  "",
  "/tgifpatfill",
  " { tgifpatdict begin",
  "      /h exch def",
  "      /w exch def",
  "      /lty exch def",
  "      /ltx exch def",
  "      /cellsz exch def",
  "      /pat exch def",
  "",
  "      /widthlim w cellsz div cvi 1 sub def",
  "      /retstr widthlim 1 add string def",
  "      /i 0 def",
  "",
  "      tgiforigctm setmatrix",
  "      ltx lty translate",
  "      w h true [1 0 0 1 0 0] {tgifpatproc} imagemask",
  "      ltx neg lty neg translate",
  "   end",
  " } def",
  "",
  NULL
};

static char *psPatternMacro[] = {
  "/pat3 <8000000008000000> def",
  "/pat4 <8800000022000000> def",
  "/pat5 <8800220088002200> def",
  "/pat6 <8822882288228822> def",
  "/pat7 <aa55aa55aa55aa55> def",
  "/pat8 <77dd77dd77dd77dd> def",
  "/pat9 <77ffddff77ffddff> def",
  "/pat10 <77ffffff77ffffff> def",
  "/pat11 <7fffffff7fffffff> def",
  "/pat12 <8040200002040800> def",
  "/pat13 <40a00000040a0000> def",
  "/pat14 <ff888888ff888888> def",
  "/pat15 <ff808080ff080808> def",
  "/pat16 <f87422478f172271> def",
  "/pat17 <038448300c020101> def",
  "/pat18 <081c22c180010204> def",
  "/pat19 <8080413e080814e3> def",
  "/pat20 <8040201008040201> def",
  "/pat21 <8844221188442211> def",
  "/pat22 <77bbddee77bbddee> def",
  "/pat23 <c1e070381c0e0783> def",
  "/pat24 <7fbfdfeff7fbfdfe> def",
  "/pat25 <3e1f8fc7e3f1f87c> def",
  "/pat26 <0102040810204080> def",
  "/pat27 <1122448811224488> def",
  "/pat28 <eeddbb77eeddbb77> def",
  "/pat29 <83070e1c3870e0c1> def",
  "/pat30 <fefdfbf7efdfbf7f> def",
  "/pat31 <7cf8f1e3c78f1f3e> def",
  "",
  NULL
};

static char *psCenterTextMacro[] = {
  "/tgifcentertext { dup stringwidth pop 2 div neg 0 rmoveto } def",
  "",
  NULL
};

static char *psRightTextMacro[] = {
  "/tgifrighttext { dup stringwidth pop neg 0 rmoveto } def",
  "",
  NULL
};

static char *psReencodeMacro[] = {
  "/tgifreencsmalldict 12 dict def",
  "/tgifReEncodeSmall",
  " { tgifreencsmalldict begin",
  "      /newcodesandnames exch def",
  "      /newfontname exch def",
  "      /basefontname exch def",
  "",
  "      /basefontdict basefontname findfont def",
  "      /newfont basefontdict maxlength dict def",
  "",
  "      basefontdict",
  "      { exch dup /FID ne",
#ifdef GS_REENCODE_BUG
  "        1 index /UniqueID ne and",
#endif
  "         { dup /Encoding eq",
  "            { exch dup length array copy newfont 3 1 roll put }",
  "            { exch newfont 3 1 roll put }",
  "            ifelse",
  "         }",
  "         { pop pop }",
  "         ifelse",
  "      }",
  "      forall",
  "",
  "      newfont /FontName newfontname put",
  "      newcodesandnames aload pop",
  "",
  "      newcodesandnames length 2 idiv",
  "      { newfont /Encoding get 3 1 roll put}",
  "      repeat",
  "",
  "      newfontname newfont definefont pop",
  "   end",
  " } def",
  "",
  NULL
};

void DumpPSMacro (FP)
   FILE	* FP;
{
   register int	i;

   fprintf (FP, "\n");
   if (!psUsePSAdobe) {
      for (i = 0; psAdobeMacro[i] != NULL; i++) {
         fprintf (FP, "%s\n", psAdobeMacro[i]);
      }
   }

   fprintf (FP, "/tgifdict %1d dict def\n", psDictCount);
   fprintf (FP, "tgifdict begin\n");
   fprintf (FP, "\n");

   if (psUseMinRadius) {
      for (i = 0; psMinRadiusMacro[i] != NULL; i++) {
         fprintf (FP, "%s\n", psMinRadiusMacro[i]);
      }
   }
   if (psUseEllipse) {
      for (i = 0; psEllipseMacro[i] != NULL; i++) {
         fprintf (FP, "%s\n", psEllipseMacro[i]);
      }
   }
   if (psUseArrow) {
      for (i = 0; psArrowMacro[i] != NULL; i++) {
         fprintf (FP, "%s\n", psArrowMacro[i]);
      }
   }
   if (psUseArc) {
      for (i = 0; psArcMacro[i] != NULL; i++) {
         fprintf (FP, "%s\n", psArcMacro[i]);
      }
   }
   if (psUseBWPattern) {
      for (i = 0; psBWPatternMacro[i] != NULL; i++) {
         fprintf (FP, "%s\n", psBWPatternMacro[i]);
      }
   }
   if (psUseColorImage) {
      for (i = 0; psColorImageMacro[i] != NULL; i++) {
         fprintf (FP, "%s\n", psColorImageMacro[i]);
      }
   }
   if (psUseColorPattern) {
      for (i = 0; psColorPatternMacro[i] != NULL; i++) {
         fprintf (FP, "%s\n", psColorPatternMacro[i]);
      }
   }
   if (psUsePattern) {
      for (i = 0; psPatternMacro[i] != NULL; i++) {
         fprintf (FP, "%s\n", psPatternMacro[i]);
      }
   }
   if (psUseCenterText) {
      for (i = 0; psCenterTextMacro[i] != NULL; i++) {
         fprintf (FP, "%s\n", psCenterTextMacro[i]);
      }
   }
   if (psUseRightText) {
      for (i = 0; psRightTextMacro[i] != NULL; i++) {
         fprintf (FP, "%s\n", psRightTextMacro[i]);
      }
   }
   if (psUseReencode) {
      for (i = 0; psReencodeMacro[i] != NULL; i++) {
         fprintf (FP, "%s\n", psReencodeMacro[i]);
      }
   }
}

int ParsePsAdobeString (s, use_ps_adobe_string, adobe_str, epsf_str)
   char	* s, * adobe_str, * epsf_str;
   int	* use_ps_adobe_string;
{
   register char	* c_ptr=s, * c_ptr1;

   if (!(strcmp ("false", c_ptr) == 0 || strcmp ("False", c_ptr) == 0))
   {
      if (*c_ptr >= '0' && *c_ptr <= '9')
      {
         *use_ps_adobe_string = TRUE;
         for (c_ptr1 = c_ptr; *c_ptr1 != '\0'; c_ptr1++)
            if (*c_ptr1 == '/')
            {
               *c_ptr1 = ' ';
               break;
            }
         sscanf (c_ptr, "%s", adobe_str);
         c_ptr = &c_ptr[strlen(adobe_str)];
         switch (*c_ptr)
         {
            case '\0': break;
            case ' ':
               c_ptr++;
               if (*c_ptr >= '0' && *c_ptr <= '9')
                  sscanf (c_ptr, "%s", epsf_str);
               else
                  return (FALSE);
               break;
            default: return (FALSE);
         }
      }
      else
         return (FALSE);
   }
   return (TRUE);
}
