/*
**
** doc_misc.c
**
** Copyright (C) 1995-1997 Johannes Plass
** 
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
** 
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
** 
** Author:   Johannes Plass (plass@thep.physik.uni-mainz.de)
**           Department of Physic
**           Johannes Gutenberg-University
**           Mainz, Germany
**
*/

/*
#define MESSAGES
*/
#include "message.h"

#include <stdlib.h>
#include <stdio.h>

#include "paths.h"
#include INC_X11(Intrinsic.h)
#include INC_XMU(CharSet.h)
#include "Ghostview.h"
#include "main_resources.h"

#include "gv.h"
#include "ps.h"

/*##################################################################*/
/* doc_scanFile */
/*##################################################################*/

int
doc_scanFile(fPP,docP,file_name,file_name_scanP,cmd_scan_pdf)
   FILE ** fPP;
   Document *docP;
   String file_name;
   String *file_name_scanP;
   String cmd_scan_pdf;
{
   Document d;
   int i,ret;

   BEGINMESSAGE(doc_scanFile)
   d = (Document)NULL;
   ret = 0;
   if (*fPP && file_name && app_res.respect_dsc) d = psscan(fPP,file_name,file_name_scanP,cmd_scan_pdf);
   if (d) {
      d->labels_useful=1;
      d->structured   =0;
      if ((!d->epsf && d->numpages>0) || (d->epsf && d->numpages>1)) {
         ret = d->structured = 1;
         if (d->numpages == 1) d->labels_useful=1;
         else for (i = 1; i < d->numpages; i++)
            if (strcmp(d->pages[i-1].label,d->pages[i].label)) { d->labels_useful = 1; break; }
      }
   }
   *docP = d;

#  ifdef MESSAGES
     if (!d) { INFMESSAGE(unsuccessful) }
     else {
         INFMESSAGE(successful)
         if (d->structured)    { INFMESSAGE(structured)    } else { INFMESSAGE(not structured)    }
         if (d->labels_useful) { INFMESSAGE(labels useful) } else { INFMESSAGE(labels not useful) }
     }
#  endif
   ENDMESSAGE(doc_scanFile)
   return ret;
}

/*##################################################################*/
/* doc_putPageInRange */
/*##################################################################*/

int
doc_putPageInRange(d,pagenumber)
   Document d;
   int pagenumber;
{
   BEGINMESSAGE(doc_putPageInRange)
   if (d && d->structured) {
      if (pagenumber >= (int)(d->numpages)) pagenumber = ((int)d->numpages)-1;
      if (pagenumber < 0) pagenumber = 0;
   } else pagenumber = 0;
   ENDMESSAGE(doc_putPageInRange)
   return pagenumber;
}    

/*############################################################*/
/* doc_preferredMediaOfPage */
/*############################################################*/

int
doc_preferredMediaOfPage(d,pagenumber,llxP,llyP,urxP,uryP)
   Document d;
   int pagenumber;
   int *llxP,*llyP,*urxP,*uryP;
{
   int *dbb = NULL;
   struct documentmedia *media = NULL;
   int retry;

   BEGINMESSAGE(doc_preferredMediaOfPage)

   if (!d) {
      INFMESSAGE(no document) ENDMESSAGE(doc_preferredMediaOfPage)
      return(MEDIA_ID_INVALID);
   }

   retry=1;
#  define BB_VALID (dbb[URX]>dbb[LLX] && dbb[URY]>dbb[LLY])
   if (d->structured && 0<=pagenumber && pagenumber<(int)d->numpages)
      { dbb = d->pages[pagenumber].boundingbox;  if BB_VALID retry=0; }
   if (retry && d->structured)
      { dbb = d->default_page_boundingbox; if BB_VALID retry=0; }
   if (retry && d->epsf)
      { dbb = d->boundingbox;              if BB_VALID retry=0; }
#  undef BB_VALID
   if (!retry) {
      INFMESSAGE(MEDIA_ID_BB) ENDMESSAGE(doc_preferredMediaOfPage)
      *llxP=dbb[LLX]; *llyP=dbb[LLY]; *urxP=dbb[URX]; *uryP=dbb[URY];
      return(MEDIA_ID_BB);
   }

   if (d->structured && 0<=pagenumber && pagenumber<(int)d->numpages)
      { media = d->pages[pagenumber].media;  if (media) retry=0; }
   if (retry)
      { media = d->default_page_media; if (media) retry=0; }
   if (!retry) {
      struct documentmedia *tmp;
      int i,m=MEDIA_ID_INVALID;
      for (tmp=d->media,i=0; i<d->nummedia; i++,tmp++)
          if (media==tmp) { m = i; break; }
      if (m!=MEDIA_ID_INVALID) {
         INFIMESSAGE(doc prefers media,m) ENDMESSAGE(doc_preferredMediaOfPage)
         *llxP=*llyP=0; *urxP=media->width-1; *uryP=media->height-1;
         return(m);
      }
   }
   INFMESSAGE(MEDIA_ID_INVALID) ENDMESSAGE(doc_preferredMediaOfPage)
   return(MEDIA_ID_INVALID);
} 


/*##################################################################*/
/* doc_preferredOrientationOfPage */
/*##################################################################*/

int
doc_preferredOrientationOfPage(d,page)
   Document d;
   int page;
{
   int o;

   BEGINMESSAGE(doc_preferredOrientationOfPage)
   o  = O_NONE;
   if (d) {
      if (d->structured && 0<=page && page<(int)d->numpages && d->pages[page].orientation != O_NONE)
         o = d->pages[page].orientation;
      else if (d->default_page_orientation != O_NONE)
         o = d->default_page_orientation;
      else if (d->orientation != O_NONE)
         o = d->orientation;
   }
   ENDMESSAGE(doc_preferredOrientationOfPage)
   return(o);
}

/*############################################################*/
/* doc_convStringToPage */
/*############################################################*/

int
doc_convStringToPage(d,pageLabel)
   Document d;
   String pageLabel;
{
   int i,j;
   int page;

   BEGINMESSAGE(doc_convStringToPage)
   page=-1;
   if (pageLabel && d && d->labels_useful) for (i=0; i<d->numpages; i++) {
      INFMESSAGE(0)
      if (d->pageorder == DESCEND) j = (int)d->numpages-1-i;
      else                         j = i;
      if (!strcmp(pageLabel,d->pages[j].label)) { page=i; break; }
   }
      INFMESSAGE(1)
   if (page<0 && pageLabel) page=atoi(pageLabel)-1;
   if (page<0) page=0;
   IMESSAGE(page)
   ENDMESSAGE(doc_convStringToPage)
   return(page);
}

/*############################################################*/
/* doc_convDocOrientToXtOrient */
/*############################################################*/

XtPageOrientation
doc_convDocOrientToXtOrient(orientation,swapLandscape)
   int orientation;
   int swapLandscape;
{
   XtPageOrientation ret;

   BEGINMESSAGE(doc_convDocOrientToXtOrient)

   switch (orientation) {
   case O_PORTRAIT:
      INFMESSAGE(portrait)
      ret= XtPageOrientationPortrait;
      break;
   case O_UPSIDEDOWN:
      INFMESSAGE(upsidedown)
      ret = XtPageOrientationUpsideDown;
      break;
   case O_LANDSCAPE:
     if (swapLandscape) {INFMESSAGE(seascape)  ret = XtPageOrientationSeascape; }
     else               {INFMESSAGE(landscape) ret = XtPageOrientationLandscape;}
      break;
   case O_SEASCAPE:
     if (swapLandscape) {INFMESSAGE(landscape)ret = XtPageOrientationLandscape;}
      else              {INFMESSAGE(seascape)ret = XtPageOrientationSeascape;}
      break;
   case O_NONE:
      INFMESSAGE(unspecified)
      ret = XtPageOrientationUnspecified;
      break;
   default:
      INFMESSAGE(doc_convDocOrientToXtOrient: unknown orientation)
      ret = XtPageOrientationUnspecified;
      break;
   }
   ENDMESSAGE(doc_convDocOrientToXtOrient)

   return ret;
}

/*############################################################*/
/* doc_convStringToDocOrient */
/*############################################################*/

int
doc_convStringToDocOrient(ostr)
   String ostr;
{
   int o=O_PORTRAIT;
   BEGINMESSAGE(doc_convStringToDocOrient)
   if (ostr) {
SMESSAGE(ostr)
#     define IS_STR(aaa,bbb) (!XmuCompareISOLatin1(aaa,bbb))
      if      IS_STR(ostr,"Automatic")      { INFMESSAGE(is O_AUTOMATIC)      o = O_AUTOMATIC;  }
      else if IS_STR(ostr,"Portrait")       { INFMESSAGE(is O_PORTRAIT)       o = O_PORTRAIT;   }
      else if IS_STR(ostr,"Landscape")      { INFMESSAGE(is O_LANDSCAPE)      o = O_LANDSCAPE;  }
      else if IS_STR(ostr,"Seascape")       { INFMESSAGE(is O_SEASCAPE)       o = O_SEASCAPE;   }
      else if IS_STR(ostr,"Upside-Down")    { INFMESSAGE(is O_UPSIDEDOWN)     o = O_UPSIDEDOWN; }
      else if IS_STR(ostr,"Swap-Landscape") { INFMESSAGE(is O_SWAP_LANDSCAPE) o = O_SWAP_LANDSCAPE; }
#     undef IS_STR
   }
   ENDMESSAGE(doc_convStringToDocOrient)
   return o;
}

/*############################################################*/
/* doc_convStringToPageMedia */
/*############################################################*/

int
doc_convStringToPageMedia(d,mstr)
   Document d;
   String mstr;
{
   int media = MEDIA_ID_INVALID;
   int i;

   BEGINMESSAGE(doc_convStringToPageMedia)
   /*### automatic pagemedia evaluation */
   if (!XmuCompareISOLatin1(mstr,"Automatic")) media = MEDIA_ID_AUTO;
   /*### document pagemedia */
   if (media==MEDIA_ID_INVALID  && d && d->nummedia) {
      for (i = 0; i<d->nummedia; i++) 
          if (!XmuCompareISOLatin1(mstr,d->media[i].name)) media = i;
   }
   /*### standard pagemedia */
   if (media==MEDIA_ID_INVALID) {
      int num_doc_papersizes=0;
      if (d) num_doc_papersizes = d->nummedia;
      for (i=0; papersizes[i].name; i++)
	  if (!XmuCompareISOLatin1(mstr,papersizes[i].name)) media = num_doc_papersizes+i;
   }
   IMESSAGE(media)
   ENDMESSAGE(doc_convStringToPageMedia)
   return media;
}
