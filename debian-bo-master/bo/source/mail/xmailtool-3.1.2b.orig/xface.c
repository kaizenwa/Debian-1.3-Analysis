/*

Copyright 1990 by Cray Research, Inc.

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of Cray Research, Inc. not be used in
advertising or publicity pertaining to distribution of the software without
specific, written prior permission.  Cray Research, Inc. makes no
representations about the suitability of this software for any purpose.  It
is provided "as is" without express or implied warranty.

*/

static char xface_rcsid[]="$Id: xface.c,v 1.4 1994/05/02 21:58:11 bobo Exp $";


#include <stdio.h>
#include <stdlib.h>
#include <X11/Xos.h>
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Text.h>

#include "mailhandler.h"

extern struct HEADLIST **sortedList;
extern int cur_msg_num,LetterCount;
extern char *NoXFaceLine;
extern Display *dpy;

#if NeedFunctionPrototypes
extern int uncompface(char *);
#else
extern int uncompface();
#endif

static char face_buf[2048];
static char line[512];
static unsigned short sbuf[256];
static char cbuf[512];

static XawTextBlock stxt = {0, 8, "\nX-Face:",0},
                    rtxt;

static char revtable[256] = {
        0, -128,   64,  -64,   32,  -96,   96,  -32,
       16, -112,   80,  -48,   48,  -80,  112,  -16,
        8, -120,   72,  -56,   40,  -88,  104,  -24,
       24, -104,   88,  -40,   56,  -72,  120,   -8,
        4, -124,   68,  -60,   36,  -92,  100,  -28,
       20, -108,   84,  -44,   52,  -76,  116,  -12,
       12, -116,   76,  -52,   44,  -84,  108,  -20,
       28, -100,   92,  -36,   60,  -68,  124,   -4,
        2, -126,   66,  -62,   34,  -94,   98,  -30,
       18, -110,   82,  -46,   50,  -78,  114,  -14,
       10, -118,   74,  -54,   42,  -86,  106,  -22,
       26, -102,   90,  -38,   58,  -70,  122,   -6,
        6, -122,   70,  -58,   38,  -90,  102,  -26,
       22, -106,   86,  -42,   54,  -74,  118,  -10,
       14, -114,   78,  -50,   46,  -82,  110,  -18,
       30,  -98,   94,  -34,   62,  -66,  126,   -2,
        1, -127,   65,  -63,   33,  -95,   97,  -31,
       17, -111,   81,  -47,   49,  -79,  113,  -15,
        9, -119,   73,  -55,   41,  -87,  105,  -23,
       25, -103,   89,  -39,   57,  -71,  121,   -7,
        5, -123,   69,  -59,   37,  -91,  101,  -27,
       21, -107,   85,  -43,   53,  -75,  117,  -11,
       13, -115,   77,  -51,   45,  -83,  109,  -19,
       29,  -99,   93,  -35,   61,  -67,  125,   -3,
        3, -125,   67,  -61,   35,  -93,   99,  -29,
       19, -109,   83,  -45,   51,  -77,  115,  -13,
       11, -117,   75,  -53,   43,  -85,  107,  -21,
       27, -101,   91,  -37,   59,  -69,  123,   -5,
        7, -121,   71,  -57,   39,  -89,  103,  -25,
       23, -105,   87,  -41,   55,  -73,  119,   -9,
       15, -113,   79,  -49,   47,  -81,  111,  -17,
       31,  -97,   95,  -33,   63,  -65,  127,   -1,
} ;


#if NeedFunctionPrototypes
Pixmap get_face(struct HEADLIST *d)
#else
Pixmap get_face(d)
struct HEADLIST *d;
#endif
{
 int len,i,j,temp,doing_xface,notdone;
 char *ptr;

	if(d==(struct HEADLIST *)0 || d->XFace==NoXFaceLine)
	{
		return(None);
	}

	strcpy(face_buf,&(d->XFace[strspn(d->XFace," \t")]));

 /* step three - convert using uncompface */

 if (uncompface(face_buf) < 0) {
  return(None);
 }

 /* step four - convert ascii output of uncompface into binary */

 ptr = face_buf ;
 for (i = 0; i < 48; i++) {
#ifdef REVORDER
  for (j = 2; j >= 0; j--)
#else
  for (j = 0; j < 3; j++)
#endif /*REVORDER*/
  {
   while (*ptr == ' ' || *ptr == '\t' || *ptr == '\n') ptr++ ;
   sscanf(ptr, "0x%X", &temp) ;
   sbuf[(i<<1)+i + j] = (short) temp ;
   ptr = index(ptr, ',') ;
   ptr++ ;
  }
 }

 /* step five - convert binary output of uncompface into X11 Bitmap data */

 for (i = 0; i < 256; i++) {
  cbuf[(i<<1)+0] = revtable[(sbuf[i] >> 8) & 0xFF] ;
  cbuf[(i<<1)+1] = revtable[sbuf[i] & 0xFF] ;
 }

 /* last effort - return the Pixmap created from this data */

 return(
  XCreateBitmapFromData(dpy, DefaultRootWindow(dpy),cbuf,48,48));

}

