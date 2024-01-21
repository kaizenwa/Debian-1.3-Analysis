/*

Copyright (c) 1989  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

*/

/* 
 * XpmToggleP.h - Private definitions for XpmToggle widget
 * 
 * Adapted by Pierre Ficheux (pierre@rd.lectra.fr) from
 *         Chris D. Peterson
 *         MIT X Consortium
 *         kit@expo.lcs.mit.edu
 *  
 * Date:   January 12, 1989
 *
 */
#ident "$Id: XpmToggleP.h,v 1.1 1996/09/14 22:27:47 pierre Exp $"

#ifndef _XpmToggleP_h
#define _XpmToggleP_h

#include <X11/Xaw/SimpleP.h>

#include "XpmToggle.h"
#include "XpmButtonP.h"

/***********************************************************************
 *
 * XpmToggle Widget Private Data
 *
 ***********************************************************************/

#define streq(a, b) ( strcmp((a), (b)) == 0 )

typedef struct _RadioGroup {
  struct _RadioGroup *prev, *next; /* Pointers to other elements in group. */
  Widget widget;		  /* Widget corrosponding to this element. */
} RadioGroup;

/************************************
 *
 *  Class structure
 *
 ***********************************/

   /* New fields for the XpmToggle widget class record */
typedef struct _XpmToggleClass  {
    XtActionProc Set;
    XtActionProc Unset;
    XtPointer extension;
} XpmToggleClassPart;

   /* Full class record declaration */
typedef struct _XpmToggleClassRec {
    CoreClassPart	core_class;
    SimpleClassPart	simple_class;
    XpmButtonClassPart	xpmbutton_class;
    XpmToggleClassPart  xpmtoggle_class;
} XpmToggleClassRec;

extern XpmToggleClassRec xpmToggleClassRec;

/***************************************
 *
 *  Instance (widget) structure 
 *
 **************************************/

    /* New fields for the XpmToggle widget record */
typedef struct {
    /* resources */
    Widget      widget;
    XtPointer   radio_data;

    /* private data */
    RadioGroup * radio_group;
} XpmTogglePart;

   /* Full widget declaration */
typedef struct _XpmToggleRec {
    CorePart         core;
    SimplePart	     simple;
    XpmButtonPart    xpmbutton;
    XpmTogglePart    xpmtoggle;
} XpmToggleRec;

#endif /* _XpmTogleP_h */


