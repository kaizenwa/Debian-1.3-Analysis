/*
**
** widgets_misc.c
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

#include <stdio.h>

#include "paths.h"
#include INC_X11(Intrinsic.h)
#include INC_X11(StringDefs.h)
#include INC_XAW(AsciiText.h)
#include INC_XAW(Cardinals.h)
#include INC_XAW(Toggle.h)
#include "Frame.h"

#include "d_memdebug.h"
#include "gv.h"
#include "widgets_misc.h"

/*##################################################*/
/* widgets_createLabeledToggle */
/* Create a labeled toggle. */
/*##################################################*/

Widget
widgets_createLabeledToggle(name, parent)
   String name;
   Widget parent;
{
   char   tmp[50];
   Widget toggle;

   INFMESSAGE(executing widgets_createLabeledToggle)
   toggle = XtCreateManagedWidget(name,toggleWidgetClass,parent,NULL,ZERO);
   sprintf(tmp,"%sLabel",name);
   XtCreateManagedWidget(tmp, labelWidgetClass,parent,NULL,ZERO);
   return(toggle);
}

/*##################################################*/
/* widgets_getToggle */
/*##################################################*/

int
widgets_getToggle(w)
   Widget  w;
{
   Arg     args[1];
   Boolean b;

   BEGINMESSAGE(widgets_getToggle)
   XtSetArg(args[0],XtNstate, &b);
   XtGetValues(w,args,ONE);   
   ENDMESSAGE(widgets_getToggle)
   return (b ? 1 : 0);
}

/*##################################################*/
/* widgets_setToggle */
/*##################################################*/

void
widgets_setToggle(w,value)
   Widget   w;
   int      value;
{
   Arg      args[1];
   Boolean  b;

   BEGINMESSAGE(widgets_setToggle)
   b = (Boolean) (value != 0);
   XtSetArg(args[0],XtNstate,b);
   XtSetValues(w,args,ONE);
   ENDMESSAGE(widgets_setToggle)
}

/*##################################################*/
/* widgets_createLabeledOneLineText */
/* Create a labeled/single-line-text-field */
/* widget pair. */
/*##################################################*/

static char *OneLineTextTr =  "\
<Key>Return: no-op()\n\
<Key>Down: no-op()\n\
<Key>Up: no-op()\n\
<Key>Linefeed: no-op()\n\
Ctrl<Key>J: no-op()\n\
Ctrl<Key>M: no-op()\n\
Ctrl<Key>N: no-op()\n\
Ctrl<Key>O: no-op()\n\
Ctrl<Key>P: no-op()\n\
Ctrl<Key>R: no-op()\n\
Ctrl<Key>S: no-op()\n\
Ctrl<Key>V: no-op()\n\
Ctrl<Key>Z: no-op()\n\
Meta<Key>V: no-op()\n\
Meta<Key>Z: no-op()\n\
<Key>BackSpace: delete-previous-character()\n\
<Key>Right: forward-character()\n\
<Key>Left: backward-character()\
";

Widget
widgets_createLabeledTextField(name, parent)
   String   name;
   Widget   parent;
{
   Arg      args[9];
   Cardinal n;
   char     tmp[50];
   Widget   frame,text;
   static XtTranslations trans=(XtTranslations)NULL;
   
   INFMESSAGE(executing widgets_createLabeledTextField)   
          sprintf(tmp,"%sLabel",name);

          XtSetArg(args[0], XtNresize, False);
   XtCreateManagedWidget(tmp,labelWidgetClass,parent,args,ONE);

          sprintf(tmp,"%sFrame",name);
   frame = XtCreateManagedWidget(tmp,frameWidgetClass,parent,NULL,ZERO);
          								n=0;
          XtSetArg(args[n], XtNtype, XawAsciiString);			n++;
          XtSetArg(args[n], XtNuseStringInPlace, False);		n++;
          XtSetArg(args[n], XtNscrollHorizontal, XawtextScrollNever);	n++;
          XtSetArg(args[n], XtNscrollVertical, XawtextScrollNever);	n++;
          XtSetArg(args[n], XtNdisplayCaret, False);			n++;
          XtSetArg(args[n], XtNeditType, XawtextEdit);			n++;
          XtSetArg(args[n], XtNresize, XawtextResizeWidth);		n++;
   text = XtCreateManagedWidget(name,asciiTextWidgetClass,frame,args,n);
          if (!trans) trans=XtParseTranslationTable(OneLineTextTr);
          XtOverrideTranslations(text,trans);

   return text;
}

/*##################################################*/
/* widgets_getText */
/*##################################################*/

char *
widgets_getText(w)
   Widget w;
{
   Arg    args[1];
   String value;

   BEGINMESSAGE(widgets_getText)
   XtSetArg(args[0], XtNstring, &value);
   XtGetValues(w, args, ONE);
   ENDMESSAGE(widgets_getText)
   return value;
}

/*##################################################*/
/* widgets_setText */
/*##################################################*/

void
widgets_setText(w,value)
   Widget w;
   String value;
{
   Arg      args[1];

   BEGINMESSAGE(widgets_setText)
   XtSetArg(args[0], XtNstring, value);
   XtSetValues(w,args,ONE);
   ENDMESSAGE(widgets_setText)
}

/*##################################################*/
/* widgets_preferButton */
/*##################################################*/

void 
widgets_preferButton(w,prefer)
   Widget   w;
   int prefer;
{
   BEGINMESSAGE(widgets_preferButton)
   if (w) {
      XEvent event;
      Position rx,ry;
      XtTranslateCoords(w, 0, 0, &rx, &ry);
      event.xcrossing.type        = prefer ? EnterNotify : LeaveNotify;
      event.xcrossing.display     = XtDisplay(w);
      event.xcrossing.window      = XtWindow(w);
      event.xcrossing.root        = RootWindowOfScreen(XtScreen(w));
      event.xcrossing.subwindow   = None;
      event.xcrossing.time        = 0;
      event.xcrossing.x           = 0;
      event.xcrossing.y           = 0;
      event.xcrossing.x_root      = (int)rx;
      event.xcrossing.y_root      = (int)ry;
      event.xcrossing.mode        = 0;
      event.xcrossing.detail      = 0;
      event.xcrossing.same_screen = 0;
      event.xcrossing.focus       = 0;
      event.xcrossing.state       = 0;
      XSendEvent(XtDisplay(w),XtWindow(w), False,
                 prefer ? EnterWindowMask : LeaveWindowMask, &event);
   }
   ENDMESSAGE(widgets_preferButton)
}










