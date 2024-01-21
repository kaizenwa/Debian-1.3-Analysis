/*
**
** actions.c
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
 * This code is derived from:
*/

/*
 * actions.c -- X11 actions for ghostview.
 * Copyright (C) 1992  Timothy O. Theisen
 *   Author: Tim Theisen           Systems Programmer
 * Internet: tim@cs.wisc.edu       Department of Computer Sciences
 *     UUCP: uwvax!tim             University of Wisconsin-Madison
 *    Phone: (608)262-0438         1210 West Dayton Street
 *      FAX: (608)262-9777         Madison, WI   53706
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more de *    Whether to include the backing pixmap code.   
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
*/

/*
#define MESSAGES
#define MESSAGES1
*/
#include "message.h"

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "paths.h"
#include INC_X11(Intrinsic.h)
#include INC_X11(StringDefs.h)
#include INC_XAW(Cardinals.h)
#include INC_XAW(SimpleMenu.h)
#include INC_XAW(SmeBSB.h)
#include INC_XAW(SmeLine.h) 
#include INC_XAW(Scrollbar.h)
#include INC_XAW(MenuButton.h)
#include INC_X11(IntrinsicP.h)
#include "Aaa.h"
#include "Clip.h"
#include "Ghostview.h"

#ifdef VMS
#   include <unixio.h>
#endif

#include "actions.h"
#include "callbacks.h"
#include "gv.h"
#include "ps.h"
#include "doc_misc.h"
#include "dialog.h"  /* include the popup headers for action_delete_window */
#include "note.h"
#include "info.h"
#include "main_resources.h"
#include "main_globals.h"
#include "options.h"
#include "version.h"

/*############################################################*/
/* action_miscMenu */
/*############################################################*/

#define MISC_MENU_IDLE		(1<<0)
#define MISC_MENU_STATE_1	(1<<1)
#define MISC_MENU_STATE_2	(1<<2)
#define MISC_MENU_INIT		(1<<3)
#define MISC_MENU_POPUP		(1<<4)
#define MISC_MENU_RESET		(1<<5)
#define MISC_MENU_CHECK		(1<<6)

void
action_miscMenu (w, event, params, num_params)
  Widget w;
  XEvent *event;
  String *params;
  Cardinal *num_params; 
{
  static int mode = MISC_MENU_IDLE;
  static Widget gvw = (Widget)NULL;
  static Widget menuwidget = (Widget)NULL;
  static int xo=0,yo=0;
  int x,y;

  BEGINMESSAGE(action_miscMenu)

  if (!event) {
    INFMESSAGE(received reset request)
    if (menuwidget) XtDestroyWidget(menuwidget);
    menuwidget = (Widget)NULL;
    gvw = (Widget)NULL;
    mode = MISC_MENU_IDLE;
    ENDMESSAGE(action_magMenu)
    return;
  }

  if (*num_params < 1) {
    INFMESSAGE(no parameter)
    ENDMESSAGE(action_miscMenu)
    return;
  }

# define MISC_MENU_HAVE(aaa,bbb) (!strcmp(params[0],(aaa)) && mode&(bbb))
  if      MISC_MENU_HAVE("init" ,  MISC_MENU_IDLE)    mode = (MISC_MENU_INIT	| MISC_MENU_STATE_1);
  else if MISC_MENU_HAVE("popup" , MISC_MENU_STATE_1) mode = (MISC_MENU_POPUP	| MISC_MENU_STATE_2);
  else if MISC_MENU_HAVE("check" , MISC_MENU_STATE_2) mode = (MISC_MENU_CHECK	| MISC_MENU_STATE_2);
  else if MISC_MENU_HAVE("reset" , (MISC_MENU_IDLE|MISC_MENU_STATE_1|MISC_MENU_STATE_2)) 
                                                      mode = (MISC_MENU_RESET	| MISC_MENU_STATE_2);
  else {
    INFMESSAGE(no mode)
    ENDMESSAGE(action_miscMenu)
    return;
  }
#   undef MISC_MENU_HAVE

  if (mode & MISC_MENU_INIT) {
    INFMESSAGE(MISC_MENU_INIT)
    if (gvw!=page || event->type == ButtonRelease || event->type == ButtonPress) {
      xo = (int) event->xbutton.x_root;
      yo = (int) event->xbutton.y_root;
      gvw = w;
    } else {
      INFMESSAGE(event not in main window or not a button press or button release)
      mode = MISC_MENU_IDLE;
    }
    ENDMESSAGE(action_miscMenu)
    return;
  }

  if (mode & MISC_MENU_POPUP) {
    Arg args[4];
    Cardinal n;
    Widget entry;

    INFMESSAGE(MISC_MENU_POPUP)

    if (event->type != ButtonRelease && event->type != ButtonPress) {
      INFMESSAGE(event not a button press or button release)
      action_miscMenu(gvw,(XEvent*)NULL,NULL,NULL);
      ENDMESSAGE(action_miscMenu)
      return;
    }
    x = (int) event->xbutton.x_root;
    y = (int) event->xbutton.y_root;
    if (abs(x-xo)>1 || abs(y-yo)>1) {
      INFMESSAGE(succesive  events are unrelated)
      action_miscMenu(gvw,(XEvent*)NULL,NULL,NULL);
      ENDMESSAGE(action_miscMenu)
      return;
    }

  	 					n=0;
    menuwidget = XtCreatePopupShell("miscMenu", simpleMenuWidgetClass,w, args, n);
 	   					n=0;
    entry = XtCreateManagedWidget("update", smeBSBObjectClass,menuwidget, args, n);
            XtAddCallback(entry,XtNcallback,cb_checkFile,(XtPointer)CHECK_FILE_DATE);
    entry = XtCreateManagedWidget("redisplay", smeBSBObjectClass,menuwidget, args, n);
            XtAddCallback(entry,XtNcallback,cb_redisplay,(XtPointer)NULL);
	    XtSetSensitive(entry,(gv_filename != NULL));
    entry = XtCreateManagedWidget("line", smeLineObjectClass,menuwidget,args,n);
    entry = XtCreateManagedWidget("mark", smeBSBObjectClass,menuwidget, args, n);
            XtAddCallback(entry,XtNcallback,cb_setPageMark,(XtPointer)(SPM_CURRENT|SPM_MARK));
	    XtSetSensitive(entry,(toc_text != NULL));
    entry = XtCreateManagedWidget("unmark", smeBSBObjectClass,menuwidget, args, n);
            XtAddCallback(entry,XtNcallback,cb_setPageMark,(XtPointer)(SPM_CURRENT|SPM_UNMARK));
	    XtSetSensitive(entry,(toc_text != NULL));
    entry = XtCreateManagedWidget("line", smeLineObjectClass,menuwidget,args,n);
    entry = XtCreateManagedWidget("stop", smeBSBObjectClass,menuwidget, args, n);
            XtAddCallback(entry,XtNcallback,cb_stopInterpreter,(XtPointer)NULL);

    {
      int menu_x, menu_y;
      Dimension menu_width,entry_height,menu_border;
      Position button_x, button_y;

      if (!XtIsRealized(menuwidget)) XtRealizeWidget(menuwidget);

                                                        n=0;
      XtSetArg(args[n], XtNheight, &entry_height);      ++n;
      XtGetValues(entry, args, n);

                                                        n=0;
      XtSetArg(args[n], XtNwidth, &menu_width);         ++n;
      XtSetArg(args[n], XtNborderWidth, &menu_border);  ++n;
      XtGetValues(menuwidget, args, n);
  
      XtTranslateCoords(w, event->xbutton.x, event->xbutton.y, &button_x, &button_y);
      menu_x = button_x-menu_width/2 -menu_border;
      menu_y = button_y-entry_height/2;

                                                        n=0;
      XtSetArg(args[n], XtNx, menu_x);                  n++;
      XtSetArg(args[n], XtNy, menu_y);                  n++;
      XtSetValues(menuwidget, args, n);
      XtPopup(menuwidget,XtGrabExclusive);           
    }
    ENDMESSAGE(action_miscMenu)
    return;
  }

  if (mode & MISC_MENU_CHECK) {
    INFMESSAGE(MISC_MENU_CHECK)
    if (menuwidget) { 
      Arg args[5];
      Cardinal n;
      Position ulx,uly,lrx,lry,evx,evy;
      int rx,ry;
      Dimension width,height;
                                                         n=0;
      XtSetArg(args[n], XtNwidth,  &width);              n++;
      XtSetArg(args[n], XtNheight, &height);             n++;
      XtGetValues(menuwidget, args, n);
      XtTranslateCoords(menuwidget, 0, 0, &ulx, &uly);
      XtTranslateCoords(menuwidget, (Position)width, (Position)height, &lrx, &lry);
      IIMESSAGE(ulx,uly)
      IIMESSAGE(lrx,lry)
      {
	Window root, child;
	int dummyx, dummyy;
	unsigned int dummymask;
	XQueryPointer(XtDisplay(w), XtWindow(w), &root, &child, &rx, &ry,
		      &dummyx, &dummyy, &dummymask);
      }
      evx = (Position)rx;
      evy = (Position)ry;
      IIMESSAGE(evx,evy)
      if (evx<=ulx || evx >= lrx || evy <= uly || evy >= lry) {
	INFMESSAGE(pointer outside window)
        mode = mode | MISC_MENU_RESET;
      }
    }
  }

  if (mode & MISC_MENU_RESET) {
    INFMESSAGE(MISC_MENU_RESET)
    action_miscMenu(gvw,(XEvent*)NULL,NULL,NULL);
  }

  ENDMESSAGE(action_magMenu)
}

/*############################################################*/
/* action_otherPage */
/*############################################################*/

void
action_otherPage(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String *params;
  Cardinal *num_params;
{
  static int xo=0,yo=0;
  static Time to = 0;
  int x,y,vpwx,vpwy;
  Dimension vpww;
  Time t;
  Position px,py;
  Arg args[1];
  Widget vpw;
 
  BEGINMESSAGE1(action_otherPage)

  if (event->type != ButtonRelease || w != page) {
    INFMESSAGE1(event not a button release or not in main window)
    ENDMESSAGE1(action_otherPage)
    return;
  }

  vpw = XtParent(XtParent(w));
  XtTranslateCoords(vpw, 0, 0, &px, &py);
  vpwx = (int)px;
  vpwy = (int)py;
  x = ((int) event->xbutton.x_root) - vpwx + 1;
  y = ((int) event->xbutton.y_root) - vpwy + 1;
  t = ((XMotionEvent*)event)->time;
#if 0
  printf("to=%d xo=%d yo=%d\n",(int)to,xo,yo);
  printf("t= %d x= %d y= %d\n",(int)t,x,y);
#endif 
  if (t - to < 400) {
    if (xo - x < 2 && yo - y < 2) {
      XtSetArg(args[0], XtNwidth,  &vpww);
      XtGetValues(vpw, args, ONE);
      if (2*x<(int)vpww) cb_showPreviousPage(w,(XtPointer)NULL,(XtPointer)NULL);
      else               cb_showNextPage(w,(XtPointer)NULL,(XtPointer)NULL);
    }
  }
  to = t;
  xo = x;
  yo = y;
  ENDMESSAGE1(action_otherPage)
}

/*############################################################*/
/* action_movePage */
/*############################################################*/

void
action_movePage(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
#   define HISTORY_POINTS 10
#   define DECAY_TIME 200
    int x,y;
    static int posx[HISTORY_POINTS+1],posix;
    static int posy[HISTORY_POINTS+1],posiy;
    static int xp,yp,pw,ph,pvw,pvh;
    static Bool initialized=False;
    static Time to;
    Widget vpw;

    BEGINMESSAGE1(action_movePage)

    if (XtClass(w) != ghostviewWidgetClass) {
       INFMESSAGE1(not a ghostview widget) ENDMESSAGE1(action_movePage)
       return;
    }
    vpw = XtParent(XtParent(w));
 
    if (*num_params) {
       if (!strcmp(params[0],"adjusted")) { /* called by cb_pageAdjustNotify */
          XawPannerReport *rep= (XawPannerReport*) params[1];
          INFMESSAGE(adjusting)
	  xp  = (int) (-rep->slider_x);
	  yp  = (int) (-rep->slider_y);
          pw  = (int) (rep->canvas_width);
          ph  = (int) (rep->canvas_height);
          pvw = (int) (rep->slider_width);
          pvh = (int) (rep->slider_height);
          IIMESSAGE1(xp,yp) IIMESSAGE1(pw,ph) IIMESSAGE1(pvw,pvh)
          initialized = True;
       } else if (!strcmp(params[0],"move")) {
          INFMESSAGE(moving)
          if (event->type != MotionNotify) goto break_movePage;
          if (initialized==True && pvw && pvh) {
             int dx,dy;
             double relfactor=1.0;  /* some default value */
             double absfactor=0.0;  /* some default value */
             x = (int) event->xbutton.x_root;
             y = (int) event->xbutton.y_root;

             if ((((XMotionEvent*)event)->time - to) > DECAY_TIME) {
                if (posix>0) { posx[0]=posx[posix]; posix=0; }
                if (posiy>0) { posy[0]=posy[posiy]; posiy=0; }
             }
             if (posix>0 && (x-posx[posix])*(posx[posix]-posx[posix-1]) < 0) {
                               posx[0]=posx[posix]; posix=0;
             }
             if (posiy>0 && (y-posy[posiy])*(posy[posiy]-posy[posiy-1]) < 0) {
                               posy[0]=posy[posiy]; posiy=0;
             }
             to = ((XMotionEvent*)event)->time;
             ++posix;
             ++posiy;

             if (posix>HISTORY_POINTS) {
                posix=1;
                while (posix<=HISTORY_POINTS) { posx[posix-1]=posx[posix]; posix++; }
                posix=HISTORY_POINTS;
             }
             posx[posix] = x;
             if (posiy>HISTORY_POINTS) {
                posiy=1;
                while (posiy<=HISTORY_POINTS) { posy[posiy-1]=posy[posiy]; posiy++; }
                posiy=HISTORY_POINTS;
             }
             posy[posiy] = y;

             dx = (x - posx[0])/(posix);
             dy = (y - posy[0])/(posiy);
#if 0
             printf("time=%d x=%d y=%d dx=%d dy=%d\n",(int)to,x,y,dx,dy);
             printf("posix=%d posx[posix]=%d posx[0]=%d\n",posix,posx[posix],posx[0]);
             printf("posiy=%d posy[posiy]=%d posy[0]=%d\n",posiy,posy[posiy],posy[0]);
#endif
             if (dx || dy) {
                if (*num_params>=2) relfactor = atof((char*)(params[1]));
                relfactor = relfactor >= 0 ? (relfactor<=100 ? relfactor : 100) : 0;
                if (*num_params>=3) absfactor = atof((char*)(params[2]));
                absfactor = absfactor >= 0 ? (absfactor<=200 ? absfactor : 200) : 0;
                IIMESSAGE1(absfactor,relfactor)
                if (app_res.reverse_scrolling) { dx = -dx; dy = -dy; }
                xp = (int) (xp-(dx*absfactor)-(relfactor*pw*dx)/pvw);
                yp = (int) (yp-(dy*absfactor)-(relfactor*ph*dy)/pvh);
                ClipWidgetSetCoordinates(vpw,xp,yp);
             }
          }
       } else if (!strcmp(params[0],"start")) {
           Position positx,posity;
           Arg args[2];
           INFMESSAGE(start)
           if (event->type != ButtonPress) goto break_movePage;
           initialized = False;
           gv_scroll_mode = SCROLL_MODE_GHOSTVIEW;
           XtSetArg(args[0], XtNx, (Position*)&positx);
           XtSetArg(args[1], XtNy, (Position*)&posity);
           XtGetValues(XtParent(w), args, TWO);
           xp = (int)positx; yp = (int)posity;
           posix=posiy=0;
           posx[0] = (int) event->xbutton.x_root;
           posy[0] = (int) event->xbutton.y_root;
           to = ((XMotionEvent*)event)->time;
           IIMESSAGE1(xp,yp)
           ClipWidgetSetCoordinates(vpw,xp,yp);
       } else if (!strcmp(params[0],"stop")) {
           INFMESSAGE(stop)
           gv_scroll_mode = SCROLL_MODE_NONE;
           initialized = False;
       }
    }
    ENDMESSAGE1(action_movePage)
    return;

break_movePage:
    INFMESSAGE1(interrupting due to wrong event type)
    initialized = False;
    gv_scroll_mode = SCROLL_MODE_NONE;
    ENDMESSAGE1(action_movePage) return;
}

/*##################################################################*/
/* action_panner */
/*##################################################################*/

void
action_panner(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    int x,y,cw,ch;
    static int xo,yo,xp,yp;
    static Bool initialized=False;
    Widget panner  = w; 

    BEGINMESSAGE(action_panner)

    if (!strcmp(params[0],"move") && initialized) {
       INFMESSAGE1(move)
       y = yp + (int)event->xbutton.y_root - yo;
       ch = (int)panner->core.height-(int)slider->core.height;
       if (y>ch) y=ch; else if (y<0) y=0;
       x = xp + (int)event->xbutton.x_root - xo;
       cw = (int)panner->core.width-(int)slider->core.width;
       if (x>cw) x=cw; else if (x<0) x=0;
       if (x!=xp || y!=yp) {
          int pxp,pyp,dw,dh;
          XtMoveWidget(slider,x,y);
          IIMESSAGE(x,y) IIMESSAGE(xp,yp) IIMESSAGE(xo,yo)
          dw = (int)viewControl->core.width  - (int)viewClip->core.width;
          dh = (int)viewControl->core.height - (int)viewClip->core.height;
          if (cw) pxp = (x*dw+cw/2)/cw; else pxp = 0;
          if (ch) pyp = (y*dh+ch/2)/ch; else pyp = 0;
          IIMESSAGE(x,y) IIMESSAGE(xp,yp) IIMESSAGE(xo,yo) IIMESSAGE(pxp,pyp)
          ClipWidgetSetCoordinates(viewClip,-pxp,-pyp);
          xp = x; xo = (int) event->xbutton.x_root;
          yp = y; yo = (int) event->xbutton.y_root;
       }
    }
    else if (strcmp(params[0],"on") == 0) {
       INFMESSAGE(on)
       gv_scroll_mode = SCROLL_MODE_PANNER;
       initialized = True;
       xp = (int) slider->core.x; xo = (int) event->xbutton.x_root;
       yp = (int) slider->core.y; yo = (int) event->xbutton.y_root;
    }
    else if (strcmp(params[0],"off") == 0) {
       INFMESSAGE(off)
       gv_scroll_mode = SCROLL_MODE_NONE;
       initialized = False;
    }
    ENDMESSAGE(action_panner)
}

/*##################################################################*/
/* action_handleDSC */
/* Call the cb_handleDSC callback */
/*##################################################################*/

void
action_handleDSC(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String *params;
  Cardinal *num_params;
{
  BEGINMESSAGE(action_handleDSC)
    cb_handleDSC(w, (XtPointer)1, NULL);
  ENDMESSAGE(action_handleDSC)
}

/*##################################################################*/
/* action_antialias */
/* Call the cb_antialias callback */
/*##################################################################*/

void
action_antialias(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    BEGINMESSAGE(action_antialias)
    cb_antialias(w, NULL, NULL);
    ENDMESSAGE(action_antialias)
}

/*##################################################################*/
/* action_quit */
/* Call the quit callback to stop ghostview */
/*##################################################################*/

void
action_quit(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    BEGINMESSAGE(action_quit)
    cb_quitGhostview(w, NULL, NULL);
    ENDMESSAGE(action_quit)
}

/*##################################################################*/
/* action_open */
/* Popup the open file dialog box. */
/*##################################################################*/

void
action_open(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    BEGINMESSAGE(action_open)
    cb_openFile((Widget)NULL,(XtPointer)NULL, NULL);
    ENDMESSAGE(action_open)
}

/*##################################################################*/
/* action_redisplay */
/* Call the cb_redisplay callback */
/*##################################################################*/

void
action_redisplay(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    BEGINMESSAGE(action_redisplay)
    if (!XtIsSensitive(showEntry)) {INFMESSAGE(insensitive)ENDMESSAGE(action_redisplay)return;}
    cb_redisplay((Widget)NULL,NULL,NULL);
    ENDMESSAGE(action_redisplay)
}

/*##################################################################*/
/* action_reopen */
/*##################################################################*/

void
action_reopen(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    BEGINMESSAGE(action_reopen)
    if (!XtIsSensitive(reopenEntry)) {INFMESSAGE(insensitive) ENDMESSAGE(action_reopen) return; }
    cb_reopen((Widget)NULL,(XtPointer)NULL,(XtPointer)NULL);
    ENDMESSAGE(action_reopen)
}

/*##################################################################*/
/* action_save */
/* Popup the save file dialog box. */
/*##################################################################*/

void
action_save(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    BEGINMESSAGE(action_save)
    if (*num_params<1)  {
       INFMESSAGE(no parameter)
       ENDMESSAGE(action_save)
       return;
    }
    if (!strcmp(params[0],"marked")) {
       if (!XtIsSensitive(saveMarkedEntry)) {
          INFMESSAGE(save denied)
          ENDMESSAGE(action_save)return;
       }
       cb_save((Widget)NULL,(XtPointer)(PAGE_MODE_CURRENT|PAGE_MODE_MARKED),NULL);
    } else if (!strcmp(params[0],"all")) {
       if (!XtIsSensitive(saveAllEntry)) {
          INFMESSAGE(save denied)
          ENDMESSAGE(action_save)
          return;
       }
       cb_save((Widget)NULL,(XtPointer)(PAGE_MODE_ALL),NULL);
    }
    ENDMESSAGE(action_save)
}

/*##################################################################*/
/* action_print */
/* Popup the print file dialog box. */
/*##################################################################*/

void
action_print(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{

    BEGINMESSAGE(action_print)
    if (*num_params<1)  {
       INFMESSAGE(no parameter)
       ENDMESSAGE(action_print)
       return;
    }
    if (!strcmp(params[0],"marked")) {
       if (!XtIsSensitive(printMarkedEntry)) {
          INFMESSAGE(print denied)
          ENDMESSAGE(action_print)
          return;
       }
       cb_print((Widget)NULL,(XtPointer)(PAGE_MODE_CURRENT|PAGE_MODE_MARKED),NULL);
    } else if (!strcmp(params[0],"all")) {
       if (!XtIsSensitive(printAllEntry)) {
          INFMESSAGE(print denied)
          ENDMESSAGE(action_print)
          return;
       }
       cb_print((Widget)NULL,(XtPointer)PAGE_MODE_ALL,NULL);
    }
    ENDMESSAGE(action_print)
}

/*##################################################################*/
/* action_prev */
/* Call the cb_showPreviousPage callback */
/*##################################################################*/

void
action_prev(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    BEGINMESSAGE(action_prev)
    if (!XtIsSensitive(prevEntry)) {INFMESSAGE(insensitive)ENDMESSAGE(action_prev)return;}
    cb_showPreviousPage((Widget)w,(XtPointer)NULL,(XtPointer)NULL);
    ENDMESSAGE(action_prev)
}

/*##################################################################*/
/* action_showThisPage */
/* Call the cb_showThisPage callback */
/*##################################################################*/

void
action_showThisPage(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    BEGINMESSAGE(action_showThisPage)
    if (!XtIsSensitive(showEntry)) {INFMESSAGE(insensitive)ENDMESSAGE(action_showThisPage)return;}
    cb_showThisPage((Widget)NULL,NULL,NULL);
    ENDMESSAGE(action_showThisPage)
}

/*##################################################################*/
/* action_next */
/* Call the cb_showNextPage callback */
/*##################################################################*/

void
action_next(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    BEGINMESSAGE(action_next)
    if (!XtIsSensitive(nextEntry)) {INFMESSAGE(insensitive)ENDMESSAGE(action_next)return;}
    cb_showNextPage((Widget)w,(XtPointer)NULL,(XtPointer)NULL);
    ENDMESSAGE(action_next)
}

/*##################################################################*/
/* action_center */
/* Call the center_page callback */
/*##################################################################*/

void
action_center(w, event, params, num_params)
  Widget w;
  XEvent *event;
  String *params;
  Cardinal *num_params;
{
  BEGINMESSAGE(action_center)
  if (!XtIsSensitive(centerEntry)) {
    INFMESSAGE(insensitive)
    ENDMESSAGE(action_center)
    return;
  }
  cb_positionPage((Widget)w,(XtPointer)1,(XtPointer)NULL);
  ENDMESSAGE(action_center)
}

/*##################################################################*/
/* action_setPageMark */
/* Call the cb_setPageMark callback */
/*##################################################################*/

void
action_setPageMark(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    char *selection = "selection";
    char *current   = "current";
    char *even      = "even";
    char *odd       = "odd";
    char *mark      = "mark";
    char *toggle    = "toggle";
    int spm=0;

    BEGINMESSAGE(action_setPageMark)
    if (*num_params<2) { INFMESSAGE(no parameters)ENDMESSAGE(action_setPageMark)return; }

    if      (!strcmp(params[0],selection)) spm=spm|SPM_SELECTION;
    else if (!strcmp(params[0],even))      spm=spm|SPM_EVEN;
    else if (!strcmp(params[0],odd))       spm=spm|SPM_ODD;
    else if (!strcmp(params[0],current))   spm=spm|SPM_CURRENT;
    else                                   spm=spm|SPM_ALL;
    if      (!strcmp(params[1],toggle))    spm=spm|SPM_TOGGLE;
    else if (!strcmp(params[1],mark))      spm=spm|SPM_MARK;
    else                                   spm=spm|SPM_UNMARK;
   
    cb_setPageMark((Widget)NULL,(XtPointer)spm,NULL);

    ENDMESSAGE(action_setPageMark)
}

/*##################################################################*/
/* action_autoResize */
/* Call the cb_autoResize callback */
/*##################################################################*/

void
action_autoResize(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    BEGINMESSAGE(action_autoResize)
    cb_autoResize(w, NULL, NULL);
    ENDMESSAGE(action_autoResize)
}

/*##################################################################*/
/* action_set_magstep */
/* Get the magstep from the parameter string and
 * call the cb_setMagstep callback with that magstep */
/*##################################################################*/

void
action_set_magstep(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    int i;

    BEGINMESSAGE(action_set_magstep)
    if (*num_params < 1) {INFMESSAGE(no parameters)ENDMESSAGE(action_set_magstep)return;}
    if (!strcmp(params[0],"+"))      i = app_res.magstep+1;
    else if (!strcmp(params[0],"-")) i = app_res.magstep-1;
    else                             i = atoi(params[0]);
    cb_setMagstep(w, (XtPointer)i, NULL);
    ENDMESSAGE(action_set_magstep)
}

/*##################################################################*/
/* action_set_orientation */
/* Set orientation action routine.  Converts text parameter
 * to XtPageOrientation and calls cb_setOrientation callback */
/*##################################################################*/

void
action_set_orientation(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    int o;

    BEGINMESSAGE(action_set_orientation)
    if (*num_params != 1) {
       INFMESSAGE(no parameter)
       ENDMESSAGE(action_set_orientation)
       return;
    }
    o = doc_convStringToDocOrient(params[0]);
    if (o != O_NONE) cb_setOrientation(w, (XtPointer)o, NULL);
    ENDMESSAGE(action_set_orientation)
}

/*##################################################################*/
/* action_set_pagemedia */
/* Set pagemedia action routine.  Converts text parameter
 * to index into the pagemedia widgets and calls the cb_setPagemedia
 * callback. */
/*##################################################################*/

void
action_set_pagemedia(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    int m;

    BEGINMESSAGE(action_set_pagemedia)
    if (*num_params != 1) {
       INFMESSAGE(no parameter)
       ENDMESSAGE(action_set_pagemedia) 
       return;
    }

    m = doc_convStringToPageMedia(doc,params[0]);
    if (m!= MEDIA_ID_INVALID) cb_setPagemedia(w, (XtPointer)m, NULL);

    ENDMESSAGE(action_set_pagemedia)
}

/*##################################################################*/
/* action_dismissPopup */
/* dismiss a popup window */
/*##################################################################*/

#define IS_ZOOM(sss) (!strcmp(XtName(sss),"zoomPopup"))
void
action_dismissPopup(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
   Widget s;
   BEGINMESSAGE(action_dismissPopup)
   if (w) {
      INFSMESSAGE(calling widget:,XtName(w))
      if (XtClass(w) == aaaWidgetClass)	s = XtParent(w);
      else					s = w;
      if      (s==infopopup)    {INFMESSAGE(infopopup)     cb_popdownInfoPopup((Widget)NULL,NULL,NULL);    }
      else if (s==dialogpopup)  {INFMESSAGE(dialogpopup)   cb_popdownDialogPopup((Widget)NULL,NULL,NULL);  }
      else if (s==notepopup)    {INFMESSAGE(notepopup)     cb_popdownNotePopup((Widget)NULL,NULL,NULL);    }
      else if (s==optionpopup)  {INFMESSAGE(optionpopup)   cb_popdownOptionPopup((Widget)NULL,NULL,NULL);  }
      else if (s==versionpopup) {INFMESSAGE(versionpopup)  cb_popdownVersionPopup((Widget)NULL,NULL,NULL); }
      else if (s==FileSel_popup){INFMESSAGE(Filesel_popup) XtPopdown(s);				    }
      else if IS_ZOOM(s)        {INFMESSAGE(zoomPopup)     XtDestroyWidget(s);                   }
   }
   ENDMESSAGE(action_dismissPopup)
}

/*##################################################################*/
/* action_delete_window */
/* Implement WM_DELETE_WINDOW protocol */
/*##################################################################*/

void
action_deleteWindow(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
   BEGINMESSAGE(action_deleteWindow)
   if (w) {
      INFSMESSAGE(widget:,XtName(w))
      if (event->type == ClientMessage && event->xclient.data.l[0] == wm_delete_window) {
         if (w==toplevel)     { INFMESSAGE(toplevel) cb_quitGhostview((Widget)NULL,NULL,NULL); }
         else                 { action_dismissPopup(w,NULL,NULL,NULL); }
      }
   }
   ENDMESSAGE(action_deleteWindow)
}

/*##################################################################*/
/* action_scroll */
/* scroll main viewport */
/*##################################################################*/

#define SM_UP    (1<<0)
#define SM_DOWN  (1<<1)
#define SM_LEFT  (1<<2)
#define SM_RIGHT (1<<3)

void
action_scroll(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    int dir=0;
    int x,y,nx,ny,cw,ch,d;

    BEGINMESSAGE(action_scroll)
    if (*num_params != 1) {
       INFMESSAGE(invalid parameters) ENDMESSAGE(action_scroll)
       return;
    }

    if      (!strcmp(params[0],"up"))    dir= SM_UP;
    else if (!strcmp(params[0],"down"))  dir= SM_DOWN;
    else if (!strcmp(params[0],"left"))  dir= SM_LEFT;
    else if (!strcmp(params[0],"right")) dir= SM_RIGHT;

    cw = (int)viewClip->core.width;
    ch = (int)viewClip->core.height;

    nx = x = (int)viewControl->core.x;
    ny = y = (int)viewControl->core.y;

    if (dir&(SM_LEFT|SM_RIGHT)) {
       d = (int)viewControl->core.width - cw;
       if (d>0) {
           INFMESSAGE(scrolling up or down)
           if (dir&SM_LEFT) nx = x + cw/3;
           else             nx = x - cw/3;
       }
    }
    if (dir&(SM_UP|SM_DOWN)) {
       d = (int)viewControl->core.height - ch;
       if (d>0) {
           INFMESSAGE(scrolling up or down)
           if (dir&SM_UP)   ny = y + ch/3;
           else             ny = y - ch/3;
       }
    }

    if (nx != x || ny != y) ClipWidgetSetCoordinates(viewClip,nx,ny);
    ENDMESSAGE(action_scroll)
}


/*##################################################################*/
/* action_erase_locator */
/* Pop down locator window */
/*##################################################################*/

void
action_erase_locator(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    Arg args[1];

    BEGINMESSAGE1(action_erase_locator)
    if (!show_locator) {INFMESSAGE1(no locator)ENDMESSAGE1(action_erase_locator)return;}
    XtSetArg(args[0], XtNlabel, "");
    XtSetValues(locator, args, ONE);
    ENDMESSAGE1(action_erase_locator)
}

/*##################################################################*/
/* action_checkFile */
/* Check to see if file changed */
/*##################################################################*/

void
action_checkFile(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    BEGINMESSAGE(action_checkFile)
    if (*num_params != 1) {INFMESSAGE(no parameters) ENDMESSAGE(action_checkFile) return;}
    if (!strcmp(params[0],"date"))
       cb_checkFile((Widget)NULL,(XtPointer)CHECK_FILE_DATE,NULL);
    else if (!strcmp(params[0],"version"))
       cb_checkFile((Widget)NULL,(XtPointer)CHECK_FILE_VERSION,NULL);
    ENDMESSAGE(action_checkFile)
}

/*##################################################################*/
/* action_popup_menu */
/*##################################################################*/

/* ARGSUSED */
void
action_popup_menu(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{
  Widget temp;
  Widget menu;
  Arg arglist[10];
  Cardinal num_args;
  int menu_x, menu_y;
  Dimension menu_width, menu_height,menu_border,button_width, button_height,button_border;
  Position button_x, button_y;
  char *name = GV_LABEL_MENU_NAME;

  BEGINMESSAGE(action_popup_menu)
  temp = w;
  menu = NULL;
  while(temp != NULL) {
    menu = XtNameToWidget(temp,name);
    if (menu == NULL) temp = XtParent(temp);
    else break;
  }

  if (menu == NULL) {
#if 0
    char error_buf[BUFSIZ];
    (void) sprintf(error_buf, "  %s: %s %s.",gv_application_name,"Could not find menu widget named",name);
    XtAppWarning(XtWidgetToApplicationContext(w), error_buf);
#endif
    ENDMESSAGE(action_popup_menu)
    return;
  }
  if (!XtIsRealized(menu)) XtRealizeWidget(menu);

                                                              num_args=0;
  XtSetArg(arglist[num_args], XtNwidth, &menu_width);         ++num_args;
  XtSetArg(arglist[num_args], XtNheight, &menu_height);       ++num_args;
  XtSetArg(arglist[num_args], XtNborderWidth, &menu_border);  ++num_args;
  XtGetValues(menu, arglist, num_args);
                                                              num_args=0;
  XtSetArg(arglist[num_args], XtNheight, &button_height);      ++num_args;
  XtSetArg(arglist[num_args], XtNwidth, &button_width);        ++num_args;
  XtSetArg(arglist[num_args], XtNborderWidth, &button_border); ++num_args;
  XtGetValues(w, arglist, num_args);
  
  menu_width = menu_width + 2*menu_border;
  button_height = button_height + 2*button_border;
  menu_height = menu_height + 2*menu_border;

  XtTranslateCoords(w, 0, 0, &button_x, &button_y);
  menu_x = button_x;
  menu_y = button_y;

  if (menu_x >= 0) {
    int scr_width = WidthOfScreen(XtScreen(menu));
    if (menu_x + menu_width > scr_width)
      menu_x = scr_width - menu_width;
  }
  if (menu_x < 0)  menu_x = 0;

  if (menu_y >= 0) {
    int scr_height = HeightOfScreen(XtScreen(menu));
    if (menu_y + menu_height > scr_height)
      menu_y = scr_height - menu_height;
  }
  if (menu_y < 0) menu_y = 0;

  num_args = 0;
  XtSetArg(arglist[num_args], XtNx, menu_x); num_args++;
  XtSetArg(arglist[num_args], XtNy, menu_y); num_args++;
  if (menu_width+2*menu_border<button_width+2*button_border) {
     XtSetArg(arglist[num_args], XtNwidth, button_width+2*button_border-2*menu_border); num_args++;
  }
  XtSetValues(menu, arglist, num_args);

  XtPopupSpringLoaded(menu);
  ENDMESSAGE(action_popup_menu)
}

