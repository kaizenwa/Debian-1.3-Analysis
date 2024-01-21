/*
**
** action_mag.c
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
#include <math.h>

#include "paths.h"
#include INC_X11(Intrinsic.h)
#include INC_X11(StringDefs.h)
#include INC_XAW(SimpleMenu.h)
#include INC_XAW(SmeBSB.h)
#include INC_XAW(SmeLine.h) 
#include INC_XAW(Cardinals.h)
#include INC_XAW(Scrollbar.h)
#include "Ghostview.h"

#include "actions.h"
#include "action_mag.h"
#include "d_memdebug.h"
#include "main_resources.h"
#include "zoom.h"

/*############################################################*/
/* action_magMenu */
/*############################################################*/

typedef struct
{
   Position locx1,locy1,locx2,locy2;
   Position oldx,oldy,oldwidth,oldheight;
   GC gc;
   Widget menuwidget;
} magMenu_data;

#define MAG_INIT_0	(1<<0)
#define MAG_INIT_1	(1<<1)
#define MAG_INIT_2	(1<<2)
#define MAG_BEGIN	(1<<3)
#define MAG_EXTEND	(1<<4)
#define MAG_CHOOSE	(1<<5)
#define MAG_SHOW	(1<<6)
#define MAG_RESET	(1<<7)
#define MAG_CHECK	(1<<8)

void
action_magMenu (w, event, params, num_params)
   Widget w;
   XEvent *event;
   String *params;
   Cardinal *num_params; 
{
   static magMenu_data *d = (magMenu_data *)NULL;
   static int mode=MAG_INIT_0;
   GhostviewReturnStruct ret_val;
   Bool popup_zoom = False;
   static Widget gvw = NULL;;

   BEGINMESSAGE(action_magMenu)

   if (!event) {
      INFMESSAGE(received reset request)
      if (d) {
         INFMESSAGE(resetting)
         if (d->menuwidget) XtDestroyWidget(d->menuwidget);
         XtReleaseGC(w,d->gc);
         GV_XtFree((char*)d);
         d = (magMenu_data *)NULL;
      }
      mode = MAG_INIT_0;
      ENDMESSAGE(action_magMenu)
      return;
   }

   if (*num_params < 1) {
      INFMESSAGE(no parameter) ENDMESSAGE(action_magMenu)
      return;
   }

#   define MAG_HAVE(aaa,bbb) (!strcmp(params[0],(aaa)) && mode&(bbb))
    if      MAG_HAVE("begin"  , MAG_INIT_0) mode = (MAG_BEGIN		| MAG_INIT_1);
    else if MAG_HAVE("extend" , MAG_INIT_1) mode = (MAG_EXTEND		| MAG_INIT_1);
    else if MAG_HAVE("choose" , MAG_INIT_1) mode = (MAG_CHOOSE		| MAG_INIT_2);
    else if MAG_HAVE("check"  , MAG_INIT_2) mode = (MAG_CHECK		| MAG_INIT_2);
    else if MAG_HAVE("show"   , MAG_INIT_2) mode = (MAG_SHOW|MAG_RESET	| MAG_INIT_2);
    else if MAG_HAVE("reset"  , (MAG_INIT_0|MAG_INIT_1|MAG_INIT_2)) 
                                            mode = (MAG_RESET		| MAG_INIT_2);
    else {
         INFMESSAGE(no mode) ENDMESSAGE(action_magMenu)
         return;
    }
#   undef MAG_HAVE

    if (mode&MAG_BEGIN) {
       XGCValues values;

       INFMESSAGE(MAG_BEGIN)
       gvw = w;
       values.foreground   = app_res.highlight_pixel;
       values.line_width   = 2;
       values.function     = GXxor;  
       d = (magMenu_data *) GV_XtMalloc(sizeof(magMenu_data));
       d->gc = XtGetGC(w,(GCFunction|GCForeground|GCLineWidth),&values); 
       d->locx1 = event->xbutton.x;
       d->locy1 = event->xbutton.y;
       d->menuwidget = NULL;
       IIMESSAGE(d->locx1,d->locy1)
       d->oldwidth=d->oldheight=0;
       ENDMESSAGE(action_magMenu)
       return;
    }

    if (mode&MAG_EXTEND) {
       Position x,y;
       Dimension width,height;
       INFMESSAGE(MAG_EXTEND)
       d->locx2=event->xbutton.x;
       d->locy2=event->xbutton.y;
       if (d->locx2 > d->locx1) { x=d->locx1; width =(Dimension)(d->locx2-d->locx1); }
       else                     { x=d->locx2; width =(Dimension)(d->locx1-d->locx2); }
       if (d->locy2 > d->locy1) { y=d->locy1; height=(Dimension)(d->locy2-d->locy1); }
       else                     { y=d->locy2; height=(Dimension)(d->locy1-d->locy2); }

       if (d->oldwidth && d->oldheight)
          XDrawRectangle(XtDisplay(w),XtWindow(w), d->gc, 
          d->oldx,d->oldy,d->oldwidth,d->oldheight);
       if (width && height)
          XDrawRectangle(XtDisplay(w),XtWindow(w), d->gc, 
          x,y,width,height);
       d->oldx=x; d->oldy=y; d->oldwidth=width; d->oldheight=height;
       ENDMESSAGE(action_magMenu)
       return;
    }

    if (mode&MAG_CHOOSE) {
       Arg args[5];
       Cardinal n;
       Widget entry = NULL;
       String name = "magMenu";
       char entrylabel[256];
       char entryname[10];
       String res;
       char *pos;
       char *tmp;
       int numparam=1;
       String list;

       INFMESSAGE(MAG_CHOOSE)
  	 							   n=0;
       d->menuwidget = XtCreatePopupShell(name, simpleMenuWidgetClass,w, args, n);

       res = GV_XtNewString(app_res.mag_menu_entries);
       list=res;
       while ((pos=strchr(list,')'))) {
          ++pos; *pos='\0';
          tmp=list;
          while (*tmp!='\"') ++tmp;
          ++tmp; 
          strcpy(entrylabel,tmp);
          tmp=strchr(entrylabel,'\"');
          *tmp='\0';
          sprintf(entryname,"%d",numparam);
 	   							   n=0;
          XtSetArg(args[n], XtNlabel,entrylabel);	   n++;
          entry = XtCreateManagedWidget(entryname, smeBSBObjectClass,d->menuwidget, args, n);

          list=pos; ++list;
          numparam++;
          if (!(*list)) break;
       }
       GV_XtFree(res);
       {
          int menu_x, menu_y;
          Dimension menu_width,entry_height,menu_border;
          Position button_x, button_y;

          if (!XtIsRealized(d->menuwidget)) XtRealizeWidget(d->menuwidget);

                                                                   n=0;
          XtSetArg(args[n], XtNheight, &entry_height);      ++n;
          XtGetValues(entry, args, n);

                                                                   n=0;
          XtSetArg(args[n], XtNwidth, &menu_width);         ++n;
          XtSetArg(args[n], XtNborderWidth, &menu_border);  ++n;
          XtGetValues(d->menuwidget, args, n);
  
          XtTranslateCoords(w, event->xbutton.x, event->xbutton.y, &button_x, &button_y);
          menu_x = button_x-menu_width/2 -menu_border;
          menu_y = button_y-entry_height/2;

                                                                   n=0;
          XtSetArg(args[n], XtNx, menu_x);                  n++;
          XtSetArg(args[n], XtNy, menu_y);                  n++;
          XtSetValues(d->menuwidget, args, n);
          XtPopup(d->menuwidget,XtGrabExclusive);           
       }

       d->locx2=event->xbutton.x;
       d->locy2=event->xbutton.y;
       IIMESSAGE(d->locx2,d->locy2)
       ENDMESSAGE(action_magMenu)
       return;
    }

    if (mode&MAG_SHOW) {
       Widget entry = XawSimpleMenuGetActiveEntry(d->menuwidget);
       INFMESSAGE(MAG_SHOW)
       if (entry) {
          String res = GV_XtNewString(app_res.mag_menu_entries);
          int entrynum = atoi(XtName(entry));
          char *param;
          char *tmp = NULL;
          int i=0;
          float xdpi,ydpi;
          int dwidth,dheight;

          param=res;
          while (i<entrynum) { tmp=strchr(param,'('); ++tmp; ++i; param=tmp;}
          param=strchr(tmp,','); ++param;
          tmp=strchr(param,')'); *tmp='\0';

          SMESSAGE(param)
          i = sscanf(param,"%f %f %d %d",&xdpi,&ydpi,&dwidth,&dheight);
          if (i==4) {
             Position locx = (d->locx1+d->locx2)/2;
             Position locy = (d->locy1+d->locy2)/2;
             if (abs(d->locx1-d->locx2)<4 || abs(d->locy1-d->locy2)<4) {
                d->locx1 = locx-dwidth;  d->locx2 = locx+dwidth;
                d->locy1 = locy-dheight; d->locy2 = locy+dheight;
             }
             GhostviewGetBBofArea(gvw,d->locx1,d->locy1,d->locx2,d->locy2,&ret_val);
             ret_val.xdpi = (xdpi * ret_val.xdpi);
             ret_val.ydpi = (ydpi * ret_val.ydpi);
             if (ret_val.width && ret_val.height) popup_zoom = True;
          }
          GV_XtFree(res);
       }
    }

   if (mode&MAG_CHECK) {
      INFMESSAGE(MAG_CHECK)
      if (d->menuwidget) { 
         Arg args[5];
         Cardinal n;
         Position ulx,uly,lrx,lry,evx,evy;
         int rx,ry;
         Dimension width,height;
                                                            n=0;
         XtSetArg(args[n], XtNwidth,  &width);              n++;
         XtSetArg(args[n], XtNheight, &height);             n++;
         XtGetValues(d->menuwidget, args, n);
         XtTranslateCoords(d->menuwidget, 0, 0, &ulx, &uly);
         XtTranslateCoords(d->menuwidget, (Position)width, (Position)height, &lrx, &lry);
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
            if (d->oldwidth && d->oldheight)
               XDrawRectangle(XtDisplay(gvw),XtWindow(gvw), d->gc, 
                        d->oldx,d->oldy,d->oldwidth,d->oldheight);
            action_magMenu(gvw,(XEvent*)NULL,NULL,NULL);
         } else {
            INFMESSAGE(pointer in window)
            ENDMESSAGE(action_magMenu)
            return;
         }
      }

   }

   if (mode&MAG_RESET) {
      INFMESSAGE(MAG_RESET)
      if (d) {
         if (d->oldwidth && d->oldheight)
            XDrawRectangle(XtDisplay(gvw),XtWindow(gvw), d->gc, 
                           d->oldx,d->oldy,d->oldwidth,d->oldheight);
      }
      action_magMenu(gvw,(XEvent*)NULL,NULL,NULL);
   }


   if (popup_zoom) {
      INFMESSAGE(popping up zoom window)
      zoom_createZoom(gvw,(XtPointer)(&ret_val));
   }
   gvw = NULL;

   ENDMESSAGE(action_magMenu)
}
