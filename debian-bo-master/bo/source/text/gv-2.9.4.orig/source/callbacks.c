/*
**
** callbacks.c
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
 * callbacks.c -- X11 callbacks for ghostview.
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
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
*/

/*
#define MESSAGES
*/
#include "message.h"

#include "config.h"

#include <stdio.h>
#include <stdlib.h>

#ifndef BUFSIZ
#   define BUFSIZ 1024
#endif

#include "paths.h"
#include INC_X11(Intrinsic.h)
#include INC_X11(StringDefs.h)
#include INC_X11(Shell.h)
#include INC_XAW(Cardinals.h)
#include INC_XAW(AsciiText.h)
#include "Clip.h"
#include "FileSel.h"
#include "Ghostview.h"
#include INC_X11(IntrinsicP.h)

#include "actions.h"
#include "callbacks.h"
#include "d_memdebug.h"
#include "file.h"
#include "gv.h"
#include "ps.h"
#include "info.h"
#include "popup.h"
#include "popup_misc.h"
#include "process.h"
#include "dialog.h"
#include "main_resources.h"
#include "main_globals.h"
#include "misc.h"
#include "note.h"
#include "save.h"
#include "misc_private.h"

#ifdef VMS
#   define unlink remove
#else
#   include <sys/types.h>
#   include <sys/stat.h>
#   include <unistd.h>
#endif

static char* save_directory = NULL;
static char* open_directory = NULL;

/*############################################################*/
/* cb_adjustSlider */
/*############################################################*/

void
cb_adjustSlider(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    static Dimension opw=0,oph=0,opvw=0,opvh=0;
    static Position opvx=0,opvy=0;
    XawPannerReport *report = (XawPannerReport*) call_data; 
    Dimension pw  = (Dimension) (report->canvas_width);
    Dimension ph  = (Dimension) (report->canvas_height);
    Dimension pvw = (Dimension) (report->slider_width);
    Dimension pvh = (Dimension) (report->slider_height);
    Position  pvx = (Position)  (report->slider_x);
    Position  pvy = (Position)  (report->slider_y);

    BEGINMESSAGE(cb_adjustSlider)
    if (gv_scroll_mode == SCROLL_MODE_PANNER) {
       INFMESSAGE(aborting due to wrong scroll mode) ENDMESSAGE(view_cb_adjustSlider)
       return;
    }
    if (!show_panner) {INFMESSAGE(panner not used)ENDMESSAGE(cb_adjustSlider)return;}
    
    if ((pw!=opw)||(ph!=oph)||(pvw!=opvw)||(pvh!=opvh)||(pvx!=opvx)||(pvy!=opvy)) {
       Arg args[5];
       Dimension sw,sh,cw,ch,bw;
       Position  sx,sy;
       static Dimension osw=0,osh=0;
       static Position  osx=0,osy=0;

       INFMESSAGE(detected changes)
       XtSetArg(args[0], XtNwidth,&cw);
       XtSetArg(args[1], XtNheight,&ch);
       XtSetArg(args[2], XtNborderWidth,&bw);
       XtGetValues(panner, args, THREE);

       sw = (Dimension) ((cw*pvw+pw/2)/pw);
       sh = (Dimension) ((ch*pvh+ph/2)/ph);
       if (pw>pvw) sx = (Position) (((cw-sw)*pvx+(pw-pvw)/2)/(pw-pvw)); else sx = 0;
       if (ph>pvh) sy = (Position) (((ch-sh)*pvy+(ph-pvh)/2)/(ph-pvh)); else sy = 0;

       IIMESSAGE(cw,ch)
       IIMESSAGE(sw,sh) IIMESSAGE(sx,sy)
       IIMESSAGE(pw,ph) IIMESSAGE(pvw,pvh) IIMESSAGE(pvx,pvy)

       INFMESSAGE(redisplaying slider)
       XtConfigureWidget(slider,sx,sy,sw,sh,bw);
       osw=sw; osh=sh; osx=sx; osy=sy;
       opw=pw; oph=ph; opvw=pvw; opvh=pvh; opvx=pvx; opvy=pvy;
    }
    ENDMESSAGE(cb_adjustSlider)
}

/*##################################################################*/
/* cb_antialias */
/*##################################################################*/

void cb_antialias(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
{
  BEGINMESSAGE(cb_antialias)
  app_res.antialias = app_res.antialias ? False : True;
  if (gv_filename) {
    cb_stopInterpreter(page,NULL,NULL);
    cb_redisplay(page,NULL,NULL);
  }
  ENDMESSAGE(cb_antialias)
}

/*##################################################################*/
/* cb_handleDSC */
/*##################################################################*/

void cb_handleDSC(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
{
  char *s;
  Cardinal n;
  Arg args[2];

  BEGINMESSAGE(cb_handleDSC)
  if (client_data) {
    app_res.respect_dsc = app_res.respect_dsc ? False : True;
    if (gv_filename) {
      cb_stopInterpreter(page,NULL,NULL);
      cb_reopen(page,NULL,NULL);
    }
  }
  s = app_res.respect_dsc ? "Respecting file structure" : "Ignoring file structure   ";
                                  n=0;
  XtSetArg(args[n], XtNlabel, s); n++;
  XtSetValues(dscEntry,args,n);
  ENDMESSAGE(cb_handleDSC)
}

/*##################################################################*/
/* cb_stopInterpreter */
/*##################################################################*/

void cb_stopInterpreter(w, client_data, call_data)
   Widget w;
   XtPointer client_data, call_data;
{
   BEGINMESSAGE(cb_stopInterpreter)
   GhostviewDisableInterpreter(page);
   ENDMESSAGE(cb_stopInterpreter)
}

/*##################################################################*/
/* cb_pageAdjustNotify */
/*##################################################################*/

void
cb_pageAdjustNotify(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    BEGINMESSAGE(cb_pageAdjustNotify)
    if (gv_scroll_mode != SCROLL_MODE_GHOSTVIEW) {
       INFMESSAGE(aborting due to wrong scroll mode)
       ENDMESSAGE(cb_pageAdjustNotify)
       return;      
    } else {
       String params[2];
       Cardinal num_params=2;
       params[0]= "adjusted";
       params[1]= (char*) call_data;
       action_movePage(page,(XEvent*)NULL,params,&num_params);
    }
    ENDMESSAGE(cb_pageAdjustNotify)
}

/*##################################################################*/
/* cb_checkFile */
/*##################################################################*/

void
cb_checkFile(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    int changed;

    BEGINMESSAGE(cb_checkFile)
IMESSAGE((int)client_data)
    changed = check_file(((int)client_data));
    if (changed==1) show_page(current_page,NULL);
    ENDMESSAGE(cb_checkFile)
}

/*##################################################################*/
/* cb_print */
/*##################################################################*/

static char *make_pagelist(mode)
   int mode;
{
   int i=0;
   Boolean mode_valid=False;
   char *pagelist=NULL;

   BEGINMESSAGE(make_pagelist)
   if (toc_text && (mode&(PAGE_MODE_CURRENT|PAGE_MODE_MARKED))) {
      pagelist = (char*) GV_XtMalloc((doc->numpages+1)*sizeof(char));
      i=0;
      while (i < (int)doc->numpages)  { pagelist[i++]=' '; }
      pagelist[i] = '\0';
      if (mode==PAGE_MODE_CURRENT) {
         pagelist[current_page]='*';
         mode_valid=True;
      } else if (mode==PAGE_MODE_MARKED) {
         for (i=0; i < doc->numpages; i++) {
	    if (toc_text[toc_entry_length * i] == '*') {
               pagelist[i]='*';
               mode_valid=True;
            }
         }
      }
   }
   if (!mode_valid) {
      if (pagelist) GV_XtFree(pagelist);
      pagelist=NULL;
   }
   ENDMESSAGE(make_pagelist)
   return pagelist;
}

static char *get_pagelist(modep)
   int *modep;
{
   char *pagelist=NULL;
   int mode= *modep;

   BEGINMESSAGE(get_pagelist)
   if (toc_text && (mode&(PAGE_MODE_CURRENT|PAGE_MODE_MARKED))) {
      if (mode&PAGE_MODE_MARKED) {
         pagelist=make_pagelist(PAGE_MODE_MARKED);
         if (pagelist) mode=PAGE_MODE_MARKED;
      }
      if (!pagelist && (mode&PAGE_MODE_CURRENT)) {
         pagelist=make_pagelist(PAGE_MODE_CURRENT);
         if (pagelist) mode=PAGE_MODE_CURRENT;
      }
      if (!pagelist) mode=PAGE_MODE_INVALID;
   } else if (mode==PAGE_MODE_ALL) {
      pagelist=NULL; /* all pages */
   } else {
      mode=PAGE_MODE_INVALID;
   }
   *modep=mode;
   ENDMESSAGE(get_pagelist)
   return pagelist;
}

void
cb_print(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    char *prompt=GV_PRINT_MESSAGE;
    char *buttonlabel=GV_PRINT_BUTTON_LABEL;
    char *message;
    char *pagelist=NULL;

    BEGINMESSAGE(cb_print)

    if (!gv_filename) {
       INFMESSAGE(no file)
       ENDMESSAGE(cb_print)
       return;
    }

    gv_print_mode = (int)client_data;
    pagelist=get_pagelist(&gv_print_mode);
    if (pagelist) GV_XtFree(pagelist);
    if (gv_print_mode==PAGE_MODE_INVALID) {
       INFMESSAGE(invalid print mode)
       ENDMESSAGE(cb_print)
       return;
    }

    if (app_res.confirm_print) {
       if        (gv_print_mode==PAGE_MODE_MARKED) {
          message=GV_PRINT_MARKED_MESSAGE; INFMESSAGE(printing marked pages)
       } else if (gv_print_mode == PAGE_MODE_CURRENT) {
          message=GV_PRINT_PAGE_MESSAGE;   INFMESSAGE(printing current page)
       } else {
          message=GV_PRINT_ALL_MESSAGE;    INFMESSAGE(printing document)
       }
       DialogPopupSetPrompt(prompt);
       DialogPopupSetMessage(message);
       DialogPopupSetButton(DIALOG_BUTTON_DONE,buttonlabel,cb_doPrint);
       DialogPopupSetButton(DIALOG_BUTTON_CANCEL,NULL,cb_cancelPrint);
       DialogPopupSetText(gv_print_command);
       cb_popupDialogPopup((Widget)NULL,NULL,NULL);
       ENDMESSAGE(cb_print)
       return;
    }   
    cb_doPrint((Widget)NULL,NULL,(XtPointer)gv_print_command);
    ENDMESSAGE(cb_print)
}

/*##################################################################*/
/* cb_doPrint */
/*##################################################################*/

void
cb_doPrint(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    String print_command;
    String error=NULL;
    char *pagelist=NULL;

    BEGINMESSAGE(cb_doPrint)

    if (call_data) print_command = (String)(call_data);  /* dialog was not used */  
    else           print_command = DialogPopupGetText(); /* dialog was used */  
    if (!print_command) print_command="";
    SMESSAGE(print_command)

    cb_popdownNotePopup((Widget)NULL,(XtPointer)NULL,NULL);

    pagelist=get_pagelist(&gv_print_mode);
    if (gv_print_mode != PAGE_MODE_INVALID) {
       SaveData sd          = save_allocSaveData();
       sd->save_fn          = NULL;
       sd->src_fn           = gv_filename ? GV_XtNewString(gv_filename) : NULL;
       sd->conv_fn          = NULL;
       sd->pagelist         = pagelist ? GV_XtNewString(pagelist) : NULL;
       sd->print_cmd        = print_command ? GV_XtNewString(print_command) : NULL;
       sd->convert          = gv_scanfilename ? 1 : 0;
       sd->save_to_file     = (gv_print_kills_file || pagelist) ? 1 : 0;
       sd->save_to_printer  = 1;
       sd->print_kills_file = gv_print_kills_file;
       error = save_saveFile(sd);
    }
    if (error) {
       NotePopupShowMessage(error);
       GV_XtFree(error);
    } else {
       cb_popdownDialogPopup((Widget)NULL,(XtPointer)NULL,NULL);
    }
    if (pagelist) GV_XtFree(pagelist);

    ENDMESSAGE(cb_doPrint)
}

/*##################################################################*/
/* cb_cancelPrint */
/*##################################################################*/

void
cb_cancelPrint(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    BEGINMESSAGE(cb_cancelPrint)
    cb_popdownNotePopup((Widget)NULL,(XtPointer)NULL,NULL);
    cb_popdownDialogPopup((Widget)NULL,(XtPointer)NULL,NULL);
    ENDMESSAGE(cb_cancelPrint)
}

/*##################################################################*/
/* cb_save */
/*##################################################################*/

void
cb_save(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    Arg args[10];
    Cardinal n;  
    char *title="Save";
    char *buttonlabel;
    Widget button = XtNameToWidget(FileSel,"button2");
    char *pagelist;
    char *name;
    char *path;
    char ext[20];
    size_t pathlen;
    char default_path[GV_MAX_FILENAME_LENGTH];

    BEGINMESSAGE(cb_save)

    gv_save_mode = (int)client_data;
    pagelist=get_pagelist(&gv_save_mode);
    if (pagelist) GV_XtFree(pagelist);
    if (gv_save_mode==PAGE_MODE_INVALID) {
       INFMESSAGE(invalid save mode)
       ENDMESSAGE(cb_save)
       return;
    }

    path=".";
    if (!save_directory && app_res.default_save_dir) path=app_res.default_save_dir;
    else if (save_directory)                         path=save_directory;
    XawFileSelectionSetPath(FileSel,path);
    XawFileSelectionScan(FileSel,XawFileSelectionRescan);
    path = XawFileSelectionGetPath(FileSel);
    name = file_locateFilename(gv_filename);
    ext[0]='\0';

    if        (gv_save_mode==PAGE_MODE_MARKED) {
       XawFileSelectionRemoveButton(FileSel, 3);
       buttonlabel="Save Marked Pages";  INFMESSAGE(saving marked pages)
       strcpy(ext,"_pages");
    } else if (gv_save_mode==PAGE_MODE_CURRENT) {
       XawFileSelectionRemoveButton(FileSel, 3);
       buttonlabel="Save Current Page";  INFMESSAGE(saving current page)
       if (0<=current_page && current_page <= 9998) sprintf(ext,"_page_%d",(current_page+1));
       else strcpy(ext,"_page");
    } else {
       buttonlabel="Save Document";      INFMESSAGE(saving all pages)
       if (gv_scanfilename) {
          Widget button3;
          XawFileSelectionAddButton(FileSel, 3, cb_doSave, (XtPointer)FILE_TYPE_PDF);
          button3 = XtNameToWidget(FileSel,"button3");
          n=0;
          XtSetArg(args[n], XtNlabel, "Save as PDF"); ++n;
          XtSetValues(button3,args,n);
       }
    }

    pathlen = strlen(path)+strlen(name)+strlen(ext);
    if (pathlen<GV_MAX_FILENAME_LENGTH-1) {
       sprintf(default_path,"%s%s",path,name);
       file_stripVersionNumber(default_path);
       strcat(default_path,ext);
       XawFileSelectionSetPath(FileSel,default_path);
    }   

    n=0;
    XtSetArg(args[n], XtNtitle,title); ++n;
    XtSetValues(FileSel_popup, args, n);
    n=0;
    XtSetArg(args[n], XtNlabel, buttonlabel); ++n;
    XtSetValues(button,args,n);

    XtRemoveAllCallbacks(button, XtNcallback);
    XtAddCallback(button, XtNcallback,cb_doSave,NULL);

    XawFileSelectionPreferButton(FileSel,2);

    popup_positionPopup(FileSel_popup,fileButton,POPUP_POSITION_POS,85,25);
    cb_popupPopup(w, (XtPointer)FileSel_popup, call_data);
    ENDMESSAGE(cb_save)
}

/*##################################################################*/
/* cb_doSave */
/*##################################################################*/

void
cb_doSave(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    String name;
    String error=NULL;
    char *pagelist;
    int type;

    BEGINMESSAGE(cb_doSave)
    if (client_data) type = (int)client_data;
    else type = FILE_TYPE_PS;

    name = XawFileSelectionGetPath(FileSel);
    cb_popdownNotePopup((Widget)NULL,(XtPointer)NULL,NULL);
    if (save_directory) GV_XtFree(save_directory);
    save_directory= file_getDirOfPath(name);
    SMESSAGE(name)
    pagelist=get_pagelist(&gv_save_mode);
    if (gv_save_mode != PAGE_MODE_INVALID) {
       SaveData sd          = save_allocSaveData();
       sd->save_fn          = name ? GV_XtNewString(name) : NULL;
       sd->src_fn           = gv_filename ? GV_XtNewString(gv_filename) : NULL;
       sd->conv_fn          = NULL;
       sd->pagelist         = pagelist ? GV_XtNewString(pagelist) : NULL;
       sd->print_cmd        = NULL;
       sd->convert          = (gv_scanfilename && type==FILE_TYPE_PS) ? 1 : 0;
       sd->save_to_file     = 1;
       sd->save_to_printer  = 0;
       sd->print_kills_file = gv_print_kills_file;
       error = save_saveFile(sd);
    }
    if (error) {
       NotePopupShowMessage(error);
       GV_XtFree(error);
    } else {
       XtPopdown(FileSel_popup);
    }    
    if (pagelist) GV_XtFree(pagelist);

    ENDMESSAGE(cb_doSave)
}

/*##################################################################*/
/* cb_openFile */
/*##################################################################*/

void
cb_openFile(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    Arg args[1];
    Cardinal n;
    Widget button = XtNameToWidget(FileSel,"button2");

    BEGINMESSAGE(cb_openFile)

    XawFileSelectionRemoveButton(FileSel, 3);
    n=0;
    XtSetArg(args[n], XtNtitle, "Open File"); ++n;
    XtSetValues(FileSel_popup, args, n);
    n=0;
    XtSetArg(args[n], XtNlabel, "Open File"); ++n;
    XtSetValues(button, args, n);
    XtRemoveAllCallbacks(button, XtNcallback);
    XtAddCallback(button, XtNcallback,cb_doOpenFile,NULL);

    {
       char *path=".";
       if (open_directory) path=open_directory;
       XawFileSelectionSetPath(FileSel,path);
    }
    XawFileSelectionScan(FileSel,XawFileSelectionRescan);
    XawFileSelectionPreferButton(FileSel,2);
    popup_positionPopup(FileSel_popup,fileButton,POPUP_POSITION_POS,85,25);
    cb_popupPopup(w, (XtPointer)FileSel_popup, call_data);
    ENDMESSAGE(cb_openFile)
}   

/*##################################################################*/
/* cb_doOpenFile */
/*##################################################################*/

void
cb_doOpenFile(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    String name,error;

    BEGINMESSAGE(cb_doOpenFile)
    name = XawFileSelectionGetPath(FileSel);
    SMESSAGE(name)
    if (open_directory) GV_XtFree(open_directory);
    open_directory=file_getDirOfPath(name);
    SMESSAGE(open_directory)
    if ((error = misc_testFile(name))) {
       NotePopupShowMessage(error);
       GV_XtFree(error);
    } else {
       cb_popdownNotePopup((Widget)NULL,(XtPointer)NULL,NULL);
       XtPopdown(FileSel_popup);
       show_page(REQUEST_NEW_FILE,(XtPointer)name);
    }
    ENDMESSAGE(cb_doOpenFile)
}

/*##################################################################*/
/* cb_reopen */
/* Explicitly reopen the file. */
/*##################################################################*/

void
cb_reopen(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    char *error=NULL;
    BEGINMESSAGE(reopen_file)

    if ((error = misc_testFile(gv_filename))) {
       NotePopupShowMessage(error);
       GV_XtFree(error);
    } else {
       cb_popdownNotePopup((Widget)NULL,(XtPointer)NULL,NULL);
       show_page(REQUEST_REOPEN,NULL);
    }
    ENDMESSAGE(reopen_file)
}

/*##################################################################*/
/* cb_redisplay */
/*##################################################################*/

void
cb_redisplay(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    BEGINMESSAGE(cb_redisplay)
    if (w && (XtClass(w) == ghostviewWidgetClass) && (w != page)) {
       INFMESSAGE(redisplay on zoom widget not enabled)
       ENDMESSAGE(cb_redisplay)
       return;
    }    
    show_page(REQUEST_REDISPLAY,NULL);
    ENDMESSAGE(cb_redisplay)
}

/*##################################################################*/
/* cb_showPreviousPage */
/* If the new_page is different from the current page show it.  */
/* If not at the first page, show the previous page. */
/*##################################################################*/

void
cb_showPreviousPage(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    XawTextPosition pos;
    int new_page;

    BEGINMESSAGE(cb_showPreviousPage)
    if (!toc_text) {INFMESSAGE(no toc) ENDMESSAGE(cb_showPreviousPage) return;}
    if (gv_pending_page_request>NO_CURRENT_PAGE) {
       new_page=gv_pending_page_request-1;
    } else {
       pos = XawTextGetInsertionPoint(toc);
       if ((new_page = pos/toc_entry_length) == current_page) {
          new_page = current_page - 1;
       }
    }
    if (new_page >= 0) show_page(new_page,NULL);
    ENDMESSAGE(cb_showPreviousPage)
}

/*##################################################################*/
/* cb_showThisPage */
/* Show this page.  */
/*##################################################################*/

void
cb_showThisPage(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    BEGINMESSAGE(cb_showThisPage)
    if (toc_text) {
        XawTextPosition pos, end;
        int new_page;

        XawTextGetSelectionPos(toc, &pos, &end);
        IMESSAGE((int)pos) IMESSAGE((int)end)
        if (pos == end) {
           pos = XawTextGetInsertionPoint(toc);
        }
        new_page = pos/toc_entry_length;
        IMESSAGE(toc_entry_length) IMESSAGE(new_page)
        show_page(new_page,NULL);
    } else {
        cb_redisplay((Widget)NULL,NULL,NULL);
    }
    ENDMESSAGE(cb_showThisPage)
}


/*##################################################################*/
/* cb_showNextPage */
/* If the new_page is different from the current page show it.  */
/* If not at the last page, show the next page. */
/*##################################################################*/

void
cb_showNextPage(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    XawTextPosition pos;
    int new_page = 0;

    BEGINMESSAGE(cb_showNextPage)
    if (toc_text) {
       if (gv_pending_page_request>NO_CURRENT_PAGE) {
          INFMESSAGE(pending page request)
          new_page=gv_pending_page_request+1;
       } else {
          pos = XawTextGetInsertionPoint(toc);
          if ((new_page = pos/toc_entry_length) == current_page) {
             new_page = current_page + 1;
          }
       }
       if (new_page >= doc->numpages) { ENDMESSAGE(cb_showNextPage) return; }
    }
    show_page(new_page,NULL);
    ENDMESSAGE(cb_showNextPage)
}
         
/*##################################################################*/
/* cb_positionPage */
/*##################################################################*/

void
cb_positionPage(w, client_data, call_data)
  Widget w;
  XtPointer client_data, call_data;
{
  Widget clip,control,gvw;
  int clw,clh,cow,coh,px=0,py=0,x=0,y=0;
  Boolean center=False;
  Boolean scroll=False;
  Boolean have_pagepos=False;

  BEGINMESSAGE(cb_positionPage)

  if (w && (XtClass(w) == ghostviewWidgetClass)) {
    gvw     = w;
    control = XtParent(w);
    clip    = XtParent(control);
  } else {
    gvw     = page;
    control = viewControl;
    clip    = viewClip;
  }
  clw = (int)clip->core.width;
  clh = (int)clip->core.height;
  cow = (int)control->core.width;
  coh = (int)control->core.height;

  have_pagepos=misc_restorePagePosition(&px,&py);
  if ((int)client_data || (app_res.auto_center == True && !have_pagepos))
     center = True;

  if (have_pagepos) {
    GhostviewReturnStruct grs;
    Position ocx,ocy,cx,cy;

    INFMESSAGE(using saved page position)
    cx = ((Position)clip->core.width)/2  - control->core.x - page->core.x;
    cy = ((Position)clip->core.height)/2 - control->core.y - page->core.y;
    
    GhostviewGetAreaOfBB (gvw,px,py,px,py,&grs);
    ocx = (int) grs.psx;
    ocy = (int) grs.psy;
    x = (int)control->core.x - (int)(ocx - cx);
    y = (int)control->core.y - (int)(ocy - cy);
    scroll=True;
  } else if (center) {
    INFMESSAGE(centering)
    x = -(cow - clw)/2;
    y = -(coh - clh)/2;
    scroll=True;
  }
  if (scroll) {
    INFIIMESSAGE(setting position to,x,y)
    gv_scroll_mode=SCROLL_MODE_GHOSTVIEW;
    ClipWidgetSetCoordinates(clip,x,y);
    gv_scroll_mode=SCROLL_MODE_NONE;
  }
  ENDMESSAGE(cb_positionPage)
}

/*##################################################################*/
/* cb_setPageMark */
/* Set/unset the 'page marked' property */
/*##################################################################*/

void
cb_setPageMark(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    XawTextPosition begin,end;
    int r=(int)client_data;
    int i;

    BEGINMESSAGE(cb_setPageMark)
    if (!toc_text) {INFMESSAGE(no toc) ENDMESSAGE(cb_setPageMark) return; }
    if (r & SPM_SELECTION) {       INFMESSAGE(operation affects selection)
       XawTextGetSelectionPos(toc, &begin, &end);
       end--;
    } else if (r & SPM_CURRENT) {  INFMESSAGE(opration affects current)
       begin=(XawTextPosition) current_page*toc_entry_length; end=begin+(XawTextPosition)(toc_entry_length-1);
    } else {                       INFMESSAGE(operation affects all)
       begin=(XawTextPosition)0; end=(XawTextPosition)strlen(toc_text);
    }
    if (begin==end) { ENDMESSAGE(cb_setPageMarkSelection) return; }
    for (i = begin/toc_entry_length; i <= end/toc_entry_length; i++) {
        if (!(((r&SPM_EVEN) && i/2==(i+1)/2)  || ((r&SPM_ODD) && i/2!=(i+1)/2))) {
           if      (r & SPM_MARK)   toc_text[i*toc_entry_length] = '*';
           else if (r & SPM_UNMARK) toc_text[i*toc_entry_length] = ' ';
           else if (r & SPM_TOGGLE) toc_text[i*toc_entry_length] = toc_text[i*toc_entry_length]==' ' ? '*' :' ';
	   XawTextInvalidate(toc, i*toc_entry_length, i*toc_entry_length+1);
        }
    }
    ENDMESSAGE(cb_setPageMark)
}

/*##################################################################*/
/* cb_autoResize */
/*##################################################################*/

void
cb_autoResize(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    BEGINMESSAGE(cb_autoResize)
    app_res.auto_resize = !(app_res.auto_resize);
    show_page(REQUEST_TOGGLE_RESIZE,NULL);
    ENDMESSAGE(cb_autoResize)
}

/*##################################################################*/
/* cb_setMagstep */
/*##################################################################*/

void
cb_setMagstep(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    int i=(int)client_data;
    BEGINMESSAGE(cb_setMagstep)
    i = (i <= app_res.maximum_magstep ? i : app_res.maximum_magstep);
    i = (i >= app_res.minimum_magstep ? i : app_res.minimum_magstep);
    app_res.magstep = i;
    misc_savePagePosition();
    show_page(REQUEST_NEW_MAGSTEP,NULL);
    misc_resetPagePosition();
    ENDMESSAGE(cb_setMagstep)
}

/*##################################################################*/
/* cb_setOrientation */
/*##################################################################*/

void
cb_setOrientation(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    int o = (int) client_data;
    int changed = 1;

    BEGINMESSAGE(cb_setOrientation)
    switch (o) {
       case O_AUTOMATIC:
            INFMESSAGE(swapping auto orientation)
            if (gv_orientation_auto != 0) gv_orientation_auto = 0;
            else gv_orientation_auto = 1;
            break;
       case O_PORTRAIT:
       case O_SEASCAPE:
       case O_LANDSCAPE:
       case O_UPSIDEDOWN:
            gv_orientation = o;
            INFIMESSAGE(new orientation,gv_orientation)
            break;
       case O_SWAP_LANDSCAPE:
            INFMESSAGE(swapping landscape)
            if (gv_swap_landscape != 0) gv_swap_landscape = 0;
            else gv_swap_landscape = 1;
            break;
       default:
            INFMESSAGE(unknown orientation)
            changed = 0;
            break;
    }
    if (changed) {
       misc_savePagePosition();
       show_page(REQUEST_NEW_ORIENTATION,NULL);
       misc_resetPagePosition();
    }
    ENDMESSAGE(cb_setOrientation)
}

/*##################################################################*/
/* cb_setPagemedia */
/*##################################################################*/

void
cb_setPagemedia(w, client_data, call_data)
   Widget w;
   XtPointer client_data, call_data;
{
   int media = (int)client_data;

   BEGINMESSAGE(cb_setPagemedia)
   switch (media) {
   case MEDIA_ID_AUTO:
        INFMESSAGE(toggling automatic media detection)
        gv_pagemedia_auto = gv_pagemedia_auto ? 0 : 1;
	if (gv_pagemedia == MEDIA_ID_BB) 
	  gv_pagemedia = gv_pagemedia_old = MEDIA_ID_INVALID;
        break;
   default:
        INFIMESSAGE(changing to pagemedia,media)
        gv_pagemedia = media;
        break;
   }
   show_page(REQUEST_NEW_PAGEMEDIA,NULL);
   ENDMESSAGE(cb_setPagemedia)
}

/*##################################################################*/
/* cb_track */
/* track mouse pointer */
/*##################################################################*/

void
cb_track(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    GhostviewReturnStruct *p = (GhostviewReturnStruct *)call_data;

    BEGINMESSAGE1(cb_track)
    /* locator events have zero width and height */
    if ((p->width == 0)&&(p->height == 0)) {
        if (show_locator) {
           static char buf[MAX_LOCATOR_LENGTH];
           static int x,y;
           if ((x != p->psx) || (y != p->psy) || (buf[0]='\0')) {
    	      sprintf(buf, app_res.locator_format, p->psx, p->psy);
              update_label(locator,buf);
           }
           x=p->psx; y=p->psy;
        }
        ENDMESSAGE1(cb_track)
	return;
    }


   ENDMESSAGE1(cb_track)
}

/*##################################################################*/
/* cb_message */
/* Process messages from ghostscript */
/* Refresh occurs when window was resized unexpectedly */
/*##################################################################*/

void
cb_message(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    int i;
    char *error;

    BEGINMESSAGE(cb_message)
    if (!strcmp((char *) call_data, "Failed")) {
        INFMESSAGE(Failed)
	if ((Widget)client_data == page) {
            error = "\nError: PostScript interpreter failed in main window.\n\n";
	} else {
            error = "\nError: PostScript interpreter failed in zoom window.\n\n";
	}
	cb_appendInfoPopup((Widget)NULL,(XtPointer)NULL,(XtPointer)error);
    } else if (!strcmp((char *) call_data, "BadAlloc")) {
        INFMESSAGE(BadAlloc)
	if ((Widget)client_data == page) {
	    error = "\nWarning: Could not allocate backing pixmap in main window.\n\n";
	} else {
	    error = "\nWarning: Could not allocate backing pixmap in zoom window.\n\n";
	}
	cb_appendInfoPopup((Widget)NULL,(XtPointer)NULL,(XtPointer)error);
    } else if (!strcmp((char *) call_data, "Refresh")) {
        INFMESSAGE(Refresh)
	if (toc_text) {
	    GhostviewSendPS(w, psfile, doc->beginprolog,
			    doc->lenprolog, False);
	    GhostviewSendPS(w, psfile, doc->beginsetup,
			    doc->lensetup, False);
	    if (doc->pageorder == DESCEND)
		i = (doc->numpages - 1) - current_page;
	    else
		i = current_page;
	    GhostviewSendPS(w, psfile, doc->pages[i].begin,
			    doc->pages[i].len, False);
	}
    } else if (!strcmp((char *) call_data, "Page")) {
        INFMESSAGE(completed page)
	if ((gv_pending_page_request!=NO_CURRENT_PAGE) && (toc_text) && ((Widget)client_data == page)) {
           INFIMESSAGE(pending request for, gv_pending_page_request)
           show_page(gv_pending_page_request,NULL);
	}
    }
    ENDMESSAGE(cb_message)
}

/*##################################################################*/
/* cb_destroy */
/* Destroy popup window */
/*##################################################################*/

void
cb_destroy(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    BEGINMESSAGE(cb_destroy)
    XtDestroyWidget((Widget)client_data);
    ENDMESSAGE(cb_destroy)
}

/*------------------------------------------------------------------*/
/* cb_shutdown */
/*------------------------------------------------------------------*/

static void
cb_shutdown(w, client_data, call_data)
   Widget w;
   XtPointer client_data;
   XtPointer call_data;
{
   BEGINMESSAGE(cb_shutdown)

   if (psfile) fclose(psfile);
   if (gv_scanfilename) {
       unlink(gv_scanfilename);
       PS_XtFree(gv_scanfilename);
   }
   if (gv_oldfilename)        GV_XtFree(gv_oldfilename);
   if (gv_filename)           GV_XtFree(gv_filename);
   if (doc)                   psfree(doc);
   if (olddoc)                psfree(olddoc);
   if (gv_user_defaults_file) GV_XtFree(gv_user_defaults_file);
   if (gv_ad_file)            GV_XtFree(gv_ad_file);
   if (gv_style_file)         GV_XtFree(gv_style_file);
   if (magstepEntry)          GV_XtFree(magstepEntry);
   if (open_directory)        GV_XtFree(open_directory);
   if (save_directory)        GV_XtFree(save_directory);
   if (toc_text)              GV_XtFree(toc_text);
   if (pagemediaEntry)        GV_XtFree(pagemediaEntry);
   process_kill_all_processes();
   GV_MemoryDUMP
   GV_XtMemoryDUMP
   XtDestroyApplicationContext(app_con);
   ENDMESSAGE(cb_shutdown)
   ENDMESSAGE(exiting Ghostview)
   exit(EXIT_STATUS_NORMAL);
}

/*##################################################################*/
/* cb_destroyGhost */
/* destroy callback for Ghostview widgets. */
/* The disable interpreter call ensures that ghostscript is killed. */
/* Once the count goes to 0, we are sure that all forked processes have */
/* been killed and that we can safely exit. */
/*##################################################################*/

void
cb_destroyGhost(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    BEGINMESSAGE(cb_destroyGhost)
    GhostviewDisableInterpreter((Widget) client_data);
    num_ghosts--;
    if (num_ghosts) {
       ENDMESSAGE(cb_destroyGhost)
       return;
    }
    gv_exiting=1;
    if (dying) old_Xerror(XtDisplay(w), &bomb);
    /* Okay, okay, I'm a little pedantic. But I want to see the line
           MemDebug:   Stack is CLEAN !
           XtMemDebug: Stack is CLEAN !
       when activating the memory debug routines. For this we have to ensure
       that all destroy routines of all widgets are executed before leaving
       the application. So we just create a new shell hook a destroy
       callback to it and destroy it immediately. The trick is that the
       creation of the shell is delayed until the main loop next becomes idle;
       and this will be after the dust of the above destruction has settled down.
    */
    toplevel = XtAppCreateShell("shutdown",gv_class,applicationShellWidgetClass,gv_display,NULL,0);
    XtAddCallback(toplevel,XtNdestroyCallback,cb_shutdown,(XtPointer)NULL);
    XtDestroyWidget(toplevel);
}

/*##################################################################*/
/* cb_quitGhostview */
/* Start application folding up by Destroying the top level widget. */
/* The application exits when the last interpreter is killed during */
/* a destroy callback from ghostview widgets. */
/*##################################################################*/
void
cb_quitGhostview(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
   BEGINMESSAGE(cb_quitGhostview)
   XtUnmapWidget(toplevel);   
   XtDestroyWidget(toplevel);
   ENDMESSAGE(cb_quitGhostview)
}

