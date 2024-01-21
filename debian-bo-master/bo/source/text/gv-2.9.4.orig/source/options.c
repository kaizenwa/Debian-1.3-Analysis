/*
**
** options.c
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

#include "config.h"

#include <stdio.h>

#include "paths.h"
#include INC_X11(Intrinsic.h)
#include INC_XAW(Command.h)
#include INC_X11(StringDefs.h)
#include INC_X11(Shell.h)
#include "Aaa.h"
#include "FileSel.h"
#include "Frame.h"

#include "callbacks.h"
#include "d_memdebug.h"
#include "file.h"
#include "gv.h"
#include "popup_misc.h"
#include "widgets_misc.h"
#include "main_resources.h"
#include "main_globals.h"
#include "misc.h"
#include "note.h"
#include "options.h"

#ifdef VMS
#   define unlink remove
#else
#   include <unistd.h>
#endif

#ifndef max
#   define max(a, b)    ((a) > (b) ? (a) : (b))
#endif

/*### Application dependencies ##############################################*/

#define  OPTIONS_TOPLEVEL 	toplevel	/* the Apllication Shell   */
#define  OPTIONS_APPLIC_CONTEXT app_con		/* the Apllication Context */
#define  OPTIONS_POPUP_NAME	"optionPopup"
#define  OPTIONS_POPUP		optionpopup

#define MESSAGE_STRING_LEN 512
#define MAX_RECORD_LENGTH 512
#define MAX_OPTIONS 10

static Widget   optionControl;
static Widget   reverseScrollingToggle,confirmPrintToggle,antialiasToggle;
static Widget   print_command, scratch_dir,default_save_dir;
static Bool	optionPopupCreated = False;
static Bool	optionPopupVisible = False;

/*###############################################################################
   SaveOptionsToFile(argn,argi,argfi,argi)
   Modify the resource file according to a given array of options.
   Some care is taken to respect the appearance of the resource file.
      argn: number of options to be saved
      argi: argn strings identifying the resources to be modified
      argv: the values given to the resources argi
###############################################################################*/

#   define END_OF_RECORD "\n"

static void
SaveOptionsToFile(argn,argi,argv)
   int  argn;
   char argi[MAX_OPTIONS][MAX_RECORD_LENGTH];
   char argv[MAX_OPTIONS][MAX_RECORD_LENGTH];
{
   FILE *tempfile;
   FILE *infile;
   char line[MAX_RECORD_LENGTH];
   char *tempfilename;
   int  i;
   char *comm, *res;
   char errorMessage[MESSAGE_STRING_LEN];

   BEGINMESSAGE(SaveOptionsToFile)
   if (argn == 0) {INFMESSAGE(nothing to do) return;}
   else --argn;

   if (!gv_user_defaults_file) {
      sprintf(errorMessage,"Save aborted: \nUndefined destination file.");
      NotePopupShowMessage(errorMessage);
      INFMESSAGE(undefined destination file)
      ENDMESSAGE(SaveOptionsToFile)
      return;
   }
   INFSMESSAGE(trying to write to,gv_user_defaults_file)

   infile=fopen(gv_user_defaults_file,"r"); 
   tempfilename=file_getTmpFilename(gv_user_defaults_file,gv_user_defaults_file);
   INFSMESSAGE(using temporary file,tempfilename)
   
   if (!tempfilename || !(tempfile = fopen(tempfilename,"w"))) {
      sprintf(errorMessage,"Save aborted: \nCannot create temporary file");
      NotePopupShowMessage(errorMessage);
      INFMESSAGE(cannot create temporary file)
      ENDMESSAGE(SaveOptionsToFile)
      fclose(infile);
      GV_XtFree(tempfilename);
      return;
   }

   if (infile) {
      while (fgets(line,MAX_RECORD_LENGTH,infile)) {
         i=0;
         while (i <= argn) {
            if ((res=strstr(line,argi[i]))) {
               comm=strchr(line,'!');
               if (!comm) comm=res;
               if (comm-res >= 0) {  
                  INFSMESSAGE(found:,line)
                  res=strchr(res,':');
                  while ((*(++res) == ' ') || (*res == '\t'));     
                  *res = '\0';
                  strcat(line,argv[i]);
                  INFSMESSAGE(replaced by:,line)
                  strcat(line,END_OF_RECORD);
                  while (i<argn) {
                     strcpy(argi[i],argi[i+1]);
                     strcpy(argv[i],argv[i+1]);
                     ++i;
                  }
                  --argn;
               }
               else i=argn+1;
            }
            else ++i;
         }
         fputs(line,tempfile);
      }
      fclose(infile);
   }

   if (argn>=0) fputs(END_OF_RECORD,tempfile);
   while (argn >= 0) {
      strcat(argi[argn]," ");
      strcat(argi[argn],argv[argn]);   
      strcat(argi[argn],END_OF_RECORD);
      fputs(argi[argn],tempfile);
      INFSMESSAGE(added to resource file:,argi[argn])
      --argn;
   }
   fclose(tempfile);

   if (rename(tempfilename,gv_user_defaults_file)) {
      sprintf(errorMessage,"Save aborted: \nCannot rename temporary '%s'\n to '%s'",tempfilename,gv_user_defaults_file);
      NotePopupShowMessage(errorMessage);
      unlink(tempfilename);
      GV_XtFree(tempfilename);
      INFMESSAGE(Cannot rename temporary file)
      ENDMESSAGE(SaveOptionsToFile)
      return;
   }
   GV_XtFree(tempfilename);

   ENDMESSAGE(SaveOptionsToFile)
}

/*###############################################################################
   setOptionsAtEntry
###############################################################################*/

static void setOptionsAtEntry()
{
   BEGINMESSAGE(setOptionsAtEntry)

   widgets_setToggle(antialiasToggle, (app_res.antialias ? 1 : 0));
   widgets_setToggle(confirmPrintToggle, (app_res.confirm_print ? 1 : 0));
   widgets_setToggle(reverseScrollingToggle, (app_res.reverse_scrolling ? 1 : 0));

   SMESSAGE(gv_print_command)
   widgets_setText(print_command, gv_print_command);
   SMESSAGE(app_res.scratch_dir)
   widgets_setText(scratch_dir,  app_res.scratch_dir);
   SMESSAGE(app_res.default_save_dir)
   widgets_setText(default_save_dir,  app_res.default_save_dir);

   ENDMESSAGE(setOptionsAtEntry)
}

/*###############################################################################
   changeOptionsAtExit
###############################################################################*/

#define OPTION_LENGTH 255

static void changeOptionsAtExit()
{
   Arg args[2];
   Cardinal n;
   char    *value, *str;
   static Boolean s_print_command = False;
   static Boolean s_scratch_dir =   False;
   static Boolean s_default_save_dir = False;
   Boolean antialias;

   BEGINMESSAGE(changeOptionsAtExit)

   app_res.confirm_print = widgets_getToggle(confirmPrintToggle) ? True : False ;
   app_res.reverse_scrolling = widgets_getToggle(reverseScrollingToggle) ? True : False ;

   antialias = app_res.antialias;
   app_res.antialias = widgets_getToggle(antialiasToggle) ? True : False;
   if (antialias != app_res.antialias) {
      cb_stopInterpreter(page,NULL,NULL);
      cb_redisplay(page,NULL,NULL);
   }

   value = widgets_getText(print_command);
   str = GV_XtNewString(value);
   if   (s_print_command == True) {INFMESSAGE(freeing) GV_XtFree(gv_print_command);} 
   else  s_print_command =  True;
   gv_print_command =  str;

   value = widgets_getText(scratch_dir);
   str = GV_XtNewString(value);
   if   (s_scratch_dir == True) {INFMESSAGE(freeing) GV_XtFree(app_res.scratch_dir);} 
   else  s_scratch_dir =  True;
   app_res.scratch_dir =  str;

   value = widgets_getText(default_save_dir);
   str = GV_XtNewString(value);
   if   (s_default_save_dir == True) {INFMESSAGE(freeing) GV_XtFree(app_res.default_save_dir);} 
   else  s_default_save_dir =  True;
   app_res.default_save_dir =  str;

							n=0;
   XtSetArg(args[n], XtNtmpDir, app_res.scratch_dir);	n++;
   XtSetArg(args[n], XtNreverseScrolling,app_res.reverse_scrolling);n++;
   XtSetValues(FileSel, args, n);

   ENDMESSAGE(changeOptionsAtExit)
}

/*###############################################################################
   cb_popupOptionPopup
###############################################################################*/

void cb_popupOptionPopup(w, client_data, call_data)
   Widget	w;
   XtPointer	client_data, call_data;
{
   BEGINMESSAGE(popupOptionPopup)
   if (!optionPopupCreated) makeOptionPopup();
   if (!optionPopupVisible) {
      setOptionsAtEntry();
      popup_positionPopup(optionpopup,fileButton,POPUP_POSITION_POS,85,25);
      XtPopup(optionpopup, XtGrabNone);
      optionPopupVisible=True;
   }
   ENDMESSAGE(popupOptionPopup)
}      

/*###############################################################################
   cb_saveOptionPopup
   Note: the resources MUST be terminated by ':'
###############################################################################*/

static
void cb_saveOptionPopup(w, client_data, call_data)
   Widget	w;
   XtPointer	client_data, call_data;
{
   int 		argn = 0;
   char		argi[7][MAX_RECORD_LENGTH];
   char		argv[7][MAX_RECORD_LENGTH];

   BEGINMESSAGE(cb_saveOptionPopup)

   if (!optionPopupVisible) {
      INFMESSAGE(option popup not up)
      ENDMESSAGE(cb_saveOptionPopup)
      return;
   }

   sprintf(argi[argn],"%s.printCommand:",gv_intern_name);      
   strcpy(argv[argn],widgets_getText(print_command));       ++argn;

   sprintf(argi[argn],"%s.scratchDir:",gv_name);      
   strcpy(argv[argn],widgets_getText(scratch_dir));         ++argn;
   
   sprintf(argi[argn],"%s.defaultSaveDir:",gv_name);      
   strcpy(argv[argn],widgets_getText(default_save_dir));         ++argn;

   sprintf(argi[argn],"%s.confirmPrint:",gv_name);      
   if (widgets_getToggle(confirmPrintToggle))  strcpy(argv[argn],"True");
   else strcpy(argv[argn],"False");                   ++argn;

   sprintf(argi[argn],"%s.reverseScrolling:",gv_name);      
   if (widgets_getToggle(reverseScrollingToggle))  strcpy(argv[argn],"True");
   else strcpy(argv[argn],"False");                   ++argn;

   sprintf(argi[argn],"%s.antialias:",gv_name);      
   if (widgets_getToggle(antialiasToggle))  strcpy(argv[argn],"True");
   else strcpy(argv[argn],"False");                   ++argn;

   SaveOptionsToFile(argn,argi,argv);

   ENDMESSAGE(cb_saveOptionPopup)
}

/*###############################################################################
   cb_popdownOptionPopup
###############################################################################*/

void cb_popdownOptionPopup(w, client_data, call_data)
   Widget	w;
   XtPointer	client_data, call_data;
{
   BEGINMESSAGE(cb_popdownOptionPopup)
   if (optionPopupVisible) {
      cb_popdownNotePopup((Widget)NULL,(XtPointer)NULL,NULL);
      XtPopdown(optionpopup);
      optionPopupVisible=False;
   }
   ENDMESSAGE(cb_popdownOptionPopup)
}

/*###############################################################################
   cb_doneOptionPopup
###############################################################################*/

void cb_doneOptionPopup(w, client_data, call_data)
   Widget	w;
   XtPointer	client_data, call_data;
{
   BEGINMESSAGE(doneOptionPopup)
   if (optionPopupVisible) {
      cb_popdownOptionPopup((Widget)NULL,(XtPointer)NULL,NULL);
      changeOptionsAtExit();
   }
   ENDMESSAGE(doneOptionPopup)
}

/*###############################################################################
   makeOptionPopup
###############################################################################*/

#if 0
static void
check(w, data, event, cont)
Widget w;
XtPointer data;
XEvent *event;
Boolean *cont;
{
   BEGINMESSAGE(check)
   ENDMESSAGE(check)

}
#endif

static char *addTextTrans =
"\
<MapNotify>:display-caret(on,always)display-caret(off,always)\n\
<EnterNotify>:display-caret(on,always)\n\
<LeaveNotify>:display-caret(off,always)\
";

void makeOptionPopup()
{
   Arg          args[10];
   Cardinal     n;
   Dimension	minw,minh;
   Widget       w;
   static XtTranslations trans=(XtTranslations)NULL;

   BEGINMESSAGE(makeOptionPopup)

   if (optionPopupCreated) {INFMESSAGE(popup exists) ENDMESSAGE(makeOptionPopup) }

        						n=0;
        XtSetArg(args[n], XtNallowShellResize, True);	n++;
   OPTIONS_POPUP = XtCreatePopupShell(OPTIONS_POPUP_NAME,transientShellWidgetClass,OPTIONS_TOPLEVEL, args, n);

         						n=0;
        XtSetArg(args[n], XtNresizeWidth, True);	n++;
        XtSetArg(args[n], XtNresizeHeight, True);	n++;
        XtSetArg(args[n], XtNconditionedResize, False);	n++;
        XtSetArg(args[n], XtNmaximumWidth, 0);	 	n++;
        XtSetArg(args[n], XtNmaximumHeight,0);	 	n++;
   optionControl = XtCreateManagedWidget("optionControl",aaaWidgetClass,OPTIONS_POPUP,args,n);

#if 0
    XtAddEventHandler(optionControl, (EventMask)
KeyPressMask | KeyReleaseMask | ButtonPressMask |
			   ButtonReleaseMask | EnterWindowMask |
			   LeaveWindowMask | PointerMotionMask | 
			   Button1MotionMask |
			   Button2MotionMask | Button3MotionMask |
			   Button4MotionMask | Button5MotionMask |
			   ButtonMotionMask | KeymapStateMask

, TRUE,
                      check, (XtPointer)NULL);

#endif

   antialiasToggle        = widgets_createLabeledToggle("antialias", optionControl);

   confirmPrintToggle     = widgets_createLabeledToggle("confirmPrint", optionControl);

   reverseScrollingToggle = widgets_createLabeledToggle("scrolling", optionControl);

        						n=0;
   w = XtCreateManagedWidget("done", commandWidgetClass,optionControl, args, n);
         XtAddCallback(w, XtNcallback, cb_doneOptionPopup,NULL); 
   w = XtCreateManagedWidget("save", commandWidgetClass,optionControl, args, n);
         XtAddCallback(w, XtNcallback, cb_saveOptionPopup,NULL);
   w = XtCreateManagedWidget("cancel", commandWidgetClass,optionControl, args, n);
         XtAddCallback(w, XtNcallback, cb_popdownOptionPopup, NULL);
 
   print_command    = widgets_createLabeledTextField("printCommand", optionControl);
   scratch_dir      = widgets_createLabeledTextField("scratchDir",   optionControl);
   default_save_dir = widgets_createLabeledTextField("saveDir",      optionControl);
   if (!trans) trans=XtParseTranslationTable(addTextTrans);
   XtOverrideTranslations(print_command,trans);
   XtOverrideTranslations(scratch_dir,trans);
   XtOverrideTranslations(default_save_dir,trans);

/*
                                               n=0;
   XtSetArg(args[n], XtNtranslations, trans);  ++n;
   XtSetValues(print_command, args, n);
   XtSetValues(scratch_dir, args, n);
   XtSetValues(default_save_dir, args, n);
*/

   setOptionsAtEntry();
   XtRealizeWidget(OPTIONS_POPUP);

   AaaWidgetGetNaturalSize((AaaWidget)optionControl,&minw,&minh);
   IIMESSAGE(minw,minh)
                                          n=0;
   XtSetArg(args[n], XtNminWidth, minw);  ++n;
   XtSetArg(args[n], XtNminHeight, minh); ++n;
   XtSetArg(args[n], XtNmaxWidth, XtUnspecifiedShellInt);  ++n;
   XtSetArg(args[n], XtNmaxHeight, minh); ++n;
   XtSetValues(OPTIONS_POPUP, args, n);

                                       n=0;
   XtSetArg(args[n], XtNwidth, minw+30);  ++n;
   XtSetValues(optionControl, args, n);

   XSetWMProtocols(XtDisplay(OPTIONS_POPUP),XtWindow(OPTIONS_POPUP),&wm_delete_window,1);

   optionPopupCreated = True;  
                                
   ENDMESSAGE(makeOptionPopup)
}
