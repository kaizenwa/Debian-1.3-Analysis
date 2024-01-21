/*

Copyright (c) 1995  Randolf Werner

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from the X Consortium.

*/

/*******************************************************************
 * Module "xedit.c" contains main procedure and creates widgets    *
 *                                                                 *
 * Randolf Werner                                                  *
 * University Koblenz                                              *
 * Germany , 6.3.91                                                *
 *								   *
 * Because some stuff is taken from the "xedit" programm here      *
 * comes the copyright message of "xedit"                          *
 *******************************************************************/

/*
 *			  COPYRIGHT 1987
 *		   DIGITAL EQUIPMENT CORPORATION
 *		       MAYNARD, MASSACHUSETTS
 *			ALL RIGHTS RESERVED.
 *
 * THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE AND
 * SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL EQUIPMENT CORPORATION.
 * DIGITAL MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THIS SOFTWARE FOR
 * ANY PURPOSE.  IT IS SUPPLIED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 *
 * IF THE SOFTWARE IS MODIFIED IN A MANNER CREATING DERIVATIVE COPYRIGHT RIGHTS,
 * APPROPRIATE LEGENDS MAY BE PLACED ON THE DERIVATIVE WORK IN ADDITION TO THAT
 * SET FORTH ABOVE.
 *
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Digital Equipment Corporation not be 
 * used in advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 */

static char abouttext[] = "\n\n\n\
                                                                 EDITOR (Version 1.0)\n\
\n\
                                               The interactive Texteditor for X-Windows\n\
\n\
        Authors:   Randolf Werner, University Koblenz      (Main programming)\n\
                           Karl-Heinz Staudt, University Koblenz  (Basic design of the fileselector)\n\
                           Chris D. Peterson, MIT X Consortium   (Some stuff taken from his \"xedit\")\n\
                           César Crusius   (Drag'n'drop & accents stuff)\n\
\n";
#include "xedit.h"
#include <sys/param.h>
#include <unistd.h>
#include <stdlib.h>
#include "icon.h"

/* Stop Bitmap */
#define stop_width 31
#define stop_height 31
static char stop_bits[] = {
   0x00, 0xfe, 0x3f, 0x80, 0x00, 0x01, 0x40, 0x80, 0x80, 0xfc, 0x9f, 0x80,
   0x40, 0xfe, 0x3f, 0x81, 0x20, 0xff, 0x7f, 0x82, 0x90, 0xff, 0xff, 0x84,
   0xc8, 0xff, 0xff, 0x89, 0xe4, 0xff, 0xff, 0x93, 0xf2, 0xff, 0xff, 0xa7,
   0xf9, 0xff, 0xff, 0xcf, 0xf9, 0xff, 0xff, 0xcf, 0xf9, 0xff, 0xff, 0xcf,
   0x19, 0x83, 0x31, 0xcc, 0xe9, 0xee, 0xae, 0xcb, 0xe9, 0xef, 0xae, 0xcb,
   0x19, 0xef, 0x2e, 0xcc, 0xf9, 0xee, 0xae, 0xcf, 0xe9, 0xee, 0xae, 0xcf,
   0x19, 0xef, 0xb1, 0xcf, 0xf9, 0xff, 0xff, 0xcf, 0xf9, 0xff, 0xff, 0xcf,
   0xf9, 0xff, 0xff, 0xcf, 0xf2, 0xff, 0xff, 0xa7, 0xe4, 0xff, 0xff, 0x93,
   0xc8, 0xff, 0xff, 0x89, 0x90, 0xff, 0xff, 0x84, 0x20, 0xff, 0x7f, 0x82,
   0x40, 0xfe, 0x3f, 0x81, 0x80, 0xfc, 0x9f, 0x80, 0x00, 0x01, 0x40, 0x80,
   0x00, 0xfe, 0x3f, 0x80};

#define saved_width 16
#define saved_height 16
static unsigned char saved_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x00, 0x10, 0x00, 0x10, 0x00, 0x08,
   0x00, 0x08, 0x00, 0x04, 0x08, 0x02, 0x10, 0x02, 0x20, 0x01, 0x40, 0x01,
   0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

#define notsaved_width 16
#define notsaved_height 16
static unsigned char notsaved_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
   
/* Global widgets */
Widget textwindow, messwidget, labelwindow, filenamewindow;
Widget file_s;
Widget line_popup,line_text,error_popup,error_label;
Widget warn_popup,warn_label,warn_box;
Widget option_popup,tabsize_text;
Widget wrap_group,indent_group,autofill_group;
Widget sed_popup,sed_text;
Widget help_popup,help_text;
Widget replace_text,search_text,search_popup;
Widget start_group,direction_group;
Widget veto_popup,about_popup;
Widget command_popup,command_text;
Widget pipe_popup,pipe_text;
Widget top;
char   **wrapmode,**indentmode,**autofillmode; 
Display *CurDpy;
XtTranslations inserttrans,overwritetrans;
Widget modelabel,dirtylabel;
static XtInputId pipe_id;
Pixmap savedbitmap,notsavedbitmap;

struct _app_resources app_resources;

#define offset(field) XtOffset(struct _app_resources*, field)
static XtResource resources[] = {
   {"enableBackups", "EnableBackups", XtRBoolean, sizeof(Boolean),
         offset(enableBackups), XtRImmediate, FALSE},
   {"backupNameSuffix", "BackupNameSuffix", XtRString, sizeof(char *),
         offset(backupNameSuffix),XtRString, ".BAK"},
   {"printCommand", "PrintCommand",XtRString, sizeof(char *),
         offset(printCommand),XtRString, "lpr -p -T %t %f"},
   {"autoIndent", "AutoIndent", XtRBoolean, sizeof(Boolean),
         offset(autoIndent), XtRImmediate, FALSE},
   {"tabsize", "Tabsize",XtRInt, sizeof(int),
         offset(tabsize),XtRImmediate, (caddr_t)(int)8},
   {"maxScrollbreak", "MaxScrollbreak",XtRInt, sizeof(int),
         offset(maxScrollbreak),XtRImmediate, (caddr_t)(int)3},
   {"commands", "Commands",XtRString, sizeof(char *),
   	offset(commands), XtRString, NULL},
   {"pipes", "Pipes",XtRString, sizeof(char *),
   	offset(pipes), XtRString, NULL},
   {"textwidth", "Textwidth",XtRInt, sizeof(int),
   	offset(textwidth), XtRImmediate, (caddr_t)(int)80},
   {"textheight", "Textheight",XtRInt, sizeof(int),
   	offset(textheight), XtRImmediate, (caddr_t)(int)40},
   {"overwritetranslations", "Overwritetranslations",XtRString, sizeof(char *),
   	offset(overwritetranslations), XtRString, NULL},
   {"inserttranslations", "Inserttranslations",XtRString, sizeof(char *),
   	offset(inserttranslations), XtRString, NULL}  	 		   	
};
#undef offset

typedef struct mycallback
    {
    Widget *shellw;
    XtCallbackProc call;
    XtPointer data;
    } MYCALLBACK;

MYCALLBACK wm_delete_list[] = {
    {&top, DoFileQuit, NULL},
    {&line_popup, jumpline_close, NULL},
    {&search_popup, search_ready, NULL},
    {&veto_popup, veto_ready, (caddr_t) CANCEL},
    {&option_popup, option_ready, (caddr_t) CANCEL},
    {&sed_popup, sed_close, NULL},
    {&help_popup, help_close, NULL},
    {&about_popup, about_close, NULL},
    {&error_popup, error_ready, NULL},
    {&warn_popup, warn_ready, (caddr_t) ABORT},
    {&command_popup, command_close, NULL},
    {&pipe_popup, pipe_close, NULL},
    {NULL, NULL, NULL} };

static String fallback_resources[] = {
#ifdef MUTEKEYS
#include "Editor.mutekeys.ad.h"
#else
#include "Editor.nomute.ad.h"
#endif
NULL
};
    

/********************************************************************************
 *   Action procedure for calling the Callbacks of an menu entry		*
 ********************************************************************************/
void ActionCallMenu(widget,event,params,n_params)
Widget widget;
XEvent *event;
String *params;
Cardinal *n_params;
{
Widget menuwidget;

if(*n_params != 1)
    {
    return;
    }
menuwidget = XtNameToWidget(top,params[0]);
if(menuwidget!=NULL)
    {
    XtCallCallbacks(menuwidget,XtNcallback,NULL);
    }
}


static XtActionsRec action_goto_line[]={{"goto_line",goto_line},};
static XtActionsRec action_autoindent[]={{"autoindent",autoindent},
					 {"xedskiplineend",xedskiplineend},};
static XtActionsRec menue_actions[]={{"xedCallMenu",ActionCallMenu},};
#ifdef MUTEKEYS
static XtActionsRec mute_keys[]={{ "isoAccent", isoAccent },};
#endif

#ifdef SCROLLBREAK
static XtActionsRec action_cursorup[]={{"cursor_up",cursor_up},};
static XtActionsRec action_cursordown[]={{"cursor_down",cursor_down},};
#endif


/********************************************************************************
 *   Set the WM_DELTE_WINDOW property of a widgets window                	*
 ********************************************************************************/
void set_wm_delete(w)
Widget w;
{
Atom delete_atom;

if(XtWindow(w) != 0)
    {
    delete_atom = XInternAtom(XtDisplay(w),"WM_DELETE_WINDOW",False);
    XSetWMProtocols(XtDisplay(w),XtWindow(w),&delete_atom,1);
    }
}


/********************************************************************************
 *   Event handler for WM_DELTE_WINDOW message			              	*
 ********************************************************************************/
void wm_delete_event(w,data,event,contin)
Widget w;
XtPointer data;
XEvent *event;
Boolean *contin;
{
Atom delete_atom;
MYCALLBACK *callback;

delete_atom = XInternAtom(XtDisplay(w),"WM_DELETE_WINDOW",False);
if(event->type == ClientMessage)
    if(event->xclient.format == 32)
        if(event->xclient.data.l[0] == delete_atom)
            {
            for(callback = &wm_delete_list[0];callback->shellw != NULL; callback++)
                if(w == *callback->shellw)
                    {
                    callback->call(w,callback->data,NULL);
                    break;
                    }
            }

}

/********************************************************************************
 *   Read input from stdin until EOF						*
 ********************************************************************************/
static void pipe_input(client_data,source,id)
XtPointer client_data;
int *source;
XtInputId *id;
{
XawTextBlock block;
XawTextPosition position;
char buf[1024];
int n;

block.firstPos = 0;
block.ptr = buf;
block.format = FMT8BIT;

n=read(0,buf,1024);
block.length=n;
XawTextSetInsertionPoint(textwindow,9999999);
position = XawTextGetInsertionPoint(textwindow);
XawTextReplace(textwindow,position,position,&block);
ResetSourceChanged(textwindow);
if(n==0)
    {
    XtRemoveInput(pipe_id);
    Feep();
    }    
}


/********************************************************************************
 *  Creating all popups								*
 ********************************************************************************/
static void creat_popups(top)
Widget top;
{
Widget widget,widget2,box,box2;
Arg args[10];
XtTranslations trans;
Pixmap bitmap;
char tabstring[256];
XFontStruct *font;
Boolean fillmode;

  /* Popup for Goto Line */
  line_popup=XtCreatePopupShell("Goto line",transientShellWidgetClass,top,args,0);
  widget=XtCreateManagedWidget("line_box",boxWidgetClass,line_popup,NULL,0);
  XtSetArg(args[0],XtNlabel,"Linenumber ?");
  XtSetArg(args[1],XtNborderWidth,0);
  XtCreateManagedWidget("line_label",labelWidgetClass,widget,args,2);
  XtSetArg(args[0],XtNeditType,XawtextEdit);
  line_text=XtCreateManagedWidget("line_text",asciiTextWidgetClass,widget,args,1);
  XtAppAddActions(XtWidgetToApplicationContext(line_text),action_goto_line,1);
  trans=XtParseTranslationTable(":<Key>Return:goto_line()");
  XtOverrideTranslations(line_text,trans);

  /* Popup for error messages */
  error_popup=XtCreatePopupShell("Error",transientShellWidgetClass,top,args,0);
  widget=XtCreateManagedWidget("error_box",boxWidgetClass,error_popup,NULL,0);
  bitmap = XCreateBitmapFromData(CurDpy,RootWindow(CurDpy,DefaultScreen(CurDpy)),
                                 stop_bits,stop_width,stop_height);
  XtSetArg(args[0],XtNbitmap,bitmap);
  XtSetArg(args[1],XtNborderWidth,0);
  XtCreateManagedWidget("stop_icon",labelWidgetClass,widget,args,2);
  XtSetArg(args[0],XtNlabel,"Fehlermeldung                !");
  XtSetArg(args[1],XtNborderWidth,0);
  error_label=XtCreateManagedWidget("error_label",labelWidgetClass,widget,args,2);
  XtSetArg(args[0],XtNlabel,"  OK  ");
  widget=XtCreateManagedWidget("error_ok",commandWidgetClass,widget,args,1);
  XtAddCallback(widget,XtNcallback,error_ready,0);

  /* Popup for warning messages */
  warn_popup=XtCreatePopupShell("Warning",transientShellWidgetClass,top,args,0);
  XtSetArg(args[0],XtNorientation,XtorientVertical);
  warn_box = widget=XtCreateManagedWidget("warn_box",boxWidgetClass,warn_popup,args,1);
  XtSetArg(args[0],XtNbitmap,bitmap);
  XtSetArg(args[1],XtNborderWidth,0);
  XtCreateManagedWidget("stop_icon2",labelWidgetClass,widget,args,2);
  XtSetArg(args[0],XtNlabel,"Warnung                                !");
  XtSetArg(args[1],XtNborderWidth,0);
  warn_label=XtCreateManagedWidget("warn_label",labelWidgetClass,widget,args,2);
  XtSetArg(args[0],XtNborderWidth,0);
  XtSetArg(args[1],XtNorientation,XtorientHorizontal);
  XtSetArg(args[2],XtNshowGrip,False); 
  XtSetArg(args[3],XtNskipAdjust,True);   
  widget = XtCreateManagedWidget("warn_box2",boxWidgetClass,warn_box,args,4);
  XtSetArg(args[0],XtNlabel,"       ABORT       ");
  widget2=XtCreateManagedWidget("warn_abort",commandWidgetClass,widget,args,1);
  XtAddCallback(widget2,XtNcallback,warn_ready,ABORT);
  XtSetArg(args[0],XtNlabel,"     CONTINUE     ");
  widget2=XtCreateManagedWidget("warn_continue",commandWidgetClass,widget,args,1);
  XtAddCallback(widget2,XtNcallback,warn_ready,(caddr_t)CONTINUE);

  /* Popup for options */
  wrapmode = (char**) malloc(3*sizeof(char*));
  wrapmode[0]=  "Never";
  wrapmode[1]=  "Line";
  wrapmode[2]=  "Word";
  indentmode = (char**) malloc(2*sizeof(char*));
  indentmode[0]= "yes";
  indentmode[1]= "false";
  autofillmode = (char**) malloc(2*sizeof(char*));
  autofillmode[0]= "yes";
  autofillmode[1]= "false";  
  option_popup=XtCreatePopupShell("option_popup",transientShellWidgetClass,top,args,0);
    box = XtCreateManagedWidget("option_box",boxWidgetClass,option_popup,NULL,0);
      XtSetArg(args[0],XtNborderWidth,0);
      XtSetArg(args[1],XtNorientation,XtorientHorizontal);
      box2 = XtCreateManagedWidget("option_box2",boxWidgetClass,box,args,2); 
        XtSetArg(args[0],XtNlabel,"Wrapmode:   ");
        XtSetArg(args[1],XtNborderWidth,0);
        XtCreateManagedWidget("warp_label",labelWidgetClass,box2,args,2);
        XtSetArg(args[0],XtNlabel,"Never");
	XtSetArg(args[1],XtNradioData,(caddr_t)wrapmode[0]);
        wrap_group = widget = XtCreateManagedWidget("wrap_never",toggleWidgetClass,box2,args,2);
        XtSetArg(args[0],XtNlabel,"Line ");
        XtSetArg(args[1],XtNradioGroup,widget);
	XtSetArg(args[2],XtNradioData,(caddr_t)wrapmode[1]);
        XtCreateManagedWidget("wrap_line",toggleWidgetClass,box2,args,3);
        XtSetArg(args[0],XtNlabel,"Word ");  
        XtSetArg(args[1],XtNradioGroup,widget);
	XtSetArg(args[2],XtNradioData,(caddr_t)wrapmode[2]);
        XtCreateManagedWidget("wrap_word",toggleWidgetClass,box2,args,3);
      XtSetArg(args[0],XtNborderWidth,0);
      XtSetArg(args[1],XtNorientation,XtorientHorizontal);
      box2 = XtCreateManagedWidget("option_box4",boxWidgetClass,box,args,2); 
        XtSetArg(args[0],XtNlabel,"Tabsize:    ");
        XtSetArg(args[1],XtNborderWidth,0);
 	XtCreateManagedWidget("tab_label",labelWidgetClass,box2,args,2);
        XtSetArg(args[0],XtNeditType,XawtextEdit);
        if(app_resources.tabsize > 0)
            sprintf(tabstring,"%d",app_resources.tabsize);
        else
            sprintf(tabstring,"8");
	XtSetArg(args[1],XtNstring,tabstring);
        tabsize_text=XtCreateManagedWidget("tab_text",asciiTextWidgetClass,box2,args,2);
      XtSetArg(args[0],XtNborderWidth,0);
      XtSetArg(args[1],XtNorientation,XtorientHorizontal);
      box2 = XtCreateManagedWidget("option_box4",boxWidgetClass,box,args,2); 
        XtSetArg(args[0],XtNlabel,"Autoindent: ");
        XtSetArg(args[1],XtNborderWidth,0);
        XtCreateManagedWidget("indent_label",labelWidgetClass,box2,args,2);
        XtSetArg(args[0],XtNlabel,"Yes  ");
	XtSetArg(args[1],XtNradioData,(caddr_t)indentmode[0]);
        indent_group = XtCreateManagedWidget("indent_yes",toggleWidgetClass,box2,args,2);
        XtSetArg(args[0],XtNlabel,"No   ");
        XtSetArg(args[1],XtNradioGroup,indent_group);
	XtSetArg(args[2],XtNradioData,(caddr_t)indentmode[1]);
        widget = XtCreateManagedWidget("indent_no",toggleWidgetClass,box2,args,3);
	XtSetArg(args[0],XtNstate,True);
        if(app_resources.autoIndent)
            XtSetValues(indent_group,args,1);
        else
            XtSetValues(widget,args,1);
            
      XtSetArg(args[0],XtNborderWidth,0);
      XtSetArg(args[1],XtNorientation,XtorientHorizontal);
      box2 = XtCreateManagedWidget("option_box6",boxWidgetClass,box,args,2); 
        XtSetArg(args[0],XtNlabel,"Autofill:   ");
        XtSetArg(args[1],XtNborderWidth,0);
        XtCreateManagedWidget("autofill_label",labelWidgetClass,box2,args,2);
        XtSetArg(args[0],XtNlabel,"Yes  ");
	XtSetArg(args[1],XtNradioData,(caddr_t)autofillmode[0]);
        autofill_group = XtCreateManagedWidget("autofill_yes",toggleWidgetClass,box2,args,2);
        XtSetArg(args[0],XtNlabel,"No   ");
        XtSetArg(args[1],XtNradioGroup,autofill_group);
	XtSetArg(args[2],XtNradioData,(caddr_t)autofillmode[1]);
        widget = XtCreateManagedWidget("autofill_no",toggleWidgetClass,box2,args,3);
        XtSetArg(args[0],XtNautoFill,&fillmode);
        XtGetValues(textwindow,args,1);
	XtSetArg(args[0],XtNstate,True);
        if(fillmode)
            XtSetValues(autofill_group,args,1);
        else
            XtSetValues(widget,args,1);
            
      XtSetArg(args[0],XtNborderWidth,0);
      XtSetArg(args[1],XtNorientation,XtorientHorizontal);
      box2 = XtCreateManagedWidget("option_box5",boxWidgetClass,box,args,2);
        XtSetArg(args[0],XtNlabel,"  OK  ");
        widget2=XtCreateManagedWidget("option_ok",commandWidgetClass,box2,args,1);
        XtAddCallback(widget2,XtNcallback,option_ready,OK);
        XtSetArg(args[0],XtNlabel,"CANCEL");
        widget2=XtCreateManagedWidget("option_cancel",commandWidgetClass,box2,args,1);
        XtAddCallback(widget2,XtNcallback,option_ready,(caddr_t)CANCEL);

  /* Popup for sed commands */
  sed_popup=XtCreatePopupShell("sed",wmShellWidgetClass,top,args,0);
  box = XtCreateManagedWidget("sed_paned",panedWidgetClass,sed_popup,args,0);
      XtSetArg(args[0],XtNlabel,"Perform a stream editor command (sed)");
      XtSetArg(args[1],XtNborderWidth,0);
      XtSetArg(args[2],XtNshowGrip,False);
      XtCreateManagedWidget("sed_label",labelWidgetClass,box,args,3);
      
      XtSetArg(args[0],XtNborderWidth,0);
      XtSetArg(args[1],XtNorientation,XtorientHorizontal);
      XtSetArg(args[2],XtNshowGrip,False);
      XtSetArg(args[3],XtNinternalBorderWidth,0);
      box2 = XtCreateManagedWidget("option_box4",panedWidgetClass,box,args,4); 
 
	  XtSetArg(args[0],XtNborderWidth,0);
	  XtSetArg(args[1],XtNlabel,"Command: ");
	  XtSetArg(args[2],XtNshowGrip,False);
	  XtCreateManagedWidget("sed_label2",labelWidgetClass,box2,args,3);
	  
          XtSetArg(args[0],XtNeditType,XawtextEdit);
          XtSetArg(args[2],XtNshowGrip,False);
          sed_text=XtCreateManagedWidget("sed_text",asciiTextWidgetClass,box2,args,2);
      
      XtSetArg(args[0],XtNborderWidth,0);
      XtSetArg(args[1],XtNorientation,XtorientHorizontal);
      XtSetArg(args[2],XtNshowGrip,False); 
      XtSetArg(args[3],XtNskipAdjust,True);   
      box2 = XtCreateManagedWidget("sed_box3",boxWidgetClass,box,args,4);
      
          XtSetArg(args[0],XtNlabel," Do it ");
          widget = XtCreateManagedWidget("sed_doit",commandWidgetClass,box2,args,1);
          XtAddCallback(widget,XtNcallback,sed_do,NULL);

          XtSetArg(args[0],XtNlabel,"Do it Selecton");
          widget = XtCreateManagedWidget("sed_doit_sel",commandWidgetClass,box2,args,1);
          XtAddCallback(widget,XtNcallback,sed_do_sel,NULL);
          
          XtSetArg(args[0],XtNlabel,"Undo it");
          widget = XtCreateManagedWidget("sed_undoit",commandWidgetClass,box2,args,1);
          XtAddCallback(widget,XtNcallback,sed_undo,NULL);
          
          XtSetArg(args[0],XtNlabel," Close ");
          widget = XtCreateManagedWidget("sed_close",commandWidgetClass,box2,args,1);
	  XtAddCallback(widget,XtNcallback,sed_close,NULL);
	  
  /* Popup for command */
  command_popup=XtCreatePopupShell("command",wmShellWidgetClass,top,args,0);
  box = XtCreateManagedWidget("command_paned",panedWidgetClass,command_popup,args,0);
      XtSetArg(args[0],XtNlabel,"          Perform a User command          ");
      XtSetArg(args[1],XtNborderWidth,0);
      XtSetArg(args[2],XtNshowGrip,False);
      XtCreateManagedWidget("command_label",labelWidgetClass,box,args,3);
      
      XtSetArg(args[0],XtNborderWidth,0);
      XtSetArg(args[1],XtNorientation,XtorientHorizontal);
      XtSetArg(args[2],XtNshowGrip,False);
      XtSetArg(args[3],XtNinternalBorderWidth,0);
      box2 = XtCreateManagedWidget("option_box4",panedWidgetClass,box,args,4); 
 
	  XtSetArg(args[0],XtNborderWidth,0);
	  XtSetArg(args[1],XtNlabel,"Command: ");
	  XtSetArg(args[2],XtNshowGrip,False);
	  XtCreateManagedWidget("command_label2",labelWidgetClass,box2,args,3);
	  
          XtSetArg(args[0],XtNeditType,XawtextEdit);
          XtSetArg(args[2],XtNshowGrip,False);
          command_text=XtCreateManagedWidget("command_text",asciiTextWidgetClass,box2,args,2);
      
      XtSetArg(args[0],XtNborderWidth,0);
      XtSetArg(args[1],XtNorientation,XtorientHorizontal);
      XtSetArg(args[2],XtNshowGrip,False); 
      XtSetArg(args[3],XtNskipAdjust,True);   
      box2 = XtCreateManagedWidget("command_box3",boxWidgetClass,box,args,4);
      
          XtSetArg(args[0],XtNlabel," Do it ");
          widget = XtCreateManagedWidget("command_doit",commandWidgetClass,box2,args,1);
          XtAddCallback(widget,XtNcallback,command_do,NULL);
          
          XtSetArg(args[0],XtNlabel," Close ");
          widget = XtCreateManagedWidget("command_close",commandWidgetClass,box2,args,1);
	  XtAddCallback(widget,XtNcallback,command_close,NULL);

  /* Popup for command */
  pipe_popup=XtCreatePopupShell("pipe",wmShellWidgetClass,top,args,0);
  box = XtCreateManagedWidget("pipe_paned",panedWidgetClass,pipe_popup,args,0);
      XtSetArg(args[0],XtNlabel,"           Perform a User pipe            ");
      XtSetArg(args[1],XtNborderWidth,0);
      XtSetArg(args[2],XtNshowGrip,False);
      XtCreateManagedWidget("pipe_label",labelWidgetClass,box,args,3);
      
      XtSetArg(args[0],XtNborderWidth,0);
      XtSetArg(args[1],XtNorientation,XtorientHorizontal);
      XtSetArg(args[2],XtNshowGrip,False);
      XtSetArg(args[3],XtNinternalBorderWidth,0);
      box2 = XtCreateManagedWidget("option_box4",panedWidgetClass,box,args,4); 
 
	  XtSetArg(args[0],XtNborderWidth,0);
	  XtSetArg(args[1],XtNlabel,"Command: ");
	  XtSetArg(args[2],XtNshowGrip,False);
	  XtCreateManagedWidget("pipe_label2",labelWidgetClass,box2,args,3);
	  
          XtSetArg(args[0],XtNeditType,XawtextEdit);
          XtSetArg(args[2],XtNshowGrip,False);
          pipe_text=XtCreateManagedWidget("pipe_text",asciiTextWidgetClass,box2,args,2);
      
      XtSetArg(args[0],XtNborderWidth,0);
      XtSetArg(args[1],XtNorientation,XtorientHorizontal);
      XtSetArg(args[2],XtNshowGrip,False); 
      XtSetArg(args[3],XtNskipAdjust,True);   
      box2 = XtCreateManagedWidget("pipe_box3",boxWidgetClass,box,args,4);
      
          XtSetArg(args[0],XtNlabel," Do it ");
          widget = XtCreateManagedWidget("pipe_doit",commandWidgetClass,box2,args,1);
          XtAddCallback(widget,XtNcallback,pipe_do,NULL);
          
          XtSetArg(args[0],XtNlabel," Close ");
          widget = XtCreateManagedWidget("pipe_close",commandWidgetClass,box2,args,1);
	  XtAddCallback(widget,XtNcallback,pipe_close,NULL);
	  	  
  /* Popup for help */
  help_popup=XtCreatePopupShell("help",wmShellWidgetClass,top,args,0);
  box = XtCreateManagedWidget("help_paned",panedWidgetClass,help_popup,args,0);
      	    	
      XtSetArg(args[0],XtNtype,XawAsciiFile);
      XtSetArg(args[1],XtNstring,HELPFILE);
      XtSetArg(args[2],XtNscrollVertical,XawtextScrollAlways);
      help_text = XtCreateManagedWidget("help_text",asciiTextWidgetClass,box,args,3);

      XtSetArg(args[0],XtNlabel," Close ");
      XtSetArg(args[1],XtNshowGrip,False);
      XtSetArg(args[2],XtNskipAdjust,True);
      widget = XtCreateManagedWidget("help_close",commandWidgetClass,box,args,3);
      XtAddCallback(widget,XtNcallback,help_close,NULL);

  /* Popup for about */
  about_popup=XtCreatePopupShell("About Xed",transientShellWidgetClass,top,args,0);
  box = XtCreateManagedWidget("about_paned",panedWidgetClass,about_popup,args,0);
      	    	
      XtSetArg(args[0],XtNtype,XawAsciiString);
      XtSetArg(args[1],XtNstring,abouttext);
      XtSetArg(args[2],XtNwidth,710);
      XtSetArg(args[3],XtNheight,270);
      font = XLoadQueryFont(CurDpy,"-adobe-times-medium-i-normal--20-140-100-100-p-94-iso8859-1");
      if(font == NULL)
          font = XLoadQueryFont(CurDpy,"-adobe-times-medium-i-normal--18-180-75-75-p-94-iso8859-1");
      if(font == NULL)
          XtCreateManagedWidget("about_text",asciiTextWidgetClass,box,args,4);
      else
          {
          XtSetArg(args[4],XtNfont, font);
          XtCreateManagedWidget("about_text",asciiTextWidgetClass,box,args,5);
          }
          
      XtSetArg(args[0],XtNlabel," Close ");
      XtSetArg(args[1],XtNshowGrip,False);
      XtSetArg(args[2],XtNskipAdjust,True);
      widget = XtCreateManagedWidget("about_close",commandWidgetClass,box,args,3);
      XtAddCallback(widget,XtNcallback,about_close,NULL); 
       
  /* Popup for Search and Replace */
  search_popup=XtCreatePopupShell("Search",wmShellWidgetClass,top,args,0);
  XtSetArg(args[0],XtNinternalBorderWidth,0);
  box = XtCreateManagedWidget("search_paned",panedWidgetClass,search_popup,args,1);

      XtSetArg(args[0],XtNborderWidth,0);
      XtSetArg(args[1],XtNorientation,XtorientHorizontal);
      XtSetArg(args[2],XtNshowGrip,False); 
      XtSetArg(args[3],XtNskipAdjust,True);   
      box2 = XtCreateManagedWidget("search_box1",boxWidgetClass,box,args,4);
          
          XtSetArg(args[0],XtNlabel,"Start at    :");
          XtSetArg(args[1],XtNborderWidth,0);
          XtCreateManagedWidget("start_at",labelWidgetClass,box2,args,2);
          
          XtSetArg(args[0],XtNlabel,"Cursor position");
          XtSetArg(args[1],XtNstate,True);
          start_group = widget = XtCreateManagedWidget("Cursor position",toggleWidgetClass,box2,args,2);
          
          XtSetArg(args[0],XtNlabel,"Textbegining");
          XtSetArg(args[1],XtNradioGroup,widget);
          XtCreateManagedWidget("Textbegining",toggleWidgetClass,box2,args,2);
          
          XtSetArg(args[0],XtNlabel,"Textending");
          XtSetArg(args[1],XtNradioGroup,widget);
          XtCreateManagedWidget("Textending",toggleWidgetClass,box2,args,2);
          
      XtSetArg(args[0],XtNborderWidth,0);
      XtSetArg(args[1],XtNorientation,XtorientHorizontal);
      XtSetArg(args[2],XtNshowGrip,False); 
      XtSetArg(args[3],XtNskipAdjust,True);   
      box2 = XtCreateManagedWidget("search_box2",boxWidgetClass,box,args,4);
          
          XtSetArg(args[0],XtNlabel,"Direction   :");
          XtSetArg(args[1],XtNborderWidth,0);
          XtCreateManagedWidget("direction",labelWidgetClass,box2,args,2); 

          XtSetArg(args[0],XtNlabel,"Forward");
          XtSetArg(args[1],XtNstate,True);
          direction_group = widget = XtCreateManagedWidget("Forward",toggleWidgetClass,box2,args,2);
          
          XtSetArg(args[0],XtNlabel,"Backward");
          XtSetArg(args[1],XtNradioGroup,widget);
          XtCreateManagedWidget("Backward",toggleWidgetClass,box2,args,2);

      XtSetArg(args[0],XtNborderWidth,0);
      XtSetArg(args[1],XtNorientation,XtorientHorizontal);
      XtSetArg(args[2],XtNshowGrip,False);
      XtSetArg(args[3],XtNinternalBorderWidth,0);
      XtSetArg(args[4],XtNskipAdjust,True);
      box2 = XtCreateManagedWidget("search_box3",panedWidgetClass,box,args,5); 
 
	  XtSetArg(args[0],XtNborderWidth,1);
	  XtSetArg(args[1],XtNlabel,"Search for  : ");
	  XtSetArg(args[2],XtNshowGrip,False);
	  XtSetArg(args[3],XtNinternalWidth,8);
	  XtSetArg(args[4],XtNinternalHeight,8);
	  XtCreateManagedWidget("search_label",labelWidgetClass,box2,args,5);
	  
          XtSetArg(args[0],XtNeditType,XawtextEdit);
          XtSetArg(args[1],XtNshowGrip,False);
          XtSetArg(args[2],XtNtopMargin,8);
          XtSetArg(args[3],XtNscrollVertical,XawtextScrollWhenNeeded);
          search_text=XtCreateManagedWidget("search_text",asciiTextWidgetClass,box2,args,5);
	  
      XtSetArg(args[0],XtNborderWidth,0);
      XtSetArg(args[1],XtNorientation,XtorientHorizontal);
      XtSetArg(args[2],XtNshowGrip,False);
      XtSetArg(args[3],XtNinternalBorderWidth,0);
      XtSetArg(args[4],XtNskipAdjust,True);
      box2 = XtCreateManagedWidget("search_box4",panedWidgetClass,box,args,5); 
 
	  XtSetArg(args[0],XtNborderWidth,0);
	  XtSetArg(args[1],XtNlabel,"Replace with: ");
	  XtSetArg(args[2],XtNshowGrip,False);
	  XtSetArg(args[3],XtNinternalWidth,8);
	  XtSetArg(args[4],XtNinternalHeight,8);
	  XtCreateManagedWidget("replace_label",labelWidgetClass,box2,args,5);
	  
          XtSetArg(args[0],XtNeditType,XawtextEdit);
          XtSetArg(args[1],XtNshowGrip,False);
          XtSetArg(args[2],XtNtopMargin,8);
          XtSetArg(args[3],XtNscrollVertical,XawtextScrollWhenNeeded);
         
          replace_text=XtCreateManagedWidget("replace_text",asciiTextWidgetClass,box2,args,4);

      XtSetArg(args[0],XtNborderWidth,0);
      XtSetArg(args[1],XtNorientation,XtorientHorizontal);
      XtSetArg(args[2],XtNshowGrip,False); 
      XtSetArg(args[3],XtNskipAdjust,True);   
      box2 = XtCreateManagedWidget("search_box5",boxWidgetClass,box,args,4);
          
          XtSetArg(args[0],XtNlabel,"   Search   ");
          widget=XtCreateManagedWidget("search",commandWidgetClass,box2,args,1); 
          XtAddCallback(widget,XtNcallback,search,NULL);
          
          XtSetArg(args[0],XtNlabel,"  Replace   ");
          widget=XtCreateManagedWidget("replace",commandWidgetClass,box2,args,1); 
          XtAddCallback(widget,XtNcallback,replace,REPLACE);
          
          XtSetArg(args[0],XtNlabel,"Replace veto");
          widget=XtCreateManagedWidget("replace_veto",commandWidgetClass,box2,args,1); 
          XtAddCallback(widget,XtNcallback,replace,(caddr_t)REPLACE_VETO);
          
          XtSetArg(args[0],XtNlabel,"Replace all ");
          widget=XtCreateManagedWidget("replace_all",commandWidgetClass,box2,args,1); 
          XtAddCallback(widget,XtNcallback,replace,(caddr_t)REPLACE_ALL);

          XtSetArg(args[0],XtNlabel,"   Cancel   ");
          widget = XtCreateManagedWidget("dirrection",commandWidgetClass,box2,args,1);
          XtAddCallback(widget,XtNcallback,search_ready,NULL); 

  /* Popup for Replace veto */
  veto_popup=XtCreatePopupShell("Veto",transientShellWidgetClass,top,args,0);
      box = XtCreateManagedWidget("veto_box",boxWidgetClass,veto_popup,NULL,0);
          XtSetArg(args[0],XtNlabel,"Replace ?");
          XtSetArg(args[1],XtNborderWidth,0);
          XtCreateManagedWidget("veto_label",labelWidgetClass,box,args,2);

          XtSetArg(args[0],XtNborderWidth,0);
          XtSetArg(args[1],XtNorientation,XtorientHorizontal);
          box2 = XtCreateManagedWidget("veto_box2",boxWidgetClass,box,args,2);
  
              XtSetArg(args[0],XtNlabel," Yes  ");
              widget=XtCreateManagedWidget("veto_yes",commandWidgetClass,box2,args,1);
              XtAddCallback(widget,XtNcallback,veto_ready,(caddr_t)YES);

              XtSetArg(args[0],XtNlabel,"  No  ");
              widget=XtCreateManagedWidget("veto_no",commandWidgetClass,box2,args,1);
              XtAddCallback(widget,XtNcallback,veto_ready,(caddr_t)NO);

              XtSetArg(args[0],XtNlabel,"Cancel");
              widget=XtCreateManagedWidget("veto_cancel",commandWidgetClass,box2,args,1);
              XtAddCallback(widget,XtNcallback,veto_ready,(caddr_t)CANCEL);
}


/********************************************************************************
 *    Creating menus, statusline and textwindow				        *
 ********************************************************************************/
static void makeButtonsAndBoxes(parent, filename)
Widget parent;
char * filename;
{
  Widget outer,b_row;
  Arg arglist[10];
  Cardinal num_args;
  char *pos,*pos2,*cname;
  Widget widget,menu;
  XtTranslations trans;
  XFontStruct *font;

  outer = XtCreateManagedWidget( "paned", panedWidgetClass, parent,
				NULL, ZERO);
 
  b_row= XtCreateManagedWidget("buttons", panedWidgetClass, outer, NULL, 0);
  {

    /* File - Menue */
    menu=XtCreatePopupShell("filemenu",simpleMenuWidgetClass,parent,NULL,0);

    widget=XtCreateManagedWidget("load",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoFileLoad,NULL);

    widget=XtCreateManagedWidget("insert",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoFileInsert,NULL);

    XtCreateManagedWidget("line",smeLineObjectClass,menu,NULL,0);

    widget=XtCreateManagedWidget("save",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoFileSave,NULL);

    widget=XtCreateManagedWidget("saveas",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoFileSaveAs,NULL);

    widget=XtCreateManagedWidget("savesel",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoFileSaveSelection,NULL);
	
    XtCreateManagedWidget("line",smeLineObjectClass,menu,NULL,0);

    widget=XtCreateManagedWidget("print",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoFilePrint,NULL);

    widget=XtCreateManagedWidget("printsel",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoFilePrintSelection,NULL);

    XtCreateManagedWidget("line",smeLineObjectClass,menu,NULL,0);

    widget=XtCreateManagedWidget("quit",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoFileQuit,NULL);
		
    XtSetArg(arglist[0],XtNlabel,"File");
    XtSetArg(arglist[1],XtNmenuName,"filemenu");
    XtCreateManagedWidget("file_menu_button",menuButtonWidgetClass,b_row,arglist,2);


    /* Edit Menue */
    menu=XtCreatePopupShell("editmenu",simpleMenuWidgetClass,parent,NULL,0);

    widget=XtCreateManagedWidget("cut",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoEditCut,NULL);

    widget=XtCreateManagedWidget("paste",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoEditPaste,NULL);

    widget=XtCreateManagedWidget("right",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoEditShiftSelRight,NULL);

    widget=XtCreateManagedWidget("left",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoEditShiftSelLeft,NULL);

    XtSetArg(arglist[0],XtNlabel,"Edit");
    XtSetArg(arglist[1],XtNmenuName,"editmenu");
    XtCreateManagedWidget("edit_menu_button",menuButtonWidgetClass,b_row,arglist,2);

    /* Jump - Menue */
    menu=XtCreatePopupShell("jumpmenu",simpleMenuWidgetClass,parent,NULL,0);

    widget=XtCreateManagedWidget("line",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoJumpLine,NULL);

    widget=XtCreateManagedWidget("begin",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoJumpBegin,0);

    widget=XtCreateManagedWidget("end",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoJumpEnd,0);

    widget=XtCreateManagedWidget("selstart",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoJumpSelStart,NULL);

    widget=XtCreateManagedWidget("selend",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoJumpSelEnd,NULL);

    XtSetArg(arglist[0],XtNlabel,"Jump");
    XtSetArg(arglist[1],XtNmenuName,"jumpmenu");
    XtCreateManagedWidget("jump_menu_button",menuButtonWidgetClass,b_row,arglist,2); 


    /* Search - Menue */
    menu=XtCreatePopupShell("searchmenu",simpleMenuWidgetClass,parent,NULL,0);
    widget=XtCreateManagedWidget("search",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoSearchSearch,NULL);

    widget=XtCreateManagedWidget("search_selection",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoSearchSearchselection,NULL);

    XtCreateManagedWidget("line",smeLineObjectClass,menu,NULL,0);

    widget=XtCreateManagedWidget("replace",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoSearchReplace,NULL);

    widget=XtCreateManagedWidget("replace_selection",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoSearchReplaceselection,NULL);

    XtCreateManagedWidget("line",smeLineObjectClass,menu,NULL,0);

    widget=XtCreateManagedWidget("find_bracket",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoSearchFindbracket,NULL);

    widget=XtCreateManagedWidget("check_brackets",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoSearchCheckbrackets,NULL);

    XtSetArg(arglist[0],XtNlabel,"Search");
    XtSetArg(arglist[1],XtNmenuName,"searchmenu");
    XtCreateManagedWidget("search_menu_button",menuButtonWidgetClass,b_row,arglist,2);

    /* Spezial - Menue */
    menu=XtCreatePopupShell("spezialmenu",simpleMenuWidgetClass,parent,NULL,0);
    widget=XtCreateManagedWidget("options",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoSpezialOptions,NULL);

    widget=XtCreateManagedWidget("call_sed",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoSpezialCallSed,NULL);

    widget=XtCreateManagedWidget("help",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoSpezialHelp,NULL);

    widget=XtCreateManagedWidget("about",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoSpezialAbout,NULL);

    XtSetArg(arglist[0],XtNlabel,"Special");
    XtSetArg(arglist[1],XtNmenuName,"spezialmenu");
    XtCreateManagedWidget("spezial_menu_button",menuButtonWidgetClass,b_row,arglist,2);

    
    /* Commands - Menue */
    menu=XtCreatePopupShell("commandsmenu",simpleMenuWidgetClass,parent,NULL,0);
    widget=XtCreateManagedWidget("command0",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoCommand0,NULL);
        
        /* Parse User Commands */
        pos = app_resources.commands;
        
	while(pos != NULL)
	    {
	    pos2 = index(pos,'\t');
	    if(pos2 == NULL)
	        break;
	    *pos2='\0';pos2++;
	    cname = pos;pos=pos2;
	    pos2 = index(pos,'\n');
	    if(pos2 != NULL)
	         {
	        *pos2 = '\0';
	         pos2++;
	         }
            XtSetArg(arglist[0],XtNlabel,cname);   
            widget=XtCreateManagedWidget(cname,smeBSBObjectClass,menu,arglist,1);
            XtAddCallback(widget,XtNcallback,command_exec,pos);
 	    pos=pos2; 	       	    
	    }
	    
    XtSetArg(arglist[0],XtNlabel,"Commands");
    XtSetArg(arglist[1],XtNmenuName,"commandsmenu");
    XtCreateManagedWidget("commands_menu_button",menuButtonWidgetClass,b_row,arglist,2);
	    
    /* Pipes - Menue */
    menu=XtCreatePopupShell("pipesmenu",simpleMenuWidgetClass,parent,NULL,0);
    widget=XtCreateManagedWidget("pipe0",smeBSBObjectClass,menu,NULL,0);
    XtAddCallback(widget,XtNcallback,DoPipe0,NULL);

        /* Parse User Pipes */
        pos = app_resources.pipes;
        
	while(pos != NULL)
	    {
	    pos2 = index(pos,'\t');
	    if(pos2 == NULL)
	        break;
	    *pos2='\0';pos2++;
	    cname = pos;pos=pos2;
	    pos2 = index(pos,'\n');
	    if(pos2 != NULL)
	         {
	        *pos2 = '\0';
	         pos2++;
	         }
            XtSetArg(arglist[0],XtNlabel,cname);   
            widget=XtCreateManagedWidget(cname,smeBSBObjectClass,menu,arglist,1);
            XtAddCallback(widget,XtNcallback,pipe_exec,pos);
 	    pos=pos2; 	       	    
	    }    
	    
    XtSetArg(arglist[0],XtNlabel,"Pipes");
    XtSetArg(arglist[1],XtNmenuName,"pipesmenu");
    XtCreateManagedWidget("pipes_menu_button",menuButtonWidgetClass,b_row,arglist,2);
    
    /* Dirty Button */
    savedbitmap = XCreateBitmapFromData(CurDpy,RootWindow(CurDpy,DefaultScreen(CurDpy)),
                                 saved_bits,saved_width,saved_height);
    notsavedbitmap = XCreateBitmapFromData(CurDpy,RootWindow(CurDpy,DefaultScreen(CurDpy)),
                                 notsaved_bits,notsaved_width,notsaved_height);
    XtSetArg(arglist[0],XtNbitmap,savedbitmap);
    XtSetArg(arglist[1],XtNborderWidth,0);
    dirtylabel=XtCreateManagedWidget("dirty",labelWidgetClass,b_row,arglist,2);
   
    /* Menue Ending */
    XtSetArg(arglist[0],XtNlabel," -Insert-  ");
    modelabel=XtCreateManagedWidget("editmode",commandWidgetClass,b_row,arglist,1);
    XtAddCallback(modelabel,XtNcallback,xedtoggleoverwrite,0);
  }

  num_args = 0;
  if (filename != NULL) 
    {
    XtSetArg(arglist[num_args], XtNlabel, filename); 
    num_args++;
    }

  XtSetArg(arglist[num_args],XtNjustify,XtJustifyCenter); num_args++;
  XtSetArg(arglist[num_args],XtNlabel,"no file yet"); num_args++;
  labelwindow = XtCreateManagedWidget("labelWindow",labelWidgetClass, 
				      outer, arglist, num_args);

  num_args = 0;
  inserttrans=XtParseTranslationTable(app_resources.inserttranslations);
  overwritetrans=XtParseTranslationTable(app_resources.overwritetranslations);
  XtSetArg(arglist[num_args], XtNtype, XawAsciiFile); num_args++;
  XtSetArg(arglist[num_args], XtNeditType, XawtextEdit); num_args++;
  XtSetArg(arglist[num_args], XtNshowGrip, False); num_args++;
  XtSetArg(arglist[num_args] ,XtNscrollHorizontal,XawtextScrollWhenNeeded); num_args++;
  XtSetArg(arglist[num_args], XtNtranslations, inserttrans); num_args++;
  textwindow =  XtCreateManagedWidget("editWindow", asciiTextWidgetClass, 
				      outer, arglist, num_args);
  XtAppAddActions(XtWidgetToApplicationContext(textwindow),action_autoindent,2);
#ifdef MUTEKEYS
	XtAppAddActions(XtWidgetToApplicationContext(textwindow),mute_keys,1);
#endif
  trans=XtParseTranslationTable(":<Key>Return:autoindent()");
  
  XtSetArg(arglist[0], XtNfont, &font);
  XtSetArg(arglist[1],XtNtextSink,&widget);
  XtGetValues(textwindow,arglist,2);
  XtSetArg(arglist[0], XtNwidth,XTextWidth(font,"W",1)*app_resources.textwidth+20);
  XtSetArg(arglist[1], XtNheight,XawTextSinkMaxHeight(widget,app_resources.textheight));
  XtSetValues(textwindow,arglist,2);
  
  if(app_resources.autoIndent)
      XtOverrideTranslations(textwindow,trans);
  if(app_resources.tabsize > 0)
    set_tabsize(textwindow,app_resources.tabsize);

  XtAppAddActions(XtWidgetToApplicationContext(textwindow),menue_actions,1);

#ifdef SCROLLBREAK 
  XtAppAddActions(XtWidgetToApplicationContext(textwindow),action_cursorup,1);
  trans=XtParseTranslationTable(":<Key>Up:cursor_up()");
  XtOverrideTranslations(textwindow,trans);
  XtAppAddActions(XtWidgetToApplicationContext(textwindow),action_cursordown,1);
  trans=XtParseTranslationTable(":<Key>Down:cursor_down()");
  XtOverrideTranslations(textwindow,trans);
#endif
 
  XtRealizeWidget(parent);  
  if (filename != NULL)
      load_file(filename);
  ResetSourceChanged(textwindow);

}


/********************************************************************************
 *  Feep the bell								*
 ********************************************************************************/
void Feep()
{
  XBell(CurDpy, 0);
}


/********************************************************************************
 *       Main function								*
 ********************************************************************************/
void
main(argc, argv)
int argc;
char **argv;
{
  String filename = NULL;
  int line_nr,n;
  int readstdin=0;
  XawTextBlock block;
  char c;
  char workdir[MAXPATHLEN],completename[MAXPATHLEN];
  XtAppContext xtcontext;
  
  top = XtAppInitialize(&xtcontext, "Editor", NULL, 0,
			&argc, argv, fallback_resources, NULL, 0);
			     
  file_s = init_file_select(top);

  XtGetApplicationResources(top, &app_resources, resources,
			    XtNumber(resources), NULL, 0);
			    
  CurDpy = XtDisplay(top);
  if (argc > 1) 
    {
    if(strcmp(argv[1],"-")!=0)
      {
      Boolean exists;
      filename = argv[1];
      if(filename[0] != '/')
          {
          getdirectory(workdir,MAXPATHLEN);
          sprintf(completename,"%s/%s",workdir,filename);
          filename = completename;
          }

      switch ( CheckFilePermissions(filename, &exists)) 
        {
        case NO_READ:
	  if (exists)
	    fprintf(stderr, 
		    "File %s exists, and could not be opened for reading.\n", 
		    filename);
	  else
	    fprintf(stderr, "File %s %s %s",  filename, "does not exist,",
		    "and the directory could not be opened for writing.\n");
	  exit(1);
        case READ_OK:
        case WRITE_OK:         
        case CREATE_OK:
	  makeButtonsAndBoxes(top, filename);
	  break;
        default:
	  fprintf(stderr, "%s %s", "Internal function MaybeCreateFile()",
		"returned unexpected value.\n");
	  exit(1);
        }
      }
    else
	readstdin=1;         
    }

  /* Check EOF on stdin */
  if(readstdin)
      {
      n = read(0,&c,1);
      if(n==0)
          exit(0);
      }
  
  if((argc <=1)||(readstdin))  
      makeButtonsAndBoxes(top, NULL);
      
  creat_popups(top);     
  XtRealizeWidget(top);
            
  /* Initialize the icon window */
  ReadIcons(top);
  	
  XtAddEventHandler(top,NoEventMask,True,wm_delete_event,NULL);
  XtAddEventHandler(line_popup,NoEventMask,True,wm_delete_event,NULL);
  XtAddEventHandler(search_popup,NoEventMask,True,wm_delete_event,NULL);
  XtAddEventHandler(veto_popup,NoEventMask,True,wm_delete_event,NULL);
  XtAddEventHandler(option_popup,NoEventMask,True,wm_delete_event,NULL);
  XtAddEventHandler(sed_popup,NoEventMask,True,wm_delete_event,NULL);
  XtAddEventHandler(help_popup,NoEventMask,True,wm_delete_event,NULL);
  XtAddEventHandler(about_popup,NoEventMask,True,wm_delete_event,NULL);
  XtAddEventHandler(error_popup,NoEventMask,True,wm_delete_event,NULL);
  XtAddEventHandler(warn_popup,NoEventMask,True,wm_delete_event,NULL);
  XtAddEventHandler(command_popup,NoEventMask,True,wm_delete_event,NULL);
  XtAddEventHandler(pipe_popup,NoEventMask,True,wm_delete_event,NULL);
  set_wm_delete(top);
    
  if ((argc > 2)&&(!readstdin))
      {
      line_nr = atoi(argv[2]);
      goto_line2(line_nr);
      }
  
  if(readstdin)
      {
      block.firstPos = 0;
      block.ptr = &c;
      block.format = FMT8BIT;
      block.length=1;
      XawTextReplace(textwindow,0,0,&block);
      ResetSourceChanged(textwindow);     
      pipe_id = XtAppAddInput(XtWidgetToApplicationContext(textwindow),0,
                          (XtPointer)XtInputReadMask,pipe_input,NULL); 
      }
	
  /* Drag And Drop protocol stuff */
  DndInitialize(top);
  DndRegisterIconDrop(FileDropHandler);
  DndRegisterDragWidget(labelwindow,FileDragHandler,NULL);
  DndRegisterDropWidget(labelwindow,FileDropHandler,NULL);

  /* Let's go */
  XtAppMainLoop(xtcontext);
}
