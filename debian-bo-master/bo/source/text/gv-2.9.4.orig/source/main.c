/*
**
** main.c
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

#define _GV_MAIN_C_

/*
#define MESSAGES
*/
#include "message.h"

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "paths.h"
#include INC_X11(Intrinsic.h)
#include INC_X11(cursorfont.h)
#include INC_X11(StringDefs.h)
#include INC_X11(Shell.h)
#include INC_XAW(Cardinals.h)
#include INC_XAW(AsciiText.h)
#include INC_XAW(MenuButton.h)
#include INC_XAW(SimpleMenu.h)
#include INC_XAW(SmeBSB.h)
#include INC_XAW(SmeLine.h)
#include INC_XAW(Label.h)
#include INC_XAW(Command.h)
#include INC_XAW(XawInit.h)
#include INC_XMU(Editres.h)
#include "Aaa.h"
#include "Clip.h"
#include "FileSel.h"
#include "Frame.h"
#include "Ghostview.h"

#ifdef USE_PIXMAP_CODE
#   include "StrToPmap.h"
#endif

#ifdef VMS
#   include <unixlib.h>
#   include "strcasecmp.h"
#   include <stat.h>
#else
#   include <sys/stat.h>
#   include <unistd.h>
#endif

#include "gv.h"
#include "action_mag.h"
#include "actions.h"
#include "callbacks.h"
#include "d_memdebug.h"
#include "dialog.h"
#include "error.h"
#include "file.h"
#include "note.h"
#include "info.h"
#include "main_resources.h"
#include "main_globals.h"
#include "main_parseCmdLine.h"
#include "misc.h"
#include "options.h"
#include "popup.h"
#include "process.h"
#include "ps.h"
#include "doc_misc.h"
#include "version.h"
#include "stdc.h"

#define BITMAP_ARGS(name)\
           (const char*)CONCAT(name,_bits),\
           CONCAT(name,_width),\
           CONCAT(name,_height)
 
#ifdef FALLBACK_ICON_PIXMAP
#   include FALLBACK_ICON_PIXMAP
#endif
#ifdef FALLBACK_SELECTED_BITMAP
#   include FALLBACK_SELECTED_BITMAP
#endif
#ifdef FALLBACK_DOCUMENT_BITMAP
#   include FALLBACK_DOCUMENT_BITMAP
#endif

static XtActionsRec actions[] = {
 { "GV_Antialias"	, action_antialias		},
 { "GV_Center"		, action_center			},
 { "GV_CheckFile"	, action_checkFile		},
 { "GV_DeleteWindow"	, action_deleteWindow		},
#if 0
 { "GV_Dialog"		, action_handleDialogPopup	},
#endif /* 0 */
 { "GV_DismissPopup"	, action_dismissPopup		},
 { "GV_EraseLocator"	, action_erase_locator		},
 { "GV_HandleDSC"	, action_handleDSC		},
 { "GV_MagMenu"         , action_magMenu           	},
 { "GV_MiscMenu"        , action_miscMenu           	},
 { "GV_MovePage"	, action_movePage		},
 { "GV_Next"		, action_next			},
 { "GV_Open"		, action_open			},
 { "GV_OtherPage"	, action_otherPage		},
 { "GV_Panner"		, action_panner			},
 { "GV_PopupMenu"	, action_popup_menu		},
 { "GV_Previous"	, action_prev			},
 { "GV_Print"		, action_print			},
 { "GV_Quit"		, action_quit			},
 { "GV_Redisplay"	, action_redisplay		},
 { "GV_Reopen"		, action_reopen			},
 { "GV_Resizing"	, action_autoResize		},
 { "GV_Save"		, action_save			},
 { "GV_Scroll"		, action_scroll			},
 { "GV_SetMagstep"	, action_set_magstep		},
 { "GV_SetOrientation"	, action_set_orientation	},
 { "GV_SetPageMark"	, action_setPageMark		},
 { "GV_SetPageMedia"	, action_set_pagemedia		},
 { "GV_Show"		, action_showThisPage		},
 { "GV_TogDialPrefBut"	, action_preferDialogPopupButton },
};

#ifdef FALLBACK_RESOURCES
static String fallback_resources[] = {
#   include FALLBACK_RESOURCES
    NULL
};
#endif

static String intern_resources[] = {
#   include INTERN_RESOURCES_H
    NULL
};

#ifdef FALLBACK_MESSAGES
static String fallback_messages[] = {
#   include FALLBACK_MESSAGES
    NULL
};
#endif

/*--------------------------------------------------------------
   dummyCvtStringToPixmap
   Dummy String to Pixmap converter. Used to suppress warnings
   about missing String to Pixmap converter.
   Background: on Motif displays 'xrdb -q' shows resource
   entries "*topShadowPixmap: unspecified_pixmap" and
   "*bottomShadowPixmap: unspecified_pixmap". Since the ThreeD
   widget of Xaw3d also uses these resources without
   installing a converter we get tons of warnings. ###jp###
--------------------------------------------------------------*/

static Boolean
dummyCvtStringToPixmap(dpy, args, num_args, fromVal, toVal,converter_data)
   Display   *dpy;
   XrmValue  *args;
   Cardinal  *num_args;
   XrmValue  *fromVal;
   XrmValue  *toVal;
   XtPointer *converter_data;
{   
   BEGINMESSAGE(dummyCvtStringToPixmap)
#  ifdef MESSAGES
   {
      char *name = (char*) fromVal->addr;
      INFSMESSAGE(will not convert,name)
   }
#  endif
   ENDMESSAGE(dummyCvtStringToPixmap)
   return(False);
}

/*### Procedure and Macro Declarations ###########################################*/

static void output_message();
static char *GetPseudoResource();
static char *GetInternResource();
static char *main_mergeFileIntoDatabase();

#ifdef max
#   undef max
#endif
#define max(a,b) ((a)>(b)?(a):(b))
#ifdef min
#   undef min
#endif
#define min(a,b) ((a)<(b)?(a):(b))

/*#################################################################################
   Main
#################################################################################*/

int
main(argc, argv)
int  argc;
char *argv[];
{

  {
    struct stat  sbuf;
    Screen       *screen;
    Arg          args[20];
    Cardinal     n;
    Widget       lineEntry;
    int          number;
    int          i;
    static       String nothing = "";
    static       XawTextSelectType sarry[] = {XawselectLine, XawselectAll, XawselectNull};
    Widget       cont_child[50];
    Cardinal     cont_child_num=0;
    XrmDatabase  db;
    Dimension    maximum_width,maximum_height;
    unsigned int gwidth=0,gheight=0;

    MAINBEGINMESSAGE(main)

/*###  initializing global variables ####################################*/

    INFMESSAGE(initializing global variables)
    gv_scroll_mode = SCROLL_MODE_NONE;
    gv_class = GV_CLASS;
    gv_intern_name = GV_INTERN_NAME;
    gv_application_name = GV_APPLICATION_NAME;
    gv_pending_page_request=NO_CURRENT_PAGE;
    gv_filename    = NULL;
    gv_oldfilename = NULL;
    infopopup      = NULL;
    optionpopup    = NULL;
    dialogpopup    = NULL;
    notepopup      = NULL;
    versionpopup   = NULL;
    FileSel_popup  = NULL;
    pagemediaEntry = NULL;
#ifdef VMS
    gv_print_kills_file = 1;
#else
    gv_print_kills_file = 0;
#endif

/*###  initializing toolkit and the application context #################*/

    INFMESSAGE(initializing toolkit and the application context)
    XtToolkitInitialize();
    app_con = XtCreateApplicationContext();
    XtAppAddActions(app_con, actions, XtNumber(actions));

#   ifdef FALLBACK_RESOURCES
       INFMESSAGE(setting fallback resources)
       XtAppSetFallbackResources(app_con, fallback_resources);
#   endif

/*### opening display #######################################################*/

   INFMESSAGE(opening display)
   {
      char tmp[GV_MAX_FILENAME_LENGTH];
#ifdef VMS
      sprintf(tmp,"DECW$USER_DEFAULTS:%s.DAT;",gv_class);
      gv_user_defaults_file = GV_XtNewString(tmp);
#else
      const char *str_XUSERFILESEARCHPATH="XUSERFILESEARCHPATH";
      char *ori_XUSERFILESEARCHPATH;
#     ifndef USER_DEAULTS
#        define USER_DEFAULTS "~/.gv"
#     endif
      strcpy(tmp,USER_DEFAULTS);
      file_translateTildeInPath(tmp);
      gv_user_defaults_file = GV_XtNewString(tmp);
      ori_XUSERFILESEARCHPATH = getenv(str_XUSERFILESEARCHPATH);
      setenv(str_XUSERFILESEARCHPATH,gv_user_defaults_file,1);
#endif
      INFSMESSAGE(user resources:,gv_user_defaults_file)

      gv_display = XtOpenDisplay(app_con,NULL,NULL,gv_class,options,XtNumber(options),&argc,argv);
      if (gv_display==NULL) {
         fprintf(stderr, "  %s: Unable to open DISPLAY.\n", gv_application_name);
         exit(EXIT_STATUS_ERROR);
      }
      XtGetApplicationNameAndClass(gv_display,&gv_name,&gv_class);
      SMESSAGE(gv_name)SMESSAGE(gv_class)
      db=XtDatabase(gv_display);
      if (!db) {
         fprintf(stderr,"  %s:  No database associated with display\n",gv_application_name);
         XtDestroyApplicationContext(app_con);
         exit(EXIT_STATUS_ERROR);
      }
      screen = DefaultScreenOfDisplay(gv_display);
#ifndef VMS
      if (ori_XUSERFILESEARCHPATH) setenv(str_XUSERFILESEARCHPATH,ori_XUSERFILESEARCHPATH,1);
      else unsetenv(str_XUSERFILESEARCHPATH);
#endif
   }

/*### parsing what's left of the command line ###################*/

    INFMESSAGE(parsing remaining command line)
    gv_filename     = NULL;
    gv_ad_file      = NULL;
    gv_style_file   = NULL;
    gv_gs_safer     = -1;
    gv_gs_quiet     = -1;
    gv_gs_arguments = NULL;
    gv_scanfilename = NULL;
    {
       int retval;
       retval = main_parseCmdLine(argc,argv);
       if      (retval==1) output_message(gv_display,"usage",0);
       else if (retval==2) output_message(gv_display,"help",0);
    }

/*### internal resources ########################################*/

    INFMESSAGE(merging compile time internal resources into the database)
    {
       String *sP = intern_resources;
       while (*sP) {
          SMESSAGE(*sP)
          XrmPutLineResource(&db,*sP++);
       }
    }
    INFMESSAGE(merging user specified internal resources into the database)
    {
#      define RES_LINELENGTH 512
       FILE *fp;
       char line[RES_LINELENGTH];
       fp=fopen(gv_user_defaults_file,"r"); 
       if (fp) {
          while (fgets(line,RES_LINELENGTH,fp)) {
             if (!strncmp(line,"GVintern",3)) {
                INFSMESSAGE(merging,line)
                XrmPutLineResource(&db,line);
             }
          }
          fclose(fp);
       }
    }

/*### handling resource files ###################################*/

   {
      if (gv_style_file) {     
         INFSMESSAGE(merging style resource file into database,gv_style_file)
         main_mergeFileIntoDatabase(gv_display,gv_style_file,0);
      }

      if (gv_ad_file) {
         char *fn;
         INFSMESSAGE(merging ad resource file into database,gv_ad_file)    
         fn=main_mergeFileIntoDatabase(gv_display,gv_ad_file,1);
         if (fn) {
            GV_XtFree(gv_user_defaults_file);
            gv_user_defaults_file=fn;
            INFSMESSAGE(options will be saved to,gv_user_defaults_file)    
         }
      }

#ifdef VMS
      {
         char tmp[GV_MAX_FILENAME_LENGTH];
         sprintf(tmp,"DECW$USER_DEFAULTS:%s_CLASS.DAT",gv_class);
         INFSMESSAGE(loading class resources:,tmp)
         XrmCombineFileDatabase(tmp,&db,False);
         sprintf(tmp,"DECW$SYSTEM_DEFAULTS:%s_CLASS.DAT",gv_class);
         INFSMESSAGE(loading class resources:,tmp)
         XrmCombineFileDatabase(tmp,&db,False);
      }
#endif
   }


/*### getting internal resources ############################################*/

   INFMESSAGE(getting internal resources)
   {
      char *s;
      gv_print_command       = GetInternResource(gv_display,"printCommand");
      gv_gs_interpreter      = GetInternResource(gv_display,"gsInterpreter");
      gv_gs_cmd_scan_pdf     = GetInternResource(gv_display,"gsCmdScanPDF");
      gv_gs_cmd_conv_pdf     = GetInternResource(gv_display,"gsCmdConvPDF");
      gv_gs_x11_device       = GetInternResource(gv_display,"gsX11Device");
      gv_gs_x11_alpha_device = GetInternResource(gv_display,"gsX11AlphaDevice");
      if (gv_gs_safer < 0) { /* get it only if it was not found on the command line */
         s                        = GetInternResource(gv_display,"gsSafer");
         if (!strcasecmp(s,"true"))  gv_gs_safer = 1; else gv_gs_safer = 0;
      }
      if (gv_gs_quiet < 0) { /* get it only if it was not found on the command line */
         s                        = GetInternResource(gv_display,"gsQuiet");
         if (!strcasecmp(s,"true"))  gv_gs_quiet = 1; else gv_gs_quiet = 0;
      }
      if (!gv_gs_arguments) { /* get it only if it was not found on the command line */
         gv_gs_arguments          = GetInternResource(gv_display,"gsArguments");
      }
   }

/*### handling geometry resource #######################################*/

    INFMESSAGE(handling geometry resource)
    {
       char tmp[GV_MAX_FILENAME_LENGTH];
       char *pos;

       pos = GetPseudoResource(gv_display,"geometry","Geometry");
       if (pos) {
          int gx=0,gy=0;
          int flag;
          char g[20];
          INFSMESSAGE(found geometry resource,pos)
          flag=XParseGeometry(pos,&gx,&gy,&gwidth,&gheight);
          IIMESSAGE(gx,gy) IIMESSAGE(gwidth,gheight)
          if (gwidth)  gwidth   = max(gwidth,GV_MINIMUM_SIZE);
          if (gheight) gheight  = max(gheight,GV_MINIMUM_SIZE);
                                                                             tmp[0]='\0';
          if (flag&WidthValue) {
             sprintf(g,"%d",(Dimension)gwidth);                              strcat(tmp,g);
          }
          if (flag&HeightValue) {
             sprintf(g,"x%d",(Dimension)gheight);                            strcat(tmp,g);
          }
          if (flag&XValue) {
             sprintf(g,"%s%d",((flag&XNegative) ? (gx ? "":"-") : "+"),gx);   strcat(tmp,g);
          }
          if (flag&YValue) {
             sprintf(g,"%s%d",((flag&YNegative) ? (gy ? "":"-") : "+"),gy);   strcat(tmp,g);
          }
          if (tmp[0]!='\0') {
             char s[255];
             INFSMESSAGE(corrected geometry,tmp)
             sprintf(s,"%s.geometry",gv_name);
             SMESSAGE(s) SMESSAGE(tmp)
             XrmPutStringResource(&db,s,tmp);
#            ifdef MESSAGES
                pos = GetPseudoResource(gv_display,"geometry","Geometry");
                if (pos) { INFSMESSAGE(check:,pos) }
#            endif
          }
       }
    }

/*### initializing widget set and creating application shell #########################*/

    INFMESSAGE(initializing widget set)
    XawInitializeWidgetSet();
#ifdef USE_PIXMAP_CODE
    XfwfInstallStringToPixmapConverter();
#else
    XtAppSetTypeConverter(app_con,XtRString,XtRPixmap,dummyCvtStringToPixmap,
                          NULL,0,XtCacheNone,NULL);   
#endif
    old_Xerror = XSetErrorHandler(catch_Xerror);
    wm_delete_window = XInternAtom(gv_display, "WM_DELETE_WINDOW", False);

    INFMESSAGE(creating the application shell)
    {
       char title[256];
#ifdef SHOW_COMPILE_DATE
       sprintf(title,"%s (compiled on %s)",versionIdentification[0],__DATE__);
#else
       sprintf(title,"%s",versionIdentification[0]);
#endif
								n=0;
             XtSetArg(args[n], XtNallowShellResize, True);	n++;
             XtSetArg(args[n], XtNtitle, title);		n++;
       toplevel = XtAppCreateShell(NULL,gv_class,applicationShellWidgetClass,
                  gv_display,args,n);
    }

    /* support for Editres ###jp### 06/18/95 */
    XtAddEventHandler(toplevel, (EventMask) 0, TRUE,
                      _XEditResCheckMessages, (XtPointer)NULL);
    
#ifdef FALLBACK_ICON_NAME
    INFMESSAGE(setting the icon) 
    {
       Pixmap icon_pixmap;
       XtSetArg(args[0], XtNiconPixmap, &icon_pixmap);
       XtGetValues(toplevel, args, 1);
       if (icon_pixmap == None) {
          icon_pixmap = 
          XCreateBitmapFromData(gv_display, RootWindowOfScreen(screen),
                                BITMAP_ARGS(FALLBACK_ICON_NAME));
          XtSetArg(args[0], XtNiconPixmap, icon_pixmap);
          XtSetValues(toplevel, args, 1);
       }
    }
#endif

/*### getting application resources #########################################*/

    INFMESSAGE(retrieving and analyzing application resources)
    {
       XtGetApplicationResources(toplevel,(XtPointer) &app_res,resources,
                                 XtNumber(resources),NULL,ZERO);
       if (!strcmp(app_res.version,GV_DEFAULT_VERSION))
	  output_message(gv_display,"missing-resources",0);
       else {
	 int n1,n2,v1,v2,v3,v1c,v2c,v3c;
	 v1 = v2 = v3 = v1c = v2c = v3c = 0;
	 n1 = sscanf(app_res.version,"%*s %d.%d.%d",&v1,&v2,&v3);
	 n2 = sscanf(versionCompatibility,"%*s %d.%d.%d",&v1c,&v2c,&v3c);
	 if ((n1 < 2) || (n2 < 2)
	     || (v1 < v1c)
	     || (v1 == v1c && v2 < v2c)
	     || (v1 == v1c && v2 == v2c && v3 < v3c))
	   output_message(gv_display,"version-mismatch",0);
       }

       if ((!strrchr(app_res.version,' ')) ||
           ((int)(1000*atof(strrchr(app_res.version,' ')))<(int)(1000*atof(strrchr(versionCompatibility,' '))))
          ) output_message(gv_display,"version-mismatch",0);

#ifdef FALLBACK_SELECTED_NAME
       if (app_res.selected_bitmap == None)
           app_res.selected_bitmap =
              XCreateBitmapFromData(gv_display, RootWindowOfScreen(screen),
                                  BITMAP_ARGS(FALLBACK_SELECTED_NAME));
#endif
#ifdef FALLBACK_DOCUMENT_NAME
       if (app_res.document_bitmap == None)
           app_res.document_bitmap =
              XCreateBitmapFromData(gv_display, RootWindowOfScreen(screen),
                                  BITMAP_ARGS(FALLBACK_DOCUMENT_NAME));
#endif    
       app_res.minimum_width  = app_res.minimum_width  < 300 ? 300 : app_res.minimum_width; 
       app_res.minimum_height = app_res.minimum_height < 300 ? 300 : app_res.minimum_height; 
    }


/*### Parsing maximum width, maximum height resources, creating control ####################*/

    {
       char *pos;
       int width,height;
       char* max_size_screen = "screen";

       INFMESSAGE(parsing maximum size resources)
       pos= strstr(app_res.maximum_width,max_size_screen);
       if (pos) { width=WidthOfScreen(XtScreen(toplevel))+atoi(pos+strlen(max_size_screen)); }
       else     { width=atoi(app_res.maximum_width); }
       maximum_width = (width > 0 ? (Dimension) width : 0);
       maximum_width = max(maximum_width,(Dimension)app_res.minimum_width);
       if (maximum_width<(Dimension)gwidth) maximum_width=(Dimension)gwidth;
       pos= strstr(app_res.maximum_height,max_size_screen);
       if (pos) { height=HeightOfScreen(XtScreen(toplevel))+atoi(pos+strlen(max_size_screen)); }
       else     { height=atoi(app_res.maximum_height); }
       maximum_height = (height > 0 ? (Dimension) height : 0);
       maximum_height = max(maximum_height,(Dimension)app_res.minimum_height);
       if (maximum_height<(Dimension)gheight) maximum_height=(Dimension)gheight;
       IIMESSAGE(maximum_width,maximum_height)

       INFMESSAGE(creating control)
                               				n=0;
       if (gwidth) {
          app_res.auto_resize=False;
          XtSetArg(args[n], XtNresizeWidth, False);	n++;
          XtSetArg(args[n], XtNwidth, (Dimension)gwidth);n++;
          INFIMESSAGE(forcing width for control:,gwidth)
       } else {
          XtSetArg(args[n], XtNresizeWidth, True);	n++;
       }
       if (gheight) {
          app_res.auto_resize=False;
          XtSetArg(args[n], XtNresizeHeight, False);	n++;
          XtSetArg(args[n], XtNheight, (Dimension)gheight);n++;
          INFIMESSAGE(forcing height for control:,gheight)
       } else {
          XtSetArg(args[n], XtNresizeHeight, True);	n++;
       }
       XtSetArg(args[n], XtNmaximumWidth, maximum_width); n++;
       XtSetArg(args[n], XtNmaximumHeight,maximum_height);n++; 
       XtSetArg(args[n], XtNminimumWidth, (Dimension)app_res.minimum_width); n++;
       XtSetArg(args[n], XtNminimumHeight,(Dimension)app_res.minimum_height);n++; 
       XtSetArg(args[n], XtNconditionedResize,False);	n++;
       control = XtCreateWidget("control",aaaWidgetClass,toplevel,args,n);
    }

/*### Creating the Menu ###############################################################*/

    INFMESSAGE(menus)

							n=0;
	    XtSetArg(args[n], XtNresize, False);	n++;
    fileButton = XtCreateManagedWidget("fileButton",menuButtonWidgetClass,control,args,n);
    cont_child[cont_child_num] = fileButton; cont_child_num++;

							n=0;
    fileMenu = XtCreatePopupShell("menu", simpleMenuWidgetClass,fileButton,args,n);
    openEntry = XtCreateManagedWidget("open", smeBSBObjectClass,fileMenu,args,n);
            XtAddCallback(openEntry, XtNcallback, cb_openFile, (XtPointer)NULL);
    reopenEntry = XtCreateManagedWidget("reopen", smeBSBObjectClass,fileMenu,args,n);
            XtAddCallback(reopenEntry, XtNcallback,cb_reopen, (XtPointer)NULL);
    printAllEntry = XtCreateManagedWidget("printAllPages", smeBSBObjectClass,fileMenu,args,n);
            XtAddCallback(printAllEntry, XtNcallback, cb_print,(XtPointer)PAGE_MODE_ALL);
    printMarkedEntry = XtCreateManagedWidget("printMarkedPages", smeBSBObjectClass,fileMenu,args,n);
            XtAddCallback(printMarkedEntry, XtNcallback,cb_print,(XtPointer)(PAGE_MODE_MARKED|PAGE_MODE_CURRENT));
    saveAllEntry = XtCreateManagedWidget("saveAllPages", smeBSBObjectClass,fileMenu,args,n);
            XtAddCallback(saveAllEntry, XtNcallback, cb_save, (XtPointer)PAGE_MODE_ALL);
    saveMarkedEntry = XtCreateManagedWidget("saveMarkedPages", smeBSBObjectClass,fileMenu,args,n);
            XtAddCallback(saveMarkedEntry, XtNcallback, cb_save,(XtPointer)(PAGE_MODE_MARKED|PAGE_MODE_CURRENT));
    lineEntry = XtCreateManagedWidget("line", smeLineObjectClass,fileMenu,args,n);
    stopEntry = XtCreateManagedWidget("stop", smeBSBObjectClass,fileMenu,args,n);
            XtAddCallback(stopEntry, XtNcallback, cb_stopInterpreter,(XtPointer)NULL);
    dscEntry = XtCreateManagedWidget("dsc", smeBSBObjectClass,fileMenu,args,n);
            XtAddCallback(dscEntry, XtNcallback, cb_handleDSC,(XtPointer)1);
            cb_handleDSC(dscEntry,(XtPointer)NULL,(XtPointer)NULL);
    lineEntry = XtCreateManagedWidget("line", smeLineObjectClass,fileMenu,args,n);
    optionEntry = XtCreateManagedWidget("options", smeBSBObjectClass,fileMenu,args,n);
            XtAddCallback(optionEntry, XtNcallback, cb_popupOptionPopup,(XtPointer)NULL);
    copyrightEntry = XtCreateManagedWidget("copyright", smeBSBObjectClass,fileMenu,args,n);
            XtAddCallback(copyrightEntry, XtNcallback,cb_popupVersionPopup,(XtPointer)NULL);
    quitEntry = XtCreateManagedWidget("quit", smeBSBObjectClass,fileMenu,args,n);
            XtAddCallback(quitEntry, XtNcallback, cb_quitGhostview, (XtPointer)0);

							n=0;
	    XtSetArg(args[n], XtNresize, False);	n++;
    pageButton = XtCreateManagedWidget("pageButton", menuButtonWidgetClass,control,args,n);
    cont_child[cont_child_num] = pageButton; cont_child_num++;

							n=0;
    pageMenu = XtCreatePopupShell("menu", simpleMenuWidgetClass,pageButton,args,n);
    nextEntry = XtCreateManagedWidget("next", smeBSBObjectClass,pageMenu,args,n);
            XtAddCallback(nextEntry, XtNcallback, cb_showNextPage, (XtPointer)0);
    showEntry = XtCreateManagedWidget("show", smeBSBObjectClass,pageMenu,args,n);
            XtAddCallback(showEntry, XtNcallback, cb_redisplay, (XtPointer)0);
    prevEntry = XtCreateManagedWidget("prev", smeBSBObjectClass,pageMenu,args,n);
            XtAddCallback(prevEntry, XtNcallback, cb_showPreviousPage, (XtPointer)0);
    lineEntry = XtCreateManagedWidget("line", smeLineObjectClass,pageMenu,args,n);
    centerEntry = XtCreateManagedWidget("center", smeBSBObjectClass,pageMenu,args,n);
           XtAddCallback(centerEntry, XtNcallback, cb_positionPage, (XtPointer)1);

							n=0;
	XtSetArg(args[n], XtNresize, True);		n++;
	XtSetArg(args[n], XtNlabel, " 1.000 ");		n++;
    magstepButton = XtCreateManagedWidget("magstepButton",menuButtonWidgetClass,control,args,n);
    cont_child[cont_child_num] =magstepButton; cont_child_num++;

							n=0;
    magstepMenu = XtCreatePopupShell("menu", simpleMenuWidgetClass,magstepButton,args,n);
    magstepEntry = (Widget *) GV_XtMalloc((app_res.maximum_magstep-app_res.minimum_magstep+1) *sizeof(Widget));
           XtSetArg(args[n], XtNleftMargin, 20);                        n++;

    {
       char buf[15];
       float scalefactor= 1.4142;
       float fscale = 1.0;

       i = app_res.minimum_magstep; i < 0 ? i = -i : i ;
       while (0<i--) fscale /= scalefactor;
       i = app_res.minimum_magstep;
       for (i = app_res.minimum_magstep; i <= app_res.maximum_magstep; i++) {
          sprintf(buf, " %5.3f ",fscale);
          magstepEntry[i-app_res.minimum_magstep] = XtCreateManagedWidget(buf, smeBSBObjectClass,magstepMenu,args,n);
          XtAddCallback(magstepEntry[i-app_res.minimum_magstep], XtNcallback,cb_setMagstep, (XtPointer)i);
          fscale *= scalefactor;
       }
    }

    {
 							n=0;
	   XtSetArg(args[n], XtNresize, True);		n++;
       orientationButton = XtCreateManagedWidget("orientationButton",menuButtonWidgetClass,control,args,n);
       cont_child[cont_child_num] = orientationButton; cont_child_num++;

							n=0;
       orientationMenu = XtCreatePopupShell("menu", simpleMenuWidgetClass,orientationButton,args,n);
							n=0;
           XtSetArg(args[n], XtNleftMargin, 20);	n++;
       autoOrientEntry = XtCreateManagedWidget("automatic",smeBSBObjectClass,orientationMenu,args,n);
           XtAddCallback(autoOrientEntry,XtNcallback,cb_setOrientation,(XtPointer)O_AUTOMATIC);
       lineEntry = XtCreateManagedWidget("line",smeLineObjectClass,orientationMenu,args,ZERO);
       portraitEntry = XtCreateManagedWidget("portrait",smeBSBObjectClass,orientationMenu,args,n);
           XtAddCallback(portraitEntry,XtNcallback,cb_setOrientation,(XtPointer)O_PORTRAIT);
       landscapeEntry = XtCreateManagedWidget("landscape",smeBSBObjectClass,orientationMenu,args,n);
           XtAddCallback(landscapeEntry,XtNcallback,cb_setOrientation,(XtPointer)O_LANDSCAPE);
       upsidedownEntry = XtCreateManagedWidget("upsidedown",smeBSBObjectClass,orientationMenu,args,n);
           XtAddCallback(upsidedownEntry,XtNcallback,cb_setOrientation, (XtPointer)O_UPSIDEDOWN);
       seascapeEntry = XtCreateManagedWidget("seascape",smeBSBObjectClass,orientationMenu,args,n);
           XtAddCallback(seascapeEntry,XtNcallback,cb_setOrientation,(XtPointer)O_SEASCAPE);

#ifdef USE_SWAP_LANDSCAPE
       lineEntry = XtCreateManagedWidget("line",smeLineObjectClass,orientationMenu,args,ZERO);
       swapEntry = XtCreateManagedWidget("swap",smeBSBObjectClass,orientationMenu,args,n);
           XtAddCallback(swapEntry,XtNcallback,cb_setOrientation,(XtPointer)O_SWAP_LANDSCAPE);
#endif
							n=0;
	  XtSetArg(args[n], XtNresize, True);		n++;
       pagemediaButton = XtCreateManagedWidget("pagemediaButton",menuButtonWidgetClass,control,args,n);
       cont_child[cont_child_num] = pagemediaButton; cont_child_num++;
    }


    {
       INFMESSAGE(creating processButton and processMenu)
           					n=0;
          XtSetArg(args[n], XtNresize, False);	n++;
       processButton = XtCreateManagedWidget(
               "processButton", menuButtonWidgetClass,control,args,n);
       cont_child[cont_child_num] = processButton; cont_child_num++;
       processMenu=NULL;
    }

/*### Optional Widgets ##############################################################*/

    {
       char *layout;
       char *widgetname;

       layout = GetPseudoResource(gv_display,"control.layout","Aaa.Layout");
       if (!layout) {
          fprintf(stderr,"  %s: Error, layout resource not found\n",versionIdentification[0]);
          exit(EXIT_STATUS_ERROR);
       }

       INFMESSAGE(optional widgets: labels)
       {
          char wn[20];
          int i=1;
						n=0;
          XtSetArg(args[n], XtNresize, True);	n++;
          while (i<10) {
             sprintf(wn,"label%d",i);
             if (!strstr(layout,wn)) break;
             INFIMESSAGE(creating label,i)
             cont_child[cont_child_num]=XtCreateWidget(wn,labelWidgetClass,control,args,n);
             cont_child_num++;
             ++i;
          }
       }

#if 0
       INFMESSAGE(optional widgets: lines)
       {
          char wn[20];
          int i=1;
						n=0;
          XtSetArg(args[n], XtNresize, True);	n++;
          while (i<10) {
             sprintf(wn,"line%d",i);
             if (!strstr(layout,wn)) break;
             INFIMESSAGE(creating label,i)
             cont_child[cont_child_num]=XtCreateWidget(wn,threeDWidgetClass,control,args,n);
             cont_child_num++;
             ++i;
          }
       }
#endif

       {
          int i=0;
          struct { Widget		*widget;
                   Bool 		*show;
                   String		name;
                   XtCallbackProc	callback;
                   XtPointer		client_data;
          } b[] = {
            { &w_toggleCurrentPage , &show_toggleCurrentPage , "toggleCurrent" , cb_setPageMark , (XtPointer)(SPM_CURRENT|SPM_TOGGLE) },
            { &w_toggleAllPages    , &show_toggleAllPages    , "toggleAll"     , cb_setPageMark , (XtPointer)(SPM_ALL|SPM_TOGGLE)     },
            { &w_toggleEvenPages   , &show_toggleEvenPages   , "toggleEven"    , cb_setPageMark , (XtPointer)(SPM_EVEN|SPM_TOGGLE)    },
            { &w_toggleOddPages    , &show_toggleOddPages    , "toggleOdd"     , cb_setPageMark , (XtPointer)(SPM_ODD|SPM_TOGGLE)     },
            { &w_unmarkAllPages    , &show_unmarkAllPages    , "unmarkAll"     , cb_setPageMark , (XtPointer)(SPM_ALL|SPM_UNMARK)     },
            { &w_printMarkedPages  , &show_printMarkedPages  , "printMarked"   , cb_print       , (XtPointer)(PAGE_MODE_MARKED|PAGE_MODE_CURRENT)},
            { &w_printAllPages     , &show_printAllPages     , "printAll"      , cb_print       , (XtPointer)PAGE_MODE_ALL           },
            { &w_saveMarkedPages   , &show_saveMarkedPages   , "saveMarked"    , cb_save        , (XtPointer)(PAGE_MODE_MARKED|PAGE_MODE_CURRENT)},
            { &w_saveAllPages      , &show_saveAllPages      , "saveAll"       , cb_save        , (XtPointer)PAGE_MODE_ALL            },
            { &w_openFile          , &show_openFile          , "openFile"      , cb_openFile ,    (XtPointer)NULL },
            { &w_autoResize        , &show_autoResize        , "autoResize"    , cb_autoResize ,  (XtPointer)NULL },
            { &w_showThisPage      , &show_showThisPage      , "redisplay"     , cb_redisplay ,   (XtPointer)NULL },
            { &w_updateFile        , &show_updateFile        , "updateFile"    , cb_checkFile ,   (XtPointer)CHECK_FILE_VERSION },
            { &w_checkFile         , &show_checkFile         , "checkFile"     , cb_checkFile ,   (XtPointer)CHECK_FILE_DATE },
            { NULL         , NULL        , NULL     , NULL ,  NULL },
          };
          INFMESSAGE(optional widgets: buttons)
						n=0;
          XtSetArg(args[n], XtNresize, True);	n++;
          while (b[i].widget) {
             INFSMESSAGE(creating widget,b[i].name)
             *(b[i].show) = strstr(layout,b[i].name) ? True : False;
             if (*(b[i].show)) {
                *(b[i].widget) = XtCreateWidget(b[i].name,commandWidgetClass,control,args,n);
                XtAddCallback(*(b[i].widget),XtNcallback,b[i].callback,b[i].client_data);
                cont_child[cont_child_num] = *(b[i].widget); cont_child_num++;
             }
             ++i;
          }
       }


       INFMESSAGE(optional widgets: informational widgets)
#      define _mw_(widget,show,name)						\
         widgetname=name;							\
         show = strstr(layout,widgetname) ? True : False;			\
         if (show) {								\
            widget = XtCreateWidget(widgetname,labelWidgetClass,control,args,n);\
            cont_child[cont_child_num] = widget; cont_child_num++;		\
         }

	      						n=0;
	XtSetArg(args[n], XtNresize, True);		n++;
       _mw_( titlebutton , show_title   , "titleButton" );
       _mw_( datebutton  , show_date    , "dateButton"  );

       widgetname="locator";
       show_locator = strstr(layout,widgetname) ? True : False;
       if (show_locator) {
          char buf[MAX_LOCATOR_LENGTH];
          sprintf(buf,app_res.locator_format,9999,9999);
          XtSetArg(args[n], XtNlabel,buf);		n++;
          locator = XtCreateWidget(widgetname,labelWidgetClass,control,args,n);
          cont_child[cont_child_num] =locator; cont_child_num++;
       }
#      undef _mw_


       INFMESSAGE(panner)
       widgetname="pannerFrame";
       show_panner = strstr(layout,widgetname) ? True : False;
       if (show_panner) {

  									n=0;
          pannerFrame = XtCreateWidget("pannerFrame",frameWidgetClass,control,args,n);
          cont_child[cont_child_num] = pannerFrame; cont_child_num++;
  									n=0;
          panner = XtCreateManagedWidget("panner", compositeWidgetClass,pannerFrame, args, n);
         	      							n=0;
          XtSetArg(args[n], XtNresize,False);				n++;
          XtSetArg(args[n], XtNlabel,"");			      	n++;
          slider = XtCreateManagedWidget("slider", labelWidgetClass,panner, args, n);
       }

    } /* end of optional widgets */

/*### Table of Contents ###########################################################*/

    INFMESSAGE(table of contents)

									n=0;
            XtSetArg(args[n], XtNresizable, True);			n++;
    tocFrame = XtCreateWidget("tocFrame",frameWidgetClass,control,args,n);

    cont_child[cont_child_num] = tocFrame; cont_child_num++;

									n=0;
	    XtSetArg(args[n], XtNresize, False);			n++;
            XtSetArg(args[n], XtNdisplayCaret, False);			n++;
            XtSetArg(args[n], XtNuseStringInPlace, True);		n++;
            XtSetArg(args[n], XtNlength, 0);				n++;
            XtSetArg(args[n], XtNstring, nothing);			n++;
            XtSetArg(args[n], XtNselectTypes, sarry);			n++;
            XtSetArg(args[n], XtNscrollVertical, XawtextScrollAlways);	n++;
            XtSetArg(args[n], XtNwidth,TOC3D_INITIAL_WIDTH);	n++;
            XtSetArg(args[n], XtNheight,TOC3D_INITIAL_HEIGHT);	n++;
    toc = XtCreateManagedWidget("toc", asciiTextWidgetClass,tocFrame, args, n);

/*### The Page View ###########################################################*/

   INFMESSAGE(viewport)

									n=0;
           XtSetArg(args[n], XtNresizable, True);			n++;
   viewFrame = XtCreateWidget("viewFrame", frameWidgetClass,control,args,n);
   cont_child[cont_child_num] = viewFrame; cont_child_num++;

									n=0;
   viewClip = XtCreateManagedWidget("viewClip", clipWidgetClass,viewFrame,args,n);
           if (show_panner) XtAddCallback(viewClip, XtNreportCallback,cb_adjustSlider,(XtPointer)NULL);
            XtAddCallback(viewClip, XtNreportCallback,cb_pageAdjustNotify,(XtPointer)NULL);

									n=0;
           XtSetArg(args[n], XtNconditionedResize,False);		n++;
   viewControl = XtCreateManagedWidget("viewControl", aaaWidgetClass,viewClip,args,n);

									n=0;
            XtSetArg(args[n], XtNautoSetup,False);			n++;
            XtSetArg(args[n], XtNinterpreter,gv_gs_interpreter);	n++;
            if (gv_gs_safer)  XtSetArg(args[n], XtNsafer,True);
            else              XtSetArg(args[n], XtNsafer,False);
                                                                        n++;
            if (gv_gs_quiet)  XtSetArg(args[n], XtNquiet,True);
            else              XtSetArg(args[n], XtNquiet,False);
                                                                        n++;
            XtSetArg(args[n], XtNarguments,gv_gs_arguments);            n++;
   page = XtCreateManagedWidget("page", ghostviewWidgetClass,viewControl, args,n);
            num_ghosts++;

            XtAddCallback(page, XtNcallback, cb_track, (XtPointer)NULL);
            XtAddCallback(page, XtNdestroyCallback, cb_destroyGhost, (XtPointer)page);
            XtAddCallback(page, XtNmessageCallback, cb_message, (XtPointer)page);
            XtAddCallback(page, XtNoutputCallback,cb_appendInfoPopup, (XtPointer)NULL);

									n=0;
            XtSetArg(args[n], XtNxdpi, &default_xdpi);			n++;
            XtSetArg(args[n], XtNydpi, &default_ydpi);			n++;
            XtGetValues(page, args, n);

/*### checking gv_filename and opening psfile #############################*/

    INFMESSAGE(checking gv_filename and opening psfile)
    if (gv_filename && strcmp(gv_filename, "-")) {
#ifdef VMS
       INFSMESSAGE(opening:,gv_filename)
       if ((psfile = fopen(gv_filename, "r", "mbc=100")) == NULL) {
          char *p;
          INFMESSAGE(failed)
          p=strrchr(gv_filename,']');
          if (!p) p=strrchr(gv_filename,':');
          if (!p) p=gv_filename;
          if (!strchr(p,'.')) {
             p = (char*) GV_XtMalloc(((unsigned)strlen(gv_filename)+4));
             sprintf(p,"%s.ps",gv_filename);
             GV_XtFree(gv_filename);
             gv_filename = p;

          }
          INFSMESSAGE(opening:,gv_filename)
          if ((psfile = fopen(gv_filename, "r", "mbc=100")) == NULL) {
             open_fail_error(errno,GV_ERROR_OPEN_FAIL,gv_filename,1);
             exit(EXIT_STATUS_ERROR);
          }
       }
#else
       INFSMESSAGE(opening:,gv_filename)
       if ((psfile = fopen(gv_filename,"r")) == NULL) {
          char *p;
          INFMESSAGE(failed)
          p = strrchr(gv_filename,'.');
          if (!p) p = gv_filename;
          if (p && strcmp(p,".ps")) {
             p = (char*) GV_XtMalloc(((unsigned)strlen(gv_filename)+4));
             sprintf(p,"%s.ps",gv_filename);
             GV_XtFree(gv_filename);
             gv_filename = p;
          }
          if ((psfile = fopen(gv_filename,"r")) == NULL) {
             fprintf(stderr, "  %s:  Cannot open ",gv_application_name);
             perror(gv_filename);
             exit(EXIT_STATUS_ERROR);
          }
       }
#endif
       stat(gv_filename, &sbuf);
       mtime = sbuf.st_mtime;
    }


/*### remaining initialization #####################################################*/

    INFMESSAGE(remaining initialization)

    gv_num_std_pagemedia = 0;
    while (papersizes[gv_num_std_pagemedia].name) gv_num_std_pagemedia++;

    GhostviewDisableInterpreter(page);

    gv_pagemedia = MEDIA_ID_INVALID;
    setup_ghostview();

    {
       int o;
       gv_orientation_old         = O_NONE;
       o = doc_convStringToDocOrient(app_res.default_orientation);
       if (o == O_AUTOMATIC) {
          gv_orientation_auto     = 1;
          gv_orientation_auto_old = 0;
          gv_orientation          = O_NONE;
       } else {
          gv_orientation_auto     = 0;
          gv_orientation_auto_old = 1;
          gv_orientation          = o;
       }
    }

    gv_fallback_orientation = doc_convStringToDocOrient(app_res.fallback_orientation);
    if (gv_fallback_orientation != O_PORTRAIT   && gv_fallback_orientation != O_LANDSCAPE &&
        gv_fallback_orientation != O_UPSIDEDOWN && gv_fallback_orientation != O_SEASCAPE)
        gv_fallback_orientation = O_PORTRAIT;

    gv_fallback_pagemedia   = doc_convStringToPageMedia(doc,app_res.fallback_pagemedia);
    if (gv_fallback_pagemedia == MEDIA_ID_INVALID) doc_convStringToPageMedia(doc,"A4");

    gv_swap_landscape_old = -1;
    if (app_res.swap_landscape) gv_swap_landscape = 1;
    else                        gv_swap_landscape = 0;

    gv_exiting = 0; 

    number = doc_convStringToPage(doc,app_res.page);
    number = doc_putPageInRange(doc,number);

    /* Coerce magstep to fall in range */
    if (app_res.magstep < app_res.minimum_magstep) app_res.magstep = app_res.minimum_magstep;
    if (app_res.magstep > app_res.maximum_magstep) app_res.magstep = app_res.maximum_magstep;

/*### managing the children ######################################################*/

    INFMESSAGE(managing children of control)
    XtManageChildren((WidgetList)cont_child,cont_child_num);
    INFMESSAGE(managing control)
    XtManageChild(control);
    XtSetMappedWhenManaged(toplevel, False);
    INFMESSAGE(realizing toplevel)
    XtRealizeWidget(toplevel);
    XSetWMProtocols(gv_display, XtWindow(toplevel), &wm_delete_window, 1);

/*### Creating the File Selection Popup ###########################################*/

    INFMESSAGE(creating file selection popup)
                              				n=0;
            XtSetArg(args[n], XtNallowShellResize,True);n++;
    FileSel_popup = XtCreatePopupShell("fileSelPopup",transientShellWidgetClass,toplevel,args,n);
                              				n=0;
            XtSetArg(args[n], XtNviewMode, XawFileSelectionRescan); n++;
            XtSetArg(args[n], XtNbuttons,  2);		n++;
            XtSetArg(args[n], XtNpreferredButton, 2);	n++;
            XtSetArg(args[n], XtNreverseScrolling,app_res.reverse_scrolling);n++;
            if (app_res.scratch_dir) {
               XtSetArg(args[n], XtNtmpDir, app_res.scratch_dir); n++;
            }
    FileSel = XtCreateManagedWidget("fileSel",file_selectionWidgetClass,FileSel_popup,args,n);
	XtAddCallback(XtNameToWidget(FileSel,"button1"), XtNcallback,cb_popdownPopup,FileSel_popup);
	XtAddCallback(XtNameToWidget(FileSel,"button1"), XtNcallback,cb_popdownNotePopup,(XtPointer)NULL);

    XtRealizeWidget(FileSel_popup);
    XSetWMProtocols(gv_display,XtWindow(FileSel_popup),&wm_delete_window,1);

/*### now we become visible ######################################################*/

    INFMESSAGE(switching off resize for buttons and labels)
							n=0;
	XtSetArg(args[0], XtNresize, False);		n++;
    if (show_toggleCurrentPage)	XtSetValues(w_toggleCurrentPage,args,n);
    if (show_toggleAllPages)	XtSetValues(w_toggleAllPages,args,n);
    if (show_toggleEvenPages)	XtSetValues(w_toggleEvenPages,args,n);
    if (show_toggleOddPages)	XtSetValues(w_toggleOddPages,args,n);
    if (show_unmarkAllPages)	XtSetValues(w_unmarkAllPages,args,n);
    if (show_saveMarkedPages)	XtSetValues(w_saveMarkedPages,args,n);
    if (show_printMarkedPages)	XtSetValues(w_printMarkedPages,args,n);
    if (show_printAllPages)	XtSetValues(w_printAllPages,args,n);
    if (show_openFile)		XtSetValues(w_openFile,args,n);
    if (show_autoResize)	XtSetValues(w_autoResize,args,n);
    if (show_showThisPage)	XtSetValues(w_showThisPage,args,n);
    if (show_updateFile)	XtSetValues(w_updateFile,args,n);
    if (show_checkFile)		XtSetValues(w_checkFile,args,n);
    if (show_locator) {
       				XtSetArg(args[n], XtNlabel,"");		n++;
				XtSetValues(locator,args,n);
    }

    process_menu(NULL,PROCESS_MENU_HIDE); /* hide the process button */

    setup_layout_ghostview();

    if (gv_filename) current_page=number;
    show_page(REQUEST_SETUP,NULL);

    /* must allow control to resize */
    AaaWidgetAllowResize((AaaWidget)control,True,True);

#ifdef USE_PIXMAP_CODE
#   define PMW 30
#   define PMH 30
						n=0;
          XtSetArg(args[0], XtNheight, PMW);	n++;
          XtSetArg(args[1], XtNwidth, PMH);	n++;
          XtSetArg(args[2], XtNlabel, "");	n++;
				XtSetValues(w_printAllPages,args,n);
				XtSetValues(w_printMarkedPages,args,n);
				XtSetValues(w_saveAllPages,args,n);
				XtSetValues(w_saveMarkedPages,args,n);
#endif

    INFMESSAGE(mapping toplevel)
    XtMapWidget(toplevel);
  }

  INFMESSAGE(waiting for events now)
  XtAppMainLoop(app_con);

  /* should never get here */
  return 1;
}

/*-----------------------------------------------------------
   output_message
-----------------------------------------------------------*/

#define MAX_RECORD_LENGTH 256

static void
output_message_line(fd,line)
   FILE *fd;
   char *line;
{
   if (line[0]!='!') {
      if      (line[0]=='C')  fprintf(fd,&(line[1]),gv_class);
      else if (line[0]=='N')  fprintf(fd,&(line[1]),gv_name);
      else if (line[0]=='A')  fprintf(fd,&(line[1]),gv_application_name);
      else if (line[0]=='V')  fprintf(fd,&(line[1]),versionIdentification[0]);
      else if (line[0]=='\n') fprintf(fd,line);
      else                    fprintf(fd,&(line[1]));
      if (!strrchr(line,'\n')) fprintf(fd,"\n");
   }
}

static void
output_message(dpy,identifier,go_back)
   Display *dpy;
   char *identifier;
   Boolean go_back;
{
   FILE *helpfile;
   char *helpfilename;
   char line[MAX_RECORD_LENGTH];
   int  show=0;

   helpfilename = GetPseudoResource(dpy,"messageFile","MessageFile");
   helpfile= NULL;
   if (helpfilename) {
      if ((helpfile=fopen(helpfilename,"r"))==NULL) {
            open_fail_error(errno,GV_ERROR_OPEN_FAIL,helpfilename,1);
            show=3;
      }
   }

   if (helpfile) {
      while (show<2 && fgets(line,MAX_RECORD_LENGTH,helpfile)) {
         if (line[0]=='#' && strstr(line,identifier)) ++show;
         else if (show==1) output_message_line(stderr,line);
      }
      fclose(helpfile);
   }
  
#  ifdef FALLBACK_MESSAGES
      if (show==0) {
         char *linep;
         int  n=0;
         while (show<2 && (linep=fallback_messages[n++])) {
            if (linep[0]=='#' && strstr(linep,identifier)) ++show;
            else if (show==1) output_message_line(stderr,linep);
         }
      }
#  endif

   if (show==0)
      fprintf(stderr,"  %s: No message text for \"%s\"\n",
              gv_application_name,identifier);
   if (go_back && show<3) return;
   fclose(psfile);
   XtDestroyApplicationContext(app_con);
   exit(EXIT_STATUS_ERROR);
}   

/*---------------------------------------------------------------
    main_mergeFileIntoDatabase
---------------------------------------------------------------*/

static char* main_mergeFileIntoDatabase(dpy,name,allocate)
   Display *dpy;
   char *name;
   int allocate;
{
   XrmDatabase db=XrmGetDatabase(dpy);
   char tmp[GV_MAX_FILENAME_LENGTH];
   int useful=0;

   BEGINMESSAGE(main_mergeFileIntoDatabase)

   if (name) {
      strcpy(tmp,name);
      file_translateTildeInPath(tmp);

      if (file_fileIsNotUseful(tmp)) {
         INFSMESSAGE(not useful,tmp)
         if (name != file_locateFilename(name)) useful=-1;
         else {
#           ifdef VMS
               sprintf(tmp,"DECW$USER_DEFAULTS:%s",name);
#           else
               sprintf(tmp,"~/%s",name);
               file_translateTildeInPath(tmp);
#           endif
         }
      } else useful=1;

      if (!useful) {
#ifdef VMS
         if (file_fileIsNotUseful(tmp)) {
            INFSMESSAGE(not useful,tmp)
            sprintf(tmp,"SYS$LOGIN:%s",name);
         } else useful=1;     
#endif
         if (!useful && file_fileIsNotUseful(tmp)) {
            INFSMESSAGE(not useful,tmp)
            sprintf(tmp,"%s%s",GV_LIB,name);
         } else useful=1;

         if (!useful && file_fileIsNotUseful(tmp)) {
            INFSMESSAGE(not useful,tmp)
            INFMESSAGE(giving up)
         } else useful=1;
      }

      if (useful==1) {
         INFSMESSAGE(merging,tmp)
         XrmCombineFileDatabase(tmp,&db,True);
         if (allocate) name = GV_XtNewString(tmp);
      }
      else name=NULL;
   }
   ENDMESSAGE(main_mergeFileIntoDatabase)
   return(name);
}

/*-------------------------------------------------------
    GetPseudoResource
-------------------------------------------------------*/

static char *GetPseudoResource(dpy,name_val,class_val)
   Display *dpy;
   char *name_val;
   char *class_val;
{
   char *result=NULL;
   XrmDatabase db=XrmGetDatabase(dpy);

   BEGINMESSAGE(GetPseudoResource)
   if (db) {
      XrmValue rm_value;
      String str_type;
      char str_name[GV_MAX_FILENAME_LENGTH],str_class[GV_MAX_FILENAME_LENGTH];
      Bool success=False;
     
      sprintf(str_name, "%s.%s",gv_name,name_val);
      sprintf(str_class,"%s.%s",gv_class,class_val);
      INFSMESSAGE(looking for,str_class)
      success = XrmGetResource(db,str_name,str_class,&str_type,&rm_value);
      if  (success && rm_value.size>1) { result = (char*) rm_value.addr; INFSMESSAGE(found entry,result) }
      else                             { result = NULL;                  INFMESSAGE(no entry found)      }
   }
   ENDMESSAGE(GetPseudoResource)
   return(result);
}

/*--------------------------------------------------
    GetInternResource
--------------------------------------------------*/

static char *GetInternResource(dpy,name)
   Display *dpy;
   char *name;
{
   char *result=NULL;
   XrmDatabase db=XrmGetDatabase(dpy);

   BEGINMESSAGE(GetInternResource)
   if (db) {
      XrmValue rm_value;
      char str_name[GV_MAX_FILENAME_LENGTH];
      String str_type;
      Bool success=False;
  
      sprintf(str_name, "%s.%s",gv_intern_name,name);
      INFSMESSAGE(looking for,str_name)
      success = XrmGetResource(db,str_name,str_name,&str_type,&rm_value);
      if  (success) { result = (char*) rm_value.addr; INFSMESSAGE(found entry,result) }
      else                             {
          INFMESSAGE(no entry found)
          fprintf(stderr, "  %s: Error, cannot get value for resource \"%s\"\n",gv_application_name,str_name);
          exit(EXIT_STATUS_ERROR);
      }
   }
   ENDMESSAGE(GetInternResource)
   return(result);
}

