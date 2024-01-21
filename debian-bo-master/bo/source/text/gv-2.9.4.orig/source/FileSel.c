/*
**
** FileSel.c
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
** Author:   Johannes Plass (plass@thep.physik.uni-mainz.de)*
**           Department of Physics
**           Johannes Gutenberg-University
**           Mainz, Germany
**
*/

/*
#define MESSAGES
*/
#include "message.h"

#include "config.h"

#include <stdlib.h> /* for malloc etc.*/
#include <ctype.h>  /* for toupper    */
#include <stdio.h>

#ifdef VMS
#   include <unixio.h> /* for chdir etc. */
#else
#   include <sys/types.h>
#   include <sys/stat.h>
#   include <dirent.h>
#   include <unistd.h>
    /* Damn it, don't ever use getwd with stupid linux ###jp### */
#   define getwd(aaa) getcwd((aaa),(FS_MAXNAMLEN-2))    
#endif

#include "paths.h"
#include INC_X11(Xlib.h)
#include INC_X11(Xos.h)
#include INC_X11(IntrinsicP.h)
#include INC_X11(StringDefs.h)
#include INC_XMU(Misc.h)
#include INC_XMU(CharSet.h)		/* for XmuCompareISOLatin1() */
#include INC_XAW(XawInit.h)
#include INC_XAW(Command.h)
#include INC_XAW(AsciiText.h)
#include "Clip.h"
#include "Frame.h"
#include "FileSelP.h"

#include "d_memdebug.h"

/*
#define USE_LISTC_WIDGET
*/
#ifdef USE_LISTC_WIDGET
#   include "Listc.h"
#   define LISTWIDGETCLASS  listcWidgetClass
#   define LISTWIDGET       ListcWidget
#   define XAWLISTCHANGE    XawListcChange
#else
#   include INC_XAW(List.h)
#   define LISTWIDGETCLASS  listWidgetClass
#   define LISTWIDGET       ListWidget
#   define XAWLISTCHANGE    XawListChange
#endif

/*####################################################################
   OS dependant Definitions
####################################################################*/

#ifdef VMS
#   define FS_MAXNAMLEN 255
#   define DIR_SEPARATOR_STRING "."
#   define DIR_SPECIFICATION_START_STRING "["
#   define DIR_SPECIFICATION_END_STRING "]"

#   define CHANGE_TO_HEAD_OF_DIR_SPEC(path) {				\
              char *locat;					        \
              if (!(locat = strrchr(path,'['))) {                       \
                 if (!(locat = strrchr(path,':')+1)) {			\
                    INFMESSAGE(unable to extract node/disk information)	\
                    ENDMESSAGE(TopDirSelectionProc)			\
                    return;						\
                 }							\
              }								\
              else *(locat)='\0';					\
           }

#   define ONE_STEP_UP "[-]"		/* chdir argument to make one step up in the directory tree. */
#   define HOME getenv("SYS$LOGIN")	/* chdir argument to go home (the login directory) */
#else /*################################ VMS ############################*/

#   define FS_MAXNAMLEN 255
#   define DIR_SEPARATOR_STRING "/"
#   define DIR_SPECIFICATION_START_STRING ""
#   define DIR_SPECIFICATION_END_STRING ""

#   define CHANGE_TO_HEAD_OF_DIR_SPEC(path) {	\
              char*p=path;			\
              *p='/'; p++; *p='\0';		\
           }

#   define ONE_STEP_UP ".."		/* chdir argument to make one step up in the directory tree. */
#   define HOME getenv("HOME")	        /* chdir argument to go home (the login directory) */
#endif

/*####################################################################
   Initializations
####################################################################*/

static String emptyList[] = {NULL};
static String unknownList[] = {"<unknown>",NULL};
static String cannotopenList[] = {"<cannot open>",NULL};

static void FS_textfieldFocusAction();
static void FS_textfieldDeleteAction();
static void FS_listAction();
static void FS_preferButtonAction();

static XtActionsRec file_selectionActionsTable[] = {
       { "FS_textfieldFocusAction",  (XtActionProc) FS_textfieldFocusAction },
       { "FS_textfieldDeleteAction", (XtActionProc) FS_textfieldDeleteAction },
       { "FS_preferButton",          (XtActionProc) FS_preferButtonAction },
       { "List",      (XtActionProc) FS_listAction }
};

static String list_translations =
"#replace\n\
~Button2 <Btn1Down>:	List(set) List(start-move)\n\
~Button2 <Btn1Motion>:	List(move)\n\
~Button2 <Btn1Up>:	List(notify) List(stop-move)\n\
~Button1 <Btn2Down>:	List(set) List(start-move)\n\
	 <Btn2Motion>:	List(move,0,2.0)\n\
~Button1 <Btn2Up>:	List(page) List(stop-move)\
";

static String curlist_translations =
"#replace\n\
~Button2 <Btn1Down>:	List(start-move)\n\
~Button2 <Btn1Motion>:	List(move)\n\
~Button2 <Btn1Up>:	List(set) List(notify) List(stop-move)\n\
~Button1 <Btn2Down>:	List(start-move)\n\
	 <Btn2Motion>:	List(move,0,2.0)\n\
~Button1 <Btn2Up>:	List(page) List(stop-move)\
";

static String TextField_translations =
"#override\n\
<Key>Down:	no-op()\n\
<Key>Up:	no-op()\n\
<Key>Linefeed: 	no-op()\n\
<Key>space: 	no-op()\n\
Ctrl<Key>J: 	no-op()\n\
Ctrl<Key>M: 	no-op()\n\
Ctrl<Key>N: 	no-op()\n\
Ctrl<Key>O: 	no-op()\n\
Ctrl<Key>P: 	no-op()\n\
Ctrl<Key>R: 	no-op()\n\
Ctrl<Key>S: 	no-op()\n\
Ctrl<Key>V: 	no-op()\n\
Ctrl<Key>Z: 	no-op()\n\
Meta<Key>V: 	no-op()\n\
Meta<Key>Z: 	no-op()\n\
<Key>BackSpace: FS_textfieldDeleteAction()\n\
<Key>Delete: 	FS_textfieldDeleteAction()\n\
<Key>Right: 	forward-character()\n\
<Key>Left: 	backward-character()\n\
<Key>Return: 	no-op()\n\
<Key>Tab: 	FS_preferButton(next)\n\
<Btn1Down>:	FS_textfieldFocusAction() select-start()\n\
<Btn3Down>:	FS_textfieldFocusAction() extend-start()\
"; 

static String TextField_accelerators =
"#override\n\
<Key>Return:	set() notify() unset()\
";

#if 0
#define FILE_SELECTION_LAYOUT \
"\
"
#endif

/*####################################################################
   Macros and Definitions
####################################################################*/

/* general Xt Macros */

#ifdef MIN
#   undef MIN
#endif
#define MIN(_a_,_b_) (_a_)<(_b_)?(_a_):(_b_)
#ifdef MAX
#   undef MAX
#endif
#define MAX(_a_,_b_) (_a_)>(_b_)?(_a_):(_b_)

#define USE_Arg(num)  Arg args[num]; Cardinal argn = 0

#define ADD_Callback(widget,proc) XtAddCallback((Widget)(widget),XtNcallback,(proc),NULL)
#define ADD_Callback_Data(widget,proc,data) XtAddCallback((Widget)(widget),XtNcallback,(proc),(data))
#define ADD_Widget(name,class,parent) XtCreateManagedWidget((name),class,(Widget)(parent),NULL,(Cardinal)0)  
#define ADD_PopupShell(name,class,parent) XtCreatePopupShell((name),class,(Widget)(parent),NULL,(Cardinal)0)

#define ADD_Widget_Arg(name,class,parent) XtCreateManagedWidget((name),class,(parent),args,argn)
#define RESET_Arg argn=0
#define GOT_Arg   (argn)
#define SET_Arg(name,value)		\
	   XtSetArg(args[argn],(name),(value));	argn++

#define SET_Values(widget)   XtSetValues((widget),args,argn)
#define SET_Value(widget,name,value)	\
           RESET_Arg;			\
           SET_Arg((name),(value));	\
           SET_Values((widget))

#define GET_Values(widget)   XtGetValues((Widget)(widget),args,argn)
#define GET_Value(widget,name,value)	\
           RESET_Arg;			\
           SET_Arg((name),(value));	\
           GET_Values((widget))

#define streq(a,b) (strcmp((a),(b))==0)
#define resource(name) (appResources->name)

/* FileSelection specific Macros */

#define FS_WIDGET		FileSelectionWidget fs = (FileSelectionWidget)
#define FS_FILE_SELECTION       fs

#define FS_HOMEBUTTON		fs->file_selection.homebuttonFS
#define FS_TMPBUTTON		fs->file_selection.tmpbuttonFS
#define FS_RESCANBUTTON 	fs->file_selection.rescanbuttonFS
#define FS_FILTERBUTTON		fs->file_selection.filterbuttonFS
#define FS_BUTTON1		fs->file_selection.button1FS
#define FS_BUTTON2		fs->file_selection.button2FS
#define FS_BUTTON3		fs->file_selection.button3FS
#define FS_BUTTON4		fs->file_selection.button4FS

#define FS_PATHFRAME		fs->file_selection.pathframeFS
#define FS_PATH			fs->file_selection.pathFS
#define FS_FILTERFRAME		fs->file_selection.filterframeFS
#define FS_FILTER		fs->file_selection.filterFS
#define FS_OLD_TEXTFIELD	fs->file_selection.old_textfieldFS
#define FS_TOPFRAME		fs->file_selection.topframeFS
#define FS_TOPCLIP		fs->file_selection.topclipFS
#define FS_TOPAAA		fs->file_selection.topaaaFS
#define FS_TOPLIST		fs->file_selection.toplistFS
#define FS_CURFRAME		fs->file_selection.curframeFS
#define FS_CURCLIP		fs->file_selection.curclipFS
#define FS_CURAAA		fs->file_selection.curaaaFS
#define FS_CURLIST		fs->file_selection.curlistFS
#define FS_SUBFRAME		fs->file_selection.subframeFS
#define FS_SUBCLIP		fs->file_selection.subclipFS
#define FS_SUBAAA		fs->file_selection.subaaaFS
#define FS_SUBLIST		fs->file_selection.sublistFS

#define TOPDIR			fs->file_selection.topdir
#define TOPDIR_ALLOC		fs->file_selection.topdir_alloc
#define TOPDIR_ENTRIES  	fs->file_selection.topdir.num_of_entries
#define TOPDIR_ENTRY(num) 	fs->file_selection.topdir.entry[(num)]
#define TOPDIR_LIST		fs->file_selection.topdir.entry

#define CURDIR			fs->file_selection.curdir
#define CURDIR_ALLOC		fs->file_selection.curdir_alloc
#define CURDIR_ENTRIES  	fs->file_selection.curdir.num_of_entries
#define CURDIR_ENTRY(num) 	fs->file_selection.curdir.entry[(num)]
#define CURDIR_LIST		fs->file_selection.curdir.entry

#define SUBDIR			fs->file_selection.subdir
#define SUBDIR_ALLOC		fs->file_selection.subdir_alloc
#define SUBDIR_ENTRIES  	fs->file_selection.subdir.num_of_entries
#define SUBDIR_ENTRY(num) 	fs->file_selection.subdir.entry[(num)]
#define SUBDIR_LIST		fs->file_selection.subdir.entry

#define PATH_RESOURCE		fs->file_selection.path
#define FILTER_RESOURCE		fs->file_selection.filter
#define TMP_DIR_RESOURCE	fs->file_selection.tmp_dir
#define VIEWMODE		fs->file_selection.view_mode
#define HIGHLIGHT		fs->file_selection.highlightPixel
#define OLD_HIGHLIGHT		fs->file_selection.old_highlightPixel

#define PATH			fs->file_selection.path_field_value
#define FILTER			fs->file_selection.filter_field_value
#define BOTH			fs->file_selection.both
#define APP_DIR			fs->file_selection.app_dir
#define CURRENT_DIR             fs->file_selection.current_dir

#define BUTTONS            	fs->file_selection.internal_buttons
#define BUTTONS_RESOURCE        fs->file_selection.buttons
#define PREFERRED_BUTTON	fs->file_selection.preferredButton

#define REVERSE_SCROLLING	fs->file_selection.reverse_scrolling

#define REALLOC_MORE_IF_NEEDED(list,needed,current) 					\
    if (needed >= current) {								\
       current *= 2;									\
       list = (String *) FS_XtRealloc((char *) list,(unsigned)(current*sizeof(String)));	\
    }
#define ALLOC_LIST(list,needed) 							\
    list = (String *) FS_XtMalloc((unsigned)(needed* sizeof(String)))

#define POSITION(pos) ((pos==1)+2*(pos==2)+4*(pos==3)+8*(pos==4)) 
#define IS_BUTTON(pos) (POSITION(pos) & BUTTONS) 
#define POSITION_TO_BUTTON_NAME(pos,name) sprintf((name),"button%d",(int)(pos))

#define MULTICLICK_INTERVAL ((unsigned long) 400)
#define DISABLED        ((XtIntervalId) 0)
#define MULTICLICK      fs->file_selection.multiclick
#define ENABLE_MULTICLICK                                   		\
    MULTICLICK = XtAppAddTimeOut(                       		\
                    XtWidgetToApplicationContext((Widget)FS_FILE_SELECTION),\
                    MULTICLICK_INTERVAL,                		\
                    MulticlickNotify,					\
                    ((XtPointer)FS_FILE_SELECTION)			\
                 )

#define DESTROY_MULTICLICK		\
    if (MULTICLICK) {			\
       XtRemoveTimeOut(MULTICLICK);	\
       MULTICLICK = DISABLED;		\
    }

#define offset(field) XtOffsetOf(FileSelectionRec, file_selection.field)
#define lay_offset(field) XtOffsetOf(FileSelectionRec, aaa.field)

static XtResource resources[] = {
    {XtNpath,XtCPath,XtRString,sizeof(String),offset(path),XtRImmediate,(XtPointer)NULL},
    {XtNtmpDir,XtCTmpDir,XtRString,sizeof(String),offset(tmp_dir),XtRImmediate,(XtPointer)NULL},
    {XtNfilter,XtCFilter,XtRString,sizeof(String),offset(filter),XtRImmediate,(XtPointer)NULL},
    {XtNviewMode, XtCViewMode, XtRViewMode,sizeof(int),offset(view_mode),XtRImmediate,(XtPointer)XawFileSelectionRescan},
    {XtNbuttons,XtCButtons,XtRInt,sizeof(int),offset(buttons),XtRImmediate,(XtPointer)0},
    {XtNpreferredButton,XtCPreferredButton,XtRInt,sizeof(int),offset(preferredButton),XtRImmediate,(XtPointer)0},
    {XtNhighlightPixel, XtCHighlightPixel, XtRPixel, sizeof(Pixel),offset(highlightPixel), XtRImmediate, (XtPointer)NULL}, 
#if 0
    {XtNlayout, XtCLayout, XtRLayout, sizeof(BoxPtr),lay_offset(layout),XtRString,FILE_SELECTION_LAYOUT},
#endif
    {XtNlayout, XtCLayout, XtRLayout, sizeof(BoxPtr),lay_offset(layout),XtRLayout,NULL},
    {XtNresizeWidth,  XtCBoolean, XtRBoolean, sizeof(Boolean),lay_offset(resize_width),XtRBoolean,False},
    {XtNresizeHeight, XtCBoolean, XtRBoolean, sizeof(Boolean),lay_offset(resize_height),XtRBoolean,False},
};
#undef offset
#undef lay_offset

static Boolean SetValues();
static void ClassInitialize(), Initialize(), Realize(), Resize(), Destroy();
static void filterProc(),rescanProc(),homeProc();
static void TopDirSelectionProc(), CurDirSelectionProc(), SubDirSelectionProc();
static void AdjustListSizes();
static XtActionProc listSetAction=NULL;
static XtActionProc listUnsetAction=NULL;
static XtActionProc listNotifyAction=NULL;

FileSelectionClassRec file_selectionClassRec = {
  {
/* core class fields */
    /* superclass         */   (WidgetClass) (&aaaClassRec),
    /* class name         */   "FileSelection",
    /* size               */   sizeof(FileSelectionRec),
    /* class_initialize   */   ClassInitialize,
    /* class_part init    */   NULL,
    /* class_inited       */   FALSE,
    /* initialize         */   Initialize,
    /* initialize_hook    */   NULL,
    /* realize            */   Realize,
    /* actions            */   file_selectionActionsTable,
    /* num_actions        */   XtNumber(file_selectionActionsTable),
    /* resources          */   resources,
    /* resource_count     */   XtNumber(resources),
    /* xrm_class          */   NULLQUARK,
#if defined(VMS) || defined(linux) || defined(SYSV) || defined(SVR4)
    /* compress_motion    */   0,
    /* compress_exposure  */   0,
    /* compress_enterleave*/   0,
#else
    /* compress_motion    */   NULL,
    /* compress_exposure  */   NULL,
    /* compress_enterleave*/   NULL,
#endif
    /* visible_interest   */   FALSE,
    /* destroy            */   Destroy,
    /* resize             */   Resize,
    /* expose             */   NULL,
    /* set_values         */   SetValues,
    /* set_values_hook    */   NULL,
    /* set_values_almost  */   XtInheritSetValuesAlmost,
    /* get_values_hook    */   NULL,
    /* accept_focus       */   NULL,
    /* version            */   XtVersion,
    /* callback_private   */   NULL,
    /* tm_table           */   NULL,
    /* query_geometry     */   XtInheritQueryGeometry,
    /* display_accelerator*/   XtInheritDisplayAccelerator,
    /* extension          */   NULL
   }, 
   {
/* composite class fields */
    /* geometry_manager   */   XtInheritGeometryManager,
    /* change_managed     */   XtInheritChangeManaged,
    /* insert_child       */   XtInheritInsertChild,
    /* delete_child       */   XtInheritDeleteChild,
    /* extension          */   NULL
   }, 
   {
/* constraint class fields */
    /* subresources       */   NULL,
    /* subresource_count  */   0,
    /* constraint_size    */   sizeof(FileSelectionConstraintsRec),
    /* initialize         */   NULL,
    /* destroy            */   NULL,
    /* set_values         */   NULL,
    /* extension          */   NULL
   },
  { 
/* aaa class fields */
    /* foo                */   0
  },
  { 
/* file selection class fields */
    /* empty              */   0
  }  
};

WidgetClass file_selectionWidgetClass = (WidgetClass) &file_selectionClassRec;

/*-------------------------------------------------------------------------------
   CvtStringToViewMode
-------------------------------------------------------------------------------*/

static Boolean
CvtStringToViewMode (dpy, args, num_args, from, to, converter_data)
    Display     *dpy;
    XrmValue    *args;
    Cardinal    *num_args;
    XrmValue    *from, *to;
    XtPointer   *converter_data;
{
    char 	*in = (char *) from->addr;
    int 	value;
    
    BEGINMESSAGE(CvtStringToViewMode)
    INFSMESSAGE(trying to convert:,in)
    
    if      (streq(in,"rescan")) value = XawFileSelectionRescan;
    else if (streq(in,"filter")) value = XawFileSelectionFilter;
    else {
       String params[1];
       Cardinal num_params =1;
       params[0] = (String) from->addr;
       XtAppWarningMsg(
          XtDisplayToApplicationContext(dpy),
          "CvtStringToViewMode",
          "UnknownParameter",
          "XawFileSelectionWidget",
          "Cannot translate \"%s\" to ViewMode.",
          params,
          &num_params
       );
       INFMESSAGE(failed - unknown parameter)
       ENDMESSAGE(CvtStringToViewMode) 
       return False;
    }
 
    if (to->addr != NULL) {
       if (to->size < sizeof(int)) {
          to->size = sizeof(int);
          INFMESSAGE(failed - not enough memory allocated)
          ENDMESSAGE(CvtStringToViewMode)
          return False;
       }
       *(int *)(to->addr) = value;
    }
    else {
       static int static_val;
       static_val = value;
       to->addr = (caddr_t) &static_val;
    }
    to->size = sizeof(int);
    
    INFMESSAGE(succesfull)
    ENDMESSAGE(CvtStringToViewMode)
    return True;
}

/*-------------------------------------------------------------------------------
   FreeViewMode
-------------------------------------------------------------------------------*/

static void
FreeViewMode (app, to, data, args, num_args)
    XtAppContext    app;
    XrmValue        *to;
    XtPointer       data;
    XrmValuePtr     args;
    Cardinal        *num_args;
{
    BEGINMESSAGE(FreeViewMode)
    ENDMESSAGE(FreeViewMode)
}

/*-------------------------------------------------------------------------------
   ClassInitialize
-------------------------------------------------------------------------------*/

static void 
ClassInitialize()
{
   BEGINMESSAGE(ClassInitialize)
   XawInitializeWidgetSet();
   XtSetTypeConverter ( XtRString, XtRViewMode,
                        CvtStringToViewMode,(XtConvertArgList)NULL, (Cardinal)0,
                        XtCacheNone, 
                        FreeViewMode
                      );
   ENDMESSAGE(ClassInitialize)
}

/*-------------------------------------------------------------------------------
   Realize
-------------------------------------------------------------------------------*/

static void Realize (w, valueMask, attrs)
    Widget w;
    XtValueMask *valueMask;
    XSetWindowAttributes *attrs;
{
   FS_WIDGET w;
   BEGINMESSAGE(Realize)
   (*file_selectionWidgetClass->core_class.superclass->core_class.realize)(w, valueMask, attrs);
   AdjustListSizes(w);
   FS_textfieldFocusAction(FS_PATH, NULL, NULL, NULL);
   ENDMESSAGE(Realize)
}

/*-------------------------------------------------------------------------------
   Resize
-------------------------------------------------------------------------------*/

static void 
Resize(w)
   Widget w;
{
   BEGINMESSAGE(Resize)
   (*file_selectionWidgetClass->core_class.superclass->core_class.resize)(w);
/*
   if (XtIsRealized(w)) AdjustListSizes(w);
*/
   ENDMESSAGE(Resize)
}   

/*-------------------------------------------------------------------------------
   strwild
-------------------------------------------------------------------------------*/

static Boolean strwild(string,wild)
   char 	*string, *wild;
{
   char 	*cwild;
   int 		nwild = 0;
   int 		wildlen;

   INFMESSAGE1(executing strwild)

   strcpy((cwild=malloc(strlen(wild)+1)),wild);
   wild = cwild; while ((wild=strchr(wild,'*'))) { ++nwild;  *wild++ = '\0'; } wild=cwild;
   wildlen=strlen(wild);

   if ((wildlen) && ((strncmp(string,wild,wildlen)) || ((!nwild) && (*(string += wildlen))))) {
         free(cwild); return(FALSE); 
   }
   wild += (wildlen+1);
   
   while (nwild) {
      wildlen=strlen(wild);
      if ((wildlen) && ((!(string = strstr(string,wild))) || ((nwild==1) && (*(string += wildlen))))) { 
         free(cwild); return(FALSE); 
      }
      wild += (wildlen+1);
      --nwild;
   }
   free(cwild);
   return(TRUE);
}

#ifdef VMS
/*-------------------------------------------------------------------------------
   strreplace
-------------------------------------------------------------------------------*/

static void
strreplace(out,find,replace,in)
   char *out;
   char *find;
   char *replace;
   char *in;
{
   int locat = 0;
   int findlength;
   char *intemp;
   char *temp;

   INFMESSAGE1(executing strreplace)

   findlength = strlen(find);
   if (!(*in) || !(*find)) return;

   intemp = malloc(strlen(in)+1);
   strcpy(intemp,in);

   temp=intemp;
   while ((temp=strstr(temp,find))) { *temp='\0'; temp += findlength; ++locat; }

   temp=intemp; *out = '\0';
   while ((locat--) > 0) {
      strcat(out,temp); strcat(out,replace);
      temp = strchr(temp,'\0') + findlength;
   }
   strcat(out,temp);
   free(intemp);
}  
#endif /* end of VMS */

/*----------------------------------------------------------------------
   appendDirEndSeparator
----------------------------------------------------------------------*/

static void
appendDirEndSeparator(path)
   char *path;
{
   size_t l=0;
   BEGINMESSAGE(appendDirEndSeparator)
   INFSMESSAGE(old:,path)
   if (path) l=strlen(path);
#ifdef VMS
   if (l && l<=(FS_MAXNAMLEN-2) && path[l-1] != ']' && path[l-1] != ':') {
      char* bra=strchr(path,'[');
      char* col=strchr(path,':');
      if (!col && !bra) strcat(path,":");
      else strcat(path,"]");
   }
#else
   if (l && l<=(FS_MAXNAMLEN-2) && path[l-1] != '/') strcat(path,"/");   
#endif
   INFSMESSAGE(new:,path);
   ENDMESSAGE(appendDirEndSeparator)
}

/*----------------------------------------------------------------------
   appendPathElement
----------------------------------------------------------------------*/

static void
appendPathElement(path,element)
   char *path;
   char *element;
{
   size_t lp=0,le=0;
   BEGINMESSAGE(appendPathElement)
   INFSMESSAGE(old:,path)
   if (path) {
      appendDirEndSeparator(path);
      lp=strlen(path);
   }
   if (element) le=strlen(element);
   if (lp && le && le+lp<=(FS_MAXNAMLEN-3)) {
#     ifdef VMS
         if (path[lp-1]==']') path[lp-1]='.';
         else { strcat(path,"["); ++lp; }
#     endif
      strcat(path,element);
      appendDirEndSeparator(path);
   }
   INFSMESSAGE(new:,path)
   ENDMESSAGE(appendPathElement)
}

/*-------------------------------------------------------------------------------
   translateTildeInPath
-------------------------------------------------------------------------------*/

static void
translateTildeInPath(path)
   char *path;
{
   char *pos;

   BEGINMESSAGE(translateTildeInPath)
   INFSMESSAGE(old,path)
   if (path && (pos=strchr(path,'~'))) {
      char *home;
      char tmp[FS_MAXNAMLEN];
#ifdef VMS
      home=getenv("SYS$LOGIN");
#else
      home=getenv("HOME");
#endif
      if (home && strlen(home)+strlen(path) < FS_MAXNAMLEN-1) {
         *pos='\0'; pos++;
         strcpy(tmp,path);
         strcat(tmp,home);
         strcat(tmp,pos);
         strcpy(path,tmp);
      }
   }
   INFSMESSAGE(new,path)
   ENDMESSAGE(translateTildeInPath)
}

/*-------------------------------------------------------------------------------
   FScompareEntries
-------------------------------------------------------------------------------*/

static int FScompareEntries(a, b)
  const void *a;
  const void *b;
{
  String *p = (String*) a;
  String *q = (String*) b;

# ifdef VMS /*versions should be sorted correctly (1.11.94)*/
    char *vp,*vq;
    int result;
    vq=strrchr(*q,';');
    vp=strrchr(*p,';');
    if ((vq) && (vp)) {
      *vp='\0'; *vq='\0';
      result=strcmp(*p,*q);
      if (!result) result = strlen(vq+1)-strlen(vp+1);
      if (!result) result = -strcmp(vp+1,vq+1);
      *vp=';'; *vq=';';
      return result;
    } else {
      return strcmp(*p,*q);
    }
# else
    return strcmp(*p,*q);
# endif
}

/*-------------------------------------------------------------------------------
   SetPreferredButton
-------------------------------------------------------------------------------*/

static void preferButton(w,prefer)
   Widget w;
{
   BEGINMESSAGE(preferButton)
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
   ENDMESSAGE(preferButton)
}

static void SetPreferredButton(w,position,install)
   Widget	w;
   int 		position;
   int		install;
{
   FS_WIDGET 	w;
   char 	name[10];
   Widget 	button;
   static XtAccelerators accelerators = (XtAccelerators)NULL;
   USE_Arg(2);

   BEGINMESSAGE(SetPreferredButton)
   if (!accelerators) accelerators=XtParseAcceleratorTable(TextField_accelerators);

   IMESSAGE(position)
   if (IS_BUTTON(position)) {
      POSITION_TO_BUTTON_NAME(position,name);
      button = XtNameToWidget((Widget)FS_FILE_SELECTION,name);
      if (!install) { 
         SET_Value(button,XtNaccelerators,(XtAccelerators)NULL);
         preferButton(button,0);
         if (PREFERRED_BUTTON==position) PREFERRED_BUTTON=0;
      } else {
         SET_Value(button,XtNaccelerators,(XtAccelerators)accelerators);
         XtInstallAccelerators(FS_PATH,button);
         XtInstallAccelerators(FS_FILTER,button);
         preferButton(button,1);
         PREFERRED_BUTTON=position;
      }
      IMESSAGE(PREFERRED_BUTTON)
   }
   ENDMESSAGE(SetPreferredButton)
}

/*-------------------------------------------------------------------------------
    FS_preferButtonAction
-------------------------------------------------------------------------------*/

static void FS_preferButtonAction(w, event, params, nparams)
   Widget	w;
   XEvent	*event;
   String	*params;
   Cardinal	*nparams;
{
   FileSelectionWidget FS_FILE_SELECTION;

   BEGINMESSAGE(FS_preferButtonAction)

   if (!w || !params || !nparams || !(*nparams)) {
      INFMESSAGE(illegal call)
      ENDMESSAGE(FS_preferButtonAction)
      return;
   }

   while (w && XtClass(w) != file_selectionWidgetClass) w = XtParent(w);
   if (!w) {
      INFMESSAGE(could not find file selection widget)
      ENDMESSAGE(FS_preferButtonAction)
      return;
   }
   FS_FILE_SELECTION = (FileSelectionWidget)w;

   if (streq(params[0],"next")) {
      int old,new;
      INFMESSAGE(next)
      new=old=PREFERRED_BUTTON;
      INFIMESSAGE(old preferred button:,old)
      do {
         ++new; if (new>4) new=1;
      } while (new!=old && !IS_BUTTON(new));
      if (new != old) {
         SetPreferredButton(w,old,FALSE);
         SetPreferredButton(w,new,TRUE);
      }
      INFIMESSAGE(new preferred button:,new)
   }
   ENDMESSAGE(FS_preferButtonAction)
}


/*-------------------------------------------------------------------------------
    FS_listAction
-------------------------------------------------------------------------------*/

static int buttonEventIsUseful(event,moving,posix,posiy,firstposx,firstposy)
   XEvent *event;
   int moving;
   int posix;
   int posiy;
   int firstposx;
   int firstposy;
{
   int useful=1;
   BEGINMESSAGE(buttonEventIsUseful)
   if (event->type != ButtonPress && event->type != ButtonRelease) 
      useful=0;
   else {
      int x,y;
      x = (int) event->xbutton.x_root;
      y = (int) event->xbutton.y_root;
      if (moving) {
         if ((posix>1 || abs(x-firstposx)>1) ||
             (posiy>1 || abs(y-firstposy)>1)   ) 
            useful=0;
      }
   }
   IMESSAGE(useful)
   ENDMESSAGE(buttonEventIsUseful)
   return(useful);
}

static void FS_listAction(w, event, params, nparams)
   Widget	w;
   XEvent	*event;
   String	*params;
   Cardinal	*nparams;
{
#  define HISTORY_POINTS 10
#  define DECAY_TIME 200
   static int firstposx,posx[HISTORY_POINTS+1],posix;
   static int firstposy,posy[HISTORY_POINTS+1],posiy;
   static int childx,childy,childw,childh,clipw,cliph;
   static int moving=0;
   static Time to;
   Widget child;
   Widget clip;
   Widget list;
   FileSelectionWidget FS_FILE_SELECTION;

   BEGINMESSAGE(FS_listAction)

   if (!w || !event || !params || !nparams || !(*nparams)) {
      INFMESSAGE(illegal call)
      ENDMESSAGE(FS_listAction)
      return;
   }
   if (XtClass(w) != LISTWIDGETCLASS) {
      INFMESSAGE(caller is not a list widget)
      ENDMESSAGE(FS_listAction)
      return;
   }

   list =w;
   child=list;
   clip =XtParent(child);

   w=clip;
   while (w && XtClass(w) != file_selectionWidgetClass) w = XtParent(w);
   if (!w) {
      INFMESSAGE(could not find file selection widget)
      ENDMESSAGE(FS_listAction)
      return;
   }
   FS_FILE_SELECTION = (FileSelectionWidget) w;

   SMESSAGE1(XtName(clip))
   SMESSAGE1(XtName(child))
   SMESSAGE1(XtName(list))

   if (streq(params[0],"set")) {
      INFMESSAGE(set)
      if (buttonEventIsUseful(event,moving,posix,posiy,firstposx,firstposy)) {
         int x=event->xbutton.x;
         event->xbutton.x=10;
         listSetAction(list, event, NULL, NULL);
         event->xbutton.x=x;
      }
   } else if (streq(params[0],"notify")) {
      INFMESSAGE(notify)
      if (buttonEventIsUseful(event,moving,posix,posiy,firstposx,firstposy)) {
         int x=event->xbutton.x;
         event->xbutton.x=10;
         listNotifyAction(list, event, NULL, NULL);
         event->xbutton.x=x;
      }
   } else if (streq(params[0],"unset")) {
      INFMESSAGE(unset)
      if (buttonEventIsUseful(event,moving,posix,posiy,firstposx,firstposy)) {
         int x=event->xbutton.x;
         event->xbutton.x=10;
         listUnsetAction(list, event, NULL, NULL);
         event->xbutton.x=x;
      }
   } else if (streq(params[0],"start-move")) {
      INFMESSAGE(start-move)
      if (event->type != ButtonPress) goto break_scrolling;
      moving = 1;
      posix=posiy=0;
      firstposx = posx[0] = (int) event->xbutton.x_root;
      firstposy = posy[0] = (int) event->xbutton.y_root;
      to = ((XMotionEvent*)event)->time;
   } else if (streq(params[0],"move")) {
      if (event->type != MotionNotify) goto break_scrolling;
      childx = (int) child->core.x;
      childy = (int) child->core.y;
      childw = (int) child->core.width;
      childh = (int) child->core.height;
      clipw  = (int) clip->core.width;
      cliph  = (int) clip->core.height;
      IIMESSAGE1(childx,childy)
      IIMESSAGE1(childw,childh)
      IIMESSAGE1(clipw,cliph)
      INFMESSAGE(move)
      if (moving && clipw && cliph) {
         int x,y;
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
            if (*nparams>=2) relfactor = atof((char*)(params[1]));
            relfactor = relfactor >= 0 ? (relfactor<=100 ? relfactor : 100) : 0;
            if (*nparams>=3) absfactor = atof((char*)(params[2]));
            absfactor = absfactor >= 0 ? (absfactor<=200 ? absfactor : 200) : 0;
            IIMESSAGE1(absfactor,relfactor)
            if (REVERSE_SCROLLING) { dx = -dx; dy = -dy; }
            childx = (int) (childx-(dx*absfactor)-(relfactor*childw*dx)/clipw);
            childy = (int) (childy-(dy*absfactor)-(relfactor*childh*dy)/cliph);
            ClipWidgetSetCoordinates(clip,childx,childy);
            childx = (int) child->core.x;
            childy = (int) child->core.y;
            childw = (int) child->core.width;
            childh = (int) child->core.height;
            clipw  = (int) clip->core.width;
            cliph  = (int) clip->core.height;
         }
      }
   } else if (streq(params[0],"stop-move")) {
break_scrolling:
      INFMESSAGE(stop-move)
      moving = 0;
   } else if (streq(params[0],"page")) {
      int x,y;
      Position midx,midy;
      if (!buttonEventIsUseful(event,moving,posix,posiy,firstposx,firstposy))
         goto stop_page;
      INFMESSAGE(page)
      x = (int) event->xbutton.x_root;
      y = (int) event->xbutton.y_root;
      childx = (int) child->core.x;
      childy = (int) child->core.y;
      childw = (int) child->core.width;
      childh = (int) child->core.height;
      clipw  = (int) clip->core.width;
      cliph  = (int) clip->core.height;
      XtTranslateCoords(clip,0,((Position)cliph/2),&midx,&midy);
      if (y<midy) childy = (int) (childy + abs(cliph-20));
      else  childy = (int) (childy - abs(cliph-20));
      ClipWidgetSetCoordinates(clip,childx,childy);
   }
stop_page:
   ENDMESSAGE(FS_listAction)
   return;
}

/*-------------------------------------------------------------------------------
   FS_textfieldDeleteAction 
-------------------------------------------------------------------------------*/

static void FS_textfieldDeleteAction(w, event, parms, nparms)
   Widget	w;
   XEvent	*event;
   String	*parms;
   Cardinal	*nparms;
{
   BEGINMESSAGE(FS_textfieldDeleteAction)

   if (XtIsSubclass(w,asciiTextWidgetClass)) {
      XawTextPosition begin_sel,end_sel;

      XawTextGetSelectionPos(w,&begin_sel,&end_sel);
      if (begin_sel != end_sel) 
         XtCallActionProc(w,"kill-selection",(XEvent *)NULL,(String *)NULL,(Cardinal)NULL);
      else 
         XtCallActionProc(w,"delete-previous-character",(XEvent *)NULL,(String *)NULL,(Cardinal)NULL);
   }       

   ENDMESSAGE(FS_textfieldDeleteAction)
}

/*-------------------------------------------------------------------------------
   FS_textfieldFocusAction 
-------------------------------------------------------------------------------*/

static void FS_textfieldFocusAction(w, event, parms, nparms)
   Widget	w;
   XEvent	*event;
   String	*parms;
   Cardinal	*nparms;
{
   USE_Arg(5);

   BEGINMESSAGE(FS_textfieldFocusAction)

   if (XtIsSubclass(w,asciiTextWidgetClass)) {
      FS_WIDGET XtParent(XtParent(w));

      if ((FS_OLD_TEXTFIELD) && (w != FS_OLD_TEXTFIELD)) {
         RESET_Arg;
         if (HIGHLIGHT) { SET_Arg(XtNbackground,OLD_HIGHLIGHT); }     
         SET_Arg(XtNdisplayCaret,     False);
         SET_Values(FS_OLD_TEXTFIELD);
      } 

      if ((!FS_OLD_TEXTFIELD) || (FS_OLD_TEXTFIELD != w)) {
         XtSetKeyboardFocus((Widget)FS_FILE_SELECTION, w);
         if (HIGHLIGHT) { GET_Value(w,XtNbackground,&(OLD_HIGHLIGHT)); } 
         RESET_Arg;
         if (HIGHLIGHT) { SET_Arg(XtNbackground,HIGHLIGHT); }     
         SET_Arg(XtNdisplayCaret,      True);
         SET_Values(w);
      }
      FS_OLD_TEXTFIELD = w;
   }
   ENDMESSAGE(FS_textfieldFocusAction)
}  

/*-------------------------------------------------------------------------------
    MulticlickNotify
-------------------------------------------------------------------------------*/

static void MulticlickNotify (client_data, idp)
    XtPointer client_data;
    XtIntervalId *idp;
{
    FS_WIDGET client_data;
   
    BEGINMESSAGE(MulticlickNotify) 
    MULTICLICK = DISABLED;
    ENDMESSAGE(MulticlickNotify)
}                               

/*-------------------------------------------------------------------------------
   Destroy
-------------------------------------------------------------------------------*/

static void 
Destroy(w)
   Widget 	w;
{
   FS_WIDGET w;

   BEGINMESSAGE(Destroy)
   while ((--TOPDIR_ENTRIES) >=0 ) FS_XtFree(TOPDIR_ENTRY(TOPDIR_ENTRIES)); ++TOPDIR_ENTRIES;
   while ((--CURDIR_ENTRIES) >=0 ) FS_XtFree(CURDIR_ENTRY(CURDIR_ENTRIES)); ++CURDIR_ENTRIES;
   while ((--SUBDIR_ENTRIES) >=0 ) FS_XtFree(SUBDIR_ENTRY(SUBDIR_ENTRIES)); ++SUBDIR_ENTRIES;

   FS_XtFree(TOPDIR_LIST);
   FS_XtFree(CURDIR_LIST);
   FS_XtFree(SUBDIR_LIST);
   FS_XtFree(PATH);
   FS_XtFree(FILTER);
   FS_XtFree(BOTH);
   FS_XtFree(CURRENT_DIR);
   FS_XtFree(APP_DIR);
   DESTROY_MULTICLICK;

   ENDMESSAGE(Destroy)
}

/*-------------------------------------------------------------------------------
   SetValues
-------------------------------------------------------------------------------*/

static Boolean SetValues(current, request, new, in_args, in_num_args)
   Widget 	current, request, new;
   ArgList 	in_args;
   Cardinal 	*in_num_args;
{
   FS_WIDGET	new;
   FileSelectionWidget rfs = (FileSelectionWidget)request;  
   USE_Arg(5);

   BEGINMESSAGE(SetValues)

   if (PATH_RESOURCE != rfs->file_selection.path_field_value) {
      if (PATH_RESOURCE) { SET_Value(FS_PATH,XtNstring,PATH_RESOURCE); }
      else { SET_Value(FS_PATH,XtNstring,APP_DIR); }
      INFMESSAGE(changing Path Selection Field)
   }
   
   if (FILTER_RESOURCE != rfs->file_selection.filter_field_value) {
      SET_Value(FS_FILTER,XtNstring,FILTER_RESOURCE);
      INFMESSAGE(changing File Selection Field)
   }

   if (PREFERRED_BUTTON != rfs->file_selection.preferredButton) {
      SetPreferredButton(new,rfs->file_selection.preferredButton,FALSE);
      SetPreferredButton(new,PREFERRED_BUTTON,TRUE);
      INFMESSAGE(switched Accelerators)
   }

   ENDMESSAGE(SetValues)
   return False;
}

/*-------------------------------------------------------------------------------
   UpdateLists
-------------------------------------------------------------------------------*/

static void UpdateLists(FS_FILE_SELECTION,attemptResize) 
   FileSelectionWidget FS_FILE_SELECTION;
   Boolean attemptResize;
{
   INFMESSAGE(executing UpdateLists)
   IMESSAGE(TOPDIR_ENTRIES)
   IMESSAGE(CURDIR_ENTRIES)
   IMESSAGE(SUBDIR_ENTRIES)
#  ifdef MESSAGES
     if (!TOPDIR_ENTRIES && TOPDIR_ENTRY(0)) 
        {INFMESSAGE(Warning: no TOP entries but first entry is not NULL)}
     if (!CURDIR_ENTRIES && CURDIR_ENTRY(0)) 
        {INFMESSAGE(Warning: no CUR entries but first entry is not NULL)}
     if (!SUBDIR_ENTRIES && SUBDIR_ENTRY(0)) 
        {INFMESSAGE(Warning: no SUB entries but first entry is not NULL)}
#  endif

   XAWLISTCHANGE(FS_TOPLIST, TOPDIR_LIST, TOPDIR_ENTRIES, 0, attemptResize);
   XAWLISTCHANGE(FS_CURLIST, CURDIR_LIST, CURDIR_ENTRIES, 0, attemptResize);
   XAWLISTCHANGE(FS_SUBLIST, SUBDIR_LIST, SUBDIR_ENTRIES, 0, attemptResize);
}

/*-------------------------------------------------------------------------------
   AdjustListSizes
-------------------------------------------------------------------------------*/

static void AdjustListSizes(FS_FILE_SELECTION) 
   FileSelectionWidget FS_FILE_SELECTION;
{
   BEGINMESSAGE(AdjustListSizes)
   UpdateLists(FS_FILE_SELECTION,True);
   ENDMESSAGE(AdjustListSizes)
}

/*----------------------------------------------------------------------
   SetIncompleteDirectoryView
----------------------------------------------------------------------*/

static void
SetIncompleteDirectoryView(w,list)
   Widget	w;
   String	*list;
{
   FS_WIDGET 	w;

   BEGINMESSAGE(SetIncompleteDirectoryView)
   XAWLISTCHANGE(FS_CURLIST, list, 1, 0, True); 
   chdir(APP_DIR);
   ENDMESSAGE(SetIncompleteDirectoryView)
}  

/*----------------------------------------------------------------------
   SetDirectoryView
   Updates all lists according to the path given in the path selection
   field.
----------------------------------------------------------------------*/

static void
SetDirectoryView(w,viewmode)
   Widget	w;
   int		viewmode;
{
   FS_WIDGET 		w;
   DIR			*dirp;
   struct dirent	*dp;
   String  		str;
   char 		*temp;
   Boolean 		accepted;
   char 		path[FS_MAXNAMLEN];
   char                 *filter;
#  ifdef VMS
      char		tempfile[FS_MAXNAMLEN];
#  endif
   USE_Arg(5);
   
   BEGINMESSAGE(SetDirectoryView)

   {
      int s;
      char tmp[FS_MAXNAMLEN];
      GET_Value(FS_PATH,XtNstring,&temp);
      if (!temp || temp[0]=='\0') temp=CURRENT_DIR;
      else if (strchr(temp,'~')) {
         strncpy(tmp,temp, FS_MAXNAMLEN-2);
         tmp[FS_MAXNAMLEN-1]='\0';
         translateTildeInPath(tmp);
         temp=tmp;
      }
      INFSMESSAGE(trying to chdir to,temp)
      s=chdir(temp);
      if (s && temp==CURRENT_DIR) {
         temp=APP_DIR;
         s=chdir(temp);
      }
      if (s) {
         SetIncompleteDirectoryView(w,unknownList); 
         INFMESSAGE(unable to change directory appropriately)
         ENDMESSAGE(SetDirectoryView)
         return;
      } 
   }

   if (!(dirp=opendir("."))) {
      SetIncompleteDirectoryView(w,cannotopenList);
      INFMESSAGE(unable to open directory) ENDMESSAGE(SetDirectoryView)
      return;
   }

   getwd(path);
   appendDirEndSeparator(path);
   SET_Value(FS_PATH,XtNstring,path);
   FS_XtFree(CURRENT_DIR); CURRENT_DIR = FS_XtNewString(path);
   SMESSAGE(CURRENT_DIR)

   while ((--TOPDIR_ENTRIES) >=0 ) FS_XtFree((char *)TOPDIR_ENTRY(TOPDIR_ENTRIES)); ++TOPDIR_ENTRIES;
   while ((--CURDIR_ENTRIES) >=0 ) FS_XtFree((char *)CURDIR_ENTRY(CURDIR_ENTRIES)); ++CURDIR_ENTRIES;
   while ((--SUBDIR_ENTRIES) >=0 ) FS_XtFree((char *)SUBDIR_ENTRY(SUBDIR_ENTRIES)); ++SUBDIR_ENTRIES;

   GET_Value(FS_FILTER,XtNstring,&filter);
   SMESSAGE(filter)

   if (streq(filter,"")) viewmode = XawFileSelectionRescan;
   if (viewmode == XawFileSelectionDefaultScan) viewmode = VIEWMODE;

#  ifdef VMS
      temp = filter;
      if (viewmode == XawFileSelectionFilter) {
         while ((*(temp) = toupper(*temp))) ++temp;
         if (strchr(filter,'.')) {
            temp=strchr(filter,';');
            if ( ((temp) && (!(*(temp+1)))) || (!temp) ) {
               strncpy(tempfile,filter,FS_MAXNAMLEN-3); tempfile[FS_MAXNAMLEN-3] = '\0';
               filter = tempfile;
               strcat(filter,";*");
            }
         }
      }
      SMESSAGE(filter)
#  endif

   accepted = TRUE;
   while ((dp = readdir(dirp))) {
      str = dp->d_name;
#     ifdef VMS
         if ((temp=strstr(str,".DIR"))) {
            *temp = '\0';
            REALLOC_MORE_IF_NEEDED(SUBDIR_LIST,SUBDIR_ENTRIES+1,SUBDIR_ALLOC);
            SUBDIR_ENTRY(SUBDIR_ENTRIES) = FS_XtNewString(str);
            SMESSAGE(SUBDIR_ENTRY(SUBDIR_ENTRIES))
            SUBDIR_ENTRIES++;
         } else {
            if (viewmode==XawFileSelectionFilter) accepted=strwild(str,filter);
            if (accepted) {
               REALLOC_MORE_IF_NEEDED(CURDIR_LIST,CURDIR_ENTRIES+1,CURDIR_ALLOC);
               CURDIR_ENTRY(CURDIR_ENTRIES) = FS_XtNewString(str);
               SMESSAGE(CURDIR_ENTRY(CURDIR_ENTRIES))
               CURDIR_ENTRIES++;
            }
#           ifdef MESSAGES
            else {
               INFMESSAGE(list entry not accepted by viewmode)
            }
#           endif
         }
#     else /*end of VMS */
         if (!(*str == '.' && *(str+1)=='\0')) {
            struct stat s;
            stat(str,&s);
            if (S_ISDIR(s.st_mode)) {
               REALLOC_MORE_IF_NEEDED(SUBDIR_LIST,SUBDIR_ENTRIES+1,SUBDIR_ALLOC);
               SUBDIR_ENTRY(SUBDIR_ENTRIES) = FS_XtNewString(str);
               SMESSAGE(SUBDIR_ENTRY(SUBDIR_ENTRIES))
               SUBDIR_ENTRIES++;
            } else {
               if (viewmode==XawFileSelectionFilter) accepted=strwild(str,filter);
               if (accepted) {
                  REALLOC_MORE_IF_NEEDED(CURDIR_LIST,CURDIR_ENTRIES+1,CURDIR_ALLOC);
                  CURDIR_ENTRY(CURDIR_ENTRIES) = FS_XtNewString(str);
                  SMESSAGE(CURDIR_ENTRY(CURDIR_ENTRIES))
                  CURDIR_ENTRIES++;
               }
#              ifdef MESSAGES
               else {
                  INFMESSAGE(list entry not accepted by viewmode)
               }
#              endif
            }
         }
#     endif
   }

   IMESSAGE(CURDIR_ENTRIES)
   if (!CURDIR_ENTRIES) {
      CURDIR_ENTRY(CURDIR_ENTRIES) = (String) NULL;
      INFMESSAGE(no entries in this direcory)
   }
   else qsort( CURDIR_LIST, CURDIR_ENTRIES, sizeof(char *), FScompareEntries);

   IMESSAGE(SUBDIR_ENTRIES)
   if (!SUBDIR_ENTRIES) {
      SUBDIR_ENTRY(SUBDIR_ENTRIES) = (String) NULL;
      INFMESSAGE(no subdirectories)
   }
   else qsort( SUBDIR_LIST, SUBDIR_ENTRIES, sizeof(char *), FScompareEntries);

   closedir(dirp);

   {
      if (!(chdir(ONE_STEP_UP))) {
         INFMESSAGE(one step up is allowed)
         TOPDIR_ENTRY(TOPDIR_ENTRIES) = FS_XtNewString(".."); TOPDIR_ENTRIES++;
         chdir(CURRENT_DIR);
      }

#     ifdef VMS
      {
         int ntops = 0;
         char *p;
         p = strrchr(path,':');
         if (p) {
            ++p;
            if (strchr(p,'[')) {
               strreplace(p,".]","]",p);
               strreplace(p,"][",".",p);
               strreplace(p,".000000","",p);
               strreplace(p,"000000.","",p);
               strreplace(p,"[000000]","",p);
               strreplace(p,"]","",p);
               strreplace(p,"[","",p);
               strcpy(path,p);
            }
         }
         SMESSAGE(path)

         if (path) {
            int ntops = 1;
            temp=path;
            while ((temp=strchr(temp,'.'))) { 
               *(temp++) = '\0'; ++ntops; 
            }
            temp=path;
            while ((ntops--)) {
               REALLOC_MORE_IF_NEEDED(TOPDIR_LIST,TOPDIR_ENTRIES+1,TOPDIR_ALLOC);
               TOPDIR_ENTRY(TOPDIR_ENTRIES) = FS_XtNewString(temp);
               SMESSAGE1(TOPDIR_ENTRY(TOPDIR_ENTRIES))
               TOPDIR_ENTRIES++;
               temp = strchr(temp+1,'\0')+1;
            }
         }
      }
#     else /*end of VMS*/
      {
         REALLOC_MORE_IF_NEEDED(TOPDIR_LIST,TOPDIR_ENTRIES+1,TOPDIR_ALLOC);
         TOPDIR_ENTRY(TOPDIR_ENTRIES) = FS_XtNewString("/");
         SMESSAGE(TOPDIR_ENTRY(TOPDIR_ENTRIES))
         TOPDIR_ENTRIES++;
         if (path) {
            char *p=path;
            SMESSAGE(p)
            strcat(path,"/");
 	    if (*p=='/') p++;
            temp=p;
            while ((*temp != '/') && (p=strchr(p,'/'))) { 
               *p = '\0';
               p++;
               REALLOC_MORE_IF_NEEDED(TOPDIR_LIST,TOPDIR_ENTRIES+1,TOPDIR_ALLOC);
               TOPDIR_ENTRY(TOPDIR_ENTRIES) = FS_XtNewString(temp);
               SMESSAGE(TOPDIR_ENTRY(TOPDIR_ENTRIES))
               TOPDIR_ENTRIES++;
               temp=p;
            }
         }
      }
#     endif

      IMESSAGE1(TOPDIR_ENTRIES)
      if (!TOPDIR_ENTRIES) {
         TOPDIR_ENTRY(TOPDIR_ENTRIES) = (String) NULL;
         INFMESSAGE1(no topdirectories)
      }
   }

   AdjustListSizes(FS_FILE_SELECTION);
   chdir(APP_DIR);

   ENDMESSAGE(SetDirectoryView)
   return;
}

/*-------------------------------------------------------------------------------
   homeProc
   callback for the home button
-------------------------------------------------------------------------------*/

static void
homeProc(w, client_data, call_data)
   Widget	w;
   XtPointer	client_data, call_data;
{
   FS_WIDGET XtParent(w);
   USE_Arg(2);
   char *home;

   BEGINMESSAGE(homeProc)
#  ifdef VMS
      home=getenv("SYS$LOGIN");
#  else
      home=getenv("HOME");
#  endif
   if (home) {
      SET_Value(FS_PATH,XtNstring,home);
      SetDirectoryView(FS_FILE_SELECTION,VIEWMODE);
   }
   ENDMESSAGE(homeProc)
}

/*-------------------------------------------------------------------------------
   tmpProc
   callback for the tmp button
-------------------------------------------------------------------------------*/

static void
tmpProc(w, client_data, call_data)
   Widget	w;
   XtPointer	client_data, call_data;
{
   FS_WIDGET XtParent(w);
   USE_Arg(2);

   BEGINMESSAGE(homeProc)

   if (TMP_DIR_RESOURCE) {
      SET_Value(FS_PATH,XtNstring,TMP_DIR_RESOURCE);
      SetDirectoryView(FS_FILE_SELECTION,VIEWMODE);
   }
   else homeProc(w, client_data, call_data);
   ENDMESSAGE(homeProc)
}

/*-------------------------------------------------------------------------------
   rescanProc
   callback for the rescan button
-------------------------------------------------------------------------------*/

static void
rescanProc(w, client_data, call_data)
   Widget	w;
   XtPointer	client_data, call_data;
{
   FS_WIDGET XtParent(w);

   BEGINMESSAGE(rescanProc)
   SetDirectoryView(FS_FILE_SELECTION,XawFileSelectionRescan);
   ENDMESSAGE(rescanProc)
}

/*-------------------------------------------------------------------------------
   filterProc
   callback for the filter button
-------------------------------------------------------------------------------*/

static void
filterProc(w, client_data, call_data)
   Widget	w;
   XtPointer	client_data, call_data;
{
   FS_WIDGET XtParent(w);

   BEGINMESSAGE(filterProc)
   SetDirectoryView(FS_FILE_SELECTION,XawFileSelectionFilter);
   ENDMESSAGE(filterProc)
}

/*----------------------------------------------------------------------
   TopDirSelectionProc
   callback for topdirectory list
----------------------------------------------------------------------*/

static void
TopDirSelectionProc(w, client_data, call_data)
   Widget	w;
   XtPointer	client_data, call_data;
{
   XawListReturnStruct  *list_struct = (XawListReturnStruct *) call_data;
   FS_WIDGET XtParent(XtParent(XtParent(XtParent(w))));
   USE_Arg(5);
   char newpath[FS_MAXNAMLEN];
   
   BEGINMESSAGE(TopDirSelectionProc)

   if (list_struct->list_index != XAW_LIST_NONE) {
      if (!(strcmp((String)list_struct->string,".."))) {
         if (chdir(CURRENT_DIR)) {
            INFMESSAGE(unable to switch to current directory) ENDMESSAGE(TopDirSelectionProc)
            return;
         }
         if (chdir(ONE_STEP_UP)) {
            INFMESSAGE(unable to step up)
            ENDMESSAGE(TopDirSelectionProc)
            return;
         }
         getwd(newpath);
      }
      else {
         if (chdir(CURRENT_DIR)) {
            INFMESSAGE(unable to switch to current directory) ENDMESSAGE(TopDirSelectionProc)
            return;
         }

         strcpy(newpath,CURRENT_DIR);
         CHANGE_TO_HEAD_OF_DIR_SPEC(newpath);
         strcat(newpath,DIR_SPECIFICATION_START_STRING);
         {
            int item = -1;
            while (++item <= list_struct->list_index) {
               if (strcmp(TOPDIR_ENTRY(item),"..") && strcmp(TOPDIR_ENTRY(item),"/")) {
                  strcat(newpath,TOPDIR_ENTRY(item));
                  if (item<list_struct->list_index) strcat(newpath,DIR_SEPARATOR_STRING);
               }
            }
         }
      }
      appendDirEndSeparator(newpath);
      SMESSAGE(newpath)
      SET_Value(FS_PATH,XtNstring,newpath);
   }
   SetDirectoryView(FS_FILE_SELECTION,VIEWMODE);

   ENDMESSAGE(TopDirSelectionProc)
}

/*-------------------------------------------------------------------------------
   CurDirSelectionProc
   callback for current directory list
-------------------------------------------------------------------------------*/

static void
CurDirSelectionProc(w, client_data, call_data)
   Widget	w;
   XtPointer	client_data, call_data;
{
   FS_WIDGET	XtParent(XtParent(XtParent(XtParent(w))));
   char		name[10];
   XawListReturnStruct  *list_struct = (XawListReturnStruct *) call_data;
   USE_Arg(2);

   BEGINMESSAGE(CurDirSelectionProc)

   if (list_struct->list_index != XAW_LIST_NONE) {
      char *path="<path too long>";
      char tmp[FS_MAXNAMLEN];
      size_t l;
      if ((l=strlen(CURRENT_DIR)) + strlen(list_struct->string) <= FS_MAXNAMLEN-2) {
         strcpy(tmp,CURRENT_DIR);
         appendDirEndSeparator(tmp);
         strcat(tmp,list_struct->string);
         path=tmp;
      }
      SET_Value(FS_PATH,XtNstring,path);
   
      if (MULTICLICK) {
         DESTROY_MULTICLICK;
         if (IS_BUTTON(PREFERRED_BUTTON)) {
            POSITION_TO_BUTTON_NAME(PREFERRED_BUTTON,name);
            XtCallCallbacks(XtNameToWidget((Widget)FS_FILE_SELECTION,name),XtNcallback,call_data);
         }
      }
      else ENABLE_MULTICLICK;
   }

   ENDMESSAGE(CurDirSelectionProc)
}

/*-------------------------------------------------------------------------------
   SubDirSelectionProc
   callback for subdirectory list
-------------------------------------------------------------------------------*/

static void
SubDirSelectionProc(w, client_data, call_data)
   Widget	w;
   XtPointer	client_data, call_data;
{
   XawListReturnStruct  *list_struct = (XawListReturnStruct *) call_data;
   FS_WIDGET XtParent(XtParent(XtParent(XtParent(w))));
   USE_Arg(2);
   char newpath[FS_MAXNAMLEN];
   
   BEGINMESSAGE(SubDirSelectionProc)

   if (list_struct->list_index != XAW_LIST_NONE) {
      if (!strcmp(list_struct->string,"..")) {
         TopDirSelectionProc(w, client_data, call_data);
         ENDMESSAGE(SubDirSelectionProc)
         return;
      }
      strncpy(newpath,CURRENT_DIR,FS_MAXNAMLEN-1); newpath[FS_MAXNAMLEN-1]='\0';      
      appendPathElement(newpath,list_struct->string);
      SET_Value(FS_PATH,XtNstring,newpath);
   }
   SetDirectoryView(FS_FILE_SELECTION,VIEWMODE);

   ENDMESSAGE(SubDirSelectionProc)
}

/*-------------------------------------------------------------------------------
   CreateTextField
-------------------------------------------------------------------------------*/

static void CreateTextField(frameP,textP,value,text_trans,namebase,parent)
   Widget *frameP;
   Widget *textP;
   char *value;
   XtTranslations text_trans;
   String namebase;
   Widget parent;
{
   USE_Arg(10);
   char name[15];

   BEGINMESSAGE(CreateTextField)

               RESET_Arg;
	       SET_Arg(XtNresizable,        True);
               sprintf(name,"%sframe",namebase);
   *frameP   = ADD_Widget(name,frameWidgetClass,parent);

               RESET_Arg;
               SET_Arg(XtNdisplayCaret,     False);
               SET_Arg(XtNuseStringInPlace, False);
               SET_Arg(XtNstring,           value);
               SET_Arg(XtNeditType,         XawtextEdit);
               SET_Arg(XtNscrollHorizontal, XawtextScrollNever);
               SET_Arg(XtNscrollVertical,   XawtextScrollNever);
               SET_Arg(XtNtranslations,	    text_trans);
               SET_Arg(XtNtype,             XawAsciiString);
               SET_Arg(XtNresize,           XawtextResizeNever);
               sprintf(name,"%stext",namebase);
   *textP = ADD_Widget_Arg(name, asciiTextWidgetClass,*frameP);

   ENDMESSAGE(CreateTextField)
}

/*-------------------------------------------------------------------------------
   CreateList
-------------------------------------------------------------------------------*/

static void
CreateList(frameP,clipP,aaaP,listP,list_trans,namebase,parent)
   Widget *frameP;
   Widget *clipP;
   Widget *aaaP;
   Widget *listP;
   XtTranslations list_trans;
   String namebase;
   Widget parent;
{
   USE_Arg(8);
   char name[15];

   BEGINMESSAGE(CreateList)

                RESET_Arg;
		SET_Arg(XtNresizable,		True);
                sprintf(name,"%sframe",namebase);
   *frameP =	ADD_Widget_Arg(name,frameWidgetClass,parent);

                RESET_Arg;
		SET_Arg(XtNconditionedResize,	False);
                sprintf(name,"%saaa",namebase);
   *aaaP =	ADD_Widget_Arg(name,aaaWidgetClass,*frameP);

                RESET_Arg;
   *clipP =	ADD_Widget_Arg("clip",clipWidgetClass,*aaaP);

                RESET_Arg;
                SET_Arg(XtNforceColumns,	True);
                SET_Arg(XtNdefaultColumns,	1);
                SET_Arg(XtNlist,		emptyList);
                SET_Arg(XtNlongest,		0); 
                SET_Arg(XtNverticalList,	True); 
                SET_Arg(XtNborderWidth,		0); 
                SET_Arg(XtNtranslations,	list_trans);
   *listP =     ADD_Widget_Arg("list",LISTWIDGETCLASS,*clipP);

   ENDMESSAGE(CreateList)
}

/*-------------------------------------------------------------------------------
   Initialize
-------------------------------------------------------------------------------*/

static void Initialize(request, new, argl, num_argl)
   Widget 	request, new;
   ArgList 	argl;
   Cardinal 	*num_argl;
{
   FS_WIDGET 	new;

   BEGINMESSAGE(Initialize)

   { 
       char app_dir[FS_MAXNAMLEN];
       getwd(app_dir);
       APP_DIR = FS_XtNewString(app_dir);
   }  

   FS_RESCANBUTTON  = ADD_Widget("rescan",commandWidgetClass,new);
                ADD_Callback(FS_RESCANBUTTON,rescanProc);

   FS_FILTERBUTTON  = ADD_Widget("filter",commandWidgetClass,new);
                ADD_Callback(FS_FILTERBUTTON,filterProc);

   FS_TMPBUTTON  = ADD_Widget("tmp",commandWidgetClass,new);
                ADD_Callback(FS_TMPBUTTON,tmpProc);

   FS_HOMEBUTTON  = ADD_Widget("home",commandWidgetClass,new);
                ADD_Callback(FS_HOMEBUTTON,homeProc);

   BUTTONS = 0;
   IMESSAGE(BUTTONS_RESOURCE)
   if ((BUTTONS_RESOURCE > 0) && (BUTTONS_RESOURCE<5)) {
      if (BUTTONS_RESOURCE > 0) {
         FS_BUTTON1 = ADD_Widget("button1",commandWidgetClass,new);
         BUTTONS += 1;
      }
      if (BUTTONS_RESOURCE > 1) {
         FS_BUTTON2 = ADD_Widget("button2",commandWidgetClass,new);
         BUTTONS += 2;
      }
      if (BUTTONS_RESOURCE > 2) {
         FS_BUTTON3 = ADD_Widget("button3",commandWidgetClass,new);
         BUTTONS += 4;
      }
      if (BUTTONS_RESOURCE > 3) {
         FS_BUTTON4 = ADD_Widget("button4",commandWidgetClass,new);
         BUTTONS += 8;
      }
      IMESSAGE(BUTTONS)
   }

   {
      XtTranslations text_trans;
      String value;
      text_trans=XtParseTranslationTable(TextField_translations);
      value=PATH_RESOURCE ? PATH_RESOURCE : APP_DIR;
      CreateTextField(&FS_PATHFRAME,  &FS_PATH,  value,text_trans,"path",  new);
      value=FILTER_RESOURCE;
      CreateTextField(&FS_FILTERFRAME,&FS_FILTER,value,text_trans,"filter",new);
   }

   SetPreferredButton(new,PREFERRED_BUTTON,TRUE);

   {
      XtTranslations list_trans;
      list_trans=XtParseTranslationTable(list_translations);
      CreateList(&FS_TOPFRAME,&FS_TOPCLIP,&FS_TOPAAA,&FS_TOPLIST,list_trans,"top",new);
      ADD_Callback(FS_TOPLIST,TopDirSelectionProc);
      CreateList(&FS_SUBFRAME,&FS_SUBCLIP,&FS_SUBAAA,&FS_SUBLIST,list_trans,"sub",new);
      ADD_Callback(FS_SUBLIST,SubDirSelectionProc);
      list_trans=XtParseTranslationTable(curlist_translations);
      CreateList(&FS_CURFRAME,&FS_CURCLIP,&FS_CURAAA,&FS_CURLIST,list_trans,"cur",new);
      ADD_Callback(FS_CURLIST,CurDirSelectionProc);
   }

   TOPDIR_ALLOC    = 20;  TOPDIR_ENTRIES = 0; ALLOC_LIST(TOPDIR_LIST,TOPDIR_ALLOC);
   TOPDIR_ENTRY(0) = NULL;
   CURDIR_ALLOC    = 100; CURDIR_ENTRIES = 0; ALLOC_LIST(CURDIR_LIST,CURDIR_ALLOC);
   CURDIR_ENTRY(0) = NULL;
   SUBDIR_ALLOC    = 20;  SUBDIR_ENTRIES = 0; ALLOC_LIST(SUBDIR_LIST,SUBDIR_ALLOC);
   SUBDIR_ENTRY(0) = NULL;

   FS_OLD_TEXTFIELD = (Widget)NULL;

   CURRENT_DIR = FS_XtNewString("");
   PATH        = FS_XtNewString("");
   FILTER      = FS_XtNewString("");
   BOTH        = FS_XtNewString("");
   MULTICLICK = DISABLED;

   XtCallActionProc(FS_PATH,"FS_textfieldFocusAction",(XEvent *)NULL,(String *)NULL,(Cardinal)NULL);
   XtCallActionProc(FS_FILTER,"FS_textfieldFocusAction",(XEvent *)NULL,(String *)NULL,(Cardinal)NULL);

   {
      XtActionList actions;
      Cardinal i,nactions;

      XtGetActionList(LISTWIDGETCLASS, &actions, &nactions);

      if (actions) for (i = 0 ; i < nactions ; i++) {
         SMESSAGE(actions[i].string)
         if      (streq(actions[i].string, "Set"))    listSetAction    = actions[i].proc;
         else if (streq(actions[i].string, "Unset"))  listUnsetAction  = actions[i].proc;
         else if (streq(actions[i].string, "Notify")) listNotifyAction = actions[i].proc;
         if (listSetAction && listUnsetAction && listNotifyAction) {
	    XtFree((char *) actions);
            INFMESSAGE(found all List actions)
            ENDMESSAGE(ClassInitialize)
	    return;
         }
      }
      ENDMESSAGE(Initialize)
      XtError("FileSel Widget: Cannot steal List Actions.");
   }

   ENDMESSAGE(Initialize)
}

/*=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   PUBLIC ROUTINES
-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*/   
   
/*########################################################################
   XawFileSelectionGetFile
########################################################################*/

char *
#if NeedFunctionPrototypes
XawFileSelectionGetPath(Widget w)
#else
XawFileSelectionGetPath(w)
   Widget 	w;
#endif
{
   FS_WIDGET	w;
   String	path=NULL;
   USE_Arg(2);
   char tmp[FS_MAXNAMLEN];

   BEGINMESSAGE(XawFileSelectionGetPath)

   GET_Value(FS_PATH,XtNstring,&path);
   if (!path) path ="";
   strncpy(tmp,path,FS_MAXNAMLEN-3);
   tmp[FS_MAXNAMLEN-3]='\0';
   translateTildeInPath(tmp);
   if (PATH) FS_XtFree(PATH);
   PATH = FS_XtNewString(path);
   INFSMESSAGE(returning, PATH)

   ENDMESSAGE(XawFileSelectionGetPath)
   return(PATH);
}

/*########################################################################
   XawFileSelectionSetPath
########################################################################*/

void
#if NeedFunctionPrototypes
XawFileSelectionSetPath(Widget w,String string)
#else
XawFileSelectionSetPath(w,string)
   Widget 	w;
   String 	string;
#endif
{
   FS_WIDGET w;
   USE_Arg(3);

   BEGINMESSAGE(XawFileSelectionSetPath)
   if (string) {
      SET_Value(FS_PATH,XtNstring,string);
   }
   ENDMESSAGE(XawFileSelectionGetPath)
}

/*########################################################################
   XawFileSelectionScan
########################################################################*/

void
#if NeedFunctionPrototypes
XawFileSelectionScan(Widget w,int indicator)
#else
XawFileSelectionScan(w,indicator)
   Widget 	w;
   int 		indicator;
#endif
{
   FS_WIDGET	w;

   BEGINMESSAGE(XawFileSelectionScan)

   if (indicator==XawFileSelectionDefaultScan) indicator = VIEWMODE;
   switch (indicator) {
      case XawFileSelectionFilter:
         filterProc(FS_FILTERBUTTON,(XtPointer)NULL,(XtPointer)NULL);
         break;
      case XawFileSelectionRescan:
         rescanProc(FS_RESCANBUTTON,(XtPointer)NULL,(XtPointer)NULL);
         break;
   }

   ENDMESSAGE(XawFileSelectionScan)
}

/*########################################################################
   XawFileSelectionAddButton
########################################################################*/

void
#if NeedFunctionPrototypes
XawFileSelectionAddButton(Widget w, int position, XtCallbackProc function, XtPointer param)
#else
XawFileSelectionAddButton(w, position, function, param)
   Widget		w;
   int 			position;
   XtCallbackProc	function;
   XtPointer		param;
#endif
{
   FS_WIDGET 		w;
   char			name[10];
   Widget		button;

   BEGINMESSAGE(XawFileSelectionAddButton)

   if (IS_BUTTON(position)) {
      INFMESSAGE(desired Button Position is already used) ENDMESSAGE(XawFileSelectionAddButton)
      return;
   }

   POSITION_TO_BUTTON_NAME(position,name);
   IMESSAGE(position) SMESSAGE(name);
   button = ADD_Widget(name,commandWidgetClass,FS_FILE_SELECTION);
   BUTTONS += POSITION(position);
   if (function) XtAddCallback(button, XtNcallback, function, param);

   ENDMESSAGE(XawFileSelectionAddButton)
}

/*########################################################################
   XawFileSelectionRemoveButton
########################################################################*/

void
#if NeedFunctionPrototypes
XawFileSelectionRemoveButton(Widget w, int position)
#else
XawFileSelectionRemoveButton(w, position)
   Widget		w;
   int 			position;
#endif 
{
   FS_WIDGET 		w;
   char 		name[10];
   USE_Arg(3);
   Widget button;

   BEGINMESSAGE(XawFileSelectionRemoveButton)

   if (!(IS_BUTTON(position))) { 
      INFMESSAGE(Unused Button Position) ENDMESSAGE(XawFileSelectionRemoveButton)
      return;
   }

   POSITION_TO_BUTTON_NAME(position,name);
   button = XtNameToWidget((Widget)FS_FILE_SELECTION,name);
   SET_Arg( XtNheight, 0);
   SET_Arg( XtNwidth,  0);
   SET_Values(button);
   XtDestroyWidget(button);
   BUTTONS -= POSITION(position);
   IMESSAGE(BUTTONS)
   ENDMESSAGE(XawFileSelectionRemoveButton)
}

/*########################################################################
   XawFileSelectionPreferButton
########################################################################*/

void
#if NeedFunctionPrototypes
XawFileSelectionPreferButton(Widget w, int position)
#else
XawFileSelectionPreferButton(w, position)
   Widget		w;
   int 			position;
#endif 
{
   int i;

   BEGINMESSAGE(XawFileSelectionPreferButton)
   for (i=1 ; i<4 ; i++ ) SetPreferredButton(w,i,(i == position) ? 1 : 0);
   ENDMESSAGE(XawFileSelectionPreferButton)
}

/*###############################################################################
  unused code
###############################################################################*/

#if 0

#define MIN_NUM_OF_LETTERS 10
 
static void
GetListSize(l,widthp,heightp,preferredWidth,preferredHeight)  
   LISTWIDGET l;
   Dimension *widthp;
   Dimension *heightp;
   Dimension *preferredWidth;
   Dimension *preferredHeight;
{
   USE_Arg(11);
   Dimension internalWidth,internalHeight;
   Dimension columnSpacing,rowSpacing;
   int longest;
   int numberStrings;
   XFontStruct *font;
   Dimension tempdim;

   BEGINMESSAGE(GetListSize)

   SET_Arg( XtNheight,		heightp);
   SET_Arg( XtNwidth,		widthp);
   SET_Arg( XtNinternalHeight,	&internalHeight);
   SET_Arg( XtNinternalWidth,	&internalWidth);
   SET_Arg( XtNrowSpacing,	&rowSpacing);
   SET_Arg( XtNcolumnSpacing,	&columnSpacing);
   SET_Arg( XtNlongest,		&longest);
   SET_Arg( XtNnumberStrings,	&numberStrings);
   SET_Arg( XtNfont,		&font);
   GET_Values(l);

   *preferredWidth  = (Dimension) (2*internalWidth  + columnSpacing + longest);
   tempdim          = (Dimension) (font->max_bounds.width+font->min_bounds.width)/2*MIN_NUM_OF_LETTERS;
   *preferredWidth  = (Dimension) (*preferredWidth > tempdim ? *preferredWidth : tempdim);
   *preferredHeight = (Dimension) (
                       numberStrings*(font->max_bounds.ascent+font->max_bounds.descent+rowSpacing) + 2*internalHeight
                    );
   IIMESSAGE(*widthp,*heightp) 
   IIMESSAGE(internalWidth,internalHeight)
   IIMESSAGE(rowSpacing,columnSpacing)
   IIMESSAGE(longest,numberStrings)
   IIMESSAGE(font->max_bounds.ascent,font->max_bounds.descent)
   IIMESSAGE(*preferredWidth,*preferredHeight)

   ENDMESSAGE(GetListSize)
}

#endif /* unused code */
