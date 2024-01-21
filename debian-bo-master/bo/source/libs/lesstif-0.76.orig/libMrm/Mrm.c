/**
 *
 * $Id: Mrm.c,v 1.8 1996/11/12 03:54:01 miers Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 *  Original author:  Geoffrey W. Ritchey
 *                    codesmit@southwind.net
 *
*/ 

#include <stdarg.h>
#include <stdlib.h>
#include <Mrm/MrmPublic.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <Xm/PushB.h>
#include <Xm/BulletinB.h>
#include <Xm/Label.h>
#include <Xm/CascadeB.h>
#include <Xm/Separator.h>
#include <Xm/PanedW.h>
#include <Xm/Text.h>
#include <Xm/FileSB.h>
#include <Xm/ToggleB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/MessageB.h>
#include <XmI/XmXpm.h>
#include <X11/Xlibint.h>
#include <X11/Xlib.h>

#include "uil.h"
#include "lookup.h"
#include "misc.h"

#define MAX_COMPLEX_STRING 2048
#define MAX_ARGS 40

static Display *display = NULL;
Colormap colormap;
static Screen *theScreen;
static Window window;
static Widget parent;
static char *theName;
static char *theClass;

enum {GEO, LATE, ON_CREATE};
#define ANOTHER_WIDGET -1

typedef Widget (*CreateFunction)(Widget, String, ArgList, Cardinal);

typedef struct _CreateFunctionsType{
  char *Name;
  CreateFunction function;
} CreateFunctionsType;

typedef struct _MyHierStruct {
  char *CreateName;
  int CreateClass;
  Arg *args;
  int argCount;
  int SizeArgs;
} MyHierStruct;

extern CreateFunctionsType CreateFunctions[];

typedef struct _SymbolTable {
  struct _SymbolTable *Next;
  int type;
  int Index;
  char *Name;
  char *value;
  char Access;
} SymbolTableType;

typedef struct _callbackType {
  struct _callbackType *next;
  int id;
  int index;
  char *argName;
  char *functionName;
  SymbolTableType *Parameters;
} callbackType;
  
typedef struct _DummyWidget {
  char *Name;
  int  class;
  char managed;
  int  parent;
  int NumberOfInherit;
  int *inherit;
  int  NumberOfChildren;
  int *children;
  callbackType *callbacks;
  SymbolTableType *attributes;
} DummyWidget;

typedef struct _UilModuleType {
  char **Definitions;
  char **Names;
  int NumberOfWidgets;
  Widget *WidgetArray;
  DummyWidget *DummyWidgetArray;
  SymbolTableType *SymbolTable;
} UilModuleType;

typedef struct _GeoType {
  int module_id;
  int this_id;
  struct _GeoType *next;
  DummyWidget *DW;
} GeoType;

typedef struct _ColorTableType {
  char *Reference;
  int IsAddress;
  char *Color;
  char *StringXPM;
} ColorTableType;

typedef struct _ColorTableHeaderType {
  int NumColors;
  ColorTableType *ColorTable;
} ColorTableHeaderType;

int GetSymbolTableValue(XtArgVal *,SymbolTableType *, int, SymbolTableType *);
SymbolTableType *GetSymbolTable(SymbolTableType *);
SymbolTableType *LookupValue(SymbolTableType *SymbolTable, char *name);
FILE *UILOpen(char *FileName,char *mode,FILE *old) ;
char *Substitute(char *buffer);
void ReadWidgetTree(UilModuleType *Module);
void PrintWidgetTree(UilModuleType *Module);
void PrintWidgets(UilModuleType *Module);
void MakePixmap(Display *d, Pixmap *pixmap, PixmapType *data);
void FreeSymbolTable(SymbolTableType *t);
void GetCallbacks(int fileno, int wid_id, DummyWidget *DW);
void GetCallbacks(int fileno, int wid_id, DummyWidget *DW);
void AddGeometryAttributes(GeoType **GeometryList, int module_id, int this_id,
		      DummyWidget *DW);
void SetGeometryAttributes(GeoType **GeometryList);
void ReadList(UilModuleType *Module);
void ReadWidgets(UilModuleType *Module);
void PrintArgs(SymbolTableType *Next);
void PrintControls(int count, int *ids, DummyWidget *Array);
void PrintCallbacks(callbackType *Next);
void PrintSymbolTable(SymbolTableType *SymbolTable);

GeoType *GeometryList = NULL;

SymbolTableType *ReadAttributes();
void ReadCallbacks(callbackType **last);

SymbolTableType *GlobalSymbolTable = NULL;
int NumberFiles;    
String *FileList;

int MaxAttributes;

#define MAX_HIER 20

UilModuleType UilModule[MAX_HIER];

int NumberRegisteredFunctions;
extern MrmRegisterArg *RegisteredFunctions;
XmString DecodeComplexString(char *value);
void Xm_List(Widget parent_widget, DummyWidget *DW, int module_id);

void GetAttributes(Display *d,Window w, int module_id, 
		   int *n, Arg *args,
		   DummyWidget *DW, SymbolTableType 
		   *FormSymbolTable, int OnGeometry);

void MrmInitialize ()
{
}


#define MAX_DEF 2048
#define MAX_STR 256

Cardinal MrmCloseHierarchy(MrmHierarchy hierarchy_id)
{
  return MrmSUCCESS;
}

Cardinal MrmOpenHierarchy( MrmCount num_files , String *name_list , MrmOsOpenParamPtr *os_ext_list , MrmHierarchy *hierarchy_id_return )
{
  Exit(LOC, "Must replace MrmOpenHierarchy with MrmOpenHierarchyPerDisplay\nBecause I can't figure out how to get the display if you don't\n");
  return MrmFAILURE;
}

Cardinal MrmOpenHierarchyPerDisplay( Display *theDisplay, MrmCount num_files , String *name_list , MrmOsOpenParamPtr *os_ext_list , MrmHierarchy *hierarchy_id_return )
{
  int fileIndex;


  /*
     MrmSUCCESS     The function executed successfully.
     
     MrmFAILURE     The function failed.
     */
  display = theDisplay;
  window = DefaultRootWindow(theDisplay);
  theScreen = XDefaultScreenOfDisplay(theDisplay);
  colormap = DefaultColormapOfScreen(theScreen);

  XtGetApplicationNameAndClass(display, &theName, &theClass);

  NumberFiles = num_files;
  FileList = name_list;

  hierarchy_id_return = NULL;

  for (fileIndex = 0; fileIndex < num_files; fileIndex++) {
    if (NULL == UILOpen(FileList[fileIndex],"r",stdin)) {
      Exit(LOC, "Can't open %s\n", FileList[fileIndex]);
    }
    ReadWidgetTree(&UilModule[fileIndex]);
    if (1) {
      fprintf(stderr,"GLOBAL *******\n");
      PrintSymbolTable(GlobalSymbolTable);
      fprintf(stderr,"LOCAL  %d\n",fileIndex);
      PrintSymbolTable(UilModule[fileIndex].SymbolTable);
      PrintWidgetTree(&UilModule[fileIndex]);
    }
  }
  return MrmSUCCESS;
}

Cardinal MrmRegisterNames(MrmRegisterArglist reglist, MrmCount num_reg)
{
/*
       MrmSUCCESS     The function executed successfully.

       MrmFAILURE     The function failed.
*/
	NumberRegisteredFunctions = num_reg;
	RegisteredFunctions = reglist;
	return MrmSUCCESS;
}

Cardinal MrmFetchIconLiteral(MrmHierarchy hierarchy_id, String index,
			     Screen *scrn, Display *d, Pixel fg, Pixel bg, 
			     Pixmap *pixmap)
{
  SymbolTableType *symbol;

  for (symbol = GlobalSymbolTable; symbol; symbol = symbol->Next)
    if (strcmp(index, symbol->Name) == 0) {
      if (MrmRtypeXBitmapFile == symbol->type) {
	BitMapType *bm = (BitMapType *)symbol->value;

	*pixmap = XCreatePixmapFromBitmapData(d, window, bm->data, 
					     bm->width, bm->height,
					     fg,bg, 
					     DefaultDepth(display,0));
      }
      else
	MakePixmap(d, pixmap, (PixmapType *) symbol->value);
      return MrmSUCCESS;
    }
  return MrmNOT_FOUND;
}

void MakePixmap(Display *d, Pixmap *pixmap, PixmapType *data)
{
  char *temp, *value;
  char buffer[256];
  int fileno;
  ColorTableHeaderType *ColorTableHeader;
  ColorTableType *ColorTable;
  XmXpmAttributes attributes;
  char Header[256];
  char **andrew = NULL;
  int NumColors;
  int i;
  SymbolTableType *symbol;
  XImage *im, *imshape;

  attributes.valuemask = 0;
  temp = data->ColorTable;
  if (NULL == (symbol = LookupValue(GlobalSymbolTable, temp))){
    for (fileno = 0; fileno < NumberFiles; fileno++) { 
      if ( NULL != 
	  (symbol = LookupValue(UilModule[fileno].SymbolTable, temp)))
	break;
    }
    if (NULL == symbol)
      Exit(LOC, "Couldn't find %s in symbol tables\n",temp);
  }
  ColorTableHeader = (ColorTableHeaderType *)symbol->value;
  ColorTable = ColorTableHeader->ColorTable;
  NumColors = ColorTableHeader->NumColors;
  if (NULL == ColorTable[0].StringXPM) {
    for (i = 0; i < NumColors; i++) {
      value = ColorTable[i].Color;
      if (ColorTable[i].IsAddress) {
	if (NULL == (symbol = LookupValue(GlobalSymbolTable, value))){
	  for (fileno = 0; fileno < 3; fileno++) { /* FIX ME 3-> */
	    if ( NULL != 
		(symbol =LookupValue(UilModule[fileno].SymbolTable,value)))
	      break;
	  }
	  if (NULL == symbol)
	    Exit(LOC, "Couldn't find %s in symbol tables\n",value);
	}
	value = symbol->value;
      }
      sprintf(buffer, "%s     c %s",ColorTable[i].Reference, value);
      ColorTable[i].StringXPM = Store(buffer);
    }
  }
  andrew = (char **)malloc(sizeof(char *) * (1 + NumColors + data->width));
  sprintf(Header,"%d %d %d 1 -1 -1",data->width, data->height, NumColors);
  andrew[0] = Header;
  for (i = 0; i < NumColors; i++)
    andrew[i+1] = ColorTable[i].StringXPM;
  for (i = 0; i < data->width; i++)
    andrew[i+NumColors + 1] = &data->data[i*data->width];

  /* the following is a total hack because I want to go to bed.  The Pixel
   * value in XmGetPixmapByDepth are totally made up. */
  if (_XmXpmCreateImageFromData(d, andrew, &im, &imshape, &attributes) ==
      XmXpmSuccess) {
    GC gc;

    *pixmap = XCreatePixmap(d, window, im->width, im->height, im->depth);

    gc = XCreateGC(d, window, 0, NULL);

    XPutImage(d, *pixmap, gc, im, 0, 0, 0, 0, im->width, im->height);

    XFreeGC(d, gc);

    /* this should probably be enabled */
#if 0
    XDestroyImage(im);
    if (imshape)
      XDestroyImage(imshape);
#endif

    _XmXpmFreeAttributes(&attributes);
  }

  free(andrew);
}

Cardinal MrmFetchColorLiteral(MrmHierarchy hierarchy_id, String index,
			      Display *display, Colormap cmap,
			      Pixel *pixel_return)
{
  return MrmFAILURE;
}

Cardinal MrmFetchLiteral(MrmHierarchy hierarchy_id, String index, Display *d, XtPointer *value, MrmCode *type)
{
  SymbolTableType *symbol;
/*
   MrmBAD_HIERARCHY
   MrmNOT_FOUND
   MrmWRONG_TYPE
   MrmFAILURE
*/

  for (symbol = GlobalSymbolTable; symbol; symbol = symbol->Next) {
    if (strcmp(index, symbol->Name) == 0) {
      *value = (XtPointer ) &symbol->value;
#if 0
      if (*type != symbol->type)
	return MrmWRONG_TYPE;
#endif
      return MrmSUCCESS;
    }
  }
  return MrmNOT_FOUND;
}
 
Cardinal MrmFetchWidgetOverride(MrmHierarchy hierarchy_id, String index, Widget parent_widget, String override_name, ArgList override_args, Cardinal override_num_args, Widget *widget, MrmType *class)
{
  Cardinal returnValue = 
    MrmFetchWidget(hierarchy_id, index, parent_widget, widget,class);
  XtSetValues(*widget,override_args,override_num_args);
  return returnValue;
}

static int FetchCount = 0;
SymbolTableType *InstantationSymbolTable = NULL;

Cardinal MrmFetchWidget(MrmHierarchy hierarchy_id, String index, Widget parent_widget, Widget *widget, MrmType *class)
{
  DummyWidget *DW = NULL;
  int this_id = -1, child_id = -1;  
  int i = 0;
  int k, n;
  int module_id = 0;
  Widget child;
  MrmType type;
  Bool Found = False;
  int file;
  Arg *args, ARGS[MAX_ARGS];
  int SizeArgs = 0;
  MyHierStruct *MyHier = (MyHierStruct *) hierarchy_id;
  char *CreateName;
  int CreateClass;

  FetchCount++;
  if (1 == FetchCount) {
    FreeSymbolTable(InstantationSymbolTable);
    InstantationSymbolTable = NULL;
  }
  parent = parent_widget;

  for (file = 0; !Found && (file < NumberFiles); file++) {
    for ( k = 0; k < UilModule[file].NumberOfWidgets; k++){
      if (strcmp(index,UilModule[file].DummyWidgetArray[k].Name) == 0) {
	module_id = file;
	Found = True;
	DW = &UilModule[file].DummyWidgetArray[k];
	this_id = k;
	break;
      }
    }
  }
fprintf(stderr,"create %s\n",DW->Name);
if (strcmp(DW->Name,"view_anim") == 0)
fprintf(stderr,"separatorFrame\n");

  if ((FetchCount != 1) && (NULL != hierarchy_id)) {
    args = MyHier->args;
    n = MyHier->argCount;
    SizeArgs = MyHier->SizeArgs;
    CreateName = MyHier->CreateName;
    CreateClass = MyHier->CreateClass;
  }
  else {
    CreateName = DW->Name;
    CreateClass = DW->class;
    n = 0;
    args = ARGS;
    SizeArgs = 0;
  }

  if (!Found) {
    fprintf(stderr,"Fetch return Not FOUND\n");
    FetchCount--;
    return (MrmNOT_FOUND);
  }

  GetAttributes(/*display*/ NULL, /*window*/ 0, 
		module_id, &n, args,
		DW, InstantationSymbolTable, ON_CREATE);

  if (DW->NumberOfInherit) {
    for (i = 0; i < DW->NumberOfInherit; i++) {
      char *index;
      int inherit_id;
      MyHierStruct *Hier;

      if (NULL == MyHier) {
	Hier = (MyHierStruct *) malloc(sizeof(MyHierStruct));
	Hier->CreateName = DW->Name;
	Hier->CreateClass = DW->class;
	Hier->args = args;
	Hier->argCount = n;
	Hier->SizeArgs = SizeArgs;
      }
      else
	Hier = MyHier;
      inherit_id = DW->inherit[i];
      index = UilModule[module_id].DummyWidgetArray[inherit_id-1].Name;

      MrmFetchWidget((MrmHierarchy)Hier, index, parent_widget, widget, class);
    }
  }
  else {
    if (XM_LIST_INDEX == CreateClass) {
      Xm_List(parent_widget, DW, module_id);
      *widget = NULL;
      FetchCount--;
      return MrmSUCCESS;
    }
  
    if (NULL == CreateFunctions[CreateClass].function) {
      char Name[20];
      strcpy(Name, &CreateFunctions[CreateClass].Name[2]);
      Name[0] = '*';
      *widget = XtNameToWidget(parent_widget, Name);
    }
    else {
      SymbolTableType *new =(SymbolTableType *)malloc(sizeof(SymbolTableType));
      
      *widget =CreateFunctions[CreateClass].function(parent_widget,
						     CreateName,args, n);
      new->value = (char *)*widget;
      new->Name = Store(CreateName);
      new->Next = InstantationSymbolTable;
      InstantationSymbolTable = new;
      new->type = ANOTHER_WIDGET;
    }
  }
  if (NULL == *widget){
    Exit(LOC, "Couldn't create widget %s",DW->Name);
  }
  UilModule[module_id].WidgetArray[this_id] = *widget;
  for (i = 0; i < DW->NumberOfChildren; i++) {
    Cardinal returnCode;
    DummyWidget *ChildDummy;

    child_id = DW->children[i];
    ChildDummy = &UilModule[module_id].DummyWidgetArray[child_id-1];

    if (MrmSUCCESS != 
	(returnCode = 
	 MrmFetchWidget(NULL, ChildDummy->Name,
			UilModule[module_id].WidgetArray[this_id],
				     &child, &type))) { 
      FetchCount--;
      return (returnCode); 
    }

    if ((0 == strcmp(CreateFunctions[DW->class].Name,"XmCascadeButton")) ||
	(0 == strcmp(CreateFunctions[DW->class].Name,"XmOptionMenu"))) {
      Arg args[1];

      XtSetArg(args[0], XmNsubMenuId, child);
      XtSetValues(UilModule[module_id].WidgetArray[this_id],args,1);
    }
    if (child && UilModule[module_id].DummyWidgetArray[child_id-1].managed) {
        XtManageChild(child); 
    }
  }

  n = 0;
  GetAttributes(/*display*/ NULL, /*window*/ 0, 
		module_id, &n, args,
		DW, InstantationSymbolTable, LATE);

  XtSetValues(*widget, args, n);
  GetCallbacks(module_id, this_id, DW);
  UilModule[module_id].WidgetArray[this_id] = *widget;
  AddGeometryAttributes(&GeometryList, module_id, this_id, DW);
  if (1 == FetchCount) {  /* Just before we return to initial caller do GEOM */
    SetGeometryAttributes(&GeometryList);
  } 
  FetchCount--;
  return MrmSUCCESS;
}

void AddGeometryAttributes(GeoType **GeometryList, int module_id, int this_id, 
		      DummyWidget *DW)
{
  GeoType *node = (GeoType *) malloc(sizeof(GeoType));
  node->module_id = module_id;
  node->this_id = this_id;
  node->next = *GeometryList;
  *GeometryList = node;
  node->DW = DW;
}

void SetGeometryAttributes(GeoType **GeometryList)
{
  GeoType *node = *GeometryList;
  GeoType *next;
  Arg args[MAX_ARGS];
  int n;
  Widget widget;

  while (node) {
    next = node->next;
    widget = UilModule[node->module_id].WidgetArray[node->this_id];
    n = 0;
    GetAttributes(/*display*/ NULL, /*window*/ 0, 
		  node->module_id, &n, args,
		  node->DW, InstantationSymbolTable, GEO);
    XtSetValues(widget, args, n); 
    free(node);
    node = next;
  }
  *GeometryList = NULL;
}
  
void GetCallbacks(int fileno, int wid_id, DummyWidget *DW)
{
  callbackType *Next = DW->callbacks;
  XtPointer ParameterValue;
  SymbolTableType *SymbolTable = NULL;
  Widget w = UilModule[fileno].WidgetArray[wid_id];

  while (Next) {
    if (Next->Parameters) {
      if (MrmRtypeAddrName == Next->Parameters->type) {
	char *temp;
	
	temp = Next->Parameters->value;
    	if ( 1 != LookUpPredefines(temp, (long *)&ParameterValue ))
	if ( NULL == 
	    (SymbolTable = LookupValue(UilModule[fileno].SymbolTable, temp))){
	  if (NULL == (SymbolTable = LookupValue(GlobalSymbolTable, temp))){
	    Exit(LOC, "Couldn't find %s in symbol tables\n",temp);
	  }
	}
      }
      else
	SymbolTable = (XtPointer) (Next->Parameters);
    }
    else
      ParameterValue = NULL;
    if (SymbolTable) {
      if (MrmRtypeInteger != SymbolTable->type)
	ParameterValue = (XtPointer) SymbolTable->value;
      else
	ParameterValue = (XtPointer) &( SymbolTable->value);
    }
    if (strcmp("createCallback", Next->argName) != 0)
      XtAddCallback(w, Next->argName,
		  (XtCallbackProc) LookUpFunction(Next->functionName),
		  ParameterValue);
    else
      ((XtCallbackProc)
	LookUpFunction(Next->functionName)) (w,ParameterValue,NULL);
    Next = Next->next;
  }
  return;
}

void GetAttributes(Display *d,Window w, int module_id, 
		   int *n, Arg *args,
		   DummyWidget *DW, SymbolTableType 
		   *FormSymbolTable, int State)
{
  int count = 0;
  XtArgVal value;
  SymbolTableType *Next = DW->attributes;
  int Do;
  
/*  *n = 0;   We will append to existing attributes - make sure n is 0 
              before calling this routine if that is intended 
*/
  while (Next) {
    if (Next->Access&S) {  /* Attribute can be set after widget creation */
      Do = (LATE == State);
    }
    else if (Next->Access&C) { /* Attribute must be set at creation time */
      Do = (ON_CREATE == State);
    }
    else {              /* Special '0' case - very late attribute setting*/
      Do = (GEO == State);
    }
    if (Do) {
      count = GetSymbolTableValue(&value, Next, module_id, FormSymbolTable);
      XtSetArg(args[*n], Next->Name, value);
      (*n)++;
      if (*n >= MAX_ARGS)
	Exit(LOC, "Too many args\n");
      if (count > 0) {        /* Certain attributes have a count associated */
	char buff[256];       /* with them.  This is a temporary(?) way to  */
	                      /* identify them and automatically include it */
	if (0 == strcmp(Next->Name,XmNitems))
	  strcpy(buff, XmNitemCount);
#if XmVERSION > 1
	else if (0 == strcmp(Next->Name,XmNdetail))
	  strcpy(buff, XmNdetailCount);
	else if (0 == strcmp(Next->Name,XmNdetailColumnHeading))
	  strcpy(buff, XmNdetailColumnHeadingCount);
	else if (0 == strcmp(Next->Name,XmNvalues))
	  strcpy(buff, XmNnumValues);
#endif
	else
	  Warn(LOC, "Can't find %s\n",Next->Name);
	XtSetArg(args[*n], buff, count); (*n)++;
	if (*n >= MAX_ARGS)
	  Exit(LOC, "You guessed it - Too many args\n");
      }
    }
    Next = Next->Next;
  }
}

int GetSymbolTableValue(XtArgVal *value, SymbolTableType *Next, 
			     int module_id, SymbolTableType *FormSymbolTable)
{
  int status = 0;
  char *temp;
  BitMapType *bitmap;
  SymbolTableType *SymbolTable;

  switch(Next->type) {
  case MrmRtypePixmapImage: {
    PixmapType *data;
/*    GC theGC = DefaultGC(display, 0); */
    char *Image;
    Pixel fg,bg;
    Pixmap icon;
/*    int i,j; */

    data = (PixmapType *) Next->value;
    Image = data->data;
    XtVaGetValues(parent, XmNforeground, &bg, XmNbackground, &fg, NULL, NULL);
    MakePixmap(display, &icon, data);
/*
    icon = XCreatePixmap(display, window, data->width, data->height, 
			 DefaultDepth(display,0));
    for (i = 0; i < data->height; i++)
      for (j = 0; j < data->width; j++)
	if (*Image++ == ' ')
	  XDrawPoint(display, icon, theGC, j, i);
	else {
	  XSetForeground(display, theGC, bg);
	  XDrawPoint(display, icon, theGC, j, i);
	  XSetForeground(display, theGC, fg);
	}
*/
    *value = (long) icon;

    break;
  }
  case MrmRtypeXBitmapFile: 
    bitmap = (BitMapType *) Next->value;;
    *value = (long)XCreateBitmapFromData(display,window, 
					bitmap->data,
					bitmap->width, bitmap->height);
    break;
  case MrmRtypeKeysym: {
    char *sym = (char *) Next->value;
    if ('~' == sym[0]) {
      temp = &sym[1];
      if ( NULL == 
	  (SymbolTable = LookupValue(UilModule[module_id].SymbolTable, temp))){
	if (NULL == (SymbolTable = LookupValue(GlobalSymbolTable, temp))){
	  if (NULL == (SymbolTable = LookupValue(FormSymbolTable,temp))) {
	    Exit(LOC, "Could not find %s\n", temp);
	  }
	}
      }
      *value = XStringToKeysym(SymbolTable->value);
    }
    else
      *value = XStringToKeysym(Next->value);
    break;
  }
  case MrmRtypeInteger:
  case MrmRtypeBoolean:
    *value = (long) Next->value;
    break;
  case MrmRtypeFont:{
    XFontStruct *theFont = XLoadQueryFont(display,Next->value);
    XmFontListEntry theEntry;

    if (NULL == theFont)
      Exit(LOC, "Can't load font %s\n", Next->value);
    theEntry = XmFontListEntryCreate(XmFONTLIST_DEFAULT_TAG,
						     XmFONT_IS_FONT, theFont);
    *value = (long) XmFontListAppendEntry(NULL, theEntry);
    break;
  }
  case MrmRtypeColor: {
    XColor exact, def;

    if (XAllocNamedColor(display, colormap, Next->value, &def, &exact)) {
      *value = (XtArgVal)def.pixel; 
    }
    else
      Exit(LOC, "Couldn't resolve color: %s\n",Next->value);
    break;
  }
  case MrmRtypeAddrName:
    temp = Next->value;
    if ( 1 == LookUpPredefines(temp, value))
	break;
    if ( NULL == 
	(SymbolTable = LookupValue(UilModule[module_id].SymbolTable, temp))){
      if (NULL == (SymbolTable = LookupValue(GlobalSymbolTable, temp))){
	if (NULL == (SymbolTable = LookupValue(FormSymbolTable,temp))) {
	  Exit(LOC, "Could not find %s\n", temp);
	}
      }
    }
    status=GetSymbolTableValue(value, SymbolTable, module_id, FormSymbolTable);
    break;
  case MrmRtypeChar8Vector:{
    XmString *strings;
    char **array;
    char **temparray;
    int i = 0;
    
    array = (char **) Next->value;
    for (temparray = array; *temparray; temparray++) i++;
    strings = (XmString *) malloc(i*sizeof(XmString*));
    for (i = 0; *array; array++,i++){
      char *sym;
      sym = *array;
      if ('~' == sym[0]) {
	temp = &sym[1];
	if ( NULL == 
	    (SymbolTable=LookupValue(UilModule[module_id].SymbolTable, temp))){
	  if (NULL == (SymbolTable = LookupValue(GlobalSymbolTable, temp))){
	    if (NULL == (SymbolTable = LookupValue(FormSymbolTable,temp))) {
	      Exit(LOC, "Could not find %s\n", temp);
	    }
	  }
	}
	strings[i] = XmStringCreate(SymbolTable->value,XmFONTLIST_DEFAULT_TAG);
      }
      else
	strings[i] = XmStringCreate(*array, XmFONTLIST_DEFAULT_TAG);
    }
    *value = (XtArgVal) strings;
    status = i;
    break;
  }
  case MrmRtypeChar8:
    *value = (XtArgVal) XmStringCreateSimple(Next->value); 
    break;
  case MrmRtypeCString:
    *value = (XtArgVal) DecodeComplexString(Next->value);
    break;
  case ANOTHER_WIDGET:  /* Another Widget */
    *value = (XtArgVal) (Next->value);
    break;
  default:
    Exit(LOC, "UNKNOWN ATTRIBUTE: %d\n", Next->type);
  }
  return status;
}

XmString DecodeComplexString(char *value)
{   
  char *source = (char *) value;
  int i;
  
/*  a complex string is represented in the uid file as a post-fix
    'polish notation' equation.
*/
/*  COMPLEX_STRING = STRING COMPLEX_STRING      */
/*                 | COMMAND COMPLEX_STRING     */
/*                 | ZERO                       */
/*  STRING = " [^"]* "                          */
/*  COMMAND = C | S                             */

  for (i = 0; source[i]; i++ ) {
    switch (source[i]) {
    case ('"'): {
      char *string1 = &source[i+1];
      for(i=i+1;source[i] && (source[i] != '\"');i++)
	;
      source[i] = 0;
      push((char *)XmStringCreate(string1,XmFONTLIST_DEFAULT_TAG));
      break;
    }
    case ('S'): {  /* Code meaning Append a Separator */
      XmString string1 = (XmString) pop();
      push((char *)XmStringConcat(string1,XmStringSeparatorCreate()));
      break;
    }
    case ('C'): {  /* Concatenate two strings at top of stack */
      XmString string1 = (XmString) pop();
      XmString string2 = (XmString) pop();
      
      push((char *)XmStringConcat(string2, string1));
      break;
    }
    }
  }
  return (XmString) pop();
}

SymbolTableType *GetSymbolTable(SymbolTableType *Table)
{
  char name[256], ch, *temp;
  int i,j;
  SymbolTableType *NewTable = NULL;
  SymbolTableType *LastTable = NULL;

  if (Table)
    for (LastTable = Table; LastTable->Next; LastTable = LastTable->Next)
      ;
  for (ch = getchar(); ch; ch = getchar()) {
    for (i = 0,name[i] = ch; name[i] != '"'; i++,name[i] = getchar()) 
      ;
    name[i] = 0;
    NewTable = (SymbolTableType *)malloc(sizeof(SymbolTableType));
    NewTable->Next = NULL;
    if (LastTable) {
      LastTable->Next = NewTable;
    }
    else if (NULL == Table)
      Table = NewTable;
    LastTable = NewTable;
    NewTable->Name = Store(name);
    NewTable->type = getchar();
    switch(NewTable->type) {
    case MrmRtypeInteger:
    case MrmRtypeBoolean:
      fread(&NewTable->value,sizeof(long), 1, stdin);
      break;
    case MrmRtypeXBitmapFile: {
      int SizeOfData;
      BitMapType *bitmap;

      bitmap = (BitMapType *)malloc(sizeof(BitMapType));
      fread(bitmap, sizeof(BitMapType)-sizeof(long), 1, stdin);
      SizeOfData = bitmap->width*bitmap->height>>3;
      bitmap->data = (char *)malloc(SizeOfData);
      fread(bitmap->data, sizeof(char), SizeOfData, stdin);
      NewTable->value = (char *) bitmap;
      break;
    }
    case MrmRtypeChar8Vector:{
      char *last;
      char **array = NULL;
      int size = 0;
      int index = 0;
      temp = last = &name[0];
      while ((*temp = getchar())) {
	if ('"' == *temp) {
	  *temp = 0;
	  if (size <= index) {
	    size += 10;
	    array = (char **)realloc(array, size * sizeof(char *));
	  }
	  array[index] = Store(last);
	  index++;
	  last = temp+1;
	}
	temp++;
      }
      array[index] = NULL;
      NewTable->value = (char *) array;
      break;
    }
    case MrmRtypeKeysym:
    case MrmRtypeChar8:
      temp = &name[0];
      while ((*temp++ = getchar()));
      NewTable->value = (char *) Store(name);
      break;
    case MrmRtypeCString: {
      char temp[2048];
      int i;

      for (i = 0; (i < 2048) && (temp[i] = getchar()); i++);
      if (2048 == i)
	Exit(LOC, "Static storage area exhausted\n");
      NewTable->value = (char *) Store(temp);
      break;
    }
    case MrmRtypeColorTable: {
      ColorTableType *ColorTable;
      ColorTableHeaderType *ColorTableHeader;
      int NumColors;
      char type; 
      char temp[2048];
      fread(&NumColors,sizeof(int), 1, stdin);
      ColorTable = (ColorTableType *)malloc(sizeof(ColorTableType)*NumColors);
      ColorTableHeader = (ColorTableHeaderType *)
	malloc(sizeof(ColorTableHeader));
      ColorTableHeader->ColorTable = ColorTable;
      ColorTableHeader->NumColors = NumColors;
      for (j = 0; j < NumColors; j++) {
	for (i = 0; (i < 2048) && (temp[i] = getchar()); i++);
	if (2048 == i)
	  Exit(LOC, "Static storage area exhausted\n");
	ColorTable[j].Reference = Store(temp);
	fread(&type,sizeof(type), 1, stdin);  
	ColorTable[j].IsAddress = (MrmRtypeAddrName == type);
	ColorTable[j].StringXPM = NULL;
	for (i = 0; (i < 2048) && (temp[i] = getchar()); i++);
	if (2048 == i)
	  Exit(LOC, "Static storage area exhausted\n");
	ColorTable[j].Color = Store(temp);
      }
      NewTable->value = (char *)ColorTableHeader;
      break;
    }
    case MrmRtypeFont:
    case MrmRtypeColor: {
      char temp[2048];
      int i;
      
      for (i = 0; (i < 2048) && (temp[i] = getchar()); i++);
      if (2048 == i)
	Exit(LOC, "Static storage area exhausted\n");
      NewTable->value = Store(temp);
      break;
    }
    case MrmRtypePixmapImage: {
      PixmapType *pixmap;
      int SizeOfData;
      int i;
      char temp[2048];

      pixmap = (PixmapType *)malloc(sizeof(PixmapType));
      fread(&pixmap->width, sizeof(int), 1, stdin);
      fread(&pixmap->height, sizeof(int), 1, stdin);
      SizeOfData = pixmap->width*pixmap->height;
      for (i = 0; (i < 2048) && (temp[i] = getchar()); i++);
      if (2048 == i)
	Exit(LOC, "Static storage area exhausted\n");
      pixmap->ColorTable = Store(temp);
      pixmap->data = (char *)malloc(SizeOfData);
      fread(pixmap->data, sizeof(char), SizeOfData, stdin);
      NewTable->value = (char *) pixmap;
      break;
    }
    case MrmRtypeAddrName: {
      int i;
      char temp[2048];

      for (i = 0; (i < 2048) && (temp[i] = getchar()); i++);
      if (2048 == i)
	Exit(LOC, "Static storage area exhausted\n");
      NewTable->value = Store(temp);
      break;
    }
    default:
      Exit(LOC, "UNKNOWN ATTRIBUTE: code = %d\n",NewTable->type);
    }
  }
      
  if (NULL == Table)
    return NewTable;
  else
    return Table;
}

SymbolTableType *
LookupValue(SymbolTableType *SymbolTable, char *name)
{
  while (SymbolTable) {
    if (0 == strcmp(SymbolTable->Name,name))
      return SymbolTable;
    else
      SymbolTable = SymbolTable->Next;
  }
  return NULL;
}

void PrintWidgetTree(UilModuleType *Module)
{
  PrintWidgets(Module);
}
/* LIST SYMBOL_TABLE(Global) SYMBOL_TABLE(local) WIDGETS */
void ReadWidgetTree(UilModuleType *Module)  
{
  ReadList(Module);
  GlobalSymbolTable = 
    GetSymbolTable(GlobalSymbolTable); 
  Module->SymbolTable = 
    GetSymbolTable(NULL);
  ReadWidgets(Module);
}

/* LIST = INT(count) NAMES  CHAR(0) */
void ReadList(UilModuleType *Module)
{
  int NumberOfWidgets;
  int i; 
  char c;

  fread(&NumberOfWidgets,sizeof(int), 1, stdin);  /* INT(widget count)  */
  fread(&MaxAttributes,sizeof(int), 1, stdin);    /* INT(max attributes)  */

  Module->NumberOfWidgets = NumberOfWidgets;
  Module->Definitions = (char **) malloc(sizeof(char *) * NumberOfWidgets);
  Module->WidgetArray = (Widget *) malloc(sizeof(Widget) * NumberOfWidgets);
  Module->DummyWidgetArray = (DummyWidget *) 
    malloc(sizeof(DummyWidget) * NumberOfWidgets);

  for (i = 0; (((c = getc(stdin)) != EOF) && c);i++) { /* NAMES CHAR(0)   */
    char string[MAX_STR];
    int j = 0;

    for(j = 0; (c != EOF) && (c != '"'); j++){ /* NAMES = NAME "  */
      string[j] = c;                                 /* NAME = [^"]*    */
      if (j >= MAX_STR)
	Exit(LOC, "String def too long\n");
      c = getc(stdin);
    }
    string[j] = 0;
    Module->DummyWidgetArray[i].Name = Store(string);
  }
  if (c) 
    Exit(LOC, "ERROR in header\n");
}

void PrintWidgets(UilModuleType *Module)
{
  DummyWidget *DW;
  int i;

  for (i = 0; i < Module->NumberOfWidgets; i++) {
    DW = &Module->DummyWidgetArray[i];
    fprintf(stderr,"oject %s: %s {\n",DW->Name,
	    CreateFunctions[DW->class].Name);
    PrintArgs(DW->attributes);
    PrintControls(DW->NumberOfChildren,DW->children, Module->DummyWidgetArray);
    PrintCallbacks(DW->callbacks);
    fprintf(stderr,"};\n");
  }
}

void PrintArgs(SymbolTableType *Next)
{
  fprintf(stderr,"\targuments {\n");
  while (Next) {
    switch (Next->type) {
    case MrmRtypeInteger:
    case MrmRtypeBoolean:
      fprintf(stderr,"\t\t%s = %d;\n",Next->Name,(int)Next->value);
      break;
    case MrmRtypeKeysym:
    case MrmRtypeChar8:
    case MrmRtypeAddrName:
    case MrmRtypeCString:
      fprintf(stderr,"\t\t%s = \"%s\";\n",Next->Name,Next->value);
      break;
    default:
      fprintf(stderr,"!  ????  %s \n",Next->Name);
      break;
    }
    Next = Next->Next;
  }
  fprintf(stderr,"\t};\n");
}

void PrintControls(int count, int *ids, DummyWidget *Array)
{
  int i; 
  DummyWidget *DW;

  fprintf(stderr,"\tcontrols {\n");
  for (i = 0; i < count; i++){
    DW = &Array[ids[i]-1];
    fprintf(stderr,"\t\t%s %s;\n", CreateFunctions[DW->class].Name, DW->Name);
  };
  fprintf(stderr,"\t}\n");
}

void PrintCallbacks(callbackType *Next)
{
  SymbolTableType *NextParam;

  fprintf(stderr,"\tcallbacks {\n");
  while (Next) {
    fprintf(stderr,"\t\t%s = procedure %s(",
	    Next->argName, Next->functionName);
    for (NextParam = Next->Parameters; NextParam; NextParam = NextParam->Next)
      switch (NextParam->type) {
      case MrmRtypeInteger:
      case MrmRtypeBoolean:
	fprintf(stderr,"%d",(int) NextParam->value);
	break;
      case MrmRtypeKeysym:
      case MrmRtypeAddrName:
      case MrmRtypeCString:
      case MrmRtypeChar8:
	fprintf(stderr,"%s",NextParam->value);
	break;
      default:
	break;
      }
    fprintf(stderr,");\n");
    Next = Next->next;
  };
  fprintf(stderr,"\t};\n");
}
/*
// WIDGETS = WIDGET WIDGETS
//         | NULL
*/
void ReadWidgets(UilModuleType *Module)
{
  char c;
  int WidgetIndex;
  DummyWidget *DW;
  int i, index, size;
  int this_id, child_id,inherit_id;
  char temp[256];

/*
   In the following definition:

   integer size = 4 bytes
   
   INT(0) - an integer sized zero to mark
   the end of a list  e.g. INHERIT is a list of inherited widgets but
   if a widget id of zero is read, we know we have come to the end of the list
   
   CHAR(MrmTypeClassRecName) - a byte sized value of 22 (MrmTypeClassRecName) 
   to indicate the start of a widget.
*/
  
/* WIDGET = CHAR(MrmTypeClassRecName) NAME(widget's name) " INT(id) INT(class) 
//           CHAR(managed) INT(parent) INHERIT INT(0) CHILDREN INT(0) 
//           CALLBACKS TABLE(attributes)
*/
  for (c = getc(stdin), WidgetIndex = 0;
       c == MrmRtypeClassRecName;
       c = getc(stdin),WidgetIndex++) {  /* CHAR(MrmTypeClassRecName) */
    for (i = 0,c = getchar(); c != '"'; c = getchar(),i++) { /* NAME " */
      temp[i] = c; 
    }
    temp[i] = 0;
    DW = &Module->DummyWidgetArray[WidgetIndex];
    DW->Name = Store(temp);
if (strcmp(DW->Name, "templatePick") == 0)
fprintf(stderr,"STOP\n");
    fread(&this_id, sizeof(this_id), 1, stdin);       /*INT(id)    */
    if (this_id != (WidgetIndex+1))
      Exit(LOC, "INDEX ERROR:widgets should be stored in file by id number\n");
    fread(&DW->class, sizeof(DW->class), 1, stdin);     /*INT(class) */
    fread(&DW->managed, sizeof(DW->managed), 1, stdin); /*CHAR(managed) */
    fread(&DW->parent, sizeof(DW->parent), 1, stdin);   /*INT(parent)*/
    DW->inherit = NULL;
    size = 0;
    index = 0;
    for (fread(&inherit_id,sizeof(inherit_id), 1, stdin); /*INHERIT = INT INH*/
	 inherit_id;                                      /*INT(0)  */
	 fread(&inherit_id,sizeof(inherit_id), 1, stdin)) {
      if (size <= index) {
	size += 10;
	DW->inherit = (int *) realloc(DW->inherit, size * sizeof(inherit_id));
      }
      DW->inherit[index] = inherit_id;
      index++;
    }
    DW->NumberOfInherit = index;
    DW->children = NULL, size = 0, index = 0;
    for (fread(&child_id, sizeof(child_id), 1, stdin);/*CHILDREN=INT CHILDREN*/
	 child_id;                                    /*INT(0) */
	 fread(&child_id, sizeof(child_id), 1, stdin)) {
      if (size <= index) {
	size += 10;
	DW->children = (int *)realloc(DW->children, size * sizeof(child_id));
      }
      DW->children[index] = child_id;
      index++;
    }
    DW->NumberOfChildren = index;
    ReadCallbacks(&(DW->callbacks));
    DW->attributes = ReadAttributes();
  }
}

/* CALLBACKS = CHAR(MrmRtypeCallback) INT(id) INT(index) NAME(argname) " 
//               NAME(function name) " CHAR(# of params) PARAMS CALLBACKS
//           | NULL
// PARAMS = CHAR(type) INT(value) PARAMS      integer,bool
//        | CHAR(type) STRING CHAR(0) PARAMS   AddrName,CString,String,keysym
*/
void ReadCallbacks(callbackType **first)
{
  char c,temp[MAX_COMPLEX_STRING];
  int i, j;
  SymbolTableType **ParamPointer;
  char NumberOfParams;
  callbackType *last;
  callbackType *new;

  *first = last = NULL;
  c = getchar();
  for (; c == MrmRtypeCallback; c = getchar()) { /* CHAR(MrmRtypeCallback) */
    new = (callbackType *) malloc(sizeof(callbackType));
    new->Parameters = NULL;
    new->next = NULL;
    if (last)
      last->next = new;
    last = new;
    if (NULL == *first)
      *first = new;
    fread(&new->id, sizeof(new->id), 1, stdin);      /* INT(id)     */
    for (i = 0,c = getchar(); c != '"'; c = getchar())       /* NAME(argname)*/
      temp[i++] = c;
    temp[i] = 0;
    new->argName = Store(temp);
    for (i = 0,c = getchar(); c != '"'; c = getchar())/* NAME(function name) */
      temp[i++] = c;
    temp[i] = 0;
    new->functionName = Store(temp);
/* PARAMS = CHAR(type) INT(value) PARAMS      integer,bool       */
/*        | CHAR(type) STRING CHAR(0) PARAMS   AddrName,CString,String,keysym*/
    NumberOfParams = getchar();
    ParamPointer = &(new->Parameters);
    new->Parameters = NULL;
    for (i = 0; i < NumberOfParams; i++) {
      *ParamPointer = (SymbolTableType *) malloc(sizeof(SymbolTableType));
      (*ParamPointer)->type = getchar();
      switch ((*ParamPointer)->type) {
      case MrmRtypeInteger:
      case MrmRtypeBoolean:/* INT(value)*/
	fread(&((*ParamPointer)->value), sizeof((*ParamPointer)->value), 
	      1, stdin);
	break;
      case MrmRtypeKeysym: 
      case MrmRtypeColor:
      case MrmRtypeAddrName:
      case MrmRtypeCString:
      case MrmRtypeChar8:    /* STRING(value) CHAR(0) */
	for (j = 0; (j < MAX_COMPLEX_STRING) && (temp[j] = getchar()); j++) ;
	if (MAX_COMPLEX_STRING == j){
	  temp[256] = 0;  /* truncate */
	  Exit(LOC, "String too complex: %s...", temp);
	}
	(*ParamPointer)->value = Store(temp);
	break;
      }
      (*ParamPointer)->Next = NULL;
      ParamPointer = &((*ParamPointer)->Next);
    }
  }
  ungetc(c,stdin);
}

/*  TABLE(attributes) = EXPRESSION TABLE(attributes)
//                    | NULL
*/
SymbolTableType *ReadAttributes()
{
  char c;
  int i;
  char temp[MAX_COMPLEX_STRING];
  SymbolTableType *Return = NULL, *Next, *Last = NULL;

/* EXPRESSION = CHAR(MrmRtypeResource) NAME " CHAR(type) INT(value)
//        | CHAR(MrmRtypeResource) NAME " CHAR(type) NAME(string value) CHAR(0)
//        | CHAR(MrmRtypeResource) NAME " CHAR(type) NAME(bitmap value) CHAR(0)
//        | CHAR(MrmRtypeResource) NAME " CHAR(type) NAME(pixmap value) CHAR(0)
*/
  for (c = getchar(); 
       c == MrmRtypeResource; 
       c = getchar()) { /* CHAR(MrmRtypeResource) */
    Next = (SymbolTableType *) malloc(sizeof(SymbolTableType));
    Next->Next = NULL;
    if (NULL == Last)
      Return = Next;
    else
      Last->Next = Next;
    fread(&(Next->Index), sizeof(Next->Index), 1, stdin);
    GetArgValues(Next->Index, &Next->Name, &Next->Access);
    Next->type = getchar();                   /* CHAR(type) */
    switch(Next->type) {
    case MrmRtypePixmapImage: {
      int SizeOfData;
      PixmapType *pixmap;
      char temp[2048];
      int i;

      pixmap = (PixmapType *) malloc(sizeof(PixmapType));
      fread(pixmap, sizeof(PixmapType)-sizeof(long), 1, stdin);/*NAME(pixmap)*/
      for (i = 0; (i < 2048) && (temp[i] = getchar()); i++);  /*NAME(colortab*/
      if (2048 == i)
	Exit(LOC, "Static storage area exhausted\n");
      pixmap->ColorTable = Store(temp);
      SizeOfData = pixmap->width*pixmap->height;
      pixmap->data = (char *) malloc(SizeOfData);
      fread(pixmap->data, sizeof(char), SizeOfData, stdin);  /* image data */
      Next->value = (char *) pixmap;
      break;
    }
    case MrmRtypeXBitmapFile: {
      int SizeOfData;
      BitMapType *bitmap;

      bitmap = (BitMapType *)malloc(sizeof(BitMapType));
      fread(bitmap, sizeof(BitMapType)-sizeof(long), 1, stdin);/*NAME(bitmap)*/
      SizeOfData = bitmap->width*bitmap->height>>3;
      bitmap->data = (char *)malloc(SizeOfData);
      fread(bitmap->data, sizeof(char), SizeOfData, stdin);
      Next->value = (char *) bitmap;
      break;
    }
    case MrmRtypeInteger:
    case MrmRtypeBoolean:
      fread(&(Next->value), sizeof(Next->value), 1, stdin);    /* INT(value)*/
      break;
    case MrmRtypeKeysym:
    case MrmRtypeChar8:
    case MrmRtypeColor:
    case MrmRtypeAddrName:
    case MrmRtypeCString:
      i = 0;                   /* NAME(value) CHAR(0) */
      while ((i < MAX_COMPLEX_STRING) && (temp[i++] = getchar()));   
      if (MAX_COMPLEX_STRING == i) {
	temp[256] = 0;
	Exit(LOC, "String too complex: %s",temp);
      }
      temp[i] = 0;
      Next->value = Store(temp);  
      break;
    default:
      Exit(LOC, "Unknown Attribute type: %d\n",c);
    }
    Last = Next;
  }
  ungetc(c,stdin);
  return Return;
}

void Xm_List(Widget parent_widget, DummyWidget *DW, int module_id)
{
  Arg args[10];
  int n = 0;
  int i;
  int ListCount = 0;
  SymbolTableType *Next;
  XtArgVal value;
  char *temp;
  SymbolTableType *SymbolTable;
  XmString *ListItem;

  
  for (Next = DW->attributes; Next; Next = Next->Next) {
    switch(Next->type) {
    case MrmRtypeInteger:
      value = (long) Next->value;
      break;
    case MrmRtypeAddrName:
      temp = Next->value;
      if ( NULL == 
	  (SymbolTable = LookupValue(UilModule[module_id].SymbolTable, temp))){
	if (NULL == (SymbolTable = LookupValue(GlobalSymbolTable, temp))){
	  LookUpPredefines(temp, &value);
	  break;
	}
      }
      if (0 == strcmp(Next->Name,"items")) {
	char **strings = (char **) SymbolTable->value;

	while (*strings++) 
	  ListCount++;
	ListItem = (XmString *) malloc(ListCount * sizeof(XmString));
	strings = (char **) SymbolTable->value;
	for (i = 0; i < ListCount; i++) {
	  char *sym = strings[i];
	  char s[256];

	  if ('~' == sym[0]) {
	    temp = &sym[1];
	    if ( NULL == 
		(SymbolTable= LookupValue(UilModule[module_id].SymbolTable, 
					  temp))){
	      if (NULL ==(SymbolTable =LookupValue(GlobalSymbolTable, temp))){
		Exit(LOC, "Could not find %s\n", temp);
	      }
	    }
	    strcpy(s, SymbolTable->value);
	    ListItem[i] = XmStringCreate(s, XmFONTLIST_DEFAULT_TAG);
	  }
	  else
	    ListItem[i] = XmStringCreate(strings[i], XmFONTLIST_DEFAULT_TAG);
	}
	value = (XtArgVal) ListItem;
      }
      else
	value = (XtArgVal) SymbolTable->value;
      break;
    default:
      Exit(LOC, "UNKNOWN ATTRIBUTE\n");
    }
    XtSetArg(args[n], Next->Name, value); n++;
  };
  XtSetArg(args[n], XmNitemCount, ListCount); n++;
  XtSetValues(XtNameToWidget(parent_widget,"*List"), args, n);
}


FILE *UILOpen(char *FileName,char *mode,FILE *old) 
{
  FILE *Return = NULL;
  int i,j;
  char *theBase;
  char buffer[256];
  static char Eval = 0;
  char *Base[] = {
    "$HOME",
    "/usr/X11/lib/X11"
  };
  char *Paths[] = {
    "%L/uid/%N/%U%S",        
    "%l/uid/%N/%U%S",
    "uid/%N/%U%S",
    "%L/uid/%U%S",
    "%l/uid/%U%S",
    "uid/%U%S",
    "%U%S",
    "%T",
  };
  static int BaseSize = sizeof(Base)/sizeof(Base[0]);
  static int PathSize = sizeof(Paths)/sizeof(Paths[0]);
  char *UILPath;
  
  if (NULL != (Return = freopen(FileName,mode,old))) {
    fprintf(stderr,"Opening %s\n", FileName);
    return Return;
  }
  if (!Eval) {
    Eval = 1;
    for (j = 0; j < BaseSize; j++) {
      theBase = Base[j];
      if ('$' == theBase[0])
	Base[j] = getenv(&theBase[1]);
      fprintf(stderr,"Base %d = %s\n",j, Base[j]);
    }
  }
  UILPath = getenv("UILPATH");
  if (NULL != UILPath) {
    for (i = j = 0; ;j++, i++) {
      if ((':' == UILPath[i]) || (0 == UILPath[i])) {
	buffer[j++] = '/';
	strcpy(&buffer[j],FileName);
	j = -1;
	if (NULL != (Return = freopen(Substitute(buffer),mode,old))) {
	  Warn(LOC,"Opening %s\n", buffer);
	  return Return;
	}
	if (0 == UILPath[i])
	  break;
      }
      else
	buffer[j] = UILPath[i];
    }
  }
  for (j = 1; (NULL == Return) && (j < BaseSize); j++)
    if (Base[j])
      for(i = 0, sprintf(buffer,"%s/%s/%s", Base[j], Paths[i], FileName);
	  (NULL == (Return = freopen(Substitute(buffer),mode,old))) &&
	  ( i < PathSize);
	  i++,sprintf(buffer,"%s/%s/%s", Base[j], Paths[i], FileName)) {
      };
  if (NULL != Return)
    Warn(LOC, "Opening %s\n",buffer);
  return Return;
}

char *Substitute(char *buffer)
{
  static char buffer2[256];
  char *string = buffer2;
  
  while (*buffer) {
    if ('%' == *buffer) {
      buffer++;
      switch (*buffer++) {
      case 'N':
	strcpy(string,theName);
	string += strlen(theName);
	break;
      case 'U':
	strcpy(string,"FROM_LIST");   /* FIX ME */
	string += 9;
	break;
      case 'T':
	strcpy(string,"uid");
	string += 3;
	break;
      case 'S':
	strcpy(string,".uid");
	string += 4;
	break;
      case 'L':
	strcpy(string,"LANG");    /* FIX ME */
	string+=4;
	break;
      case 'l':
	strcpy(string,"lang");    /* FIX ME */
	string+= 4;
	break;
      default:
	Exit(LOC, "Unknown format\n");
      }
    }
    else {
      *string++ = *buffer++;
    }
  }
  *string = 0;
  return buffer2;
}

void PrintSymbolTable(SymbolTableType *SymbolTable)
{
  for (; SymbolTable; SymbolTable = SymbolTable->Next)
    fprintf(stderr,"      %s\n",SymbolTable->Name);
}


void FreeSymbolTable(SymbolTableType *t)
{
  SymbolTableType *n;

  while (t) {
    n = t->Next;
    free(t);
    t = n;
  }
}

