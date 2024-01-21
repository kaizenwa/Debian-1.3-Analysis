/*
 *  Trash - the OffiX waste basket
 *  Copyright (C) 1996  César Crusius
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */
 
#include "Trash.h"

#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Toggle.h>

#include <stdio.h>

/* Local variables */
dndSortedList	FilesList;
XtAppContext	app;			// may be exported
Widget		toplevel;		// may be exported
Display		*dpy;			// may be exported
Widget		TrashList,InfoLabel;	// may be exported
Widget		ConfButton;

static Atom WM_DELETE_WINDOW;

char NewPath[MAXPATHLEN],OldPath[MAXPATHLEN];

static String fallback_resources[] = {
#include "Trash.ad.h"
NULL
};

#ifndef XawChainTop  /* X11R4 */
#define XawChainTop XtChainTop
#define XawChainLeft XtChainLeft
#define XawChainRight XtChainRight
#define XawChainBottom XtChainBottom
#define XtSetLanguageProc(x,y,z)  /* nothing */
#endif

/* Local function prototypes */
static int  TrashEntrySortFunc(dndPointer, dndPointer);
static int  TrashNumberSort(dndPointer, dndPointer);
static void TrashListCb(Widget, XtPointer, XtPointer);
static void UndeleteCb(Widget, XtPointer, XtPointer);
static void DeleteCb(Widget, XtPointer, XtPointer);
static void DeleteAllCb(Widget, XtPointer, XtPointer);
static void DropEventHandler(Widget,XtPointer,XEvent *,Boolean *);
static void WMDeleteHandler(Widget,XtPointer,XEvent *,Boolean *);
static int  Confirm(Widget, char *);

int
main(int argc,char *argv[])
{
	FilesList.chgsortcrit(TrashEntrySortFunc);
	
	if(!InitTrashDir(".trash")) return 1;
	ReadTrashIndex(&FilesList);
	
       	toplevel=XtVaAppInitialize(&app,"Trash",NULL,0,
				   &argc,argv,
				   fallback_resources,NULL);
	DndInitialize(toplevel);
	DndRegisterOtherDrop(DropEventHandler);
	DndRegisterIconDrop(DropEventHandler);
	dpy=XtDisplay(toplevel);
	
	Widget Form=XtVaCreateManagedWidget("Form",
			formWidgetClass, toplevel,
			XtNborderWidth, 0,
			NULL);
	
	Widget UndelButton=XtVaCreateManagedWidget("Undelete",
		commandWidgetClass, Form,
		XtNlabel, "Undelete",
		XtNleft, XawChainLeft,
		XtNbottom, XawChainTop,
		XtNtop, XawChainTop, NULL);	
	Widget DelButton=XtVaCreateManagedWidget("Delete",
		commandWidgetClass, Form,
		XtNlabel, "Delete", 
		XtNbottom, XawChainTop,
		XtNtop, XawChainTop,
		XtNfromHoriz, UndelButton, NULL);
	Widget DelAllButton=XtVaCreateManagedWidget("DeleteAll",
		commandWidgetClass, Form,
		XtNlabel, "Delete All", 
		XtNbottom, XawChainTop,
		XtNtop, XawChainTop,
		XtNfromHoriz, DelButton, NULL);
	ConfButton=XtVaCreateManagedWidget("Confirm",
		toggleWidgetClass, Form,
		XtNstate, True,
		XtNlabel, "Confirm", 
		XtNright, XawChainRight,
		XtNbottom, XawChainTop,
		XtNtop, XawChainTop,
		XtNfromHoriz, DelAllButton, NULL);

	TrashList=XtVaCreateManagedWidget("TrashList",
		listWidgetClass, Form,
		XtNleft, XawChainLeft,
		XtNright, XawChainRight,
		XtNtop, XawChainTop,
		XtNbottom, XawChainBottom,
		XtNresizable, True,
		XtNfromVert, UndelButton, 
#ifndef HAVE_X11R6
                XtNwidth, 300,
#endif
                NULL);
	InfoLabel=XtVaCreateManagedWidget("InfoLabel",
		labelWidgetClass, Form,
		XtNborderWidth, 0,
		XtNleft, XawChainLeft,
		XtNright, XawChainRight,
		XtNbottom, XawChainBottom,
		XtNtop, XawChainBottom,
		XtNlabel, "File name:\nDirectory:",
		XtNjustify, XtJustifyLeft,
		XtNresizable, True,
		XtNfromVert, TrashList, NULL);
	
	XtAddCallback(TrashList,XtNcallback,TrashListCb,NULL);
	XtAddCallback(UndelButton,XtNcallback,UndeleteCb,NULL);
	XtAddCallback(DelButton,XtNcallback,DeleteCb,NULL);
	XtAddCallback(DelAllButton,XtNcallback,DeleteAllCb,NULL);
	
	UpdateFileList(TrashList,&FilesList);
	  
	
	XtRealizeWidget(toplevel);
	
	Dimension AppWidth;
	int Dist;
#ifdef linux
        XtVaGetValues(Form,XtNwidth,&AppWidth,XtNdefaultDistance,&Dist,NULL);
#else
        XtVaGetValues(toplevel,XtNwidth,&AppWidth,XtNdefaultDistance,&Dist,NULL);
#endif
	XtVaSetValues(InfoLabel,XtNwidth,AppWidth-2*Dist,
		      XtNresize,False,NULL);
	XtVaSetValues(TrashList,XtNwidth,AppWidth-2*Dist-2,
			XtNresize, False, NULL);
	/* This is necessary for the icon management */
	DndRegisterDropWidget(InfoLabel,DropEventHandler,NULL);

	ReadIcons();
	UpdateIcon(FilesList.size());


	/* We must be able to exit gracefully */
	WM_DELETE_WINDOW=XInternAtom(dpy,"WM_DELETE_WINDOW",False);
	XSetWMProtocols(dpy,XtWindow(toplevel),&WM_DELETE_WINDOW,1);
	XtAddEventHandler(toplevel,NoEventMask,True,WMDeleteHandler,NULL);

	XtAppMainLoop(app);
	return 0;
}

void WMDeleteHandler(Widget widget,XtPointer ,XEvent *event,Boolean *)
{
	if(event->type != ClientMessage)	return;
	if(event->xclient.format != 32)		return;
	if(event->xclient.data.l[0] != WM_DELETE_WINDOW) return;
	
	if(FilesList.size()) DeleteAllCb(widget,NULL,NULL);
	exit(0);
}


int
TrashEntrySortFunc(dndPointer ObjA, dndPointer ObjB)
{
	return strcmp(((TrashEntry*)ObjA)->OrigName,
		      ((TrashEntry*)ObjB)->OrigName);
}

int
TrashNumberSort(dndPointer ObjA, dndPointer ObjB)
{
	return ((TrashEntry*)ObjA)->FileID-((TrashEntry*)ObjB)->FileID;
}

void
TrashListCb(Widget ,XtPointer ,XtPointer call_data)
{
	XawListReturnStruct *data=(XawListReturnStruct*)call_data;
	TrashEntry *entry=(TrashEntry*)(FilesList.go(data->list_index+1));
	sprintf(NewPath,"File name: %s\nDirectory: %s",
		entry->OrigName,entry->OrigDir);
	XtVaSetValues(InfoLabel,XtNlabel,NewPath,NULL);
}

void
UndeleteCb(Widget ,XtPointer,XtPointer)
{
	XtVaSetValues(TrashList,XtNresizable,False,NULL);
	XawListReturnStruct *data=XawListShowCurrent(TrashList);
	if(data->list_index==XAW_LIST_NONE) return;
	TrashEntry *entry=(TrashEntry*)(FilesList.go(data->list_index+1));
	sprintf(NewPath,"%s/%s",entry->OrigDir,entry->OrigName);
	sprintf(OldPath,"%ld",entry->FileID);
	
	if(TrashMove(OldPath,NewPath))
	{
		FilesList.remove();
		UpdateFileList(TrashList,&FilesList);
		UpdateIcon(FilesList.size());
	}
}

void
DeleteCb(Widget w,XtPointer,XtPointer)
{
	XtVaSetValues(TrashList,XtNresizable,False,NULL);
	XawListReturnStruct *data=XawListShowCurrent(TrashList);
	if(data->list_index==XAW_LIST_NONE) return;
	TrashEntry *entry=(TrashEntry*)(FilesList.go(data->list_index+1));
	sprintf(OldPath,"%ld",entry->FileID);
	
	sprintf(NewPath,"Delete %s",entry->OrigName);
	if(!Confirm(w,NewPath)) return;
	
	if(TrashDelete(OldPath))
	{
		FilesList.remove();
		UpdateFileList(TrashList,&FilesList);
		UpdateIcon(FilesList.size());
	}
}

void
DeleteAllCb(Widget w,XtPointer,XtPointer)
{
	dndListIndex Size=FilesList.size();
	XtVaSetValues(TrashList,XtNresizable,False,NULL);
	
	if(!Size) return;
	if(!Confirm(w,"Delete all files")) return;
	
	FilesList.getfirst();
	for(dndListIndex i=0;i!=Size;i++)
	{
		TrashEntry *entry=(TrashEntry*)(FilesList.getthis());
		sprintf(OldPath,"%ld",entry->FileID);
	
		if(TrashDelete(OldPath)) FilesList.remove();
		else			 FilesList.getnext();
	}
	UpdateFileList(TrashList,&FilesList);
	UpdateIcon(FilesList.size());
}


/*-------------------- DND stuff ---------------------------------------*/
static int ProcessOneFile(char *Path);

void
DropEventHandler(Widget ,XtPointer ,XEvent *event,Boolean *)
{
	XtVaSetValues(TrashList,XtNresizable,False,NULL);

	int Type=DndDataType(event);
	if(Type==DndNotDnd) return;
	
	unsigned char *Data;
	unsigned long DataSize;
	
	DndGetData(&Data,&DataSize);
	
	if(Type==DndFile || Type==DndExe || Type==DndDir)
	{	if(ProcessOneFile((char*)Data))
	  		UpdateFileList(TrashList,&FilesList);
		UpdateIcon(FilesList.size());
		return;
	}
	if(Type==DndFiles)
	{	while(*Data)
		{	ProcessOneFile((char*)Data);
			Data+=strlen((char*)Data)+1;
		}
		UpdateFileList(TrashList,&FilesList);
		UpdateIcon(FilesList.size());
	}
}

static int ProcessOneFile(char *Path)
{
	/* Find the first free file ID on trash dir */
	FilesList.sort(TrashNumberSort);
	TrashEntry *entry=(TrashEntry*)(FilesList.getfirst());
    	dndListIndex FirstFree;
	for(FirstFree=1;entry!=NULL;
	    entry=(TrashEntry*)(FilesList.getnext()))
	{	if(entry->FileID!=FirstFree) entry=NULL;
		else			     FirstFree++;
	}
	FilesList.sort(TrashEntrySortFunc);

	/* Now try to move to trash dir. On success, update the *
	 * data list (not the widget)				*/
	sprintf(NewPath,"%ld",FirstFree);
	if(TrashMove(Path,NewPath))
	{
		char *Name=Path;
		while(strchr(Name+1,'/')!=NULL) Name=strchr(Name+1,'/');
		*Name='\0'; Name++;
		
		/* Create the new list entry */
		TrashEntry *NewEntry=new TrashEntry;
		NewEntry->FileID=FirstFree;
		strcpy(NewEntry->OrigDir,Path);
		strcpy(NewEntry->OrigName,Name);
		FilesList.add(NewEntry);

		/* Leave the things as we find it */
		Name--; *Name='/';
		return 1;
	}
	return 0;
}

static Widget Popup;
static int ConfFlag;

static void ConfPopupCb(Widget,XtPointer ,XtPointer )
{ ConfFlag=1; XtPopdown(Popup); }

static void CancPopupCb(Widget ,XtPointer ,XtPointer )
{ ConfFlag=0; XtPopdown(Popup); }

int Confirm(Widget widget, char *prompt)
{
	XtVaGetValues(ConfButton,XtNstate,&ConfFlag,NULL);
	if(ConfFlag==False) return 1;

	int x,y,width,height;
	unsigned int mask;
	Window root,child;

	Screen *scrn=XtScreen(toplevel);
	int scr_width = WidthOfScreen(scrn);
 	int scr_height = HeightOfScreen(scrn);

	XQueryPointer(dpy, DefaultRootWindow(dpy), &root, &child, &x, &y, 
		  &width, &height, &mask);

	width=160; height=60;
	if (x + width > scr_width)      x = scr_width - width;
    	else if (x < 0)			x = 0;
    	if (y + height > scr_height)    y = scr_height - height;
    	else if (y < 0)		     	y = 0;
	
        Widget Dialog;
        
        Popup=XtVaCreatePopupShell("popup",transientShellWidgetClass,
				   widget, XtNx, x, XtNy, y, 0);
        Dialog=XtVaCreateManagedWidget("transient",dialogWidgetClass,
				       Popup,XtNlabel, prompt, NULL);
        XawDialogAddButton(Dialog, "Confirm", ConfPopupCb, Dialog);
        XawDialogAddButton(Dialog, "Cancel", CancPopupCb, Dialog);

	ConfFlag=-1;
	XtPopup(Popup,XtGrabExclusive);
	do
	{	XEvent event;
		XtAppNextEvent(app,&event);
		XtDispatchEvent(&event);
	}
	while(ConfFlag==-1);
	
	return ConfFlag;
}
