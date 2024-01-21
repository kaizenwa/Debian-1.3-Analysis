
#include <OffiX/DragAndDrop.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <stdio.h>

XtAppContext app;

void DisplayDndData(Widget,XEvent *event);

void IconDropEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void UnknownDropEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void RootDropEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void RawDropEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void RawDragEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void FileDropEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void FileDragEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void FilesDropEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void FilesDragEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void TextDropEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void TextDragEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void DirDropEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void DirDragEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void LinkDropEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void LinkDragEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void ExeDropEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void ExeDragEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void URLDropEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void URLDragEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void MIMEDropEventHandler(Widget,XtPointer,XEvent*,Boolean*);
void MIMEDragEventHandler(Widget,XtPointer,XEvent*,Boolean*);

int
main(int argc, char* argv[])
{
	Widget toplevel,form;
	Widget RawButton,FileButton,FilesButton,TextButton,DirButton,
	       LinkButton,ExeButton,URLButton,MIMEButton;
	
	toplevel=XtVaAppInitialize(&app,"Test2",NULL,0,
				   (Cardinal*)&argc,argv,
				   NULL,NULL);
	
	DndInitialize(toplevel);
	DndRegisterRootDrop(RootDropEventHandler);
	DndRegisterOtherDrop(UnknownDropEventHandler);
	DndRegisterIconDrop(IconDropEventHandler);

	form=XtVaCreateManagedWidget("Form",
		formWidgetClass, toplevel,
		NULL);
	
	RawButton=XtVaCreateManagedWidget("RawButton",
		commandWidgetClass, form,
		XtNlabel, "Raw Data", NULL);
	FileButton=XtVaCreateManagedWidget("FileButton",
		commandWidgetClass, form,
		XtNlabel, "File Name", 
		XtNfromHoriz, RawButton, NULL);
	FilesButton=XtVaCreateManagedWidget("FilesButton",
		commandWidgetClass, form,
		XtNlabel, "File List", 
		XtNfromHoriz, FileButton, NULL);
	TextButton=XtVaCreateManagedWidget("TextButton",
		commandWidgetClass, form,
		XtNlabel, "Text", 
		XtNfromHoriz, FilesButton, NULL);
	DirButton=XtVaCreateManagedWidget("DirButton",
		commandWidgetClass, form,
		XtNlabel, "Directory", 
		XtNfromHoriz, TextButton, NULL);
	LinkButton=XtVaCreateManagedWidget("LinkButton",
		commandWidgetClass, form,
		XtNlabel, "Link", 
		XtNfromHoriz, DirButton, NULL);
	ExeButton=XtVaCreateManagedWidget("ExeButton",
		commandWidgetClass, form,
		XtNlabel, "Program", 
		XtNfromHoriz, LinkButton, NULL);
	URLButton=XtVaCreateManagedWidget("URLButton",
		commandWidgetClass, form,
		XtNlabel, "URL", 
		XtNfromHoriz, ExeButton, NULL);
	MIMEButton=XtVaCreateManagedWidget("MIMEButton",
		commandWidgetClass, form,
		XtNlabel, "MIME", 
		XtNfromHoriz, URLButton, NULL);

	DndRegisterDragWidget(RawButton,RawDragEventHandler,NULL);
	DndRegisterDragWidget(FileButton,FileDragEventHandler,NULL);
	DndRegisterDragWidget(FilesButton,FilesDragEventHandler,NULL);
	DndRegisterDragWidget(TextButton,TextDragEventHandler,NULL);
	DndRegisterDragWidget(DirButton,DirDragEventHandler,NULL);
	DndRegisterDragWidget(LinkButton,LinkDragEventHandler,NULL);
	DndRegisterDragWidget(ExeButton,ExeDragEventHandler,NULL);
	DndRegisterDragWidget(URLButton,URLDragEventHandler,NULL);
	DndRegisterDragWidget(MIMEButton,MIMEDragEventHandler,NULL);

	DndRegisterDropWidget(RawButton,RawDropEventHandler,NULL);
	DndRegisterDropWidget(FileButton,FileDropEventHandler,NULL);
	DndRegisterDropWidget(FilesButton,FilesDropEventHandler,NULL);
	DndRegisterDropWidget(TextButton,TextDropEventHandler,NULL);
	DndRegisterDropWidget(DirButton,DirDropEventHandler,NULL);
	DndRegisterDropWidget(LinkButton,LinkDropEventHandler,NULL);
	DndRegisterDropWidget(ExeButton,ExeDropEventHandler,NULL);
	DndRegisterDropWidget(URLButton,URLDropEventHandler,NULL);
	DndRegisterDropWidget(MIMEButton,MIMEDropEventHandler,NULL);
	
	XtRealizeWidget(toplevel);
	XtAppMainLoop(app);
	return 0;		
}

void DisplayDndData(Widget widget,XEvent *event)
{
	static char *names[]={
		"Unknown data",
		"Raw data",
		"File name",
		"File list",
		"Text",
		"Directory",
		"Link",
		"Program",
		"URL",
		"Mime message"
	};
	char *filename; unsigned long i;
	unsigned char *Data;
	int Type,DropX,DropY,RootX,RootY;
	unsigned long Size;
		
	Type=DndDataType(event);
	DndGetData(&Data,&Size);
	DndDropCoordinates(widget,event,&DropX,&DropY);
	DndDropRootCoordinates(event,&RootX,&RootY);
	
	fprintf(stderr,"Data type: %s : Data Size : %ld\n",names[Type],Size);
	fprintf(stderr,"Protocol version : %ld\n",
		event->xclient.data.l[4]);
	fprintf(stderr,"Window (%d,%d) Root (%d,%d)\n",
		DropX,DropY,RootX,RootY);
	fprintf(stderr,"Raw Contents : ");
	for(i=0;i!=Size;i++)
	{	if(Data[i]!=0) fprintf(stderr,"%c",Data[i]);
		else  fprintf(stderr,"#");
	}
	fprintf(stderr,"\nContents:\n");
	if(Type == DndFiles)
	{		
		filename = Data;
		while (filename[0] != '\0')
		{
			fprintf(stderr,"%s\n",filename);
			filename = filename + strlen(filename) + 1;
		}	
	}	
	else
		fprintf(stderr,"%s\n",Data);
	  
	fprintf(stderr,"------------------------------------------\n");
}

void RawDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	fprintf(stderr,"------------------------------------------\n");
	fprintf(stderr,"Dropped on RAW !\n");
	DisplayDndData(widget,event);
}

void FileDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	fprintf(stderr,"------------------------------------------\n");
	fprintf(stderr,"Dropped on FILE !\n");
	DisplayDndData(widget,event);
}

void FilesDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	fprintf(stderr,"------------------------------------------\n");
	fprintf(stderr,"Dropped on FILES !\n");
	DisplayDndData(widget,event);
}

void TextDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	fprintf(stderr,"------------------------------------------\n");
	fprintf(stderr,"Dropped on TEXT !\n");
	DisplayDndData(widget,event);
}

void DirDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	fprintf(stderr,"------------------------------------------\n");
	fprintf(stderr,"Dropped on DIR !\n");
	DisplayDndData(widget,event);
}

void LinkDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	fprintf(stderr,"------------------------------------------\n");
	fprintf(stderr,"Dropped on LINK !\n");
	DisplayDndData(widget,event);
}

void ExeDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	fprintf(stderr,"------------------------------------------\n");
	fprintf(stderr,"Dropped on PROG !\n");
	DisplayDndData(widget,event);
}

void URLDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	fprintf(stderr,"------------------------------------------\n");
	fprintf(stderr,"Dropped on URL !\n");
	DisplayDndData(widget,event);
}

void MIMEDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	fprintf(stderr,"------------------------------------------\n");
	fprintf(stderr,"Dropped on MIME !\n");
	DisplayDndData(widget,event);
}

void RawDragEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	DndSetData(DndRawData,"Anything!",10);
	DndHandleDragging(widget,event);
}

void FileDragEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	DndSetData(DndFile,"Makefile",9);
	DndHandleDragging(widget,event);
}

void FilesDragEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	DndSetData(DndFiles,"Makefile\0Imakefile\0",20);
	DndHandleDragging(widget,event);
}

void TextDragEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	DndSetData(DndText,"Hello. I'm DND!",16);
	DndHandleDragging(widget,event);
}

void DirDragEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	DndSetData(DndDir,"..",3);
	DndHandleDragging(widget,event);
}

void LinkDragEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	DndSetData(DndLink,"Stale link",11);
	DndHandleDragging(widget,event);
}

void ExeDragEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	DndSetData(DndExe,"sed",4);
	DndHandleDragging(widget,event);
}

void URLDragEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	DndSetData(DndURL,"http://www.inf.ufrgs.br/~ahentz/OffiX",38);
	DndHandleDragging(widget,event);
}

void MIMEDragEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	DndSetData(DndMIME,"Some MIME data",15);
	DndHandleDragging(widget,event);
}

void RootDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	fprintf(stderr,"Dropped on root window!\n");
}

void UnknownDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	fprintf(stderr,"Dropped somewhere here!\n");
}

void IconDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	fprintf(stderr,"Dropped on my icon!\n");
}
