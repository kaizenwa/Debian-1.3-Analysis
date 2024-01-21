#include "Files.h"
#include <stdlib.h>


extern 	ExternalApplication(Display *dpy,int x, int y, char *label, 
			    char *icon, char *cmd, char *drop, char *dir);

static int doMoveCopyFiles(FileWindowRec *fw_source, 
						   char *from, char *to, int move);
static void fileEndMoveCopy(FileWindowRec *fw, FileWindowRec *fw_source, 
							int indice, int move);
static void fileEndAction(FileWindowRec *fw, FileWindowRec *fw_source,
						  int indice);
static void computeFromAndTo(FileWindowRec *fw, FileWindowRec *fw_source, 
					  int indice, char *to, char *from);
static void RootDropCommand(FileRec *file, char *directory, int type);
static void handleExternalDrop(Widget widget, XEvent* event);

int Faking = 0;

/*---------------------------------------------------------------------------
  PUBLIC FUNCTIONS
---------------------------------------------------------------------------*/
void RootDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	int i, type;
	FileWindowRec *fw_source;	

	if (DndDataType(event)==DndNotDnd) return;
	
	/* where did it come from */	
	i = findWindow(DndSourceWindow(event), &fw_source);	
	if (!fw_source) 
	{
	    error("General Protection Fault :-)", "Please, try again!");
	    return;
	}
	type = DndDataType(event);
	 /* Open directories if dragged onto the root window */	
	/* if (type == DndExe || type == DndDir) */
	RootDropCommand(fw_source->files[i], 
			fw_source->directory, type);

}

void IconBoxDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
  int i, button;
  FileWindowRec *fw, *fw_source;
  int move;

  if (DndDataType(event)==DndNotDnd) return;
  
  /* where did it come from */	
  findWindow(DndSourceWindow(event), &fw_source);	
  if (!fw_source)
  {
      /* dropped from another application */
      handleExternalDrop(widget, event);
      return;
  }
  
  /* which buttons were pressed */	
  button = DndDragButtons(event);
  move = button & Button1Mask ? True : False;
  /* who am I */
  i =  findWidget(widget, &fw);  /* i equals -1 here */
  if (!fw) 
  {
    error("General Protection Fault :-)", "Please, try again!");
    return;
  }
  if (fw != fw_source)
	  fileEndMoveCopy(fw, fw_source, i, move);  
}

void DragEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
	int i, indice, n, size, type;
	FileWindowRec *fw;
	char *os_dados=NULL, path[MAXPATHLEN];
	
	indice = findWidget(widget, &fw);
	
	if (!fw)  return;
  	if (indice == -1) return;

	size = strlen(fw->directory);
	strcpy(path,fw->directory);
	if (path[size-1] != '/')
	{
		path[size++] = '/';
		path[size] = '\0';
	}
	if (!Faking && !fw->files[indice]->selected)
		Faking = 1;
	if(Faking)
		  fileFakeSelect(widget, event, NULL,0);
	
	if (fw->n_selections == 1 || Faking)
   	{
		int type;
		if (S_ISDIR(fw->files[indice]->stats.st_mode)) 
		      type = DndDir;
		else if (S_ISLNK(fw->files[indice]->stats.st_mode))
		      type = DndLink;  /* Link */
		else if (fw->files[indice]->stats.st_mode & 
			     (S_IXUSR | S_IXGRP | S_IXOTH))
		      type = DndExe;   /* Executable */
		else type = DndFile;
  
		os_dados = (char *)malloc( size + strlen(fw->files[indice]->name) + 1);
		strcpy(os_dados, path);
		strcat(os_dados, fw->files[indice]->name);
		DndSetData(type, os_dados, (unsigned long)(strlen(os_dados)+1));
	}	
	else
	{
		char *beginning;
		type = DndFiles;
		/* How much memory do we have to alloc */
	  	for (i=0, n=0; i<fw->n_files; i++)
    			if (fw->files[i]->selected) 
	    			n += strlen(fw->files[i]->name) + size + 1;
		/* do it */
		os_dados = (char *)calloc(1, n+1);
		/* fill in the data */
		beginning = os_dados;
	  	for (i=0; i<fw->n_files; i++)
    		if (fw->files[i]->selected)
			{
				strcpy(os_dados, path);
				strcat(os_dados, fw->files[i]->name);
				os_dados += strlen(os_dados) + 1;
			}
		DndSetData(type, beginning, (unsigned long)(n+1));
		os_dados = beginning;
	}	
	if(DndHandleDragging(widget, event))
	{
   		if (Faking)
		{
				Faking = 0;
				fileSelect(widget, event, NULL, 0);
		}
		if (!freeze) intUpdate();
	}
	
	if (os_dados)
		free(os_dados);
}

void DropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
  int i, button;
  FileWindowRec *fw, *fw_source;
  int move;
	
  if (DndDataType(event)==DndNotDnd) return;
  
  /* where did it come from */	
  findWindow(DndSourceWindow(event), &fw_source);
  if (!fw_source)
  {
      /* dropped from another application */
      handleExternalDrop(widget, event);
      return;
  }
 
  /* which buttons were pressed */	
  button = DndDragButtons(event);
  move = button & Button1Mask ? True : False;
  /* who am I */
  i =  findWidget(widget, &fw);
  if (!fw) 
  {
    error("General Protection Fault :-)", "Please, try again!");
    return;
  }
 
  /* Don't do anything if source and sink file windows are the same and:
   *			I'm a selected file; or
   * 			(I'm not a directory ...
   *   			and I'm not a stale link ...
   *		       	and I don't have a drop action).
   */
  if (fw == fw_source &&
	(fw->files[i]->selected ||
    (!S_ISDIR(fw->files[i]->stats.st_mode) &&
	!(fw->files[i]->stats.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH)) &&
	!(fw->files[i]->type && *fw->files[i]->type->drop_action))))
    return;	
  /* Otherwise, if dragged onto a window or a directory, do the move */
  if (S_ISDIR(fw->files[i]->stats.st_mode))
      fileEndMoveCopy(fw, fw_source, i, move);
  /* Otherwise, if the file has a drop action, invoke the action */
	else if (fw->files[i]->type && *fw->files[i]->type->drop_action)
		fileEndAction(fw, fw_source, i);
  /* Otherwise, it must be a normal file so just do the move */
  else 
	  fileEndMoveCopy(fw, fw_source, i, move);
  
}

/*---------------------------------------------------------------------------
  PRIVATE FUNCTIONS
---------------------------------------------------------------------------*/
static void fileEndAction(FileWindowRec *fw, FileWindowRec *fw_source,
						  int indice)
{
	int l, i, n_selected, size;
	FileList files;
	char to[MAXPATHLEN], from[MAXPATHLEN];
	char *path, **argv;
	char *action = varPopup(fw->files[indice]->type->icon_bm,
							fw->files[indice]->type->drop_action);

	if (!action) return;

	computeFromAndTo(fw, fw_source, indice, from, to);

	l = strlen(to);
	path = (char *)alloca(l+strlen(fw->files[indice]->name)+2);
	strcpy(path, to);
	if (path[l-1] != '/')
		path[l++] = '/';
	strcpy(path+l, fw->files[indice]->name);
	/* so far, path = to_dir + name */
 	
	/* how many files were selected */
	files = fw_source->files;
  	for (i=0,n_selected =0; i<fw_source->n_files; i++) 
    	if (files[i]->selected) n_selected++;
	/* How many args do we have */	
	size = user.arg0flag ? 5 : 4;
	/* alloc space for args + files */
	argv = (char **) XtMalloc( (size + 1 + n_selected) * sizeof(char *));
	/* fill in with shell parameters */
  	argv[0] = XtNewString(user.shell);
  	argv[1] = XtNewString("-c");
  	argv[2] = XtNewString(action);
  	if (user.arg0flag)
    		argv[3] = XtNewString(user.shell);
  	argv[size-1] = XtNewString(path);
	/* now, we are going to append the files to the command */
  	for (i=0; i<fw_source->n_files; i++)
    	if (files[i]->selected) 
	    	argv[size++] = XtNewString(files[i]->name);
	argv[size] = NULL;
	/********** execute the action ************/
  	executeApplication(user.shell, from, argv);
	/* now, free argv */
  	for (i=0; argv[i]; i++)
    	XTFREE(argv[i]);
	XTFREE(argv);
}

static void fileEndMoveCopy(FileWindowRec *fw, FileWindowRec *fw_source, 
							int indice, int move)
{
	char to[MAXPATHLEN], from[MAXPATHLEN];
	char msg1[10], msg2[10];
	
	if (move)	
	{
		strcpy(msg1, "Move:");
		strcpy(msg2, "Moving");
	}	
	else
	{
		strcpy(msg1, "Copy:");
		strcpy(msg2, "Copying");
	}
	
	computeFromAndTo(fw, fw_source, indice, from, to);
	
	if (!strcmp(from, to)) 
	{
    	error(msg1, "Source and destination are identical");
		return;
	}
	if (access(to, W_OK)) 
	{
    	error("No write access to this directory","");
	    return;
	}
	freeze = True;

	if (resources.confirm_moves) {
	    char s1[0xff], s2[0xff], s3[0xff];
	    sprintf(s1, "%s %d item%c", msg2, fw_source->n_selections,
		    fw_source->n_selections > 1 ? 's' : ' ');
	    sprintf(s2, "from: %s", from);
	    sprintf(s3, "to: %s", to);
	    if (!confirm(s1, s2, s3))
		{		
			freeze = False;
		  	return;
		}
	}

	if(doMoveCopyFiles(fw_source, from, to, move))
	{	  
		markForUpdate(from); 
		if(move)  markForUpdate(to);
		intUpdate();
	}
	freeze = False;
}

static int doMoveCopyFiles(FileWindowRec *fw_source, 
						   char *from, char *to, int move)
{
	int i, n_done = 0, result;
	int len_from, len_to;
	char msg1[10], msg2[10], msg3[10];
	
	if (move)	
	{
		strcpy(msg1, "move");
		strcpy(msg2, "Move:");
		strcpy(msg3, "moving");
	}	
	else
	{
		strcpy(msg1, "copy");
		strcpy(msg2, "Copy:");
		strcpy(msg3, "copying");
	}

	len_from = strlen(from);
  	len_to = strlen(to);

	if (from[len_from-1] != '/') 
	{
    	from[len_from++] = '/';
	    from[len_from] = '\0';
	}
	if (to[len_to-1] != '/') 
	{
	    to[len_to++] = '/';
	    to[len_to] = '\0';
	}
	
	for (i=0; i < fw_source->n_files; i++)
    	if (fw_source->files[i]->selected) 
		{
      		if (!strcmp(fw_source->files[i]->name, ".") ||
				  !strcmp(fw_source->files[i]->name, "..")) 
			{
				char s[0xff];
				sprintf(s, "Cannot %s . or..", msg1);
				error(s, "");
				continue;
    		}		
		    strcpy(from+len_from, fw_source->files[i]->name);
			strcpy(to+len_to, fw_source->files[i]->name);
			if (exists(to) && resources.confirm_overwrite) 
			{
				char s[0xff];
				sprintf(s, "%s file %s already exists at destination",
							msg2, fw_source->files[i]->name);
				if (!confirm(s, "Overwrite?", ""))
				if (aborted)
				    break;
				else
				    continue;
			}
			result = move ? rename(from,to) : rcopy(from,to);
			if (result) 
			{
				char s[0xff];
				sprintf(s, "Error %s %s:", msg3, fw_source->files[i]->name);
				sysError(s);
		    } 
			else
				n_done++;
		}
	return n_done;
}

static void computeFromAndTo(FileWindowRec *fw, FileWindowRec *fw_source, 
					  int indice, char *from, char *to)
{
	strcpy(from, fw_source->directory);
  	if ( indice != -1
		&& S_ISDIR(fw->files[indice]->stats.st_mode) 
		&& strcmp(fw->files[indice]->name, ".")) 
	{
    	if (chdir(fw->directory) || chdir(fw->files[indice]->name) 
								 || !getwd(to)) 
      	{
        	sysError("System error:");
        	return;
      	}
	} 
  	else
    	strcpy(to, fw->directory);
}

void RootDropCommand(FileRec *file, char *directory, int type)
{
    char *data;
    Window root, child;
    int x, y, x_win, y_win, size, i;
    unsigned int mask;
    char label[200], cmd[1024], drop[1024], icon[1024];
    Display *dpy = XtDisplay(toplevel);
    
    XQueryPointer(dpy, DefaultRootWindow(dpy), &root, &child, &x, &y, 
		  &x_win, &y_win, &mask);
    DndGetData(&data,&size);
    strcpy(cmd, data);
    strcpy(label, file->name);
    searchPath(icon, resources.pixmap_path,ROOTDROP_DEFAULT_ICON);
    strcpy(drop,"None");
    
    if (file->type)
    {
#ifdef XPM			  
	if (*file->type->icon)
	    searchPath(icon, resources.pixmap_path,file->type->icon);
#endif
	if (type == DndDir)
	{
	    if (resources.default_browser)
		strcpy(cmd,  resources.default_browser);
	    else
		strcpy(cmd, "files");
	    strcat(cmd,	" ");
	    strcat(cmd,	directory);
	    strcat(cmd,	"/");
	    strcat(cmd,	file->name); 
	    strcpy(drop, "cp $DROPOBJECT");
	    strcat(drop, "  ");
	    strcat(drop, directory);
	    strcat(drop, "/");
	    strcat(drop,	file->name);
	}
	else if (*file->type->push_action)
	{			  				
	    if (strcmp(file->type->push_action, "EDIT")||
		strcmp(file->type->push_action, "VIEW"))
		strcpy(cmd,	file->type->push_action);
	    
	    if (!strcmp(cmd, "EDIT"))
	    {
		strcpy(cmd, resources.default_editor);
		strcat(cmd,	" ");
		strcat(cmd,	directory);
		strcat(cmd,	"/");
		strcat(cmd,	file->name); 
	    }
	    else 
	    {
		strcpy(cmd,file->type->push_action);
		if (strchr(cmd,'$'))
		{
		    for (i=0; i<3; i++)
			cmd[strlen(cmd)-i] = 0;
		    strcat(cmd,	" ");
		    strcat(cmd,	directory);
		    strcat(cmd,	"/");
		    strcat(cmd,	file->name); 
		}
	    }
	    if (*file->type->drop_action)
	    {
		for (i=0; i<3; i++)
		    (file->type->push_action)[strlen(file->type->push_action)-i]=0;
		sprintf(drop, "%s $DROPOBJECT", file->type->push_action);
	    }
	}
    }
    if (type==DndDir)
    {
	char dir[MAXPATHLEN];
	strcpy(dir, directory);
	strcat(dir, "/");
	strcat(dir, file->name); 
	ExternalApplication(dpy,x,y,label,icon,cmd,drop,dir);
    }
    else
	ExternalApplication(dpy,x,y,label,icon,cmd,drop,NULL);
}

static void handleExternalDrop(Widget widget, XEvent* event)
{
    unsigned char *Data;
    unsigned long Size;
    char to[MAXPATHLEN], from[MAXPATHLEN];
    char *t;
    FILE *fp;
    FileWindowRec *fw;
    int i, Type;
    /* who am I */
    i =  findWidget(widget, &fw);  /* i equals -1 here */
    if (!fw) 
    {
	error("General Protection Fault :-)", "Please, try again!");
	return;
    } 
    Type = DndDataType(event);
    switch (Type)  
    {
    case DndNotDnd:
	return;
    case DndRawData:
    case DndText:
        DndGetData(&Data,&Size);
	t = tempnam(fw->directory, "New");
	fp = fopen(t, "w");
	free(t);
	if(!fp || fwrite(Data, 1, Size, fp)!=Size)
	{
	    error("Copy: Cannot write file ", to);
	    return;
	}
	fclose(fp);
	break;
    case DndFile:
    case DndDir:
        DndGetData(&Data,&Size);
	strcpy(from, Data);
    	strcpy(to, fw->directory);
	if (Type==DndFile)
	{
	    char *s = from+strlen(from);
	    while(*s!='/') s--;
	    strcat(to,s);
	}
	if (!strcmp(from, to)) 
	{
	    error("Copy:", "Source and destination are identical");
	    return;
	}
	if (Type==DndDir && access(to, W_OK)) 
	{
	    error("No write access to this directory","");
	    return;
	}
	freeze = True;

	if (resources.confirm_copies) 
	{
	    char s1[0xff], s2[0xff], s3[0xff];
	    sprintf(s1, "Copying");
	    sprintf(s2, "from: %s", from);
	    sprintf(s3, "to: %s", to);
	    if (!confirm(s1, s2, s3))
	    {		
		freeze = False;
		return;
	    }
	}
	
	if (!strcmp(from, ".") || !strcmp(from, "..")) 
	{
	    error("Cannot copy . or ..", "");
	    break;
	}		
	if (exists(to) && resources.confirm_overwrite) 
	{
	    char s[0xff];
	    sprintf(s, "Copy: file %s already exists at destination", from);
	    if (!confirm(s, "Overwrite?", ""))
		if (aborted)
		    break;
	}
	if (rcopy(from,to)) 
	{
	    char s[0xff];
	    sprintf(s, "Error copying %s:",  from);
	    sysError(s);
	    break;
	} 
	markForUpdate(to); 
	intUpdate();
	freeze = False;
	break;
    default:
	error("Dnd Error", "Don't know how to handle this drop!");
	break;
    }
	freeze = False;
}




