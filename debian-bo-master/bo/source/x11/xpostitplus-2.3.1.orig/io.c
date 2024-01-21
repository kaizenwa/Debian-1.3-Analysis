#ifndef lint
static char	*RCSid = "$Id$";
#endif

/*
 * io.c - routines to handle file input and output
 *
 * Michael J. Hammel
 * Contractor
 * 6001 S. Yosemite St. #F104
 * Greenwood Village, CO 80111
 * mjhammel@csn.net
 *
 * $Log$
 *
 * 
 */
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Shell.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#ifdef USE_DIR
#include <sys/dir.h>
#else
#include <dirent.h>
#endif

#include "xpostit.h"

/* globals */

/* local variables */
static Widget 		io_widget;
static Widget		io_exists_widget;
static Widget 		file_viewform;
static Widget 		file_viewport;
static Widget 		dir_viewform;
static Widget 		dir_viewport;
static Widget		io_label;
static Widget		io_text;
static Widget		io_boxes[3];
static Widget		io_buttons[2];

static char			masterpath[MAXPATHLEN];
static char			outpath[MAXPATHLEN];
static PostItNote	*localpn = (PostItNote *)0;
static Position	local_x=0;
static Position	local_y=0;
static int			io_type;

/* external variables */
extern Widget			toplevel;

/* external routines */
extern PostItNote	*FindNote();
extern void InsertText();
extern void ErrPopUp();

/* local routines */
void	IOPopDown();
void	IOAcceptCB();
void	IOCancel();
void	IODirCB();
void	IOFileCB();
void	IOExists();
void	IOExistsAccept();
void	IOExistsCancel();
void	IOExistsPopDown();
void	IOWriteFile();
int	FillList();


/*
 * CreateIOPrompt - create a file selection widget
 * pn		pointer to a PostItNote structure
 * type	either XPOSTIT_OPEN_FILE, or XPOSTIT_EXPORT_FILE
 * path	path to directory which is to be used in file selection
 */
void
CreateIOPrompt(pn, type, path)
PostItNote 	*pn;
int			type;
char			*path;
{


	Arg 				args[30];
	register int 	nargs;
	XtCallbackRec	callbacks[2];
	Window 			root, child;
	unsigned int 	buttons;
	int 				root_x, root_y, child_x, child_y;

	Widget 			io_form;

	/*
	 * save the type in static storage for the callbacks to use 
	 */
	io_type = type;

	/* determine where to pop up the window */
	if ( pn != localpn )
	{
		/* get mouse location */
		XQueryPointer(display, XtWindow(toplevel), &root, &child,
		      	&root_x, &root_y, &child_x, &child_y, &buttons);
		localpn = pn;

		local_x = root_x;
		local_y = root_y;
	}
	else
	{
		/*
		 * Use the current location - if we use the mouse pointer each
		 * time we enter here we end up with a window that migrates 
		 * southeast with each directory thats selected.
		 */
		root_x = local_x;
		root_y = local_y;
	}

	/*
	 * Create a popup shell widget
	 */
	nargs = 0;
	SetArg(XtNtitle, PostItIOPrompt);
	SetArg(XtNx, root_x);
	SetArg(XtNy, root_y);
	io_widget = XtCreatePopupShell(
			"FileSelectionShell", 
			transientShellWidgetClass,
			toplevel, 
			args, nargs);

	/* the form inside which all other windows will be put */
	nargs = 0;
	SetArg(XtNborderWidth, 0);
	SetArg(XtNresizable, TRUE);
	io_form = XtCreateManagedWidget(
			"IOForm", 
			formWidgetClass,
			io_widget, 
			args, nargs);

	nargs = 0;
	SetArg( XtNborderWidth, 0);
	SetArg( XtNtop, XtChainTop ); 
	SetArg( XtNbottom, XtChainTop ); 
	SetArg( XtNleft, XtChainLeft ); 
	SetArg( XtNright, XtChainRight ); 
	SetArg( XtNvertDistance, 0 ); 
	SetArg( XtNhorizDistance, 0 ); 
	SetArg( XtNdefaultDistance, 0 ); 
	SetArg( XtNvSpace, 2 ); 
	SetArg( XtNhSpace, 2 ); 
	SetArg( XtNorientation, XtorientHorizontal ); 
	io_boxes[0] = XtCreateManagedWidget(
			"IOBox", 
			boxWidgetClass,
			io_form, 
			args, nargs);

	/* the directory list */
	nargs = 0;
	SetArg(XtNallowHoriz, FALSE);
	SetArg(XtNallowVert, TRUE);
	SetArg(XtNuseBottom, FALSE);
	SetArg(XtNforceBars, TRUE);
	dir_viewport = XtCreateManagedWidget(
			"IODirViewport", 
			viewportWidgetClass,
			io_boxes[0], 
			args, nargs);

	/*
	 * create the form inside which all the buttons go
	 */
	nargs = 0;
	SetArg(XtNborderWidth, 0);
	dir_viewform = XtCreateManagedWidget(
			"IODirViewForm", 
			formWidgetClass,
			dir_viewport, 
			args, nargs);


	/* get the list of directories */
	if ( FillList(path, dir_viewform, DIR_TYPE, pn) == -1 ) 
	{
		ErrPopUp("Failed to get list of directories.");
		return;
	}

	/* reset height of viewport, so its not too big */
	nargs = 0;
	SetArg(XtNheight, 100);
	XtSetValues(dir_viewport,args, nargs);


	/* the file list */
	nargs = 0;
	SetArg(XtNallowHoriz, FALSE);
	SetArg(XtNallowVert, TRUE);
	SetArg(XtNuseBottom, FALSE);
	SetArg(XtNforceBars, TRUE);
	file_viewport = XtCreateManagedWidget(
			"IOFileViewport", 
			viewportWidgetClass,
			io_boxes[0], 
			args, nargs);

	/*
	 * create the form inside which all the buttons go
	 */
	nargs = 0;
	SetArg(XtNborderWidth, 0);
	file_viewform = XtCreateManagedWidget(
			"IOFileViewForm", 
			formWidgetClass,
			file_viewport, 
			args, nargs);


	/* get the list of files */
	if ( FillList(path, file_viewform, FILE_TYPE, pn) == -1 ) 
	{
		ErrPopUp("Failed to get list of directories.");
		return;
	}

	/* reset height of viewport, so its not too big */
	nargs = 0;
	SetArg(XtNheight, 100);
	XtSetValues(file_viewport,args, nargs);


	/* the label for the filename text input field */
	nargs = 0;
	SetArg(XtNlabel, "File:");
	SetArg(XtNborderWidth, 0);
	SetArg( XtNtop, XtChainTop ); 
	SetArg( XtNbottom, XtChainTop ); 
	SetArg( XtNleft, XtChainLeft ); 
	SetArg( XtNright, XtChainRight ); 
	SetArg( XtNfromVert, io_boxes[0] ); 
	io_label = XtCreateManagedWidget(
			"IOLabel", 
			labelWidgetClass,
			io_form, 
			args, nargs);


	/* the filename text input field */
	nargs = 0;
	SetArg(XtNborderWidth, 1);
	SetArg(XtNstring, path);
	SetArg(XtNwidth, (app_res.name_width*(strlen((char *)path))+10)); 
	SetArg(XtNresize, XawtextResizeBoth);
	SetArg(XtNvertDistance, 0);
	SetArg(XtNeditType, XawtextEdit);
	SetArg(XtNwrap, XawtextWrapNever);
	SetArg(XtNscrollVertical, XawtextScrollNever);
	SetArg(XtNscrollHorizontal, XawtextScrollNever);
	SetArg( XtNtop, XtChainTop ); 
	SetArg( XtNbottom, XtChainTop ); 
	SetArg( XtNleft, XtChainLeft ); 
	SetArg( XtNright, XtChainRight ); 
	SetArg( XtNfromVert, io_label ); 
	io_text = XtCreateManagedWidget(
			"IOText", 
			asciiTextWidgetClass,
			io_form, 
			args, nargs);

	nargs = 0;
	SetArg( XtNborderWidth, 0);
	SetArg( XtNtop, XtChainTop ); 
	SetArg( XtNbottom, XtChainTop ); 
	SetArg( XtNleft, XtChainLeft ); 
	SetArg( XtNright, XtChainRight ); 
	SetArg( XtNvertDistance, 0 ); 
	SetArg( XtNhorizDistance, 0 ); 
	SetArg( XtNdefaultDistance, 0 ); 
	SetArg( XtNvSpace, 2 ); 
	SetArg( XtNhSpace, 2 ); 
	SetArg( XtNorientation, XtorientHorizontal ); 
	SetArg( XtNfromVert, io_text ); 
	io_boxes[1] = XtCreateManagedWidget(
			"IOBox", 
			boxWidgetClass,
			io_form, 
			args, nargs);


	/* The Cancel/Accept buttons */
	bzero(callbacks, sizeof(callbacks));
	SetCallback(IOAcceptCB, (XtPointer)pn);
	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNlabel, "Accept");
	io_buttons[1] = XtCreateManagedWidget(
		"IOButtons", 
		commandWidgetClass,
		io_boxes[1], 
		args, nargs);


	bzero(callbacks, sizeof(callbacks));
	SetCallback(IOCancel, (XtPointer)pn);
	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNlabel, "Cancel");
	io_buttons[0] = XtCreateManagedWidget(
		"IOButtons", 
		commandWidgetClass, 
		io_boxes[1], 
		args, nargs); 

	XtPopup( io_widget, XtGrabNonexclusive );
 
}

/*
 * IOCancel - Cancel the file selection window
 */
void
IOCancel(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	IOPopDown();
}

/*
 * IOPopDown - removes the IO Dialog box
 */
void
IOPopDown()
{
	XtPopdown( io_widget );
	XtDestroyWidget(io_widget);

}

/*
 * FillList - creates a list of regular files or directories in the 
 * current directory
 *
 *	path		the current directory to look in
 *	form		the form in which the buttons will go
 *	type		either DIR_TYPE or FILE_TYPE
 * pn			pointer to the current postitnote
 */
int
FillList(
	char			*path,
	Widget		form,
	int			type,
	PostItNote	*pn
)
{
	int			count, last;
	struct stat	stat_buf;
	DIR			*dp;
	char			filename[MAXPATHLEN];
	int			localtype;
	int			maxwidth;
	Dimension	width;
	Boolean		first;

	Arg			args[10];
	int			nargs;
	Widget		entry[XP_MAX_FILES];

#ifdef USE_DIR
	register struct direct *d;
#else
	register struct dirent *d;
#endif


	/*
	 * Make sure the caller gave us something to work with
	 */
	if ( path == NULL )
		return (-1);

	/* save a copy */
	strcpy(masterpath, path);

	/* open the directory */
	if ((dp = opendir(path)) == NULL)
	{
		perror(path);
		fprintf(stderr, "xpostit: Can't open directory - %s\n", path);
		return(-1);
	}

	/* determine what type of files where looking for */
	switch ( type )
	{
		case DIR_TYPE:
			localtype = S_IFDIR;
			break;

		case FILE_TYPE:
			localtype = S_IFREG;
			break;

		default:
			return(-1);
			break;
	}

	count = 0;
	first = True;
	maxwidth = 0;
	while ( ((d = readdir(dp)) != NULL) && (count < XP_MAX_FILES) )
	{
		/* ignore the current directory */
		if ( strcmp( d->d_name, "." ) == 0 )
			continue;

		if ( strcmp( path, "/" ) == 0 )
			/* ignore the parent directory if we're already at the root */
			if ( strcmp( d->d_name, ".." ) == 0 )
				continue;
			else
				strcpy (filename, path);
		else
		{
			strcpy (filename, path);
			strcat (filename, "/");
		}

		/* add the filename onto the path */
		strcat (filename, d->d_name);

		if ( stat(filename, &stat_buf) != 0 )
		{
			perror(path);
			fprintf(stderr, "xpostit: Can't stat file - %s\n", filename );
			closedir(dp);
			return(-1);
		}
		else
		{
			if ( stat_buf.st_mode & localtype )
			{

				/* add a button for this file */

				nargs = 0;
				SetArg(XtNborderWidth, 1);
				SetArg(XtNlabel, d->d_name);
				SetArg(XtNshapeStyle, XmuShapeRectangle);
				SetArg(XtNhorizDistance, 0);
				SetArg(XtNjustify, XtJustifyLeft);
				SetArg(XtNvertDistance, 0);
				if ( !first )
				{
					SetArg(XtNfromVert, entry[count-1]);
				}
				entry[count] = XtCreateManagedWidget(
						"ListButton", 
						commandWidgetClass,
						form, 
						args, nargs);

				switch ( type )
				{
					case DIR_TYPE:
						XtAddCallback(entry[count], XtNcallback, 
							IODirCB, (XtPointer)pn);
						break;
					case FILE_TYPE:
						XtAddCallback(entry[count], XtNcallback, 
							IOFileCB, (XtPointer)pn);
						break;
				}
		
				nargs = 0;
				SetArg(XtNwidth, &width);
				XtGetValues(entry[count], args, nargs);
				if (width>maxwidth)
					maxwidth=width;
		
				first=False;

				/* increment count */
				count++;

			}
		}
	}
	last = count;
	if ( count == 0 )
	{
		/*
		 * No files/dirs found, we need to tell the user (plus this
		 * helps avoid a zero width/height viewport which causes Xt
		 * to puke.
		 */
		nargs = 0;
		switch ( type )
		{
			case DIR_TYPE:
				SetArg(XtNlabel, "(No Directories)");
				break;
			case FILE_TYPE:
				SetArg(XtNlabel, "(No Files)");
				break;
		}
		SetArg(XtNborderWidth, 0);
		SetArg( XtNtop, XtChainTop ); 
		SetArg( XtNbottom, XtChainBottom ); 
		SetArg( XtNleft, XtChainLeft ); 
		SetArg( XtNright, XtChainRight ); 
		entry[0] = XtCreateManagedWidget(
			"ListButton", 
			labelWidgetClass,
			form, 
			args, nargs);
	}
	else
		for ( count=0; count<last; count++)
		{
			nargs = 0;
			SetArg(XtNwidth, maxwidth+33);
			SetArg(XtNleft, XtChainLeft);
			SetArg(XtNright, XtChainRight);
			SetArg(XtNtop, XtChainTop);
			SetArg(XtNbottom, XtChainTop);
			XtSetValues(entry[count], args, nargs);
		}

	closedir(dp);
	return(0);

}


/*
 * Close, destroy, and remake the file list window when a directory
 * was clicked on in the original window.
 */
void
IODirCB(
	Widget	w,
	caddr_t	client_data,
	caddr_t	call_data
)
{
	char			*buf;
	char			path[MAXPATHLEN];
	Arg			args[3];
	int			nargs;
	PostItNote 	*pn = (PostItNote *)client_data;

	/* get the path name */
	nargs = 0;
	SetArg(XtNlabel, &buf);
	XtGetValues( w, args, nargs );

	/* If its the parent directory... */
	if ( strcmp(buf, "..") == 0 )
	{
		if ( (strlen(masterpath)) > MAXPATHLEN )
		{
			/* this should never happen */
			sprintf(path, 
						"Path name too long! Max length: %d\n"
						"Path name (masterpath): %s\n", 
						MAXPATHLEN, masterpath);
			ErrPopUp(path);
			return;
		}
		else
		{
			/* pull off the last directory from the masterpath */
			strcpy(path, masterpath);
			buf = path + strlen(path) - 1;
			while ( (strncmp(buf, "/", 1) != 0) && (buf != path) )
				buf--;
			if ( buf != path )
				strncpy(buf,"\0",1);
			else
				strcpy(path, "/");
		}
	}
	else

		/* make sure its not going to overflow our local buffer */
		if ( (strlen(buf)+strlen(masterpath)) > MAXPATHLEN )
		{
			sprintf(path, 
						"Path name too long! Max length: %d\n"
						"Path name: %s\n", 
						MAXPATHLEN, masterpath);
			ErrPopUp(path);
			return;
		}
		else
		{
			/* rebuild the masterpath */
			strcpy ( path, masterpath );
			if ( strcmp (masterpath,"/") != 0 )
				strcat ( path, "/");
			strcat ( path, buf );
		}

	/* close and destroy the file selection window */
	XtPopdown( io_widget );
	XtDestroyWidget( io_widget );

	/* recreate the window */
	CreateIOPrompt(pn, io_type, path);
}

/*
 * Process the file selected
 */
void
IOFileCB(
	Widget	w,
	caddr_t	client_data,
	caddr_t	call_data
)
{
	FILE			*fp;
	PostItNote 	*pn = (PostItNote *)client_data;
	char			*buf;
	char			path[MAXPATHLEN];
	char			output[128+MAXPATHLEN];
	struct stat	stat_buf;
	Arg			args[3];
	int			nargs;
	int			len;

	/* get the path name */
	nargs = 0;
	SetArg(XtNlabel, &buf);
	XtGetValues( w, args, nargs );

	switch (io_type)
	{
		case XPOSTIT_OPEN_FILE:
			if ( (strlen(buf)+strlen(masterpath)) > MAXPATHLEN )
			{
				sprintf(output, "File name (%s) is too long!", buf);
				ErrPopUp(output);
				return;
			}
			else
			{
				/* rebuild the masterpath */
				strcpy ( path, masterpath );
				strcat ( path, "/");
				strcat ( path, buf );
			}
			/* make sure file exists one more time */
			if ( stat(path, &stat_buf) == 0 )
				InsertText( pn, path, False, FILE_TYPE );
			else
			{
				sprintf(output, "%s does not exist!", path);
				ErrPopUp(output);
				return;
			}
			IOPopDown();
			break;

		case XPOSTIT_EXPORT_FILE:
			if ( (strlen(buf)+strlen(masterpath)) > MAXPATHLEN )
			{
				sprintf(output, "File name (%s) is too long!", buf);
				ErrPopUp(output);
				return;
			}
			else
			{
				/* rebuild the masterpath */
				strcpy ( path, masterpath );
				strcat ( path, "/");
				strcat ( path, buf );
			}

			/*
			 * Get the length of the text in the window.
			 */
			len = strlen(pn->pn_text);

			/*
			 * Open the first temporary file for writing
			 */
			if ((fp = fopen(path, "w")) == NULL) {
				sprintf(output, "Can't open %s for writing!", path);
				ErrPopUp(output);
				fclose(fp);
				return;
			}

			/*
			 * Write out the text of the note.
			 */
			if (len)
				fwrite(pn->pn_text, sizeof(char), len, fp);

			/* add a newline and terminator */
			fwrite("\n", sizeof(char), strlen("\n"), fp);

			fclose(fp);
			IOPopDown();

			break;
	}

}

/*
 * Process the file selected
 * This is a hack cuz I got tired and didn't want to figure out
 * how to merge this with the IOFileCB() routine.  So shoot me.
 */
void
IOAcceptCB(
	Widget	w,
	caddr_t	client_data,
	caddr_t	call_data
)
{
	PostItNote 	*pn = (PostItNote *)client_data;
	char			*buf;
	char			path[MAXPATHLEN];
	char			output[128+MAXPATHLEN];
	struct stat	stat_buf;
	Arg			args[3];
	int			nargs;

	/* get the path name */
	nargs = 0;
	SetArg(XtNstring, &buf);
	XtGetValues( io_text, args, nargs );

	switch (io_type)
	{
		case XPOSTIT_OPEN_FILE:
			if ( strlen(buf) > MAXPATHLEN )
			{
				sprintf(output, "File name (%s) is too long!", buf);
				ErrPopUp(output);
				return;
			}
			else
				strcpy ( path, buf );

			/*
			 * make sure file exists one more time
			 * and check if its a file or directory.
			 */
			if ( stat(path, &stat_buf) == 0 )
				if ( stat_buf.st_mode & S_IFDIR )
				{
					/* close and destroy the file selection window */
					XtPopdown( io_widget );
					XtDestroyWidget( io_widget );

					/* recreate the window */
					CreateIOPrompt(pn, io_type, path);
				}
				else
					InsertText( pn, path, False, FILE_TYPE );
			else
			{
				sprintf(output, "%s does not exist!", path);
				ErrPopUp(output);
				return;
			}
			IOPopDown();
			break;

		case XPOSTIT_EXPORT_FILE:
			if ( strlen(buf) > MAXPATHLEN )
			{
				sprintf(output, "File name (%s) is too long!", buf);
				ErrPopUp(output);
				return;
			}
			else
				strcpy ( path, buf );

			/*
			 * check if file exists and if its a directory.
			 */
			if ( stat(path, &stat_buf) == 0 )
			{
				if ( stat_buf.st_mode & S_IFDIR )
				{
					/* close and destroy the file selection window */
					XtPopdown( io_widget );
					XtDestroyWidget( io_widget );

					/* recreate the window */
					CreateIOPrompt(pn, io_type, path);
					return;
				}
				else
				{
					strcpy(outpath, path);
					XtPopdown( io_widget );
					XtDestroyWidget( io_widget );
					IOExists(pn);
					return;
				}
			}

			strcpy(outpath, path);
			IOWriteFile(pn);
			IOPopDown();

			break;
	}

}

/*
 * Tell the user the file exists and give them a chance to
 * cancel the write.
 */
void
IOExists(
	PostItNote	*pn
)
{
	Arg 				args[30];
	register int 	nargs;
	XtCallbackRec	callbacks[2];
	Window 			root, child;
	unsigned int 	buttons;
	int 				root_x, root_y, child_x, child_y;
	Widget 			io_exists_form, io_exists_label;
	Widget			io_buttons[2];

	/* get mouse location */
	XQueryPointer(display, XtWindow(toplevel), &root, &child,
	     	&root_x, &root_y, &child_x, &child_y, &buttons);

	/*
	 * Create a popup shell widget
	 */
	nargs = 0;
	SetArg(XtNtitle, PostItIOPrompt);
	SetArg(XtNx, root_x);
	SetArg(XtNy, root_y);
	io_exists_widget = XtCreatePopupShell(
			"FileExistsShell", 
			transientShellWidgetClass,
			toplevel, 
			args, nargs);

	/* the form inside which all other windows will be put */
	nargs = 0;
	SetArg(XtNborderWidth, 0);
	SetArg(XtNresizable, TRUE);
	io_exists_form = XtCreateManagedWidget(
			"IOForm", 
			formWidgetClass,
			io_exists_widget, 
			args, nargs);

	/* the label with the warning message */
	nargs = 0;
	SetArg(XtNlabel, "File Exists! Are you sure you want to export to this file?");
	SetArg(XtNborderWidth, 0);
	SetArg( XtNtop, XtChainTop ); 
	SetArg( XtNbottom, XtChainTop ); 
	SetArg( XtNleft, XtChainLeft ); 
	SetArg( XtNright, XtChainRight ); 
	io_exists_label = XtCreateManagedWidget(
			"IOExistsLabel", 
			labelWidgetClass,
			io_exists_form, 
			args, nargs);

	/* The Cancel/Accept buttons */
	bzero(callbacks, sizeof(callbacks));
	SetCallback(IOExistsAccept, (XtPointer)pn);
	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNlabel, "Accept");
	SetArg( XtNtop, XtChainTop ); 
	SetArg( XtNbottom, XtChainTop ); 
	SetArg( XtNleft, XtChainLeft ); 
	SetArg( XtNright, XtChainRight ); 
	SetArg( XtNfromVert, io_exists_label ); 
	io_buttons[1] = XtCreateManagedWidget(
		"IOButtons", 
		commandWidgetClass,
		io_exists_form, 
		args, nargs);


	bzero(callbacks, sizeof(callbacks));
	SetCallback(IOExistsCancel, (XtPointer)pn);
	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNlabel, "Cancel");
	SetArg( XtNtop, XtChainTop ); 
	SetArg( XtNbottom, XtChainTop ); 
	SetArg( XtNleft, XtChainLeft ); 
	SetArg( XtNright, XtChainRight ); 
	SetArg( XtNfromVert, io_exists_label ); 
	SetArg( XtNfromHoriz, io_buttons[1] ); 
	io_buttons[0] = XtCreateManagedWidget(
		"IOButtons", 
		commandWidgetClass, 
		io_exists_form, 
		args, nargs); 

	XtPopup( io_exists_widget, XtGrabNonexclusive );
 

}

void
IOExistsAccept(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	IOWriteFile((PostItNote *)client_data);
	IOExistsPopDown();
}


void
IOExistsCancel(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	IOExistsPopDown();
}

void
IOExistsPopDown()
{
	XtPopdown( io_exists_widget );
	XtDestroyWidget(io_exists_widget);
}

/*
 * Write the note to a file
 */
void
IOWriteFile(
	PostItNote	*pn
)
{
	FILE		*fp;
	int		len;
	char		path[MAXPATHLEN];
	char		output[128+MAXPATHLEN];

	strcpy (path, outpath);

	/*
	* Get the length of the text in the window.
	*/
	len = strlen(pn->pn_text);

	/*
	* Open the first temporary file for writing
	*/
	if ((fp = fopen(path, "w")) == NULL) {
		sprintf(output, "Can't open %s for writing!", path);
		ErrPopUp(output);
		fclose(fp);
		return;
	}

	/*
	* Write out the text of the note.
	*/
	if (len)
		fwrite(pn->pn_text, sizeof(char), len, fp);

	/* add a newline and terminator */
	fwrite("\n", sizeof(char), strlen("\n"), fp);

	fclose(fp);
}
