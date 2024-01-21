#ifndef lint
static char	*RCSid = "$Id: util.c,v 2.0 1995/03/27 18:56:22 mjhammel Exp $";
#endif

/*
 * util.c - utility routines.
 *
 * David A. Curry
 * SRI International
 * 333 Ravenswood Avenue
 * Menlo Park, CA 94025
 * davy@itstd.sri.com
 *
 * Modified by
 * Michael J. Hammel (03/01/95)
 * Contractor
 * 1150 Inca St. TH 70
 * Denver, CO 80204
 * mjhammel@csn.org
 *
 * $Log: util.c,v $
 * Revision 2.0  1995/03/27  18:56:22  mjhammel
 * Initial update to 2.0
 *
 *
 * Revision 1.2  90/06/14  11:21:14  davy
 * Ported to X11 Release 4.
 * 
 * Revision 1.1  90/06/13  09:48:49  davy
 * Initial revision
 * 
 */
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Shell.h>
#include <sys/param.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pwd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "xpostit.h"

extern void ErrPopUp();

#ifdef PROMPT_FOR_SAVE
static void SOEPopDown();
static void SOEAccept();
static void SOECancel();
static Widget	soe_widget;
#endif

/*
 * ByeBye - clean up and exit.
 */
void
ByeBye()
{

#ifdef PROMPT_FOR_SAVE
	Window 			root, child;
	int 				root_x, root_y, child_x, child_y;
	unsigned int 	buttons;
	Widget			soe_form, soe_label, soe_buttons[2];
	Arg				args[15];
	int				nargs;
	XtCallbackRec	callbacks[2];
#endif

#ifndef PROMPT_FOR_SAVE
	/*
	 * If saving notes is on, save all notes.
	 */
	if (app_res.save_notes)
		SaveAllNotes(False);

	XtUnmapWidget(toplevel);
	XCloseDisplay(display);
	exit(0);

#else
	if (app_res.save_notes)
	{
		SaveAllNotes(False);

		XtUnmapWidget(toplevel);
		XCloseDisplay(display);
		exit(0);
	}
	/*
	 * else - ask the user if they want to save the notes
	 * this should be a compile time option.
	 */
	else 
	{
		/* get mouse location */
		XQueryPointer(display, XtWindow(toplevel), &root, &child,
		      &root_x, &root_y, &child_x, &child_y, &buttons);

		/*
		 * Create a popup shell widget
		 */
		nargs = 0;
		SetArg(XtNtitle, PostItSaveOnExitPrompt);
		SetArg(XtNx, root_x);
		SetArg(XtNy, root_y);
		soe_widget = XtCreatePopupShell(
				"SOEShell", 
				transientShellWidgetClass,
				toplevel, 
				args, nargs);
	
		/* the form inside which all other windows will be put */
		nargs = 0;
		SetArg(XtNborderWidth, 0);
		soe_form = XtCreateManagedWidget(
				"SOEForm", 
				formWidgetClass,
				soe_widget, 
				args, nargs);

		nargs = 0;
		SetArg( XtNborderWidth, 0);
		SetArg( XtNtop, XtChainTop ); 
		SetArg( XtNbottom, XtChainTop ); 
		SetArg( XtNleft, XtChainLeft ); 
		SetArg( XtNright, XtChainLeft ); 
		SetArg( XtNvertDistance, 0 ); 
		SetArg( XtNhorizDistance, 0 ); 
		SetArg( XtNdefaultDistance, 0 ); 
		SetArg( XtNlabel, "Save Changed Notes?"); 
		soe_label = XtCreateManagedWidget(
			"SOELabel", 
			labelWidgetClass,
			soe_form, 
			args, nargs);

		/*
		 * create the Yes/No command buttons 
		 */
		bzero(callbacks, sizeof(callbacks));
		SetCallback(SOEAccept, NULL);
		nargs = 0;
		SetArg(XtNcallback, callbacks);
		SetArg(XtNlabel, "Yes");
		SetArg( XtNborderWidth, 0);
		SetArg( XtNtop, XtChainTop ); 
		SetArg( XtNbottom, XtChainTop ); 
		SetArg( XtNleft, XtChainLeft ); 
		SetArg( XtNright, XtChainLeft ); 
		SetArg( XtNvertDistance, 0 ); 
		SetArg( XtNhorizDistance, 0 ); 
		SetArg( XtNdefaultDistance, 0 ); 
		SetArg( XtNfromVert, soe_label ); 
		soe_buttons[0] = XtCreateManagedWidget(
			"SOEButtons", 
			commandWidgetClass,
			soe_form, 
			args, nargs);
	
		bzero(callbacks, sizeof(callbacks));
		SetCallback(SOECancel, NULL);
		nargs = 0;
		SetArg(XtNcallback, callbacks);
		SetArg(XtNlabel, "No");
		SetArg( XtNborderWidth, 0);
		SetArg( XtNtop, XtChainTop ); 
		SetArg( XtNbottom, XtChainTop ); 
		SetArg( XtNleft, XtChainLeft ); 
		SetArg( XtNright, XtChainLeft ); 
		SetArg( XtNvertDistance, 0 ); 
		SetArg( XtNhorizDistance, 0 ); 
		SetArg( XtNdefaultDistance, 0 ); 
		SetArg( XtNfromVert, soe_label ); 
		SetArg( XtNfromHoriz, soe_buttons[0] ); 
		soe_buttons[1] = XtCreateManagedWidget(
			"SOEButtons", 
			commandWidgetClass,
			soe_form, 
			args, nargs);
	
		XtPopup( soe_widget, XtGrabNonexclusive );
	}
#endif

}

#ifdef PROMPT_FOR_SAVE
/*
 * SOECancel - exit without saving notes
 */
void
SOECancel(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	SOEPopDown();
	XtUnmapWidget(toplevel);
	XCloseDisplay(display);
	exit(0);
}

/*
 * SOEAccept - saving notes and then exit
 */
void
SOEAccept(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	SOEPopDown();
	SaveAllNotes(False);
	XtUnmapWidget(toplevel);
	XCloseDisplay(display);
	exit(0);
}

/*
 * SOEPopDown - removes the Save On Exit Dialog box
 */
void
SOEPopDown()
{
	XtPopdown( soe_widget );
	XtDestroyWidget(soe_widget);

}
#endif

/*
 * SetNoteDir - determine the path to the note directory.
 */
void
SetNoteDir()
{
	char *getenv();
	char *home, *s;
	struct passwd *pwd;
	char fname[MAXPATHLEN];
	struct stat *stat_buf;
	int rc;

	/*
	 * If it's an absolute path name,
	 * we're done.
	 */
	if (app_res.note_dir[0] != '/')
	{

		/*
		 * Find the user's home directory.
		 */
		if ((home = getenv("HOME")) == NULL) {
			if ((pwd = getpwuid(getuid())) == NULL) {
				fprintf(stderr, "xpostit: who are you?\n");
				exit(1);
			}
	
			home = pwd->pw_dir;
		}

		/*
		 * Save the path.
		 */
		sprintf(fname, "%s/%s", home, app_res.note_dir);

		s = SafeAlloc(strlen(fname) + 1);
		app_res.note_dir = s;
		strcpy(s, fname);
	}
	
	/* now we have the name of the directory pointed at by app_res.note_dir
	 * so check if it exists, and if it doesn't, try to create it
	 */

    stat_buf = (struct stat *) malloc( sizeof(struct stat) );
    if ( ( rc = stat ( app_res.note_dir, stat_buf ) ) !=0 ) {
        switch ( errno ) {
            case EACCES:
                printf ( "Search permission denied for portion of path");
                printf ( " leading to %s\n", app_res.note_dir );
                printf ( "Aborting.\n");
                free ( stat_buf );
                exit ( -1 );
                break;
            case ENOTDIR:
                printf ( "A portion of the pathname to %s", app_res.note_dir);
                printf ( " is invalid\n" );
                printf ( "Aborting.\n");
                free ( stat_buf );
                exit ( -1 );
                break;
            case ENOENT:
				/* if it doesn't exist, try to create it */
                printf ( "%s: no such file or directory\n", app_res.note_dir);
                printf ( "Trying to create.\n");
				rc = mkdir ( app_res.note_dir, 0755 );
				if ( rc != 0 ) {
					printf ("Can't create %s\n; Aborting", app_res.note_dir );
                	free ( stat_buf );
                	exit ( -1 );
				}
                break;
            default:
                printf ("Unknown error (%d) while stat()'ing %s\n", errno,
                    app_res.note_dir );
                exit ( -1 );
                break;
        }
    }
 
}

/*
 * MakeFname - make a file name from a note index number.
 */
char *
MakeFname(index)
register int index;
{
	char *s;
	char fname[MAXPATHLEN];

	sprintf(fname, "%s/%s%d", app_res.note_dir, PostItNoteFname, index);
	s = SafeAlloc(strlen(fname) + 1);
	strcpy(s, fname);
	return(s);
}

/*
 * SafeAlloc - allocate n bytes of memory, exit if we run out.
 */
char *
SafeAlloc(nbytes)
register int nbytes;
{
	register char *s;

	if ((s = (char *)malloc(nbytes)) == NULL) {
		fprintf(stderr, "xpostit: out of memory.\n");
		exit(1);
	}

	bzero(s, nbytes);
	return(s);
}

/*
 * AutoSave - save all notes automatically when timer expires
 */
void
AutoSave(client_data, id)
XtPointer client_data;
XtIntervalId	*id;
{
	XtIntervalId	timer;

	SaveAllNotes(False);
	timer = XtAppAddTimeOut (
				XtWidgetToApplicationContext(toplevel),
				timer_interval,
				AutoSave,
				NULL);
}

/*
 * Generic routine for inserting text into a note from a file
 */
void
InsertText(
	PostItNote	*pn,
	char			*tmpfile,
	int			remove,
	int			type
)
{
	Arg	args[3];
	int	nargs, position;
	int	fp;
	char	*buf;
	XawTextBlock	text_block;
	struct stat	stat_buf;
	char	cmd[1024];

	/*
	 * get the position of the cursor in the note
	 */
	nargs = 0;
	SetArg(XtNinsertPosition, &position);
	XtGetValues(pn->pn_textwidget, args, nargs);

	if ( type == FILE_TYPE )
	{

		/*
		 * insert the text into the note at the cursor location
		 */
		if ( stat(tmpfile, &stat_buf) != 0 )
		{
			sprintf(cmd,"Can't stat file: %s", tmpfile);
			ErrPopUp(cmd);
			if ( remove )
			{
				sprintf (cmd, "rm %s", tmpfile );
				system ( cmd );
			}
			return;
		}
		if ((fp = open(tmpfile, O_RDONLY)) == -1)
		{
			fprintf(stderr, "xpostit: ");
			perror(tmpfile);
			sprintf(cmd,"Can't open file for\nreading: %s", tmpfile);
			ErrPopUp(cmd);
			if ( remove )
			{
				sprintf (cmd, "rm %s", tmpfile );
				system ( cmd );
			}
			return;
		}
		buf = (char *) SafeAlloc((int)stat_buf.st_size+2);
		while ( read(fp, buf, (int)stat_buf.st_size ) != 0 )
		{
			text_block.firstPos = 0;
			text_block.length = strlen(buf);
			text_block.ptr = buf;
			text_block.format = FMT8BIT;
			if ( XawTextReplace( pn->pn_textwidget, (long)position,
				(long)position, &text_block) != XawEditDone )
			{
				ErrPopUp("Can't add text from file to note!!");
				free(buf);
				close (fp);
				return;
			} 
			position += strlen(buf);
		}
		close (fp);
		free(buf);
	
		/*
		 * cleanup
		 */
		if ( remove )
		{
			sprintf (cmd, "rm %s", tmpfile );
			system ( cmd );
		}
	}
	else
	{
		/*
		 * "tmpfile" is actually a text buffer that we want to insert.
		 * This was added as an afterthought (does it show?)
		 */
		text_block.firstPos = 0;
		text_block.length = strlen(tmpfile);
		text_block.ptr = tmpfile;
		text_block.format = FMT8BIT;
		if ( XawTextReplace( pn->pn_textwidget, (long)position,
			(long)position, &text_block) != XawEditDone )
		{
			ErrPopUp("Can't add text from buffer to note!!");
			return;
		} 
	}
}
	
