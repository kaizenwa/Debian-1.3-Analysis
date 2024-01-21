/*****************************************************************************
 *
 *  xdbx - X Window System interface to the dbx debugger
 *
 *  Copyright 1989 The University of Texas at Austin
 *  Copyright 1990 Microelectronics and Computer Technology Corporation
 *
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of The University of Texas
 *  and Microelectronics and Computer Technology Corporation (MCC) not be 
 *  used in advertising or publicity pertaining to distribution of
 *  the software without specific, written prior permission.  The
 *  University of Texas and MCC makes no representations about the 
 *  suitability of this software for any purpose.  It is provided "as is" 
 *  without express or implied warranty.
 *
 *  THE UNIVERSITY OF TEXAS AND MCC DISCLAIMS ALL WARRANTIES WITH REGARD TO
 *  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TEXAS OR MCC BE LIABLE FOR
 *  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 *  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 *  CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 *  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  Author:  	Po Cheung
 *  Created:   	March 10, 1989
 * 
 *****************************************************************************
 * 
 *  xxgdb - X Window System interface to the gdb debugger
 *  
 * 	Copyright 1990,1993 Thomson Consumer Electronics, Inc.
 *  
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of Thomson Consumer
 *  Electronics (TCE) not be used in advertising or publicity pertaining
 *  to distribution of the software without specific, written prior
 *  permission.  TCE makes no representations about the suitability of
 *  this software for any purpose.  It is provided "as is" without express
 *  or implied warranty.
 *
 *  TCE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 *  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
 *  SHALL TCE BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES
 *  OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 *  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 *  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 *  SOFTWARE.
 *
 *  Adaptation to GDB:  Pierre Willard
 *  XXGDB Created:   	December, 1990
 *
 *****************************************************************************/

/*  source.c
 *
 *    Create the source window and handle display of file.
 *
 *    source_init(): 	Initialization routine.
 *    Update():		Action proc to update source window on scrollbar action.
 *    NotifyResize():	Action proc to update source window on resize.
 *    CreateSourceWindow(): Create the source window.
 *    BuildLinePos():	Build an array of starting text position of each line.
 *    LookUpFileTable():Check out source file info from a file table.
 *    SaveDisplayedFileInfo(): records displayed file info into file table.
 *    DisplayFile():	Display a file on the source window
 *    StartEditor():    Start a child process editor on the displayed file.
 *    LoadFile():	Search for a file and open it for display.
 */

#ifndef NeXT
#include <malloc.h>
#endif
#include <stdlib.h>

#include <X11/Xos.h>
#include <sys/stat.h>
#include <pwd.h>
#include "global.h"

#ifdef SYSV 
#ifdef sco
#   include <fcntl.h>
#endif
#endif /* SYSV */

#ifdef GDB
#include <string.h>
#endif

#define	MAXDIRS	256			/* max number of dirs in dirList */

char		CurrentFile[MAXNAME];	/* current contents of file variable */
Widget		sourceForm,		/* parent of sourceWindow */
		sourceWindow;		/* text window for source file */
FileRec  	*displayedFile;		/* pointer to table entry of currently
					   displayed file */

static FileRec	**fileTable;		/* table of file records */
static int	fileTableSize;		/* size of file table */
static char 	*dirList[MAXDIRS];	/* list of dirs for searching files */

void source_init()
{
    dirList[0] = NULL;
}

/*
 *  Update topline, bottomline, arrow sign, updown sign, stop signs, and
 *  line label.
 */
/* ARGSUSED */
void Update(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    XawTextPosition     pos;
    int			topline;
    FileRec 		*file;

    if (displayedFile) {
    	file = displayedFile;
	pos = XawTextTopPosition(sourceWindow);
	file->topPosition = pos;
	topline = TextPositionToLine(pos);
	/* Update the symbols only if the text scrolls */
	if (file->topline != topline) {
	    file->topline = topline;
	    file->bottomline = MIN (file->topline + file->lines - 1, 
				    file->lastline);
	    /* 
	      03/26/91 mod 7 GWC
	      Fixed a bug where the special margin symbols (arrows, stop signs,
	      etc.) did not scroll when one moved the text with keyboard commands
	      such as Ctrl<Key>n.  To do this the Update action procedure should
	      be called after text widget actions such as next-line.
	      Unfortunately Update needed to be enhanced a bit not to always warp
	      the cursor to the top of the window.  You can now call Update with a
	      parameter "warp" to warp the cursor to the top of the screen; the
	      default is not to warp.
	      */
	    if (*num_params == 1 && strcmp(params[0], "warp") == 0)
	      {
		XawTextSetInsertionPoint(sourceWindow,
					 file->linepos[file->topline]);
	      }

	    UpdateLineLabel(file->topline);
    	    UpdateStops(file);
    	    UpdateArrow(file);
    	    UpdateUpdown(file);
    	    UpdateBomb(file);
	}
	else {/* Update caret position only */
	    pos = XawTextGetInsertionPoint(sourceWindow);
	    UpdateLineLabel(TextPositionToLine(pos));
	}
    }
}

/*
 *  Update bottomline, arrow sign, updown sign and stop signs on resize.
 *  Invoked by ConfigureNotify event.
 */
/* ARGSUSED */
static void NotifyResize(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    XawTextPosition pos;
    TextWidget  ctx = (TextWidget) sourceWindow;
    FileRec	*file;

    if ((file = displayedFile)) {
	file->lines = ctx->text.lt.lines;
	pos = XawTextTopPosition(sourceWindow);
	file->topline = TextPositionToLine(pos);
        file->bottomline = MIN (file->topline + file->lines - 1, 
				file->lastline);
        UpdateStops(file);
        UpdateArrow(file);
        UpdateUpdown(file);
        UpdateBomb(file);
    }
}

/*  Update the position of the caret */
/*  ARGSUSED */
#ifdef notdef
void UpdateLine(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    XawTextPosition pos;
    int	line;

    pos = XawTextGetInsertionPoint(w);
    line = TextPositionToLine(pos);
    UpdateLineLabel(line);
}
#endif

/*  My select-start routine that cancels the effect of automatic scrolling
 *  near the bottom of an Athena text widget window.
 */
/*  ARGSUSED */
void SelectStart(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    XawTextPosition topPosition;

    /* remember the top display position before automatic scrolling */
    /* displayedFile->topPosition = XawTextTopPosition(w); */
    topPosition = XawTextTopPosition(w);

    XtCallActionProc(w, "select-start", event, params, *num_params);

    /* reset to remembered position if top position changed */
    /* if (XawTextTopPosition(w) != displayedFile->topPosition)
    	TextSetTopPosition(w, displayedFile->topPosition); */
    if (XawTextTopPosition(w) != topPosition)
    	TextSetTopPosition(w, topPosition);
}

/*  My select-end routine to store the text selection into both the PRIMARY
 *  selection and cut buffer 0. 
 */
/*  ARGSUSED */
void SelectEnd(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    XawTextPosition begin, end, start;
    Widget textsrc;
    XawTextBlock buffer;
    char s_storage[LINESIZ]; /* fix bug where if selection is past 10k, xxgdb crashes */
    char* s = &s_storage[0];
    int nchars;

    XawTextGetSelectionPos(w, &begin, &end);
    XawTextSetSelection(w, begin, end);
    if (begin == end) return;
    if (end - begin > LINESIZ) s = (char*)malloc(end - begin + LINESIZ);
    textsrc = XawTextGetSource(w);
    strcpy(s, "");
    for (start=begin, nchars=end-begin; nchars > 0; 
	start=begin+buffer.length, nchars-=buffer.length) {
    	XawTextSourceRead(textsrc, start, &buffer, nchars);
	strncat(s, buffer.ptr, buffer.length);
    }
    XStoreBytes(display, s, strlen(s));
    if (end - begin > LINESIZ) free(s);
}

/*  This is my own select word routine to replace the standard action
 *  procedure provided by the Text widget.
 *  It selects a word delimited by DELIMITERS, not whitespace.
 */
/* ARGSUSED */
void SelectWord(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    XawTextPosition pos, left, right, start;
    XawTextBlock buffer;
    Widget	textsrc;
    char 	s[LINESIZ];
    char 	*p, *ls, *rs;
    int		nchars;

    pos = XawTextGetInsertionPoint(w);
    textsrc = XawTextGetSource(w);

    XawTextSourceRead(textsrc, pos, &buffer, 1);
    if (buffer.length == 0 || (buffer.length == 1 &&
	strchr(app_resources.delimiters, (int)*(buffer.ptr)) != NULL)) {
	XStoreBytes(display, NULL, 0);
	return;
    }

    left = XawTextSourceScan(textsrc, pos+1, XawstWhiteSpace, XawsdLeft, 1,
                             FALSE);
    right = XawTextSourceScan(textsrc, left, XawstWhiteSpace, XawsdRight, 1,
                              FALSE);
    
    strcpy(s, "");
    for (start=left, nchars=right-left; nchars > 0; 
	start=left+buffer.length, nchars-=buffer.length) {
    	XawTextSourceRead(textsrc, start, &buffer, nchars);
	strncat(s, buffer.ptr, buffer.length);
    }

    if (!strcmp(s, "")) return;
    p = s+pos-left;
    ls = (char *) strtok(s, app_resources.delimiters);
    rs = (char *) strtok(NULL, app_resources.delimiters);
    if (!ls) return;
    while (rs<=p && rs!=NULL) {
	ls = rs;
	rs = (char *) strtok(NULL, app_resources.delimiters);
    }
    left = left + ls - s;
    right = left + strlen(ls) - 1; 

    XawTextUnsetSelection(w);
    XStoreBytes(display, ls, strlen(ls));
    XawTextSetSelection(w, left, right+1);
}

/*  Print the value of the expression  in cut buffer 0. */
/*  ARGSUSED */
void PrintSelection(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    char command[LINESIZ];
    char *string;
    int nbytes;

    string = XFetchBytes(display, &nbytes);
    if (nbytes == 0) {
        UpdateMessageWindow(PRINT_HELP, NULL);
        bell(0);
        return;
    }
    sprintf(command, "print %s\n", string);
    send_command(command);
    AppendDialogText(command);
}

#ifdef EDIT_BUTTON
/* allow invocation of favorite editor from within interface */
extern void StartEditor();
void EdAction(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
  StartEditor();
}
#endif /* EDIT_BUTTON */

/* fixes keybindings in source window */
extern PopupSearch();
void Search(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
  PopupSearch(w, NULL, NULL);
}

/* 
 *  On top of a form widget, we have a text widget with scrollbar, label
 *  widgets for the stop sign, arrow sign, and updown signs.
 */

/* add popupsearch which is triggered by ^S in file window also add
-editor switch which can be set to vi or emacs (default is emacs) and
have operative keys in the editor window for moving around (move stop
signs and such around too) */

void CreateSourceWindow(parent)
Widget parent;
{
    TextWidget ctx;
    Arg args[MAXARGS];
    Cardinal n;

    static XtActionsRec sbar_actions[] = {
        {"NotifyResize",   NotifyResize},
        {"Update", 	   Update},
        {NULL, NULL}
    };

    /* fixes keybindings in source window */
    static XtActionsRec text_actions[] = {
        {"Update", 	   Update},
#ifdef EDIT_BUTTON
        {"Editor",         EdAction},
#endif
		{"Search",         Search},
        {NULL, NULL}
    };

#ifdef EDIT_BUTTON

    static String eTextTranslations = "#override \n\
        Ctrl<Key>V:    next-page() Update(warp) \n\
        Meta<Key>V:    previous-page() Update(warp) \n\
        Ctrl<Key>N:    next-line() Update() \n\
        Ctrl<Key>P:    previous-line() Update() \n\
        Ctrl<Key>Z:    scroll-one-line-up() Update(warp) \n\
        Meta<Key>Z:    scroll-one-line-down() Update(warp) \n\
        Meta<Key>]:    forward-paragraph() Update(warp) \n\
        Meta<Key>[:    backward-paragraph() Update(warp) \n\
        Meta<Key>F:    forward-word() Update() \n\
        Meta<Key>B:    backward-word() Update() \n\
        Ctrl<Key>F:    forward-character() Update() \n\
        Ctrl<Key>B:    backward-character() Update() \n\
        Meta<Key>E:    Editor() \n\
        Meta<Key><:   beginning-of-file() Update(warp) \n\
        Meta<Key>>:   end-of-file() Update(warp) \n\
        <Key>L:        redraw-display() Update() \n\
        <Key>S:        Search() Update() \n\
        <Key>R:        Search() Update() \n\
        <Btn1Down>:             SelectStart() SelectWord() \n\
	Shift<Btn1Up>:          Update() SelectEnd() PrintSelection() \n\
	<Btn1Up>:               Update() SelectEnd() \n\
      ";
  
    static String vTextTranslations = "#override \n\
        Ctrl<Key>F:    next-page() Update(warp) \n\
        Ctrl<Key>B:    previous-page() Update(warp) \n\
        Ctrl<Key>D:    next-page() Update() \n\
        Ctrl<Key>U:    previous-page() Update() \n\
        <Key>Return:   next-line() Update() \n\
        <Key>-:        previous-line() Update() \n\
        <Key>j:        next-line() Update() \n\
        <Key>k:        previous-line() Update() \n\
        <Key>space:    forward-character() Update() \n\
        <Key>BackSpace: backward-character() Update() \n\
        <Key>1:        beginning-of-file() Update(warp) \n\
        <Key>G:        end-of-file() Update(warp) \n\
        <Key>E:        Editor() \n\
        <Key>L:        redraw-display() Update() \n\
        <Key>/:        Search() Update() \n\
        <Key>?:        Search() Update() \n\
        <Btn1Down>:             SelectStart() SelectWord() \n\
	Shift<Btn1Up>:          Update() SelectEnd() PrintSelection() \n\
	<Btn1Up>:               Update() SelectEnd() \n\
    ";

#else /* not EDIT_BUTTON */

    static String eTextTranslations = "#override \n\
        Ctrl<Key>V:    next-page() Update(warp) \n\
        Meta<Key>V:    previous-page() Update(warp) \n\
        Ctrl<Key>N:    next-line() Update() \n\
        Ctrl<Key>P:    previous-line() Update() \n\
        Ctrl<Key>Z:    scroll-one-line-up() Update(warp) \n\
        Meta<Key>Z:    scroll-one-line-down() Update(warp) \n\
        Meta<Key>]:    forward-paragraph() Update(warp) \n\
        Meta<Key>[:    backward-paragraph() Update(warp) \n\
        Meta<Key>F:    forward-word() Update() \n\
        Meta<Key>B:    backward-word() Update() \n\
        Ctrl<Key>F:    forward-character() Update() \n\
        Ctrl<Key>B:    backward-character() Update() \n\
        Meta<Key><:   beginning-of-file() Update(warp) \n\
        Meta<Key>>:   end-of-file() Update(warp) \n\
        <Key>L:        redraw-display() Update() \n\
        <Key>S:        Search() Update() \n\
        <Key>R:        Search() Update() \n\
        <Btn1Down>:             SelectStart() SelectWord() \n\
	Shift<Btn1Up>:          Update() SelectEnd() PrintSelection() \n\
	<Btn1Up>:               Update() SelectEnd() \n\
      ";
  
    static String vTextTranslations = "#override \n\
        Ctrl<Key>F:    next-page() Update(warp) \n\
        Ctrl<Key>B:    previous-page() Update(warp) \n\
        Ctrl<Key>D:    next-page() Update() \n\
        Ctrl<Key>U:    previous-page() Update() \n\
        <Key>Return:   next-line() Update() \n\
        <Key>-:        previous-line() Update() \n\
        <Key>j:        next-line() Update() \n\
        <Key>k:        previous-line() Update() \n\
        <Key>space:    forward-character() Update() \n\
        <Key>BackSpace: backward-character() Update() \n\
        <Key>1:        beginning-of-file() Update(warp) \n\
        <Key>G:        end-of-file() Update(warp) \n\
        <Key>L:        redraw-display() Update() \n\
        <Key>/:        Search() Update() \n\
        <Key>?:        Search() Update() \n\
        <Btn1Down>:             SelectStart() SelectWord() \n\
	Shift<Btn1Up>:          Update() SelectEnd() PrintSelection() \n\
	<Btn1Up>:               Update() SelectEnd() \n\
    ";

#endif /* EDIT_BUTTON */

    /* fixes keybindings in source window */
    static String sbarTranslations = "\
        <Configure>:    NotifyResize() \n\
        <Btn2Down>:     StartScroll(Continuous) MoveThumb() NotifyThumb() \
                        Update() \n\
        <Btn2Motion>:   MoveThumb() NotifyThumb() Update() \n\
        <BtnUp>:        NotifyScroll(Proportional) EndScroll() Update() \n\
    ";

    n = 0;
    XtSetArg(args[n], XtNdefaultDistance, 0);                           n++;
    sourceForm = XtCreateManagedWidget("sourceForm", formWidgetClass, 
					 parent, args, n);

    n = 0;
    XtSetArg(args[n], XtNborderWidth, 0);				n++;
    XtSetArg(args[n], XtNtype, (XtArgVal)XawAsciiFile);			n++;
    XtSetArg(args[n], XtNstring, (XtArgVal)"/dev/null");		n++;
    XtSetArg(args[n], XtNscrollVertical, (XtArgVal) XawtextScrollAlways);n++;
    sourceWindow = XtCreateManagedWidget("sourceWindow", asciiTextWidgetClass,
					  sourceForm, args, n);

    ctx = (TextWidget) sourceWindow;
    if (ctx->text.vbar)
    	XtOverrideTranslations(ctx->text.vbar, 
				XtParseTranslationTable(sbarTranslations));
    XtAppAddActions(app_context, sbar_actions, XtNumber(sbar_actions));

    /* fixes keybindings in source window */
    XtAppAddActions(app_context, text_actions, XtNumber(text_actions));
    if (app_resources.bindings && strcmp(app_resources.bindings, "vi") == 0)
      XtOverrideTranslations((Widget) ctx, XtParseTranslationTable(vTextTranslations));
    else
      XtOverrideTranslations((Widget) ctx, XtParseTranslationTable(eTextTranslations));

	/* setup tabulation */
	if (app_resources.tabstop >= 0) {
		int tab, tabs[256];
		for (n = 0, tab = 0; n < sizeof tabs / sizeof *tabs; n++)
			tabs[n] = (tab += app_resources.tabstop);
		XawTextSinkSetTabs(ctx->text.sink, sizeof tabs / sizeof *tabs, tabs);
	}
}


/*
 *  Build the array which gives the starting text position of each line.
 *  > Estimate the number of lines in the file and allocate memory buffer.
 *  > Starting position of line #1 is 0, and is stored in linepos[1].
 *  > Search for '\n' till end of buffer.
 */
static void BuildLinePos(file)
FileRec *file;
{
    char *p;
    int	 line, nlines;

    nlines = MAX(1, file->filesize/CHARS_PER_LINE);
    file->linepos = (XawTextPosition *)
		    XtMalloc ((nlines+2) * sizeof(XawTextPosition));
    p = file->buf;
    line = 0;
    file->linepos[line++] = 0;
    file->linepos[line++] = 0;
    while (*p) {
	if (*p++ == '\n') {
	    if (line == nlines) { 	/* buffer full, need more memory */
                file->linepos = (XawTextPosition *) XtRealloc ((void*)file->linepos, 
			  (nlines + ADD_LINES) * sizeof(XawTextPosition));
		nlines += ADD_LINES;
            }
            file->linepos[line++] = p - file->buf;
	}
    }
    file->lastline = line - 2;
    file->linepos = (XawTextPosition *) XtRealloc 	/* shrink to min size */
			((void*)file->linepos, line * sizeof(XawTextPosition));
}

/*
 * Function to check the file table.
 * This might be useful after a 'dir' or 'cd' command when
 * there might be another path to the same files.
 */

static void CheckLookUpFileTable()
{
	int i;
	char * newfullname;

	for (i=0; fileTable && i<fileTableSize; i++)
		{
		if (fileTable[i] != NULL)
			{
			newfullname = GetPathname(fileTable[i]->filename);
			if (newfullname != NULL)
				{
				/* if the two files are different, then it means there
				is a new full path for this file. So we better forget
				everything about the old file.
				*/
				if (strcmp (newfullname, fileTable[i]->pathname))
					{
					/* if filenames  are different */
					if (debug)
						fprintf (stderr, "Clearing file table entry \"%s\" : was \"%s\" : is \"%s\"\n",
							fileTable[i]->filename,
							fileTable[i]->pathname,
							newfullname);

					AppendDialogText("Warning : new path to \"");
					AppendDialogText(fileTable[i]->filename);
					AppendDialogText("\" is \"");
					AppendDialogText(newfullname);
					AppendDialogText("\".\n");

					if (displayedFile ==  fileTable[i])
					  	{
						displayedFile = NULL;
						}

					XtFree((char *)fileTable[i]->buf);
					XtFree((char *)fileTable[i]->linepos);
					XtFree((char *)fileTable[i]);
					fileTable[i] = NULL;
					}
				XtFree (newfullname);
				}
			}
		}
}

/*
 * Function to clean up the file table and update the
 * display if necessary.
 *
 */
void CleanUpFileTable ()
{
	CheckLookUpFileTable();
	if (displayedFile == NULL)
		LoadCurrentFile();
}

/*
 * Look up the file table for an entry with "filename"
 * If not found, create an entry and initialize proper fields,
 * else, return pointer to entry found.
 */
static int LookUpFileTable(pathname, filename, file)
char *pathname, *filename;
FileRec **file;
{
    struct stat fileinfo;
    int  	fd;
    int 	i, j, n;
	int 	available;
	
	available = -1;

    for (i=0; fileTable && i<fileTableSize; i++) {
	if (fileTable[i] == NULL) {
		if (available != -1)
			available = i;
	} else {
		if (strcmp(fileTable[i]->pathname, pathname) == 0) /* file found */
			{
	   		if (stat(pathname, &fileinfo) == -1)
				{
				UpdateMessageWindow("Error: cannot stat file %s", pathname);
	        	*file = fileTable[i];
				return 0;
	    		}

	    	if (fileinfo.st_mtime > fileTable[i]->mtime) /* file modified */
				{
				XtFree((char *)fileTable[i]->buf);
				XtFree((char *)fileTable[i]->linepos);
				XtFree((char *)fileTable[i]);
				fileTable[i] = NULL;
				UpdateMessageWindow("WARNING : file %s was modified", pathname);
	    		}

	    	if (displayedFile && 		/* same as displayed file */
		     	strcmp(pathname, displayedFile->pathname) == 0)
				{
				if (fileTable[i] == NULL) /* means file was modified */
					displayedFile = NULL;
				else
					{
					*file = NULL;
					return 0;
	    			}
				}
	    	else
				{
	    		*file = fileTable[i];
				return 0;
	    		}
	  		}
		}
    }

    /* Record file into file table */

    if (available == -1) {		/* file table full, enlarge it */
	available = fileTableSize;
	fileTableSize += ADD_SIZE;
	fileTable = (FileRec **) 
		     XtRealloc ((void*)fileTable, fileTableSize * sizeof(FileRec *));
	for (j=available; j<fileTableSize; j++)
	    fileTable[j] = NULL;
    }

    if ((fd = open(pathname, O_RDONLY)) == -1) {
	UpdateMessageWindow("Error: cannot open file %s", pathname);
	return -1;
    }
    if (fstat(fd, &fileinfo) == -1) {
	UpdateMessageWindow("Error: cannot fstat file %s", pathname);
	close(fd);
	return -1;
    }
	i = available;
    fileTable[i] = (FileRec *) XtMalloc (sizeof(FileRec));
    fileTable[i]->filesize = fileinfo.st_size + 1;
    fileTable[i]->mtime = fileinfo.st_mtime;
    fileTable[i]->buf = XtMalloc((int)fileTable[i]->filesize);
    if ((n = read(fd, fileTable[i]->buf, (int) fileTable[i]->filesize)) == -1) {
	UpdateMessageWindow("Error: cannot read file %s", pathname);
	XtFree(fileTable[i]->buf);
	XtFree((void*)fileTable[i]);
	fileTable[i] = NULL;
	close(fd);
	return -1;
    }
    fileTable[i]->buf[n] = '\0';
    fileTable[i]->pathname = XtNewString(pathname);
    fileTable[i]->filename = XtNewString(filename);
    fileTable[i]->currentline = 1;
    fileTable[i]->topline = 1;
    fileTable[i]->bottomline = 0;
    fileTable[i]->topPosition = 0;
    BuildLinePos(fileTable[i]);
    close(fd);
    *file = fileTable[i];
    return 0;
}

/*  
 *  Remember file position and current line before closing.
 */
static void SaveDisplayedFileInfo()
{
    XawTextPosition pos;

    if (displayedFile) {
    	displayedFile->topPosition = XawTextTopPosition(sourceWindow);
	pos = XawTextGetInsertionPoint(sourceWindow);
	displayedFile->currentline = TextPositionToLine(pos);
    }
}


/*   DisplayFile() displays the file onto the source window.  It
 *     uses topPosition to remember where it was last opened.  But it
 *     must recalculate bottomline because the window size might be
 *     different.
 */
static void DisplayFile(file)
FileRec *file;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;
    TextWidget 	ctx = (TextWidget) sourceWindow;

    n = 0;
    XtSetArg(args[n], XtNdisplayPosition, (XtArgVal)file->topPosition);	n++;
    XtSetArg(args[n], XtNstring, (XtArgVal) file->pathname);		n++;
    XtSetArg(args[n], XtNeditType, (XtArgVal) XawtextRead);		n++;
    XtSetValues(sourceWindow, args, n);
    file->lines = ctx->text.lt.lines;
    file->bottomline = MIN (file->topline + file->lines - 1, file->lastline);
}


/*  Given a filename starting with a tilde (`~'), it expands ~[user] to
 *  the home directory of that user, or to the login home directory if user
 *  is not specified.
 */
static char *expand(filename)
char *filename;
{
    struct passwd *pwd;
    char 	  *string, *name, newfile[MAXNAME];

    string = XtNewString(filename+1);
    if (*string == '\0' || *string == '/')
	name = (char *) getlogin();
    else
    	name = (char *) strtok(string, "/");
    if (name == NULL)
	return filename;
    pwd = (struct passwd *) getpwnam(name);
    if (pwd && pwd->pw_dir) {
    	sprintf(newfile, "%s%s", pwd->pw_dir, filename+strlen(name)+1);
    	return XtNewString(newfile);
    }
    else
	return filename;
}


/*  Create a list of directories for searching source files.
 *  It reads the list of directories specified by the user, adding
 *  the current directory into the list if it is not already there.
 *
 *  With fix from Dave Gagne (daveg@fs1.ee.ubc.ca) 7/30/90
 */
void MakeDirList(output)
char *output;
{
    /* fix bug where if text of a directories command is > 1k, crashes.  Now works to 4k */
    char *s, list[LINESIZ], command[LINESIZ];
    int  i, use_cwd;

    for (i=0; dirList[i]; i++)			/* remove old list */
	XtFree(dirList[i]);
    i = 0;
    use_cwd = TRUE;
    if (output) {                                        /* create list */
#ifdef GDB	/* GDB uses ':' as separator character */
        s = (char *) strtok(output, ": \n");
#else
        s = (char *) strtok(output, " \n");
#endif /* GDB */
        while (s) {
            dirList[i] = XtNewString(s);

            if (dirList[i][0] == '~')                   /* expand '~' */
                dirList[i] = expand(dirList[i]);
            if (LASTCH(dirList[i]) == '/')              /* remove last '/' */
                LASTCH(dirList[i]) = '\0';
            if (strcmp(dirList[i], ".") == 0)        /* watch for "." */
                use_cwd = FALSE;

            ++i;
#ifdef GDB	/* GDB uses ':' as separator character */
            s = (char *) strtok(NULL, ": \n");
#else
            s = (char *) strtok(NULL, " \n");
#endif /* GDB */
        }
        dirList[i] = NULL;
    }

    if (use_cwd) {				/* include current dir */
	dirList[i++] = XtNewString(".");		
    	dirList[i] = NULL;
    }

#if defined(NeXT) && defined(GDB)
	/* for NeXT computer, send 'directory' command for each directory */
    for (i=0; dirList[i]; i++) {
    sprintf(command, "directory %s\n", dirList[i]);
    query_gdb (command, PARSE_OFF | ECHO_OFF | FILTER_OFF);
    }
#else /* not NeXT */
    strcpy(list, "");				/* tell dbx our new list */
    for (i=0; dirList[i]; i++) {
	strcat(list, dirList[i]);
	strcat(list, " ");
    }
#ifdef GDB
    sprintf(command, "directory %s\n", list);
    query_gdb (command, PARSE_OFF | ECHO_OFF | FILTER_OFF);
#else
    sprintf(command, "use %s\n", list);
    Parse = False;
    query_dbx(command);
#endif /* GDB */
#endif /* not NeXT */
}

/*  Returns the full pathname of a given file.
 *  It searches for the file from a list of directories.
 */
char *GetPathname(filename)
char *filename;
{
    char	pathname[LINESIZ];
    int 	i;

    if (filename == NULL || strcmp(filename, "") == 0)
		return NULL;
    for (i=0; dirList[i]; i++) {
		if (*filename == '/' && access(filename, R_OK) == -1) { 
			/* this handles the exceptional case of sun4 dbx output */
			strcpy(filename, &filename[1]);
		}
		if (*filename == '/' || *filename == '~')
			strcpy(pathname, filename);
		else if (strcmp(dirList[i], ".") == 0)
			sprintf(pathname, "%s/%s", cwd, filename);
	     
#ifdef GDB	/* (PW)(SH)11SEP91 : for gdb 4.0 */
		else if (strcmp(dirList[i], "$cwd") == 0)
			sprintf(pathname, "%s/%s", cwd, filename);
		else if (strcmp(dirList[i], "$cdir") == 0)
			sprintf(pathname, "%s/%s", cdir, filename);
#endif /* GDB */

		else if (*dirList[i] == '/' || *dirList[i] == '~')
			sprintf(pathname, "%s/%s", dirList[i], filename);
		else
			sprintf(pathname, "%s/%s/%s", cwd, dirList[i], filename);

#ifdef GDB 
		simplify_path (pathname);  /* be sure to get only significant path */
#endif	     

		if (access(pathname, R_OK) == 0) {
			if (debug)
				fprintf(stderr,"Full path of %s is \"%s\"\n", filename, pathname);
			return XtNewString(pathname);
		}

		if (*filename == '/' || *filename == '~') {
			break;	/* no need to loop over all directories */
		}
    }
    UpdateMessageWindow("File not found: %s", filename);
	bell(0);
    return NULL;
}

/*
 * Given a file name, LoadFile attempts to open it and displays it onto
 * the source window:
 *   1. get the full pathname of the file
 *   2. LookUpFileTable() returns a pointer to the file's entry if it's
 *      already in the table; else, creates an entry and return a pointer.
 *   3. save the current displayedFile info
 *   4. display the file
 *   5. update the file label and the various signs on the source window.
 *  LoadFile returns 0 upon successful completion, -1 otherwise.
 */
int LoadFile(filename)
char *filename;
{
    FileRec 	*file;
    char	*pathname;

    pathname = GetPathname(filename);
    if (pathname == NULL) { 
	return -1;
    }
    if (LookUpFileTable(pathname, filename, &file) != -1) {
	if (file) {	/* load new file */
	    SaveDisplayedFileInfo();
	    DisplayFile(file);
	    UpdateFileLabel(pathname);
	    XawTextUnsetSelection(sourceWindow);
	    XawTextSetInsertionPoint(sourceWindow, file->linepos[file->currentline]);
	    UpdateLineLabel(file->currentline);
	    UpdateStops(file);
	    UpdateArrow(file);
	    UpdateUpdown(file);
	    UpdateBomb(file);
	    displayedFile = file;
	}
    	return 0;
    }
    else {		/* LookUpFileTable() fails */
    	return -1;
    }
}

int LoadCurrentFile()
{
#ifdef GDB
    query_gdb ("info line\n", PARSE_ON | ECHO_OFF | FILTER_OFF);
#else
    query_dbx("file\n");
#endif /* GDB */
    return LoadFile(CurrentFile);
}

#ifdef EDIT_BUTTON
/* simply add editor button that calls  $XXGDBWINEDIT, $WINEDIT, xxgdbedit in that order */
/* allow invocation of fav. editor from within interface */
/* button and the EdAction action procedure for the source window */
void StartEditor ()
{
  XawTextPosition pos;
  char* editor;
  char string[128];
  int result;
  
  if (displayedFile == NULL) return;
  editor = (char *) getenv("XXGDBWINEDIT");
  if (editor == NULL)
    editor = (char *) getenv("WINEDIT");
  if (editor == NULL)
    editor = "xxgdbedit";
  pos = XawTextGetInsertionPoint(sourceWindow);
  displayedFile->currentline = TextPositionToLine(pos);
  sprintf(string, "nohup %s +%d %s&\n",
	  editor, displayedFile->currentline, displayedFile->pathname);
  result =  system(string);
  printf("result from system call: %d \n", result);
  /* the following is more efficient but needs some work
  {
  int pid;
  if (!(pid = fork()))
    { 
      execlp(editor, editor, linenum, displayedFile->pathname, (char *) 0);
      printf("editor command fails\n");
    }
  else 
    {
      if (pid == -1) printf("unable to start editor\n");
    }
  }
  */
}
#endif /* EDIT_BUTTON */

#ifdef GDB
/*
 * Function to get the full path of a source file.
 *
 * This function is implemented by doing a 'list sourcefile;1'
 * and then a 'info source'. That is the only way I found to
 * get this fullpath. If there is a better way, change here.
 *
 * Note that we have to save and restore the current source
 * file in case it is not the same as 'filename'.
 *
 */
char *
GetSourcePathname (filename)
char *filename;
{
char *srcpath;
char curr_src [MAXPATHLEN];
char list_src_cmd [MAXPATHLEN+10]; /* +10 for room for "list :1\n" */

    if (filename == NULL || strcmp(filename, "") == 0)
	return NULL;

	/* (PW)19NOV93: it is important to get new string because,
	   "info source" below will free Token.file (which could be
	   same as filename here.
	   */

	filename = XtNewString (filename);

	/* get current source */

	query_gdb("info source\n", PARSE_ON | ECHO_OFF | FILTER_OFF);
	
	strcpy (curr_src, source_path);

	if (*curr_src == 0)	{
		srcpath = GetPathname (filename);	/* when info source is not supported */
	} else {

		/* tell gdb to go to filename */

		sprintf (list_src_cmd,"list %s:1\n", filename);

		query_gdb(list_src_cmd, PARSE_OFF | ECHO_OFF | FILTER_OFF);

		/* get source of filename  */

		query_gdb("info source\n", PARSE_ON | ECHO_OFF | FILTER_OFF);

		if (*source_fullpath)
			srcpath = XtNewString (source_fullpath);
		else
			srcpath = NULL;

		/* reset original source */

		sprintf (list_src_cmd,"list %s:1\n", curr_src);

		query_gdb(list_src_cmd, PARSE_OFF | ECHO_OFF | FILTER_OFF);

		if (srcpath == NULL)
			srcpath = GetPathname (filename);	/* when info source is not supported */
	}

	XtFree (filename);

	return 	srcpath;
}
#endif /* GDB */
