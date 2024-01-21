/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USERS OWN RISK.
 */
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <unistd.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#include "Compat.h"
#include "Knapp.h"
#include "Layout.h"
#include "Message.h"
#include "Scrollable.h"
#include "ScrBar.h"
#include "ScrList.h"
#include "Toggle.h"
#include "TextField.h"
#include "Util.h"

#include "FileSelP.h"

/* a few dirty hacks */

#if !defined(MAXPATHLEN) || (MAXPATHLEN < 1024)
#  undef  MAXPATHLEN
#  define MAXPATHLEN 1024
#endif

static XtResource resources[] = {
    {XtNinput, XtCInput, XtRBool, sizeof(Bool),
     XtOffsetOf(FileSelRec, wm.wm_hints.input), XtRImmediate, (XtPointer)True},
    {XtNallowShellResize, XtCAllowShellResize, XtRBoolean, sizeof(Boolean),
     XtOffsetOf(FileSelRec, shell.allow_shell_resize),
     XtRImmediate, (XtPointer)True},
#define offset(field) XtOffsetOf(FileSelRec, filesel.field)
    {XtNcallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(callback), XtRCallback, (XtPointer)NULL},
    {XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
     offset(cursor), XtRString, (XtPointer)"top_left_arrow"},
    {XtNbusyCursor, XtCCursor, XtRCursor, sizeof(Cursor),
     offset(busy_cursor), XtRString, (XtPointer)"watch"},
    {XtNdirectory, XtCDirectory, XtRString, sizeof(String),
     offset(directory), XtRCallProc, NULL},
    {XtNpreferredColumns, XtCPreferredColumns, XtRDimension, sizeof(Dimension),
     offset(pref_cols), XtRImmediate, (XtPointer)16},
    {XtNshowDotFiles, XtCShowDotFiles, XtRBoolean, sizeof(Boolean),
     offset(show_dot_files), XtRImmediate, (XtPointer)True},
#undef offset
};

static void		Initialize(Widget, Widget, ArgList, Cardinal*);
static void		Destroy(Widget);
static void		Realize(Widget, XtValueMask*, XSetWindowAttributes*);
static Boolean		SetValues(Widget, Widget, Widget, ArgList, Cardinal*);

static void close_filesel(Widget, XEvent*, String*, Cardinal*);

static XtActionsRec actions[] = {
    {"close-filesel",	close_filesel},
};

static char translations[] =
"<Message>WM_PROTOCOLS:	close-filesel() \n";

FileSelClassRec fileSelClassRec = {
    {                                   /* core fields                  */
        (WidgetClass) &topLevelShellClassRec, /* superclass             */
        "FileSel",                      /* class_name                   */
        sizeof(FileSelRec),	        /* widget_size                  */
        NULL,                           /* class_initialize             */
        NULL,                           /* class_part_initialize        */
        FALSE,                          /* class_inited                 */
        Initialize,                     /* initialize                   */
        NULL,                           /* initialize_hook              */
        Realize,                        /* realize                      */
        actions,                        /* actions                      */
        XtNumber(actions),              /* num_actions                  */
        resources,                      /* resources                    */
        XtNumber(resources),            /* num_resources                */
        NULLQUARK,                      /* xrm_class                    */
        TRUE,                           /* compress_motion              */
	TRUE,				/* compress exposure		*/
        TRUE,                           /* compress_enterleave          */
        FALSE,                          /* visible_interest             */
        Destroy,                        /* destroy                      */
        XtInheritResize,                /* resize                       */
        NULL,	                        /* expose                       */
        SetValues,                      /* set_values                   */
        NULL,                           /* set_values_hook              */
        XtInheritSetValuesAlmost,       /* set_values_almost            */
        NULL,                           /* get_values_hook              */
        NULL,				/* accept_focus                 */
        XtVersion,                      /* version                      */
        NULL,                           /* callback_private             */
        translations,                   /* tm_table                     */
        XtInheritQueryGeometry,		/* query_geometry               */
        XtInheritDisplayAccelerator,    /* display_accelerator          */
        NULL                            /* extension                    */
    },
    {					/* composite fields		*/
	XtInheritGeometryManager,	/* geometry_manager		*/
	XtInheritChangeManaged,		/* change_managed		*/
	XtInheritInsertChild,		/* insert_child			*/
	XtInheritDeleteChild,		/* delete_child			*/
	NULL,				/* extension			*/
    },
    {					/* shell fields			*/
	NULL,				/* extension			*/
    },
    {					/* wm shell fields		*/
	NULL,				/* extension			*/
    },
    {					/* vendor shell fields		*/
	NULL,				/* extension			*/
    },
    {					/* transient shell fields	*/
	NULL,				/* extension			*/
    },
    {					/* filesel fields		*/
	NULL,				/* extension			*/
    }
};

WidgetClass fileSelWidgetClass = (WidgetClass)&fileSelClassRec;

/*************************************************************************/

typedef enum {
    FileTypeDirectory  = 0,
    FileTypeExecutable = 1,
    FileTypeBrokenlink = 2,
    FileTypeDocument   = 3,
    FileTypeSysdoc     = 4,
    FileTypeDotdot     = 5
} FileType;

#define ICON_SIZE	16

static void init_pixmaps(FileSelWidget w)
{
    static unsigned char
	icon_bits[MAX_FILE_TYPE + 1][ICON_SIZE * ICON_SIZE] =
    {
	/* FileTypeDirectory */
	{0x00, 0x00, 0x00, 0x00, 0x3c, 0x00, 0x42, 0x00,
	 0x81, 0xff, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80,
	 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80,
	 0x01, 0x80, 0x01, 0x80, 0xff, 0xff, 0x00, 0x00},
	/* FileTypeExecutable */
	{0xFF, 0xFF, 0x01, 0x80, 0x01, 0xB0, 0x01, 0xB0,
	 0x01, 0x80, 0xFD, 0xBF, 0x01, 0x80, 0x01, 0x80,
	 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80,
	 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0xFF, 0xFF},
	/* FileTypeBrokenlink */
	{0xFF, 0xFF, 0x01, 0x80, 0x01, 0x80, 0xC1, 0x81,
	 0x21, 0x82, 0x21, 0x82, 0x01, 0x82, 0x01, 0x81,
	 0x81, 0x80, 0x81, 0x80, 0x01, 0x80, 0x81, 0x80,
	 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0xFF, 0xFF},
	/* FileTypeDocument */
	{0xE0, 0x7F, 0x30, 0x40, 0x28, 0x40, 0x24, 0x40,
	 0x3E, 0x40, 0x02, 0x40, 0x02, 0x40, 0x02, 0x40, 
	 0x02, 0x40, 0x02, 0x40, 0x02, 0x40, 0x02, 0x40,
	 0x02, 0x40, 0x02, 0x40, 0x02, 0x40, 0xFE, 0x7F},
	/* FileTypeSysdoc */
	{0xF8, 0x1F, 0x04, 0x20, 0x04, 0x20, 0x04, 0x20,
	 0x04, 0x20, 0x04, 0x20, 0x04, 0x20, 0x04, 0x20, 
	 0x04, 0x20, 0x04, 0x20, 0xF8, 0x1F, 0xF8, 0x1F,
	 0x04, 0x20, 0x02, 0x40, 0x01, 0x80, 0xFF, 0xFF},
	/* FileTypeDotdot */
	{0x80, 0x01, 0xC0, 0x03, 0xE0, 0x07, 0xF0, 0x0F,
	 0xF8, 0x1F, 0xFC, 0x3F, 0xFE, 0x7F, 0xFF, 0xFF,
	 0xE0, 0x07, 0xE0, 0x07, 0xE0, 0x07, 0xE0, 0x07,
	 0xE0, 0x07, 0xE0, 0x07, 0xE0, 0x07, 0xE0, 0x07},
    };
    Display	*disp = XtDisplay(w);
    Window	root = RootWindow(disp, XScreenNumberOfScreen(XtScreen(w)));
    int		i;

    for (i = 0 ; i <= MAX_FILE_TYPE ; i++)
	w->filesel.pixmap[i] =
	    XCreateBitmapFromData(disp, root, (char *)icon_bits[i], 16, 16);
}

static const char *layout_string =
"vertical {"
"	height choose "
"	horizontal { "
"		2 "
"		vertical { "
"			0 <+inf> "
"			directory "
"			0 <+inf> "
"		} "
"		dirfield <+inf-inf*> "
"		4 + width scrbar "
"	} "
"	(height choose / 2) "
"	horizontal { "
"		2 "
"		list <+inf-inf*+inf-inf> "
"		2 "
"		scrbar <*+inf> "
"		2 "
"	} "
"	(height choose / 2) "
"	horizontal { "
"		2 "
"		vertical { "
"			0 <+inf> "
"			file "
"			0 <+inf> "
"		} "
"		(width directory - width file) "
"		filefield <+inf-inf*> "
"		4 + width scrbar "
"	} "
"	height choose "
"	horizontal { "
"		2 <+inf-inf>"
"		choose "
"		2 <+inf-inf> "
"		dotfiles "
"		2 <+inf-inf> "
"		cancel "
"		2 <+inf-inf> "
"	} "
"	height choose "
"} ";

#ifndef S_ISSOCK
#  if defined(S_IFSOCK) && defined(S_IFMT) /* suggested by Marc J. Fraioli */
#    define S_ISSOCK(mode) (((mode) & S_IFMT) == S_IFSOCK)
#  else
#    define S_ISSOCK(mode) 0
#  endif
#endif

#ifndef S_ISFIFO
#  if defined(S_IFFIFO) && defined(S_IFMT)
#    define S_ISFIFO(mode) (((mode) & S_IFMT) == S_IFFIFO)
#  else
#    define S_ISFIFO(mode) 0
#  endif
#endif

static FileType stat_file(char *buffer)
{
    struct stat	stat_buf;

    if (stat(buffer, &stat_buf) < 0)
	return FileTypeBrokenlink;

    if (S_ISDIR(stat_buf.st_mode))
	return FileTypeDirectory;
    if (stat_buf.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH))
	return FileTypeExecutable;
    if (S_ISREG(stat_buf.st_mode))
	return FileTypeDocument;
    if (S_ISCHR(stat_buf.st_mode) || S_ISFIFO(stat_buf.st_mode) ||
	S_ISBLK(stat_buf.st_mode) || S_ISSOCK(stat_buf.st_mode))
	return FileTypeSysdoc;

    return FileTypeBrokenlink;
}

static int strp_comp(const void *s1, const void *s2)
{
    return strcmp(*(const char **)s1, *(const char **)s2);
}

static int change_dir(FileSelWidget w, char *dir)
{
    char		buffer[MAXPATHLEN + 1];
    char		*new_dir = NULL;
    char		**list = NULL;
    struct stat		stat_buf;
    DIR			*dirp;
    struct dirent	*dp;
    long		i, n, n_alloc, len;
    Arg			arg;

    if (dir)
	if (*dir == '/')
	    new_dir = XtNewString(dir);
	else if (*dir == '\0')
	    new_dir = XtNewString(w->filesel.directory);
	else {
	    new_dir =
		XtMalloc(strlen(w->filesel.directory) + strlen(dir) + 8);
	    if (strcmp(w->filesel.directory, "/") == 0)
		sprintf(new_dir, "/%s", dir);
	    else
		sprintf(new_dir, "%s/%s", w->filesel.directory, dir);
	}
    else if (strcmp(w->filesel.directory, "/") == 0)
	new_dir = XtNewString("/");
    else {
	char	*c;

	new_dir = XtNewString(w->filesel.directory);
	len = strlen(new_dir);
	if (new_dir[len-1] == '/') {
	    new_dir[len-1] = '\0';
	    len--;
	}

	c = strrchr(new_dir, '/');
	if (c)
	    if (c == new_dir)
		new_dir[1] = '\0';
	    else
		*c = '\0';
    }

    if (stat(new_dir, &stat_buf) < 0) {
	perror(new_dir);
	XtFree(new_dir);
	return -1;
    }

    dirp = opendir(new_dir);
    if (!dirp) {
	perror(new_dir);
	XtFree(new_dir);
	return -1;
    }

    if (XtIsRealized((Widget)w)) {
	XDefineCursor(XtDisplay(w), XtWindow(w), w->filesel.busy_cursor);
	XFlush(XtDisplay(w));
    }

    n = 0;
    n_alloc = 0;
    while ( (dp = readdir(dirp)) ) {
	if (n > n_alloc - 2) {
	    n_alloc = 2 * (n_alloc + 1);
	    list = (char **)XtRealloc((char *)list,
				      n_alloc * sizeof(char *));
	}

	if (dp->d_name[0] != '.' ||
	    (dp->d_name[1] == '.' && dp->d_name[2] == '\0') ||
	    (w->filesel.show_dot_files && dp->d_name[1] != '\0')) {
	    list[n] = XtNewString(dp->d_name);
	    n++;
	}
    }

    qsort(list, n, sizeof(char *), strp_comp);

    XtFree(w->filesel.directory);
    w->filesel.directory = new_dir;
    sprintf(buffer, "%s/", new_dir);
    len = strlen(buffer);
    w->filesel.types = (unsigned char *)XtRealloc((char *)w->filesel.types, n);

    ScrListClearLines(w->filesel.list);
    ScrollableSuspend(w->filesel.list);

    for (i = 0 ; i < n ; i++) {
	FileType	type;

	if (list[i][0] == '.' &&
	    list[i][1] == '.' &&
	    list[i][2] == '\0')
	    type = FileTypeDotdot;
	else {
	    sprintf(buffer + len, "%s", list[i]);
	    type = stat_file(buffer);
	}
	ScrListAddLine(w->filesel.list, list[i], w->filesel.pixmap[type]);
	w->filesel.types[i] = type;
	XtFree(list[i]);
    }

    ScrollableResume(w->filesel.list);

    XtSetArg(arg, XtNbuffer, w->filesel.directory);
    XtSetValues(w->filesel.dir_field, &arg, 1);
    XtSetArg(arg, XtNbuffer, "");
    XtSetValues(w->filesel.file_field, &arg, 1);

    if (XtIsRealized((Widget)w)) {
	XDefineCursor(XtDisplay(w), XtWindow(w), w->filesel.cursor);
	XFlush(XtDisplay(w));
    }

    return 0;
}

static void call_callbacks(FileSelWidget w, long row)
{
    XtCallbackList	c_list = w->filesel.callback;
    char		*file = "";
    char		*call_data;
    long		len;
    int			do_free = False;

    if (row >= 0)
	file = ScrListGetString(w->filesel.list, row);
    else {
	file = TextFieldGetBuffer(w->filesel.file_field);
	do_free = True;
    }

    if (!c_list || !file || !w->filesel.directory) {
	if (do_free)
	    XtFree(file);
	return;
    }

    len = strlen(file) + strlen(w->filesel.directory) + 8;
    call_data = XtMalloc(len);
    if (file[0] == '/')
	sprintf(call_data, "%s%s", w->filesel.directory, file);
    else
	sprintf(call_data, "%s/%s", w->filesel.directory, file);

    XtCallCallbackList((Widget)w, c_list, (XtPointer)call_data);
    XtFree(call_data);
    if (do_free)
	XtFree(file);
}

static void dir_field_callback(Widget gw,
			       XtPointer client_data,
			       XtPointer call_data)
{
    FileSelWidget	w = (FileSelWidget)client_data;

    if (change_dir(w, (char *)call_data) < 0)
	XBell(XtDisplay(w), 0);
}

static void file_field_callback(Widget gw,
				XtPointer client_data,
				XtPointer call_data)
{
    FileSelWidget	w = (FileSelWidget)client_data;
    char		*buffer = (char *)call_data;
    long		n = ScrollableGetVSize(w->filesel.list);

    if (buffer) {
	long	i;

	for (i = 0 ; i < n ; i++) {
	    char	*item = ScrListGetString(w->filesel.list, i);

	    if (!item)
		break;
	    if (strcmp(item, buffer) == 0) {
		Pixmap	pixmap = ScrListGetPixmap(w->filesel.list, i);

		if (pixmap == w->filesel.pixmap[FileTypeDirectory]) {
		    if (change_dir(w, buffer) < 0)
			XBell(XtDisplay(w), 0);
		} else if (pixmap == w->filesel.pixmap[FileTypeDotdot]) {
		    if (change_dir(w, NULL) < 0)
			XBell(XtDisplay(w), 0);
		} else
		    call_callbacks(w, i);
		return;
	    }
	}
    }

    call_callbacks(w, -1);
}

static void file_field_tab_callback(Widget gw,
				    XtPointer client_data,
				    XtPointer call_data)
{
    FileSelWidget	w = (FileSelWidget)client_data;
    char		*buffer = (char *)call_data;
    char		*c = NULL;
    long		n = ScrollableGetVSize(w->filesel.list);
    Arg			arg;

    if (n > 0 && buffer) {
	long	len = strlen(buffer);
	long	first = 0;
	long	last;

	if (len == 0)
	    last = n;
	else {
	    while (first < n) {
		int	temp;
		char	*string = ScrListGetString(w->filesel.list, first);

		temp = strncmp(string, buffer, len);
		if (temp == 0)
		    break;
		if (temp > 0)
		    return;
		first++;
	    }
	    if (first == n)
		return;

	    last = first + 1;
	    while (last < n &&
		   strncmp(ScrListGetString(w->filesel.list, last),
			   buffer, len) == 0)
		last++;
	}

	c = ScrListGetString(w->filesel.list, first);
	for (;;) {
	    if (c[len] == '\0')
		break;
	    for (n = first + 1 ; n < last ; n++)
		if (ScrListGetString(w->filesel.list, n)[len] != c[len])
		    break;

	    if (n != last)
		break;
	    len++;
	}

	buffer = XtMalloc(len + 1);
	memcpy(buffer, c, len);
	buffer[len] = '\0';

	XtSetArg(arg, XtNbuffer, buffer);
	XtSetValues(gw, &arg, 1);

	XtFree(buffer);
    }
}

static void list_callback(Widget gw,
			  XtPointer client_data,
			  XtPointer call_data)
{
    FileSelWidget	w = (FileSelWidget)client_data;
    long		row = (long)call_data;
    Pixmap		pixmap = ScrListGetPixmap(gw, row);
    char		*file = ScrListGetString(gw, row);

    if (pixmap == w->filesel.pixmap[FileTypeDirectory]) {
	if (change_dir(w, file) < 0)
	    XBell(XtDisplay(w), 0);
    } else if (pixmap == w->filesel.pixmap[FileTypeDotdot]) {
	if (change_dir(w, NULL) < 0)
	    XBell(XtDisplay(w), 0);
    } else
	call_callbacks(w, row);
}

static void list_select_callback(Widget gw,
				 XtPointer client_data,
				 XtPointer call_data)
{
    FileSelWidget	w = (FileSelWidget)client_data;
    long		row = (long)call_data;
    char		*file = ScrListGetString(gw, row);
    Arg			arg;

    XtSetArg(arg, XtNbuffer, file);
    XtSetValues(w->filesel.file_field, &arg, 1);
}

static void cancel_callback(Widget gw,
			    XtPointer client_data,
			    XtPointer call_data)
{
    FileSelWidget	w = (FileSelWidget)client_data;
    XtCallbackList	c_list = w->filesel.callback;

    if (c_list)
	XtCallCallbackList((Widget)w, c_list, NULL);
}

static void choose_callback(Widget gw,
			    XtPointer client_data,
			    XtPointer call_data)
{
    FileSelWidget	w = (FileSelWidget)client_data;

    call_callbacks(w, -1);
}

static void show_callback(Widget gw,
			  XtPointer client_data,
			  XtPointer call_data)
{
    FileSelWidget	w = (FileSelWidget)client_data;
    Boolean		*set = (Boolean *)call_data;

    if (!set)
	return;

    *set = w->filesel.show_dot_files = !w->filesel.show_dot_files;
    change_dir(w, "");
}

static void popup_callback(Widget gw,
			   XtPointer client_data,
			   XtPointer call_data)
{
    FileSelWidget	w = (FileSelWidget)gw;
    long		n;
    Arg			arg;

    XtSetArg(arg, XtNbuffer, w->filesel.directory);
    XtSetValues(w->filesel.dir_field, &arg, 1);
    XtSetArg(arg, XtNbuffer, "");
    XtSetValues(w->filesel.file_field, &arg, 1);
    ScrollableSetVPos(w->filesel.list, 0);
    n = ScrListGetFirstSelected(w->filesel.list);
    if (n >= 0)
	ScrListSetSelected(w->filesel.list, n, False);
}

static void close_filesel(Widget gw, XEvent *event,
			  String *params, Cardinal *no_params)
{
    FileSelWidget	w = (FileSelWidget)gw;
    Display		*disp = XtDisplay(w);
    Atom		wm_delete_window, wm_protocols;
    XtCallbackList	c_list = w->filesel.callback;

    if (event->type != ClientMessage)
	return;

    wm_delete_window = intern_atom(disp, "WM_DELETE_WINDOW");
    wm_protocols     = intern_atom(disp, "WM_PROTOCOLS");

    if (event->xclient.message_type != wm_protocols ||
	event->xclient.data.l[0]    != wm_delete_window)
	return;

    if (c_list)
	XtCallCallbackList((Widget)w, c_list, NULL);
}

/*************************************************************************/

static void Initialize(Widget grequest, Widget gnew,
		       ArgList args, Cardinal *no_args)
{
    FileSelWidget	new = (FileSelWidget)gnew;
    Arg			arg[8];
    Pixel		pix;
    char		*home = getenv("HOME");
    Widget		layout, temp;

    init_pixmaps(new);
    new->filesel.types = NULL;

    XtAddCallback((Widget)new, XtNpopupCallback, popup_callback, NULL);

    layout =
	XtVaCreateManagedWidget("layout", layoutWidgetClass, (Widget)new,
				XtVaTypedArg, XtNlayout, XtRString,
				layout_string, sizeof(String),
				(void *)0);

    XtSetArg(arg[0], XtNbackground, &pix);
    XtGetValues(layout, arg, 1);
    XtSetArg(arg[0], XtNborderColor, pix);
    XtSetArg(arg[1], XtNpreferredChars, 20);
    XtSetArg(arg[2], XtNfocusRoot, new);
    XtSetArg(arg[3], XtNsingleLine, True);
    new->filesel.dir_field =
	XtCreateManagedWidget("dirfield", textFieldWidgetClass,
			      layout, arg, 4);
    XtAddCallback(new->filesel.dir_field, XtNcallback,
		  dir_field_callback, (XtPointer)new);
    new->filesel.file_field =
	XtCreateManagedWidget("filefield", textFieldWidgetClass,
			      layout, arg, 4);
    XtAddCallback(new->filesel.file_field, XtNcallback,
		  file_field_callback, (XtPointer)new);
    XtAddCallback(new->filesel.file_field, XtNtabCallback,
		  file_field_tab_callback, (XtPointer)new);

    XtCreateManagedWidget("directory", messageWidgetClass, layout, NULL, 0);
    XtCreateManagedWidget("file", messageWidgetClass, layout, NULL, 0);

    temp = XtCreateManagedWidget("scrbar", scrBarWidgetClass, layout, NULL, 0);

    XtSetArg(arg[0], XtNatLeastOne, False);
    XtSetArg(arg[1], XtNatMostOne, True);
    XtSetArg(arg[2], XtNdepthOne, True);
    XtSetArg(arg[3], XtNpixmapWidth, 16);
    XtSetArg(arg[4], XtNpixmapHeight, 16);
    XtSetArg(arg[5], XtNusePixmaps, True);
    XtSetArg(arg[6], XtNpreferredColumns, new->filesel.pref_cols);
    XtSetArg(arg[7], XtNvBar, temp);
    new->filesel.list =
	XtCreateManagedWidget("list", scrListWidgetClass, layout, arg, 8);
    XtAddCallback(new->filesel.list, XtNcallback,
		  list_callback, (XtPointer)new);
    XtAddCallback(new->filesel.list, XtNselectCallback,
		  list_select_callback, (XtPointer)new);

    temp = XtCreateManagedWidget("cancel", knappWidgetClass, layout, NULL, 0);
    XtAddCallback(temp, XtNcallback, cancel_callback, (XtPointer)new);

    temp = XtCreateManagedWidget("choose", knappWidgetClass, layout, NULL, 0);
    XtAddCallback(temp, XtNcallback, choose_callback, (XtPointer)new);

    XtSetArg(args[0], XtNset, new->filesel.show_dot_files);
    temp = XtCreateManagedWidget("dotfiles", toggleWidgetClass,
				 layout, args, 1);
    XtAddCallback(temp, XtNcallback, show_callback, (XtPointer)new);

    if (!new->filesel.directory)
	if (home)
	    new->filesel.directory = XtNewString(home);
	else
	    new->filesel.directory = XtNewString("/");
    else if (new->filesel.directory[0] == '/')
	new->filesel.directory = XtNewString(new->filesel.directory);
    else {
	char	*dir = new->filesel.directory;

	if (!home)
	    home = "";
	new->filesel.directory = XtMalloc(strlen(home) + strlen(dir) + 4);
	sprintf(new->filesel.directory, "%s/%s", home, dir);
    }

    XtSetKeyboardFocus((Widget)new, new->filesel.file_field);

    if (change_dir(new, new->filesel.directory) < 0)
	if (home)
	    change_dir(new, home);
}

static void Destroy(Widget gw)
{
    FileSelWidget	w = (FileSelWidget)gw;
    Display		*disp = XtDisplay(w);
    int			i;

    for (i = 0 ; i <= MAX_FILE_TYPE ; i++)
	XFreePixmap(disp, w->filesel.pixmap[i]);
    XtFree((char *)w->filesel.types);
}

static void Realize(Widget w, XtValueMask *mask,
		    XSetWindowAttributes *attributes)
{
    Display	*disp = XtDisplay(w);
    Atom	wm_delete_window;

    topLevelShellWidgetClass->core_class.realize(w, mask, attributes);

    wm_delete_window = intern_atom(disp, "WM_DELETE_WINDOW");
    XSetWMProtocols(disp, XtWindow(w), &wm_delete_window, 1);
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    Boolean	redisplay = False;

    return redisplay;
}
