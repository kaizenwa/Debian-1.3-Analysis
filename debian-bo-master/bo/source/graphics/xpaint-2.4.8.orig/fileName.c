/* +-------------------------------------------------------------------+ */
/* | Copyright 1992, 1993, David Koblas (koblas@netcom.com)	       | */
/* | Copyright 1995, 1996 Torsten Martinsen (bullestock@dk-online.dk)  | */
/* |								       | */
/* | Permission to use, copy, modify, and to distribute this software  | */
/* | and its documentation for any purpose is hereby granted without   | */
/* | fee, provided that the above copyright notice appear in all       | */
/* | copies and that both that copyright notice and this permission    | */
/* | notice appear in supporting documentation.	 There is no	       | */
/* | representations about the suitability of this software for	       | */
/* | any purpose.  this software is provided "as is" without express   | */
/* | or implied warranty.					       | */
/* |								       | */
/* +-------------------------------------------------------------------+ */

/* $Id: fileName.c,v 1.7 1996/06/09 17:27:49 torsten Exp $ */

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Scrollbar.h>

#include "Paint.h"
#include "PaintP.h"
#include "xpaint.h"
#include "misc.h"
#include "image.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#if defined(SYSV) || defined(SVR4)
#include <dirent.h>
#else
#include <sys/dir.h>
#endif
#include <pwd.h>

#include "rw/rwTable.h"
#include "graphic.h"
#include "protocol.h"

#define MAX_PATH	1024

#ifndef NOSTDHDRS
#include <stdlib.h>
#include <unistd.h>
#endif

/*
**  swiped from X11/Xfuncproto.h
**   since qsort() may or may not be defined with a constant sub-function
 */
#ifndef _Xconst
#if __STDC__ || defined(__cplusplus) || defined(c_plusplus) || (FUNCPROTO&4)
#define _Xconst const
#else
#define _Xconst
#endif
#endif				/* _Xconst */

static void *lastId = NULL;

void *
GetFileNameGetLastId()
{
    if (lastId == NULL)
	return RWtableGetReaderID();
    return lastId;
}

/*
**  "Std" Save functions
 */

/*
 * This function is called by all image save functions.
 * If 'flag' is True, save the entire image; otherwise, save the region.
 */
static void 
stdSaveCommonCallback(Widget paint, char *file, Boolean flag, RWwriteFunc f)
{
    Pixmap pix, mask = None;
    int width, height;
    Colormap cmap;
    Image *image;

    if (*file == '\0') {
	Notice(paint, "No file name supplied");
	return;
    }
    StateSetBusy(True);
    if (flag) {
	PwGetPixmap(paint, &pix, &width, &height);
    } else {
	if (!PwRegionGet(paint, &pix, &mask)) {
	    Notice(paint, "Unable to get region");
	    StateSetBusy(False);
	    return;
	}
    }
    XtVaGetValues(paint, XtNcolormap, &cmap, NULL);

    if ((image = PixmapToImage(paint, pix, cmap)) == NULL) {
	Notice(paint, "Unable to create image for saving");
	StateSetBusy(False);
	return;
    }
    if (mask != None)
	PixmapToImageMask(paint, image, mask);

    image->refCount++;
    if (f(file, image)) {
	Notice(paint, "Error saving file:\n   %s", RWGetMsg());
    } else if (flag) {
	PaintWidget p = (PaintWidget) paint;

	XtVaSetValues(paint, XtNdirty, False, NULL);
	if (p->paint.filename != NULL)
	    free(p->paint.filename);
	p->paint.filename = xmalloc(strlen(file) + 1);
	strcpy(p->paint.filename, file);
	EnableRevert(paint);
    }
    image->refCount--;
    ImageDelete(image);
    StateSetBusy(False);
}

static void 
saveRegionFileCallback(Widget paint, XtPointer str, XtPointer func)
{
    stdSaveCommonCallback(paint, (char *) str, False, (RWwriteFunc) func);
}

static void 
saveFileCallback(Widget paint, XtPointer str, XtPointer func)
{
    char *cp;

    stdSaveCommonCallback(paint, (char *) str, True, (RWwriteFunc) func);

    if ((cp = strrchr(str, '/')) == NULL)
	cp = str;
    else
	cp++;

    XtVaSetValues(GetShell(paint), XtNtitle, str, XtNiconName, cp, NULL);
}

void 
StdSaveRegionFile(Widget w, XtPointer paintArg, XtPointer junk)
{
    Widget paint = (Widget) paintArg;

    if (PwRegionGet(paint, NULL, NULL))
	GetFileName(paint, True, NULL, saveRegionFileCallback, NULL);
    else
	Notice(paint, "No region selected presently");
}

void 
StdSaveAsFile(Widget w, XtPointer paintArg, XtPointer junk)
{
    Widget paint = (Widget) paintArg;
    String name;
    String nm = XtName(GetShell(paint));

    XtVaGetValues(GetShell(paint), XtNtitle, &name, NULL);

    if (strcmp(name, DEFAULT_TITLE) == 0 || strcmp(nm, name) == 0)
	name = NULL;

    GetFileName(paint, True, name, saveFileCallback, NULL);
}

void 
StdSaveFile(Widget w, XtPointer paintArg, XtPointer junk)
{
    Widget paint = (Widget) paintArg;
    String name = NULL;
    String nm = XtName(GetShell(paint));
    RWwriteFunc f = NULL;
    void *id;

    if (strcmp(nm, "Canvas") == 0) {
	XtVaGetValues(GetShell(paint), XtNtitle, &name, NULL);

	if (strcmp(name, DEFAULT_TITLE) == 0 || strcmp(nm, name) == 0)
	    name = NULL;
    }
    if (name != NULL) {
	if ((id = getArgType(paint)) != NULL) {
	    f = (RWwriteFunc) RWtableGetWriter(id);
	} else if ((id = GraphicGetReaderId(paint)) != NULL) {
	    f = (RWwriteFunc) RWtableGetWriter(RWtableGetEntry(id));
	}
	if (f != NULL) {
	    stdSaveCommonCallback(paint, name, True, f);
	    return;

	}
    }
    GetFileName(paint, True, name, saveFileCallback, NULL);
}

/*
**
 */
void *
ReadMagic(char *file)
{
    RWreadFunc f = RWtableGetReader(RWtableGetEntry(MAGIC_READER));

    return (void *) f(file);
}

/*
**  The code begins
 */
typedef struct arg_s {
    XtCallbackProc okFunc;
    void *type;
    Boolean isRead, isSimple;
    Widget w, text, scrollbar;
    XtPointer closure;
    char dirname[MAX_PATH];
    Widget list, cwd_w, info;
    int first;
    Boolean isPoped;
    /*
    **	Use for caching, list purposes
     */
    Widget parent;
    int browserType;
    struct arg_s *next;
} arg_t;

static void 
fileTypeCallback(Widget w, XtPointer argArg, XtPointer junk)
{
    arg_t *arg = (arg_t *) argArg;
    String nm = XtName(w);

    arg->type = RWtableGetEntry(nm);
}

static void 
emptyList(arg_t * arg)
{
    String *strs;
    int i, n;

    if (arg->first) {
	arg->first = False;
	return;
    }
    XtVaGetValues(arg->list, XtNnumberStrings, &n, XtNlist, &strs, NULL);

    for (i = 0; i < n; i++)
	XtFree((XtPointer) strs[i]);
    XtFree((XtPointer) strs);
}

static char *
doDirname(arg_t * arg, char *file)
{
    static char newPath[MAX_PATH];
    char *cp;

    if (file == NULL) {
#ifdef NOSTDHDRS
	getwd(newPath);
#else
	getcwd(newPath, sizeof(newPath));
#endif
	return newPath;
    }
    if (*file == '~') {
	struct passwd *pw;

	file++;
	if (*file == '/' || *file == '\0') {
	    pw = getpwuid(getuid());
	} else {
	    char buf[80], *bp = buf;

	    while (*file != '/' && *file != '\0')
		*bp++ = *file++;
	    *bp = '\0';
	    pw = getpwnam(buf);
	}
	if (pw == NULL)
	    return NULL;
	while (*file == '/')
	    file++;
	strcpy(newPath, pw->pw_dir);
    } else if (*file == '/') {
	file++;
	newPath[0] = '\0';
    } else {
	strcpy(newPath, arg->dirname);
    }
    if (strcmp(newPath, "/") != 0)
	strcat(newPath, "/");
    while (*file != '\0') {
	char *ep;

	if ((ep = strchr(file, '/')) == NULL)
	    ep = file + strlen(file);
	if (strncmp(file, "./", 2) == 0 || strcmp(file, ".") == 0)
	    goto bottom;
	if (strncmp(file, "../", 3) == 0 || strcmp(file, "..") == 0) {
	    /*
	    **	First strip trailing '/'
	     */
	    char *cp = newPath + strlen(newPath) - 1;
	    if (cp != newPath)
		*cp = '\0';
	    cp = strrchr(newPath, '/');
	    if (cp == newPath)
		strcpy(newPath, "/");
	    else
		*cp = '\0';
	    goto bottom;
	}
	strncat(newPath, file, ep - file);
      bottom:
	file = ep;
	if (*file == '/')
	    strcat(newPath, "/");
	while (*file == '/')
	    file++;
    }

    /*
    **	Strip any trailing '/'s
     */
    cp = newPath + strlen(newPath) - 1;
    if (*cp == '/' && cp != newPath)
	*cp = '\0';

    return newPath;
}

static int 
strqsortcmp(char **a, char **b)
{
    String astr = *a;
    String bstr = *b;
    String aend = astr + strlen(astr) - 1;
    String bend = bstr + strlen(bstr) - 1;

    if (strncmp(astr, "../", 3) == 0)
	return -1;
    if (strncmp(bstr, "../", 3) == 0)
	return 1;

    if (*aend == '/' && *bend != '/')
	return -1;
    if (*aend != '/' && *bend == '/')
	return 1;
    return strcmp(astr, bstr);
}

static void 
setCWD(arg_t * arg, char *dir)
{
    DIR *dirp;
#if defined(SYSV) || defined(SVR4) || defined(__alpha)
    struct dirent *e;
#else
    struct direct *e;
#endif
    int count = 0, i = 0;
    int dirCount = 0, fileCount = 0;
    String *list;
    char fileStr[MAX_PATH], *filePtr;
    static char infoStr[256];
    struct stat statbuf;
    Widget sb;

    if (dir == NULL)
	dir = arg->dirname;

    if (stat(dir, &statbuf) < 0 || (statbuf.st_mode & S_IFDIR) == 0)
	return;

    if ((dirp = opendir(dir)) == NULL)
	return;

    StateSetBusyWatch(True);

    while (readdir(dirp) != NULL)
	count++;
    rewinddir(dirp);

    list = (String *) XtCalloc(sizeof(String *), count + 1);

    strcpy(fileStr, dir == NULL ? arg->dirname : dir);
    filePtr = fileStr + strlen(fileStr);
    *filePtr++ = '/';
    while ((e = readdir(dirp)) != NULL) {
	struct stat statbuf;
	char *nm = e->d_name;

	if (nm[0] == '.') {
	    /*
	    **	Skip '.'
	     */
	    if (nm[1] == '\0')
		continue;
	    if (nm[1] == '.' && nm[2] == '\0') {
		list[i++] = XtNewString("../ (Go up 1 directory level)");
		continue;
	    }
	}
	strcpy(filePtr, nm);
	if (stat(fileStr, &statbuf) < 0) {
	    list[i++] = XtNewString(nm);
	    continue;
	}
	if ((statbuf.st_mode & S_IFDIR) != 0) {
	    list[i] = XtMalloc(sizeof(char) * strlen(nm) + 2);
	    strcpy(list[i], nm);
	    strcat(list[i], "/");
	    i++;
	    dirCount++;
	    continue;
	}
	/*
	**  Now only if it is a file.
	 */
	if ((statbuf.st_mode & (S_IFMT & ~S_IFLNK)) == 0) {
	    list[i++] = XtNewString(nm);
	    fileCount++;
	}
    }

    closedir(dirp);

    emptyList(arg);

    qsort(list, i, sizeof(String),
	  (int (*)(_Xconst void *, _Xconst void *)) strqsortcmp);

    if (dir != NULL) {
	strcpy(arg->dirname, dir);
	XtVaSetValues(arg->cwd_w, XtNlabel, dir, NULL);
    }
    XawListChange(arg->list, list, i, 0, True);
    XtVaSetValues(arg->text, XtNstring, "", NULL);
    sprintf(infoStr, "%d Directories, %d Files", dirCount, fileCount);
    XtVaSetValues(arg->info, XtNlabel, infoStr, NULL);

    if ((sb = XtNameToWidget(arg->scrollbar, "vertical")) != None) {
	float top = 0.0;

	XawScrollbarSetThumb(sb, top, -1.0);
	XtCallCallbacks(sb, XtNjumpProc, (XtPointer) & top);
    }
    StateSetBusyWatch(False);
}

static void 
okCallback(Widget w, XtPointer argArg, XtPointer junk)
{
    arg_t *arg = (arg_t *) argArg;
    String str;
    RWreadFunc f;
    struct stat statbuf;
    char *file;
    char *cp;
    char *nm;

    XtVaGetValues(arg->text, XtNstring, &str, NULL);
    if (str == NULL || *str == '\0') {
	XawListReturnStruct *lr = XawListShowCurrent(arg->list);

	if (lr->list_index == XAW_LIST_NONE)
	    return;
	str = lr->string;
    }
    /*
    **	Got a valid string, check to see if it is a directory
    **	  if not try and read/write the file.
     */
    if ((file = doDirname(arg, str)) == NULL) {
	Notice(w, "No such file or directory:\n	    %s", str);
	return;
    }
    if (stat(file, &statbuf) >= 0 && (statbuf.st_mode & S_IFDIR) != 0) {
	setCWD(arg, file);
	return;
    }
    if ((cp = strrchr(file, '/')) != NULL) {
	*cp = '\0';
	if (stat(file, &statbuf) >= 0 && (statbuf.st_mode & S_IFDIR) != 0)
	    setCWD(arg, file);
	*cp = '/';
    }
    arg->isPoped = False;
    XtPopdown(GetShell(w));

    if (arg->isSimple) {
	arg->okFunc(arg->w, (XtPointer) arg->closure, (XtPointer) file);
	return;
    }
    if (arg->isRead)
	f = (RWreadFunc) RWtableGetReader(arg->type);
    else
	f = (RWreadFunc) RWtableGetWriter(arg->type);

    if (arg->type != NULL &&
	strcmp(nm = RWtableGetId(arg->type), MAGIC_READER) != 0)
	lastId = (void *) nm;
    else
	lastId = NULL;

    StateSetBusy(True);
    if (arg->okFunc != NULL) {
	if (!arg->isRead) {
	    arg->okFunc(arg->w, file, (XtPointer) f);
	} else {
	    Image *image = f(file);
	    if (image == NULL)
		Notice(w, "Unable to open input file \"%s\"\n	%s", file, RWGetMsg());
	    else
		arg->okFunc(arg->w, (XtPointer) file, (XtPointer) image);
	}
    }
    StateSetBusy(False);
}

static void 
cancelCallback(Widget w, XtPointer argArg, XtPointer junk)
{
    arg_t *arg = (arg_t *) argArg;
    arg->isPoped = False;
    XtPopdown(GetShell(w));
}

static void 
listCallback(Widget bar, XtPointer argArg, XtPointer itemArg)
{
    XawListReturnStruct *item = (XawListReturnStruct *) itemArg;
    arg_t *arg = (arg_t *) argArg;
    String str, label;

    if (strncmp(item->string, "../", 3) == 0)
	label = "../";
    else
	label = item->string;

    XtVaGetValues(arg->text, XtNstring, &str, NULL);

    if (strcmp(str, label) == 0)
	okCallback(bar, argArg, NULL);
    else
	XtVaSetValues(arg->text, XtNstring, label, NULL);
}

static Widget
buildBrowser(Widget parent, char *title, arg_t * arg)
{
    static Boolean inited = False;
    static XtTranslations trans, toglt;
    Widget form;
    Widget title_w, name, list, vport, cwd, info;

    if (!inited) {
	inited = True;

	trans = XtParseTranslationTable("#override\n\
						 <Key>Return: no-op()\n\
						 <Key>Linefeed: no-op()\n\
						 Ctrl<Key>M: no-op()\n\
						 Ctrl<Key>J: no-op()\n");
	toglt = XtParseTranslationTable("<BtnDown>,<BtnUp>: set() notify()");
    }
#ifdef NOSTDHDRS
    getwd(arg->dirname);
#else
    getcwd(arg->dirname, sizeof(arg->dirname));
#endif

    form = XtVaCreateManagedWidget("broswer",
				   formWidgetClass, parent,
				   XtNborderWidth, 0,
				   XtNright, XtChainRight,
				   XtNleft, XtChainLeft,
				   XtNtop, XtChainTop,
				   NULL);

    title_w = XtVaCreateManagedWidget("title",
				      labelWidgetClass, form,
				      XtNborderWidth, 0,
				      XtNlabel, title,
				      XtNtop, XtChainTop,
				      XtNbottom, XtChainTop,
				      XtNjustify, XtJustifyLeft,
				      NULL);

    name = XtVaCreateManagedWidget("name",
				   asciiTextWidgetClass, form,
				   XtNfromVert, title_w,
				   XtNeditType, XawtextEdit,
				   XtNwrap, XawtextWrapNever,
				   XtNresize, XawtextResizeWidth,
				   XtNtranslations, trans,
				   XtNbottom, XtChainTop,
				   XtNtop, XtChainTop,
				   XtNwidth, 250,
				   NULL);

    vport = XtVaCreateManagedWidget("vport",
				    viewportWidgetClass, form,
				    XtNfromVert, name,
				    XtNtop, XtChainTop,
				    XtNbottom, XtChainBottom,
				    XtNallowVert, True,
				    XtNallowHoriz, True,
				    XtNuseBottom, True,
				    XtNuseRight, True,
				    XtNwidth, 250,
				    XtNheight, 200,
				    NULL);

    list = XtVaCreateManagedWidget("files",
				   listWidgetClass, vport,
				   XtNborderWidth, 1,
				   XtNverticalList, True,
				   XtNforceColumns, True,
				   XtNdefaultColumns, 1,
				   XtNnumberStrings, 0,
				   NULL);

    cwd = XtVaCreateManagedWidget("cwd",
				  labelWidgetClass, form,
				  XtNlabel, arg->dirname,
				  XtNborderWidth, 0,
				  XtNfromVert, vport,
				  XtNtop, XtChainBottom,
				  XtNbottom, XtChainBottom,
				  XtNjustify, XtJustifyLeft,
				  XtNresizable, True,
				  NULL);
    info = XtVaCreateManagedWidget("info",
				   labelWidgetClass, form,
				   XtNborderWidth, 0,
				   XtNfromVert, cwd,
				   XtNtop, XtChainBottom,
				   XtNbottom, XtChainBottom,
				   XtNjustify, XtJustifyLeft,
				   NULL);

    XtAddCallback(list, XtNcallback, listCallback, (XtPointer) arg);

    arg->info = info;
    arg->cwd_w = cwd;
    arg->list = list;
    arg->text = name;
    arg->scrollbar = vport;

    arg->first = True;

    setCWD(arg, doDirname(arg, NULL));

    return form;
}

static Widget
buildOpenBroswer(Widget w, arg_t * arg)
{
    Widget shell, browser, okButton, cancelButton, form;
    Widget toggle, firstToggle = None;
    XtAccelerators accel;
    XtTranslations toglt;
    int i;
    char **list;

    shell = XtVaCreatePopupShell("filebrowser",
			       transientShellWidgetClass, GetToplevel(w),
				 NULL);
    form = XtVaCreateManagedWidget("form",
				   formWidgetClass, shell,
				   XtNborderWidth, 0,
				   NULL);
    browser = buildBrowser(form, "Open File Named:", arg);

    accel = XtParseAcceleratorTable("#override\n\
					 <Key>Return: set() notify() unset()\n\
					 <Key>Linefeed: set() notify() unset()");

    okButton = XtVaCreateManagedWidget("ok",
				       commandWidgetClass, form,
				       XtNfromHoriz, browser,
				       XtNaccelerators, accel,
				       XtNbottom, XtChainTop,
				       XtNleft, XtChainRight,
				       XtNright, XtChainRight,
				       NULL);
    cancelButton = XtVaCreateManagedWidget("cancel",
					   commandWidgetClass, form,
					   XtNfromHoriz, okButton,
					   XtNbottom, XtChainTop,
					   XtNleft, XtChainRight,
					   XtNright, XtChainRight,
					   NULL);

    toggle = okButton;
    toglt = XtParseTranslationTable("<BtnDown>,<BtnUp>: set() notify()");

    list = RWtableGetReaderList();

    for (i = 0; list[i] != NULL; i++) {
	toggle = XtVaCreateManagedWidget(list[i],
					 toggleWidgetClass, form,
					 XtNtranslations, toglt,
					 XtNradioGroup, firstToggle,
					 XtNfromVert, toggle,
					 XtNfromHoriz, browser,
					 XtNtop, XtChainBottom,
					 XtNbottom, XtChainBottom,
					 XtNleft, XtChainRight,
					 XtNright, XtChainRight,
					 NULL);
	if (firstToggle == None) {
	    arg->type = NULL;
	    XtVaSetValues(toggle, XtNstate, True,
			  XtNvertDistance, 100,
			  NULL);
	    firstToggle = toggle;
	}
	XtAddCallback(toggle, XtNcallback, fileTypeCallback, (XtPointer) arg);
    }

    arg->isSimple = False;
    arg->isRead = True;

    XtAddCallback(okButton, XtNcallback, okCallback, (XtPointer) arg);
    XtAddCallback(cancelButton, XtNcallback, cancelCallback, (XtPointer) arg);
    AddDestroyCallback(shell,
		       (DestroyCallbackFunc) cancelCallback, (XtPointer) arg);

    XtSetKeyboardFocus(form, arg->text);
    XtInstallAccelerators(arg->text, okButton);

    return shell;
}

static Widget
buildSaveBroswer(Widget w, arg_t * arg)
{
    Widget shell, browser, okButton, cancelButton, form;
    Widget toggle, firstToggle = None;
    XtAccelerators accel;
    XtTranslations toglt;
    int i;
    char **list, *rdr = NULL;

    shell = XtVaCreatePopupShell("filebrowser",
			       transientShellWidgetClass, GetToplevel(w),
				 NULL);
    form = XtVaCreateManagedWidget("form",
				   formWidgetClass, shell,
				   XtNborderWidth, 0,
				   NULL);
    browser = buildBrowser(form, "Save In File:", arg);

    accel = XtParseAcceleratorTable("#override\n\
					 <Key>Return: set() notify() unset()\n\
					 <Key>Linefeed: set() notify() unset()");

    okButton = XtVaCreateManagedWidget("ok",
				       commandWidgetClass, form,
				       XtNfromHoriz, browser,
				       XtNaccelerators, accel,
				       XtNbottom, XtChainTop,
				       NULL);
    cancelButton = XtVaCreateManagedWidget("cancel",
					   commandWidgetClass, form,
					   XtNfromHoriz, okButton,
					   XtNbottom, XtChainTop,
					   NULL);

    toggle = okButton;
    toglt = XtParseTranslationTable("<BtnDown>,<BtnUp>: set() notify()");
    list = RWtableGetWriterList();

    if (RWtableGetWriter(GraphicGetReaderId(w)) != NULL) {
	rdr = (char *) GraphicGetReaderId(w);
    }
    for (i = 0; list[i] != NULL; i++) {
	toggle = XtVaCreateManagedWidget(list[i],
					 toggleWidgetClass, form,
					 XtNtranslations, toglt,
					 XtNradioGroup, firstToggle,
					 XtNfromVert, toggle,
					 XtNfromHoriz, browser,
					 XtNtop, XtChainBottom,
					 XtNbottom, XtChainBottom,
					 NULL);
	if (firstToggle == None) {
	    arg->type = NULL;
	    XtVaSetValues(toggle, XtNvertDistance, 100, NULL);
	    firstToggle = toggle;
	    if (rdr == NULL)
		XtVaSetValues(toggle, XtNstate, True, NULL);
	}
	if (rdr != NULL && strcmp(rdr, list[i]) == 0) {
	    arg->type = RWtableGetEntry(list[i]);
	    XtVaSetValues(toggle, XtNstate, True, NULL);
	}
	XtAddCallback(toggle, XtNcallback, fileTypeCallback, (XtPointer) arg);
    }

    arg->isSimple = False;
    arg->isRead = False;

    XtAddCallback(okButton, XtNcallback, okCallback, (XtPointer) arg);
    XtAddCallback(cancelButton, XtNcallback, cancelCallback, (XtPointer) arg);
    AddDestroyCallback(shell,
		       (DestroyCallbackFunc) cancelCallback, (XtPointer) arg);

    XtSetKeyboardFocus(form, arg->text);
    XtInstallAccelerators(arg->text, okButton);

    return shell;
}

static Widget
buildSimpleBroswer(Widget w, arg_t * arg, Boolean isSave)
{
    Widget shell, browser, okButton, cancelButton, form;
    XtAccelerators accel;

    shell = XtVaCreatePopupShell("filebrowser",
			       transientShellWidgetClass, GetToplevel(w),
				 NULL);
    form = XtVaCreateManagedWidget("form",
				   formWidgetClass, shell,
				   XtNborderWidth, 0,
				   NULL);
    browser = buildBrowser(form, isSave ? "Save In File:"
			   : "Load from File:"
			   ,arg);

    accel = XtParseAcceleratorTable("#override\n\
					 <Key>Return: set() notify() unset()\n\
					 <Key>Linefeed: set() notify() unset()");

    okButton = XtVaCreateManagedWidget("ok",
				       commandWidgetClass, form,
				       XtNfromHoriz, browser,
				       XtNaccelerators, accel,
				       XtNbottom, XtChainTop,
				       NULL);
    cancelButton = XtVaCreateManagedWidget("cancel",
					   commandWidgetClass, form,
					   XtNfromHoriz, okButton,
					   XtNbottom, XtChainTop,
					   NULL);

    arg->isSimple = True;

    XtAddCallback(okButton, XtNcallback, okCallback, (XtPointer) arg);
    XtAddCallback(cancelButton, XtNcallback, cancelCallback, (XtPointer) arg);
    AddDestroyCallback(shell,
		       (DestroyCallbackFunc) cancelCallback, (XtPointer) arg);

    XtSetKeyboardFocus(form, arg->text);
    XtInstallAccelerators(arg->text, okButton);

    return shell;
}

/*
**  
**
 */

static arg_t *argList = NULL;

static void 
freeArg(Widget w, arg_t * arg)
{
    arg_t *c = argList, **pp = &argList;

    while (c != arg && c != NULL) {
	pp = &c->next;
	c = c->next;
    }

    *pp = arg->next;

    XtDestroyWidget(GetShell(arg->text));
    XtFree((XtPointer) arg);
}

void *
getArgType(Widget w)
{
    arg_t *cur;

    w = GetShell(w);

    for (cur = argList; cur != NULL; cur = cur->next)
	if (cur->parent == w &&
	    (cur->browserType == 0 ||
	     cur->browserType == 1))
	    break;

    if (cur == NULL)
	return NULL;
    return cur->type;
}


static arg_t *
getArg(Widget w, Boolean type, Boolean * built)
{
    Widget shell, p = GetShell(w);
    arg_t *cur;

    *built = False;

    for (cur = argList; cur != NULL; cur = cur->next)
	if (p == cur->parent && cur->browserType == type)
	    return cur;

    cur = XtNew(arg_t);
    cur->parent = p;
    cur->browserType = type;

    switch (type) {
    case 0:
	shell = buildOpenBroswer(w, cur);
	break;
    case 1:
	shell = buildSaveBroswer(w, cur);
	break;
    case 2:
	shell = buildSimpleBroswer(w, cur, True);
	break;
    case 3:
	shell = buildSimpleBroswer(w, cur, False);
	break;
    }

    cur->next = argList;	/* Add cur to front of list */
    argList = cur;

    XtAddCallback(p, XtNdestroyCallback,
		  (XtCallbackProc) freeArg, (XtPointer) cur);

    *built = True;

    return cur;
}

void 
GetFileName(Widget w, int type, char *def, XtCallbackProc okFunc, XtPointer data)
{
    arg_t *arg;
    Boolean built;
    Position x, y;

    XtVaGetValues(GetShell(w), XtNx, &x, XtNy, &y, NULL);

    arg = getArg(w, type, &built);
    arg->closure = data;

    XtVaSetValues(GetShell(arg->text), XtNx, x + 24, XtNy, y + 24, NULL);

    if (def != NULL && *def != '\0') {
	char *cp, dirname[MAX_PATH];
	strcpy(dirname, def);
	cp = strrchr(dirname, '/');

	if (cp != NULL && cp != dirname)
	    *cp++ = '\0';
	else if (cp == NULL)
	    cp = dirname;

	if (built)		/* This browser has not been used before, init it. */
	    setCWD(arg, doDirname(arg, dirname));

	XtVaSetValues(arg->text, XtNstring, cp,
		      XtNinsertPosition, strlen(cp),
		      NULL);
    } else if (!built) {
	/*
	**  This browser has been used before, rescan directory.
	 */
	setCWD(arg, NULL);
    }
    arg->okFunc = okFunc;
    arg->w = w;

    arg->isPoped = True;
    XtPopup(GetShell(arg->text), XtGrabNone);
}
