#include <Xm/CutPaste.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <stdio.h>

Widget toplevel;
static void to_clipbd(), from_clipbd();

void
main(int argc, char **argv)
{
    Widget rowcol, button;
    XtAppContext app;

    XtSetLanguageProc(NULL, NULL, NULL);

    toplevel = XtVaAppInitialize(&app, "Demo", NULL, 0,
				 &argc, argv, NULL, NULL);

    rowcol = XtVaCreateWidget("rowcol", xmRowColumnWidgetClass,
			      toplevel, NULL);

    button = XtVaCreateManagedWidget("button1", xmPushButtonWidgetClass,
				     rowcol, XtVaTypedArg, XmNlabelString,
				     XmRString, "Copy To Clipboard", 18,
				     NULL);

    XtAddCallback(button, XmNactivateCallback, to_clipbd, NULL);

    button = XtVaCreateManagedWidget("button2", xmPushButtonWidgetClass,
				     rowcol, XtVaTypedArg, XmNlabelString,
				     XmRString, "Retrieve From Clipboard", 24,
				     NULL);

    XtAddCallback(button, XmNactivateCallback, from_clipbd, NULL);

    XtManageChild(rowcol);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);
}

static void
copy_by_name(Widget w, int *data_id, int *private_id, int *reason)
{
    Display *dpy = XtDisplay(toplevel);
    Window window = XtWindow(toplevel);
    static int cnt;
    int status;
    char buf[32];

    printf("Copy by name called\n\treason: %s, private_id: %d, data_id: %d\n",
	   *reason == XmCR_CLIPBOARD_DATA_REQUEST ? "request" : "delete",
	   *private_id, *data_id);

    if (*reason == XmCR_CLIPBOARD_DATA_REQUEST) {
	sprintf(buf, "stuff-%d", cnt++);

	do {
	    status = XmClipboardCopyByName(dpy, window, *data_id,
					   buf, strlen(buf)+1,
					   *private_id = cnt);
	} while (status != ClipboardSuccess);

	printf("copied: %s to clipboard\n", buf);
    }
}

static void
to_clipbd(Widget w, XtPointer client_data, XtPointer call_data)
{
    long item_id = 0;
    int status;
    XmString clip_label;
    Display *dpy = XtDisplayOfObject(toplevel);
    Window window = XtWindowOfObject(toplevel);

    clip_label = XmStringCreateLocalized("to_clipbd");

    do {
	status = XmClipboardBeginCopy(dpy, window, clip_label, w,
				      copy_by_name, &item_id);
    } while (status == ClipboardLocked);

    do {
	status = XmClipboardCopy(dpy, window, item_id, "STRING",
				 NULL, 8L, 0, NULL);
    } while (status == ClipboardLocked);

    XmStringFree(clip_label);

    do {
	status = XmClipboardEndCopy(dpy, window, item_id);
    } while (status == ClipboardLocked);

    printf("started copy to clipboard\n");
}

static void
from_clipbd(Widget w, XtPointer client_data, XtPointer call_data)
{
    int status;
    unsigned total_bytes;
    char *data, buf[32];
    unsigned long recvd;
    Display *dpy = XtDisplayOfObject(toplevel);
    Window window = XtWindowOfObject(toplevel);

    do {
	status = XmClipboardStartRetrieve(dpy, window, CurrentTime);
    } while (status == ClipboardLocked);
    
    data = XtMalloc(1);
    total_bytes = 1;

    do {
	buf[0] = 0;
	status = XmClipboardRetrieve(dpy, window, "STRING",
				     buf, sizeof(buf), &recvd, NULL);

	if (status == ClipboardNoData) {
	    printf("no data on clipboard\n");
	    break;
	}

	if (!(data = XtRealloc(data, total_bytes + recvd))) {
	    XtError("Can't allocate space for data");
	    break;
	}

	strncpy(&data[total_bytes-1], buf, recvd);
	total_bytes += recvd;

    } while (status == ClipboardTruncate);

    data[total_bytes] = 0;

    if (status == ClipboardSuccess)
	printf("Retrieved '%s' from clipboard.\n", data);

    status = XmClipboardEndRetrieve(dpy, window);
}

