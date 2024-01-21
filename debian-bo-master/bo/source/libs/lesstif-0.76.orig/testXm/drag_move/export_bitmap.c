#include <Xm/Xm.h>
#include <Xm/AtomMgr.h>
#include "bitmap.h"

Boolean
export_bitmap(w, selection, target, type_return, value, length,
	      format, max_length, client_data, request_id)
	Widget          w;
	Atom           *selection, *target, *type_return;
	XtPointer      *value, client_data;
	unsigned long  *length, *max_length;
	int            *format;
{
	Bitmap_p        bitmap;
	XTextProperty   tmp_prop;
	int             status = 0;

	if (*target == XA_STRING) {
		/*
		 * Get bitmap name from clientData resource of the drag
		 * context and convert to property
		 */
		XtVaGetValues(w, XmNclientData, &bitmap, 0);
		status = XmbTextListToTextProperty(XtDisplay(w), &(bitmap->name), 1,
			       (XICCEncodingStyle) XStringStyle, &tmp_prop);
		if (status == Success || status > 0) {
			*type_return = XA_STRING;
			*format = 8;
			*value = (XtPointer) XtMalloc((unsigned) tmp_prop.nitems);
			memcpy((void *) *value,
			(void *) tmp_prop.value, (unsigned) tmp_prop.nitems);
			XFree((char *) tmp_prop.value);
			*length = tmp_prop.nitems;
			return True;
		}
	}
	/*
	 * Respond to "DELETE" transfer request by making the pixmap
	 * non-draggable
	 */
	if (*target == XmInternAtom(XtDisplay(w), "DELETE", False)) {
		XtVaGetValues(w, XmNclientData, &bitmap, 0);
		bitmap->draggable = False;
		*type_return = XmInternAtom(XtDisplay(w), "NULL", False);
		*value = NULL;
		*length = 0;
		*format = 8;
		return True;
	}
	return False;
}
