/*
**
** popup.c
**
** Copyright (C) 1995-1997 Johannes Plass
** 
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
** 
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
** 
** Author:   Johannes Plass (plass@thep.physik.uni-mainz.de)
**           Department of Physic
**           Johannes Gutenberg-University
**           Mainz, Germany
**
*/

/*
#define MESSAGES
*/
#include "message.h"

#include "config.h"

#include <stdio.h>

#include "paths.h"
#include INC_X11(Intrinsic.h)
#include INC_X11(StringDefs.h)
#include INC_X11(Shell.h)

/*##################################################################*/
/* cb_popdownPopup Callback */
/*##################################################################*/

void
cb_popdownPopup(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    BEGINMESSAGE(cb_popdownPopup)
    XtPopdown((Widget)client_data);
    ENDMESSAGE(cb_popdownPopup)
}

/*##################################################################*/
/* cb_popupPopup Callback */
/* Popup a window. */
/*##################################################################*/

void
cb_popupPopup(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    BEGINMESSAGE(cb_popupPopup)
    XtPopup((Widget)client_data,XtGrabNone);
    XRaiseWindow(XtDisplay((Widget)client_data), XtWindow((Widget)client_data));
    ENDMESSAGE(cb_popupPopup)
}
