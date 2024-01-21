/*************************************************
*               Exim Monitor                     *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "em_hdr.h"


/* This module contains functions for displaying text in a
text widget. It is not used for the log widget, because that
is dynamically updated and has special scrolling requirements. */


/*************************************************
*               Empty the widget                 *
*************************************************/

void text_empty(Widget w)
{
XawTextBlock b;
b.firstPos = 0;
b.ptr = (char *)(&b);
b.format = FMT8BIT;
b.length = 0;
XawTextReplace(w, 0, 999999, &b);
}


/*************************************************
*           Cause position to be displayed       *
*************************************************/

void text_position(Widget w, int position)
{
XawTextSetInsertionPoint(w, position);
}


/*************************************************
*                 Display text                   *
*************************************************/

void text_show(Widget w, char *s)
{
XawTextBlock b;
b.firstPos = 0;
b.ptr = s;
b.format = FMT8BIT;
b.length = (int)strlen(s);
XawTextReplace(w, 999999, 999999, &b);
XawTextSetInsertionPoint(w, 99999);
}


/*************************************************
*           Display text from format             *
*************************************************/

void text_showf(Widget w, char *s, ...)
{
va_list ap;
char buffer[1024];
va_start(ap, s);
vsprintf(buffer, s, ap);
va_end(ap);
text_show(w, buffer);
}

/* End of em_text.c */
