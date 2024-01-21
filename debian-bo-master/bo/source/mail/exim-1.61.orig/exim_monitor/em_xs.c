/*************************************************
*               Exim Monitor                     *
*************************************************/

/* Copyright (c) University of Cambridge, 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* This file contains a number of subroutines that are in effect
just alternative packaging for calls to various X functions that
happen to be convenient for this program. */

#include "em_hdr.h"



/*************************************************
*                  xs_SetValues                  *
*************************************************/

/* Unpick a variable-length argument list and set up an
appropriate call to XtSetValues. To make it reasonably
efficient, we keep a working Arg structure of length 10;
anything setting more than that uses malloc/free. */

static Arg xs_temparg[10];

void xs_SetValues(Widget w, Cardinal num_args, ...)
{
int i;
va_list ap;
Arg *aa = (num_args > 10)? (Arg *)malloc(num_args*sizeof(Arg)) : xs_temparg;
va_start(ap, num_args);
for (i = 0; i < num_args; i++)
  {
  aa[i].name = va_arg(ap, String);
  aa[i].value = va_arg(ap, XtArgVal);
  }
XtSetValues(w, aa, num_args);
if (num_args > 10) free(aa);
}

/* End of em_xs.c */

