/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "exim.h"


/*************************************************
*            Add new header on end of chain      *
*************************************************/

/* header_last points to the last header during message reception and delivery;
otherwise it is NULL. We add new headers only when header_last is not NULL.
The function may get called sometimes when it is NULL (e.g. during address
verification where rewriting options exist).

Arguments:
  type      Exim header type character
  format    sprintf format
  ...       arguments for the format

Returns:    nothing
*/

void
header_add(int type, char *format, ...)
{
int slen;
header_line *new;
va_list ap;

if (header_last == NULL) return;

va_start(ap, format);
vsprintf(big_buffer, format, ap);
va_end(ap);
slen = (int)strlen(big_buffer);

new = (header_line *)store_malloc(sizeof(header_line) + slen);
strcpy(new->text, big_buffer);
new->next = NULL;
new->prev = header_last;
new->type = type;
new->slen = slen;

header_last->next = new;
header_last = new;
}


/*************************************************
*          Check the name of a header            *
*************************************************/

/* This function is necessary because the colon is allowed to be separated from
the name by white space. If it were not, a simple strncmpic would do.

Arguments:
  h         points to the header line
  name      points to the header name to be checked, excluding the :
  len       length of the name

Returns:    TRUE if the header starts with the name, followed by optional
            white space and a colon
*/

BOOL
header_checkname(header_line *h, char *name, int len)
{
char *text = h->text;
while (len-- > 0) if (tolower(*text++) != tolower(*name++)) return FALSE;
while (isspace(*text)) text++;
return (*text == ':');
}

/* End of header.c */
