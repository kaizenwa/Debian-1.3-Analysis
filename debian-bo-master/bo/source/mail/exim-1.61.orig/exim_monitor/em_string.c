/*************************************************
*                 Exim Monitor                   *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "em_hdr.h"


/*************************************************
*           Remove part of a string              *
*************************************************/

/* If the second argument is NULL, then the first argument is
removed if present; otherwise everything between the first
and the second arguments, inclusive, is removed. */

void str_remove(char *s, char *start, char *end)
{
char *p = strstr(s, start);
char *q;
if (p == NULL) return;
if (end != NULL)
  {
  q = strstr(s, end);
  if (q == NULL) return;
  q += (int)strlen(end);
  }
else q = p + (int)strlen(start);
memmove(p, q, (int)strlen(s) - (q - s) + 1);
}



/*************************************************
*        Extract keyed field from a string       *
*************************************************/

/* The second argument is the key: everything following it up
to the next <words> occurrence of white space is returned as
the value, in bit of substr storage (see below). Leading spaces
are included in the data. Intermediate white spaces cannot be
newlines. If there is no match, return an empty string. */

char *str_subfield(char *s, char *k, int words)
{
char *q;
char *p = strstric(s, k, FALSE);
if (p == NULL) return "";
q = p += (int)strlen(k);
while (*q == ' ') q++;
while (*q && (*q != ' ' || --words > 0) && *q != '\n') q++;
return substr(s, p - s, q - p);
}




/***********************************************************
*                       Substring handling                 *
***********************************************************/

/* A number of the exim monitor routines need to extract substrings from
longer strings, and we do this into a large block of store that just
gets used up as we go along. There's a reset routine that reclaims
this store; this is called at the start of each exim monitor routine
that is going to extract ephemeral substrings. */

#define substr_buffer_size 4096

static char *substr_buffer = NULL;
static char *substr_ptr;


/*************************************************
*             Set up store for substrings        *
*************************************************/

void substr_reset(void)
{
if (substr_buffer == NULL)
  substr_buffer = (char *)malloc(substr_buffer_size);
substr_ptr = substr_buffer;
}


/*************************************************
*             Extract one substring              *
*************************************************/

char *substr(char *s, int start, int len)
{
char *ss = substr_ptr;
strncpy(substr_ptr, s + start, len);
substr_ptr[len] = 0;
substr_ptr += len + 1;
return ss;
}

/* End of em_string.c */
