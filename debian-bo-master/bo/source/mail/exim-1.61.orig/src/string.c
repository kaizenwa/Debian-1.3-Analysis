/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Miscellaneous string-handling functions */


#include "exim.h"





/*************************************************
*              Format message size               *
*************************************************/

/* Convert a message size in bytes to printing form, rounding
according to the magnitude of the number. A value of zero causes
a string of spaces to be returned.

Arguments:
  size        the message size in bytes
  buffer      where to put the answer

Returns:      pointer to the buffer
              a string of exactly 5 characters is normally returned
*/

char *
string_formatsize(int size, char *buffer)
{
if (size == 0) strcpy(buffer, "     ");
else if (size < 1024) sprintf(buffer, "%5d", size);
else if (size < 10*1024)
  sprintf(buffer, "%4.1fK", (double)size / 1024.0);
else if (size < 1024*1024)
  sprintf(buffer, "%4dK", (size + 512)/1024);
else if (size < 10*1024*1024)
  sprintf(buffer, "%4.1fM", (double)size / (1024.0 * 1024.0));
else
  sprintf(buffer, "%4dM", (size + 512 * 1024)/(1024*1024));
return buffer;
}



/*************************************************
*       Convert a number to base 62 format       *
*************************************************/

/* Convert a long integer into an ASCII base 62 string.
Always return exactly 6 characters plus zero, in a static area.

Argument: a long integer
Returns:  pointer to base 62 string
*/

char *
string_base62(unsigned long int value)
{
static char yield[7];
static char base62_chars[] =
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
char *p = yield + sizeof(yield) - 1;
*p = 0;
while (p > yield)
  {
  *(--p) = base62_chars[value % 62];
  value /= 62;
  }
return yield;
}



/*************************************************
*          Interpret escape sequence             *
*************************************************/

/* This function is called from several places where escape sequences are to be
interpreted in strings.

Arguments:
  pp       points a pointer to the initiating "\" in the string;
           the pointer gets updated to point to the final character
Returns:   the value of the character escape
*/

int
string_interpret_escape(char **pp)
{
char *p = *pp;
int ch = *(++p);
if (isdigit(ch) && ch != '8' && ch != '9')
  {
  ch -= '0';
  if (isdigit(p[1]) && p[1] != '8' && p[1] != '9')
    {
    ch = ch * 8 + *(++p) - '0';
    if (isdigit(p[1]) && p[1] != '8' && p[1] != '9')
      ch = ch * 8 + *(++p) - '0';
    }
  }
else switch(ch)
  {
  case 'n':  ch = '\n'; break;
  case 'r':  ch = '\r'; break;
  case 't':  ch = '\t'; break;
  case 'x':
  ch = 0;
  if (isxdigit(p[1]))
    {
    ch = ch * 16 +
      strchr(hex_digits, tolower(*(++p))) - hex_digits;
    if (isxdigit(p[1])) ch = ch * 16 +
      strchr(hex_digits, tolower(*(++p))) - hex_digits;
    }
  break;
  }
*pp = p;
return ch;
}



/*************************************************
*          Ensure string is printable            *
*************************************************/

/* This function is called for critical strings. It checks for any
non-printing characters, and if any are found, it makes a new copy
of the string with suitable escape sequences. The old string is
freed if the flag is true.

Arguments:
  s          input string
  free_old   if TRUE and new string generated, free the old one

Returns:     string with non-printers encoded as printing sequences
*/

char *
string_printing(char *s, BOOL free_old)
{
int nonprintcount = 0;
int length = 0;
unsigned char *t = (unsigned char *)s;
char *ss, *tt;

while (*t != 0)
  {
  length++;
  if (!isprint(*t++)) nonprintcount++;
  }

if (nonprintcount == 0) return s;

/* Get a new block of store guaranteed big enough to hold the
expanded string. */

ss = store_malloc(length + nonprintcount * 4 + 1);

/* Copy everying, escaping non printers. The unsigned char thing is
for systems that have signed chars by default. */

t = (unsigned char *)s;
tt = ss;

while (*t != 0)
  {
  if (isprint(*t)) *tt++ = *t++; else
    {
    *tt++ = '\\';
    switch (*t)
      {
      case '\n': *tt++ = 'n'; break;
      case '\r': *tt++ = 'r'; break;
      case '\b': *tt++ = 'b'; break;
      case '\t': *tt++ = 't'; break;
      case '\v': *tt++ = 'v'; break;
      case '\f': *tt++ = 'f'; break;
      default: sprintf(tt, "%03o", *t); tt += 3; break;
      }
    t++;
    }
  }
*tt = 0;

/* Free the old if requested, and return the new. */

if (free_old) store_free(s);
return ss;
}




/*************************************************
*            Copy and save string                *
*************************************************/

/*
Argument: string to copy
Returns:  copy of string in new store
*/

char *
string_copy(char *s)
{
char *ss = (char *)store_malloc((int)strlen(s) + 1);
strcpy(ss, s);
return ss;
}



/*************************************************
*       Copy, lowercase and save string          *
*************************************************/

/*
Argument: string to copy
Returns:  copy of string in new store, with letters lowercased
*/

char *
string_copylc(char *s)
{
char *ss = (char *)store_malloc((int)strlen(s) + 1);
char *p = ss;
while (*s != 0) *p++ = tolower(*s++);
*p = 0;
return ss;
}



/*************************************************
*       Copy and save string, given length       *
*************************************************/

/* It is assumed the data contains no zeros. A zero is added
onto the end.

Arguments:
  s         string to copy
  n         number of characters

Returns:    copy of string in new store
*/

char *
string_copyn(char *s, int n)
{
char *ss = (char *)store_malloc(n + 1);
strncpy(ss, s, n);
ss[n] = 0;
return ss;
}


/*************************************************
* Copy, lowercase, and save string, given length *
*************************************************/

/* It is assumed the data contains no zeros. A zero is added
onto the end.

Arguments:
  s         string to copy
  n         number of characters

Returns:    copy of string in new store, with letters lowercased
*/

char *
string_copynlc(char *s, int n)
{
char *ss = (char *)store_malloc(n + 1);
char *p = ss;
while (n-- > 0) *p++ = tolower(*s++);
*p = 0;
return ss;
}



/*************************************************
*          Format a string and save it           *
*************************************************/

/*
Arguments:
  format    a printf() format
  ...       arguments for format

Returns:    pointer to fresh piece of store containing sprintf'ed string
*/

char *
string_sprintf(char *format, ...)
{
char buffer[8192];
va_list ap;
va_start(ap, format);
vsprintf(buffer, format, ap);
va_end(ap);
return string_copy(buffer);
}



/*************************************************
*         Case-independent strncmp() function    *
*************************************************/

/*
Arguments:
  s         first string
  t         second string
  n         number of characters to compare

Returns:    < 0, = 0, or > 0, according to the comparison
*/

int
strncmpic(char *s, char *t, int n)
{
while (n--)
  {
  int c = tolower(*s++) - tolower(*t++);
  if (c) return c;
  }
return 0;
}


/*************************************************
*         Case-independent strcmp() function     *
*************************************************/

/*
Arguments:
  s         first string
  t         second string

Returns:    < 0, = 0, or > 0, according to the comparison
*/

int
strcmpic(char *s, char *t)
{
while (*s != 0)
  {
  int c = tolower(*s++) - tolower(*t++);
  if (c) return c;
  }
return *t;
}


/*************************************************
*         Case-independent strstr() function     *
*************************************************/

/* The third argument specifies whether whitespace is required
to follow the matched string.

Arguments:
  s              string to search
  t              substring to search for
  space_follows  if TRUE, match only if whitespace follows

Returns:         pointer to substring in string, or NULL if not found
*/

char *
strstric(char *s, char *t, int space_follows)
{
char *p = t;
char *yield = NULL;
int cl = tolower(*p);
int cu = toupper(*p);

while (*s)
  {
  if (*s == cl || *s == cu)
    {
    if (yield == NULL) yield = s;
    if (*(++p) == 0)
      {
      if (!space_follows || s[1] == ' ' || s[1] == '\n' ) return yield;
      yield = NULL;
      p = t;
      }
    cl = tolower(*p);
    cu = toupper(*p);
    }
  else if (yield != NULL)
    {
    yield = NULL;
    p = t;
    cl = tolower(*p);
    cu = toupper(*p);
    }
  s++;
  }
return NULL;
}



/*************************************************
*       Get first string from separated list     *
*************************************************/

/* The maximum substring length is determined by the size of the
buffer for returning string values.

Arguments:
  list       points to list of strings to intialize a sequence;
             NULL to get the next string from previous sequence
  separator  the separator character; it can be represented in the
             list by duplication

Returns:     pointer to fixed string containing the next substring,
             or NULL if not more
*/

static unsigned char *listptr = NULL;

char *
string_firstinlist(char *list, int separator)
{
listptr = (unsigned char *)list;
return string_nextinlist(separator);
}



/*************************************************
*       Get next string from separated list      *
*************************************************/

/* The maximum substring length is determined by the size of the
buffer for returning string values.

Arguments:
  separator  the separator character; it can be represented in the
             list by duplication

Returns:     pointer to fixed string containing the next substring,
             or NULL if not more
*/

char *
string_nextinlist(int separator)
{
int p = 0;
unsigned char *s;

if (listptr == NULL || listptr[0] == 0) return NULL;

for (s = listptr; *s != 0; s++)
  {
  if (*s == separator && *(++s) != separator) break;
  if (p < STRING_RETURN_SIZE - 1) string_return_buffer[p++] = *s;
  }

string_return_buffer[p] = 0;
listptr = s;
return string_return_buffer;
}



/*************************************************
*             Add chars to string                *
*************************************************/

/* This function is used when building up strings of unknown length.
Room is always left for a terminating zero to be added.

Arguments:
  string   points to the start of the string
  size     the current size of the store (updated if changed)
  ptr      the offset at which to add characters, updated
  s        points to characters to add
  len      count of characters to add

Returns:   pointer to the start of the string, changed if copied for expansion
*/

char *
string_cat(char *string, int *size, int *ptr, char *s, int len)
{
int p = *ptr;
if (p + len >= *size)
  {
  char *newstring;
  while (*size <= p + len) *size += 50;
  newstring = (char *)store_malloc(*size);
  memcpy(newstring, string, p);
  store_free(string);
  string = newstring;
  }
strncpy(string + p, s, len);
*ptr = p + len;
return string;
}

/* End of string.c */
