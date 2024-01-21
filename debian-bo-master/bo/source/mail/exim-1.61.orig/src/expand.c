/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


/* Functions for handling string expansion. */


#include "exim.h"


/* Recursively called function */

static char *expand_string_internal(char *, BOOL, char **, BOOL);



/*************************************************
*            Local statics and tables            *
*************************************************/

typedef struct {
  char *name;
  int   type;
  void *value;
} var_entry;

enum {
  vtype_int,            /* value is address of int */
  vtype_string,         /* value is address of string */
  vtype_stringptr,      /* value is address of pointer to string */
  vtype_msgbody,        /* as stringptr, but read when first required */
  vtype_localpart,      /* extract local part from string */
  vtype_domain,         /* extract domain from string */
  vtype_recipients,     /* extract recipients from chain of addresses */
                        /* (not currently in use) */
  vtype_todbsdin,       /* value not used; generate BSD inbox tod */
  vtype_todf,           /* value not used; generate full tod */
  vtype_todl,           /* value not used; generate log tod */
  vtype_reply           /* value not used; get reply from headers */
  };

/* This pointer gets set to values looked up in databases. It must
come before the following table. */

static char *lookup_value = NULL;

/* This table must be kept in alphabetical order. */

static var_entry var_table[] = {
  { "caller_gid",          vtype_int,         &real_gid },
  { "caller_uid",          vtype_int,         &real_uid },
  { "compile_date",        vtype_stringptr,   &version_date },
  { "compile_number",      vtype_stringptr,   &version_cnumber },
  { "domain",              vtype_stringptr,   &deliver_domain },
  { "home",                vtype_stringptr,   &deliver_home },
  { "host",                vtype_stringptr,   &deliver_host },
  { "key",                 vtype_stringptr,   &lookup_key },
  { "local_part",          vtype_stringptr,   &deliver_localpart },
  { "local_part_prefix",   vtype_stringptr,   &deliver_localpart_prefix },
  { "local_part_suffix",   vtype_stringptr,   &deliver_localpart_suffix },
  { "message_body",        vtype_msgbody,     &message_body },
  { "message_id",          vtype_stringptr,   &message_id },
  { "message_precedence",  vtype_stringptr,   &message_precedence },
  { "message_size",        vtype_int,         &message_size },
  { "original_domain",     vtype_stringptr,   &deliver_domain_orig },
  { "original_local_part", vtype_stringptr,   &deliver_localpart_orig },
  { "primary_hostname",    vtype_stringptr,   &primary_hostname },
  { "received_protocol",   vtype_stringptr,   &received_protocol },
  { "recipients_count",    vtype_int,         &recipients_count },
  { "reply_address",       vtype_reply,       NULL },
  { "return_path",         vtype_stringptr,   &return_path },
  { "route_option",        vtype_stringptr,   &route_option },
  { "sender_address",      vtype_stringptr,   &sender_address },
  { "sender_address_domain", vtype_domain,    &sender_address },
  { "sender_address_local_part", vtype_localpart, &sender_address },
  { "sender_fullhost",     vtype_stringptr,   &sender_fullhost },
  { "sender_host_address", vtype_stringptr,   &sender_host_address },
  { "sender_host_name",    vtype_stringptr,   &sender_host_name },
  { "sender_ident",        vtype_stringptr,   &sender_ident },
  { "spool_directory",     vtype_stringptr,   &spool_directory },
  { "tod_bsdinbox",        vtype_todbsdin,    NULL },
  { "tod_full",            vtype_todf,        NULL },
  { "tod_log",             vtype_todl,        NULL },
  { "value",               vtype_stringptr,   &lookup_value },
  { "version_number",      vtype_stringptr,   &version_string }
};

static int var_table_size = sizeof(var_table)/sizeof(var_entry);
static char var_buffer[256];





/*************************************************
*             Pick out a name from a string      *
*************************************************/

/* If the name is too long, it is silently truncated.

Arguments:
  name      points to a buffer into which to put the name
  max       is the length of the buffer
  s         points to the first alphabetic character of the name
  extras    chars other than alphanumerics to permit

Returns:    pointer to the first character after the name

Note: The test for *s != 0 in the while loop is necessary because
strchr() yields non-NULL if the character is zero (which is not something
I expected). */

static char *
read_name(char *name, int max, char *s, char *extras)
{
int ptr = 0;
while (*s != 0 && (isalnum(*s) || strchr(extras, *s) != NULL))
  {
  if (ptr < max-1) name[ptr++] = *s;
  s++;
  }
name[ptr] = 0;
return s;
}



/*************************************************
*     Pick out the rest of a header name         *
*************************************************/

/* A variable name starting $header_ (or just $h_ for those who like
abbreviations) might not be the complete header name because headers can
contain any printing characters in their names, except ':'. This function is
called to read the rest of the name, chop h[eader]_ off the front, and put ':'
on the end, if the name was terminated by white space.

Arguments:
  name      points to a buffer in which the name read so far exists
  max       is the length of the buffer
  s         points to the first character after the name so far, i.e. the
            first non-alphameric character after $header_xxxxx

Returns:    a pointer to the first character after the header name
*/

static char *
read_header_name(char *name, int max, char *s)
{
int prelen = strchr(name, '_') - name + 1;
int ptr = strlen(name) - prelen;
if (ptr > 0) memmove(name, name+prelen, ptr);
while (isgraph(*s) && *s != ':')
  {
  if (ptr < max-1) name[ptr++] = *s;
  s++;
  }
if (*s == ':') s++;
name[ptr++] = ':';
name[ptr] = 0;
return s;
}



/*************************************************
*           Pick out a number from a string      *
*************************************************/

/* Arguments:
  n     points to an integer into which to put the number
  s     points to the first digit of the number

Returns:  a pointer to the character after the last digit
*/

static char *
read_number(int *n, char *s)
{
*n = 0;
while (isdigit(*s)) *n = *n * 10 + (*s++ - '0');
return s;
}



/*************************************************
*        Extract keyed subfield from a string    *
*************************************************/

/* This function is also used by the search routines when given a multi-level
key to search for. The yield is in dynamic store; NULL means that the key
was not found.

Arguments:
  key       points to the name of the subkey
  s         points to the string from which to extract the subfield

Returns:    NULL if the subfield was not found, or
            a pointer to the subfield's data
*/

char *
expand_getkeyed(char *key, char *s)
{
int length = (int)strlen(key);
while (isspace(*s)) s++;

/* Loop to search for the key */

while (*s != 0)
  {
  int subkeylength, datalength;
  char *data;
  char *subkey = s;

  while (*s != 0 && *s != '=' && !isspace(*s)) s++;
  subkeylength = s - subkey;

  while (isspace(*s)) s++;
  if (*s == '=') while (isspace(*(++s)));

  /* For now, just find the end of the data field - interpret quoted
  string later if it is actually wanted. */

  data = s;
  if (*s == '\"')
    {
    while (*(++s) != 0 && *s != '\"')
      {
      if (*s == '\\' && s[1] != 0) s++;
      }
    if (*s == '\"') s++;
    }
  else while (*s != 0 && !isspace(*s)) s++;
  datalength = s - data;

  /* If keys match, set up the subfield as the yield and return. If
  the value is quoted, interpret the string (which cannot be longer than
  the original). */

  if (length == subkeylength && strncmp(key, subkey, length) == 0)
    {
    char *yield = store_malloc(datalength + 1);

    if (*data == '\"')
      {
      int i = 0;
      for (;;)
        {
        int ch = *(++data);
        if (ch == 0 || ch == '\"') break;
        if (ch == '\\') ch = string_interpret_escape(&data);
        yield[i++] = ch;
        }
      yield[i] = 0;
      }

    /* Not a quoted string */
    else
      {
      strncpy(yield, data, datalength);
      yield[datalength] = 0;
      }
    return yield;
    }

  /* Move on to next subkey */

  while (isspace(*s)) s++;
  }

return NULL;
}



/*************************************************
*               Find value of a variable         *
*************************************************/

/* The table of variables is kept in alphabetic order, so we
can search it using a binary chop. The "choplen" variable is
nothing to do with the binary chop. It can be set non-zero to
cause chars at the end of the returned string to be disregarded.
It should already be zero on entry.

Arguments:
  name      the name of the variable being sought
  choplen   a pointer to an int which is set if characters at the end
            of the returned data are to be ignored (typically '\n' at the
            end of header lines); it should normally be set zero before
            calling this function

Returns:    NULL if the variable does not exist, or
            a pointer to the variable's contents
*/

static char *
find_variable(char *name, int *choplen)
{
int first = 0;
int last = var_table_size;

while (last > first)
  {
  header_line *h;
  char *s, *domain, *localpart;
  char **ss;
  int middle = (first + last)/2;
  int c = strcmp(name, var_table[middle].name);

  if (c == 0) switch (var_table[middle].type)
    {
    case vtype_int:
    sprintf(var_buffer, "%d", *(int *)(var_table[middle].value)); /* Integer */
    return var_buffer;

    case vtype_string:                         /* String */
    return (char *)(var_table[middle].value);

    case vtype_stringptr:                      /* Pointer to string */
    s = *((char **)(var_table[middle].value));
    return (s == NULL)? "" : s;

    case vtype_localpart:                      /* Get local part from address */
    s = *((char **)(var_table[middle].value));
    if (s == NULL) return "";
    domain = strrchr(s, '@');
    if (domain == NULL) return s;
    localpart = domain - 1;
    while (localpart > s && localpart[-1] != ':') localpart--;
    strncpy(var_buffer, localpart, domain - localpart);
    var_buffer[domain - localpart] = 0;
    return var_buffer;

    case vtype_domain:                         /* Get domain from address */
    s = *((char **)(var_table[middle].value));
    if (s == NULL) return "";
    domain = strrchr(s, '@');
    return (domain == NULL)? "" : domain + 1;

    case vtype_msgbody:                        /* Pointer to msgbody string */
    ss = (char **)(var_table[middle].value);
    if (*ss == NULL && deliver_datafile >= 0)  /* Read body when needed */
      {
      char *body;
      int len = message_body_visible;
      if (len > message_size) len = message_size;
      *ss = body = store_malloc(len+1);
      body[0] = 0;
      lseek(deliver_datafile, data_start_offset, SEEK_SET);
      len = read(deliver_datafile, body, len);
      if (len >= 0) body[len] = 0;
      while (*body != 0)
        {
        if (*body == '\n') *body = ' ';
        body++;
        }
      }
    return (*ss == NULL)? "" : *ss;

    case vtype_todbsdin:                       /* BSD inbox time of day */
    return tod_stamp(tod_bsdin);

    case vtype_todf:                           /* Full time of day */
    return tod_stamp(tod_full);

    case vtype_todl:                           /* Log format time of day */
    return tod_stamp(tod_log);

    case vtype_reply:                          /* Get reply address */
    s = NULL;
    for (h = header_list; h != NULL; h = h->next)
      {
      if (h->type == htype_replyto) s = strchr(h->text, ':') + 1;
      if (h->type == htype_from && s == NULL) s = strchr(h->text, ':') + 1;
      }
    if (s == NULL) return "";
    while (isspace(*s)) s++;
    if (choplen != NULL) *choplen = 1;     /* Disregard final \n in header */
    return s;

    /****** This code was for an idea that didn't quite make it. Keep it
    around in case it gets revived one day. ******/

    #ifdef NEVER
    case vtype_recipients:                     /* Get recipients list */
      {
      address_item *addr;
      int size = 128;
      int ptr = 0;
      s = store_malloc(size);
      for (addr = (address_item *)(var_table[middle].value); addr != NULL;
           addr = addr->next)
        {
        if (ptr != 0) s = string_cat(s, &size, &ptr, " ", 1);
        s = string_cat(s, &size, &ptr, addr->orig, (int)strlen(addr->orig));
        }
      s[ptr] = 0;                             /* string_cat leaves room */
      }
    return s;
    #endif    /* NEVER */
    }

  else if (c > 0) first = middle + 1;
  else last = middle;
  }

return NULL;
}




/*************************************************
*          Find the value of a header            *
*************************************************/

/*
Arguments:
  name       the name of the header, without the leading $header_ or $h_
  freestore  a pointer to a BOOL which is set TRUE if the result is a
             concatenation of several headers in a new bit of store which
             the caller must subsequently free; it should be set FALSE on
             entry

Returns:     NULL if the header does not exist, else
             a pointer to the header's contents
*/

static char *
find_header(char *name, BOOL *freestore)
{
int len = (int)strlen(name);
char *yield = NULL;
header_line *h;

for (h = header_list; h != NULL; h = h->next)
  {
  if (h->type != htype_old)
    {
    if (len <= h->slen && strncmpic(name, h->text, len) == 0)
      {
      if (yield == NULL)
        {
        yield = h->text + len;
        while (isspace(*yield)) yield++;
        }
      else
        {
        char *newyield = store_malloc((int)strlen(yield) + h->slen - len);
        strcpy(newyield, yield);
        strcat(newyield, h->text + len);
        if (*freestore) store_free(yield); else *freestore = TRUE;
        yield = newyield;
        }
      }
    }
  }
return yield;
}




/*************************************************
*        Read and evaluate a condition           *
*************************************************/

/*
Arguments:
  s        points to the start of the condition text
  yield    points to a BOOL to hold the result of the condition test;
           if NULL, we are just reading through a condition that is
           part of an "or" combination to check syntax, or in a state
           where the answer isn't required

Returns:   a pointer to the first character after the condition, or
           NULL after an error
*/

static char *
eval_condition(char *s, BOOL *yield)
{
BOOL testfor = TRUE;
char name[256];

for (;;)
  {
  while (isspace(*s)) s++;
  if (*s == '!') { testfor = !testfor; s++; } else break;
  }

s = read_name(name, 256, s, "_");

/* def: tests for a non-zero or non-NULL variable */

if (strcmp(name, "def") == 0 && *s == ':')
  {
  char *value;
  s = read_name(name, 256, s+1, "_");
  value = find_variable(name, NULL);
  if (value == NULL)
    {
    expand_string_message = string_sprintf("unknown variable: %s", name);
    return NULL;
    }
  if (yield != NULL)
    *yield = (value[0] != 0 && strcmp(value, "0") != 0) == testfor;
  return s;
  }

/* exists: tests for file existence */

else if (strcmp(name, "exists") == 0)
  {
  char *filename;
  struct stat statbuf;
  while (isspace(*s)) s++;
  if (*s != '{') goto COND_FAILED_CURLY;
  filename = expand_string_internal(s+1, TRUE, &s, FALSE);
  if (filename == NULL) return NULL;
  if (*s++ != '}') goto COND_FAILED_CURLY;
  if (yield != NULL)
    *yield = (stat(filename, &statbuf) == 0) == testfor;
  store_free(filename);
  return s;
  }

/* eq: tests for string equality */

/* match: does a regular expression match and sets up the numerical
variables if it succeeds */

else if (strcmp(name, "eq") == 0 || strcmp(name, "match") == 0)
  {
  int i;
  regexp *re;
  char *rerror;
  char *sub[2];

  for (i = 0; i < 2; i++)
    {
    while (isspace(*s)) s++;
    if (*s != '{') goto COND_FAILED_CURLY;
    sub[i] = expand_string_internal(s+1, TRUE, &s, yield == NULL);
    if (sub[i] == NULL) return NULL;
    if (*s++ != '}') goto COND_FAILED_CURLY;
    }

  /* Result not required */

  if (yield == NULL)
    {
    store_free(sub[0]);
    store_free(sub[1]);
    return s;
    }

  /* Straight comparison */

  if (name[0] == 'e')
    {
    *yield = (strcmp(sub[0], sub[1]) == 0) == testfor;
    store_free(sub[0]);
    store_free(sub[1]);
    return s;
    }

  /* Regular expression match */

  rerror = "";
  regcomp_error_pointer = &rerror;
  re = regcomp(sub[1]);
  regcomp_error_pointer = NULL;
  if (re == NULL)
    {
    expand_string_message = string_sprintf("regular expression error in "
      "\"%s\": %s", sub[1], rerror);
    return NULL;
    }

  *yield = regexec(re, sub[0]);
  if (*yield)
    {
    expand_nmax = 0;
    for (; expand_nmax < NSUBEXP; expand_nmax++)
      {
      expand_nstring[expand_nmax] = re->startp[expand_nmax];
      expand_nlength[expand_nmax] = re->endp[expand_nmax] -
        expand_nstring[expand_nmax];
      }
    expand_nmax--;
    }

  free(re);   /* NB *not* store_free, as got by regcomp */

  store_free(sub[1]);  /* Can't free sub[0]; pointers point to it */
  return s;
  }

/* and/or: computes logical and/or of several conditions */

else if (strcmp(name, "or") == 0 || strcmp(name, "and") == 0)
  {
  BOOL temp;
  BOOL *ptr = (yield == NULL)? NULL : &temp;
  BOOL and = strcmp(name, "and") == 0;
  int comb = and;

  while (isspace(*s)) s++;
  if (*s++ != '{') goto COND_FAILED_CURLY;

  for (;;)
    {
    while (isspace(*s)) s++;
    if (*s != '{') break;
    s = eval_condition(s+1, ptr);
    if (s == NULL) return NULL;
    while (isspace(*s)) s++;
    if (*s++ != '}') goto COND_FAILED_CURLY;
    if (yield != NULL)
      {
      if (and)
        {
        comb &= temp;
        if (!comb) ptr = NULL;  /* once false, don't evaluate any more */
        }
      else
        {
        comb |= temp;
        if (comb) ptr = NULL;   /* once true, don't evaluate any more */
        }
      }
    }

  if (*s++ != '}') goto COND_FAILED_CURLY;
  if (yield != NULL) *yield = (comb == testfor);
  return s;
  }

/* Unknown type of condition */

expand_string_message = string_sprintf("unknown condition \"%s\"", name);
return NULL;

/* Bracketing error */

COND_FAILED_CURLY:
expand_string_message = "missing or misplaced { or }";
return NULL;
}




/*************************************************
*                 Expand string                  *
*************************************************/

/* Returns the expanded string in malloc'ed store. Interpreted
sequences are:

   \c                    where c is any character -> copies c literally
   $name                 substitutes the variable
   ${name}               ditto
   ${op:string}          operates on the string value
   ${extract {key} {string}}
                         extracts keyed substring; null if not found
   ${if cond {s1} {s2}}  conditional; the yielded string is expanded
                         {s2} can be replaced by "fail" or be omitted

One-key+file lookups:
   ${lookup{key}search-type{file}{found}{not-found}}
                         the key, file, & strings are expanded; $value available
                         and {not-found} can be replaced by "fail" or be
                         omitted.
                         The key can in fact consist of mainkey:subkey, in
                         which case a subfield is extracted from the found
                         string, which must consist of key=value pairs.

Database-query lookups:
   ${lookup search-type{query}{found}{not-found}}
                         the query and strings are expanded; $value available
                         and {not-found} can be replaced by "fail" or be
                         omitted.

Operators:
   lc                      lowercase the string
   length_<n>              take first n chars only
   l_<n>                   ditto
   substr_<m>_<n>          substring n chars from offset m
   s_<m>_<n>               ditto
   expand                  expands the string one more time

Conditions:
   !cond                   negates condition
   def:variable            variable is defined and not empty
   exists {string}         file exists
   match {string}{re}      regular expression match
   eq {string1}{string2}   strings are equal, case included
   or {{cond1}{cond2}...}  as it says
   and {{cond1}{cond2}...} ditto

We use an internal routine recursively to handle embedded substrings. The
external function follows. The yield is NULL if the expansion failed, and there
are two cases: if something collapsed syntactically, or if "fail" was given
as the action on a lookup failure. These can be distinguised by looking at the
variable expand_string_forcedfail, which is TRUE in the latter case.

The skipping flag is set true when expanding a substring that isn't actually
going to be used (after "if" or "lookup") and it prevents lookups from
happening lower down.

Arguments:
  string         the string to be expanded
  ket_ends       true if expansion is to stop at }
  left           if not NULL, a pointer to the first character after the
                 expansion is placed here (typically used with ket_ends)
  skipping       TRUE for recursive calls when the value isn't actually going
                 to be used (to allow for optimisation)

Returns:         NULL if expansion fails:
                   expand_string_forcefail is set TRUE if failure was forced
                   expand_string_message contains a textual error message
                 a pointer to the expanded string on success
*/

static char *
expand_string_internal(char *string, BOOL ket_ends, char **left, BOOL skipping)
{
int ptr = 0;
int size = sizeof(string) + 50;
char *yield = (char *)store_malloc(size);
char *s = string;

expand_string_forcedfail = FALSE;
expand_string_message = "";

while (*s != 0)
  {
  char *value;
  char name[256];

  /* \ escapes the next character, which must exist, or else
  the expansion fails. */

  if (*s == '\\')
    {
    if (*(++s) == 0)
      {
      expand_string_message = "\\ at end of string";
      goto EXPAND_FAILED;
      }
    yield = string_cat(yield, &size, &ptr, s++, 1);
    continue;
    }

  /* Anything other than $ is just copied verbatim, unless we are
  looking for a terminating } character. */

  if (ket_ends && *s == '}') break;

  if (*s != '$')
    {
    yield = string_cat(yield, &size, &ptr, s++, 1);
    continue;
    }

  /* No { after the $ - must be a plain name or a number for string
  match variable. There has to be a fudge for variables that are the
  names of header fields preceded by "$header_" because header field
  names can contain any printing characters except space and colon.
  For those that don't like typing this much, "$h_" is a synonym for
  "$header_". A non-existent header yields a NULL value; nothing is
  inserted. */

  if (isalpha(*(++s)))
    {
    BOOL freestore = FALSE;
    int choplen = 0;
    s = read_name(name, 256, s, "_");
    if (strncmp(name, "header_", 7) == 0 || strncmp(name, "h_", 2) == 0)
      {
      s = read_header_name(name, 256, s);
      value = find_header(name, &freestore);
      choplen = 1;
      }
    else
      {
      value = find_variable(name, &choplen);
      if (value == NULL)
        {
        expand_string_message = string_sprintf("unknown variable: %s", name);
        goto EXPAND_FAILED;
        }
      }
    if (value != NULL)
      {
      int len = (int)strlen(value) - choplen;
      if (len < 0) len = 0;
      yield = string_cat(yield, &size, &ptr, value, len);
      if (freestore) store_free(value);
      }
    continue;
    }

  if (isdigit(*s))
    {
    int n;
    s = read_number(&n, s);
    if (n <= expand_nmax)
      yield = string_cat(yield, &size, &ptr, expand_nstring[n],
        expand_nlength[n]);
    continue;
    }

  /* Otherwise, if there's no '{' after $ it's an error. */

  if (*s != '{')
    {
    expand_string_message = "$ not followed by letter, digit, or {";
    goto EXPAND_FAILED;
    }

  /* After { there can be various things, but they all start with
  an initial word, except for a number for a string match variable. */

  if (isdigit(*(++s)))
    {
    int n;
    s = read_number(&n, s);
    if (*s++ != '}')
      {
      expand_string_message = "} expected after number";
      goto EXPAND_FAILED;
      }
    if (n <= expand_nmax)
      yield = string_cat(yield, &size, &ptr, expand_nstring[n],
        expand_nlength[n]);
    continue;
    }

  if (!isalpha(*s))
    {
    expand_string_message = "letter or digit expected after ${";
    goto EXPAND_FAILED;
    }
  s = read_name(name, 256, s, "_");

  /* Handle conditionals - preserve the values of the numerical expansion
  variables in case they get changed by a regular expression match in the
  condition. If not, they retain their external settings. At the end
  of this "if" section, they get restored to their previous values. */

  if (strcmp(name, "if") == 0)
    {
    BOOL cond = TRUE;
    char *sub1, *sub2;
    char *save_expand_nstring[EXPAND_MAXN+1];
    int save_expand_nlength[EXPAND_MAXN+1];
    int save_expand_nmax = expand_nmax;
    int i;

    for (i = 0; i <= expand_nmax; i++)
      {
      save_expand_nstring[i] = expand_nstring[i];
      save_expand_nlength[i] = expand_nlength[i];
      }

    s = eval_condition(s, skipping? NULL : &cond);
    if (s == NULL) goto EXPAND_FAILED;  /* message already set */

    /* The condition must be followed by one or two substrings,
    enclosed in braces, with optional space between them and
    after them. Then there must be a final } to end. */

    while (isspace(*s)) s++;
    if (*s != '{') goto EXPAND_FAILED_CURLY;
    sub1 = expand_string_internal(s+1, TRUE, &s, skipping);
    if (sub1 == NULL) goto EXPAND_FAILED;
    if (*s++ != '}') goto EXPAND_FAILED_CURLY;

    while (isspace(*s)) s++;
    if (*s == '}') sub2 = NULL;
    else if (*s == '{')
      {
      sub2 = expand_string_internal(s+1, TRUE, &s, skipping);
      if (sub2 == NULL) goto EXPAND_FAILED;
      if (*s++ != '}') goto EXPAND_FAILED_CURLY;
      }
    else
      {
      s = read_name(name, 256, s, "_");
      if (strcmp(name, "fail") == 0)
        {
        if (!cond)
          {
          expand_string_message = "\"if\" failed and \"fail\" requested";
          expand_string_forcedfail = TRUE;
          goto EXPAND_FAILED;
          }
        else sub2 = NULL;
        }
      else
        {
        expand_string_message = "syntax error in \"else\" substring";
        goto EXPAND_FAILED;
        }
      }

    while (isspace(*s)) s++;
    if (*s++ != '}') goto EXPAND_FAILED_CURLY;

    /* Add the appropriate string to the result */

    if (cond)
      yield = string_cat(yield, &size, &ptr, sub1, (int)strlen(sub1));
    else if (sub2 != NULL)
      yield = string_cat(yield, &size, &ptr, sub2, (int)strlen(sub2));

    store_free(sub1);
    if (sub2 != NULL) store_free(sub2);

    /* Restore external setting of expansion variables */

    expand_nmax = save_expand_nmax;
    for (i = 0; i <= expand_nmax; i++)
      {
      expand_nstring[i] = save_expand_nstring[i];
      expand_nlength[i] = save_expand_nlength[i];
      }

    continue;
    }

  /* Handle database lookups. If "skipping" is TRUE, we are expanding an
  internal string that isn't actually going to be used. All we need to
  do is check the syntax, so don't do a lookup at all. */

  if (strcmp(name, "lookup") == 0)
    {
    int stype;
    int pv = 0;
    void *handle;
    char *ss, *key, *filename, *sub1, *sub2;
    char *lookup_errmsg = NULL;
    char *save_lookup_value = lookup_value;

    lookup_value = NULL;

    /* Get the key we are to look up for single-key+file style lookups.
    Otherwise set the key NULL pro-tem. */

    while (isspace(*s)) s++;
    if (*s == '{')
      {
      key = expand_string_internal(s+1, TRUE, &s, FALSE);
      if (key == NULL) goto EXPAND_FAILED;
      if (*s++ != '}') goto EXPAND_FAILED_CURLY;
      while (isspace(*s)) s++;
      }
    else key = NULL;

    /* Find out the type of database */

    if (!isalpha(*s))
      {
      expand_string_message = "missing lookup type";
      goto EXPAND_FAILED;
      }
    s = read_name(name, 256, s, "-_");
    ss = name;

    if (strncmp(ss, "partial", 7) == 0)
      {
      ss += 7;
      if (isdigit (*ss))
        while (isdigit(*ss)) pv = pv*10 + *ss++ - '0';
      else pv = 2;
      if (*ss++ != '-')
        {
        expand_string_message = string_sprintf("unknown lookup type \"%s\"",
          name);
        goto EXPAND_FAILED;
        }
      }

    if (strcmp(ss, "lsearch") == 0) stype = stype_lsearch;
    else if (strcmp(ss, "dbm") == 0) stype = stype_dbm;
    else if (strcmp(ss, "nis") == 0)
      {
      if (have_nis) stype = stype_nis; else
        {
        expand_string_message = "lookup type \"nis\" unavailable";
        goto EXPAND_FAILED;
        }
      }
    else if (strcmp(ss, "nis0") == 0)
      {
      if (have_nis) stype = stype_nis0; else
        {
        expand_string_message = "lookup type \"nis0\" unavailable";
        goto EXPAND_FAILED;
        }
      }
    else if (strcmp(ss, "nisplus") == 0)
      {
      if (have_nisplus) stype = stype_nisplus; else
        {
        expand_string_message = "lookup type \"nisplus\" unavailable";
        goto EXPAND_FAILED;
        }
      }
    else
      {
      expand_string_message = string_sprintf("unknown lookup type \"%s\"",
        name);
      goto EXPAND_FAILED;
      }

    /* Check that a key was provided for those lookup types that need it,
    and was not supplied for those that use the query style, and that
    "partial" was provided only for a non-query lookup. */

    if (stype < stype_querystyle)
      {
      if (key == NULL)
        {
        expand_string_message = string_sprintf("missing {key} for single-"
          "key \"%s\" lookup", name);
        goto EXPAND_FAILED;
        }
      }
    else
      {
      if (pv > 0)
        {
        expand_string_message = string_sprintf("\"partial\" is not permitted "
          "for lookup type \"%s\"", ss);
        goto EXPAND_FAILED;
        }

      if (key != NULL)
        {
        expand_string_message = string_sprintf("a single key was given for "
          "lookup type \"%s\", which is not a single-key lookup type", name);
        goto EXPAND_FAILED;
        }
      }

    /* Get the next string in quotes and expand it. It is the file name for
    single-key+file lookups, and the whole query otherwise. */

    while (isspace(*s)) s++;
    if (*s != '{') goto EXPAND_FAILED_CURLY;
    filename = expand_string_internal(s+1, TRUE, &s, FALSE);
    if (filename == NULL) goto EXPAND_FAILED;
    if (*s++ != '}') goto EXPAND_FAILED_CURLY;
    while (isspace(*s)) s++;

    /* If this isn't a single-key+file lookup, re-arrange the variables
    to be appropriate for the search_ functions. */

    if (key == NULL)
      {
      key = filename;
      filename = NULL;
      }

    /* If skipping, don't do the next bit - it has the result of
    leaving lookup_value == NULL, as if the entry was not found. The
    search_close() function is not called here, leaving any files open.
    This is deliberate; at suitable places in higher logic, search_tidyup()
    is called to tidy all open files. This can save opening the same file
    several times.

    For the moment, do not request search_find to set up $1 on a partial
    match, because that could conflict with existing numeric variables.
    Thought needed for possible future addition here. */

    if (!skipping)
      {
      handle = search_open(filename, stype, 0, NULL, NULL,
        &expand_string_message);
      if (handle == NULL) goto EXPAND_FAILED;
      lookup_value = search_find(handle, filename, key, stype,
        pv, NULL, &lookup_errmsg);
      }

    /* The key and file name strings are no longer needed */

    store_free(key);
    if (filename != NULL) store_free(filename);

    /* Expand the first substring; $value will get the looked up value. */

    while (isspace(*s)) s++;
    if (*s != '{') goto EXPAND_FAILED_CURLY;
    sub1 = expand_string_internal(s+1, TRUE, &s, lookup_value == NULL);
    if (sub1 == NULL) goto EXPAND_FAILED;
    if (*s++ != '}') goto EXPAND_FAILED_CURLY;

    /* If the lookup succeeded, add the new string to the output. */

    if (lookup_value != NULL)
      yield = string_cat(yield, &size, &ptr, sub1, (int)strlen(sub1));

    /* There now follows either another substring, or "fail", or nothing. */

    while (isspace(*s)) s++;
    if (*s == '}') sub2 = NULL;
    else if (*s == '{')
      {
      sub2 = expand_string_internal(s+1, TRUE, &s, lookup_value != NULL);
      if (sub2 == NULL) goto EXPAND_FAILED;
      if (*s++ != '}') goto EXPAND_FAILED_CURLY;
      if (lookup_value == NULL)
        yield = string_cat(yield, &size, &ptr, sub2, (int)strlen(sub2));
      }

    /* If the word "fail" is present, and lookup failed, set a flag indicating
    it was a forced failure rather than a syntactic error. */

    else
      {
      s = read_name(name, 256, s, "_");
      if (strcmp(name, "fail") == 0)
        {
        if (lookup_value == NULL)
          {
          expand_string_message =
            string_sprintf("lookup failed and \"fail\" requested%s%s",
              (lookup_errmsg == NULL)? "" : ": ",
              (lookup_errmsg == NULL)? "" : lookup_errmsg);
          expand_string_forcedfail = TRUE;
          goto EXPAND_FAILED;
          }
        else sub2 = NULL;
        }
      else
        {
        expand_string_message = "syntax error in \"not found\" substring";
        goto EXPAND_FAILED;
        }
      }

    while (isspace(*s)) s++;
    if (*s++ != '}') goto EXPAND_FAILED_CURLY;

    /* Free store and continue */

    store_free(sub1);
    if (sub2 != NULL) store_free(sub2);
    if (lookup_value != NULL) store_free(lookup_value);
    lookup_value = save_lookup_value;
    continue;
    }

  /* Handle keyed substring extraction */

  if (strcmp(name, "extract") == 0)
    {
    int i;
    char *sub[3];
    for (i = 0; i < 2; i++)
      {
      while (isspace(*s)) s++;
      if (*s != '{') goto EXPAND_FAILED_CURLY;
      sub[i] = expand_string_internal(s+1, TRUE, &s, FALSE);
      if (sub[i] == NULL) goto EXPAND_FAILED;
      if (*s++ != '}') goto EXPAND_FAILED_CURLY;
      }
    while (isspace(*s)) s++;
    if (*s++ != '}') goto EXPAND_FAILED_CURLY;
    sub[2] = expand_getkeyed(sub[0], sub[1]);
    if (sub[2] != NULL)
      {
      yield = string_cat(yield, &size, &ptr, sub[2], (int)strlen(sub[2]));
      store_free(sub[2]);
      }
    store_free(sub[0]);
    store_free(sub[1]);
    continue;
    }

  /* Handle operations on a subsequent string */

  if (*s == ':')
    {
    char *sub = expand_string_internal(s+1, TRUE, &s, FALSE);
    if (sub == NULL) goto EXPAND_FAILED;
    s++;

    /* expand expands another time */

    if (strcmp(name, "expand") == 0)
      {
      char *expanded = expand_string_internal(sub, FALSE, NULL, FALSE);
      if (expanded == NULL)
        {
        expand_string_message =
          string_sprintf("internal expansion of \"%s\" failed: %s", sub,
            expand_string_message);
        goto EXPAND_FAILED;
        }
      yield = string_cat(yield, &size, &ptr, expanded, (int)strlen(expanded));
      store_free(expanded);
      store_free(sub);
      continue;
      }

    /* lc lowercases */

    if (strcmp(name, "lc") == 0)
      {
      int count = 0;
      char *t = sub - 1;
      while (*(++t) != 0) { *t = tolower(*t); count++; }
      yield = string_cat(yield, &size, &ptr, sub, count);
      store_free(sub);
      continue;
      }

    /* length_n or l_n takes just the first n characters or the whole
    string, whichever is the shorter; substr_m_n or s_m_n take n characters
    from offset m; l_n is synonymous with s_0_n. */

    if (strncmp(name, "length_", 7) == 0 || strncmp(name, "l_", 2) == 0 ||
        strncmp(name, "substr_", 7) == 0 || strncmp(name, "s_", 2) == 0)
      {
      int len = 0;
      int offset = 0;
      int *pn = (name[0] == 's')? &offset : &len;
      int sublen = (int)strlen(sub);
      char *num = strchr(name, '_') + 1;

      while (*num != 0)
        {
        if (*num == '_' && pn == &offset)
          {
          pn = &len;
          num++;
          }
        else if (!isdigit(*num))
          {
          expand_string_message =
            string_sprintf("non-digit after underscore in \"%s\"", name);
          goto EXPAND_FAILED;
          }
        else *pn = (*pn)*10 + *num++ - '0';
        }

      if (sublen < offset + len) len = sublen - offset;
      if (len > 0)
        yield = string_cat(yield, &size, &ptr, sub + offset, len);
      store_free(sub);
      continue;
      }

    /* Unknown operator */

    expand_string_message =
      string_sprintf("unknown expansion operator \"%s\"", name);
    goto EXPAND_FAILED;
    }

  /* Handle a plain name */

  if (*s++ == '}')
    {
    BOOL freestore = FALSE;
    int choplen = 0;
    if (strncmp(name, "header_", 7) == 0 || strncmp(name, "h_", 2) == 0)
      {
      s = read_header_name(name, 256, s);
      value = find_header(name, &freestore);
      choplen = 1;
      }
    else
      {
      value = find_variable(name, &choplen);
      if (value == NULL)
        {
        expand_string_message = string_sprintf("unknown variable: %s", name);
        goto EXPAND_FAILED;
        }
      }
    if (value != NULL)
      {
      yield = string_cat(yield, &size, &ptr, value,
        (int)strlen(value) - choplen);
      if (freestore) store_free(value);
      }
    continue;
    }

  /* Else there's something wrong */

  goto EXPAND_FAILED;
  }

/* Expansion succeeded; add a terminating zero and return the
expanded string. If left != NULL, return a pointer to the terminator. */

yield[ptr] = 0;
if (left != NULL) *left = s;
return yield;

/* This is the failure exit: easiest to program with a goto. */

EXPAND_FAILED_CURLY:
expand_string_message = "missing or misplaced { or }";

EXPAND_FAILED:
store_free(yield);
return NULL;
}


/* This is the external function call.

Argument: the string to be expanded
Returns:  the expanded string, or NULL if expansion failed
*/

char *
expand_string(char *string)
{
lookup_value = NULL;
return expand_string_internal(string, FALSE, NULL, FALSE);
}



/*************************************************
**************************************************
*             Stand-alone test program           *
**************************************************
*************************************************/

#ifdef STAND_ALONE

void
regerror(char *s)
{
if (regcomp_error_pointer == NULL)
  log_write(0, LOG_PANIC_DIE, "regular expression error: %s%s%s", s,
    (regexp_compiling == NULL)? "" : " while compiling ",
    (regexp_compiling == NULL)? "" : regexp_compiling);
else *regcomp_error_pointer = string_copy(s);
}


int main(void)
{
char buffer[256];
debug_level = 1;
debug_file = stderr;
big_buffer = malloc(big_buffer_size);

printf("Testing string expansion\n");

expand_nstring[1] = "string 1....";
expand_nlength[1] = 8;
expand_nmax = 1;

while (fgets(buffer, 256, stdin) != NULL)
  {
  char *yield = expand_string(buffer);
  if (yield != NULL)
    {
    printf("%s", yield);
    store_free(yield);
    }
  else printf("Failed: %s\n", expand_string_message);
  }

return 0;
}

#endif

/* End of expand.c */
