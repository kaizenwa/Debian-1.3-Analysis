/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Functions for matching strings */


#include "exim.h"



/*************************************************
*           Generalized string match             *
*************************************************/

/* We are passed the subject and pattern as strings, and a pointer to a pointer
to a regular expression block. If the pattern is a regular expression and the
pointer points to NULL, set up a new re_block and compile the r.e. For
non regular expression, if the first character of the pattern is *, the match
is on the tail of the item. If the pattern starts with <searchtype>; then
do a file lookup, using the remainder as the file name. Keep a list of open
files for efficiency.

Arguments:
  s            the subject string to be checked
  pattern      the pattern to check it against, either literal, starting with *,
               or starting with ^ for a r.e.
  chain_add    address of anchor of a chain of compiled r.e. blocks; can be
               NULL if no regular expression processing is required
  expand_setup if < 0, don't set up any numeric expansion variables;
               if = 0, set $0 to whole subject, and either
                 $1 to what matches * or
                 $1, $2, ... to r.e. bracketed items
               if > 0, don't set $0, but do set either
                 $n to what matches *, or
                 $n, $n+1, ... to r.e. bracketed items
               (n = expand_setup)
  use_partial  if FALSE, override any partial- search types

Returns:       TRUE if matched; FALSE if not
*/

BOOL
match_check_string(char *s, char *pattern, re_block **chain_ad,
  int expand_setup, BOOL use_partial)
{
BOOL yield;
re_block *p;

/* If required to set up $0, initialize the data but don't turn on by setting
expand_nmax until the match is assured. */

expand_nmax = -1;
if (expand_setup == 0)
  {
  expand_nstring[0] = s;
  expand_nlength[0] = (int)strlen(s);
  }
else if (expand_setup > 0) expand_setup--;

/* No regexp pointer given, or pattern is not a regular expression. */

if (chain_ad == NULL || pattern[0] != '^')
  {
  int search_type = 0;      /* keep picky compiler happy */
  int partial = -1;
  char *error, *key, *result;
  void *handle;


  /* Tail match */

  if (pattern[0] == '*')
    {
    int patlen = (int)strlen(++pattern);
    int slen = (int)strlen(s);
    if (patlen > slen) return FALSE;
    yield = strncmp(s + slen - patlen, pattern, patlen) == 0;
    if (yield && expand_setup >= 0)
      {
      expand_nstring[++expand_setup] = s;
      expand_nlength[expand_setup] = slen - patlen;
      expand_nmax = expand_setup;
      }
    return yield;
    }


  /* Exact string match */

  if (strchr(pattern, ';') == NULL)
    {
    yield = strcmp(s, pattern) == 0;
    if (yield && expand_setup >= 0) expand_nmax = expand_setup;
    return yield;
    }


  /* The remaining possibilities are various matches by file lookup; for
  single-key lookups the key is unprocessed, but for query-type lookups
  there has to be some processing to get the key into the query.

  If the pattern starts with "partial-" (with an optional number before
  the -) then set up for doing partial matches by widening and narrowing
  the domain if the original doesn't match. This does not as yet apply to
  NIS+ searches. [Needs thought.] */

  if (strncmp(pattern, "partial", 7) == 0)
    {
    char *orig_pattern = pattern;
    pattern += 7;
    if (isdigit(*pattern))
      {
      partial = 0;
      while (isdigit(*pattern))
        partial = partial * 10 + *pattern++ - '0';
      }
    else partial = 2;
    if (*pattern++ != '-')
      log_write(0, LOG_PANIC_DIE, "malformed partial search type in string "
        "match: %s", orig_pattern);
    }

  /* However, partial matching is not appropriate for certain lookups
  (e.g. when looking up user@domain for sender rejection. */

  if (!use_partial) partial = -1;

  /* Now determine the kind of lookup */

  key = s;

  if (strncmp(pattern, "lsearch;", 8) == 0)
    {
    search_type = stype_lsearch;
    pattern += 8;
    }
  else if (strncmp(pattern, "dbm;", 4) == 0)
    {
    search_type = stype_dbm;
    pattern += 4;
    }
  else if (strncmp(pattern, "nis;", 4) == 0)
    {
    if (have_nis)
      {
      search_type = stype_nis;
      pattern += 4;
      }
    else log_write(0, LOG_PANIC_DIE, "search type \"nis\" is not available "
      "(not in binary - see HAVE_NIS) in string match: %s", pattern);
    }
  else if (strncmp(pattern, "nis0;", 5) == 0)
    {
    if (have_nis)
      {
      search_type = stype_nis0;
      pattern += 5;
      }
    else log_write(0, LOG_PANIC_DIE, "search type \"nis0\" is not available "
      "(not in binary - see HAVE_NIS) in string match: %s", pattern);
    }

  /* NIS+ is special in that the query has to be expanded in order
  to get the key inserted into the query. Currently, partial searches
  are not implemented. */

  else if (strncmp(pattern, "nisplus;", 8) == 0)
    {
    if (have_nisplus)
      {
      lookup_key = s;
      key = expand_string(pattern+8);
      lookup_key = NULL;
      if (key == NULL)
        log_write(0, LOG_PANIC_DIE, "failed to expand NIS+ query %s: %s",
          pattern+8, expand_string_message);
      pattern = NULL;
      search_type = stype_nisplus;
      }
    else log_write(0, LOG_PANIC_DIE, "search type \"nisplus\" is not available "
      "(not in binary - see HAVE_NISPLUS) in string match: %s", pattern);
    }

  /* Unknown search type */

  else log_write(0, LOG_PANIC_DIE, "unknown search type in string match: %s",
    pattern);

  /* Now do the actual lookup; throw away the actual data returned; partial
  matching is all handled inside search_find(). */

  if (pattern != NULL) while (isspace(*pattern)) pattern++;
  handle = search_open(pattern, search_type, 0, NULL, NULL, &error);
  if (handle == NULL) log_write(0, LOG_PANIC_DIE, "%s", error);
  result = search_find(handle, pattern, key, search_type,
    partial, &expand_setup, &error);

  /* Free the key if not the same as the original string; free the result,
  which is not needed, and return appropriately. */

  if (key != s) store_free(key);
  if (result == NULL) return FALSE;
  store_free(result);
  expand_nmax = expand_setup;
  return TRUE;
  }


/* Regular expression match: compile if necessary */

p = *chain_ad;
if (p == NULL)
  {
  p = store_malloc(sizeof(re_block));
  *chain_ad = p;
  p->next = NULL;
  regexp_compiling = pattern;
  p->re = regcomp(pattern);
  regexp_compiling = NULL;
  }

/* Perform a regular expression match and set up $ variables if required. */

yield = regexec(p->re, s);
if (yield && expand_setup >= 0)
  {
  for (expand_nmax = expand_setup + 1; expand_nmax < NSUBEXP; expand_nmax++)
    {
    expand_nstring[expand_nmax] = p->re->startp[expand_nmax];
    expand_nlength[expand_nmax] = p->re->endp[expand_nmax] -
      expand_nstring[expand_nmax];
    }
  expand_nmax--;
  }
return yield;
}



/*************************************************
*            Match in colon-separated list       *
*************************************************/

/*
Arguments:
  s              string to search for
  list           colon separated list of patterns, or NULL
  chain_ad       address of anchor of chain or r.e. blocks for holding compiled
                 regular expressions for this colon-separated list; NULL if no
                 r.e. processing required
  at_is_primary  if TRUE, a list entry of "@" is interpreted as the primary
                 name for the host

Returns:         TRUE if matched; FALSE if not
*/

BOOL
match_isinlist(char *s, char *list, re_block **chain_ad, BOOL at_is_primary)
{
char *ss;

if (list == NULL) return FALSE;

for (ss = string_firstinlist(list, ':');
     ss != NULL;
     ss = string_nextinlist(':'))
  {
  if (*ss == '@' && ss[1] == 0 && at_is_primary) ss = primary_hostname;
  if (match_check_string(s, ss, chain_ad, -1, TRUE)) return TRUE;
  if (ss[0] == '^' && *chain_ad != NULL) chain_ad = &((*chain_ad)->next);
  }

return FALSE;
}


/*************************************************
*           Do file existence tests              *
*************************************************/

/* This function is given a colon-separated list of files whose existence
is to be tested. The string is first expanded, and the resulting file names
must be absolute, but "!" may appear precede an (absolute) file name to
indicate that non-existence is what is wanted.

Argument:
  s        a colon-separated list of files whose existence is to be tested,
           or NULL; a leading "!" causes the test for that file to be for
           non-existence.

Returns:   OK if s == NULL or all files' existence is as required;
           DEFER if the existence of at least one of the files is
             unclear (an error other than non-existence occurred);
           FAIL otherwise.
           In all cases, errno contains what it was set to by the final
           call to stat(), or 0 if there were no files in the list.
*/

int
match_exists(char *s)
{
char *ss, *file;
int yield = OK;
int save_errno = 0;
struct stat statbuf;

if (s == NULL) return OK;
ss = expand_string(s);
if (ss == NULL)
  log_write(0, LOG_MAIN|LOG_PANIC_DIE, "expansion of %s failed: %s", s,
    expand_string_message);

for (file = string_firstinlist(ss, ':'); file != NULL;
     file = string_nextinlist(':'))
  {
  int rc;
  BOOL invert = FALSE;

  if (*file == '!')
    {
    invert = TRUE;
    file++;
    }

  if (*file != '/')
    log_write(0, LOG_MAIN|LOG_PANIC_DIE, "file name for existence test is not "
      "fully qualified: %s", file);

  errno = 0;
  rc = stat(file, &statbuf);
  save_errno = errno;

  if (rc < 0 && errno != ENOENT && errno != ENOTDIR)
    {
    yield = DEFER;
    break;
    }

  if ((rc < 0) != invert)
    {
    yield = FAIL;
    break;
    }
  }

store_free(ss);
errno = save_errno;
return yield;
}




/*************************************************
*    Test whether address matches address list   *
*************************************************/

/* This function is given an address and a string list of things to
match it against. The list may contain individual addresses, regular
expressions, and lookup specifications. The address to check can consist of
just a domain, which will then match only domain items or items specified as
*@domain. The value of the second argument must then be given as 0.

Arguments:
  address       address to test
  domain        offset to the domain in the address
  list          string list to check against
  chain_ad      pointer to chain of compiled re's for caching
  expand_setup  controls setting up of $n variables - passed through
                to match_check_string (q.v.)
  separator     separator character for the list; may be 0 for one item

Returns:      TRUE if the address matches something in the list
*/

BOOL
match_address_list(char *address, int domain, char *list, re_block **chain_ad,
  int expand_setup, int separator)
{
char *localpart, *test_address, *p;
int llen;

/* Ensure the domain is lower-cased before doing any comparisons. */

strcpy(big_buffer, address);
address = big_buffer;
for (p = address + domain; *p != 0; p++) *p = tolower(*p);

/* The local part follows the colon in a source-routed address; otherwise
it starts at the beginning. */

localpart = (address[0] == '@')? strchr(address, ':') + 1 : address;

/* Compute the length of the local part; if domain == 0 (which won't happen for
a source route) the length is zero. */

llen = (domain == 0)? 0 : domain - (localpart - address) - 1;

/* If expand_setup is zero, we need to set up $0 to the whole thing, in
case there is a match. Can't use the built-in facilities of match_check_string,
as we may just be calling that for part of the address (the domain). */

if (expand_setup == 0)
  {
  expand_nstring[0] = localpart;
  expand_nlength[0] = llen;
  expand_setup++;
  }

/* Loop for each address in the list. Note that string_nextinlist() always
returns a string copied into a dedicated bit of store; hence we can do
things like lowercasing it without affecting the input string. */

for (test_address = string_firstinlist(list, separator);
     test_address != NULL;
     test_address = string_nextinlist(separator))
  {
  int expand_inc = 0;
  char *sdomain;

  /* Handle a regular expression, which must match the entire
  incoming address. Note that localpart will be pointing to the
  end part of a source-routed address. */

  if (test_address[0] == '^')
    {
    if (match_check_string(localpart, test_address, chain_ad, expand_setup,
      TRUE)) return TRUE;
    chain_ad = &((*chain_ad)->next);
    continue;
    }

  /* If not a regular expression, either part may begin with an
  asterisk, and both parts must match. If there's no '@' in the
  pattern, then it is just a domain and treated as if it had
  *@ on the front. */

  sdomain = strrchr(test_address, '@');

  /* No @ => assume user matches; set domain = whole thing */

  if (sdomain == NULL) sdomain = test_address;

  /* Check the local part if one is given in the list. A null local part
  is treated as '*'. */

  else
    {
    int sllen = sdomain - test_address;
    sdomain += 1;
    if (sllen > 0)
      {
      if (test_address[0] == '*')
        {
        int cllen = sllen - 1;
        if (llen < cllen ||
          strncmpic(localpart+llen-cllen, test_address + 1, cllen) != 0)
            continue;
        if (expand_setup > 0)
          {
          expand_nstring[expand_setup] = localpart;
          expand_nlength[expand_setup] = llen - cllen;
          expand_inc = 1;
          }
        }
      else if (llen != sllen || strncmpic(localpart, test_address, llen) != 0)
        continue;
      }
    }

  /* If the local part matched, check the domain using the generalized
  function, which supports file lookups. */

  if (match_check_string(address + domain, sdomain, NULL,
    expand_setup + expand_inc, TRUE)) return TRUE;

  /* If we have no match and the pattern is a single lookup pattern
  without a local part, then try the entire address, but do not do any
  partial matching, which won't be appropriate. */

  if (sdomain == test_address && strchr(test_address, ';') != NULL &&
    match_check_string(localpart, test_address, NULL, -1, FALSE))
      return TRUE;
  }

return FALSE;
}

/* End of match.c */
