/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Functions for parsing addresses */


#include "exim.h"


static char  fixed_phrase[256];
static char *last_comment_position;



/* In stand-alone mode, provide a replacement for deliver_make_addr()
and rewrite_address so as to avoid having to drag in too much redundant
apparatus. */

#ifdef STAND_ALONE

static address_item *deliver_make_addr(char *address)
{
address_item *addr = store_malloc(sizeof(address_item));
addr->next = NULL;
addr->parent = NULL;
addr->orig = address;
return addr;
}

static char *rewrite_address(char *recipient, BOOL dummy1, BOOL dummy2)
{
return recipient;
}

#endif




/*************************************************
*             Find the end of an address         *
*************************************************/

/* Scan over a string looking for the termination of an address at a comma,
or end of the string. It's the source-routed addresses which cause much pain
here.

Argument:
  s        pointer to the start of an address
  nl_ends  if TRUE, '\n' terminates an address

Returns:   pointer past the end of the address (null or comma)
*/

char *
parse_find_address_end(char *s, BOOL nl_ends)
{
BOOL source_routing = *s == '@';
int no_term = source_routing? 1 : 0;

while (*s != 0 && (*s != ',' || no_term > 0) && (*s != '\n' || !nl_ends))
  {
  /* Skip quoted items that are not inside brackets. Note that
  quoted pairs are allowed inside quoted strings. */

  if (*s == '\"')
    {
    while (*(++s) != 0)
      {
      if (*s == '\\' && s[1] != 0) s++;
        else if (*s == '\"') { s++; break; }
      }
    }

  /* Skip comments, which may include nested brackets, but quotes
  are not recognized inside comments, though quoted pairs are. */

  else if (*s == '(')
    {
    int level = 1;
    while (*(++s) != 0)
      {
      if (*s == '\\' && s[1] != 0) s++;
        else if (*s == '(') level++;
          else if (*s == ')' && --level <= 0) { s++; break; }
      }
    }

  /* Non-special character; just advance. Passing the colon in a source
  routed address means that any subsequent comma or colon may terminate unless
  inside angle brackets. */

  else
    {
    if (*s == '<')
      {
      source_routing = s[1] == '@';
      no_term = source_routing? 2 : 1;
      }
    else if (*s == '>') no_term--;
    else if (source_routing && *s == ':') no_term--;
    s++;
    }
  }

return s;
}



/*************************************************
*            Find last @ in an address           *
*************************************************/

/* We have to be a bit more clever than just a plain search, in order to handle
addresses like "thing@thong"@a.b.c correctly. Since quotes may not legally
be part of a domain name, we can give up on hitting the first quote when
searching from the right.

Argument:  pointer to an address
Returns:   pointer to the last @ in an address, or NULL if none
*/

char *
parse_find_at(char *s)
{
char *t = s + (int)strlen(s);
while (--t >= s)
  {
  if (*t == '@') return t;
  if (*t == '\"') return NULL;
  }
return NULL;
}




/***************************************************************************
* In all the functions below that read a particular object type from       *
* the input, return the new value of the pointer s (the first argument),   *
* and put the object into the store pointed to by t (the second argument), *
* adding a terminating zero. If no object is found, t will point to zero   *
* on return.                                                               *
***************************************************************************/


/*************************************************
*          Skip white space and comment          *
*************************************************/

/* Algorithm:
  (1) Skip spaces.
  (2) If char not '(', return.
  (3) Skip till matching ')', not counting any characters
      escaped with '\'.
  (4) Move past ')' and goto (1).

The start of the last potential comment position is remembered to
make it possible to ignore comments at the end of compound items.

Argument: current character pointer
Regurns:  new character pointer
*/

static char *
skip_comment(char *s)
{
last_comment_position = s;
while (*s)
  {
  int c, level;
  while (isspace(*s)) s++;
  if (*s != '(') break;
  level = 1;
  while((c = *(++s)) != 0)
    {
    if (c == '(') level++;
    else if (c == ')') { if (--level <= 0) { s++; break; } }
    else if (c == '\\' && s[1] != 0) s++;
    }
  }
return s;
}



/*************************************************
*                 Read an atom                   *
*************************************************/

/* An atom is a sequence of any characters except specials, space,
and controls. The specials are ( ) < > @ , ; : \ " . [ and ].

Arguments:
  s        current character pointer
  t        where to put the atom

Returns:   new character pointer
*/

static char *
read_atom(char *s, char *t)
{
s = skip_comment(s);
while (!iscntrl(*s) && strchr(" ()<>@,;:\\\".[]", *s) == NULL) *t++ = *s++;
*t = 0;
return skip_comment(s);
}



/*************************************************
*                 Read a word                    *
*************************************************/

/* A <word> is either a quoted string, or an <atom>.

Arguments:
  s        current character pointer
  t        where to put the word

Returns:   new character pointer
*/

static char *
read_word (char *s, char *t)
{
s = skip_comment(s);
if (*s == '\"')
  {
  int c;
  *t++ = '\"';
  while ((c = *(++s)) > 0 && c != '\"')
    {
    *t++ = c;
    if (c == '\\' && s[1] != 0) *t++ = *(++s);
    }
  *t++ = '\"';
  *t = 0;
  return skip_comment(s + ((c == '\"')? 1:0));
  }
else return read_atom(s, t);
}




/*************************************************
*             Read a domain                      *
*************************************************/

/* A domain is either a sequence of sub-domains, separated by dots. Remove
trailing dots if strip_trailing_dot is set. A subdomain is an atom. If
forbid_domain_literals is FALSE, a "domain" may also be an IP address enclosed
in []. Make sure the output is set to the null string if there is a syntax
error as well as if there is no domain at all.

Arguments:
  s          current character pointer
  t          where to put the domain
  errorptr   put error message here on failure (*t will be 0 on exit)

Returns:     new character pointer
*/

static char *
read_domain(char *s, char *t, char **errorptr)
{
char *tt = t;
s = skip_comment(s);

/* Handle domain literals if permitted. An RFC 822 domain literal may contain
any char except [ ] \, including linear white space, and may contain quoted
characters. However, RFC 821 restricts literals to being dot-separated 3-digit
numbers. Go for a sequence of digits and dots here; later this will be checked
for being a syntactically valid IP address if it ever gets to a router. */

if (*s == '[')
  {
  *t++ = *s++;
  while (*s == '.' || isdigit(*s)) *t++ = *s++;
  if (*s == ']') *t++ = *s++; else
    {
    *errorptr = "malformed domain literal";
    *tt = 0;
    }
  if (forbid_domain_literals)
    {
    *errorptr = "domain literals not allowed";
    *tt = 0;
    }
  *t = 0;
  return skip_comment(s);
  }

/* Handle proper domains */

for (;;)
  {
  s = read_atom(s, t);
  if (*t == 0)
    {
    if (strip_trailing_dot && t > tt) t[-1] = 0; else
      {
      *errorptr = "domain missing or malformed";
      *tt = 0;
      }
    return s;
    }
  t += (int)strlen(t);
  if (*s != '.') break;
  *t++ = *s++;
  }
return s;
}



/*************************************************
*            Read a local-part                   *
*************************************************/

/* A local-part is a sequence of words, separated by periods. A null word
between dots is not strictly allowed but apparently many mailers permit it,
so, sigh, better be compatible. Even accept a trailing dot...

Arguments:
  s           current character pointer
  t           where to put the local part
  error       where to point error text
  allow_null  TRUE if an empty local part is not an error

Returns:   new character pointer
*/

static char *
read_local_part(char *s, char *t, char **error, BOOL allow_null)
{
char *tt = t;
*error = NULL;
for (;;)
  {
  s = skip_comment(s);
  s = read_word(s, t);
  if (*t == 0)
    {
    if (t == tt && !allow_null)
      *error = "missing or malformed local part";
    return s;
    }
  t += (int)strlen(t);
  if (*s != '.') break;
  do { *t++ = *s++; } while (*s == '.');
  }
return s;
}


/*************************************************
*            Read route part of route-addr       *
*************************************************/

/* The pointer is at the initial "@" on entry. Return it following
the terminating colon. If collapse_source_routes is set, return
an empty string.

Arguments:
  s          current character pointer
  t          where to put the route
  errorptr   where to put an error message

Returns:     new character pointer
*/

static char *
read_route(char *s, char *t, char **errorptr)
{
char *t_init = t;
*errorptr = NULL;

while (*s == '@')
  {
  *t++ = '@';
  s = read_domain(s+1, t, errorptr);
  if (*t == 0) return s;
  t += (int)strlen(t);
  if (*s != ',') break;
  *t++ = *s++;
  s = skip_comment(s);
  }

if (*s == ':') *t++ = *s++;
  else *errorptr = "colon expected after route";

if (collapse_source_routes) t = t_init;
*t = 0;
return skip_comment(s);
}



/*************************************************
*                Read addr-spec                  *
*************************************************/

/* Addr-spec is local-part@domain. We make the domain optional -
the expected terminator for the whole thing is passed to check this.
This function is called only when we know we have a route-addr.

Arguments:
  s          current character pointer
  t          where to put the addr-spec
  term       expected terminator (0 or >)
  errorptr   where to put an error message
  domainptr  set to point to the start of the domain

Returns:     new character pointer
*/

static char *
read_addr_spec(char *s, char *t, int term, char **errorptr, char **domainptr)
{
s = read_local_part(s, t, errorptr, FALSE);
if (*errorptr != NULL)
  *errorptr = string_sprintf("%s in route address", *errorptr);
else
  {
  t += (int)strlen(t);
  if (*s != term)
    {
    if (*s != '@') *errorptr = "@ missing in route address"; else
      {
      *t++ = *s++;
      *domainptr = t;
      s = read_domain(s, t, errorptr);
      }
    }
  }
return s;
}



/*************************************************
*           Extract route address                *
*************************************************/

/* This function extracts an address from a full RFC822 mailbox and returns it
in a piece of malloc'ed store. We take the easy way and get a piece of store
the same size as the input, and then copy into it whatever is necessary. If we
cannot find a valid address (syntax error), return NULL, and point the error
pointer to the reason. The arguments "start" and "end" are used to return the
offsets of the first and one past the last characters in the original mailbox
of the address that has been extracted, to aid in re-writing. The argument
"domain" is set to point to the first character after "@" in the final part of
the returned address, or zero if there is no @. Note: if the address is of the
form @domain,...:route-addr "domain" refers to the final route-addr.

There are some special fudges here for handling RFC 822 group address notation
which may appear in certain headers. If the flag parse_allow_group is set
TRUE and parse_found_group is FALSE when this function is called, an address
which is the start of a group (i.e. preceded by a phrase and a colon) is
recognized; the phrase is ignored and the flag parse_found_group is set. If
this flag is TRUE at the end of an address, then if an extraneous semicolon is
found, it is ignored and the flag is cleared. This logic is used only when
scanning through addresses in headers, either to fulfil the -t option or for
rewriting.

Arguments:
  mailbox     points to the RFC822 mailbox
  errorptr    where to point an error message
  start       set to start offset in mailbox
  end         set to end offset in mailbox
  domain      set to domain offset in result, or 0 if no domain present
  allow_null  allow <> if TRUE

Returns:      points to the extracted address, or NULL on error
*/

#define FAILED(s) { *errorptr = s; goto PARSE_FAILED; }

char *
parse_extract_address(char *mailbox, char **errorptr, int *start, int *end,
  int *domain, BOOL allow_null)
{
char *yield = store_malloc((int)strlen(mailbox) + 1);
char *startptr, *endptr;
char *s = mailbox;
char *t = yield;

*domain = 0;

/* At the start of the string we expect either an addr-spec or a phrase
preceding a <route-addr>. If groups are allowed, we might also find a phrase
preceding a colon and an address. If we find an initial word followed by
a dot, strict interpretation of the RFC would cause it to be taken
as the start of an addr-spec. However, many mailers break the rules
and use addresses of the form "a.n.other <ano@somewhere>" and so we
allow this case. */

RESTART:   /* Come back here after passing a group name */

s = skip_comment(s);
startptr = s;                                 /* In case addr-spec */
s = read_local_part(s, t, errorptr, TRUE);    /* Dot separated words */
if (*errorptr != NULL) goto PARSE_FAILED;

/* If the terminator is neither < nor @ then the format of the address
must either be a bare local-part (we are now at the end), or a phrase
followed by a route-addr (more words must follow). */

if (*s != '@' && *s != '<')
  {
  if (*s == 0 || *s == ';')
    {
    if (*t == 0) FAILED("empty address");
    endptr = last_comment_position;
    goto PARSE_SUCCEEDED;              /* Bare local part */
    }

  /* Expect phrase route-addr, or phrase : if groups permitted, but allow
  dots in the phrase; complete the loop only when '<' or ':' is encountered -
  end of string will produce a null local_part and therefore fail. We don't
  need to keep updating t, as the phrase isn't to be kept. */

  while (*s != '<' && (!parse_allow_group || *s != ':'))
    {
    s = read_local_part(s, t, errorptr, FALSE);
    if (*errorptr != NULL)
      {
      *errorptr = string_sprintf("%s (expected word or \"<\")", *errorptr);
      goto PARSE_FAILED;
      }
    }

  if (*s == ':')
    {
    parse_found_group = TRUE;
    parse_allow_group = FALSE;
    s++;
    goto RESTART;
    }

  /* Assert *s == '<' */
  }

/* At this point the next character is either '@' or '<'. If it is '@',
only a single local-part has previously been read. An angle bracket
signifies the start of a <route-addr>. Throw away anything we have saved so
far before processing it. Note that this is "if" rather than "else if"
because it's also used after reading a preceding phrase.

There are a lot of broken sendmails out there that put additional pairs
of <> round <route-addr>s. If strip_excess_angle_brackets is set, allow
any number of them, as long as they match. */

if (*s == '<')
  {
  char *domainptr = yield;
  BOOL source_routed = FALSE;
  int bracket_count = 1;

  s++;
  if (strip_excess_angle_brackets)
    while (*s == '<') { bracket_count++; s++; }

  t = yield;
  startptr = s;
  s = skip_comment(s);

  /* Read an optional series of routes, each of which is a domain. They
  are separated by commas and terminated by a colon. If collapse_source_routes
  is set, the syntax will be checked, but nothing will be returned in t. */

  if (*s == '@')
    {
    s = read_route(s, t, errorptr);
    if (*errorptr != NULL) goto PARSE_FAILED;
    t += (int)strlen(t);
    source_routed = TRUE;
    }

  /* Now an addr-spec, terminated by '>'. If there is no preceding route,
  we must allow an empty addr-spec if allow_null is TRUE, to permit the
  address "<>" in some circumstances. A source-routed address MUST have
  a domain in the final part. */

  if (allow_null && !source_routed && *s == '>')
    {
    *t = 0;
    *errorptr = NULL;
    }
  else
    {
    s = read_addr_spec(s, t, '>', errorptr, &domainptr);
    if (*errorptr != NULL) goto PARSE_FAILED;
    *domain = domainptr - yield;
    if (source_routed && *domain == 0)
      FAILED("domain missing in source-routed address");
    }

  endptr = s;
  if (*errorptr != NULL) goto PARSE_FAILED;
  while (bracket_count-- > 0)
    if (*s++ != '>') FAILED("> missing at end of route address");
  s = skip_comment(s);
  }

/* Hitting '@' after the first local-part means we have definitely got an
addr-spec, on a strict reading of the RFC, and the rest of the string
should be the domain. However, for flexibility we allow for a route-address
not enclosed in <> as well, which is indicated by an empty first local
part preceding '@'. */

else if (*t == 0)
  {
  char *domainptr = yield;
  s = read_route(s, t, errorptr);
  if (*errorptr != NULL) goto PARSE_FAILED;
  t += (int)strlen(t);
  s = read_addr_spec(s, t, 0, errorptr, &domainptr);
  if (*errorptr != NULL) goto PARSE_FAILED;
  *domain = domainptr - yield;
  endptr = last_comment_position;
  if (*domain == 0) FAILED("domain missing in source-routed address");
  }

/* This is the strict case of local-part@domain. */

else
  {
  t += (int)strlen(t);
  *t++ = *s++;
  *domain = t - yield;
  s = read_domain(s, t, errorptr);
  if (*t == 0) goto PARSE_FAILED;
  endptr = last_comment_position;
  }

/* Use goto to get here from the bare local part case. Arrive by falling
through for other cases. Endptr may have been moved over whitespace, so
move it back past white space if necessary. */

PARSE_SUCCEEDED:
if (*s != 0)
  {
  if (parse_found_group && *s == ';')
    {
    parse_found_group = FALSE;
    parse_allow_group = TRUE;
    }
  else FAILED("extraneous characters at end of address");
  }
*start = startptr - mailbox;      /* Return offsets */
while (isspace(endptr[-1])) endptr--;
*end = endptr - mailbox;
return yield;

/* Use goto (via the macro FAILED) to get to here from a variety of places. */

PARSE_FAILED:
store_free(yield);
return NULL;
}

#undef FAILED



/*************************************************
*            Fix up an RFC 822 "phrase"          *
*************************************************/

/* This function is called to repair any syntactic defects in the "phrase" part
of an RFC822 address. In particular, it is applied to the user's name as read
from the passwd file when accepting a local message.

If the string contains existing quoted strings or comments containing
freestanding quotes, then we just quote those bits that need quoting -
otherwise it would get awfully messy and probably not look good. If not, we
quote the whole thing if necessary. Thus

   John Q. Smith            =>  "John Q. Smith"
   John "Jack" Smith        =>  John "Jack" Smith
   John "Jack" Q. Smith     =>  John "Jack" "Q." Smith
   John (Jack) Q. Smith     =>  "John (Jack) Q. Smith"
   John ("Jack") Q. Smith   =>  John ("Jack") "Q." Smith
but
   John (\"Jack\") Q. Smith =>  "John (\"Jack\") Q. Smith"

Sheesh! This is tedious code. It is a great pity that the syntax of RFC822 is
the way it is...

Non-printing characters are turned into question marks, which do not need to be
quoted. The result is passed back in a static buffer, and must therefore be
copied - this makes sense, since it is usually going to be added to some other
string.

Argument: an RFC822 phrase
Returns:  the fixed RFC822 phrase
*/

char *
parse_fix_phrase(char *s)
{
int ch;
BOOL quoted = FALSE;
char *t = fixed_phrase + 1;
char *yield = t;

while (isspace(*s)) s++;

while ((ch = (*s++) & 127) != 0)
  {
  if (!isprint(ch)) ch = '?';

  /* Copy over quoted strings, remembering we encountered one */

  if (ch == '\"')
    {
    *t++ = '\"';
    while ((ch = *s++ & 127) > 0 && ch != '\"')
      {
      *t++ = ch;
      if (ch == '\\' && *s != 0) *t++ = *s++ & 127;
      }
    *t++ = '\"';
    if (ch == 0) break;
    quoted = TRUE;
    }

  /* Copy over comments, noting if they contain freestanding quote
  characters */

  else if (ch == '(')
    {
    int level = 1;
    *t++ = '(';
    while((ch = *s++ & 127) != 0)
      {
      *t++ = ch;
      if (ch == '(') level++;
      else if (ch == ')') { if (--level <= 0) break; }
      else if (ch == '\\' && *s != 0) *t++ = *s++ & 127;
      else if (ch == '\"') quoted = TRUE;
      }
    if (ch == 0)
      {
      while (level--) *t++ = ')';
      break;
      }
    }

  /* Handle special characters that need to be quoted */

  else if (strchr(")<>@,;:\\.[]", ch) != NULL)
    {
    /* If hit previous quotes just make one quoted "word" */

    if (quoted)
      {
      char *tt = t++;
      while (*(--tt) != ' ' && *tt != '\"' && *tt != ')') tt[1] = *tt;
      tt[1] = '\"';
      *t++ = ch;
      while((ch = *s++ & 127) != 0)
        if (ch == ' ' || ch == '\"') break; else *t++ = ch;
      *t++ = '\"';
      }

    /* Else quote the whole string so far, and the rest up to
    any following quotes. */

    else
      {
      fixed_phrase[0] = '\"';
      yield--;
      *t++ = ch;

      while ((ch = *s++ & 127) != 0)
        {
        /* If hit subsequent quotes, insert our quote before any
        trailing spaces and go into quoted mode. */

        if (ch == '\"')
          {
          int count = 0;
          while (t[-1] == ' ') { t--; count++; }
          *t++ = '\"';
          while (count-- > 0) *t++ = ' ';
          quoted = TRUE;
          break;
          }

        /* If hit a subsequent comment, check it for unescaped quotes,
        and if so, end our quote before it. */

        else if (ch == '(')
          {
          char *ss = s;     /* Char after '(' */
          int level = 1;
          while((ch = *ss++ & 127) != 0)
            {
            if (ch == '(') level++;
            else if (ch == ')') { if (--level <= 0) break; }
            else if (ch == '\\' && ss[1] != 0) ss++;
            else if (ch == '\"') { quoted = TRUE; break; }
            }

          /* Comment contains unescaped quotes; end our quote before
          the start of the comment. */

          if (quoted)
            {
            int count = 0;
            while (t[-1] == ' ') { t--; count++; }
            *t++ = '\"';
            while (count-- > 0) *t++ = ' ';
            break;
            }

          /* Comment does not contain unescaped quotes; include it in
          our quote. */

          else
            {
            if (ch == 0) ss--;
            *t++ = '(';
            strncpy(t, s, ss-s);
            t += ss-s;
            s = ss;
            }
          }

        /* Not a comment or quote; include this character in our quotes. */

        else *t++ = ch;
        }
      }

    /* Add a final quote if we hit the end of the string, and move back the
    pointer to the char that terminated the quote. */

    if (ch == 0) *t++ = '\"';
    s--;
    }

  /* Non-special character; just copy it over */

  else *t++ = ch;
  }

*t = 0;
return yield;
}



/*************************************************
*          Extract addresses from a list         *
*************************************************/

/* This function is called by the forwardfile and aliasfile (and maybe other)
directors to scan a string containing a list of addresses separated by commas
(with optional white space) or by newlines, and to generate a chain of address
items from them.

The SunOS5 documentation for alias files is not very clear on the syntax; it
does not say that either a comma or a newline can be used for separation.
However, that is the way smail does it, so we follow suit.

If a # character is encountered in a white space position, then characters from
there to the next newline are skipped.

If an unqualified address begins with '\', just skip that character. This gives
compatibility with sendmail's use of \ to prevent looping. Exim has its own
loop prevention scheme which handles other cases too - see the code in
direct_address(). If a qualified address begins with '\' it is a syntax error.

An "address" can be a specification of a file or a pipe; the latter may often
need to be quoted because it may contain spaces, but we don't want to retain
the quotes. Quotes may appear in normal addresses too, and should be retained.
We can distinguish between these cases, because in addresses, quotes are used
only for parts of the address, not the whole thing. Therefore, we remove quotes
from items when they entirely enclose them, but not otherwise.

An "address" can also be of the form :include:pathname to include a list of
addresses contained in the specified file.

Any unqualified addresses are qualified with and rewritten if necessary, via
the rewrite_address() function.

Yield 0 if the string was successfully parsed; non-zero otherwise, with
a message pointed to by the error variable. A positive value is given if
the failure is simply a failure to open or subsequently stat an :include: file
- some callers treat this as a soft error - and a negative value is given
otherwise.

If optional is TRUE, then failure to generate any addresses, without a syntax
error, is not an error.

If directory is not NULL, then require any :include: files to begin with the
directory string AND be regular files.

Arguments:
  s           pointer to the list of addresses (typically a complete .forward
                file or a list of entries in an alias file)
  anchor      where to hang the chain of newly-created addresses
  error       where to return an error text
  optional    if TRUE and no addresses generated, treat as success; otherwise
                no addresses generated is an error
  allow_blackhole
              if TRUE, allow :blackhole: to generate no addresses without
                an error, even if optional is FALSE
  rewrite     TRUE if generated addresses are to be passed through the
                rewriting functions
  directory   if NULL, no checks are done on :include: files
              if "*", no included files are allowed
              otherwise, included file names must start with the given
                directory
  syntax_errors
              if not NULL, it carries on after syntax errors in addresses,
                building up a list of errors as error blocks chained on
                here

Returns:      0 on success, non-zero otherwise: >0 if failed to stat or open
              an :include: file; <0 for other errors.
*/

int
parse_extract_addresses(char *s, address_item **anchor, char **error,
  BOOL optional, BOOL allow_blackhole, BOOL rewrite,
  char *directory, error_block **syntax_errors)
{
int count = 0;

DEBUG(9) debug_printf("parse_extract_addresses: %s\n", s);

for (;;)
  {
  int len;
  char *ss, *nexts;
  address_item *addr;
  BOOL inquote = FALSE;

  for (;;)
    {
    while (isspace(*s) || *s == ',') s++;
    if (*s == '#') { while (*s != 0 && *s != '\n') s++; } else break;
    }

  if (*s == 0)
    {
    if (count > 0 || optional) return 0;
    *error = "no addresses generated";
    return -1;
    }

  /* Find the end of the next address. Quoted strings in addresses may contain
  escaped characters; I haven't found a proper specification of .forward or
  alias files that mentions the quoting properties, but it seems right to do
  the escaping thing in all cases, so use the function that finds the end of an
  address. However, don't let a quoted string extend over the end of a line. */

  ss = parse_find_address_end(s, TRUE);

  /* Remember where we finished, for starting the next one. */

  nexts = ss;

  /* Remove any trailing spaces; we know there's at least one non-space. */

  while (isspace(ss[-1])) ss--;

  /* We now have s->start and ss->end of the next address. Remove quotes
  if they completely enclose, remembering the address started with a quote
  for handling pipes and files. Another round of removal of leading and
  trailing spaces is then required. */

  if (*s == '\"' && ss[-1] == '\"')
    {
    s++;
    ss--;
    inquote = TRUE;
    while (s < ss && isspace(*s)) s++;
    while (ss > s && isspace(ss[-1])) ss--;
    }

  /* Set up the length of the address. */

  len = ss - s;

  DEBUG(9)
    {
    int save = s[len];
    s[len] = 0;
    debug_printf("extracted: %s\n", s);
    s[len] = save;
    }

  /* If the address is :blackhole: and black holes are permitted, then ignore
  it, and set the "optional" flag so that generating no addresses at all in
  this expansion is no longer an error. */

  if (allow_blackhole && strncmp(s, ":blackhole:", len) == 0) optional = TRUE;

  /* If the address is of the form :include:pathname, read the file, and call
  this function recursively to extract the addresses from it. There are
  possible security implications when called from forwardfile. If directory is
  NULL, do no checks (we are running under a suitable uid). Otherwise, insist
  that the file name starts with the given directory and is a regular file.
  If the directory is "*", no included files are allowed. */

  else if (strncmp(s, ":include:", 9) == 0)
    {
    char *filebuf;
    char filename[256];
    struct stat statbuf;
    address_item *last;
    int extracted;
    FILE *f;

    strncpy(filename, s+9, len-9);
    filename[len-9] = 0;

    /* Insist on absolute path */

    if (filename[0]!= '/')
      {
      *error = string_sprintf("included file %s is not an absolute path",
        filename);
      return -1;
      }

    /* Check file name if required - this is requested only when not running
    under a suitable uid. */

    if (directory != NULL)
      {
      int len = (int)strlen(directory);
      char *p = filename + len;

      if (directory[0] == '*')
        {
        *error = "included files not permitted";
        return -1;
        }
      else if (strncmp(filename, directory, len) != 0 || *p != '/')
        {
        *error = string_sprintf("included file %s is not in directory %s",
          filename, directory);
        return -1;
        }

      /* It is necessary to check that every component inside the directory
      is NOT a symbolic link, in order to keep the file inside the directory.
      This is mighty tedious. It is also not totally foolproof in that it
      leaves the possibility of a race attack, but I don't know how to do
      any better. */

      while (*p != 0)
        {
        int temp;
        while (*(++p) != 0 && *p != '/');
        temp = *p;
        *p = 0;
        if (lstat(filename, &statbuf) != 0)
          {
          *error = string_sprintf("failed to stat %s (component of included "
            "file)", filename);
          *p = temp;
          return -1;
          }

        *p = temp;

        if ((statbuf.st_mode & S_IFMT) == S_IFLNK)
          {
          *error = string_sprintf("included file %s in the %s directory "
            "involves a symbolic link", filename, directory);
          return -1;
          }
        }
      }

    /* Open and stat the file */

    if ((f = fopen(filename, "r")) == NULL)
      {
      *error = string_sprintf("failed to open included file %s: %s",
        filename, strerror(errno));
      return +1;
      }

    if (fstat(fileno(f), &statbuf) != 0)
      {
      *error = string_sprintf("failed to stat included file %s: %s",
        filename, strerror(errno));
      fclose(f);
      return +1;
      }

    /* If directory was checked, double check that we opened a regular file */

    if (directory != NULL && (statbuf.st_mode & S_IFMT) != S_IFREG)
      {
      *error = string_sprintf("included file %s is not a regular file in "
        "the %s directory", filename, directory);
      return -1;
      }

    /* Get a buffer and read the contents */

    filebuf = store_malloc(statbuf.st_size + 1);
    if (fread(filebuf, 1, statbuf.st_size, f) != statbuf.st_size)
      {
      *error = string_sprintf("error while reading included file %s: %s",
        filename, strerror(errno));
      fclose(f);
      return -1;
      }
    filebuf[statbuf.st_size] = 0;
    fclose(f);

    addr = NULL;
    extracted = parse_extract_addresses(filebuf, &addr, error, optional,
      allow_blackhole, rewrite, directory, syntax_errors);
    store_free(filebuf);
    if (extracted != 0) return extracted;    /* error already set */

    last = addr;
    while (last->next != NULL) { count++; last = last->next; }
    last->next = *anchor;
    *anchor = addr;
    count++;
    }

  /* Else (not :include:) ensure address is syntactically correct and fully
  qualified if not a pipe or a file, removing a leading \ if present on an
  unqualified address. For pipes and files we must handle quoting. It's
  not quite clear exactly what to do for partially quoted things, but the
  common case of having the whole thing in quotes is straightforward. If this
  was the case, inquote will have been set TRUE above and the quotes removed.

  There is a possible ambiguity over addresses whose local parts start with
  a vertical bar or a slash, and the latter do in fact occur, thanks to X.400.
  Consider a .forward file that contains the line

     /X=xxx/Y=xxx/OU=xxx/@some.gate.way

  Is this a file or an X.400 address? Does it make any difference if it is in
  quotes? On the grounds that file names of this type are rare, Exim treats
  something that parses as an RFC 822 address and has a domain as an address
  rather than a file or a pipe. This is also how an address such as the above
  would be treated if it came in from outside. */

  else
    {
    int start, end, domain;
    char *recipient = NULL;
    int save = s[len];
    s[len] = 0;

    /* If it starts with \ and the rest of it parses as a valid mail address
    without a domain, carry on with that address. Otherwise arrange for it
    to fall through, causing an error message on the re-parse. */

    if (*s == '\\')
      {
      recipient =
        parse_extract_address(s+1, error, &start, &end, &domain, FALSE);
      if (recipient != NULL && domain != 0)
        {
        store_free(recipient);
        recipient = NULL;
        }
      }

    /* Try parsing the item as an address. */

    if (recipient == NULL) recipient =
      parse_extract_address(s, error, &start, &end, &domain, FALSE);

    /* If item starts with / or | and is not a valid address, or there
    is no domain, treat it as a file or pipe. If it was a quoted item,
    remove the quoting occurrences of \ within it. */

    if ((*s == '|' || *s == '/') && (recipient == NULL || domain == 0))
      {
      char *t = store_malloc((int)strlen(s) + 1);
      char *p = t;
      char *q = s;
      while (*q != 0)
        {
        if (inquote)
          {
          *p++ = (*q == '\\')? *(++q) : *q;
          q++;
          }
        else *p++ = *q++;
        }
      *p = 0;
      addr = deliver_make_addr(t);
      addr->pfr = TRUE;              /* indicates pipe/file/reply */
      }

    /* Item must be an address. Complain if not, else qualify, rewrite and set
    up the control block. It appears that people are in the habit of using
    empty addresses but with comments as a way of putting comments into
    alias and forward files. Therefore, ignore the error "empty address".
    Mailing lists might want to tolerate syntax errors; there is therefore
    an option to do so. */

    else
      {
      if (recipient == NULL)
        {
        if (strcmp(*error, "empty address") == 0)
          {
          *error = NULL;
          s = nexts;
          continue;
          }
        if (syntax_errors != NULL)
          {
          error_block *e = store_malloc(sizeof(error_block));
          error_block *last = *syntax_errors;
          if (last == NULL) *syntax_errors = e; else
            {
            while (last->next != NULL) last = last->next;
            last->next = e;
            }
          e->next = NULL;
          e->text1 = *error;
          e->text2 = string_copy(s);
          s[len] = save;
          s = nexts;
          continue;
          }
        else
          {
          *error = string_sprintf("%s in \"%s\"", *error, s);
          s[len] = save;   /* _after_ using it for *error */
          return -1;
          }
        }

      /* Address was successfully parsed */

      recipient = rewrite? rewrite_address(recipient, TRUE, TRUE) :
        rewrite_address_qualify(recipient, TRUE);
      addr = deliver_make_addr(recipient);
      }

    /* Restore the final character in the original data, and add to the
    output chain. */

    s[len] = save;
    addr->next = *anchor;
    *anchor = addr;
    count++;
    }

  /* Advance pointer for the next address */

  s = nexts;
  }
}




/*************************************************
**************************************************
*             Stand-alone test program           *
**************************************************
*************************************************/

#ifdef STAND_ALONE
int main(void)
{
int start, end, domain;
char buffer[256];

printf("Testing parse_extract_address without group syntax\n");

while (fgets(buffer, 256, stdin) != NULL)
  {
  char *out;
  char *errmess;
  buffer[(int)strlen(buffer) - 1] = 0;
  out = parse_extract_address(buffer, &errmess, &start, &end, &domain, FALSE);
  if (out == NULL) printf("*** bad address: %s\n", errmess); else
    {
    char extract[100];
    strncpy(extract, buffer+start, end-start);
    extract[end-start] = 0;
    printf("%s %d %d %d \"%s\"\n", out, start, end, domain, extract);
    store_free(out);
    }
  }

printf("Testing parse_extract_address with group syntax\n");

parse_allow_group = TRUE;
while (fgets(buffer, 256, stdin) != NULL)
  {
  char *out;
  char *errmess;
  buffer[(int)strlen(buffer) - 1] = 0;
  out = parse_extract_address(buffer, &errmess, &start, &end, &domain, FALSE);
  if (out == NULL) printf("*** bad address: %s\n", errmess); else
    {
    char extract[100];
    strncpy(extract, buffer+start, end-start);
    extract[end-start] = 0;
    printf("%s %d %d %d \"%s\"\n", out, start, end, domain, extract);
    store_free(out);
    }
  }

printf("Testing parse_fix_phrase\n");

while (fgets(buffer, 256, stdin) != NULL)
  {
  buffer[(int)strlen(buffer)-1] = 0;
  printf("%s\n", parse_fix_phrase(buffer));
  }

printf("Testing parse_extract_addresses\n");

while (fgets(buffer, 256, stdin) != NULL)
  {
  char *out;
  char *errmess;
  int extracted;
  address_item *anchor = NULL;
  buffer[(int)strlen(buffer) - 1] = 0;
  if ((extracted = parse_extract_addresses(buffer, &anchor, &errmess,
      FALSE, TRUE, TRUE, NULL)) == 0)
    {
    while (anchor != NULL)
      {
      address_item *addr = anchor;
      anchor = anchor->next;
      printf("%d %s\n", addr->pfr, addr->orig);
      store_free(addr);
      }
    }
  else printf("Failed: %d %s\n", extracted, errmess);
  }

return 0;
}

#endif

/* End of parse.c */
