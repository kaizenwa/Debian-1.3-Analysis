/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Functions concerned with rewriting headers */


#include "exim.h"

/* Names for testing rewriting */

static char *rrname[] = {
  "  sender",
  "    from",
  "      to",
  "      cc",
  "     bcc",
  "reply-to",
  "env-from",
  "  env-to"
};



/*************************************************
*            Ensure an address is qualified      *
*************************************************/

/* If a change is made, the store used by the incoming address is freed.

Arguments:
  s              address to check
  is_recipient   TRUE if a recipient address; FALSE if a sender address

Returns:         fully-qualified address
*/

char *
rewrite_address_qualify(char *s, BOOL is_recipient)
{
if (parse_find_at(s) == NULL)
  {
  char *new = string_sprintf("%s@%s", s,
    is_recipient? qualify_domain_recipient : qualify_domain_sender);
  store_free(s);
  s = new;
  }
return s;
}



/*************************************************
*               Rewrite a single address         *
*************************************************/

/* The incoming address is NOT freed. The yield is the input address if there
is no rewriting to be done. For source-routed addresses, it is only the final
route-addr that gets considered. Assume the input is a valid address. This
function is called only

Arguments:
  s           address to rewrite
  flag        indicates which header this address comes from; it must
                match the flags in the rewriting rule
  whole       if not NULL, set TRUE if the rewriting rule contained the
                "whole" bit and it is a header that is being rewritten
  add_header  if TRUE and rewriting occurs, add an "X-rewrote-xxx" header
                if headers are in existence
  name        name of header, for use when adding X-rewrote-xxxx

Returns:      new address if rewritten; the input address if no change;
              for a header rewrite, if the "whole" bit is set, the entire
              rewritten address is returned, not just the active bit.
*/

static char *
rewrite_one(char *s, int flag, BOOL *whole, BOOL add_header, char *name)
{
rewrite_rule *rule;
char *yield = s;
char *subject = s;
char *domain = NULL;
char *user = NULL;
int pre_length = 0;

/* Scan the rewriting rules */

for (rule = rewrite_rules; rule != NULL; rule = rule->next)
  {
  int start, end, pdomain;
  char *save_localpart, *save_domain;
  char *error, *new, *newparsed;
  char buffer[256];

  if ((rule->flags & flag) == 0) continue;

  /* If this is the first rule we are trying on this address, set domain
  to point past the last '@' in the address and user to the start of the
  address or following the last ':' if source-routed. If a source routed
  address does not have an @ after the : then rewrite it using the
  immediately preceding domain. */

  if (domain == NULL)
    {
    domain = strrchr(subject, '@') + 1;
    if (subject[0] != '@')
      {
      user = subject;
      pre_length = 0;
      }
    else
      {
      user = strchr(subject, ':') + 1;
      if (domain < user)
        {
        if (domain == subject+1)
          user = subject = string_sprintf("%s@%.*s", user, user - domain - 1,
            domain);
        else
          {
          subject = string_sprintf("%.*s:%s@%.*s", domain - subject - 2,
            subject, user, user - domain - 1, domain);
          user = strchr(subject, ':') + 1;
          }
        domain = strrchr(subject, '@') + 1;
        DEBUG(9)
          debug_printf("re-wrote source routed address as %s\n", subject);
        }
      pre_length = user - subject;
      }
    }

  /* Use the general function for matching an address against a list (here
  just one item, so set the separator to 0). */

  if (!match_address_list(user, domain - user, rule->key, &(rule->re), 0, 0))
    continue;

  /* The source address matches, and numerical variables have been
  set up. Expand the replacement string. While doing so, set $local_part
  and $domain to the appropriate values, restoring whatever value they
  previously had afterwards. */

  save_localpart = deliver_localpart;
  save_domain = deliver_domain;

  sprintf(buffer, "%.*s", domain-user-1, user);
  deliver_localpart = buffer;
  deliver_domain = domain;

  new = expand_string(rule->replacement);

  deliver_localpart = save_localpart;
  deliver_domain = save_domain;

  /* If the expansion failed with the "forcedfail" flag, don't generate
  an error - just give up on this rewriting rule. Otherwise we have a
  configuration error. */

  if (new == NULL)
    {
    if (expand_string_forcedfail) continue;
    log_write(0, LOG_MAIN|LOG_PANIC, "Expansion of %s failed while rewriting: "
      "%s", rule->replacement, expand_string_message);
    break;
    }

  /* If this was a source-routed address, put back the initial part on the
  front of what was generated. */

  if (pre_length != 0)
    {
    char *fullnew = string_sprintf("%.*s%s", pre_length, subject, new);
    store_free(new);
    new = fullnew;
    }

  /* Validate what has been generated. */

  newparsed = parse_extract_address(new, &error, &start, &end, &pdomain,
    FALSE);

  if (newparsed == NULL || pdomain == 0)
    {
    log_write(0, LOG_MAIN|LOG_PANIC, "Rewrite of %s yielded unqualified or "
      "unparseable address \"%s\"", subject, new);
    }
  else
    {
    DEBUG(2) debug_printf("%s rewritten as %s\n", subject, newparsed);

    /* A header will only actually be added if header_last is non-NULL,
    i.e. during message rectption or delivery. */

    if (add_header)
      header_add(htype_old, "X-rewrote-%s: %s\n", name, subject);

    /* If we are on the second or subsequent re-write, free the previous
    yield and subject, if different. */

    if (yield != s) store_free(yield);
    if (subject != yield) store_free(subject);

    /* If whole is not NULL and this rewrite rule has the "whole" flag set,
    and we are rewriting a header, yield the entire new address, not just the
    active part, and tell the caller so. */

    if (whole != NULL && (rule->flags & rewrite_whole) != 0 &&
        (flag & rewrite_all_headers) != 0)
      {
      *whole = TRUE;
      yield = new;
      subject = newparsed;
      }

    /* Rule just rewrites active part, or handling an envelope */

    else
      {
      if (whole != NULL) *whole = FALSE;
      subject = yield = newparsed;
      store_free(new);
      }
    domain = NULL;    /* Reset for next rule */
    }
  }

if (subject != yield) store_free(subject);
expand_nmax = -1;
return yield;
}



/*************************************************
*        Rewrite address for filter              *
*************************************************/

/* When running the mail filtering code, $local_part is taken from the
message's envelope, but tests are done on the contents of headers. It may be
that the addresses in the headers have been subject to rewriting, while the
addresses in the envelope have not. The particular case in point is the
"personal" test. This function applies the given rewriting rules (it is called
with rewrite_from or rewrite_to); because the fourth argument given to
rewrite_one() is FALSE, the fifth is not relevant.

Arguments:
  s           address to rewrite
  flags       either rewrite_from or rewrite_to

Returns:      result of call to rewrite_one()
*/

char *
rewrite_address_for_filter(char *s, int flag)
{
return rewrite_one(s, flag, NULL, FALSE, "");
}





/*************************************************
*         Ensure qualification and rewrite       *
*************************************************/

/* This function is called for envelope addresses, the boolean specifying
whether a recipient or a sender. It must first of all ensure the address is
fully qualified, and then apply any relevant re-writing rules. The add-header
flag causes a header to be added, recording the old address. This is marked
"old", so that it is never transported anywhere; it exists for local checking
and debugging purposes.

Arguments:
  s             the address to be considered
  is_recipient  TRUE for recipient addresses; FALSE otherwise
  add_header    add "X-rewrote-xxx" header when rewriting

Returns:        possibly rewritten address
*/

char *
rewrite_address(char *s, BOOL is_recipient, BOOL add_header)
{
int flag = is_recipient? rewrite_envto : rewrite_envfrom;
s = rewrite_address_qualify(s, is_recipient);
if ((rewrite_existflags & flag) != 0)
  {
  char *new = rewrite_one(s, flag, NULL, add_header, is_recipient?
    "recipient" : "sender");
  if (new != s) s = new;
  }
return s;
}



/*************************************************
*    Qualify and possibly rewrite one header     *
*************************************************/

/* This is called only from rewrite_header() below, either when reading a
message. or when routing, in order to rewrite addresses that get changed by a
router. This is normally the addition of full qualification to a partial
domain. The first rewriting rule in this case is "change routed_old into
routed_new", and it applies to all header lines that contain addresses. Then
header-specific rewriting rules are applied.

Arguments:
  h           pointer to header line block
  flag        indicates which header this is
  routed_old  if not NULL, this is a rewrite caused by a router, changing
                this domain into routed_new
  routed_new  new routed domain if routed_old is not NULL

Returns:      NULL if header unchanged; otherwise the rewritten header
*/

static header_line *
rewrite_one_header(header_line *h, int flag, char *routed_old, char *routed_new)
{
int lastnewline = 0;
header_line *newh = NULL;
char *s = strchr(h->text, ':') + 1;
while (isspace(*s)) s++;

DEBUG(5) debug_printf("rewrite_one_header: type=%c:\n  %s", h->type, h->text);

parse_allow_group = TRUE;     /* Allow group syntax */

/* Loop for multiple addresses in the header */

while (*s != 0)
  {
  char *sprev;
  char *ss = parse_find_address_end(s, FALSE);
  char *recipient, *new, *errmess;
  BOOL changed = FALSE;
  int terminator = *ss;
  int start, end, domain;

  /* Temporarily terminate the string at this point, and extract
  the route address within. Then put back the terminator and prepare for the
  next address, saving the start of the old one. */

  *ss = 0;
  recipient = parse_extract_address(s,&errmess,&start,&end,&domain,FALSE);
  *ss = terminator;
  sprev = s;
  s = ss + (terminator? 1:0);
  while (isspace(*s)) s++;

  /* There isn't much we can do for syntactic disasters at this stage.
  Pro tem (possibly for ever) ignore them. */

  if (recipient == NULL) continue;

  /* If routed_old is not NULL, this is a rewrite caused by a router,
  consisting of changing routed_old into routed_new, and applying to all
  headers. The new value should always be fully qualified, but it may be
  something that has an explicit re-write rule set, so we need to check the
  configured rules subsequently as well. (Example: there's an explicit rewrite
  turning *.foo.com into foo.com, and an address is supplied as abc@xyz, which
  the DNS lookup turns into abc@xyz.foo.com). However, if no change is made
  here, don't bother carrying on. */

  if (routed_old != NULL)
    {
    new = NULL;

    /* For source-routed addresses, the router will have been looking
    at the first one. */

    if (recipient[0] == '@')
      {
      int firstterm;
      char *t = strpbrk(recipient, ":,");
      if (t == NULL) return NULL;  /* paranoid precaution */
      firstterm = *t;
      *t = 0;

      if (strcmpic(recipient+1, routed_old) == 0)
        {
        *t = firstterm;
        new = string_sprintf("@%s%s", routed_new, t);
        DEBUG(2)
          debug_printf("%s rewritten by router as %s\n", recipient, new);
        }
      }

    /* For non-source-routed addresses, "domain" will be pointing to
    the domain, and should be non-zero, since a router can't route without
    a domain! */

    else if (domain > 0 && strcmpic(recipient+domain, routed_old) == 0)
      {
      recipient[domain-1] = 0;
      new = string_sprintf("%s@%s", recipient, routed_new);
      DEBUG(2)
        {
        recipient[domain-1] = '@';
        debug_printf("%s rewritten by router as %s\n", recipient, new);
        }
      }

    /* Free the original; if there isn't a new address, do no more on this
    iteration. */

    store_free(recipient);
    if (new != NULL)
      {
      recipient = new;
      changed = TRUE;
      }
    else continue;
    }


  /* This is not a router-inspired rewrite. Ensure the address is fully
  qualified. (Note: rewrite_address-qualify frees the old store.) From
  or Reply-to headers are treated as senders, the rest as recipients. This
  matters only when there are different qualify strings. */

  else
    {
    new = rewrite_address_qualify(recipient,
      (flag & (rewrite_from | rewrite_replyto)) == 0);
    changed = (new != recipient);
    recipient = new;
    }

  /* If there are rewrite rules for this type of header, apply
  them. This test is just for efficiency, to save scanning the rules
  in cases when nothing is going to change. If the final rewrite rule
  had the "whole" flag set, adjust the pointers so that the whole address
  gets replaced, except possibly a final \n. */

  if ((rewrite_existflags & flag) != 0)
    {
    BOOL whole;
    new = rewrite_one(recipient, flag, &whole, FALSE, NULL);
    if (new != recipient)
      {
      changed = TRUE;
      if (whole)
        {
        start = 0;
        end = ss - sprev;
        if (sprev[end-1] == '\n') end--;
        }
      }
    }

  /* If the address has changed, create a new header containing the
  rewritten address. We do not need to set the chain pointers at this
  stage. */

  if (changed)
    {
    int remlen;
    int newlen = (int)strlen(new);
    int oldlen = end - start;

    header_line *prev = (newh == NULL)? h : newh;
    header_line *newnewh =
      store_malloc(sizeof(header_line) + prev->slen - oldlen + newlen + 4);
    char *newt = newnewh->text;

    newnewh->type = prev->type;
    newnewh->slen = prev->slen - oldlen + newlen;

    /* Build the new header text by copying the old and putting in the
    replacement. This process may make the header substantially longer
    than it was before - qualification of a list of bare addresses can
    often do this - so we stick in a newline after the re-written address
    if it has increased in length and ends more than 40 characters in. In
    fact, the code is not perfect, since it does not scan for existing
    newlines in the header, but it doesn't seem worth going to that
    amount of trouble. */

    strncpy(newt, prev->text, sprev - prev->text + start);
    newt += sprev - prev->text + start;
    *newt = 0;
    strcat(newt, new);
    newt += newlen;
    remlen = s - (sprev + end);
    if (remlen > 0)
      {
      strncpy(newt, sprev + end, remlen);
      newt += remlen;
      *newt = 0;
      }

    /* Must check that there isn't a newline here anyway; in particular, there
    will be one at the very end of the header, where we DON'T want to insert
    another one! The pointer s has been skipped over white space, so just
    look back to see if the last non-space-or-tab was a newline. */

    if (newlen > oldlen && newt - newnewh->text - lastnewline > 40)
      {
      char *p = s - 1;
      while (p >= prev->text && (*p == ' ' || *p == '\t')) p--;
      if (*p != '\n')
        {
        lastnewline = newt - newnewh->text;
        strcat(newt, "\n\t");
        newnewh->slen += 2;
        }
      }

    /* Finally, the remaining unprocessed addresses, if any. */

    strcat(newt, s);

    DEBUG(5) debug_printf("newlen=%d newtype=%c newtext:\n%s",
      newnewh->slen, newnewh->type, newnewh->text);

    s = newnewh->text + (s - prev->text) - oldlen + newlen;

    DEBUG(5) debug_printf("remainder: %s", (*s == 0)? "\n" : s);

    if (newh != NULL) store_free(newh);
    newh = newnewh;
    if (new != recipient) store_free(new);
    }

  store_free(recipient);
  }

parse_allow_group = FALSE;  /* Reset group flags */
parse_found_group = FALSE;

return newh;
}




/*************************************************
*              Rewrite a header line             *
*************************************************/

/* This function may be passed any old header line. It must detect those which
contain addresses, then then apply any rewriting rules that apply. If
routed_old is NULL, only the configured rewriting rules are consulted.
Otherwise, the rewriting rule is "change routed_old into routed_new", and it
applies to all header lines that contain addresses. Then header-specific
rewriting rules are applied.

The old header line is flagged as "old". Old headers are saved on the spool for
debugging but are never sent to any recipients.

Arguments:
  h            header line to rewrite
  routed_old  if not NULL, this is a rewrite caused by a router, changing
                this domain into routed_new
  routed_new  new routed domain if routed_old is not NULL

Returns:      NULL if header unchanged; otherwise the rewritten header - it is
                up to the caller to chain it as required
*/

header_line *
rewrite_header(header_line *h, char *routed_old, char *routed_new)
{
switch (h->type)
  {
  case htype_sender:
  return rewrite_one_header(h, rewrite_sender, routed_old, routed_new);
  break;

  case htype_from:
  return rewrite_one_header(h, rewrite_from, routed_old, routed_new);
  break;

  case htype_to:
  return rewrite_one_header(h, rewrite_to, routed_old, routed_new);
  break;

  case htype_cc:
  return rewrite_one_header(h, rewrite_cc, routed_old, routed_new);
  break;

  case htype_bcc:
  return rewrite_one_header(h, rewrite_bcc, routed_old, routed_new);
  break;

  case htype_replyto:
  return rewrite_one_header(h, rewrite_replyto, routed_old, routed_new);
  break;
  }

return NULL;
}



/************************************************
*            Test rewriting rules               *
************************************************/

/* Called from the mainline as a result of the -brw option. Test the
address for all possible cases.

Argument: the address to test
Returns:  nothing
*/

void rewrite_test(char *s)
{
int i, start, end, domain;
char *recipient, *error;

if (parse_find_at(s) == NULL)
  s = string_sprintf("%s@%s", s, qualify_domain_recipient);

recipient = parse_extract_address(s, &error, &start, &end, &domain, FALSE);

if (recipient == NULL)
  {
  printf("Syntax error in address: %s\n", error);
  return;
  }

for (i = 0; i < 8; i++)
  {
  BOOL whole = FALSE;
  int flag = 1 << i;
  char *new = rewrite_one(recipient, flag, &whole, FALSE, "");
  printf("%s: ", rrname[i]);
  if (whole || (flag & rewrite_all_headers) == 0) printf("%s\n", new);
    else printf("%.*s%s%s\n", start, s, new, s+end);
  }
}

/* End of rewrite.c */
