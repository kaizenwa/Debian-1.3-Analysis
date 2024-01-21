/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


/* General functions concerned with directing, and generic director options. */


#include "exim.h"



/* Generic options for directors, all of which live inside director_instance
data blocks and hence have the opt_public flag set. */

optionlist optionlist_directors[] = {
  { "domains",            opt_stringptr|opt_public,
                 (void *)offsetof(director_instance, domains) },
  { "driver",             opt_stringptr|opt_public,
                 (void *)offsetof(director_instance, driver_name) },
  { "except_domains",     opt_stringptr|opt_public,
                 (void *)offsetof(director_instance, except_domains) },
  { "except_local_parts", opt_stringptr|opt_public,
                 (void *)offsetof(director_instance, except_local_parts) },
  { "fail_verify",        opt_bool_verify|opt_hidden|opt_public,
                 (void *)offsetof(director_instance, fail_verify_sender) },
  { "fail_verify_recipient", opt_bool|opt_public,
                 (void *)offsetof(director_instance, fail_verify_recipient) },
  { "fail_verify_sender", opt_bool|opt_public,
                 (void *)offsetof(director_instance, fail_verify_sender) },
  { "local_parts",        opt_stringptr|opt_public,
                 (void *)offsetof(director_instance, local_parts) },
  { "more",               opt_bool|opt_public,
                 (void *)offsetof(director_instance, more) },
  { "prefix",             opt_stringptr|opt_public,
                 (void *)offsetof(director_instance, prefix) },
  { "prefix_optional",    opt_bool|opt_public,
                 (void *)offsetof(director_instance, prefix_optional) },
  { "require_files",      opt_stringptr|opt_public,
                 (void *)offsetof(director_instance, require_files) },
  { "suffix",             opt_stringptr|opt_public,
                 (void *)offsetof(director_instance, suffix) },
  { "suffix_optional",    opt_bool|opt_public,
                 (void *)offsetof(director_instance, suffix_optional) },
  { "transport",          opt_transportptr|opt_public,
                 (void *)offsetof(director_instance, transport) },
  { "unseen",             opt_bool|opt_public,
                 (void *)offsetof(director_instance, unseen) },
  { "verify",             opt_bool_verify | opt_hidden|opt_public,
                 (void *)offsetof(director_instance, verify_sender) },
  { "verify_only",        opt_bool|opt_public,
                 (void *)offsetof(director_instance, verify_only) },
  { "verify_recipient",   opt_bool|opt_public,
                 (void *)offsetof(director_instance, verify_recipient) },
  { "verify_sender",      opt_bool|opt_public,
                 (void *)offsetof(director_instance, verify_sender) }
};

int optionlist_directors_size =
  sizeof(optionlist_directors)/sizeof(optionlist);



/*************************************************
*           Find a local user                    *
*************************************************/

/* Try several times (if configured) to find a local user, in case delays in
NIS or NFS whatever cause an incorrect refusal. It's a pity that getpwnam()
doesn't have some kind of indication as to why it has failed. If the string
given consists entirely of digits, and the third argument is not NULL, assume
the string is the numerical value of the uid. Otherwise it is looked up using
getpwnam(). The uid is passed back via return_uid, if not NULL, and the
pointer to a passwd structure, if found, is passed back via pw, if not NULL.

Because this may be called several times in succession for the same user for
different directors, cache the result of the previous getpwnam call so that it
can be re-used. Note that we can't just copy the structure, as the store it
points to can get trashed.

Arguments:
  s           the login name or textual form of the numerical uid of the user
  pw          if not NULL, return the result of getpwnam here, or set NULL
                if no call to getpwnam is made (s numeric, return_uid != NULL)
  return_uid  if not NULL, return the uid via this address

Returns:      TRUE if s is numerical or was looked up successfully

*/

static struct passwd pwcopy;
static struct passwd *lastpw = NULL;
static char lastname[48] = { 0 };
static char lastdir[128];

BOOL
direct_finduser(char *s, struct passwd **pw, uid_t *return_uid)
{
if (strcmp(lastname, s) != 0)
  {
  int i = 0;

  if (return_uid != NULL && (isdigit(*s) || *s == '-') &&
       s[strspn(s+1, "0123456789")+1] == 0)
    {
    *return_uid = (uid_t)atoi(s);
    if (pw != NULL) *pw = NULL;
    return TRUE;
    }

  strncpy(lastname, s, sizeof(lastname));

  for (;;)
    {
    if ((lastpw = getpwnam(s)) != NULL) break;
    if (++i > finduser_retries) break;
    sleep(1);
    }

  if (lastpw != NULL)
    {
    pwcopy.pw_uid = lastpw->pw_uid;
    pwcopy.pw_gid = lastpw->pw_gid;
    strncpy(lastdir, lastpw->pw_dir, sizeof(lastdir));
    pwcopy.pw_name = lastname;
    pwcopy.pw_dir = lastdir;
    lastpw = &pwcopy;
    }
  }

if (lastpw == NULL) return FALSE;

if (return_uid != NULL) *return_uid = lastpw->pw_uid;
if (pw != NULL) *pw = lastpw;

return TRUE;
}




/*************************************************
*           Find a local group                   *
*************************************************/

/* Try several times (if configured) to find a local group, in case delays in
NIS or NFS whatever cause an incorrect refusal. It's a pity that getgrnam()
doesn't have some kind of indication as to why it has failed.

Arguments:
  s           the group namd or textual form of the numerical gid
  return_gid  return the gid via this address

Returns:      TRUE if the group was found; FALSE otherwise

*/

BOOL
direct_findgroup(char *s, gid_t *return_gid)
{
int i = 0;
struct group *gr;

if ((isdigit(*s) || *s == '-') && s[strspn(s+1, "0123456789")+1] == 0)
  {
  *return_gid = (gid_t)atoi(s);
  return TRUE;
  }

for (;;)
  {
  if ((gr = getgrnam(s)) != NULL)
    {
    *return_gid = gr->gr_gid;
    return TRUE;
    }
  if (++i > finduser_retries) break;
  sleep(1);
  }

return FALSE;
}




/*************************************************
*          Find user by expanding string         *
*************************************************/

/* This must always succeed; if looking up the user fails, panic.

Arguments:
  string       the string to be expanded, yielding a login name or a numerical
                 uid value (to be passed to direct_finduser())
  driver_name  caller name for panic error message (only)
  driver_type  caller type for panic error message (only)
  pw           return passwd entry via this pointer
  uid          return uid via this pointer

Returns:       nothing
*/

void
direct_find_expanded_user(char *string, char *driver_name,
  char *driver_type, struct passwd **pw, uid_t *uid)
{
char *user = expand_string(string);
if (user == NULL)
  log_write(0, LOG_PANIC_DIE, "Failed to expand user string \"%s\" from the "
    "%s %s: %s", string, driver_name, driver_type, expand_string_message);
if (!direct_finduser(user, pw, uid))
  log_write(0, LOG_PANIC_DIE, "Failed to find user \"%s\" from expanded string "
    "\"%s\" from the %s %s", user, string, driver_name, driver_type);
store_free(user);
}



/*************************************************
*          Find group by expanding string        *
*************************************************/

/* This must always succeed; if looking up group fails, panic.

Arguments:
  string       the string to be expanded, yielding a group name or a numerical
                 gid value (to be passed to direct_findgroup())
  driver_name  caller name for panic error message (only)
  driver_type  caller type for panic error message (only)
  gid          return gid via this pointer

Returns:       the yield of direct_findgroup()
*/

void
direct_find_expanded_group(char *string, char *driver_name,
  char *driver_type, gid_t *gid)
{
char *group = expand_string(string);
if (group == NULL)
  log_write(0, LOG_PANIC_DIE, "Failed to expand group string \"%s\" from the "
    "%s %s: %s", string, driver_name, driver_type, expand_string_message);
if (!direct_findgroup(group, gid))
  log_write(0, LOG_PANIC_DIE, "Failed to find group \"%s\" from expanded string "
    "\"%s\" from the %s %s", group, string, driver_name, driver_type);
store_free(group);
}



/*************************************************
*             Initialize director list           *
*************************************************/

/* Read the directors configuration file, and set up a chain of director
instances according to its contents. Each director has generic options and may
also have its own private options. This function is only ever called when
directors == NULL. We use generic code in readconf to do the work. */

void
direct_init(void)
{
director_instance *d;
readconf_driver_init("director",
  (driver_instance **)(&directors),     /* chain anchor */
  (driver_info *)directors_available,   /* available drivers */
  sizeof(director_info),                /* size of info blocks */
  &director_defaults,                   /* default values for generic options */
  sizeof(director_instance),            /* size of instance block */
  optionlist_directors,                 /* generic options */
  optionlist_directors_size);

/* A director may not have more=FALSE and unseen=TRUE */

for (d = directors; d != NULL; d = d->next)
  if (d->unseen && !d->more )
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "the combination of \"unseen\" and \"no_more\" is not permitted on "
      "a director,\n  but was set for the %s director", d->name);
}




/*************************************************
*           Tidy up after directing              *
*************************************************/

/* Directors are entitled to keep hold of certain resources in their instance
blocks so as to save setting them up each time. An example is the open file for
the aliasfile director. Such directors must provide a tidyup entry point which
is called when all directing is finished via this function. */

void
direct_tidyup(void)
{
director_instance *d;
for (d = directors; d != NULL; d = d->next)
  if (d->info->tidyup != NULL) (d->info->tidyup)(d);
}





/*************************************************
*                 Direct one address             *
*************************************************/

/* This function is passed in one address item, for processing by the
directors. It has been determined that the address is for one of the local
domains. The action can be one of:

  . adding the address to the chain on addr_local, for local delivery, and
    returning OK;
  . adding the address to the chain on addr_remote, for remote delivery, and
    returning OK;
  . returning FAIL;
  . returning DEFER;
  . adding one or more new addresses, with this one as parent, to the chain
    on addr_new, for reprocessing ab initio, and returning OK.

The verify flag is set if this is being called for verification rather than
delivery. If the director doesn't have its "verify" flag set, it is skipped.
Otherwise, the flag is passed to the director in case it needs to know.

If the director has the "more" flag set false (i.e. "no_more" has been
specified) then, if it fails to match a name, no further directors are tried.
This includes the case when the director would have been called, but for the
verify flag's being FALSE.

The generic options "domain", "prefix", and "suffix" are handled at this top
level.

Arguments
  addr         the address to direct
  addr_local   the chain onto which to hang the address for local delivery
  addr_remote  the chain onto which to hang the address for remote delivery
  addr_new     the chain onto which to hang newly-generated addresses
  addr_succeed the chain onto which to hang addresses that we are finished
                 with (e.g. they've been expanded into something else)
  verify       v_none if delivering rather than verifying,
               v_sender if verifying a sender address
               v_recipient if verifying a recipient address

Returns:       OK, FAIL, or DEFER, as described above
*/


int
direct_address(address_item *addr,
  address_item **addr_local,
  address_item **addr_remote,
  address_item **addr_new,
  address_item **addr_succeed,
  int verify)
{
director_instance *d;
address_item *parent;
BOOL more = TRUE;

DEBUG(9) debug_printf("directing %s\n", addr->orig);

/* Loop through all director instances. */

for (d = directors; more && d != NULL; d = d->next)
  {
  int yield;
  int suffixchar = 0;
  int suffixend = -1;
  char *oldlocal_part = NULL;
  BOOL loop_detected = FALSE;

  /* Skip this director if not verifying and it has verify_only set */

  if (verify == v_none && d->verify_only)
    {
    DEBUG(9) debug_printf("%s director skipped: verify_only set\n",
      d->name);
    continue;
    }

  /* Skip this director if domain mismatch */

  if (d->domains != NULL &&
       !match_isinlist(addr->domain, d->domains, &(d->re_domains), TRUE))
    {
    DEBUG(9) debug_printf("%s director skipped: domain mismatch\n",
      d->name);
    continue;
    }

  if (d->except_domains != NULL &&
      match_isinlist(addr->domain, d->except_domains, &(d->re_except_domains),
        TRUE))
    {
    DEBUG(9) debug_printf("%s director skipped: except domain mismatch\n",
      d->name);
    continue;
    }

  /* Skip this director if local part mismatch */

  if (d->local_parts != NULL &&
       !match_isinlist(addr->local_part, d->local_parts, &(d->re_local_parts),
         FALSE))
    {
    DEBUG(9) debug_printf("%s director skipped: local part mismatch\n",
      d->name);
    continue;
    }

  if (d->except_local_parts != NULL &&
      match_isinlist(addr->local_part, d->except_local_parts,
        &(d->re_except_local_parts), FALSE))
    {
    DEBUG(9) debug_printf("%s director skipped: except local part mismatch\n",
      d->name);
    continue;
    }

  /* If this director's "more" flag is FALSE, arrange that no subsequent
  directors are called. */

  more = d->more;
  DEBUG(9)
    {
    if (!more) debug_printf("%s director has more set FALSE\n", d->name);
    }

  /* Skip this director if verifying and it hasn't got the verify flag */

  if ((verify == v_sender && !d->verify_sender) ||
      (verify == v_recipient && !d->verify_recipient))
    {
    DEBUG(9) debug_printf("%s director skipped: verify %d %d %d\n",
      d->name, verify, d->verify_sender, d->verify_recipient);
    continue;
    }

  /* Handle any configured prefix by replacing the local_part address,
  saving it for restoration if the director fails. Skip the director
  if the prefix doesn't match, unless the prefix is optional. */

  if (d->prefix != NULL)
    {
    BOOL matched = FALSE;
    char *prefix;

    /* Loop for possible list of prefixes. If any one begins with '*',
    special wildcard checking is done. */

    for (prefix = string_firstinlist(d->prefix, ':'); prefix != NULL;
         prefix = string_nextinlist(':'))
      {
      int plen = (int)strlen(prefix);
      if (prefix[0] == '*')
        {
        char *p;
        prefix++;
        for (p = addr->local_part + (int)strlen(addr->local_part) - (--plen);
             p >= addr->local_part; p--)
          {
          if (strncmpic(prefix, p, plen) == 0)
            {
            matched = TRUE;
            plen += p - addr->local_part;
            break;
            }
          }
        }
      else matched = (strncmpic(prefix, addr->local_part, plen) == 0);

      if (matched)
        {
        oldlocal_part = addr->local_part;
        addr->local_part += plen;
        addr->prefix = string_copyn(oldlocal_part, plen);
        DEBUG(9) debug_printf("stripped prefix %s\n", addr->prefix);
        break;
        }
      }

    if (!matched && !d->prefix_optional)
      {
      DEBUG(9) debug_printf("%s director skipped: prefix mismatch\n",
        d->name);
      continue;
      }
    }

  /* Handle any configured suffix likewise, but for this we have to
  fudge the end of the address; save the character so it can be put
  back if the director fails. */

  if (d->suffix != NULL)
    {
    BOOL matched = FALSE;
    char *suffix;

    for (suffix = string_firstinlist(d->suffix, ':'); suffix != NULL;
         suffix = string_nextinlist(':'))
      {
      int slen = (int)strlen(suffix);
      int alen = (int)strlen(addr->local_part);

      if (suffix[slen-1] == '*')
        {
        char *p;
        char *pend = addr->local_part + alen - (--slen) + 1;
        for (p = addr->local_part; p < pend; p++)
          {
          if (strncmpic(suffix, p, slen) == 0)
            {
            matched = TRUE;
            slen = alen - (p - addr->local_part);
            break;
            }
          }
        }

      else matched = alen > slen &&
        strncmpic(d->suffix, addr->local_part + alen - slen, slen) == 0;

      if (matched)
        {
        suffixend = alen - slen;
        suffixchar = addr->local_part[suffixend];
        addr->suffix = string_copy(addr->local_part+suffixend);
        addr->local_part[suffixend] = 0;
        DEBUG(9) debug_printf("stripped suffix %s\n", addr->suffix);
        break;
        }
      }

    /* If failed to match the suffix, restore any change the prefix
    handler made. */

    if (!matched && !d->suffix_optional)
      {
      DEBUG(9) debug_printf("%s director skipped: suffix mismatch\n",
        d->name);
      if (oldlocal_part != NULL)
        {
        addr->local_part = oldlocal_part;
        addr->prefix = NULL;
        }
      continue;
      }
    }

  /* Loop protection: If this address has a parent with the same address that
  was directed by this director, we skip this director. This prevents a variety
  of possibly looping states, and saves us having to do anything special for
  the forwardfile director. */

  for (parent = addr->parent; parent != NULL; parent = parent->parent)
    {
    if (strcmpic(parent->local_part, addr->local_part) == 0 &&
        parent->director == d)
      {
      DEBUG(9) debug_printf("%s director skipped: previously directed %s\n",
        d->name, addr->local_part);
      loop_detected = TRUE;
      break;
      }
    }
  if (loop_detected) continue;


  /* Prefix and suffix (if any) have been stripped; ready to call the
  director, first setting up values that may be used in string expansions.
  Reset them afterwards. */

  deliver_set_expansions(addr);
  DEBUG(9) debug_printf("calling %s director\n", d->name);
  yield = (d->info->code)(d, addr, addr_local, addr_remote, addr_new,
    addr_succeed, verify != v_none);
  deliver_set_expansions(NULL);

  /* Success or deferral means we are finished with this address, as does an
  internal or configuration failure. If we succeed on a director that has
  "fail_verify" set, convert the result into a fail. If we succeed on a
  director that has "unseen" set on it, we must make a copy of the address to
  hand on to the subsequent directors. Actually, we can't pass it on directly;
  we have to put it on the new queue, but since it has the same address as
  the current one, it will be passed by all previous directors and also the
  one that has just succeeded, by the loop-avoidance code. */

  if (yield == OK || yield == DEFER || yield == ERROR)
    {
    addr->director = d;

    if (yield == OK)
      {
      if ((verify == v_sender && d->fail_verify_sender) ||
          (verify == v_recipient && d->fail_verify_recipient))
        {
        yield = FAIL;
        addr->message = string_sprintf("%s director forced verify failure",
          d->name);
        }
      else if (d->unseen)
        {
        char *new_address =
          string_sprintf("%s@%s", addr->local_part, addr->domain);
        address_item *new = deliver_make_addr(new_address);
        new->parent = addr;
        new->ignore_error |= addr->ignore_error;
        new->errors_address = addr->errors_address;
        addr->child_count++;
        new->next = *addr_new;
        *addr_new = new;
        }
      }

    DEBUG(2)
      {
      if (yield == OK)
        {
        debug_printf("%s director succeeded for %s\n", d->name,
          addr->local_part);
        debug_printf("  transport: %s\n", (addr->transport == NULL)?
          "<none>" : addr->transport->name);
        }
      else if (yield == DEFER)
        {
        debug_printf("%s director deferred %s\n", d->name, addr->local_part);
        debug_printf("  message: %s\n", (addr->message == NULL)?
          "<none>" : addr->message);
        }
      else
        {
        debug_printf("%s director: error for %s\n", d->name, addr->local_part);
        debug_printf("  message: %s\n", (addr->message == NULL)?
          "<none>" : addr->message);
        }
      }
    return yield;
    }

  /* Restore prefix & suffix for the next director. */

  if (suffixend >= 0) addr->local_part[suffixend] = suffixchar;
  if (oldlocal_part != NULL) addr->local_part = oldlocal_part;
  addr->prefix = addr->suffix = NULL;

  /* Break the loop if a director forced a failure. */

  if (yield == FORCEFAIL) break;
  }

/* No directors accepted this address; fail it. */

if (addr->message == NULL)
  addr->message = string_sprintf("unknown local-part \"%s\" in domain \"%s\"",
    addr->local_part, addr->domain);
return FAIL;
}

/* End of direct.c */
