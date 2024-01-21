/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Functions concerned with routing, and the list of generic router options. */


#include "exim.h"



/* Generic options for routers, all of which live inside router_instance
data blocks and which therefore have the opt_public flag set. */

optionlist optionlist_routers[] = {
  { "domains",            opt_stringptr|opt_public,
                 (void *)offsetof(router_instance, domains) },
  { "driver",             opt_stringptr|opt_public,
                 (void *)offsetof(router_instance, driver_name) },
  { "except_domains",     opt_stringptr|opt_public,
                 (void *)offsetof(router_instance, except_domains) },
  { "except_local_parts", opt_stringptr|opt_public,
                 (void *)offsetof(router_instance, except_local_parts) },
  { "fail_verify",        opt_bool_verify|opt_hidden|opt_public,
                 (void *)offsetof(router_instance, fail_verify_sender) },
  { "fail_verify_recipient", opt_bool|opt_public,
                 (void *)offsetof(router_instance, fail_verify_recipient) },
  { "fail_verify_sender", opt_bool|opt_public,
                 (void *)offsetof(router_instance, fail_verify_sender) },
  { "local_parts",        opt_stringptr|opt_public,
                 (void *)offsetof(router_instance, local_parts) },
  { "more",               opt_bool|opt_public,
                 (void *)offsetof(router_instance, more) },
  { "pass_on_timeout",    opt_bool|opt_public,
                 (void *)offsetof(router_instance, pass_on_timeout) },
  { "require_files",      opt_stringptr|opt_public,
                 (void *)offsetof(router_instance, require_files) },
  { "self",               opt_stringptr|opt_public,
                 (void *)(offsetof(router_instance, self)) },
  { "transport",          opt_transportptr|opt_public,
                 (void *)offsetof(router_instance, transport) },
  { "unseen",             opt_bool|opt_public,
                 (void *)offsetof(router_instance, unseen) },
  { "verify",             opt_bool_verify|opt_hidden|opt_public,
                 (void *)offsetof(router_instance, verify_sender) },
  { "verify_only",        opt_bool|opt_public,
                 (void *)offsetof(router_instance, verify_only) },
  { "verify_recipient",   opt_bool|opt_public,
                 (void *)offsetof(router_instance, verify_recipient) },
  { "verify_sender",      opt_bool|opt_public,
                 (void *)offsetof(router_instance, verify_sender) }
};

int optionlist_routers_size =
  sizeof(optionlist_routers)/sizeof(optionlist);


/*************************************************
*             Initialize router list             *
*************************************************/

/* Read the routers configuration file, and set up a chain of router
instances according to its contents. Each router has generic options and may
also have its own private options. This function is only ever called when
routers == NULL. We use generic code in readconf to do the work. */

void
route_init(void)
{
router_instance *r;

readconf_driver_init("router",
  (driver_instance **)(&routers),     /* chain anchor */
  (driver_info *)routers_available,   /* available drivers */
  sizeof(router_info),                /* size of info blocks */
  &router_defaults,                   /* default values for generic options */
  sizeof(router_instance),            /* size of instance block */
  optionlist_routers,                 /* generic options */
  optionlist_routers_size);

/* A router may not have more=FALSE and unseen=TRUE. The "self" option
needs to be decoded into a code value and possibly a new domain string
and a rewrite boolean. */

for (r = routers; r != NULL; r = r->next)
  {
  char *s;

  if (r->unseen && !r->more )
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "the combination of \"unseen\" and \"no_more\" is not permitted on "
      "a router,\n  but was set for the %s router", r->name);

  /* If the setting is NULL, convert it to "freeze" so that it prints
  correctly. Can't yet set this as the data default because we want to
  detect "unset" for overriding by the obsolete self_mx option in lookuphost. */

  if (r->self == NULL) r->self = "freeze";
  s = r->self;

  if      (strcmp(s, "freeze") == 0)    r->self_code = self_freeze;
  else if (strcmp(s, "defer") == 0)     r->self_code = self_defer;
  else if (strcmp(s, "send") == 0)      r->self_code = self_send;
  else if (strcmp(s, "fail_soft") == 0) r->self_code = self_fail;
  else if (strcmp(s, "fail_hard") == 0) r->self_code = self_forcefail;
  else if (strncmp(s, "reroute:", 8) == 0)
    {
    s += 8;
    while (isspace(*s)) s++;
    if (strncmp(s, "rewrite:", 8) == 0)
      {
      r->self_rewrite = TRUE;
      s += 8;
      while (isspace(*s)) s++;
      }
    r->self = s;
    r->self_code = self_local;
    }
  else log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s router:\n  "
      "%s is not valid for the self option", r->name, s);
  }
}



/*************************************************
*             Tidy up after routing              *
*************************************************/

/* Routers are entitled to keep hold of certain resources in their instance
blocks so as to save setting them up each time. An example is an open file.
Such routers must provide a tidyup entry point which is called when all routing
is finished, via this function. */

void
route_tidyup(void)
{
router_instance *r;
for (r = routers; r != NULL; r = r->next)
  if (r->info->tidyup != NULL) (r->info->tidyup)(r);
}




/*************************************************
*                 Route one address              *
*************************************************/

/* This function is passed in one address item, for processing by the
routers. It has been determined that the address is (apparently) not for one of
the local domains. The action can be one of:

  . add the address to the chain on addr_remote for remote delivery and
      return OK;
  . return FAIL;
  . return DEFER;
  . return ISLOCAL - this means that the address turned out to be local
      after all - usually as the result of DNS expansion to fully qualified.
  . return PASS - meaning the router did something to the address, and wants
      it passed to the next router.

FAIL and PASS have the same effect, except for logging.

The verify flag is set if this is being called for verification rather than
delivery. If the router doesn't have its "verify" flag set, it is skipped.

Arguments:
  addr         address to route
  addr_local   chain of local-delivery addresses
  addr_remote  chain of remote-delivery addresses
  addr_new     chain for newly created addresses
  verify       v_none if not verifying
               v_sender if verifying a sender address
               v_recipient if verifying a recipient address

Returns:       OK, FAIL, DEFER, ISLOCAL or PASS (see above)
*/

int
route_address(address_item *addr, address_item **addr_local,
  address_item **addr_remote, address_item **addr_new, int verify)
{
BOOL more = TRUE;
router_instance *r;

DEBUG(9) debug_printf("routing %s, domain %s\n", addr->orig, addr->domain);

/* Set the domain used by the routers; a router is permitted to change this and
yield PASS, causing subsequent routers to use the new name, without changing
addr->domain, which is the envelope address. Equally, a router is also
permitted to change the envelope address. */

addr->route_domain = addr->domain;


/* Loop through all router instances. */

for (r = routers; more && r != NULL; r = r->next)
  {
  char *old_route_domain = addr->route_domain;
  address_item *parent;
  BOOL loop_detected = FALSE;
  int yield = FALSE;

  /* Skip this router if not verifying and it has verify_only set */

  if (verify == v_none && r->verify_only)
    {
    DEBUG(9) debug_printf("%s router skipped: verify_only set\n",
      r->name);
    continue;
    }

  /* Skip this router if domain mismatch */

  if (r->domains != NULL &&
       !match_isinlist(addr->route_domain, r->domains, &(r->re_domains), TRUE))
    {
    DEBUG(9) debug_printf("%s router skipped: domain mismatch\n",
      r->name);
    continue;
    }

  if (r->except_domains != NULL &&
       match_isinlist(addr->route_domain, r->except_domains,
         &(r->re_except_domains), TRUE))
    {
    DEBUG(9) debug_printf("%s router skipped: except domain mismatch\n",
      r->name);
    continue;
    }

  /* Skip this router if local part mismatch */

  if (r->local_parts != NULL &&
       !match_isinlist(addr->local_part, r->local_parts,
         &(r->re_local_parts), FALSE))
    {
    DEBUG(9) debug_printf("%s router skipped: local part mismatch\n",
      r->name);
    continue;
    }

  if (r->except_local_parts != NULL &&
       match_isinlist(addr->local_part, r->except_local_parts,
         &(r->re_except_local_parts), FALSE))
    {
    DEBUG(9) debug_printf("%s router skipped: except local part mismatch\n",
      r->name);
    continue;
    }

  /* If this router's "more" flag is FALSE, arrange that no subsequent
  routers are called. */

  more = r->more;
  DEBUG(9)
    {
    if (!more) debug_printf("%s router has more set FALSE\n", r->name);
    }

  /* Skip this router if verifying and it hasn't got the verify flag */

  if ((verify == v_sender && !r->verify_sender) ||
      (verify == v_recipient && !r->verify_recipient))
    {
    DEBUG(9) debug_printf("%s router skipped: verify %d %d %d\n",
      r->name, verify, r->verify_sender, r->verify_recipient);
    continue;
    }

  /* Loop protection: If this address has a parent with the same address that
  was routed by this router, we skip this router. This prevents a looping
  states when a new address is created by the use of "unseen" on a router.
  That is (at present) the only time a router can generate another address
  and hence make an address a parent; however, forwarding of local addresses
  can of course cause there to be parents at this point even without the
  use of "unseen". */

  for (parent = addr->parent; parent != NULL; parent = parent->parent)
    {
    if (strcmpic(parent->local_part, addr->local_part) == 0 &&
        strcmpic(parent->domain, addr->domain) == 0 &&
        parent->router == r)
      {
      DEBUG(9) debug_printf("%s router skipped: previously routed %s@%s\n",
        r->name, addr->local_part, addr->domain);
      loop_detected = TRUE;
      break;
      }
    }
  if (loop_detected) continue;

  /* Before calling the router, set up values that may be used in string
  expansions. Afterwards unset them, just in case. Also, unset the rewrite
  flag; routers must explicitly request rewriting. */

  deliver_domain = addr->route_domain;
  deliver_localpart = addr->local_part;
  addr->rewrite_headers = FALSE;

  /* Do file existence tests - must do after setting expansion values
  as the string is expanded - and if they succeed, run the router. Otherwise
  set an appropriate yield value. */

  switch(match_exists(r->require_files))
    {
    case DEFER:
    addr->message = string_sprintf("file existence defer in %s router: %s",
      r->name, strerror(errno));
    yield = DEFER;
    break;

    case FAIL:
    DEBUG(9) debug_printf("%s router: file existence failure\n", r->name);
    yield = FAIL;
    break;

    case OK:
    yield = (r->info->code)(r, addr, addr_local, addr_remote);
    break;
    }

  deliver_domain = NULL;
  deliver_localpart = NULL;

  /* If succeeded while verifying but fail_verify is set, convert into
  a failure. */

  if (((verify == v_sender && r->fail_verify_sender) ||
       (verify == v_recipient && r->fail_verify_recipient)) &&
      (yield == OK || yield == PASS))
    {
    addr->message = string_sprintf("%s router forced verify failure",
      r->name);
    break;
    }

  /* If router succeeded and "unseen" is set, make a new address with this
  as parent, to be handled by other routers. */

  if (yield == OK && r->unseen)
    {
    char *new_address =
      (addr->local_part[0] == ',' || addr->local_part[0] == ':')?
        string_sprintf("@%s%s", addr->domain, addr->local_part) :
        string_sprintf("%s@%s", addr->local_part, addr->domain);
    address_item *new = deliver_make_addr(new_address);
    new->parent = addr;
    new->ignore_error |= addr->ignore_error;
    new->errors_address = addr->errors_address;
    addr->child_count++;
    new->next = *addr_new;
    *addr_new = new;
    }

  /* Router modified the address; loop for next router. */

  if (yield == PASS)
    {
    DEBUG(2) debug_printf("%s router passed, rewriting %s as %s\n", r->name,
      old_route_domain, addr->route_domain);
    continue;
    }


  /* Failed to handle this address; loop for next router. */

  if (yield == FAIL)
    {
    DEBUG(2) debug_printf("%s router failed\n", r->name);
    continue;
    }


  /* Router wants this address to be failed; do not loop for next router. */

  if (yield == FORCEFAIL)
    {
    DEBUG(2) debug_printf("%s router forced address failure\n", r->name);
    break;
    }


  /* Deferral means we are finished with this address, as does an internal
  or configuration failure. */

  if (yield == DEFER || yield == ERROR)
    {
    DEBUG(2)
      {
      if (yield == DEFER)
        {
        debug_printf("%s router deferred %s\n", r->name, addr->route_domain);
        debug_printf("  message: %s\n", (addr->message == NULL)?
          "<none>" : addr->message);
        }
      else
        {
        debug_printf("%s router: error for %s\n", r->name, addr->route_domain);
        debug_printf("  message: %s\n", (addr->message == NULL)?
          "<none>" : addr->message);
        }
      }

    addr->router = r;
    return yield;
    }


  /* The yield is either OK or ISLOCAL. In both cases we rewrite envelope and
  headers if requested before finishing with this address. The rewriting takes
  its data from what the router did to the route_address, which may no longer
  be the same as the envelope address (in which case the rewriting probably
  isn't relevant there or in the headers, but the code does support it). */

  if (addr->rewrite_headers &&
      strcmp(old_route_domain, addr->route_domain) != 0)
    {
    /* First the domain in the envelope, if it was the same as the old routing
    domain. */

    if (strcmp(addr->domain, old_route_domain) == 0)
      addr->domain = addr->route_domain;

    /* Now the headers, except when verifying, when there aren't any! The
    first header is always "Received:" so we can skip it. There may not
    always be headers, however - in "-bt" mode there won't be any. */

    if (verify == v_none && header_list != NULL)
      {
      header_line *h;
      for (h = header_list->next; h != NULL; h = h->next)
        {
        header_line *newh = rewrite_header(h, old_route_domain,
          addr->route_domain);

        /* The old header gets its type set to "old", and the new one is
        inserted in the chain immediately following. The header_changed
        flag is set to force the spool header file to be rewritten. */

        if (newh != NULL)
          {
          h->type = htype_old;
          newh->prev = h;
          newh->next = h->next;
          h->next = newh;
          if (newh->next == NULL) header_last = newh;
            else newh->next->prev = newh;
          h = newh;
          header_changed = TRUE;
          }
        }
      }
    }


  /* If the yield was ISLOCAL, an apparently remote address was really that
  of a local domain (e.g. foo.bar got looked up in the DNS and turned into
  foo.bar.co.uk, which is one of the local domains). We return ISLOCAL so that
  the address can be passed to the directors instead. If rewrite_headers
  is not set we won't have processed the domain above, so do it here. This
  doesn't happen for domain expansion, but it can happen when MX->self causes
  re-routing. */

  if (yield == ISLOCAL)
    {
    DEBUG(2) debug_printf("%s router found %s to be local\n",
      r->name, old_route_domain);
    if (!addr->rewrite_headers && strcmp(addr->domain, old_route_domain) == 0)
      addr->domain = addr->route_domain;
    return ISLOCAL;
    }


  /* The only remaining possibility is that the router succeeded. */

  DEBUG(2)
    {
    debug_printf("routed by %s router:\n  deliver to ", r->name);

    if (addr->local_part[0] == ',' || addr->local_part[0] == ':')
      debug_printf("@%s%s\n", addr->domain, addr->local_part);
    else
      debug_printf("%s@%s\n", addr->local_part, addr->domain);

    debug_printf("  transport: %s\n", (addr->transport == NULL)?
      "<none>" : addr->transport->name);

    if (addr->errors_address != NULL)
      debug_printf("  errors to %s\n", addr->errors_address);

    if (addr->host_list != NULL)
      {
      host_item *h;
      for (h = addr->host_list; h != NULL; h = h->next)
        {
        debug_printf("  host %s ", h->name);
        if (h->address != NULL)
          debug_printf("[%s] ", h->address);
        if (h->mx >= 0) debug_printf("MX=%d", h->mx);
        debug_printf("\n");
        }
      }
    }

  /* Record which router did it, and return success, having cleared any
  temporary error message set by a router that failed. */

  addr->router = r;
  addr->message = NULL;
  return OK;
  }

/* No routers accepted this address; fail it. */

if (addr->message == NULL) addr->message = "unknown mail domain";
return FAIL;
}

/* End of route.c */
