/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */



#include "../exim.h"
#include "lookuphost.h"



/* Options specific to the lookuphost router. */

optionlist lookuphost_router_options[] = {
  { "gethostbyname",   opt_bool,
      (void *)(offsetof(lookuphost_router_options_block, gethostbyname)) },
  { "mx_domains",         opt_stringptr,
      (void *)(offsetof(lookuphost_router_options_block, mx_domains)) },
  { "non_mx_domains",     opt_stringptr,
      (void *)(offsetof(lookuphost_router_options_block, non_mx_domains)) },
  { "qualify_single",  opt_bool,
      (void *)(offsetof(lookuphost_router_options_block, qualify_single)) },
  { "rewrite_headers", opt_bool,
      (void *)(offsetof(lookuphost_router_options_block, rewrite_headers)) },
  { "search_parents",  opt_bool,
      (void *)(offsetof(lookuphost_router_options_block, search_parents)) },
  { "self_mx",         opt_stringptr,
      (void *)(offsetof(lookuphost_router_options_block, self_mx)) },
  { "widen_domains",   opt_stringptr,
      (void *)(offsetof(lookuphost_router_options_block, widen_domains)) }
};

/* Size of the options list. An extern variable has to be used so that its
address can appear in the tables drtables.c. */

int lookuphost_router_options_count =
  sizeof(lookuphost_router_options)/sizeof(optionlist);

/* Default private options block for the lookuphost router. */

lookuphost_router_options_block lookuphost_router_option_defaults = {
  FALSE,           /* gethostbyname */
  TRUE,            /* qualify_single */
  TRUE,            /* search_parents */
  TRUE,            /* rewrite_headers */
  NULL,            /* self_mx */
  NULL,            /* widen_domains */
  NULL,            /* mx_domains */
  NULL,            /* non_mx_domains */
  NULL,            /* re_mx_domains */
  NULL             /* re_non_mx_domains */
};



/*************************************************
*          Initialization entry point            *
*************************************************/

/* Called for each instance, after its options have been read, to
enable consistency checks to be done, or anything else that needs
to be set up. */

void
lookuphost_router_init(router_instance *rblock)
{
lookuphost_router_options_block *ob =
  (lookuphost_router_options_block *)(rblock->options_block);

/* There must be a transport. */

if (rblock->transport == NULL)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s router:\n  "
    "a transport specification is required", rblock->name);

/* The self_mx option used to apply only to self-referential MX records.
Nowadays the generic "self" option applies to all forms of self reference.
For upwards compatibility, accept the old option and use it to set the
new variables, but only if they are unset. However, remove this at some point
in the future, say a year or so from now (29-Sep-96). */

if (ob->self_mx != NULL)
  {
  if (rblock->self == NULL)
    {
    char *s = ob->self_mx;
    rblock->self = s;
    if      (strcmp(s, "freeze") == 0)    rblock->self_code = self_freeze;
    else if (strcmp(s, "defer") == 0)     rblock->self_code = self_defer;
    else if (strcmp(s, "send") == 0)      rblock->self_code = self_send;
    else if (strcmp(s, "fail_soft") == 0) rblock->self_code = self_fail;
    else if (strcmp(s, "fail_hard") == 0) rblock->self_code = self_forcefail;
    else if (strncmp(s, "local:", 6) == 0)
      {
      s += 6;
      while (isspace(*s)) s++;
      if (strncmp(s, "rewrite:", 8) == 0)
        {
        rblock->self_rewrite = TRUE;
        s += 8;
        while (isspace(*s)) s++;
        }
      if (match_isinlist(s, local_domains, &re_local_domains, TRUE))
        {
        rblock->self = s;
        rblock->self_code = self_local;
        }
      else log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s router:\n  "
        "%s is not a local domain", rblock->name, s);
      }
    else log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s router:\n  "
        "%s is not valid for the self option", rblock->name, s);
    }
  else ob->self_mx = rblock->self;   /* for printing */
  }
}



/*************************************************
*              Main entry point                  *
*************************************************/

/* See local README for interface details */

int
lookuphost_router_entry(
  router_instance *rblock,        /* data for this instantiation */
  address_item *addr,             /* address we are working on */
  address_item **addr_local,      /* add it to this if successful & local */
  address_item **addr_remote)     /* add it to this if successful & remote */
{
host_item h;
int rc;
lookuphost_router_options_block *ob =
  (lookuphost_router_options_block *)(rblock->options_block);
char *widen = NULL;
char *fully_qualified_name;

DEBUG(2)
  debug_printf("%s router called for %s: %s lookup: route_domain = %s\n",
  rblock->name, addr->orig, (ob->gethostbyname)? "gethostbyname" : "dns",
  addr->route_domain);


/* Set up an initial host item, and then call the appropriate function to fill
in its address and chain on additional host_items if necessary. */

h.next = NULL;
h.name = addr->route_domain;
h.address = NULL;
h.mx = -1;
h.status = hstatus_unknown;
h.why = hwhy_unknown;
h.last_try = 0;


/* Loop to cope with explicit widening of domains as configured. */

if (ob->widen_domains != NULL)
  widen = string_firstinlist(ob->widen_domains, ':');

for (;;)
  {
  if (ob->gethostbyname)
    rc = host_find_byname(&h, &fully_qualified_name);

  /* Unfortunately, we cannot set the mx_only option in advance, because the
  DNS lookup may extend an unqualified name. Therefore, we must do the test
  subsequently. The MX-only test cannot be applied to source-routed addresses,
  which can be identified by a local-part starting with ',' or ':'. */

  else
    {
    rc = host_find_bydns(&h, FALSE, FALSE, ob->qualify_single,
      ob->search_parents, &fully_qualified_name);

    if ((rc == HOST_FOUND || rc == HOST_FOUND_LOCAL) &&
        h.mx < 0 &&                            /* Found A records only */
        addr->local_part[0] != ',' &&          /* Not source routed */
        addr->local_part[0] != ':' &&
        (ob->non_mx_domains == NULL ||         /* Not in non_mx_domains list */
        !match_isinlist(fully_qualified_name, ob->non_mx_domains,
        &(ob->re_non_mx_domains), TRUE)) &&
        (ob->mx_domains != NULL &&             /* In mx-domains list */
        match_isinlist(fully_qualified_name, ob->mx_domains,
        &(ob->re_mx_domains), TRUE)) &&
        !match_isinlist(fully_qualified_name,  /* Not turned into a local */
          local_domains, &re_local_domains, TRUE))              /* domain */
      {
      DEBUG(2) debug_printf("%s router rejected %s: no MX record(s)\n",
        rblock->name, fully_qualified_name);
      return FAIL;
      }
    }

  /* Deferral returns forthwith, and anything other than failure breaks the
  loop. */

  if (rc == HOST_FIND_AGAIN)
    {
    if (rblock->pass_on_timeout)
      {
      DEBUG(2) debug_printf("%s router timed out & pass_on_timeout set\n",
        rblock->name);
      return FAIL;
      }
    addr->message = "host lookup did not complete";
    return DEFER;
    }

  if (rc != HOST_FIND_FAILED) break;

  /* Check to see if the failure is the result of MX records pointing
  to non-existent domains, and if so, set an appropriate error message;
  otherwise "unknown mail domain" is used, which is confusion. Also, in this
  case don't do the widening. We need check only the first host to see if
  its MX has been filled in, but there is no address, because if there were
  any usable addresses returned, we would not have had HOST_FIND_FAILED. */

  if (h.mx >= 0 && h.address == NULL)
    {
    addr->message = "all relevant MX records point to non-existent hosts";
    return FAIL;
    }

  /* If there are any configured widening domains, widen the name we
  have got and try again. Otherwise, fail. */

  if (widen == NULL) return FAIL;
  h.name = string_sprintf("%s.%s", addr->route_domain, widen);
  widen = string_nextinlist(':');
  DEBUG(2) debug_printf("%s router widened %s to %s\n", rblock->name,
    addr->route_domain, h.name);
  }


/* If the original domain name has been changed as a result of the host lookup,
change the name in the address structure and request header rewrites if so
configured. Then check to see if the fully qualified name is in fact one of the
local domain names. If so, return ISLOCAL so that the address can be passed
back to the directors, and force header rewriting. */

if (strcmp(addr->route_domain, fully_qualified_name) != 0)
  {
  addr->route_domain = fully_qualified_name;
  addr->rewrite_headers = ob->rewrite_headers;
  if (match_isinlist(fully_qualified_name, local_domains, &re_local_domains,
      TRUE))
    {
    addr->rewrite_headers = TRUE;
    return ISLOCAL;
    }
  }


/* If the yield is HOST_FOUND_LOCAL, the remote domain name either found MX
records with the lowest numbered one pointing to a host with an IP address that
is set on one of the interfaces of this machine, or found A records or got
addresses from gethostbyname() that contain one for this machine. This can
happen quite legitimately if the original name was a shortened form of a local
domain, but if so, the fully qualified name will be a local domain and will
have been detected above. If it is not, there may be some kind of configuration
error or lookuphost error.

In the case of a source-routed address, the best plan is to move on to the next
host in the source routing, as happens if a source-routed address starts with
a local domain. This is done by rewriting the domain and local part, and
returning ISLOCAL. For non-source-routed addresses, the action to be taken can
be configured by the self option as follows:

.  freeze: Log the incident, freeze, and return ERROR
.  defer:  Log the incident and return ERROR
.  send:   Carry on with the delivery regardless - this makes sense only
           if the SMTP listener on this machine is a differently configured
           MTA
.  <local-domain>: Change the domain to the given local domain and return
   ISLOCAL so it gets passed back to the directors. The domain was checked
   for being local at initialization.

The default is freeze, since this state is normally a serious error.

The handling of all of this is now in a separate function, as it is also
required for other routers. */

if (rc == HOST_FOUND_LOCAL)
  {
  rc = host_self_action(addr, h.mx, rblock->self_code, rblock->self_rewrite,
    rblock->self);
  if (rc != OK) return rc;
  }

/* Get store in which to preserve the original host item, chained on
to the address. */

addr->host_list = store_malloc(sizeof(host_item));
addr->host_list[0] = h;

/* Fill in the transport (known to exist), queue the address for local or
remote delivery, and yield success. */

addr->transport = rblock->transport;
if (addr->transport->info->local)
  {
  addr->next = *addr_local;
  *addr_local = addr;
  }
else
  {
  addr->next = *addr_remote;
  *addr_remote = addr;
  }

DEBUG(9) debug_printf("%s router succeeded\n", rblock->name);
return OK;
}

/* End of routers/lookuphost.c */
