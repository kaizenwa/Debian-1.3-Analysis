/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "../exim.h"
#include "ipliteral.h"



/* Options specific to the ipliteral router. Because some compilers do not like
empty declarations ("undefined" in the Standard) we put in a dummy value. */


optionlist ipliteral_router_options[] = {
  { "", opt_hidden, NULL }
};

/* Size of the options list. An extern variable has to be used so that its
address can appear in the tables drtables.c. */

int ipliteral_router_options_count =
  sizeof(ipliteral_router_options)/sizeof(optionlist);

/* Default private options block for the ipliteral router. Again, a dummy
value is present to keep some compilers happy. */

ipliteral_router_options_block ipliteral_router_option_defaults = { 0 };



/*************************************************
*          Initialization entry point            *
*************************************************/

/* Called for each instance, after its options have been read, to
enable consistency checks to be done, or anything else that needs
to be set up. */

void
ipliteral_router_init(router_instance *rblock)
{
/*
ipliteral_router_options_block *ob =
  (ipliteral_router_options_block *)(rblock->options_block);
*/

/* There must be a transport. */

if (rblock->transport == NULL)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s router:\n  "
    "a transport specification is required", rblock->name);
}



/*************************************************
*              Main entry point                  *
*************************************************/

/* See local README for interface details */

int
ipliteral_router_entry(
  router_instance *rblock,        /* data for this instantiation */
  address_item *addr,             /* address we are working on */
  address_item **addr_local,      /* add it to this if successful & local */
  address_item **addr_remote)     /* add it to this if successful & remote */
{
transport_instance *transport = rblock->transport;
/*
ipliteral_router_options_block *ob =
  (ipliteral_router_options_block *)(rblock->options_block);
*/
host_item *h;
char *domain = addr->route_domain;
int len = (int)strlen(domain);

DEBUG(2) debug_printf("%s router called for %s: route_domain = %s\n",
  rblock->name, addr->orig, addr->route_domain);

/* Check that the domain is an IP address enclosed in square brackets. If
not, the router fails. Otherwise route to the single IP address, setting the
host name to "(unnamed)". */

if (domain[0] != '[' || domain[len-1] != ']') return FAIL;
domain[len-1] = 0;  /* temporarily */

if (!regexec(regexp_ip_address, domain+1))
  {
  domain[len-1] = ']';
  return FAIL;
  }

/* Set up a host item */

addr->host_list = h = store_malloc(sizeof(host_item));

h->next = NULL;
h->name = string_copy("(unnamed)");
h->address = string_copy(domain+1);
h->mx = -1;
h->status = hstatus_unknown;
h->why = hwhy_unknown;
h->last_try = 0;

domain[len-1] = ']';   /* restore */

/* Determine whether the host is the local host, and if so, take action
according to the configuration. */

if (host_scan_for_local_hosts(h, h) == HOST_FOUND_LOCAL)
  {
  int rc = host_self_action(addr, -1, rblock->self_code, rblock->self_rewrite,
    rblock->self);
  if (rc != OK) return rc;
  }

/* Fill in the transport, queue the address for local or remote delivery, and
yield success. For local delivery, of course, the IP address won't be used. */

addr->transport = transport;
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

return OK;
}

/* End of routers/ipliteral.c */
