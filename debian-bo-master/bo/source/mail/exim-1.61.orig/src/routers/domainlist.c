/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "../exim.h"
#include "domainlist.h"



/* Options specific to the domainlist router. The partial_match option is
not set explicitly, but as a side-effect of search_type. An entry in this
table allows the option-reading function to find where to put the value. */

optionlist domainlist_router_options[] = {
  { "*partial_match", opt_int | opt_hidden,
      (void *)(offsetof(domainlist_router_options_block, partial_match)) },
  { "modemask",       opt_octint,
      (void *)(offsetof(domainlist_router_options_block, modemask)) },
  { "owners",         opt_uidlist,
      (void *)(offsetof(domainlist_router_options_block, owners)) },
  { "owngroups",      opt_gidlist,
      (void *)(offsetof(domainlist_router_options_block, owngroups)) },
  { "route_file",     opt_stringptr,
      (void *)(offsetof(domainlist_router_options_block, route_file)) },
  { "route_list",     opt_stringptr,
      (void *)(offsetof(domainlist_router_options_block, route_list)) },
  { "route_query",    opt_stringptr,
      (void *)(offsetof(domainlist_router_options_block, route_query)) },
  { "search_type",    opt_searchtype,
      (void *)(offsetof(domainlist_router_options_block, search_type)) }
};

/* Size of the options list. An extern variable has to be used so that its
address can appear in the tables drtables.c. */

int domainlist_router_options_count =
  sizeof(domainlist_router_options)/sizeof(optionlist);

/* Default private options block for the domainlist router. */

domainlist_router_options_block domainlist_router_option_defaults = {
  -1,           /* search_type */
  0,            /* partial_match */
  022,          /* modemask */
  NULL,         /* owners */
  NULL,         /* owngroups */
  NULL,         /* route_file */
  NULL,         /* route_query */
  NULL,         /* route_list */
  NULL          /* re_list */
};





/*************************************************
*          Initialization entry point            *
*************************************************/

/* Called for each instance, after its options have been read, to
enable consistency checks to be done, or anything else that needs
to be set up. */

void
domainlist_router_init(router_instance *rblock)
{
domainlist_router_options_block *ob =
  (domainlist_router_options_block *)(rblock->options_block);

/* One of route_list or route_file or route_query must be specified */

if (ob->route_file == NULL && ob->route_list == NULL && ob->route_query == NULL)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s router:\n  "
    "either route_list or route_file or route_query required", rblock->name);

/* A search type is mandatory if a file name or query is given. */

if ((ob->route_file != NULL || ob->route_query != NULL) && ob->search_type < 0)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s router:\n  "
    "a search type is required when a file or query is given", rblock->name);

/* Otherwise, search type can be unset and that's all we need to do. */

if (ob->search_type < 0) return;

/* For single-key+file search types, a file name is mandatory; for other types
a query is mandatory. An absolute file name is mandatory for lsearch and dbm;
but can't check for absoluteness if the name is being looked up. */

if (ob->search_type < stype_querystyle)
  {
  if (ob->route_query != NULL)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s router:\n  "
      "\"query\" specified for a single-key search type", rblock->name);

  if (ob->route_file == NULL)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s router:\n  "
      "no file name specified but search type set", rblock->name);

  if ((ob->search_type == stype_lsearch || ob->search_type == stype_dbm) &&
       ob->route_file[0] != '/' && ob->route_file[0] != '$')
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s router:\n  "
      "an absolute file path name is required for lsearch or dbm",
      rblock->name);
  }

/* Non-single-key search type */

else
  {
  if (ob->route_file != NULL)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s router:\n  "
      "\"file\" specified for a non-single-key search type", rblock->name);

  if (ob->route_query == NULL)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s router:\n  "
      "no query specified but non-single-key search type set", rblock->name);
  }
}




/*************************************************
*             Parse route list item              *
*************************************************/

/* The format of a route list item is:

  <domain> [<host[list]> [<options>]]

if obtained from a configuration string. The host list may be enclosed in
single or double quotes. When read from a file, the domain isn't present (it's
the key used for the lookup). Point the arguments to the various parts of the
line, and insert zeroes to terminate the strings. If domain == NULL, then
there's no domain at the start.

Arguments:
  s         pointer to route list item
  domain    if not NULL, where to put the domain pointer
  hostlist  where to put the host[list] pointer
  options   where to put the options pointer

Returns:    FALSE if domain expected and string is empty;
            TRUE otherwise
*/

static BOOL
parse_routelist_item(char *s, char **domain, char **hostlist, char **options)
{
if (domain != NULL)
  {
  while (*s != 0 && isspace(*s)) s++;
  if (*s == 0) return FALSE;            /* missing data */
  *domain = s;
  while (*s != 0 && !isspace(*s)) s++;
  if (*s != 0) *s++ = 0;
  }

while (*s != 0 && isspace(*s)) s++;

if (*s == '\"' || *s == '\'')
  {
  *hostlist = ++s;
  while (*s != 0 && *s != '\"' && *s != '\'') s++;
  }
else
  {
  *hostlist = s;
  while (*s != 0 && !isspace(*s)) s++;
  }

if (*s != 0)
  {
  *s++ = 0;
  while (*s != 0 && isspace(*s)) s++;
  }

*options = s;
return TRUE;
}



/*************************************************
*              Main entry point                  *
*************************************************/

/* The domainlist router recognizes explicitly listed domains. They may be in a
configuration string, in which case wild cards and regular expressions are
available, or they may be looked up in a file, in which case only exact matches
are available. The route_domain field of the address is replaced by a new
domain taken from the listing.

The address can then be passed on to subsequent routers, or the router can be
configured that the address be looked up here, either using gethostbyname or in
the DNS. In this case, routing is completed here, and a transport must be
specified for the router. Furthermore, more than one host may be specified. */

/* See local README for interface details */

int
domainlist_router_entry(
  router_instance *rblock,        /* data for this instantiation */
  address_item *addr,             /* address we are working on */
  address_item **addr_local,      /* add it to this if successful & local */
  address_item **addr_remote)     /* add it to this if successful & remote */
{
BOOL matched = FALSE;
BOOL free_routelist_item = FALSE;
BOOL individual_transport_set = FALSE;
int lookup_type;
host_item *h, *prev, **hchain;
char *routelist_item = NULL;
char *domain, *hostlist, *newhostlist, *options, *name;
domainlist_router_options_block *ob =
  (domainlist_router_options_block *)(rblock->options_block);
transport_instance *transport = rblock->transport;

DEBUG(2) debug_printf("%s router called for %s: route_domain = %s\n",
  rblock->name, addr->orig, addr->route_domain);

/* The initialization check ensures that either route_file or route_list or
both are specified. The list takes precedence over the file. */

if (ob->route_list != NULL)
  {
  re_block **rechain = &(ob->re_list);

  for (routelist_item = string_firstinlist(ob->route_list, ';');
       routelist_item != NULL;
       routelist_item = string_nextinlist(';'))
    {
    DEBUG(9) debug_printf("routelist_item = %s\n", routelist_item);
    if (!parse_routelist_item(routelist_item, &domain, &hostlist, &options))
      continue;     /* Ignore blank items */

    /* Check the current route_domain; if it matches, set flag and break
    the loop. The final argument of zero requests $0 be set to the whole
    thing and $1, $2, etc. as appropriate. If no match, advance the regular
    expression compiled pointer if we have just passed a regular expression. */

    if (match_check_string(addr->route_domain, domain, rechain, 0, TRUE))
      {
      matched = TRUE;
      break;
      }
    else if (domain[0] == '^' && *rechain != NULL)
      rechain = &((*rechain)->next);
    }
  }

DEBUG(9) debug_printf("after handling route_lists items, matched = %d\n",
           matched);

/* Handle a file (or other) lookup if the string search hasn't succeeded, or if
there was no string supplied. The search functions return a handle identifying
the search. For files this is a FILE * or a DBM *; for other things is is < 0.
If this router has been called earlier for this message, the search database
may already be open. */

if (!matched && (ob->route_file != NULL || ob->route_query != NULL))
  {
  int expand_setup = 0;
  char *error;
  char *filename;
  char *query;
  void *handle;

  if (ob->search_type < stype_querystyle)
    {
    query = addr->route_domain;
    filename = expand_string(ob->route_file);
    if (filename == NULL)
      {
      addr->message = string_sprintf("%s router: failed to expand %s: %s",
        rblock->name, ob->route_file, expand_string_message);
      log_write(0, LOG_MAIN|LOG_PANIC, "%s", addr->message);
      addr->special_action = SPECIAL_FREEZE;
      return ERROR;
      }
    }
  else
    {
    filename = NULL;
    query = expand_string(ob->route_query);
    if (query == NULL)
      {
      addr->message = string_sprintf("%s router: failed to expand %s: %s",
        rblock->name, ob->route_query, expand_string_message);
      log_write(0, LOG_MAIN|LOG_PANIC, "%s", addr->message);
      addr->special_action = SPECIAL_FREEZE;
      return ERROR;
      }
    }

  handle = search_open(filename, ob->search_type, ob->modemask,
    ob->owners, ob->owngroups, &error);

  if (handle == NULL)
    {
    addr->message = string_sprintf("%s router: %s", rblock->name, error);
    log_write(0, LOG_MAIN, "%s router: %s", rblock->name, error);
    addr->special_action = SPECIAL_FREEZE;
    return ERROR;
    }

  /* Set up $0 for expansion, but don't turn it on yet. */

  expand_nstring[0] = addr->route_domain;
  expand_nlength[0] = (int)strlen(addr->route_domain);

  /* Now search the file (or whatever) for the entry we are interested in.
  The text is returned in dynamic store. Partial matching is handled within
  the search_find() function. */

  routelist_item = search_find(handle, filename, query, ob->search_type,
    ob->partial_match, &expand_setup, &error);

  /* If we found the domain, parse the resulting data; in this case,
  parsing never fails as an empty entry is permitted. Turn on the
  expansion variables that have been set up above. */

  if (routelist_item != NULL)
    {
    (void) parse_routelist_item(routelist_item, NULL, &hostlist, &options);
    free_routelist_item = TRUE;
    matched = TRUE;
    expand_nmax = expand_setup;
    }
  else
    DEBUG(9) debug_printf("search_find failed: %s\n", error);
  }


/* If we haven't matched anything, the router fails */

if (!matched) return FAIL;


/* After a match, we expand the hostlist item. It may then pointing to an empty
string, or to a single host or a list of hosts; options is pointing to the rest
of the routelist item, which is either empty or contains various option words.
A host list must be present unless a local transport is defined, in which case
it must not be present. */

DEBUG(9) debug_printf("original hostlist=%s options=%s\n", hostlist, options);

newhostlist = expand_string(hostlist);
expand_nmax = -1;

/* If the expansion was forced to fail, just fail the router. Otherwise
there is a configuration problem. */

if (newhostlist == NULL)
  {
  if (expand_string_forcedfail) return FAIL;
  addr->message = string_sprintf("%s router: failed to expand %s: %s",
    rblock->name, hostlist , expand_string_message);
  log_write(0, LOG_MAIN|LOG_PANIC, "%s", addr->message);
  addr->special_action = SPECIAL_FREEZE;
  return ERROR;
  }
else hostlist = newhostlist;

DEBUG(9) debug_printf("expanded hostlist=\"%s\" options=%s\n", hostlist,
  options);

/* Set default lookup type and scan the options */

lookup_type = lk_pass;

while (*options != 0)
  {
  int term;
  char *s = options;
  while (*options != 0 && !isspace(*options)) options++;
  term = *options;
  *options = 0;

  if (strcmp(s, "byname") == 0) lookup_type = lk_byname;
  else if (strcmp(s, "bydns") == 0) lookup_type = lk_bydns;
  else if (strcmp(s, "bydns_a") == 0) lookup_type = lk_bydns_a;
  else if (strcmp(s, "bydns_mx") == 0) lookup_type = lk_bydns_mx;
  else
    {
    transport_instance *t;
    for (t = transports; t != NULL; t = t->next)
      {
      if (strcmp(t->name, s) == 0)
        {
        transport = t;
        individual_transport_set = TRUE;
        break;
        }
      }
    if (t == NULL)
      {
      s = string_sprintf("unknown routing option or transport name \"%s\"", s);
      log_write(0, LOG_MAIN, "Error in %s router: %s", rblock->name, s);
      addr->message = string_sprintf("error in router: %s", s);
      addr->special_action = SPECIAL_FREEZE;
      if (free_routelist_item) store_free(routelist_item);
      store_free(hostlist);
      return ERROR;
      }
    }

  if (term != 0)
    {
    options++;
    while (*options != 0 && isspace(*options)) options++;
    }
  }

/* Deal with the case of a local transport. A single host is allowed (can
be used, for example, as an expansion in a file name). */

if (transport != NULL && transport->info->local)
  {
  if (hostlist[0] != 0)
    {
    host_item *h;

    if (strchr(hostlist, ':') != NULL)
      {
      log_write(0, LOG_MAIN, "Error in %s router: attempt to pass more than one "
        "host name to a local transport", rblock->name);
      addr->message = "error in router: attempt to pass more than one host "
        "name to a local transport";
      addr->special_action = SPECIAL_FREEZE;
      if (free_routelist_item) store_free(routelist_item);
      store_free(hostlist);
      return ERROR;
      }

    addr->host_list = h = store_malloc(sizeof(host_item));
    h->name = string_copy(hostlist);
    h->address = NULL;
    h->mx = -1;
    h->status = hstatus_unknown;
    h->why = hwhy_unknown;
    h->last_try = 0;
    h->next = NULL;
    }

  /* There is nothing more to do other than to queue the address for the
  local transport, remembering to free the route item if it came from a file
  lookup. */

  if (free_routelist_item) store_free(routelist_item);
  store_free(hostlist);
  addr->transport = transport;
  addr->next = *addr_local;
  *addr_local = addr;
  return OK;
  }


/* There is either no transport or a remote transport. A host list is mandatory
in either case. */

if (hostlist[0] == 0)
  {
  log_write(0, LOG_MAIN, "Error in %s router: no host(s) specified for %s",
    rblock->name, domain);
  addr->message =
    string_sprintf("error in router: no host(s) specified for %s", domain);
  addr->special_action = SPECIAL_FREEZE;
  if (free_routelist_item) store_free(routelist_item);
  store_free(hostlist);
  return ERROR;
  }

/* If configured to pass the host on to the next router, there must be only one
host name. */

if (lookup_type == lk_pass)
  {
  if (strchr(hostlist, ':') != NULL || individual_transport_set)
    {
    char *s = individual_transport_set?
      "transport specified when passing domain on to next router" :
      "attempt to pass more than one domain to next router";
    addr->message = string_sprintf("error in router: %s", s);
    log_write(0, LOG_MAIN, "Error in %s router: %s", rblock->name, s);
    addr->special_action = SPECIAL_FREEZE;
    if (free_routelist_item) store_free(routelist_item);
    store_free(hostlist);
    return ERROR;
    }

  addr->route_domain = hostlist;  /* and don't free it! */
  if (free_routelist_item) store_free(routelist_item);
  return PASS;
  }


/* Otherwise we finish the routing here by building a chain of host items
for the list of configured hosts, and then finding their addresses. Typically
there is only one host. */

hchain = &(addr->host_list);

for (name = string_firstinlist(hostlist, ':'); name != NULL;
     name = string_nextinlist(':'))
  {
  h = store_malloc(sizeof(host_item));
  h->name = string_copy(name);
  h->address = NULL;
  h->mx = -1;
  h->status = hstatus_unknown;
  h->why = hwhy_unknown;
  h->last_try = 0;

  h->next = NULL;
  *hchain = h;
  hchain = &(h->next);
  }


/* Free the route item if it came from a file lookup, and free the textual
hostlist. */

if (free_routelist_item) store_free(routelist_item);
store_free(hostlist);


/* Look up each host address. A lookup may add additional items into the chain
if there are multiple addresses. If any host is identified as being the local
host, omit it and any subsequent hosts - i.e. treat the list like an ordered
list of MX hosts. If the first host is the local host, act according to the
"self" option in the configuration. */

prev = NULL;
for (h = addr->host_list; h != NULL; prev = h, h = h->next)
  {
  char *canonical_name;
  int rc;

  if (h->address != NULL) continue;

  if (lookup_type == lk_byname)
    rc = host_find_byname(h, &canonical_name);
  else
    rc = host_find_bydns(h, (lookup_type == lk_bydns_mx),
      (lookup_type == lk_bydns_a), FALSE, FALSE, &canonical_name);

  if (rc == HOST_FIND_AGAIN)
    {
    if (rblock->pass_on_timeout)
      {
      DEBUG(2) debug_printf("%s router timed out & pass_on_timeout set\n",
        rblock->name);
      return FAIL;
      }
    return DEFER;
    }

  if (rc == HOST_FIND_FAILED)
    {
    log_write(0, LOG_MAIN, "Error in %s router: lookup of host %s failed",
      rblock->name, h->name);
    addr->message = "error in router: host lookup failed";
    addr->special_action = SPECIAL_FREEZE;
    return ERROR;
    }

  if (rc == HOST_FOUND_LOCAL)
    {
    if (prev != NULL)
      {
      DEBUG(8)
        {
        debug_printf("Removed from host list:\n");
        for (; h != NULL; h = h->next) debug_printf("  %s\n", h->name);
        }
      prev->next = NULL;
      break;
      }
    rc = host_self_action(addr, h->mx, rblock->self_code, rblock->self_rewrite,
      rblock->self);
    if (rc != OK) return rc;
    }
  }

/* Finally, since we have done all the routing here, there must be a transport
defined for these hosts. It will be a remote one, as a local transport is
dealt with above. */

if (transport == NULL)
    {
    log_write(0, LOG_MAIN, "Error in %s router: no transport defined",
      rblock->name);
    addr->message = "error in router: transport missing";
    addr->special_action = SPECIAL_FREEZE;
    return ERROR;
    }

/* Fill in the transport, queue for remote delivery. */

addr->transport = transport;
addr->next = *addr_remote;
*addr_remote = addr;

return OK;
}

/* End of routers/domainlist.c */
