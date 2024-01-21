/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "../exim.h"
#include "iplookup.h"


/* IP connection types */

#define ip_udp 0
#define ip_tcp 1


/* Options specific to the iplookup router. */

optionlist iplookup_router_options[] = {
  { "hosts",    opt_stringptr,
      (void *)(offsetof(iplookup_router_options_block, hosts)) },
  { "optional", opt_bool,
      (void *)(offsetof(iplookup_router_options_block, optional)) },
  { "protocol", opt_stringptr,
      (void *)(offsetof(iplookup_router_options_block, protocol_name)) },
  { "query",   opt_stringptr,
      (void *)(offsetof(iplookup_router_options_block, query)) },
  { "reroute", opt_stringptr,
      (void *)(offsetof(iplookup_router_options_block, reroute)) },
  { "response_pattern", opt_stringptr,
      (void *)(offsetof(iplookup_router_options_block, response_pattern)) },
  { "service",  opt_int,
      (void *)(offsetof(iplookup_router_options_block, service)) },
  { "timeout",  opt_time,
      (void *)(offsetof(iplookup_router_options_block, timeout)) }
};

/* Size of the options list. An extern variable has to be used so that its
address can appear in the tables drtables.c. */

int iplookup_router_options_count =
  sizeof(iplookup_router_options)/sizeof(optionlist);

/* Default private options block for the iplookup router. */

iplookup_router_options_block iplookup_router_option_defaults = {
  -1,       /* service */
  ip_udp,   /* protocol */
  5,        /* timeout */
  NULL,     /* protocol_name */
  NULL,     /* hosts */
  NULL,     /* query; NULL => local_part@domain */
  NULL,     /* response_pattern; NULL => don't apply regexp */
  NULL,     /* reroute; NULL => just used returned data */
  NULL,     /* re_response_pattern; compiled pattern */
  FALSE     /* optional */
};



/*************************************************
*          Initialization entry point            *
*************************************************/

/* Called for each instance, after its options have been read, to
enable consistency checks to be done, or anything else that needs
to be set up. */

void
iplookup_router_init(router_instance *rblock)
{
iplookup_router_options_block *ob =
  (iplookup_router_options_block *)(rblock->options_block);

/* A service and a host list must be given */

if (ob->service < 0)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s router:\n  "
    "a service (port) must be specified", rblock->name);

if (ob->hosts == NULL)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s router:\n  "
    "a host list must be specified", rblock->name);

/* A transport must NOT be given. */

if (rblock->transport != NULL)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s router:\n  "
    "a transport must not be given for the %s driver",
      rblock->name, rblock->info->driver_name);

/* Translate protocol name into value */

if (ob->protocol_name != NULL)
  {
  if (strcmp(ob->protocol_name, "udp") == 0) ob->protocol = ip_udp;
  else if (strcmp(ob->protocol_name, "tcp") == 0) ob->protocol = ip_tcp;
  else log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s router:\n  "
    "protocol not specified as udp or tcp", rblock->name);
  }

/* If a response pattern is given, compile it now to get the error early. */

if (ob->response_pattern != NULL)
  {
  regexp_compiling = ob->response_pattern;
  ob->re_response_pattern = regcomp(ob->response_pattern);
  regexp_compiling = NULL;
  }
}



/*************************************************
*              Main entry point                  *
*************************************************/

/* See local README for interface details */

int
iplookup_router_entry(
  router_instance *rblock,        /* data for this instantiation */
  address_item *addr,             /* address we are working on */
  address_item **addr_local,      /* add it to this if successful & local */
  address_item **addr_remote)     /* add it to this if successful & remote */
{
u_short net_port;
char *query = NULL;
char reply[256];
char *hostname, *reroute, *domain;
host_item *host = store_malloc(sizeof(host_item));
iplookup_router_options_block *ob =
  (iplookup_router_options_block *)(rblock->options_block);
regexp *re = ob->re_response_pattern;
int count, query_socket, query_len;
int error_yield = (ob->optional)? PASS : DEFER;
int yield = PASS;

DEBUG(2) debug_printf("%s router called for %s: route_domain = %s\n",
  rblock->name, addr->orig, addr->route_domain);

/* Build the query string to send. If not explicitly given, a default of
"user@domain user@domain" is used. */

if (ob->query == NULL)
  query = string_sprintf("%s@%s %s@%s", addr->local_part, addr->domain,
    addr->local_part, addr->domain);
else
  {
  query = expand_string(ob->query);
  if (query == NULL)
    {
    log_write(0, LOG_MAIN, "%s router: failed to expand %s", rblock->name,
      ob->query);
    addr->message = string_sprintf("%s router: failed to expand %s",
      rblock->name, ob->query);
    addr->special_action = SPECIAL_FREEZE;
    return ERROR;
    }
  }
query_len = (int)strlen(query);
DEBUG(9) debug_printf("%s router query is %s\n", rblock->name, query);

/* Create a socket, for UDP or TCP, as configured. */

query_socket = socket(AF_INET,
  (ob->protocol == ip_udp)? SOCK_DGRAM:SOCK_STREAM, 0);
if (query_socket < 0)
  {
  log_write(0, LOG_MAIN, "socket creation failed in %s router", rblock->name);
  return error_yield;
  }

/* Now bind the socket to the required port for each of the hosts in turn.
Initialization insists on the port being set and there being a host list. */

net_port = htons(ob->service);

for (hostname = string_firstinlist(ob->hosts, ':');
     hostname != NULL;
     hostname = string_nextinlist(':'))
  {
  struct sockaddr_in sin;
  host_item *h;

  host->name = hostname;
  host->address = NULL;
  host->next = NULL;

  if (host_find_byname(host, NULL) == HOST_FIND_FAILED) continue;

  /* Loop for possible multiple IP addresses for the given name. */

  for (h = host; h != NULL; h = h->next)
    {
    int rc, save_errno;
    if (h->address == NULL) continue;

    memset(&sin, 0, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_port = net_port;
    sin.sin_addr.s_addr = (S_ADDR_TYPE)inet_addr(h->address);

    /* Just try the next one if connection or sending fails; use the
    transport timeout handler code for timing out the connection - this
    isn't a transport, but it's not worth making up something else. In fact,
    timeouts can occur only for TCP calls; for a UDP socket, "connect" always
    works (the router will timeout later on the read call). */

    transport_sigalrm_seen = FALSE;
    signal(SIGALRM, transport_timeout_handler);
    alarm(ob->timeout);

    rc = connect(query_socket, (struct sockaddr *)&sin, sizeof(sin));
    save_errno = errno;

    alarm(0);
    signal(SIGALRM, SIG_DFL);

    if (rc < 0)
      {
      DEBUG(9)
        {
        if (save_errno == EINTR && transport_sigalrm_seen)
          save_errno = ETIMEDOUT;
        debug_printf("connection to %s failed: %s\n", host->address,
          strerror(save_errno));
        }
      continue;
      }

    if (send(query_socket, query, query_len, 0) < 0)
      {
      DEBUG(9) debug_printf("send to %s failed\n", host->address);
      continue;
      }

    /* The recv() function call is timed. There is a loop to cover the
    possibility of select() getting interrupted (rare, but can happen with,
    e.g. the SIGUSR1 signal from exiwhat, or returning with a positive result
    but no ready descriptor). Is this in fact possible? */

    for (;;)
      {
      int rc;
      fd_set select_inset;
      struct timeval tv;

      FD_ZERO (&select_inset);
      FD_SET (query_socket, &select_inset);
      tv.tv_sec = ob->timeout;
      tv.tv_usec = 0;
      rc = select(query_socket + 1, (SELECT_ARG2_TYPE *)&select_inset,
        NULL, NULL, &tv);

      /* Handle an interrupt. */

      if (rc < 0 && errno == EINTR) continue;

      /* Handle a timeout or any other error while select() was waiting. Treat
      the latter as if a timeout had occurred. */

      if (rc <= 0)
        {
        errno = ETIMEDOUT;
        count = -1;
        break;
        }

      /* If the socket is ready, initialize empty buffer in case nothing gets
      read, then read the response and break out of this select retry loop. */

      if (FD_ISSET(query_socket, &select_inset))
        {
        *reply = 0;
        count = recv(query_socket, reply, 255, 0);
        break;
        }
      }

    if (count < 0)
      {
      DEBUG(9) debug_printf("%s from %s\n", (errno == ETIMEDOUT)?
        "timed out" : "recv failed", host->address);
      continue;
      }

    reply[count] = 0;
    DEBUG(9) debug_printf("%s router received %s from %s\n",
      rblock->name, reply, host->address);
    break;
    }

  /* If h == NULL we have tried all the IP addresses and failed on all of them,
  so we must continue to try more host names. Otherwise we have succeeded. */

  if (h != NULL) break;
  }

/* Free the query and close the socket */

if (query != NULL) store_free(query);
close(query_socket);

/* If hostname is NULL, we have failed to find any host, or failed to
connect to any of the IP addresses, or timed out while reading or writing to
those we have connected to. In all cases, we must pass if optional and
defer otherwise. */

if (hostname == NULL)
  {
  DEBUG(9) debug_printf("%s router failed to get anything\n", rblock->name);
  return error_yield;
  }


/* We have received a response from the querying service. This router might do
routing according to the local part. Hence we mustn't subsequently copy its
routing for another address. */

addr->routed_by_domain = FALSE;


/* If a response pattern was supplied, match the returned string against it. A
failure to match causes the router to fail. After a successful match, set
up the numerical variables for expanding the rerouted address. */

if (re != NULL)
  {
  if (!regexec(re, reply))
    {
    DEBUG(9) debug_printf("%s router: %s failed to match response %s\n",
      rblock->name, ob->response_pattern, reply);
    return FAIL;
    }
  expand_nmax = 0;
  for (; expand_nmax < NSUBEXP; expand_nmax++)
    {
    expand_nstring[expand_nmax] = re->startp[expand_nmax];
    expand_nlength[expand_nmax] = re->endp[expand_nmax] -
      expand_nstring[expand_nmax];
    }
  expand_nmax--;
  }

/* If no response pattern was supplied, set up ${0} as the response up to the
first white space (if any). Also, if no query was specified, check that what
follows the white space matches user@domain. */

else
  {
  int n = 0;
  while (reply[n] != 0 && !isspace(reply[n])) n++;
  expand_nmax = 0;
  expand_nstring[0] = reply;
  expand_nlength[0] = n;

  if (ob->query == NULL)
    {
    int nn = n;
    while (isspace(reply[nn])) nn++;
    if (strcmp(query + query_len/2 + 1, reply+nn) != 0)
      {
      DEBUG(9) debug_printf("%s router: failed to match identification "
        "in response %s\n", rblock->name, reply);
      return FAIL;
      }
    }

  reply[n] = 0;  /* Terminate for the default case */
  }

/* If an explicit rerouting string is specified, expand it. Otherwise, use
what was sent back verbatim. */

if (ob->reroute != NULL)
  {
  reroute = expand_string(ob->reroute);
  if (reroute == NULL)
    {
    log_write(0, LOG_MAIN, "%s router: failed to expand %s", rblock->name,
      ob->reroute);
    addr->message = string_sprintf("%s router: failed to expand %s",
      rblock->name, ob->reroute);
    addr->special_action = SPECIAL_FREEZE;
    return ERROR;
    }
  }
else reroute = reply;

/* We should now have a new address in the form user@domain. This must be
used to update the local part, the domain that is being routed, and the
actual address domain. */

domain = strchr(reroute, '@');
if (domain == NULL)
  {
  log_write(0, LOG_MAIN, "%s router: reroute string %s is not of the form "
    "user@domain", rblock->name, reroute);
  addr->message = string_sprintf("%s router: reroute string %s is not of the "
    "form user@domain", rblock->name, reroute);
  addr->special_action = SPECIAL_FREEZE;
  return ERROR;
  }

/* Update the domain and local part. */

addr->local_part = string_copyn(reroute, domain - reroute);
addr->domain = addr->route_domain = string_copy(++domain);

/* Check whether the new domain is actually a local domain. If so, we have to
pass back this address to the directors. */

if (match_isinlist(domain, local_domains, &re_local_domains, TRUE))
  yield = ISLOCAL;

/* Free dynamic store, cancel numerical expansion variables, and return */

if (reroute != reply) store_free(reroute);

expand_nmax = -1;
return yield;
}

/* End of routers/iplookup.c */
