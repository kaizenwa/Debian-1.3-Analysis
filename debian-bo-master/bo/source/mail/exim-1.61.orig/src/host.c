/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Functions for finding hosts, either by gethostbyname, gethostbyaddr, or via
the DNS. Also contains a function for finding all the running interfaces of
the current host - overridable by the setting of local_interfaces. Also
contains a random number function, used for randomizing hosts with equal MXs
but available for use in other parts of Exim. */


#include "exim.h"



#if PACKETSZ < 1024
  #define MAXPACKET 1024
#else
  #define MAXPACKET PACKETSZ
#endif

static unsigned char *dns_answer = NULL;
static int dns_answerlen = 0;

enum { RESET_NEXT, RESET_ANSWERS, RESET_ADDITIONAL };

enum { DNS_SUCCEED, DNS_NOMATCH, DNS_AGAIN, DNS_FAIL };




/* Structure for holding a partially decoded DNS record; the name has been
uncompressed, but the data pointer is into the raw data. */

typedef struct {
  char  name[DNS_MAXNAME];        /* domain name */
  int   type;                     /* record type */
  int   size;                     /* size of data */
  u_char *data;                   /* pointer to data */
} dns_record;





/*************************************************
*              Random number generator           *
*************************************************/

/* This is a simple pseudo-random number generator. It does not have to be
very good for the uses to which it is put.

Arguments:
  limit:    one more than the largest number required

Returns:    a pseudo-random number in the range 0 to limit-1
*/

int
random_number(int limit)
{
static unsigned int seed = -1;
if (seed == -1)
  {
  int p = (int)getpid();
  seed = (int)time(NULL) ^ ((p << 16) | p);
  }
seed = 1103515245 * seed + 12345;
return (unsigned int)(seed >> 16) % limit;
}





/*************************************************
*        Decode actions for self reference       *
*************************************************/

/* This function is called from a number of routers on receiving
HOST_FOUND_LOCAL when looking up a supposedly remote host. There are
two cases:

(1)  If the current address is source-routed, then we can remove the
     first source-routing domain and try again with the remainder by
     returning ISLOCAL. This will cause the new address to be routed
     ab initio.

(2)  For a non-source-routed address, the action is controlled by a
     generic configuration option called "self" on each router, which
     can be one of:

     .  freeze: Log the incident, freeze, and return ERROR
     .  defer:  Log the incident and return ERROR
     .  send:   Carry on with the delivery regardless - this makes sense only
                if the SMTP listener on this machine is a differently
                configured MTA
     .  <new-domain>: Change the domain to the given domain and return
        ISLOCAL so it gets passed back to the directors or routers

Arguments:
  addr     the address being routed
  mx       the MX value, or -1 if MX records not used
  code     the action to be taken (one of the self_xxx enums) for non-
             source-routed addresses
  rewrite  TRUE if rewriting headers required for ISLOCAL
  new      new domain to be used for ISLOCAL

Returns:   (1) ISLOCAL for a source-routed address, or
           (2) ERROR, DEFER, ISLOCAL, FAIL, FORCEFAIL, or OK, according
               to the value of code
*/

int
host_self_action(address_item *addr, int mx, int code, BOOL rewrite, char *new)
{
char *format, *msg;

/* Handle a source-routed address */

if (addr->local_part[0] == ',' || addr->local_part[0] == ':')
  {
  char *p = addr->local_part + 1;

  if (addr->local_part[0] == ':')
    {
    while (*p != '@') p++;
    *p = 0;
    addr->domain = addr->route_domain = p+1;
    addr->local_part++;
    }
  else
    {
    while (*p != ',' && *p != ':') p++;
    addr->domain = addr->route_domain =
      string_copyn(addr->local_part + 2, p - addr->local_part - 2);
    addr->local_part = p;
    }

  DEBUG(2) debug_printf("source-routed address is local: rewrote\n  "
    "domain = %s\n  local_part = %s\n", addr->route_domain,
      addr->local_part);
  return ISLOCAL;
  }

/* Handle a non-source-routed address */

if (mx >= 0)
  {
  format = "lowest MX record for %s points to local host";
  msg = "lowest numbered MX record points to local host";
  }
else
  {
  format = "remote host address for %s is the local host";
  msg = "remote host address is the local host";
  }

switch (code)
  {
  case self_freeze:
  log_write(0, LOG_MAIN, format, addr->route_domain);
  addr->message = msg;
  addr->special_action = SPECIAL_FREEZE;
  return ERROR;

  case self_defer:
  addr->message = msg;
  return DEFER;

  case self_local:
  DEBUG(2)
    {
    debug_printf(format, addr->route_domain);
    debug_printf(": domain changed to %s\n", new);
    }
  addr->route_domain = string_copy(new);
  addr->rewrite_headers = rewrite;
  return ISLOCAL;

  case self_send:
  DEBUG(2)
    {
    debug_printf(format, addr->route_domain);
    debug_printf(": configured to try delivery anyway\n");
    }
  return OK;

  case self_fail:
  DEBUG(2)
    {
    debug_printf(format, addr->route_domain);
    debug_printf(": router failed soft\n");
    }
  addr->message = msg;
  return FAIL;

  case self_forcefail:
  DEBUG(2)
    {
    debug_printf(format, addr->route_domain);
    debug_printf(": router failed hard\n");
    }
  addr->message = msg;
  return FORCEFAIL;
  }

return FAIL;   /* paranoia */
}



/*************************************************
*         Find addresses on local interfaces     *
*************************************************/

/* This function finds the addresses of all the running interfaces on the
machine. A chain of blocks containing the textual form of the addresses is
returned. Solaris 2 has the SIOGIFNUM call to get the number of interfaces, but
other OS (including Solaris 1) appear not to. So just screw in a largeish fixed
number. Unfortunately, the www addressing scheme means that some hosts have a
very large number of virtual interfaces. Such hosts are recommended to set
local_interfaces to avoid problems with this (see host_find_interfaces below).

Arguments:    none
Returns:      a chain of ip_address_items, each pointing to a textual
              version of an IP address
*/

#define MAXINTERFACES 250

static ip_address_item *
host_find_running_interfaces(void)
{
struct ifconf ifc;
struct ifreq ifreq, *ifr;
int n, vs, size;
struct sockaddr_in iface;
ip_address_item *yield = NULL;
ip_address_item *last = NULL;
ip_address_item  *next;
char buf[MAXINTERFACES*sizeof(struct ifreq)];

/* We have to create a socket in order to do ioctls on it to find out
what we want to know. */

if ((vs = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
  log_write(0, LOG_PANIC_DIE, "Unable to create socket to find interface "
    "addresses: %d %s", errno, strerror(errno));

/* Get the interface configuration. */

ifc.ifc_len = sizeof(buf);
ifc.ifc_buf = buf;

if (ioctl(vs, SIOCGIFCONF, (char *)&ifc) < 0)
  log_write(0, LOG_PANIC_DIE, "Unable to get interface configuration: %d %s",
    errno, strerror(errno));

/* For each interface, check it is an IP interface, get its flags, and see if
it is up; if not, skip */

for(n = ifc.ifc_len, ifr = ifc.ifc_req; n > 0;
    ifr = (struct ifreq *)((char *)ifr + size))
  {
  size = sizeof(*ifr);
  n -= size;

  /* If not an IP interface, skip */

  if (ifr->ifr_addr.sa_family != AF_INET) continue;

  /* Get the interface flags, and if the interface is down, continue */

  ifreq = *ifr;
  if (ioctl(vs, SIOCGIFFLAGS, (char *)&ifreq) < 0)
    log_write(0, LOG_PANIC_DIE, "Unable to get interface flags: %d %s",
      errno, strerror(errno));
  if ((ifreq.ifr_flags & IFF_UP) == 0) continue;

  /* Get the IP address of the interface */

  if (ioctl(vs, SIOCGIFADDR, (char *)&ifreq) < 0)
    log_write(0, LOG_PANIC_DIE, "Unable to get interface address: %d %s",
      errno, strerror(errno));
  iface = *(struct sockaddr_in *)&ifreq.ifr_addr;

  /* Create a data block for the address, fill in the data, and put it on the
  chain. */

  next = store_malloc(sizeof(ip_address_item));
  next->next = NULL;
  sprintf(next->address, "%s", inet_ntoa(iface.sin_addr));
  if (yield == NULL) yield = last = next; else
    {
    last->next = next;
    last = next;
    }

  DEBUG(8) debug_printf("Actual local interface address is %s\n",
    last->address);
  }

/* Close the socket, and return the chain of data blocks. */

close(vs);
return yield;
}



/*************************************************
*         Find addresses on local interfaces     *
*************************************************/

/* This function finds the addresses of local IP interfaces. If the option
local_interfaces is not set, it calls host_find_running_interfaces() in order
to find all the actual interfaces on the host. Otherwise it takes the list in
local_interfaces. Used to detect when apparently remote hosts are really local.
A chain of blocks containing the textual form of the addresses is returned.

Arguments:    none
Returns:      a chain of ip_address_items, each pointing to a textual
              version of an IP address
*/

ip_address_item *
host_find_interfaces(void)
{
char *s;
ip_address_item *yield = NULL;
ip_address_item *last = NULL;
ip_address_item *next;

if (local_interfaces == NULL) return host_find_running_interfaces();

for (s = string_firstinlist(local_interfaces, ':'); s != NULL;
     s = string_nextinlist(':'))
  {
  if (!regexec(regexp_ip_address, s))
    log_write(0, LOG_MAIN|LOG_PANIC_DIE, "Malformed IP address \"%s\" in "
      "local_interfaces", s);

  next = store_malloc(sizeof(ip_address_item));
  next->next = NULL;
  strcpy(next->address, s);

  if (yield == NULL) yield = last = next; else
    {
    last->next = next;
    last = next;
    }

  DEBUG(8) debug_printf("Configured local interface address is %s\n",
    last->address);
  }

return yield;
}





/*************************************************
*       Scan host list for local hosts           *
*************************************************/

/* Scan through a chain of addresses and check whether any of them is the
address of an interface on the local machine. If so, remove that address and
any previous ones with the same MX value, and all subsequent ones (which will
have greater or equal MX values) from the chain. Note: marking them as unusable
is NOT the right thing to do because it causes the hosts not to be used for
other domains, for which they may well be correct.

If the very first host is a local host, then all MX records had a precedence
greater than or equal to that of the local host. Either there's a problem in
the DNS, or an apparently remote name turned out to be an abbreviation for the
local host. Give a specific return code, and let the caller decide what to do.
Otherwise, give a success code if at least one host address has been found.

Arguments:
  host        the first host in the chain
  last        the last host in the chain

Returns:
  HOST_FOUND       if there is at least one host with an IP address on the chain
                     and an MX value less than any MX value associated with the
                     local host
  HOST_FOUND_LOCAL if a local host is among the lowest-numbered MX hosts; when
                     the host addresses were obtained from A records or
                     gethostbyname(), the MX values are set to -1.
  HOST_FIND_FAILED if no valid hosts with set IP addresses were found
*/

int
host_scan_for_local_hosts(host_item *host, host_item *last)
{
int yield = HOST_FIND_FAILED;
host_item *prev = NULL;
host_item *h;

if (local_interface_data == NULL) local_interface_data = host_find_interfaces();

for (h = host; h != last->next; h = h->next)
  {
  if (h->address != NULL)
    {
    ip_address_item *ip;
    for (ip = local_interface_data; ip != NULL; ip = ip->next)
      {
      if (strcmp(h->address, ip->address) == 0)
        {
        if (prev == NULL)
          {
          DEBUG(8) debug_printf((h->mx >= 0)?
            "local host has lowest MX\n" :
            "local host found for non-MX address\n");
          return HOST_FOUND_LOCAL;
          }

        DEBUG(8)
          {
          debug_printf("local host in list - removed hosts:\n");
          for (h = prev->next; h != last->next; h = h->next)
            debug_printf("  %s %s %d\n", h->name, h->address, h->mx);
          }

        prev->next = NULL;
        return yield;
        }
      }

    /* At least one remote address has been found */

    yield = HOST_FOUND;
    }

  /* Update prev to point to the last host item before any that have
  the same MX value as the one we have just considered. */

  if (h->next == NULL || h->next->mx != h->mx) prev = h;
  }

return yield;
}




/*************************************************
*        Find host name given an IP address      *
*************************************************/

/* For the moment, just use gethostbyaddr, and pass back a
single string. Lower case it.

Argument: an IP address in dotted-quad form
Returns:  the name of the host or NULL if not found
*/

char *
host_find_byaddr(char *address)
{
char *s, *t, *yield;
struct hostent *hosts;
struct in_addr addr;
DEBUG(2) debug_printf("host_find_by_addr called\n");
addr.s_addr = (S_ADDR_TYPE)inet_addr(address);
hosts = gethostbyaddr((char *)(&addr), sizeof(addr), AF_INET);
if (hosts == NULL) return NULL;
s = (char *)hosts->h_name;
t = yield = store_malloc((int)strlen(s) + 1);
while (*s != 0) *t++ = tolower(*s++);
*t = 0;
return yield;
}




/*************************************************
*    Find IP address(es) for host by name        *
*************************************************/

/* The input is a host_item structure with the name filled in. We use
gethostbyname(). Of course, gethostbyname() may use the DNS, but it doesn't do
MX processing. If more than one address is given, chain on additional host
items, with other relevant fields copied.

The lookup may result in a change of name. For compatibility with the dns
lookup, return this via fully_qualified_name as well as updating the host item.
The lookup may also yield more than one IP address, in which case chain on
subsequent host_item structures.

Arguments:
  host                   a host item with the name filled in; the address is
                         to be filled in; multiple IP addresses cause other
                         host items to be chained on.
  fully_qualified_name   if not NULL, set to point to host name for
                         compatibility with host_find_bydns

Returns:                 HOST_FIND_FAILED  Failed to find the host or domain
                         HOST_FOUND        Host found
                         HOST_FOUND_LOCAL  Host found and is the local host
*/

int
host_find_byname(host_item *host, char **fully_qualified_name)
{
int yield = HOST_FOUND;
char **addrlist;
host_item *last = host;
struct hostent *hostdata = gethostbyname(host->name);

if (hostdata == NULL || (hostdata->h_addr_list)[0] == NULL)
  return HOST_FIND_FAILED;

/* Replace the name with the fully qualified one if necessary, and fill in
the fully_qualified_name pointer. */

if (strcmp(host->name, hostdata->h_name) != 0)
  host->name = string_copy((char *)hostdata->h_name);
if (fully_qualified_name != NULL) *fully_qualified_name = host->name;

/* We know there is at least one address; use it for the existing
host item block. */

addrlist = hostdata->h_addr_list;
host->address =
  string_copy(inet_ntoa(*((struct in_addr *)(*addrlist++))));
host->mx = -1;
host->status = hstatus_unknown;
host->why = hwhy_unknown;

/* Now add further host item blocks for any other addresses, keeping
the order. */

while (*addrlist != NULL)
  {
  host_item *next = store_malloc(sizeof(host_item));
  next->name = host->name;
  next->ident_string = host->ident_string;
  next->address =
    string_copy(inet_ntoa(*((struct in_addr *)(*addrlist++))));
  next->status = hstatus_unknown;
  next->why = hwhy_unknown;
  next->last_try = 0;
  next->mx = -1;
  next->next = last->next;
  last->next = next;
  last = next;
  }

/* Now check to see if this is the local host. */

yield = host_scan_for_local_hosts(host, last);

DEBUG(8)
  {
  host_item *h;
  if (fully_qualified_name != NULL)
    debug_printf("fully qualified name = %s\n", *fully_qualified_name);
  for (h = host; h != last->next; h = h->next)
    debug_printf("%s %s %d %s\n", h->name,
      (h->address == NULL)? "<null>" : h->address, h->mx,
      (h->status >= hstatus_unusable)? "*" : "");
  }

/* Return the found status. */

return yield;
}



/*************************************************
*       Get next DNS record from answer block    *
*************************************************/

/* Call this with reset == RESET_ANSWERS to scan the answer block, reset ==
RESET_ADDITIONAL to scan the additional records, and reset == RESET_NEXT to get
the next record. The result is in static storage which must be copied if it is
to be preserved.

Argument:   option specifing what portion to scan, as described above
Returns:    next dns record, or NULL when no more
*/

static dns_record *
next_rr(int reset)
{
static int rrcount;
static dns_record rr;
static unsigned char *ptr;

int namelen;
HEADER *h = (HEADER *)dns_answer;


/* Reset the saved data when requested to, and skip to the first required RR */

if (reset != RESET_NEXT)
  {
  rrcount = ntohs(h->qdcount);
  ptr = dns_answer + sizeof(HEADER);

  /* Skip over questions; failure to expand the name just gives up */

  while (rrcount-- > 0)
    {
    namelen = dn_expand(dns_answer, dns_answer + dns_answerlen, ptr,
      (u_char *) &(rr.name), DNS_MAXNAME);
    if (namelen < 0) { rrcount = 0; return NULL; }
    ptr += namelen + 4;    /* skip name & type & class */
    }

  /* Get the number of answer records. */

  rrcount = ntohs(h->ancount);

  /* Skip over answers and NS records if wanting to look at the additional
  records. */

  if (reset == RESET_ADDITIONAL)
    {
    rrcount += ntohs(h->nscount);
    while (rrcount-- > 0)
      {
      namelen = dn_expand(dns_answer, dns_answer + dns_answerlen, ptr,
        (u_char *) &(rr.name), DNS_MAXNAME);
      if (namelen < 0) { rrcount = 0; return NULL; }
      ptr += namelen + 8;     /* skip name & type & class & TTL */
      GETSHORT(rr.size, ptr); /* size of data portion */
      ptr += rr.size;         /* skip over it */
      }
    rrcount = ntohs(h->arcount);
    }
  }


/* The variable "ptr" is now pointing at the next RR, and rrcount contains the
number of RR records left. */

if (rrcount-- <= 0) return NULL;

/* Expand the RR domain name into the slot in the static rr structure; if this
fails, behave as if no more records (something safe). */

namelen = dn_expand(dns_answer, dns_answer + dns_answerlen, ptr,
  (u_char *) &(rr.name), DNS_MAXNAME);
if (namelen < 0) { rrcount = 0; return NULL; }

/* Move the pointer past the name and fill in the rest of the data structure
from the following bytes. */

ptr += namelen;
GETSHORT(rr.type, ptr);        /* Record type */
ptr += 6;                      /* Don't want class or TTL */
GETSHORT(rr.size, ptr);        /* Size of data portion */
rr.data = ptr;                 /* The record's data follows */
ptr += rr.size;                /* Advance to next RR */

/* Return a pointer to the static structure. */

return &rr;
}




/*************************************************
*        Initializer and configure resolver      *
*************************************************/

/* Initialize the resolver and the storage for holding DNS answers if this is
the first time we have been here, and set the resolver options.

Arguments:
  qualify_single    TRUE to set the RES_DEFNAMES option
  search_parents    TRUE to set the RES_DNSRCH option

Returns:            nothing
*/

static void
dns_init(BOOL qualify_single, BOOL search_parents)
{
if (dns_answer == NULL)
  {
  dns_answer = store_malloc(MAXPACKET);
  res_init();
  DEBUG(11) _res.options |= RES_DEBUG;
  }
_res.options &= ~(RES_DNSRCH | RES_DEFNAMES);
_res.options |= (qualify_single? RES_DEFNAMES : 0) |
                (search_parents? RES_DNSRCH : 0);

if (dns_retrans > 0) _res.retrans = dns_retrans;
if (dns_retry > 0) _res.retry = dns_retry;
}




/*************************************************
*              Do basic DNS lookup               *
*************************************************/

/* Call the resolver to look up the given domain name, using the given type,
and check the result.

Arguments:
  name      name to look up
  type      type of DNS record required (T_A, T_MX, etc)

Returns:    DNS_SUCCEED   successful lookup
            DNS_NOMATCH   name not found, or no data found for the given type
            DNS_AGAIN     soft failure, try again later
            DNS_FAIL      DNS failure
*/

static int
dns_basic_lookup(char *name, int type)
{
dns_answerlen = res_search(name, C_IN, type, dns_answer, MAXPACKET);

if (dns_answerlen < 0) switch (h_errno)
  {
  case HOST_NOT_FOUND:
  DEBUG(8) debug_printf("DNS lookup of %s (%s) gave HOST_NOT_FOUND\n",
    name, (type == T_A)? "A" : (type == T_MX)? "MX" : "?");
  return DNS_NOMATCH;

  case TRY_AGAIN:
  DEBUG(8) debug_printf("DNS lookup of %s (%s) gave TRY_AGAIN\n",
    name, (type == T_A)? "A" : (type == T_MX)? "MX" : "?");
  return DNS_AGAIN;

  case NO_RECOVERY:
  DEBUG(8) debug_printf("DNS lookup of %s (%s) gave NO_RECOVERY\n",
    name, (type == T_A)? "A" : (type == T_MX)? "MX" : "?");
  return DNS_FAIL;

  case NO_DATA:
  DEBUG(8) debug_printf("DNS lookup of %s (%s) gave NO_DATA\n",
    name, (type == T_A)? "A" : (type == T_MX)? "MX" : "?");
  return DNS_NOMATCH;

  default:
  DEBUG(8) debug_printf("DNS lookup of %s (%s) gave unknown DNS error %d\n",
    name, (type == T_A)? "A" : (type == T_MX)? "MX" : "?", h_errno);
  return DNS_FAIL;
  }

DEBUG(8) debug_printf("DNS lookup of %s (%s) succeeded\n",
  name, (type == T_A)? "A" : (type == T_MX)? "MX" : "?");

return DNS_SUCCEED;
}




/************************************************
*        Do a DNS lookup and handle CNAMES      *
************************************************/

/* Look up the given domain name, using the given type. Follow CNAMEs if
necessary, but only so many times. There aren't supposed to be CNAME chains in
the DNS, but you are supposed to cope with them if you find them.

The assumption is made that if the resolver gives back records of the requested
type *and* a CNAME, we don't need to make another call to look up the CNAME. I
can't see how it could return only some of the right records. If it's done a
CNAME lookup in the past, it will have all of them; if not, it won't return
any.

If fully_qualified_name is not NULL, set it to point to the full name returned
by the resolver, if this is different to what it is given, unless the returned
name starts with "*" as some nameservers seem to be returning wildcards in this
form.

Arguments:
  name                  domain name to look up
  type                  DNS record type (T_A, T_MX, etc)
  fully_qualified_name  if not NULL, return the returned name here

Returns:                DNS_SUCCEED   successful lookup
                        DNS_NOMATCH   name not found, or no data found
                        DNS_AGAIN     soft failure, try again later
                        DNS_FAIL      DNS failure
*/

static int
dns_lookup(char *name, int type, char **fully_qualified_name)
{
int i;
char *orig_name = name;

/* Loop to follow CNAME chains so far, but no further... */

for (i = 0; i < 10; i++)
  {
  char data[256];
  dns_record *rr, cname_rr, type_rr;
  int datalen, rc;

  /* DNS lookup failures get passed straight back. */

  if ((rc = dns_basic_lookup(name, type)) != DNS_SUCCEED) return rc;

  /* We should have either records of the required type, or a CNAME record,
  or both. We need to know whether both exist for getting the fully qualified
  name, but avoid scanning more than necessary. Note that we must copy the
  contents of any rr blocks returned by next_rr() as they use the same static
  area. */

  cname_rr.data = type_rr.data = NULL;
  for (rr = next_rr(RESET_ANSWERS); rr != NULL; rr = next_rr(RESET_NEXT))
    {
    if (rr->type == type)
      {
      if (type_rr.data == NULL) type_rr = *rr;
      if (cname_rr.data != NULL) break;
      }
    else if (rr->type == T_CNAME) cname_rr = *rr;
    }

  /* If a CNAME was found, take the fully qualified name from it; otherwise
  from the first data record, if present. */

  if (fully_qualified_name != NULL)
    {
    if (cname_rr.data != NULL)
      {
      if (strcmp(cname_rr.name, *fully_qualified_name) != 0 &&
          cname_rr.name[0] != '*')
        *fully_qualified_name = string_copy(cname_rr.name);
      }
    else if (type_rr.data != NULL)
      {
      if (strcmp(type_rr.name, *fully_qualified_name) != 0 &&
          type_rr.name[0] != '*')
        *fully_qualified_name = string_copy(type_rr.name);
      }
    }

  /* If any data records of the correct type were found, we are done. */

  if (type_rr.data != NULL) return DNS_SUCCEED;

  /* If there are no data records, we need to re-scan the DNS using the
  domain given in the CNAME record, which should exist (otherwise we should
  have had a failure from dns_lookup). However code against the possibility of
  its not existing. */

  if (cname_rr.data == NULL) return DNS_FAIL;
  datalen = dn_expand(dns_answer, dns_answer + dns_answerlen,
    cname_rr.data, (u_char *) data, 256);
  if (datalen < 0) return DNS_FAIL;
  name = data;
  }

/*Control reaches here after 10 times round the CNAME loop. Something isn't
right... */

log_write(0, LOG_MAIN, "CNAME loop for %s encountered", orig_name);
return DNS_FAIL;
}





/*************************************************
*        Fill in a host address from the DNS     *
*************************************************/

/* Given a host item, with its name and mx fields set, fill in its IP address
from the DNS. If it is multi-homed, create additional host items for the
additional addresses, copying all the other fields, and randomizing the order.

The host name may be changed if the DNS returns a different name - e.g. fully
qualified or changed via CNAME. If fully_qualified_name is not NULL, dns_lookup
ensures that it points to the fully qualified name. However, this is the fully
qualified version of the original name; if a CNAME is involved, the actual
canonical host name may be different again, and so we get it directly from the
relevant RR. Note that we do NOT change the mx field of the host item in this
function as it may be called to set the addresses of hosts taken from MX
records.

Arguments:
  host                  points to initial host item
  fully_qualified_name  if not NULL, return fully qualified name here

Returns:       HOST_FIND_FAILED     couldn't find A record
               HOST_FIND_AGAIN      try again later
               HOST_FOUND           found A record(s)
*/

static int
dns_set_address(host_item *host, char **fully_qualified_name)
{
dns_record *rr;
host_item *last = NULL;
int rc = dns_lookup(host->name, T_A, fully_qualified_name);

if (rc == DNS_NOMATCH) return HOST_FIND_FAILED;
if (rc == DNS_AGAIN || rc == DNS_FAIL) return HOST_FIND_AGAIN;

/* Fill in the given host item with the first address found; create
additional items for any others. */

for (rr = next_rr(RESET_ANSWERS); rr != NULL; rr = next_rr(RESET_NEXT))
  {
  if (rr->type == T_A)
    {
    char *address = string_sprintf("%d.%d.%d.%d",
      rr->data[0], rr->data[1], rr->data[2], rr->data[3]);

    if (last == NULL)
      {
      if (strcmpic(host->name, rr->name) != 0)
        host->name = string_copy(rr->name);
      host->address = address;
      host->sort_key = host->mx * 100 + random_number(100);
      host->status = hstatus_unknown;
      host->why = hwhy_unknown;
      last = host;
      }
    else
      {
      int new_sort_key = host->mx * 100 + random_number(100);
      host_item *next = store_malloc(sizeof(host_item));

      /* New address goes first: insert the new block after the first one
      (so as not to disturb the original pointer) but put the new address
      in the original block. */

      if (new_sort_key < host->sort_key)
        {
        *next = *host;
        host->next = next;
        host->address = address;
        host->sort_key = new_sort_key;
        if (last == host) last = next;
        }

      /* Otherwise scan down the addresses for this host to find the
      one to insert after. */

      else
        {
        host_item *h = host;
        while (h != last)
          {
          if (new_sort_key < h->next->sort_key) break;
          h = h->next;
          }
        *next = *h;
        h->next = next;
        next->address = address;
        next->sort_key = new_sort_key;
        if (h == last) last = next;      /* Inserted after last */
        }
      }
    }
  }


/* If we found at least one A record, the host->address field will have
been filled in. */

return (host->address == NULL)? HOST_FIND_FAILED : HOST_FOUND;
}




/*************************************************
*  Find IP addresses and names for host via DNS  *
*************************************************/

/* The input is a host_item structure with the name filled in. The lookup may
result in more than one IP address, in which case chain on subsequent host_item
structures. The names of the hosts are in general different to the original
domain name, but in addition, the original name may not be fully qualified.
Use the fully_qualified_name argument to return the official name, as returned
by the resolver.

Arguments:
  host                  point to initial host item
  mx_only               if TRUE, require MX record(s) to exist
  a_only                if TRUE, don't look for MX records
  qualify_single        if TRUE, ask DNS to qualify single-component names
  search_parents        if TRUE, asd DNS to search parent domains
  fully_qualified_name  if not NULL, return fully-qualified name

Returns:                HOST_FIND_FAILED  Failed to find the host or domain
                        HOST_FIND_AGAIN   Could not resolve at this time
                        HOST_FOUND        Host found
                        HOST_FOUND_LOCAL  The lowest MX record points to this
                                          machine, if MX records were found, or
                                          an A record that was found contains
                                          an address of the local host
*/

int
host_find_bydns(host_item *host, BOOL mx_only, BOOL a_only,
  BOOL qualify_single, BOOL search_parents, char **fully_qualified_name)
{
host_item *h, *last;
dns_record *rr;
int rc = DNS_FAIL;
int mx_count, mx_addressed, yield;

/* Set the default fully qualified name to the incoming name, initialize the
resolver if necessary, and set up the relevant options. */

if (fully_qualified_name != NULL) *fully_qualified_name = host->name;
dns_init(qualify_single, search_parents);

/* Search the DNS for MX records, possibly via a CNAME. */

if (!a_only)
  {
  rc = dns_lookup(host->name, T_MX, fully_qualified_name);
  if (rc == DNS_FAIL) return HOST_FIND_FAILED;
  if (rc == DNS_AGAIN) return HOST_FIND_AGAIN;
  }

/* If there were no MX records and mx_only is FALSE, or if a_only is TRUE,
try for an A record. If we find it (or them) check to see that it isn't
the local host. */

if (rc != DNS_SUCCEED)
  {
  if (!mx_only || a_only)
    {
    host_item *next = host->next;
    last = host;
    host->mx = -1;
    rc = dns_set_address(host, fully_qualified_name);

    /* If one or more A records have been found, find the last one and
    check that none of them are local. Since we know the host items all
    have their IP addresses inserted, host_scan_for_local_hosts() can only
    return HOST_FOUND or HOST_FOUND_LOCAL. */

    if (rc == HOST_FOUND)
      {
      while (last->next != next) last = last->next;
      rc = host_scan_for_local_hosts(host, last);
      }

    DEBUG(8)
      {
      host_item *h;
      if (host->address != NULL)
        {
        if (fully_qualified_name != NULL)
          debug_printf("fully qualified name = %s\n", *fully_qualified_name);
        for (h = host; h != next; h = h->next)
          debug_printf("%s %s %d %d %s\n", h->name,
            (h->address == NULL)? "<null>" : h->address, h->mx, h->sort_key,
            (h->status >= hstatus_unusable)? "*" : "");
        }
      }

    return rc;
    }

  DEBUG(8) debug_printf("No MX records, and mx_only is FALSE\n");
  return HOST_FIND_FAILED;
  }

/* We have found one or more MX records. Sort them according to precedence, and
create host_item blocks for them. For equal precedences one is supposed to
randomize the order. To make this happen, the sorting is actually done on the
MX valud * 100 + a random number. This is put into a host field called
sort_key. Remove any duplicates that point to the same host, retaining only the
one with the lowest precedence. We cannot yet check for precedence greater than
that of the local host, because that test cannot be properly done until the
addresses have been found - an MX record may point to a name for this host
which is not the primary hostname. */

last = NULL;
mx_count = mx_addressed = 0;

for (rr = next_rr(RESET_ANSWERS); rr != NULL; rr = next_rr(RESET_NEXT))
  {
  int precedence, datalen;
  unsigned char *s;             /* MUST be unsigned for GETSHORT */
  char data[256];

  if (rr->type != T_MX) continue;
  s = rr->data;
  GETSHORT(precedence, s);

  /* Get the name of the host pointed to. */

  datalen = dn_expand(dns_answer, dns_answer + dns_answerlen, s,
    (u_char *) data, 256);

  /* Check that we haven't already got this host on the chain; if we have,
  keep only the lower precedence. This situation shouldn't occur, but you
  never know what junk might get into the DNS. */

  if (last != NULL) for (h = host; h != last->next; h = h->next)
    {
    if (strcmpic(h->name, data) == 0)
      {
      if (precedence < h->mx) h->mx = precedence;
      goto NEXT_MX_RR;
      }
    }

  /* Count the number of MX hosts we are setting up. */

  mx_count++;

  /* If this is the first MX record, put the data into the existing host
  block. Otherwise, add a new block in the correct place; it it has to be
  before the first block, copy the first block's data to a new second block. */

  if (last == NULL)
    {
    host->name = string_copy(data);
    host->address = NULL;
    host->mx = precedence;
    host->sort_key = precedence * 100 + random_number(100);
    host->status = hstatus_unknown;
    host->why = hwhy_unknown;
    last = host;
    }

  /* Make a new host item and seek the correct insertion place */

  else
    {
    int sort_key = precedence * 100 + random_number(100);

    host_item *next = store_malloc(sizeof(host_item));
    next->name = string_copy(data);
    next->address = NULL;
    next->mx = precedence;
    next->sort_key = sort_key;
    next->status = hstatus_unknown;
    next->why = hwhy_unknown;
    next->last_try = 0;

    /* Handle the case when we have to insert before the first item. */

    if (sort_key < host->sort_key)
      {
      host_item htemp;
      htemp = *host;
      *host = *next;
      *next = htemp;
      host->next = next;
      if (last == host) last = next;
      }

    /* Else scan down the items we have inserted as part of this exercise;
    don't go further. */

    else
      {
      for (h = host; h != last; h = h->next)
        {
        if (sort_key < h->next->sort_key)
          {
          next->next = h->next;
          h->next = next;
          break;
          }
        }

      /* Join on after the last host item that's part of this
      processing if we haven't stopped sooner. */

      if (h == last)
        {
        next->next = last->next;
        last->next = next;
        last = next;
        }
      }
    }

  NEXT_MX_RR: continue;
  }


/* Now we have to ensure addresses exist for all the hosts. We have ensured
above that the names in the host items are all unique. The addresses
may have been returned in the additional data section of the DNS query.
Because it is more expensive to scan the returned DNS records (because you
have to expand the names) we do a single scan over them, and multiple scans
of the chain of host items (which is typically only 3 or 4 long anyway.)
Add extra host items for multi-homed hosts. */

for (rr = next_rr(RESET_ADDITIONAL); rr != NULL; rr = next_rr(RESET_NEXT))
  {
  char s[20];

  if (rr->type != T_A) continue;

  /* Find the first host that matches this record's name. If there isn't
  one, move on to the next RR. */

  for (h = host; h != last->next; h = h->next)
    { if (strcmpic(h->name, rr->name) == 0) break; }
  if (h == last->next) continue;

  /* Set up the textual address for this RR. */

  sprintf(s, "%d.%d.%d.%d", rr->data[0], rr->data[1],
    rr->data[2], rr->data[3]);

  /* If the address is already set for this host, it may be that
  we just have a duplicate A-record. Alternatively, this may be
  a multi-homed host. Search all items with the same host name
  (they will all be together) and if this address is found, skip
  to the next RR. */

  if (h->address != NULL)
    {
    int new_sort_key;
    host_item *thishostlast;
    host_item *hh = h;
    do
      {
      if (hh->address != NULL && strcmp(s, hh->address) == 0) goto NEXT_RR;
      thishostlast = hh;
      hh = hh->next;
      }
    while (hh != last->next && strcmpic(hh->name, rr->name) == 0);

    /* We have a multi-homed host, since we have a new address for
    an existing name. Create a copy of the current item, and give it
    the new address. RRs can be in arbitrary order, but one is supposed
    to randomize the addresses of multi-homed hosts, so compute a new
    sorting key and do that. */

    new_sort_key = h->mx * 100 + random_number(100);
    hh = store_malloc(sizeof(host_item));

    /* New address goes first: insert the new block after the first one
    (so as not to disturb the original pointer) but put the new address
    in the original block. */

    if (new_sort_key < h->sort_key)
      {
      *hh = *h;
      h->next = hh;
      h->address = string_copy(s);
      h->sort_key = new_sort_key;
      }

    /* Otherwise scan down the addresses for this host to find the
    one to insert after. */

    else
      {
      while (h != thishostlast)
        {
        if (new_sort_key < h->next->sort_key) break;
        h = h->next;
        }
      *hh = *h;
      h->next = hh;
      hh->address = string_copy(s);
      hh->sort_key = new_sort_key;
      }

    if (h == last) last = hh;       /* Inserted after last */
    }

  /* The existing item doesn't have its address set yet. Set it, and
  count the number of existing items we have filled in. */

  else
    {
    h->address = string_copy(s);
    mx_addressed++;
    }

  /* Carry on to the next RR. It would be nice to be able to stop when
  mx_addressed >= mx_count, but we can't be sure there won't be an additional
  address for a multi-homed host further down the list. */

  NEXT_RR: continue;
  }

/* Set the default yield to failure */

yield = HOST_FIND_FAILED;

/* If we haven't found all the addresses in the additional section, we
need to search for A records explicitly. The names shouldn't point to
CNAMES, but we use the general lookup function that handles them, just
in case. If any lookup gives a soft error, change the default yield. */

if (mx_addressed < mx_count)
  {
  for (h = host; h != last->next; h = h->next)
    {
    if (h->address != NULL) continue;
    rc = dns_set_address(h, NULL);

    /* If the DNS lookup failed, mark this host item unusable and carry
    on. If no addresses are found, we'll fail below. */

    if (rc != HOST_FOUND)
      {
      h->status = hstatus_unusable;
      if (rc == HOST_FIND_AGAIN)
        {
        yield = rc;
        h->why = hwhy_deferred;
        }
      else h->why = hwhy_failed;
      }
    else mx_addressed++;
    }
  }

/* Scan the list of hosts for any whose IP addresses are on the local host.
If any are found, all hosts with the same or higher MX values are removed.
However, if the local host has the lowest numbered MX, then HOST_FOUND_LOCAL
is returned. Otherwise, if at least one host with an IP address is on the list,
HOST_FOUND is returned. Otherwise, HOST_FIND_FAILED is returned, but in this
case do not update the yield, as it might have been set to HOST_FIND_AGAIN
just above here. If not, it will already be HOST_FIND_FAILED. */

rc = host_scan_for_local_hosts(host, last);
if (rc != HOST_FIND_FAILED) yield = rc;

DEBUG(8)
  {
  if (fully_qualified_name != NULL)
    debug_printf("fully qualified name = %s\n", *fully_qualified_name);
  debug_printf("host_find_bydns yield = %d returned hosts:\n", yield);
  for (h = host; h != last->next; h = h->next)
    debug_printf("  %s %s %d %d %s\n", h->name,
      (h->address == NULL)? "<null>" : h->address, h->mx, h->sort_key,
      (h->status >= hstatus_unusable)? "*" : "");
  }

return yield;
}





/*************************************************
**************************************************
*             Stand-alone test program           *
**************************************************
*************************************************/

#ifdef STAND_ALONE

int main(int argc, char **argv)
{
host_item h;
BOOL byname = FALSE;
BOOL mx_only = FALSE;
BOOL a_only = FALSE;
BOOL qualify_single = TRUE;
BOOL search_parents = TRUE;
char buffer[256];

primary_hostname = "";
debug_level = 8;
debug_file = stdout;

if (argc > 1) primary_hostname = argv[1];

/* So that debug level changes can be done first */
dns_init(qualify_single, search_parents);

printf("> ");
while (fgets(buffer, 256, stdin) != NULL)
  {
  int rc;
  int len = (int)strlen(buffer);
  char *fully_qualified_name;

  while (len > 0 && isspace(buffer[len-1])) len--;
  buffer[len] = 0;

  if (strcmp(buffer, "q") == 0) break;

  if (strcmp(buffer, "byname") ==0) byname = TRUE;
  else if (strcmp(buffer, "no_byname") ==0) byname = FALSE;
  else if (strcmp(buffer, "a_only") == 0) a_only = TRUE;
  else if (strcmp(buffer, "no_a_only") == 0) a_only = FALSE;
  else if (strcmp(buffer, "mx_only") == 0) mx_only = TRUE;
  else if (strcmp(buffer, "no_mx_only") == 0) mx_only = FALSE;
  else if (strcmp(buffer, "qualify_single") == 0) qualify_single = TRUE;
  else if (strcmp(buffer, "no_qualify_single") == 0) qualify_single = FALSE;
  else if (strcmp(buffer, "search_parents") == 0) search_parents = TRUE;
  else if (strcmp(buffer, "no_search_parents") == 0) search_parents = FALSE;
  else if (strncmp(buffer, "retrans", 7) == 0)
    {
    sscanf(buffer+8, "%d", &dns_retrans);
    _res.retrans = dns_retrans;
    }
  else if (strncmp(buffer, "retry", 5) == 0)
    {
    sscanf(buffer+6, "%d", &dns_retry);
    _res.retry = dns_retry;
    }
  else if (isdigit(buffer[0]))
    {
    sscanf(buffer, "%d", &debug_level);
    _res.options &= ~RES_DEBUG;
    DEBUG(11) _res.options |= RES_DEBUG;
    }
  else
    {
    h.name = buffer;
    h.next = NULL;
    h.mx = -1;
    h.status = hstatus_unknown;
    h.why = hwhy_unknown;
    h.address = "";

    rc = byname? host_find_byname(&h, &fully_qualified_name) :
      host_find_bydns(&h, mx_only, a_only, qualify_single, search_parents,
        &fully_qualified_name);

    if (rc == HOST_FIND_FAILED) printf("Failed\n");
      else if (rc == HOST_FIND_AGAIN) printf("Again\n");
        else if (rc == HOST_FOUND_LOCAL) printf("Local\n");
    }

  printf("\n> ");
  }
printf("\n");
}
#endif

/* End of host.c */
