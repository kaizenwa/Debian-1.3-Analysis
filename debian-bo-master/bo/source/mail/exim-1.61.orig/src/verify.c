/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Functions concerned with verifying things. */


#include "exim.h"



/*************************************************
*           Local static variables               *
*************************************************/


static char ident_buffer[128];
static BOOL sender_ok;
static BOOL sender_is_local;



/*************************************************
*            Verify an email address             *
*************************************************/

/* The local flag indicates whether an unqualified address is acceptable or
not. The local_domain flag is set TRUE if the address turns out to be in a
local domain, FALSE if it is in a remote domain. If the file is NULL, don't
print anything. If newaddress is not null, return the address, possibly
modified by the routing process. The yield is OK, FAIL, or DEFER. If debugging,
output fuller information, provided there is an output file. If log_details is
TRUE, write details of failures to the reject log. If address_test is true,
don't skip directors and routers that have no_verify set - we are doing a test
rather than a verify.

Arguments:
  s               address to verify
  is_recipient    TRUE if this is a recipient address; FALSE if a sender - this
                    affects qualification and rewriting
  local           if TRUE, an unqualified address; otherwise give an error
  f               if not NULL, write the result to this file
  local_domain    set TRUE if address turns out to be in a local domain
  newaddress      if not NULL, return the top-level address, possibly modified
  log_details     if TRUE, write details of failures to the reject log
  address_test    testing address (-bt) rather than verifying

Returns:          OK
                  FAIL
                  DEFER
*/

int
verify_address(char *s, BOOL is_recipient, BOOL local, FILE *f,
  BOOL *local_domain, char **newaddress, BOOL log_details, BOOL address_test)
{
int start, end, domain;
BOOL full_info = (f == NULL)? FALSE : debug_level > 0;
BOOL allok = TRUE;
int verify_type = address_test? v_none : is_recipient? v_recipient : v_sender;
address_item *addr_orig;
address_item *addr_new;
address_item *addr_remote = NULL;
address_item *addr_local = NULL;
address_item *addr_succeed = NULL;

char *errmess;
char *receiver =
  parse_extract_address(s, &errmess, &start, &end, &domain, FALSE);

if (receiver == NULL)
  {
  if (f != NULL) fprintf(f, "exim: %s - bad address: %s\n", s, errmess);
  if (log_details)
    log_write(0, LOG_REJECT, "%s - bad address: %s", s, errmess);
  return FAIL;
  }

/* Add qualify domain if permitted. */

if (parse_find_at(receiver) == NULL)
  {
  if (!local)
    {
    if (f != NULL) fprintf(f, "A domain is required for \"%s\"\n", receiver);
    if (log_details)
      log_write(0, LOG_REJECT, "a domain is required for \"%s\"", receiver);
    return FAIL;
    }
  receiver = rewrite_address_qualify(receiver, is_recipient);
  }

/* Rewrite and report on it. It is guaranteed that the address of the new
string will be different to the old, even though the old one is freed in the
process. */

if (rewrite_rules != NULL)
  {
  char *old = receiver;
  receiver = rewrite_address(receiver, is_recipient, FALSE);
  if (receiver != old)
    {
    if (f != NULL) fprintf(f, "Address rewritten as: %s\n", receiver);
    if (log_details)
      log_write(0, LOG_REJECT, "%s rewritten as: %s", old, receiver);
    }
  }

/* Set up an initial address structure. */

addr_new = addr_orig = deliver_make_addr(receiver);

/* We need a loop, since a directed address might generate a number of new
addresses. We must also cope with generated pipes and files at the top
level. (See also the code/comment in deliver.c.) However, it is usually
the case that the forwardfile director has its verify flag turned off.

The loop is used after directing, however, only when the verify_actions flag is
set, and this can only be set locally. Remote enquiries just get information
about the top level address, not anything that it generated.

In the case of a router discovering that an apparently remote address is in
fact local, the loop is always re-run. */

while (addr_new != NULL)
  {
  int rc;
  address_item *addr = addr_new;
  addr_new = addr->next;
  addr->next = NULL;

  /* Handle generated pipe, file or reply addresses */

  if (addr->pfr)
    {
    allok = FALSE;
    if (addr->orig[0] == '|')
      {
      if (f != NULL) fprintf(f, "%s -> %s %s\n", addr->parent->orig,
        addr->orig, addr->allow_pipe? "" : "*** forbidden ***");
      continue;
      }
    else if (addr->orig[0] == '/')
      {
      if (f != NULL) fprintf(f, "%s -> %s %s\n", addr->parent->orig,
        addr->orig, addr->allow_file? "" : "*** forbidden ***");
      continue;
      }
    else if (addr->orig[0] == '>')
      {
      if (f != NULL) fprintf(f, "%s -> mail %s %s\n", addr->parent->orig,
        addr->orig+1, addr->allow_reply? "" : "*** forbidden ***");
      continue;
      }
    }

  /* All addresses should either have been made fully qualified above,
  or been qualified when generated by a director, so panic if we find
  an unqualified one. */

  if (parse_find_at(addr->orig) == NULL)
    log_write(0, LOG_PANIC_DIE, "Unqualified address found: %s", addr->orig);

  /* Determine locality - this sets "local_part", "domain", and "local"
  fields. Pass back the locality if a variable is supplied. */

  deliver_setlocal(addr);

  if (local_domain != NULL) *local_domain = addr->local;

  /* DEBUG and/or log_details: show what's been done to this address */

  DEBUG(7)
    {
    debug_printf("address %s\n", addr->orig);
    debug_printf("  local_part=%s domain=%s local=%d\n",
      addr->local_part, addr->domain, addr->local);
    }

  if (log_details)
    log_write(0, LOG_REJECT, "%s: local part = %s domain = %s local = %s",
      addr->orig, addr->local_part, addr->domain,
      (addr->local)? "true":"false");

  /* Handle a local address with the directors, or a remote address with the
  routers, and output or return the result except when full_info is set, in
  which case continue for other (generated) addresses. When full_info is set,
  f will not be NULL. Don't output anything for success in that case at this
  stage. Note that a director may set up local or remote delivery. */

  rc = (addr->local)?
    direct_address(addr, &addr_local, &addr_remote, &addr_new, &addr_succeed,
      verify_type) :
    route_address(addr, &addr_local, &addr_remote, &addr_new, verify_type);

  /* If a remote address turned out to be local after all, set it up for
    reprocessing and restart the loop. */

  if (rc == ISLOCAL)
    {
    if (log_details)
      log_write(0, LOG_REJECT, "routing %s caused it to become local",
        addr->orig);
    if (addr->local_part[0] == ',' || addr->local_part[0] == ':')
      addr->orig = string_sprintf("@%s%s", addr->domain, addr->local_part);
    else
      addr->orig = string_sprintf("%s@%s", addr->local_part, addr->domain);
    addr->next = addr_new;
    addr_new = addr;
    continue;
    }

  /* Handle hard failures */

  if (rc == FAIL)
    {
    allok = FALSE;
    if (f != NULL) fprintf(f, "%s %s:\n  %s%s%s\n",
      addr->orig,
      address_test? "is undeliverable" : "failed to verify",
      (addr->basic_errno <= 0)? "" : strerror(addr->basic_errno),
      (addr->basic_errno <= 0)? "" : ": ",
      (addr->message != NULL)? addr->message :
        (addr->basic_errno <= 0)? "unknown error" : "");

    if (log_details) log_write(0, LOG_REJECT,
      "%s is undeliverable: %s%s%s", addr->orig,
      (addr->basic_errno <= 0)? "" : strerror(addr->basic_errno),
      (addr->basic_errno <= 0)? "" : ": ",
      (addr->message != NULL)? addr->message :
        (addr->basic_errno <= 0)? "unknown error" : "");

    if (!full_info) return FAIL;
    }

  /* If the yield is ERROR or PANIC, there has been some cock-up in the
  directors or routers. This doesn't really mean the address is undeliverable
  or unverifyable, so we treat it the same as DEFER. */

  else if (rc == DEFER || rc == ERROR || rc == PANIC)
    {
    allok = FALSE;
    if (f != NULL)
      fprintf(f, "%s cannot be resolved at this time:\n  %s%s%s\n",
      addr->orig,
      (addr->basic_errno <= 0)? "" : strerror(addr->basic_errno),
      (addr->basic_errno <= 0)? "" : ": ",
      (addr->message != NULL)? addr->message :
        (addr->basic_errno <= 0)? "unknown error" : "");

    if (log_details)
      log_write(0, LOG_REJECT, "%s cannot be resolved at this time: %s%s%s",
      addr->orig,
      (addr->basic_errno <= 0)? "" : strerror(addr->basic_errno),
      (addr->basic_errno <= 0)? "" : ": ",
      (addr->message != NULL)? addr->message :
        (addr->basic_errno <= 0)? "unknown error" : "");

    if (!full_info) return DEFER;
    }

  /* Handle successful routing or directing when short info wanted */

  else if (!full_info)
    {
    if (f != NULL) fprintf(f, "%s %s\n",
      addr->orig,
      address_test? "is deliverable" : "verified");
    if (newaddress != NULL)
      {
      *newaddress = (addr->local_part[0] == ',' || addr->local_part[0] == ':')?
         string_sprintf("@%s%s", addr->domain, addr->local_part) :
         string_sprintf("%s@%s", addr->local_part, addr->domain);
      }
    return OK;
    }
  }

/* Display the full results of the successful directing and routing,
including any generated addresses. Control gets here only when full_info is
set, which requires f not to be NULL, and this occurs only when a top-level
verify is called with the debugging switch on. For verification, if the
debugging value is greater than 1, show everything; for 1 (which is also -v)
show what "the normal user" might be interested in. For address testing,
always show everything.

If there are no local and no remote addresses, and there were no pipes, files,
or autoreplies, and there were no errors or deferments, the message is to be
discarded, usually because of the use of :blackhole: in an alias file. */

if (allok && addr_local == NULL && addr_remote == NULL)
  fprintf(f, "mail to %s is discarded\n", s);

full_info = debug_level > 1 || address_test;

while (addr_local != NULL)
  {
  address_item *addr = addr_local;
  address_item *p = addr->parent;
  fprintf(f, "%s", addr->orig);
  while (p != NULL)
    {
    fprintf(f, "\n    <-- %s", p->orig);
    p = p->parent;
    }
  fprintf(f, "\n  local delivery to %s in domain %s\n",
    addr->local_part, addr->domain);
  if (full_info)
    {
    if (addr->director != NULL) fprintf(f, "  director = %s, ",
      addr->director->name);
    if (addr->router != NULL) fprintf(f, "  router = %s, ", addr->router->name);
    fprintf(f, "transport = %s\n", addr->transport->name);
    }
  addr_local = addr->next;
  }

while (addr_remote != NULL)
  {
  address_item *addr = addr_remote;
  address_item *p = addr->parent;

  fprintf(f, "%s", addr->orig);
  while (p != NULL)
    {
    fprintf(f, "\n    <-- %s", p->orig);
    p = p->parent;
    }

  /* Show director or router, and transport */

  if (addr->director != NULL)
    {
    fprintf(f, "\n  remote delivery to %s in domain %s\n",
      addr->local_part, addr->domain);
    if (full_info) fprintf(f, "  director = %s, ", addr->director->name);
    }

  else if (addr->router != NULL)
    {
    if (addr->local_part[0] == ',' || addr->local_part[0] == ':')
      fprintf(f, "\n  remote delivery to @%s%s", addr->domain,
      addr->local_part);
    else
      fprintf(f, "\n  remote delivery to %s@%s", addr->local_part,
      addr->domain);

    if (strcmp(addr->domain, addr->route_domain) != 0)
      fprintf(f, " via domain %s", addr->route_domain);
    fprintf(f, "\n");

    if (full_info) fprintf(f, "  router = %s, ", addr->router->name);
    }

  else if (full_info) fprintf(f, "  director/router unset, ");

  if (full_info)
    fprintf(f, "transport = %s\n", (addr->transport == NULL)? "unset" :
      addr->transport->name);

  /* Show any hosts that are set up; fiddle a bit to get a nice format */

  if (addr->host_list != NULL)
    {
    host_item *h;
    int maxlen = 0;
    int maxaddlen = 0;
    for (h = addr->host_list; h != NULL; h = h->next)
      {
      int len = (int)strlen(h->name);
      if (len > maxlen) maxlen = len;
      if (h->address != NULL)
        {
        len = (int)strlen(h->address);
        if (len > maxaddlen) maxaddlen = len;
        }
      }
    for (h = addr->host_list; h != NULL; h = h->next)
      {
      int len = (int)strlen(h->name);
      fprintf(f, "  host %s ", h->name);
      while (len++ < maxlen) fprintf(f, " ");
      if (h->address != NULL)
        {
        fprintf(f, "[%s] ", h->address);
        len = (int)strlen(h->address);
        while (len++ < maxaddlen) fprintf(f," ");
        }
      if (h->mx >= 0) fprintf(f, "MX=%d", h->mx);
      fprintf(f, "\n");
      }
    }
  addr_remote = addr->next;
  }

return OK;  /* This value doesn't actually matter */
}





/*************************************************
*          Setup host list matching data         *
*************************************************/

/* This function is called for a number of parameters containing lists
of strings of the form [ident@]host, to scan the strings it contains and build
a chain of control blocks of the broken-down or compiled data for subsequent
use. The daemon calls this at its start if it is listening for SMTP calls. It
is also called from verify_sender_preliminary for other kinds of SMTP input.

Arguments:
  list      colon-separated list of items
  anchor    where to chain the created blocks

Returns:    nothing
*/

void
verify_setup_hostlist(char *list, host_item **anchor)
{
char *s, *t;

DEBUG(2) debug_printf("verify_setup_hostlist called\n");
if (*anchor != NULL) return;                  /* Just in case */

for (s = string_firstinlist(list, ':'); s != NULL;
     s = string_nextinlist(':'))
  {
  host_item *h = store_malloc(sizeof(host_item));
  h->next = NULL;
  *anchor = h;
  anchor = &(h->next);

  /* Make a permanent copy of the string. */

  s = string_copy(s);

  /* If there's an '@' in the string, it starts with an ident string;
  carve that off. */

  if ((t = strchr(s, '@')) != NULL)
    {
    *t = 0;
    h->ident_string = s;
    s = t+1;
    }
  else h->ident_string = NULL;

  h->name = s;
  h->compiled_name = NULL;
  h->address = NULL;

  /* If the name is of the form of an IP address, we can set the address
  field immediately. */

  if (regexec(regexp_ip_address, h->name)) h->address = h->name;

  /* Else if the name is not wildcarded (starting with * or ^) or a file lookup
  (contains ';') then we can look up the IP address(es) once and for all.
  Ignore failure - this will just cause slower reverse lookups to happen later.
  Move the anchor pointer on to the end for multi-homed hosts. */

  else if (s[0] != '*' && s[0] != '^' && strchr(s, ';') == NULL)
    {
    char *fully_qualified_name;
    if (host_find_byname(h, &fully_qualified_name) != HOST_FOUND)
      h->address = NULL;
    while (*anchor != NULL) anchor = &((*anchor)->next);  /* Multihomed */
    }
  }
}




/*************************************************
*         Check host+ident is in list            *
*************************************************/

/* This function is called from a number of places to test whether the current
calling host (plus ident) is in a list of hosts + idents.

Arguments:
  list       host list for checking
  anchor     anchor for broken down data (may already be done if via daemon)

Returns:     TRUE if current host+ident is in the list
*/

BOOL
verify_check_host(char *list, host_item **anchor)
{
host_item *h;
char *name = NULL;

/* We can't do anything if the calling host's address is not available or if
the list is empty. */

if (sender_host_address == NULL || list == NULL) return FALSE;

/* Initialize the munged data if not already done so (for calls via the
daemon it should already be set up). */

if (*anchor == NULL) verify_setup_hostlist(list, anchor);

/* Scan each listed item, checking the ident string if present, and then
checking the address. For single names, the address will be in the host item.
If the address isn't set in the host item, we have to match on the textual name
of the host, which should be wildcarded. This requires a reverse lookup of the
sender address. */

for (h = *anchor; h != NULL; h = h->next)
  {
  char *ident;
  BOOL match;

  /* Handle ident checking. If the test string starts with \ just ignore that
  character; otherwise if it starts with ! the test is negated. */

  if ((ident = h->ident_string) != NULL)
    {
    match = TRUE;
    if (sender_ident == NULL) continue;
    if (*ident == '\\') ident++;
      else if (*ident == '!') { ident++; match = FALSE; }
    if ((strcmp(ident, sender_ident) == 0) != match) continue;
    }

  /* If we have the IP address already, test that. */

  if (h->address != NULL)
    {
    if (strcmp(h->address, sender_host_address) == 0) return TRUE;
      else continue;
    }

  /* Otherwise match on the name. Optimise for the special case of "*" */

  if (strcmp(h->name, "*") == 0) return TRUE;

  if (name == NULL) name = host_find_byaddr(sender_host_address);
  if (name != NULL)
    {
    BOOL string_matches =
      match_check_string(name, h->name, &(h->compiled_name), -1, TRUE);
    if (string_matches)
      {
      store_free(name);
      return TRUE;
      }
    }
  }

if (name != NULL) store_free(name);
return FALSE;
}




/*************************************************
*          Setup net list matching data          *
*************************************************/

/* This function is called for a number of parameters containing lists
of strings of the form <ip-address>/<mask>, to scan the strings it contains and
build a chain of control blocks of the broken-down or compiled data for
subsequent use. The daemon calls this at its start if it is listening for SMTP
calls.

Arguments:
  list      colon-separated list of items
  anchor    where to chain the created blocks

Returns:    nothing
*/

void
verify_setup_netlist(char *list, ip_net_item **anchor)
{
char *s;

DEBUG(2) debug_printf("verify_setup_netlist called\n");
if (*anchor != NULL) return;                  /* Just in case */

for (s = string_firstinlist(list, ':'); s != NULL;
     s = string_nextinlist(':'))
  {
  int x[8];
  ip_net_item *n = store_malloc(sizeof(ip_net_item));
  n->next = NULL;
  *anchor = n;
  anchor = &(n->next);

  /* The strings were syntax-checked at read-in time */

  sscanf(s, "%d.%d.%d.%d/%d.%d.%d.%d", x, x+1, x+2, x+3, x+4, x+5, x+6, x+7);

  n->mask = (x[4] << 24) + (x[5] << 16) + (x[6] << 8) + x[7];
  n->address = ((x[0] << 24) + (x[1] << 16) + (x[2] << 8) + x[3]) & n->mask;
  }
}



/*************************************************
*         Check address is in net                *
*************************************************/

/* This function is called from a number of places to test whether the IP
address of the current calling host is in a list of networks.

Arguments:
  list       net list for checking
  anchor     anchor for broken down data (may already be done if via daemon)

Returns:     TRUE if IP address of current host is in the list
*/

BOOL
verify_check_net(char *list, ip_net_item **anchor)
{
int x[4];
IP_ADDRESS ipadd;
ip_net_item *n;

/* We can't do anything if the calling host's address is not available, or if
the list is empty. */

if (sender_host_address == NULL || list == NULL) return FALSE;

/* Initialize the munged data if not already done so (for calls via the
daemon it should already be set up. */

if (*anchor == NULL) verify_setup_netlist(list, anchor);

/* Convert the textual form of the host address to binary. We do this by
steam so that its the same as when we read the list, thus avoiding any
net/host ordering problems. */

sscanf(sender_host_address, "%d.%d.%d.%d", x, x+1, x+2, x+3);
ipadd = (x[0] << 24) + (x[1] << 16) + (x[2] << 8) + x[3];

/* Scan each listed item. */

for (n = *anchor; n != NULL; n = n->next)
  {
  if ((ipadd & n->mask) == n->address) return TRUE;
  }

return FALSE;
}




/*************************************************
*            Verify the sender of a message      *
*************************************************/

/* The next two functions operate in tandem, and the second must not be
called without first calling the first one. The reason for this approach is
that some SMTP mailers treat any error returned after the data has been
transmitted as temporary (contrary to RFC821) and keep retrying, even after
they have been sent a 5xx error at the previous attempt. To get round this,
exim keeps a database of failed messages and their hosts, and if the same bad
address is received from the same host soon afterwards, it is rejected at the
preliminary stage (meaning after MAIL FROM for SMTP) in the hope that the far
end might now give up.

The reason for not rejecting at this stage in all cases is that remote
postmasters, when told their systems have been sending out bad messages, always
ask "what were the headers?" and so one needs to have read them in order to log
them. This also helps track down mail forgers. It also makes it possible to
replace bad envelope sender addresses with good ones from inside the message if
that option is configured.

September 1996: Some mailers keep on trying even after getting a 5xx error for
MAIL FROM. If the same bad address is received from the same host for a third
time in a short time, MAIL FROM is accepted, but refuse_all_rcpts is set, and
all subsequent RCPT TO commands get rejected with a 550.

The RFCs imply that the final local-part@domain of a route address should be
intelligible to all parties. It is unfortunately the case that some mailers
abuse this and supply final domain addresses that are meaningful only to
them.

As we are interested only in whether we can route back to this address, we
don't worry about this case, but a possible upgrade would be to make checking
the final address a configurable option. That would then catch

  @valid.domain:junk@junk.domain

which at present gets through. Later: If the new collapse_source_routes option
is set, source routes are collapsed during parsing, so won't appear here, which
is in effect the upgrade mentioned. */




/*************************************************
*        First check on sender address           *
*************************************************/

/* This function is called as soon as a sender address has been received
from an SMTP connection. Unless the host is in the list of those from which no
verification is required, it verifies the address. If it is bad and
sender_verify_reject is FALSE, it gives an OK response with a warning message.

When sender_verify_reject is TRUE and a bad address is received, it checks to
see if the same address has recently been rejected. If not, it sets a flag for
verify_sender() to interrogate. If it has been rejected once recently, an error
return is given for MAIL FROM. If it has been rejected more than once recently,
then MAIL FROM is accepted, but a flag is set to cause all subsequent RCPT TO
commands to be rejected.

Arguments:
  errcode    set this to an SMTP error code on failure
  errmess    set this to point to an error message on failure

Returns:     TRUE if address verified, or did not fail recently, or host
               is in the exception list, i.e. if is OK to proceed
*/

BOOL
verify_sender_preliminary(int *errcode, char **errmess)
{
char *newaddr;
EXIM_DB *dbmfile;
db_reject *reject;
int rejectlen;
char buffer[SENDER_ADDRESS_MAXLENGTH + 256];

/* If the sender address is empty, it's an error message with, in effect,
no sender, and we can't check anything. */

if (sender_address[0] == 0)
  {
  sender_ok = OK;
  return TRUE;
  }

/* See if this is one of the trusted hosts/identd combinations or nets
for which we accept all addresses. If so, do no further checking. */

if (verify_check_host(sender_verify_except_hosts,
      &sender_verify_except_hostlist) ||
    verify_check_net(sender_verify_except_nets,
      &sender_verify_except_netlist))
  {
  DEBUG(2) debug_printf("matched in sender_verify_except list\n");
  sender_ok = OK;
  return TRUE;
  }

/* Run a verification on the address. */

sender_ok =
  verify_address(sender_address, FALSE, FALSE, NULL, &sender_is_local, &newaddr,
    sender_verify_log_details, FALSE);

/* After a successful return, the address may have been changed (typically
a domain will be canonicized or expanded by a router). A new copy is always
given. */

if (sender_ok == OK)
  {
  DEBUG(2) debug_printf("%s verified ok as %s\n", sender_address, newaddr);
  store_free(sender_address);
  sender_address = newaddr;
  return TRUE;
  }

/* Defer is usually a DNS time out. If try_verify switch is set, which means
accept if soft error, accept it with an appropriate message. Otherwise, pass it
on, leaving sender_ok set to DEFER, leaving possible (temporary) rejection to
the second verification function. */

if (sender_ok == DEFER)
  {
  if (sender_try_verify)
    {
    DEBUG (2) debug_printf("%s verification deferred: accepted unverified\n");
    *errcode = 250;
    *errmess = "warning: temporarily unable to resolve sender address: "
      "accepted unverified";
    if (sender_verify_log_details)
      log_write(0, LOG_REJECT, "%s verification deferred but sender_try_verify "
        "is true", sender_address);
    sender_ok = OK;
    }
  return TRUE;
  }

/* Otherwise it's a hard failure. If rejection is not required, accept with
a warning. */

if (!sender_verify_reject)
  {
  *errcode = 250;
  *errmess = sender_is_local?
    "warning: unknown local-part in sender address" :
    "warning: cannot route to sender address";

  if (sender_verify_log_details)
    log_write(0, LOG_REJECT, "%s failed verification but sender_verify_reject "
      "is false", sender_address);

  sender_ok = OK;
  return TRUE;
  }

/* See if this address from this host has recently been rejected. If it has
not, pass now, leaving rejection to the second function, which will create
a DBM entry if necessary (it might not be necessary if the sender is fixed
up from the headers). */

dbmfile = db_open("reject", O_RDWR);
if (dbmfile == NULL) return TRUE;

sprintf(buffer, "%s:%s", sender_address,
  (sender_host_name != NULL)? sender_host_name :
  (sender_host_address != NULL)? sender_host_address : "");
reject = db_read_with_length(dbmfile, buffer, &rejectlen);

if (reject == NULL || time(NULL) - reject->time_stamp > 24*60*60)
  {
  db_close(dbmfile);
  return TRUE;
  }

/* There's been a recent rejection. If there has only been one, reject now
(i.e. reject the MAIL FROM) and up the count. For compatibility with older
versions of Exim that didn't have the count, inspect the length of the
record. This change made in September 1996; after a few months this could
be removed. */

if (rejectlen < sizeof(db_reject) || !reject->rejected_mail_from)
  {
  db_reject newreject;
  *errcode = 501;
  *errmess = sender_is_local?
    "unknown local-part in sender" : "cannot route to sender";
  newreject.rejected_mail_from = TRUE;
  db_write(dbmfile, buffer, &newreject, sizeof(db_reject));
  db_close(dbmfile);
  DEBUG(2) debug_printf("%s verification failed after MAIL FROM\n",
    sender_address);
  return FALSE;
  }

/* There has been a previous recent rejection after MAIL FROM; the mailer
at the far end is horribly broken. Allow through this MAIL FROM with warning
text, but set refuse_all_rcpts to cause all RCPT TO commands to be failed
with 550 - which seems to be the only thing some mailers understand. */

db_close(dbmfile);
refuse_all_rcpts = TRUE;
*errcode = 250;
*errmess = "reject all recipients: 3 times bad sender";
DEBUG(2) debug_printf("%s verification failed - will reject all recipients",
  sender_address);
return TRUE;
}




/*************************************************
*        Second check on sender address          *
*************************************************/


/* This function is called when a message has been completely read, but the
headers haven't yet been written to the spool file, if the sender_verify
option is set. The sender check actually took place in the preliminary
function; its result is left in sender_ok. If it is bad, it may (depending on
the configuration) be permitted to replace it with a value taken from one of
the headers (From, Sender) if that address is viable.

Arguments:
  errcode    set this to an SMTP error code on failure
  errmess    set this to point to an error message on failure

Returns:     TRUE if address verified or fixed up, FALSE otherwise
*/

BOOL
verify_sender(int *errcode, char **errmess)
{
EXIM_DB *dbmfile;
db_reject reject;
char buffer[256];

/* Sender verified OK at preliminary check. */

if (sender_ok == OK) return TRUE;

/* If configured, have a look at the headers and if there is a valid Sender,
Reply-to or From header, then use that address instead of the broken envelope
sender. Just look at the first address in the header. Insert some X- headers to
record what was done. This facility has some blessing from RFC 822:

    o   The "Sender" field mailbox should be sent  notices  of
        any  problems in transport or delivery of the original
        messages.  If there is no  "Sender"  field,  then  the
        "From" field mailbox should be used.

    o   If the "Reply-To" field exists, then the reply  should
        go to the addresses indicated in that field and not to
        the address(es) indicated in the "From" field.
*/

if (sender_verify_fixup)
  {
  header_line *h;
  header_line *sender = NULL;
  header_line *from = NULL;
  header_line *reply_to = NULL;

  for (h = header_list; h != NULL; h = h->next)
    {
    if (h->type == htype_from) from = h;
    else if (h->type == htype_replyto) reply_to = h;
    else if (h->type == htype_sender) sender = h;
    }

  h = (sender != NULL)? sender : (reply_to != NULL)? reply_to : from;

  if (h != NULL)
    {
    int terminator, new_ok;
    char *ss, *newaddr;
    char *s = strchr(h->text, ':') + 1;

    while (isspace(*s)) s++;
    ss = parse_find_address_end(s, FALSE);
    terminator = *ss;
    *ss = 0;
    new_ok =
      verify_address(s, FALSE, FALSE, NULL, &sender_is_local, &newaddr,
        sender_verify_log_details, FALSE);
    *ss = terminator;

    if (new_ok == OK)
      {
      DEBUG(2) debug_printf("%s (taken from header) verified ok as %s\n"
        "  used instead of envelope sender\n", sender_address, newaddr);
      log_write(0, LOG_MAIN, "return-path %s rewritten as %s using %s",
        sender_address,
        newaddr,
        (h->type == htype_replyto)? "Reply-To" :
        (h->type == htype_sender)? "Sender" : "From");
      header_add(htype_other,
        "X-BadReturnPath: %s rewritten as %s\n  using \"%s\" header\n",
        sender_address,
        newaddr,
        (h->type == htype_replyto)? "Reply-To" :
        (h->type == htype_sender)? "Sender" : "From");
      sender_address = newaddr;
      return TRUE;
      }
    }
  }

/* If the original verification attempt deferred, give a temporary error
return. */

if (sender_ok == DEFER)
  {
  DEBUG(2) debug_printf("%s verification deferred\n", sender_address);
  *errcode = 451;
  *errmess = "temporarily unable to verify sender address (try again later)";
  return FALSE;
  }

/* Otherwise it's a hard failure. Update the database to record this rejection.
O_RDWR (rather than O_WRONLY) is needed by Berkeley native DB. If opening
fails, don't worry. */

dbmfile = db_open("reject", O_RDWR|O_CREAT);
if (dbmfile != NULL)
  {
  sprintf(buffer, "%s:%s", sender_address,
    (sender_host_name != NULL)? sender_host_name :
    (sender_host_address != NULL)? sender_host_address : "");
  reject.rejected_mail_from = FALSE;
  db_write(dbmfile, buffer, &reject, sizeof(db_reject));
  db_close(dbmfile);
  }

/* Now give a hard error */

*errcode = 554;
*errmess = sender_is_local?
  "unknown local-part in sender" : "cannot route to sender";

DEBUG(2) debug_printf("%s verification failed after data\n", sender_address);
return FALSE;
}





/*************************************************
*            Get RFC 1413 identification         *
*************************************************/

/* Attempt to get an id from the sending machine via the RFC 1413 protocol. If
the timeout is set to zero, then the query is not done. There may also be lists
of hosts and nets which are exempt. We copy as many bytes from the result as
will fit into ident_buffer and then free the store that ident_id has got
(several K, I think). To guard against malefactors sending non-printing
characters which could, for example, disrupt a message's headers, make sure the
string consists of printing characters only.

Argument: the socket of the connection for which the ident value is required
Returns:  nothing

Side effect: any received ident value is put in sender_ident (NULL otherwise)
*/

void
verify_get_ident(int socket)
{
sender_ident = NULL;
if (rfc1413_query_timeout > 0 &&
    (rfc1413_except_hosts == NULL ||
      !verify_check_host(rfc1413_except_hosts, &rfc1413_except_hostlist)) &&
    (rfc1413_except_nets == NULL ||
      !verify_check_net(rfc1413_except_nets, &rfc1413_except_netlist)))
  {
  char *ident_ptr = ident_id(socket, rfc1413_query_timeout);
  if (ident_ptr != NULL)
    {
    strncpy(ident_buffer, ident_ptr, sizeof(ident_buffer));
    ident_buffer[sizeof(ident_buffer) - 1] = 0;
    sender_ident = string_printing(ident_buffer, FALSE);
    /* NB: free() not store_free() as it wasn't got by store_malloc() */
    free(ident_ptr);
    }
  }
}

/* End of verify.c */
