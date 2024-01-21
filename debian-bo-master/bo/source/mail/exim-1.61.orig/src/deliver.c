/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* The main code for delivering a message. */


#include "exim.h"


/* Data block for keeping track of subprocesses for parallel remote
delivery. */

typedef struct pardata {
  address_item *addrlist;      /* chain of addresses */
  pid_t pid;                   /* subprocess pid */
  int fd;                      /* pipe fd for getting result from subprocess */
} pardata;



/*************************************************
*            Local static variables              *
*************************************************/

static address_item *addr_defer = NULL;
static address_item *addr_direct = NULL;
static address_item *addr_duplicate = NULL;
static address_item *addr_failed = NULL;
static address_item *addr_local = NULL;
static address_item *addr_new = NULL;
static address_item *addr_remote = NULL;
static address_item *addr_route = NULL;
static address_item *addr_succeed = NULL;

static BOOL update_spool;
static BOOL remove_journal;
static int  parcount = 0;
static pardata *parlist = NULL;
static int  return_count;


/* Table for turning base-62 numbers into binary */

static char tab62[] =
          {0,1,2,3,4,5,6,7,8,9,0,0,0,0,0,0,     /* 0-9 */
           0,10,11,12,13,14,15,16,17,18,19,20,  /* A-K */
          21,22,23,24,25,26,27,28,29,30,31,32,  /* L-W */
          33,34,35, 0, 0, 0, 0, 0,              /* X-Z */
           0,36,37,38,39,40,41,42,43,44,45,46,  /* a-k */
          47,48,49,50,51,52,53,54,55,56,57,58,  /* l-w */
          59,60,61};                            /* x-z */

/* Default data for newly created address */

static address_item default_addr = {
  NULL,                 /* next */
  NULL,                 /* parent */
  NULL,                 /* orig */
  NULL,                 /* unique */
  NULL,                 /* local_part */
  NULL,                 /* prefix */
  NULL,                 /* suffix */
  NULL,                 /* domain */
  NULL,                 /* route_domain */
  NULL,                 /* errors_address */
  FALSE,                /* pfr */
  FALSE,                /* rewrite_headers */
  FALSE,                /* local */
  FALSE,                /* delivered */
  TRUE,                 /* routed_by_domain */
  FALSE,                /* ignore_error */
  NULL,                 /* extra_headers */
  NULL,                 /* director */
  NULL,                 /* router */
  NULL,                 /* transport */
  NULL,                 /* host_list */
  NULL,                 /* transported */
  0,                    /* child_count */
  -1,                   /* uid */
  -1,                   /* gid */
  FALSE,                /* uid_set */
  FALSE,                /* gid_set */
  -1,                   /* mode */
  NULL,                 /* home_dir */
  NULL,                 /* current_dir */
  NULL,                 /* route_option */
  FALSE,                /* initgroups */
  FALSE,                /* expand_pipe */
  FALSE,                /* allow_pipe */
  FALSE,                /* allow_file */
  FALSE,                /* allow_reply */
  DEFER,                /* transport_return */
  ERRNO_UNKNOWNERROR,   /* errno */
  0,                    /* more_errno */
  NULL,                 /* message */
  SPECIAL_NONE,         /* special_action */
  FALSE,                /* dr_retry_exists */
  FALSE,                /* retry_skipped */
  FALSE,                /* retry_timedout */
  NULL,                 /* retries */
  NULL,                 /* return_filename */
  -1,                   /* return_file */
  NULL                  /* reply */
};



/*************************************************
*             Make a new address item            *
*************************************************/

/* This function gets the store and initializes with default values. The
transport_return value defaults to DEFER, so that any unexpected failure to
deliver does not wipe out the message.

Argument:
  address     the RFC822 address string

Returns:      a pointer to an initialized address_item
*/

address_item *
deliver_make_addr(char *address)
{
address_item *addr = store_malloc(sizeof(address_item));
*addr = default_addr;
addr->orig = address;
addr->unique = addr->orig;
return addr;
}




/*************************************************
*              Compare lists of hosts            *
*************************************************/

/* This function is given two pointers to chains of host items, and it yields
TRUE if the lists refer to the same hosts in the same order, except that
multiple hosts with the same non-negative MX values are permitted to appear in
different orders. Round-robinning nameservers can cause this to happen.

This enables exim to use a single SMTP transaction for sending to two entirely
different domains that happen to end up pointing at the same hosts. For
identical domains, the two pointers may well be the same, as the routing is
normally just copied.

Arguments:
  one       points to the first host list
  two       points to the second host list

Returns:    TRUE if the lists refer to the same host set
*/

static BOOL
same_hosts(host_item *one, host_item *two)
{
if (one == two) return TRUE;

while (one != NULL && two != NULL)
  {
  if (strcmp(one->name, two->name) != 0)
    {
    int mx = one->mx;
    host_item *end_one = one;
    host_item *end_two = two;

    /* Batch up only if MX >= 0 */

    if (mx < 0) return FALSE;

    /* Find the ends of the shortest sequence of identical MX values */

    while (end_one->next != NULL && end_one->next->mx == mx &&
           end_two->next != NULL && end_two->next->mx == mx)
      {
      end_one = end_one->next;
      end_two = end_two->next;
      }

    /* If there aren't any duplicates, there's no match. */

    if (end_one == one) return FALSE;

    /* For each host in the 'one' sequence, check that it appears in the 'two'
    sequence, returning FALSE if not. */

    for (;;)
      {
      host_item *hi;
      for (hi = two; hi != end_two->next; hi = hi->next)
        if (strcmp(one->name, hi->name) == 0) break;
      if (hi == end_two->next) return FALSE;
      if (one == end_one) break;
      one = one->next;
      }

    /* All the hosts in the 'one' sequence were found in the 'two' sequence.
    Ensure both are pointing at the last host, and carry on as for equality. */

    two = end_two;
    }

  /* Hosts matched */

  one = one->next;
  two = two->next;
  }

/* True if both are NULL */

return (one == two);
}



/*************************************************
*          Determine locality of an address      *
*************************************************/

/* This function determines whether an address is local or not, and sets the
"local" field in the address accordingly. It also sets the "local_part" and
"domain" fields, and lowercases appropriate parts of the address.

Argument:
  addr      points to an addr_item block containing the original address
            in its orig field

Returns:    nothing
*/

void
deliver_setlocal(address_item *addr)
{
char *domain = parse_find_at(addr->orig);
char *active = addr->orig;

/* If this is a source-routed address, strip off any local domains
at the start, and point to the rest as the current active address. If
we can determine that delivery is not local, unset the local flag and
set up the local part and the domain. If route stripping gets us to the
final address (after ':') check that it has a domain attached. If it
doesn't, turn it into domain form by attaching the final domain removed.
Actually, I don't think this case can occur as it will have been rejected
by the parser as a bad address, but leave the code in for safety. */

addr->local = TRUE;
while (*active == '@')
  {
  char *p = active;
  while (*(++p) != ',' && *p != ':');
  domain = string_copynlc(active+1, p-active-1);

  /* The source route points at a local domain */

  if (match_isinlist(domain, local_domains, &re_local_domains, TRUE))
    {
    if (*p == ':')
      {
      char *newdomain;
      if ((newdomain = strchr(p+1, '@')) == NULL)
        {
        int n = (int)strlen(p+1);
        char *newactive =
          store_malloc((int)strlen(domain) + n + 2);
        sprintf(newactive, "%s@%s", p+1, domain);
        active = newactive;
        domain = active + n;
        }
      else
        {
        active = p+1;
        domain = newdomain;
        }
      }
    else active = p+1;
    }

  /* It's a remote, source-routed address. In this case the "local_part"
  is the remainder of the address, identifiable as such because it begins
  with a comma or a colon. */

  else
    {
    addr->local_part = p;
    addr->domain = domain;
    addr->local = FALSE;
    return;
    }
  }


/* Deal with a non-source-routed address (which might be the final component of
a local source-routed address). If it is local, we must be prepared to do the
"percent hack" for configured domains.

On reaching this point, "active" points to the operative part of the address,
and the '@' in the address is pointed to by "domain". Both these are pointers
inside the "orig" address, and so the data they point to must be copied
so that it can be modified if necessary.

On most Unix systems, local parts should be treated caselessly, but there is an
option not to do this. Domains are always caseless. */

addr->domain = string_copylc(domain+1);

/* The address is a local domain */

if (match_isinlist(addr->domain, local_domains, &re_local_domains, TRUE))
  {
  if (locally_caseless)
    addr->local_part = string_copynlc(active, domain-active);
  else
    addr->local_part = string_copyn(active, domain-active);

  /* We do the percent hack only for those domains that are listed in
  percent_hack_domains. A loop is needed to handle the case of multiple
  local %-hacks. */

  while (match_isinlist(addr->domain, percent_hack_domains,
         &re_percent_hack_domains, TRUE))
    {
    char *pc = strrchr(addr->local_part, '%');
    if (pc == NULL) break;

    /* Handle a % hack by making a copy of the new domain and then removing
    it from the local_part field. If the new domain is remote, we are done;
    otherwise let the loop continue to check for more % hacks. */

    addr->domain = string_copy(pc+1);
    *pc = 0;

    if (!match_isinlist(addr->domain, local_domains, &re_local_domains, TRUE))
      {
      addr->local = FALSE;
      break;
      }
    }
  }

/* The address refers to a remote domain. Don't mess with the case
of the local part. */

else
  {
  addr->local_part = string_copyn(active, domain-active);
  addr->local = FALSE;
  }
}




/*************************************************
*     Set expansion values for an address        *
*************************************************/

/* Certain expansion variables are valid only when handling an address.
This function sets them up or clears them, according to its argument.

Argument:
  addr         the address in question, or NULL to clear values
Returns:       nothing
*/

void
deliver_set_expansions(address_item *addr)
{
address_item *addr_orig;

if (addr == NULL)
  {
  deliver_domain = NULL;
  deliver_domain_orig = NULL;
  deliver_localpart = NULL;
  deliver_localpart_orig = NULL;
  deliver_localpart_prefix = NULL;
  deliver_localpart_suffix = NULL;
  deliver_recipients = NULL;
  route_option = NULL;
  deliver_host = NULL;              /* get set individually */
  deliver_home = NULL;              /* but can be cleared here */
  return;
  }

/* Find the original address */

for (addr_orig = addr; addr_orig->parent != NULL;
  addr_orig = addr_orig->parent);

/* Things that are always set */

deliver_recipients = addr;
route_option = addr->route_option;
deliver_host = (addr->host_list == NULL)? "" : addr->host_list->name;

/* If only one address there is no complication */

if (addr->next == NULL)
  {
  deliver_domain = addr->domain;
  deliver_localpart = addr->local_part;
  deliver_localpart_prefix = addr->prefix;
  deliver_localpart_suffix = addr->suffix;

  /* If delivering to a pipe or file, or sending an autoreply, get the local
  part from the parent if there is a parent. */

  if (addr->pfr && addr->parent != NULL)
    {
    deliver_localpart = addr->parent->local_part;
    deliver_localpart_prefix = addr->parent->prefix;
    deliver_localpart_suffix = addr->parent->suffix;
    }

  deliver_domain_orig = addr_orig->domain;
  deliver_localpart_orig = addr_orig->local_part;
  }

/* For multiple addresses, don't set local part, and set the domain only
if it is the same for all of them. */

else
  {
  address_item *addr2;
  for (addr2 = addr->next; addr2 != NULL; addr2 = addr2->next)
    {
    if (strcmp(addr->domain, addr2->domain) != 0) return;
    }
  deliver_domain = addr->domain;
  }
}




/*************************************************
*           Perform a local delivery             *
*************************************************/

/* Each local delivery is performed in a separate process which sets its
uid and gid as specified. This is a safer way than simply changing and
restoring using seteuid; there is a body of opinion that seteuid cannot be
used safely. Besides, not all systems have seteuid.

If the security level is 1 or 3 this function will be running sete{g,u}id to
the exim uid and gid, and must use sete{g,u}id to regain root privilege before
using set{g,u}id. Otherwise it is running as root.

If the uid/gid are specified in the transport_instance, they are used; the
transport initialization must ensure that either both or neither are set.
Otherwise, the values associated with the address are used. If neither are set,
it is a configuration error.

The transport or the address may specify a home directory (transport over-
rides), and if they do, this is set as $home and as the working directory.
Otherwise $home is left unset and the cwd is set to "/" - a directory that
should be accessible to all users.

Using a separate process makes it more complicated to get error information
back. We use a pipe to pass the return code and also an error code and error
text string back to the parent process.

Arguments:
  addr       points to an address block for this delivery; for "normal" local
             deliveries this is the only address to be delivered, but for
             pseudo-remote deliveries (e.g. by batch SMTP to a file or pipe)
             a number of addresses can be handled simultaneously, and in this
             case addr will point to a chain of addresses with the same
             characteristics.

Returns:     nothing
*/

static void
deliver_local(address_item *addr)
{
BOOL uid_set = FALSE;
BOOL gid_set = FALSE;
BOOL use_initgroups;
uid_t uid;
gid_t gid;
int status, len, rc;
int pfd[2];
pid_t pid;
char *working_directory;
address_item *addr2;
transport_instance *tp = addr->transport;
char buffer[256];

/* If transport not set or not local, panic */

if (tp == NULL)
  log_write(0, LOG_PANIC_DIE, "No transport set for local delivery of %s",
    addr->orig);
if (!tp->info->local)
  log_write(0, LOG_PANIC_DIE, "Attempt local delivery with non-local "
  "transport for %s: transport %s", addr->orig, tp->name);

/* Set up variables that are relevant to a single delivery, for use
in various expansions and the environment of scripts. Need to set them as
early as this in case an expansion is required to get a uid or gid or home. */

deliver_set_expansions(addr);

/* Get the uid and gid to use. First see if the transport either set explicit
values, or set the deliver_as_creator flag. It should not be possible for both
to be set. Take the use_initgroups flag from the transport if using uid/gid
from there, otherwise from the address. */

use_initgroups = tp->initgroups;

/* First see if there's a gid on the transport. Exim panics if no gid can be
found from an expanded string. */

if (tp->gid_set)
  {
  gid = tp->gid;
  gid_set = TRUE;
  }
else if (tp->expand_gid != NULL)
  {
  direct_find_expanded_group(tp->expand_gid, tp->name, "transport", &gid);
  gid_set = TRUE;
  }

/* Now pick up a uid from the transport if one is set. */

if (tp->uid_set)
  {
  uid = tp->uid;
  uid_set = TRUE;
  }

/* Try for an expandable uid field. If it ends up as a numeric id, it does
not provide a passwd value from which a gid can be taken. Exim panics if
no user can be found from an expanded string. */

else if (tp->expand_uid != NULL)
  {
  struct passwd *pw;
  direct_find_expanded_user(tp->expand_uid, tp->name, "transport", &pw, &uid);
  uid_set = TRUE;
  if (!gid_set && pw != NULL)
    {
    gid = pw->pw_gid;
    gid_set = TRUE;
    }
  }

/* Otherwise, test the deliver_creator flag. */

else if (tp->deliver_as_creator)
  {
  uid = originator_uid;
  if (!gid_set) gid = originator_gid;
  uid_set = gid_set = TRUE;
  }

/* If the transport didn't specify anything, then the address must. Note
that the address may have come via a router, in which case the only place
a uid/gid can be set is on the transport. */

else
  {
  if (!addr->uid_set)
    {
    if (addr->director != NULL)
      log_write(0, LOG_MAIN|LOG_PANIC_DIE, "Neither the %s director nor the %s "
        "transport set a uid for local delivery of %s", addr->director->name,
        tp->name, addr->orig);
    else
      log_write(0, LOG_MAIN|LOG_PANIC_DIE, "The %s transport has not set a "
        "uid for local delivery of %s", tp->name, addr->orig);
    }
  uid = addr->uid;
  if (!gid_set) gid = addr->gid;
  uid_set = gid_set = TRUE;
  use_initgroups = addr->initgroups;
  }

/* If no gid has been set by this time, it is a disaster. */

if (!gid_set)
  log_write(0, LOG_MAIN|LOG_PANIC_DIE, "User set without group for %s transport",
    tp->name);

/* See if the uid is on the list of banned uids for local delivery, and if
so, substitute the nobody user if configured. If there is no nobody, default to
"nobody", and if that isn't available, give up. */

if (never_users != NULL)
  {
  int i;
  uid_t *nn = never_users;
  for (i = 1; i <= (int)(nn[0]); i++) if (nn[i] == uid)
    {
    if (!nobody_uid_set)
      {
      struct passwd *pw;
      if (direct_finduser("nobody", &pw, NULL))
        {
        nobody_uid = pw->pw_uid;
        nobody_gid = pw->pw_gid;
        nobody_uid_set = nobody_gid_set = TRUE;
        }
      }
    if (nobody_uid_set)
      {
      uid = nobody_uid;
      gid = nobody_gid;
      }
    else
      log_write(0, LOG_PANIC_DIE, "Uid %d is not permitted to deliver, but "
        "there is no \"nobody\" user specified", (int)uid);
    }
  }

/* See if either the transport or the address specifies a home (current)
directory. Expand it if necessary. If nothing is set, use "/", which is
assumed to be a directory to which all users have access. It is necessary
to be in a visible directory for some operating systems when running pipes,
as some commands (e.g. "rm" under Solaris 2.5) require this. */

deliver_home = (tp->home_dir != NULL)? tp->home_dir :
               (addr->home_dir != NULL)? addr->home_dir : NULL;

if (deliver_home != NULL)
  {
  char *rawhome = deliver_home;
  deliver_home = expand_string(rawhome);
  if (deliver_home == NULL)
    log_write(0, LOG_MAIN|LOG_PANIC_DIE,
      "home directory string %s failed to expand: %s",
      rawhome, expand_string_message);
  if (*deliver_home != '/')
    log_write(0, LOG_MAIN|LOG_PANIC_DIE,
    "home directory path %s is not absolute",
    deliver_home);
  }

/* If an explicit current directory was set by the address or transport,
expand and use it; otherwise default to the home directory, if any; otherwise
default to "/". */

working_directory =
  (tp->current_dir != NULL)? tp->current_dir :
  (addr->current_dir != NULL)? addr->current_dir : NULL;

if (working_directory != NULL)
  {
  char *raw = working_directory;
  working_directory = expand_string(raw);
  if (working_directory == NULL)
    log_write(0, LOG_MAIN|LOG_PANIC_DIE,
      "home directory string %s failed to expand: %s",
      raw, expand_string_message);
  if (*working_directory != '/')
    log_write(0, LOG_MAIN|LOG_PANIC_DIE,
      "current directory path %s is not absolute", working_directory);
  }
else working_directory = (deliver_home == NULL)? "/" : deliver_home;

DEBUG(2) debug_printf("delivering %s as %s using %s:\n  "
  "uid=%d gid=%d home=%s current=%s\n",
  addr->orig, addr->local_part, tp->name, (int)uid, (int)gid,
  (deliver_home == NULL)? "null" : deliver_home, working_directory);

/* If one of the return_output flags is set on the transport, create and open a
file in the message log directory for the transport to write its output onto.
This is mainly used by pipe transports. The file needs to be unique to the
address. */

if (tp->return_output || tp->return_fail_output ||
    tp->log_output || tp->log_fail_output)
  {
  addr->return_filename =
    string_sprintf("%s/msglog/%s-%d-%d", spool_directory, message_id, getpid(),
      return_count++);
  addr->return_file =
    open(addr->return_filename, O_WRONLY|O_CREAT|O_APPEND, 0400);
  if (addr->return_file < 0)
    log_write(0, LOG_PANIC_DIE, "Unable to create file for %s transport to "
      "return message: %s", tp->name, strerror(errno));

  /* If there is an exim uid and we are currently root, the file must be
  changed so that exim is the owner, because that will be the uid when it
  comes to be read. This happens when security = setuid (i.e. seteuid not
  being used). */

  if (exim_uid_set && geteuid() == root_uid &&
    chown(addr->return_filename, exim_uid, exim_gid) < 0)
      log_write(0, LOG_PANIC_DIE, "Unable to chmod return message file for %s "
        "transport: %s", tp->name, strerror(errno));
  }

/* Create the pipe for inter-process communication. */

if (pipe(pfd) != 0)
  log_write(0, LOG_PANIC_DIE, "Creation of pipe failed: %s", strerror(errno));

/* Now fork the process to do the real work in the sub-process. */

if ((pid = fork()) == 0)
  {
  /* Prevent core dumps, as we don't want them in users' home directories.
  HP-UX doesn't have RLIMIT_CORE; I don't know how to do this in that
  system. */

  #ifdef RLIMIT_CORE
  struct rlimit rl;
  rl.rlim_cur = 0;
  rl.rlim_max = 0;
  if (setrlimit(RLIMIT_CORE, &rl) < 0)
    log_write(0, LOG_MAIN|LOG_PANIC, "setrlimit failed: %s", strerror(errno));
  #endif

  /* If the transport has a setup entry, call this first, while still
  privileged. (Appendfile uses this to expand quota, for example, while
  able to read private files.) */

  #ifdef TRANSPORT_DEBUG
  if (debug_transport == NULL)
  #endif

  if (addr->transport->setup != NULL)
    {
    addr->message = (addr->transport->setup)(addr->transport);
    if (addr->message != NULL)
      {
      addr->transport_return = PANIC;
      goto PASS_BACK;
      }
    }

  /* Ignore SIGINT and SIGTERM during delivery. Also ignore SIGUSR1, as
  when the process becomes unprivileged, it won't be able to write to the
  process log. SIGHUP is ignored throughout exim, except when it is being
  run as a daemon. */

  signal(SIGINT, SIG_IGN);
  signal(SIGTERM, SIG_IGN);
  signal(SIGUSR1, SIG_IGN);

  /* Close the unwanted half of the pipe, and set the required gid/uid, first
  regaining root privilege if necessary. Note that mac_sete{g,u}id expands
  to a failure on systems without the sete{g,u}id functions, but on such
  systems we should always be running as root here.

  If debug_transport is not NULL, do not change uid and gid here, since
  we want to continue running as root or exim in order to write to the
  debugging output file. */

  close(pfd[pipe_read]);

  #ifdef TRANSPORT_DEBUG
  if (debug_transport == NULL)
  #endif

  if (geteuid() != root_uid && mac_seteuid(root_uid) < 0)
    log_write(0, LOG_PANIC_DIE, "Unable to get root to set uid and gid for "
      "local delivery to %s", addr->local_part);

  /* Ignore initgroups if unable to get a passwd entry for the uid, or if
  initgroups fails. At least one OS returns +1 for initgroups failure, so
  just check for non-zero. */

  else
    {
    if (use_initgroups)
      {
      struct passwd *pw = getpwuid(uid);
      if (pw != NULL)
        {
        if (initgroups(pw->pw_name, gid) != 0)
          DEBUG(2) debug_printf("initgroups failed for uid=%d: %s\n", (int)uid,
            strerror(errno));
        }
      else DEBUG(2) debug_printf("no passwd entry for uid = %d: initgroups "
        "ignored\n", (int)uid);
      }

    if (setgid(gid) < 0 || setuid(uid) < 0)
        log_write(0, LOG_PANIC_DIE, "Unable to set uid=%d or gid=%d for local "
          "delivery to %s", (int)uid, (int)gid, addr->local_part);
    }

  if (chdir(working_directory) < 0)
    {
    addr->transport_return = DEFER;
    addr->basic_errno = errno;
    addr->message = string_sprintf("failed to chdir to %s", working_directory);
    }

  else
    {
    DEBUG(2) debug_printf("Local delivery process %d uid=%d gid=%d\n",
      (int)getpid(), (int)geteuid(), (int)getegid());
    set_process_info("delivering %s to %s using %s", message_id,
     addr->local_part, addr->transport->name);

    /* Now call the transport, write any error codes, special action and message
    down the pipe, and terminate the process successfully. */

    #ifdef TRANSPORT_DEBUG
    if (debug_transport != NULL)
      (debug_transport->code)(addr->transport, addr);
    else
    #endif

    (addr->transport->info->code)(addr->transport, addr);
    }

  /* Pass the result back down the pipe. The label is the subject of a goto
  when a call to the transport's setup function fails. */

  PASS_BACK:
  write(pfd[pipe_write], (void *)&(addr->transport_return), sizeof(int));
  write(pfd[pipe_write], (void *)&(addr->basic_errno), sizeof(int));
  write(pfd[pipe_write], (void *)&(addr->more_errno), sizeof(int));
  write(pfd[pipe_write], (void *)&(addr->special_action), sizeof(int));
  if (addr->message != NULL)
    write(pfd[pipe_write], addr->message, (int)strlen(addr->message) + 1);
  close(pfd[pipe_write]);
  exit(EXIT_SUCCESS);
  }

/* Panic if the fork did not succeed. */

if (pid <= 0)
  log_write(0, LOG_MAIN|LOG_PANIC_DIE, "Fork failed for local delivery to %s",
    addr->orig);

/* Read the pipe to get the delivery status codes and a possible error message.
Our copy of the writing end must be closed first, as otherwise read() won't
return zero on an empty pipe. Afterwards, close the reading end. */

close(pfd[pipe_write]);
len = read(pfd[pipe_read], (void *)&status, sizeof(int));
if (len > 0)
  {
  addr->transport_return = status;
  len = read(pfd[pipe_read], (void *)&(addr->basic_errno), sizeof(int));
  len = read(pfd[pipe_read], (void *)&(addr->more_errno), sizeof(int));
  len = read(pfd[pipe_read], (void *)&(addr->special_action), sizeof(int));
  len = read(pfd[pipe_read], (void *)buffer, sizeof(buffer));
  if (len > 0) addr->message = string_copy(buffer);
  }
close(pfd[pipe_read]);

/* If the delivery succeeded, write all the addresses immediately to the
journal file, to ensure is is recorded asap. For local addresses, the
unique field contains the lower cased form of the local part (when
locally_caseless is set - the usual case), and this is the main thing
not to deliver to again. Failure to write the journal is panic-worthy,
but don't stop, as it may prove possible subsequently to update the spool
file in order to record the delivery. */

if (addr->transport_return == OK)
  {
  for (addr2 = addr; addr2 != NULL; addr2 = addr2->next)
    {
    int len;
    sprintf(big_buffer, "%c%s\n", addr->pfr? 'Y':'N', addr2->unique);
    len = (int)strlen(big_buffer);
    if (write(journal_fd, big_buffer, len) != len)
      log_write(0, LOG_MAIN|LOG_PANIC, "failed to update journal for %s: %s",
        addr2->unique, strerror(errno));
    }
  }

/* Ensure the journal file is pushed out to disc. */

if (fsync(journal_fd) < 0)
  log_write(0, LOG_MAIN|LOG_PANIC, "failed to fsync journal: %s",
    strerror(errno));

/* Reset variables that are relevant to a single delivery. */

deliver_set_expansions(NULL);

/* Wait for the process to finish. If it terminates with a non-zero code,
freeze the message. Take care to handle the case when the subprocess doesn't
seem to exist. This has been seen on one system when Exim was called from an
MUA that set SIGCHLD to SIG_IGN. When that happens, wait() doesn't recognize
the termination of child processes. Exim now resets SIGCHLD to SIG_DFL, but
this code should still be robust. If the return status is set to OK we assume
the message was delivered by the subprocess, as it wrote this status down the
pipe. */

while ((rc = wait(&status)) != pid)
  {
  if (rc < 0 && errno == ECHILD)      /* Process has vanished */
    {
    if (addr->transport_return == DEFER && addr->message == NULL)
      addr->message =
        string_sprintf("%s transport process vanished unexpectedly",
          addr->transport->driver_name);
    log_write(0, LOG_MAIN, "%s transport process vanished unexpectedly",
      addr->transport->driver_name);
    status = 0;
    break;
    }
  }

if ((status & 0xffff) != 0)
  {
  addr->transport_return = DEFER;
  addr->special_action = SPECIAL_FREEZE;
  addr->message =
    string_sprintf("%s transport process returned a non-zero status (0x%04x)",
      addr->transport->driver_name, status);
  }

/* If more than one address was involved (batched SMTP or other batching), copy
the fields into all the others. */

for (addr2 = addr->next; addr2 != NULL; addr2 = addr2->next)
  {
  addr2->transport_return = addr->transport_return;
  addr2->basic_errno = addr->basic_errno;
  addr2->more_errno = addr->more_errno;
  addr2->special_action = addr->special_action;
  addr2->message = addr->message;
  }
}



/*************************************************
*      Decrease counts in parents and mark done  *
*************************************************/

/* This function is called when an address is complete. The chain of parents is
scanned, and the count of children of each parent is decremented. If it becomes
zero for any parent, that address is added to the non-recipients tree because
it is complete. We must also scan the duplicates address list for such an
address and mark any duplicates as complete as well by recursive call to this
function.

Arguments:
  addr      points to the completed address item
  now       the current time as a string, for writing to the message log

Returns:    a pointer to the address item for the original ancestor.
*/

static address_item *
child_done(address_item *addr, char *now)
{
int decrement = 1;
while (addr->parent != NULL)
  {
  addr = addr->parent;
  if ((addr->child_count -= decrement) <= 0)
    {
    address_item *dup;
    tree_add_nonrecipient(addr->orig, addr->pfr);
    update_spool = TRUE;
    fprintf(message_log, "%s %s: children all complete\n", now, addr->orig);
    fflush(message_log);
    for (dup = addr_duplicate; dup != NULL; dup = dup->next)
      {
      if (strcmp(addr->unique, dup->unique) == 0)
        {
        tree_add_nonrecipient(dup->orig, dup->pfr);
        (void)child_done(dup, now);
        }
      }
    }
  else decrement = 0;
  }
return addr;
}




/*************************************************
*    Actions at the end of handling an address   *
*************************************************/

/* This is a function for processing a single address when all that can be done
with it has been done.

Arguments:
  addr         points to the address block
  result       the result of the delivery attempt
  logflags     flags for log_write() (LOG_MAIN and/or LOG_PANIC or zero)
  driver_type  indicates which type of driver (transport, director, or
               router) was last to process the address
  logchar      '=' or '-' for use when logging deliveries with => or ->

Returns:       the result of the delivery attempt, which might get changed
               to FAIL when there is generated output to return
*/

static int
post_process_one(address_item *addr, int result, int logflags, int driver_type,
  int logchar)
{
char *now = tod_stamp(tod_log);
char *driver_kind = "?";
char *driver_name = "";

DEBUG(9) debug_printf("post-process %s\n", addr->orig);

/* Set up driver kind and name for logging */

if (driver_type == DTYPE_TRANSPORT)
  {
  if (addr->transport != NULL)
    {
    driver_name = addr->transport->name;
    driver_kind = " transport";
    }
  else driver_kind = "transporting";
  }
else if (driver_type == DTYPE_DIRECTOR)
  {
  if (addr->director != NULL)
    {
    driver_name = addr->director->name;
    driver_kind = " director";
    }
  else driver_kind = "directing";
  }
else if (driver_type == DTYPE_ROUTER)
  {
  if (addr->router != NULL)
    {
    driver_name = addr->router->name;
    driver_kind = " router";
    }
  else driver_kind = "routing";
  }

/* If there's an error message set, ensure that it contains only printing
characters - it should, but occasionally things slip in and this at least
stops the log format from getting wrecked. */

if (addr->message != NULL)
  addr->message = string_printing(addr->message, FALSE);

/* If we used a transport that has one of the "return_output" options set, and
if it did in fact generate some output, then for return_output we treat the
message as failed if it was not already set that way, so that the output gets
returned to the sender, provided there is a sender to send it to. For
return_fail_output do this only if the delivery failed. Otherwise we just
unlink the file, and remove the name so that if the delivery failed, we don't
try to send back an empty or unwanted file. The log_output options operate only
on a non-empty file.

In any case, we close the message file, because we cannot afford to leave a
file-descriptor for one address while processing (maybe very many) others. */

if (addr->return_file >= 0)
  {
  BOOL return_output = FALSE;
  struct stat statbuf;
  fsync(addr->return_file);

  /* If there is no output, do nothing. */

  if (fstat(addr->return_file, &statbuf) == 0 && statbuf.st_size > 0)
    {
    transport_instance *tb = addr->transport;

    /* Handle logging options */

    if (tb->log_output || (result == FAIL && tb->log_fail_output))
      {
      char *s;
      FILE *f = fopen(addr->return_filename, "r");
      if (f == NULL)
        log_write(0, LOG_MAIN|LOG_PANIC, "failed to open %s to log output "
          "from %s transport: %s", addr->return_filename, tb->name,
          strerror(errno));
      else
        {
        s = fgets(big_buffer, big_buffer_size, f);
        if (s != NULL)
          {
          char *p = big_buffer + (int)strlen(big_buffer);
          while (p > big_buffer && isspace(p[-1])) p--;
          *p = 0;
          s = string_printing(big_buffer, FALSE);
          log_write(0, LOG_MAIN, "<%s>: %s transport output: %s",
            addr->orig, tb->name, s);
          if (s != big_buffer) store_free(s);
          }
        fclose(f);
        }
      }

    /* Handle returning options, but only if there is an address
    to return the text to. */

    if (sender_address[0] != 0 || addr->errors_address != NULL)
      {
      if (tb->return_output)
        {
        result = FAIL;
        if (addr->basic_errno == 0 && addr->message == NULL)
          addr->message = "return message generated";
        return_output = TRUE;
        }
      else if (result == FAIL && tb->return_fail_output) return_output = TRUE;
      }
    }

  /* Get rid of the file unless is is to be returned, but close it in
  all cases. */

  if (!return_output)
    {
    unlink(addr->return_filename);
    addr->return_filename = NULL;
    }

  close(addr->return_file);
  }

/* The sucess case happens only after delivery by a transport. */

if (result == OK)
  {
  address_item *topaddr, *dup;
  addr->next = addr_succeed;
  addr_succeed = addr;

  /* Don't deliver to this address again. For local addresses, the unique
  field contains the lower cased form of the local part. However, we do need
  also to have the original form of the address updated in the spool file so
  that listings of the spool mark the address as delivered. */

  DEBUG(9) debug_printf("%s succeeded: adding to nonrecipients list\n",
    addr->orig);

  tree_add_nonrecipient(addr->unique, addr->pfr);
  if (addr->local && addr->parent == NULL)
    tree_add_nonrecipient(addr->orig, addr->pfr);

  /* Check the list of duplicate addresses and ensure they are now marked
  done as well. Also their parents. */

  for (dup = addr_duplicate; dup != NULL; dup = dup->next)
    {
    if (strcmp(addr->unique, dup->unique) == 0)
      {
      tree_add_nonrecipient(dup->orig, dup->pfr);
      (void)child_done(dup, now);
      }
    }

  /* Ensure the header file gets re-written */

  update_spool = TRUE;

  /* Update the message log */

  if (addr->parent == NULL)
    fprintf(message_log, "%s %s: %s%s succeeded\n", now, addr->orig,
      driver_name, driver_kind);
  else
    fprintf(message_log, "%s %s <%s>: %s%s succeeded\n", now, addr->orig,
      addr->parent->orig, driver_name, driver_kind);
  fflush(message_log);

  /* Crawl back up the parents chain, decreasing the counts, and handling
  any that are complete. */

  topaddr = child_done(addr, now);

  /* Log the delivery. Local deliveries must have come via a director, but code
  carefully to avoid crashing if data is missing. Remote deliveries can come
  via a director or via a router, and should always have a pointer to
  the host item that succeeded. */

  if (addr->transport->info->local)
    {
    BOOL show_parent = addr->pfr && addr->parent != topaddr;
    log_write(0, LOG_MAIN, "%c> %s%s%s%s <%s> D=%s T=%s",
      logchar,
      addr->local_part,
      show_parent? " (" : "",
      show_parent? addr->parent->orig : "",
      show_parent? ")" : "",
      topaddr->orig,
     (addr->director == NULL)? "" : addr->director->name,
      addr->transport->name);
    }

  /* Remote delivery */

  else
    {
    char buffer[256];
    char *confirmation = "";
    BOOL equals_orig;
    BOOL source_routed =
      addr->local_part[0] == ',' || addr->local_part[0] == ':';

    if (source_routed)
      sprintf(buffer, "@%s%s", addr->domain, addr->local_part);
        else sprintf(buffer, "%s@%s", addr->local_part, addr->domain);
    equals_orig = strcmp(buffer, topaddr->orig) == 0;

    if (log_smtp_confirmation && addr->message != NULL)
      {
      int i;
      char *p;
      char *s = addr->message;
      strcpy(big_buffer, " C=\"");
      p = big_buffer + (int)strlen(big_buffer);
      for (i = 0; i < 100 && s[i] != 0; i++)
        {
        if (s[i] == '\"' || s[i] == '\\') *p++ = '\\';
        *p++ = s[i];
        }
      *p++ = '\"';
      *p = 0;
      confirmation = big_buffer;
      }

    log_write(0, LOG_MAIN, "%c> %s%s%s%s%s%s%s%s T=%s H=%s [%s]%s%s",
      logchar,
      buffer,
      equals_orig? "" : " <",
      equals_orig? "" : topaddr->orig,
      equals_orig? "" : ">",
     (addr->director == NULL)? "" : " D=",
     (addr->director == NULL)? "" : addr->director->name,
     (addr->router == NULL)? ""   : " R=",
     (addr->router == NULL)? ""   : addr->router->name,
      addr->transport->name,
     (addr->transported == NULL)? "" : addr->transported->name,
     (addr->transported == NULL)? "" : addr->transported->address,
     (continue_transport == NULL)? "" : "*",
     confirmation);
    }
  }


/* Soft failure, or local delivery process failed; freezing may be
requested. */

else if (result == DEFER || result == PANIC || result == ERROR)
  {
  char *parent = (addr->parent == NULL)? NULL : addr->parent->orig;
  if (result == PANIC) logflags |= LOG_PANIC_DIE;
  addr->next = addr_defer;
  addr_defer = addr;

  /* Log the deferment in the message log */

  fprintf(message_log, "%s %s%s%s%s: %s%s deferred: %s%s%s\n",
    now,
    addr->orig,
    (parent == NULL)? "" : " <",
    (parent == NULL)? "" : parent,
    (parent == NULL)? "" : ">",
    driver_name,
    driver_kind,
    (addr->basic_errno <= 0)? "" : strerror(addr->basic_errno),
    (addr->basic_errno <= 0 || addr->message == NULL)? "" : ": ",
    (addr->message != NULL)? addr->message :
      (addr->basic_errno <= 0)? "unknown error" : "");
  fflush(message_log);

  /* Either driver_name contains something and driver_kind contains
  " director" or whatever (note the leading space), or driver_name is
  a null string and driver_kind contains "directing" etc, without the
  leading space, if all directing or routing has been deferred.  For
  errors of the time "retry time not reached" (also remotes skipped on
  queue run), require a log level of 5. */

  log_write((addr->basic_errno <= ERRNO_RETRY_BASE)? 5 : 0,
    logflags, "== %s%s%s%s %c%s%s defer (%d): %s%s%s",
    addr->orig,
    (parent == NULL)? "" : " <",
    (parent == NULL)? "" : parent,
    (parent == NULL)? "" : ">",
    (driver_name[0] == 0)? driver_kind[0] : toupper(driver_kind[1]),
    (driver_name[0] == 0)? driver_kind+1  : "=",
    (driver_name[0] == 0)? "" : driver_name,
    addr->basic_errno,
    (addr->basic_errno <= 0)? "" : strerror(addr->basic_errno),
    (addr->basic_errno <= 0 || addr->message == NULL)? "" : ": ",
    (addr->message == NULL)? "" : addr->message);

  /* The only currently implemented special action is to freeze the
  message. */

  if (addr->special_action == SPECIAL_FREEZE)
    {
    deliver_freeze = TRUE;
    deliver_frozen_at = time(NULL);
    update_spool = TRUE;
    fprintf(message_log, "*** Frozen ***\n");
    fflush(message_log);
    log_write(0, LOG_MAIN, "Frozen");
    }
  }


/* Hard failure. If there is an address to which an error message can be sent,
put this address on the failed list. If not, put it on the deferred list and
freeze the mail message for human attention. The latter action can also be
explicitly requested by a router/director/transport. */

else
  {
  fprintf(message_log, "%s %s%s%s%s: %s%s failed: %s%s%s\n",
    now,
    addr->orig,
    (addr->parent == NULL)? "" : " <",
    (addr->parent == NULL)? "" : addr->parent->orig,
    (addr->parent == NULL)? "" : ">",
    driver_name,
    driver_kind,
    (addr->basic_errno <= 0)? "" : strerror(addr->basic_errno),
    (addr->basic_errno <= 0)? "" : ": ",
    (addr->message == NULL)? "" : addr->message);
  fflush(message_log);

  log_write(0, LOG_MAIN, "** %s%s%s%s%s%s%s%s%s%s%s%s%s%s%s: %s%s%s",
     addr->orig,
    (addr->parent == NULL)? "" : " <",
    (addr->parent == NULL)? "" : addr->parent->orig,
    (addr->parent == NULL)? "" : ">",
    (addr->director == NULL)? ""  : " D=",
    (addr->director == NULL)? ""  : addr->director->name,
    (addr->router == NULL)? ""    : " R=",
    (addr->router == NULL)? ""    : addr->router->name,
    (addr->transport == NULL)? "" : " T=",
    (addr->transport == NULL)? "" : addr->transport->name,
    (addr->transported == NULL)? "" : " H=",
    (addr->transported == NULL)? "" : addr->transported->name,
    (addr->transported == NULL)? "" : " [",
    (addr->transported == NULL)? "" : addr->transported->address,
    (addr->transported == NULL)? "" : "]",
    (addr->basic_errno <= 0)? "" : strerror(addr->basic_errno),
    (addr->basic_errno <= 0)? "" : ": ",
    (addr->message == NULL)? ""  : addr->message);

  /* If this is a delivery error, or a message for which no replies are
  wanted, and ignore_errmsg_errors is set, force the ignore_error flag.
  This will cause the address to be discarded later (with a log entry). */

  if ((user_null_sender || sender_address[0] == 0) &&
      ignore_errmsg_errors) addr->ignore_error = TRUE;

  /* Freeze the message if requested, or if this is a delivery
  error message or a message for which no replies are ever wanted.
  Exim used to test

    (sender_address[0] == 0 && addr->errors_address == NULL)

  but this can lead to meltdown when delivery to the errors_address
  is failing. Don't freeze if errors are being ignored - the actual
  code to ignore occurs later, instead of sending a message. */

  if (!addr->ignore_error &&
      (addr->special_action == SPECIAL_FREEZE ||
      user_null_sender || sender_address[0] == 0))
    {
    char *info = (addr->special_action == SPECIAL_FREEZE)? "" :
      user_null_sender?
        " (message created with -f <>)" : " (delivery error message)";
    deliver_freeze = TRUE;
    deliver_frozen_at = time(NULL);
    update_spool = TRUE;
    addr->next = addr_defer;
    addr_defer = addr;
    fprintf(message_log, "*** Frozen%s\n", info);
    fflush(message_log);
    log_write(0, LOG_MAIN, "Frozen%s", info);
    }

  /* Don't put the address on the nonrecipients tree yet; wait until an
  error message has been successfully sent. */

  else
    {
    addr->next = addr_failed;
    addr_failed = addr;
    }
  }

return result;   /* Might have changed because of returned output */
}




/*************************************************
*   Post-process a set of remote addresses       *
*************************************************/

/* Do what has to be done immediately after a remote delivery for each set of
addresses, then re-write the spool if necessary. Note that post_process_one
puts the address on an appropriate queue; hence we must fish off the next
one first. This function is also called if there is a problem with setting
up a subprocess to do a remote delivery in parallel. In this case, the final
argument contains a message, and the action must be forced to DEFER.

Argument:
   addr      pointer to chain of address items
   logflags  flags for logging
   msg       NULL for normal cases; -> error message for unexpected problems

Returns:     nothing
*/

static void
remote_post_process(address_item *addr, int logflags, char *msg)
{
host_item *h;

/* If any host addresses were found to be unusable, add them to the unusable
tree so that subsequent deliveries don't try them. */

for (h = addr->host_list; h != NULL; h = h->next)
  {
  if (h->address == NULL) continue;
  if (h->status >= hstatus_unusable) tree_add_unusable(h);
  }

/* Now handle each address on the chain. The transport has placed '=' or '-'
into the special_action field for each successful delivery. */

while (addr != NULL)
  {
  address_item *next = addr->next;
  if (msg != NULL)
    {
    addr->message = msg;
    addr->transport_return = DEFER;
    }
  (void)post_process_one(addr, addr->transport_return, logflags,
    DTYPE_TRANSPORT, addr->special_action);
  addr = next;
  }
}




/*************************************************
*     Wait for one remote delivery subprocess    *
*************************************************/

/* This function is called when doing remote deliveries in parallel. It must
wait for the completion of one subprocess, empty the control block slot, and
return a pointer to the address chain.

Arguments:    non
Returns:      pointer to the chain of addresses handled by the process;
              NULL if no subprocess found - this is an unexpected error
*/

static address_item *
par_wait(void)
{
int poffset, status;
address_item *addr, *addrlist;
pid_t pid;

/* Wait for a subprocess to finish. If wait() yields "no subprocesses" it
is an error. */

DEBUG(2) debug_printf("Waiting for remote delivery process to finish\n");

while ((pid = wait(&status)) < 0) if (errno == ECHILD) return NULL;

DEBUG(1)
  {
  if (status == 00)
    debug_printf("Remote delivery process %d ended\n", pid);
  else
    debug_printf("Remote delivery process %d ended: status=%04x\n", pid,
      status);
  }

/* Find the data block that corresponds to this subprocess. */

for (poffset = 0; poffset < remote_max_parallel; poffset++)
  if (pid == parlist[poffset].pid) break;

if (poffset >= remote_max_parallel)
  log_write(0, LOG_MAIN|LOG_PANIC, "Process %d finished: not found in remote "
    "transport process list", pid);

/* Get the chain of processed addresses */

addrlist = parlist[poffset].addrlist;

/* If the process did not finish cleanly, record an error and freeze, and also
ensure the journal is not removed, incase the delivery did actually happen. */

if ((status & 0xffff) != 0)
  {
  char *msg =
    string_sprintf("%s transport process returned a non-zero status (0x%04x)",
      addrlist->transport->driver_name, status);
  for (addr = addrlist; addr != NULL; addr = addr->next)
    {
    addr->transport_return = DEFER;
    addr->special_action = SPECIAL_FREEZE;
    addr->message = msg;
    }
  remove_journal = FALSE;
  }

/* Else read the pipe to get the delivery status codes and a possible error
message for each address, optionally preceded by unusability data for the hosts
and also by optional retry data. Read in large chunks into the big buffer and
then scan through, interpreting the data therein. In most cases, only a single
read will be necessary. No individual item will ever be anywhere near 500 bytes
in length, so by ensuring that we read the next chunk when there is less than
500 bytes left in the non-final chunk, we can assume each item is complete in
store before handling it. */

else
  {
  host_item *h;
  retry_item *r;
  int fd = parlist[poffset].fd;
  char *endptr = big_buffer;
  char *ptr = endptr;
  char *msg = NULL;
  BOOL unfinished = TRUE;
  BOOL done = FALSE;

  /* Point to the first address; the A items will be in order */

  addr = addrlist;

  /* Loop through all items */

  while (!done)
    {
    int remaining = endptr - ptr;

    /* Read (first time) or top up the chars in the buffer if necessary */

    if (remaining < 500 && unfinished)
      {
      int len;
      int available = big_buffer_size - remaining;

      if (remaining > 0) memmove(big_buffer, ptr, remaining);

      ptr = big_buffer;
      endptr = big_buffer + remaining;
      len = read(fd, endptr, available);

      if (len < 0)
        {
        msg = string_sprintf("failed to read pipe from transport process "
          "%d for transport %s", pid, addr->transport->driver_name);
        break;
        }

      endptr += len;
      unfinished = len == available;
      }

    /* Handle each possible type of item, assuming the complete item is
    available in store. */

    switch (*ptr++)
      {
      /* Host items exist only if any hosts were marked unusable. Match
      up by checking the IP address. */

      case 'H':
      for (h = addrlist->host_list; h != NULL; h = h->next)
        {
        if (h->address == NULL || strcmp(h->address, ptr+2) != 0) continue;
        h->status = ptr[0];
        h->why = ptr[1];
        }
      ptr += 2;
      while (*ptr++);
      break;

      /* Retry items must cause copies to be added to the toplevel address
      in this process. */

      case 'R':
      r = store_malloc(sizeof(retry_item));
      r->next = addrlist->retries;
      addrlist->retries = r;
      r->delete = *ptr++;
      r->destination = string_copy(ptr);
      while (*ptr++);
      memcpy(&(r->basic_errno), ptr, sizeof(r->basic_errno));
      ptr += sizeof(r->basic_errno);
      memcpy(&(r->more_errno), ptr, sizeof(r->more_errno));
      ptr += sizeof(r->more_errno);
      r->message = (*ptr)? string_copy(ptr) : NULL;
      while(*ptr++);
      break;

      /* Address items are in the order of items on the address chain. */

      case 'A':
      if (addr == NULL)
        {
        msg = string_sprintf("address count mismatch for data read from pipe "
          "for transport process %d for transport %s", pid,
            addr->transport->driver_name);
        done = TRUE;
        break;
        }

      addr->transport_return = *ptr++;
      addr->special_action = *ptr++;
      memcpy(&(addr->basic_errno), ptr, sizeof(addr->basic_errno));
      ptr += sizeof(addr->basic_errno);
      memcpy(&(addr->more_errno), ptr, sizeof(addr->more_errno));
      ptr += sizeof(addr->more_errno);
      addr->message = (*ptr)? string_copy(ptr) : NULL;
      while(*ptr++);

      if (*ptr)
        {
        h = store_malloc(sizeof(host_item));
        h->name = string_copy(ptr);
        while (*ptr++);
        h->address = string_copy(ptr);
        while(*ptr++);
        addr->transported = h;
        }
      else ptr++;

      addr = addr->next;
      break;

      /* Z marks the logical end of the data. */

      case 'Z':
      done = TRUE;
      break;

      /* Anything else is a disaster. */

      default:
      msg = string_sprintf("malformed data (%d) read from pipe for transport "
        "process %d for transport %s", ptr[-1], pid,
          addr->transport->driver_name);
      done = TRUE;
      break;
      }
    }

  /* If we have finished without error, but haven't had data for every
  address, something is wrong. */

  if (msg == NULL && addr != NULL)
    msg = string_sprintf("insufficient address data read from pipe "
      "for transport process %d for transport %s", pid,
        addr->transport->driver_name);

  /* Finished with the pipe. */

  close(fd);

  /* If an error message is set, something has gone wrong in getting back
  the delivery data. Put the message into each address and freeze it. */

  if (msg != NULL)
    {
    for (addr = addrlist; addr != NULL; addr = addr->next)
      {
      addr->transport_return = DEFER;
      addr->special_action = SPECIAL_FREEZE;
      addr->message = msg;
      }
    }
  }

/* Mark the data slot unused, decrement the count of subprocesses, and return
the address chain. */

parlist[poffset].pid = 0;
parcount--;
return addrlist;
}



/*************************************************
*              Deliver one message               *
*************************************************/

/* This is the function which is called when a message is to be delivered. It
is passed the id of the message. It is possible that the message no longer
exists, if some other process has delivered it, and it is also possible that
the message is being worked on by another process, in which case the data file
will be locked.

During a queue run, if deliver_selectstring is set, skip messages that do not
have a recipient that matches the selection string.

If no delivery is attempted for any of the above reasons, the function returns
FALSE, otherwise TRUE.

If the give_up flag is set true, do not attempt any deliveries, but instead
fail all outstanding addresses and return the message to the sender (or
whoever).

A delivery operation has a process all to itself; we never deliver more than
one message in the same process. Therefore we needn't worry about store
leakage.

This function is called while running as root unless the security level is
1 or 3, in which case root can be regained by calling mac_sete{g,u}id.

Arguments:
  id          the id of the message to be delivered
  forced      TRUE if delivery was forced by an administrator; this overrides
              retry delays and causes a delivery to be tried regardless
  local_only  TRUE if remote routing and delivery is to be bypassed
  give_up     TRUE if an administrator has requested that delivery attempts
              be abandoned

Returns:      OK if an attempt was made at delivering the message; DEFER if the
              load average was too high to attempt delivery; FAIL otherwise
*/

int
deliver_message(char *id, BOOL forced, BOOL local_only, BOOL give_up)
{
int i, rc, fd;
time_t now = time(NULL);
address_item *addr_last = NULL;
FILE *jread;
BOOL ignore_recipients = FALSE;
BOOL filter_give_up = FALSE;
EXIM_DB *dbm_file;
char spoolname[256];

DEBUG(1) debug_printf("delivering message %s\n", id);
set_process_info("delivering %s", id);

/* Make the forcing flag available for directors/routers/transports, set up
the global message id field, and initialize the count for returned files and
the message size. */

deliver_force = forced;
strcpy(message_id, id);
return_count = 0;
message_size = 0;

/* Initialise some flags */

update_spool = FALSE;
remove_journal = TRUE;

/* If deliver_load_max is non-negative, check that the load average is low
enough to permit deliveries. If we are not root here, we can become root
by seteuid() and need to do this for the benefit of some OS that require it. */

if (!forced && deliver_load_max >= 0)
  {
  #ifdef LOAD_AVG_NEEDS_ROOT
  if (geteuid() != root_uid)
    {
    mac_seteuid(root_uid);
    load_average = os_getloadavg();
    mac_setegid(exim_gid);
    mac_seteuid(exim_uid);
    }
  else
  #endif

  load_average = os_getloadavg();

  DEBUG(2) debug_printf("load average = %.2f max = %.2f\n",
    (double)load_average/1000.0,
    (double)deliver_load_max/1000.0);
  if (load_average > deliver_load_max)
    {
    DEBUG(2) debug_printf("abandoning delivery\n");
    return DEFER;
    }
  }

/* Open and lock the message's data file. Exim locks on this one because the
header file may get replaced as it is re-written during the delivery process.
Any failures cause messages to be written to the log. */

if (!spool_open_datafile(id))
  {
  /* If this exim run was passed an open channel by a transport, we can't
  go on to make use of it, so attempt to close it down tidily. */

  if (continue_transport != NULL)
    {
    transport_instance *t;
    for (t = transports; t != NULL; t = t->next)
      {
      if (strcmp(t->name, continue_transport) == 0)
        {
        if (t->info->closedown != NULL) (t->info->closedown)(t);
        break;
        }
      }
    }

  /* Do no more */

  return FAIL;
  }

/* Now read the contents of the header file, which will set up the headers in
store, and also the list of recipients and the tree of non-recipients and
assorted flags. If there is a reading or format error, give up; if the message
has been around for sufficiently long, remove it. */

sprintf(spoolname, "%s-H", id);
if ((rc = spool_read_header(spoolname, TRUE)) != spool_read_OK)
  {
  if (errno == ERRNO_SPOOLFORMAT)
    {
    struct stat statbuf;
    sprintf(big_buffer, "%s/input/%s", spool_directory, spoolname);
    if (stat(big_buffer, &statbuf) == 0)
      log_write(0, LOG_MAIN, "Format error in spool file %s: size=%d",
        spoolname, statbuf.st_size);
    else log_write(0, LOG_MAIN, "Format error in spool file %s", spoolname);
    }
  else
    log_write(0, LOG_MAIN, "Error reading spool file %s: %s", spoolname,
      strerror(errno));

  /* If we managed to read the envelope data, received_time contains the
  time the message was received. Otherwise, we can calculate it from the
  message id. */

  if (rc != spool_read_hdrerror)
    {
    received_time = 0;
    for (i = 0; i < 6; i++)
      received_time = received_time * 62 + tab62[id[i] - '0'];
    }

  /* If we've had this malformed message too long, sling it. */

  if (now - received_time > keep_malformed)
    {
    sprintf(spoolname, "%s/msglog/%s", spool_directory, id);
    unlink(spoolname);
    sprintf(spoolname, "%s/input/%s-D", spool_directory, id);
    unlink(spoolname);
    sprintf(spoolname, "%s/input/%s-H", spool_directory, id);
    unlink(spoolname);
    sprintf(spoolname, "%s/input/%s-J", spool_directory, id);
    unlink(spoolname);
    log_write(0, LOG_MAIN, "Message removed because older than %s",
      readconf_printtime(keep_malformed));
    }

  close(deliver_datafile);
  deliver_datafile = -1;
  return FAIL;
  }

/* The spool header file has been read. Look to see if there is an existing
journal file for this message. If there is, it means that a previous delivery
attempt crashed (program or host) before it could update the spool header file.
Read the list of delivered addresses from the journal and add them to the
nonrecipients tree. Then update the spool file. We can leave the journal in
existence, as it will get further successful deliveries added to it in this
run, and it will be deleted if this function gets to its end successfully.
Otherwise it might be needed again. */

sprintf(spoolname, "%s/input/%s-J", spool_directory, id);
jread = fopen(spoolname, "r");
if (jread != NULL)
  {
  while (fgets(big_buffer, big_buffer_size, jread) != NULL)
    {
    int n = (int)strlen(big_buffer);
    big_buffer[n-1] = 0;
    tree_add_nonrecipient(big_buffer+1, big_buffer[0] == 'Y');
    DEBUG(2) debug_printf("Previously delivered address %s taken from "
      "journal file\n", big_buffer+1);
    }
  fclose(jread);
  if (spool_write_header(message_id) <= 0)
    log_write(0, LOG_MAIN|LOG_PANIC_DIE, "unable to update spool header "
      "file for %s after reading journal", message_id);
  }
else if (errno != ENOENT)
  {
  log_write(0, LOG_MAIN|LOG_PANIC, "attempt to open journal for reading gave: "
    "%s", strerror(errno));
  return FAIL;
  }

/* A null recipients list indicates some kind of disaster. */

if (recipients_list == NULL)
  {
  log_write(0, LOG_MAIN, "Spool error: no recipients for %s", spoolname);
  close(deliver_datafile);
  deliver_datafile = -1;
  return FAIL;
  }

/* If the message is frozen, do not attempt delivery, unless sufficient
time has passed since the last freezing, or delivery is forced by an
admin user, in which case we assume a manual thaw. */

if (deliver_freeze)
  {
  if ((auto_thaw <= 0 || now <= deliver_frozen_at + auto_thaw) &&
      (!forced || !admin_user))
    {
    log_write(5, LOG_MAIN, "Message is frozen");
    close(deliver_datafile);
    deliver_datafile = -1;
    return FAIL;
    }

  if (forced) deliver_manual_thaw = TRUE;
    else log_write(5, LOG_MAIN, "Unfrozen by auto-thaw");
  deliver_freeze = FALSE;
  update_spool = TRUE;
  }

/* If a system-wide, address-independent message filter is specified,
run it now. If it yields "delivered", then ignore the true recipients
of the message. Failure of the filter file is logged, and the delivery
attempt fails. */

if (message_filter != NULL)
  {
  int action;
  BOOL delivered;
  char *error;
  return_path = sender_address;

  /* Any error in the filter file causes a delivery to be abandoned. */

  if (!filter_system_interpret(&addr_new, &delivered, &action, &error))
    {
    log_write(0, LOG_MAIN|LOG_PANIC, "Error in message_filter file: %s",
      string_printing(error, FALSE));
    close(deliver_datafile);
    deliver_datafile = -1;
    return FAIL;
    }

  /* The filter can request that a message be frozen, but this does not
  take place if the message has been manually thawed. */

  if (action == SPECIAL_FREEZE && !deliver_manual_thaw) deliver_freeze = TRUE;

  /* The filter can request that a message be failed. */

  else if (action == SPECIAL_FAIL) give_up = filter_give_up = TRUE;

  /* Delivery can be restricted only to those recipients (if any) that the
  filter specified. */

  if (delivered)
    {
    ignore_recipients = TRUE;
    log_write(1, LOG_MAIN, "original recipients ignored (message_filter)");
    }

  /* If any new addresses were created by the filter, fake up a "parent"
  for them. This is necessary for pipes, etc., which are expected to have
  parents, and it also gives some sensible logging for others. Allow
  pipes, files, and autoreplies, and run them as the filter uid if set,
  otherwise as the current uid. These addresses also get an extra header,
  X-Envelope-To, containing up to the first 100 envelope recipients. */

  if (addr_new != NULL)
    {
    int size = 1024;
    int slen;
    header_line *h = (header_line *)store_malloc(sizeof(header_line) + size);

    int uid = (message_filter_uid_set)? message_filter_uid : geteuid();
    int gid = (message_filter_gid_set)? message_filter_gid : getegid();
    address_item *parent = deliver_make_addr("message filter");
    address_item *p;

    strcpy(h->text, "X-Envelope-To: ");
    slen = (int)strlen(h->text);

    for (i = 0; i < recipients_count && i < 100; i++)
      {
      char *s = recipients_list[i];
      int len = (int)strlen(s);

      if (slen + len + 4 >= size)
        {
        header_line *hh;
        size *= 2;
        hh = (header_line *)store_malloc(sizeof(header_line) + size);
        memcpy(hh->text, h->text, slen);
        store_free(h);
        h = hh;
        }

      if (i != 0)
        {
        strcpy(h->text + slen, ",\n ");
        slen += 3;
        }

      strcpy(h->text + slen, s);
      slen += len;
      }

    strcpy(h->text + slen, "\n");
    h->slen = slen + 1;
    h->next = h->prev = NULL;
    h->type = htype_other;

    parent->domain = qualify_domain_recipient;
    parent->local_part = "message filter";

    for (p = addr_new; p != NULL; p = p->next)
      {
      addr_last = p;
      p->extra_headers = h;
      parent->child_count++;
      p->parent = parent;
      if (p->pfr)
        {
        p->allow_pipe = p->allow_file = p->allow_reply = TRUE;
        p->uid = uid;
        p->gid = gid;
        }
      DEBUG(9) debug_printf("message_filter added %s\n", p->orig);
      }
    }
  }

/* Scan the recipients list, and for every one that is not in the non-
recipients tree, add an addr item to the chain of new addresses. Duplicates are
handled later by a different tree structure; we can't just extend the
non-recipients tree, because that will be re-written to the spool if the
message is deferred, and in any case there are casing complications for local
addresses. */

if (!ignore_recipients) for (i = 0; i < recipients_count; i++)
  {
  if (tree_search_addr(tree_nonrecipients, recipients_list[i], FALSE) == NULL)
    {
    address_item *new = deliver_make_addr(recipients_list[i]);
    if (addr_new == NULL) addr_new = new; else addr_last->next = new;
    addr_last = new;
    }
  }

DEBUG(7)
  {
  address_item *p = addr_new;
  debug_printf("Delivery address list:\n");
  while (p != NULL)
    {
    debug_printf("  %s\n", p->orig);
    p = p->next;
    }
  }

/* Set up the buffers used for copying over the file when delivering. */

deliver_in_buffer = store_malloc(DELIVER_BUFFER_SIZE);
deliver_out_buffer = store_malloc(DELIVER_BUFFER_SIZE);


/* Open the message log file. This records details of deliveries, deferments,
and failures for the benefit of the mail administrator. The log is not used by
exim itself to track the progress of a message; that is done by rewriting the
header spool file.

Exim is running as root here, unless seteuid() has been used to reduce
privilege while directing and routing. The message log can therefore end up
being owned by root. However, if an exim uid is defined, the msglog directory
will be owned by exim, and so when the time comes to delete the file, the
ownership doesn't matter. So don't waste effort making exim own it. However,
ensure that the mode is the same as other spool files. */

sprintf(spoolname, "%s/msglog/%s", spool_directory, id);
fd = open(spoolname, O_WRONLY|O_APPEND|O_CREAT, SPOOL_MODE);

if (fd < 0)
  {
  if(errno == ENOENT)
    {
    directory_make(spool_directory, "msglog", MSGLOG_DIRECTORY_MODE);
    fd = open(spoolname, O_WRONLY|O_APPEND|O_CREAT, SPOOL_MODE);
    }
  if (fd < 0)
    {
    log_write(0, LOG_MAIN|LOG_PANIC, "Couldn't open message log %s: %s",
      spoolname, strerror(errno));
    return FAIL;
    }
  }

/* Make sure the file's group is the Exim gid if exim_uid exists (can't have
exim_uid set without exim_gid), and double-check the mode because the group
setting doesn't always get set automatically. */

if (exim_uid_set)
  {
  fchown(fd, exim_uid, exim_gid);
  fchmod(fd, SPOOL_MODE);
  }

/* Now make a C stream out of it. */

message_log = fdopen(fd, "a");
if (message_log == NULL)
  {
  log_write(0, LOG_MAIN|LOG_PANIC, "Couldn't fdopen message log %s: %s",
    spoolname, strerror(errno));
  return FAIL;
  }


/* If deliver_freeze is set at this point, it is as a result of a call to the
system filter file. */

if (deliver_freeze)
  {
  if (spool_write_header(message_id) <= 0)
    log_write(0, LOG_MAIN|LOG_PANIC_DIE, "unable to update spool header "
      "file for %s", message_id);
  fprintf(message_log, "*** Frozen by message filter\n");
  fclose(message_log);
  log_write(0, LOG_MAIN, "Frozen by message filter");
  close(deliver_datafile);
  deliver_datafile = -1;
  return FAIL;
  }


/* If asked to give up on this message, simply put all the addresses on
the failed chain, and set an appropriate error message. If the message
has a null sender and an address has no errors address, send it to the global
errors address. */

if (give_up)
  {
  struct passwd *pw;
  char *by, *msg;
  address_item *addr;

  addr_failed = addr_new;
  addr_new = NULL;

  if (filter_give_up)
    {
    by = "message filter";
    msg = "delivery cancelled";
    }
  else
    {
    pw = getpwuid(real_uid);
    by = (pw != NULL)? pw->pw_name : string_sprintf("uid %d", (int)real_uid);
    msg = "delivery cancelled by administrator";
    }

  log_write(0, LOG_MAIN, "cancelled by %s", by);
  for (addr = addr_failed; addr != NULL; addr = addr->next)
    {
    addr->message = msg;
    if (sender_address[0] == 0 && addr->errors_address == NULL)
      addr->errors_address = errors_address;
    }
  }


/* Until there are no more new addresses, handle each one as follows:

 . If this is a generated address (indicated by the presence of a parent
   pointer) then check to see whether it is a pipe or a file, and if so,
   handle it directly here. The director that produced the address will have
   set the allow flags into the address, and also set the uid/gid required.
   Having the directors generate new addresses and then checking them here at
   the outer level is tidier than making each director do its checking, and
   means that directors don't need access to the failed address queue.

 . Determine if it is a local address; this may involve removing one
   or more leading "@<local-domain>" strings, and/or handing the percent
   hack on the local user name. A subroutines handles all this, setting the
   "local", "local_part", and "domain" fields in the address.

.  If it is a local address that was generated by another director, determine
   if any of its parents have the same local name. If so, generate a different
   string for previous delivery checking, and do the check. Without this
   code, if the address spqr generates spqr via a forward or alias file,
   delivery of the generated spqr stops further attempts at the top level spqr,
   which is not what is wanted.

 . Local addresses get put on the addr_direct chain, while remote addresses
   get put on the addr_route chain. However, before doing so, we check up
   on the retry database to see if a delay is set for directing or routing
   the address. If so, the address gets put directly onto the addr_defer
   chain. For directed addresses, while we have the retry database open, we
   check for the existence of a transport retry record, and save the next_try
   time if one is found. This saves a later database access for normal
   deliveries to local users.

 . Now we run the local addresses through the directors. A director may put
   the address on the addr_local chain for local delivery, or put it on the
   addr_failed chain if it is undeliveable, or it may generate child addresses
   and put them on the addr_new chain, or it may defer an address. All these
   things are passed as arguments so that the directors can be called for
   verification purposes as well.

 . Because directing may produce new addresses, we have to have an outer loop
   to do this all again. The reason for not doing the directing as soon as
   an address is determined to be local is that directing can in principle take
   some appreciable amount of time, and we do not want to have the retry
   database open any longer than is necessary, nor do we want to open and close
   it for each local address.

.  When all the directing is done, run the remote addresses through the
   routers. It may turn out that some apparently remote addresses are in fact
   local ones that have been abbreviated and so not recognized as local until
   the router expands them. Such addresses get put onto the addr_new chain
   and the whole thing recycles yet again. Wheels within wheels...
*/

header_changed = FALSE;            /* No headers rewritten yet */
while (addr_new != NULL)           /* Loop again after routing */
  {
  while (addr_new != NULL)         /* Loop again after directing */
    {
    address_item *addr;
    dbm_file = db_open("retry", O_RDONLY);

    /* Loop for current batch of new addresses */

    while (addr_new != NULL)
      {
      addr = addr_new;
      addr_new = addr->next;

      /* Handle generated address that is a pipe or a file or an autoreply.
      If two different users specify delivery to the same pipe or file, there
      should be two different deliveries, so build a unique string that
      incorporates the original address, and use this for duplicate testing
      and recording delivery. */

      if (addr->pfr)
        {
        addr->unique = string_sprintf("%s:%s", addr->orig, addr->parent->orig);

        if (tree_search(tree_nonrecipients, addr->unique) != NULL)
          {
          DEBUG(9) debug_printf("%s was previously delivered: discarded\n",
            addr->orig);
          (void)child_done(addr, tod_stamp(tod_log));
          continue;
          }

        if (tree_search(tree_duplicates, addr->unique) != NULL)
          {
          DEBUG(9) debug_printf("%s is a duplicate address: discarded\n",
            addr->orig);
          addr->next = addr_duplicate;
          addr_duplicate = addr;
          continue;
          }

        tree_add_duplicate(addr->unique, TRUE);

        /* Handle a pipe */

        if (addr->orig[0] == '|')
          {
          if (!addr->allow_pipe)
            {
            addr->basic_errno = ERRNO_FORBIDPIPE;
            addr->message = "delivery to pipe forbidden";
            (void)post_process_one(addr, FAIL, 0, DTYPE_DIRECTOR, 0);
            continue;   /* with the next new address */
            }
          else
            {
            addr->local_part = addr->orig;
            addr->domain = addr->parent->domain;
            addr->transport = transport_address_pipe;
            if (addr->transport == NULL)
              {
              addr->basic_errno = ERRNO_MISSINGPIPE;
              addr->message = string_sprintf("%s transport not configured",
                address_pipe_transport);
              addr->errors_address = errors_address;
              (void)post_process_one(addr, FAIL, 0, DTYPE_DIRECTOR, 0);
              continue;   /* with the next new address */
              }
            else
              {
              addr->next = addr_local;
              addr_local = addr;
              }
            }
          }

        /* Handle a file */

        else if (addr->orig[0] == '/')
          {
          if (!addr->allow_file)
            {
            addr->basic_errno = ERRNO_FORBIDFILE;
            addr->message = "delivery to file forbidden";
            (void)post_process_one(addr, FAIL, 0, DTYPE_DIRECTOR, 0);
            continue;   /* with the next new address */
            }
          else
            {
            addr->local_part = addr->orig;
            addr->domain = addr->parent->domain;
            addr->transport = transport_address_file;
            if (addr->transport == NULL)
              {
              addr->basic_errno = ERRNO_MISSINGFILE;
              addr->message = string_sprintf("%s transport not configured",
                address_file_transport);
              addr->errors_address = errors_address;
              (void)post_process_one(addr, FAIL, 0, DTYPE_DIRECTOR, 0);
              continue;   /* with the next new address */
              }
            else
              {
              addr->next = addr_local;
              addr_local = addr;
              }
            }
          }

        /* Handle an auto-reply */

        else
          {
          if (!addr->allow_reply)
            {
            addr->basic_errno = ERRNO_FORBIDREPLY;
            addr->message = "autoreply forbidden";
            (void)post_process_one(addr, FAIL, 0, DTYPE_DIRECTOR, 0);
            continue;   /* with the next new address */
            }
          else
            {
            addr->local_part = addr->orig;
            addr->domain = addr->parent->domain;
            addr->transport = transport_address_reply;
            if (addr->transport == NULL)
              {
              addr->basic_errno = ERRNO_MISSINGREPLY;
              addr->message = string_sprintf("%s transport not configured",
                address_reply_transport);
              addr->errors_address = errors_address;
              (void)post_process_one(addr, FAIL, 0, DTYPE_DIRECTOR, 0);
              continue;   /* with the next new address */
              }
            else
              {
              addr->next = addr_local;
              addr_local = addr;
              }
            }
          }

        continue;   /* with the next new address */
        }


      /* All addresses should have been made fully qualified when the message
      was accepted or when a director generated a new address, so panic if we
      find an unqualified one. */

      if (parse_find_at(addr->orig) == NULL)
        log_write(0, LOG_PANIC_DIE, "Unqualified address \"%s\" found in %s",
          addr->orig, id);

      /* Determine locality - this sets the "local_part", "domain", and "local"
      fields, and lowercases the domain and the local part if local. */

      deliver_setlocal(addr);

      /* Now we can check for duplicates and previously delivered addresses.
      We need to check local addresses using the lower cased form of the local
      part. If the address is local, we must first check for a matching address
      in its parents, and if so, generate a different string to use for
      duplication checking. Stick a \ on the front in the simple case; it *is*
      possible for there to be more than one level; use \n\ for subsequent
      cases. */

      if (addr->local)
        {
        address_item *parent;
        for (parent = addr->parent; parent != NULL; parent = parent->parent)
          {
          if (strcmpic(addr->local_part, parent->local_part) == 0 &&
              strcmpic(addr->domain, parent->domain) == 0)
            break;
          }
        if (parent != NULL)
          {
          if (parent->unique[0] == '\\')
            {
            addr->unique =
              string_sprintf("\\%c\\%s@%s",
              (parent->unique[2] == '\\')? parent->unique[1] + 1 : '1',
              addr->local_part, addr->domain);
            }
          else addr->unique =
            string_sprintf("\\%s@%s", addr->local_part, addr->domain);
          }
        else addr->unique =
          string_sprintf("%s@%s", addr->local_part, addr->domain);
        }

      /* Remote addresses may occasionally have parents, if the "unseen"
      option is in use. In these cases, an alternative unique name is
      required. */

      else if (addr->parent != NULL)
        {
        address_item *parent;
        for (parent = addr->parent; parent != NULL; parent = parent->parent)
          {
          if (strcmp(addr->local_part, parent->local_part) == 0 &&
              strcmp(addr->domain, parent->domain) == 0)
            break;
          }
        if (parent != NULL)
          {
          if (parent->unique[0] == '\\')
            {
            addr->unique =
              string_sprintf("\\%c\\%s",
              (parent->unique[2] == '\\')? parent->unique[1] + 1 : '1',
              addr->unique);
            }
          else addr->unique = string_sprintf("\\%s", addr->unique);
          }
        }

      DEBUG(9) debug_printf("unique=%s\n", addr->unique);

      /* Check for previous delivery. This can happen if configuration files
      change or if things got out of step. Ensure the counts in any parents
      are updated. */

      if (tree_search_addr(tree_nonrecipients, addr->unique, addr->pfr) != NULL)
        {
        DEBUG(9) debug_printf("%s was previously delivered: discarded\n",
          addr->unique);
        (void)child_done(addr, tod_stamp(tod_log));
        continue;
        }

      /* Check for duplication. Remember duplicated addresses so they can
      be marked "delivered" when the duplicate is delivered. */

      if (tree_search_addr(tree_duplicates, addr->unique, addr->pfr) != NULL)
        {
        DEBUG(9) debug_printf("%s is a duplicate address: discarded\n",
          addr->unique);
        addr->next = addr_duplicate;
        addr_duplicate = addr;
        continue;
        }

      /* Remember the first, to check for subsequent duplicates. */

      tree_add_duplicate(addr->unique, addr->pfr);

      /* If the address is local, check on directing retry status, and add
      either to the directing chain or the defer chain. */

      if (addr->local)
        {
        char *destination = string_sprintf("D:%s@%s", addr->local_part,
          addr->domain);
        db_retry *retry_record =
          (dbm_file == NULL)? NULL : db_read(dbm_file, destination);

        /* Defer directing unless no retry data or we've passed the next
        retry time, or this message is forced. However, if the retry time
        has expired, try just one more time. If this fails, subsequent
        processing of the retry data should cause the address to fail.
        This ensures that local addresses are always directed at least
        once before being rejected. */

        if (retry_record != NULL && now < retry_record->next_try &&
            !deliver_force && !retry_record->expired)
          {
          addr->message = "retry time not reached";
          addr->basic_errno = ERRNO_DRETRY;
          (void)post_process_one(addr, DEFER, LOG_MAIN, DTYPE_DIRECTOR, '=');
          }

        /* Otherwise set up for directing. Remember whether there is a retry
        record or not, so a request for its updating can be forced when
        directing occurs. */

        else
          {
          addr->next = addr_direct;
          addr_direct = addr;
          addr->dr_retry_exists = (retry_record != NULL);
          DEBUG(7) debug_printf("%s: queued for directing\n", addr->orig);
          }
        }

      /* If the address is not local, check on routing retry status, and
      add either to the remote chain or the defer chain. */

      else
        {
        char *destination = string_sprintf("R:%s", addr->domain);
        db_retry *retry_record =
          (dbm_file == NULL)? NULL : db_read(dbm_file, destination);

        /* Defer routing unless no retry data or we've passed the next
        retry time, or this message is forced. However, if the retry time
        has expired, allow the routing attempt. If it fails again, the
        address will be failed. This ensures that each address is routed
        at least once, even after long-term routing failures. */

        if (retry_record != NULL && now < retry_record->next_try &&
            !deliver_force && !retry_record->expired)
          {
          addr->message = "retry time not reached";
          addr->basic_errno = ERRNO_RRETRY;
          (void)post_process_one(addr, DEFER, LOG_MAIN, DTYPE_ROUTER, '=');
          }

        /* Queue for routing, remembering whether there is a retry record or
        not, so a request for its updating can be forced when routing occurs. */

        else
          {
          addr->dr_retry_exists = (retry_record != NULL);
          addr->next = addr_route;
          addr_route = addr;
          DEBUG(7) debug_printf("%s: queued for routing\n", addr->orig);
          }
        }
      }

    /* The database is closed while directing and routing is happening.
    Requests to update it are put on a chain and all processed together
    at the end. */

    if (dbm_file != NULL) db_close(dbm_file);

    /* Run the local addresses that are not already deferred through the
    directors. If a director defers an address, or if there is an existing
    retry record in the database, add a retry item. Note that a director is
    permitted to generate a remote delivery if it is set up with a non-local
    transport. Ensure return_path is available so that it can be referred to
    in filter files or elsewhere. */

    while ((addr = addr_direct) != NULL)
      {
      int rc;
      addr_direct = addr->next;
      addr->next = NULL;
      return_path = (addr->errors_address != NULL)?
        addr->errors_address : sender_address;
      if ((rc = direct_address(addr, &addr_local, &addr_remote, &addr_new,
        &addr_succeed, v_none)) == DEFER || addr->dr_retry_exists)
          retry_add_item(addr, "D", TRUE, NULL, rc != DEFER);

      /* Handle addresses that are finished with because of a failure. */

      if (rc != OK)
        (void)post_process_one(addr, rc, LOG_MAIN, DTYPE_DIRECTOR, '=');
      }
    }       /* Loop to handle any new addresses created by the directors */


  /* DEBUG: verify what's happened after all the directing */

  DEBUG(7)
    {
    address_item *p = addr_local;
    debug_printf("After directing:\n  Local addresses:\n");
    while (p != NULL)
      {
      debug_printf("    %s\n", p->orig);
      p = p->next;
      }

    p = addr_remote;
    debug_printf("  Remote addresses:\n");
    while (p != NULL)
      {
      debug_printf("    %s\n", p->orig);
      p = p->next;
      }

    p = addr_failed;
    debug_printf("  Failed addresses:\n");
    while (p != NULL)
      {
      debug_printf("    %s\n", p->orig);
      p = p->next;
      }

    p = addr_route;
    debug_printf("  Addresses to be routed:\n");
    while (p != NULL)
      {
      debug_printf("    %s\n", p->orig);
      p = p->next;
      }

    p = addr_defer;
    debug_printf("  Deferred addresses:\n");
    while (p != NULL)
      {
      debug_printf("    %s\n", p->orig);
      p = p->next;
      }
    }

  /* If either of queue_remote or local_only is set, we don't even want to
  try routing remote addresses, so just defer them all. */

  if (queue_remote || local_only)
    {
    while (addr_route != NULL)
      {
      address_item *addr = addr_route;
      addr_route = addr->next;
      addr->basic_errno = ERRNO_LOCAL_ONLY;
      addr->message = local_only?
        "remote deliveries being skipped" :
        "queue_remote option set";
      (void)post_process_one(addr, DEFER, LOG_MAIN, DTYPE_ROUTER, '=');
      }
    }

  /* Now route those remote addresses that are not deferred. Routing may take
  some time (DNS timeouts) and I originally planned to get the local deliveries
  done before the routing, but since routing may cause addresses to get
  re-written, this is not the best plan. */

  while (addr_route != NULL)
    {
    int rc;
    address_item *addr = addr_route;
    char *old_domain = addr->domain;
    addr_route = addr->next;

    /* Just in case some router parameter refers to it. */

    return_path = (addr->errors_address != NULL)?
      addr->errors_address : sender_address;

    /* If a router defers an address, or if there is an existing retry record
    in the database, add a retry item. */

    if ((rc = route_address(addr, &addr_local, &addr_remote, &addr_new,
      v_none)) == DEFER || addr->dr_retry_exists)
        retry_add_item(addr, "R", FALSE, NULL, rc != DEFER);

    /* If an address turned out to be local after all, put it back on the
    addr_new chain for re-directing (or re-routing if it was source routed),
    and build a new original address. In fact, we need to do this by creating
    a new address with the old one as parent, so that the original address
    gets marked done when the child is done. */

    if (rc == ISLOCAL)
      {
      address_item *old = addr;
      char *new_address;

      if (addr->local_part[0] == ',' || addr->local_part[0] == ':')
        new_address = string_sprintf("@%s%s", addr->domain, addr->local_part);
      else
        new_address = string_sprintf("%s@%s", addr->local_part, addr->domain);

      addr = deliver_make_addr(new_address);
      addr->parent = old;
      addr->ignore_error |= old->ignore_error;
      addr->errors_address = old->errors_address;
      old->child_count++;

      addr->next = addr_new;
      addr_new = addr;
      }

    /* Handle addresses that are finished with. */

    else if (rc != OK)
      (void)post_process_one(addr, rc, LOG_MAIN, DTYPE_ROUTER, '=');

    /* Successful routing: look to see if there are any more addresses waiting
    to be routed that have the same domain as this one started out with, and
    if so, copy the results of this routing for them and put them on the
    remote or local delivery queue as appropriate. However, we must only do
    this when the router permits it to happen - essentially it can be done
    only if the routing does not depend on the local part, and only the
    router can know. */

    else if (addr->routed_by_domain)
      {
      address_item **chain = &addr_route;
      BOOL local = addr->transport->info->local;

      while (*chain != NULL)
        {
        address_item *addr2 = *chain;

        if (strcmp(addr2->domain, old_domain) != 0)
          {
          chain = &(addr2->next);
          continue;
          }

        *chain = addr2->next;

        if (local)
          {
          addr2->next = addr_local;
          addr_local = addr2;
          }
        else
          {
          addr2->next = addr_remote;
          addr_remote = addr2;
          }

        addr2->domain = addr->domain;
        addr2->route_domain = addr->route_domain;
        addr2->router = addr->router;
        addr2->transport = addr->transport;
        addr2->host_list = addr->host_list;

        DEBUG(7) debug_printf("Routing for %s copied from %s\n",
          addr2->orig, addr->orig);
        }
      }
    }
  }     /* Restart entire process if any remotes became local */


/* Free any resources that were cached during directing and routing. It's
debatable as to whether direct_tidyup() should be called immediately after
directing rather than waiting till routing is done. The point is that routing
can cause further directing if a "remote" domain turns out to be local.
However, as the resources are typically just open files (e.g. aliases) I don't
think it's critical. */

search_tidyup();
direct_tidyup();
route_tidyup();

/* Problems with res_init() have sometimes caused overwriting to occur while
routing. Do a double check that message_id has not been overwritten, as this is
a serious disaster. */

if (strcmp(message_id, id) != 0)
  log_write(0, LOG_MAIN|LOG_PANIC_DIE, "Panic: message_id %s has been "
    "overwritten with %s", id, message_id);


/* If this is a run to continue deliveries to an external channel that is
already set up, defer any local deliveries. */

if (continue_transport != NULL)
  {
  if (addr_defer == NULL) addr_defer = addr_local; else
    {
    address_item *addr = addr_defer;
    while (addr->next != NULL) addr = addr->next;
    addr->next = addr_local;
    }
  addr_local = NULL;
  }


/* Because address rewriting can happen in the routers or directors, we should
not really do ANY deliveries until all addresses have been routed or directed,
so that all recipients of the message get the same headers. However, this is in
practice not always possible, since sometimes remote addresses give DNS
timeouts for days on end. The pragmatic approach is to deliver what we can now,
saving any rewritten headers so that at least the next lot of recipients
benefit from the rewriting that has already been done.

If any headers have been rewritten during routing or directing, update the
spool file to remember them for all subsequent deliveries. This can be delayed
till later if there is only address to be delivered - if it succeeds the spool
write need not happen. */

if (header_changed &&
    ((addr_local != NULL &&
       (addr_local->next != NULL || addr_remote != NULL)) ||
     (addr_remote != NULL && addr_remote->next != NULL)))
  {
  if (update_spool && spool_write_header(message_id) <= 0)
    log_write(0, LOG_MAIN|LOG_PANIC_DIE, "unable to update spool header "
      "file for %s", message_id);
  header_changed = FALSE;
  }


/* If there are any deliveries to be done, open the journal file. This is used
to record successful deliveries as soon as possible after each delivery is
known to be complete. This can happen in subprocesses of this process for
parallel remote deliveries. A file opened with O_APPEND is used so that several
processes can run simultaneously.

The journal is just insurance against crashes. When the spool file is
ultimately updated at the end of processing, the journal is deleted. If a
journal is found to exist at the start of delivery, the addresses listed
therein are added to the non-recipients. */

if (addr_local != NULL || addr_remote != NULL)
  {
  sprintf(spoolname, "%s/input/%s-J", spool_directory, id);
  journal_fd = open(spoolname, O_WRONLY|O_APPEND|O_CREAT, SPOOL_MODE);

  if (journal_fd < 0)
    {
    log_write(0, LOG_MAIN|LOG_PANIC, "Couldn't open journal file %s: %s",
      spoolname, strerror(errno));
    return FAIL;
    }

  /* Make sure the file's group is the Exim gid if exim_uid exists (can't have
  exim_uid set without exim_gid), and double-check the mode because the group
  setting doesn't always get set automatically. */

  if (exim_uid_set)
    {
    fchown(journal_fd, exim_uid, exim_gid);
    fchmod(journal_fd, SPOOL_MODE);
    }
  }


/* True local deliveries are always done one at a time. However, local
deliveries that are the result of routing rather than directing can be batched
up in some cases. Typically this is when writing batched SMTP output files for
use by some external transport mechanism. Batching can also be specified in
other special cases using directors or routers. */

DEBUG(2) debug_printf(">>> Local deliveries >>>\n");

while (addr_local != NULL)
  {
  int result = DEFER;
  int logflags = LOG_MAIN;
  int logchar = '=';
  transport_instance *tp;
  address_item *addr = addr_local;
  addr_local = addr->next;
  addr->next = NULL;

  /* An internal disaster if there is no transport. */

  if ((tp = addr->transport) == NULL)
    {
    logflags |= LOG_PANIC;
    addr->message = "No transport set by director";
    }

  /* Otherwise we can try a delivery, but first check for batched addresses
  and possible amalgamation. We can't amalgamate if the transport depends on
  the local-part in any way. If amalgamation is permitted (even if it doesn't
  occur), don't use the local part in any retry key. Otherwise, use it if
  configured to do so. */

  else
    {
    char *destination;
    db_retry *retry_record = NULL;
    BOOL retry_use_local_part = tp->retry_use_local_part;

    if (tp->local_batch > local_batch_one &&
        !readconf_depends((driver_instance *)tp, "local_part"))
      {
      BOOL tp_sets_uid = tp->uid_set || tp->expand_uid != NULL ||
        tp->deliver_as_creator;
      int local_batch = tp->local_batch;
      int batch_count = 1;
      address_item **anchor = &addr_local;
      address_item *last = addr;
      address_item *next;

      retry_use_local_part = FALSE;

      /* If the transport's options depend on the domain, turn the batch
      option from "all" into "domain". */

      if (local_batch == local_batch_all &&
        readconf_depends((driver_instance *)tp, "domain"))
          local_batch = local_batch_domain;

      /* Pick off all addresses which have the same transport and errors
      address and (first) host and route_options. If the transport has no
      uid/gid setting, they must also have the same uid/gid. If the
      local_batch option is "domain" they must also have the same domain.
      There is a maximum count set in the transport's batch_max field. */

      while ((next = *anchor) != NULL && batch_count < tp->batch_max)
        {
        if (next->transport == addr->transport &&
            next->errors_address == addr->errors_address &&
            (next->route_option == addr->route_option ||
              (next->route_option != NULL && addr->route_option != NULL &&
                strcmp(next->route_option, addr->route_option) == 0)) &&
            (tp_sets_uid ||
              (next->uid_set && addr->uid_set && next->uid == addr->uid &&
               next->gid_set && addr->gid_set && next->gid == addr->gid)) &&
            (local_batch == local_batch_all ||
              strcmp(next->domain, addr->domain) == 0) &&
            ((addr->host_list == NULL && next->host_list == NULL) ||
             (addr->host_list != NULL && next->host_list != NULL &&
              strcmp(addr->host_list->name, next->host_list->name) == 0)))
          {
          *anchor = next->next;
          next->next = NULL;
          last->next = next;
          last = next;
          batch_count++;
          }
        else anchor = &(next->next);
        }
      }

    /* We are set to try a delivery, but first check to see if there is
    a retry time that we need to wait for. The local part may or may not
    be used as part of the retry key. */

    destination = string_sprintf("T:%s%s%s",
      retry_use_local_part? addr->local_part : "",
      retry_use_local_part? "@" : "",
      addr->domain);
    dbm_file = db_open("retry", O_RDONLY);
    if (dbm_file != NULL)
      {
      retry_record = db_read(dbm_file, destination);
      db_close(dbm_file);
      }

    /* Attempt delivery if no retry record, the next try time has been reached,
    or, if the retry time has expired, try for delivery one more time. If this
    causes deferment, the message will get failed by the retry checking code
    later on. This means that a local delivery will always be tried at least
    once, even when the recipient has been failing for a very long time. This
    is reasonable because trying a local delivery is relatively cheap. Contrast
    remote deliveries, where there options controlling what happens after a
    long outage. */

    if (retry_record == NULL || now >= retry_record->next_try ||
         retry_record->expired || deliver_force)
      {
      return_path = (addr->errors_address != NULL)?
        addr->errors_address : sender_address;
      deliver_local(addr);
      result = addr->transport_return;

      DEBUG(9) debug_printf("%s transport returned %s\n",
        tp->name,
        (result == OK)?    "OK" :
        (result == DEFER)? "DEFER" :
        (result == FAIL)?  "FAIL" :
        (result == PANIC)? "PANIC" : "?");

      /* If there is a retry_record, or if delivery is deferred, build a retry
      item for setting a new retry time or deleting the old retry record from
      the database. These items are handled all together after all addresses
      have been handled (so the database is open just for a short time for
      updating). */

      if (result == DEFER || retry_record != NULL)
        retry_add_item(addr, "T", FALSE, destination+2, result != DEFER);
      }

    /* Defer if retry time not reached (default setting is result = DEFER). */

    else
      {
      addr->message = "Retry time not yet reached";
      addr->basic_errno = ERRNO_LRETRY;
      }
    }

  /* Do what has to be done immediately after a delivery. Note that when
  handling several batched addresses we mustn't rely on the next field
  afterwards, as the post_processing function may put the address onto a
  chain. If a pipe delivery generated text to be sent back, the result
  is changed to FAIL, and we must copy this for subsequent addresses
  in the batch. */

  while (addr != NULL)
    {
    address_item *next = addr->next;
    int newresult =
      post_process_one(addr, result, logflags, DTYPE_TRANSPORT, logchar);
    if (newresult != result)
      {
      address_item *addr2;
      for (addr2 = next; addr2 != NULL; addr2 = addr2->next)
        {
        addr2->basic_errno = addr->basic_errno;
        addr2->message = addr->message;
        }
      result = newresult;
      }
    addr = next;
    if (result == OK) logchar = '-';
    }
  }


/* There may have been expansion lookups and string matches during local
deliveries. Free any cached resources so as not to hold them during remote
deliveries. */

search_tidyup();


/* Once the local deliveries are done, we do not need to be root any longer,
so if a non-root uid has been specified, give up privileges for good at this
point, whatever the security level, using seteuid to recover root privilege
if it has been temporarily given up. */

if (exim_uid_set)
  {
  if (geteuid() != root_uid) mac_seteuid(root_uid);
  setgid(exim_gid);
  setuid(exim_uid);
  }


/* The function that read the headers counted the number of Received: headers.
If there are too many, we must not make any remote deliveries. */

if (received_count > received_headers_max)
  {
  log_write(0, LOG_MAIN, "** Too many \"Received\" headers for remote delivery");
  DEBUG(2) debug_printf("Too many \"Received\" headers for remote delivery\n");
  while (addr_remote != NULL)
    {
    address_item *addr = addr_remote;
    addr_remote = addr->next;
    addr->message = "Too many \"Received\" headers for remote delivery";
    addr->next = addr_failed;
    addr_failed = addr;
    }
  }

/* If remote_sort is set, arrange that the chain of addresses for remote
deliveries is ordered according to the strings specified. Try to make this
shuffling reasonably efficient by handling sequences of addresses rather than
just single ones. */

if (remote_sort != NULL)
  {
  char *pattern;
  address_item **aptr = &addr_remote;

  for (pattern = string_firstinlist(remote_sort, ':');
       pattern != NULL && *aptr != NULL;
       pattern = string_nextinlist(':'))
    {
    address_item *moved = NULL;
    address_item **bptr = &moved;

    while (*aptr != NULL)
      {
      address_item **next;
      if (match_check_string((*aptr)->domain, pattern, &re_remote_sort,
          -1, TRUE))
        {
        aptr = &((*aptr)->next);
        continue;
        }

      next = &((*aptr)->next);
      while (*next != NULL &&
        !match_check_string((*next)->domain, pattern, &re_remote_sort,
          -1, TRUE)) next = &((*next)->next);

      /* If the batch of non-matchers is at the end, add on any that were
      extracted further up the chain, and end this iteration. Otherwise,
      extract them from the chain and hang on the moved chain. */

      if (*next == NULL)
        {
        *next = moved;
        break;
        }

      *bptr = *aptr;
      *aptr = *next;
      *next = NULL;
      bptr = next;
      aptr = &((*aptr)->next);
      }

    /* If the loop ended because the final address matched, *aptr will
    be NULL. Add on to the end any extracted non-matching addresses. If
    *aptr is not NULL, the loop ended via "break" when *next is null, that
    is, there was a string of non-matching addresses at the end. In this
    case the extracted addresses have already been added on the end. */

    if (*aptr == NULL) *aptr = moved;
    }

  DEBUG(9)
    {
    address_item *addr;
    debug_printf("remote addresses after sorting:\n");
    for (addr = addr_remote; addr != NULL; addr = addr->next)
      debug_printf("  %s\n", addr->orig);
    }
  }

/* Do remote deliveries. We must pick off the queue all addresses that have the
same transport, remote destination, and errors_address, and hand them to the
transport in one go, subject to some configured limitations. If this is a run
to continue delivering to an existing delivery channel, skip all but those
addresses that can go to that channel. The skipped addresses just get defered.
*/

DEBUG(2) debug_printf(">>> Remote deliveries >>>\n");

/* Remote deliveries may be handled in parallel by separate subprocesses, up to
a configured maximum number at once. */

parcount = 0;

while (addr_remote != NULL)
  {
  int address_count = 1;
  int address_count_max;
  BOOL multi_domain;
  address_item **anchor = &addr_remote;
  address_item *addr = addr_remote;
  address_item *last = addr;
  address_item *next;

  /* Pull the first address right off the list. */

  addr_remote = addr->next;
  addr->next = NULL;

  /* If no transport has been set, there has been a big screw-up somewhere. */

  if (addr->transport == NULL)
    {
    addr->message = "No transport set by router or director";
    addr->transport_return = DEFER;
    remote_post_process(addr, LOG_MAIN|LOG_PANIC, NULL);
    continue;
    }

  /* If this transport has a setup function, call it now so that it gets
  run in this process and not in any subprocess. That way, the results of
  any setup are usable in all subprocesses. */

  if (addr->transport->setup != NULL)
    {
    (void)((addr->transport->setup)(addr->transport));
    }

  /* Get the flag which specifies whether the transport can handle different
  domains that nevertheless resolve to the same set of hosts, and the maximum
  number of addresses it can handle at once. Zero means unlimited. */

  multi_domain = addr->transport->multi_domain;
  address_count_max = addr->transport->max_addresses;
  if (address_count_max == 0) address_count_max = 999999;

  /* Pick off all addresses which have the same transport, errors address,
  destination, and route_option. In some cases they point to the same host
  list, but we also need to check for identical host lists generated from
  entirely different domains. The host list pointers can be NULL in the case
  where the hosts are defined in the transport. If all addresses have the same
  domain, we can set the $domain expansion variable - configurations can
  arrange this by using the "domain" option, and then being able to look up
  things by domain can be useful. In fact, there is now a flag which can be set
  on a transport that restricts it to handling one domain at a time. There is
  also a configured maximum limit of addresses that can be handled at once. */

  deliver_domain = addr->domain;
  while ((next = *anchor) != NULL && address_count < address_count_max)
    {
    if ((multi_domain || strcmp(next->domain, addr->domain) == 0) &&
        next->transport == addr->transport &&
        next->errors_address == addr->errors_address &&
        same_hosts(next->host_list, addr->host_list) &&
        (next->route_option == addr->route_option ||
          (next->route_option != NULL && addr->route_option != NULL &&
            strcmp(next->route_option, addr->route_option) == 0)))
      {
      *anchor = next->next;
      next->next = NULL;
      last->next = next;
      last = next;
      address_count++;
      if (multi_domain && deliver_domain != NULL &&
        strcmp(next->domain, deliver_domain) != 0)
          deliver_domain = NULL;
      }
    else anchor = &(next->next);
    }

  /* If there is only one address, we can set $local_part. */

  deliver_localpart = (addr->next == NULL)? addr->local_part : NULL;

  /* If this is a run to continue delivery down an already-established
  channel, check that this set of addresses matches the transport and
  the channel. If it does not, defer the addresses. If a host list exists,
  we must check that the continue host is on the list. Otherwise, the
  host is set in the transport, and must therefore of necessity be the
  same for all addresses. */

  if (continue_transport != NULL)
    {
    BOOL ok = strcmp(continue_transport, addr->transport->name) == 0;

    if (ok && addr->host_list != NULL)
      {
      ok = FALSE;
      for (next = addr; !ok && next != NULL; next = next->next)
        {
        host_item *h;
        for (h = next->host_list; h != NULL; h = h->next)
          {
          if (strcmp(h->name, continue_hostname) == 0)
            { ok = TRUE; break; }
          }
        }
      }

    /* Addresses not suitable; defer and skip to next address. */

    if (!ok)
      {
      next = addr;
      while (next->next != NULL) next = next->next;
      next->next = addr_defer;
      addr_defer = addr;
      continue;
      }
    }

  /* Compute the return path, set up the route_option string, make the
  recipients list available for expansion, ensure that the transport is indeed
  a remote one, and then hand it the chain of addresses. */

  return_path = (addr->errors_address != NULL)?
    addr->errors_address : sender_address;
  route_option = addr->route_option;
  deliver_recipients = addr;

  if (addr->transport->info->local)
    log_write(0, LOG_PANIC_DIE, "Attempt non-local delivery with local "
      "transport for %s: transport %s", addr->orig, addr->transport->name);

  /* The transports set up the process info themselves as they may connect
  to more than one remote machine. */

  #ifdef TRANSPORT_DEBUG
  if (debug_transport != NULL)
    (debug_transport->code)(addr->transport, addr);
  else
  #endif

  /* If parallel delivery is disabled, or if there are no outstanding delivery
  subprocesses, and this is the final delivery to be made, then do the delivery
  in this process. */

  if (remote_max_parallel < 2 || (parcount == 0 && addr_remote == NULL))
    {
    (addr->transport->info->code)(addr->transport, addr);
    set_process_info("delivering %s (just run %s for %s%s)", id,
      addr->transport->name, addr->orig, (addr->next == NULL)? "" : ", ...");
    remote_post_process(addr, LOG_MAIN, NULL);
    }

  /* Parallel delivery is required. Create up to the configured number
  of subprocesses, each of which must pass back the delivery state via
  a pipe. */

  else
    {
    pid_t pid;
    int poffset;
    int pfd[2];

    /* If the maximum number of subprocesses already exist, wait for
    one to finish. If we can't find one, there is some shambles. Better
    not bomb out, as that might lead to multiple copies of the message.
    Just log and assume all done. */

    while (parcount >= remote_max_parallel)
      {
      address_item *doneaddr = par_wait();
      if (doneaddr == NULL)
        {
        log_write(0, LOG_MAIN|LOG_PANIC,
          "remote delivery process count got out of step");
        parcount = 0;
        }
      else remote_post_process(doneaddr, LOG_MAIN, NULL);
      }

    /* If the data for keeping a list of processes hasn't yet been
    set up, do so. */

    if (parlist == NULL)
      {
      parlist = store_malloc(remote_max_parallel * sizeof(pardata));
      for (poffset = 0; poffset < remote_max_parallel; poffset++)
        parlist[poffset].pid = 0;
      }

    /* Find a free slot in the pardata list */

    for (poffset = 0; poffset < remote_max_parallel; poffset++)
      if (parlist[poffset].pid == 0) break;

    /* If there isn't one, there has been a horrible disaster. */

    if (poffset >= remote_max_parallel)
      {
      remote_post_process(addr, LOG_MAIN|LOG_PANIC,
        "Unexpectedly no free subprocess slot");
      continue;
      }

    /* Create the pipe for inter-process communication. */

    if (pipe(pfd) != 0)
      {
      remote_post_process(addr, LOG_MAIN|LOG_PANIC,
        string_sprintf("unable to create pipe: %s", strerror(errno)));
      continue;
      }

    /* Now fork a sub-process to do the remote delivery. */

    if ((pid = fork()) == 0)
      {
      int fd = pfd[pipe_write];
      host_item *h;
      retry_item *r;
      debug_pid = getpid();
      DEBUG(1) debug_printf("Remote delivery process started\n");

      /* Close the unwanted half of the pipe, set the process state,
      and run the transport. */

      close(pfd[pipe_read]);
      set_process_info("delivering %s to %s using %s", message_id,
        addr->route_domain, addr->transport->name);

      (addr->transport->info->code)(addr->transport, addr);

      /* Pass the result back down the pipe. This is a lot more information
      that is needed for a local delivery. We have to send back the error
      status for each address, the usability status for each host that is
      flagged as unusable, and all the retry items. Each type of information
      is flagged by an identifying byte, and is then in a fixed format (with
      strings terminated by zeros), and there is a final terminator at the
      end. The host information and retry information is all attached to
      the first address, so that gets sent at the start. */

      /* Host unusability information: for most success cases this will
      be null. */

      for (h = addr->host_list; h != NULL; h = h->next)
        {
        if (h->address == NULL || h->status < hstatus_unusable) continue;
        sprintf(big_buffer, "H%c%c%s", h->status, h->why, h->address);
        write(fd, big_buffer, (int)strlen(big_buffer+3) + 4);
        }

      /* Retry information: for most success cases this will be null. */

      for (r = addr->retries; r != NULL; r = r->next)
        {
        char *ptr;
        sprintf(big_buffer, "R%c%s", r->delete, r->destination);
        ptr = big_buffer + (int)strlen(big_buffer+2) + 3;
        memcpy(ptr, &(r->basic_errno), sizeof(r->basic_errno));
        ptr += sizeof(r->basic_errno);
        memcpy(ptr, &(r->more_errno), sizeof(r->more_errno));
        ptr += sizeof(r->more_errno);
        if (r->message == NULL) *ptr++ = 0; else
          {
          strcpy(ptr, r->message);
          while(*ptr++);
          }
        write(fd, big_buffer, ptr - big_buffer);
        }

      /* Information about what happened to each address */

      for(; addr != NULL; addr = addr->next)
        {
        char *ptr = big_buffer + 3;
        sprintf(big_buffer, "A%c%c", addr->transport_return,
          addr->special_action);
        memcpy(ptr, &(addr->basic_errno), sizeof(addr->basic_errno));
        ptr += sizeof(addr->basic_errno);
        memcpy(ptr, &(addr->basic_errno), sizeof(addr->more_errno));
        ptr += sizeof(addr->more_errno);
        if (addr->message == NULL) *ptr++ = 0; else
          {
          strcpy(ptr, addr->message);
          while(*ptr++);
          }
        if (addr->transported == NULL) *ptr++ = 0; else
          {
          strcpy(ptr, addr->transported->name);
          while(*ptr++);
          strcpy(ptr, addr->transported->address);
          while(*ptr++);
          }
        write(fd, big_buffer, ptr - big_buffer);
        }

      /* Add termination flag, close the pipe, and that's it. */

      write(fd, "Z", 1);
      close(fd);
      exit(EXIT_SUCCESS);
      }

    /* Fork failed; defer with error message */

    if (pid <= 0)
      {
      remote_post_process(addr, LOG_MAIN|LOG_PANIC,
        string_sprintf("fork failed for remote delivery to %s: %s",
          addr->route_domain, strerror(errno)));
      continue;
      }

    /* Fork succeeded; close unwanted half of pipe, increment the count, and
    remember relevant data for when the process finishes. */

    close(pfd[pipe_write]);
    parcount++;
    parlist[poffset].addrlist = addr;
    parlist[poffset].pid = pid;
    parlist[poffset].fd = pfd[pipe_read];
    }
  }

/* If we are doing parallel deliveries, there will be subprocesses still
running when we get to this point. Wait for them and post-process their
addresses. */

while (parcount > 0)
  {
  address_item *doneaddr = par_wait();
  if (doneaddr == NULL)
    {
    log_write(0, LOG_MAIN|LOG_PANIC,
      "remote delivery process count got out of step");
    parcount = 0;
    }
  else remote_post_process(doneaddr, LOG_MAIN, NULL);
  }

DEBUG(9) debug_printf("tidying up after delivering %s\n", message_id);
set_process_info("tidying up after delivering %s", message_id);

/* Finished with the message log. */

fclose(message_log);

/* Next we must update the retry database. We do this in one fell swoop at the
end in order not to keep opening and closing (and locking) the database. The
code for handling retries is hived off into a separate module for convenience.
We pass it the addresses of the various chains, because deferred addresses can
get moved onto the failed chain if the retry cutoff time has expired for all
alternative destinations. */

retry_update(&addr_defer, &addr_failed, &addr_succeed);

/* If there are any deferred addresses, see if this message has been on the
queue for longer than the maximum retry timeout for any retry rule. This can
happen if there are lots of messages for one domain, one or two of which
occasionally get through, resetting the retry clock. Though individual
addresses get failed if they get tried and the message has been around longer
than their timeout, if there are sufficiently many messsages on the queue, some
may not get tried for a very long time, as they get skipped after a young
message has been failed. As an ultimate backstop, fail any message that has
been on the queue longer than the longest retry time of any retry rule. */

if (now - received_time > retry_maximum_timeout)
  {
  while (addr_defer != NULL)
    {
    address_item *addr = addr_defer;
    addr_defer = addr->next;
    addr->next = addr_failed;
    addr_failed = addr;
    addr->retry_timedout = TRUE;
    addr->message = (addr->message == NULL)? "message retry timeout exceeded" :
      string_sprintf("%s: message retry timeout exceeded", addr->message);
    log_write(0, LOG_MAIN, "** %s: message retry timeout exceeded", addr->orig);
    }
  }

/* If any addresses failed, we must send a message to somebody, unless
ignore_error is set, in which case no action is taken. Simple delivery
failures go back to the sender; other cases may go to mailmaster or to an
address owner. It is possible for several messages to get sent if there are
addresses with different requirements. */

while (addr_failed != NULL)
  {
  pid_t pid;
  int fd;
  address_item *addr;
  address_item *handled_addr = NULL;
  address_item **paddr;
  address_item *msgchain = NULL;
  address_item **pmsgchain = &msgchain;

  char *recipient = (addr_failed->errors_address == NULL)?
    ((sender_address[0] == 0)? errors_address : sender_address) :
    addr_failed->errors_address;

  /* If this is an error delivery message for which there is no other
  errors address, and it has been failed because of a retry timeout, ignore it.
  This case happens when a delivery error message times out after (auto)
  thawing, since the normal handling of failures for such messages is to defer
  and freeze them. */

  if (addr_failed->retry_timedout && sender_address[0] == 0 &&
    recipient == errors_address)
      addr_failed->ignore_error = TRUE;

  /* If the first address on the list has ignore_error set, just remove
  it from the list, throw away any saved message file, log it, and
  mark the recipient done. */

  if (addr_failed->ignore_error)
    {
    addr = addr_failed;
    addr_failed = addr->next;
    if (addr->return_filename != NULL) unlink(addr->return_filename);

    log_write(0, LOG_MAIN, "%s%s%s%s: error ignored",
      addr->orig,
      (addr->parent == NULL)? "" : " <",
      (addr->parent == NULL)? "" : addr->parent->orig,
      (addr->parent == NULL)? "" : ">");

    tree_add_nonrecipient(addr->unique, addr->pfr);
    if (spool_write_header(message_id) <= 0)
      log_write(0, LOG_MAIN|LOG_PANIC_DIE, "unable to update spool header "
        "file for %s", message_id);
    }

  /* Otherwise, handle the sending of a message. Find the error address for
  the first address, then send a message that includes all failed addresses
  that have the same error address. */

  else
    {
    /* Make a subprocess to send a message */

    pid = child_open(mailer_argv, NULL, 077, NULL, NULL, &fd,
      (debug_file != NULL)? fileno(debug_file) : -1);

    /* Creation of child failed */

    if (pid < 0)
      log_write(0, LOG_MAIN|LOG_PANIC_DIE, "Failed to create child process to send "
        "failure message");

    /* Creation of child succeeded */

    else
      {
      int ch, rc;
      int filecount = 0;
      char *bcc;
      FILE *f = fdopen(fd, "w");
      if (errors_reply_to != NULL)
        fprintf(f, "Reply-to: %s\n", errors_reply_to);
      fprintf(f, "From: Mail Delivery System <Mailer-Daemon@%s>\n",
        qualify_domain_sender);
      fprintf(f, "To: %s\n", recipient);

      DEBUG(9) debug_printf("sending error message to: %s\n", recipient);

      /* Quietly copy to configured additional addresses if required. */

      bcc = moan_check_errorcopy(recipient);
      if (bcc != NULL)
        {
        fprintf(f, "Bcc: %s\n", bcc);
        store_free(bcc);
        }

      if (strcmpic(sender_address, recipient) == 0)
        fprintf(f, "Subject: Mail delivery failed: returning message to sender\n\n"
"This message was created automatically by mail delivery software.\n\n"
"A message that you sent could not be delivered to all of its recipients. The\n"
"following address(es) failed:\n\n");
      else
        fprintf(f, "Subject: Mail delivery failed\n\n"
"This message was created automatically by mail delivery software.\n\n"
"A message sent by\n\n  <%s>\n\n"
"could not be delivered to all of its recipients. The following address(es)\n"
"failed:\n\n", sender_address);

      /* Scan the addresses for all that have the same errors_address, removing
      them from the addr_failed chain, but putting them on the msgchain if
      they have a file name for a return message. (There has already been a
      check in post_process_one() for the existence of data in the message
      file.) */

      paddr = &addr_failed;
      for (addr = addr_failed; addr != NULL; addr = *paddr)
        {
        if (strcmp(recipient, (addr->errors_address == NULL)?
            ((sender_address[0] == 0)? errors_address : sender_address) :
              addr->errors_address) != 0 || addr->ignore_error)
          {
          paddr = &(addr->next);
          continue;
          }
        *paddr = addr->next;

        /* Output this address and its error message. The message field may
        be quite long in some cases. To cope with this we split and indent
        it at any colon characters. It will have been turned into printing
        characters - also split at any occurrences of "\n". These can occur
        in multi-line SMTP error responses. */

        if (addr->pfr && addr->parent != NULL)
          {
          fprintf(f, "  %s:\n    ", string_printing(addr->parent->orig, FALSE));
          if (addr->orig[0] == '>')
            fprintf(f, "generated mail to %s:\n    ", addr->orig + 1);
          else
            fprintf(f, "generated %s:\n    ", addr->orig);
          }
        else
          {
          fprintf(f, "  %s:\n    ", string_printing(addr->orig, FALSE));
          if (addr->parent != NULL &&
              strcmpic(addr->parent->orig, addr->orig) != 0)
            fprintf(f, "(generated from %s):\n    ",
              string_printing(addr->parent->orig, FALSE));
          }

        if (addr->basic_errno > 0)
          fprintf(f, "%s:\n    ", strerror(addr->basic_errno));

        if (addr->message == NULL)
          { if (addr->basic_errno <= 0) fprintf(f, "unknown error\n"); }
        else
          {
          char *s = addr->message;
          while (*s != 0)
            {
            if (*s == '\\' && s[1] == 'n')
              {
              fprintf(f, "\n    ");
              s += 2;
              }
            else
              {
              fputc(*s, f);
              if (*s++ == ':' && isspace(*s)) fprintf(f, "\n   ");
              }
            }
          fputc('\n', f);
          }

        /* Add to msgchain if there's a return file. */

        if (addr->return_filename != NULL)
          {
          *pmsgchain = addr;
          addr->next = NULL;
          pmsgchain = &(addr->next);
          filecount++;
          }

        /* Else save so that we can tick off the recipient when the
        message is sent. */

        else
          {
          addr->next = handled_addr;
          handled_addr = addr;
          }
        }

      /* If there were any file messages passed by the local transports, include
      them in the message. Then put the address on the handled chain. */

      if (msgchain != NULL)
        {
        address_item *nextaddr;
        fprintf(f,
          "\nThe following text was generated during the delivery attempt%s:\n",
          (filecount > 1)? "s" : "");
        for (addr = msgchain; addr != NULL; addr = nextaddr)
          {
          FILE *fm = fopen(addr->return_filename, "r");
          int save_errno = errno;
          fprintf(f, "\n------ %s%s%s ------\n\n",
            string_printing(addr->local_part, FALSE),
           addr->pfr? "" : "@",
           addr->pfr? "" : addr->domain);
          if (fm == NULL)
            fprintf(f, "    +++ Exim error... failed to open text file: %s\n",
              strerror(save_errno));
          else
            {
            while ((ch = fgetc(fm)) != EOF) fputc(ch, f);
            fclose(fm);
            }
          unlink(addr->return_filename);

          /* Can now add to handled chain, first fishing off the next
          address on the msgchain. */

          nextaddr = addr->next;
          addr->next = handled_addr;
          handled_addr = addr;
          }
        }

      /* Now copy the message, trying to give an intelligible comment if
      it is too long for it all to be copied. The limit isn't strictly
      applied because of the buffering. */

      if (return_size_limit > 0)
        {
        struct stat statbuf;
        int max = (return_size_limit/DELIVER_BUFFER_SIZE + 1)*DELIVER_BUFFER_SIZE;
        if (fstat(deliver_datafile, &statbuf) == 0 && statbuf.st_size > max)
          {
          fprintf(f, "\n"
"------ This is a copy of the message, including all the headers.\n"
"------ The body of the message is %d characters long; only the first\n"
"------ %d or so are included here.\n\n", (int)statbuf.st_size, (max/1000)*1000);
          }
        else fprintf(f, "\n"
"------ This is a copy of the message, including all the headers. ------\n\n");
        }
      else fprintf(f, "\n"
"------ This is a copy of the message, including all the headers. ------\n\n");

      fflush(f);
      transport_write_message(NULL, fileno(f), topt_add_return_path, NULL,
        return_size_limit);

      /* Close the file, which should send an EOF to the child process
      that is receiving the message. Wait for it to finish. */

      fclose(f);
      rc = child_close(pid, 0);     /* Waits for child to close, no timeout */

      /* If the process failed, there was some disaster in setting up the
      error message. Ensure that addr_defer is non-null, which will have
      the effect of leaving the message on the spool. The failed addresses
      will get tried again next time. */

      if (rc != 0)
        {
        log_write(0, LOG_MAIN, "Process failed (%d) when writing error message "
          "to %s", rc, recipient);
        if (addr_defer == NULL) addr_defer = (address_item *)(+1);
        }

      /* The message succeeded. Ensure that the recipients that failed are
      now marked finished with on the spool and their parents updated. */

      else
        {
        char *logtod = tod_stamp(tod_log);
        log_write(5, LOG_MAIN, "Error message sent to %s", recipient);
        for (addr = handled_addr; addr != NULL; addr = addr->next)
          {
          address_item *dup;

          tree_add_nonrecipient(addr->unique, addr->pfr);
          if (addr->local && addr->parent == NULL)
            tree_add_nonrecipient(addr->orig, addr->pfr);
          (void)child_done(addr, logtod);

          /* Check the list of duplicate addresses and ensure they are now
          marked done as well. Also their parents. */

          for (dup = addr_duplicate; dup != NULL; dup = dup->next)
            {
            if (strcmp(addr->unique, dup->unique) == 0)
              {
              tree_add_nonrecipient(dup->orig, dup->pfr);
              (void)child_done(dup, logtod);
              }
            }
          }

        if (spool_write_header(message_id) <= 0)
          log_write(0, LOG_MAIN|LOG_PANIC_DIE, "unable to update spool header "
            "file for %s", message_id);
        }
      }
    }
  }

/* If delivery was frozen and the mailmaster wants to be told, generate
an appropriate message, unless the message is a local error message - to
prevent loops - or any message that is addressed to the local mailmaster. */

if (deliver_freeze && freeze_tell_mailmaster && !local_error_message)
  {
  BOOL moan = TRUE;
  int i;
  for (i = 0; i < recipients_count; i++)
    {
    if (strcmp(recipients_list[i], errors_address) == 0)
      {
      moan = FALSE;
      break;
      }
    }

  if (moan) moan_tell_mailmaster(addr_defer, "Message frozen",
    "Message %s has been frozen. The sender is <%s>.\n", message_id,
    sender_address);
  }


/* If there are now no deferred addresses, we are done. Preserve the
message log if so configured. */

if (addr_defer == NULL)
  {
  int rc;
  sprintf(spoolname, "%s/msglog/%s", spool_directory, id);
  if (preserve_message_logs)
    {
    sprintf(big_buffer, "%s/msglog.OLD/%s", spool_directory, id);
    if ((rc = rename(spoolname, big_buffer)) < 0)
      {
      directory_make(spool_directory, "msglog.OLD", MSGLOG_DIRECTORY_MODE);
      rc = rename(spoolname, big_buffer);
      }
    if (rc < 0)
      log_write(0, LOG_PANIC_DIE, "failed to move %s to the msglog.OLD "
        "directory", spoolname);
    }
  else
    {
    if (unlink(spoolname) < 0)
      log_write(0, LOG_PANIC_DIE, "failed to unlink %s", spoolname);
    }

  /* Remove the two message files. */

  sprintf(spoolname, "%s/input/%s-D", spool_directory, id);
  if (unlink(spoolname) < 0)
    log_write(0, LOG_PANIC_DIE, "failed to unlink %s", spoolname);
  sprintf(spoolname, "%s/input/%s-H", spool_directory, id);
  if (unlink(spoolname) < 0)
    log_write(0, LOG_PANIC_DIE, "failed to unlink %s", spoolname);
  log_write(0, LOG_MAIN, "Completed");
  }

/* Otherwise, lose any temporary files that were catching output from pipes for
any of the deferred addresses, and see if the message has been on the queue for
so long that it is time to send a warning message to the sender, unless it is a
mailer-daemon. If we can't make a process to send the message, don't worry.

For mailing list expansions we want to send the warning message to the
mailing list manager. We can't do a perfect job here, as some addresses may
have different errors addresses, but if we take the errors address from
each deferred address it will probably be right in most cases. */

else
  {
  address_item *addr;
  char *recipients = "";

  for (addr = addr_defer; addr != NULL; addr = addr->next)
    {
    if (addr->return_filename != NULL) unlink(addr->return_filename);

    if (sender_address[0] != 0)
      {
      if (addr->errors_address == NULL)
        {
        if (strstr(recipients, sender_address) == NULL)
          recipients = string_sprintf("%s%s%s", recipients,
            (recipients[0] == 0)? "" : ",", sender_address);
        }
      else
        {
        if (strstr(recipients, addr->errors_address) == NULL)
          recipients = string_sprintf("%s%s%s", recipients,
            (recipients[0] == 0)? "" : ",", addr->errors_address);
        }
      }
    }

  /* Now the warning stuff */

  if (delay_warning > 0 && sender_address[0] != 0)
    {
    int queue_time = time(NULL) - received_time;
    int count = queue_time/delay_warning;

    if (warning_count < count)
      {
      header_line *h;
      int fd;
      pid_t pid = child_open(mailer_argv, NULL, 077, NULL, NULL, &fd,
        (debug_file != NULL)? fileno(debug_file) : -1);

      if (pid > 0)
        {
        FILE *f = fdopen(fd, "w");
        if (errors_reply_to != NULL)
          fprintf(f, "Reply-to: %s\n", errors_reply_to);
        fprintf(f, "From: Mail Delivery System <Mailer-Daemon@%s>\n",
          qualify_domain_sender);
        fprintf(f, "To: %s\n", recipients);
        fprintf(f, "Subject: Warning: message %s delayed\n\n", message_id);
        fprintf(f,
"This message was created automatically by mail delivery software.\n\n");
        if (strcmp(recipients, sender_address) == 0)
          fprintf(f,
"A message that you sent has not yet been delivered to all its recipients\n"
"after more than ");
        else fprintf(f,
"A message sent by\n\n  <%s>\n\n"
"has not yet been delivered to all its recipients after more than \n",
        sender_address);
        queue_time = count * delay_warning;
        if (queue_time < 120*60) fprintf(f, "%d minutes", queue_time/60);
          else fprintf(f, "%d hours", queue_time/3600);
        fprintf(f, " on the queue on %s.\n\n", primary_hostname);

        fprintf(f, "The message identifier is:     %s\n", message_id);

        for (h = header_list; h != NULL; h = h->next)
          {
          if (strncmpic(h->text, "Subject:", 8) == 0)
            fprintf(f, "The subject of the message is: %s", h->text + 9);
          else if (strncmpic(h->text, "Date:", 5) == 0)
            fprintf(f, "The date of the message is:    %s", h->text + 6);
          }
        fprintf(f, "\n");

        fprintf(f, "The address%s to which the message has not yet been "
          "delivered %s:\n\n",
          (addr_defer->next == NULL)? "" : "es",
          (addr_defer->next == NULL)? "is": "are");

        while (addr_defer != NULL)
          {
          address_item *addr = addr_defer;
          addr_defer = addr->next;
          fprintf(f, "  %s\n", string_printing(addr->orig, FALSE));
          }

        fprintf(f,
"\nNo action is required on your part. Delivery attempts will continue for\n"
"some time, and this message will be repeated at intervals if the message\n"
"remains undelivered. Eventually the mail delivery software will give up,\n"
"and when that happens, the message will be returned to you.\n");

        /* Close and wait for child process to complete, without a timeout.
        If there's an error, don't update the count. */

        fclose(f);
        if (child_close(pid, 0) == 0)
          {
          warning_count = count;
          update_spool = TRUE;    /* Ensure spool rewritten */
          }
        }
      }
    }

  /* If there have been any updates to the non-recipients list, or other things
  that get written to the spool, we must now update the spool header file so
  that it has the right information for the next delivery attempt. If there
  was more than one address being delivered, the header_change update is done
  earlier, in case one succeeds and then something crashes. */

  DEBUG(9)
    debug_printf("delivery defered: update_spool=%d header_changed=%d\n",
      update_spool, header_changed);

  if ((update_spool || header_changed) && spool_write_header(message_id) <= 0)
    log_write(0, LOG_MAIN|LOG_PANIC_DIE, "unable to update spool header "
      "file for %s", message_id);
  }

/* Now we can close and remove the journal file. Its only purpose is to record
successfully completed deliveries asap so that this information doesn't get
lost if Exim (or the machine) crashes. Forgetting about a failed delivery is
not serious, as trying it again is nor harmful. The journal might not be open
if all addresses were deferred at routing or directing. Nevertheless, we must
remove it if it exists (may have been lying around from a crash during the
previous delivery attempt). We don't remove the journal if a delivery
subprocess failed to pass back delivery information; this is controlled by
the remove_journal flag. */

if (journal_fd >= 0) close(journal_fd);
if (remove_journal)
  {
  sprintf(spoolname, "%s/input/%s-J", spool_directory, id);
  if (unlink(spoolname) < 0 && errno != ENOENT)
    log_write(0, LOG_PANIC_DIE, "failed to unlink %s: %s", spoolname,
      strerror(errno));
  }

/* Closing the data file frees the lock; if the file has been unlinked it
will go away. Otherwise the message becomes available for another process
to try delivery. */

close(deliver_datafile);
deliver_datafile = -1;
DEBUG(2) debug_printf("end delivery of %s\n", id);
return OK;
}

/* End of deliver.c */
