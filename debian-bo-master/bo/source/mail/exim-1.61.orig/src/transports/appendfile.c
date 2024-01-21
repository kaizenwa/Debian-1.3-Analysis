/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "../exim.h"
#include "appendfile.h"



/* Options specific to the appendfile transport. They must be in alphabetic
order (note that "_" comes before the lower case letters). Some of them are
stored in the publicly visible instance block - these are flagged with the
opt_public flag. */

optionlist appendfile_transport_options[] = {
  { "*expand_group",     opt_stringptr | opt_hidden | opt_public,
      (void *)(offsetof(transport_instance, expand_gid)) },
  { "*expand_user",      opt_stringptr | opt_hidden | opt_public,
      (void *)(offsetof(transport_instance, expand_uid)) },
  { "*set_group",         opt_bool | opt_hidden | opt_public,
      (void *)(offsetof(transport_instance, gid_set)) },
  { "*set_user",          opt_bool | opt_hidden | opt_public,
      (void *)(offsetof(transport_instance, uid_set)) },
  { "allow_symlink",     opt_bool,
      (void *)(offsetof(appendfile_transport_options_block, allow_symlink)) },
  { "batch",             opt_local_batch | opt_public,
      (void *)(offsetof(transport_instance, local_batch)) },
  { "batch_max",         opt_int | opt_public,
      (void *)(offsetof(transport_instance, batch_max)) },
  { "bsmtp",             opt_local_batch | opt_public,
      (void *)(offsetof(transport_instance, local_smtp)) },
  { "check_group",       opt_bool,
      (void *)(offsetof(appendfile_transport_options_block, check_group)) },
  { "create_directory",  opt_bool,
      (void *)(offsetof(appendfile_transport_options_block, create_directory)) },
  { "create_file",       opt_stringptr,
      (void *)(offsetof(appendfile_transport_options_block, create_file_string)) },
  { "current_directory", opt_stringptr | opt_public,
      (void *)(offsetof(transport_instance, current_dir)) },
  { "delivery_date_add", opt_bool,
      (void *)(offsetof(appendfile_transport_options_block, delivery_date_add)) },
  { "directory",         opt_stringptr,
      (void *)(offsetof(appendfile_transport_options_block, dirname)) },
  { "directory_mode",    opt_octint,
      (void *)(offsetof(appendfile_transport_options_block, dirmode)) },
  { "envelope_to_add",   opt_bool,
      (void *)(offsetof(appendfile_transport_options_block, envelope_to_add)) },
  { "file",              opt_stringptr,
      (void *)(offsetof(appendfile_transport_options_block, filename)) },
  { "file_must_exist",   opt_bool,
      (void *)(offsetof(appendfile_transport_options_block, file_must_exist)) },
  { "from_hack",         opt_bool,
      (void *)(offsetof(appendfile_transport_options_block, from_hack)) },
  { "group",             opt_expand_gid | opt_public,
      (void *)(offsetof(transport_instance, gid)) },
  { "lock_interval",     opt_time,
      (void *)(offsetof(appendfile_transport_options_block, lock_interval)) },
  { "lock_retries",      opt_int,
      (void *)(offsetof(appendfile_transport_options_block, lock_retries)) },
  { "lockfile_mode",     opt_octint,
      (void *)(offsetof(appendfile_transport_options_block, lockfile_mode)) },
  { "lockfile_timeout",  opt_time,
      (void *)(offsetof(appendfile_transport_options_block, lockfile_timeout)) },
  { "mode",              opt_octint,
      (void *)(offsetof(appendfile_transport_options_block, mode)) },
  { "notify_comsat",     opt_bool,
      (void *)(offsetof(appendfile_transport_options_block, notify_comsat)) },
  { "prefix",            opt_stringptr,
      (void *)(offsetof(appendfile_transport_options_block, prefix)) },
  { "quota",             opt_stringptr,
      (void *)(offsetof(appendfile_transport_options_block, quota)) },
  { "require_lockfile",  opt_bool,
      (void *)(offsetof(appendfile_transport_options_block, require_lockfile)) },
  { "retry_use_local_part", opt_bool | opt_public,
      (void *)offsetof(transport_instance, retry_use_local_part) },
  { "return_path_add",   opt_bool,
      (void *)(offsetof(appendfile_transport_options_block, return_path_add)) },
  { "suffix",            opt_stringptr,
      (void *)(offsetof(appendfile_transport_options_block, suffix)) },
  { "use_lockfile",      opt_bool,
      (void *)(offsetof(appendfile_transport_options_block, use_lockfile)) },
  { "user",              opt_expand_uid | opt_public,
      (void *)(offsetof(transport_instance, uid)) },
};

/* Size of the options list. An extern variable has to be used so that its
address can appear in the tables drtables.c. */

int appendfile_transport_options_count =
  sizeof(appendfile_transport_options)/sizeof(optionlist);

/* Default private options block for the appendfile transport. */

appendfile_transport_options_block appendfile_transport_option_defaults = {
  NULL,           /* file name */
  NULL,           /* dir name */
  "From ${if def:return_path{$return_path}{MAILER-DAEMON}} ${tod_bsdinbox}\n",
                  /* prefix */
  "\n",           /* suffix */
  "anywhere",     /* create_file_string (string value for create_file) */
  NULL,           /* quota */
  0,              /* quota_value */
  0600,           /* mode */
  0700,           /* dirmode */
  0600,           /* lockfile_mode */
  30*60,          /* lockfile_timeout */
  10,             /* lock_retries */
   3,             /* lock_interval */
  create_anywhere,/* create_file */
  FALSE,          /* allow_symlink */
  FALSE,          /* check_group */
  TRUE,           /* create_directory */
  FALSE,          /* notify_comsat */
  TRUE,           /* require_lockfile */
  TRUE,           /* use_lockfile */
  TRUE,           /* from_hack */
  TRUE,           /* return_path_add */
  TRUE,           /* delivery_date_add */
  TRUE,           /* envelope_to_add */
  FALSE           /* file_must_exist */
};



/*************************************************
*              Setup entry point                 *
*************************************************/

/* Called for each delivery in the privileged state, just before the uid/gid
are changed and the main entry point is called. We use this function to
expand any quota setting, so that it can access files that may not be readable
by the user.

Argument:   tblock   points to the transport instance
Returns:             NULL if OK, pointer to error string if not
*/

char *
appendfile_transport_setup(transport_instance *tblock)
{
appendfile_transport_options_block *ob =
  (appendfile_transport_options_block *)(tblock->options_block);
double d;
char *s, *rest;

if (ob->quota == NULL)
  {
  ob->quota_value = 0;
  return NULL;
  }

s = expand_string(ob->quota);
if (s == NULL)
  return string_sprintf("Expansion of \"%s\" (quota setting "
    "for %s transport) failed: %s", ob->quota, tblock->name,
    expand_string_message);

d = strtod(s, &rest);
if (tolower(*rest) == 'k') { d *= 1024.0; rest++; }
if (tolower(*rest) == 'm') { d *= 1024.0*1024.0; rest++; }
while (isspace(*rest)) rest++;

if (*rest != 0)
  return string_sprintf("Malformed quota setting \"%s\" for "
    "%s transport", s, tblock->name);

ob->quota_value = (int)d;
return NULL;
}



/*************************************************
*          Initialization entry point            *
*************************************************/

/* Called for each instance, after its options have been read, to
enable consistency checks to be done, or anything else that needs
to be set up. */

void
appendfile_transport_init(transport_instance *tblock)
{
appendfile_transport_options_block *ob =
  (appendfile_transport_options_block *)(tblock->options_block);

/* Set up the setup entry point, to be called in the privileged state */

tblock->setup = appendfile_transport_setup;

/* Retry_use_local_part defaults TRUE if unset */

if (tblock->retry_use_local_part == 2) tblock->retry_use_local_part = TRUE;

/* Lock_retries must be greater than zero */

if (ob->lock_retries == 0) ob->lock_retries = 1;

/* Only one of a file name or directory name must be given. */

if (ob->filename != NULL && ob->dirname != NULL)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s transport:\n  "
  "only one of \"file\" or \"directory\" can be specified", tblock->name);

/* If a file name was specified, it must be an absolute path. Can check here
only if there are no expansions. */

if (ob->filename != NULL && ob->filename[0] != '/' && ob->filename[0] != '$')
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s transport:\n  "
  "the file option must specify an absolute path", tblock->name);

/* If a directory name was specified, it must be an absolute path. Can check
here only if there are no expansions. */

if (ob->dirname != NULL && ob->dirname[0] != '/' && ob->dirname[0] != '$')
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s transport:\n  "
  "the directory option must specify an absolute path", tblock->name);

/* If a fixed uid field is set, then a gid field must also be set. */

if (tblock->uid_set && !tblock->gid_set)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
    "user set without group for the %s transport", tblock->name);

/* If "create_file" is set, check that a valid option is given, and set the
integer variable. */

if (ob->create_file_string != NULL)
  {
  int value = 0;
  if (strcmp(ob->create_file_string, "anywhere") == 0) value = create_anywhere;
  else if (strcmp(ob->create_file_string, "belowhome") == 0) value =
    create_belowhome;
  else if (strcmp(ob->create_file_string, "inhome") == 0)
    value = create_inhome;
  else
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "invalid value given for \"file_create\" for the %s transport: %s",
        tblock->name, ob->create_file_string);
  ob->create_file = value;
  }

/* If batch SMTP is set, ensure the generic local batch option matches. */

if (tblock->local_smtp != local_batch_off)
  tblock->local_batch = tblock->local_smtp;
}



/*************************************************
*                  Notify comsat                 *
*************************************************/

/* The comsat daemon is the thing that provides asynchronous notification of
the arrival of local messages, if requested by the user by "biff y". It is a
BSD thing that uses a TCP/IP protocol for communication. A message consisting
of the text "user@offset" must be sent, where offset is the place in the
mailbox where new mail starts. There is no scope for telling it which file to
look at, which makes it a less than useful if mail is being delivered into a
non-standard place such as the user's home directory.

Arguments:
  user       user name
  offset     offset in mailbox

Returns:     nothing
*/

static void
notify_comsat(char *user, int offset)
{
int fd;
struct sockaddr_in sa;
struct hostent *hp;
struct servent *sp;
char buffer[256];

DEBUG(2) debug_printf("notify_comsat called\n");

if ((hp = gethostbyname("localhost")) == NULL)
  {
  DEBUG(2) debug_printf("\"localhost\" unknown\n");
  return;
  }

if (hp->h_addrtype != AF_INET)
  {
  DEBUG(2) debug_printf("local host not TCP/IP\n");
  return;
  }

if ((sp = getservbyname("biff", "udp")) == NULL)
  {
  DEBUG(2) debug_printf("biff/udp is an unknown service");
  return;
  }

sa.sin_port = sp->s_port;
sa.sin_family = hp->h_addrtype;
memcpy(&sa.sin_addr, hp->h_addr, hp->h_length);

if ((fd = socket(AF_INET, SOCK_DGRAM, 0)) == -1)
  {
  DEBUG(2) debug_printf("failed to create comsat socket: %s\n",
    strerror(errno));
  return;
  }

sprintf(buffer, "%s@%d\n", user, offset);
if (sendto(fd, buffer, (int)strlen(buffer) + 1, 0, (struct sockaddr *)(&sa),
    sizeof(sa)) < 0)
  {
  DEBUG(2) debug_printf("send to comsat failed: %s\n", strerror(errno));
  }

close(fd);
}



/*************************************************
*       Write block and check for errors         *
*************************************************/

/* On failing to write the given number of bytes, but having written
some, try to write the rest in order to force an over-quota error to
appear. Otherwise no error is set.

Arguments:
  fd        file descriptor to write to
  buffer    pointer to block
  count     number of bytes

Returns:    TRUE if write successful
*/

static BOOL
write_block(int fd, char *buffer, int count)
{
int rc;
if ((rc = write(fd, buffer, count)) != count &&
    (rc < 0 || write(fd, buffer+rc, count-rc) != count-rc)) return FALSE;
return TRUE;
}



/*************************************************
*             Write formatted string             *
*************************************************/

/*
Arguments:
  fd          file descriptor
  format      string format
  ...         arguments for format

Returns:      the yield of write_block()
*/

static BOOL
write_string(int fd, char *format, ...)
{
va_list ap;
va_start(ap, format);
vsprintf(big_buffer, format, ap);
va_end(ap);
return write_block(fd, big_buffer, strlen(big_buffer));
}



/*************************************************
*              Main entry point                  *
*************************************************/

/* See local README for general interface details.

Appendfile delivery is tricky and has led to various security problems in other
mailers. The logic used here is therefore laid out in some detail. When this
function is called, we are running in a subprocess which has had its gid and
uid set to the appropriate values. Therefore, we cannot write directly to the
exim logs. Any errors must be handled by setting appropriate return codes.
Note that the default setting for addr->transport_return is DEFER, so it need
not be set unless some other value is required.

(1)  If the addr->pfr flag is set and the local_part field starts with '/' then
     this is a delivery to a file after .forward or alias expansion. Otherwise,
     there must be a configured file name or directory name.

The following items apply in the case when a file name (as opposed to a
directory name) is given:

(2f) If the file name to be expanded contains a reference to $local_part,
     check that the user name doesn't contain a / because that would cause
     a reference to another directory.

(3f) Expand the file name.

(4f) If the file name is /dev/null, return success (optimization).

(5f) If a lock file is required, create it (see separate comments below about
     the algorithm for doing this). It is important to do this before opening
     the mailbox if NFS is in use.

(6f) Stat the file, using lstat() rather than stat(), in order to pick up
     details of any symbolic link.

(7f) If the file already exists:

     Check the owner and group if necessary. Complain and defer if they are
     wrong.

     If it is a symbolic link AND the allow_symlink option is set (NOT the
     default), go back to (5f) but this time use stat() instead of lstat().

     If it's not a regular file, complain to local administrator and defer
     delivery with a local error code that causes subsequent delivery to
     be prevented until manually enabled.

     Check permissions. If the required permissions are *less* than the
     existing ones, or supplied by the address (often by the user via filter),
     chmod the file. Otherwise, complain and defer.

     Save the inode number.

     Open with O_WRONLY + O_APPEND, thus failing if the file has vanished.

     If open fails because the file does not exist, go to (6); on any other
     failure except EWOULDBLOCK, complain & defer. For EWOULDBLOCK (NFS
     failure), just defer.

     Check the inode number hasn't changed - I realize this isn't perfect (an
     inode can be reused) but it's cheap and will catch some of the races.

     Check it's still a regular file.

     Check that the owner and permissions haven't changed.

(8f) If file does not exist initially:

     Open with O_WRONLY + O_EXCL + O_CREAT with configured mode, unless we know
     this is via a symbolic link (only possible if allow_symlinks is set), in
     which case don't use O_EXCL, as it dosn't work.

     If open fails because the file already exists, go to (6). To avoid looping
     for ever in a situation where the file is continuously being created and
     deleted, all of this happens inside a loop that operates lock_retries
     times and includes the fcntl locking. If the loop completes without the
     file getting opened, complain and defer with a code that freezes delivery
     attempts.

     If open fails for any other reason, defer for subsequent delivery except
     when this is a file delivery resulting from an alias or forward expansion
     and the error is EPERM or ENOENT or EACCES, in which case FAIL as this is
     most likely a user rather than a configuration error.

(9f) We now have the file checked and open for writing. Lock it using fcntl -
     if fcntl() fails, close the file and goto (6), up to lock_retries times,
     after sleeping for a while. If it still fails, give up and defer delivery.

(10f)Save the access time (for subsequent restoration) and the size of the
     file (for comsat and for re-setting if delivery fails in the middle -
     e.g. for quota exceeded).

The following items apply in the case when a directory name is given:

(2d) Create a new file in the directory using a temporary name, by opening for
     writing and with O_CREAT.

(3d) Generate a unique name for the file from the time and the inode number,
     and rename file file.

The following items apply in all cases:

(11) We now have the file open for writing, and locked if it was given as a
     file name. Write the message and flush the file, unless there is a setting
     of the local quota option, in which case we can check for its excession
     without doing any writing.

     If there is a quota error on writing, defer the address. Timeout logic
     will determine for how long retries are attempted. We restore the mailbox
     to its original length. There doesn't seem to be a uniform error code
     for quota excession (it even differs between SunOS4 and some versions of
     SunOS5) so a system-dependent macro called ERRNO_QUOTA is used for it, and
     the value gets put into errno_quota at compile time.

     For any other error (most commonly disc full), do the same.

(12) Restore the atime; notify_comsat if required; close the file (which
     unlocks it if it was locked).

This yields FAIL only when a file name is generated by an alias or forwarding
operation and attempting to open it gives EPERM, ENOENT, or EACCES. All other
failures return DEFER (in addr->transport_return). */


void
appendfile_transport_entry(
  transport_instance *tblock,      /* data for this instantiation */
  address_item *addr)              /* address we are working on */
{
appendfile_transport_options_block *ob =
  (appendfile_transport_options_block *)(tblock->options_block);
struct stat statbuf;
char *filename;
char *lockname = NULL;     /* keep picky compiler happy */
char *hitchname;
char tempname[256];
time_t now = time(NULL);
struct utimbuf times;
BOOL smtp_dots = FALSE;
BOOL return_path_add = ob->return_path_add;
BOOL delivery_date_add = ob->delivery_date_add;
BOOL envelope_to_add = ob->envelope_to_add;
BOOL isdirectory = FALSE;
uid_t uid = getuid();
gid_t gid = getgid();
int mode = (addr->mode > 0)? addr->mode : ob->mode;
int saved_size = 0;
int hd = -1;
int fd = -1;
int yield = FAIL;
int i, len;

DEBUG(9) debug_printf("appendfile transport entered\n");

/* See if this is the address_file transport, used to deliver to files
specified via .forward or an alias file. */

if (addr->pfr && addr->local_part[0] == '/') filename = addr->local_part;

/* Handle a non-address file delivery. One of the file or directory options is
mandatory; it will have already been checked to be an absolute path. */

else
  {
  char *fdname = ob->filename;
  if (fdname == NULL)
    {
    fdname = ob->dirname;
    isdirectory = TRUE;
    }

  if (fdname == NULL)
    {
    addr->transport_return = PANIC;
    addr->message = string_sprintf("Mandatory file or directory option "
      "missing from %s transport", tblock->name);
    return;
    }

  filename = expand_string(fdname);

  if (filename == NULL)
    {
    addr->transport_return = PANIC;
    addr->message = string_sprintf("Expansion of \"%s\" (file or directory "
      "name for %s transport) failed: %s", fdname, tblock->name,
      expand_string_message);
    return;
    }

  /* If the file name contained a reference to $local_part, check that it
  doesn't attempt to change directory. */

  if (strchr(deliver_localpart, '/') != NULL &&
     (strstr(fdname, "${local_part}") != NULL ||
      strstr(fdname, "$local_part") != NULL))
    {
    addr->message = "appendfile: user name contains /";
    addr->basic_errno = ERRNO_USERSLASH;
    addr->special_action = SPECIAL_FREEZE;
    return;
    }

  /* Check that the expanded name is absolute. This check is needed in case
  it started with an expansion item. If not, the check is done at
  initialization time. */

  if (filename[0] != '/')
    {
    addr->message = string_sprintf("appendfile: file or directory name "
      "\"%s\" is not absolute", filename);
    addr->basic_errno = ERRNO_NOTABSOLUTE;
    addr->special_action = SPECIAL_FREEZE;
    return;
    }
  }



DEBUG(9) debug_printf("appendfile: mode=%o notify_comsat=%d quota=%d\n"
  "  %s=%s\n  prefix=%s\n  suffix=%s\n",
  mode, ob->notify_comsat, ob->quota_value,
  isdirectory? "directory" : "file",
  filename,
  (ob->prefix == NULL)? "null":ob->prefix,
  (ob->suffix==NULL)? "null":ob->suffix);


/* If the -N option is set, can't do any more. */

if (dont_deliver)
  {
  debug_printf("*** delivery by %s transport bypassed by -N option\n",
    tblock->name);
  addr->transport_return = OK;
  return;
  }

/* Handle the case of a file name. If the file name is /dev/null, we can save
ourselves some effort and just give a success return right away. */

if (!isdirectory)
  {
  BOOL use_lstat = TRUE;
  BOOL file_opened = FALSE;

  if (strcmp(filename, "/dev/null") == 0)
    {
    addr->transport_return = OK;
    return;
    }

  /* The locking of mailbox files is worse than the naming of cats, which is
  known to be "a difficult matter" (T.S. Eliot) and just as cats must have
  three different names, so several different styles of locking are used.

  Research in other programs that lock mailboxes shows that there is no
  universally standard method. Having mailboxes NFS-mounted on the system that
  is delivering mail is not the best thing, but people do run like this,
  and so the code must do its best to cope.

  Two different locking approaches are taken. Unless no_use_lockfile is set,
  we attempt to build a lock file in a way that will work over NFS, and we
  also use fcntl. Failures to lock cause retries after a sleep, but only for
  a certain number of tries. Lockfiles should always be used with NFS, because
  of the following:

  Originally, Exim got the file opened before doing anything about locking.
  However, a very occasional problem was observed on Solaris 2 when delivering
  over NFS. It is seems that when a file is opened with O_APPEND, the file size
  gets remembered at open time. If another process on another host (that's
  important) has the file open and locked and writes to it and then releases
  the lock while the first process is waiting to get the lock, the first
  process may fail to write at the new end point of the file - despite the very
  definite statement about O_APPEND in the man page for write(). Experiments
  have reproduced this problem, but I do not know any way of forcing a host to
  update its attribute cache for an open NFS file. It would be nice if it did
  so when a lock was taken out, but this does not seem to happen. Anyway, to
  reduce the risk of this problem happening, we now created the lock file
  *before* opening the mailbox. That will prevent two different Exims opening
  the file simultaneously. It may not prevent clashes with MUAs, however, but
  Pine seems to operate in the same way.

  The logic for creating the lock file is:

  . The name of the lock file is <mailbox-name>.lock

  . First, create a "hitching post" name by adding the primary host name,
    current time and pid to the lock file name. This should be unique.

  . Create the hitching post file using WRONLY + CREAT + EXCL.

  . If that fails EACCES, we assume it means that the user is unable to create
    files in the mail spool directory. Some installations might operate in this
    manner, so there is a configuration option to allow this state not to be an
    error - we proceed to lock using fcntl only, after the file is open.

  . Otherwise, an error causes a deferment of the address.

  . Hard link the hitching post to the lock file name.

  . If the link succeeds, we have successfully created the lock file. Simply
    close and unlink the hitching post file.

  . If the link does not succeed, proceed as follows:

    o Fstat the hitching post file, and then close and unlink it.

    o Now examine the stat data. If the number of links to the file is exactly
      2, the locking succeeded but for some reason, e.g. an NFS server crash,
      the return never made it back, so the link() function gave a failure
      return.

  . This method allows for the lock file to be created by some other process
    right up to the moment of the attempt to hard link it, and is also robust
    against NFS server crash-reboots, which would probably result in timeouts
    in the middle of link().

  . System crashes may cause lock files to get left lying around, and some means
    of flushing them is required. The approach of writing a pid (used by smail
    and by elm) into the file isn't useful when NFS may be in use. Pine uses a
    timeout, which seems a better approach. Since any program that writes to a
    mailbox using a lock file should complete its task very quickly, Pine
    removes lock files that are older than 5 minutes. We allow the value to be
    configurable on the director. */


  /* Build a lock file if configured to do so - the existence of a lock
  file is subsequently checked by looking for a non-negative value of the
  file descriptor hd - even though the file is no longer open. */

  if (ob->use_lockfile)
    {
    len = (int)strlen(filename);
    lockname = store_malloc(len + 8);
    sprintf(lockname, "%s.lock", filename);
    hitchname = store_malloc(len + 32 + (int)strlen(primary_hostname));
    sprintf(hitchname, "%s.%s.%08x.%08x", lockname, primary_hostname,
      (unsigned int)now, (unsigned int)getpid());

    DEBUG(9) debug_printf("lock name: %s\nhitch name: %s\n", lockname,
      hitchname);

    /* Lock file creation retry loop */

    for (i = 0; i < ob->lock_retries; sleep(ob->lock_interval), i++)
      {
      int rc;
      hd = open(hitchname, O_WRONLY | O_CREAT | O_EXCL, ob->lockfile_mode);
      if (hd < 0)
        {
        if (errno != EACCES || ob->require_lockfile)
          {
          addr->basic_errno = errno;
          addr->message = "creating lock file hitching post";
          return;
          }
        else break;
        }

      /* Attempt to hitch the hitching post to the lock file. If link()
      succeeds (the common case, we hope) all is well. Otherwise, fstat the
      file, and get rid of the hitching post. If the number of links was 2,
      the link was created, despite the failure of link(). If the hitch was
      not successful, try again, having unlinked the lock file if it is too
      old.

      There's a version of Linux (2.0.27) which doesn't update its local cache
      of the inode after link() by default - which many think is a bug - but
      if the link succeeds, this code will be OK. It just won't work in the
      case when link() fails after having actually created the link. The Linux
      NFS person is fixing this; a temporary patch is available if anyone is
      sufficiently worried. */

      if ((rc = link(hitchname, lockname)) != 0) fstat(hd, &statbuf);
      close(hd);
      unlink(hitchname);
      if (rc != 0 && statbuf.st_nlink != 2)
        {
        if (ob->lockfile_timeout > 0 && stat(lockname, &statbuf) == 0 &&
            now - statbuf.st_ctime > ob->lockfile_timeout)
          {
          DEBUG(2) debug_printf("unlinking timed-out lock file\n");
          unlink(lockname);
          }
        DEBUG(9) debug_printf("link of hitching post failed - retrying\n");
        continue;
        }

      DEBUG(9) debug_printf("lock file created\n");
      break;
      }

    /* Check for too many tries at creating the lock file */

    if (i >= ob->lock_retries)
      {
      addr->basic_errno = ERRNO_LOCKFAILED;
      addr->message = string_sprintf("failed to lock mailbox %s (lock file)",
        filename);
      return;
      }
    }


  /* We now have to get the file open and lock it with fcntl(). Stat the file,
  and act on existence or non-existence. This is in a loop to handle the case
  of a file's being created or deleted as we watch, and also to handle retries
  when the locking fails. Rather than holding the file open while waiting for
  the fcntl() lock, we close and do the whole thing again. This should be
  safer, especially for NFS files, which might get altered from other hosts,
  making their cached sizes incorrect.

  With the default settings, no symlinks are permitted, but there is an option
  to permit symlinks for those sysadmins that know what they are doing.
  Shudder. However, insist that the initial symlink is owned by the right user.
  Thus lstat() is used initially; if a symlink is discovered, the loop is
  repeated such that stat() is used, to look at the end file. */

  for (i = 0; i < ob->lock_retries; i++)
    {
    flock_t lock_data;
    file_opened = FALSE;

    if((use_lstat? lstat(filename, &statbuf) : stat(filename, &statbuf)) != 0)
      {
      /* Let's hope that failure to stat (other than non-existence) is a
      rare event. */

      if (errno != ENOENT)
        {
        addr->basic_errno = errno;
        addr->message = string_sprintf("attempting to stat mailbox %s",
          filename);
        goto RETURN;
        }

      /* File does not exist. If it is required to pre-exist this state is an
      error. */

      if (ob->file_must_exist)
        {
        addr->basic_errno = errno;
        addr->message = string_sprintf("mailbox %s does not exist, "
          "but file_must_exist is set", filename);
        addr->special_action = SPECIAL_FREEZE;
        goto RETURN;
        }

      /* If file creation is permitted in certain directories only, check that
      this is actually the case. Current checks are for in or below the
      home directory. */

      if (ob->create_file != create_anywhere)
        {
        BOOL OK = FALSE;
        if (deliver_home != NULL)
          {
          int len = (int)strlen(deliver_home);
	  char *file = filename;
          while (file[0] == '/' && file[1] == '/') file++;
          if (strncmp(file, deliver_home, len) == 0 && file[len] == '/' &&
            (ob->create_file == create_belowhome ||
              strchr(file+len+2, '/') == NULL)) OK = TRUE;
          }

        if (!OK)
          {
          addr->basic_errno = errno;
          addr->message = string_sprintf("mailbox %s does not exist, "
            "but creation outside the home directory is not permitted",
            filename);
          addr->special_action = SPECIAL_FREEZE;
          goto RETURN;
          }
        }

      /* Attempt to create and open the file. If open fails because of
      pre-existence, go round the loop again. For any other error, defer the
      address, except for an alias or forward generated file name with EPERM,
      ENOENT, or EACCES, as those are most likely to be user errors rather
      than Exim config errors. When a symbolic link is permitted and points
      to a non-existent file, we get here with use_lstat = FALSE. In this case
      we mustn't use O_EXCL, since it doesn't work. */

      fd = open(filename, O_WRONLY | O_APPEND | O_CREAT |
        (use_lstat? O_EXCL : 0), mode);
      if (fd < 0)
        {
        if (errno == EEXIST) continue;
        addr->basic_errno = errno;
        addr->message = string_sprintf("while creating mailbox %s",
          filename);
        if (addr->pfr && addr->local_part[0] == '/' &&
            (errno == EPERM || errno == ENOENT || errno == EACCES))
          addr->transport_return = FAIL;
        goto RETURN;
        }

      /* We have successfully created and opened the file. Ensure that the group
      and the mode are correct. */

      chown(filename, uid, gid);
      chmod(filename, mode);
      }


    /* The file already exists. Test its type, ownership, and permissions, and
    save the inode for checking later. If symlinks are permitted (not the
    default or recommended state) it may be a symlink that already exists.
    Check its ownership and then look for the file at the end of the link(s).
    This at least prevents one user creating a symlink for another user in
    a sticky directory. */

    else
      {
      int oldmode = statbuf.st_mode;
      int inode = statbuf.st_ino;
      BOOL islink = (oldmode & S_IFMT) == S_IFLNK;

      /* Group is checked only if check_group is set. */

      if (statbuf.st_uid != uid || (ob->check_group && statbuf.st_gid != gid))
        {
        addr->basic_errno = ERRNO_BADUGID;
        addr->message = string_sprintf("mailbox %s has wrong uid or gid%s",
          filename, islink? " (symlink)" : "");
        addr->special_action = SPECIAL_FREEZE;
        goto RETURN;
        }

      /* If symlinks are permitted (not recommended), the lstat() above will
      have found the symlink. Its ownership has just been checked; go round
      the loop again, using stat() instead of lstat(). That will never yield a
      mode of S_IFLNK. */

      if (islink && ob->allow_symlink)
        {
        use_lstat = FALSE;
        i--;                   /* Don't count this time round */
        continue;
        }

      /* An actual file exists. Check that it is a regular file. */

      if ((oldmode & S_IFMT) != S_IFREG)
        {
        addr->basic_errno = ERRNO_NOTREGULAR;
        addr->message = string_sprintf("mailbox %s is not a regular file",
          filename);
        addr->special_action = SPECIAL_FREEZE;
        goto RETURN;
        }

      /* If the mode is not what it would be for a newly created file, change
      the permissions if the mode is supplied for the address. Otherwise,
      reduce but do not extend the permissions. If the newly created
      permissions are greater than the existing permissions, don't change
      things when the mode is not from the address. */

      if ((oldmode = (oldmode & 07777)) != mode)
        {
        int diffs = oldmode ^ mode;
        if (addr->mode > 0 || (diffs & oldmode) == diffs)
          {
          DEBUG(2) debug_printf("chmod %o %s\n", mode, filename);
          if (chmod(filename, mode) < 0)
            {
            addr->basic_errno = errno;
            addr->message = string_sprintf("attempting to chmod mailbox %s",
              filename);
            addr->special_action = SPECIAL_FREEZE;
            goto RETURN;
            }
          }
        else
          {
          addr->basic_errno = ERRNO_BADMODE;
          addr->message = string_sprintf("mailbox %s has wrong mode %o",
            filename, oldmode);
          addr->special_action = SPECIAL_FREEZE;
          goto RETURN;
          }
        }

      /* We are happy with the existing file. Open it, and then do further
      tests to ensure that it is the same file that we were just looking at.
      If the file does not now exist, restart this loop, going back to using
      lstat again. For an NFS error, just defer; other opening errors are
      more serious. */

      fd = open(filename, O_WRONLY | O_APPEND, mode);
      if (fd < 0)
        {
        if (errno == ENOENT)
          {
          use_lstat = TRUE;
          continue;
          }
        addr->basic_errno = errno;
        if (errno != EWOULDBLOCK)
          {
          addr->message = string_sprintf("while opening mailbox %s", filename);
          addr->special_action = SPECIAL_FREEZE;
          }
        goto RETURN;
        }

      /* This fstat really shouldn't fail, as we have an open file! There's a
      dilemma here. We use fstat in order to be sure we are peering at the file
      we have got open. However, that won't tell us if the file was reached
      via a symbolic link. We checked this above, but there is a race exposure
      if the link was created between the previous lstat and the open. However,
      it would have to be created with the same inode in order to pass the
      check below. If ob->allow_symlink is set, causing the use of stat rather
      than lstat above, symbolic links may be there anyway, and the checking is
      weaker. */

      if (fstat(fd, &statbuf) < 0)
        {
        addr->basic_errno = errno;
        addr->message = string_sprintf("attempting to stat open mailbox %s",
          filename);
        addr->special_action = SPECIAL_FREEZE;
        goto RETURN;
        }

      /* Check the inode; this is isn't a perfect check, but gives some
      confidence. */

      if (inode != statbuf.st_ino)
        {
        addr->basic_errno = ERRNO_INODECHANGED;
        addr->message = string_sprintf("opened mailbox %s inode number changed "
          "from %d to %d", filename, inode, statbuf.st_ino);
        addr->special_action = SPECIAL_FREEZE;
        goto RETURN;
        }

      /* Check it's still a regular file and the uid, gid, and permissions have
      not changed. */

      if ((statbuf.st_mode & S_IFMT) != S_IFREG)
        {
        addr->basic_errno = ERRNO_NOTREGULAR;
        addr->message =
          string_sprintf("opened mailbox %s is not a regular file", filename);
        addr->special_action = SPECIAL_FREEZE;
        goto RETURN;
        }

      if (statbuf.st_uid != uid || (ob->check_group && statbuf.st_gid != gid))
        {
        addr->basic_errno = ERRNO_BADUGID;
        addr->message = string_sprintf("opened mailbox %s has wrong uid or gid",
          filename);
        addr->special_action = SPECIAL_FREEZE;
        goto RETURN;
        }

      if ((statbuf.st_mode & 07777) != mode)
        {
        addr->basic_errno = ERRNO_BADMODE;
        addr->message = string_sprintf("opened mailbox %s has wrong mode %o "
          "(%o expected)", filename, statbuf.st_mode & 07777, mode);
        addr->special_action = SPECIAL_FREEZE;
        goto RETURN;
        }

      /* The file is OK. Carry on to do the locking. */
      }

    /* We now have an open file, and must lock it using fcntl. If a lock file
    is also required, it was created above and hd was left >= 0. If fcntl()
    fails, close the file and go round the loop all over again, after waiting
    for a bit. */

    file_opened = TRUE;
    lock_data.l_type = F_WRLCK;
    lock_data.l_whence = lock_data.l_start = lock_data.l_len = 0;
    if (fcntl(fd, F_SETLK, &lock_data) >= 0) break;

    DEBUG(9) debug_printf("fcntl failed - retrying\n");
    close(fd);
    fd = -1;
    use_lstat = TRUE;             /* Reset to use lstat first */
    sleep(ob->lock_interval);
    }

  /* Test for exceeding the maximum number of tries. Either the file remains
  locked, or, if we haven't got it open, something is terribly wrong... */

  if (i >= ob->lock_retries)
    {
    if (!file_opened)
      {
      addr->basic_errno = ERRNO_EXISTRACE;
      addr->message = string_sprintf("mailbox %s: existence unclear", filename);
      addr->special_action = SPECIAL_FREEZE;
      }
    else
      {
      addr->basic_errno = ERRNO_LOCKFAILED;
      addr->message = string_sprintf("failed to lock mailbox %s (fcntl)",
        filename);
      }
    goto RETURN;
    }

  DEBUG(9) debug_printf("mailbox %s is locked\n", filename);

  /* Save access time (for subsequent restoration), modification time (for
  restoration if updating fails), size of file (for comsat and for re-setting if
  delivery fails in the middle - e.g. for quota exceeded). */

  if (fstat(fd, &statbuf) < 0)
    {
    addr->basic_errno = errno;
    addr->message = string_sprintf("while fstatting opened mailbox %s",
      filename);
    goto RETURN;
    }

  times.actime = statbuf.st_atime;
  times.modtime = statbuf.st_mtime;
  saved_size = statbuf.st_size;
  }

/* Handle the case of creating a unique file in a given directory. A temporary
name is used to create the file. Later, when it is written, the name is changed
to a unique one. There is no need to lock the file. An attempt is made to
create the directory if it does not exist. */

else
  {
  sprintf(tempname, "%s/temp.%d", filename, (int)getpid());
  fd = open(tempname, O_WRONLY|O_CREAT, mode);
  if (fd < 0 &&                                    /* failed to open, and */
      (errno != ENOENT ||                          /* either not not exists */
       !ob->create_directory ||                    /* or not allowed to make */
       mkdir(filename, ob->dirmode) < 0 ||         /* or failed to create dir */
       (fd = open(tempname, O_WRONLY|O_CREAT, mode)) < 0)) /* then open */
    {
    addr->basic_errno = errno;
    addr->message = string_sprintf("while creating file %s", tempname);
    return;
    }
  chown(tempname, uid, gid);
  chmod(tempname, mode);
  }


/* At last we can write the message to the file, preceded by any configured
prefix line, and followed by any configured suffix line. If there are any
writing errors, we must defer. */

yield = OK;
errno = 0;

/* If there is a local quota setting, check that we are not going to exceed it
with this message. */

if (ob->quota_value > 0)
  {
  if (saved_size + message_size > ob->quota_value)
    {
    yield = DEFER;
    errno = ERRNO_EXIMQUOTA;
    }
  }

/* If the local_smtp option is not unset, we need to write SMTP prefix
information. The various different values for batching are handled outside; if
there is more than one address available here, all must be included. Force
SMTP dot-handling. */

if (tblock->local_smtp != local_smtp_off && yield == OK)
  {
  smtp_dots = TRUE;
  return_path_add = delivery_date_add = envelope_to_add = FALSE;
  if (!write_string(fd, "MAIL FROM: <%s>\n",
    (addr->errors_address != NULL)? addr->errors_address : sender_address))
      yield = DEFER;
  else
    {
    address_item *a;
    for (a = addr; a != NULL; a = a->next)
      {
      if ((a->local_part[0] == ',' || a->local_part[0] == ':')?
        !write_string(fd,
          "RCPT TO: <@%s%s>\n", a->domain, a->local_part) :
        !write_string(fd,
          "RCPT TO: <%s@%s>\n", a->local_part, a->domain))
        { yield = DEFER; break; }
      }
    if (yield == OK && !write_string(fd, "DATA\n")) yield = DEFER;
    }
  }

/* Now any other configured prefix. */

if (yield == OK && ob->prefix != NULL)
  {
  char *prefix = expand_string(ob->prefix);
  if (prefix == NULL)
    {
    addr->transport_return = PANIC;
    addr->message = string_sprintf("Expansion of \"%s\" (prefix for %s "
      "transport) failed", ob->prefix, tblock->name);
    goto RETURN;
    }
  if (!write_string(fd, "%s", prefix)) yield = DEFER;
  }

/* The options to write_message request a return-path header if configured, the
unix "from" hack if configured, no CRs added before LFs, and no SMTP dot
handling except when local_smtp is set. Pass the errors_address from the
address if present. This is used to create return-path if requested (forced
off for local_smtp). */

if (yield == OK &&
  !transport_write_message(addr, fd,
    (return_path_add? topt_add_return_path : 0) |
    (delivery_date_add? topt_add_delivery_date : 0) |
    (envelope_to_add? topt_add_envelope_to : 0 ) |
    (ob->from_hack? topt_from_hack : 0 ) |
    (smtp_dots? topt_smtp_dots : 0),
    addr->errors_address, 0))
  yield = DEFER;

/* Now a configured suffix. */

if (yield == OK && ob->suffix != NULL)
  {
  char *suffix = expand_string(ob->suffix);
  if (suffix == NULL)
    {
    addr->transport_return = PANIC;
    addr->message = string_sprintf("Expansion of \"%s\" (suffix for %s "
      "transport) failed", ob->suffix, tblock->name);
    goto RETURN;
    }
  if (!write_string(fd, "%s", suffix)) yield = DEFER;
  }

/* If local_smtp, write the terminating dot. */

if (yield == OK && tblock->local_smtp != local_smtp_off &&
  !write_block(fd, ".\n", 2)) yield = DEFER;

/* Force out the remaining data to check for any errors */

if (yield == OK && fsync(fd) < 0) yield = DEFER;

/* Handle error while writing the file. Control should come here directly after
the error, with the reason in errno. */

if (yield != OK)
  {
  /* Save the error number. It will ultimately cause a strerror() call to
  generate some text. */

  addr->basic_errno = errno;

  /* Handle system quota excession. Set more_errno to the time since the mailbox
  was last read, and add an explanatory phrase for the error message, since
  some systems don't have special quota-excession errors. */

  if (errno == errno_quota)
    {
    addr->more_errno = time(NULL) - times.actime;
    #ifndef EDQUOT
    addr->message = string_sprintf("probably user's quota exceeded while "
      "writing to mailbox %s", filename);
    #endif
    DEBUG(9) debug_printf("System quota exceeded for %s: time since mailbox "
      "read = %s\n", filename, readconf_printtime(addr->more_errno));
    }

  /* Handle Exim's own quota-imposition */

  else if (errno == ERRNO_EXIMQUOTA)
    {
    addr->more_errno = time(NULL) - times.actime;
    addr->message = string_sprintf("MTA-imposed quota exceeded while "
      "writing to mailbox %s", filename);
    DEBUG(9) debug_printf("Exim quota exceeded for %s: time since mailbox "
      "read = %s\n", filename, readconf_printtime(addr->more_errno));
    }

  /* For other errors, a general-purpose explanation */

  else addr->message = string_sprintf("error while writing to mailbox %s",
    filename);

  /* For a write to a directory, remove the file. */

  if (isdirectory) unlink(tempname);

  /* For a file, reset the file size to what it was before we started, leaving
  the last modification time unchanged, so it will get reset also. All systems
  investigated so far have ftruncate(), whereas not all have the F_FREESP
  fcntl() call (BSDI & FreeBSD do not). */

  else ftruncate(fd, saved_size);
  }

/* Handle successful writing - we want the modification time to be now.
Remove the default backstop error number. For a directory, now is the time
to rename the file with a unique name, constructed from the time and the file's
inode in base 62 in the form tttttt-iiiiii. As soon as such a name appears
it may get used by another process. */

else
  {
  times.modtime = now;
  addr->basic_errno = 0;

  if (isdirectory)
    {
    if (fstat(fd, &statbuf) < 0)
      {
      addr->basic_errno = errno;
      addr->message = string_sprintf("while fstatting opened message file %s",
        tempname);
      yield = DEFER;
      }

    /* Build the new name. It starts with 'q' for smail compatibility. */

    else
      {
      char newname[256];
      char *p = newname + (int)strlen(tempname);
      strcpy(newname, tempname);
      while (p[-1] != '/') p--;
      *p++ = 'q';
      strcpy(p, string_base62(time(NULL)));
      p += (int)strlen(p);
      sprintf(p, "-%s", string_base62(statbuf.st_ino));

      if (rename(tempname, newname) < 0)
        {
        addr->basic_errno = errno;
        addr->message = string_sprintf("while renaming message file %s as %s",
          tempname, newname);
        unlink(tempname);
        yield = DEFER;
        }
      }
    }
  }


/* For a file, restore the last access time (atime), and set the modification
time as required - changed if write succeeded, unchanged if not. */

if (!isdirectory) utime(filename, &times);

/* Notify comsat if configured to do so. It only makes sense if the configured
file is the one that the comsat daemon knows about. */

if (ob->notify_comsat && yield == OK)
  notify_comsat(deliver_localpart, saved_size);

/* Pass back the final return code in the address structure */

DEBUG(2) debug_printf("appendfile yields %d with errno=%d more_errno=%d\n",
  yield, addr->basic_errno, addr->more_errno);

addr->transport_return = yield;

/* Close the file, which will release the fcntl lock. If everything has gone
right but close fails, defer the message. Then unlink the lock file, if
present. This point in the code is jumped to from a number of places when
errors are detected, in order to get the file closed and the lock file tidied
away. */

RETURN:

if (fd >= 0 && close(fd) < 0 && yield == OK)
  {
  addr->basic_errno = errno;
  addr->message = string_sprintf("while closing mailbox %s", filename);
  addr->transport_return = DEFER;
  }

if (hd >= 0) unlink(lockname);
}

/* End of transport/appendfile.c */
