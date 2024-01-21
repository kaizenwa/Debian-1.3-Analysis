/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Functions that operate on the input queue. */


#include "exim.h"



/*************************************************
*              Perform a queue run               *
*************************************************/

/*The arguments give the messages to start and stop at; NULL means start at the
beginning or stop at the end. If the given start message doesn't exist, we
start at the next lexically greater one, and likewise we stop at the after the
previous lexically lesser one if the given stop message doesn't exist. Because
a queue run can take some time, stat each file before forking, in case it has
been delivered in the meantime by some other means.

The global variables queue_force and queue_local may be set to cause forced
deliveries or local-only deliveries, respectively.

If deliver_selectstring is not NULL, the deliver_message() function skips
messages whose recipients do not contain the string. As this option is used
when a machine comes back online, we want to ensure that at least one
delivery attempt takes place, so force the first one.

Arguments:
  start_id   message id to start at, or NULL for all
  stop_id    message id to end at, or NULL for all

Returns:     nothing
*/

void
queue_run(char *start_id, char *stop_id)
{
BOOL force_delivery = queue_run_force || deliver_selectstring != NULL;
queue_filename *f;
log_write(0, LOG_MAIN, "Start queue run: pid=%d%s%s", getpid(),
  (deliver_selectstring == NULL)? "" : " -R ",
  (deliver_selectstring == NULL)? "" : deliver_selectstring);

/* Turn off the flags that cause SMTP deliveries and remote processing not to
happen. */

queue_smtp = queue_remote = FALSE;

/* Run the queue. If a start or finish id is given, we must take the queue in
its natural order. Otherwise "randomize" it so we don't always do things in the
same order. If deliver_selectstring is set, check for an undelivered address
that matches the string. This does mean we have to read the header file, and
then again when actually delivering, but it's cheaper than forking a delivery
process for each file. */

for (f = queue_get_spool_list(start_id == NULL && stop_id == NULL);
     f != NULL; f = f->next)
  {
  if (stop_id != NULL && strcmp(f->text, stop_id) > 0) break;
  if (start_id == NULL || strcmp(f->text, start_id) >= 0)
    {
    pid_t pid;
    int status;
    struct stat statbuf;
    char buffer[256];

    sprintf(buffer, "%s/input/%s", spool_directory, f->text);
    if (stat(buffer, &statbuf) < 0) continue;

    /* Check for a matching address if deliver_selectstring is set. If so,
    we do a fully delivery - don't want to omit other addresses since
    their routing might trigger re-writing etc. */

    if (deliver_selectstring != NULL)
      {
      BOOL found = FALSE;
      if (spool_read_header(f->text, FALSE) != spool_read_OK) continue;

      if (recipients_list != NULL)
        {
        int i;
        for (i = 0; i < recipients_count; i++)
          {
          if (
            strstric(recipients_list[i],deliver_selectstring,FALSE) != NULL &&
              tree_search_addr(tree_nonrecipients,recipients_list[i],FALSE)
                == NULL)
            {
            found = TRUE;
            break;
            }
          }
        accept_free_recipients();
        }

      if (sender_address != NULL) store_free(sender_address);

      /* If no addresses matched the string, skip this message. */

      if (!found)
        {
        DEBUG(9) debug_printf("%s: no addresses matched %s\n",
          f->text, deliver_selectstring);
        continue;
        }
      }

    /* OK, got a message we want to deliver; cut the -H off the name first */

    set_process_info("running queue: %s", f->text);
    f->text[SPOOL_NAME_LENGTH-2] = 0;
    if ((pid = fork()) == 0)
      _exit(deliver_message(f->text, force_delivery, queue_run_local, FALSE));

    if (pid < 0)
      log_write(0, LOG_PANIC_DIE, "fork of delivery process failed\n");

    set_process_info("running queue: waiting for %s (%d)", f->text, pid);
    while (wait(&status) != pid);
    set_process_info("running queue");

    /* A successful return means a delivery was attempted; turn off the
    force flag for any subsequent calls unless queue_force is set. */

    if ((status & 0xffff) == 0) force_delivery = queue_run_force;

    /* Otherwise, if the delivery yielded DEFER it means the load average
    was too high to attempt a delivery. Abandon the queue run. It seems
    sensible to include the load average on the log; for some OS we have to
    be root to do this, and if we are not root here, we'll be able to regain
    it with seteuid. There doesn't seem much point in restoring the non-root
    euid as we are going to finish the process. */

    if ((status & 0x00ff) == 0 && (status & 0xff00) == (DEFER << 8))
      {
      #ifdef LOAD_AVG_NEEDS_ROOT
      if (geteuid() != root_uid) mac_seteuid(root_uid);
      #endif
      log_write(0, LOG_MAIN, "Abandon queue run (load %.2f, max %.2f): "
        "pid = %d",
        (double)os_getloadavg()/1000.0,
        (double)deliver_load_max/1000.0,
        getpid());
      return;
      }
    }
  }
log_write(0, LOG_MAIN, "End queue run: pid=%d", getpid());
}



/*************************************************
*             Get list of spool files            *
*************************************************/

/* Scan the spool directory and return a list of the relevant file names
therein. If the argument is TRUE, they are returned in "randomized" order.
Actually, the order is anything but random, but the algorithm is cheap, and
the point is simply to ensure that the same order doesn't occur every time, in
case a particular message is causing a remote MTA to barf - we would like to
try other messages to that MTA first. If the argument is FALSE, sort the list
according to the file name. This should give the order in which the messages
arrived. It is used only for presentation to humans, so the insertion sort
that it does is not part of the normal operational code.

Argument:   TRUE if the order of the list is to be unpredictable
Returns:    pointer to a chain of queue name items
*/

queue_filename *
queue_get_spool_list(BOOL randomize)
{
int flags = 0;
int resetflags = -1;
queue_filename *yield = NULL;
queue_filename *last = NULL;
struct dirent *ent;
DIR *dd;
char buffer[256];

sprintf(buffer, "%s/input", spool_directory);
dd = opendir(buffer);
if (dd == NULL) return NULL;

/* The file names are added onto the start or end of the list according to the
bits of the flags variable. When randomizing, get a collection of bits from the
current time. Use the bottom 16 and just keep re-using them if necessary. */

if (randomize) resetflags = time(NULL) & 0xFFFF;

/* Now scan the directory. */

while ((ent = readdir(dd)) != NULL)
  {
  char *name = ent->d_name;
  if ((int)strlen(name) == SPOOL_NAME_LENGTH &&
      strcmp(name + SPOOL_NAME_LENGTH - 2, "-H") == 0)
    {
    queue_filename *next =
      store_malloc(sizeof(queue_filename) + (int)strlen(name));
    strcpy(next->text, name);

    /* First item becomes the top and bottom of the list. */

    if (yield == NULL)
      {
      next->next = NULL;
      yield = last = next;
      }

    /* If randomizing, insert at either top or bottom, randomly. This is, I
    argue, faster than doing a sort by allocating a random number to each item,
    and it also saves having to store the number with each item. */

    else if (randomize)
      {
      if (flags == 0) flags = resetflags;
      if ((flags & 1) == 0)
        {
        next->next = yield;
        yield = next;
        }
      else
        {
        next->next = NULL;
        last->next = next;
        last = next;
        }
      flags = flags >> 1;
      }

    /* Otherwise do an insertion sort based on the name. First see if
    it should go before the first item. */

    else if (strcmp(next->text, yield->text) < 0)
      {
      next->next = yield;
      yield = next;
      }

    /* Otherwise find the item it should go after; check the last one
    first, because that will often be the case. */

    else
      {
      queue_filename *this;
      if (strcmp(next->text, last->text) < 0)
        {
        for (this = yield; this != last; this = this->next)
          if (strcmp(next->text, this->next->text) < 0) break;
        }
      else this = last;
      next->next = this->next;
      this->next = next;
      if (this == last) last = next;
      }
    }
  }

closedir(dd);
return yield;
}




/************************************************
*          List messages on the queue           *
************************************************/

/* We get a list of file names as quickly as possible, then scan each one for
information to output. If any disappear while we are processing, just leave
them out. This function is a top-level function that is obeyed as a result of
the -bp argument. There is no point wasting resources in freeing the store used
for the list of names at the end. However, as there may be a lot of messages on
the queue, we must tidy up the store after reading the headers for each one. */

void
queue_list(void)
{
int i;
int now = (int)time(NULL);
queue_filename *f = queue_get_spool_list(FALSE);

for (; f != NULL; f = f->next)
  {
  int rc, save_errno;
  int size = 0;
  BOOL env_read;

  message_size = 0;
  rc = spool_read_header(f->text, FALSE);
  save_errno = errno;
  env_read = (rc == spool_read_OK || rc == spool_read_hdrerror);

  if (env_read)
    {
    struct stat statbuf;
    sprintf(big_buffer, "%s/input/%s", spool_directory, f->text);
    big_buffer[(int)strlen(big_buffer)-1] = 'D';
    if (stat(big_buffer, &statbuf) == 0) size = message_size + statbuf.st_size;
    i = (now - received_time)/60;  /* minutes on queue */
    if (i > 90)
      {
      i = (i + 30)/60;
      if (i > 72) printf("%2dd ", (i + 12)/24); else printf("%2dh ", i);
      }
    else printf("%2dm ", i);
    }

  fprintf(stdout, "%s ", string_formatsize(size, big_buffer));
  for (i = 0; i < 16; i++) fputc(f->text[i], stdout);

  if (env_read && sender_address != NULL)
    {
    printf(" <%s>", sender_address);
    store_free(sender_address);
    }

  if (rc != spool_read_OK)
    {
    printf("\n    ");
    if (save_errno == ERRNO_SPOOLFORMAT)
      {
      struct stat statbuf;
      sprintf(big_buffer, "%s/input/%s", spool_directory, f->text);
      if (stat(big_buffer, &statbuf) == 0)
        printf("*** spool format error: size=%d ***", (int)statbuf.st_size);
      else printf("*** spool format error ***");
      }
    else printf("*** spool read error: %s ***", strerror(save_errno));
    if (rc != spool_read_hdrerror)
      {
      printf("\n");
      continue;
      }
    }

  if (deliver_freeze) printf(" *** frozen ***");

  printf("\n");

  if (recipients_list != NULL)
    {
    for (i = 0; i < recipients_count; i++)
      {
      printf("        %s %s\n",
        (tree_search_addr(tree_nonrecipients,recipients_list[i],FALSE) == NULL)?
          " ":"D", recipients_list[i]);
      store_free(recipients_list[i]);
      }
    printf("\n");
    store_free(recipients_list);
    }

  tree_free(tree_nonrecipients);
  }
}



/*************************************************
*             Act on a specific message          *
*************************************************/

/* Actions that require a list of addresses make use of
argv/argc/recipients_arg. Other actions do not. This function does its
own authority checking.

Arguments:
  id              id of the message to work on
  action          which action is required (MSG_xxx)
  f               if not NULL, write messages to this file
  argv            the original argv for Exim
  argc            the original argc for Exim
  recipients_arg  offset to the list of recipients in argv

Returns:          FALSE if there was any problem
*/

BOOL
queue_action(char *id, int action, FILE *f, char **argv, int argc,
  int recipients_arg)
{
int i;
BOOL yield = TRUE;
struct passwd *pw;
char *doing = NULL;
char *username;
char spoolname[256];

/* Set the global message_id variable, used when re-writing spool files. This
also causes message ids to be added to log messages. */

strcpy(message_id, id);

/* Open and lock the data file to ensure that no other process is working on
this message. If the file does not exist, continue only if the action is remove
and the user is an admin user, to allow for tidying up broken states. */

if (!spool_open_datafile(id))
  {
  if (errno == ENOENT)
    {
    yield = FALSE;
    if (f != NULL) fprintf(f, "Spool data file for %s does not exist\n", id);
    if (action != MSG_REMOVE || !admin_user) return FALSE;
    if (f != NULL) fprintf(f, "Continuing to ensure all files removed\n");
    }
  else
    {
    if (f != NULL) fprintf(f, "Message %s is locked\n", id);
    return FALSE;
    }
  }

/* Read the spool header file for the message. Again, continue after an
error only in the case of deleting by an administrator. */

sprintf(spoolname, "%s-H", id);
if (spool_read_header(spoolname, TRUE) != spool_read_OK)
  {
  yield = FALSE;
  if (f != NULL)
    {
    if (errno != ERRNO_SPOOLFORMAT)
      fprintf(f, "Spool read error for %s: %s\n", spoolname, strerror(errno));
    else
      fprintf(f, "Spool format error for %s\n", spoolname);
    }
  if (action != MSG_REMOVE || !admin_user)
    {
    close(deliver_datafile);
    deliver_datafile = -1;
    return FALSE;
    }
  if (f != NULL) fprintf(f, "Continuing to ensure all files removed\n");
  }

/* Check that the user running this process is entitled to operate on this
message. Only admin users may freeze/thaw, add/cancel recipients, or otherwise
mess about, but the original sender is permitted to remove a message. */

if (!admin_user && (action != MSG_REMOVE || real_uid != originator_uid))
  {
  if (f != NULL) fprintf(f, "Permission denied\n");
  close(deliver_datafile);
  deliver_datafile = -1;
  return FALSE;
  }

/* Set up the user name for logging. */

pw = getpwuid(real_uid);
username = (pw != NULL)? pw->pw_name : string_sprintf("uid %d", (int)real_uid);

/* Take the necessary action. */

if (f != NULL) fprintf(f, "Message %s ", id);

switch(action)
  {
  case MSG_FREEZE:
  if (deliver_freeze)
    {
    yield = FALSE;
    if (f != NULL) fprintf(f, "is already frozen\n");
    }
  else
    {
    deliver_freeze = TRUE;
    deliver_manual_thaw = FALSE;
    deliver_frozen_at = time(NULL);
    if (spool_write_header(id) > 0)
      {
      if (f != NULL) fprintf(f, "is now frozen\n");
      log_write(0, LOG_MAIN, "frozen by %s", username);
      }
    else
      {
      yield = FALSE;
      if (f != NULL) fprintf(f, "could not be frozen: failed to rewrite "
        "spool header file\n");
      log_write(0, LOG_MAIN, "failed to rewrite header file");
      }
    }
  break;


  case MSG_THAW:
  if (!deliver_freeze)
    {
    yield = FALSE;
    if (f != NULL) fprintf(f, "is not frozen\n");
    }
  else
    {
    deliver_freeze = FALSE;
    deliver_manual_thaw = TRUE;
    if (spool_write_header(id) > 0)
      {
      if (f != NULL) fprintf(f, "is no longer frozen\n");
      log_write(0, LOG_MAIN, "unfrozen by %s", username);
      }
    else
      {
      yield = FALSE;
      if (f != NULL) fprintf(f, "could not be unfrozen: failed to rewrite "
        "spool header file\n");
      log_write(0, LOG_MAIN, "failed to rewrite header file");
      }
    }
  break;


  case MSG_REMOVE:
  sprintf(spoolname, "%s/msglog/%s", spool_directory, id);
  if (unlink(spoolname) < 0)
    {
    if (errno != ENOENT)
      {
      yield = FALSE;
      if (f != NULL)
        fprintf(f, "Error while removing %s: %s\n", spoolname,
          strerror(errno));
      }
    }

  for (i = 0; i < 3; i++)
    {
    sprintf(spoolname, "%s/input/%s-%c", spool_directory, id, "DHJ"[i]);
    if (unlink(spoolname) < 0)
      {
      if (errno != ENOENT)
        {
        yield = FALSE;
        if (f != NULL)
          fprintf(f, "Error while removing %s: %s\n", spoolname,
            strerror(errno));
        }
      }
    }

  /* In the common case, the datafile is open (and locked), so give the
  obvious message. Otherwise be more specific. */

  if (f != NULL)
    {
    if (deliver_datafile >= 0) fprintf(f, "has been removed\n");
      else fprintf(f, "has been removed or did not exist\n");
    }

  log_write(0, LOG_MAIN, "removed by %s", username);
  break;


  case MSG_MARK_ALL_DELIVERED:
  for (i = 0; i < recipients_count; i++)
    {
    tree_add_nonrecipient(recipients_list[i], FALSE);
    }
  if (spool_write_header(id) > 0)
    {
    if (f != NULL) fprintf(f, "has been modified\n");
    for (i = 0; i < recipients_count; i++)
      log_write(0, LOG_MAIN, "address <%s> marked delivered by %s",
        recipients_list[i], username);
    }
  else
    {
    yield = FALSE;
    if (f != NULL) fprintf(f, "- failed to rewrite spool header file "
      "while marking all recipients delivered\n");
    log_write(0, LOG_MAIN, "failed to rewrite header file while "
      "marking all recipients delivered");
    }
  break;


  case MSG_EDIT_SENDER:
  if (recipients_arg < argc - 1)
    {
    yield = FALSE;
    if (f != NULL)
      fprintf(f, "- only one sender address can be specified\n");
    break;
    }
  doing = "editing sender";
  /* Fall through */

  case MSG_ADD_RECIPIENT:
  if (doing == NULL) doing = "adding recipient";
  /* Fall through */

  case MSG_MARK_DELIVERED:
  if (doing == NULL) doing = "marking as delivered";

  /* Common code for EDIT_SENDER, ADD_RECIPIENT, & MARK_DELIVERED */

  if (recipients_arg >= argc)
    {
    yield = FALSE;
    if (f != NULL)
      fprintf(f, "- error while %s: no address given\n", doing);
    }
  else
    {
    for (; recipients_arg < argc; recipients_arg++)
      {
      int start, end, domain;
      char *errmess;
      char *receiver =
        parse_extract_address(argv[recipients_arg], &errmess, &start, &end,
          &domain, (action == MSG_EDIT_SENDER));

      if (receiver == NULL)
        {
        yield = FALSE;
        if (f != NULL)
          fprintf(f, "- error while %s:\n  bad address %s: %s\n",
            doing, argv[recipients_arg], errmess);
        }
      else if (receiver[0] != 0 && domain == 0)
        {
        yield = FALSE;
        if (f != NULL)
          fprintf(f, "- error while %s:\n  bad address %s: "
            "domain missing\n", doing, argv[recipients_arg]);
        }
      else
        {
        if (action == MSG_ADD_RECIPIENT)
          {
          accept_add_recipient(receiver);
          log_write(0, LOG_MAIN, "recipient <%s> added by %s",
            receiver, username);
          }
        else if (action == MSG_MARK_DELIVERED)
          {
          for (i = 0; i < recipients_count; i++)
            if (strcmp(recipients_list[i], receiver) == 0) break;
          if (i >= recipients_count)
            {
            if (f != NULL)
              fprintf(f, "- error while %s:\n  %s is not a recipient:"
                " message not updated\n", doing, receiver);
            yield = FALSE;
            }
          else
            {
            tree_add_nonrecipient(receiver, FALSE);
            log_write(0, LOG_MAIN, "address <%s> marked delivered by %s",
              receiver, username);
            }
          }
        else  /* MSG_EDIT_SENDER */
          {
          sender_address = receiver;
          log_write(0, LOG_MAIN, "sender address changed to <%s> by %s",
            receiver, username);
          }
        }
      }


    if (yield)
      {
      if (spool_write_header(id) > 0)
        {
        if (f != NULL) fprintf(f, "has been modified\n");
        }
      else
        {
        yield = FALSE;
        if (f != NULL) fprintf(f, "- failed to rewrite spool header file "
          "while %s\n", doing);
        log_write(0, LOG_MAIN, "failed to rewrite header file while %s",
          doing);
        }
      }
    }
  break;


  case MSG_EDIT_BODY:
  if (recipients_arg < argc)
    {
    yield = FALSE;
    if (f != NULL)
      fprintf(f, "- only one message can be edited at once\n");
    }

  /* Make a copy of the body, and let the editor work on that. If the editor
  returns successfully, replace the body with the new text. This is the only
  way to allow the editor to do intermittent saves while preserving the right
  of the human to abort the whole thing. As soon as the rename() is done, the
  message becomes available for some other process to work on, since the new
  file is not locked, but that's OK because the next thing this process does is
  to close the old file anyway. */

  else
    {
    int copy_fd;
    int rc;
    char copyname[256];

    /* Make a temporary file to copy to */

    sprintf(copyname, "%s/input/%s-D-%d", spool_directory, id, (int)getpid());
    copy_fd = open(copyname, O_WRONLY|O_CREAT, SPOOL_MODE);
    if (copy_fd < 0)
      {
      if (f != NULL) fprintf(f, "not modified: opening copy file failed: %s",
        strerror(errno));
      yield = FALSE;
      break;
      }

    /* Make sure it has the same characteristics as the -D file */

    if (exim_uid_set)
      {
      fchown(copy_fd, exim_uid, exim_gid);
      fchmod(copy_fd, SPOOL_MODE);
      }

    /* Copy the contents of the -D file */

    while((rc = read(deliver_datafile, big_buffer, big_buffer_size)) > 0)
      {
      if (write(copy_fd, big_buffer, rc) != rc)
        {
        if (f != NULL) fprintf(f, "not modified: copying failed (write): %s\n",
          strerror(errno));
        yield = FALSE;
        break;
        }
      }

    if (rc < 0)
      {
      if (f != NULL) fprintf(f, "not modified: copying failed (read): %s\n",
        strerror(errno));
      yield = FALSE;
      }

    close(copy_fd);

    /* Now call the editor and act according to its yield */

    if (yield)
      {
      sprintf(spoolname, "/bin/sh -c \"${VISUAL:-${EDITOR:-vi}} %s\"", copyname);

      if ((system(spoolname) & 0xffff) == 0)
        {
        sprintf(spoolname, "%s/input/%s-D", spool_directory, id);
        if ((rc = rename(copyname, spoolname)) == 0)
          {
          if (f != NULL) fprintf(f, "has been modified\n");
          log_write(0, LOG_MAIN, "body edited by %s", username);
          }
        else
          {
          if (f != NULL) fprintf(f, "not modified: rename failed: %s\n",
            strerror(errno));
          yield = FALSE;
          }
        }
      else
        {
        if (f != NULL) fprintf(f, "not modified: editing failed\n");
        yield = FALSE;
        }
      }

    /* Get rid of the copy file if something went wrong */

    if (!yield) unlink(copyname);
    }
  break;
  }


/* Closing the datafile releases the lock and permits other processes
to operate on the message (if it still exists). */

close(deliver_datafile);
deliver_datafile = -1;
return yield;
}

/* End of queue.c */
