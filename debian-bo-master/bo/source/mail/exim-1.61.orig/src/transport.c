/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* General functions concerned with transportation, and generic options for all
transports. */


#include "exim.h"



/* Generic options for transports, all of which live inside transport_instance
data blocks and which therefore have the opt_public flag set. Note that there
are other options living inside this structure which can be set only from
certain transports. */

optionlist optionlist_transports[] = {
  { "driver",    opt_stringptr|opt_public,
                 (void *)offsetof(transport_instance, driver_name) },
};

int optionlist_transports_size =
  sizeof(optionlist_transports)/sizeof(optionlist);


/*************************************************
*             Initialize transport list           *
*************************************************/

/* Read the transports configuration file, and set up a chain of transport
instances according to its contents. Each transport has generic options and may
also have its own private options. This function is only ever called when
transports == NULL. We use generic code in readconf to do most of the work. */

void
transport_init(void)
{
transport_instance *t;

readconf_driver_init("transport",
  (driver_instance **)(&transports),     /* chain anchor */
  (driver_info *)transports_available,   /* available drivers */
  sizeof(transport_info),                /* size of info block */
  &transport_defaults,                   /* default values for generic options */
  sizeof(transport_instance),            /* size of instance block */
  optionlist_transports,                 /* generic options */
  optionlist_transports_size);

/* Now scan the configured transports and ... ? */

for (t = transports; t != NULL; t = t->next)
  {
  /* No checks currently done */
  }
}



/*************************************************
*             Timeout handler                    *
*************************************************/

/* This is called by the any transport that wants to apply timeouts
to its activities. It then sets transport_chunk_timeout before calling
transport_write_message() to apply timeouts to individual message chunks.

Argument: the signal value (SIGALRM)
Returns:  nothing
*/

void
transport_timeout_handler(int sig)
{
transport_sigalrm_seen = TRUE;
}



/*************************************************
*             Write block of data                *
*************************************************/

/* Subroutine called by write_chunk actually to write a data block. It
applies a timeout if transport_chunk_timeout is greater than zero. Also,
on some systems at least, if a quota is exceeded *during* the write, the yield
is the number of bytes written and errno is not set. Therefore we always try to
write a second time to output the remainder of the data after a non-negative
return from write() (except after a timeout). This time, no bytes should get
written, and a proper error should get put into errno.

Arguments:
  fd        file descriptor to write to
  block     block of bytes to write
  len       number of bytes to write

Returns:    TRUE on success, FALSE on failure (with errno preserved)
*/

static BOOL
write_block(int fd, char *block, int len)
{
int i, rc;
for (i = 0; i < 2; i++)
  {
  DEBUG(9)
    debug_printf("writing data block: timeout=%d\n", transport_chunk_timeout);
  if (transport_chunk_timeout > 0) alarm(transport_chunk_timeout);
  rc = write(fd, block, len);
  if (transport_chunk_timeout > 0) alarm(0);
  if (rc == len) return TRUE;
  if (rc < 0) return FALSE;
  if (transport_sigalrm_seen)
    {
    errno = ETIMEDOUT;
    return FALSE;
    }
  len -= rc;
  block += rc;
  }
return rc == len;
}




/*************************************************
*              Write character chunk             *
*************************************************/

/* Subroutine used by transport_write_message to scan character chunks for
newlines and act appropriately. The object is to minimise the number of writes.
In practice, either from_hack or (use_crlf & smtp_dots) is likely to be set,
but not both. However, the code works with any combination.

If the from hack or smtp_dots options are requested, we must do the necessary
at the start of the chunk if it is the start of the data section, or if the
previous last char was '\n'. This is handled by a static flag.

If a transport wants data transfers to be timed, it sets a non-zero value
in transport_chunk_timeout and sets up transport_timeout_handler() as the
SIGALRM handler. A non-zero transport_chunk_timeout causes a timer to be set
for each chunk of data written from here via write_block() above. If time runs
out, then a write() fails and provokes an error return. The caller can then
inspect transport_sigalrm_seen to check for a timeout.

Arguments:
  fd         file descript to write to
  chunk      pointer to data to write
  len        length of data to write
  from_hack  TRUE if the "from hack" is wanted
  usr_crlf   TRUE if CR LF is wanted at the end of each line
  smtp_dots  TRUE if the SMTP dot-escaping algorithm is wanted

Returns:     TRUE on success, FALSE on failure (with errno preserved)
*/

static BOOL was_nl;     /* NL status flag */

static BOOL
write_chunk(int fd, char *chunk, int len, BOOL from_hack, BOOL use_crlf,
  BOOL smtp_dots)
{
char *start = chunk;
char *end = chunk + len;
char *outptr = deliver_out_buffer;
char *ptr;

/* If none of the processing options is set, we can just write out the
block in one go without further ado. */

if (!(from_hack || use_crlf || smtp_dots)) return write_block(fd, chunk, len);

/* Otherwise, scan the buffer for newlines while copying into the output
buffer. This is to ensure we write the main data output in big chunks. Assume
len > 0. First handle the case when the preceding char was NL. */

if (was_nl)
  {
  if (from_hack && len >= 5 && strncmp(start, "From ", 5) == 0)
    *outptr++ = '>';
  else if (smtp_dots && *start == '.')
    *outptr++ = '.';
  was_nl = FALSE;
  }

/* Now process the characters in the chunk. */

for (ptr = start; ptr < end; ptr++)
  {
  register int ch;

  /* If there isn't room for another 4 characters, flush the buffer. */

  if ((len = outptr - deliver_out_buffer) > DELIVER_BUFFER_SIZE - 5)
    {
    if (!write_block(fd, deliver_out_buffer, len)) return FALSE;
    outptr = deliver_out_buffer;
    }

  if ((ch = *ptr) == '\n')
    {
    int left = end - ptr - 1;  /* count of chars left after NL */

    /* Insert CR before NL if required */

    if (use_crlf) *outptr++ = '\r';

    /* Now the NL */

    *outptr++ = '\n';

    /* If it happens that \n is the last character in the chunk, set the
    flag for next time (and for checking the final char). */

    if (left <= 0) was_nl = TRUE;

    /* The "from hack" inserts ">" before any line starting with "From ". It
    is a case-sensitive test. */

    else if (from_hack && left >= 5 && strncmp(ptr+1, "From ", 5) == 0)
      *outptr++ = '>';

    /* The smtp dot convention inserts a dot before any line starting with a
    dot. (We don't need to test left >= 1 because we know it isn't <= 0.) */

    else if (smtp_dots && ptr[1] == '.') *outptr++ = '.';
    }

  else *outptr++ = ch;
  }

/* Write out any remaining data in the buffer before returning. */

return (len = outptr - deliver_out_buffer) <= 0 ||
  write_block(fd, deliver_out_buffer, len);
}




/*************************************************
*                Write the message               *
*************************************************/

/* This function writes the message to the given file descriptor. The headers
are in the in-store data structure, and the rest of the message is in the open
file descriptor deliver_datafile. Make sure we start it at the beginning.

. If add_return_path is TRUE, a "return-path:" header is added to the message,
  containing the sender's address, unless it is null, or the user_null_sender
  flag is set (indicating an error message), in which case just "<>" is given.

. If add_envelope_to is TRUE, a "envelope-to:" header is added to the message,
  giving the top-level envelope address that caused this delivery to happen.

. If add_delivery_date is TRUE, a "delivery-date:" header is added to the
  message. It gives the time and date that delivery took place.

. If from_hack is true, the "from hack" is done to lines starting with the text
  "From " (what a shoddy specification). The from hack is not applied to
  headers.

. If use_crlf is true, newlines are turned into CRLF (SMTP output).

. If smtp_dots is true, lines starting with . get an extra . added.

The yield is TRUE if all went well, and FALSE if not. Exit *immediately* after
any writing or reading error, leaving the code in errno intact. Error exits
can include timeouts for certain transports, which are requested by setting
transport_chunk_timeout non-zero.

Arguments:
  addr                  (chain of) addresses (for extra headers)
  fd                    file descriptor to write the message to
  options               bit-wise options:
    add_return_path       if TRUE, add a "return-path" header
    add_envelope_to       if TRUE, add a "envelope-to" header
    add_delivery_date     if TRUE, add a "delivery-date" header
    from_hack             if TRUE, apply the "from hack"
    use_crlf              if TRUE, turn NL into CR LF
    smtp_dots             if TRUE, implement the SMTP dot escaping algorithm
  errors_to             if not NULL, used for the return-path header
  size_limit            if > 0, this is a limit to the size of message written;
                          it is used when returning messages to their senders,
                          and is approximate rather than exact, owing to chunk
                          buffering

Returns:                TRUE on success; FALSE (with errno) on failure
*/

BOOL
transport_write_message(address_item *addr, int fd, int options,
  char *errors_to, int size_limit)
{
int written = 0;
int len;
header_line *h;
BOOL from_hack = (options & topt_from_hack) != 0;
BOOL use_crlf  = (options & topt_use_crlf)  != 0;
BOOL smtp_dots = (options & topt_smtp_dots) != 0;

/* Add return-path: if requested, and there is one. */

if ((options & topt_add_return_path) != 0)
  {
  char buffer[SENDER_ADDRESS_MAXLENGTH + 20];
  sprintf(buffer, "Return-path: <%s>\n",
    (errors_to != NULL)? errors_to : user_null_sender? "" : sender_address);
  len = strlen(buffer);
  if (!write_chunk(fd, buffer, len, FALSE, use_crlf, FALSE)) return FALSE;
  }

/* Add envelope-to: if requested, picking up from all the addresses. */

if ((options & topt_add_envelope_to) != 0)
  {
  int len;
  address_item *p;
  char buffer[256];
  strcpy(buffer, "Envelope-to: ");
  len = (int)strlen(buffer);

  for (p = addr; p != NULL; p = p->next)
    {
    address_item *pp = p;
    while (pp->parent != NULL) pp = pp->parent;

    strcpy(buffer + len, pp->orig);
    len += (int)strlen(pp->orig);

    if (p->next == NULL)
      {
      strcat(buffer, "\n");
      len += 1;
      }
    else
      {
      strcat(buffer, ",\n ");
      len += 3;
      }

    if (!write_chunk(fd, buffer, len, FALSE, use_crlf, FALSE)) return FALSE;
    buffer[0] = 0;
    len = 0;
    }
  }

/* Add delivery-date: if requested. */

if ((options & topt_add_delivery_date) != 0)
  {
  char buffer[100];
  sprintf(buffer, "Delivery-date: %s\n", tod_stamp(tod_full));
  len = strlen(buffer);
  if (!write_chunk(fd, buffer, len, FALSE, use_crlf, FALSE)) return FALSE;
  }

/* Then the message's headers. Don't write any that are flagged as "old"; that
means they were rewritten, or are a record of envelope rewriting, or were
removed (e.g. Bcc). */

for (h = header_list; h != NULL; h = h->next)
  {
  if (h->type == htype_old) continue;
  if (!write_chunk(fd, h->text, h->slen, FALSE, use_crlf, smtp_dots))
    return FALSE;
  }

/* Add on any address-specific headers at the end, copying from all
addresses that are being transported. */

while (addr != NULL)
  {
  for (h = addr->extra_headers; h!= NULL; h = h->next)
    if (!write_chunk(fd, h->text, h->slen, FALSE, use_crlf, smtp_dots))
      return FALSE;
  addr = addr->next;
  }


/* Separate headers from body with a blank line; set the static flag for
the chunk writer. Note: use_crlf won't be in use when writing to a file, so we
are only every trying to write one byte to a file. If this fails because of
quote excession, a proper errno should result. */

if (use_crlf? (write(fd, "\r\n", 2) != 2) : (write(fd, "\n", 1) != 1))
  return FALSE;
was_nl = TRUE;

/* Ensure the body is positioned at the start of its file, following the
message id if present, then write it, applying the size limit if required. */

lseek(deliver_datafile, data_start_offset, SEEK_SET);
while ((len = read(deliver_datafile,deliver_in_buffer,DELIVER_BUFFER_SIZE)) > 0)
  {
  if (!write_chunk(fd, deliver_in_buffer, len, from_hack, use_crlf, smtp_dots))
    return FALSE;
  if (size_limit > 0)
    {
    written += len;
    if (written > size_limit) break;
    }
  }

/* A read error on the body will have left len == -1 and errno set. */

if (len != 0) return FALSE;

/* If the last character of the body was not '\n', manufacture one. Note:
use_crlf won't be in use when writing to a file, so we are only every trying to
write one byte to a file. If this fails because of quote excession, a proper
errno should result. */

if (!was_nl)
  {
  if (use_crlf? (write(fd, "\r\n", 2) != 2) : (write(fd, "\n", 1) != 1))
    return FALSE;
  }

return TRUE;
}




/*************************************************
*            Update waiting database             *
*************************************************/

/* This is called when an address is deferred by remote transports that are
capable of sending more than one message over one connection. A database is
maintained for each transport, keeping track of which messages are waiting for
which hosts. The transport can then consult this when eventually a successful
delivery happens, and if it finds that another message is waiting for the same
host, it can fire up a new process to deal with it using the same connection.

The database records are keyed by host name. They can get full if there are
lots of messages waiting, and so there is a continuation mechanism for them.

Each record contains a list of message ids, packed end to end without any
zeros. Each one is MESSAGE_ID_LENGTH bytes long. The count field says how many
in this record, and the sequence field says if there are any other records for
this host. If the sequence field is 0, there are none. If it is 1, then another
record with the name <hostname>:0 exists; if it is 2, then two other records
with sequence numbers 0 and 1 exist, and so on.

Currently, an exhaustive search of all continuation records has to be done to
determine whether to add a message id to a given record. This shouldn't be
too bad except in extreme cases. I can't figure out a *simple* way of doing
better.

Old records should eventually get swept up by the exim_tidydb utility.

Arguments:
  hostlist  list of hosts that this message could be sent to
  tpname    name of the transport

Returns:    nothing
*/

void
transport_update_waiting(host_item *hostlist, char *tpname)
{
char buffer[256];
char *prevname = "";
host_item *host;
EXIM_DB *dbm_file;

/* Open the database for this transport */

sprintf(buffer, "wait-%s", tpname);
dbm_file = db_open(buffer, O_RDWR|O_CREAT);
if (dbm_file == NULL)
  {
  log_write(0, LOG_MAIN, "Failed to open %s database: %s", buffer,
    strerror(errno));
  return;
  }

/* Scan the list of hosts for which this message is waiting, and ensure
that the message id is in each host record. */

for (host = hostlist; host!= NULL; host = host->next)
  {
  BOOL already = FALSE;
  db_wait *host_record;
  char *s;
  int i, host_length;

  /*Skip if this is the same host as we just processed; otherwise remember
  the name for next time. */

  if (strcmp(prevname, host->name) == 0) continue;
  prevname = host->name;

  /* Look up the host record; if there isn't one, make an empty one. */

  host_record = db_read(dbm_file, host->name);
  if (host_record == NULL)
    {
    host_record = store_malloc(sizeof(db_wait) + MESSAGE_ID_LENGTH);
    host_record->count = host_record->sequence = 0;
    }

  /* Compute the current length */

  host_length = host_record->count * MESSAGE_ID_LENGTH;

  /* Search the record to see if the current message is already in it. */

  for (s = host_record->text; s < host_record->text + host_length;
       s += MESSAGE_ID_LENGTH)
    {
    if (strncmp(s, message_id, MESSAGE_ID_LENGTH) == 0)
      { already = TRUE; break; }
    }

  /* If we haven't found this message in the main record, search any
  continuation records that exist. */

  for (i = host_record->sequence - 1; i >= 0 && !already; i--)
    {
    db_wait *cont;
    sprintf(buffer, "%s:%d", host->name, i);
    cont = db_read(dbm_file, buffer);
    if (cont != NULL)
      {
      int clen = cont->count * MESSAGE_ID_LENGTH;
      for (s = cont->text; s < cont->text + clen; s += MESSAGE_ID_LENGTH)
        {
        if (strncmp(s, message_id, MESSAGE_ID_LENGTH) == 0)
          { already = TRUE; break; }
        }
      store_free(cont);
      }
    }

  /* If this message is already in a record, no need to update. */

  if (already) continue;


  /* If this record is full, write it out with a new name constructed
  from the sequence number, increase the sequence number, and empty
  the record. */

  if (host_record->count >= WAIT_NAME_MAX)
    {
    sprintf(buffer, "%s:%d", host->name, host_record->sequence);
    db_write(dbm_file, buffer, host_record, sizeof(db_wait) + host_length);
    host_record->sequence++;
    host_record->count = 0;
    host_length = 0;
    }

  /* If this record is not full, increase the size of the record to
  allow for one new message id. */

  else
    {
    db_wait *newr =
      store_malloc(sizeof(db_wait) + host_length + MESSAGE_ID_LENGTH);
    memcpy(newr, host_record, sizeof(db_wait) + host_length);
    host_record = newr;
    }

  /* Now add the new name on the end */

  memcpy(host_record->text + host_length, message_id, MESSAGE_ID_LENGTH);
  host_record->count++;
  host_length += MESSAGE_ID_LENGTH;

  /* Update the database */

  db_write(dbm_file, host->name, host_record, sizeof(db_wait) + host_length);
  }

/* All now done */

db_close(dbm_file);
}




/*************************************************
*         Test for waiting messages              *
*************************************************/

/* This function is called by a remote transport which uses the previous
function to remember which messages are waiting for which remote hosts. It's
called after a successful delivery and its job is to check whether there is
another message waiting for the same host. However, it doesn't do this if the
current continue sequence is greater than the maximum supplied as an argument,
or greater than the global batch_max, if that is set.

Arguments:
  transport_name    name of the transport
  hostname          name of the host
  local_batch_max   maximum number of messages down one connection
                      as set by the caller transport
  new_message_id    set to the message id of a waiting message

Returns:            TRUE if new_message_id set; FALSE otherwise
*/

BOOL
transport_check_waiting(char *transport_name, char *hostname,
  int local_batch_max, char *new_message_id)
{
db_wait *host_record;
int host_length, path_len;
EXIM_DB *dbm_file;
char buffer[256];

DEBUG(4)
  {
  debug_printf("transport_check_waiting entered\n");
  debug_printf("  sequence=%d local_max=%d global_max=%d\n",
    continue_sequence, local_batch_max, batch_max);
  }

/* Do nothing if we have hit the batch maximum. */

if ((local_batch_max > 0 && continue_sequence >= local_batch_max) ||
    (batch_max >= 0 && continue_sequence >= batch_max))
  {
  DEBUG(4) debug_printf("batch max reached: returning\n");
  return FALSE;
  }

/* Open the waiting information database. */

sprintf(buffer, "wait-%s", transport_name);
dbm_file = db_open(buffer, O_RDWR|O_CREAT);
if (dbm_file == NULL)
  {
  log_write(0, LOG_MAIN, "Failed to open %s database", buffer);
  return FALSE;
  }

/* See if there is a record for this host; if not, there's nothing to do. */

host_record = db_read(dbm_file, hostname);
if (host_record == NULL)
  {
  db_close(dbm_file);
  DEBUG(4) debug_printf("no messages waiting for %s\n", hostname);
  return FALSE;
  }

/* If the data in the record looks corrupt, just log something and
don't try to use it. */

if (host_record->count > WAIT_NAME_MAX)
  {
  log_write(0, LOG_MAIN|LOG_PANIC, "smtp-wait database entry for %s has bad "
    "count=%d (max=%d)", hostname, host_record->count, WAIT_NAME_MAX);
  return FALSE;
  }

/* Scan the message ids in the record from the end towards the beginning,
until one is found for which a spool file actually exists. If the record gets
emptied, delete it and continue with any continuation records that may exist.
*/

host_length = host_record->count * MESSAGE_ID_LENGTH;

/* Loop to handle continuation host records in the database */

for (;;)
  {
  BOOL found = FALSE;

  sprintf(buffer, "%s/input/", spool_directory);
  path_len = (int)strlen(buffer);

  for (host_length -= MESSAGE_ID_LENGTH; host_length >= 0;
       host_length -= MESSAGE_ID_LENGTH)
    {
    struct stat statbuf;
    strncpy(new_message_id, host_record->text + host_length,
      MESSAGE_ID_LENGTH);
    new_message_id[MESSAGE_ID_LENGTH] = 0;
    sprintf(buffer + path_len, "%s-D", new_message_id);

    /* The listed message may be the one we are currently processing. If
    so, we want to remove it from the list without doing anything else.
    If not, do a stat to see if it is an existing message. If it is, break
    the loop to handle it. No need to bother about locks; as this is all
    "hint" processing, it won't matter if it doesn't exist by the time exim
    actually tries to deliver it. */

    if (strcmp(new_message_id, message_id) != 0 &&
        stat(buffer, &statbuf) == 0)
      {
      found = TRUE;
      break;
      }
    }

  /* If we have removed all the message ids from the record delete the record.
  If there is a continuation record, fetch it and remove it from the file,
  as it will be rewritten as the main record. Repeat in the case of an
  empty continuation. */

  while (host_length <= 0)
    {
    int i;
    db_wait *newr = NULL;

    /* Search for a continuation */

    for (i = host_record->sequence - 1; i >= 0 && newr == NULL; i--)
      {
      sprintf(buffer, "%s:%d", hostname, i);
      newr = db_read(dbm_file, buffer);
      }

    /* If no continuation, delete the current and break the loop */

    if (newr == NULL)
      {
      db_delete(dbm_file, hostname);
      break;
      }

    /* Else replace the current with the continuation */

    db_delete(dbm_file, buffer);
    store_free(host_record);
    host_record = newr;
    host_length = host_record->count * MESSAGE_ID_LENGTH;
    }

  /* If we found an existing message, break the continuation loop. */

  if (found) break;

  /* If host_length <= 0 we have emptied a record and not found a good message,
  and there are no continuation records. Otherwise there is a continuation
  record to process. */

  if (host_length <= 0)
    {
    db_close(dbm_file);
    DEBUG(4) debug_printf("waiting messages already delivered\n");
    return FALSE;
    }
  }

/* Control gets here when an existing message has been encountered; its
id is in new_message_id, and host_length is the revised length of the
host record. If it is zero, the record has been removed. Update the
record if required, close the database, and return TRUE. */

if (host_length > 0)
  {
  host_record->count = host_length/MESSAGE_ID_LENGTH;
  db_write(dbm_file, hostname, host_record, (int)sizeof(db_wait) + host_length);
  }

db_close(dbm_file);
return TRUE;
}



/*************************************************
*    Deliver waiting message down same socket    *
*************************************************/

/* Fork a new exim process to deliver the message, and do a re-exec, both to
get a clean delivery process, and to regain root privilege in cases where it
has been given away.

Arguments:
  transport_name  to pass to the new process
  hostname        ditto
  id              the new message to process
  fd_in           the input socket
  fd_out          the output socket

Returns:          FALSE if fork fails; TRUE otherwise
*/

BOOL
transport_pass_socket(char *transport_name, char *hostname, char *id,
  int socket_fd)
{
pid_t pid;
int status;

if ((pid = vfork()) == 0)
  {
  int fd, i;
  int fd_in = socket_fd;
  int fd_out = dup(fd_in);
  char *argv[11];

  /* Disconnect entirely from the parent process. */

  if (vfork() != 0) _exit(EXIT_SUCCESS);

  /* Set up the calling arguments */

  i = 0;
  argv[i++] = exim_path;
  if (debug_level > 0)
    argv[i++] = string_sprintf("-d%d", debug_level);
  if (batch_max >= 0)
    argv[i++] = string_sprintf("-oB%d", batch_max);
  if (config_changed)
    {
    argv[i++] = "-C";
    argv[i++] = config_filename;
    }
  argv[i++] = "-MC";
  argv[i++] = transport_name;
  argv[i++] = hostname;
  argv[i++] = string_sprintf("%d", continue_sequence + 1);
  argv[i++] = id;
  argv[i++] = NULL;

  /* Arrange for the channel to be on stdin and stdout */

  for (fd = mac_maxfd; fd >= 0; fd--)
    if (fd != fd_in && fd != fd_out &&
      (debug_file == NULL || fd != fileno(debug_file)))
        close(fd);

  if (fd_in != 0)
    {
    dup2(fd_in, 0);
    close(fd_in);
    }

  if (fd_out != 1)
    {
    dup2(fd_out, 1);
    close(fd_out);
    }

  DEBUG(4) debug_printf("fork %s %s %s %s %s %s %s %s %s %s\n", argv[0],
    argv[1], argv[2], argv[3], argv[4], argv[5],
      (argv[6] == NULL)? "" : argv[6],
      (argv[7] == NULL)? "" : argv[7],
      (argv[8] == NULL)? "" : argv[8],
      (argv[9] == NULL)? "" : argv[9]);

  execv(argv[0], argv);

  /* Failed to execv, or getrlimit failed */

  DEBUG(4) debug_printf("fork failed: %s\n", strerror(errno));

  _exit(errno);         /* Note: must be _exit(), NOT exit() */
  }

/* If the process creation succeeded, wait for the first-level child, which
immediately exits, leaving the second level process entirely disconnected from
this one. */

if (pid > 0)
  {
  int rc;
  while ((rc = wait(&status)) != pid && (rc >= 0 || errno != ECHILD));
  return TRUE;
  }
else return FALSE;
}



/*************************************************
*            Test host for serialization         *
*************************************************/

/* This function is called when a host is listed for serialization of
connections. We open the relevant database and look for a record, which implies
an existing connection. If not found, create one and return TRUE.

Arguments:
  transport_name    name of transport
  hostname          name of host

Returns:            TRUE if OK to proceed; FALSE otherwise
*/


BOOL
transport_check_serialized(char *transport_name, char *hostname)
{
db_serialize *host_record;
db_serialize new_record;
EXIM_DB *dbm_file;
char buffer[256];

DEBUG(4) debug_printf("transport_check_serialized entered\n");

/* Open and lock the waiting information database. */

sprintf(buffer, "serialize-%s", transport_name);
dbm_file = db_open(buffer, O_RDWR|O_CREAT);
if (dbm_file == NULL)
  {
  log_write(0, LOG_MAIN, "Failed to open %s database", buffer);
  return FALSE;
  }

/* See if there is a record for this host; if there is, we cannot proceed with
the connection unless the record is very old. */

host_record = db_read(dbm_file, hostname);
if (host_record != NULL && time(NULL) - host_record->time_stamp < 6*60*60)
  {
  db_close(dbm_file);
  DEBUG(4) debug_printf("outstanding connection to %s\n", hostname);
  return FALSE;
  }

/* We can proceed - insert a new record or update the old one. At present
the count field is not used; just set it to 1. */

new_record.count = 1;
db_write(dbm_file, hostname, &new_record, (int)sizeof(db_serialize));
db_close(dbm_file);
return TRUE;
}



/*************************************************
*            Release host serialization          *
*************************************************/

/* This function is called when a serialized host's connection ends.
We open the relevant database and delete it's record.

Arguments:
  transport_name    name of transport
  hostname          name of host

Returns:            nothing
*/

void
transport_end_serialized(char *transport_name, char *hostname)
{
EXIM_DB *dbm_file;
char buffer[256];

DEBUG(4) debug_printf("transport_end_serialized entered\n");

sprintf(buffer, "serialize-%s", transport_name);
dbm_file = db_open(buffer, O_RDWR|O_CREAT);
if (dbm_file == NULL)
  {
  log_write(0, LOG_MAIN, "Failed to open %s database", buffer);
  return;
  }

db_delete(dbm_file, hostname);
db_close(dbm_file);
}

/* End of transport.c */
