/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Functions for writing log files. */


#include "exim.h"



/*************************************************
*           Local static variables               *
*************************************************/

static char *mainlog_name;

static int mainlogfd = -1;
static int mainlog_inode = -1;
static int processlogfd = -1;
static int rejectlogfd = -1;

static BOOL panic_recurseflag = FALSE;




/*************************************************
*                Open a log file                 *
*************************************************/

/* This function opens one of a number of logs, which all reside in the same
directory, creating the directory if it does not exist and an explicit path for
log files has not been specified. This may be called recursively on failure, in
order to open the panic log.

If exim is configured to avoid running as root wherever possible, the log files
must be owned by the non-privileged user. To ensure this, first try an open
without O_CREAT - most of the time this will succeed. If it fails, try to
create the file, and adjust the owner if necessary.

Arguments:
  fd       where to return the resulting file descriptor
  name     log file basic name ("main", "reject", etc.)

Returns:   nothing
*/

static void
open_log(int *fd, char *name)
{
char buffer[256];

if (log_file_path[0] == 0)
  sprintf(buffer, "%s/log/%slog", spool_directory, name);
else
  sprintf(buffer, log_file_path, name);

if (strcmp(name, "main") == 0) mainlog_name = string_copy(buffer);

*fd = open(buffer, O_APPEND|O_WRONLY, LOG_MODE);
if (*fd >= 0) return;

/* Try creating the file */

*fd = open(buffer, O_CREAT|O_APPEND|O_WRONLY, LOG_MODE);

/* If creation failed and LOG_FILE_PATH is undefined, causing log_file_path to
be the null string, attempt to build a log directory in the spool directory. */

if (*fd < 0 && errno == ENOENT && log_file_path[0] == 0)
  {
  directory_make(spool_directory, "log", LOG_DIRECTORY_MODE);
  *fd = open(buffer, O_CREAT|O_APPEND|O_WRONLY, LOG_MODE);
  }

/* Creation succeeded; change owner if we are currently root, and ensure
the mode is as specified. */

if (*fd >= 0)
  {
  if (exim_uid_set && geteuid() == root_uid)
    chown(buffer, exim_uid, exim_gid);
  chmod(buffer, LOG_MODE);
  return;
  }

/* Creation failed */

log_write(0, LOG_PANIC_DIE, "Cannot open %s: %s", buffer, strerror(errno));
/* Never returns */
}



/*************************************************
*            Write message to log file           *
*************************************************/

/* The message always gets '\n' added on the end of it, since more than one
process may be writing to the log at once and we don't want intermingling to
happen in the middle of lines. To be absolutely sure of this we write the data
into a private buffer and then put it out in a single write() call.

The flags determine which log(s) the message is written to, and in the
case of the panic log, whether the process should die afterwards.

This function is called from some functions that are shared between exim and
its utilities. The variable really_exim is TRUE only when it is exim itself
that is running. If it is not, don't try to write to the log (permission will
probably be denied) - just write to stderr instead.

Avoid actually writing to the logs when exim is called with -bv or -bt to
test an address, but take other actions, such as panicking.

In exim proper, the buffer for building the message is got at start-up, so that
nothing gets done if it can't be got. However, some functions that are shared
occasionally obey log_write calls in error situations, and it is simplest to
put a single malloc() here rather than put one in each utility. Malloc is
used directly because the store functions may call log_write().

If a message_id exists, we include it after the timestamp.

Arguments:
  flags     each bit indicates some independent action:
              LOG_RECIPIENTS  add recipients list to message
              LOG_MAIN        write to main log
              LOG_PROCESS     write to process log
              LOG_REJECT      write to reject log
              LOG_PANIC       write to panic log
              LOG_PANIC_DIE   write to panic log and then crash
              LOG_CONFIG      add "Exim configuration error:\n  "
              LOG_CONFIG2     add "Exim configuration error for "
  level     write to main long only if log_level is >= this value
  format    a printf() format
  ...       arguments for format

Returns:    nothing
*/

void
log_write(int level, int flags, char *format, ...)
{
char *ptr;
BOOL testing = verify_only || address_test_mode;
int paniclogfd;
va_list ap;

/* Ensure we have a buffer (see comment above); this should never be obeyed
when running exim proper.  */

if (log_buffer == NULL)
  {
  log_buffer = (char *)malloc(LOG_BUFFER_SIZE);
  if (log_buffer == NULL)
    {
    fprintf(stderr, "exim: failed to get store for log buffer\n");
    exit(EXIT_FAILURE);
    }
  }

/* If debugging, show all log entries, but don't show headers. Do it all
in one go so that it doesn't get split when multi-processing. */

DEBUG(1)
  {
  ptr = log_buffer;
  sprintf(ptr, "LOG: %d%s%s%s%s\n  ", level,
    ((flags & LOG_MAIN) != 0)?    " MAIN"   : "",
    ((flags & LOG_PANIC) != 0)?   " PANIC"  : "",
    ((flags & LOG_PROCESS) != 0)? " PROCESS": "",
    ((flags & LOG_REJECT) != 0)?  " REJECT" : "");
  if ((flags & LOG_CONFIG) != 0)
    strcat(ptr, "Exim configuration error\n  ");
  else if ((flags & LOG_CONFIG2) != 0)
  strcat(ptr, "Exim configuration error for ");
  while(*ptr) ptr++;
  va_start(ap, format);
  vsprintf(ptr, format, ap);
  va_end(ap);
  while(*ptr) ptr++;
  strcat(ptr, "\n");
  fprintf(debug_file, "%s", log_buffer);
  fflush(debug_file);
  }

/* If no log file is specified, we are in a mess. */

if (flags == 0)
  log_write(0, LOG_PANIC_DIE, "log_write called with no flags set");

/* Create the main message in the log buffer, including the message
id except for the process log and when called by a utility. */

ptr = log_buffer;
if (really_exim && (flags & LOG_PROCESS) == 0 && message_id[0] != 0)
  sprintf(ptr, "%s %s ", tod_stamp(tod_log), message_id);
else sprintf(ptr, "%s ", tod_stamp(tod_log));

if ((flags & LOG_CONFIG) != 0)
  strcat(ptr, "Exim configuration error\n  ");
else if ((flags & LOG_CONFIG2) != 0)
  strcat(ptr, "Exim configuration error for ");

while(*ptr) ptr++;

va_start(ap, format);
vsprintf(ptr, format, ap);
while(*ptr) ptr++;
va_end(ap);

/* Add list of recipients to the message if required; the raw list,
before rewriting, was saved in raw_recipients. */

if ((flags & LOG_RECIPIENTS) != 0)
  {
  int i;
  sprintf(ptr, " for");
  while (*ptr) ptr++;
  for (i = 0; i < recipients_count; i++)
    {
    char *s = raw_recipients[i];
    if (log_buffer + LOG_BUFFER_SIZE - ptr < (int)strlen(s) + 3) break;
    sprintf(ptr, " %s", s);
    while (*ptr) ptr++;
    }
  }

sprintf(ptr, "\n");
while(*ptr) ptr++;

/* Handle loggable errors when running a utility, or when address testing.
Write to stderr unless debugging (when it will already have been written). */

if (!really_exim || testing)
  {
  if (debug_level <= 0) fprintf(stderr, "%s", log_buffer);
  if ((flags & LOG_PANIC_DIE) == LOG_PANIC_DIE) exit(EXIT_FAILURE);
  return;
  }

/* Handle the main log. The log gets left open during a message's delivery
once it has been opened, but we don't want to keep on writing to it for too
long after it has been renamed. Therefore, do a stat() and see if the inode
has changed, and if so, re-open. */

if ((flags & LOG_MAIN) != 0 && log_level >= level)
  {
  struct stat statbuf;
  if (mainlogfd >= 0)
    {
    if (stat(mainlog_name, &statbuf) < 0 || statbuf.st_ino != mainlog_inode)
      {
      DEBUG(1) debug_printf("Closing main log after inode change\n");
      close(mainlogfd);
      mainlogfd = -1;
      mainlog_inode = -1;
      }
    }
  if (mainlogfd < 0)
    {
    open_log(&mainlogfd, "main");     /* No return on error */
    if (fstat(mainlogfd, &statbuf) >= 0) mainlog_inode = statbuf.st_ino;
    }
  write(mainlogfd, log_buffer, ptr - log_buffer);
  }

/* Handle the log for rejected messages: log recipients and the headers if any,
but watch out for overflowing the buffer. Stick a separator between messages. */

if ((flags & LOG_REJECT) != 0)
  {
  header_line *h;

  if (rejectlogfd < 0)
    open_log(&rejectlogfd, "reject"); /* No return on error */

  if (recipients_count > 0)
    {
    int i;
    sprintf(ptr, "To: %s\n", recipients_list[0]);
    while (*ptr) ptr++;
    for (i = 1; i < recipients_count && i < 3; i++)
      {
      sprintf(ptr, "    %s\n", recipients_list[i]);
      while (*ptr) ptr++;
      }
    if (i < recipients_count)
      {
      strcat(ptr, "    ...\n");
      ptr += 8;
      }
    }

  for (h = header_list; h != NULL; h = h->next)
    {
    if (log_buffer + LOG_BUFFER_SIZE - ptr < (int)strlen(h->text) + 100) break;
    sprintf(ptr, "%c %s", h->type, h->text);
    while(*ptr) ptr++;
    }
  sprintf(ptr, "----------------------------------------"
    "--------------------------------------\n");
  while(*ptr) ptr++;
  write(rejectlogfd, log_buffer, ptr - log_buffer);
  }


/* Handle the process log file, where exim processes can be made to dump
details of what they are doing by sending them a USR1 signal. Note that
a message id is not automatically added above. */

if ((flags & LOG_PROCESS) != 0)
  {
  if (processlogfd < 0)
    open_log(&processlogfd, "process");  /* No return on error */
  write(processlogfd, log_buffer, ptr - log_buffer);
  }


/* Handle the panic log, which is not kept open like the others. If it fails to
open, there will be a recursive call that ends up here. We detect this and
attempt to write to the system log as a last-ditch try at telling somebody. In
all cases, try to write to stderr and/or debug_file. */

if ((flags & LOG_PANIC) != 0)
  {
  if (stderr != NULL && stderr != debug_file) fprintf(stderr, "%s", log_buffer);

  if (panic_recurseflag)
    {
    if (debug_file != NULL)
      fprintf(debug_file, "exim: could not open panic log: aborting\n");
    if (stderr != NULL && stderr != debug_file)
      fprintf(stderr, "exim: could not open panic log: aborting\n");
    syslog(LOG_MAIN|LOG_ERR, "exim: could not open panic log");
    exit(EXIT_FAILURE);
    }

  panic_recurseflag = TRUE;
  open_log(&paniclogfd, "panic");  /* Won't return on failure */
  panic_recurseflag = FALSE;
  write(paniclogfd, log_buffer, ptr - log_buffer);
  close(paniclogfd);

  if ((flags & LOG_PANIC_DIE) != LOG_PANIC) exit(EXIT_FAILURE);
  }
}



/*************************************************
*            Close any open log files            *
*************************************************/

void
log_close(void)
{
if (mainlogfd >= 0)
  { close(mainlogfd); mainlogfd= -1; }
if (processlogfd >= 0)
  { close(processlogfd); processlogfd = -1; }
if (rejectlogfd >= 0)
  { close(rejectlogfd); rejectlogfd = -1; }
}


/*************************************************
**************************************************
*             Stand-alone test program           *
**************************************************
*************************************************/

#ifdef STAND_ALONE
int main(void)
{
printf("spool directory = %s\n", spool_directory);
log_write(0, LOG_MAIN, "Test output to the log file");
return 0;
}

#endif

/* End of log.c */
