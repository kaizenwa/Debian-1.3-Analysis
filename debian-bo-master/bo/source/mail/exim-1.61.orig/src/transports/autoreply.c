/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "../exim.h"
#include "autoreply.h"



/* Options specific to the autoreply transport. They must be in alphabetic
order (note that "_" comes before the lower case letters). Those starting
with "*" are not settable by the user but are used by the option-reading
software for alternative value types. Some options are publicly visible and so
are stored in the driver instance block. These are flagged with opt_public. */

optionlist autoreply_transport_options[] = {
  { "*expand_group",     opt_stringptr | opt_hidden | opt_public,
      (void *)(offsetof(transport_instance, expand_gid)) },
  { "*expand_user",      opt_stringptr | opt_hidden | opt_public,
      (void *)(offsetof(transport_instance, expand_uid)) },
  { "*set_group",         opt_bool | opt_hidden | opt_public,
      (void *)(offsetof(transport_instance, gid_set)) },
  { "*set_user",          opt_bool | opt_hidden | opt_public,
      (void *)(offsetof(transport_instance, uid_set)) },
  { "bcc",               opt_stringptr,
      (void *)(offsetof(autoreply_transport_options_block, bcc)) },
  { "cc",                opt_stringptr,
      (void *)(offsetof(autoreply_transport_options_block, cc)) },
  { "file",              opt_stringptr,
      (void *)(offsetof(autoreply_transport_options_block, file)) },
  { "file_expand",     opt_bool,
      (void *)(offsetof(autoreply_transport_options_block, file_expand)) },
  { "file_optional",     opt_bool,
      (void *)(offsetof(autoreply_transport_options_block, file_optional)) },
  { "from",              opt_stringptr,
      (void *)(offsetof(autoreply_transport_options_block, from)) },
  { "group",             opt_expand_gid | opt_public,
      (void *)(offsetof(transport_instance, gid)) },
  { "headers",           opt_stringptr,
      (void *)(offsetof(autoreply_transport_options_block, headers)) },
  { "initgroups",        opt_bool | opt_public,
      (void *)(offsetof(transport_instance, initgroups)) },
  { "log",               opt_stringptr,
      (void *)(offsetof(autoreply_transport_options_block, logfile)) },
  { "mode",              opt_octint,
      (void *)(offsetof(autoreply_transport_options_block, mode)) },
  { "once",              opt_stringptr,
      (void *)(offsetof(autoreply_transport_options_block, oncelog)) },
  { "return_message",    opt_bool,
      (void *)(offsetof(autoreply_transport_options_block, return_message)) },
  { "subject",           opt_stringptr,
      (void *)(offsetof(autoreply_transport_options_block, subject)) },
  { "text",              opt_stringptr,
      (void *)(offsetof(autoreply_transport_options_block, text)) },
  { "to",                opt_stringptr,
      (void *)(offsetof(autoreply_transport_options_block, to)) },
  { "user",              opt_expand_uid | opt_public,
      (void *)(offsetof(transport_instance, uid)) },
};

/* Size of the options list. An extern variable has to be used so that its
address can appear in the tables drtables.c. */

int autoreply_transport_options_count =
  sizeof(autoreply_transport_options)/sizeof(optionlist);

/* Default private options block for the autoreply transport. */

autoreply_transport_options_block autoreply_transport_option_defaults = {
  NULL,           /* from */
  NULL,           /* to */
  NULL,           /* cc */
  NULL,           /* bcc */
  NULL,           /* subject */
  NULL,           /* headers */
  NULL,           /* text */
  NULL,           /* file */
  NULL,           /* logfile */
  NULL,           /* oncelog */
  0600,           /* mode */
  FALSE,          /* file_expand */
  FALSE,          /* file_optional */
  FALSE           /* return message */
};



/*************************************************
*          Initialization entry point            *
*************************************************/

/* Called for each instance, after its options have been read, to
enable consistency checks to be done, or anything else that needs
to be set up. */

void
autoreply_transport_init(transport_instance *tblock)
{
/*
autoreply_transport_options_block *ob =
  (autoreply_transport_options_block *)(tblock->options_block);
*/

/* If a fixed uid field is set, then a gid field must also be set. */

if (tblock->uid_set && !tblock->gid_set)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
    "user set without group for the %s transport", tblock->name);
}




/*************************************************
*          Expand string and check               *
*************************************************/

/* If the expansion fails, the error is set up in the address. Expanded
strings must be checked to ensure they contain only printing characters
and white space. If not, the function fails.

Arguments:
   s         string to expand
   addr      address that is being worked on
   name      transport name, for error text
   text      if TRUE, \n is allowed in the expanded string

Returns:     expanded string if expansion succeeds;
             NULL otherwise
*/

static char *
checkexpand(char *s, address_item *addr, char *name, BOOL text)
{
char *t;
char *ss = expand_string(s);

if (ss == NULL)
  {
  addr->transport_return = FAIL;
  addr->message = string_sprintf("Expansion of \"%s\" failed in %s transport: "
    "%s", s, name, expand_string_message);
  return NULL;
  }

for (t = ss; *t != 0; t++)
  {
  if (!isprint(*t) && *t != '\t' && (*t != '\n' || !text))
    {
    s = string_printing(s, TRUE);
    addr->transport_return = FAIL;
    addr->message = string_sprintf("Expansion of \"%s\" in %s transport "
      "contains non-printing characters", s, name);
    return NULL;
    }
  }

return ss;
}




/*************************************************
*              Main entry point                  *
*************************************************/

/* See local README for interface details. */

void
autoreply_transport_entry(
  transport_instance *tblock,      /* data for this instantiation */
  address_item *addr)              /* address we are working on */
{
int fd, pid, rc;
int log_fd = -1;
BOOL file_expand;
char *from, *to, *cc, *bcc, *subject, *headers, *text, *file;
char *logfile, *oncelog;
char *now = NULL;
header_line *h;
EXIM_DB *dbm_file = NULL;
FILE *f;
FILE *ff = NULL;
autoreply_transport_options_block *ob =
  (autoreply_transport_options_block *)(tblock->options_block);

DEBUG(2) debug_printf("%s transport entered\n", tblock->name);

/* Set up for the good case */

addr->transport_return = OK;
addr->basic_errno = 0;

/* If the address is pointing to a reply block, then take all the data
from that block. It has typically been set up by a mail filter processing
director. Otherwise, the data must be supplied by this transport, and
it has to be expanded here. */

if (addr->reply != NULL)
  {
  DEBUG(9) debug_printf("taking data from address\n");
  from = addr->reply->from;
  to = addr->reply->to;
  cc = addr->reply->cc;
  bcc = addr->reply->bcc;
  subject = addr->reply->subject;
  headers = addr->reply->headers;
  text = addr->reply->text;
  file = addr->reply->file;
  logfile = addr->reply->logfile;
  oncelog = addr->reply->oncelog;
  file_expand = addr->reply->file_expand;
  }
else
  {
  DEBUG(9) debug_printf("taking data from transport\n");
  from = ob->from;
  to = ob->to;
  cc = ob->cc;
  bcc = ob->bcc;
  subject = ob->subject;
  headers = ob->headers;
  text = ob->text;
  file = ob->file;
  logfile = ob->logfile;
  oncelog = ob->oncelog;
  file_expand = ob->file_expand;

  if ((from  != NULL &&
        (from = checkexpand(from, addr, tblock->name, FALSE)) == NULL) ||
      (to    != NULL &&
        (to = checkexpand(to, addr, tblock->name, FALSE)) == NULL) ||
      (cc    != NULL &&
        (cc = checkexpand(cc, addr, tblock->name, FALSE)) == NULL) ||
      (bcc   != NULL &&
        (bcc = checkexpand(bcc, addr, tblock->name, FALSE)) == NULL) ||
      (subject   != NULL &&
        (subject = checkexpand(subject, addr, tblock->name, FALSE)) == NULL) ||
      (headers != NULL &&
        (headers = checkexpand(headers, addr, tblock->name, TRUE)) == NULL) ||
      (text  != NULL &&
        (text = checkexpand(text, addr, tblock->name, TRUE)) == NULL) ||
      (file  != NULL &&
        (file = checkexpand(file, addr, tblock->name, FALSE)) == NULL) ||
      (logfile != NULL &&
        (logfile = checkexpand(logfile, addr, tblock->name, FALSE)) == NULL) ||
      (oncelog != NULL &&
        (oncelog = checkexpand(oncelog, addr, tblock->name, FALSE)) == NULL))
    return;
  }


/* If the -N option is set, can't do any more. */

if (dont_deliver)
  {
  debug_printf("*** delivery by %s transport bypassed by -N option\n",
    tblock->name);
  addr->transport_return = OK;
  return;
  }


/* If the oncelog field is set, we send want to send only one message to
the given recipient(s). This works only on the "To" field. If there is no "To"
field the message is always sent. The "To" field is used as the key in a dbm
file; if it contains more than one recipient, the effect might not be quite as
envisaged. */

if (oncelog != NULL && to != NULL)
  {
  EXIM_DATUM key_datum, result_datum;
  dbm_file = EXIM_DBOPEN(oncelog, O_RDWR|O_CREAT, ob->mode);
  if (dbm_file == NULL)
    {
    addr->transport_return = DEFER;
    addr->message = string_sprintf("Failed to open %s file %s when sending "
      "message from %s transport: %s", EXIM_DBTYPE, oncelog, tblock->name,
      strerror(errno));
    return;
    }

  EXIM_DATUM_DATA(key_datum) = to;
  EXIM_DATUM_SIZE(key_datum) = (int)strlen(to) + 1;

  if (EXIM_DBGET(dbm_file, key_datum, result_datum))
    {
    DEBUG(9) debug_printf("message already sent to %s\n", to);
    log_fd = open(logfile, O_WRONLY|O_APPEND|O_CREAT, ob->mode);
    if (log_fd >= 0)
      {
      char *ptr = log_buffer;
      if (now == NULL) now = tod_stamp(tod_log);
      sprintf(ptr, "%s\n  previously sent to %s\n", now, to);
      while(*ptr) ptr++;
      write(log_fd, log_buffer, ptr - log_buffer);
      }
    EXIM_DBCLOSE(dbm_file);
    return;
    }
  }


/* Ensure any requested file is available. */

if (file != NULL)
  {
  ff = fopen(file, "r");
  if (ff == NULL && !ob->file_optional)
    {
    addr->transport_return = DEFER;
    addr->message = string_sprintf("Failed to open file %s when sending "
      "message from %s transport: %s", file, tblock->name, strerror(errno));
    return;
    }
  }

/* Make a subprocess to send the message */

pid = child_open(mailer_argv, NULL, 077, NULL, NULL, &fd,
  (debug_file != NULL)? fileno(debug_file) : -1);

/* Creation of child failed; defer this delivery. */

if (pid < 0)
  {
  DEBUG(2) debug_printf("Failed to create child process\n");
  addr->transport_return = DEFER;
  addr->message = string_sprintf("Failed to create child process to send "
    "message from %s transport", tblock->name);
  return;
  }

/* Create the message to be sent - recipients are taken from the headers,
as the -t option is used. The "headers" stuff *must* be last in case there
are newlines in it which might, if placed earlier, screw up other headers. */

f = fdopen(fd, "w");

if (from != NULL) fprintf(f, "From: %s\n", from);
if (to != NULL) fprintf(f, "To: %s\n", to);
if (cc != NULL) fprintf(f, "Cc: %s\n", cc);
if (bcc != NULL) fprintf(f, "Bcc: %s\n", bcc);
if (subject != NULL) fprintf(f, "Subject: %s\n", subject);

/* Generate In-Reply-To from the message_id header; there should
always be one, but code defensively. */

for (h = header_list; h != NULL; h = h->next)
  if (h->type == htype_id) break;

if (h != NULL)
  {
  char *s = strchr(h->text, ':') + 1;
  while (isspace(*s)) s++;
  fprintf(f, "In-Reply-To: %s\n", s);
  }

/* Add any specially requested headers */

if (headers != NULL) fprintf(f, "%s\n", headers);
fprintf(f, "\n");

if (text != NULL)
  {
  fprintf(f, "%s", text);
  if (text[(int)strlen(text)-1] != '\n') fprintf(f, "\n");
  }

if (ff != NULL)
  {
  while (fgets(big_buffer, big_buffer_size, ff) != NULL)
    {
    if (file_expand)
      {
      char *s = expand_string(big_buffer);
      fprintf(f, "%s", (s == NULL)? big_buffer : s);
      if (s != NULL) store_free(s);
      }
    else fprintf(f, "%s", big_buffer);
    }
  }

/* Copy the original message if required, observing the return size
limit. */

if (ob->return_message)
  {
  if (return_size_limit > 0)
    {
    struct stat statbuf;
    int max = (return_size_limit/DELIVER_BUFFER_SIZE + 1)*DELIVER_BUFFER_SIZE;
    if (fstat(deliver_datafile, &statbuf) == 0 && statbuf.st_size > max)
      {
      fprintf(f, "\n"
"------ This is a copy of your message, including all the headers.\n"
"------ The body of your message is %d characters long; only the first\n"
"------ %d or so are included here.\n\n", (int)statbuf.st_size, (max/1000)*1000);
      }
    else fprintf(f, "\n"
"------ This is a copy of your message, including all the headers. ------\n\n");
    }
  else fprintf(f, "\n"
"------ This is a copy of your message, including all the headers. ------\n\n");

  fflush(f);
  transport_write_message(addr, fileno(f), 0, NULL, return_size_limit);
  }

/* End the message and wait for the child process to end; no timeout. */

fclose(f);
rc = child_close(pid, 0);

/* Update the "sent to" log whatever the yield. This errs on the side of
missing out a message rather than risking sending more than one. */

if (dbm_file != NULL)
  {
  EXIM_DATUM key_datum, value_datum;
  if (now == NULL) now = tod_stamp(tod_log);
  EXIM_DATUM_DATA(key_datum) = to;
  EXIM_DATUM_SIZE(key_datum) = (int)strlen(to) + 1;
  EXIM_DATUM_DATA(value_datum) = now;
  EXIM_DATUM_SIZE(value_datum) = (int)strlen(now) + 1;
  EXIM_DBPUT(dbm_file, key_datum, value_datum);
  EXIM_DBCLOSE(dbm_file);
  }

/* If sending failed, defer to try again - but if once is set the next
try will skip, of course. */

if (rc != 0)
  {
  addr->transport_return = DEFER;
  addr->message = string_sprintf("Failed to send message from %s "
    "transport (%d)", tblock->name, rc);
  }

/* Log the sending of the message if successful and required. If the file
fails to open, it's hard to know what to do. We cannot write to the Exim
log from here, since we may be running under an unprivileged uid. We don't
want to fail the delivery, since the message has been successfully sent. For
the moment, ignore open failures. Write the log entry as a single write() to a
file opened for appending, in order to avoid interleaving of output from
different processes. The log_buffer can be used exactly as for main log
writing. Oh, wouldn't it be nice if all C systems had ANSI-compliant sprintf()
funtions? For now, code without using the result of sprintf(). */

if (logfile != NULL)
  {
  if (now == NULL) now = tod_stamp(tod_log);
  if (log_fd < 0)
    log_fd = open(logfile, O_WRONLY|O_APPEND|O_CREAT, ob->mode);
  if (log_fd >= 0)
    {
    char *ptr = log_buffer;
    DEBUG(9) debug_printf("logging message details\n");
    sprintf(ptr, "%s\n", now);
    while(*ptr) ptr++;
    if (from != NULL)
      {
      sprintf(ptr, "  From: %s\n", from);
      while(*ptr) ptr++;
      }
    if (to != NULL)
      {
      sprintf(ptr, "  To: %s\n", to);
      while(*ptr) ptr++;
      }
    if (cc != NULL)
      {
      sprintf(ptr, "  Cc: %s\n", cc);
      while(*ptr) ptr++;
      }
    if (bcc != NULL)
      {
      sprintf(ptr, "  Bcc: %s\n", bcc);
      while(*ptr) ptr++;
      }
    if (subject != NULL)
      {
      sprintf(ptr, "  Subject: %s\n", subject);
      while(*ptr) ptr++;
      }
    if (headers != NULL)
      {
      sprintf(ptr, "  %s\n", headers);
      while(*ptr) ptr++;
      }
    write(log_fd, log_buffer, ptr - log_buffer);
    }
  else DEBUG(2) debug_printf("Failed to open log file %s for %s "
    "transport: %s\n", logfile, tblock->name, strerror(errno));
  }

if (log_fd >= 0) close(log_fd);

DEBUG(2) debug_printf("%s transport succeeded\n", tblock->name);
}

/* End of transport/autoreply.c */
