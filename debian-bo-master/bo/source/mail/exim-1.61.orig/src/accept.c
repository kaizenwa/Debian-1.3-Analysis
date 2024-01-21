/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Code for receiving a message and setting up spool files. */


#include "exim.h"

#define header_blocksize    256
#define header_maxheaders   500


/*************************************************
*                Local static variables          *
*************************************************/

static FILE *smtp_out;
static FILE *data_file;
static int  data_fd;

static int  message_sequence_number = 0;
static char last_message_id[MESSAGE_ID_LENGTH + 1];
static char spool_name[256];



/*************************************************
*              Data read timeout                 *
*************************************************/

/* Handler function for timeouts that occur while reading the data that
comprises a message.

Argument:  the signal number
Returns:   nothing
*/

static void
data_timeout_handler(int sig)
{
if (smtp_input)
  {
  if (!smtp_batched_input)
    {
    DEBUG(9)
      debug_printf("421 %s SMTP incoming data timeout - closing connection.\n",
      primary_hostname);
    fprintf(smtp_out, "421 %s SMTP incoming data timeout - closing connection.\\n",
      primary_hostname);
    fflush(smtp_out);
    }
  else moan_smtp_batch("421 SMTP incoming data timeout - message abandoned");
  log_write(4, LOG_MAIN, "SMTP incoming data timeout (message abandoned)%s%s",
    (sender_fullhost != NULL)? " while connected to " : "",
    (sender_fullhost != NULL)? sender_fullhost : "");
  }
else
  {
  fprintf(stderr, "exim: timed out while reading - message abandoned\n");
  log_write(4, LOG_MAIN, "timed out while reading local message");
  }

if (data_file != NULL) fclose(data_file);
  else if (data_fd >= 0) close(data_fd);
if (spool_name[0] != 0) unlink(spool_name);

exit(EXIT_FAILURE);
}



/*************************************************
*           SIGTERM or SIGINT received           *
*************************************************/

/* Handler for SIGTERM or SIGINT signals that occur while reading the
data that comprises a message.

Argument:  the signal number
Returns:   nothing
*/

static void
data_sigterm_sigint_handler(int sig)
{
if (smtp_input)
  {
  if (!smtp_batched_input)
    {
    DEBUG(9) debug_printf("421 %s Service not available - closing connection.\n",
      primary_hostname);
    fprintf(smtp_out, "421 %s Service not available - closing connection.\r\n",
      primary_hostname);
    }
  else
    moan_smtp_batch("421 Service not available - SIGTERM or SIGINT received");
  log_write(0, LOG_MAIN, "SMTP connection closed after %s%s%s.",
    (sig == SIGTERM)? "SIGTERM" : "SIGINT",
    sender_fullhost? " while connected to " : "",
    sender_fullhost? sender_fullhost : "");
  }
else
  {
  if (filter_test == NULL)
    {
    fprintf(stderr, "\nexim: %s received - message abandoned\n",
      (sig == SIGTERM)? "SIGTERM" : "SIGINT");
    log_write(0, LOG_MAIN, "%s received while reading local message",
      (sig == SIGTERM)? "SIGTERM" : "SIGINT");
    }
  }

if (data_file != NULL) fclose(data_file);
  else if (data_fd >= 0) close(data_fd);
if (spool_name[0] != 0) unlink(spool_name);

exit(EXIT_FAILURE);
}



/*************************************************
*          Add new recipient to list             *
*************************************************/

/* This function builds a list of recipient addresses in argc/argv
format. The argument should point to dynamically obtained store, as
it will be freed when the list is freed.

Argument:  the next address to add to recipients_list
Returns:   nothing
*/

void
accept_add_recipient(char *receiver)
{
if (recipients_count >= recipients_list_max)
  {
  char **oldlist = recipients_list;
  int oldmax = recipients_list_max;
  recipients_list_max = recipients_list_max? 2*recipients_list_max : 50;
  recipients_list = (char **) store_malloc(recipients_list_max * sizeof(char *));
  if (oldlist != NULL)
    {
    memcpy(recipients_list, oldlist, oldmax * sizeof(char *));
    store_free(oldlist);
    }
  }
recipients_list[recipients_count++] = receiver;
}



/*************************************************
*               Free address list                *
*************************************************/

/* This function is called to throw away the list of addresses
that have been built up, in cases of error or explicit resetting.

Arguments:  none
Returns:    nothing
*/

void
accept_free_recipients(void)
{
if (recipients_list != NULL)
  {
  while (--recipients_count >= 0) store_free(recipients_list[recipients_count]);
  store_free(recipients_list);
  }
recipients_list = NULL;
recipients_count = recipients_list_max = 0;
}



/*************************************************
*     Read data portion of a non-SMTP message    *
*************************************************/

/* This function is called to read the remainder of a message (following the
headers) when the input is not from SMTP - we are receiving a local message on
a standard input stream. The message is always terminated by EOF, and is also
terminated by a dot on a line by itself if the flag dot_ends is TRUE. Split the
two cases for maximum efficiency.

Arguments:
  fin       a FILE from which to read the message
  fout      a FILE to which to write the message

Returns:    FALSE if the message is too big; TRUE otherwise, even if there
              is an error while writing - a general check on the output file
              (using ferror()) is done later to pick up any error states.
*/

static BOOL
read_message_data(FILE *fin, FILE *fout)
{
int ch_state;
register int ch;

/* Handle the case when only EOF terminates the message */

if (!dot_ends)
  {
  while ((ch = getc(fin)) != EOF)
    {
    if (fputc(ch, fout) == EOF) break;
    if (message_size_limit > 0 && ++message_size > message_size_limit)
      return FALSE;
    }
  return TRUE;  /* => not too big, not that there's been no error */
  }

/* Handle the case when a dot on its own or EOF terminate */

ch_state = 1;

while ((ch = getc(fin)) != EOF)
  {
  switch (ch_state)
    {
    case 0:                         /* Normal state */
    if (ch == '\n') ch_state = 1;
    break;

    case 1:                         /* After "\n" */
    if (ch == '.')
      {
      ch_state = 2;
      continue;
      }
    if (ch != '\n') ch_state = 0;
    break;

    case 2:                         /* After "\n." */
    if (ch == '\n') return TRUE;
    if (message_size++, fputc('.', fout) == EOF)
      return TRUE;  /* => not too big, not that there's been no error */
    ch_state = 0;
    break;
    }

  if (fputc(ch, fout) == EOF) break;
  if (message_size_limit > 0 && ++message_size > message_size_limit)
    return FALSE;
  }
return TRUE;  /* => not too big, not that there's been no error */
}




/*************************************************
*      Read data portion of an SMTP message      *
*************************************************/

/* This function is called to read the remainder of an SMTP message (after the
headers), or to skip over it when an error has occurred. In this case, the
output file is passed as NULL. If any line begins with a dot, that character is
skipped. A timeout per-line is applied, and the input is only successfully
terminated by CR LF . CR LF unless it is local (non-network) SMTP, in which
case the CRs are optional.

FUDGE: It seems that sites on the net send out messages with just LF
terminators, despite the warnings in the RFCs, and other MTAs handle this. So
we make the CRs optional in all cases. Restore the original spec by
uncommenting two partial conditions below that involve sender_fullhost.

Arguments:
  fin       a FILE from which to read the message
  fout      a FILE to which to write the message; NULL if skipping
  was_EOF   if non_NULL, set TRUE if EOF was encountered
              (implying truncated message); should be FALSE on entry

Returns:    FALSE if the message is too big; TRUE otherwise, even if there
              is an error while writing - a general check on the output file
              (using ferror()) is done later to pick up any error states.
*/

static BOOL
read_message_data_smtp(FILE *fin, FILE *fout, BOOL *was_EOF)
{
int ch_state = 0;
register int ch;

alarm(smtp_receive_timeout);       /* Gets reset after each LF */

while ((ch = getc(fin)) != EOF)
  {
  if (ch == '\n') alarm(smtp_receive_timeout);
  switch (ch_state)
    {
    case 0:                             /* After LF or CRLF */
    if (ch == '.')
      {
      ch_state = 3;
      continue;                         /* Don't ever write . after LF */
      }
    ch_state = 1;

    /* Else fall through to handle as normal char. */

    case 1:                             /* Normal state */
    if (ch == '\n' /*** && sender_fullhost == NULL ***/ )
      {
      ch_state = 0;
      }
    else if (ch == '\r')
      {
      ch_state = 2;
      continue;
      }
    break;

    case 2:                             /* After CR */
    if (ch == '\n') ch_state = 0; else
      {
      if (fout != NULL && (message_size++, fputc('\r', fout) == EOF))
        goto END_SMTP_DATA;             /* A few lines below */
      if (ch != '\r') ch_state = 1; else continue;
      }
    break;

    case 3:                             /* After [CR] LF . */
    if (ch == '\n' /*** && sender_fullhost == NULL ***/ )
      goto END_SMTP_DATA;               /* A few lines below */
    if (ch == '\r')
      {
      ch_state = 4;
      continue;
      }
    ch_state = 1;
    break;

    case 4:                             /* After [CR] LF . CR */
    if (ch == '\n' || (fout != NULL &&
      (message_size++, fputc('\r', fout) == EOF)))
        goto END_SMTP_DATA;             /* A few lines below */
    if (ch == '\r')
      {
      ch_state = 2;
      continue;
      }
    ch_state = 1;
    break;
    }

  /* Add the character to the spool file, unless skipping; then loop for the
  next. */

  if (fout != NULL)
    {
    if (fputc(ch, fout) == EOF) goto END_SMTP_DATA;
    if (message_size_limit > 0 && ++message_size > message_size_limit)
      {
      alarm(0);
      return FALSE;
      }
    }
  }

/* Fall through here if EOF encountered. This indicates some kind of error,
since a correct message is terminated by [CR] LF . [CR] LF. */

if (was_EOF != NULL) *was_EOF = TRUE;

/* Goto is used to get here out of the middle of a switch inside the
above loop. */

END_SMTP_DATA: alarm(0);
return TRUE;  /* => not too big, not that there's been no error */
}


/*************************************************
*                 Accept message                 *
*************************************************/

/* Accept a message on the given input, and put it into a pair of
spool files. Either a non-null list of recipients, or the extract flag
will be true, or both. The flag sender_local is true for locally
generated messages, and smtp_input is true if the message is to be
handled using SMTP conventions about termination and lines starting
with dots.

If a message was successfully read, message_id[0] will be non-zero.

The general actions of this function are:

  . Read the headers of the message (if any) into a chain of store
    blocks.

  . If there is a "sender:" header and the message is locally originated,
    throw it away.

  . If recipients are to be extracted from the message, build the
    recipients list from the headers, removing any that were on the
    original recipients list, and at the same time, remove any bcc
    header that may be present.

  . Get the spool file for the data, sort out its unique name, open
    and lock it (but don't give it the name yet).

  . Generate a "Message-ID" header if the message doesn't have one.

  . Generate a "Received" header.

  . Ensure the recipients list is fully qualified and rewritten if necessary.

  . If there are any rewriting rules, apply them to the sender address
    and also to the headers.

  . If there is no from: header, generate one.

  . Otherwise, if the sender is local, check that from: is correct, and
    if not, generate a Sender header unless message comes from root.

  . Ensure the presence of "to" or "cc" or "bcc".

  . If there is no "date" header, generate one.

  . Copy the rest of the input, or up to a terminating "." if in
    SMTP mode, to the data file. Leave it open, to hold the lock.

  . Write the envelope and the headers to a new file.

  . Set the name for the header file; close it.

  . Set the name for the data file; close it.

Because this function can potentially be called many times in a single
SMTP connection, we must take care to free the store obtained by
store_malloc().

Arguments:
  fin            a FILE to read the message from
  fout           stdout when reading from stdin; the SMTP socket when reading
                   from an SMTP connection.
  extract_recip  TRUE if recipients are to be extracted from the message's
                   headers
Returns:  TRUE   there are more messages to be read (SMTP input)
          FALSE  there are no more messages to be read (non-SMTP input
                 or SMTP connection collapsed, or other failure)
*/

BOOL
accept_msg(FILE *fin, FILE *fout, BOOL extract_recip)
{
int  i;
int  ptr = 0;
int  msg_size;
int  ptrmax = header_blocksize;
int  header_count = 1;
int  process_info_len = (int)strlen(process_info);
BOOL dot_ended = FALSE;
BOOL extracted_bcc = FALSE;
BOOL had_zero = FALSE;
BOOL last_was_cr = FALSE;
BOOL smtp_yield = TRUE;
/***
BOOL smtp_remote = (sender_fullhost != NULL);
***/

flock_t lock_data;

error_block *bad_addresses = NULL;

/* Final message to give to SMTP caller, and flag for whether it is
in dynamic store or not. */

char *smtp_reply = NULL;
BOOL free_smtp_reply = FALSE;

/* Pointers to receive the addresses of headers we specifically check
for. */

header_line *from_header = NULL;
header_line *to_header = NULL;
header_line *cc_header = NULL;
header_line *bcc_header = NULL;
header_line *date_header = NULL;
header_line *subject_header = NULL;
header_line *msgid_header = NULL;
header_line *resentmsgid_header = NULL;

/* Variables for use when building the Received: header. */

char *received;
char *timestamp;
header_line *received_header;

/* Set up the control block for the next header to be read. Then initialize the
chain of headers by setting up a place-holder for Received: header, pointed to
by the global anchor variable. We keep header_last pointing to the end of the
chain to make adding headers simple. */

header_line *h;   /* Working pointer */
header_line *next =
  (header_line *)store_malloc(sizeof(header_line)+header_blocksize);
header_line place_holder;

header_last = &place_holder;

place_holder.next = NULL;
place_holder.type = htype_other;
place_holder.text[0] = 0;

header_list = header_last;
header_list->next = header_list->prev = NULL;
header_list->text[0] = 0;

/* Initialize message id to be null (indicating no message read), and the
header names list to be the normal list. Indicate there is no data file open
yet, and set the count for size checking. */

message_id[0] = 0;
header_names = header_names_normal;
data_file = NULL;
data_fd = -1;
spool_name[0] = 0;
message_size = 0;

/* Release any open files that might have been cached while preparing to
accept the message - e.g. by verifying addresses - because reading a message
might take a fair bit of real time. */

search_tidyup();

/* Remember the time of reception and initialize the warnings count */

received_time = time(NULL);
warning_count = 0;

/* If SMTP input, make the output file generally visible, set the handler for
timeout, and initialize the alarm - it gets reset after each newline. */

if (smtp_input)
  {
  smtp_out = fout;
  signal(SIGALRM, data_timeout_handler);
  alarm(smtp_receive_timeout);
  }

/* If not SMTP input, timeout happen only if configured. */

else if (accept_timeout > 0)
  {
  signal(SIGALRM, data_timeout_handler);
  alarm(accept_timeout);
  }

/* SIGTERM and SIGINT are caught always. */

signal(SIGTERM, data_sigterm_sigint_handler);
signal(SIGINT, data_sigterm_sigint_handler);

/* Header lines in messages are not supposed to be very long, though when
unfolded, to: and cc: headers can take up a lot of store. We must also cope
with the possibility of junk being thrown at us. We therefore read into chunks
of store that are header_blocksize bytes long, which should be perfectly
adequate for most real header lines. Longer lines then require a new block of
store and copying. To cope with total lunacies, impose an upper limit on the
length of a header line. Should there be a limit on the total number of header
lines? The store will fill up if not. Better have one. We must also cope with
the possibility of binary zeros in the data. Hence we cannot use fgets().
Folded header lines are unfolded, leaving the '\n' characters inside them, so
that writing them out reproduces the input. */

/* Loop for each header */

for (;;)
  {
  char *p;
  int ch = getc(fin);

  /* If we hit EOF on a real SMTP connection, it's an error, since incoming
  SMTP must have a correct "." terminator. */

  if (ch == EOF && smtp_input && !smtp_batched_input)
    {
    if (sender_fullhost != NULL)
      log_write(4, LOG_MAIN, "%s unexpected disconnection", sender_fullhost);
    else
      log_write(5, LOG_MAIN, "unexpected EOF from local SMTP connection");
    smtp_reply = "554 Lost incoming connection";
    smtp_yield = FALSE;
    goto TIDYUP;                       /* Skip to end of function */
    }

  /* Handle long lines; what to do if upper limit reached? Best to refuse
  the message as just chopping the line probably causes more trouble. The
  limit is very generous. */

  if (ptr >= ptrmax)
    {
    /* Handle failure due to a humungously long header line. Add what we
    have so far onto the headers list so that it gets reflected in any
    error message, and back up the just-read character. */

    if (ptr >= HEADER_MAXLENGTH - 1)
      {
      ungetc(ch, fin);
      next->text[ptr] = 0;
      next->slen = ptr;
      next->type = htype_other;
      next->next = NULL;
      next->prev = header_last;
      header_last->next = next;
      header_last = next;

      if (smtp_input)
        {
        smtp_reply = "552 Header line is ridiculously overlong";
        (void)read_message_data_smtp(fin, NULL, NULL);  /* Swallow */
        goto TIDYUP;                             /* Skip to end of function */
        }

      else
        {
        if (error_handling == ERRORS_SENDER)
          {
          DEBUG(1) debug_printf("Header line longer than %d characters "
            "received\n", HEADER_MAXLENGTH - 1);
          moan_to_sender(ERRMESS_VLONGHEADER, NULL, header_list->next, fin);
          }
        else
          fprintf(stderr, "exim: header line longer than %d characters "
          "received: message not accepted\n", HEADER_MAXLENGTH - 1);
        exit(EXIT_FAILURE);
        }
      }

    /* Not yet reached the large upper limit: extend the line. */

    else
      {
      header_line *newnext;
      ptrmax += header_blocksize;
      newnext = (header_line *)store_malloc(sizeof(header_line)+ptrmax);
      memcpy(newnext->text, next->text, ptr);
      store_free(next);
      next = newnext;
      if (ptrmax >= HEADER_MAXLENGTH) ptrmax = HEADER_MAXLENGTH - 1;
      }
    }

  /* Cope with receiving a binary zero. There is dispute about whether
  these should be allowed in RFC 822 messages. The middle view is that they
  should not be allowed in headers, at least. Exim takes this attitude at
  the moment. We can't just stomp on them here, because we don't know that
  this line is a header yet. Set a flag to cause scanning later. */

  if (ch == 0) had_zero = TRUE;

  /* Test for termination. Lines in remote SMTP are terminated by CRLF, while
  those from data files use just LF. Treat LF in local SMTP input as a
  terminator too.

  FUDGE: There are sites out there that don't send CRs before their LFs, and
  other MTAs accept this. We are therefore forced into this "liberalisation"
  too. To undo it, uncomment the commented section of the condition below
  and the declaration of smtp_remote above.

  Treat EOF as if it were '\n'. If not a termination, just add the character
  to the header line. */

  if ((ch != '\n' /*** || (smtp_remote && !last_was_cr) ***/ ) && ch != EOF)
    {
    next->text[ptr++] = ch;
    last_was_cr = (ch == '\r');
    }

  /* Input line terminated; remove CR from stored text. */

  else
    {
    if (smtp_input && next->text[ptr-1] == '\r') ptr--;
    next->text[ptr++] = '\n';

    /* A blank line signals the end of the headers; free the unwanted
    space and set next to NULL to indicate this. For SMTP input, or if the
    "dot_ends" flag is set, a line containing only "." also signals the end
    of the headers, since it is the end of the whole input. */

    if (ptr == 1 ||
       ((smtp_input || dot_ends) && ptr == 2 && next->text[0] == '.'))
      {
      store_free(next);
      next = NULL;
      if (ptr == 2) dot_ended = TRUE;  /* Flag so no more gets read */
      break;
      }

    /* There is data in the line; see if the next input character
    is a whitespace character. If it is, we have a continuation of this
    header line. */

    if (ch != EOF)
      {
      int nextch = getc(fin);
      if (nextch == ' ' || nextch == '\t')
        {
        next->text[ptr++] = nextch;
        continue;                      /* Iterate the loop */
        }
      else if (nextch != EOF) ungetc(nextch, fin);   /* Leave for next time */
      else ch = EOF;                   /* Cause main loop to exit at end */
      }

    /* We have got to the real line end. Terminate the string. If it turns
    out to be a real header, internal binary zeros will be squashed later. */

    next->text[ptr] = 0;
    next->slen = ptr;

    /* Add the length into the running total and check against the
    overall limit. We don't expect to fail here, but if someone does send
    an infinite number of headers we want to catch them. Just stop reading
    headers - the code to read the body will then also hit the buffer. */

    message_size += ptr;
    if (message_size_limit > 0 && message_size > message_size_limit)
      break;

    /* A line that is not syntactically correct for a header also marks
    the end of the headers. In this case, we leave next containing the
    first data line. This might actually be several lines because of the
    continuation logic applied above, but that doesn't matter.

    WORMCAN. It turns out that smail, and presumably sendmail, accept leading
    lines of the form

    From ph10 Fri Jan  5 12:35 GMT 1996

    in messages. The "mail" command on Solaris 2 sends such lines. I cannot
    find any documentation of this, but for compatibility it had better be
    accepted. Exim restricts it to the case of non-smtp messages, and
    treats it as an alternative to the -f command line option. Thus it is
    ignored except for trusted users or filter testing. Otherwise it is taken
    as the sender address. */

    if (!smtp_input && header_last == &place_holder &&
        regexec(regexp_From, next->text))
      {
      if (trusted_caller || filter_test != NULL)
        {
        char buffer[256];
        char *errmess, *newsender;
        int start, end, domain;
        int length = regexp_From->endp[1] - regexp_From->startp[1];

        strncpy(buffer, regexp_From->startp[1], length);
        buffer[length] = 0;
        newsender = parse_extract_address(buffer, &errmess, &start, &end,
            &domain, TRUE);
        if (newsender != NULL)
          {
          if (domain == 0)
            newsender = rewrite_address_qualify(newsender, FALSE);
          sender_address = newsender;
          user_name = "";
          sender_local = FALSE;
          if (filter_test != NULL)
            printf("Sender from \"From\" line: %s\n", sender_address);
          }
        }

      store_free(next);
      }

    /* Not a leading "From " line */

    else
      {
      p = next->text;
      while (isgraph(*p) && *p != ':') p++;
      while (isspace(*p)) p++;
      if (*p != ':') break;

      /* We have a valid header line. If there were any binary zeroes in
      the line, stomp on them here. */

      if (had_zero)
        {
        for (p = next->text; p < next->text + ptr; p++) if (*p == 0) *p = '?';
        }

      /* It is perfectly legal to have an empty continuation line
      at the end of a header, but it is confusing to humans
      looking at such messages, since it looks like a blank line.
      Reduce confusion by removing redundant white space at the
      end. We know that there is at least one printing character
      (the ':' tested for above) so there is no danger of running
      off the end. */

      p = next->text + ptr - 2;
      for (;;)
        {
        while (*p == ' ' || *p == '\t') p--;
        if (*p != '\n') break;
        ptr = (p--) - next->text + 1;
        next->text[ptr] = 0;
        next->slen = ptr;
        }

      /* Add the header to the chain */

      next->type = htype_other;
      next->next = NULL;
      next->prev = header_last;
      header_last->next = next;
      header_last = next;
      if (header_count++ >= header_maxheaders) { next = NULL; break; }

      /* If any resent- headers exist, change the pointer to the active header
      names. */

      if (strncmpic(next->text, "resent-", 7) == 0)
        header_names = header_names_resent;
      }

    /* If we have hit EOF, break out of the loop, indicating no pending data
    line. */

    if (ch == EOF) { next = NULL; break; }

    /* Set up for the next header, unless we have received an unreasonble
    number of headers. */

    next = (header_line *)store_malloc(sizeof(header_line)+header_blocksize);
    ptr = 0;
    ptrmax = header_blocksize;
    had_zero = last_was_cr = FALSE;
    if (smtp_input) alarm(smtp_receive_timeout);   /* Reset the timer */
    }
  }

/* Cancel timeout if set. */

if (smtp_input) alarm(0);

/* At this point, we have read all the headers into a data structure
in main store. The first header is still the dummy placeholder for the
Received: header we are going to generate a bit later on. If next != NULL, it
contains the first data line - which terminated the headers before reaching a
blank line (not the normal case). */

DEBUG(9)
  {
  debug_printf(">>Original headers:\n");
  for (h = header_list->next; h != NULL; h = h->next)
    debug_printf("%s", h->text);
  debug_printf("\n");
  if (next != NULL) debug_printf("%s", next->text);
  }

/* End of file on a real SMTP connection is an error. If an incoming SMTP call
is dropped immediately after valid headers, the next thing we will see is EOF.
We must pick test for this specially, as further down the reading of the data
is skipped if already at EOF. */

if (smtp_input && !smtp_batched_input && feof(fin))
  {
  if (sender_fullhost != NULL)
    log_write(4, LOG_MAIN, "%s unexpected disconnection", sender_fullhost);
  else
    log_write(5, LOG_MAIN, "unexpected EOF from local SMTP connection");
  smtp_reply = "554 Lost incoming connection";
  smtp_yield = FALSE;
  goto TIDYUP;                       /* Skip to end of function */
  }

/* If this is a filter test run and no headers were read, output a warning
in case there is a mistake in the test message. */

if (filter_test != NULL && header_list->next == NULL)
  printf("Warning: no message headers read\n");

/* Scan the headers for those that we must test for. If any resent- headers
exist, then we must use the set of resent- headers, and not use the others, as
mandated by RFC 822. However, we keep both kinds of message-id, so that we can
log message-id if resent-message-id does not exist in the presence of other
resent- headers. If there is a "sender:" header (of the appropriate type) and
the message is locally originated, mark it "old" so that it will not be
transmitted with the message.

The Return-path: header is supposed to be added to messages when they leave the
SMTP system. We shouldn't receive messages that already contain Return-path.
However, since exim generates Return-path: on local delivery, resent messages
may well contain it. We therefore provide an option to remove any Return-path:
headers on input. Removal actually means flagging as "old", which prevents the
header being transmitted with the message.

Similar remarks apply to the non-standard Delivery-date: and Envelope-to:
headers.

If we are testing a mail filter file, use the value of the Return-Path header
to set up the return_path variable, which is not otherwise set. */

for (h = header_list->next; h != NULL; h = h->next)
  {
  if (header_checkname(h, "return-path", 11))
    {
    if (return_path_remove) h->type = htype_old;
    if (filter_test != NULL)
      {
      char *start = h->text + 12;
      while (isspace(*start)) start++;
      return_path = string_copyn(start, (int)strlen(start) - 1);
      }
    }
  else if (header_checkname(h, header_names[hn_sender].name,
    header_names[hn_sender].len))
      h->type = sender_local? htype_old : htype_sender;
  else if (header_checkname(h, header_names[hn_from].name,
    header_names[hn_from].len)) { from_header = h; h->type = htype_from; }
  else if (header_checkname(h, header_names[hn_to].name,
    header_names[hn_to].len)) { to_header = h; h->type = htype_to; }
  else if (header_checkname(h, header_names[hn_cc].name,
    header_names[hn_cc].len)) { cc_header = h; h->type = htype_cc; }
  else if (header_checkname(h, header_names[hn_bcc].name,
    header_names[hn_bcc].len)) { bcc_header = h; h->type = htype_bcc; }
  else if (header_checkname(h, header_names[hn_replyto].name,
    header_names[hn_replyto].len)) { bcc_header = h; h->type = htype_replyto; }
  else if (header_checkname(h, header_names[hn_date].name,
    header_names[hn_date].len)) date_header = h;
  else if (header_checkname(h, header_names[hn_subject].name,
    header_names[hn_subject].len)) subject_header = h;
  else if (header_checkname(h, "message-id", 10))
    { msgid_header = h; h->type = htype_id; }
  else if (header_checkname(h, "resent-message-id", 17))
    resentmsgid_header = h;
  else if (header_checkname(h, "received", 8)) h->type = htype_received;
  else if (header_checkname(h, "delivery-date", 13) &&
    delivery_date_remove) h->type = htype_old;
  else if (header_checkname(h, "envelope-to", 11) &&
    envelope_to_remove) h->type = htype_old;
  }

/* Extract recipients from the headers if that is required. In this case,
any recipients already listed are to be REMOVED from the message, so
we need to build a non-recipients tree for that list, because in subsequent
processing this data is held in a tree and that's what the spool_write_header()
function expects. We don't need to worry about freeing the store for the
tree, since this happens only when a single message is being received. Make
sure that non-recipient addresses are fully qualified and rewritten if
necessary. */

if (extract_recip)
  {
  int rcount = 0;
  error_block **bnext = &bad_addresses;

  while (recipients_count-- > 0)
    {
    char *s = rewrite_address(recipients_list[recipients_count], TRUE, TRUE);
    tree_add_nonrecipient(s, FALSE);
    store_free(s);
    }

  store_free(recipients_list);
  recipients_list = NULL;
  recipients_count = recipients_list_max = 0;
  parse_allow_group = TRUE;             /* Allow address group syntax */

  /* Now scan the headers */

  for (h = header_list->next; h != NULL; h = h->next)
    {
    if (h->type == htype_to || h->type == htype_cc || h->type == htype_bcc)
      {
      char *s = strchr(h->text, ':') + 1;
      while (isspace(*s)) s++;

      while (*s != 0)
        {
        char *ss = parse_find_address_end(s, FALSE);
        char *recipient, *errmess;
        int terminator = *ss;
        int start, end, domain;

        /* Check on maximum */

        if (recipients_max > 0 && ++rcount > recipients_max)
          {
          if (error_handling == ERRORS_STDERR)
            fprintf(stderr, "exim: too many recipients\n");
          else
            moan_to_sender(ERRMESS_TOOMANYRECIP, NULL, NULL, stdin);
          exit(EXIT_FAILURE);
          }

        /* Temporarily terminate the string at this point, and extract
        the route address within. */

        *ss = 0;
        recipient = parse_extract_address(s,&errmess,&start,&end,&domain,FALSE);

        /* Keep a list of all the bad addresses so we can send a single
        error message at the end. */

        if (recipient == NULL)
          {
          int len = (int)strlen(s);
          error_block *b = store_malloc(sizeof(error_block));
          while (len > 0 && isspace(s[len-1])) len--;
          b->next = NULL;
          b->text1 = string_printing(string_copyn(s, len), TRUE);
          b->text2 = errmess;
          *bnext = b;
          bnext = &(b->next);
          }
        else accept_add_recipient(recipient);

        /* Put back the terminator */

        *ss = terminator;
        s = ss + (terminator? 1:0);
        while (isspace(*s)) s++;
        }

      /* If this was the bcc: header, mark it "old", which means it
      will be kept on the spool, but not transmitted as part of the
      message. */

      if (h->type == htype_bcc)
        {
        h->type = htype_old;
        bcc_header = NULL;
        extracted_bcc = TRUE;
        }
      }
    }

  parse_allow_group = FALSE;      /* Reset group syntax flags */
  parse_found_group = FALSE;
  }

/* Now build the unique message id. From smail we copy the idea of using the
current time in base-62 format. Smail adds to this the inode of the data file,
but on fast processors this isn't good enough, because a message can be
completely handled within one second, and another can then arrive and get the
same inode and therefore the same message id. Instead we use the pid of this
process plus a sequence number which gets reset when the time changes, because
a single process is capable of receiving more than one message - indeed, an
SMTP connection might stay for quite some time. Uniqueness now relies on the
fact that process numbers operate on a fairly large cycle, so that it is
unbelievably unlikely that a process could receive a message, finish, and
another process with the same pid could be started to receive another message
within the same second.

There doesn't seem to be anything in the RFC which requires a message id to
start with a letter, but Smail was changed to ensure this. The external form of
the message id (as supplied by string expansion) therefore starts with an
additional leading 'E'. The spool file names do not include this leading
letter and it is not used internally.

Note that string_base62() returns its data in a static storage block, so it
must be copied before calling string_base62() again. It always returns exactly
6 characters. We use just 2 of these for the sequence number. This allows a
single process to accept up to 62x62 messages in one second. I think it will be
some time before processors are up to that...

NOTE: If ever the format of message ids is changed, the regular expression for
checking that a string is in this format must be updated in a corresponding
way. It appears in the initializing code in exim.c. The macro MESSAGE_ID_LENGTH
must also be changed to reflect the correct string length. */

strcpy(message_id, string_base62((long int)received_time));
strcat(message_id, "-");
strcat(message_id, string_base62((long int)getpid()));
if (strncmp(message_id, last_message_id, MESSAGE_ID_LENGTH - 3) != 0)
  message_sequence_number = 0;
sprintf(message_id + MESSAGE_ID_LENGTH - 3, "-%2s",
  string_base62((long int)message_sequence_number++) + 4);
strcpy(last_message_id, message_id);

/* Add the current message id onto the current process info string. */

sprintf(process_info + process_info_len, " id=%s", message_id);

/* Now that we have the message-id, if there is no message-id: header, generate
one. This can be user-configured if required, but we had better flatten any
illegal characters therein. */

if ((header_names == header_names_normal && msgid_header == NULL) ||
    (header_names != header_names_normal && resentmsgid_header == NULL))
  {
  BOOL use_default = TRUE;
  if (message_id_text != NULL)
    {
    char *e = expand_string(message_id_text);
    if (e != NULL)
      {
      char *ee;
      for (ee = e; *ee != 0; ee++)
        if (iscntrl(*ee) || strchr(" ()<>@,;:\\\".[]", *ee) != NULL) *ee = '-';
      header_add(htype_id, "%s: <%s.%s@%s>\n", header_names[hn_msgid].name,
        message_id_external, e, primary_hostname);
      store_free(e);
      use_default = FALSE;
      }
    }
  if (use_default)
    header_add(htype_id, "%s: <%s@%s>\n", header_names[hn_msgid].name,
      message_id_external, primary_hostname);
  }

/* Generate our own Received: header. We must not do this earlier because the
message_id is usually included in the expansion string for this header.
Generate text for the received header by expanding the configured string, and
also get the timestamp to put on the end of the text, as required by RFC 822.
Note: the checking for too many Received: headers is handled by the delivery
code. */

timestamp = expand_string("${tod_full}");
received = expand_string(received_header_text);

if (received == NULL)
  log_write(0, LOG_PANIC_DIE, "Expansion of \"%s\" (received_header_text) "
    "failed: %s", string_printing(received_header_text, FALSE),
      expand_string_message);

received_header = (header_line *)store_malloc(sizeof(header_line) +
  strlen(received) + strlen(timestamp) + 3);
sprintf(received_header->text, "%s; %s\n", received, timestamp);
store_free(received);
store_free(timestamp);

/* Replace the place-holder with the real header. We know there must be at
least two headers already in existence - the place-holder for Received, and a
Message-Id header. */

received_header->prev = NULL;
received_header->next = header_list->next;
received_header->type = htype_received;
received_header->slen = (int)strlen(received_header->text);
(header_list->next)->prev = received_header;
header_list = received_header;


/* Ensure the recipients list is fully qualified and rewritten. If we
are to log recipients, first keep a copy of the raw ones. */

if (log_received_recipients)
  {
  raw_recipients = store_malloc(recipients_count * sizeof(char *));
  for (i = 0; i < recipients_count; i++)
    raw_recipients[i] = string_copy(recipients_list[i]);
  }

for (i = 0; i < recipients_count; i++)
  recipients_list[i] = rewrite_address(recipients_list[i], TRUE, TRUE);

/* If there are any rewriting rules, apply them to the sender address. */

if (rewrite_rules != NULL)
  {
  DEBUG(5) debug_printf("Rewriting rules exist\n");
  if (sender_address[0] != 0)
    sender_address = rewrite_address(sender_address, FALSE, TRUE);
  }

/* The headers must all be run through rewrite_header, because it ensures that
addresses are fully qualified, as well as applying any rewriting rules that may
exist. Take care to keep the from_header pointer correct if it gets re-written.
Note: start at second header; no point wasting time on Received. */

for (h = header_list->next; h != NULL; h = h->next)
  {
  header_line *newh = rewrite_header(h, NULL, NULL);

  /* The old header gets its type set to "old", and the new one is
  inserted in the chain immediately following. */

  if (newh != NULL)
    {
    if (h == from_header) from_header = newh;
    h->type = htype_old;
    newh->prev = h;
    newh->next = h->next;
    h->next = newh;
    if (newh->next == NULL) header_last = newh;
      else newh->next->prev = newh;
    h = newh;
    }
  }

search_tidyup();    /* Free any cached resources */


/* At this point, the only one of the saved header variables containing
addresses that is still valid if not NULL is from_header. We haven't bother to
updated any of the others, since the code below merely tests for their being
NULL. Beware, though, if any changes require anything else. */


/* If there is no from: header, generate one. If this is a delivery error from
a non-local source, we can't generate a proper address. */

if (from_header == NULL)
  {
  if (sender_address[0] == 0)
    {
    if (!smtp_input || sender_local)
      header_add(htype_from, "%s: Mail Delivery System <Mailer-Daemon@%s>\n",
        header_names[hn_from].name, qualify_domain_sender);
    else
      header_add(htype_from, "%s: Remote Mail Delivery System <>\n",
        header_names[hn_from].name);
    }
  else
    {
    if (!smtp_input || sender_local)
      header_add(htype_from, "%s: %s%s%s%s\n", header_names[hn_from].name,
        user_name, (user_name[0] == 0)? "" : " <", sender_address,
        (user_name[0] == 0)? "" : ">");
    else
      header_add(htype_from, "%s: %s\n", header_names[hn_from].name,
        sender_address);
    }
  }

/* Otherwise, if the sender is local, check that from: is correct, and
if not, generate a sender: header. Any previously-existing sender: header was
removed above. Note that sender_local is TRUE only if the caller of exim
is not trusted, or if a trusted caller did not supply a -f argument for
non-smtp input. If the from: header contains more than one address, then
the call to parse_extract_address fails, and a sender: header is inserted,
as required. */

else if (sender_local)
  {
  BOOL make_sender = TRUE;
  int start, end, domain;
  char *errmess;
  char *from_address =
    parse_extract_address(strchr(from_header->text, ':') + 1, &errmess,
      &start, &end, &domain, FALSE);

  if (from_address != NULL &&
    (strcmpic(sender_address, from_address) == 0 ||
      (domain == 0 && strcmpic(from_address, user_login) == 0)))
        make_sender = FALSE;

  if (make_sender) header_add(htype_sender, "%s: %s <%s>\n",
    header_names[hn_sender].name, user_name, sender_address);
  }

/* A message is not legal unless it has at least one of "to", "cc", or "bcc".
Note that although the minimal examples in RFC822 show just "to" or "bcc", the
full syntax spec allows "cc" as well. If any resent- header exists, this
applies to the set of resent- headers rather than the normal set.

If we extracted recipients from bcc, then we simply add an empty bcc header,
which is legal according to RFC 822. Do the same for input via SMTP - thus not
disclosing possible bcc recipients. Otherwise (i.e. for a message whose
envelope came in on the command line) generate to: from the envelope address
fields, unless always_bcc is set. Don't bother if there are no recipients (an
error case that is picked up below); such a message isn't going anywhere. */

if (recipients_count > 0 && to_header == NULL && bcc_header == NULL &&
    cc_header == NULL)
  {
  if (extracted_bcc || smtp_input || always_bcc)
    header_add(htype_bcc, "%s:\n", header_names[hn_bcc].name);

  /* This is a bit tedious but is presumably rare. */

  else
    {
    int i;
    int p = 0;
    int count = 0;
    header_line *new;
    char *temp;

    for (i = 0; i < recipients_count; i++)
      count += (int)strlen(recipients_list[i]) + 3;

    temp = store_malloc(count);
    count = (int)strlen(header_names[hn_to].name) + 2;
    for (i = 0; i < recipients_count; i++)
      {
      int j = (int)strlen(recipients_list[i]);
      if (i != 0)
        {
        if (count + j > 72)
          {
          strcpy(temp + p, ",\n ");
          p += 3;
          count = 0;
          }
        else
          {
          strcpy(temp + p, ", ");
          p += 2;
          count += 2;
          }
        }
      strcpy(temp + p, recipients_list[i]);
      p += j;
      count += j;
      }

    header_add(htype_to, "%s: %s\n", header_names[hn_to].name, temp);
    store_free(temp);

    /* Ensure the new header is rewritten if required */

    new = rewrite_header(header_last, NULL, NULL);
    if (new != NULL)
      {
      header_last->next = new;
      header_last->type = htype_old;
      new->next = NULL;
      new->prev = header_last;
      header_last = new;
      }
    }
  }

/* If there is no date header, generate one. */

if (date_header == NULL)
  header_add(htype_other, "%s: %s\n", header_names[hn_date].name,
    tod_stamp(tod_full));

/* Show the complete set of headers if debugging, and the next line
if present. */

DEBUG(9)
  {
  debug_printf(">>Final headers:\n");
  for (h = header_list; h != NULL; h = h->next)
    debug_printf("%c %s", h->type, h->text);
  debug_printf("\n");
  if (next != NULL) debug_printf("%s", next->text);
  }

/* The headers are now complete in store. If we are running in filter
testing mode, that is all this function does. Return FALSE to indicate
no more messages expected. */

if (filter_test != NULL)
  {
  process_info[process_info_len] = 0;
  return FALSE;
  }

/* Open a new spool file for the data portion of the message. We need
to access it both via a file descriptor and a stream. Try to make the
directory if it isn't there. */

sprintf(spool_name, "%s/input/%s-D", spool_directory, message_id);
data_fd = open(spool_name, O_RDWR|O_CREAT|O_EXCL, SPOOL_MODE);
if (data_fd < 0)
  {
  if (errno == ENOENT)
    {
    directory_make(spool_directory, "input", INPUT_DIRECTORY_MODE);
    data_fd = open(spool_name, O_RDWR|O_CREAT|O_EXCL, SPOOL_MODE);
    }
  if (data_fd < 0)
    log_write(0, LOG_MAIN | LOG_PANIC_DIE, "Failed to create spool file",
      spool_name);
  }

/* Make sure the file's group is the Exim gid if exim_uid exists (can't have
exim_uid set without exim_gid), and double-check the mode because the group
setting doesn't always get set automatically. */

if (exim_uid_set)
  {
  fchown(data_fd, exim_uid, exim_gid);
  fchmod(data_fd, SPOOL_MODE);
  }

/* We now have data file open. Build a stream for it and lock it. */

data_file = fdopen(data_fd, "w+");
lock_data.l_type = F_WRLCK;
lock_data.l_whence = lock_data.l_start = lock_data.l_len = 0;
if (fcntl(data_fd, F_SETLK, &lock_data) < 0)
  log_write(0, LOG_MAIN | LOG_PANIC_DIE, "Cannot lock %s", spool_name);

/* We have an open, locked data file. Write the message id to it to make it
self-identifying. Then read the remainder of the input of this message and
write it to the data file. If the variable next != NULL, it contains the first
data line (which was read as a header but then turned out not to have the right
format); write it (remembering that it might contain binary zeros) and then
free it. The result of fwrite() isn't inspected; instead we call ferror()
below. */

fprintf(data_file, "%s-D\n", message_id);
if (next != NULL)
  {
  char *s = next->text;
  int len = next->slen;
  if (smtp_input && next->text[0] == '.')
    {
    s++;
    len--;
    }
  fwrite(s, 1, len, data_file);
  store_free(next);
  }

/* Note that we might already be at end of file, or the logical end of file
(indicated by '.'), or might have encountered an error while writing the
message id or "next" line. */

if (!ferror(data_file) && !feof(fin) && !dot_ended)
  {
  BOOL smtp_EOF = FALSE;
  BOOL size_ok = smtp_input? read_message_data_smtp(fin, data_file, &smtp_EOF) :
    read_message_data(fin, data_file);

  /* Handle premature termination of non-batched SMTP (can't happen for
  non-SMTP input) */

  if (smtp_EOF && !smtp_batched_input)
    {
    unlink(spool_name);                /* Lose data file when closed */
    message_id[0] = 0;                 /* Indicate no message accepted */
    if (sender_fullhost != NULL)
      log_write(4, LOG_MAIN, "%s unexpected disconnection", sender_fullhost);
    else
      log_write(5, LOG_MAIN, "unexpected EOF from local SMTP connection");
    smtp_reply = "554 Lost incoming connection";
    smtp_yield = FALSE;
    goto TIDYUP;                       /* Skip to end of function */
    }

  /* Handle message that is too big */

  if (!size_ok)
    {
    unlink(spool_name);                /* Lose the data file when closed */
    if (smtp_input)
      {
      smtp_reply = "552 Message too large";
      log_write(2, LOG_MAIN|LOG_REJECT, "rejected%s%s%s%s%s: message too large "
        "(max=%d)",
        (sender_fullhost == NULL)? "" : " from ",
        (sender_fullhost == NULL)? "" : sender_fullhost,
        (sender_ident == NULL)? "" : " (",
        (sender_ident == NULL)? "" : sender_ident,
        (sender_ident == NULL)? "" : ")",
        message_size_limit);
      (void) read_message_data_smtp(fin, NULL, NULL);  /* Swallow */
      message_id[0] = 0;               /* Indicate no message accepted */
      goto TIDYUP;                     /* Skip to end of function */
      }
    else
      {
      fseek(data_file, (long int)data_start_offset, SEEK_SET);
      log_write(2, LOG_MAIN|LOG_REJECT, "rejected local message from <%s>: "
        "too large (max=%d)", sender_address, message_size_limit);
      if (error_handling == ERRORS_SENDER)
        {
        DEBUG(1) debug_printf("Data too large while reading input for "
          "message %s (%d read; limit is %d)\n", message_id,
            message_size, message_size_limit);
        moan_to_sender(ERRMESS_TOOBIG, NULL, header_list, data_file);
        }
      else fprintf(stderr, "exim: message too large\n");
      fclose(data_file);
      exit(EXIT_FAILURE);
      }
    }
  }

/* The message body has now been read into the data file. Call fflush() to
empty the buffers in C, and then call fsync() to get the data written out onto
the disc, as fflush() doesn't do this (or at least, it isn't documented as
having to do this). If there was an I/O error on either input or output,
attempt to send an error message, and unlink the spool file. For non-SMTP input
we can then give up. Note that for SMTP input we must swallow the remainder of
the input in cases of output errors, since the far end doesn't expect to see
anything until the terminating dot line is sent. */

if (fflush(data_file) == EOF || ferror(data_file) ||
    fsync(fileno(data_file)) < 0 || ferror(fin))
  {
  unlink(spool_name);                /* Lose the data file */
  if (smtp_input)
    {
    if (ferror(fin)) smtp_reply = "451 Error in reading input data"; else
      {
      smtp_reply = "451 Error in writing spool file";
      (void) read_message_data_smtp(fin, NULL, NULL);  /* Swallow */
      }
    message_id[0] = 0;               /* Indicate no message accepted */
    goto TIDYUP;                     /* Skip to end of function */
    }
  else
    {
    fseek(data_file, (long int)data_start_offset, SEEK_SET);
    if (error_handling == ERRORS_SENDER)
      {
      DEBUG(1) debug_printf("Error while %s file for message %s\n",
        ferror(fin)? "reading input" : "writing spool data",
        message_id);
      moan_to_sender(ferror(fin)? ERRMESS_MESSAGEREAD : ERRMESS_SPOOLWRITE,
        NULL, header_list, data_file);
      }
    else
      fprintf(stderr, "exim: %s\n", ferror(fin)?
        "input read failure" : "spool write failure");
    fclose(data_file);
    exit(EXIT_FAILURE);
    }
  }


/* No I/O errors were encountered while writing the data file. */

DEBUG(9) debug_printf("Data file written for message %s\n", message_id);


/* If this is smtp_input, the sender is not local, and sender verification is
requested, call the main checking function and arrange to reject the message if
it fails. */

if (smtp_input && !sender_local && (sender_verify || sender_try_verify))
  {
  char *errmess;
  int errcode;

  if (!verify_sender(&errcode, &errmess))
    {
    unlink(spool_name);            /* Lose the data file */
    smtp_reply = string_sprintf("%d rejected: %s <%s>",
      errcode, errmess, sender_address);
    free_smtp_reply = TRUE;

    /* Log the incident to the main and reject logs (the latter will log
    the headers), set no message current, and then skip to the end of this
    function. */

    log_write(3, LOG_MAIN|LOG_REJECT, "rejected%s%s: %s <%s>",
      (sender_fullhost == NULL)? "" : " from ",
      (sender_fullhost == NULL)? "" : sender_fullhost,
      errmess,
      sender_address);
    message_id[0] = 0;
    goto TIDYUP;
    }
  }


/* If there were any bad addresses in the headers (detected only if -t was
specified), or if there were no recipients (possibly as a result of bad
addresses), send a message to the sender of this message, or write it to
stderr if the error handling option is set that way. We need to rewind the
data file in order to read it. In the case of no recipients or stderr error
writing, throw the data file away afterwards, and exit. (This can't be SMTP,
which always ensures there's at least one syntactically good recipient
address.) */

if ((bad_addresses != NULL || recipients_count == 0))
  {
  DEBUG(2)
    {
    if (recipients_count == 0) debug_printf("*** No recipients\n");
    if (bad_addresses != NULL)
      {
      error_block *eblock = bad_addresses;
      debug_printf("*** Bad address(es)\n");
      while (eblock != NULL)
        {
        debug_printf("  %s: %s\n", eblock->text1, eblock->text2);
        eblock = eblock->next;
        }
      }
    }

  fseek(data_file, (long int)data_start_offset, SEEK_SET);

  if (error_handling == ERRORS_SENDER)
    moan_to_sender((bad_addresses == NULL)? ERRMESS_NOADDRESS :
      (recipients_list == NULL)? ERRMESS_BADNOADDRESS : ERRMESS_BADADDRESS,
        bad_addresses, header_list, data_file);
  else
    {
    if (bad_addresses == NULL)
      fprintf(stderr, "exim: no recipients in message\n");
    else
      {
      fprintf(stderr, "exim: invalid address%s",
        (bad_addresses->next == NULL)? ":" : "es:\n");
      while (bad_addresses != NULL)
        {
        fprintf(stderr, "  %s: %s\n", bad_addresses->text1,
          bad_addresses->text2);
        bad_addresses = bad_addresses->next;
        }
      }
    }

  if (recipients_count == 0 || error_handling == ERRORS_STDERR)
    {
    unlink(spool_name);
    fclose(data_file);
    exit(EXIT_FAILURE);
    }
  }


/* Data file successfully written. Ignore signals while we are writing the
header file. */

signal(SIGALRM, SIG_IGN);
signal(SIGTERM, SIG_IGN);
signal(SIGINT, SIG_IGN);


/* Keep the data file open until we have written the header file, in order to
hold onto the lock. If writing the header file fails, we have failed to accept
this message. */

if ((msg_size = spool_write_header(message_id)) == 0)
  {
  DEBUG(2) debug_printf("Failed to write header file for %s\n", message_id);
  unlink(spool_name);           /* Lose the data file */
  if (smtp_input)
    {
    smtp_reply = "451 Error in writing spool file";
    message_id[0] = 0;          /* Indicate no message accepted */
    }
  else
    {
    fseek(data_file, (long int)data_start_offset, SEEK_SET);
    if (error_handling == ERRORS_SENDER)
      moan_to_sender(ERRMESS_SPOOLWRITE, NULL, header_list, data_file);
    else
      fprintf(stderr, "exim: spool write failure\n");
    fclose(data_file);
    exit(EXIT_FAILURE);
    }
  }


/* Else log the arrival of a new message while the file is still locked, just
in case the machine is *really* fast, and delivers it first! Include any
message id that is in the message - since the syntax of a message id is
actually an addr-spec, we can use the parse routine to canonicize it. */

else
  {
  char *errmess, *subject;
  struct stat statbuf;
  int start, end, domain;

  char *old_id = (resentmsgid_header != NULL)? resentmsgid_header->text :
    (msgid_header != NULL)? msgid_header->text : NULL;

  /* If an addr-spec contains a quoted string, it can contain any characters
  except " \ and CR and so in particular it can contain NL! Therefore, make
  sure we use a printing-characters only version for the log. */

  if (old_id != NULL)
    {
    old_id = parse_extract_address(strchr(old_id, ':') + 1,
      &errmess, &start, &end, &domain, FALSE);
    if (old_id != NULL) old_id = string_printing(old_id, TRUE);
    }

  /* If subject logging is turned on, create suitable printing-character
  text. */

  if (log_subject && subject_header != NULL)
    {
    int i;
    char *p, *ss;
    char *s = subject_header->text + 7;

    while (*s != ':') s++;
    while (isspace(*(++s)));
    ss = string_printing(s, FALSE);

    strcpy(big_buffer, " T=\"");
    p = big_buffer + (int)strlen(big_buffer);

    /* All headers end with \n; omit it from the displayed text */

    for (i = 0; i < 100 && ss[i+2] != 0; i++)
      {
      if (ss[i] == '\"' || ss[i] == '\\') *p++ = '\\';
      *p++ = ss[i];
      }
    *p++ = '\"';
    *p = 0;

    if (ss != s) store_free(ss);
    subject = big_buffer;
    }
  else subject = "";

  /* Add data size to header size */

  fflush(data_file);
  fstat(data_fd, &statbuf);
  msg_size += statbuf.st_size;

  /* Generate log entry */

  log_write(0, LOG_MAIN | (log_received_recipients? LOG_RECIPIENTS : 0),
    "<= %s%s%s%s%s%s%s%s%s S=%d%s%s%s",
    (sender_address[0] == 0)?    "<>" : sender_address,
    (message_reference != NULL)? " R=" : "",
    (message_reference != NULL)? message_reference : "",
    (sender_fullhost != NULL)?   " H=" : "",
    (sender_fullhost != NULL)?   sender_fullhost : "",
    (sender_ident != NULL)?      " U=" : "",
    (sender_ident != NULL)?      sender_ident : "",
    (received_protocol != NULL)? " P=" : "",
    (received_protocol != NULL)? received_protocol : "",
    msg_size,
    (old_id != NULL)? " id=" : "",
    (old_id != NULL)? old_id : "",
    subject);

  if (old_id != NULL) store_free(old_id);
  }


/* Either a message has been successfully received and written to the two spool
files, or an error in writing the spool has occurred for an SMTP message, or
an SMTP message has been rejected because of a bad sender. (For a non-SMTP
message we will have already given up because there's no point in carrying on!)
In either event, we must now close (and thereby unlock) the data file. In the
successful case, this leaves the message on the spool, ready for delivery. In
the error case, the spool file will be deleted. Then tidy up store, interact
with an SMTP call if necessary, and return.

A fflush() was done earlier in the expectation that any write errors on the
data file will be flushed(!) out thereby. Nevertheless, it is theoretically
possible for fclose() to fail - but what to do? What has happened to the lock
if this happens? */

TIDYUP:
process_info[process_info_len] = 0;              /* Remove message id */
if (data_file != NULL) fclose(data_file);        /* Frees the lock */

/* Free up the space used to store headers. It is important to set
header_last = NULL as this prevents certain rewrites that might happen
during subsequent verifying (of another incoming message) from trying
to add headers when they shouldn't. */

while (header_list != NULL)
  {
  header_line *this = header_list;
  header_list = header_list->next;
  if (this != &place_holder) store_free(this);
  }
header_last = NULL;

/* Reset signal handlers */

signal(SIGALRM, SIG_DFL);
signal(SIGTERM, SIG_DFL);
signal(SIGINT, SIG_DFL);

/* Tell an SMTP caller the state of play, and yield TRUE unless EOF has been
encountered - meaning there may be more incoming messages from this connection.
Otherwise return FALSE for non-SMTP callers (where there is only ever one
message). */

if (smtp_input)
  {
  /* Handle interactive SMTP callers */

  if (!smtp_batched_input)
    {
    if (smtp_reply == NULL)
      {
      DEBUG(9) debug_printf("250 OK id=%s\n", message_id);
      fprintf(fout, "250 OK id=%s\r\n", message_id);
      }
    else
      {
      DEBUG(9) debug_printf("%s\n", smtp_reply);
      fprintf(fout, "%s\r\n", smtp_reply);
      }
    fflush(fout);
    }

  /* For batched SMTP, generate an error message on failure, and do
  nothing on success. */

  else if (smtp_reply != NULL) moan_smtp_batch(smtp_reply);

  /* Release store if requested, and return */

  if (free_smtp_reply) store_free(smtp_reply);
  return smtp_yield;
  }

return FALSE;  /* No more messages */
}

/* End of accept.c */
