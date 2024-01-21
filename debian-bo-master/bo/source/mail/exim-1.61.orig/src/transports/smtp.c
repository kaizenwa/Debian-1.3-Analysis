/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

#include "../exim.h"
#include "smtp.h"

#define PENDING          256
#define PENDING_DEFER   (PENDING + DEFER)
#define PENDING_OK      (PENDING + OK)


/* Options specific to the smtp transport. They must be in alphabetic
order (note that "_" comes before the lower case letters). Some live in the
transport_instance block so as to be publicly visible; these are flagged with
opt_public. */

optionlist smtp_transport_options[] = {
  { "batch_max",            opt_int,
      (void *)(offsetof(smtp_transport_options_block, batch_max)) },
  { "command_timeout",      opt_time,
      (void *)(offsetof(smtp_transport_options_block, command_timeout)) },
  { "connect_timeout",      opt_time,
      (void *)(offsetof(smtp_transport_options_block, connect_timeout)) },
  { "data_timeout",         opt_time,
      (void *)(offsetof(smtp_transport_options_block, data_timeout)) },
  { "delay_after_cutoff", opt_bool,
      (void *)(offsetof(smtp_transport_options_block, delay_after_cutoff)) },
  { "dns_qualify_single",   opt_bool,
      (void *)(offsetof(smtp_transport_options_block, dns_qualify_single)) },
  { "dns_search_parents",   opt_bool,
      (void *)(offsetof(smtp_transport_options_block, dns_search_parents)) },
  { "fallback_hosts",       opt_stringptr,
      (void *)(offsetof(smtp_transport_options_block, fallback_hosts)) },
  { "final_timeout",        opt_time,
      (void *)(offsetof(smtp_transport_options_block, final_timeout)) },
  { "gethostbyname",        opt_bool,
      (void *)(offsetof(smtp_transport_options_block, gethostbyname)) },
  { "hosts",                opt_stringptr,
      (void *)(offsetof(smtp_transport_options_block, hosts)) },
  { "max_rcpt",             opt_int | opt_public,
      (void *)(offsetof(transport_instance, max_addresses)) },
  { "multi_domain",         opt_bool | opt_public,
      (void *)(offsetof(transport_instance, multi_domain)) },
  { "mx_domains",           opt_stringptr,
      (void *)(offsetof(smtp_transport_options_block, mx_domains)) },
  { "non_mx_domains",       opt_stringptr,
      (void *)(offsetof(smtp_transport_options_block, non_mx_domains)) },
  { "serialize_hosts",      opt_stringptr,
      (void *)(offsetof(smtp_transport_options_block, serialize_hosts)) },
  { "serialize_nets",       opt_stringptr,
      (void *)(offsetof(smtp_transport_options_block, serialize_nets)) },
  { "service",              opt_stringptr,
      (void *)(offsetof(smtp_transport_options_block, service)) }
};

/* Size of the options list. An extern variable has to be used so that its
address can appear in the tables drtables.c. */

int smtp_transport_options_count =
  sizeof(smtp_transport_options)/sizeof(optionlist);

/* Default private options block for the smtp transport. */

smtp_transport_options_block smtp_transport_option_defaults = {
  NULL,            /* hosts */
  NULL,            /* fallback_hosts */
  NULL,            /* hostlist */
  NULL,            /* fallback_hostlist */
  "smtp",          /* service */
  NULL,            /* mx_domains */
  NULL,            /* non_mx_domains */
  NULL,            /* re_mx_domains */
  NULL,            /* re_non_mx_domains */
  NULL,            /* serialize_hosts */
  NULL,            /* re_serialize_hosts */
  NULL,            /* serialize_nets */
  NULL,            /* serialize_netlist */
  0,               /* batch_max */
  5*60,            /* command_timeout */
  0,               /* connect_timeout; 0 => system default */
  5*60,            /* data timeout */
  10*60,           /* final timeout */
  FALSE,           /* gethostbyname */
  TRUE,            /* dns_qualify_single */
  TRUE,            /* dns_search_parents */
  TRUE             /* delay_after_cutoff */
};


/* Local statics */

static int   deliver_socket;
static char *smtp_command;



/*************************************************
*             Setup entry point                  *
*************************************************/

/* This function is called when the transport is about to be used,
but before running it in a sub-process. This enables setup work to
be done that will be remembered and used in all subprocesses. We use
it to handle the net serialization lists. The return of all transport
setup functions is char *, but for this one there is nothing returnable.

Argument: pointer to the transport instance block
Returns:  NULL
*/

char *
smtp_transport_setup(transport_instance *tblock)
{
smtp_transport_options_block *ob =
  (smtp_transport_options_block *)(tblock->options_block);
if (ob->serialize_nets != NULL && ob->serialize_netlist == NULL)
  verify_setup_netlist(ob->serialize_nets, &(ob->serialize_netlist));

return NULL;
}



/*************************************************
*          Initialization entry point            *
*************************************************/

/* Called for each instance, after its options have been read, to
enable consistency checks to be done, or anything else that needs
to be set up.

Argument:   pointer to the transport instance block
Returns:    nothing
*/

void
smtp_transport_init(transport_instance *tblock)
{
char *s;
smtp_transport_options_block *ob =
  (smtp_transport_options_block *)(tblock->options_block);

/* Set up the setup entry point, to be called before subprocesses for this
transport. */

tblock->setup = smtp_transport_setup;

/* Complain if any of the timeouts are zero. */

if (ob->command_timeout <= 0 || ob->data_timeout <= 0 ||
    ob->final_timeout <= 0)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
    "command, data, or final timeout value is zero for %s transport",
      tblock->name);

/* If there are any fallback hosts listed, build a chain of host items
for them, but do not do any lookups at this time. */

if (ob->fallback_hosts != NULL)
  {
  char *name;
  host_item **hh = &(ob->fallback_hostlist);
  for (name = string_firstinlist(ob->fallback_hosts, ':');
       name != NULL;
       name = string_nextinlist(':'))
    {
    host_item *h = store_malloc(sizeof(host_item));
    h->next = NULL;
    h->name = string_copy(name);
    h->address = NULL;
    h->mx = -1;
    h->status = hstatus_unknown;
    h->why = hwhy_unknown;
    h->last_try = 0;
    *hh = h;
    hh = &(h->next);
    }
  }

/* If serialze_nets is set, check their syntax */

for (s = string_firstinlist(ob->serialize_nets, ':'); s != NULL;
     s = string_nextinlist(':'))
  {
  if (!regexec(net_regexp, s))
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
      "error in IP net specification %s for %s transport", s, tblock->name);
  }
}



/*************************************************
*              Read SMTP response                *
*************************************************/

/* This function reads an SMTP response with a timeout, and returns the
response in the given buffer. It also analyzes the first digit of the reply
code and returns FALSE if it is not acceptable. The following is an abstract of
the theory of reply codes from RFC 821:

  1yz   Positive Preliminary reply
  2yz   Positive Completion reply
  3yz   Positive Intermediate reply
  4yz   Transient Negative Completion reply
  5yz   Permanent Negative Completion reply

For all responses:
  1yz => error (no SMTP commands expect this reply)
  4yz => fail, but try again
  5yz => fail, but don't try again

For all except DATA (including after final '.'):
  2yz => success
  3yz => error

For DATA:
  2yz => error
  3yz => send data

A zero is added after any data that is read, to make it a valid C string. FALSE
is also returned after a reading error. In this case buffer[0] will be zero,
and the error code will be in errno.

Arguments:
  buffer    where to put the response
  size      the size of the buffer
  datacmd   TRUE if this call follows the DATA command
  dotcmt    TRUE if this call follows the "." command
  ob        points to the transport's options block

Returns:    TRUE if a valid, non-error response was received; else FALSE
*/

static BOOL
read_response(char *buffer, int size, BOOL datacmd, BOOL dotcmd,
  smtp_transport_options_block *ob)
{
int count, rc;
fd_set select_inset;
struct timeval tv;
char *ptr = buffer;
char *readptr = buffer;

/* Ensure errno starts out zero */

errno = 0;

/* Loop for handling SMTP responses that do not all come in one packet
(multiline or otherwise). Each call to recv is timed by means of the
timeout in the select() function. This works on all OS - and is more
efficient that the use of signal() and alarm(). */

for (;;)
  {
  DEBUG(9) debug_printf("SMTP response timeout = %d\n",
    dotcmd? ob->final_timeout : ob->command_timeout);

  /* Loop to cover select() getting interrupted, and the possibility of
  select() returning with a positive result but no ready descriptor. Is
  this in fact possible? */

  for (;;)
    {
    FD_ZERO (&select_inset);
    FD_SET (deliver_socket, &select_inset);
    tv.tv_sec = (dotcmd? ob->final_timeout : ob->command_timeout);
    tv.tv_usec = 0;
    rc = select(deliver_socket + 1, (SELECT_ARG2_TYPE *)&select_inset,
      NULL, NULL, &tv);

    /* If some interrupt arrived, just retry. We presume this to be rare,
    but it can happen (e.g. the SIGUSR1 signal sent by exiwhat causes
    select() to exit). */

    if (rc < 0 && errno == EINTR)
      {
      DEBUG(9) debug_printf("EINTR while selecting for SMTP response\n");
      continue;
      }

    /* Handle a timeout, and treat any other error as a timeout */

    if (rc <= 0)
      {
      errno = ETIMEDOUT;
      buffer[0] = 0;
      return FALSE;
      }

    /* If the socket is ready, initialize empty buffer in case nothing gets
    read, then read the response and break out of this select retry loop. */

    if (FD_ISSET(deliver_socket, &select_inset))
      {
      *readptr = 0;
      count = recv(deliver_socket, readptr, size-1, 0);
      break;
      }
    }

  /* Handle an EOF (i.e. close down of the connection). */

  if (count == 0)
    {
    buffer[0] = 0;
    return FALSE;
    }

  /* Any other error in reading returns FALSE, leaving errno unchanged. */

  else if (count < 0)
    {
    buffer[0] = 0;
    return FALSE;
    }

  /* Adjust size in case we have to read another packet, and adjust the
  count to be the length of the line we are about to inspect. */

  size -= count;
  count += readptr - ptr;

  /* See if the final two characters in the buffer are \r\n. If not, we
  have to read another packet. At least, that is what we should do on a strict
  interpretation of the RFC. In practice, it seems that there are sites sending
  only LF at the ends of responses and other MTAs cope with this. So we have to
  follow the crowd. */

  /*** if (count < 2 || ptr[count-1] != '\n' || ptr[count-2] != '\r') ***/

  if (ptr[count-1] != '\n')
    {
    DEBUG(9)
      {
      int i;
      debug_printf("SMTP input line incomplete in one buffer:\n  ");
      for (i = 0; i < count; i++)
        if (isprint(ptr[i])) debug_printf("%c", ptr[i]);
          else debug_printf("<%d>", ptr[i]);
      debug_printf("\n");
      }
    readptr = ptr + count;
    continue;
    }

  /* Ensure the buffer contains a C string, remove any whitespace at the end of
  it and print it if debugging. */

  while (count > 0 && isspace(ptr[count-1])) count--;
  ptr[count] = 0;
  DEBUG(1) debug_printf("  SMTP<< %s\n", ptr);

  /* Check the format of the response: it must start with three digits; if
  these are followed by a space or end of line, the response is complete. If
  they are followed by '-' this is a multi-line response and we must look for
  another line until the final line is reached. The only use made of multi-line
  responses is to pass them back as error messages. We therefore just
  concatenate them all within the buffer, which should be large enough to
  accept any reasonable number of lines. A multiline response may already
  have been read in one packet - hence the loop here. */

  for(;;)
    {
    if (count < 3 ||
       !isdigit(ptr[0]) || !isdigit(ptr[1]) || !isdigit(ptr[2]) ||
       (ptr[3] != '-' && ptr[3] != ' ' && ptr[3] != 0))
      {
      errno = ERRNO_SMTPFORMAT;    /* format error */
      return FALSE;
      }

    /* For a multi-line response see if the next line is already read, and if
    so, stay in this loop to check it. */

    if (ptr[3] == '-')
      {
      char *p = ptr + 3;
      while (*(++p) != 0)
        {
        if (*p == '\r' && p[1] == '\n')
          {
          p += 2;
          ptr = p;
          break;
          }
        }
      if (*p == 0) break;
      }
    else break;
    }

  /* End of response. If the line we are looking at is the final line,
  we are done. Otherwise more data has to be read. */

  if (ptr[3] != '-') break;

  /* Move the reading pointer upwards in the buffer and insert \n in case this
  is an error message that subsequently gets printed. Set the scanning pointer
  to the reading pointer position. */

  ptr += count;
  *ptr++ = '\n';
  readptr = ptr;

  /* If buffer is too full, something has gone wrong. */

  if (size < 10)
    {
    errno = ERRNO_SMTPFORMAT;
    return FALSE;
    }
  }

/*Return a value that depends on the SMTP return code. */

return (strchr("145", buffer[0]) == NULL) &&
  ((datacmd && buffer[0] == '3') || (!datacmd && buffer[0] == '2'));
}




/*************************************************
*             Write SMTP command                 *
*************************************************/

/* After writing the command, point smtp_command at the buffer so that it
can be reflected in any error message.

Arguments:   a format and optional data values; the format starts with one
             of HELO, MAIL FROM, RCPT TO, DATA, ".", or QUIT.
Returns:     nothing
*/

static void
write_command(char *format, ...)
{
int count;
va_list ap;
va_start(ap, format);
vsprintf(big_buffer, format, ap);
va_end(ap);
count = (int)strlen(big_buffer);
send(deliver_socket, big_buffer, count, 0);
big_buffer[count-2] = 0;     /* remove \r\n for debug and error message */
DEBUG(1) debug_printf("  SMTP>> %s\n", big_buffer);
smtp_command = big_buffer;
}



/*************************************************
*      Tidy SMTP command for error message       *
*************************************************/

/* If smtp_command points to big_buffer, tidy the command therein so that
it looks nicer in an error message.

Arguments:   none
Returns:     smtp_command, tidied if relevant
*/

static char *
tidy_smtp_command(void)
{
if (smtp_command == big_buffer)
  {
  int i;
  int count = (int)strlen(big_buffer);
  for (i = 0; i < 4; i++) big_buffer[i] = toupper(big_buffer[i]);
  if (strncmp(big_buffer, "MAIL", 4) == 0 ||
      strncmp(big_buffer, "RCPT", 4) == 0)
    {
    for (; big_buffer[i] != ':'; i++) big_buffer[i] = toupper(big_buffer[i]);
    memmove(big_buffer+i+2, big_buffer+i+1, count-i);
    big_buffer[i+1] = ' ';
    }
  }
return smtp_command;
}





/*************************************************
*   Set delivery info into all active addresses  *
*************************************************/

/* Only addresses whose status is >= PENDING are relevant. A lesser
status means that an address is not currently being processed.

Arguments:
  addrlist     points to a chain of addresses
  errno_value  to put in each address's errno field
  msg          to put in each address's message field
  rc           to put in each address's transport_return field

Returns:       nothing
*/

static
void set_errno(address_item *addrlist, int errno_value, char *msg, int rc)
{
address_item *addr;
for (addr = addrlist; addr != NULL; addr = addr->next)
  {
  if (addr->transport_return < PENDING) continue;
  addr->basic_errno = errno_value;
  if (msg != NULL) addr->message = msg;
  addr->transport_return = rc;
  }
}



/*************************************************
*          Check an SMTP response                *
*************************************************/

/* This function is given an errno code and the SMTP response buffer
to analyse, together with the host identification for generating messages. It
sets an appropriate message and puts the first digit of the response code into
the yield variable. If no response was actually read, a suitable digit is
chosen.

Arguments:
  host         the current host, to get its name for messages
  errno_value  the errno value
  buffer       the SMTP response buffer
  yield        where to put a one-digit SMTP response code
  message      where to put an errror message

Returns:       TRUE if an SMTP "QUIT" command should be sent, else FALSE
*/

static BOOL check_response(host_item *host, int *errno_value, char *buffer,
  int *yield, char **message)
{
*yield = '4';    /* Default setting is to give a temporary error */

if (*errno_value == ETIMEDOUT)
  {
  *message = string_sprintf("SMTP timeout while connected to %s [%s] "
    "after %s", host->name, host->address, tidy_smtp_command());
  return FALSE;
  }

/* Handle malformed SMTP response */

if (*errno_value == ERRNO_SMTPFORMAT)
  {
  *message = string_sprintf("Malformed SMTP response from %s [%s] after %s",
    host->name, host->address, tidy_smtp_command());
  return FALSE;
  }

/* Handle non-timeout errors. */

if (buffer[0] != 0)
  {
  char *s = string_printing(buffer, FALSE);
  *message = string_sprintf("SMTP error from remote mailer after %s: "
    "host %s [%s]: %s", tidy_smtp_command(), host->name, host->address, s);
  if (s != buffer) store_free(s);
  *yield = buffer[0];
  return TRUE;
  }

/* No data was read. If there is no errno, this must be the EOF (i.e.
connection closed) case, which causes deferral. Otherwise, put the host's
identity in the message, leaving the errno value to be interpreted as well. In
all cases, we have to assume the connection is now dead. */

if (*errno_value == 0)
  {
  *errno_value = ERRNO_SMTPCLOSED;
  *message = string_sprintf("Remote host %s [%s] closed connection after %s",
    host->name, host->address, tidy_smtp_command());
  }
else *message = string_sprintf("host %s [%s]: %s", host->name, host->address,
  strerror(*errno_value));

return FALSE;
}




/*************************************************
*          Check the final SMTP response         *
*************************************************/

/* This function is called only after sending QUIT. There isn't anything that
can be done after errors here, but we log the incident.

Arguments:
  host      points to the current host
  buffer    the buffer read after QUIT

Returns:    nothing
*/

static void
check_final_response(host_item *host, char *buffer)
{
if (errno == ETIMEDOUT)
  log_write(4, LOG_MAIN, "SMTP timeout while connected to %s [%s] "
    "after %s", host->name, host->address, tidy_smtp_command());

/* Handle malformed SMTP response */

else if (errno == ERRNO_SMTPFORMAT)
  log_write(0, LOG_MAIN, "Malformed SMTP response from %s [%s] after %s",
    host->name, host->address, tidy_smtp_command());

/* Handle non-timeout errors. */

else if (buffer[0] != 0)
  log_write(0, LOG_MAIN, "SMTP error from %s [%s] after %s: %s", host->name,
    host->address, tidy_smtp_command(), buffer);

/* No data was read. If there is no errno, this must be the EOF (i.e.
connection closed) case. */

else if (errno == 0)
  log_write(4, LOG_MAIN, "host %s [%s] closed connection after %s", host->name,
    host->address, tidy_smtp_command());
}





/*************************************************
*          Write error message to logs           *
*************************************************/

/* This writes to the main log and to the message log.

Arguments:
  addr     the address item containing error information
  host     the current host

Returns:   nothing
*/

static void
write_logs(address_item *addr, host_item *host)
{
if (addr->message != NULL)
  {
  log_write(0, LOG_MAIN, "%s", addr->message);
  fprintf(message_log, "%s %s\n", tod_stamp(tod_log), addr->message);
  }
else
  {
  log_write(0, LOG_MAIN, "%s [%s]: %s",
    host->name,
    host->address,
    strerror(addr->basic_errno));
  fprintf(message_log, "%s %s [%s]: %s\n",
    tod_stamp(tod_log),
    host->name,
    host->address,
    strerror(addr->basic_errno));
  }
fflush(message_log);
}



/*************************************************
*       Deliver address list to given host       *
*************************************************/

/* If continue_hostname is not null, we get here only when continuing to
deliver down an existing channel. The channel was passed as the standard
input and output.

Otherwise, we have to make a connection to the remote host, and do the
initial protocol exchange.

Arguments:
  addrlist        chain of potential addresses to deliver; only those whose
                  transport_return field is set to PENDING_DEFER are currently
                  being processed; others should be skipped - they have either
                  been delivered to an earlier host or IP address, or been
                  failed by one of them.
  host            host to deliver to
  port            TCP/IP port to use, in network byte order
  transport_name  name of the transport, for checking wait queues
  ob              transport options block, for timeout data
  copy_host       TRUE if host set in addr->transported must be copied, because
                    it is specific to this call of the transport

Returns:          OK    - the connection was made and the delivery attempted;
                          the result for each address is in its data block.
                  DEFER - the connection could not be made, or something failed
                          while setting up the SMTP session, or there was a
                          non-message-specific error, such as a timeout.
*/

static int
smtp_deliver(address_item *addrlist, host_item *host, int port,
  char *transport_name, smtp_transport_options_block *ob, BOOL copy_host)
{
address_item *addr;
struct sockaddr_in s_in;
int yield = OK;
BOOL ok = FALSE;
BOOL send_rset = TRUE;
BOOL send_quit = TRUE;
BOOL setting_up = TRUE;
char new_message_id[MESSAGE_ID_LENGTH + 1];
char buffer[4096];

smtp_command = "initial connection";

if (continue_hostname == NULL)
  {
  int rc, save_errno;

  /* Create a socket, and connect it to the remote host. A failure to connect
  causes a DEFER error. */

  deliver_socket = socket(AF_INET, SOCK_STREAM, 0);
  if (deliver_socket < 0)
    log_write(0, LOG_PANIC_DIE, "socket creation failed: %s", strerror(errno));

  DEBUG(1) debug_printf("Connecting to %s [%s] ... ", host->name, host->address);

  memset(&s_in, 0, sizeof(s_in));
  s_in.sin_family = AF_INET;
  s_in.sin_port = port;
  s_in.sin_addr.s_addr = (S_ADDR_TYPE)inet_addr(host->address);

  /* If no connection timeout is set, just call conenct() without setting
  a timer, thereby allowing the inbuilt timeout to operate. */

  transport_sigalrm_seen = FALSE;

  if (ob->connect_timeout > 0)
    {
    signal(SIGALRM, transport_timeout_handler);
    alarm(ob->connect_timeout);
    }

  rc = connect(deliver_socket, (struct sockaddr *)&s_in, sizeof(s_in));
  save_errno = errno;

  if (ob->connect_timeout > 0)
    {
    alarm(0);
    signal(SIGALRM, SIG_DFL);
    }

  /* A failure whose error code is "Interrupted system call" is in fact
  an externally applied timeout if the signal handler has been run. */

  if (rc < 0)
    {
    if (save_errno == EINTR && transport_sigalrm_seen) save_errno = ETIMEDOUT;
    set_errno(addrlist, save_errno, NULL, DEFER);
    close(deliver_socket);
    DEBUG(1) debug_printf("failed\n");
    return DEFER;
    }

  DEBUG(1) debug_printf("connected\n");

  /* Now we run the SMTP sending protocol. The first thing is to wait for an
  initial OK response. The dreaded "goto" is nevertheless a reasonably clean way
  of programming this kind of logic, where you want to escape on any error. */

  if (!read_response(buffer, sizeof(buffer), FALSE, FALSE, ob))
    goto RESPONSE_FAILED;

  /* Tell the remote who we are...

  EHLO processing could be added here, but since at the moment there is no
  need for obtaining the additional information that EHLO provides, don't
  bother with it, thereby avoiding the hassle of implementing all the
  necessary fudges to cope with non-RFC821-conforming mailers.

  Exim originally sent "Helo" at this point and ran for nearly a year that way.
  Then somebody tried it with a Microsoft mailer... It seems that all other
  mailers use upper case for some reason (the RFC is quite clear about case
  independence) so, for peace of mind, I gave in. */

  write_command("HELO %s\r\n", primary_hostname);
  if (!read_response(buffer, sizeof(buffer), FALSE, FALSE, ob))
    goto RESPONSE_FAILED;
  }

/* For continuing deliveries down the same channel, the socket is just the
standard input or output. */

else deliver_socket = fileno(stdin);

/* The setting up of the SMTP call is now complete. Any subsequent errors are
message-specific. */

setting_up = FALSE;

/* Initiate a message transfer. The user_null_sender flag is set if a local
message was received with "-f <>" on the command line from a non-trusted user.
The sender of the message is still present in the From: or Sender: header
lines; only the envelope is affected. For a trusted user, using "-f <>" causes
the sender to be set up as mailer-daemon@qualify_domain in a From: line if
there isn't one. */

write_command("MAIL FROM:<%s>\r\n", user_null_sender? "" : return_path);
if (!read_response(buffer, sizeof(buffer), FALSE, FALSE, ob))
  goto RESPONSE_FAILED;


/* Pass over all the recipient addresses and note whether any of them are
accepted. Handle both conventional and source-routed addresses. The relevant
addresses to be handled by this host have status PENDING_DEFER. */

for (addr = addrlist; addr != NULL; addr = addr->next)
  {
  if (addr->transport_return != PENDING_DEFER) continue;

  if (addr->local_part[0] == ',' || addr->local_part[0] == ':')
    write_command("RCPT TO:<@%s%s>\r\n", addr->domain, addr->local_part);
  else
    write_command("RCPT TO:<%s@%s>\r\n", addr->local_part, addr->domain);

  /* The remote is permitted to reject some addresses while accepting others.
  However certain errors clearly abort the whole process. Set the value in
  transport_return to PENDING_OK if the address is accepted. If there is a
  subsequent general error, it will get reset accordingly. If not, it will
  get converted to OK at the end. */

  if (read_response(buffer, sizeof(buffer), FALSE, FALSE, ob))
    {
    ok = TRUE;
    addr->transport_return = PENDING_OK;
    }
  else
    {
    char *s;
    if (errno != 0 || buffer[0] == 0) goto RESPONSE_FAILED;
    s = string_printing(buffer, FALSE);
    addr->message =
      string_sprintf("SMTP error from remote mailer after %s: "
        "host %s [%s]: %s", tidy_smtp_command(), host->name, host->address, s);
    if (s != buffer) store_free(s);
    write_logs(addr, host);
    addr->transport_return = (buffer[0] == '5')? FAIL : DEFER;
    }
  }


/* Now prepare to send the text of the message if there were any good
recipients. If there were no good recipients, just set ok TRUE, since we have
handled address-specific errors already. */

if (!ok) ok = TRUE; else
  {
  write_command("DATA\r\n");
  if (!read_response(buffer, sizeof(buffer), TRUE, FALSE, ob))
    goto RESPONSE_FAILED;

  /* OK to send the message itself, with SMTP dot-handling protocol. Set
  the appropriate timeout value to be used for each chunk. The SIGALRM handler
  must be set up here. (Haven't been able to make it work using select()
  for writing yet.) */

  transport_sigalrm_seen = FALSE;
  signal(SIGALRM, transport_timeout_handler);
  transport_chunk_timeout = ob->data_timeout;
  smtp_command = "sending data block";
  ok = transport_write_message(addrlist, deliver_socket,
    topt_use_crlf | topt_smtp_dots, NULL, 0);
  transport_chunk_timeout = 0;   /* for subsequent transports */
  signal(SIGALRM, SIG_DFL);

  if (!ok)
    {
    buffer[0] = 0;              /* There hasn't been a response */
    goto RESPONSE_FAILED;
    }

  /* Termination of the data */

  write_command(".\r\n");
  smtp_command = "end of data";
  ok = read_response(buffer, sizeof(buffer), FALSE, TRUE, ob);

  /* If all went well, mark the recipient addresses as completed,
  record which host/IPaddress they were delivered to, and cut out
  RSET when sending another message down the same channel. Write the
  completed addresses to the journal now so that they are recorded in
  case there is a crash of hardware or software before the spool gets
  updated. Also record the final SMTP confirmation if needed. */

  if (ok)
    {
    int flag = '=';
    int len;
    host_item *thost;

    char *conf = NULL;
    send_rset = FALSE;

    /* Make a copy of the host if it is local to this invocation
    of the transport. */

    if (copy_host)
      {
      thost = store_malloc(sizeof(host_item));
      *thost = *host;
      thost->name = string_copy(host->name);
      thost->address = string_copy(host->address);
      }
    else thost = host;

    /* Set up confirmation if needed */

    if (log_smtp_confirmation)
      {
      char *s = string_printing(buffer, FALSE);
      conf = (s == buffer)? string_copy(s) : s;
      }

    /* Process all transported addresses */

    for (addr = addrlist; addr != NULL; addr = addr->next)
      {
      if (addr->transport_return == PENDING_OK)
        {
        addr->transport_return = OK;
        addr->transported = thost;
        addr->special_action = flag;
        addr->message = conf;
        flag = '-';

        /* Just carry on after write error, as it may prove possible to
        update the spool file later. */

        sprintf(buffer, "N%s\n", addr->orig);
        len = (int)strlen(buffer);
        if (write(journal_fd, buffer, len) != len)
          log_write(0, LOG_MAIN|LOG_PANIC, "failed to write journal for "
            "address %s: %s", addr->orig, strerror(errno));
        }
      }

    /* Ensure the journal file is pushed out to disc. */

    if (fsync(journal_fd) < 0)
      log_write(0, LOG_MAIN|LOG_PANIC, "failed to fsync journal: %s",
        strerror(errno));
    }
  }


/* Handle general (not specific to one address) failures here. The value of ok
is used to skip over this code on the falling through case. A timeout causes a
deferral. Other errors may defer or fail according to the response code, and
may set up a special errno value, e.g. after connection chopped, which is
assumed if errno == 0 and there is no text in the buffer. If control reaches
here during the setting up phase (i.e. before MAIL FROM) then always defer, as
the problem is not related to this specific message. */

if (!ok)
  {
  int code, save_errno;
  char *message = NULL;

  RESPONSE_FAILED:
  save_errno = errno;
  ok = FALSE;        /* For when gone to */
  send_quit = check_response(host, &save_errno, buffer, &code, &message);

  /* If the failure happened while setting up the call, then defer all
  addresses and yield DEFER, indicating that this host shouldn't be
  tried again for a while. This should also be the action if there was
  an I/O error or a timeout, indicated by errno being non-zero. */

  if (setting_up || save_errno != 0)
    {
    yield = DEFER;
    set_errno(addrlist, save_errno, message, yield);
    }

  /* Otherwise we have a message-specific error response from the remote
  host. This is one of
    (a) negative response to "mail from"
    (b) negative response to "data"
    (c) negative response to "."
  It won't be a negative response to "rcpt to", as that is dealt with
  separately above. The action in all cases is to set an appropriate
  error code for all the addresses, but to leave yield set to OK because
  the host itself has not failed. For a temporary error, write to the
  logs for information, even though this might duplicate what is logged
  for the address if there are no more hosts. */

  else
    {
    if (code == '4')
      {
      log_write(0, LOG_MAIN, "%s", message);
      fprintf(message_log, "%s %s\n", tod_stamp(tod_log), message);
      }
    set_errno(addrlist, save_errno, message, (code == '5')? FAIL : DEFER);
    }
  }


/* If all has gone well, send_quit will be set TRUE, implying we can end the
SMTP session tidily. However, it is desirable to send more than one message
down the SMTP connection if there are several waiting, provided we haven't
already sent so many as to hit the configured limit. The function
transport_check_waiting looks for a waiting message and returns its id. Then
transport_pass_socket tries to set up a continued delivery by passing the
socket on to another process. The variable send_rset is FALSE unless a message
has just been successfully transfered. */

if (ok && send_quit)
  {
  if (transport_check_waiting(transport_name, host->name, ob->batch_max,
      new_message_id))
    {
    if (send_rset)
      {
      write_command("RSET\r\n");
      ok = read_response(buffer, sizeof(buffer), FALSE, TRUE, ob);
      }
    if (ok && transport_pass_socket(transport_name, host->name, new_message_id,
      deliver_socket)) send_quit = FALSE;
    }
  }

/* End off tidily with QUIT unless the connection has died or the socket has
been passed to another process. There isn't much we can do if there's an error
response to QUIT. Certainly we should not start failing addresses at this
point. There's now a function that logs things. */

if (send_quit)
  {
  write_command("QUIT\r\n");
  if (!read_response(buffer, sizeof(buffer), FALSE, FALSE, ob))
    check_final_response(host, buffer);
  }

/* Close the socket, and return the appropriate value. */

close(deliver_socket);
return yield;
}




/*************************************************
*              Closedown entry point             *
*************************************************/

/* This function is called when exim is passed an open smtp channel
from another incarnation, but the message which it has been asked
to deliver no longer exists. The channel is on stdin/stdout.

We might do fancy things like looking for another message to send down
the channel, but if the one we sought has gone, it has probably been
delivered by some other process that itself will seek further messages,
so just close down our connection.

Argument:   pointer to the transport instance block
Returns:    nothing
*/

void
smtp_transport_closedown(transport_instance *tblock)
{
char buffer[256];
smtp_transport_options_block *ob =
  (smtp_transport_options_block *)(tblock->options_block);
deliver_socket = fileno(stdin);
write_command("QUIT\r\n");
(void) read_response(buffer, sizeof(buffer), FALSE, FALSE, ob);
close(deliver_socket);
}



/*************************************************
*               Free a host list                 *
*************************************************/

/* Host lists that are build from expanded strings are one-off and
have to be freed at the end of processing. This function is called
only when expanded_hosts is not NULL. The host names pointed to
are all substrings of expanded_hosts.

Arguments:
  host            points to a chain of host items
  expanded_hosts  the string containing an expanded host list

Returns:          nothing
*/

static void
free_hosts(host_item *host, char *expanded_hosts)
{
store_free(expanded_hosts);
while (host != NULL)
  {
  host_item *this = host;
  host = host->next;
  if (this->address != NULL) store_free(this->address);
  store_free(this);
  }
}




/*************************************************
*              Main entry point                  *
*************************************************/

/* See local README for interface details. As this is a remote transport, it is
given a chain of addresses to be delivered in one connection, if possible. */

void smtp_transport_entry(
  transport_instance *tblock,      /* data for this instantiation */
  address_item *addrlist)          /* addresses we are working on */
{
int cutoff_retry;
int port;
int hosts_total = 0;
int hosts_retry = 0;
int hosts_fail  = 0;
int hosts_defer = 0;
int hosts_serial = 0;
address_item *addr;
BOOL expired = TRUE;
char *expanded_hosts = NULL;
smtp_transport_options_block *ob =
  (smtp_transport_options_block *)(tblock->options_block);
host_item *hostlist = addrlist->host_list;
host_item *fallback_anchor = NULL;
host_item *host;

DEBUG(2) debug_printf("%s transport entered\n", tblock->name);


/* See if a host list is defined for the addresses - they must all have the
same one in order to be passed to a single transport. If not, we use the host
list supplied with the transport. It is an error for this not to exist in this
case. */

if (hostlist == NULL)
  {
  if (ob->hosts == NULL)
    log_write(0, LOG_PANIC_DIE, "%s transport called with no hosts set",
      tblock->name);

  DEBUG(9) debug_printf("using the transport's hosts: %s\n", ob->hosts);

  /* If the transport's host list contains no '$' characters, it is fixed and
  therefore a chain of hosts can be built once and for all, and remembered
  for subsequent use by other calls to this transport. If, on the other hand,
  the host list does contain '$', we expand it and arrange to throw away the
  resulting host list at the end of processing. In the fixed case, as the
  hosts string will never be used again, it doesn't matter that we replace
  all the : characters with zeros. */

  if (ob->hostlist == NULL)
    {
    host_item **anchor = &hostlist;
    char *s = ob->hosts;

    if (strchr(s, '$') != NULL)
      {
      expanded_hosts = expand_string(s);
      if (expanded_hosts == NULL)
        log_write(0, LOG_PANIC_DIE, "failed to expand hostlist %s in %s "
          "transport: %s", s, tblock->name, expand_string_message);
      DEBUG(9) debug_printf("Expanded host list %s to %s\n", s, expanded_hosts);
      s = expanded_hosts;
      }

    while (*s != 0)
      {
      char *t = strchr(s, ':');
      host_item *hitem = store_malloc(sizeof(host_item));
      hitem->next = NULL;
      hitem->name = s;
      hitem->address = NULL;
      hitem->mx = -1;
      hitem->status = hstatus_unknown;
      hitem->why = hwhy_unknown;
      hitem->last_try = 0;
      *anchor = hitem;
      anchor = &(hitem->next);
      if (t == NULL) break; else { *t = 0; s = t+1; }
      }
    }

  /* If there was no expansion of hosts, save the host list for
  next time. */

  if (expanded_hosts == NULL) ob->hostlist = hostlist;
  }

/* If there are any fallback hosts, add them onto the end of the
hostlist chain. Their addresses will be looked up only if necessary. Remember
where they were added, so they can be removed again if the other list is
going to be freed. */

if (ob->fallback_hostlist != NULL)
  {
  for (host = hostlist; host->next != NULL; host = host->next);
  host->next = ob->fallback_hostlist;
  fallback_anchor = host;
  }

/* If the queue_smtp flag is set, we don't actually want to do any
deliveries. Instead, all addresses are to be deferred, and the hints
as to which hosts they are waiting for must be set. Can't use the
set_errno function here, as it only sets PENDING return values. */

if (queue_smtp)
  {
  address_item *addr;
  transport_update_waiting(hostlist, tblock->name);
  for (addr = addrlist; addr != NULL; addr = addr->next)
    {
    addr->transport_return = DEFER;
    addr->basic_errno = 0;
    addr->message = "queue_smtp option set";
    }
  DEBUG(2) debug_printf("Leaving %s transport: queue_smtp option set\n",
    tblock->name);
  if (expanded_hosts != NULL)
    {
    if (fallback_anchor != NULL) fallback_anchor->next = NULL;
    free_hosts(hostlist, expanded_hosts);
    }
  return;
  }


/* Sort out the service, i.e. the port number. We want the port number in
network byte order, and that's what getservbyname() produces, so we have
to use htons() if the configuration specified a port by number instead of
by name. */

if (isdigit(*ob->service))
  {
  char *end;
  port = (int)htons((unsigned short)strtol(ob->service, &end, 0));
  if (end != ob->service + (int)strlen(ob->service))
    log_write(0, LOG_PANIC_DIE, "Invalid SMTP service: %s", ob->service);
  }
else
  {
  struct servent *smtp_service = getservbyname(ob->service, "tcp");
  if (smtp_service == NULL)
    log_write(0, LOG_PANIC_DIE, "TCP service \"%s\" not found", ob->service);
  port = smtp_service->s_port;
  }


/* For each host-plus-IP-address on the list:

.  If this is a continued delivery and the host isn't the one with the
   current connection, skip.

.  If the status is unusable (i.e. previously failed or retry checked), skip.

.  If no IP address set, get the address, either by turning the name into
   an address, calling gethostbyname if gethostbyname is on, or by calling
   the DNS. The DNS may yield multiple addresses, in which case insert the
   extra ones into the list.

.  Get the retry data if not previously obtained for this address and set the
   field which remembers the state of this address. Skip if the retry time is
   not reached. If not, remember whether retry data was found. The retry string
   contains both the name and the IP address.

.  Scan the list of addresses and mark those whose status is DEFER as
   PENDING_DEFER. These are the only ones that will be processed in this cycle
   of the hosts loop.

.  Make a delivery attempt - addresses marked PENDING_DEFER will be tried.
   Some addresses may be successfully delivered, others may fail, and yet
   others may get temporary errors and so get marked DEFER.

.  The return from the delivery attempt is OK if a connection was made and a
   valid SMTP dialogue was completed. Otherwise it is DEFER.

.  If OK, add a "remove" retry item for this host/IPaddress, if any.

.  If fail to connect, or other defer state, add a retry item.

.  If there are any addresses whose status is still DEFER, carry on to the
   next host/IPaddress, otherwise return.

If we get to the end of the list, all hosts have deferred at least one address,
or not reached their retry times. If delay_after_cutoff is unset, it requests a
delivery attempt to those hosts whose last try was before the arrival time of
the current message. To cope with this, we have to go round the loop a second
time. After that, set the status and error data for any addresses that haven't
had it set already. */


for (cutoff_retry = 0; expired &&
     cutoff_retry < ((ob->delay_after_cutoff)? 1 : 2);
     cutoff_retry++)
  {
  for (host = hostlist; host != NULL; host = host->next)
    {
    int rc;
    BOOL serialized = FALSE;
    address_item *first_addr = NULL;
    char *retry_key = NULL;

    /* If this is a continued delivery, we are interested only in the host
    which matches the name of the existing open channel. */

    if (continue_hostname != NULL && strcmp(continue_hostname, host->name) != 0)
      continue;

    /* Count hosts being considered - purely for an intelligent comment
    if none are usable. */

    hosts_total++;

    /* If the address hasn't yet been obtained from the host name, look it up
    now, unless the host is already marked as unusable at this time. If the
    "name" is in fact an IP address, just copy it over; otherwise call host_find
    to look it up with or without the DNS. */

    if (host->address == NULL && host->status < hstatus_unusable)
      {
      if (regexec(regexp_ip_address, host->name))
        host->address = host->name;
      else
        {
        char *canonical_name;
        if (ob->gethostbyname) rc = host_find_byname(host, &canonical_name);

        /* Do an MX-only lookup if the name is *not* in non_mx_domains
        and *is* in mx_domains. */

        else
          {
          BOOL mx_only =

            (ob->non_mx_domains == NULL ||
            !match_isinlist(host->name, ob->non_mx_domains,
            &(ob->re_non_mx_domains), TRUE)) &&

            (ob->mx_domains != NULL &&
            match_isinlist(host->name, ob->mx_domains,
            &(ob->re_mx_domains), TRUE));

          rc = host_find_bydns(host, mx_only, FALSE, ob->dns_qualify_single,
            ob->dns_search_parents, &canonical_name);
          }

        /* Failure to find the host at this time (usually DNS temporary failure)
        is really a kind of routing failure rather than a transport failure.
        Therefore we add a retry item of the routing kind, not to stop us trying
        to look this name up here again, but to ensure the address gets timed
        out if the failures go on long enough. A complete failure at this point
        commonly points to a configuration error, but the best action is still
        to carry on for the next host. */

        if (rc == HOST_FIND_AGAIN || rc == HOST_FIND_FAILED)
          {
          retry_add_item(addrlist, "R", FALSE, host->name, FALSE);
          expired = FALSE;
          if (rc == HOST_FIND_AGAIN) hosts_defer++; else hosts_fail++;
          DEBUG(2) debug_printf("rc = %s for %s\n", (rc == HOST_FIND_AGAIN)?
            "HOST_FIND_AGAIN" : "HOST_FIND_FAILED", host->name);
          continue;
          }

        if (rc == HOST_FOUND_LOCAL)
          log_write(0, LOG_PANIC_DIE, "%s transport found host %s to be local",
            tblock->name, host->name);
        }
      }

    /* The first time round the outer loop, check the status of the host by
    inspecting the retry data. The second time round, we are interested only
    in expired hosts that haven't been tried since this message arrived. */

    if (cutoff_retry == 0)
      {
      /* Ensure the status of the address is set by checking retry data if
      necessary. This returns the retry database key if a retry record was
      actually read. */

      retry_key = retry_check_address(host);

      DEBUG(2) debug_printf("%s [%s] status = %s\n", host->name,
        (host->address == NULL)? "" : host->address,
        (host->status == hstatus_usable)? "usable" :
        (host->status == hstatus_unusable)? "unusable" :
        (host->status == hstatus_unusable_expired)? "unusable (expired)" : "?");

      /* Skip this address if not usable at this time, noting if it wasn't
      actually expired, both locally and in the address. */

      switch (host->status)
        {
        case hstatus_unusable:
        expired = FALSE;
        addrlist->retry_skipped = TRUE;
        /* Fall through */

        case hstatus_unusable_expired:
        switch (host->why)
          {
          case hwhy_retry: hosts_retry++; break;
          case hwhy_failed:  hosts_fail++; break;
          case hwhy_deferred: hosts_defer++; break;
          }
        continue;
        }

      /* IP address is usable */

      expired = FALSE;
      }

    /* Second time round the loop: if the address is set but expired, and
    the message is newer than the last try, let it through. */

    else
      {
      if (host->address == NULL ||
          host->status != hstatus_unusable_expired ||
          host->last_try > received_time)
        continue;
      DEBUG(2)
        debug_printf("trying expired host %s [%s]\n",
          host->name, host->address);
      }

    /* If this host is listed as one to which access must be serialized,
    see if another Exim process has a connection to it, and if so, skip
    this host. If not, update the database to record our connection to it
    and remember this for later deletion. */

    serialized = match_isinlist(host->name, ob->serialize_hosts,
      &(ob->re_serialize_hosts), FALSE);

    if (!serialized && ob->serialize_nets != NULL)
      {
      int ipadd;
      int x[4];
      ip_net_item *n;
      sscanf(host->address, "%d.%d.%d.%d", x, x+1, x+2, x+3);
      ipadd = (x[0] << 24) + (x[1] << 16) + (x[2] << 8) + x[3];
      for (n = ob->serialize_netlist; n != NULL; n = n->next)
        if ((ipadd & n->mask) == n->address) { serialized = TRUE; break; }
      }

    /* Serialization required */

    if (serialized && !transport_check_serialized(tblock->name, host->name))
      {
      DEBUG(2) debug_printf("skipping host %s because another Exim process "
        "is connected to it\n", host->name);
      hosts_serial++;
      continue;
      }

    /* OK, we have an IP address that is not waiting for its retry time to
    arrive and is not expired, OR (second time round the loop) we have an
    expired host that hasn't been tried since the message arrived. Have a go
    at delivering the message to it, but previous delivery attempts may have
    left error stuff set up; flush that first, and record whether we got here
    via an MX record or not in the more_errno field of the address. We are
    interested only in addresses that are still marked DEFER - others may
    have got delivered to a previously considered IP address. Set their
    status to PENDING_DEFER to indicate which ones are relevant this time.
    Save the first one for use later. */

    for (addr = addrlist; addr != NULL; addr = addr->next)
      {
      if (addr->transport_return != DEFER) continue;
      if (first_addr == NULL) first_addr = addr;
      addr->transport_return = PENDING_DEFER;
      addr->basic_errno = 0;
      addr->more_errno = (host->mx >= 0)? 'M' : 'A';
      addr->message = NULL;
      }

    DEBUG(2) debug_printf("delivering %s to %s [%s] (%s%s)",
      message_id, host->name, host->address, addrlist->orig,
      (addrlist->next == NULL)? "" : ", ...");

    set_process_info("delivering %s to %s [%s] (%s%s)",
      message_id, host->name, host->address, addrlist->orig,
      (addrlist->next == NULL)? "" : ", ...");

    if (dont_deliver)
      {
      debug_printf("*** delivery by %s transport bypassed by -N option\n",
        tblock->name);
      set_errno(addrlist, 0, NULL, OK);
      for (addr = addrlist; addr != NULL; addr = addr->next)
        {
        addr->special_action = '*';
        addr->message = "delivery bypassed by -N option";
        }
      rc = OK;
      }
    else
      rc = smtp_deliver(addrlist, host, port, tblock->name, ob,
        expanded_hosts != NULL);

    set_process_info("delivering %s (just tried %s [%s] for %s%s)",
      message_id, host->name, host->address, addrlist->orig,
      (addrlist->next == NULL)? "" : ", ...");

    /* Release serialization if set up */

    if (serialized) transport_end_serialized(tblock->name, host->name);

    /* Yield should either be OK or DEFER or FAIL */

    DEBUG(2) debug_printf("Delivery yield = %s\n",
      (rc == OK)? "OK" : (rc == DEFER)? "DEFER" : (rc == FAIL)? "FAIL" : "?");

    /* If the result is DEFER, there was a non-message-specific problem.
    We need to write to the logs saying what happened for this host. If
    all hosts defer there will be a general message written at the end. */

    if (rc == DEFER) write_logs(first_addr, host);

    /* If the result is DEFER, or if a retry record is known to exist, we
    need to add an item to the retry chain for updating the retry database
    at the end of delivery. We only need to add the item to the top address,
    of course. Also, if DEFER, we mark the IP address unusable so as to skip it
    for any other delivery attempts using the same address. (It is copied into
    the unusable tree at the outer level, so even if different address blocks
    contain the same address, it still won't get tried again.) */

    if (rc == DEFER || retry_key != NULL)
      {
      if (retry_key == NULL)
        retry_key = string_sprintf("T:%s:%s", host->name, host->address);
      retry_add_item(first_addr, "T", FALSE, retry_key + 2, rc != DEFER);
      if (rc == DEFER)
        {
        host->status = hstatus_unusable;
        host->why = hwhy_deferred;
        }
      store_free(retry_key);
      }

    /* Any return other than DEFER (should only ever be OK) means that the
    addresses have got their final statuses filled in for this host. If no
    addresses are marked DEFER, we are done with this chain of addresses. */

    if (rc != DEFER)
      {
      BOOL some_deferred = FALSE;
      for (addr = addrlist; addr != NULL; addr = addr->next)
        {
        if (addr->transport_return == DEFER)
          {
          some_deferred = TRUE;
          break;
          }
        }

      /* If none deferred, tidy up and return. */

      if (!some_deferred)
        {
        DEBUG(2) debug_printf("Leaving %s transport\n",tblock->name);
        if (expanded_hosts != NULL)
          {
          if (fallback_anchor != NULL) fallback_anchor->next = NULL;
          free_hosts(hostlist, expanded_hosts);
          }
        return;
        }
      }
    }

  /* This is the end of the loop that repeats iff expired is TRUE and
  ob->delay_after_cutoff is FALSE. The second time round we will
  try those hosts that haven't been tried since the message arrived. */

  DEBUG(2)
    {
    debug_printf("all IP addresses skipped or deferred at least one "
      "address:%s all expired\n", expired? "" : " not");
    if (expired && !ob->delay_after_cutoff && cutoff_retry == 0)
      debug_printf("retrying IP addresses not tried since message arrived\n");
    }
  }


/* Get here if all IP addresses are skipped or defer at least one address. Add
a standard message to each deferred address if there hasn't been an error, that
is, if it hasn't actually been tried this time. The variable "expired" will be
TRUE unless at least one address was not expired. However, if
ob->delay_after_cutoff is FALSE, some of these expired hosts might have been
tried. If so, an error code will be set, and the failing of the message is
handled by the retry code later. */

for (addr = addrlist; addr != NULL; addr = addr->next)
  {
  if (addr->transport_return == DEFER &&
       (addr->basic_errno == ERRNO_UNKNOWNERROR || addr->basic_errno == 0) &&
       addr->message == NULL)
    {
    addr->basic_errno = ERRNO_HRETRY;
    if (expired)
      {
      addr->message = (ob->delay_after_cutoff)?
        "retry time not reached for any host after a long failure period" :
        "all hosts have been failing for a long time and were last tried "
          "after this message arrived";
      addr->transport_return = FAIL;
      }
    else
      {
      if (hosts_retry == hosts_total)
        addr->message = "retry time not reached for any host";
      else if (hosts_fail == hosts_total)
        addr->message = "all host address lookups failed permanently";
      else if (hosts_defer == hosts_total)
        addr->message = "all host address lookups failed temporarily";
      else if (hosts_serial == hosts_total)
        addr->message = "connection limit reached for all hosts";
      else if (hosts_fail+hosts_defer == hosts_total)
        addr->message = "all host address lookups failed";
      else addr->message = "some host address lookups failed and retry time "
        "not reached for other hosts or connection limit reached";
      }
    }
  }


/* Update the database which keeps information about which messages are waiting
for which hosts to become available, free the host list if it was generated
from an expanded string, and that's it. */

transport_update_waiting(hostlist, tblock->name);
if (expanded_hosts != NULL)
  {
  if (fallback_anchor != NULL) fallback_anchor->next = NULL;
  free_hosts(hostlist, expanded_hosts);
  }

DEBUG(2) debug_printf("Leaving %s transport\n", tblock->name);
}

/* End of transport/smtp.c */
