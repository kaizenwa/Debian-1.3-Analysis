/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Functions for handling an incoming SMTP call. */


#include "exim.h"


#define cmd_buffer_size 512      /* Ref. RFC 821 */


/* Structure for SMTP command list */

typedef struct {
  char *name;
  int len;
  int cmd;
} smtp_cmd_list;

/* Codes for identifying commands */

enum { HELO_CMD, EHLO_CMD, MAIL_CMD, RCPT_CMD, DATA_CMD, VRFY_CMD,
  EXPN_CMD, QUIT_CMD, RSET_CMD, NOOP_CMD, DEBUG_CMD, HELP_CMD,
  ETRN_CMD, EOF_CMD, OTHER_CMD };

/* Local variables for remembering whether a calling host is permitted to
use unqualified sender or recipient addresses. */

static BOOL allow_unqualified_sender;
static BOOL allow_unqualified_sender_set;
static BOOL allow_unqualified_recipient;
static BOOL allow_unqualified_recipient_set;



/*************************************************
*                Local static variables          *
*************************************************/

static BOOL host_allow_relay_anywhere;
static BOOL host_allow_relay_anywhere_set;
static BOOL sender_allow_relay_anywhere;
static BOOL sender_allow_relay_anywhere_set;

static BOOL host_refuse_all_rcpts;
static BOOL sender_refuse_all_rcpts;

static char *smtp_data;
static FILE *smtp_in;
static FILE *smtp_out;

static char cmd_buffer[cmd_buffer_size + 1];

static smtp_cmd_list cmd_list[] = {
  { "helo",       sizeof("helo")-1,       HELO_CMD },
  { "ehlo",       sizeof("ehlo")-1,       EHLO_CMD },
  { "mail from:", sizeof("mail from:")-1, MAIL_CMD },
  { "rcpt to:",   sizeof("rcpt to:")-1,   RCPT_CMD },
  { "data",       sizeof("data")-1,       DATA_CMD },
  { "vrfy",       sizeof("vrfy")-1,       VRFY_CMD },
  { "expn",       sizeof("expn")-1,       EXPN_CMD },
  { "quit",       sizeof("quit")-1,       QUIT_CMD },
  { "rset",       sizeof("rset")-1,       RSET_CMD },
  { "noop",       sizeof("noop")-1,       NOOP_CMD },
  { "debug",      sizeof("debug")-1,      DEBUG_CMD },
  { "help",       sizeof("help")-1,       HELP_CMD },
  { "etrn",       sizeof("etrn")-1,       ETRN_CMD} };

static smtp_cmd_list *cmd_list_end =
  cmd_list + sizeof(cmd_list)/sizeof(smtp_cmd_list);



/*************************************************
*          SMTP command read timeout             *
*************************************************/

/* Signal handler for timing out incoming SMTP commands

Argument: signal number (SIGALRM)
Returns:  nothing
*/

static void
command_timeout_handler(int sig)
{
if (!smtp_batched_input)
  {
  DEBUG(3) debug_printf("421 %s: SMTP command timeout - closing connection\n",
    primary_hostname);
  fprintf(smtp_out, "421 %s: SMTP command timeout - closing connection\r\n",
    primary_hostname);
  fflush(smtp_out);
  }
log_write(4, LOG_MAIN, "SMTP command timeout%s%s",
  (sender_fullhost != NULL)? " while connected to " : "",
  (sender_fullhost != NULL)? sender_fullhost : "");
exit(EXIT_FAILURE);
}



/*************************************************
*               SIGTERM received                 *
*************************************************/

/* Signal handler for handling SIGTERM

Argument: signal number (SIGTERM)
Returns:  nothing
*/

static void
command_sigterm_handler(int sig)
{
if (!smtp_batched_input)
  {
  DEBUG(3) debug_printf("421 %s: Service not available - closing connection\n",
    primary_hostname);
  fprintf(smtp_out, "421 %s: Service not available - closing connection\r\n",
    primary_hostname);
  }
log_write(0, LOG_MAIN, "SMTP connection closed after SIGTERM%s%s",
  sender_fullhost? " while connected to " : "",
  sender_fullhost? sender_fullhost : "");
exit(EXIT_FAILURE);
}



/*************************************************
*           Read one command line                *
*************************************************/

/* Strictly, SMTP commands coming over the net are supposed to end with CRLF.
There are sites that don't do this, and in any case internal SMTP probably
should check only for LF. Consequently, we check here for LF only. The line
ends up with [CR]LF removed from its end. If we get an overlong line, treat as
an unknown command. Carry on in the face of interrupts. (There appears to be no
standard way of disabling them.) The command is read into the static
cmd_buffer.

Arguments:  none
Returns:    a code identifying the command (enumerated above)
*/

static int
smtp_read_command(void)
{
int c;
int ptr = 0;
smtp_cmd_list *p;

alarm(smtp_receive_timeout);

while ((c = getc(smtp_in)) != '\n' && c != EOF)
  {
  if (ptr >= cmd_buffer_size) { alarm(0); return OTHER_CMD; }
  cmd_buffer[ptr++] = c;
  }
alarm(0);

/* If hit end of file, return pseudo EOF command. Whether we have a
part-line already read doesn't matter, since this is an error state. */

if (c == EOF) return EOF_CMD;

/* Remove any CR and white space at the end of the line, and terminate the
string. */

while (ptr > 0 && isspace(cmd_buffer[ptr-1])) ptr--;
cmd_buffer[ptr] = 0;

DEBUG(3) debug_printf("%s\n", cmd_buffer);

/* Scan command list and return identity, having set the data pointer
to the start of the actual data characters. */

for (p = cmd_list; p < cmd_list_end; p++)
  {
  if (strncmpic(cmd_buffer, p->name, p->len) == 0)
    {
    smtp_data = cmd_buffer + p->len;
    while (isspace(*smtp_data)) smtp_data++;
    return p->cmd;
    }
  }

return OTHER_CMD;
}





/*************************************************
*  Initialize for incoming batched SMTP message  *
*************************************************/

/* This function is called from smtp_setup_msg() in the case when
smtp_batched_input is true. This happens when -bS is used to pass a whole batch
of messages in one file with SMTP commands between them. All errors must be
reported by sending a message, and only MAIL FROM, RCPT TO, and DATA are
relevant.

Argument: the FILE from which to read incoming SMTP
Returns:  > 0 message successfully started (reached DATA)
          = 0 QUIT read or end of file reached
          < 0 should not occur
*/

static int
smtp_setup_batch_msg(FILE *fin)
{
int done = 0;
int rcount = 0;

if (feof(fin)) return 0;       /* Treat EOF as QUIT */

received_protocol = "bsmtp";
check_relay = FALSE;           /* No relay checking, whatever config says */

/* Deal with SMTP commands. The reading routine sets up a timeout
for each one. If the timeout happens, or we get SIGTERM, exim just
gives up and dies. */

smtp_in  = fin;
signal(SIGALRM, command_timeout_handler);
signal(SIGTERM, command_sigterm_handler);

/* This loop is exited by setting done to a POSITIVE value. The values
are 2 larger than the required yield of the function. */

while (done <= 0)
  {
  char *errmess;
  char *receiver = NULL;
  char *orig_sender = NULL;
  char *orig_receiver = NULL;
  int errcode, start, end, sender_domain, receiver_domain;

  switch(smtp_read_command())
    {
    /* The HELO/EHLO commands are simply ignored, except that they do
    a reset of the state. */

    case HELO_CMD:
    case EHLO_CMD:
    case RSET_CMD:
    accept_free_recipients();
    sender_address = NULL;
    break;


    /* The MAIL FROM command requires an address as an operand. All we
    do here is to parse it for syntactic correctness. The form "<>" is
    a special case which converts into an empty string. The start/end
    pointers in the original are not used further for this address, as
    it is the canonical extracted address which is all that is kept. */

    case MAIL_CMD:
    if (sender_address != NULL)
      {
      moan_smtp_batch("503 Sender already given");
      break;
      }

    if (smtp_data[0] == 0)
      {
      moan_smtp_batch("501 MAIL FROM must have an address operand");
      break;
      }

    /* The TRUE flag allows "<>" as a sender address */

    orig_sender =
      parse_extract_address(smtp_data, &errmess, &start, &end, &sender_domain,
        TRUE);
    if (orig_sender == NULL)
      {
      moan_smtp_batch("501 %s: %s", smtp_data, errmess);
      break;
      }
    sender_address = string_copy(orig_sender);

    /* Qualify unqualified sender addresses. There doesn't seem much point
    in causing trouble here. */

    if (sender_domain == 0 && sender_address[0] != 0 && sender_address[0] != '@')
      {
      sender_address = rewrite_address_qualify(sender_address, FALSE);
      DEBUG(9) debug_printf("unqualified address %s accepted\n",
        orig_sender);
      }

    /* If configured to check sender addresses, do the preliminary check
    now, unless the sender is local (in which case whatever is given here
    is ignored anyway). The check will fail if the message is to be refused at
    this stage. Another check function is called after the message has been
    received, to do more checking when the headers are available. */

    errmess = NULL;
    refuse_all_rcpts = FALSE;

    if (sender_verify || sender_try_verify)
      {
      if (!sender_local && !verify_sender_preliminary(&errcode, &errmess))
        {
        moan_smtp_batch("%d rejected MAIL FROM: %s <%s>\n", errcode, errmess,
          orig_sender);
        log_write(1, LOG_MAIN|LOG_REJECT, "rejected MAIL FROM in SMTP batch: "
          "%s <%s>", errmess, orig_sender);
        store_free(sender_address);
        sender_address = NULL;
        break;
        }
      }

    /* RFC 821 says MAIL FROM resets state at start of message */
    accept_free_recipients();
    rcount = 0;
    break;


    /* The RCPT TO command requires an address as an operand. All we do
    here is to parse it for syntactic correctness. There may be any number
    of RCPT TO commands, specifying multiple senders. We build them all into
    a data structure that is in argc/argv format. The start/end values
    given by parse_extract_address are not used, as we keep only the
    extracted address. */

    case RCPT_CMD:
    if (sender_address == NULL)
      {
      moan_smtp_batch("503 No sender yet given");
      break;
      }

    if (smtp_data[0] == 0)
      {
      moan_smtp_batch("501 RCPT TO must have an address operand");
      break;
      }

    /* If refuse_all_rcpts is set (implying a bad sender which has been
    received at least twice before from the same host), bounce all recipients.
    */

    if (refuse_all_rcpts)
      {
      moan_smtp_batch("550 cannot route to sender address <%s>",
        sender_address);
      break;
      }

    /* Check maximum number allowed */

    if (recipients_max > 0 && ++rcount > recipients_max)
      {
      moan_smtp_batch("421 too many recipients");
      break;
      }

    /* Don't allow "<>" as a recipient address */

    orig_receiver =
      parse_extract_address(smtp_data, &errmess, &start, &end,
        &receiver_domain, FALSE);
    if (orig_receiver == NULL)
      {
      moan_smtp_batch("501 %s: %s", smtp_data, errmess);
      break;
      }
    receiver = string_copy(orig_receiver);

    /* If the receiver address is unqualified, qualify it. There doesn't seem
    much point in causing trouble here. */

    if (receiver_domain == 0 && receiver[0] != '@')
      {
      DEBUG(9) debug_printf("unqualified address %s accepted\n",
        receiver);
      receiver = rewrite_address_qualify(receiver, TRUE);
      }

    /* Add to the list of receivers, and set value to NULL to prevent
    freeing. */

    accept_add_recipient(receiver);
    receiver = NULL;
    break;


    /* The DATA command is legal only if it follows successful MAIL FROM
    and RCPT TO commands. This function is complete when a valid DATA
    command is encountered. */

    case DATA_CMD:
    if (sender_address == NULL)
      {
      moan_smtp_batch("503 MAIL FROM command must precede DATA");
      break;
      }
    if (recipients_count <= 0)
      {
      moan_smtp_batch("503 Valid RCPT TO <recipient> must precede DATA");
      break;
      }
    done = 3;
    break;


    /* The VRFY, EXPN, HELP, ETRN, and DEBUG commands are ignored. */

    case VRFY_CMD:
    case EXPN_CMD:
    case HELP_CMD:
    case DEBUG_CMD:
    case NOOP_CMD:
    case ETRN_CMD:
    break;


    case EOF_CMD:
    case QUIT_CMD:
    accept_free_recipients();
    done = 2;
    break;


    default:
    moan_smtp_batch("500 Command unrecognized: %s", cmd_buffer);
    break;
    }

  /* Free temporary store. */

  if (orig_sender != NULL) store_free(orig_sender);
  if (orig_receiver != NULL) store_free(orig_receiver);
  if (receiver != NULL) store_free(receiver);
  }

/* Reset the signal handlers used in this function, and if no
message is in progress, ensure the store is cleaned up. */

signal(SIGALRM, SIG_DFL);
signal(SIGTERM, SIG_DFL);

if (done < 3) accept_free_recipients();
return done - 2;  /* Convert yield values */
}




/*************************************************
*          Build host+ident message              *
*************************************************/

/* Used when logging rejections below.

Arguments: none
Returns:   pointer to host name and ident value, if present
*/

static char *
host_and_ident(void)
{
if (sender_ident == NULL)
  sprintf(big_buffer, "%s", sender_fullhost);
else
  sprintf(big_buffer, "%s (%s)", sender_fullhost, sender_ident);
return big_buffer;
}





/*************************************************
*       Initialize for SMTP incoming message     *
*************************************************/

/* This function conducts the initial dialogue at the start of an incoming SMTP
message, and builds a list of recipients. However, if the incoming message
is part of a batch (-bS option) a separate function is called since it would
be messy having tests splattered about all over this function. This function
therefore handles the case where interaction is occurring.

The global recipients_list is set to point to a vector of string pointers,
whose number is given by recipients_count. The global variable sender_address
is set to the sender's address. The yield is +1 if a message has been
successfully started, 0 if a QUIT command was encountered or the connection was
refused from the particular host, or -1 if the connection was lost.

If there are host accept/reject configuration settings, this is where
they are checked.

Argument:
  fin     FILE from which to read incoming SMTP
  fout    FILE to write SMTP responses are written (non-batched SMTP)
  first   TRUE for the first message read on this connection; this causes
            various connection-oriented checks to be done that can be skipped
            for subsequent messages

Returns:  > 0 message successfully started (reached DATA)
          = 0 QUIT read or end of file reached or call refused
          < 0 lost connection
*/

int
smtp_setup_msg(FILE *fin, FILE *fout, BOOL first)
{
int done = 0;
int rcount = 0;

/* Initialize */

accept_free_recipients();
sender_address = NULL;

/* Batched SMTP is handled in a different function. */

if (smtp_batched_input) return smtp_setup_batch_msg(fin);


/* The following code is obeyed for the first message in the SMTP connection
only. */

if (first)
  {
  received_protocol = "smtp";        /* Reset later if EHLO is received */
  host_allow_relay_anywhere_set = FALSE;
  host_refuse_all_rcpts = FALSE;

  /* When a message is input locally via the -bs option, sender_host_unknown
  is set. In this case no checking of acceptable hosts is done. Otherwise
  obey the code that deals with IP source routing, if configured to do so. Then
  check for forbidden hosts and reject the call if it is one of them. When -bs
  is used from inetd, this flag is not set, causing the sending host to be
  checked. Batched input, using the -bS option, calls a different function and
  does not come through here.

  (1) If sender_{host,net}_accept is set, reject a connection not from those
  hosts/nets. Otherwise accept a connection from any host that is not in
  sender_{host,net}_reject. However, if the connection is from a host listed in
  sender_{host,net}_reject_recipients, set the flag to reject all recipients,
  that is, to fail all messages (as opposed to rejecting connections). There
  can be ident values associated with any host.

  (2) If smtp_accept_max and smtp_accept_reserve are set, keep some connections
  in reserve for certain hosts and/or networks.

  (3) If check_relay is set, further checks are done on individual recipient
  addresses, at RCPT TO time, in an order which is supposed to minimize the
  work done, which is why nothing is done here - the host doesn't need to
  be checked unless the domain is a restricted one. */

  if (sender_host_unknown) check_relay = FALSE; else
    {
    BOOL reserved_host = FALSE;

    /* Log IP options (source routing info) on the outgoing socket if
    configured to do so, and flatten the options if requested, or refuse the
    call. Sadly, Linux is different to everyone else, so there has to be some
    conditional compilation here. */

    if (log_ip_options || kill_ip_options || refuse_ip_options)
      {
      #ifdef LINUX_IP_OPTIONS
      int optlen = sizeof(struct options) + MAX_IPOPTLEN;
      struct options *ipopt = store_malloc(optlen);
      #else
      struct ipoption ipoptblock;
      struct ipoption *ipopt = &ipoptblock;
      int optlen = sizeof(ipoptblock);
      #endif

      /* Occasional genuine failures of getsockopt() have been seen - for
      example, "reset by peer". Therefore, just log and give up on this
      call. */

      if (getsockopt(fileno(fout), IPPROTO_IP, IP_OPTIONS, (char *)(ipopt),
            &optlen) < 0)
        {
        log_write(0, LOG_MAIN, "getsockopt() failed from %s: %s\n",
          host_and_ident(), strerror(errno));
        DEBUG(3) debug_printf("451 SMTP service not available\n");
        fprintf(fout, "451 SMTP service not available\r\n");
        return 0;
        }

      else if (optlen > 0)
        {
        if (log_ip_options)
          {
          char *p = big_buffer;
          unsigned char *opt, *adptr;
          int optcount;
          struct in_addr addr;

          #ifdef LINUX_IP_OPTIONS
          unsigned char *optstart = (unsigned char *)(ipopt->__data);
          #else
          unsigned char *optstart = (unsigned char *)(ipopt->ipopt_list);
          #endif

          strcpy(p, "IP options on incoming call:");
          p += (int)strlen(p);

          for (opt = optstart; opt != NULL &&
               opt < (unsigned char *)(ipopt) + optlen;)
	    {
	    switch (*opt)
	      {
	      case IPOPT_EOL:
	      opt = NULL;
	      break;

	      case IPOPT_NOP:
	      opt++;
	      break;

	      case IPOPT_SSRR:
	      case IPOPT_LSRR:
              sprintf(p, " %s [@%s", (*opt == IPOPT_SSRR)? "SSRR" : "LSRR",
              #ifdef LINUX_IP_OPTIONS
                inet_ntoa(*((struct in_addr *)(&(ipopt->faddr)))));
              #else
                inet_ntoa(ipopt->ipopt_dst));
              #endif
              p += (int)strlen(p);
              optcount = (opt[1] - 3) / sizeof(struct in_addr);
              adptr = opt + 3;
              while (optcount-- > 0)
                {
                memcpy(&addr, adptr, sizeof(addr));
                sprintf(p, "%s%s", (optcount == 0)? ":" : "@",
                  inet_ntoa(addr));
                p += (int)strlen(p);
                adptr += sizeof(struct in_addr);
                }
              *p++ = ']';
	      opt += opt[1];
	      break;

	      default:
                {
                int i;
                strcat(p, "[");
                p += 2;
                for (i = 0; i < opt[1]; i++)
                  {
                  sprintf(p, "%2.2x ", opt[i]);
                  p += 3;
                  }
                *p++ = ']';
                }
	      opt += opt[1];
	      break;
	      }
	    }

          *p = 0;
          log_write(0, LOG_MAIN, "%s", big_buffer);
          }

        /* Refuse any call with IP options if configured to do so. This is
        what tcpwrappers 7.5 does. */

        if (refuse_ip_options)
          {
          log_write(0, LOG_MAIN|LOG_REJECT,
            "connection from %s refused (IP options)",
            host_and_ident());
          DEBUG(3) debug_printf("554 SMTP service not available\n");
          fprintf(fout, "554 SMTP service not available\r\n");
          return 0;
          }

        /* Kill any IP options if configured to do so. This is a defence
        against source routing, as practiced by earlier versions of
        tcpwrappers. */

        if (kill_ip_options)
          {
          if (setsockopt(fileno(fout), IPPROTO_IP, IP_OPTIONS, (char *)0, 0) != 0)
            log_write(0, LOG_MAIN|LOG_PANIC_DIE, "setsockopt() failed: %s",
              strerror(errno));
          log_write(0, LOG_MAIN, "IP options removed");
          }
        }
      }

    /* Reject connections from any host not in an accept list. */

    if (sender_host_accept != NULL || sender_net_accept != NULL)
      {
      if (!verify_check_host(sender_host_accept, &sender_host_accept_hosts) &&
          !verify_check_net(sender_net_accept, &sender_net_accept_nets))
        {
        log_write(1, LOG_MAIN|LOG_REJECT, "connection from %s refused (accept)",
          host_and_ident());
        DEBUG(3) debug_printf("554 SMTP service not available\n");
        fprintf(fout, "554 SMTP service not available\r\n");
        return 0;
        }
      }

    /* Reject connections from any host in a reject list. */

    if (sender_host_reject != NULL || sender_net_reject != NULL)
      {
      if (verify_check_host(sender_host_reject, &sender_host_reject_hosts) ||
          verify_check_net(sender_net_reject, &sender_net_reject_nets))
        {
        log_write(1, LOG_MAIN|LOG_REJECT, "connection from %s refused (reject)",
          host_and_ident());
        DEBUG(3) debug_printf("554 SMTP service not available\n");
        fprintf(fout, "554 SMTP service not available\r\n");
        return 0;
        }
      }

    /* Reject recipients from any host in a reject_recipients list. */

    if (sender_host_reject_recipients != NULL ||
        sender_net_reject_recipients != NULL)
      {
      if (verify_check_host(sender_host_reject_recipients,
            &sender_host_reject_recipients_hosts) ||
          verify_check_net(sender_net_reject_recipients,
            &sender_net_reject_recipients_nets))
        {
        log_write(1, LOG_MAIN|LOG_REJECT, "recipients from %s refused",
          host_and_ident());
        host_refuse_all_rcpts = TRUE;
        }
      }

    /* Check for reserved slots. Note that the count value doesn't include
    this process, as it gets upped in the parent process. */

    if (smtp_accept_max > 0 &&
        smtp_accept_count + 1 > smtp_accept_max - smtp_accept_reserve)
      {
      if (!verify_check_host(smtp_reserve_hosts, &smtp_reserve_hostlist) &&
          !verify_check_net(smtp_reserve_nets, &smtp_reserve_netlist))
        {
        log_write(1, LOG_MAIN, "Connection from %s temporarily refused: not in "
          "reserve list: connected=%d max=%d reserve=%d", host_and_ident(),
          smtp_accept_count, smtp_accept_max, smtp_accept_reserve);
        DEBUG(3) debug_printf("421 %s: Too many concurrent SMTP connections; "
          "please try again later\n", primary_hostname);
        fprintf(fout, "421 %s: Too many concurrent SMTP connections; "
          "please try again later\r\n", primary_hostname);
        return 0;
        }
      reserved_host = TRUE;
      }

    /* If a load level above which only messages from reserved hosts are
    accepted is set, check the load. For incoming calls via the daemon, the
    check is done in the superior process if there are no reserved hosts, to
    save a fork. In all cases, the load average will already be available
    in a global variable at this point. */

    if (smtp_load_reserve >= 0 &&
         load_average > smtp_load_reserve &&
         !reserved_host &&
         !verify_check_host(smtp_reserve_hosts, &smtp_reserve_hostlist) &&
         !verify_check_net(smtp_reserve_nets, &smtp_reserve_netlist))
      {
      log_write(1, LOG_MAIN, "Connection from %s temporarily refused: not in "
        "reserve list and load average = %.2f", host_and_ident(),
        (double)load_average/1000.0);
      DEBUG(3) debug_printf("421 %s: Too much load; "
        "please try again later\n", primary_hostname);
      fprintf(fout, "421 %s: Too much load; "
        "please try again later\r\n", primary_hostname);
      return 0;
      }
    }
  }

/* Output the initial message for an SMTP connection. It may contain
newlines, which then cause a multi-line response to be given. Also, reset
the unqualified permission flags. */

if (first)
  {
  char *s = expand_string(smtp_banner);
  char *p = s;

  if (s == NULL)
    log_write(0, LOG_PANIC_DIE, "Expansion of \"%s\" (smtp_banner) failed",
      smtp_banner);

  while (*p != 0)
    {
    int c;
    DEBUG(3) debug_printf("220%c", (strchr(p, '\n') == NULL)? ' ' : '-');
    fprintf(fout, "220%c", (strchr(p, '\n') == NULL)? ' ' : '-');
    while ((c = *p) != 0)
      {
      p++; /* NB can't include in while because used in previous while */
      if (c == '\n') break;
      DEBUG(3) debug_printf("%c", c);
      fputc(c, fout);
      }
    DEBUG(3) debug_printf("\n");
    putc('\r', fout);
    putc('\n', fout);
    }

  store_free(s);
  fflush(fout);

  allow_unqualified_sender_set = allow_unqualified_recipient_set = FALSE;
  }

/* Now deal with SMTP commands. The reading routine sets up a timeout
for each one. If the timeout happens, or we get SIGTERM, exim just
gives up and dies. */

smtp_in  = fin;
smtp_out = fout;

signal(SIGALRM, command_timeout_handler);
signal(SIGTERM, command_sigterm_handler);

/* This loop is exited by setting done to a POSITIVE value. The values
are 2 larger than the required yield of the function. */

while (done <= 0)
  {
  char *errmess;
  char *receiver = NULL;
  char *orig_sender = NULL;
  char *orig_receiver = NULL;
  char *hello = NULL;
  void *oldsignal;
  int pid;
  int multiline = ' ';
  int errcode, start, end, sender_domain, receiver_domain;

  switch(smtp_read_command())
    {
    /* The HELO/EHLO commands are permitted to appear in the middle of
    a session as well as at the beginning. They have the effect of a
    reset in addition to their other functions. Their absence at the
    start cannot be taken to be an error. */

    case HELO_CMD:
    hello = "HELO";
    received_protocol = "smtp";      /* could be resetting in principle */
    multiline = ' ';
    /* fall through with hello != NULL */

    case EHLO_CMD:
    if (hello == NULL)
      {
      hello = "EHLO";
      received_protocol = "esmtp";
      multiline = '-';
      }

    /* Ensure an argument is supplied */

    if (smtp_data[0] == 0)
      {
      DEBUG(3) debug_printf("501 %s must have a domain name operand\n", hello);
      fprintf(fout, "501 %s must have a domain name operand\r\n", hello);
      break;
      }

    /* By default, no verification of the remote host name is given, to save
    doing potentially slow DNS lookups for little gain. However, a config-
    uration option can specify networks for which this check should be
    done. It is only a soft check; the RFC mandates that. */

    if (verify_check_net(helo_verify_nets, &helo_verify_netlist))
      {
      char *true_host = host_find_byaddr(sender_host_address);
      if (true_host == NULL)
        {
        log_write(0, LOG_MAIN, "Cannot verify %s for host %s", hello,
          sender_host_address);
        }
      else
        {
        if (strcmpic(true_host, smtp_data) != 0)
          {
          log_write(6, LOG_MAIN, "%s argument %s for %s corrected to %s",
            hello, smtp_data, sender_host_address, true_host);
          strcpy(smtp_data, true_host);
          }
        store_free(true_host);
        }
      }

    /* If sender_host_unknown is true, we have got here via the -bs interface,
    not called from inetd. In this case, HELO/EHLO should *not* set the host
    name. Otherwise, we are running an IP connection and the host address will
    be set. */

    if (!sender_host_unknown)
      {
      if (sender_host_name != NULL) store_free(sender_host_name);
      sender_host_name = store_malloc((int)strlen(smtp_data) + 1);
      strcpy(sender_host_name, smtp_data);

      if (sender_fullhost != NULL) store_free(sender_fullhost);
      sender_fullhost = string_sprintf("%s [%s]", sender_host_name,
        sender_host_address);
      }

    /* Generate an OK reply, including the ident if present, and also
    the IP address if present. Reflecting back the ident is intended
    as a deterrent to mail forgers. */

    fprintf(fout, "250%c%s: Hello %s%s%s", multiline, primary_hostname,
      (sender_ident == NULL)?  "" : sender_ident,
      (sender_ident == NULL)?  "" : " at ",
      smtp_data);

    DEBUG(3) debug_printf("250%c%s: Hello %s%s%s", multiline, primary_hostname,
      (sender_ident == NULL)?  "" : sender_ident,
      (sender_ident == NULL)?  "" : " at ",
      smtp_data);

    if (sender_host_address != NULL)
      {
      fprintf(fout, " [%s]", sender_host_address);
      DEBUG(3) debug_printf(" [%s]", sender_host_address);
      }

    fprintf(fout, "\r\n");
    DEBUG(3) debug_printf("\n");

    /* If we received EHLO, we have started a multiline response. Finish it
    off with the functions supported. Currently not much! */

    if (multiline == '-')
      {
      fprintf(fout, "250-Supported functions are:\r\n");
      DEBUG(3) debug_printf("250-Supported functions are:\n");

      /* I'm not entirely happy with this, as an MTA is supposed to check
      that it has enough room to accept a message of maximum size before
      it sends this. However, there seems little point in not sending it. */

      if (message_size_limit > 0)
        {
        fprintf(fout, "250-SIZE %d\r\n", message_size_limit);
        DEBUG(3) debug_printf("250-SIZE %d\n", message_size_limit);
        }

      /* Exim does not do protocol conversion or data conversion. It is 8-bit
      clean; if it has an 8-bit character in its hand, it just sends it. It
      cannot therefore specify 8BITMIME and remain consistent with the RFCs.
      However, some users want this option simply in order to stop MUAs
      mangling messages that contain top-bit-set characters. It is therefore
      provided as an option. */

      if (accept_8bitmime)
        {
        fprintf(fout, "250-8BITMIME\r\n");
        DEBUG(3) debug_printf("250-8BITMIME\n");
        }

      /* Advertise ETRN if any hosts are permitted to issue it; a check is
      made when any host actually does. */

      if (smtp_etrn_hosts != NULL || smtp_etrn_nets != NULL)
        {
        fprintf(fout, "250-ETRN\r\n");
        DEBUG(3) debug_printf("250-ETRN\n");
        }

      /* Finish off the multiline reply with one that is always available. */

      fprintf(fout, "250 HELP\r\n");
      DEBUG(3) debug_printf("250 HELP\n");
      }

    fflush(fout);

    /* Ensure we are in the reset state */

    accept_free_recipients();
    sender_address = NULL;
    break;


    /* The MAIL FROM command requires an address as an operand. All we
    do here is to parse it for syntactic correctness. The form "<>" is
    a special case which converts into an empty string. The start/end
    pointers in the original are not used further for this address, as
    it is the canonical extracted address which is all that is kept. */

    case MAIL_CMD:
    if (sender_address != NULL)
      {
      DEBUG(3) debug_printf("503 Sender already given\n");
      fprintf(fout, "503 Sender already given\r\n");
      break;
      }

    if (smtp_data[0] == 0)
      {
      DEBUG(3) debug_printf("501 MAIL FROM must have an address operand\n");
      fprintf(fout, "501 MAIL FROM must have an address operand\r\n");
      break;
      }

    /* If there hasn't been a HELO or EHLO command, and this is not a -bs
    session, sender_host_name will be unset. If the current host matches
    helo_verify_nets, set the name by doing a reverse lookup. */

    if (sender_host_name == NULL && !sender_host_unknown &&
         verify_check_net(helo_verify_nets, &helo_verify_netlist))
      {
      sender_host_name = host_find_byaddr(sender_host_address);
      if (sender_fullhost != NULL) store_free(sender_fullhost);
      sender_fullhost = string_sprintf("%s [%s]", sender_host_name,
        sender_host_address);
      }

    /* Reset the flag that refuses all recipients for bad senders when
    the reject style is configured that way. */

    sender_refuse_all_rcpts = FALSE;

    /* Loop for handling ESMTP additions to the MAIL FROM command. At
    present this is done by rather ad-hoc coding, since only two are
    supported. */

    if (strcmp(received_protocol, "esmtp") == 0) for (;;)
      {
      /* If this session was initiated with EHLO and message_size_limit is
      non-zero, Exim will have indicated that it supports the SIZE option.
      The remote is then permitted to add "SIZE=n" on the end of the MAIL
      FROM command. Just parse this out by ad hoc code for the moment. We
      know the command line starts "MAIL FROM", so the backwards searches
      will always terminate without the need for explicit stops. */

      if (message_size_limit > 0)
        {
        char *p = smtp_data + (int)strlen(smtp_data);
        while (isspace(p[-1])) *(--p) = 0;
        while (isdigit(p[-1])) p--;
        if (*p != 0 && strncmpic(p-5, "SIZE=", 5) == 0)
          {
          int size = atoi(p);
          p[-5] = 0;
          if (size > message_size_limit)
            {
            DEBUG(3) debug_printf("552 Message size exceeds maximum permitted\n");
            fprintf(fout, "552 Message size exceeds maximum permitted\r\n");
            break;
            }
          }
        }

      /* If this session was initiated with EHLO and accept_8bitmime is set,
      Exim will have indicated that it supports the BODY=8BITMIME option. In
      fact, it does not support this according to the RFCs, in that it does not
      take any special action for forwarding messages containing 8-bit
      characters. That is why accept_8bitmime is not the default setting, but
      some sites want the action that is provided. Parse out the keyword by
      ad hoc code pro tem. We know the line starts "MAIL FROM", so the backwards
      searches will always terminate without explicit stops. */

      if (accept_8bitmime)
        {
        char *p = smtp_data + (int)strlen(smtp_data);
        while (isspace(p[-1])) *(--p) = 0;
        while (isalnum(p[-1])) p--;
        if (*p != 0 && strncmpic(p-5, "BODY=", 5) == 0) p[-5] = 0;
          else break;  /* ESMTP loop */
        }
      else break;      /* ESMTP loop */
      }

    /* Now extract the address. The TRUE flag allows "<>" as a sender
    address. */

    orig_sender =
      parse_extract_address(smtp_data, &errmess, &start, &end, &sender_domain,
        TRUE);
    if (orig_sender == NULL)
      {
      DEBUG(3) debug_printf("501 %s: %s\n", smtp_data, errmess);
      fprintf(fout, "501 %s: %s\r\n", smtp_data, errmess);
      break;
      }
    sender_address = string_copy(orig_sender);

    /* If sender_address is unqualified, reject it, unless this is a
    locally generated message, in which case it will be ignored anyway.
    However, if the sending host or net is listed as permitted to send
    unqualified addresses - typically local machines behaving as MUAs -
    then just qualify the address. Run the check only for the first message
    in a connection. */

    if (!sender_local && sender_domain == 0 && sender_address[0] != 0 &&
        sender_address[0] != '@')
      {
      if (!allow_unqualified_sender_set)
        {
        allow_unqualified_sender = verify_check_host(sender_unqualified_hosts,
          &sender_unqualified_hostlist) ||
        verify_check_net(sender_unqualified_nets, &sender_unqualified_netlist);
        allow_unqualified_sender_set = TRUE;
        }

      if (allow_unqualified_sender)
        {
        sender_domain = (int)strlen(sender_address) + 1;
        sender_address = rewrite_address_qualify(sender_address, FALSE);
        DEBUG(9) debug_printf("unqualified address %s accepted\n",
          orig_sender);
        }
      else
        {
        DEBUG(3) debug_printf("501 %s: sender address must contain a domain\n",
          smtp_data);
        fprintf(fout, "501 %s: sender address must contain a domain\r\n",
          smtp_data);
        store_free(sender_address);
        sender_address = NULL;
        break;
        }
      }

    /* If configured to reject any senders explicitly (spam filtering),
    check now, but don't bother if the host has already been rejected. The
    sender check is independent of sender verification, and does not
    happen for local senders. We must always allow through the null
    sender, though. */

    if (!host_refuse_all_rcpts && !sender_local &&
         sender_address[0] != 0 &&
         (sender_reject != NULL || sender_reject_recipients != NULL ||
          sender_accept != NULL || sender_accept_recipients != NULL))
      {
      if ((sender_accept != NULL &&
           !match_address_list(sender_address, sender_domain,
           sender_accept, &re_sender_accept, -1, ':')) ||
          match_address_list(sender_address, sender_domain,
          sender_reject, &re_sender_reject, -1, ':'))
        {
        DEBUG(3) debug_printf("501 rejected: administrative prohibition\n");
        fprintf(fout, "501 rejected: administrative prohibition\r\n");
        log_write(1, LOG_MAIN|LOG_REJECT, "sender rejected: <%s> %s%s",
          orig_sender,
          (sender_fullhost != NULL)? " H=" : "",
          (sender_fullhost != NULL)? sender_fullhost : "");
        store_free(sender_address);
        sender_address = NULL;
        break;     /* Ends case statement - MAIL FROM finished */
        }

      if ((sender_accept_recipients != NULL &&
           !match_address_list(sender_address, sender_domain,
           sender_accept_recipients, &re_sender_accept_recipients, -1, ':')) ||
          match_address_list(sender_address, sender_domain,
          sender_reject_recipients, &re_sender_reject_recipients, -1, ':'))
        {
        sender_refuse_all_rcpts = TRUE;
        log_write(1, LOG_MAIN|LOG_REJECT, "recipients refused: <%s>%s%s",
          orig_sender,
          (sender_fullhost != NULL)? " H=" : "",
          (sender_fullhost != NULL)? sender_fullhost : "");
        }
      }

    /* If configured to check sender addresses, do the preliminary check
    now, unless the sender is local (in which case whatever is given here
    is ignored anyway). The check will fail if the message is to be refused at
    this stage. Another check function is called after the message has been
    received, to do more checking when the headers are available. However,
    don't bother with the check if all recipients are in any case going to
    be refused. */

    errmess = NULL;
    refuse_all_rcpts = FALSE;

    if (sender_verify || sender_try_verify)
      {
      char *old_sender_address = sender_address;

      if (!sender_local &&
          !host_refuse_all_rcpts &&
          !sender_refuse_all_rcpts &&
          !verify_sender_preliminary(&errcode, &errmess))
        {
        DEBUG(3) debug_printf("%d rejected: %s %s\n", errcode,
          errmess, orig_sender);
        fprintf(fout, "%d rejected: %s <%s>\r\n", errcode, errmess,
          orig_sender);
        log_write(1, LOG_MAIN|LOG_REJECT, "rejected MAIL FROM: %s <%s>%s%s",
          errmess,
          orig_sender,
          (sender_fullhost != NULL)? " H=" : "",
          (sender_fullhost != NULL)? sender_fullhost : "");
        store_free(sender_address);
        sender_address = NULL;
        break;
        }

      /* Verification may cause rewriting of the address. We need to reset
      sender_domain as it might be used later on when checking recipients
      for relay permissions. */

      if (sender_address != old_sender_address)
        sender_domain = strchr(sender_address, '@') + 1 - sender_address;
      }

    /* The sender address is acceptable - for now. If verification is running
    in warning mode, errmess is set non-NULL if there is a warning to be
    given. This is also the case when a bad address has been received 3 times
    and refuse_all_rcpts is set. If the address has been verified, a new sender
    address is always produced on success. However, we reflect the original one
    to the outside world. */

    if (errmess != NULL)
      {
      DEBUG(3) debug_printf("%d %s <%s>\n", errcode, errmess, orig_sender);
      fprintf(fout, "%d %s <%s>\r\n", errcode, errmess, orig_sender);
      log_write(1, LOG_MAIN|LOG_REJECT, "%s <%s>%s%s", errmess, orig_sender,
        (sender_fullhost != NULL)? " H=" : "",
        (sender_fullhost != NULL)? sender_fullhost : "");
      }
    else
      {
      DEBUG(3) debug_printf("250 <%s> is syntactically correct\n",
        orig_sender);
      fprintf(fout, "250 <%s> is syntactically correct\r\n", orig_sender);
      }

    /* Note that we haven't yet checked this sender for permission to relay
    messages through this host. */

    sender_allow_relay_anywhere_set = FALSE;

    /* RFC 821 says MAIL FROM resets state at start of message */

    accept_free_recipients();
    rcount = 0;
    break;


    /* The RCPT TO command requires an address as an operand. All we do
    here is to parse it for syntactic correctness. There may be any number
    of RCPT TO commands, specifying multiple senders. We build them all into
    a data structure that is in argc/argv format. The start/end values
    given by parse_extract_address are not used, as we keep only the
    extracted address. */

    case RCPT_CMD:
    if (sender_address == NULL)
      {
      DEBUG(3) debug_printf("503 No sender yet given\n");
      fprintf(fout, "503 No sender yet given\r\n");
      break;
      }

    if (smtp_data[0] == 0)
      {
      DEBUG(3) debug_printf("501 RCPT TO must have an address operand\n");
      fprintf(fout, "501 RCPT TO must have an address operand\r\n");
      break;
      }

    /* Handle various cases when all recipients are to be refused. This is
    the only known way of getting some remote mailers to give up on attempting
    to send a message.

    (1) The sender verify function sets refuse_all_rcpts when a bad sender has
        been received at least twice from the same host.
    (2) This function sets host_refuse_all_rcpts if the sending host is on
        a reject by recipients list.
    (3) This function sets sender_refuse_all_rcpts if the sender is on a
        rejection by recipients list.

    If any of these flags is set, bounce all recipients, using 550, which is
    the only version of "no" that some mailers understand, apparently. */

    if (refuse_all_rcpts)
      {
      DEBUG(3) debug_printf("550 cannot route to sender address <%s>\n",
        sender_address);
      fprintf(fout, "550 cannot route to sender address <%s>\r\n",
        sender_address);
      break;
      }

    if (host_refuse_all_rcpts || sender_refuse_all_rcpts)
      {
      DEBUG(3) debug_printf("550 rejected: administrative prohibition\n");
      fprintf(fout, "550 rejected: administrative prohibition\r\n");
      break;
      }

    /* Check maximum allowed */

    if (recipients_max > 0 && ++rcount > recipients_max)
      {
      DEBUG(3) debug_printf("421 too many recipients\n");
      fprintf(fout, "421 too many recipients\r\n");
      break;
      }

    /* Don't allow "<>" as a recipient address */

    orig_receiver =
      parse_extract_address(smtp_data, &errmess, &start, &end,
        &receiver_domain, FALSE);
    if (orig_receiver == NULL)
      {
      DEBUG(3) debug_printf("501 %s: %s\n", smtp_data, errmess);
      fprintf(fout, "501 %s: %s\r\n", smtp_data, errmess);
      break;
      }
    receiver = string_copy(orig_receiver);

    /* If configured to check for mail relaying, ensure that the domain
    of the recipient is acceptable, either because it is one that is allowed
    for all sending hosts and sending addresses, or because the host and
    the sender are permitted to relay to all domains. If there is no domain,
    that's acceptable here - either it will fail below or be qualified with a
    local domain, which is always permitted. */

    if (check_relay && receiver_domain != 0)
      {
      char *lcdomain = string_copylc(orig_receiver+receiver_domain);

      /* The host test is needed only for non-local domains that are not in
      relay_domains. Host_allow_relay_anywhere gets set the first time we need
      its value. */

      if (!match_isinlist(lcdomain, local_domains, &re_local_domains, TRUE) &&
          (relay_domains == NULL ||
          !match_isinlist(lcdomain, relay_domains, &re_relay_domains, TRUE)))
        {
        /* If we haven't yet checked this host, do so. The rule for allowing
        relaying to any address is "the host is in an accept list or there are
        no accept lists, and it is not in a reject list". */

        if (!host_allow_relay_anywhere_set)
          {
          host_allow_relay_anywhere = TRUE;

          if ((sender_host_accept_relay != NULL ||
               sender_net_accept_relay != NULL)
             &&
             !verify_check_net(sender_net_accept_relay,
               &sender_net_accept_relay_nets)
             &&
             !verify_check_host(sender_host_accept_relay,
               &sender_host_accept_relay_hosts)
             ) host_allow_relay_anywhere = FALSE;

          else if ((sender_host_reject_relay != NULL ||
                    sender_net_reject_relay != NULL)
             &&
             (
             verify_check_host(sender_host_reject_relay,
               &sender_host_accept_relay_hosts)
             ||
             verify_check_net(sender_net_reject_relay,
               &sender_net_reject_relay_nets)
             )) host_allow_relay_anywhere = FALSE;

          host_allow_relay_anywhere_set = TRUE;
          DEBUG(9) debug_printf("host_allow_relay_anywhere set %s\n",
            host_allow_relay_anywhere? "TRUE" : "FALSE");
          }

        /* If the host is acceptable, check up on the sender address if
        configured to do so. The "set" flag gets cleared for each new
        "MAIL FROM". This saves doing the check for multiple recipients,
        as it could involve a file lookup and therefore be expensive. */

        if (host_allow_relay_anywhere && !sender_allow_relay_anywhere_set)
          {
          sender_allow_relay_anywhere = (sender_address_relay == NULL)? TRUE :
            match_address_list(sender_address, sender_domain,
              sender_address_relay, &re_sender_address_relay, -1, ':');
          sender_allow_relay_anywhere_set = TRUE;
          DEBUG(9) debug_printf("sender_allow_relay_anywhere set %s\n",
            sender_allow_relay_anywhere? "TRUE" : "FALSE");
          }

        /* Outside user sending to outside destination.  Naughty. */

        if (!host_allow_relay_anywhere || !sender_allow_relay_anywhere)
          {
          DEBUG(3) debug_printf("553 relaying to <%s> prohibited by "
            "administrator\n", orig_receiver);
          fprintf(fout, "553 relaying to <%s> prohibited by administrator\r\n",
            orig_receiver);
          log_write(1, LOG_MAIN|LOG_REJECT, "refused relay (%s) to %s from "
            "<%s>%s%s",
            host_allow_relay_anywhere? "sender" : "host",
            orig_receiver, sender_address,
            (sender_fullhost == NULL)? "" : " H=",
            (sender_fullhost == NULL)? "" : sender_fullhost);
          break;     /* End of handling the RCPT TO command */
          }
        }
      }

    /* If the receiver address is unqualified, reject it, unless this is a
    locally generated message. However, unqualified addresses are permitted
    from a configured list of hosts and nets - typically when behaving as
    MUAs rather than MTAs. Sad that SMTP is used for both types of traffic,
    really. As a message may have many recipients, and indeed an SMTP call
    may have many messages, we remember that a host is on the permitted list
    to avoid unnecessary double checks. */

    if (!sender_local && receiver_domain == 0 && receiver[0] != '@')
      {
      if (!allow_unqualified_recipient_set)
        {
        allow_unqualified_recipient =
          verify_check_host(receiver_unqualified_hosts,
            &receiver_unqualified_hostlist) ||
        verify_check_net(receiver_unqualified_nets,
          &receiver_unqualified_netlist);
        allow_unqualified_recipient_set = TRUE;
        }

      if (allow_unqualified_recipient)
        {
        DEBUG(9) debug_printf("unqualified address %s accepted\n",
          receiver);
        receiver = rewrite_address_qualify(receiver, TRUE);
        }
      else
        {
        DEBUG(3)
          debug_printf("501 %s: recipient address must contain a domain\n",
            smtp_data);
        fprintf(fout, "501 %s: recipient address must contain a domain\r\n",
          smtp_data);
        break;
        }
      }

    /* If configured to check the receiver address now, do so, but not if
    the host matches one of the exception lists. */

    if ((receiver_verify || receiver_try_verify) &&
        !verify_check_host(receiver_verify_except_hosts,
          &receiver_verify_except_hostlist) &&
        !verify_check_net(receiver_verify_except_nets,
          &receiver_verify_except_netlist))
      {
      BOOL receiver_local;
      int rc = verify_address(receiver, TRUE, FALSE, NULL, &receiver_local,
        NULL, FALSE, FALSE);

      /* Failure causes a hard error */

      if (rc == FAIL)
        {
        if (receiver_local)
          {
          DEBUG(3) debug_printf("550 Unknown local part in <%s>\n",
            orig_receiver);
          fprintf(fout, "550 Unknown local part in <%s>\r\n", orig_receiver);
          }
        else
          {
          DEBUG(3) debug_printf("550 Cannot route to <%s>\n", orig_receiver);
          fprintf(fout, "550 Cannot route to <%s>\r\n", orig_receiver);
          }
        log_write(3, LOG_MAIN,
          "verify failed for SMTP recipient %s from <%s>%s%s",
          orig_receiver,
          sender_address,
          (sender_fullhost == NULL)? "" : " H=",
          (sender_fullhost == NULL)? "" : sender_fullhost);
        break;     /* End of handling the RCPT TO command */
        }

      /* If verification can't be done now, give a temporary error unless
      receiver_try_verify is set, in which case accept the address, but say
      it's unverified. */

      if (rc == DEFER)
        {
        if (!receiver_try_verify)
          {
          DEBUG(3) debug_printf("451 Cannot check <%s> at this time - "
            "please try later\n", orig_receiver);
          fprintf(fout, "451 Cannot check <%s> at this time - "
            "please try later\r\n", orig_receiver);
          break;   /* End of handling the RCPT TO command */
          }

        DEBUG(3) debug_printf("250 Cannot check <%s> at this time - "
          "accepted unverified\n", orig_receiver);
        fprintf(fout, "250 Cannot check <%s> at this time - "
          "accepted unverified\r\n", orig_receiver);
        }

      /* Verification succeeded */

      else
        {
        DEBUG(3) debug_printf("250 <%s> verified\n", orig_receiver);
        fprintf(fout, "250 <%s> verified\r\n", orig_receiver);
        }
      }

    /* Otherwise the receiver address is only known to be syntactically
    acceptable. Any delivery errors will happen later. */

    else
      {
      DEBUG(3) debug_printf("250 <%s> is syntactically correct\n",
        orig_receiver);
      fprintf(fout, "250 <%s> is syntactically correct\r\n", orig_receiver);
      }

    /* Add to the list of receivers, and set value to NULL to prevent
    freeing. */

    accept_add_recipient(receiver);
    receiver = NULL;
    break;


    /* The DATA command is legal only if it follows successful MAIL FROM
    and RCPT TO commands. This function is complete when a valid DATA
    command is encountered. */

    case DATA_CMD:
    if (sender_address == NULL)
      {
      DEBUG(3) debug_printf("503 MAIL FROM command must precede DATA\n");
      fprintf(fout, "503 MAIL FROM command must precede DATA\r\n");
      break;
      }
    if (recipients_count <= 0)
      {
      DEBUG(3) debug_printf("503 Valid RCPT TO <recipient> must precede DATA\n");
      fprintf(fout, "503 Valid RCPT TO <recipient> must precede DATA\r\n");
      break;
      }
    DEBUG(3) debug_printf("354 Enter message, ending with \".\" on a line by itself\n");
    fprintf(fout, "354 Enter message, ending with \".\" on a line by itself\r\n");
    done = 3;
    break;


    /* The VRFY command is enabled by a configuration option. Despite RFC1123
    it defaults disabled. */

    case VRFY_CMD:
    if (!smtp_verify)
      {
      DEBUG(3) debug_printf("502 VRFY command not available\n");
      fprintf(fout, "502 VRFY command not available\r\n");
      }

    /* When VRFY is enabled, it verifies only addresses that contain no domain
    or one of the local domains. However, we have to let the verify function
    and the routers and directors decide what is local. */

    else
      {
      BOOL is_local_domain;
      int rc;
      char *address = parse_extract_address(smtp_data, &errmess, &start, &end,
        &receiver_domain, FALSE);

      if (address == NULL)
        {
        DEBUG(3) debug_printf("501 %s\n", errmess);
        fprintf(fout, "501 %s\r\n", errmess);
        break;
        }

      rc = verify_address(address, TRUE, TRUE, NULL, &is_local_domain, NULL,
        FALSE, FALSE);

      if (!is_local_domain)
        {
        DEBUG(3) debug_printf("551 Not a local domain\n");
        fprintf(fout, "551 Not a local domain\r\n");
        }

      else switch (rc)
        {
        case OK:
        DEBUG(3) debug_printf("250 Deliverable\n");
        fprintf(fout, "250 Deliverable\r\n");
        break;

        case DEFER:
        DEBUG(3) debug_printf("450 Cannot resolve at this time\n");
        fprintf(fout, "450 Cannot resolve at this time\r\n");
        break;

        case FAIL:
        DEBUG(3) debug_printf("550 Not deliverable\n");
        fprintf(fout, "550 Not deliverable\r\n");
        break;
        }
      }
    break;


    case EXPN_CMD:
    DEBUG(3) debug_printf("502 EXPN command not available\n");
    fprintf(fout, "502 EXPN command not available\r\n");
    break;


    case QUIT_CMD:
    DEBUG(3) debug_printf("221 %s closing connection\n", primary_hostname);
    fprintf(fout, "221 %s closing connection\r\n", primary_hostname);
    accept_free_recipients();
    done = 2;
    break;


    case RSET_CMD:
    accept_free_recipients();
    sender_address = NULL;
    DEBUG(3) debug_printf("250 Reset OK\n");
    fprintf(fout, "250 Reset OK\r\n");
    break;


    case NOOP_CMD:
    DEBUG(3) debug_printf("250 OK\n");
    fprintf(fout, "250 OK\r\n");
    break;


    case DEBUG_CMD:
    DEBUG(3) debug_printf("500 No way!\n");
    fprintf(fout, "500 No way!\r\n");
    break;


    case HELP_CMD:
    DEBUG(3)
      {
      debug_printf("214-Commands supported:\n");
      debug_printf("214-    HELO EHLO MAIL RCPT DATA\n");
      debug_printf("214     NOOP QUIT RSET HELP %s\n",
        smtp_verify? "VRFY" : "");
      }
    fprintf(fout, "214-Commands supported:\r\n");
    fprintf(fout, "214-    HELO EHLO MAIL RCPT DATA\r\n");
    fprintf(fout, "214     NOOP QUIT RSET HELP %s\r\n",
      smtp_verify? "VRFY" : "");
    break;


    case EOF_CMD:
    DEBUG(3) debug_printf("421 %s lost input connection\n", primary_hostname);
    fprintf(fout, "421 %s lost input connection\r\n", primary_hostname);

    /* Don't log unless in the middle of a message, as some mailers just
    drop the call rather than sending QUIT, and it clutters up the logs. */

    if (sender_address != NULL || recipients_count > 0)
      {
      if (sender_fullhost != NULL)
        log_write(4, LOG_MAIN, "%s unexpected disconnection", sender_fullhost);
      else
        log_write(0, LOG_MAIN, "unexpected EOF from local SMTP connection");
      }

    done = 1;
    break;


    case ETRN_CMD:
    if (smtp_etrn_hosts == NULL && smtp_etrn_nets == NULL)
      {
      DEBUG(3) debug_printf("500 Command unrecognized\n");
      fprintf(fout, "500 Command unrecognized\r\n");
      break;
      }

    if (sender_address != NULL)
      {
      DEBUG(3) debug_printf("503 ETRN not permitted inside transaction\n");
      fprintf(fout, "503 ETRN not permitted inside transaction\r\n");
      break;
      }

    if (smtp_data[0] != '#')
      {
      DEBUG(3) debug_printf("501 Syntax error\n");
      fprintf(fout, "501 Syntax error\r\n");
      break;
      }

    /* Check that the current host is permitted to do this */

    if (!verify_check_host(smtp_etrn_hosts, &smtp_etrn_hostlist) &&
        !verify_check_net(smtp_etrn_nets, &smtp_etrn_netlist))
      {
      DEBUG(3) debug_printf("458 Administrative prohibition\n");
      fprintf(fout, "458 Administrative prohibition\r\n");
      break;
      }

    /* Fork a child process and call Exim with the -R option. We don't
    want to have to wait for the process at any point, so set SIGCHLD
    to SIG_IGN before forking. It should be set that way anyway for external
    incoming SMTP, but we save and restore to be tidy. */

    oldsignal = signal(SIGCHLD, SIG_IGN);

    if ((pid = fork()) == 0)
      {
      int i = 0;
      char *argv[6];

      argv[i++] = exim_path;
      if (config_changed)
        {
        argv[i++] = "-C";
        argv[i++] = config_filename;
        }
      argv[i++] = "-R";
      argv[i++] = smtp_data+1;

      argv[i++] = (char *)0;
      execv(argv[0], argv);
      log_write(0, LOG_PANIC_DIE, "exec of exim -R failed");
      _exit(EXIT_FAILURE);         /* paranoia */
      }

    signal(SIGCHLD, oldsignal);    /* restore */

    if (pid < 0)
      {
      log_write(0, LOG_PANIC, "fork of queue-runner process failed");
      fprintf(fout, "458 Unable to start queue run\r\n");
      }
    else
      {
      DEBUG(3) debug_printf("250 OK, queue -R \'%s\' started\n", smtp_data+1);
      fprintf(fout, "250 OK, queue -R \'%s\' started\r\n", smtp_data+1);
      }
    break;


    default:
    DEBUG(3) debug_printf("500 Command unrecognized\n");
    fprintf(fout, "500 Command unrecognized\r\n");
    break;
    }

  /* Ensure output gets sent, and free temporary store. */

  fflush(fout);

  if (orig_sender != NULL) store_free(orig_sender);
  if (orig_receiver != NULL) store_free(orig_receiver);
  if (receiver != NULL) store_free(receiver);
  }

/* Reset the signal handlers used in this function, and if no
message is in progress, ensure the store is cleaned up. */

signal(SIGALRM, SIG_DFL);
signal(SIGTERM, SIG_DFL);

if (done < 3) accept_free_recipients();
return done - 2;  /* Convert yield values */
}

/* End of smtp_in.c */
