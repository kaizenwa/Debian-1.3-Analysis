/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


/* The main function: entry point, initialization, and high-level control. */


#include "exim.h"




/*************************************************
*                Regerror function               *
*************************************************/

/* This function is called whenever one of the regular expression functions
detects an error. Most regular expressions are part of the configuration, in
which case a compiling error is a disaster. However, from filter files there
can be user regular expressions. If regcomp_error_pointer is not NULL, it
points at a pointer to point the message at. Otherwise, panic.

Argument:   error message passed from regexec()
Returns:    nothing
*/

void
regerror(char *s)
{
if (regcomp_error_pointer == NULL)
  log_write(0, LOG_PANIC_DIE, "regular expression error: %s%s%s", s,
    (regexp_compiling == NULL)? "" : " while compiling ",
    (regexp_compiling == NULL)? "" : regexp_compiling);
else *regcomp_error_pointer = string_copy(s);
}



/*************************************************
*             Handler for SIGUSR1                *
*************************************************/

/* SIGUSR1 causes any exim process to write to the process log details of
what it is currently doing. It will only be used if the OS is capable of
setting up a handler that causes automatic restarting of any system call
that is in progress at the time.

Argument: the signal number (SIGUSR1)
Returns:  nothing
*/

static void
usr1_handler(int sig)
{
log_write(0, LOG_PROCESS, "%s", process_info);
log_close();
os_restarting_signal(SIGUSR1, usr1_handler);
}



/*************************************************
*            Set up processing details           *
*************************************************/

/* Save a text string for dumping when SIGUSR1 is received.

Arguments: format and arguments, as for printf()
Returns:   nothing
*/

void
set_process_info(char *format, ...)
{
int len;
va_list ap;
sprintf(process_info, "%5d %s ", (int)getpid(), version_string);
len = strlen(process_info);
va_start(ap, format);
vsprintf(process_info + len, format, ap);
DEBUG(2) debug_printf("set_process_info: %s\n", process_info);
va_end(ap);
}





/*************************************************
*          Entry point and high-level code       *
*************************************************/

/* Entry point for the Exim mailer. Analyse the arguments and arrange to take
the appropriate action. All the necessary functions are present in the one
binary. I originally thought one should split it up, but it turns out that so
much of the apparatus is needed in each chunk that one might as well just have
it all available all the time, which then makes the coding easier as well.

Arguments:
  argc      count of entries in argv
  argv      argument strings, with argv[0] being the program name

Returns:    EXIT_SUCCESS if terminated successfully
            EXIT_FAILURE otherwise
*/

int
main(int argc, char **argv)
{
int  arg_accept_timeout = -1;
int  arg_error_handling = error_handling;
int  filter_fd = -1;
int  group_count;
int  i;
int  msg_action = 0;
int  msg_action_arg = -1;
int  namelen = (int)strlen(argv[0]);
int  recipients_arg = argc;
int  sender_address_domain = 0;
int  test_retry_arg = -1;
int  test_rewrite_arg = -1;
BOOL arg_queue_only = FALSE;
BOOL arg_queue_remote = FALSE;
BOOL arg_queue_smtp= FALSE;
BOOL bi_option = FALSE;
BOOL extract_recipients = FALSE;
BOOL forced_delivery = FALSE;
BOOL deliver_give_up = FALSE;
BOOL is_inetd = FALSE;
BOOL list_queue = FALSE;
BOOL list_variables = FALSE;
BOOL local_queue_only;
BOOL more = TRUE;
BOOL one_msg_action = FALSE;
BOOL queue_only_set = FALSE;
BOOL queue_remote_set = FALSE;
BOOL queue_smtp_set = FALSE;
BOOL smtp_first = TRUE;
BOOL synchronous_delivery = FALSE;
BOOL version_printed = FALSE;
char *alias_arg = NULL;
char *called_as = "";
char *start_queue_run_id = NULL;
char *stop_queue_run_id = NULL;
char *ftest_domain = NULL;
char *ftest_localpart = NULL;
char *ftest_prefix = NULL;
char *ftest_suffix = NULL;
transport_instance *ti;
struct passwd *pw;
struct stat statbuf;
struct sockaddr inetd_sock;
struct sockaddr_in *inetd_sin = (struct sockaddr_in *)(&inetd_sock);
gid_t group_list[NGROUPS_MAX];

/* Indicate to various shared functions that this is exim, not one of
its utilities. This affects some error processing. */

really_exim = TRUE;

/* Ensure we have a buffer for constructing log entries. Use malloc directly,
because store_malloc writes a log entry on failure. */

log_buffer = (char *)malloc(LOG_BUFFER_SIZE);
if (log_buffer == NULL)
  {
  fprintf(stderr, "exim: failed to get store for log buffer\n");
  exit(EXIT_FAILURE);
  }

/* Set up the handler for the data request signal, and set the initial
descriptive text. */

set_process_info("initializing");
os_restarting_signal(SIGUSR1, usr1_handler);

/* SIGHUP is used to get the daemon to reconfigure. It gets set as appropriate
in the daemon code. For the rest of exim's uses, we ignore it. */

signal(SIGHUP, SIG_IGN);

/* We don't want to die on pipe errors as the code is written to handle
the write error instead. */

signal(SIGPIPE, SIG_IGN);

/* Under some circumstance on some OS, Exim can get called with SIGCHLD
set to SIG_IGN. This causes subprocesses that complete before the parent
process waits for them not to hang around, so when Exim calls wait(), nothing
is there. The wait() code has been made robust against this, but let's ensure
that SIGCHLD is set to SIG_DFL, because it's tidier to wait and get a process
ending status. */

signal(SIGCHLD, SIG_DFL);

/* Save the arguments for use if we re-exec exim as a daemon after receiving
SIGHUP. */

sighup_argv = argv;

/* Set up the version number. Set up the leading 'E' for the external form of
message ids, set the pointer to the internal form, and initialize it to
indicate no message being processed. */

version_init();
message_id_option[0] = '-';
message_id_external = message_id_option + 1;
message_id_external[0] = 'E';
message_id = message_id_external + 1;
message_id[0] = 0;

/* Set the umask to zero so that any files that Exim creates are created
with the modes that it specifies. */

umask(0);


/* Precompile the regular expression for matching the name of a spool file.
Keep this in step with the code that generates such names in the accept.c
module. We need to do this here, because the -M options check their arguments
for syntactic validity using mac_ismsgid, which uses this. */

regexp_spoolfile = regcomp(
  "^[a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9]"
  "\\-[a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9]"
  "\\-[0-9][0-9]$");

/* Ditto for matching an IP address. */

regexp_ip_address = regcomp(
  "^[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?\\."
  "[0-9][0-9]?[0-9]?$");

/* Ditto for matching a network identification */

net_regexp = regcomp(
  "^[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?\\."
  "[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?"
  "/[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?\\."
  "[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?$");

/* Ditto for matching a "From_" line in an incoming message, in the form
   From ph10 Fri Jan  5 12:35 GMT 1996
which the "mail" commands send to the MTA (undocumented, of course). Because
of variations in time formats, just match up to the end of the minutes. That
should be sufficient. */

regexp_From = regcomp(
  "^From +([^ ]+) +[a-zA-Z][a-zA-Z][a-zA-Z] +[a-zA-Z][a-zA-Z][a-zA-Z] +"
  "[0-9]?[0-9] +[0-9][0-9]:[0-9][0-9]");


/* If the program is called as "mailq" treat it as equivalent to "exim -bp";
this seems to be a generally accepted convention, since one finds symbolic
links called "mailq" in standard OS configurations. */

if ((namelen == 5 && strcmp(argv[0], "mailq") == 0) ||
    (namelen  > 5 && strncmp(argv[0] + namelen - 6, "/mailq", 6) == 0))
  {
  list_queue = TRUE;
  called_as = "-mailq";
  }

/* If the program is called as "rmail" treat it as equivalent to "exim -i";
thus allowing UUCP messages to be input using non-SMTP mode, i.e. preventing a
single dot on a line from terminating the message. */

if ((namelen == 5 && strcmp(argv[0], "rmail") == 0) ||
    (namelen  > 5 && strncmp(argv[0] + namelen - 6, "/rmail", 6) == 0))
  {
  dot_ends = FALSE;
  called_as = "-rmail";
  }

/* If the program is called as "rsmtp" treat it as equivalent to "exim -bS";
this is a smail convention. */

if ((namelen == 5 && strcmp(argv[0], "rsmtp") == 0) ||
    (namelen  > 5 && strncmp(argv[0] + namelen - 6, "/rsmtp", 6) == 0))
  {
  smtp_input = smtp_batched_input = TRUE;
  called_as = "-rsmtp";
  }

/* If the program is called as "runq" treat it as equivalent to "exim -q";
this is a smail convention. */

if ((namelen == 4 && strcmp(argv[0], "runq") == 0) ||
    (namelen  > 4 && strncmp(argv[0] + namelen - 5, "/runq", 5) == 0))
  {
  queue_interval = 0;
  called_as = "-runq";
  }

/* Scan the program's arguments. Some can be dealt with right away; others are
simply recorded for checking and handling afterwards. Do a high-level switch
on the second character (the one after '-'), to save some effort. */

for (i = 1; i < argc; i++)
  {
  BOOL badarg = FALSE;
  char *arg = argv[i];

  /* Handle flagged arguments */

  if (arg[0] == '-')
    {
    int switchchar = arg[1];
    char *argrest = arg+2;

    /* Make all -ex arguments synonymous with -oex arguments, since that
    is assumed by various callers. */

    if (strncmp(arg+1, "oe", 2) == 0)
      {
      switchchar = 'e';
      argrest++;
      }

    /* Make -r synonymous with -f, since it is a documented alias */

    else if (arg[1] == 'r') switchchar = 'f';

    /* Make -ov synonymous with -v */

    else if (strcmp(arg, "-ov") == 0)
      {
      switchchar = 'v';
      argrest++;
      }

    /* High-level switch on active initial letter */

    switch(switchchar)
      {
      case 'b':

      /* -bd: Run in daemon mode, awaiting SMTP connections. */

      if (strcmp(argrest, "d") == 0) daemon_listen = TRUE;

      /* -bf:  Run in mail filter testing mode
         -bfd: Set domain for filter testing
         -bfl: Set local part for filter testing
         -bfp: Set prefix for filter testing
         -bfs: Set suffix for filter testing
      */

      else if (*argrest == 'f')
        {
        if (*(++argrest) == 0)
          {
          if(++i < argc) filter_test = argv[i]; else
            {
            fprintf(stderr, "exim: file name expected after -bf\n");
            exit(EXIT_FAILURE);
            }
          }
        else
          {
          if (++i >= argc)
            {
            fprintf(stderr, "exim: string expected after %s\n", arg);
            exit(EXIT_FAILURE);
            }
          if (strcmp(argrest, "d") == 0) ftest_domain = argv[i];
          else if (strcmp(argrest, "l") == 0) ftest_localpart = argv[i];
          else if (strcmp(argrest, "p") == 0) ftest_prefix = argv[i];
          else if (strcmp(argrest, "s") == 0) ftest_suffix = argv[i];
          else { badarg = TRUE; break; }
          }
        }

      /* -bi: This option is used by sendmail to initialise *the* alias file,
      though it has the -oA option to specify a different file. Exim has no
      concept of *the* alias file, but since Sun's YP make script calls
      sendmail this way, some support must be provided. */

      else if (strcmp(argrest, "i") == 0) bi_option = TRUE;

      /* -bm: Accept and deliver message - the default option. */

      else if (strcmp(argrest, "m") == 0) continue;

      /* -bp: List the contents of the mail queue */

      else if (strcmp(argrest, "p") == 0) list_queue = TRUE;

      /* -bP: List the configuration variables given as the address list. */

      else if (strcmp(argrest, "P") == 0) list_variables = TRUE;

      /* -brt: Test retry configuration lookup */

      else if (strcmp(argrest, "rt") == 0)
        {
        test_retry_arg = i + 1;
        goto END_ARG;
        }

      /* -brw: Test rewrite configuration */

      else if (strcmp(argrest, "rw") == 0)
        {
        test_rewrite_arg = i + 1;
        goto END_ARG;
        }

      /* -bS: Read SMTP commands on standard input, but produce no replies -
      all errors are reported by sending messages. */

      else if (strcmp(argrest, "S") == 0)
        smtp_input = smtp_batched_input = TRUE;

      /* -bs: Read SMTP commands on standard input and produce SMTP replies
      on standard output. */

      else if (strcmp(argrest, "s") == 0) smtp_input = TRUE;

      /* -bt: address testing mode */

      else if (strcmp(argrest, "t") == 0) address_test_mode = TRUE;

      /* -bv: verify addresses */

      else if (strcmp(argrest, "v") == 0) verify_only = TRUE;

      /* -bV: Print version string */

      else if (strcmp(argrest, "V") == 0)
        {
        printf("Exim version %s #%s built %s\n", version_string,
          version_cnumber, version_date);
        printf("%s\n", version_copyright);
        version_printed = TRUE;
        }

      else badarg = TRUE;
      break;


      /* -C: change configuration file */

      case 'C':
      if (*argrest == 0)
        {
        if(++i < argc) argrest = argv[i]; else
          { badarg = TRUE; break; }
        }
      config_filename = argrest;
      config_changed = TRUE;
      break;


      /* -d: Set debug level (see also -v below) or set memory tracing, or
      set up stderr to a file. */

      case 'd':
      if (strcmp(argrest, "m") == 0) debug_trace_memory = TRUE;
      else if (strcmp(argrest, "f") == 0)
        {
        if (stderr_filename != NULL) freopen(stderr_filename, "a", stderr);
        }
      else if (*argrest == 0 && (i+1 >= argc || !isdigit(argv[i+1][0])))
        debug_level = 1;
      else
        {
        int n = 0;
        char *s = (*argrest == 0)? argv[++i] : argrest;
        (void)sscanf(s, "%d%n", &debug_level, &n);
        if (n+2 < (int)strlen(arg)) badarg = TRUE;
        }
      if (debug_level > 0 && !badarg)
        {
        debug_file = stderr;
        debug_printf("Debug level set to %d\n", debug_level);
        }
      break;


      /* -E: This is a local error message. This option is not intended for
      external use at all, but is not restricted to trusted callers because it
      does no harm (just suppresses certain error messages) and if exim is run
      not setuid root it won't always be trusted when it generates error
      messages using this option. If there is a message id following -E, point
      message_reference at it, for logging. */

      case 'E':
      local_error_message = TRUE;
      if (regexec(regexp_spoolfile, argrest)) message_reference = argrest;
      break;


      /* -ex: The vacation program calls sendmail with the undocumented "-eq"
      option, so it looks as if historically the -oex options are also callable
      without the leading -o. So we have to accept them. Before the switch,
      anything starting -oe has been converted to -e. Exim does not support all
      of the sendmail error flags. */

      case 'e':
      if (strcmp(argrest, "m") == 0)     arg_error_handling = ERRORS_SENDER;
      else if (strcmp(argrest, "p") == 0) arg_error_handling = ERRORS_STDERR;
      else if (strcmp(argrest, "q") == 0) arg_error_handling = ERRORS_STDERR;
      else if (strcmp(argrest, "w") == 0) arg_error_handling = ERRORS_SENDER;
      else badarg = TRUE;
      break;


      /* -F: Set sender's full name, used instead of the gecos entry from
      the password file. Since users can usually alter their gecos entries,
      there's no security involved in using this instead. The data can follow
      the -F or be in the next argument. */

      case 'F':
      if (*argrest == 0)
        {
        if(++i < argc) argrest = argv[i]; else
          { badarg = TRUE; break; }
        }
      argrest = parse_fix_phrase(argrest);
      user_name = store_malloc((int)strlen(argrest) + 1);
      strcpy(user_name, argrest);
      break;


      /* -f: Set sender's address - only actually used if run by a trusted
      user, except that the null address can be set by any user. For an
      untrusted user it is used only as the envelope address for outgoing SMTP
      and to prevent mail being sent from a filter; the actual sender is still
      put in Sender: if it doesn't match the From: header. The data can follow
      the -f or be in the next argument. The -r switch is an obsolete form of
      -f but since there appear to be programs out there that use anything that
      sendmail ever supported, better accept it - the synonymizing is done
      before the switch above. */

      case 'f':
        {
        int start, end;
        char *errmess;
        if (*argrest == 0)
          {
          if (i+1 < argc) argrest = argv[++i]; else
            { badarg = TRUE; break; }
          }
        sender_address =
          parse_extract_address(argrest, &errmess, &start, &end,
            &sender_address_domain, TRUE);
        if (sender_address == NULL)
          {
          fprintf(stderr, "exim: %s - bad address: %s\n", argrest, errmess);
          return EXIT_FAILURE;
          }
        }
      break;


      /* -h: Set the hop count for an incoming message. Exim does not currently
      support this; it always computes it by counting the Received: headers.
      To put it in will require a change to the spool header file format. */

      case 'h':
      if (*argrest == 0)
        {
        if(++i < argc) argrest = argv[i]; else
          { badarg = TRUE; break; }
        }
      if (!isdigit(*argrest)) badarg = TRUE;
      break;


      /* -i: Set flag so dot doesn't end non-SMTP input (same as -oi, seems
      not to be documented for sendmail but mailx (at least) uses it) */

      case 'i':
      if (*argrest == 0) dot_ends = FALSE; else badarg = TRUE;
      break;


      case 'M':

      /* -MC:  continue delivery of another message via an existing open
      file descriptor. This option is used for an internal call by the
      smtp transport when there is a pending message waiting to go to an
      address to which it has got a connection. The caller is required to
      be root. Four subsequent arguments are required: transport name,
      host name, sequence number, and message_id. Transports may decline
      to create new processes if the sequence number gets too big. The channel
      is stdin + stdout. This must be the last argument. There's a subsequent
      check that the real-uid is privileged. */

      if (strcmp(argrest, "C") == 0)
        {
        if (argc != i + 5)
          {
          fprintf(stderr, "exim: too many or too few arguments after -MC\n");
          return EXIT_FAILURE;
          }

        if (msg_action_arg >= 0)
          {
          fprintf(stderr, "exim: incompatible arguments\n");
          return EXIT_FAILURE;
          }

        continue_transport = argv[++i];
        continue_hostname = argv[++i];
        continue_sequence = atoi(argv[++i]);
        msg_action = MSG_DELIVER;
        msg_action_arg = ++i;
        forced_delivery = TRUE;

        if (!mac_ismsgid(argv[i]))
          {
          fprintf(stderr, "exim: malformed message id %s after -MC option\n",
            argv[i]);
          return EXIT_FAILURE;
          }
        break;
        }

      /* -M[x]: various operations on the following list of message ids:
         -M    deliver the messages, ignoring next retry times
         -Mc   deliver the messages, checking next retry times
         -Mf   freeze the messages
         -Mg   give up on the messages
         -Mt   thaw the messages
         -Mrm  remove the messages
      In the above cases, this must be the last option. There are also the
      following options which are followed by a single message id, and which
      act on that message. Some of them use the "recipient" addresses as well.
         -Mar  add recipient(s)
         -Mmad mark all recipients delivered
         -Mmd  mark recipients(s) delivered
         -Mes  edit sender
         -Meb  edit body
      */

      else if (*argrest == 0)
        {
        msg_action = MSG_DELIVER;
        forced_delivery = TRUE;
        }
      else if (strcmp(argrest, "ar") == 0)
        {
        msg_action = MSG_ADD_RECIPIENT;
        one_msg_action = TRUE;
        }
      else if (strcmp(argrest, "c") == 0)  msg_action = MSG_DELIVER;
      else if (strcmp(argrest, "eb") == 0)
        {
        msg_action = MSG_EDIT_BODY;
        one_msg_action = TRUE;
        }
      else if (strcmp(argrest, "es") == 0)
        {
        msg_action = MSG_EDIT_SENDER;
        one_msg_action = TRUE;
        }
      else if (strcmp(argrest, "f") == 0)  msg_action = MSG_FREEZE;
      else if (strcmp(argrest, "g") == 0)
        {
        msg_action = MSG_DELIVER;
        deliver_give_up = TRUE;
        }
      else if (strcmp(argrest, "mad") == 0)
        {
        msg_action = MSG_MARK_ALL_DELIVERED;
        }
      else if (strcmp(argrest, "md") == 0)
        {
        msg_action = MSG_MARK_DELIVERED;
        one_msg_action = TRUE;
        }
      else if (strcmp(argrest, "rm") == 0) msg_action = MSG_REMOVE;
      else if (strcmp(argrest, "t") == 0)  msg_action = MSG_THAW;
      else { badarg = TRUE; break; }

      /* All the -Mxx options require at least one message id. */

      msg_action_arg = i + 1;
      if (msg_action_arg >= argc)
        {
        fprintf(stderr, "exim: no message ids given after %s option\n", arg);
        return EXIT_FAILURE;
        }

      /* Some require only message ids to follow */

      if (!one_msg_action)
        {
        int j;
        for (j = msg_action_arg; j < argc; j++) if (!mac_ismsgid(argv[j]))
          {
          fprintf(stderr, "exim: malformed message id %s after %s option\n",
            argv[j], arg);
          return EXIT_FAILURE;
          }
        goto END_ARG;   /* Remaining args are ids */
        }

      /* Others require only one message id, possibly followed by addresses,
      which will be handled as normal arguments. */

      else
        {
        if (!mac_ismsgid(argv[msg_action_arg]))
          {
          fprintf(stderr, "exim: malformed message id %s after %s option\n",
            argv[msg_action_arg], arg);
          return EXIT_FAILURE;
          }
        i++;
        }
      break;


      /* Some programs seem to call the -om option without the leading o;
      for sendmail it askes for "me too". Exim always does this. */

      case 'm':
      if (*argrest != 0) badarg = TRUE;
      break;


      /* -N: don't do delivery - a debugging option that stops transports doing
      their thing. It implies debugging. */

      case 'N':
      if (*argrest == 0)
        {
        dont_deliver = TRUE;
        if (debug_level <= 0)
          {
          debug_level = 1;
          debug_file = stderr;
          }
        }
      else badarg = TRUE;
      break;


      case 'o':

      /* -oA: Set an argument for the bi command (sendmail's "alternate alias
      file" option). */

      if (*argrest == 'A')
        {
        alias_arg = argrest + 1;
        if (alias_arg[0] == 0)
          {
          if (i+1 < argc) alias_arg = argv[++i]; else
            {
            fprintf(stderr, "exim: string expected after -oA\n");
            exit(EXIT_FAILURE);
            }
          }
        }

      /* -oB: Set a batch max value for (at present only SMTP) remote
      deliveries */

      else if (*argrest == 'B')
        {
        char *p = argrest + 1;
        if (p[0] == 0)
          {
          if (i+1 < argc && isdigit(argv[i+1][0])) p = argv[++i]; else
            {
            batch_max = 1;
            p = NULL;
            }
          }

        if (p != NULL)
          {
          if (!isdigit(*p))
            {
            fprintf(stderr, "exim: number expected after -oB\n");
            exit(EXIT_FAILURE);
            }
          batch_max = atoi(p);
          }
        }

      /* -odb: background delivery */

      else if (strcmp(argrest, "db") == 0)
        {
        synchronous_delivery = FALSE;
        arg_queue_only = FALSE;
        queue_only_set = TRUE;
        }

      /* -odf: foreground delivery (smail-compatible option); same effect as
         -odi: interactive (synchronous) delivery (sendmail-compatible option)
      */

      else if (strcmp(argrest, "df") == 0 || strcmp(argrest, "di") == 0)
        {
        synchronous_delivery = TRUE;
        arg_queue_only = FALSE;
        queue_only_set = TRUE;
        }

      /* -odq: queue only */

      else if (strcmp(argrest, "dq") == 0)
        {
        synchronous_delivery = FALSE;
        arg_queue_only = TRUE;
        queue_only_set = TRUE;
        }

      /* -odqr: queue remote addresses only - do local deliveries */

      else if (strcmp(argrest, "dqr") == 0)
        {
        arg_queue_remote = TRUE;
        queue_remote_set = TRUE;
        }

      /* -odqs: queue SMTP only - do local deliveries and remote routing,
      but no remote delivery */

      else if (strcmp(argrest, "dqs") == 0)
        {
        arg_queue_smtp = TRUE;
        queue_smtp_set = TRUE;
        }

      /* -oex: Sendmail error flags. As these are also accepted without the
      leading -o prefix, for compatibility with vacation and other callers,
      they are handled with -e above. */

      /* -oi: Set flag so dot doesn't end non-SMTP input (same as -i) */

      else if (strcmp(argrest, "i") == 0) dot_ends = FALSE;

      /* -oMa: Set sender host address (root or exim only) */

      else if (strcmp(argrest, "Ma") == 0) sender_host_address = argv[++i];

      /* -oMr: Received protocol (root or exim only) */

      else if (strcmp(argrest, "Mr") == 0) received_protocol = argv[++i];

      /* -oMs: Set sender host name (root or exim only) */

      else if (strcmp(argrest, "Ms") == 0) sender_host_name = argv[++i];

      /* -oMt: Set sender ident (root or exim only) */

      else if (strcmp(argrest, "Mt") == 0) sender_ident = argv[++i];

      /* -om: Me-too flag for aliases. Exim always does this. Some programs
      seem to call this as -m (undocumented), so that is also accepted (see
      above). */

      else if (strcmp(argrest, "m") == 0) {}

      /* -or <n>: set timeout for non-SMTP acceptance */

      else if (*argrest == 'r')
        {
        if (argrest[1] == 0)
          {
          if (i+1 < argc)
            arg_accept_timeout = readconf_readtime(argv[++i], 0);
          }
        else arg_accept_timeout = readconf_readtime(argrest + 1, 0);
        if (arg_accept_timeout < 0)
          {
          fprintf(stderr, "exim: bad time value %s: abandoned\n", argv[i]);
          exit(EXIT_FAILURE);
          }
        }

      /* -oX <n>: Set SMTP listening port to <n> */

      else if (strcmp(argrest, "X") == 0) smtp_port = atoi(argv[++i]);

      else badarg = TRUE;
      break;


      case 'q':

      /* -qf...: Run the queue, forcing deliveries */

      if (*argrest == 'f')
        {
        queue_run_force = TRUE;
        argrest++;
        }

      /* -q[f]l...: Run the queue only on local deliveries */

      if (*argrest == 'l')
        {
        queue_run_local = TRUE;
        argrest++;
        }

      /* -q[f][l]: Run the queue, optionally forced, optionally local only,
      optionally starting from a given message id. */

      if (*argrest == 0 &&
          (i + 1 >= argc || argv[i+1][0] == '-' || mac_ismsgid(argv[i+1])))
        {
        queue_interval = 0;
        if (i+1 < argc && mac_ismsgid(argv[i+1]))
          start_queue_run_id = argv[++i];
        if (i+1 < argc && mac_ismsgid(argv[i+1]))
          stop_queue_run_id = argv[++i];
        }

      /* -q[f][l]<n>: Run the queue at regular intervals, optionally forced,
      optionally local only. */

      else
        {
        if (*argrest != 0)
          queue_interval = readconf_readtime(argrest, 0);
        else
          queue_interval = readconf_readtime(argv[++i], 0);
        if (queue_interval <= 0)
          {
          fprintf(stderr, "exim: bad time value %s: abandoned\n", argv[i]);
          exit(EXIT_FAILURE);
          }
        }
      break;


      /* -R: Set string to match in addresses for forced queue run to
      pick out particular messages. */

      case 'R':
      if (*argrest == 0)
        {
        if (i+1 < argc)
          {
          deliver_selectstring = argv[++i];
          if (queue_interval < 0) queue_interval = 0;
          }
        else
          {
          fprintf(stderr, "exim: string expected after -R\n");
          exit(EXIT_FAILURE);
          }
        }
      else badarg = TRUE;
      break;


      /* -r: an obsolete synonym for -f (see above) */

      /* -t: Set flag to extract recipients from body of message. */

      case 't':
      if (*argrest == 0) extract_recipients = TRUE;
        else badarg = TRUE;
      break;


      /* -v: verify things - this is the same as -d or -d1. */

      case 'v':
      if (*argrest == 0)
        {
        if (debug_level <= 0)
          {
          debug_level = 1;
          debug_file = stderr;
          }
        }
      else badarg = TRUE;
      break;


      /* -x: AIX uses this to indicate some fancy 8-bit character stuff:

        The -x flag tells the sendmail command that mail from a local
        mail program has National Language Support (NLS) extended characters
        in the body of the mail item. The sendmail command can send mail with
        extended NLS characters across networks that normally corrupts these
        8-bit characters.

      As Exim is 8-bit clean, it just ignores this flag. */

      case 'x':
      if (*argrest != 0) badarg = TRUE;
      break;

      /* All other initial characters are errors */

      default:
      badarg = TRUE;
      break;
      }         /* End of high-level switch statement */


    /* Failed to recognize the option, or syntax error */

    if (badarg)
      {
      fprintf(stderr, "exim abandoned: unknown, malformed, or incomplete "
        "option %s\n", arg);
      exit(EXIT_FAILURE);
      }
    }           /* End of handling of args starting with '-' */


  /* An argument not starting with '-' is the start of a recipients list;
  break out of the argument-scanning loop. */

  else
    {
    recipients_arg = i;
    break;
    }
  }


/* Arguments have been processed. Check for incompatibilities. */

END_ARG:

if ((
    (smtp_input || extract_recipients || recipients_arg < argc) &&
    (daemon_listen || queue_interval >= 0 || list_queue || bi_option ||
      test_retry_arg >= 0 || test_rewrite_arg >= 0 ||
      filter_test != NULL || (msg_action_arg > 0 && !one_msg_action))
    ) ||
    (
    msg_action_arg > 0 &&
    (daemon_listen || queue_interval >= 0 || list_variables || verify_only ||
     address_test_mode || bi_option || test_retry_arg >= 0 ||
     test_rewrite_arg >= 0)
    ) ||
    (
    (daemon_listen || queue_interval >= 0) &&
    (sender_address != NULL || list_variables || list_queue || verify_only ||
     address_test_mode || bi_option)
    ) ||
    (
    daemon_listen && queue_interval == 0
    ) ||
    (
    list_variables &&
    (verify_only || address_test_mode || smtp_input || extract_recipients ||
      filter_test != NULL || bi_option)
    ) ||
    (
    verify_only &&
    (address_test_mode || smtp_input || extract_recipients ||
      filter_test != NULL || bi_option)
    ) ||
    (
    address_test_mode && (smtp_input || extract_recipients ||
      filter_test != NULL || bi_option)
    ) ||
    (
    smtp_input && (sender_address != NULL || filter_test != NULL)
    ) ||
    (
    deliver_selectstring != NULL && queue_interval < 0
    )
   )
  {
  fprintf(stderr, "exim: incompatible arguments\n");
  exit(EXIT_FAILURE);
  }


/* Exim is normally entered as root (except when called from inetd under its
own uid in order to receive a message, with security level >= 2). The security
level controls how it uses set{u,g}id and/or sete{g,u}id to reduce its
privilege when not necessary. However, it always spins off sub-processes that
set their uid and gid as required for local delivery. We don't want to pass on
any extra groups that root may belong to, so just get rid of them all. If this
process isn't running as root, setgroups() has no effect.

We need to obey setgroups() at this stage, before possibly giving up root
privilege for a changed configuration file, but later on we might need to check
on the additional groups for the admin user privilege - can't do that till
after reading the config, which might specify the exim gid. Therefore, save the
group list here. */

group_count = getgroups(NGROUPS_MAX, group_list);
setgroups(0, NULL);


/* Get the real uid and gid. If the configuration file name has been altered by
an argument on the command line and the caller is not root or the exim user, or
if this is a filter testing run, remove any setuid privilege the program has,
and run as the underlying user. Otherwise, set the real ids to the effective
values (should be root unless run from inetd, which it can either be root or
the exim uid, if one is configured). When running with the security options
set, root privilege will subsequently be relinquished as soon as possible. */

real_uid = getuid();
real_gid = getgid();

if ((config_changed && real_uid != root_uid &&
    (!exim_uid_set || real_uid != exim_uid)) ||
    filter_test != NULL)
  {
  setgid(real_gid);    /* Sets real and effective */
  setuid(real_uid);
  DEBUG(9) debug_printf("Removing setuid privilege\n");
  }
else
  {
  setgid(getegid());
  setuid(geteuid());
  }

/* If testing a filter, open the file now, before wasting time doing other
setups and reading the message. */

if (filter_test != NULL)
  {
  filter_fd = open(filter_test, O_RDONLY);
  if (filter_fd < 0)
    {
    printf("exim: failed to open %s: %s\n", filter_test, strerror(errno));
    return FALSE;
    }
  }

/* Ensure there is a big buffer for temporary use in several places. */

big_buffer = store_malloc(big_buffer_size);


/* Read the main runtime configuration data; this gives up if there
is a failure. It leaves the configuration file open so that the subsequent
configuration data for delivery can be read if needed. */

readconf_main();


/* Handle calls with the -bi option. This is a sendmail option to rebuild *the*
alias file. Exim doesn't have such a concept, but this call is screwed into
Sun's YP makefiles. Handle this by calling a configured script, as the real
user who called Exim. The -oA option can be used to pass an argument to the
script. */

if (bi_option)
  {
  if (bi_command != NULL)
    {
    int i = 0;
    char *argv[3];
    argv[i++] = bi_command;
    if (alias_arg != NULL) argv[i++] = alias_arg;
    argv[i++] = NULL;

    DEBUG(2) debug_printf("exec %s %s\n", argv[0],
      (argv[1] == NULL)? "" : argv[1]);

    setgid(real_gid);
    setuid(real_uid);

    execv(argv[0], argv);
    exit(errno);
    }
  else
    {
    DEBUG(2) debug_printf("-bi used but bi_command not set; exiting\n");
    exit(EXIT_SUCCESS);
    }
  }


/* The path for Exim may be changed by the configuration file. It is also
used in the argument list for calling Exim to send an error message. */

mailer_argv[0] = exim_path;

/* Set up the -E option to give the current message id, if any. If debugging is
turned on, arrange to pass the setting when we re-exec exim for error messages,
etc. Also pass on -N if set (-d is always set if -N is.) The argument list has
several 0 entries at the end, all but the last of which can be overwritten. */

i = -1;
while (mailer_argv[++i] != (char *)0);
mailer_argv[i++] = message_id_option;

if (debug_level > 0)
  {
  mailer_argv[i++] = string_sprintf("-d%d", debug_level);
  if (dont_deliver) mailer_argv[i++] = "-N";
  }

/* Ensure any alternate configuration file is passed on */

if (config_changed)
  {
  mailer_argv[i++] = "-C";
  mailer_argv[i] = config_filename;
  }


/* If an SMTP message is being received and the uid is root or exim, check to
see whether this is a call from inetd by looking for a peername. We do this
early, while still root, so that we can also call os_getloadavg() if required,
because some OS need the first call to os_getloadavg() to be done while root,
in order to open a kernel file. The socket information is kept available for
use later. */

if (smtp_input && (real_uid == root_uid ||
    (exim_uid_set && real_uid == exim_uid)))
  {
  int size = sizeof(inetd_sock);
  if (getpeername(0, &inetd_sock, &size) == 0) is_inetd = TRUE;
  }


/* If the load average is going to be needed while receiving a message,
get it now, because some OS require the first call to os_getloadavg() to
be done as root. What a pain. */

if ((is_inetd && smtp_load_reserve >= 0) ||
    (queue_only_load >= 0 &&
      (smtp_input || extract_recipients ||
        (recipients_arg < argc && !verify_only && !address_test_mode))))
  load_average = os_getloadavg();

/* If an action on specific messages is requested, or if a daemon or queue
runner is being started, we need to know if Exim was called by an admin user.
This is the case if the real user is root or exim, or if the real group is
exim, or if one of the supplementary groups is exim. Note: this is not the same
as "trusted user" status. We don't fail all message actions immediately if not
admin_user, since some actions can be performed by others. Instead, set
admin_user for later interrogation. */

if (msg_action_arg > 0 || daemon_listen || queue_interval >= 0)
  {
  if (real_uid == root_uid ||
    (exim_uid_set && real_uid == exim_uid) ||
    (exim_gid_set && real_gid == exim_gid))
      admin_user = TRUE;
  else if (exim_gid_set)
    {
    while (--group_count >= 0)
      if (group_list[group_count] == exim_gid) { admin_user = TRUE; break; }
    }
  }


/* Only an admin user may start the daemon or force a queue run. Only an admin
user may request that a message be returned to its sender forthwith. */

if (!admin_user && (deliver_give_up || daemon_listen || queue_interval >= 0))
  {
  fprintf(stderr, "exim: permission denied\n");
  exit(EXIT_FAILURE);
  }


/* Set the working directory to be the top-level spool directory. We don't rely
on this in the code, which always uses fully qualified names, but it's useful
for core dumps etc. Don't complain if it fails - the spool directory might not
be generally accessible and calls with the -C option have lost privilege by
now. */

if (chdir(spool_directory) != 0)
  {
  directory_make(spool_directory, "", SPOOL_DIRECTORY_MODE);
  (void) chdir(spool_directory);
  }


/* If the real user is not root or the exim uid, the argument for passing
in an open TCP/IP connection for another message is not permitted, nor is
running with the -N option for any delivery action, unless this call to exim is
one that supplied an input message. */

if (real_uid != root_uid && (!exim_uid_set || real_uid != exim_uid) &&
     (continue_hostname != NULL ||
       (dont_deliver &&
         (queue_interval >= 0 || daemon_listen || msg_action_arg > 0)
       )
     ))
  {
  fprintf(stderr, "exim: Permission denied\n");
  return EXIT_FAILURE;
  }

/* See if the caller is root or exim or is in the list of trusted uids or gids.
Trusted callers are permitted to specify sender addresses with -f on the
command line. */

if (real_uid == root_uid || (exim_uid_set && real_uid == exim_uid))
  trusted_caller = TRUE;
else
  {
  int i;
  if (trusted_users != NULL)
    {
    for (i = 1; i <= (int)(trusted_users[0]); i++)
      if (trusted_users[i] == real_uid)
        { trusted_caller = TRUE; break; }
    }
  if (!trusted_caller && trusted_groups != NULL)
    {
    for (i = 1; i <= (int)(trusted_groups[0]); i++)
      if (trusted_groups[i] == real_gid)
        { trusted_caller = TRUE; break; }
    }
  }

/* If the caller is not trusted, certain arguments are ignored. Note that
authority for performing certain actions on messages is tested in the
queue_action() function. */

if (!trusted_caller)
  {
  sender_host_name = sender_host_address = sender_ident = NULL;
  received_protocol = NULL;
  }

/* The queue_only configuration option can be overridden by -odx on the command
line, the queue_smtp configuration option can be overridden by -odbs, the
queue_remote configuration option can be overridden by -odqr, and the
accept_timeout option can be overridden by -or. */

if (queue_only_set) queue_only = arg_queue_only;
if (queue_smtp_set) queue_smtp = arg_queue_smtp;
if (queue_remote_set) queue_remote = arg_queue_remote;
if (arg_accept_timeout >= 0) accept_timeout = arg_accept_timeout;

/* If an exim uid is specified, handle setuid/seteuid setting according
to the security level. The macros mac_setegid and mac_seteuid are defined as 0
on systems which do not have seteuid and setegid, so that this code will
compile on all systems. However, the security level won't be set to values that
cause the use of the sete{u,g}id functions on systems that don't have them (see
readconf.c). */

if (exim_uid_set)
  {
  /* Level 1: Use seteuid to reduce privilege at all times it is not needed,
  but use setuid if we know that privilege is never going to be required again,
  typically for enquiry type calls. However, privilege might be needed for
  the forwardfile director, so can't lose it for verification. */

  if (security_level == 1)
    {
    if (queue_interval < 0 &&                  /* not running the queue */
        !daemon_listen &&                      /* and not starting the daemon */
        (msg_action_arg < 0 ||
          msg_action != MSG_DELIVER) &&        /* and not delivering */
        !verify_only &&                        /* and not verifying */
        !address_test_mode &&                  /* and not testing addresses */
        (list_variables ||                     /* and either not receiving */
          (recipients_arg >= argc && !extract_recipients && !smtp_input) ||
          queue_only))                         /* or just queueing */
      {
      setgid(exim_gid);
      setuid(exim_uid);
      }

    /* Running the queue, starting daemon, delivering, or receiving with
    queue_only FALSE. */

    else
      {
      mac_setegid(exim_gid);
      mac_seteuid(exim_uid);
      }
    }

  /* Level 2: if neither starting the daemon nor delivering messages,
  verifying, or testing an address, always use setuid to remove all root
  privilege. Level 2 makes no use of the sete{g,u}id functions; subsequent
  re-execs are used to regain root privilege for delivering.

  Level 3: as level 2, but in addition use sete{g,u}id at other times. */

  else
    {
    if (queue_interval != 0 &&          /* no queue run, or periodic queue run */
        !daemon_listen &&               /* and not starting listener */
        (msg_action_arg < 0 ||
          msg_action != MSG_DELIVER) && /* and not delivering */
        !verify_only &&                 /* and not verifying */
        !address_test_mode)             /* not testing addresses */
      {
      setgid(exim_gid);
      setuid(exim_uid);
      }

    /* Level 3 */

    else if (security_level > 2)
      {
      mac_setegid(exim_gid);
      mac_seteuid(exim_uid);
      }
    }
  }


/* Handle a request to list the delivery queue */

if (list_queue)
  {
  set_process_info("listing the queue");
  queue_list();
  exit(EXIT_SUCCESS);
  }


/* Handle actions on specific messages, except for the force delivery action,
which is done below. Some actions take a whole list of message ids, which
are known to continue up to the end of the arguments. Others take a single
message id and then operate on the recipients list. */

if (msg_action_arg > 0 && msg_action != MSG_DELIVER)
  {
  int yield = EXIT_SUCCESS;
  set_process_info("acting on specified messages");

  if (!one_msg_action)
    {
    for (i = msg_action_arg; i < argc; i++)
      if (!queue_action(argv[i], msg_action, stdout, NULL, 0, 0))
        yield = EXIT_FAILURE;
    }

  else if (!queue_action(argv[msg_action_arg], msg_action, stdout, argv,
    argc, recipients_arg)) yield = EXIT_FAILURE;

  exit(yield);
  }


/* All the modes below here require the delivery configuration options
to be set up. Doing it here saves effort when several processes are
subsequently spun off. */

transport_init();
direct_init();
route_init();
readconf_retries();
readconf_rewrites();
fclose(config_file);


/* Handle the -brt option. This is for checking out retry configurations.
The next three arguments are a domain name or a complete address, and
optionally two error numbers. All it does is to call the function that
scans the retry configuration data. */

if (test_retry_arg >= 0)
  {
  retry_config *yield;
  int errno = 0;
  int more_errno = 0;
  char *s1, *s2;

  if (test_retry_arg >= argc)
    {
    printf("-brt needs a domain or address argument\n");
    return EXIT_FAILURE;
    }
  s1 = argv[test_retry_arg++];
  s2 = NULL;

  /* If the first argument contains no @ and no . it might be a local user
  or it might be a single-component name. Treat as a domain. */

  if (strchr(s1, '@') == NULL && strchr(s1, '.') == NULL)
    {
    printf("Warning: \"%s\" contains no '@' and no '.' characters. It is "
      "being \ntreated as a one-component domain, not as a local part.\n\n",
      s1);
    }

  /* There may be an optional second domain arg. */

  if (test_retry_arg < argc && strchr(argv[test_retry_arg], '.') != NULL)
    s2 = argv[test_retry_arg++];

  /* The final arg is an error name */

  if (test_retry_arg < argc)
    {
    char *ss = argv[test_retry_arg];
    char *error =
      readconf_retry_error(ss, ss + (int)strlen(ss), &errno, &more_errno);
    if (error != NULL)
      {
      printf("%s\n", error);
      return EXIT_FAILURE;
      }
    }

  yield = retry_find_config(s1, s2, errno, more_errno);
  if (yield == NULL) printf("No retry information found\n"); else
    {
    retry_rule *r;
    more_errno = yield->more_errno;
    printf("Retry rule: %s  ", yield->destination);

    if (yield->basic_errno == ERRNO_EXIMQUOTA)
      {
      printf("quota%s%s  ",
        (more_errno > 0)? "_" : "",
        (more_errno > 0)? readconf_printtime(more_errno) : "");
      }
    else if (yield->basic_errno == ECONNREFUSED)
      {
      printf("refused%s%s  ",
        (more_errno > 0)? "_" : "",
        (more_errno == 'M')? "MX" :
        (more_errno == 'A')? "A" : "");
      }
    else if (yield->basic_errno == ETIMEDOUT)
      {
      printf("timeout%s%s  ",
        (more_errno > 0)? "_" : "",
        (more_errno =='D')? "DNS" :
        (more_errno == 'C')? "connect" : "");
      }

    for (r = yield->rules; r != NULL; r = r->next)
      {
      printf("%c,%s", r->rule, readconf_printtime(r->timeout)); /* Do not */
      printf(",%s", readconf_printtime(r->p1));                 /* amalgamate */
      if (r->rule == 'G')
        {
        int x = r->p2;
        int f = x % 1000;
        int d = 100;
        printf(",%d.", x/1000);
        do
          {
          printf("%d", f/d);
          f %= d;
          d /= 10;
          }
        while (f != 0);
        }
      printf("; ");
      }

    printf("\n");
    }
  return EXIT_SUCCESS;
  }


/* Handle the -brw option, which is for checking out rewriting
rules. Cause log writes (on errors) to go to stderr instead. */

if (test_rewrite_arg >= 0)
  {
  really_exim = FALSE;
  if (test_rewrite_arg >= argc)
    {
    printf("-brw needs an address argument\n");
    return EXIT_FAILURE;
    }
  rewrite_test(argv[test_rewrite_arg]);
  return EXIT_SUCCESS;
  }


/* Handle a request to list one or more configuration variables */

if (list_variables)
  {
  set_process_info("listing variables");
  if (recipients_arg >= argc) readconf_print("all", NULL);
    else for (i = recipients_arg; i < argc; i++)
      {
      if (i < argc - 1 &&
          (strcmp(argv[i], "director") == 0 ||
           strcmp(argv[i], "router") == 0 ||
           strcmp(argv[i], "transport") == 0))
        {
        readconf_print(argv[i+1], argv[i]);
        i++;
        }
      else readconf_print(argv[i], NULL);
      }
  exit(EXIT_SUCCESS);
  }


/* If, when debugging, the debug_transport configuration option is set, find
the debug transport. This will then be substituted for the real transport for
all messages. */

#ifdef TRANSPORT_DEBUG
if (debug_transport_file != NULL)
  {
  transport_info *tt;
  for (tt = transports_available; tt->driver_name[0] != 0; tt++)
    {
    if (strcmp("debug", tt->driver_name) == 0)
      {
      debug_transport = tt;
      break;
      }
    }
  if (debug_transport == NULL)
    {
    fprintf(stderr, "exim: debug_transport specified, but "
      "debug transport not found\n");
    exit(EXIT_FAILURE);
    }
  }
#endif


/* Ensure the address of the mailmaster is fully qualified. Don't use
rewrite_address_qualify, as that frees the input store, and errors_address
may not be in dynamic store. */

if (strchr(errors_address, '@') == NULL)
  errors_address = string_sprintf("%s@%s", errors_address,
    qualify_domain_sender);

/* Search the configured transports for the assumed ones that will be used
for generated pipe and file addresses and auto replies. If they are not found,
do not complain now. A complaint will be generated later if an address actually
causes a pipe or file or auto-reply delivery to be generated. */

for (ti = transports; ti != NULL; ti = ti->next)
  {
  if (strcmp(ti->name, address_pipe_transport) == 0)
    transport_address_pipe = ti;
  else if (strcmp(ti->name, address_file_transport) == 0)
    transport_address_file = ti;
  else if (strcmp(ti->name, address_reply_transport) == 0)
    transport_address_reply = ti;
  }


/* Handle a request to deliver one or more messages that are already on the
queue. Values of msg_action other than MSG_DELIVER are dealt with above. This
is typically used for a small number when prodding by hand (when the option
forced_delivery will be set) or when re-execing to regain root privilege.
Each message delivery must happen in a separate process, so we fork a process
for each one, and run them sequentially so that debugging output doesn't get
intertwined, and to avoid spawning too many processes if a long list is given.
However, don't fork for the last one; this saves a process in the common case
when Exim is called to deliver just one message. */

if (msg_action_arg > 0)
  {
  set_process_info("delivering specified messages");
  if (forced_delivery) queue_smtp = queue_remote = FALSE;
  for (i = msg_action_arg; i < argc; i++)
    {
    int status;
    if (i == argc - 1)
      (void)deliver_message(argv[i], forced_delivery, FALSE, deliver_give_up);
    else if (fork() == 0)
      {
      (void)deliver_message(argv[i], forced_delivery, FALSE, deliver_give_up);
      _exit(EXIT_SUCCESS);
      }
    else wait(&status);
    }
  exit(EXIT_SUCCESS);
  }



/* Handle a request to verify a list of addresses. */

if (verify_only)
  {
  if (recipients_arg >= argc)
    {
    fprintf(stderr, "exim: no addresses given for verification\n");
    return EXIT_FAILURE;
    }
  for (i = recipients_arg; i < argc; i++)
    {
    char *s = argv[i];
    while (*s != 0)
      {
      BOOL finished = FALSE;
      char *ss = parse_find_address_end(s, FALSE);
      if (*ss == ',') *ss = 0; else finished = TRUE;
      (void) verify_address(s, TRUE, TRUE, stdout, NULL, NULL,
        FALSE, FALSE);
      s = ss;
      if (!finished) while (*(++s) != 0 && (*s == ',' || isspace(*s)));
      }
    }
  direct_tidyup();
  route_tidyup();
  search_tidyup();
  exit(EXIT_SUCCESS);
  }


/* Handle the case of address test mode - if no arguments are given, read
addresses from stdin. Set debug_level to at least 1 to get full output. */

if (address_test_mode)
  {
  if (debug_level <= 0) { debug_level = 1; debug_file = stderr; }
  if (recipients_arg < argc)
    {
    while (recipients_arg < argc)
      {
      char *s = argv[recipients_arg++];
      while (*s != 0)
        {
        BOOL finished = FALSE;
        char *ss = parse_find_address_end(s, FALSE);
        if (*ss == ',') *ss = 0; else finished = TRUE;
        (void) verify_address(string_copy(s), TRUE, TRUE, stdout, NULL,
          NULL, FALSE, TRUE);
        s = ss;
        if (!finished) while (*(++s) != 0 && (*s == ',' || isspace(*s)));
        }
      }
    }
  else
    {
    for (;;)
      {
      char buffer[256];
      printf("> ");
      if (fgets(buffer, 256, stdin) == NULL) break;
      (void) verify_address(string_copy(buffer), TRUE, TRUE, stdout, NULL, NULL,
        FALSE, TRUE);
      }
    printf("\n");
    }
  direct_tidyup();
  route_tidyup();
  search_tidyup();
  exit(EXIT_SUCCESS);
  }



/* If only a single queue run is requested, without SMTP listening, we can just
turn into a queue runner, with an optional starting message id. */

if (queue_interval == 0 && !daemon_listen)
  {
  DEBUG(1) debug_printf("Single queue run%s%s%s%s\n",
    (start_queue_run_id == NULL)? "" : " starting at ",
    (start_queue_run_id == NULL)? "" : start_queue_run_id,
    (stop_queue_run_id == NULL)?  "" : " stopping at ",
    (stop_queue_run_id == NULL)?  "" : stop_queue_run_id);
  set_process_info("running the queue (single queue run)");
  queue_run(start_queue_run_id, stop_queue_run_id);
  exit(EXIT_SUCCESS);
  }


/* Find the login name of the real user running this process. This is always
needed, because it is written into the spool file. It may also be used to
construct a from: or a sender: header, and in this case we need the user's full
name as well, so save a copy of it, checked for RFC822 syntax and munged if
necessary, if it hasn't previously been set by the -F argument. We try to get
the passwd entry more than once, in case NIS or other delays are in evidence. */

user_login = NULL;
for (i = 1; i <= 10; i++)
  {
  if ((pw = getpwuid(real_uid)) != NULL)
    {
    user_login = string_copy(pw->pw_name);

    /* If user name has not been set by -F, set it from the passwd entry
    unless -f has been used to set the sender address by a trusted user. */

    if (user_name == NULL)
      {
      if (sender_address == NULL || (!trusted_caller && filter_test == NULL))
        {
        char *s = pw->pw_gecos;
        char *t = big_buffer;

        /* Most Unix specify that a '&' character in the gecos field is
        replaced by a copy of the login name, and some even specify that
        the first character should be upper cased, so that's what we do. */

        *t = 0;
        while (*s != 0)
          {
          if (*s == '&')
            {
            strcat(t, user_login);
            *t = toupper(*t);
            while (*t != 0) t++;
            s++;
            }
          else *t++ = *s++;
          }
        *t = 0;

        /* If no pattern or no replacement supplied, just use what was
        found in the file. */

        if (gecos_pattern == NULL || gecos_name == NULL)
          user_name = string_copy(parse_fix_phrase(big_buffer));

        /* If a pattern for matching the gecos field was supplied, apply
        it and then expand the name string .*/

        else
          {
          char *name = big_buffer;
          regexp *re;
          regexp_compiling = gecos_pattern;
          re = regcomp(gecos_pattern);
          if (regexec(re, name))
            {
            char *new_name;
            expand_nmax = 0;
            for (; expand_nmax < NSUBEXP; expand_nmax++)
              {
              expand_nstring[expand_nmax] = re->startp[expand_nmax];
              expand_nlength[expand_nmax] = re->endp[expand_nmax] -
                expand_nstring[expand_nmax];
              }
            expand_nmax--;
            new_name = expand_string(gecos_name);
            expand_nmax = -1;
            if (new_name != NULL)
              {
              DEBUG(4) debug_printf("user name \"%s\" extracted from "
                "gecos field \"%s\"\n", new_name, name);
              name = new_name;
              }
            else DEBUG(4) debug_printf("failed to expand gecos_name string "
              "\"%s\": %s\n", gecos_name, expand_string_message);
            }
          else DEBUG(4) debug_printf("gecos_pattern \"%s\" did not match "
            "gecos field \"%s\"\n", gecos_pattern, name);
          user_name = string_copy(parse_fix_phrase(name));
          }
        }

      /* A trusted caller has used -f but not -F */

      else user_name = "";
      }

    /* Break the retry loop */

    break;
    }
  sleep(1);
  }

/* If we cannot get a user login, log the incident and give up, unless the
configuration specifies something to use. */

if (user_login == NULL)
  {
  if (unknown_login != NULL)
    {
    user_login = expand_string(unknown_login);
    if (user_name == NULL)
      user_name = (unknown_username != NULL)?
        expand_string(unknown_username) : "";
    }
  else log_write(0, LOG_PANIC_DIE, "Failed to get user name for uid %d",
    (int)real_uid);
  }

/* When operating on spool files, the following are the variables that
are used, so set them here so the standard code can be used to write
a header file. */

originator_login = user_login;
originator_uid = real_uid;
originator_gid = real_gid;

/* Run in daemon and/or queue-running mode. The function daemon_go() never
returns. */

if (daemon_listen || queue_interval > 0) daemon_go();


/* Arrange for message reception if recipients or SMTP were specified;
otherwise complain unless a version print happened or this is a filter
verification test. */

if (recipients_arg >= argc && !extract_recipients && !smtp_input)
  {
  if (version_printed) return EXIT_SUCCESS;
  if (filter_test == NULL)
    {
    fprintf(stderr, "exim: neither action flags nor mail addresses given\n");
    return EXIT_FAILURE;
    }
  }


/* Accept one or more new messages on the standard input; accept_msg
returns TRUE if there are more messages to be read (SMTP input), or
FALSE otherwise (not SMTP, or SMTP channel collapsed).

When a message has been read, its id is returned in message_id[]. If
doing immediate delivery, we fork a delivery process for each received
message, except for the last one, where we can save a process switch. */

/* If the sender ident has not been set (by a trusted caller) set it to
the caller. This will get overwritten below for an inetd call. */

if (sender_ident == NULL)
  sender_ident = (user_login != NULL)? user_login :
    string_sprintf("uid%d", (int)real_uid);

/* It is only in this mode that error_handling is allowed to be changed from
its default of ERRORS_SENDER by argument. (Idle thought: are any of the
sendmail error modes other than -oem ever actually used?) */

if (!smtp_input) error_handling = arg_error_handling;

/* If this is an inetd call, set up the host address and make an identd call to
get the sender_ident. Also ensure that stderr is NULL to prevent panic logging
being sent down the socket. */

else if (is_inetd)
  {
  fclose(stderr);
  sender_host_address = string_copy(inet_ntoa(inetd_sin->sin_addr));
  sender_fullhost = string_sprintf("[%s]", sender_host_address);
  verify_get_ident(0);
  set_process_info("handling incoming connection from [%s]%s%s%s via inetd",
    sender_host_address,
    (sender_ident == NULL)? "" : " (",
    (sender_ident == NULL)? "" : sender_ident,
    (sender_ident == NULL)? "" : ")");
  }


/* If stdout does not exist, then dup stdin to stdout. This can happen
if exim is started from inetd. In this case fd 0 will be set to the socket,
but fd 1 will not be set. */

if (fstat(1, &statbuf) < 0) dup2(0, 1);


/* If the sender host address has been set, build sender_fullhost. */

if (sender_host_address != NULL)
  sender_fullhost = string_sprintf("%s%s[%s]",
    (sender_host_name != NULL)? sender_host_name : "",
    (sender_host_name != NULL)? " " : "",
     sender_host_address);

/* Otherwise, set the sender host as unknown. This prevents host checking in
the case of -bs not from inetd and also for -bS. */

else sender_host_unknown = TRUE;


/* If a non-trusted user supplies a sender address of "<>", then this has the
effect of setting the user_null_sender flag, which affects the MAIL FROM
command on outgoing SMTP, handling of the MAIL command in filters, and setting
of the Return_Path header in delivered messages, but is otherwise ignored. */

if (sender_address != NULL && sender_address[0] == 0 && !trusted_caller)
  user_null_sender = TRUE;

/* If the user running this process is not trusted, or if a trusted caller did
not supply a sender address with a -f argument for the non-smtp interface, then
the sender is local and the address is the user running the process. However,
allow anybody to use -f when testing filter files. */

if ((sender_address == NULL && !smtp_input) ||
    (!trusted_caller && filter_test == NULL))
  {
  sender_local = TRUE;
  sender_address = user_login;
  sender_address_domain = 0;
  }

/* If the user running this process is trusted and has supplied a sender
address with a -f argument, or is passing in the message via SMTP and the
sending host address has been set (inetd invocation or otherwise), the sender
is not considered to be local. */

else sender_local = FALSE;

/* Ensure that the sender address is fully qualified unless it is the empty
address, which indicates an error message, or doesn't exist (root caller, smtp
interface, no -f argument). */

if (sender_address != NULL && sender_address_domain == 0 &&
    sender_address[0] != 0 && sender_address[0] != '@')
  {
  char *new_sender_address =
    store_malloc((int)strlen(sender_address) +
      (int)strlen(qualify_domain_sender) + 2);
  sprintf(new_sender_address, "%s@%s", sender_address, qualify_domain_sender);
  sender_address = new_sender_address;
  }

/* Set up the incoming protocol name and the state of the program. Root
is allowed to force received protocol via the -oMr option above, and if we are
in a non-local SMTP state it means we have come via inetd and the process info
has already been set up. */

if (smtp_input)
  {
  if (received_protocol == NULL) received_protocol = "local-smtp";
  if (sender_local) set_process_info("accepting a local SMTP message from <%s>",
    sender_address);
  }
else
  {
  if (received_protocol == NULL)
    received_protocol = string_sprintf("local%s", called_as);
  set_process_info("accepting a local non-SMTP message from <%s>",
    sender_address);
  }

/* Loop for several messages when reading SMTP input. */

local_queue_only = queue_only;


while (more)
  {
  /* In the SMTP case, we have to handle the initial SMTP input and
  build the recipients list, before calling accept_msg to read the
  message proper. Whatever sender address is actually given in the
  SMTP transaction is actually ignored for local senders - we use
  the actual sender, which is either the underlying user running this
  process or a -f argument provided by a root caller. This is indicated
  by the existence of sender_address, which will be NULL only for a trusted
  caller when no -f was given. The function smtp_setup_msg() throws away
  any previous recipients list that may be lying around from a previous
  message. */

  if (smtp_input)
    {
    int rc;
    char *real_sender_address = sender_address;
    if ((rc = smtp_setup_msg(stdin, stdout, smtp_first)) > 0)
      {
      smtp_first = FALSE;

      if (real_sender_address != NULL)
        {
        store_free(sender_address);
        sender_address = real_sender_address;
        }
      more = accept_msg(stdin, stdout, extract_recipients);
      if (real_sender_address == NULL)
        {
        store_free(sender_address);
        sender_address = real_sender_address;
        }

      if (message_id[0] == 0)
        {
        if (more) continue;
        exit(EXIT_FAILURE);
        }
      }
    else exit((rc == 0)? EXIT_SUCCESS : EXIT_FAILURE);
    }

  /* In the non-SMTP case, we have all the information from the command
  line, but must process it in case it is in the more general RFC822
  format, and in any case, to detect syntax errors. Also, it appears that
  the use of comma-separated lists as single arguments is common, so we
  had better support them. */

  else
    {
    int i;
    int rcount = 0;
    int count = argc - recipients_arg;
    char **list = argv + recipients_arg;

    /* Loop for each argument */

    for (i = 0; i < count; i++)
      {
      int start, end, domain;
      char *errmess;
      char *s = list[i];

      /* Loop for each comma-separated address */

      while (*s != 0)
        {
        BOOL finished = FALSE;
        char *receiver;
        char *ss = parse_find_address_end(s, FALSE);

        if (*ss == ',') *ss = 0; else finished = TRUE;

        /* Check max recipients - if -t was used, these aren't recipients */

        if (recipients_max > 0 && ++rcount > recipients_max &&
            !extract_recipients)
          {
          if (error_handling == ERRORS_STDERR)
            fprintf(stderr, "exim: too many recipients\n");
          else
            moan_to_sender(ERRMESS_TOOMANYRECIP, NULL, NULL, stdin);
          return EXIT_FAILURE;
          }

        receiver =
          parse_extract_address(s, &errmess, &start, &end, &domain, FALSE);
        if (receiver == NULL)
          {
          if (error_handling == ERRORS_STDERR)
            fprintf(stderr, "exim: bad address \"%s\": %s\n",
              string_printing(list[i], FALSE), errmess);
          else
            {
            error_block eblock;
            eblock.next = NULL;
            eblock.text1 = string_printing(list[i], FALSE);
            eblock.text2 = errmess;
            moan_to_sender(ERRMESS_BADARGADDRESS, &eblock, NULL, stdin);
            }
          return EXIT_FAILURE;
          }
        accept_add_recipient(receiver);
        s = ss;
        if (!finished) while (*(++s) != 0 && (*s == ',' || isspace(*s)));
        }
      }

    /* Show the recipients when debugging */

    DEBUG(9)
      {
      int i;
      if (sender_address != NULL) debug_printf("Sender: %s\n", sender_address);
      if (recipients_list != NULL)
        {
        debug_printf("Recipients:\n");
        for (i = 0; i < recipients_count; i++)
          debug_printf("  %s\n", recipients_list[i]);
        }
      }

    /* Read the data for the message. If filter_test is true, this will
    just read the headers for the message, and not write anything onto
    the spool. */

    more = accept_msg(stdin, stdout, extract_recipients);

    /* more is always FALSE here (not SMTP message) */

    if (message_id[0] == 0) return EXIT_FAILURE;
    }

  /* If this is a filter testing run, there are headers in store, but
  no message on the spool. Run the filtering code in testing mode, setting
  the domain to the qualify domain and the local part to the current user,
  unless they have been set by options. The prefix and suffix are left unset
  unless specified. The the return path is set to to the sender unless it has
  already been set from a return-path header in the message. */

  if (filter_test != NULL)
    {
    deliver_domain = (ftest_domain != NULL)?
      ftest_domain : qualify_domain_recipient;
    deliver_domain_orig = deliver_domain;
    deliver_localpart = (ftest_localpart != NULL)?
      ftest_localpart : user_login;
    deliver_localpart_orig = deliver_localpart;
    deliver_localpart_prefix = ftest_prefix;
    deliver_localpart_suffix = ftest_suffix;
    deliver_home = getenv("HOME");
    if (return_path == NULL) return_path = string_copy(sender_address);
    exit(filter_runtest(filter_fd)? EXIT_SUCCESS : EXIT_FAILURE);
    }

  /* Else act on the result of message reception. We should not get here unless
  message_id[0] is non-zero. If queue_only is set local_queue_only will be
  TRUE. If it is not, and queue_only_load is set, check that the load average
  is below it. If it is not, set local_queue_only TRUE. Note that it then
  remains this way for any subsequent messages on the same SMTP connection.
  This is a deliberate choice; even though the load average may fall, it
  doesn't seem right to deliver later messages on the same call when not
  delivering earlier ones. */

  if (!local_queue_only && queue_only_load >= 0)
    {
    local_queue_only = (load_average = os_getloadavg()) > queue_only_load;
    DEBUG(1)
      if (local_queue_only)
        debug_printf("no delivery: load average = %.2f\n",
          (double)load_average/1000.0);
    }

  if (!local_queue_only)
    {
    /* If running as root, or if we can regain root by seteuid, call the
    delivery function directly for synchronous delivery, or fork a clone of
    this process to deliver in the background. A fork failure is not a
    disaster, as the delivery will eventually happen on a subsequent queue
    run. */

    if (security_level <= 1)
      {
      if (synchronous_delivery)
        (void)deliver_message(message_id, FALSE, FALSE, FALSE);
      else if(fork() == 0)
        {
        (void)deliver_message(message_id, FALSE, FALSE, FALSE);
        _exit(EXIT_SUCCESS);
        }
      }

    /* Otherwise, we have to re-exec exim in order to regain root for the
    delivery of the message. */

    else
      {
      pid_t pid;
      if ((pid = fork()) == 0)
        {
        int i = 0;
        char *argv[11];
        argv[i++] = exim_path;
        if (debug_level > 0)
          argv[i++] = string_sprintf("-d%d", debug_level);
        if (dont_deliver) argv[i++] = "-N";
        if (queue_smtp) argv[i++] = "-odqs";
        if (queue_remote) argv[i++] = "-odqr";
        if (batch_max >= 0)
          argv[i++] = string_sprintf("-oB%d", batch_max);
        if (config_changed)
          {
          argv[i++] = "-C";
          argv[i++] = config_filename;
          }
        argv[i++] = "-Mc";
        argv[i++] = message_id;
        argv[i++] = NULL;

        DEBUG(4)
          {
          while (i < 10) argv[i++] = NULL;
          debug_printf("fork %s %s %s %s %s %s %s %s %s %s\n",
            argv[0], argv[1], argv[2],
            (argv[3] == NULL)? "" : argv[3],
            (argv[4] == NULL)? "" : argv[4],
            (argv[5] == NULL)? "" : argv[5],
            (argv[6] == NULL)? "" : argv[6],
            (argv[7] == NULL)? "" : argv[7],
            (argv[8] == NULL)? "" : argv[8],
            (argv[9] == NULL)? "" : argv[9]);
          }

        execv(argv[0], argv);
        _exit(errno);
        }

      /* In the parent, wait if synchronous delivery is required. */

      if (synchronous_delivery && pid > 0)
        {
        int status;
        while (wait(&status) != pid);
        }
      }
    }

  /* The loop will repeat if more is TRUE. */
  }

return EXIT_SUCCESS;
}

/* End of exim.c */
