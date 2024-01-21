/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Functions concerned with running Exim as a daemon */


#include "exim.h"


/*************************************************
*          Host and net setup tables             *
*************************************************/

/* Setup work for lists of hosts and nets is done once at the start of
the daemon, to save doing it for every call. These list the options that
are so processed. */

typedef struct {
  char **text;
  host_item **data;
} host_setup;

typedef struct {
  char **text;
  ip_net_item **data;
} net_setup;

host_setup host_setup_list[] = {
  { &sender_verify_except_hosts, &sender_verify_except_hostlist },
  { &rfc1413_except_hosts,       &rfc1413_except_hostlist },
  { &sender_host_accept,         &sender_host_accept_hosts },
  { &sender_host_accept_relay,   &sender_host_accept_relay_hosts },
  { &sender_host_reject,         &sender_host_reject_hosts },
  { &sender_host_reject_recipients, &sender_host_reject_recipients_hosts },
  { &sender_host_reject_relay,   &sender_host_reject_relay_hosts },
  { &smtp_etrn_hosts,            &smtp_etrn_hostlist },
  { &smtp_reserve_hosts,         &smtp_reserve_hostlist },
  { &receiver_unqualified_hosts, &receiver_unqualified_hostlist },
  { &receiver_verify_except_hosts, &receiver_verify_except_hostlist },
  { &sender_unqualified_hosts,   &sender_unqualified_hostlist },
  { NULL, NULL}
};

net_setup net_setup_list[] = {
  { &helo_verify_nets,           &helo_verify_netlist },
  { &sender_verify_except_nets,  &sender_verify_except_netlist },
  { &rfc1413_except_nets,        &rfc1413_except_netlist },
  { &sender_net_accept,          &sender_net_accept_nets },
  { &sender_net_accept_relay,    &sender_net_accept_relay_nets },
  { &sender_net_reject,          &sender_net_reject_nets },
  { &sender_net_reject_recipients, &sender_net_reject_recipients_nets },
  { &sender_net_reject_relay,    &sender_net_reject_relay_nets },
  { &smtp_etrn_nets,             &smtp_etrn_netlist },
  { &smtp_reserve_nets,          &smtp_reserve_netlist },
  { &receiver_unqualified_nets,  &receiver_unqualified_netlist },
  { &receiver_verify_except_nets, &receiver_verify_except_netlist },
  { &sender_unqualified_nets,    &sender_unqualified_netlist },
  { NULL, NULL}
};



/*************************************************
*               Local static variables           *
*************************************************/

static volatile BOOL sigalrm_seen;
static volatile BOOL sigchld_seen;
static volatile BOOL sighup_seen;

static int   accept_retry_count = 0;
static int   accept_retry_errno;
static BOOL  accept_retry_select_failed;

static int   queue_run_count = 0;
static int   queue_pid_slotcount;
static pid_t *queue_pid_slots;

static int   smtp_pid_slotcount;
static pid_t *smtp_pid_slots;




/*************************************************
*             SIGALRM Handler                    *
*************************************************/

/* All this handler does is to set a flag and re-enable the signal.

Argument: the signal number
Returns:  nothing
*/

static void
sigalrm_handler(int sig)
{
sigalrm_seen = TRUE;
signal(SIGALRM, sigalrm_handler);
}



/*************************************************
*             SIGHUP Handler                     *
*************************************************/

/* All this handler does is to set a flag and re-enable the signal.

Argument: the signal number
Returns:  nothing
*/

static void
sighup_handler(int sig)
{
sighup_seen = TRUE;
signal(SIGHUP, sighup_handler);
}



/*************************************************
*     SIGCHLD handler for main daemon process    *
*************************************************/

/* Don't re-enable the handler here, since we aren't doing the
waiting here. If the signal is re-enabled, there will just be an
infinite sequence of calls to this handler. The SIGCHLD signal is
used just as a means of waking up the daemon so that it notices
terminated subprocesses as soon as possible.

Argument: the signal number
Returns:  nothing
*/

static void
main_sigchld_handler(int sig)
{
sigchld_seen = TRUE;
signal(SIGCHLD, SIG_DFL);
}




/*************************************************
*            Handle a connected SMTP call        *
*************************************************/

/* This function is called when an SMTP connection has been accepted.
If there are too many, give an error message and close down. Otherwise
spin off a sub-process to handle the call. The listening socket is
required so that it can be closed in the sub-process.

Arguments:
  listen_socket     socket which is listening for incoming calls
  accept_socket     socket of the current accepted call
  accepted          socket information about the current call

Returns:            nothing
*/

static void
handle_smtp_call(int listen_socket, int accept_socket,
  struct sockaddr_in *accepted)
{
FILE *in;
FILE *out = fdopen(accept_socket, "w");
pid_t pid;
int dup_accept_socket = dup(accept_socket);

DEBUG(1) debug_printf("Connection request from [%s]\n",
  inet_ntoa(accepted->sin_addr));

/* Check that duplication of the socket worked, and if so, make
an input file. */

if (dup_accept_socket < 0)
  {
  DEBUG(1) debug_printf("Couldn't dup socket descriptor\n");
  DEBUG(1) debug_printf("421 %s: Connection refused: %s\n", primary_hostname,
    strerror(errno));
  fprintf(out, "421 %s: Connection refused: %s\r\n", primary_hostname,
    strerror(errno));
  fclose(out);
  return;
  }

in = fdopen(dup_accept_socket, "r");

/* Check maximum number of connections. We do not check for reserved
connections or unacceptable hosts here. That is done in the subprocess because
it might take some time. */

if (smtp_accept_max > 0 && smtp_accept_count >= smtp_accept_max)
  {
  DEBUG(1) debug_printf("rejecting SMTP connection: count=%d max=%d\n",
    smtp_accept_count, smtp_accept_max);
  DEBUG(1) debug_printf("421 %s: Too many concurrent SMTP connections; "
    "please try again later.\n", primary_hostname);
  fprintf(out, "421 %s: Too many concurrent SMTP connections; "
    "please try again later.\r\n", primary_hostname);
  fclose(out);
  fclose(in);
  log_write(4, LOG_MAIN, "Connection from [%s] refused: too many connections",
    inet_ntoa(accepted->sin_addr));
  return;
  }

/* If a load limit above which only reserved hosts are acceptable is defined,
get the load average here, and if there are in fact no reserved hosts, do
the test right away (saves a fork). If there are hosts, do the check in the
subprocess because it might take time. */

if (smtp_load_reserve >= 0)
  {
  load_average = os_getloadavg();
  if (smtp_reserve_hosts == NULL && smtp_reserve_nets == NULL &&
      load_average > smtp_load_reserve)
    {
    DEBUG(1) debug_printf("rejecting SMTP connection: load average = %.2f\n",
      (double)load_average/1000.0);
    DEBUG(1) debug_printf("421 %s: Too much load; please try again later.\n",
      primary_hostname);
    fprintf(out, "421 %s: Too much load; please try again later.\r\n",
      primary_hostname);
    fclose(out);
    fclose(in);
    log_write(4, LOG_MAIN, "Connection from [%s] refused: load average = %.2f",
      inet_ntoa(accepted->sin_addr), (double)load_average/1000.0);
    return;
    }
  }

/* Now fork the accepting process */

pid = fork();

/* Handle the child process */

if (pid == 0)
  {
  BOOL local_queue_only = queue_only;
  BOOL smtp_first = TRUE;

  /* Close the listening socket, and set the SIGCHLD handler to SIG_IGN.
  This is not the same as SIG_DFL, despite the fact that documentation often
  lists the default as "ignore". At least on some systems, setting SIG_IGN
  causes child processes that complete simply to go away without ever becoming
  defunct. You can't therefore wait for them - but in this process we don't
  want to wait for them as they are doing independent deliveries. */

  close(listen_socket);
  signal(SIGCHLD, SIG_IGN);

  /* Make the address available in ASCII representation, and also
  set it up as a default sender_fullhost, for cases when HELO is
  not present. */

  sender_host_address = string_copy(inet_ntoa(accepted->sin_addr));
  sender_fullhost = string_sprintf("[%s]", sender_host_address);

  /* Attempt to get an id from the sending machine via the RFC 1413
  protocol. We do this in the sub-process in order not to hold up the
  main process if there is any delay. */

  verify_get_ident(accept_socket);

  DEBUG(1)
    {
    debug_printf("Process %d is handling incoming connection from [%s]",
      getpid(), sender_host_address);
    if (sender_ident != NULL) debug_printf(" (%s)\n", sender_ident);
      else debug_printf("\n");
    }

  /* If there are too many child processes for immediate delivery,
  set the local_queue_only flag, which is initialized from the
  configured value and may therefore already be TRUE. */

  if (smtp_accept_queue > 0 && smtp_accept_count >= smtp_accept_queue)
    {
    DEBUG(1) debug_printf(
      "Too many connections (%d) for immediate delivery (max %d)\n",
        smtp_accept_count, smtp_accept_queue);
    local_queue_only = TRUE;
    }

  /* Now loop, accepting incoming messages from the SMTP connection. The
  end will come at the QUIT command, when accept_msg() returns FALSE.
  A break in the connection causes the process to die (see accept.c). */

  sender_local = sender_host_unknown = FALSE;
  smtp_input = TRUE;

  for (;;)
    {
    int rc;
    message_id[0] = 0;   /* Clear out any previous message_id */

    set_process_info("handling incoming connection from [%s]%s%s%s",
      sender_host_address,
      (sender_ident == NULL)? "" : " (",
      (sender_ident == NULL)? "" : sender_ident,
      (sender_ident == NULL)? "" : ")");

    DEBUG(1) debug_printf("ready for new message\n");

    /* Smtp_setup_msg() returns 0 on QUIT or if the call is from an
    unacceptable host, -1 on connection lost, and +1 on validly reaching
    DATA. Accept_msg() always returns TRUE when smtp_input is true; just
    retry if no message was accepted (can happen for invalid message
    parameters). */

    if ((rc = smtp_setup_msg(in, out, smtp_first)) > 0)
      {
      smtp_first = FALSE;
      (void) accept_msg(in, out, FALSE);
      if (message_id[0] == 0) continue;
      }
    else exit((rc == 0)? EXIT_SUCCESS : EXIT_FAILURE);

    /* Show the recipients when debugging */

    DEBUG(2)
      {
      int i;
      if (sender_address != NULL)
        debug_printf("Sender: %s\n", sender_address);
      if (recipients_list != NULL)
        {
        debug_printf("Recipients:\n");
        for (i = 0; i < recipients_count; i++)
          debug_printf("  %s\n", recipients_list[i]);
        }
      }

    /* A message has been accepted. Clean up any previous delivery processes
    that have now completed and are defunct. I tried to get SIGCHLD to do
    this, but it gave problems. This way, at most one delivery process hangs
    around until the next message is received on systems where setting SIGCHLD
    to SIG_IGN does not have the effect of causing them to go away by
    themselves. (I don't know if there are any such systems, but leaving this
    code here can do no harm.) */

    while ((pid = waitpid(-1, NULL, WNOHANG)) > 0)
      DEBUG(1) debug_printf("child %d ended\n", pid);

    /* Tidy up the store used in accepting this message */

    store_free(sender_address);
    accept_free_recipients();

    /* If queue_only is set or if there are too many incoming connections in
    existence, local_queue_only will be TRUE. If it is not, and queue_only_load
    is set, check that the load average is below it. If it is not, set
    local_queue_only TRUE. Note that it then remains this way for any
    subsequent messages on the same SMTP connection. This is a deliberate
    choice; even though the load average may fall, it doesn't seem right to
    deliver later messages on the same call when not delivering earlier ones. */

    if (!local_queue_only && queue_only_load >= 0)
      {
      local_queue_only = (load_average = os_getloadavg()) > queue_only_load;
      DEBUG(1)
        if (local_queue_only)
          debug_printf("no delivery: load average = %.2f\n",
            (double)load_average/1000.0);
      }

    /* If a delivery attempt is required, spin off a new process to handle it.
    If we are not root, we have to re-exec exim unless root can be regained by
    the use of seteuid. */

    if (!local_queue_only && (pid = fork()) == 0)
      {
      fclose(in);
      fclose(out);
      if (security_level >= 2)
        {
        int i = 0;
        char *argv[8];
        argv[i++] = exim_path;
        if (debug_level > 0)
          argv[i++] = string_sprintf("-d%d", debug_level);
        if (dont_deliver) argv[i++] = "-N";
        if (config_changed)
          {
          argv[i++] = "-C";
          argv[i++] = config_filename;
          }
        argv[i++] = "-Mc";
        argv[i++] = message_id;
        argv[i++] = (char *)0;
        execv(argv[0], argv);
        log_write(0, LOG_PANIC_DIE, "exec of exim -Mc failed");
        }
      /* No need to re-exec - put the SIGCHLD signal back to its default */
      signal(SIGCHLD, SIG_DFL);
      (void)deliver_message(message_id, FALSE, FALSE, FALSE);
      _exit(EXIT_SUCCESS);
      }

    if (pid > 0) DEBUG(1) debug_printf("forked delivery process %d\n", pid);
    }
  }


/* Carrying on in the parent daemon process... Can't do much if the fork
failed. Otherwise, keep count of the number of accepting processes and
remember the pid for ticking off when the child completes. */

if (pid < 0)
  {
  DEBUG(1) debug_printf("fork of SMTP accept process failed\n");
  fprintf(out, "421: %s Connection refused: %s\r\n", primary_hostname,
    strerror(errno));
  }
else
  {
  int i;
  for (i = 0; i < smtp_pid_slotcount; ++i)
    {
    if (smtp_pid_slots[i] <= 0)
      {
      smtp_pid_slots[i] = pid;
      smtp_accept_count++;
      break;
      }
    }
  DEBUG(2) debug_printf("%d SMTP accept process%s running\n",
    smtp_accept_count, (smtp_accept_count == 1)? "" : "es");
  }

/* Close the streams associated with the socket which will also
close the socket fds in the parent process. */

fclose(in);
fclose(out);
}




/*************************************************
*              Exim Daemon Mainline              *
*************************************************/

/* The daemon can do two jobs, either of which is optional:

   (1) Listens for incoming SMTP calls and spawns off a sub-process to handle
   each one. This is requested by the -bd option, with -oX specifying the SMTP
   port on which to listen (for testing).

   (2) Spawns a queue-running process every so often. This is controlled by the
   -q option with a an interval time. (If no time is given, a single queue run
   is done from the main function, and control doesn't get here.)

Root privilege is required in order to attach to port 25. Some systems require
it when calling socket() rather than bind(). To cope with all cases, we run as
root for both socket() and bind(). When a listener is required, this function
is entered as root for security levels 0 or 2; otherwise seteuid to exim.

Once the socket is bound, root privilege is given up if there is an exim uid,
either by seteuid for security level 1, or by setuid for higher levels. In the
latter case, a re-exec is subsequently required to cause a received message to
be delivered.

When listening is not required - i.e. the daemon is simply a means of
periodically starting a queue runner - this function is entered with the
appropriate uid already set. There are no arguments, and it never returns. */

void
daemon_go(void)
{
int *listen_sockets = NULL;
int listen_socket_count = 0;
u_short net_port;
FILE *f;
char buff[256];

/* Create the name of the file holding the daemon's pid. This is written just
to make it easier to find the daemon process. There's nothing to stop multiple
daemons running, as long as no more than one listens on a given TCP/IP port on
the same interface(s). We put a non-standard port number in the file name to
distinguish running/testing versions, and use different names for non-listening
and non-queue-running daemons, but if someone runs two similar daemons
simultaneously, then only the last started will get its pid written. */

if (smtp_port < 0)
  {
  if (pid_file_path[0] == 0)
    sprintf(buff, "%s/exim-daemon.pid", spool_directory);
  else
    sprintf(buff, pid_file_path, "");
  }
else
  {
  if (pid_file_path[0] == 0)
    sprintf(buff, "%s/exim-daemon.%d.pid", spool_directory, smtp_port);
  else
    {
    char dbuff[12];
    sprintf(dbuff, ".%d", smtp_port);
    sprintf(buff, pid_file_path, dbuff);
    }
  }

/* For daemons that aren't both listening and queue-running, add the option
that they are doing to the file name. */

if (!daemon_listen)
  sprintf(buff+(int)strlen(buff), "-q%s", readconf_printtime(queue_interval));
else if (queue_interval <= 0) strcat(buff, "-bd");

/* Close all open file descriptors and disconnect from the controlling
terminal, if we are not debugging. Most modern Unixes seem to have setsid() for
getting rid of the controlling terminal. For any OS that doesn't, setsid() can
be #defined as a no-op, or as something else. */

if (debug_level == 0 && !debug_trace_memory)
  {
  pid_t pid;
  int fd;

  /* Ensure cached files for searches and expands, which might have been
  opened while reading the configuration, are closed before doing a general
  close on all file descriptors. */

  search_tidyup();
  for (fd = mac_maxfd; fd >= 0; fd--) close(fd);

  /* Fork, in case current process is a process group leader (see 'man
  setsid' for an explanation). */

  pid = fork();
  if (pid < 0) log_write(0, LOG_PANIC_DIE, "fork() failed: %s",
    strerror(errno));
  if (pid > 0) exit(EXIT_SUCCESS);      /* in parent process, just exit */

  /* Release controlling terminal */

  (void)setsid();
  }

/* If SMTP listening is requested, set up a socket on the SMTP port or
a given port, and compile the verification acceptance data, if any, so
that it is available to all spawned processes. If local_interfaces is set,
we have to set up one or more sockets on specific IP addresses; otherwise
se listen on all addresses on a single socket. */

if (daemon_listen)
  {
  int sk;
  int on = 1;
  ip_address_item *addresses, *ipa;
  struct sockaddr_in sin;
  host_setup *hset;
  net_setup *nset;

  /* If there are lists of nets and hosts + idents for various checks,
  pre-process them now so that this work is not repeated for each message. For
  a sender_{accept,reject} list, just do a match that won't work so as to get
  any regular expressions compiled. */

  if (sender_accept != NULL)
    (void)match_isinlist("@", sender_accept, &re_sender_accept, FALSE);

  if (sender_accept_recipients != NULL)
    (void)match_isinlist("@", sender_accept_recipients,
      &re_sender_accept_recipients, FALSE);

  if (sender_reject != NULL)
    (void)match_isinlist("@", sender_reject, &re_sender_reject, FALSE);

  if (sender_reject_recipients != NULL)
    (void)match_isinlist("@", sender_reject_recipients,
      &re_sender_reject_recipients, FALSE);

  for (hset = host_setup_list; hset->text != NULL; hset++)
    if (*(hset->text) != NULL)
      verify_setup_hostlist(*(hset->text), hset->data);

  for (nset = net_setup_list; nset->text != NULL; nset++)
    if (*(nset->text) != NULL)
      verify_setup_netlist(*(nset->text), nset->data);

  /* Find the standard SMTP port if no port number given; otherwise
  convert the given port to network order. */

  if (smtp_port < 0)
    {
    struct servent *smtp_service;
    if ((smtp_service = getservbyname("smtp", "tcp")) == NULL)
      log_write(0, LOG_PANIC_DIE, "cannot find smtp/tcp service");
    net_port = smtp_service->s_port;
    smtp_port = ntohs(net_port);
    }
  else net_port = htons(smtp_port);

  /* If local_interfaces is set, create the list of local interfaces and
  set up a vector for holding the listening sockets. */

  if (local_interfaces != NULL)
    {
    addresses = local_interface_data = host_find_interfaces();
    for (ipa = addresses; ipa != NULL; ipa = ipa->next)
      listen_socket_count++;
    }

  /* Otherwise set up one address item with a null address, implying listening
  on all interfaces. */

  else
    {
    addresses = store_malloc(sizeof(ip_address_item));
    addresses->next = NULL;
    addresses->address[0] = 0;
    listen_socket_count = 1;
    }

  /* Get a vector to remember all the sockets in */

  listen_sockets = store_malloc(sizeof(int *) * listen_socket_count);

  /* Ensure root privilege. It will only not exist at this stage if seteuid
  can be used to regain it. */

  if (geteuid() != root_uid) mac_seteuid(root_uid);

  /* If any option requiring a load average to be available during the
  reception of a message is set, call os_getloadavg() while we are root
  for those OS for which this is necessary the first time it is called (in
  order to perform an "open" on the kernel memory file). */

  #ifdef LOAD_AVG_NEEDS_ROOT
  if (queue_only_load >= 0 || smtp_load_reserve >= 0) (void)os_getloadavg();
  #endif

  /* For each IP address, create a socket and bind it to the appropriate
  port. Note that if local_interfaces is not set, we bind a single socket to
  all interfaces. */

  for (ipa = addresses, sk = 0; sk < listen_socket_count; ipa = ipa->next, sk++)
    {
    int i;
    listen_sockets[sk] = socket(AF_INET, SOCK_STREAM, 0);
    if (listen_sockets[sk] < 0)
      log_write(0, LOG_PANIC_DIE, "socket creation failed: %s", strerror(errno));

    /* Set SO_REUSEADDR so that the daemon can be restarted while a connection
    is being handled.  Without this, a connection will prevent reuse of the
    smtp port for listening. */

    if (setsockopt(listen_sockets[sk], SOL_SOCKET, SO_REUSEADDR, (char *)(&on),
      sizeof(on)) < 0)
        log_write(0, LOG_PANIC_DIE, "setting SO_REUSEADDR on socket failed: %s",
          strerror(errno));

    /* Now bind the socket to the required port; if Exim is being restarted
    it may not always be possible to bind immediately, even with SO_REUSEADDR
    set, so try 10 times, waiting 30 seconds between each try. */

    memset(&sin, 0, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_port = net_port;

    if (ipa->address[0] == 0)
      sin.sin_addr.s_addr = (S_ADDR_TYPE)INADDR_ANY;
    else
      sin.sin_addr.s_addr = (S_ADDR_TYPE)inet_addr(ipa->address);

    for (i = 9; i >= 0; i--)
      {
      if (bind(listen_sockets[sk], (struct sockaddr *)&sin, sizeof(sin)) < 0)
        {
        if (i == 0)
          {
          char *msg = strerror(errno);
          log_write(0, LOG_MAIN, "socket bind() to port %d failed: %s: "
            "daemon abandoned", smtp_port, msg);
          log_write(0, LOG_PANIC_DIE, "socket bind() to port %d failed: %s",
            smtp_port, msg);
          }
        log_write(0, LOG_MAIN, "socket bind() to port %d failed: %s: waiting "
          "before trying again", smtp_port, strerror(errno));
        sleep(30);
        }
      else break;
      }
    }

  /* If exim_uid is set, give up root privilege at this point, using setuid or
  seteuid as appopriate. The macros expand to -1 on systems that don't have
  the sete{g,u}id functions, but the security level cannot be set to values
  implying the use of these functions on such systems. */

  if (exim_uid_set)
    {
    if (security_level >= 2)
      {
      setgid(exim_gid);
      setuid(exim_uid);
      }
    else
      {
      mac_setegid(exim_gid);
      mac_seteuid(exim_uid);
      }
    }

  /* Do a sanity check on the max connects value; zero means no limit,
  in which case we don't need to keep a list of them. */

  if (smtp_accept_max < 0 || smtp_accept_max > 4095) smtp_accept_max = 0;

  /* Do a sanity check on the "max connects until we queue only" (no delivery)
  value. Again, zero means no limit, and there's no point setting it unless it
  is less than the max connects limit. */

  if (smtp_accept_queue < 0 || smtp_accept_queue > 4095) smtp_accept_queue = 0;
  if (smtp_accept_max > 0 && smtp_accept_queue > smtp_accept_max)
    smtp_accept_queue = 0;

  /* Get somewhere to keep the list of SMTP accepting pids if we are keeping
  track of them, either for total number, or for queue-only handling. */

  smtp_pid_slotcount = (smtp_accept_queue > smtp_accept_max)?
    smtp_accept_queue : smtp_accept_max;

  if (smtp_pid_slotcount > 0)
    {
    int i;
    smtp_pid_slots = store_malloc(smtp_pid_slotcount * sizeof(pid_t));
    for (i = 0; i < smtp_pid_slotcount; i++) smtp_pid_slots[i] = 0;
    }

  /* Start listening on the bound sockets, establishing the maximum backlog of
  connections that is allowed. */

  for (sk = 0; sk < listen_socket_count; sk++)
    listen(listen_sockets[sk], smtp_connect_backlog);
  }


/* Get somewhere to keep the list of queue-runner pids if we are keeping track
of them (and also if we are doing queue runs). */

if (queue_interval > 0)
  {
  queue_pid_slotcount = queue_run_max;
  if (queue_pid_slotcount > 0)
    {
    int i;
    queue_pid_slots = store_malloc(queue_pid_slotcount * sizeof(pid_t));
    for (i = 0; i < queue_pid_slotcount; i++) queue_pid_slots[i] = 0;
    }
  }

/* Set up the handler for termination of child processes. */

sigchld_seen = FALSE;
signal(SIGCHLD, main_sigchld_handler);

/* Set up the handler for SIGHUP, which causes a restart of the daemon. */

sighup_seen = FALSE;
signal(SIGHUP, sighup_handler);

/* If we are to run the queue periodically, pretend the alarm has just
gone off. This will cause the first queue-runner to get kicked off straight
away, and the alarm to be reset. */

sigalrm_seen = (queue_interval > 0);


/* Log the start up of a daemon. */

if (queue_interval > 0)
  {
  if (daemon_listen)
    {
    log_write(0, LOG_MAIN,
      "exim %s daemon started: pid=%d, -q%s, listening for SMTP on port %d",
      version_string, getpid(), readconf_printtime(queue_interval), smtp_port);
    set_process_info("daemon: -q%s, listening on port %d",
      readconf_printtime(queue_interval), smtp_port);
    }
  else
    {
    log_write(0, LOG_MAIN,
      "exim %s daemon started: pid=%d, -q%s, not listening for SMTP",
      version_string, getpid(), readconf_printtime(queue_interval));
    set_process_info("daemon: -q%s, not listening",
      readconf_printtime(queue_interval));
    }
  }
else
  {
  log_write(0, LOG_MAIN,
    "exim %s daemon started: pid=%d, no queue runs, listening for SMTP on port %d",
     version_string, getpid(), smtp_port);
  set_process_info("daemon: no queue runs, port %d", smtp_port);
  }

/* Write the pid to a known file for assistance in identification. Make it
read-only. */

f = fopen(buff, "w");
if (f != NULL)
  {
  fprintf(f, "%d\n", (int)getpid());
  fchmod(fileno(f), 0644);
  fclose(f);
  DEBUG(2) debug_printf("pid written to %s\n", buff);
  }
else DEBUG(2) debug_printf("failed to open pid file %s: %s\n", buff,
  strerror(errno));

/* Close the log so it can be renamed and moved. This process doesn't write
to the log again, unless it is about to die or exec and in the latter case
it closes the log first. */

log_close();

DEBUG(2) debug_printf("daemon running with uid=%d gid=%d euid=%d egid=%d\n",
  (int)getuid(), (int)getgid(), (int)geteuid(), (int)getegid());

/* Enter the never-ending loop... */

for (;;)
  {
  struct sockaddr_in accepted;
  int len = sizeof(accepted);
  pid_t pid;

  /* This code is placed first in the loop, so that it gets obeyed at the
  start, before the first wait. This causes the first queue-runner to be
  started immediately. */

  if (sigalrm_seen)
    {
    DEBUG(9) debug_printf("SIGALRM received\n");

    /* Do a full queue run in a child process, if required, unless we already
    have enough queue runners on the go. If we are not running as root, a
    re-exec is required. */

    if (queue_interval > 0 &&
       (queue_run_max <= 0 || queue_run_count < queue_run_max))
      {
      if ((pid = fork()) == 0)
        {
        int sk;
        DEBUG(1) debug_printf("Starting queue-runner: pid %d\n", getpid());

        /* Close any open listening sockets in the child */

        for (sk = 0; sk < listen_socket_count; sk++) close(listen_sockets[sk]);

        /* Reset signals in the child */

        signal(SIGALRM, SIG_DFL);
        signal(SIGHUP,  SIG_DFL);
        signal(SIGCHLD, SIG_DFL);

        /* Re-exec if privilege has been given up */

        if (geteuid() != root_uid)
          {
          int i = 0;
          char *argv[5];
          argv[i++] = exim_path;
          if (config_changed)
            {
            argv[i++] = "-C";
            argv[i++] = config_filename;
            }
          argv[i++] = queue_run_force?
            (queue_run_local? "-qfl" : "-qf") :
            (queue_run_local? "-ql" : "-q");
          argv[i++] = (char *)0;
          execv(argv[0], argv);
          log_write(0, LOG_PANIC_DIE, "exec of exim -q failed");
          }

        /* No need to re-exec */

        queue_run(NULL, NULL);
        _exit(EXIT_SUCCESS);
        }

      if (pid < 0)
        {
        log_write(0, LOG_PANIC, "fork of queue-runner process failed");
        }
      else
        {
        int i;
        for (i = 0; i < queue_pid_slotcount; ++i)
          {
          if (queue_pid_slots[i] <= 0)
            {
            queue_pid_slots[i] = pid;
            queue_run_count++;
            break;
            }
          }
        DEBUG(2) debug_printf("%d queue-runner process%s running\n",
          queue_run_count, (queue_run_count == 1)? "" : "es");
        }
      }

    /* Reset the alarm time */

    sigalrm_seen = FALSE;
    signal(SIGALRM, sigalrm_handler);
    alarm(queue_interval);
    }


  /* Sleep till a connection happens if listening, and handle the connection if
  that is why we woke up. The FreeBSD operating system requires the use of
  select() before accept() because the latter function is not interrupted by
  a signal, and we want to wake up for SIGCHLD and SIGLARM signals. Some other
  OS do notice signals in accept() but it does no harm to have the select()
  in for all of them - and it won't then be a lurking problem for ports to
  new OS. In fact, the later addition of listening on specific interfaces only
  requires this way of working anyway. */

  if (daemon_listen)
    {
    int sk, lcount;
    int max_socket = 0;
    BOOL select_failed = FALSE;
    fd_set select_listen;

    FD_ZERO(&select_listen);
    for (sk = 0; sk < listen_socket_count; sk++)
      {
      FD_SET(listen_sockets[sk], &select_listen);
      if (listen_sockets[sk] > max_socket) max_socket = listen_sockets[sk];
      }

    DEBUG(2) debug_printf("listening on port %d...\n", smtp_port);

    if ((lcount = select(max_socket + 1, (SELECT_ARG2_TYPE *)&select_listen,
         NULL, NULL, NULL)) < 0)
      {
      select_failed = TRUE;
      lcount = 1;
      }

    /* Loop for all the sockets that are currently ready to go. If select
    actually failed, we have set the count to 1 and a flag, so as to use the
    common error code for select/accept below. */

    while (lcount-- > 0)
      {
      int accept_socket = -1;
      if (!select_failed)
        {
        for (sk = 0; sk < listen_socket_count; sk++)
          {
          if (FD_ISSET(listen_sockets[sk], &select_listen))
            {
            accept_socket = accept(listen_sockets[sk],
              (struct sockaddr *)&accepted, &len);
            FD_CLR(listen_sockets[sk], &select_listen);
            break;
            }
          }
        }

      /* If select or accept has failed and this was not caused by an
      interruption, log the incident and try again. With asymmetric TCP/IP
      routing errors such as "No route to network" have been seen here. Also
      "connection reset by peer" has been seen. These cannot be classed as
      disastrous errors, but they could fill up a lot of log. The code in smail
      crashes the daemon after 10 successive failures of accept, on the grounds
      that some OS fail continuously. Exim originally followed suit, but this
      appears to have caused problems. Now it just keeps going, but instead of
      logging each error, it batches them up when they are continuous. */

      if (accept_socket < 0 && errno != EINTR)
        {
        if (accept_retry_count == 0)
          {
          accept_retry_errno = errno;
          accept_retry_select_failed = select_failed;
          }
        else
          {
          if (errno != accept_retry_errno ||
              select_failed != accept_retry_select_failed ||
              accept_retry_count >= 50)
            {
            log_write(0, LOG_MAIN | ((accept_retry_count >= 50)? LOG_PANIC : 0),
              "%d %s() failure%s: %s",
              accept_retry_count,
              accept_retry_select_failed? "select" : "accept",
              (accept_retry_count == 1)? "" : "s",
              strerror(accept_retry_errno));
            accept_retry_count = 0;
            accept_retry_errno = errno;
            accept_retry_select_failed = select_failed;
            }
          }
        accept_retry_count++;
        }

      else
        {
        if (accept_retry_count > 0)
          {
          log_write(0, LOG_MAIN, "%d %s() failure%s: %s",
            accept_retry_count,
            accept_retry_select_failed? "select" : "accept",
            (accept_retry_count == 1)? "" : "s",
            strerror(accept_retry_errno));
          accept_retry_count = 0;
          }
        }

      /* If select/accept succeeded, deal with the connection. */

      if (accept_socket >= 0)
        handle_smtp_call(listen_sockets[sk], accept_socket, &accepted);
      }
    }

  /* If not listening, then just sleep for the queue interval. If we woke
  up early the last time for some other signal, it won't matter because
  the alarm signal will wake at the right time. This code originally used
  sleep() but it turns out that on the FreeBSD system, sleep() is not inter-
  rupted by signals, so it wasn't waking up for SIGALRM or SIGCHLD. Luckily
  select() can be used as an interruptible sleep() on all versions of Unix. */

  else
    {
    struct timeval tv;
    tv.tv_sec = queue_interval;
    tv.tv_usec = 0;
    select(0, NULL, NULL, NULL, &tv);
    }

  /* Handle the termination of a child process. Theoretically, this need
  be done only when sigchld_seen is TRUE, but rumour has it that some systems
  lose SIGCHLD signals at busy times, so to be on the safe side, just
  do it each time round. It shouldn't be too expensive. */

  while ((pid = waitpid(-1, NULL, WNOHANG)) > 0)
    {
    int i;
    DEBUG(2) debug_printf("child %d ended\n", pid);

    /* Deal with an accepting process. */

    for (i = 0; i < smtp_pid_slotcount; ++i)
      {
      if (smtp_pid_slots[i] == pid)
        {
        smtp_pid_slots[i] = 0;
        if (--smtp_accept_count < 0) smtp_accept_count = 0;
        DEBUG(2) debug_printf("%d SMTP accept process%s now running\n",
          smtp_accept_count, (smtp_accept_count == 1)? "" : "es");
        break;
        }
      }

    /* Deal with a queue-runner process. */

    for (i = 0; i < queue_pid_slotcount; ++i)
      {
      if (queue_pid_slots[i] == pid)
        {
        queue_pid_slots[i] = 0;
        if (--queue_run_count < 0) queue_run_count = 0;
        DEBUG(2) debug_printf("%d queue-runner process%s now running\n",
          queue_run_count, (queue_run_count == 1)? "" : "es");
        break;
        }
      }
    }

  /* Re-enable the SIGCHLD handler if it has been run. It can't do it
  for itself, because it isn't doing the waiting itself. */

  if (sigchld_seen)
    {
    sigchld_seen = FALSE;
    signal(SIGCHLD, main_sigchld_handler);
    }

  /* Handle being woken by SIGHUP. We know at this point that the result
  of accept() has been dealt with, so we can re-exec exim safely, first
  closing the listening socket so that it can be reused. */

  if (sighup_seen)
    {
    int sk;
    log_write(0, LOG_MAIN, "pid %d: SIGHUP received: re-exec daemon",
      getpid());
    log_close();
    for (sk = 0; sk < listen_socket_count; sk++) close(listen_sockets[sk]);
    sighup_argv[0] = exim_path;
    execv(exim_path, sighup_argv);
    log_write(0, LOG_MAIN|LOG_PANIC_DIE, "pid %d: exec of %s failed", getpid());
    }

  }   /* End of main loop */

/* Control never reaches here */
}

/* End of exim_daemon.c */

