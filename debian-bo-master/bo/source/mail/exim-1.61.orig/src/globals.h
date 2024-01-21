/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* All the global variables are defined together in this one header, so that
they are easy to find. First put any specials that are required for some
operating systems. */

#ifdef  NEED_H_ERRNO
extern int h_errno;
#endif

extern BOOL   accept_8bitmime;        /* Allow *BITMIME incoming */
extern int    accept_timeout;         /* For non-SMTP acceptance */
extern char  *address_file_transport; /* Name of transport for file addresses */
extern char  *address_pipe_transport; /* Name of transport for pipe addresses */
extern char  *address_reply_transport; /* Name of transport for reply addresses */
extern BOOL   address_test_mode;      /* True for -bt */
extern BOOL   admin_user;             /* True if caller can do admin */
extern BOOL   always_bcc;             /* Use bcc in all adding cases */
extern int    auto_thaw;              /* Auto-thaw interval */

extern int    batch_max;              /* Max SMTP batching from -oB */
extern char  *bi_command;             /* Command for -bi option */
extern char  *big_buffer;             /* Used for various temp things */
extern int    big_buffer_size;        /* Current size (can expand */

extern BOOL   check_relay;            /* True if relay checking required */
extern BOOL   collapse_source_routes; /* True if collapsing required */
extern BOOL   config_changed;         /* True if -C used */
extern FILE  *config_file;            /* Configuration file */
extern char  *config_filename;        /* Configuration file name */
extern int    config_lineno;          /* Line number in config file */
extern char  *continue_hostname;      /* Host for continued delivery */
extern int    continue_sequence;      /* Sequence num for continued delivery */
extern char  *continue_transport;     /* Transport for continued delivery */

extern BOOL   daemon_listen;          /* True if listening required */
extern off_t  data_start_offset;      /* Offset after message id in data file */
extern FILE  *debug_file;             /* Where to write debugging info */
extern int    debug_level;            /* Just what its name implies */
extern pid_t  debug_pid;              /* Pid to add to debug output */
extern BOOL   debug_trace_memory;     /* For bad problems */
extern transport_info *debug_transport; /* For catching all output */
extern char  *debug_transport_file;   /* Non-NULL if debug transport wanted */
extern int    delay_warning;          /* Time between warnings */
extern BOOL   delivery_date_remove;   /* Remove delivery-date headers */
extern char  *deliver_in_buffer;      /* Buffers for copying file */
extern char  *deliver_out_buffer;
extern int    deliver_datafile;       /* FD for data part of message */
extern char  *deliver_domain;         /* The local domain for delivery */
extern char  *deliver_domain_orig;    /* The original local domain for delivery */
extern BOOL   deliver_force;          /* TRUE if delivery was forced */
extern BOOL   deliver_freeze;         /* TRUE if delivery is frozen */
extern int    deliver_frozen_at;      /* Time of freezing */
extern char  *deliver_home;           /* Home directory for pipes */
extern char  *deliver_host;           /* (First) host for routed local deliveries */
extern int    deliver_load_max;       /* No deliveries if load > this */
extern char  *deliver_localpart;      /* The local part for delivery */
extern char  *deliver_localpart_orig; /* The original local part for delivery */
extern char  *deliver_localpart_prefix; /* The stripped prefix, if any */
extern char  *deliver_localpart_suffix; /* The stripped suffix, if any */
extern BOOL   deliver_manual_thaw;    /* TRUE if manually thawed */
extern address_item *deliver_recipients; /* Current set of addresses */
extern char  *deliver_selectstring;   /* For selecting by string */
extern director_info directors_available[]; /* Vector of available directors */
extern director_instance *directors;  /* Chain of instantiated directors */
extern director_instance director_defaults; /* Default values */
extern int    dns_retrans;            /* Retransmission time setting */
extern int    dns_retry;              /* Number of retries */
extern BOOL   dont_deliver;           /* TRUE for -N option */
extern BOOL   dot_ends;               /* TRUE if "." ends non-SMTP input */

extern int    envelope_to_remove;     /* Remove envelope_to_headers */
extern int    errno_quota;            /* Quota errno in this OS */
extern int    error_handling;         /* Error handling style */
extern char  *errors_address;         /* Normally "postmaster" */
extern char  *errors_copy;            /* For taking copies of errors */
extern char  *errors_reply_to;        /* Reply-to for error messages */
extern gid_t  exim_gid;               /* To be used with exim_uid */
extern BOOL   exim_gid_set;           /* TRUE if exim_gid set */
extern char  *exim_path;              /* Path to exec exim */
extern uid_t  exim_uid;               /* Non-root uid for exim */
extern BOOL   exim_uid_set;           /* TRUE if exim_uid set */
extern int    expand_nlength[];       /* Lengths of numbered strings */
extern int    expand_nmax;            /* Max numerical value */
extern char  *expand_nstring[];       /* Numbered strings */
extern BOOL   expand_string_forcedfail; /* TRUE if failure was "expected" */
extern char  *expand_string_message;  /* Error info for failing expansion */

extern char  *filter_test;            /* Run as a filter tester on this file */
extern int    finduser_retries;       /* Retry count for getpwnam() */
extern BOOL   forbid_domain_literals; /* As it says */
extern BOOL   freeze_tell_mailmaster; /* Message on each freeze */

extern char  *gecos_name;             /* To be expanded when pattern matches */
extern char  *gecos_pattern;          /* Pattern to match */

extern BOOL   have_nis;               /* TRUE if system has NIS */
extern BOOL   have_nisplus;           /* TRUE if system has NIS+ */
extern BOOL   have_seteuid;           /* TRUE if system has seteuid */
extern BOOL   header_changed;         /* TRUE if header data has changed */
extern header_line *header_last;      /* Last header in chain */
extern header_line *header_list;      /* Chain of header lines */
extern header_name *header_names;     /* Adjusted for presence of resent- */
extern header_name header_names_normal[];  /* to one of */
extern header_name header_names_resent[];  /* these */
extern char  *helo_verify_nets;       /* Nets for which HELO is verified */
extern ip_net_item *helo_verify_netlist;
extern char  *hex_digits;             /* Hex digit string for decoding */

extern BOOL   ignore_errmsg_errors;   /* As it says */
extern int    journal_fd;             /* Fd for journal file */

extern int    keep_malformed;         /* Time to keep malformed messages */
extern BOOL   kill_ip_options;        /* For flattening source routing */

extern int    load_average;           /* Most recently read load average */
extern char  *local_domains;          /* List of local domains */
extern BOOL   local_domains_include_host; /* Always include local host */
extern BOOL   local_domains_include_host_literals; /* Plus literals */
extern BOOL   local_error_message;    /* True if handling one of these */
extern char  *local_interfaces;       /* For forcing specific interfaces */
extern ip_address_item *local_interface_data; /* List of local interface addresses */
extern BOOL   locally_caseless;       /* Local local parts are caseless */
extern char  *log_buffer;             /* For constructing log entries */
extern char  *log_file_path;          /* If unset, use default */
extern BOOL   log_ip_options;         /* For logging IP source routing */
extern int    log_level;              /* Level of logging info */
extern BOOL   log_received_recipients; /* As it says */
extern BOOL   log_smtp_confirmation;  /* Log response after "." */
extern BOOL   log_subject;            /* As it says */
extern char  *lookup_key;             /* For query expansion */

extern char  *mailer_argv[];          /* Arg list for running mailer */
extern char  *message_body;           /* Start of message body for filter */
extern int    message_body_visible;   /* Amount visible in message_body */
extern char  *message_filter;         /* Name of system filter file */
extern gid_t  message_filter_gid;     /* Gid for running system filter */
extern BOOL   message_filter_gid_set; /* TRUE if gid set */
extern uid_t  message_filter_uid;     /* Uid for running system filter */
extern BOOL   message_filter_uid_set; /* TRUE if uid set */
extern char   message_id_option[];    /* -E<message-id> for use as option */
extern char  *message_id_external;    /* External form of following */
extern char  *message_id;             /* Internal id of message being handled */
extern char  *message_id_text;        /* Expanded to form message_id */
extern FILE  *message_log;            /* Open message log while delivering */
extern char  *message_precedence;     /* From Precedence: header */
extern int    message_size;           /* Size of message body */
extern int    message_size_limit;     /* As it says */
extern char  *message_reference;      /* Reference for error messages */

extern regexp *net_regexp;            /* For matching networks */
extern uid_t *never_users;            /* List of uids never to be used */
extern gid_t  nobody_gid;             /* Safe, unprivileged, gid */
extern uid_t  nobody_uid;             /* Safe, unprivileged, uid */
extern BOOL   nobody_gid_set;         /* TRUE if nobody_gid set */
extern BOOL   nobody_uid_set;         /* TRUE if nobody_uid set */

extern optionlist optionlist_directors[];  /* These option lists are made */
extern int    optionlist_directors_size;   /* global so that readconf can */
extern optionlist optionlist_routers[];    /* see them for printing out   */
extern int    optionlist_routers_size;     /* the options.                */
extern optionlist optionlist_transports[];
extern int    optionlist_transports_size;

extern gid_t  originator_gid;         /* Gid of whoever wrote spool file */
extern char  *originator_login;       /* Login of ditto */
extern uid_t  originator_uid;         /* Uid of ditto */

extern BOOL   parse_allow_group;      /* Allow group syntax */
extern BOOL   parse_found_group;      /* In the middle of a group */
extern char  *percent_hack_domains;   /* Local domains for which '% operates */
extern char  *pid_file_path;          /* For writing daemon pids */
extern BOOL   preserve_message_logs;  /* Save msglog files */
extern char  *primary_hostname;       /* Primary name of this computer */
extern char   process_info[];         /* For SIGUSR1 output */

extern char  *qualify_domain_recipient; /* Domain to qualify recipients with */
extern char  *qualify_domain_sender;  /* Domain to qualify senders with */
extern BOOL   queue_run_force;        /* TRUE to force during queue run */
extern BOOL   queue_run_local;        /* Local deliveries only in queue run */
extern int    queue_interval;         /* Queue running interval */
extern BOOL   queue_only;             /* TRUE to disable immediate delivery */
extern int    queue_only_load;        /* max load before auto-queue */
extern BOOL   queue_remote;           /* Queue all remote deliveries */
extern int    queue_run_max;          /* Max queue runners */
extern BOOL   queue_smtp;             /* TRUE to disable immediate STMP delivery */

extern re_block *re_local_domains;    /* For keeping compiled regexps */
extern re_block *re_percent_hack_domains;
extern re_block *re_relay_domains;
extern re_block *re_remote_sort;
extern re_block *re_sender_accept;
extern re_block *re_sender_accept_recipients;
extern re_block *re_sender_address_relay;
extern re_block *re_sender_reject;
extern re_block *re_sender_reject_recipients;

extern char **raw_recipients;         /* Before rewriting */
extern gid_t  real_gid;               /* Real gid */
extern uid_t  real_uid;               /* Real user running program */
extern BOOL   really_exim;            /* FALSE in utilities */
extern int    received_count;         /* Count of Received: headers */
extern char  *received_header_text;   /* Definition of Received: header */
extern int    received_headers_max;   /* Max count of Received: headers */
extern char  *received_protocol;      /* Name of incoming protocol */
extern int    received_time;          /* Time the message was received */
extern BOOL   receiver_try_verify;    /* Accept if soft error while verifying */
extern char  *receiver_unqualified_hosts; /* Permitted unqualified receivers */
extern host_item *receiver_unqualified_hostlist;
extern char  *receiver_unqualified_nets;
extern ip_net_item *receiver_unqualified_netlist;
extern BOOL   receiver_verify;        /* Verify receivers when received */
extern char  *receiver_verify_except_hosts; /* Don't verify for these hosts */
extern host_item *receiver_verify_except_hostlist;
extern char  *receiver_verify_except_nets;
extern ip_net_item *receiver_verify_except_netlist;
extern int    recipients_count;       /* Number of recipients */
extern char **recipients_list;        /* List of recipient addresses */
extern int    recipients_list_max;    /* Maximum number fitting in list */
extern int    recipients_max;         /* Max permitted */
extern BOOL   refuse_all_rcpts;       /* Flag for SMTP verifying */
extern BOOL   refuse_ip_options;      /* Refuse source routing */
extern char **regcomp_error_pointer;  /* For use when compiling user re's */
extern char  *regexp_compiling;       /* Points to string for error msg */
extern regexp *regexp_From;           /* For recognizing "From_" lines */
extern regexp *regexp_ip_address;     /* Compiled r.e. for matching address */
extern regexp *regexp_spoolfile;      /* Compiled r.e. for spool file names */
extern char   *relay_domains;         /* Official relay domains */
extern int     remote_max_parallel;   /* Maximum parallel delivery */
extern char   *remote_sort;           /* Remote domain sorting order */
extern retry_config *retries;         /* Chain of retry config information */
extern int    retry_interval_max;     /* Absolute maximum */
extern int    retry_maximum_timeout;  /* The maximum timeout */
extern char  *return_path;            /* Return path for a message */
extern BOOL   return_path_remove;     /* Remove return-path headers */
extern int    return_size_limit;      /* Limit messages returned to sender */
extern int    rewrite_existflags;     /* Indicate which headers have rewrites */
extern rewrite_rule *rewrite_rules;   /* Chain of rewriting rules */
extern char  *rfc1413_except_hosts;   /* RFC 1413 exceptions */
extern host_item *rfc1413_except_hostlist; /* Munged data */
extern char  *rfc1413_except_nets;    /* RFC 1413 exceptions */
extern ip_net_item *rfc1413_except_netlist; /* Munged data */
extern int    rfc1413_query_timeout;  /* Timeout on RFC 1413 calls */
extern uid_t  root_uid;               /* The uid for root */
extern char  *route_option;           /* Option string for some routers */

extern router_info routers_available[]; /* Vector of available routers */
extern router_instance *routers;      /* Chain of instantiated routers */
extern router_instance router_defaults; /* Default values */

extern int    security_level;         /* For the set(e)uid stuff */
extern char  *security_type;          /* Text name for above, set by config */
extern char  *sender_accept;          /* Allow certain senders only */
extern char  *sender_accept_recipients; /* Ditto, reject RCPT TO */
extern char  *sender_address;         /* Envelope sender */
extern char  *sender_address_relay;   /* Only non-nearby relay from these addresses */
extern char  *sender_fullhost;        /* Sender host name + address */
extern char  *sender_host_accept;     /* Accept only from these hosts */
extern host_item *sender_host_accept_hosts; /* Munged data */
extern char  *sender_host_accept_relay; /* Accept non-nearby relay only from these hosts */
extern host_item *sender_host_accept_relay_hosts; /* Munged data */
extern char  *sender_host_address;    /* IP address of sender, as chars */
extern char  *sender_host_name;       /* Host name from HELO/EHLO */
extern char  *sender_host_reject;     /* Reject from these hosts */
extern host_item *sender_host_reject_hosts; /* Munged data */
extern char  *sender_host_reject_recipients; /* Reject RCPT from these hosts */
extern host_item *sender_host_reject_recipients_hosts; /* Munged data */
extern char  *sender_host_reject_relay; /* Reject non-nearby relay from these hosts */
extern host_item *sender_host_reject_relay_hosts; /* Munged data */
extern BOOL   sender_host_unknown;    /* TRUE for -bs and -bS except inetd */
extern char  *sender_ident;           /* Sender identity via RFC 1413 */
extern BOOL   sender_local;           /* TRUE for local senders */
extern char  *sender_net_accept;      /* Accept only from these nets */
extern ip_net_item *sender_net_accept_nets; /* Munged data */
extern char  *sender_net_accept_relay; /* Accept non-nearby relay from these nets */
extern ip_net_item *sender_net_accept_relay_nets; /* Munged data */
extern char  *sender_net_reject;      /* Reject from these nets */
extern ip_net_item *sender_net_reject_nets; /* Munged data */
extern char  *sender_net_reject_recipients; /* Reject RCPT from these nets */
extern ip_net_item *sender_net_reject_recipients_nets; /* Munged data */
extern char  *sender_net_reject_relay; /* Reject non-nearby relay from these nets */
extern ip_net_item *sender_net_reject_relay_nets; /* Munged data */
extern char  *sender_reject;          /* Spam filter list */
extern char  *sender_reject_recipients; /* Ditto, reject RCPT TO */
extern BOOL   sender_try_verify;      /* Accept if soft error while verifying */
extern char  *sender_unqualified_hosts; /* Permitted unqualified senders */
extern host_item *sender_unqualified_hostlist;
extern char  *sender_unqualified_nets;
extern ip_net_item *sender_unqualified_netlist;
extern BOOL   sender_verify;          /* TRUE when verifying senders */
extern char  *sender_verify_except_hosts; /* Verification exceptions */
extern host_item *sender_verify_except_hostlist; /* Munged data */
extern char  *sender_verify_except_nets; /* Verification exceptions */
extern ip_net_item *sender_verify_except_netlist; /* Munged data */
extern BOOL   sender_verify_fixup;    /* Fix broken senders from headers */
extern BOOL   sender_verify_log_details; /* For debugging */
extern BOOL   sender_verify_reject;   /* Reject if verify fails */
extern char **sighup_argv;            /* Args for re-execing after SIGHUP */
extern int    smtp_accept_count;      /* Count of connections */
extern int    smtp_accept_max;        /* Max SMTP connections */
extern int    smtp_accept_queue;      /* Queue after so many connections */
extern int    smtp_accept_reserve;    /* Reserve these SMTP connections */
extern char  *smtp_banner;            /* Banner string (to be expanded) */
extern BOOL   smtp_batched_input;     /* TRUE if SMTP batch (no interaction) */
extern int    smtp_connect_backlog;   /* Max backlog permitted */
extern host_item *smtp_etrn_hostlist; /* Munged data */
extern char  *smtp_etrn_hosts;        /* Accept ETRN only from these hosts */
extern ip_net_item *smtp_etrn_netlist; /* Munged data */
extern char  *smtp_etrn_nets;         /* Networks for ETRN command */
extern BOOL   smtp_input;             /* TRUE if input is via SMTP */
extern int    smtp_load_reserve;      /* Only from reserved if load > this */
extern int    smtp_port;              /* Alternate SMTP port number */
extern int    smtp_receive_timeout;   /* Applies to each received line */
extern host_item *smtp_reserve_hostlist; /* Munged data */
extern char  *smtp_reserve_hosts;     /* Hosts for reserved slots */
extern ip_net_item *smtp_reserve_netlist; /* Munged data */
extern char  *smtp_reserve_nets;      /* Networks for reserved slots */
extern BOOL   smtp_verify;            /* TRUE if VRFY permitted */
extern char  *spool_directory;        /* Name of spool directory */
extern char  *stderr_filename;        /* File for use with -df */
extern char   string_return_buffer[]; /* For returning various strings */
extern BOOL   strip_excess_angle_brackets; /* Surrounding route-addrs */
extern BOOL   strip_trailing_dot;     /* Remove dots at ends of domains */

extern transport_instance *transport_address_file; /* Transport for file addresses */
extern transport_instance *transport_address_pipe; /* Transport for pipe addresses */
extern transport_instance *transport_address_reply; /* Transport for reply addresses */
extern transport_info transports_available[]; /* Vector of available transports */
extern transport_instance *transports;  /* Chain of instantiated transports */
extern transport_instance transport_defaults; /* Default values */

extern int    transport_chunk_timeout;/* Set to time out individual writes */
extern volatile BOOL transport_sigalrm_seen; /* Flag for transport timeouts */

extern tree_node *tree_duplicates;    /* Tree of duplicate addresses */
extern tree_node *tree_nonrecipients; /* Tree of nonrecipient addresses */
extern tree_node *tree_unusable;      /* Tree of unusable addresses */

extern BOOL   trusted_caller;         /* Caller is trusted */
extern gid_t *trusted_groups;         /* List of trusted groups */
extern uid_t *trusted_users;          /* List of trusted users */

extern char  *unknown_login;          /* To use when login id unknown */
extern char  *unknown_username;       /* Ditto */
extern char  *user_login;             /* Login name of user */
extern char  *user_name;              /* User full name */
extern BOOL   user_null_sender;       /* TRUE if non-trusted supplied -f <> */

extern BOOL   verify_only;            /* TRUE for -bv */
extern char  *version_copyright;      /* Copyright notice */
extern char  *version_date;           /* Date of compilation */
extern char  *version_cnumber;        /* Compile number */
extern char  *version_string;         /* Version string */

extern int    warning_count;          /* Delay warnings sent for this msg */

/* End of globals.h */
